unit MultiThreadUnit;

interface

uses
  System.Diagnostics,
  System.Threading,
  System.SyncObjs,
  System.Classes,
  System.SysUtils,
  System.StrUtils;

type
  // size entry_array to hold longest (by byte count) name
  // Dolores Hidalgo Cuna de la Independencia Nacional = 98 bytes
  // with temperature as large as: -999.9  (maybe just -99.9 ?)
  // so, add a bit, and use...
  // entry_array: array [0 .. 107] of Byte; // will hold place name and temp
  // note shortest seems to be: Xyz;0.0<LF>  -> 8 bytes
  TRawRecord = record
    RawData: array [0 .. 107] of Byte;
    // bytes for each entry line  // 105 works
  end;

  THashRecord = record // each array item is compiled data for a place
    DataCount: Integer; // number of temperature measurements
    DataSum: Integer; // temperature times ten
    DataMax: SmallInt; // temperature times ten
    DataMin: SmallInt; // temperature times ten
    DataName: String;
  end;

  PHashRecord = ^THashRecord;

const
  // see http://compoasso.free.fr/primelistweb/page/prime/liste_online_en.php
  Prime: Integer = 18701;

  ReadBufferSize: Integer = 32768; // 16384 is OK, 32768 seems optimum-ish
  ReadBufferMargin: Integer = 128; // was 128
  // reduce requested number of bytes by this much when reading

  StackCount: Integer = 5; // how many groups to split up the alphabet

  // near max on my PC memory, roughly 10 GB for 5 buffers at 3.5 million items each

  DataBufferSize1: Integer = 3500001;
  DataBufferSize2: Integer = 3500001;
  DataBufferSize3: Integer = 3500001;
  DataBufferSize4: Integer = 3500001;
  DataBufferSize5: Integer = 3500001;
  DataBufferCushion: Integer = 10; // beyound this point, sleep happens

  // split points for stacks, using first three characters
  // so, ignoring Unicode, Aaa through Zzz
  // decimal 65/97/97 through 90/122/122
  // define choice as sum of three byte values minus 258
  // that makes range 1 to 76
  SplitPoint1: Integer = 21;
  SplitPoint2: Integer = 29;
  SplitPoint3: Integer = 35;
  SplitPoint4: Integer = 44;

var
  inputFilename: String;
  outputFilename: String;
  Challenge: TFileStream;

  UseStdOut: Boolean; // True unless output file is defined
  ReadThreadCount: Integer;
  TabulateOn: Boolean; // when false, data is not pushed into stacks

  // set up for 41351 places (actually seems to be 41343 entries)
  // split into five roughly equal groups for separate hash tables

  HashRecordMap1: array [0 .. 18700] of THashRecord; // for Prime := 18701
  HashRecordMap2: array [0 .. 18700] of THashRecord; // for Prime := 18701
  HashRecordMap3: array [0 .. 18700] of THashRecord; // for Prime := 18701
  HashRecordMap4: array [0 .. 18700] of THashRecord; // for Prime := 18701
  HashRecordMap5: array [0 .. 18700] of THashRecord; // for Prime := 18701

  DataStack1: array [0 .. 3500000] of TRawRecord;
  DataStack2: array [0 .. 3500000] of TRawRecord;
  DataStack3: array [0 .. 3500000] of TRawRecord;
  DataStack4: array [0 .. 3500000] of TRawRecord;
  DataStack5: array [0 .. 3500000] of TRawRecord;

  DataStackCount1: Integer;
  DataStackCount2: Integer;
  DataStackCount3: Integer;
  DataStackCount4: Integer;
  DataStackCount5: Integer;

  DataStackLock1: TCriticalSection;
  DataStackLock2: TCriticalSection;
  DataStackLock3: TCriticalSection;
  DataStackLock4: TCriticalSection;
  DataStackLock5: TCriticalSection;

  HashRecordLock1: TCriticalSection;
  HashRecordLock2: TCriticalSection;
  HashRecordLock3: TCriticalSection;
  HashRecordLock4: TCriticalSection;
  HashRecordLock5: TCriticalSection;

  ReadLock: TCriticalSection; // controls reading file

  StationsForSort: TStringList;

  LineCount: Int64;

  ReadFile_Done1: Boolean;
  ReadFile_Done2: Boolean;
  ReadFile_Done3: Boolean;
  ParseData_Done: Boolean;

  StackMax1: Integer; // sample stack count for peak value (approx.)
  StackMax2: Integer;
  StackMax3: Integer;
  StackMax4: Integer;
  StackMax5: Integer;

procedure FileToArrays(inputFilename: String; UseStdOut: Boolean); // read
procedure LaunchReadingThread1;
procedure LaunchReadingThread2;
procedure LaunchReadingThread3;
procedure LaunchTabulateThread1;
procedure LaunchTabulateThread2;
procedure LaunchTabulateThread3;
procedure LaunchTabulateThread4;
procedure LaunchTabulateThread5;

procedure SortArrays;
procedure ArrayToFile(outFile: String; UseStdOut: Boolean);

implementation

function IntegerFixup(something: Integer): String; inline;
// fixup to adjust for storing tenths of degrees as integer
// e.g. convert integer 234 to string 23.4 and -167 to -16.7
var
  thing: String;
begin
  thing := IntToStr(something);
  thing := LeftStr(thing, Length(thing) - 1) + '.' + RightStr(thing, 1);
  // make sure we have leading zero, where applicable
  if thing[1] = '.' then
    thing := '0' + thing;
  IntegerFixup := thing.Replace('-.', '-0.');
end;

function MeanFixup(total: Integer; count: Integer): String; inline;
// fixup to adjust for storing tenths of degrees as integer
// calculate mean to one decimal place, rounded up/down depending on sign
var
  temp: string;
  ratio: Integer;
  // remainder: Integer;
  neg: Boolean;
begin
  if total < 0 then
  begin
    neg := True;
    total := -total;
  end
  else
  begin
    neg := False;
  end;
  ratio := total div count;
  if (neg) then // no rounding needed
  begin
    temp := IntToStr(ratio);
    if (ratio > 9) then
    begin
      // string modification equivalent to dividing by ten
      temp := '-' + LeftStr(temp, Length(temp) - 1) + '.' + temp[Length(temp)];
    end
    else
    begin
      if temp[1] = '0' then
      begin
        temp := '0.' + temp;
      end
      else
      begin
        temp := '-0.' + temp;
      end;
    end;
  end
  else // round up
  begin
    temp := IntToStr(ratio + 1);
    if (ratio > 8) then // 8, rather than 9, since one was added
    begin
      // string modification equivalent to dividing by ten
      temp := LeftStr(temp, Length(temp) - 1) + '.' + temp[Length(temp)];
    end
    else
    begin
      temp := '0.' + temp;
    end;
  end;
  MeanFixup := temp; // was temp;
end;

procedure SortArrays; inline;
var
  i: Integer;
begin
  StationsForSort := TStringList.Create; // and explicitly set options
  // sorts strings using ANSI (Windows) or UTF-8 (Linux) order
  StationsForSort.UseLocale := False; // True => ansi sort, NOT wanted
  StationsForSort.Duplicates := dupIgnore;
  // Ignore attempts to add duplicate strings to the list.
  StationsForSort.CaseSensitive := True; // default is false
  StationsForSort.OwnsObjects := True;
  // so, Stations object destroys its data objects
  StationsForSort.Sorted := False; // sort later

  // load strings from HashRecordMaps with the corresponding indexes
  for i := 0 to 18700 do // for Prime := 18701
  begin
    if HashRecordMap1[i].DataCount > 0 then // add
    begin
      StationsForSort.AddObject(HashRecordMap1[i].DataName, @HashRecordMap1[i]);
    end;

    if HashRecordMap2[i].DataCount > 0 then // add
    begin
      StationsForSort.AddObject(HashRecordMap2[i].DataName, @HashRecordMap2[i]);
    end;

    if HashRecordMap3[i].DataCount > 0 then // add
    begin
      StationsForSort.AddObject(HashRecordMap3[i].DataName, @HashRecordMap3[i]);
    end;

    if HashRecordMap4[i].DataCount > 0 then // add
    begin
      StationsForSort.AddObject(HashRecordMap4[i].DataName, @HashRecordMap4[i]);
    end;

    if HashRecordMap5[i].DataCount > 0 then // add
    begin
      StationsForSort.AddObject(HashRecordMap5[i].DataName, @HashRecordMap5[i]);
    end;
  end;

  StationsForSort.Sort;
end;

procedure ArrayToFile(outFile: String; UseStdOut: Boolean);
var
  outputFileStream: TFileStream;
  PlaceIndex: Integer; // location in array
  bufferStr: String;
  EntrySum: Integer;
  EntryCount: Integer;
  EntryMax: Integer;
  EntryMin: Integer;

  EntryPointer: PHashRecord;

begin
  try
    if Not(UseStdOut) then
    begin
      outputFileStream := TFileStream.Create(outFile, fmCreate);
    end;

    for PlaceIndex := 0 to StationsForSort.count - 1 do
    begin
      if PlaceIndex = 0 then
      begin
        bufferStr := '{';
      end
      else
      begin
        bufferStr := ', ';
      end;

      EntryPointer := Pointer(StationsForSort.Objects[PlaceIndex]);
      EntrySum := EntryPointer^.DataSum;
      EntryCount := EntryPointer^.DataCount;
      EntryMax := EntryPointer^.DataMax;
      EntryMin := EntryPointer^.DataMin;

      bufferStr := bufferStr + StationsForSort[PlaceIndex] + '=' +
        IntegerFixup(EntryMin) + '/' + MeanFixup(EntrySum, EntryCount) + '/' +
        IntegerFixup(EntryMax);

      if UseStdOut then // send to STDOUT, where it gets mangled
      begin
        write(bufferStr);
      end
      else
      begin
        outputFileStream.WriteBuffer(TEncoding.UTF8.GetBytes(bufferStr),
          TEncoding.UTF8.GetByteCount(bufferStr));
      end;
    end;

    bufferStr := '}' + Chr(10); // linefeed appears at end of baseline file
    if UseStdOut then // send to STDOUT, where it gets mangled in Windows
    begin
      write(bufferStr);
    end
    else
    begin
      outputFileStream.WriteBuffer(TEncoding.UTF8.GetBytes(bufferStr),
        TEncoding.UTF8.GetByteCount(bufferStr));
      outputFileStream.Free;
    end;

  finally
    if Not(UseStdOut) then
    begin
      WriteLn;
      WriteLn;
      WriteLn('Done');
    end;
  end;

end;

procedure FileToArrays(inputFilename: String; UseStdOut: Boolean);
var
  i: Integer;

begin
  DataStackCount1 := 0;
  DataStackCount2 := 0;
  DataStackCount3 := 0;
  DataStackCount4 := 0;
  DataStackCount5 := 0;

  // initialize a few things
  ReadFile_Done1 := False;
  ReadFile_Done2 := False;
  ParseData_Done := False;

  DataStackLock1 := TCriticalSection.Create;
  DataStackLock2 := TCriticalSection.Create;
  DataStackLock3 := TCriticalSection.Create;
  DataStackLock4 := TCriticalSection.Create;
  DataStackLock5 := TCriticalSection.Create;

  HashRecordLock1 := TCriticalSection.Create;
  HashRecordLock2 := TCriticalSection.Create;
  HashRecordLock3 := TCriticalSection.Create;
  HashRecordLock4 := TCriticalSection.Create;
  HashRecordLock5 := TCriticalSection.Create;

  ReadLock := TCriticalSection.Create;

  StackMax1 := 0;
  StackMax2 := 0;
  StackMax3 := 0;
  StackMax4 := 0;
  StackMax5 := 0;

  LineCount := 0;

  // pre-fill arrays
  for i := 0 to Prime - 1 do
  begin
    HashRecordMap1[i].DataCount := 0;
    HashRecordMap2[i].DataCount := 0;
    HashRecordMap3[i].DataCount := 0;
    HashRecordMap4[i].DataCount := 0;
    HashRecordMap5[i].DataCount := 0;
  end;

  // setup TStringList for sorting
  StationsForSort := TStringList.Create; // and explicitly set options
  // sorts strings using ANSI (Windows) or UTF-8 (Linux) order
  StationsForSort.UseLocale := False; // True => ansi sort, NOT wanted
  StationsForSort.Duplicates := dupIgnore;
  // Ignore attempts to add duplicate strings to the list.
  StationsForSort.CaseSensitive := True; // default is false
  StationsForSort.OwnsObjects := True;
  // so, Stations object destroys its data objects
  StationsForSort.Sorted := False; // sort later

  if FileExists(inputFilename) then
  begin
    if TabulateOn then
    begin
      LaunchTabulateThread1;
      LaunchTabulateThread2;
      LaunchTabulateThread3;
      LaunchTabulateThread4;
      LaunchTabulateThread5;
    end;

    Challenge := TFileStream.Create(inputFilename, fmOpenRead, fmShareDenyNone);
    // Challenge := TBufferedFileStream.Create(inputFilename, fmOpenRead, fmShareDenyNone);

    LaunchReadingThread1;
    if ReadThreadCount >= 2 then
      LaunchReadingThread2;
    if ReadThreadCount = 3 then
      LaunchReadingThread3;
  end
  else
  begin
    raise Exception.Create(Format('File "%s" not found.', [inputFilename]));
  end;

end;

procedure LaunchReadingThread1;
var
  ReadingThread1: TThread;

begin
  ReadingThread1 := TThread.CreateAnonymousThread(
    procedure
    var
      ReadItem1: array [0 .. 107] of Byte;
      BufferIndex: Integer; // index into read buffer
      DataIndex: Integer; // index into raw data array
      Choice: Integer;
      BytesRead: Integer;
      SafetyCount: Integer;
      PReadBuffer1: PByte;
      Posted: Boolean; // flag for successfully sending to queue

    begin
      PReadBuffer1 := System.AllocMem(ReadBufferSize);
      BytesRead := 1; // some value > 0
      while BytesRead > 0 do
      begin
        ReadLock.Acquire;
        BytesRead := Challenge.ReadData(PReadBuffer1,
          ReadBufferSize - ReadBufferMargin);

        if BytesRead < 8 then
        // problem, or just end of file?   ================
        begin
          if Not(UseStdOut) then
          begin
            WriteLn('EOF 1: ' + IntToStr(BytesRead));
          end;
          ReadLock.Release;
          Break;
        end;

        if Not((PReadBuffer1 + BytesRead - 1)^ = 10) then
        // did not get LF at end
        begin
          // read a few more bytes until we get a line feed
          for SafetyCount := 1 to ReadBufferMargin do
          begin
            if Challenge.ReadData((PReadBuffer1 + BytesRead)^, 1) <= 0 then
            // EOF?
            begin
              Break;
            end
            else
            begin
              inc(BytesRead);
              if ((PReadBuffer1 + BytesRead - 1)^ = 10) then // done
              begin
                Break;
              end;
            end;
          end;
          if SafetyCount = ReadBufferMargin then // problem
          begin
            if Not(UseStdOut) then
            begin
              WriteLn('Need bigger ReadBufferMargin');
            end;
          end;
        end;

        ReadLock.Release;

        DataIndex := 0;

        BufferIndex := 0;
        while BufferIndex < BytesRead do
        begin
          if ((PReadBuffer1 + BufferIndex)^ = 10) then // line feed
          begin
            // done collecting bytes, tack on null  as data end flag
            ReadItem1[DataIndex] := 0;

            // Posted := False;
            // hack to run reading without actually send data to stacks
            Posted := Not(TabulateOn);

            // try for an even distribution to the five stacks
            // define choice as sum of two byte values minus 161
            // Choice := ReadItem1[0] + ReadItem1[1] - 161;
            // define choice as sum of three byte values minus 258
            Choice := ReadItem1[0] + ReadItem1[1] + ReadItem1[2] - 258;

            if Choice <= SplitPoint2 then
            begin
              if Choice <= SplitPoint1 then
              begin
                Choice := 0;
              end
              else

              begin
                Choice := 1;
              end;
            end
            else
            begin
              if Choice <= SplitPoint3 then
              begin
                Choice := 2;
              end
              else
              begin
                if Choice <= SplitPoint4 then
                begin
                  Choice := 3;
                end
                else
                begin
                  Choice := 4;
                end;
              end;
            end;

            case Choice of
              0:
                begin
                  while True do // break on success
                  begin
                    if (DataStackCount1 < DataBufferSize1 - DataBufferCushion)
                    then
                    begin
                      // lock and attempt to store the data
                      while Not Posted do
                      begin
                        DataStackLock1.Acquire;
                        if (DataStackCount1 < DataBufferSize1 - 2) then
                        begin
                          Move(ReadItem1[0], DataStack1[DataStackCount1].RawData
                            [0], SizeOf(ReadItem1));
                          inc(DataStackCount1);
                          DataStackLock1.Release;
                          Posted := True;
                        end
                        else
                        begin
                          DataStackLock1.Release;
                          Sleep(1); // stack is full
                        end;
                      end;
                      If Posted then
                        Break;
                    end
                    else
                    begin
                      Sleep(1); // stack is full
                    end;
                  end;
                end;
              1:
                begin
                  while True do // break on success
                  begin
                    if (DataStackCount2 < DataBufferSize2 - DataBufferCushion)
                    then
                    begin
                      // lock and attempt to store the data
                      while Not Posted do
                      begin
                        DataStackLock2.Acquire;
                        if (DataStackCount2 < DataBufferSize2 - 2) then
                        begin
                          Move(ReadItem1[0], DataStack2[DataStackCount2].RawData
                            [0], SizeOf(ReadItem1));
                          inc(DataStackCount2);
                          DataStackLock2.Release;
                          Posted := True;
                        end
                        else
                        begin
                          DataStackLock2.Release;
                          Sleep(1); // stack is full
                        end;
                      end;
                      If Posted then
                        Break;
                    end
                    else
                    begin
                      Sleep(1); // stack is full
                    end;
                  end;
                end;
              2:
                begin
                  while True do // break on success
                  begin
                    if (DataStackCount3 < DataBufferSize3 - DataBufferCushion)
                    then
                    begin
                      // lock and attempt to store the data
                      while Not Posted do
                      begin
                        DataStackLock3.Acquire;
                        if (DataStackCount3 < DataBufferSize3 - 2) then
                        begin
                          Move(ReadItem1[0], DataStack3[DataStackCount3].RawData
                            [0], SizeOf(ReadItem1));
                          inc(DataStackCount3);
                          DataStackLock3.Release;
                          Posted := True;
                        end
                        else
                        begin
                          DataStackLock3.Release;
                          Sleep(1); // stack is full
                        end;
                      end;
                      If Posted then
                        Break;
                    end
                    else
                    begin
                      Sleep(1); // stack is full
                    end;
                  end;
                end;
              3:
                begin
                  while True do // break on success
                  begin
                    if (DataStackCount4 < DataBufferSize4 - DataBufferCushion)
                    then
                    begin
                      // lock and attempt to store the data
                      while Not Posted do
                      begin
                        DataStackLock4.Acquire;
                        if (DataStackCount4 < DataBufferSize4 - 2) then
                        begin
                          Move(ReadItem1[0], DataStack4[DataStackCount4].RawData
                            [0], SizeOf(ReadItem1));
                          inc(DataStackCount4);
                          DataStackLock4.Release;
                          Posted := True;
                        end
                        else
                        begin
                          DataStackLock4.Release;
                          Sleep(1); // stack is full
                        end;
                      end;
                      If Posted then
                        Break;
                    end
                    else
                    begin
                      Sleep(1); // stack is full
                    end;
                  end;
                end;
              4:
                begin
                  while True do // break on success
                  begin
                    if (DataStackCount5 < DataBufferSize5 - DataBufferCushion)
                    then
                    begin
                      // lock and attempt to store the data
                      while Not Posted do
                      begin
                        DataStackLock5.Acquire;
                        if (DataStackCount5 < DataBufferSize5 - 2) then
                        begin
                          Move(ReadItem1[0], DataStack5[DataStackCount5].RawData
                            [0], SizeOf(ReadItem1));
                          inc(DataStackCount5);
                          DataStackLock5.Release;
                          Posted := True;
                        end
                        else
                        begin
                          DataStackLock5.Release;
                          Sleep(1); // stack is full
                        end;
                      end;
                      If Posted then
                        Break;
                    end
                    else
                    begin
                      Sleep(1); // stack is full
                    end;
                  end;
                end;
            end;

            // done processing, reset for next line
            inc(LineCount);
            DataIndex := 0;
          end
          else // accumulate bytes
          begin
            ReadItem1[DataIndex] := (PReadBuffer1 + BufferIndex)^;
            inc(DataIndex);
          end;
          inc(BufferIndex);

        end; // of: while i < iBytesRead do

      end; // of while iBytesRead > 0 do   --- implies end of file

      ReadFile_Done1 := True;

    end);
  ReadingThread1.Start;
end;

procedure LaunchReadingThread2;
var
  ReadingThread2: TThread;

begin
  ReadingThread2 := TThread.CreateAnonymousThread(
    procedure
    var
      ReadItem2: array [0 .. 107] of Byte;
      BufferIndex: Integer; // index into read buffer
      DataIndex: Integer; // index into raw data array
      Choice: Integer;
      BytesRead: Integer;
      SafetyCount: Integer;
      PReadBuffer2: PByte;
      Posted: Boolean; // flag for successfully sending to queue

    begin
      PReadBuffer2 := System.AllocMem(ReadBufferSize);
      BytesRead := 1; // some value > 0
      while BytesRead > 0 do
      begin
        ReadLock.Acquire;
        BytesRead := Challenge.ReadData(PReadBuffer2,
          ReadBufferSize - ReadBufferMargin);

        if BytesRead < 8 then
        // problem, or just end of file?   ================
        begin
          if Not(UseStdOut) then
          begin
            WriteLn('EOF 2: ' + IntToStr(BytesRead));
          end;
          ReadLock.Release;
          Break;
        end;

        if Not((PReadBuffer2 + BytesRead - 1)^ = 10) then
        // did not get LF at end
        begin
          // read a few more bytes until we get a line feed
          for SafetyCount := 1 to ReadBufferMargin do
          begin
            if Challenge.ReadData((PReadBuffer2 + BytesRead)^, 1) <= 0 then
            // EOF?
            begin
              Break;
            end
            else
            begin
              inc(BytesRead);
              if ((PReadBuffer2 + BytesRead - 1)^ = 10) then // done
              begin
                Break;
              end;
            end;
          end;
          if SafetyCount = ReadBufferMargin then // problem
          begin
            if Not(UseStdOut) then
            begin
              WriteLn('Need bigger ReadBufferMargin');
            end;
          end;
        end;

        ReadLock.Release;

        DataIndex := 0;

        BufferIndex := 0;
        while BufferIndex < BytesRead do
        begin
          if ((PReadBuffer2 + BufferIndex)^ = 10) then // line feed
          begin
            // done collecting bytes, tack on null  as data end flag
            // DataStackItem.RawData[DataIndex] := 0;
            ReadItem2[DataIndex] := 0;

            // Posted := False;
            // hack to run reading without actually send data to stacks
            Posted := Not(TabulateOn);

            // try for an even distribution to the five stacks
            // define choice as sum of two byte values minus 161
            // Choice := ReadItem1[0] + ReadItem1[1] - 161;
            // define choice as sum of three byte values minus 258
            Choice := ReadItem2[0] + ReadItem2[1] + ReadItem2[2] - 258;

            if Choice <= SplitPoint2 then
            begin
              if Choice <= SplitPoint1 then
              begin
                Choice := 0;
              end
              else
              begin
                Choice := 1;
              end;
            end
            else
            begin
              if Choice <= SplitPoint3 then
              begin
                Choice := 2;
              end
              else
              begin
                if Choice <= SplitPoint4 then
                begin
                  Choice := 3;
                end
                else
                begin
                  Choice := 4;
                end;
              end;
            end;
            case Choice of
              0:
                begin
                  while True do // break on success
                  begin
                    if (DataStackCount1 < DataBufferSize1 - DataBufferCushion)
                    then
                    begin
                      // lock and attempt to store the data
                      while Not Posted do
                      begin
                        DataStackLock1.Acquire;
                        if (DataStackCount1 < DataBufferSize1 - 2) then
                        begin
                          Move(ReadItem2[0], DataStack1[DataStackCount1].RawData
                            [0], SizeOf(ReadItem2));
                          inc(DataStackCount1);
                          DataStackLock1.Release;
                          Posted := True;
                        end
                        else
                        begin
                          DataStackLock1.Release;
                          Sleep(1); // stack is full
                        end;
                      end;
                      If Posted then
                        Break;
                    end
                    else
                    begin
                      Sleep(1); // stack is full
                    end;
                  end;
                end;
              1:
                begin
                  while True do // break on success
                  begin
                    if (DataStackCount2 < DataBufferSize2 - DataBufferCushion)
                    then
                    begin
                      // lock and attempt to store the data
                      while Not Posted do
                      begin
                        DataStackLock2.Acquire;
                        if (DataStackCount2 < DataBufferSize2 - 2) then
                        begin
                          Move(ReadItem2[0], DataStack2[DataStackCount2].RawData
                            [0], SizeOf(ReadItem2));
                          inc(DataStackCount2);
                          DataStackLock2.Release;
                          Posted := True;
                        end
                        else
                        begin
                          DataStackLock2.Release;
                          Sleep(1); // stack is full
                        end;
                      end;
                      If Posted then
                        Break;
                    end
                    else
                    begin
                      Sleep(1); // stack is full
                    end;
                  end;
                end;
              2:
                begin
                  while True do // break on success
                  begin
                    if (DataStackCount3 < DataBufferSize3 - DataBufferCushion)
                    then
                    begin
                      // lock and attempt to store the data
                      while Not Posted do
                      begin
                        DataStackLock3.Acquire;
                        if (DataStackCount3 < DataBufferSize3 - 2) then
                        begin
                          Move(ReadItem2[0], DataStack3[DataStackCount3].RawData
                            [0], SizeOf(ReadItem2));
                          inc(DataStackCount3);
                          DataStackLock3.Release;
                          Posted := True;
                        end
                        else
                        begin
                          DataStackLock3.Release;
                          Sleep(1); // stack is full
                        end;
                      end;
                      If Posted then
                        Break;
                    end
                    else
                    begin
                      Sleep(1); // stack is full
                    end;
                  end;
                end;
              3:
                begin
                  while True do // break on success
                  begin
                    if (DataStackCount4 < DataBufferSize4 - DataBufferCushion)
                    then
                    begin
                      // lock and attempt to store the data
                      while Not Posted do
                      begin
                        DataStackLock4.Acquire;
                        if (DataStackCount4 < DataBufferSize4 - 2) then
                        begin
                          Move(ReadItem2[0], DataStack4[DataStackCount4].RawData
                            [0], SizeOf(ReadItem2));
                          inc(DataStackCount4);
                          DataStackLock4.Release;
                          Posted := True;
                        end
                        else
                        begin
                          DataStackLock4.Release;
                          Sleep(1); // stack is full
                        end;
                      end;
                      If Posted then
                        Break;
                    end
                    else
                    begin
                      Sleep(1); // stack is full
                    end;
                  end;
                end;
              4:
                begin
                  while True do // break on success
                  begin
                    if (DataStackCount5 < DataBufferSize5 - DataBufferCushion)
                    then
                    begin
                      // lock and attempt to store the data
                      while Not Posted do
                      begin
                        DataStackLock5.Acquire;
                        if (DataStackCount5 < DataBufferSize5 - 2) then
                        begin
                          Move(ReadItem2[0], DataStack5[DataStackCount5].RawData
                            [0], SizeOf(ReadItem2));
                          inc(DataStackCount5);
                          DataStackLock5.Release;
                          Posted := True;
                        end
                        else
                        begin
                          DataStackLock5.Release;
                          Sleep(1); // stack is full
                        end;
                      end;
                      If Posted then
                        Break;
                    end
                    else
                    begin
                      Sleep(1); // stack is full
                    end;
                  end;
                end;
            end;

            // done processing, reset for next line
            inc(LineCount);
            DataIndex := 0;
          end
          else // accumulate bytes
          begin
            ReadItem2[DataIndex] := (PReadBuffer2 + BufferIndex)^;
            inc(DataIndex);
          end;
          inc(BufferIndex);

        end; // of: while i < iBytesRead do

      end; // of while iBytesRead > 0 do   --- implies end of file

      ReadFile_Done2 := True;

    end);
  ReadingThread2.Start;
end;

procedure LaunchReadingThread3;
var
  ReadingThread3: TThread;

begin
  ReadingThread3 := TThread.CreateAnonymousThread(
    procedure
    var
      ReadItem3: array [0 .. 107] of Byte;
      BufferIndex: Integer; // index into read buffer
      DataIndex: Integer; // index into raw data array
      Choice: Integer;
      BytesRead: Integer;
      SafetyCount: Integer;
      PReadBuffer3: PByte;
      Posted: Boolean; // flag for successfully sending to queue

    begin
      PReadBuffer3 := System.AllocMem(ReadBufferSize);
      BytesRead := 1; // some value > 0
      while BytesRead > 0 do
      begin
        ReadLock.Acquire;
        BytesRead := Challenge.ReadData(PReadBuffer3,
          ReadBufferSize - ReadBufferMargin);

        if BytesRead < 8 then
        // problem, or just end of file?   ================
        begin
          if Not(UseStdOut) then
          begin
            WriteLn('EOF 3: ' + IntToStr(BytesRead));
          end;
          ReadLock.Release;
          Break;
        end;

        if Not((PReadBuffer3 + BytesRead - 1)^ = 10) then
        // did not get LF at end
        begin
          // read a few more bytes until we get a line feed
          for SafetyCount := 1 to ReadBufferMargin do
          begin
            if Challenge.ReadData((PReadBuffer3 + BytesRead)^, 1) <= 0 then
            // EOF?
            begin
              Break;
            end
            else
            begin
              inc(BytesRead);
              if ((PReadBuffer3 + BytesRead - 1)^ = 10) then // done
              begin
                Break;
              end;
            end;
          end;
          if SafetyCount = ReadBufferMargin then // problem
          begin
            if Not(UseStdOut) then
            begin
              WriteLn('Need bigger ReadBufferMargin');
            end;
          end;
        end;

        ReadLock.Release;

        DataIndex := 0;

        BufferIndex := 0;
        while BufferIndex < BytesRead do
        begin
          if ((PReadBuffer3 + BufferIndex)^ = 10) then // line feed
          begin
            // done collecting bytes, tack on null  as data end flag
            // DataStackItem.RawData[DataIndex] := 0;
            ReadItem3[DataIndex] := 0;

            // Posted := False;
            // hack to run reading without actually send data to stacks
            Posted := Not(TabulateOn);

            // try for an even distribution to the five stacks
            // define choice as sum of two byte values minus 161
            // Choice := ReadItem1[0] + ReadItem1[1] - 161;
            // define choice as sum of three byte values minus 258
            Choice := ReadItem3[0] + ReadItem3[1] + ReadItem3[2] - 258;

            if Choice <= SplitPoint2 then
            begin
              if Choice <= SplitPoint1 then
              begin
                Choice := 0;
              end
              else
              begin
                Choice := 1;
              end;
            end
            else
            begin
              if Choice <= SplitPoint3 then
              begin
                Choice := 2;
              end
              else
              begin
                if Choice <= SplitPoint4 then
                begin
                  Choice := 3;
                end
                else
                begin
                  Choice := 4;
                end;
              end;
            end;
            case Choice of
              0:
                begin
                  while True do // break on success
                  begin
                    if (DataStackCount1 < DataBufferSize1 - DataBufferCushion)
                    then
                    begin
                      // lock and attempt to store the data
                      while Not Posted do
                      begin
                        DataStackLock1.Acquire;
                        if (DataStackCount1 < DataBufferSize1 - 2) then
                        begin
                          Move(ReadItem3[0], DataStack1[DataStackCount1].RawData
                            [0], SizeOf(ReadItem3));
                          inc(DataStackCount1);
                          DataStackLock1.Release;
                          Posted := True;
                        end
                        else
                        begin
                          DataStackLock1.Release;
                          Sleep(1); // stack is full
                        end;
                      end;
                      If Posted then
                        Break;
                    end
                    else
                    begin
                      Sleep(1); // stack is full
                    end;
                  end;
                end;
              1:
                begin
                  while True do // break on success
                  begin
                    if (DataStackCount2 < DataBufferSize2 - DataBufferCushion)
                    then
                    begin
                      // lock and attempt to store the data
                      while Not Posted do
                      begin
                        DataStackLock2.Acquire;
                        if (DataStackCount2 < DataBufferSize2 - 2) then
                        begin
                          Move(ReadItem3[0], DataStack2[DataStackCount2].RawData
                            [0], SizeOf(ReadItem3));
                          inc(DataStackCount2);
                          DataStackLock2.Release;
                          Posted := True;
                        end
                        else
                        begin
                          DataStackLock2.Release;
                          Sleep(1); // stack is full
                        end;
                      end;
                      If Posted then
                        Break;
                    end
                    else
                    begin
                      Sleep(1); // stack is full
                    end;
                  end;
                end;
              2:
                begin
                  while True do // break on success
                  begin
                    if (DataStackCount3 < DataBufferSize3 - DataBufferCushion)
                    then
                    begin
                      // lock and attempt to store the data
                      while Not Posted do
                      begin
                        DataStackLock3.Acquire;
                        if (DataStackCount3 < DataBufferSize3 - 2) then
                        begin
                          Move(ReadItem3[0], DataStack3[DataStackCount3].RawData
                            [0], SizeOf(ReadItem3));
                          inc(DataStackCount3);
                          DataStackLock3.Release;
                          Posted := True;
                        end
                        else
                        begin
                          DataStackLock3.Release;
                          Sleep(1); // stack is full
                        end;
                      end;
                      If Posted then
                        Break;
                    end
                    else
                    begin
                      Sleep(1); // stack is full
                    end;
                  end;
                end;
              3:
                begin
                  while True do // break on success
                  begin
                    if (DataStackCount4 < DataBufferSize4 - DataBufferCushion)
                    then
                    begin
                      // lock and attempt to store the data
                      while Not Posted do
                      begin
                        DataStackLock4.Acquire;
                        if (DataStackCount4 < DataBufferSize4 - 2) then
                        begin
                          Move(ReadItem3[0], DataStack4[DataStackCount4].RawData
                            [0], SizeOf(ReadItem3));
                          inc(DataStackCount4);
                          DataStackLock4.Release;
                          Posted := True;
                        end
                        else
                        begin
                          DataStackLock4.Release;
                          Sleep(1); // stack is full
                        end;
                      end;
                      If Posted then
                        Break;
                    end
                    else
                    begin
                      Sleep(1); // stack is full
                    end;
                  end;
                end;
              4:
                begin
                  while True do // break on success
                  begin
                    if (DataStackCount5 < DataBufferSize5 - DataBufferCushion)
                    then
                    begin
                      // lock and attempt to store the data
                      while Not Posted do
                      begin
                        DataStackLock5.Acquire;
                        if (DataStackCount5 < DataBufferSize5 - 2) then
                        begin
                          Move(ReadItem3[0], DataStack5[DataStackCount5].RawData
                            [0], SizeOf(ReadItem3));
                          inc(DataStackCount5);
                          DataStackLock5.Release;
                          Posted := True;
                        end
                        else
                        begin
                          DataStackLock5.Release;
                          Sleep(1); // stack is full
                        end;
                      end;
                      If Posted then
                        Break;
                    end
                    else
                    begin
                      Sleep(1); // stack is full
                    end;
                  end;
                end;
            end;

            // done processing, reset for next line
            inc(LineCount);
            DataIndex := 0;
          end
          else // accumulate bytes
          begin
            ReadItem3[DataIndex] := (PReadBuffer3 + BufferIndex)^;
            inc(DataIndex);
          end;
          inc(BufferIndex);

        end; // of: while i < iBytesRead do

      end; // of while iBytesRead > 0 do   --- implies end of file

      ReadFile_Done3 := True;

    end);
  ReadingThread3.Start;
end;

procedure LaunchTabulateThread1;
var
  TabulateThread1: TThread;

begin
  TabulateThread1 := TThread.CreateAnonymousThread(
    procedure
    var
      DataStackItem: array [0 .. 107] of Byte;
      DataIndex: Integer; // index into raw data array
      SC: Boolean; // True after finding semi-colon
      entry_name_array: array [0 .. 107] of Byte; // will hold place name
      entry_temp_array: array [0 .. 7] of Byte; // will hold place temperature
      entry_name_array_length: Integer;
      entry_temp_array_length: Integer;
      entry_hash: Integer;
      entry_integer: Integer;
      entry_Unicode: String;
      entry_array_index: Integer; // index for entry_temp_array
      DigestedString: TArray<Byte>; // prefer not to use TArray, but for now...
      G: Cardinal;
      Hash: Cardinal;
      BTT: Integer; // temporary for bytes to temperature conversion
      HaveData: Boolean;

    begin
      while True do // main loop
      begin
        while True do // try for data from this stack
        begin
          // does this stack have data
          HaveData := False;
          if (DataStackCount1 = 0) then
          begin
            // check to see if we are done with all stacks
            if (DataStackCount1 + DataStackCount2 + DataStackCount3 +
              DataStackCount4 + DataStackCount5 = 0) then
            // might be done
            begin
              if (ReadFile_Done1 and ReadFile_Done2 and ReadFile_Done3) then
              // recheck to verify no more data
              begin
                // wait a moment to be sure
                Sleep(1);
                if (DataStackCount1 + DataStackCount2 + DataStackCount3 +
                  DataStackCount4 + DataStackCount5 = 0) then
                begin
                  HaveData := False;
                  ParseData_Done := True;
                  Break;
                end;
              end; // of: if (ReadFile_Done1 and ReadFile_Done2 and ReadFile_Done3) then
            end;
          end
          else // lock and check again
          begin
            DataStackLock1.Acquire;
            if (DataStackCount1 > 0) then // there is data
            begin
              Move(DataStack1[DataStackCount1 - 1].RawData[0], DataStackItem[0],
                SizeOf(DataStackItem));
              dec(DataStackCount1);
              DataStackLock1.Release;
              HaveData := True;
              Break;
            end;
            DataStackLock1.Release;
          end;

        end; // of: while True do // try for data from this stack

        if HaveData then
        begin
          // split raw data into name and temperature
          SC := False;
          entry_name_array_length := 0;
          entry_temp_array_length := 0;
          DataIndex := 0;
          while DataStackItem[DataIndex] <> 0 do
          begin
            if (DataStackItem[DataIndex] = 59) then
            begin
              SC := True;
            end
            else
            begin
              if SC then
              begin
                // skip decimal in number
                if Not(DataStackItem[DataIndex] = 46) then
                begin
                  entry_temp_array[entry_temp_array_length] :=
                    DataStackItem[DataIndex];
                  inc(entry_temp_array_length);
                end;
              end
              else // don't skip period in name
              begin
                entry_name_array[entry_name_array_length] :=
                  DataStackItem[DataIndex];
                inc(entry_name_array_length);
              end;
            end;
            inc(DataIndex);
          end;

          // convert characters in byte array to temperature
          if entry_temp_array[0] = 45 then
          // dash (-), so negative number
          begin
            // BTT :=  -(entry_temp_array[count -1] -48) - 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  48 - entry_temp_array[count -1] - 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  48 + 480 - entry_temp_array[count -1] - 10 * (entry_temp_array[count -2]);
            BTT := 528 - entry_temp_array[entry_temp_array_length - 1] - 10 *
              (entry_temp_array[entry_temp_array_length - 2]);
            if entry_temp_array_length = 4 then
            // do one more digit
            begin
              BTT := BTT - 100 * (entry_temp_array[1] - 48);
            end;
          end
          else
          begin
            // BTT :=  (entry_temp_array[count -1] -48) + 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  entry_temp_array[count -1] -48 + 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  entry_temp_array[count -1] -48 - 480 + 10 * (entry_temp_array[count -2]);
            BTT := -528 + (entry_temp_array[entry_temp_array_length - 1]) + 10 *
              (entry_temp_array[entry_temp_array_length - 2]);
            if entry_temp_array_length = 3 then
            // do one more digit
            begin
              BTT := BTT + 100 * (entry_temp_array[0] - 48);
            end;
          end;

          entry_integer := BTT;

          // hash name bytes, adapted from:
          (* ****************************************************************** *)
          (* Tomes of Delphi: Algorithms and Data Structures *)
          (* ------------------------------------------------------------------ *)
          (* function TDPJWHash *)
          (* ****************************************************************** *)
          // combine this with loop to setup for Unicode
          SetLength(DigestedString, entry_name_array_length);

          Hash := 0;
          for entry_array_index := 0 to entry_name_array_length - 1 do
          begin
            DigestedString[entry_array_index] := entry_name_array
              [entry_array_index];

            Hash := (Hash shl 4) + entry_name_array[entry_array_index];
            G := Hash and $F000000;
            if (G <> 0) then
              Hash := (Hash xor (G shr 24)) xor G;
          end;
          entry_hash := Hash mod 18701; // Prime = 18701

          entry_Unicode := TEncoding.UTF8.GetString(DigestedString);

          // send to appropriate hash table
          while True do
          // success is handled by breaking out of while loop
          begin
            HashRecordLock1.Acquire;
            if HashRecordMap1[entry_hash].DataCount = 0 then
            // this hash has not been used
            begin
              // store the first entry for this station
              HashRecordMap1[entry_hash].DataCount := 1;
              HashRecordMap1[entry_hash].DataSum := entry_integer;
              HashRecordMap1[entry_hash].DataMax := entry_integer;
              HashRecordMap1[entry_hash].DataMin := entry_integer;
              HashRecordMap1[entry_hash].DataName := entry_Unicode;
              HashRecordLock1.Release;
              Break;
            end
            else // hash has been used, check for collision
            begin
              if HashRecordMap1[entry_hash].DataName = entry_Unicode then
              // OK, merge data
              begin
                HashRecordMap1[entry_hash].DataCount :=
                  HashRecordMap1[entry_hash].DataCount + 1;
                HashRecordMap1[entry_hash].DataSum := HashRecordMap1[entry_hash]
                  .DataSum + entry_integer;
                if (HashRecordMap1[entry_hash].DataMax < entry_integer) then
                  HashRecordMap1[entry_hash].DataMax := entry_integer;
                if (HashRecordMap1[entry_hash].DataMin > entry_integer) then
                  HashRecordMap1[entry_hash].DataMin := entry_integer;
                HashRecordLock1.Release;
                Break;
              end
              else // collision, try next spot in HashRecordMap array
              begin
                inc(entry_hash);
                if entry_hash >= 18701 then
                  entry_hash := entry_hash - 18701;
              end;
            end;
          end; // of: while True do

        end; // of: if HaveData then

        if ParseData_Done then
        begin
          ParseData_Done := True;
          Break;
        end;

      end; // of:  while True do   // main loop

      Sleep(1);

    end);
  TabulateThread1.Start;
end;

procedure LaunchTabulateThread2;
var
  TabulateThread2: TThread;

begin
  TabulateThread2 := TThread.CreateAnonymousThread(
    procedure
    var
      DataStackItem: array [0 .. 107] of Byte;
      DataIndex: Integer; // index into raw data array
      SC: Boolean; // True after finding semi-colon
      entry_name_array: array [0 .. 107] of Byte; // will hold place name
      entry_temp_array: array [0 .. 7] of Byte; // will hold place temperature
      entry_name_array_length: Integer;
      entry_temp_array_length: Integer;
      entry_hash: Integer;
      entry_integer: Integer;
      entry_Unicode: String;
      entry_array_index: Integer; // index for entry_temp_array
      DigestedString: TArray<Byte>; // prefer not to use TArray, but for now...
      G: Cardinal;
      Hash: Cardinal;
      BTT: Integer; // temporary for bytes to temperature conversion
      HaveData: Boolean;

    begin
      while True do // main loop
      begin
        while True do // try for data from this stack
        begin
          // does this stack have data
          HaveData := False;
          if (DataStackCount2 = 0) then
          begin
            // check to see if we are done with all stacks
            if (DataStackCount1 + DataStackCount2 + DataStackCount3 +
              DataStackCount4 + DataStackCount5 = 0) then
            // might be done
            begin
              if (ReadFile_Done1 and ReadFile_Done2 and ReadFile_Done3) then
              // recheck to verify no more data
              begin
                // wait a moment to be sure
                Sleep(1);
                if (DataStackCount1 + DataStackCount2 + DataStackCount3 +
                  DataStackCount4 + DataStackCount5 = 0) then
                begin
                  HaveData := False;
                  ParseData_Done := True;
                  Break;
                end;
              end; // of: if (ReadFile_Done1 and ReadFile_Done2 and ReadFile_Done3) then
            end;
          end
          else // lock and check again
          begin
            DataStackLock2.Acquire;
            if (DataStackCount2 > 0) then // there is data
            begin
              Move(DataStack2[DataStackCount2 - 1].RawData[0], DataStackItem[0],
                SizeOf(DataStackItem));
              dec(DataStackCount2);
              DataStackLock2.Release;
              HaveData := True;
              Break;
            end;
            DataStackLock2.Release;
          end;

        end; // of: while True do // try for data from this stack

        if HaveData then
        begin
          // split raw data into name and temperature
          SC := False;
          entry_name_array_length := 0;
          entry_temp_array_length := 0;
          DataIndex := 0;
          while DataStackItem[DataIndex] <> 0 do
          begin
            if (DataStackItem[DataIndex] = 59) then
            begin
              SC := True;
            end
            else
            begin
              if SC then
              begin
                // skip decimal in number
                if Not(DataStackItem[DataIndex] = 46) then
                begin
                  entry_temp_array[entry_temp_array_length] :=
                    DataStackItem[DataIndex];
                  inc(entry_temp_array_length);
                end;
              end
              else // don't skip period in name
              begin
                entry_name_array[entry_name_array_length] :=
                  DataStackItem[DataIndex];
                inc(entry_name_array_length);
              end;
            end;
            inc(DataIndex);
          end;

          // convert characters in byte array to temperature
          if entry_temp_array[0] = 45 then
          // dash (-), so negative number
          begin
            // BTT :=  -(entry_temp_array[count -1] -48) - 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  48 - entry_temp_array[count -1] - 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  48 + 480 - entry_temp_array[count -1] - 10 * (entry_temp_array[count -2]);
            BTT := 528 - entry_temp_array[entry_temp_array_length - 1] - 10 *
              (entry_temp_array[entry_temp_array_length - 2]);
            if entry_temp_array_length = 4 then
            // do one more digit
            begin
              BTT := BTT - 100 * (entry_temp_array[1] - 48);
            end;
          end
          else
          begin
            // BTT :=  (entry_temp_array[count -1] -48) + 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  entry_temp_array[count -1] -48 + 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  entry_temp_array[count -1] -48 - 480 + 10 * (entry_temp_array[count -2]);
            BTT := -528 + (entry_temp_array[entry_temp_array_length - 1]) + 10 *
              (entry_temp_array[entry_temp_array_length - 2]);
            if entry_temp_array_length = 3 then
            // do one more digit
            begin
              BTT := BTT + 100 * (entry_temp_array[0] - 48);
            end;
          end;

          entry_integer := BTT;

          // hash name bytes, adapted from:
          (* ****************************************************************** *)
          (* Tomes of Delphi: Algorithms and Data Structures *)
          (* ------------------------------------------------------------------ *)
          (* function TDPJWHash *)
          (* ****************************************************************** *)
          // combine this with loop to setup for Unicode
          SetLength(DigestedString, entry_name_array_length);

          Hash := 0;
          for entry_array_index := 0 to entry_name_array_length - 1 do
          begin
            DigestedString[entry_array_index] := entry_name_array
              [entry_array_index];

            Hash := (Hash shl 4) + entry_name_array[entry_array_index];
            G := Hash and $F000000;
            if (G <> 0) then
              Hash := (Hash xor (G shr 24)) xor G;
          end;
          entry_hash := Hash mod 18701; // Prime = 18701

          entry_Unicode := TEncoding.UTF8.GetString(DigestedString);

          // send to appropriate hash table
          while True do
          // success is handled by breaking out of while loop
          begin
            HashRecordLock2.Acquire;
            if HashRecordMap2[entry_hash].DataCount = 0 then
            // this hash has not been used
            begin
              // store the first entry for this station
              HashRecordMap2[entry_hash].DataCount := 1;
              HashRecordMap2[entry_hash].DataSum := entry_integer;
              HashRecordMap2[entry_hash].DataMax := entry_integer;
              HashRecordMap2[entry_hash].DataMin := entry_integer;
              HashRecordMap2[entry_hash].DataName := entry_Unicode;
              HashRecordLock2.Release;
              Break;
            end
            else // hash has been used, check for collision
            begin
              if HashRecordMap2[entry_hash].DataName = entry_Unicode then
              // OK, merge data
              begin
                HashRecordMap2[entry_hash].DataCount :=
                  HashRecordMap2[entry_hash].DataCount + 1;
                HashRecordMap2[entry_hash].DataSum := HashRecordMap2[entry_hash]
                  .DataSum + entry_integer;
                if (HashRecordMap2[entry_hash].DataMax < entry_integer) then
                  HashRecordMap2[entry_hash].DataMax := entry_integer;
                if (HashRecordMap2[entry_hash].DataMin > entry_integer) then
                  HashRecordMap2[entry_hash].DataMin := entry_integer;
                HashRecordLock2.Release;
                Break;
              end
              else // collision, try next spot in HashRecordMap array
              begin
                inc(entry_hash);
                // for Prime = 18701
                if entry_hash >= 18701 then
                  entry_hash := entry_hash - 18701;
              end;
            end;
          end; // of: while True do

        end; // of: if HaveData then

        if ParseData_Done then
        begin
          ParseData_Done := True;
          Break;
        end;

      end; // of:  while True do   // main loop

      Sleep(1);

    end);
  TabulateThread2.Start;
end;

procedure LaunchTabulateThread3;
var
  TabulateThread3: TThread;

begin
  TabulateThread3 := TThread.CreateAnonymousThread(
    procedure
    var
      DataStackItem: array [0 .. 107] of Byte;
      DataIndex: Integer; // index into raw data array
      SC: Boolean; // True after finding semi-colon
      entry_name_array: array [0 .. 107] of Byte; // will hold place name
      entry_temp_array: array [0 .. 7] of Byte; // will hold place temperature
      entry_name_array_length: Integer;
      entry_temp_array_length: Integer;
      entry_hash: Integer;
      entry_integer: Integer;
      entry_Unicode: String;
      entry_array_index: Integer; // index for entry_temp_array
      DigestedString: TArray<Byte>; // prefer not to use TArray, but for now...
      G: Cardinal;
      Hash: Cardinal;
      BTT: Integer; // temporary for bytes to temperature conversion
      HaveData: Boolean;

    begin
      while True do // main loop
      begin
        while True do // try for data from this stack
        begin
          // does this stack have data
          HaveData := False;
          if (DataStackCount3 = 0) then
          begin
            // check to see if we are done with all stacks
            if (DataStackCount1 + DataStackCount2 + DataStackCount3 +
              DataStackCount4 + DataStackCount5 = 0) then
            // might be done
            begin
              if (ReadFile_Done1 and ReadFile_Done2 and ReadFile_Done3) then
              // recheck to verify no more data
              begin
                // wait a moment to be sure
                Sleep(1);
                if (DataStackCount1 + DataStackCount2 + DataStackCount3 +
                  DataStackCount4 + DataStackCount5 = 0) then
                begin
                  HaveData := False;
                  ParseData_Done := True;
                  Break;
                end;
              end; // of: if (ReadFile_Done1 and ReadFile_Done2 and ReadFile_Done3) then
            end;
          end
          else // lock and check again
          begin
            DataStackLock3.Acquire;
            if (DataStackCount3 > 0) then // there is data
            begin
              Move(DataStack3[DataStackCount3 - 1].RawData[0], DataStackItem[0],
                SizeOf(DataStackItem));
              dec(DataStackCount3);
              DataStackLock3.Release;
              HaveData := True;
              Break;
            end;
            DataStackLock3.Release;
          end;

        end; // of: while True do // try for data from this stack

        if HaveData then
        begin
          // split raw data into name and temperature
          SC := False;
          entry_name_array_length := 0;
          entry_temp_array_length := 0;
          DataIndex := 0;
          while DataStackItem[DataIndex] <> 0 do
          begin
            if (DataStackItem[DataIndex] = 59) then
            begin
              SC := True;
            end
            else
            begin
              if SC then
              begin
                // skip decimal in number
                if Not(DataStackItem[DataIndex] = 46) then
                begin
                  entry_temp_array[entry_temp_array_length] :=
                    DataStackItem[DataIndex];
                  inc(entry_temp_array_length);
                end;
              end
              else // don't skip period in name
              begin
                entry_name_array[entry_name_array_length] :=
                  DataStackItem[DataIndex];
                inc(entry_name_array_length);
              end;
            end;
            inc(DataIndex);
          end;

          // convert characters in byte array to temperature
          if entry_temp_array[0] = 45 then
          // dash (-), so negative number
          begin
            // BTT :=  -(entry_temp_array[count -1] -48) - 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  48 - entry_temp_array[count -1] - 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  48 + 480 - entry_temp_array[count -1] - 10 * (entry_temp_array[count -2]);
            BTT := 528 - entry_temp_array[entry_temp_array_length - 1] - 10 *
              (entry_temp_array[entry_temp_array_length - 2]);
            if entry_temp_array_length = 4 then
            // do one more digit
            begin
              BTT := BTT - 100 * (entry_temp_array[1] - 48);
            end;
          end
          else
          begin
            // BTT :=  (entry_temp_array[count -1] -48) + 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  entry_temp_array[count -1] -48 + 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  entry_temp_array[count -1] -48 - 480 + 10 * (entry_temp_array[count -2]);
            BTT := -528 + (entry_temp_array[entry_temp_array_length - 1]) + 10 *
              (entry_temp_array[entry_temp_array_length - 2]);
            if entry_temp_array_length = 3 then
            // do one more digit
            begin
              BTT := BTT + 100 * (entry_temp_array[0] - 48);
            end;
          end;

          entry_integer := BTT;

          // hash name bytes, adapted from:
          (* ****************************************************************** *)
          (* Tomes of Delphi: Algorithms and Data Structures *)
          (* ------------------------------------------------------------------ *)
          (* function TDPJWHash *)
          (* ****************************************************************** *)
          // combine this with loop to setup for Unicode
          SetLength(DigestedString, entry_name_array_length);

          Hash := 0;
          for entry_array_index := 0 to entry_name_array_length - 1 do
          begin
            DigestedString[entry_array_index] := entry_name_array
              [entry_array_index];

            Hash := (Hash shl 4) + entry_name_array[entry_array_index];
            G := Hash and $F000000;
            if (G <> 0) then
              Hash := (Hash xor (G shr 24)) xor G;
          end;
          entry_hash := Hash mod 18701; // Prime = 18701

          entry_Unicode := TEncoding.UTF8.GetString(DigestedString);

          // send to appropriate hash table
          while True do
          // success is handled by breaking out of while loop
          begin
            HashRecordLock3.Acquire;
            if HashRecordMap3[entry_hash].DataCount = 0 then
            // this hash has not been used
            begin
              // store the first entry for this station
              HashRecordMap3[entry_hash].DataCount := 1;
              HashRecordMap3[entry_hash].DataSum := entry_integer;
              HashRecordMap3[entry_hash].DataMax := entry_integer;
              HashRecordMap3[entry_hash].DataMin := entry_integer;
              HashRecordMap3[entry_hash].DataName := entry_Unicode;
              HashRecordLock3.Release;
              Break;
            end
            else // hash has been used, check for collision
            begin
              if HashRecordMap3[entry_hash].DataName = entry_Unicode then
              // OK, merge data
              begin
                HashRecordMap3[entry_hash].DataCount :=
                  HashRecordMap3[entry_hash].DataCount + 1;
                HashRecordMap3[entry_hash].DataSum := HashRecordMap3[entry_hash]
                  .DataSum + entry_integer;
                if (HashRecordMap3[entry_hash].DataMax < entry_integer) then
                  HashRecordMap3[entry_hash].DataMax := entry_integer;
                if (HashRecordMap3[entry_hash].DataMin > entry_integer) then
                  HashRecordMap3[entry_hash].DataMin := entry_integer;
                HashRecordLock3.Release;
                Break;
              end
              else // collision, try next spot in HashRecordMap array
              begin
                inc(entry_hash);
                // for Prime = 18701
                if entry_hash >= 18701 then
                  entry_hash := entry_hash - 18701;
              end;
            end;
          end; // of: while True do

        end; // of: if HaveData then

        if ParseData_Done then
        begin
          ParseData_Done := True;
          Break;
        end;

      end; // of:  while True do   // main loop

      Sleep(1);

    end);
  TabulateThread3.Start;
end;

// - - - - - - - - - - - - - - - - - - - - -

procedure LaunchTabulateThread4;
var
  TabulateThread4: TThread;

begin
  TabulateThread4 := TThread.CreateAnonymousThread(
    procedure
    var
      DataStackItem: array [0 .. 107] of Byte;
      DataIndex: Integer; // index into raw data array
      SC: Boolean; // True after finding semi-colon
      entry_name_array: array [0 .. 107] of Byte; // will hold place name
      entry_temp_array: array [0 .. 7] of Byte; // will hold place temperature
      entry_name_array_length: Integer;
      entry_temp_array_length: Integer;
      entry_hash: Integer;
      entry_integer: Integer;
      entry_Unicode: String;
      entry_array_index: Integer; // index for entry_temp_array
      DigestedString: TArray<Byte>; // prefer not to use TArray, but for now...
      G: Cardinal;
      Hash: Cardinal;
      BTT: Integer; // temporary for bytes to temperature conversion
      HaveData: Boolean;

    begin
      while True do // main loop
      begin
        while True do // try for data from this stack
        begin
          // does this stack have data
          HaveData := False;
          if (DataStackCount4 = 0) then
          begin
            // check to see if we are done with all stacks
            if (DataStackCount1 + DataStackCount2 + DataStackCount3 +
              DataStackCount4 + DataStackCount5 = 0) then
            // might be done
            begin
              if (ReadFile_Done1 and ReadFile_Done2 and ReadFile_Done3) then
              // recheck to verify no more data
              begin
                // wait a moment to be sure
                Sleep(1);
                if (DataStackCount1 + DataStackCount2 + DataStackCount3 +
                  DataStackCount4 + DataStackCount5 = 0) then
                begin
                  HaveData := False;
                  ParseData_Done := True;
                  Break;
                end;
              end; // of: if (ReadFile_Done1 and ReadFile_Done2 and ReadFile_Done3) then
            end;
          end
          else // lock and check again
          begin
            DataStackLock4.Acquire;
            if (DataStackCount4 > 0) then // there is data
            begin
              Move(DataStack4[DataStackCount4 - 1].RawData[0], DataStackItem[0],
                SizeOf(DataStackItem));
              dec(DataStackCount4);
              DataStackLock4.Release;
              HaveData := True;
              Break;
            end;
            DataStackLock4.Release;
          end;

        end; // of: while True do // try for data from this stack

        if HaveData then
        begin
          // split raw data into name and temperature
          SC := False;
          entry_name_array_length := 0;
          entry_temp_array_length := 0;
          DataIndex := 0;
          while DataStackItem[DataIndex] <> 0 do
          begin
            if (DataStackItem[DataIndex] = 59) then
            begin
              SC := True;
            end
            else
            begin
              if SC then
              begin
                // skip decimal in number
                if Not(DataStackItem[DataIndex] = 46) then
                begin
                  entry_temp_array[entry_temp_array_length] :=
                    DataStackItem[DataIndex];
                  inc(entry_temp_array_length);
                end;
              end
              else // don't skip period in name
              begin
                entry_name_array[entry_name_array_length] :=
                  DataStackItem[DataIndex];
                inc(entry_name_array_length);
              end;
            end;
            inc(DataIndex);
          end;

          // convert characters in byte array to temperature
          if entry_temp_array[0] = 45 then
          // dash (-), so negative number
          begin
            // BTT :=  -(entry_temp_array[count -1] -48) - 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  48 - entry_temp_array[count -1] - 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  48 + 480 - entry_temp_array[count -1] - 10 * (entry_temp_array[count -2]);
            BTT := 528 - entry_temp_array[entry_temp_array_length - 1] - 10 *
              (entry_temp_array[entry_temp_array_length - 2]);
            if entry_temp_array_length = 4 then
            // do one more digit
            begin
              BTT := BTT - 100 * (entry_temp_array[1] - 48);
            end;
          end
          else
          begin
            // BTT :=  (entry_temp_array[count -1] -48) + 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  entry_temp_array[count -1] -48 + 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  entry_temp_array[count -1] -48 - 480 + 10 * (entry_temp_array[count -2]);
            BTT := -528 + (entry_temp_array[entry_temp_array_length - 1]) + 10 *
              (entry_temp_array[entry_temp_array_length - 2]);
            if entry_temp_array_length = 3 then
            // do one more digit
            begin
              BTT := BTT + 100 * (entry_temp_array[0] - 48);
            end;
          end;

          entry_integer := BTT;

          // hash name bytes, adapted from:
          (* ****************************************************************** *)
          (* Tomes of Delphi: Algorithms and Data Structures *)
          (* ------------------------------------------------------------------ *)
          (* function TDPJWHash *)
          (* ****************************************************************** *)
          // combine this with loop to setup for Unicode
          SetLength(DigestedString, entry_name_array_length);

          Hash := 0;
          for entry_array_index := 0 to entry_name_array_length - 1 do
          begin
            DigestedString[entry_array_index] := entry_name_array
              [entry_array_index];

            Hash := (Hash shl 4) + entry_name_array[entry_array_index];
            G := Hash and $F000000;
            if (G <> 0) then
              Hash := (Hash xor (G shr 24)) xor G;
          end;
          entry_hash := Hash mod 18701; // Prime = 18701

          entry_Unicode := TEncoding.UTF8.GetString(DigestedString);

          // send to appropriate hash table
          while True do
          // success is handled by breaking out of while loop
          begin
            HashRecordLock4.Acquire;
            if HashRecordMap4[entry_hash].DataCount = 0 then
            // this hash has not been used
            begin
              // store the first entry for this station
              HashRecordMap4[entry_hash].DataCount := 1;
              HashRecordMap4[entry_hash].DataSum := entry_integer;
              HashRecordMap4[entry_hash].DataMax := entry_integer;
              HashRecordMap4[entry_hash].DataMin := entry_integer;
              HashRecordMap4[entry_hash].DataName := entry_Unicode;
              HashRecordLock4.Release;
              Break;
            end
            else // hash has been used, check for collision
            begin
              if HashRecordMap4[entry_hash].DataName = entry_Unicode then
              // OK, merge data
              begin
                HashRecordMap4[entry_hash].DataCount :=
                  HashRecordMap4[entry_hash].DataCount + 1;
                HashRecordMap4[entry_hash].DataSum := HashRecordMap4[entry_hash]
                  .DataSum + entry_integer;
                if (HashRecordMap4[entry_hash].DataMax < entry_integer) then
                  HashRecordMap4[entry_hash].DataMax := entry_integer;
                if (HashRecordMap4[entry_hash].DataMin > entry_integer) then
                  HashRecordMap4[entry_hash].DataMin := entry_integer;
                HashRecordLock4.Release;
                Break;
              end
              else // collision, try next spot in HashRecordMap array
              begin
                inc(entry_hash);
                // for Prime = 18701
                if entry_hash >= 18701 then
                  entry_hash := entry_hash - 18701;
              end;
            end;
          end; // of: while True do

        end; // of: if HaveData then

        if ParseData_Done then
        begin
          ParseData_Done := True;
          Break;
        end;

      end; // of:  while True do   // main loop

      Sleep(1);

    end);
  TabulateThread4.Start;
end;

procedure LaunchTabulateThread5;
var
  TabulateThread5: TThread;

begin
  TabulateThread5 := TThread.CreateAnonymousThread(
    procedure
    var
      DataStackItem: array [0 .. 107] of Byte;
      DataIndex: Integer; // index into raw data array
      SC: Boolean; // True after finding semi-colon
      entry_name_array: array [0 .. 107] of Byte; // will hold place name
      entry_temp_array: array [0 .. 7] of Byte; // will hold place temperature
      entry_name_array_length: Integer;
      entry_temp_array_length: Integer;
      entry_hash: Integer;
      entry_integer: Integer;
      entry_Unicode: String;
      entry_array_index: Integer; // index for entry_temp_array
      DigestedString: TArray<Byte>; // prefer not to use TArray, but for now...
      G: Cardinal;
      Hash: Cardinal;
      BTT: Integer; // temporary for bytes to temperature conversion
      HaveData: Boolean;

    begin
      while True do // main loop
      begin
        while True do // try for data from this stack
        begin
          // does this stack have data
          HaveData := False;
          if (DataStackCount5 = 0) then
          begin
            // check to see if we are done with all stacks
            if (DataStackCount1 + DataStackCount2 + DataStackCount3 +
              DataStackCount4 + DataStackCount5 = 0) then
            // might be done
            begin
              if (ReadFile_Done1 and ReadFile_Done2 and ReadFile_Done3) then
              // recheck to verify no more data
              begin
                // wait a moment to be sure
                Sleep(1);
                if (DataStackCount1 + DataStackCount2 + DataStackCount3 +
                  DataStackCount4 + DataStackCount5 = 0) then
                begin
                  HaveData := False;
                  ParseData_Done := True;
                  Break;
                end;
              end; // of: if (ReadFile_Done1 and ReadFile_Done2 and ReadFile_Done3) then
            end;
          end
          else // lock and check again
          begin
            DataStackLock5.Acquire;
            if (DataStackCount5 > 0) then // there is data
            begin
              Move(DataStack5[DataStackCount5 - 1].RawData[0], DataStackItem[0],
                SizeOf(DataStackItem));
              dec(DataStackCount5);
              DataStackLock5.Release;
              HaveData := True;
              Break;
            end;
            DataStackLock5.Release;
          end;

        end; // of: while True do // try for data from this stack

        if HaveData then
        begin
          // split raw data into name and temperature
          SC := False;
          entry_name_array_length := 0;
          entry_temp_array_length := 0;
          DataIndex := 0;
          while DataStackItem[DataIndex] <> 0 do
          begin
            if (DataStackItem[DataIndex] = 59) then
            begin
              SC := True;
            end
            else
            begin
              if SC then
              begin
                // skip decimal in number
                if Not(DataStackItem[DataIndex] = 46) then
                begin
                  entry_temp_array[entry_temp_array_length] :=
                    DataStackItem[DataIndex];
                  inc(entry_temp_array_length);
                end;
              end
              else // don't skip period in name
              begin
                entry_name_array[entry_name_array_length] :=
                  DataStackItem[DataIndex];
                inc(entry_name_array_length);
              end;
            end;
            inc(DataIndex);
          end;

          // convert characters in byte array to temperature
          if entry_temp_array[0] = 45 then
          // dash (-), so negative number
          begin
            // BTT :=  -(entry_temp_array[count -1] -48) - 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  48 - entry_temp_array[count -1] - 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  48 + 480 - entry_temp_array[count -1] - 10 * (entry_temp_array[count -2]);
            BTT := 528 - entry_temp_array[entry_temp_array_length - 1] - 10 *
              (entry_temp_array[entry_temp_array_length - 2]);
            if entry_temp_array_length = 4 then
            // do one more digit
            begin
              BTT := BTT - 100 * (entry_temp_array[1] - 48);
            end;
          end
          else
          begin
            // BTT :=  (entry_temp_array[count -1] -48) + 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  entry_temp_array[count -1] -48 + 10 * (entry_temp_array[count -2] - 48);
            // BTT :=  entry_temp_array[count -1] -48 - 480 + 10 * (entry_temp_array[count -2]);
            BTT := -528 + (entry_temp_array[entry_temp_array_length - 1]) + 10 *
              (entry_temp_array[entry_temp_array_length - 2]);
            if entry_temp_array_length = 3 then
            // do one more digit
            begin
              BTT := BTT + 100 * (entry_temp_array[0] - 48);
            end;
          end;

          entry_integer := BTT;

          // hash name bytes, adapted from:
          (* ****************************************************************** *)
          (* Tomes of Delphi: Algorithms and Data Structures *)
          (* ------------------------------------------------------------------ *)
          (* function TDPJWHash *)
          (* ****************************************************************** *)
          // combine this with loop to setup for Unicode
          SetLength(DigestedString, entry_name_array_length);

          Hash := 0;
          for entry_array_index := 0 to entry_name_array_length - 1 do
          begin
            DigestedString[entry_array_index] := entry_name_array
              [entry_array_index];

            Hash := (Hash shl 4) + entry_name_array[entry_array_index];
            G := Hash and $F000000;
            if (G <> 0) then
              Hash := (Hash xor (G shr 24)) xor G;
          end;
          entry_hash := Hash mod 18701; // Prime = 18701

          entry_Unicode := TEncoding.UTF8.GetString(DigestedString);

          // send to appropriate hash table
          while True do
          // success is handled by breaking out of while loop
          begin
            HashRecordLock5.Acquire;
            if HashRecordMap5[entry_hash].DataCount = 0 then
            // this hash has not been used
            begin
              // store the first entry for this station
              HashRecordMap5[entry_hash].DataCount := 1;
              HashRecordMap5[entry_hash].DataSum := entry_integer;
              HashRecordMap5[entry_hash].DataMax := entry_integer;
              HashRecordMap5[entry_hash].DataMin := entry_integer;
              HashRecordMap5[entry_hash].DataName := entry_Unicode;
              HashRecordLock5.Release;
              Break;
            end
            else // hash has been used, check for collision
            begin
              if HashRecordMap5[entry_hash].DataName = entry_Unicode then
              // OK, merge data
              begin
                HashRecordMap5[entry_hash].DataCount :=
                  HashRecordMap5[entry_hash].DataCount + 1;
                HashRecordMap5[entry_hash].DataSum := HashRecordMap5[entry_hash]
                  .DataSum + entry_integer;
                if (HashRecordMap5[entry_hash].DataMax < entry_integer) then
                  HashRecordMap5[entry_hash].DataMax := entry_integer;
                if (HashRecordMap5[entry_hash].DataMin > entry_integer) then
                  HashRecordMap5[entry_hash].DataMin := entry_integer;
                HashRecordLock5.Release;
                Break;
              end
              else // collision, try next spot in HashRecordMap array
              begin
                inc(entry_hash);
                if entry_hash >= 18701 then
                  entry_hash := entry_hash - 18701;
              end;
            end;
          end; // of: while True do

        end; // of: if HaveData then

        if ParseData_Done then
        begin
          ParseData_Done := True;
          Break;
        end;

      end; // of:  while True do   // main loop

      Sleep(1);

    end);
  TabulateThread5.Start;
end;

end.
