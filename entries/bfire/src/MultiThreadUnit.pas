unit MultiThreadUnit;

interface

uses
  WinApi.Windows,
  System.Diagnostics,
  System.Threading,
  System.SyncObjs,
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.Generics.Collections;

type
  // size entry_array to hold longest (by byte count) name
  // Dolores Hidalgo Cuna de la Independencia Nacional = 98 bytes
  // with temperature as large as: -999.9  (maybe just -99.9 ?)
  // so, add a bit, and use...
  // entry_array: array [0 .. 127] of Byte; // will hold place name and temp
  TRawRecord = record
    RawData: array [0 .. 127] of Byte; // bytes for each entry line
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

  ReadBufferSize: Integer = 32768; // 16384 OK; 32768 seems optimum

  // DataBufferSize (combined) max is about 100000000 for 10 GB free
  DataBufferSize1: Integer = 1000001;
  DataBufferSize2: Integer = 1000001;
  DataBufferSize3: Integer = 1000001;
  DataBufferSize4: Integer = 1000001;
  DataBufferCushion: Integer = 100000;

  // split points for stacks
  // ABCDE FGHIJK LMNOPQR STUVWXYZ (and extended)
  SplitPoint1: Byte = 68; // letter "D"
  SplitPoint2: Byte = 76; // letter "L"
  SplitPoint3: Byte = 82; // letter "R"

var
  inputFilename: String;
  outputFilename: String;
  iBytesRead: Integer;
  Challenge: TFileStream;
  ReadBuffer: PByte; // read file into this buffer

  // set up for 41351 places (actually seems to be 41343 entries)
  // split into four roughly equal stack to ho to separate hash tables
  HashRecordMap1: array [0 .. 18700] of THashRecord; // for Prime := 18701
  HashRecordMap2: array [0 .. 18700] of THashRecord; // for Prime := 18701
  HashRecordMap3: array [0 .. 18700] of THashRecord; // for Prime := 18701
  HashRecordMap4: array [0 .. 18700] of THashRecord; // for Prime := 18701

  // DataBufferSize: Integer = 1000001;
  // I think this fits in about 4 GB
  DataStack1: array [0 .. 1000000] of TRawRecord;
  DataStack2: array [0 .. 1000000] of TRawRecord;
  DataStack3: array [0 .. 1000000] of TRawRecord;
  DataStack4: array [0 .. 1000000] of TRawRecord;

  DataStackCount1: Integer;
  DataStackCount2: Integer;
  DataStackCount3: Integer;
  DataStackCount4: Integer;

  DataStackLock1: TCriticalSection;
  DataStackLock2: TCriticalSection;
  DataStackLock3: TCriticalSection;
  DataStackLock4: TCriticalSection;

  StationsForSort: TStringList;

  LineCount: Int64;

  ReadFile_Done: Boolean;
  ParseDataQ_Done1: Boolean;
  ParseDataQ_Done2: Boolean;
  ParseDataQ_Done3: Boolean;
  ParseDataQ_Done4: Boolean;

  StackMax1: Integer;  // sample stack count for peak value (approx.)
  StackMax2: Integer;
  StackMax3: Integer;
  StackMax4: Integer;


procedure FileToArrays(inputFilename: String; UseStdOut: Boolean); // read
procedure LaunchReadingThread(inputFilename: String);
procedure LaunchTabulateThread1;
procedure LaunchTabulateThread2;
procedure LaunchTabulateThread3;
procedure LaunchTabulateThread4;

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
    if UseStdOut then // send to STDOUT, where it gets mangled
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
  ReadBuffer := System.AllocMem(ReadBufferSize);

  DataStackCount1 := 0;
  DataStackCount2 := 0;
  DataStackCount3 := 0;
  DataStackCount4 := 0;

  // initialize a few things
  ReadFile_Done := False;
  ParseDataQ_Done1 := False;
  ParseDataQ_Done2 := False;
  ParseDataQ_Done3 := False;
  ParseDataQ_Done4 := False;

  DataStackLock1 := TCriticalSection.Create;
  DataStackLock2 := TCriticalSection.Create;
  DataStackLock3 := TCriticalSection.Create;
  DataStackLock4 := TCriticalSection.Create;

  StackMax1 := 0;
  StackMax2 := 0;
  StackMax3 := 0;
  StackMax4 := 0;

  // pre-fill arrays
  for i := 0 to Prime - 1 do
  begin
    HashRecordMap1[i].DataCount := 0;
    HashRecordMap2[i].DataCount := 0;
    HashRecordMap3[i].DataCount := 0;
    HashRecordMap4[i].DataCount := 0;
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
    LaunchTabulateThread1;
    LaunchTabulateThread2;
    LaunchTabulateThread3;
    LaunchTabulateThread4;
    LaunchReadingThread(inputFilename);
  end
  else
  begin
    raise Exception.Create(Format('File "%s" not found.', [inputFilename]));
  end;

end;


procedure LaunchReadingThread(inputFilename: String);
var
  ReadingThread: TThread;
  DataStackItem: TRawRecord;

begin
  ReadingThread := TThread.CreateAnonymousThread(
    procedure
    var
      BufferIndex: Integer; // index into read buffer
      DataIndex: Integer; // index into raw data array
      Choice: Byte;
      TimeOut: Integer;

    begin

      try
        Challenge := TFileStream.Create(inputFilename, fmOpenRead);
        Challenge.Seek(0, soFromBeginning);
        LineCount := 0;
        DataIndex := 0;

        iBytesRead := Challenge.Read(ReadBuffer^, ReadBufferSize);
        while iBytesRead > 0 do
        begin
          BufferIndex := 0;
          while BufferIndex < iBytesRead do
          begin
            if ((ReadBuffer + BufferIndex)^ = 10) then // line feed
            begin
              // and done collecting bytes, move to one of the stacks

              // send to the appropriate stack
              // ABCD EFGHIJKL MNOPQR STUVWXYZ (and extended)
              // SplitPoint1: Byte = 68; // letter "D"
              // SplitPoint2: Byte = 76; // letter "L"
              // SplitPoint3: Byte = 82; // letter "R"

              Choice := DataStackItem.RawData[0];

              if Choice <= SplitPoint2 then // A thru K
              begin
                if Choice <= SplitPoint1 then // A thru E
                begin
                  while True do // break on success
                    if (DataStackCount1 < DataBufferSize1 - 1) then
                    // there is room
                    begin
                      DataStackLock1.Acquire;
                      DataStack1[DataStackCount1] := DataStackItem;
                      inc(DataStackCount1);
                      DataStackLock1.Release;
                      Break;
                    end
                    else // need to waste a few cycles
                    begin
                      while (DataStackCount1 > DataBufferSize1 -
                        DataBufferCushion) do
                      begin
                        // nothing real
                        TimeOut := DataStackCount1 mod 16001;
                      end;
                    end;
                end
                else // F thru K
                begin
                  while True do // break on success
                    if (DataStackCount2 < DataBufferSize2 - 1) then
                    // there is room
                    begin
                      DataStackLock2.Acquire;
                      DataStack2[DataStackCount2] := DataStackItem;
                      inc(DataStackCount2);
                      DataStackLock2.Release;
                      Break;
                    end
                    else // need to waste a few cycles
                    begin
                      while (DataStackCount2 > DataBufferSize2 -
                        DataBufferCushion) do
                      begin
                        // nothing real
                        TimeOut := DataStackCount2 mod 16001;
                      end;
                    end;
                end;
              end
              else // K and beyond
              begin
                if Choice <= SplitPoint3 then // L thru R
                begin
                  while True do // break on success
                    if (DataStackCount3 < DataBufferSize3 - 1) then
                    // there is room
                    begin
                      DataStackLock3.Acquire;
                      DataStack3[DataStackCount3] := DataStackItem;
                      inc(DataStackCount3);
                      DataStackLock3.Release;
                      Break;
                    end
                    else // need to waste a few cycles
                    begin
                      while (DataStackCount3 > DataBufferSize3 -
                        DataBufferCushion) do
                      begin
                        // nothing real
                        TimeOut := DataStackCount3 mod 16001;
                      end;
                    end;
                end
                else // S and beyond
                begin
                  while True do // break on success
                    if (DataStackCount4 < DataBufferSize4 - 1) then
                    // there is room
                    begin
                      DataStackLock4.Acquire;
                      DataStack4[DataStackCount4] := DataStackItem;
                      inc(DataStackCount4);
                      DataStackLock4.Release;
                      Break;
                    end
                    else // need to waste a few cycles
                    begin
                      while (DataStackCount4 > DataBufferSize4 -
                        DataBufferCushion) do
                      begin
                        // nothing real
                        TimeOut := DataStackCount4 mod 16001;
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
              DataStackItem.RawData[DataIndex] := (ReadBuffer + BufferIndex)^;
              inc(DataIndex);
            end;
            inc(BufferIndex);

          end; // of: while i < iBytesRead do
          iBytesRead := Challenge.Read(ReadBuffer^, ReadBufferSize);

        end; // of while iBytesRead > 0 do   --- implies end of file

        ReadFile_Done := True;

      finally
        Challenge.Free;
        // nothing here
      end;
    end);
  ReadingThread.Start;
end;

procedure LaunchTabulateThread1;
var
  TabulateThread1: TThread;
  DataStackItem1: TRawRecord;

begin
  TabulateThread1 := TThread.CreateAnonymousThread(
    procedure
    var
      DataIndex: Integer; // index into raw data array
      SC: Boolean; // True after finding semi-colon
      entry_name_array: array [0 .. 127] of Byte; // will hold place name
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

    begin
      while True do
      begin

        // get item from stack
        while True do // break on success
        begin
          if (DataStackCount1 > 0) then // there is data
          begin
            DataStackLock1.Acquire;
            DataStackItem1 := DataStack1[DataStackCount1 - 1];
            dec(DataStackCount1);
            DataStackLock1.Release;
            Break;
          end
          else
          begin
            if ReadFile_Done then // no more data
            begin
              ParseDataQ_Done1 := True;
              Break;
            end;
          end;
        end;

        if ParseDataQ_Done1 then // escape
        begin
          Break;
        end
        else // tabulate
        begin
          // split raw data into name and temperature
          SC := False;
          entry_name_array_length := 0;
          entry_temp_array_length := 0;
          for DataIndex := 0 to SizeOf(DataStackItem1.RawData) - 1 do
          begin
            if (DataStackItem1.RawData[DataIndex] = 59) then
            begin
              SC := True;
            end
            else
            begin
              if SC then
              begin
                // skip decimal in number
                if Not(DataStackItem1.RawData[DataIndex] = 46) then
                begin
                  // skip trailing CR
                  if (DataStackItem1.RawData[DataIndex] = 13) then // done
                  begin
                    Break;
                  end
                  else
                  begin
                    entry_temp_array[entry_temp_array_length] :=
                      DataStackItem1.RawData[DataIndex];
                    inc(entry_temp_array_length);
                  end;
                end;
              end
              else // don't skip period in name
              begin
                entry_name_array[entry_name_array_length] :=
                  DataStackItem1.RawData[DataIndex];
                inc(entry_name_array_length);
              end;
            end;
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

          // convert name bytes to Unicode
          // entry_name_array_length := Length(entry_name_array);
          SetLength(DigestedString, entry_name_array_length);
          for entry_array_index := 0 to entry_name_array_length - 1 do
          begin
            DigestedString[entry_array_index] := entry_name_array
              [entry_array_index];
          end;

          entry_Unicode := TEncoding.UTF8.GetString(DigestedString);

          // hash name bytes, adapted from:
          (* ****************************************************************** *)
          (* Tomes of Delphi: Algorithms and Data Structures *)
          (* ------------------------------------------------------------------ *)
          (* function TDPJWHash *)
          (* ****************************************************************** *)
          Hash := 0;
          for entry_array_index := 0 to entry_name_array_length - 1 do
          begin
            Hash := (Hash shl 4) + entry_name_array[entry_array_index];
            G := Hash and $F000000;
            if (G <> 0) then
              Hash := (Hash xor (G shr 24)) xor G;
          end;
          entry_hash := Hash mod 18701; // Prime = 18701

          while True do // success is handled by breaking out of while loop
          begin
            if HashRecordMap1[entry_hash].DataCount = 0 then
            // this hash has not been used
            begin
              // store the first entry for this station
              HashRecordMap1[entry_hash].DataCount := 1;
              HashRecordMap1[entry_hash].DataSum := entry_integer;
              HashRecordMap1[entry_hash].DataMax := entry_integer;
              HashRecordMap1[entry_hash].DataMin := entry_integer;
              HashRecordMap1[entry_hash].DataName := entry_Unicode;
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
                Break;
              end
              else // collision, try next spot in HashRecordMap array
              begin
                entry_hash := entry_hash + 19; // try a jump

                // for Prime = 18701
                if entry_hash >= 18701 then
                  entry_hash := entry_hash - 18701;
              end;
            end;
          end; // of: while True do

        end; // of: if DigestedData = nil then

      end; // of: while True do

    end);
  TabulateThread1.Start;
end;

procedure LaunchTabulateThread2;
var
  TabulateThread2: TThread;
  DataStackItem2: TRawRecord;

begin
  TabulateThread2 := TThread.CreateAnonymousThread(
    procedure
    var
      DataIndex: Integer; // index into raw data array
      SC: Boolean; // True after finding semi-colon
      entry_name_array: array [0 .. 127] of Byte; // will hold place name
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

    begin
      while True do
      begin

        // get item from stack
        while True do // break on success
        begin
          if (DataStackCount2 > 0) then // there is data
          begin
            DataStackLock2.Acquire;
            DataStackItem2 := DataStack2[DataStackCount2 - 1];
            dec(DataStackCount2);
            DataStackLock2.Release;
            Break;
          end
          else
          begin
            if ReadFile_Done then // no more data
            begin
              ParseDataQ_Done2 := True;
              Break;
            end;
          end;
        end;

        if ParseDataQ_Done2 then // escape
        begin
          Break;
        end
        else // tabulate
        begin
          // split raw data into name and temperature
          SC := False;
          entry_name_array_length := 0;
          entry_temp_array_length := 0;
          for DataIndex := 0 to SizeOf(DataStackItem2.RawData) - 1 do
          begin
            if (DataStackItem2.RawData[DataIndex] = 59) then
            begin
              SC := True;
            end
            else
            begin
              if SC then
              begin
                // skip decimal in number
                if Not(DataStackItem2.RawData[DataIndex] = 46) then
                begin
                  // skip trailing CR
                  if (DataStackItem2.RawData[DataIndex] = 13) then // done
                  begin
                    Break;
                  end
                  else
                  begin
                    entry_temp_array[entry_temp_array_length] :=
                      DataStackItem2.RawData[DataIndex];
                    inc(entry_temp_array_length);
                  end;
                end;
              end
              else // don't skip period in name
              begin
                entry_name_array[entry_name_array_length] :=
                  DataStackItem2.RawData[DataIndex];
                inc(entry_name_array_length);
              end;
            end;
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

          // convert name bytes to Unicode
          // entry_name_array_length := Length(entry_name_array);
          SetLength(DigestedString, entry_name_array_length);
          for entry_array_index := 0 to entry_name_array_length - 1 do
          begin
            DigestedString[entry_array_index] := entry_name_array
              [entry_array_index];
          end;

          entry_Unicode := TEncoding.UTF8.GetString(DigestedString);

          // hash name bytes, adapted from:
          (* ****************************************************************** *)
          (* Tomes of Delphi: Algorithms and Data Structures *)
          (* ------------------------------------------------------------------ *)
          (* function TDPJWHash *)
          (* ****************************************************************** *)
          Hash := 0;
          for entry_array_index := 0 to entry_name_array_length - 1 do
          begin
            Hash := (Hash shl 4) + entry_name_array[entry_array_index];
            G := Hash and $F000000;
            if (G <> 0) then
              Hash := (Hash xor (G shr 24)) xor G;
          end;
          entry_hash := Hash mod 18701; // Prime = 18701

          while True do // success is handled by breaking out of while loop
          begin
            if HashRecordMap2[entry_hash].DataCount = 0 then
            // this hash has not been used
            begin
              // store the first entry for this station
              HashRecordMap2[entry_hash].DataCount := 1;
              HashRecordMap2[entry_hash].DataSum := entry_integer;
              HashRecordMap2[entry_hash].DataMax := entry_integer;
              HashRecordMap2[entry_hash].DataMin := entry_integer;
              HashRecordMap2[entry_hash].DataName := entry_Unicode;
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
                Break;
              end
              else // collision, try next spot in HashRecordMap array
              begin
                entry_hash := entry_hash + 19; // try a jump

                // for Prime = 18701
                if entry_hash >= 18701 then
                  entry_hash := entry_hash - 18701;
              end;
            end;
          end; // of: while True do

        end; // of: if DigestedData = nil then

      end; // of: while True do

    end);
  TabulateThread2.Start;
end;

procedure LaunchTabulateThread3;
var
  TabulateThread3: TThread;
  DataStackItem3: TRawRecord;

begin
  TabulateThread3 := TThread.CreateAnonymousThread(
    procedure
    var
      DataIndex: Integer; // index into raw data array
      SC: Boolean; // True after finding semi-colon
      entry_name_array: array [0 .. 127] of Byte; // will hold place name
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

    begin
      while True do
      begin

        // get item from stack
        while True do // break on success
        begin
          if (DataStackCount3 > 0) then // there is data
          begin
            DataStackLock3.Acquire;
            DataStackItem3 := DataStack3[DataStackCount3 - 1];
            dec(DataStackCount3);
            DataStackLock3.Release;
            Break;
          end
          else
          begin
            if ReadFile_Done then // no more data
            begin
              ParseDataQ_Done3 := True;
              Break;
            end;
          end;
        end;

        if ParseDataQ_Done3 then // escape
        begin
          Break;
        end
        else // tabulate
        begin
          // split raw data into name and temperature
          SC := False;
          entry_name_array_length := 0;
          entry_temp_array_length := 0;
          for DataIndex := 0 to SizeOf(DataStackItem3.RawData) - 1 do
          begin
            if (DataStackItem3.RawData[DataIndex] = 59) then
            begin
              SC := True;
            end
            else
            begin
              if SC then
              begin
                // skip decimal in number
                if Not(DataStackItem3.RawData[DataIndex] = 46) then
                begin
                  // skip trailing CR
                  if (DataStackItem3.RawData[DataIndex] = 13) then // done
                  begin
                    Break;
                  end
                  else
                  begin
                    entry_temp_array[entry_temp_array_length] :=
                      DataStackItem3.RawData[DataIndex];
                    inc(entry_temp_array_length);
                  end;
                end;
              end
              else // don't skip period in name
              begin
                entry_name_array[entry_name_array_length] :=
                  DataStackItem3.RawData[DataIndex];
                inc(entry_name_array_length);
              end;
            end;
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

          // convert name bytes to Unicode
          // entry_name_array_length := Length(entry_name_array);
          SetLength(DigestedString, entry_name_array_length);
          for entry_array_index := 0 to entry_name_array_length - 1 do
          begin
            DigestedString[entry_array_index] := entry_name_array
              [entry_array_index];
          end;

          entry_Unicode := TEncoding.UTF8.GetString(DigestedString);

          // hash name bytes, adapted from:
          (* ****************************************************************** *)
          (* Tomes of Delphi: Algorithms and Data Structures *)
          (* ------------------------------------------------------------------ *)
          (* function TDPJWHash *)
          (* ****************************************************************** *)
          Hash := 0;
          for entry_array_index := 0 to entry_name_array_length - 1 do
          begin
            Hash := (Hash shl 4) + entry_name_array[entry_array_index];
            G := Hash and $F000000;
            if (G <> 0) then
              Hash := (Hash xor (G shr 24)) xor G;
          end;
          entry_hash := Hash mod 18701; // Prime = 18701

          while True do // success is handled by breaking out of while loop
          begin
            if HashRecordMap3[entry_hash].DataCount = 0 then
            // this hash has not been used
            begin
              // store the first entry for this station
              HashRecordMap3[entry_hash].DataCount := 1;
              HashRecordMap3[entry_hash].DataSum := entry_integer;
              HashRecordMap3[entry_hash].DataMax := entry_integer;
              HashRecordMap3[entry_hash].DataMin := entry_integer;
              HashRecordMap3[entry_hash].DataName := entry_Unicode;
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
                Break;
              end
              else // collision, try next spot in HashRecordMap array
              begin
                entry_hash := entry_hash + 19; // try a jump

                // for Prime = 18701
                if entry_hash >= 18701 then
                  entry_hash := entry_hash - 18701;
              end;
            end;
          end; // of: while True do

        end; // of: if DigestedData = nil then

      end; // of: while True do

    end);
  TabulateThread3.Start;
end;

// - - - - - - - - - - - - - - - - - - - - -

procedure LaunchTabulateThread4;
var
  TabulateThread4: TThread;
  DataStackItem4: TRawRecord;

begin
  TabulateThread4 := TThread.CreateAnonymousThread(
    procedure
    var
      DataIndex: Integer; // index into raw data array
      SC: Boolean; // True after finding semi-colon
      entry_name_array: array [0 .. 127] of Byte; // will hold place name
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

    begin
      while True do
      begin

        // get item from stack
        while True do // break on success
        begin
          if (DataStackCount4 > 0) then // there is data
          begin
            DataStackLock4.Acquire;
            DataStackItem4 := DataStack4[DataStackCount4 - 1];
            dec(DataStackCount4);
            DataStackLock4.Release;
            Break;
          end
          else
          begin
            if ReadFile_Done then // no more data
            begin
              ParseDataQ_Done4 := True;
              Break;
            end;
          end;
        end;

        if ParseDataQ_Done4 then // escape
        begin
          Break;
        end
        else // tabulate
        begin
          // split raw data into name and temperature
          SC := False;
          entry_name_array_length := 0;
          entry_temp_array_length := 0;
          for DataIndex := 0 to SizeOf(DataStackItem4.RawData) - 1 do
          begin
            if (DataStackItem4.RawData[DataIndex] = 59) then
            begin
              SC := True;
            end
            else
            begin
              if SC then
              begin
                // skip decimal in number
                if Not(DataStackItem4.RawData[DataIndex] = 46) then
                begin
                  // skip trailing CR
                  if (DataStackItem4.RawData[DataIndex] = 13) then // done
                  begin
                    Break;
                  end
                  else
                  begin
                    entry_temp_array[entry_temp_array_length] :=
                      DataStackItem4.RawData[DataIndex];
                    inc(entry_temp_array_length);
                  end;
                end;
              end
              else // don't skip period in name
              begin
                entry_name_array[entry_name_array_length] :=
                  DataStackItem4.RawData[DataIndex];
                inc(entry_name_array_length);
              end;
            end;
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

          // convert name bytes to Unicode
          // entry_name_array_length := Length(entry_name_array);
          SetLength(DigestedString, entry_name_array_length);
          for entry_array_index := 0 to entry_name_array_length - 1 do
          begin
            DigestedString[entry_array_index] := entry_name_array
              [entry_array_index];
          end;

          entry_Unicode := TEncoding.UTF8.GetString(DigestedString);

          // hash name bytes, adapted from:
          (* ****************************************************************** *)
          (* Tomes of Delphi: Algorithms and Data Structures *)
          (* ------------------------------------------------------------------ *)
          (* function TDPJWHash *)
          (* ****************************************************************** *)
          Hash := 0;
          for entry_array_index := 0 to entry_name_array_length - 1 do
          begin
            Hash := (Hash shl 4) + entry_name_array[entry_array_index];
            G := Hash and $F000000;
            if (G <> 0) then
              Hash := (Hash xor (G shr 24)) xor G;
          end;
          entry_hash := Hash mod 18701; // Prime = 18701

          while True do // success is handled by breaking out of while loop
          begin
            if HashRecordMap4[entry_hash].DataCount = 0 then
            // this hash has not been used
            begin
              // store the first entry for this station
              HashRecordMap4[entry_hash].DataCount := 1;
              HashRecordMap4[entry_hash].DataSum := entry_integer;
              HashRecordMap4[entry_hash].DataMax := entry_integer;
              HashRecordMap4[entry_hash].DataMin := entry_integer;
              HashRecordMap4[entry_hash].DataName := entry_Unicode;
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
                Break;
              end
              else // collision, try next spot in HashRecordMap array
              begin
                entry_hash := entry_hash + 19; // try a jump

                // for Prime = 18701
                if entry_hash >= 18701 then
                  entry_hash := entry_hash - 18701;
              end;
            end;
          end; // of: while True do

        end; // of: if DigestedData = nil then

      end; // of: while True do

    end);
  TabulateThread4.Start;
end;

end.
