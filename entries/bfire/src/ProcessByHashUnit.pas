unit ProcessByHashUnit;

interface

uses
  System.SysUtils,
  System.StrUtils,
  Classes,
  Math,
  System.Generics.Collections;

type
  TStationRecord = record // each array item is compiled data for a place
    DataSum: Integer; // temperature times ten
    DataCount: Integer; // number of temperature measurements
    DataMax: SmallInt; // temperature times ten
    DataMin: SmallInt; // temperature times ten
  end;

  TNameRecord = record // Station name and index to Station Data record
    sName: WideString;
    sIndex: Integer;
  end;

  TStationForSortClass = class(TObject)
  public
    DataSum: Integer;
    DataCount: Integer;
    DataMax: Integer;
    DataMin: Integer;
  end;

  TStationForSort = class of TStationForSortClass;

const
  // see http://compoasso.free.fr/primelistweb/page/prime/liste_online_en.php
  Prime: Integer = 75013;

const
  myBufferSize: Integer = 16384; // 32768 - 6 sec; 16384 -> 6 sec; 8192 -> 7 sec

var
  // for the challenge, set up for 41351 places (41343 entries, plus spare space)
  StationData: array [0 .. 41350] of TStationRecord;
  // not sorted, indexed with PlaceIndex array
  StationName: array [0 .. 41350] of TNameRecord;
  // not sorted, indexed with PlaceIndex array

  HashMap: array [0 .. 75012] of Integer; // for Prime := 75013

  StationsForSort: TStringList;

  start: TDateTime; // for timing

  PlaceCount: Integer;

  // static arrays
  // size entry_name_array to hold longest (by byte count) name
  // Dolores Hidalgo Cuna de la Independencia Nacional = 98 bytes
  // size entry_temp_array to hold -9999
  entry_name_array: array [0 .. 255] of Byte; // will hold place name
  entry_temp_array: array [0 .. 15] of Byte; // will hold place temperature
  entry_name_array_length: Integer;
  entry_temp_array_length: Integer;

  // prepare empty arrays to initialize before each entry read
  empty_entry_name_array: array [0 .. 255] of Byte;
  empty_entry_temp_array: array [0 .. 15] of Byte;

procedure FileToArrays(inFile: String; UseStdOut: Boolean);
procedure HashAndSave;
function BytesToTemp: Integer;
function StaticBytesToString: String;
function BytesToIndexHash: Integer;
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

function BytesToTemp: Integer;
// convert entry_chars_temp to signed integer, '0' is ascii 48 decimal
// note: we always have at least two bytes
// temperatures range from -99.9 to 99.8
// that appears here as -999 to 998
var
  BTT: Integer;
begin
  if entry_temp_array[0] = 45 then // negative number
  begin
    // BTT :=  -(entry_temp_array[count -1] -48) - 10 * (entry_temp_array[count -2] - 48);
    // BTT :=  48 - entry_temp_array[count -1] - 10 * (entry_temp_array[count -2] - 48);
    // BTT :=  48 + 480 - entry_temp_array[count -1] - 10 * (entry_temp_array[count -2]);
    BTT := 528 - entry_temp_array[entry_temp_array_length - 1] - 10 *
      (entry_temp_array[entry_temp_array_length - 2]);
    if entry_temp_array_length = 4 then // do one more digit
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
    if entry_temp_array_length = 3 then // do one more digit
    begin
      BTT := BTT + 100 * (entry_temp_array[0] - 48);
    end;
  end;
  BytesToTemp := BTT;
end;

// adapted from:
(* ****************************************************************** *)
(* Tomes of Delphi: Algorithms and Data Structures *)
(* ------------------------------------------------------------------ *)
(* function TDPJWHash *)
(* ****************************************************************** *)
function BytesToIndexHash;
// convert entry_chars to hash
// use   Prime: Integer = 75013;
var
  G: Integer;
  i: Integer;
  Hash: Integer;
begin
  Hash := 0;
  for i := 0 to entry_name_array_length - 1 do
  begin
    Hash := (Hash shl 4) + entry_name_array[i];
    G := Hash and $F0000000;
    if (G <> 0) then
      Hash := (Hash xor (G shr 24)) xor G;
  end;
  BytesToIndexHash := Hash mod Prime;
end;

// ============================================================================
// ============================================================================
// ============================================================================

procedure InitializeThings; inline;
var
  i: Integer;
begin
  // pre-fill arrays
  for i := 0 to Prime - 1 do
    HashMap[i] := -1;
  for i := 0 to 255 do
    empty_entry_name_array[i] := 0;
  for i := 0 to 15 do
    empty_entry_temp_array[i] := 0;

  StationsForSort := TStringList.Create; // and explicitly set options
  // sorts strings using ANSI (Windows) or UTF-8 (Linux) order
  StationsForSort.UseLocale := False; // True => ansi sort, NOT wanted
  StationsForSort.Duplicates := dupIgnore;
  // Ignore attempts to add duplicate strings to the list.
  StationsForSort.CaseSensitive := True; // default is false
  StationsForSort.OwnsObjects := True;
  // so, Stations object destroys its data objects
  StationsForSort.Sorted := False; // sort later
end;

function StaticBytesToString;
var
  ugly: TArray<Byte>; // prefer not to use TArray, but for now...
  i: Integer;
begin
  SetLength(ugly, entry_name_array_length);
  for i := 0 to entry_name_array_length - 1 do
  begin
    ugly[i] := entry_name_array[i];
  end;
  StaticBytesToString := TEncoding.UTF8.GetString(ugly);
end;


procedure HashAndSave;
var
  entry_integer: Integer;
  entry_Unicode: String;
  my_data_item: TStationForSortClass;

  entry_hash: Integer;
  PlaceIndex: Integer; // location in index array

begin
  entry_integer := BytesToTemp; // only pass J

  entry_Unicode := StaticBytesToString;
  // entry_name_array is global, only pass length

  entry_hash := BytesToIndexHash;
  // entry_name_array is global, only pass length

  while True do // success is handled by breaking out of while loop
  begin
    PlaceIndex := HashMap[entry_hash];

    if PlaceIndex = -1 then // this hash has not been used
    begin
      // store the first entry for this station
      PlaceIndex := PlaceCount;
      HashMap[entry_hash] := PlaceIndex; // set it
      my_data_item := TStationForSort.Create();
      my_data_item.DataCount := 1;
      my_data_item.DataSum := entry_integer;
      my_data_item.DataMin := entry_integer;
      my_data_item.DataMax := entry_integer;
      StationsForSort.AddObject(entry_Unicode, TObject(my_data_item));

      Inc(PlaceCount);
      Break;
    end
    else // hash has been used, check for collision
    begin
      if StationsForSort[PlaceIndex] = entry_Unicode then // OK, merge data
      begin
        (StationsForSort.Objects[PlaceIndex] as TStationForSortClass).DataSum :=
          (StationsForSort.Objects[PlaceIndex] as TStationForSortClass).DataSum
          + entry_integer;

        (StationsForSort.Objects[PlaceIndex] as TStationForSortClass).DataCount
          := (StationsForSort.Objects[PlaceIndex] as TStationForSortClass)
          .DataCount + 1;

        if (StationsForSort.Objects[PlaceIndex] as TStationForSortClass).DataMax
          < entry_integer then
          (StationsForSort.Objects[PlaceIndex] as TStationForSortClass).DataMax
            := entry_integer;

        if (StationsForSort.Objects[PlaceIndex] as TStationForSortClass).DataMin
          > entry_integer then
          (StationsForSort.Objects[PlaceIndex] as TStationForSortClass).DataMin
            := entry_integer;

        Break;
      end
      else // collision, try next spot in HashMap array
      begin
        Inc(entry_hash);
        if entry_hash >= Prime then
          entry_hash := 0; // wrap around
      end;
    end;
  end; // of: while True do

end;

procedure FileToArrays(inFile: String; UseStdOut: Boolean); inline;
var
  PlaceIndex: Integer; // location in index array
  i: Integer; // used for temporary purposes

  LF: Boolean; // True after finding  #10
  SC: Boolean; // True after finding semi-colon

  iFileHandle: Integer;
  iBytesRead: Integer;
  Buffer: PByte;

  Challenge: TFileStream;

begin

  if Not(UseStdOut) then
  begin
    WriteLn('Making Hash...');
  end;

  // Load the data
  PlaceCount := 0;
  PlaceIndex := 0;
  InitializeThings;

  if FileExists(inFile) then
  begin

    try
      Challenge := TFileStream.Create(inFile, fmOpenRead);
      Challenge.Seek(0, soFromBeginning);
      // Buffer := System.AllocMem(myBufferSize +1);   // why +1 ??
      Buffer := System.AllocMem(myBufferSize); // seems OK
      i := 0;
      entry_temp_array_length := 0;
      entry_name_array_length := 0;
      LF := False;
      SC := False;
      move(empty_entry_name_array[0], entry_name_array[0], 256);
      // will hold place name
      move(empty_entry_temp_array[0], entry_temp_array[0], 16);
      // will hold temperature

      iBytesRead := Challenge.Read(Buffer^, myBufferSize);
      while iBytesRead > 0 do
      begin
        while Not LF do // read byte by byte
        begin
          if ((Buffer + i)^ = 10) then
          begin
            LF := True; // and done collecting bytes
          end
          else // accumulate bytes
          begin
            if ((Buffer + i)^ = 59) then
              SC := True;
            // skip line feed, carriage return and semi-colon
            if Not(((Buffer + i)^ = 13) or ((Buffer + i)^ = 59)) then
            begin
              if SC then
              begin
                // skip decimal in number
                if Not((Buffer + i)^ = 46) then
                begin
                  entry_temp_array[entry_temp_array_length] := (Buffer + i)^;
                  Inc(entry_temp_array_length);
                end;
              end
              else
              begin
                entry_name_array[entry_name_array_length] := (Buffer + i)^; // don't skip period in name
                Inc(entry_name_array_length);
              end;
            end;
          end;
          Inc(i); // next byte
          if i >= iBytesRead then // need next buffer
          begin
            iBytesRead := Challenge.Read(Buffer^, myBufferSize);
            i := 0; // reset to beggining of buffer
          end;

          if LF then // convert to something usable and process it
          begin
            HashAndSave;
          end;

        end; // of: if LF then

        // reset to get next line
        LF := False;
        SC := False;
        entry_temp_array_length := 0;
        entry_name_array_length := 0;
        move(empty_entry_name_array[0], entry_name_array[0], 256);
        // will hold place name
        move(empty_entry_temp_array[0], entry_temp_array[0], 16);
        // will hold temperature

      end; // of while iBytesRead > 0 do   --- implies end of file
      Challenge.Free;

    finally
      // nothing here
    end;

  end
  else
  begin
    raise Exception.Create(Format('File "%s" not found.', [inFile]));
  end;

end;

procedure SortArrays;
begin
  StationsForSort.Sort;
end;

procedure ArrayToFile(outFile: String; UseStdOut: Boolean);
// Inline; // inline not help
var
  outputFileStream: TFileStream;
  PlaceIndex: Integer; // location in array
  bufferStr: String;
  EntrySum: Integer;
  EntryCount: Integer;
  EntryMax: Integer;
  EntryMin: Integer;

begin
  try
    if Not(UseStdOut) then
    begin
      outputFileStream := TFileStream.Create(outFile, fmCreate);
    end;

    for PlaceIndex := 0 to PlaceCount - 1 do
    begin
      if PlaceIndex = 0 then
      begin
        bufferStr := '{';
      end
      else
      begin
        bufferStr := ', ';
      end;

      EntrySum := (StationsForSort.Objects[PlaceIndex]
        as TStationForSortClass).DataSum;
      EntryCount := (StationsForSort.Objects[PlaceIndex]
        as TStationForSortClass).DataCount;
      EntryMax := (StationsForSort.Objects[PlaceIndex]
        as TStationForSortClass).DataMax;
      EntryMin := (StationsForSort.Objects[PlaceIndex]
        as TStationForSortClass).DataMin;

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

end.
