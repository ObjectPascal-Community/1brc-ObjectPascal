unit ProcessUnit;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.Math;

type
  TStationDataClass = class(TObject)
  public
    DataSum: Integer;
    DataCount: Integer;
    DataMax: Integer;
    DataMin: Integer;
  end;

  TStationData = class of TStationDataClass;

var
  Stations: TStringList;
  start: TDateTime; // for timing

procedure ProcessFile(inFile: String; outFile: String; UseStdOut: Boolean);
procedure DumpFile(outFile: String; UseStdOut: Boolean);

implementation

procedure InitCities;
begin
  Stations := TStringList.Create; // explicitly set options
  Stations.Sorted := True;
  // sorts strings using ANSI (Windows) or UTF-8 (Linux) order
  Stations.UseLocale := False; // True => ansi sort, NOT wanted
  Stations.Duplicates := dupIgnore;
  // Ignore attempts to add duplicate strings to the list.
  Stations.CaseSensitive := True; // default is false
  Stations.OwnsObjects := True; // so, Stations object destroys its data objects
end;

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
  remainder: Integer;
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

procedure DumpFile(outFile: String; UseStdOut: Boolean);
var
  outputFileStream: TFileStream;
  StationCount: Integer;
  index: Integer;
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
    // not used for console
    StationCount := Stations.count;

    for index := 0 to StationCount - 1 do
    begin
      if index = 0 then
      begin
        bufferStr := '{';
      end
      else
      begin
        bufferStr := ', ';
      end;

      EntrySum := (Stations.Objects[index] as TStationDataClass).DataSum;
      EntryCount := (Stations.Objects[index] as TStationDataClass).DataCount;
      EntryMax := (Stations.Objects[index] as TStationDataClass).DataMax;
      EntryMin := (Stations.Objects[index] as TStationDataClass).DataMin;

      bufferStr := bufferStr + Stations[index] + '=' + IntegerFixup(EntryMin) +
        '/' + MeanFixup(EntrySum, EntryCount) + '/' + IntegerFixup(EntryMax);

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

procedure ProcessFile(inFile: String; outFile: String; UseStdOut: Boolean);

var
  inputStream: TFileStream;
  streamReader: TStreamReader;
  entry: String;
  entry_length: Integer;
  entry_text: String;
  entry_value: String;
  entry_integer: Integer;
  semicolon: Integer; // position of ";"
  index: Integer; // location in list
  data_item: TStationDataClass;
  conversion_error: Integer;

begin
  InitCities;
  if Not(UseStdOut) then
  begin
    WriteLn('Building Weather Station List...');
  end;

  // Load the Weather Station names
  if FileExists(inFile) then
  begin
    inputStream := TFileStream.Create(inFile, fmOpenRead);
    try
      streamReader := TStreamReader.Create(inputStream);

      try
        while not streamReader.EndOfStream do
        begin
          entry := streamReader.ReadLine;
          entry_length := Length(entry);
          semicolon := Pos(';', entry);
          entry_text := Copy(entry, 0, semicolon - 1);

          // capture number without decimal, adjusted in results, later
          entry_value := Copy(entry, semicolon + 1,
            entry_length - semicolon - 2);
          entry_value := entry_value + Copy(entry, entry_length, 1);
          val(entry_value, entry_integer, conversion_error);
          // later, trap error???

          if Stations.Find(entry_text, index) then
          // update corresponding data onject
          begin
            (Stations.Objects[index] as TStationDataClass).DataSum :=
              (Stations.Objects[index] as TStationDataClass).DataSum +
              entry_integer;
            (Stations.Objects[index] as TStationDataClass).DataCount :=
              (Stations.Objects[index] as TStationDataClass).DataCount + 1;

            if (Stations.Objects[index] as TStationDataClass).DataMax < entry_integer
            then
              (Stations.Objects[index] as TStationDataClass).DataMax :=
                entry_integer;

            if (Stations.Objects[index] as TStationDataClass).DataMin > entry_integer
            then
              (Stations.Objects[index] as TStationDataClass).DataMin :=
                entry_integer;

          end
          else // create new data object and attach to city
          begin
            data_item := TStationDataClass.Create();
            data_item.DataCount := 1;
            data_item.DataSum := entry_integer;
            data_item.DataMin := entry_integer;
            data_item.DataMax := entry_integer;
            Stations.AddObject(entry_text, TObject(data_item));
          end;

        end;
      finally
        streamReader.Free;
      end;
    finally
      inputStream.Free;

    end;
  end
  else
  begin
    raise Exception.Create(Format('File "%s" not found.', [inFile]));
  end;

end;

end.
