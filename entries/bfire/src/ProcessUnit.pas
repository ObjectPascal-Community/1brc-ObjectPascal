unit ProcessUnit;

interface

uses
  System.SysUtils,
  System.StrUtils,
  Classes,
  Math;

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
  
procedure ProcessFile(inFile: String; outFile: String);
procedure DumpFile(outFile: String);

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

function RoundTowardPositiveInfinity(test: Extended): String;
var
  temp: Extended;
  thing: String;

begin
  case Sign(test) of
    1:
      begin
        temp := SimpleRoundTo(test, -1);
        thing := FloatToStrF(temp, ffFixed, 10, 1);
      end;
    0:
      begin
        thing := '0.0';
      end;
    -1:
      begin
        temp := SimpleRoundTo(-1.0 * test, -1);
        thing := '-' + FloatToStrF(temp, ffFixed, 10, 1);
        if thing = '-0.0' then
          thing := '0.0';
      end;
  end;
  RoundTowardPositiveInfinity := thing;
end;

function IntegerFixup(something: Integer): String;
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

function MeanFixup(total: Integer; count: Integer): String;
// fixup to adjust for storing tenths of degrees as integer
// calculate mean to one decimal place, rounded toward positive infinity
var
  Mean: Double;

begin
  Mean := Double(total) / Double(count) / 10.0;
  MeanFixup := RoundTowardPositiveInfinity(Mean);
end;

procedure DumpFile(outFile: String);
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
    outputFileStream := TFileStream.Create(outFile, fmCreate);
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

      if outFile = 'CONSOLE' then // send to STDOUT, where it gets mangled
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
    if outFile = 'CONSOLE' then // send to STDOUT, where it gets mangled
    begin
      write(bufferStr);
    end
    else
    begin
      outputFileStream.WriteBuffer(TEncoding.UTF8.GetBytes(bufferStr),
        TEncoding.UTF8.GetByteCount(bufferStr));
    end;
    outputFileStream.Free;

  finally
    if outFile <> 'CONSOLE' then
    begin
      WriteLn;
      WriteLn;
      WriteLn('Done');
    end;
  end;

end;

procedure ProcessFile(inFile: String; outFile: String);

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
  if outFile <> 'CONSOLE' then
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
