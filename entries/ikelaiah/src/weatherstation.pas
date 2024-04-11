unit WeatherStation;

{$mode objfpc}{$H+}{$J-}{$modeSwitch advancedRecords}

interface

uses
  Classes
  , SysUtils
  , Math
  , streamex
  , bufstream
  , lgHashMap
  , StrUtils
  {$IFDEF DEBUG}
  , Stopwatch
  {$ENDIF}
  , Baseline.Common;

type
  { Create a record of temperature stats.

    Borrowed the concept from go's approach to improve performance, save floats as int64.
    This saved ~2 mins processing time for processing 1 billion rows.}
  TStat = record
  var
    min: int64;
    max: int64;
    sum: int64;
    cnt: int64;
  public
    function ToString: string;
  end;
  {Using pointer to TStat saves approx. 30-60 seconds for processing 1 billion rows}
  PStat = ^TStat;

type
  // Using this dictionary, now approx 4 mins faster than Generics.Collections.TDictionary
  TWeatherDictionaryLG = specialize TGHashMapLP<string, PStat>;

type
  // a type for storing valid lookup temperature
  TValidTemperatureDictionary = specialize TGHashMapLP<string, int64>;

type
  // Create a class to encapsulate the temperature observations of each weather station.
  TWeatherStation = class
  private
    fname: string;
    weatherDictionary: TWeatherDictionaryLG;
    weatherStationList: TStringList;
    lookupStrFloatToIntList: TValidTemperatureDictionary;
    procedure CreateLookupTemp;
    procedure ReadMeasurements;
    procedure ReadMeasurementsBuf;
    procedure ParseStationAndTemp(const line: string);
    procedure AddCityTemperatureLG(const cityName: string; const newTemp: int64);
    procedure SortWeatherStationAndStats;
    procedure PrintSortedWeatherStationAndStats;
  public
    constructor Create(const filename: string);
    destructor Destroy; override;
    // The main algorithm to process the temp measurements from various weather stations
    procedure ProcessMeasurements;
  end;


implementation

{
  A custom comparer for TStringList.

  The following procedure Written by Székely Balázs for the 1BRC for Object Pascal.
  URL: https://github.com/gcarreno/1brc-ObjectPascal/tree/main
}
function CustomTStringListComparer(AList: TStringList;
  AIndex1, AIndex2: integer): integer;
var
  Pos1, Pos2: integer;
  Str1, Str2: string;
begin
  Result := 0;
  Str1 := AList.Strings[AIndex1];
  Str2 := AList.Strings[AIndex2];
  Pos1 := Pos('=', Str1);
  Pos2 := Pos('=', Str2);
  if (Pos1 > 0) and (Pos2 > 0) then
  begin
    Str1 := Copy(Str1, 1, Pos1 - 1);
    Str2 := Copy(Str2, 1, Pos2 - 1);
    Result := CompareStr(Str1, Str2);
  end;
end;

// Remove dots from a string
function RemoveDots(const line: string): string;
var
  index: integer;
begin
  Result := line;
  for index := Length(Result) downto 1 do
  begin
    if Result[index] = '.' then
      Delete(Result, index, 1);
  end;
end;

function TStat.ToString: string;
var
  minR, meanR, maxR: double; // Store the rounded values prior saving to TStringList.
begin
  minR := RoundExDouble(self.min / 10);
  maxR := RoundExDouble(self.max / 10);
  meanR := RoundExDouble(self.sum / self.cnt / 10);
  Result := FormatFloat('0.0', minR) + '/' + FormatFloat('0.0', meanR) +
    '/' + FormatFloat('0.0', maxR);
end;

constructor TWeatherStation.Create(const filename: string);
begin
  // Assign filename
  fname := filename;
  // Create a lookup
  self.lookupStrFloatToIntList := TValidTemperatureDictionary.Create;
  // Create a dictionary
  weatherDictionary := TWeatherDictionaryLG.Create;
  // Create a TStringList for sorting
  weatherStationList := TStringList.Create;
end;

destructor TWeatherStation.Destroy;
var
  stationName: string;
begin

  // Free the lookup dictionary
  self.lookupStrFloatToIntList.Free;

  // Free TStringList dictionary
  weatherStationList.Free;

  // Free the dictionary - 1. Free PStat first
  for stationName in self.weatherDictionary.Keys do
    Dispose(PStat(self.weatherDictionary.Items[stationName]));

  // Free the dictionary - 2. Finally free the container itself
  weatherDictionary.Free;
end;

procedure TWeatherStation.CreateLookupTemp;
var
  startTemp: int64 = -1000;
  finishTemp: int64 = 1000;
  currentTemp: int64;
  numStr: string;
begin

  currentTemp := startTemp;

  while currentTemp <> finishTemp do
  begin
    self.lookupStrFloatToIntList.Add(formatfloat('0.0', currentTemp / 10), currentTemp);
    currentTemp := currentTemp + 1;
  end;

  {$ifdef DEBUG}
  for numStr in self.lookupStrFloatToIntList.Keys do
    WriteLn('We have key: ', numStr, ' with value of: ',
      IntToStr(self.lookupStrFloatToIntList[numStr]));
  Writeln(self.lookupStrFloatToIntList.Count);
  {$endif DEBUG}
end;

procedure TWeatherStation.PrintSortedWeatherStationAndStats;
var
  outputList: string;
  index: int64;
begin

  {$IFDEF DEBUG}
  // Display the line.
  WriteLn('Printing now: ', DateTimeToStr(Now));
  {$ENDIF DEBUG}

  if self.weatherStationList.Count = 0 then
  begin
    WriteLn('Nothing to print. The list is empty.');
    Exit;
  end;

  outputList := '';
  // Print the weather station and the temp stat
  for index := 0 to self.weatherStationList.Count - 1 do
    outputList := outputList + self.weatherStationList[index];

  // Remove last comma and space; ', ', a neat trick from Gus.
  SetLength(outputList, Length(outputList) - 2);
  WriteLn('{', outputList, '}');

  {$IFDEF DEBUG}
  // Display the line.
  WriteLn('Printing done: ', DateTimeToStr(Now));
  {$ENDIF DEBUG}
end;

procedure TWeatherStation.SortWeatherStationAndStats;
var
  wsKey: string;
begin

  {$IFDEF DEBUG}
  // Display the line.
  WriteLn('Sorting now: ', DateTimeToStr(Now));
  {$ENDIF DEBUG}

  wsKey := '';

  if self.weatherDictionary.Count = 0 then
  begin
    WriteLn('Nothing to Sort.');
    Exit;
  end;

  for wsKey in weatherDictionary.Keys do
  begin
    self.weatherStationList.Add(wsKey + '=' + weatherDictionary[wsKey]^.ToString + ', ');
  end;

  self.weatherStationList.CustomSort(@CustomTStringListComparer);

  {$IFDEF DEBUG}
  // Display the line.
  WriteLn('Sorting done: ', DateTimeToStr(Now));
  {$ENDIF DEBUG}
end;

procedure TWeatherStation.AddCityTemperatureLG(const cityName: string;
  const newTemp: int64);
var
  stat: PStat;
begin
  // If city name esxists, modify temp as needed
  if self.weatherDictionary.TryGetValue(cityName, stat) then
  begin
    // Get the temp record
    // stat := self.weatherDictionary[cityName];

    // Update min and max temps if needed
    // Re-arranged the if statement, to achieve minimal if checks.
    // This saves approx 15 seconds when processing 1 billion row.
    if (newTemp < stat^.min) or (newTemp > stat^.max) then
    begin
      // If the temp lower then min, set the new min.
      if newTemp < stat^.min then
        stat^.min := newTemp;
      // If the temp higher than max, set the new max.
      if newTemp > stat^.max then
        stat^.max := newTemp;
    end;
    // Add count for this city.
    stat^.sum := stat^.sum + newTemp;

    // Increase the counter
    stat^.cnt := stat^.cnt + 1;

    // Update the stat of this city
    // self.weatherDictionary.AddOrSetValue(cityName, stat);
    {$IFDEF DEBUG}
    // Display the line.
    WriteLn('Updated: ', cityName);
    {$ENDIF DEBUG}
  end
  else
  begin
    // Re-arranged this if portion also to achieve minimal if checks.
    // This saves approx 15 seconds when processing 1 billion row.
    // If city name doesn't exist add a new entry
    New(stat);
    stat^.min := newTemp;
    stat^.max := newTemp;
    stat^.sum := newTemp;
    stat^.cnt := 1;
    self.weatherDictionary.Add(cityName, stat);

    {$IFDEF DEBUG}
    // Display the line.
    WriteLn('weatherDictionary count: ', inttostr(self.weatherDictionary.Count));
    WriteLn('Added: ', cityName);
    {$ENDIF DEBUG}
  end;
end;

procedure TWeatherStation.ParseStationAndTemp(const line: string);
var
  delimiterPos: integer;
  parsedStation, strFloatTemp: string;
  results: array of string;
  parsedTemp, valCode: int64;
begin
  // Get position of the delimiter
  delimiterPos := Pos(';', line);
  if delimiterPos > 0 then
  begin
    // Get the weather station name
    // Using Copy and POS - as suggested by Gemini AI.
    // This part saves 3 mins faster when processing 1 billion rows.

    // No need to create a string
    // parsedStation := Copy(line, 1, delimiterPos - 1);
    strFloatTemp := Copy(line, delimiterPos + 1, Length(line));

    // Using a lookup value speeds up 30-45 seconds
    if self.lookupStrFloatToIntList.TryGetValue(strFloatTemp, parsedTemp) then
    begin
      self.AddCityTemperatureLG(Copy(line, 1, delimiterPos - 1), parsedTemp);
    end;
  end;
end;

procedure TWeatherStation.ReadMeasurements;
var
  fileStream: TFileStream;
  bufStream: TReadBufStream;
  streamReader: TStreamReader;
  line: string;
begin

  // Open the file for reading
  fileStream := TFileStream.Create(self.fname, fmOpenRead);
  try
    streamReader := TStreamReader.Create(fileStream, 65536 * 2, False);
    try
      // Read and parse chunks of data until EOF -------------------------------
      while not streamReader.EOF do
      begin
        // line := streamReader.ReadLine;
        self.ParseStationAndTemp(streamReader.ReadLine);
      end;// End of read and parse chunks of data ------------------------------
    finally
      streamReader.Free;
    end;
  finally
    // Close the file
    fileStream.Free;
  end;
end;

procedure TWeatherStation.ReadMeasurementsBuf;
var
  fileStream: TFileStream;
  memStream: TMemoryStream;
  streamReader: TStreamReader;
  buffer: TBytes;
  bytesRead, totalBytesRead, chunkSize, lineBreakPos, chunkIndex,
  index, lineCount: int64;
begin

  chunksize := 536870912 * 1;

  // Open the file for reading
  fileStream := TFileStream.Create(self.fname, fmOpenRead);
  SetLength(buffer, chunkSize);
  try
    memStream := TMemoryStream.Create;
    try
      totalBytesRead := 0;
      chunkIndex := 0;
      lineCount := 0;

      // Read and parse chunks of data until EOF
      while totalBytesRead < fileStream.Size do
      begin
        // Read more bytes and keep track on bytes read
        bytesRead := fileStream.Read(buffer[0], chunkSize);
        Inc(totalBytesRead, bytesRead);

        // Find the position of the last newline character in the chunk
        lineBreakPos := BytesRead;
        while (lineBreakPos > 0) and (Buffer[lineBreakPos - 1] <> Ord(#10)) do
          Dec(lineBreakPos);

        { Now, must ensure that if the last byte read in the current chunk
          is not a newline character, the file pointer is moved back to include
          that byte and any preceding bytes of the partial line in the next
          chunk's read operation.

          Also, no need to update the BytesRead variable in this context because
          it represents the actual number of bytes read from the file, including
          any partial line that may have been included due to moving the file
          pointer back.
          Ref: https://www.freepascal.org/docs-html/rtl/classes/tstream.seek.html}
        if lineBreakPos < bytesRead then
          fileStream.Seek(-(bytesRead - lineBreakPos), soCurrent);
        //{$IFDEF DEBUG}
        // Do something with the chunk here
        // Like counting line
        for index := 0 to lineBreakPos - 1 do
          if buffer[index] = Ord(#10) then
            lineCount := lineCount + 1;
        //{$ENDIF DEBUG}

        // Use memory stream & stream reader
        memStream.Write(buffer[0], lineBreakPos - 1);
        memStream.Position:=0;
        streamReader := TStreamReader.Create(memStream);
        try
          while not streamReader.EOF do
          begin
            // WriteLn(streamReader.ReadLine);
            self.ParseStationAndTemp(streamReader.ReadLine);
          end;
        finally
          streamReader.Free;
        end;
        //{$IFDEF DEBUG}
        // Display user feedback
        WriteLn('Line count: ', IntToStr(lineCount));
        WriteLn('Chunk ', chunkIndex, ', Total bytes read:', IntToStr(totalBytesRead));
        //{$ENDIF DEBUG}

        //{$IFDEF DEBUG}
        // Increase chunk index - a counter
        Inc(chunkIndex);
        //{$ENDIF DEBUG}
      end;
    finally
      memStream.Free;
    end;
  finally
    // Close the file
    fileStream.Free;
  end;
end;

// The main algorithm
procedure TWeatherStation.ProcessMeasurements;
begin
  self.CreateLookupTemp;
  self.ReadMeasurements;
  //self.ReadMeasurementsBuf;
  self.SortWeatherStationAndStats;
  self.PrintSortedWeatherStationAndStats;
end;

end.
