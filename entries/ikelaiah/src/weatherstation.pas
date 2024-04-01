unit WeatherStation;

{$mode objfpc}{$H+}{$J-}{$modeSwitch advancedRecords}

interface

uses
  Classes,
  SysUtils,
  Math,
  streamex,
  lgHashMap
  {$IFDEF DEBUG}
  , Stopwatch
  {$ENDIF}
  , Baseline.Common
  ;

type
  TParsedData = record
    wsName: string;
    wsTemp: int64;
  end;

type
  // Create a record of temperature stats.
  TStat = record
  var
    min: int64; // Borrowed the concept from go's approach to improve
    // performance, save floats as int64.
    max: int64; // This saved ~2 mins processing time.
    sum: int64;
    cnt: int64;
  public
    constructor Create(const newMin: int64; const newMax: int64;
      const newSum: int64; const newCount: int64);
    function ToString: string;
  end;

type
  // Create a dictionary
  TWeatherDictionaryLG = specialize TGHashMapQP<string, TStat>;

type
  // Create a class to encapsulate the temperature observations of each weather station.
  TWeatherStation = class
  private
    fname: string;
    weatherDictionary: TWeatherDictionaryLG;
    weatherStationList: TStringList;
    procedure ReadMeasurements;
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

constructor TStat.Create(const newMin: int64; const newMax: int64;
  const newSum: int64; const newCount: int64);
begin
  self.min := newMin;
  self.max := newMax;
  self.sum := newSum;
  self.cnt := newCount;
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
  // Create a dictionary
  weatherDictionary := TWeatherDictionaryLG.Create;
  // Create a TStringList for sorting
  weatherStationList := TStringList.Create;
end;

destructor TWeatherStation.Destroy;
begin
  // Free TStringLIst dictionary
  weatherStationList.Free;
  // Free the dictionary
  weatherDictionary.Free;
end;

procedure TWeatherStation.PrintSortedWeatherStationAndStats;
var
  outputList: string;
  index: int64;
begin

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
end;

procedure TWeatherStation.SortWeatherStationAndStats;
var
  wsKey: string;
begin
  wsKey := '';

  if self.weatherDictionary.GetCapacity = 0 then
  begin
    WriteLn('Nothing to Sort.');
    Exit;
  end;
  for wsKey in weatherDictionary.Keys do
  begin
    self.weatherStationList.Add(wsKey + '=' + weatherDictionary[wsKey].ToString + ', ');
  end;

  self.weatherStationList.CustomSort(@CustomTStringListComparer);
end;

procedure TWeatherStation.AddCityTemperatureLG(const cityName: string;
  const newTemp: int64);
var
  stat: TStat;
begin
  // If city name esxists, modify temp as needed
  if self.weatherDictionary.Contains(cityName) then
  begin
    // Get the temp record
    stat := self.weatherDictionary[cityName];

    // If the temp lower then min, set the new min.
    if newTemp < stat.min then
      stat.min := newTemp;

    // If the temp higher than max, set the new max.
    if newTemp > stat.max then
      stat.max := newTemp;

    // Add count for this city.
    stat.sum := stat.sum + newTemp;

    // Increase the counter
    stat.cnt := stat.cnt + 1;

    // Update the stat of this city
    self.weatherDictionary.AddOrSetValue(cityName, stat);
    {$IFDEF DEBUG}
    // Display the line.
    // WriteLn('Updated: ', cityName);
    {$ENDIF DEBUG}
  end;

  // If city name doesn't exist add a new entry
  if not self.weatherDictionary.Contains(cityName) then
  begin
    self.weatherDictionary.Add(cityName, TStat.Create(newTemp, newTemp, newTemp, 1));
    {$IFDEF DEBUG}
    // Display the line.
    // WriteLn('Added: ', cityName);
    {$ENDIF DEBUG}
  end;
end;

procedure TWeatherStation.ParseStationAndTemp(const line: string);
var
  delimiterPos: integer;
  parsedStation, strTemp: string;
  parsedTemp, valCode: int64;
begin

  // Get position of the delimiter
  delimiterPos := Pos(';', line);
  if delimiterPos > 0 then
  begin
    // Get the weather station name
    // Using Copy and POS - as suggested by Gemini AI.
    // This part saves 3 mins faster when processing 1 billion rows.
    parsedStation := Copy(line, 1, delimiterPos - 1);

    // Get the temperature recorded, as string, remove '.' from string float
    // because we want to save it as int64.
    strTemp := Copy(line, delimiterPos + 1, Length(line));
    // strTemp := StringReplace(strTemp, '.', '', [rfReplaceAll]);
    // The above operation is a bit expensive.
    // Rewrote a simple function which prevents creation of new string
    // in each iteration. Saved approx 20-30 seconds for 1 billion row.
    // Remove dots turns a float into an int.
    strTemp := RemoveDots(strTemp);

    // Add the weather station and the recorded temp (as int64) in the TDictionary
    Val(strTemp, parsedTemp, valCode);
    if valCode <> 0 then Exit;

    // Add a record in TWeatherDictionary
    self.AddCityTemperatureLG(parsedStation, parsedTemp);
  end;
end;

procedure TWeatherStation.ReadMeasurements;
var
  fileStream: TFileStream;
  streamReader: TStreamReader;
  line: string;
begin

  // Open the file for reading
  fileStream := TFileStream.Create(self.fname, fmOpenRead or fmShareDenyNone);
  try
    streamReader := TStreamReader.Create(fileStream);
    try
      // Read and parse chunks of data until EOF -------------------------------
      while not streamReader.EOF do
      begin
        line := streamReader.ReadLine;
        self.ParseStationAndTemp(line);
      end;// End of read and parse chunks of data ------------------------------
    finally
      streamReader.Free;
    end;
  finally
    // Close the file
    fileStream.Free;
  end;
end;

// The main algorithm
procedure TWeatherStation.ProcessMeasurements;
begin
  self.ReadMeasurements;
  self.SortWeatherStationAndStats;
  self.PrintSortedWeatherStationAndStats;
end;

end.
