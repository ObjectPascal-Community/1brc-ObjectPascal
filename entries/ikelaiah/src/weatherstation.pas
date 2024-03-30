unit WeatherStation;

{$mode objfpc}{$H+}{$J-}{$modeSwitch advancedRecords}
interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  Math,
  streamex,
  bufstream, lgHashMap
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
  TWeatherDictionary = specialize TDictionary<string, TStat>;
  TWeatherDictionaryLG = specialize TGHashMapQP<string, TStat>;

// The main algorithm to process the temp measurements from various weather station
procedure ProcessTempMeasurementsV4a(const filename: string);

implementation

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

procedure AddCityTemperatureLG(const cityName: string; const newTemp: int64;
  var weatherDictionary: TWeatherDictionaryLG);
var
  stat: TStat;
begin
  // If city name esxists, modify temp as needed
  if weatherDictionary.Contains(cityName) then
  begin
    // Get the temp record
    stat := weatherDictionary[cityName];

    // If the temp lower then min, set the new min.
    if newTemp < stat.min then stat.min := newTemp;

    // If the temp higher than max, set the new max.
    if newTemp > stat.max then stat.max := newTemp;

    // Add count for this city.
    stat.sum := stat.sum + newTemp;

    // Increase the counter
    stat.cnt := stat.cnt + 1;

    // Update the stat of this city
    weatherDictionary.AddOrSetValue(cityName, stat);
    {$IFDEF DEBUG}
    // Display the line.
    // WriteLn('Updated: ', cityName);
    {$ENDIF DEBUG}
  end;

  // If city name doesn't exist add a new entry
  if not weatherDictionary.Contains(cityName) then
  begin
    weatherDictionary.Add(cityName, TStat.Create(newTemp, newTemp, newTemp, 1));
    {$IFDEF DEBUG}
    // Display the line.
    // WriteLn('Added: ', cityName);
    {$ENDIF DEBUG}
  end;
end;


procedure ProcessTempMeasurementsV4a(const filename: string);
var
  fReader: TStreamReader;
  fStream: TBufferedFileStream;
  line, parsedStation, strTemp, wsKey, outputList: string;
  parsedTemp: int64;
  delimiterPos, valCode, index: integer;
  wd: TWeatherDictionaryLG;
  weatherStationList: TStringList;
begin

  // Create a city - weather dictionary
  wd := TWeatherDictionaryLG.Create;
  // Create a city - weather TStringList for sorting
  weatherStationList := TStringList.Create;

  // Create a file stream and use TReadBufStream to buffer data
  fStream := TBufferedFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    fReader := TStreamReader.Create(fStream);
    try
      while not fReader.EOF do // Start read file ----------------------------
      begin
        line := fReader.ReadLine;
        // Get position of the delimiter -------------------------------------
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
          if valCode <> 0 then
            Continue;

          // Add a record in TWeatherDictionary
          AddCityTemperatureLG(parsedStation, parsedTemp, wd);

        end; // end of checking delimiter in a line --------------------------
      end; // end of reading file --------------------------------------------

      {$IFDEF DEBUG}
            Stopwatch.StopTimer;
            WriteLn('Finished reading and parsing input file');
            Stopwatch.DisplayTimer;
      {$ENDIF}

      // Format and sort weather station by name and temp stat ---------------
      {$IFDEF DEBUG}
            Stopwatch.StartTimer;
      {$ENDIF}
      wsKey := '';
      for wsKey in wd.Keys do
      begin
        weatherStationList.Add(wsKey + '=' + wd[wsKey].ToString + ', ');
      end;
      weatherStationList.CustomSort(@CustomTStringListComparer);

      {$IFDEF DEBUG}
            Stopwatch.StopTimer;
            WriteLn('Finished creating TStringList and sorted it');
            Stopwatch.DisplayTimer;
      {$ENDIF}

      // Print TStringList - sorted by weather station and temp stat ---------
      {$IFDEF DEBUG}
            Stopwatch.StartTimer;
      {$ENDIF}
      outputList := '';
      // Print the weather station and the temp stat
      for index := 0 to weatherStationList.Count - 1 do
        outputList := outputList + weatherStationList[index];
      // Remove last comma and space; ', ', a neat trick from Gus.
      SetLength(outputList, Length(outputList) - 2);
      WriteLn('{', outputList, '}');

      {$IFDEF DEBUG}
            Stopwatch.StopTimer;
            WriteLn('Finished printing the sorted weather station and temperatures');
            Stopwatch.DisplayTimer;
      {$ENDIF}
    finally
      fReader.Free
    end; // StreamReader deallocates /////////////////////////////////////////

  finally
    fStream.Free;
    weatherStationList.Free;
    wd.Free;
  end;
end;


end.
