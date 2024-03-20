unit WeatherStation;

{$mode objfpc}{$H+}{$J-}{$modeSwitch advancedRecords}
interface

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  Generics.Collections,
  Math
  {$IFDEF DEBUG}
,  Stopwatch
  {$ENDIF}
  ;

type
  // Create a record of temperature stats
  TStat = record
  var
    min: int64; // Borrowed the concept from go's approach to improve performance, save floats as int64
    max: int64; // This saved ~2 mins processing time.
    sum: int64;
    cnt: int64;
  private
    function RoundEx(x: double): double;     // Borrowed from the baseline program
    function PascalRound(x: double): double; // Borrowed from the baseline program
  public
    constructor Create(const newMin: int64; const newMax: int64;
      const newSum: int64; const newCount: int64);
    function ToString: string;

  end;

type
  // Create a dictionary
  TWeatherDictionary = specialize TDictionary<string, TStat>;

// The main algorithm to process the temp measurements from various weather station
procedure ProcessTempMeasurements(const filename: string);

implementation

function TStat.RoundEx(x: double): double;
begin
  Result := PascalRound(x * 10.0) / 10.0;
end;

function TStat.PascalRound(x: double): double;
var
  t: double;
begin
  //round towards positive infinity
  t := Trunc(x);
  if (x < 0.0) and (t - x = 0.5) then
  begin
    // Do nothing
  end
  else if Abs(x - t) >= 0.5 then
  begin
    t := t + Math.Sign(x);
  end;

  if t = 0.0 then
    Result := 0.0
  else
    Result := t;
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
  minR := RoundEx(self.min / 10);
  maxR := RoundEx(self.max / 10);
  meanR := RoundEx(self.sum / self.cnt / 10);
  Result := FormatFloat('0.0', minR) + '/' + FormatFloat('0.0', meanR) +
    '/' + FormatFloat('0.0', maxR);
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


procedure AddCityTemperature(const cityName: string; const newTemp: int64;
  var weatherDictionary: TWeatherDictionary);
var
  stat: TStat;
begin
  // If city name esxists, modify temp as needed
  if weatherDictionary.ContainsKey(cityName) then
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
  end;

  // If city name doesn't exist add a new entry
  if not weatherDictionary.ContainsKey(cityName) then
  begin
    weatherDictionary.Add(cityName, TStat.Create(newTemp, newTemp, newTemp, 1));
  end;
end;

procedure ProcessTempMeasurements(const filename: string);
var
  wd: TWeatherDictionary;
  line, ws, strTemp: string;
  weatherStationList: TStringList;
  textFile: System.TextFile;
  delimiterPos, valCode: integer;
  intTemp: int64;
  index: integer;
begin

  // Create a city - weather dictionary
  wd := TWeatherDictionary.Create;
  weatherStationList := TStringList.Create;
  try

    // Read text file //////////////////////////////////////////////////////////
    AssignFile(textFile, filename);

    // Perform the read operation in a try..except block to handle errors gracefully
    try
      // Open the file for reading
      Reset(textFile);

      {$IFDEF DEBUG}
      // Start a timer
      Stopwatch.StartTimer;
      {$ENDIF}

      // Keep reading lines until the end of the file is reached
      while not EOF(textFile) do
      begin
        // Read a line
        ReadLn(textFile, line);

        // Get position of the delimiter
        delimiterPos := Pos(';', line);
        if delimiterPos > 0 then
        begin
          // Get the weather station name
          // Using Copy and POS - as suggested by Gemini AI.
          // This part saves 3 mins faster when processing 1 billion rows.
          ws := Copy(line, 1, delimiterPos - 1);

          // Get the temperature recorded, as string, remove '.' from string float
          // because we want to save it as int64.
          strTemp := Copy(line, delimiterPos + 1, Length(line));
          strTemp := StringReplace(strTemp, '.', '', [rfReplaceAll]);

          // Add the weather station and the recorded temp (as int64) in the TDictionary
          Val(strTemp, intTemp, valCode);
          if valCode <> 0 then Continue;
          AddCityTemperature(ws, intTemp, wd);
        end;
      end; // end while loop reading line at a time

      // Close the file
      CloseFile(textFile);

    except
      on E: Exception do
        WriteLn('File handling error occurred. Details: ', E.Message);
    end; // End of file reading ////////////////////////////////////////////////

    {$IFDEF DEBUG}
    Stopwatch.StopTimer;
    WriteLn('Finished reading and updating dictionary');
    Stopwatch.DisplayTimer;
    {$ENDIF}

    // Format and sort weather station by name and temp stat ///////////////////
    {$IFDEF DEBUG}
    Stopwatch.StartTimer;
    {$ENDIF}
    ws := '';
    for ws in wd.Keys do
    begin
      weatherStationList.Add(ws + '=' + wd[ws].ToString + ', ');
    end;
    weatherStationList.CustomSort(@CustomTStringListComparer);

    {$IFDEF DEBUG}
    Stopwatch.StopTimer;
    WriteLn('Finished creating TStringList and sorted it');
    Stopwatch.DisplayTimer;
    {$ENDIF}

    // Print TStringList - sorted by weather station and temp stat /////////////
    {$IFDEF DEBUG}
    Stopwatch.StartTimer;
    {$ENDIF}
    strTemp := '';
    // Print the weather station and the temp stat
    for index := 0 to weatherStationList.Count - 1 do
      strTemp := strTemp + weatherStationList[index];
    // Remove last comma and space; ', ', a neat trick from Gus.
    SetLength(strTemp, Length(strTemp) - 2);
    WriteLn('{', strTemp, '}');

    {$IFDEF DEBUG}
    Stopwatch.StopTimer;
    WriteLn('Finished printing the sorted weather station and temperatures');
    Stopwatch.DisplayTimer;
    {$ENDIF}

  finally
    weatherStationList.Free;
    wd.Free;
  end; // End of processing TDictionary and TStringList

end;

end.
