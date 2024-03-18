unit WeatherStation;

{$mode objfpc}{$H+}{$J-}{$modeSwitch advancedRecords}

interface

uses
  {$IFDEF UNIX}
  cthreads,
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
    min: int64;
    max: int64;
    sum: int64;
    Count: int64;
  private
    function RoundEx(x: double): double;     // Borrowed from the baseline program
    function PascalRound(x: double): double; // Borrowed from the baseline program
  public
    constructor Create(newMin: int64; newMax: int64; newSum: int64; newCount: int64);
    function ToString: string;

  end;

type
  // Create a dictionary
  TWeatherDictionary = specialize TDictionary<string, TStat>;

// The main algorithm to process the temp measurements from various weather station
procedure ProcessTempMeasurements(filename: string);


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

constructor TStat.Create(newMin: int64; newMax: int64; newSum: int64; newCount: int64);
begin
  self.min := newMin;
  self.max := newMax;
  self.sum := newSum;
  self.Count := newCount;
end;

function TStat.ToString: string;
var
  minR, meanR, maxR: double; // Store the rounded values prior saving to TStringList.
begin
  {$IFDEF DEBUG}
    Result := Format('Min: %.1f; Mean: %.1f; Maxp: %.1f; Sum: %.1f; Count %d',
      [self.min, (self.sum / self.Count), self.max,
      self.sum, self.Count]);
  {$ENDIF DEBUG}
  // Result := Format('%.1f/%.1f/%.1f', [self.min, (self.sum / self.count), self.max]);
  minR := RoundEx(self.min / 10);
  maxR := RoundEx(self.max / 10);
  meanR := RoundEx(self.sum / self.Count / 10);
  Result := FormatFloat('0.0', minR) + '/' + FormatFloat('0.0', meanR) + '/' + FormatFloat('0.0', maxR);

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


procedure AddCityTemperature(cityName: string; newTemp: int64; var weatherDictionary: TWeatherDictionary);
var
  stat: TStat;
begin
  // If city name exists, modify temp as needed
  if weatherDictionary.ContainsKey(cityName) then
  begin

    {$IFDEF DEBUG}
    WriteLn('City found: ', cityName);
    {$ENDIF DEBUG}

    // Get the temp record
    stat := weatherDictionary[cityName];

    // If the temp lower then min, set the new min.
    if newTemp < stat.min then stat.min := newTemp;

    // If the temp higher than max, set the new max.
    if newTemp > stat.max then stat.max := newTemp;

    // Add count for this city.
    stat.sum := stat.sum + newTemp;

    // Increase the counter
    stat.Count := stat.Count + 1;

    // Update the stat of this city
    weatherDictionary.AddOrSetValue(cityName, stat);
  end;

  // If city name doesn't exist add a new entry
  if not weatherDictionary.ContainsKey(cityName) then
  begin
    weatherDictionary.Add(cityName, TStat.Create(newTemp, newTemp, newTemp, 1));
    {$IFDEF DEBUG}
    WriteLn('Added: ', cityName);
    {$ENDIF DEBUG}
  end;
end;

procedure ProcessTempMeasurements(filename: string);
var
  wd: TWeatherDictionary;
  line, ws, recordedTemp: string;
  weatherStationList: TStringList;
  textFile: System.TextFile;
  isFirstKey: boolean = True;
  delimiterPos: integer;
begin

  // Start a timer
  {$IFDEF DEBUG}
  Stopwatch.StartTimer;
  {$ENDIF}

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

      // Keep reading lines until the end of the file is reached
      while not EOF(textFile) do
      begin
        // Read a line
        ReadLn(textFile, line);

        // If the line start with #, then continue/skip.
        if (line[1] = '#') then continue;

        // Get position of the delimiter
        delimiterPos := Pos(';', line);

        // Else, add an entry into the dictionary.
        // Get the weather station name
        // Using Copy ans POS - as suggested by Gemini AI.
        // This made the program 3 mins fater when processing 1 billion rows.
        ws := Copy(line, 1, delimiterPos - 1);

        // Get the temperature recorded, as string, remove '.' from string float
        // because we want to save it as int64.
        recordedTemp := Copy(line, delimiterPos + 1, Length(line));
        recordedTemp := StringReplace(recordedTemp, '.', '', [rfReplaceAll, rfIgnoreCase]);

        // Add the weather station and the recorded temp (as int64) in the TDictionary
        AddCityTemperature(ws, StrToInt64(recordedTemp), wd);

      end; // end while loop reading line at a time

      // Close the file
      CloseFile(textFile);

    except
      on E: Exception do
        WriteLn('File handling error occurred. Details: ', E.Message);
    end; // End of file reading ////////////////////////////////////////////////

    // Format and sort weather station by name and temp stat ///////////////////
    ws:='';
    for ws in wd.Keys do
    begin
      weatherStationList.Add(ws + '=' + wd[ws].ToString);
    end;
    weatherStationList.CustomSort(@CustomTStringListComparer);

    // Print TStringList - sorted by weather station and temp stat /////////////
    Write('{');
    for ws in weatherStationList do
    begin
      // If it's not the first key, print a comma
      if not isFirstKey then
        Write(', ');

      // Print the weather station and the temp stat
      Write(ws);

      // Set isFirstKey to False after printing the first key
      isFirstKey := False;
    end;

    WriteLn('}');

    {$IFDEF DEBUG}
    WriteLn('DEBUG mode on');
    {$ENDIF DEBUG}

  finally
    weatherStationList.Free;
    wd.Free;
  end; // End of processing TDictionary and TStringList

  // Stop a timer
  {$IFDEF DEBUG}
  Stopwatch.StopTimer;
  Stopwatch.DisplayTimer;
  {$ENDIF}

end;

end.
