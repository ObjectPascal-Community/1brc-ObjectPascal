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
  Stopwatch;

type
  // Create a record of temperature stats
  TStat = record
  var
    min: single;
    max: single;
    sum: single;
    count: word;
  public
    constructor Create(newMin: single;
                       newMax: single;
                       newSum: single;
                       newCount: word);
    function ToString: string;
  end;

type
  // Create a dictionary
  TWeatherDictionary = specialize TDictionary<string, TStat>;


{// A helper function to add a city temperature into the TWeatherDictionary
procedure AddCityTemperature(cityName: string;
                             newTemp: single;
                             var weatherDictionary: TWeatherDictionary);
}

// The main algorithm to process the temp measurements from various weather station
procedure ProcessTempMeasurements(filename: string);


implementation

constructor TStat.Create(newMin: single;
                         newMax: single;
                         newSum: single;
                         newCount: word);
begin
  self.min := newMin;
  self.max := newMax;
  self.sum := newSum;
  self.count := newCount;
end;

function TStat.ToString: string;
begin
  {$IFDEF DEBUG}
    Result := Format('Min: %.1f; Mean: %.1f; Maxp: %.1f; Sum: %.1f; Count %d',
      [self.min, (self.sum / self.Count), self.max,
      self.sum, self.Count]);
  {$ENDIF DEBUG}
  // Result := Format('%.1f/%.1f/%.1f', [self.min, (self.sum / self.count), self.max]);
  Result := FormatFloat('0.0', self.min) + '/' + FormatFloat('0.0', (self.sum/self.count)) + '/' + FormatFloat('0.0', self.max)

end;

{
  A custom comparer for TStringList.

  The following procedure Written by Székely Balázs for the 1BRC for Object Pascal.
  URL: https://github.com/gcarreno/1brc-ObjectPascal/tree/main
}
function CustomTStringListComparer(AList: TStringList; AIndex1, AIndex2: Integer): Integer;
var
  Pos1, Pos2: Integer;
  Str1, Str2: String;
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
    Result :=  CompareStr(Str1, Str2);
  end;
end;

procedure AddCityTemperature(cityName: string;
                             newTemp: single;
                             var weatherDictionary: TWeatherDictionary);
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
  line, ws: string;
  lineSeparated: array of string;
  weatherStationList: TStringList;
  textFile: System.TextFile;
  isFirstKey: boolean = True;
begin

  // Start a timer
  // Stopwatch.StartTimer;

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

        // Else, add an entry into the dictionary.
        lineSeparated := line.Split([';']);
        AddCityTemperature(lineSeparated[0], StrToFloat(lineSeparated[1]), wd);

      end; // end while loop reading line at a time

      // Close the file
      CloseFile(textFile);

    except
      on E: Exception do
        WriteLn('File handling error occurred. Details: ', E.Message);
    end; // End of file reading ////////////////////////////////////////////////

    // Format and sort weather station by name and temp stat ///////////////////
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

    Write('}');

    {$IFDEF DEBUG}
    WriteLn('DEBUG mode on');
    {$ENDIF DEBUG}

  finally
    weatherStationList.Free;
    wd.Free;
  end; // End of processing TDictionary and TStringList

  // Stop a timer
  // Stopwatch.StopTimer;
  // Stopwatch.DisplayTimer;

end;

end.
