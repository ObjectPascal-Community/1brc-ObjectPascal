unit uChallengeWithStringList;
(* as: uChallengeWithStringList.pas
 * by: David Cornelius
 * on: March, 2024
 * in: Delphi 12 Athens
 * to: load a text file of cities and temperatures into a TStringList
 *     then collate and sort and print out the summaries
 *
 * Very slow--not an official entry.
 * Results when reading in a 1-billion row file using LoadFromFile: Range Check Error!
 * Results when using LoadFromStream on a 10-million row file: over 23 minutes!
 *)

interface

procedure ChallengeWithStringList;

implementation

uses
  System.SysUtils, System.Classes, System.StrUtils,
  {$IFDEF DEBUG}
  System.Diagnostics,
  {$ENDIF }
  uChallengeCommon;

procedure ChallengeWithStringList;
var
  WeatherStream: TStream;
  WeatherLines: TStringList;
  SortedList: TStringList;
  WeatherCity: string;
  CityTemp: Integer;
  ListCity: Integer;
  CurrLine: Int64;
begin
  WeatherLines := TStringList.Create;
  try
    WeatherLines.Delimiter := ';';
    {$IFDEF DEBUG}
    Writeln('Reading from ' + ChallengeCommon.InputFilename);

    var StopWatch := TStopwatch.StartNew;
    {$ENDIF}

    //WeatherLines.LoadFromFile(ChallengeCommon.InputFilename, TEncoding.UTF8);

    WeatherStream := TFileStream.Create(ChallengeCommon.InputFilename, fmOpenRead);
    try
      WeatherLines.LoadFromStream(WeatherStream, TEncoding.UTF8);
    finally
      WeatherStream.Free;
    end;

    {$IFDEF DEBUG}
    StopWatch.Stop;
    Writeln(Format('Loaded %d lines from %s in %d milliseconds', [WeatherLines.Count, ChallengeCommon.InputFilename,
                         StopWatch.ElapsedMilliseconds]));
    {$ENDIF}

    SortedList := TStringList.Create(TDuplicates.dupIgnore, False, True);
    try
      // process all rows
      for CurrLine := 0 to WeatherLines.Count - 1 do begin
        // parse the data and add to our list
        if ChallengeCommon.SplitCityTemp(WeatherLines[CurrLine], WeatherCity, CityTemp) then begin
          ListCity := SortedList.IndexOf(WeatherCity);
          if ListCity = -1 then
            SortedList.AddObject(WeatherCity, TWeatherCity.Create(WeatherCity, CityTemp))
          else
            TWeatherCity(SortedList.Objects[ListCity]).AddNewTemp(CityTemp);
        end;
      end;
      SortedList.UseLocale := False;
      SortedList.Sort;

      Write('{');
      Write(Trim(TWeatherCity(SortedList.Objects[0]).OutputSumLine(True)));
      for var i := 1 to SortedList.Count - 1 do
        Write(TWeatherCity(SortedList.Objects[i]).OutputSumLine(False));
      Writeln('}');
      {$IFDEF DEBUG}
      Writeln('Unique Stations: ', SortedList.Count);
      {$ENDIF}
    finally
      SortedList.Free;
    end;
  finally
    WeatherLines.Free;
  end;
end;

end.
