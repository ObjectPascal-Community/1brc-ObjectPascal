unit uChallengeWithStringList;
(* as: uChallengeWithStringList.pas
 * by: David Cornelius
 * on: March, 2024
 * in: Delphi 12 Athens
 * to: load a text file of cities and temperatures into a TStringList
 *     then collate and sort and print out the summaries
 *
 * Results when reading in a 1-billion row file: Range Check Error!
 *)

interface

procedure ChallengeWithStringList;

implementation

uses
  System.SysUtils, System.Classes, System.StrUtils,
  uChallengeCommon;

procedure ChallengeWithStringList;
var
  WeatherLines: TStringList;
  SortedList: TStringList;
  WeatherCity: string;
  CityTemp: Double;
  ListCity: Integer;
  CurrLine: Int64;
begin
  WeatherLines := TStringList.Create;
  try
    WeatherLines.Delimiter := ';';
    {$IFDEF DEBUG}
    Writeln('Reading from ' + ChallengeCommon.InputFilename);
    {$ENDIF}
    WeatherLines.LoadFromFile(ChallengeCommon.InputFilename);
    {$IFDEF DEBUG}
    Writeln(Format('Loaded %d lines from %s', [WeatherLines.Count, ChallengeCommon.InputFilename]));
    {$ENDIF}


    SortedList := TStringList.Create;
    try
      SortedList.Sorted := True;
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

      Write('{');
      for var i := 0 to SortedList.Count - 1 do begin
        {$IFDEF DEBUG}
        Write(Format('%s=%0.1f/%0.1f/%0.1f (%d stores)%s', [TWeatherCity(SortedList.Objects[i]).CityName,
                                                            TWeatherCity(SortedList.Objects[i]).MinTemp,
                                                            TWeatherCity(SortedList.Objects[i]).Mean,
                                                            TWeatherCity(SortedList.Objects[i]).MaxTemp,
                                                            TWeatherCity(SortedList.Objects[i]).DataCount,
                                                            IfThen(i = SortedList.Count - 1, '', ', ')]));
        {$ELSE}
        Write(Format('%s=%0.1f/%0.1f/%0.1f%s', [TWeatherCity(SortedList.Objects[i]).CityName,
                                                TWeatherCity(SortedList.Objects[i]).MinTemp,
                                                TWeatherCity(SortedList.Objects[i]).Mean,
                                                TWeatherCity(SortedList.Objects[i]).MaxTemp,
                                                IfThen(i = SortedList.Count - 1, '', ',')]));
        {$ENDIF}
      end;
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
