unit uChallengeWithDictionary;
(* as: uChallengeWithDictionary.pas
 * by: David Cornelius
 * on: March, 2024
 * in: Delphi 12 Athens
 * to: Read in cities and temperatures from a text file into a TDictionary collection,
 *     then collate and sort and print out the summaries.
 *
 * NOTE: This is a single-threaded process.
 *
 * Processing time when reading in a 1-billion row file: 9 minutes (22½ minutes on WSL-Windows Subsystem for Linux)
 *)

interface

procedure ChallengeWithDictionary;

implementation

uses
  System.Classes, System.SysUtils, Generics.Collections, System.StrUtils,
  uChallengeCommon;

type
  TWeatherCityList = TDictionary<string, TWeatherCity>;

var
  WeatherCityList: TWeatherCityList;

procedure ChallengeWithDictionary;
var
  SortedList: TStringList;
  CurrWeatherCity: TWeatherCity;
  TempLine: string;
begin
  WeatherCityList := TDictionary<string, TWeatherCity>.Create;
  try
    ChallengeCommon.ReadAndParseAllData(procedure (const CityName: string; const CityTemp: Double)
      begin
        if WeatherCityList.TryGetValue(CityName, CurrWeatherCity) then
          CurrWeatherCity.AddNewTemp(CityTemp)
        else
          WeatherCityList.Add(CityName, TWeatherCity.Create(CityName, CityTemp));
      end);

      {$IFDEF DEBUG}
      Writeln(Format('Loaded %d lines from %s', [WeatherCityList.Count, ChallengeCommon.InputFilename]));
      {$ENDIF}

      // create the output list
      SortedList := TStringList.Create;
      try
        for var WeatherSum in WeatherCityList do begin
          TempLine := Format(' %s=%0.1f/%0.1f/%0.1f', [WeatherSum.Value.CityName,
                                                      WeatherSum.Value.MinTemp,
                                                      WeatherSum.Value.Mean,
                                                      WeatherSum.Value.MaxTemp]);
          SortedList.Add(TempLine);
        end;

        // sort and write out the data
        SortedList.Sort;
        SortedList.QuoteChar := #0;
        Writeln('{', Trim(SortedList.DelimitedText), '}');
      finally
        SortedList.Free;
      end;
      {$IFDEF DEBUG}
      Writeln('Unique Stations: ', WeatherCityList.Count);
      {$ENDIF}
    finally
      WeatherCityList.Free;
    end;
end;

end.
