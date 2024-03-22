unit uChallengeWithDictionary;
(* as: uChallengeWithDictionary.pas
 * by: David Cornelius
 * on: March, 2024
 * in: Delphi 12 Athens
 * to: Read in cities and temperatures from a text file into a TDictionary collection,
 *     then collate and sort and print out the summaries.
 *
 * Processing time when reading in a 1-billion row file: 5~6 minutes
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
begin
  WeatherCityList := TDictionary<string, TWeatherCity>.Create;
  try
    ChallengeCommon.ReadAndParseAllData(procedure (const CityName: string; const CityTemp: Integer)
      begin
        if WeatherCityList.TryGetValue(CityName, CurrWeatherCity) then
          CurrWeatherCity.AddNewTemp(CityTemp)
        else
          WeatherCityList.Add(CityName, TWeatherCity.Create(CityName, CityTemp));
      end);

      // create the output list
      SortedList := TStringList.Create(TDuplicates.dupAccept, False, True);
      try
        for var WeatherSum in WeatherCityList do
          SortedList.Append(WeatherSum.Value.OutputSumLine);

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
