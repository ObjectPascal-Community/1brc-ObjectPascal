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
  System.Classes, System.SysUtils, System.Generics.Collections, System.StrUtils, System.Generics.Defaults,
  uChallengeCommon;

type
  TWeatherCityList = TDictionary<string, TWeatherCity>;

  TUTF8CustomComparer = class(TInterfacedObject, IComparer<string>)
  public
    function Compare(const Left, Right: string): Integer;
  end;

var
  WeatherCityList: TWeatherCityList;

procedure ChallengeWithDictionary;
var
  CityArray: TArray<string>;
  CurrWeatherCity: TWeatherCity;
  FirstOutput: Boolean;
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
      CityArray := WeatherCityList.Keys.ToArray;
      TArray.Sort<string>(CityArray, TUTF8CustomComparer.Create);
      FirstOutput := True;
      Write('{');
      for var City in CityArray do
      begin
        Write(WeatherCityList.Items[City].OutputSumLine(FirstOutput));
        FirstOutput := False;
      end;
      Writeln('}');
      {$IFDEF DEBUG}
      Writeln('Unique Stations: ', WeatherCityList.Count);
      {$ENDIF}
    finally
      WeatherCityList.Free;
    end;
end;

{ TUTF8CustomComparer }

function TUTF8CustomComparer.Compare(const Left, Right: string): Integer;
begin
  Result := CompareStr(Left, Right);
end;

end.
