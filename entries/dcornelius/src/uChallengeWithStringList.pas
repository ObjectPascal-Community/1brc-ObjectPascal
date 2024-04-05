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
  SortedList: TStringList;
  ListCity: Integer;
begin
  SortedList := TStringList.Create(TDuplicates.dupAccept, False, True);
  try
    ChallengeCommon.ReadAndParseAllData(procedure (const CityName: string; const CityTemp: Integer)
      begin
        ListCity := SortedList.IndexOf(CityName);
        if ListCity = -1 then
          SortedList.AddObject(CityName, TWeatherCity.Create(CityName, CityTemp))
        else
          TWeatherCity(SortedList.Objects[ListCity]).AddNewTemp(CityTemp);
      end);

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
end;

end.
