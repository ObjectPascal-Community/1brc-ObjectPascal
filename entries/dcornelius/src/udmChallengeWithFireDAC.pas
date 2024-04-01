unit udmChallengeWithFireDAC;
(* as: udmChallengeWithFireDAC.pas/dfm
 * by: David Cornelius
 * on: March, 2024
 * in: Delphi 12 Athens
 * to: Read in cities and temperatures from a text file into an in-memory FireDAC table
 *     then use SQL to extract a sorted summary and print it out.
 *
 * DO NOT USE THIS METHOD! It's slow even for small tests of 1,000 or 100,000 records but testing of a 1 billion row
 * file took so long, that after 26 HOURS, I killed the process!  Someday, I might come back and try using the
 * BatchMove components to see if it loads faster.
 *)

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error,
  FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.Phys.SQLiteDef, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.ConsoleUI.Wait,
  FireDAC.Phys.SQLiteVDataSet, FireDAC.DApt;

type
  TdmChallengeWithFireDAC = class(TDataModule)
    tblWeatherData: TFDMemTable;
    tblWeatherDataCityName: TStringField;
    tblWeatherDataTemperature: TFloatField;
    FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink;
    FDConnection: TFDConnection;
    FDLocalSQL: TFDLocalSQL;
    qryCityTemps: TFDQuery;
    qryCityTempsCityName: TStringField;
    qryCityTempsTempCount: TLargeintField;
    qryCityTempsMinTemp: TFloatField;
    qryCityTempsMaxTemp: TFloatField;
    qryCityTempsSumTemp: TFloatField;
  end;

procedure ChallengeWithFireDAC;


implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  System.Math,
  uChallengeCommon;

procedure ChallengeWithFireDAC;
var
  TotalOutput: Longint;
  dmChallengeWithFireDAC: TdmChallengeWithFireDAC;

  function OutputLine: string;
  begin
    var MeanTemp := Ceil(dmChallengeWithFireDAC.qryCityTempsSumTemp.AsFloat /
                         dmChallengeWithFireDAC.qryCityTempsTempCount.AsFloat / 10.0);
    Result := Format('%s=%0.1f/%0.1f/%0.1f',
                       [dmChallengeWithFireDAC.qryCityTempsCityName.AsString,
                        dmChallengeWithFireDAC.qryCityTempsMinTemp.AsFloat / 10.0,
                        MeanTemp / 10.0,
                        dmChallengeWithFireDAC.qryCityTempsMaxTemp.AsFloat / 10.0]);
    Inc(TotalOutput);
  end;

begin
  dmChallengeWithFireDAC := TdmChallengeWithFireDAC.Create(nil);
  try
    dmChallengeWithFireDAC.tblWeatherData.Open;
    dmChallengeWithFireDAC.tblWeatherData.EmptyDataSet;

    ChallengeCommon.ReadAndParseAllData(procedure (const CityName: string; const CityTemp: Integer)
      begin
        dmChallengeWithFireDAC.tblWeatherData.InsertRecord([CityName, CityTemp]);
      end);

    dmChallengeWithFireDAC.qryCityTemps.Close;
    dmChallengeWithFireDAC.qryCityTemps.Open;

    TotalOutput := 0;
    if dmChallengeWithFireDAC.qryCityTemps.RecordCount > 0 then
    begin
      dmChallengeWithFireDAC.qryCityTemps.First;
      Write('{', OutputLine);
      if dmChallengeWithFireDAC.qryCityTemps.RecordCount > 1 then
      begin
        repeat
          dmChallengeWithFireDAC.qryCityTemps.Next;
          if not dmChallengeWithFireDAC.qryCityTemps.Eof then
            Write(', ', OutputLine);
        until dmChallengeWithFireDAC.qryCityTemps.Eof;
        Writeln('}');
      end else
        Writeln('}');
    end;

    {$IFDEF DEBUG}
    Writeln('Unique Stations: ', TotalOutput);
    {$ENDIF}

    dmChallengeWithFireDAC.qryCityTemps.Close;
  finally
    dmChallengeWithFireDAC.Free;
  end;
end;

end.
