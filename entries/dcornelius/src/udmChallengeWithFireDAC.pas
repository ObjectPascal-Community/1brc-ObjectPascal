unit udmChallengeWithFireDAC;
(* as: udmChallengeWithFireDAC.pas/dfm
 * by: David Cornelius
 * on: March, 2024
 * in: Delphi 12 Athens
 * to: Read in cities and temperatures from a text file into an in-memory FireDAC table
 *     then use SQL to extract a sorted summary and print it out.
 *
 * NOTE: This is a single-threaded process.
 *
 * Processing time when reading in a 1-billion row file: ___
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
  uChallengeCommon;

procedure ChallengeWithFireDAC;
var
  TotalOutput: Longint;
  dmChallengeWithFireDAC: TdmChallengeWithFireDAC;

  function OutputLine: string;
  begin
    var MeanTemp := ChallengeCommon.PascalRound(dmChallengeWithFireDAC.qryCityTempsSumTemp.AsFloat /
                                                dmChallengeWithFireDAC.qryCityTempsTempCount.AsFloat);
    Result := Format('%s=%0.1f/%0.1f/%0.1f',
                       [dmChallengeWithFireDAC.qryCityTempsCityName.AsString,
                        dmChallengeWithFireDAC.qryCityTempsMinTemp.AsFloat,
                        MeanTemp,
                        dmChallengeWithFireDAC.qryCityTempsMaxTemp.AsFloat]);
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
    if dmChallengeWithFireDAC.qryCityTemps.RecordCount > 0 then begin
      dmChallengeWithFireDAC.qryCityTemps.First;
      Write('{', OutputLine);
      if dmChallengeWithFireDAC.qryCityTemps.RecordCount > 1 then begin
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
