unit ResultsGenerator.Common;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes
, SysUtils
, Utilities.Data.Config
, Utilities.Data.Entries
;

type
{ TResults }
  TResults = class(TObject)
  private
    FConfig: TConfig;

    function GenerateProgressBar(APBPosition, APBMax, APBWIdth: Integer): String;
  protected
  public
    constructor Create(AConfigFile: String);
    destructor Destroy; override;

    {$IFDEF UNIX}
    {$ELSE}
    {$ENDIF}
  published
  end;

implementation

uses
{$IFDEF FPC}
  fpjson
, jsonparser
{$ELSE}
{$ENDIF}
;

const
  lineBreak = #13;

{ TResults }

constructor TResults.Create(AConfigFile: String);
var
  configStream: TFileStream;
  configJSONData: TJSONData;
begin
  { #todo 99 -ogcarreno : Config file must be used here }
  configStream:= TFileStream.Create(AConfigFile, fmOpenRead);
  try
    configJSONData:= GetJSON(configStream);
    FConfig:= TConfig.Create(configJSONData);
    configJSONData.Free;
  finally
    configStream.Free;
  end;
end;

destructor TResults.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
end;

function TResults.GenerateProgressBar(APBPosition, APBMax, APBWIdth: Integer
  ): String;
var
  percentDone: Double;
  filled: Integer;
begin
  percentDone := 100 * (APBPosition / APBMax);
  filled := trunc(APBWIdth * (percentDone / 100));
  Result := '[';
  Result := Result + StringOfChar('#', filled);
  Result := Result + StringOfChar('-', APBWIdth - filled);
  Result := Result + Format('] %5.2f %%', [percentDone]);
end;

end.
