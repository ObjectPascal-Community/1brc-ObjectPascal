unit ScriptBuilder.Common;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes
, SysUtils
, ScriptBuilder.Data.Config
, ScriptBuilder.Data.Entries
;

type
{ TBuilder }
  TBuilder = class(TObject)
  private
    FConfig: TConfig;
  protected
  public
    constructor Create(AConfigFile: String);
    destructor Destroy; override;

    procedure BuildCompileScriptBash;
  published
  end;

implementation

uses
  {$IFDEF FPC}
  fpjson
, jsonparser
, BaseUnix
  {$ELSE}
  {$ENDIF}
;

{ TBuilder }

constructor TBuilder.Create(AConfigFile: String);
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

destructor TBuilder.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
end;

procedure TBuilder.BuildCompileScriptBash;
var
  scriptStream: TFileStream;
  index: Integer;
  scriptFile, line: String;
begin
  scriptFile:= IncludeTrailingPathDelimiter(FConfig.RootFolder) + 'compile_all.sh';
  scriptStream:= TFileStream.Create(scriptFile, fmCreate);
  try
    line:= '#!/bin/bash' + LineEnding;
    line:= line + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      line:= line + 'echo "===== '+ FConfig.Entries[index].Name +' ======"' + LineEnding;
      if FConfig.Entries[index].HasRelease then
      begin
       line:= line  +
       Format('%s -B --bm="Release" "%s%s%s"', [
         FConfig.Lazbuild,
         IncludeTrailingPathDelimiter(FConfig.EntriesFolder),
         IncludeTrailingPathDelimiter(FConfig.Entries[index].EntryFolder),
         FConfig.Entries[index].LPI
       ] ) +
       LineEnding;
      end
      else
      begin
        line:= line  +
        Format('%s -B "%s%s%s"', [
          FConfig.Lazbuild,
          IncludeTrailingPathDelimiter(FConfig.EntriesFolder),
          IncludeTrailingPathDelimiter(FConfig.Entries[index].EntryFolder),
          FConfig.Entries[index].LPI
        ] ) +
        LineEnding;
      end;
      line:= line + 'echo "==========="' + LineEnding;
      line:= line + 'echo' + LineEnding + LineEnding;
    end;
    scriptStream.WriteBuffer(line[1], Length(line));
  finally
    scriptStream.Free;
  end;
  FpChmod(PChar(scriptFile), &775);
end;

end.
