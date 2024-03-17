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
    FScriptStream: TFileStream;
    FScriptFile: String;

    function GenerateProgressBar(APBPosition, APBMax, APBWIdth: Integer): String;
  protected
  public
    constructor Create(AConfigFile: String);
    destructor Destroy; override;

    {$IFDEF UNIX}
    procedure BuildCompileScriptBash;
    procedure BuildTestScriptBash;
    procedure BuildRunScriptBash;
    {$ELSE}
    procedure BuildCompileScriptPowerShell;
    procedure BuildTestScriptPowerShell;
    procedure BuildRunScriptPowerShell;
    {$ENDIF}
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

const
  lineBreak = #13;
  cCompileBash         = 'compile_all.sh';
  cTestBash            = 'test_all.sh';
  cRunBash             = 'run_all.sh';

  cReplaceName         = '[[name]]';
  cReplaceJSONResults  = '[[results-json]]';
  cReplaceEntryBinary  = '[[entry-binary]]';
  cReplaceEntryInput   = '[[input]]';
  cReplaceEntryThreads = '[[threads]]';

  cOfficialOutputHash  = '0000000000000000000000000000000000000000000000000000000000000000';

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

function TBuilder.GenerateProgressBar(APBPosition, APBMax, APBWIdth: Integer
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

procedure TBuilder.BuildCompileScriptBash;
var
  index: Integer;
  line: String;
begin
  FScriptFile:= IncludeTrailingPathDelimiter(FConfig.RootFolder) + cCompileBash;
  FScriptStream:= TFileStream.Create(FScriptFile, fmCreate);
  try
    line:= '#!/bin/bash' + LineEnding + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      Write(GenerateProgressBar(index+1, FConfig.Entries.Count, 50), lineBreak);
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
    FScriptStream.WriteBuffer(line[1], Length(line));
  finally
    FScriptStream.Free;
  end;
  FpChmod(PChar(FScriptFile), &775);
end;

procedure TBuilder.BuildTestScriptBash;
var
  index: Integer;
  line, tmpStr: String;
begin
  FScriptFile:= IncludeTrailingPathDelimiter(FConfig.RootFolder) + cTestBash;
  FScriptStream:= TFileStream.Create(FScriptFile, fmCreate);
  try
    line:= '#!/bin/bash' + LineEnding + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      Write(GenerateProgressBar(index+1, FConfig.Entries.Count, 50), lineBreak);
      line:= line + 'echo "===== '+ FConfig.Entries[index].Name +' ======"' + LineEnding;
      tmpStr:= Format('%s%s %s', [
        IncludeTrailingPathDelimiter(FConfig.BinFolder),
        FConfig.Entries[index].EntryBinary,
        FConfig.Entries[index].RunParams
      ]);
      tmpStr:= StringReplace(
        tmpStr,
        cReplaceEntryInput,
        FConfig.InputSSD,
        [rfReplaceAll]
      );
      tmpStr:= StringReplace(
        tmpStr,
        cReplaceEntryThreads,
        IntToStr(FConfig.Entries[index].Threads),
        [rfReplaceAll]
      );
      tmpStr:= Format('%s > %s%s.output', [
        tmpStr,
        IncludeTrailingPathDelimiter(FConfig.ResultsFolder),
        FConfig.Entries[index].EntryBinary
      ]);
      line:= line + tmpStr + LineEnding;
      tmpStr:= Format('sha256sum %s%s.output',[
        IncludeTrailingPathDelimiter(FConfig.ResultsFolder),
        FConfig.Entries[index].EntryBinary
      ]);
      line:= line + tmpStr + LineEnding;
      line:= line + Format('echo "%s  Official Output Hash"',[
        cOfficialOutputHash
      ]) + LineEnding;
      line:= line + 'echo "==========="' + LineEnding;
      line:= line + 'echo' + LineEnding + LineEnding;
    end;
    FScriptStream.WriteBuffer(line[1], Length(line));
  finally
    FScriptStream.Free;
  end;
  FpChmod(PChar(FScriptFile), &775);
end;

procedure TBuilder.BuildRunScriptBash;
var
  index: Integer;
  line, tmpStr: String;
begin
  FScriptFile:= IncludeTrailingPathDelimiter(FConfig.RootFolder) + cRunBash;
  FScriptStream:= TFileStream.Create(FScriptFile, fmCreate);
  try
    line:= '#!/bin/bash' + LineEnding + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      Write(GenerateProgressBar(index+1, FConfig.Entries.Count, 50), lineBreak);
      line:= line + 'echo "===== '+ FConfig.Entries[index].Name +' ======"' + LineEnding;
      tmpStr := StringReplace(
        FConfig.Hyperfine,
        cReplaceName,
        FConfig.Entries[index].EntryBinary,
        [rfReplaceAll]
      );
      tmpStr := StringReplace(
        tmpStr,
        cReplaceJSONResults,
        Format('%s%s', [
          IncludeTrailingPathDelimiter(FConfig.ResultsFolder),
          FConfig.Entries[index].EntryBinary + '-1_000_000_000.json'
        ]),
        [rfReplaceAll]
      );
      tmpStr := StringReplace(
        tmpStr,
        cReplaceEntryBinary,
        Format('%s%s %s', [
          IncludeTrailingPathDelimiter(FConfig.BinFolder),
          FConfig.Entries[index].EntryBinary,
          FConfig.Entries[index].RunParams
        ]),
        [rfReplaceAll]
      );
      tmpStr := StringReplace(
        tmpStr,
        cReplaceEntryInput,
        FConfig.InputSSD,
        [rfReplaceAll]
      );
      tmpStr := StringReplace(
        tmpStr,
        cReplaceEntryThreads,
        IntToStr(FConfig.Entries[index].Threads),
        [rfReplaceAll]
      );
      line:= line + tmpStr + LineEnding;
      line:= line + 'echo "==========="' + LineEnding;
      line:= line + 'echo' + LineEnding + LineEnding;
    end;
    FScriptStream.WriteBuffer(line[1], Length(line));
  finally
    FScriptStream.Free;
  end;
  FpChmod(PChar(FScriptFile), &775);
end;

end.
