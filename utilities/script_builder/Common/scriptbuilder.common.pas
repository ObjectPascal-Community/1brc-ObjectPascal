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
{ EPatternsLengthDonMatch }
  EPatternsLengthDonMatch = Exception;
{ TBuilder }
  TBuilder = class(TObject)
  private
    FConfig: TConfig;
    FScriptStream: TFileStream;
    FScriptFile: String;

    function GenerateProgressBar(APBPosition, APBMax, APBWIdth: Integer): String;
    function StringsReplace(
      const AString: String;
      const AOLdPattern, ANewPattern: TStringArray;
      const AFlags: TReplaceFlags
    ): String;
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

  cLazbuildDefault     = '%s -B "%s"';
  cLazbuildRelease     = '%s -B --bm="Release" "%s"';

  cReplaceName         = '[[name]]';
  cReplaceJSONResults  = '[[results-json]]';
  cReplaceEntryBinary  = '[[entry-binary]]';
  cReplaceEntryInput   = '[[input]]';
  cReplaceEntryThreads = '[[threads]]';

  cBaselineBinary      = 'baseline';
  cCompilerFPC         = 'fpc';
  //  cCompilerDelphi      = 'delphi';
  cSSD                 = 'SSD';
  cHDD                 = 'HDD';

resourcestring
  rsEPatternsLengthDOntMatch = 'Patterns length does not match';

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

function TBuilder.StringsReplace(
  const AString: String;
  const AOLdPattern, ANewPattern: TStringArray;
  const AFlags: TReplaceFlags
): String;
var
  index: Integer;
begin
  Result:= AString;
  if Length(AOLdPattern) <> Length(ANewPattern) then
    raise EPatternsLengthDonMatch.Create(rsEPatternsLengthDOntMatch);
  for index:= Low(AOLdPattern) to High(AOLdPattern) do
  begin
    Result:= StringReplace(Result, AOLdPattern[index], ANewPattern[index], AFlags);
  end;
end;

procedure TBuilder.BuildCompileScriptBash;
var
  index: Integer;
  //entry: TEntry;
  line: String;
begin
  FScriptFile:= IncludeTrailingPathDelimiter(FConfig.RootFolder) + cCompileBash;
  FScriptStream:= TFileStream.Create(FScriptFile, fmCreate);
  try
    line:= '#!/bin/bash' + LineEnding + LineEnding;
    line:= line + 'echo "******** Compile All ********"' + LineEnding;
    line:= line + 'echo' + LineEnding + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    //for entry in FConfig.Entries do
    begin
      Write(GenerateProgressBar(index+1, FConfig.Entries.Count, 50), lineBreak);
      if not FConfig.Entries[index].Active then continue;
      if FConfig.Entries[index].Compiler <> cCompilerFPC then continue;
      //if FConfig.Entries[index].EntryBinary = cBaselineBinary then continue;
      line:= line + 'echo "===== '+ FConfig.Entries[index].Name +' ======"' + LineEnding;
      if FConfig.Entries[index].HasRelease then
      begin
       line:= line  +
       Format(cLazbuildRelease, [
         FConfig.Lazbuild,
         ExpandFileName(
           ConcatPaths([
             IncludeTrailingPathDelimiter(FConfig.EntriesFolder),
             IncludeTrailingPathDelimiter(FConfig.Entries[index].EntryFolder),
             FConfig.Entries[index].LPI
           ])
         )
       ] ) +
       LineEnding;
      end
      else
      begin
        line:= line  +
        Format(cLazbuildDefault, [
          FConfig.Lazbuild,
          ExpandFileName(
            ConcatPaths([
              IncludeTrailingPathDelimiter(FConfig.EntriesFolder),
              IncludeTrailingPathDelimiter(FConfig.Entries[index].EntryFolder),
              FConfig.Entries[index].LPI
            ])
          )
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

{$IFNDEF UNIX}
procedure TBuilder.BuildCompileScriptPowerShell;
begin
  { #todo 99 -ogcarreno : Using the command line compiler, compile the binary for Linux 64b }
  { #todo 99 -ogcarreno : Using scp copy the resulting binary to Linux }
end;
{$ENDIF}

procedure TBuilder.BuildTestScriptBash;
var
  index: Integer;
  line, tmpStr: String;
begin
  FScriptFile:= IncludeTrailingPathDelimiter(FConfig.RootFolder) + cTestBash;
  FScriptStream:= TFileStream.Create(FScriptFile, fmCreate);
  try
    line:= '#!/bin/bash' + LineEnding + LineEnding;
    line:= line + 'echo "******** Test All ********"' + LineEnding;
    line:= line + 'echo' + LineEnding + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      Write(GenerateProgressBar(index+1, FConfig.Entries.Count, 50), lineBreak);
      if not FConfig.Entries[index].Active then continue;
      //if FConfig.Entries[index].EntryBinary = cBaselineBinary then continue;
      line:= line + 'echo "===== '+ FConfig.Entries[index].Name +' ======"' + LineEnding;
      tmpStr:= Format('%s%s %s', [
        IncludeTrailingPathDelimiter(FConfig.BinFolder),
        FConfig.Entries[index].EntryBinary,
        FConfig.Entries[index].RunParams
      ]);
      tmpStr:= StringsReplace(
        tmpStr,
        [
          cReplaceEntryInput,
          cReplaceEntryThreads
        ],
        [
          FConfig.InputSSD,
          IntToStr(FConfig.Entries[index].Threads)
        ],
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
        FConfig.OutputHash
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
    line:= line + 'echo "******** Run All ********"' + LineEnding;
    line:= line + 'echo' + LineEnding + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      Write(GenerateProgressBar(index+1, FConfig.Entries.Count, 50), lineBreak);
      if not FConfig.Entries[index].Active then continue;
      if FConfig.Entries[index].EntryBinary = cBaselineBinary then continue;
      line:= line + 'echo "===== '+ FConfig.Entries[index].Name +' ======"' + LineEnding;
      // Run for SSD
      tmpStr:= StringsReplace(
        FConfig.Hyperfine,
        [
          cReplaceName,
          cReplaceJSONResults,
          cReplaceEntryBinary
        ],
        [
          FConfig.Entries[index].EntryBinary,
          Format('%s%s', [
            IncludeTrailingPathDelimiter(FConfig.ResultsFolder),
            FConfig.Entries[index].EntryBinary + '-1_000_000_000-SSD.json'
          ]),
          Format('%s%s %s', [
            IncludeTrailingPathDelimiter(FConfig.BinFolder),
            FConfig.Entries[index].EntryBinary,
            FConfig.Entries[index].RunParams
          ])
        ],
        [rfReplaceAll]
      );
      tmpStr:= StringsReplace(
        tmpStr,
        [
          cReplaceEntryInput,
          cReplaceEntryThreads
        ],
        [
          FConfig.InputSSD,
          IntToStr(FConfig.Entries[index].Threads)
        ],
        [rfReplaceAll]
      );
      line:= line + 'echo "-- SSD --"' + LineEnding + tmpStr + LineEnding;
      // Run for HDD
      tmpStr:= StringsReplace(
        tmpStr,
        [
          FConfig.InputSSD,
          cSSD
        ],
        [
          FConfig.InputHDD,
          cHDD
        ],
        [rfReplaceAll]
      );
      line:= line + 'echo "-- HDD --"' + LineEnding + tmpStr + LineEnding;
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
