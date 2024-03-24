unit ScriptBuilder.Common;

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

    procedure BuildCompileScriptBash;
    procedure BuildTestScriptBash;
    procedure BuildRunScriptBash;
  published
  end;

implementation

uses
  {$IFDEF FPC}
  fpjson
, jsonparser
  {$IFDEF UNIX}
, BaseUnix
  {$ENDIF}
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
  cCompilerDelphi      = 'delphi';
  cSSD                 = 'SSD';

resourcestring
  rsEPatternsLengthDOntMatch = 'Patterns length does not match';

{ TBuilder }

constructor TBuilder.Create(AConfigFile: String);
var
  configStream: TFileStream;
  configJSONData: TJSONData;
begin
  configStream:= TFileStream.Create(AConfigFile, fmOpenRead);
  try
    configJSONData:= GetJSON(configStream);
    try
      FConfig:= TConfig.Create(configJSONData);
    finally
      configJSONData.Free;
    end;
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
  line: TJSONStringType;
begin
  FScriptFile:= IncludeTrailingPathDelimiter(FConfig.RootFolder) + cCompileBash;
  FScriptStream:= TFileStream.Create(FScriptFile, fmCreate);
  try
    line:= '#!/bin/bash' + LineEnding + LineEnding;
    line:= line + 'echo "******** Compile ********"' + LineEnding;
    line:= line + 'echo' + LineEnding + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    //for entry in FConfig.Entries do
    begin
      Write(GenerateProgressBar(index+1, FConfig.Entries.Count, 50), lineBreak);
      if not FConfig.Entries[index].Active then continue;
      {$IFDEF UNIX}
      if FConfig.Entries[index].Compiler <> cCompilerFPC then continue;
      {$ELSE}
      if FConfig.Entries[index].Compiler <> cCompilerDelphi then continue;
      {$ENDIF}
      //if FConfig.Entries[index].EntryBinary = cBaselineBinary then continue;
      line:= line + 'function ' + FConfig.Entries[index].EntryBinary + '() {' + LineEnding + LineEnding;
      line:= line + '  echo "===== '+ FConfig.Entries[index].Name +' ======"' + LineEnding;
      {$IFDEF UNIX}
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
      {$ELSE}
      line:= line + '  # Needs the Delphi command line stuff' + LineEnding;
      {$ENDIF}
      line:= line + '  echo "==========="' + LineEnding;
      line:= line + '  echo' + LineEnding + LineEnding + '}' + LineEnding + LineEnding;
    end;
    line:= line + 'if [ $1 == "" ];then'  + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      if not FConfig.Entries[index].Active then continue;
      {$IFDEF UNIX}
      if FConfig.Entries[index].Compiler <> cCompilerFPC then continue;
      {$ELSE}
      if FConfig.Entries[index].Compiler <> cCompilerDelphi then continue;
      {$ENDIF}
      line:= line + '  ' + FConfig.Entries[index].EntryBinary + LineEnding;
    end;
    line:= line + 'else'  + LineEnding;
    line:= line + '  case $1 in'  + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      if not FConfig.Entries[index].Active then continue;
      {$IFDEF UNIX}
      if FConfig.Entries[index].Compiler <> cCompilerFPC then continue;
      {$ELSE}
      if FConfig.Entries[index].Compiler <> cCompilerDelphi then continue;
      {$ENDIF}
      line:= line + '    ' + FConfig.Entries[index].EntryBinary + ')' + LineEnding;
      line:= line + '      ' + FConfig.Entries[index].EntryBinary + LineEnding;
      line:= line + '      ;;'  + LineEnding;
    end;
    line:= line + '    *)'  + LineEnding;
    line:= line + '      echo "Do not recognise $1"'  + LineEnding;
    line:= line + '      ;;'  + LineEnding;
    line:= line + '  esac'  + LineEnding;
    line:= line + 'fi'  + LineEnding;
    FScriptStream.WriteBuffer(line[1], Length(line));
  finally
    FScriptStream.Free;
  end;
  {$IFDEF UNIX}
  FpChmod(PChar(FScriptFile), &775);
  {$ENDIF}
end;

procedure TBuilder.BuildTestScriptBash;
var
  index: Integer;
  line, tmpStr: TJSONStringType;
begin
  FScriptFile:= IncludeTrailingPathDelimiter(FConfig.RootFolder) + cTestBash;
  FScriptStream:= TFileStream.Create(FScriptFile, fmCreate);
  try
    line:= '#!/bin/bash' + LineEnding + LineEnding;
    line:= line + 'echo "******** Test ********"' + LineEnding;
    line:= line + 'echo' + LineEnding + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      Write(GenerateProgressBar(index+1, FConfig.Entries.Count, 50), lineBreak);
      if not FConfig.Entries[index].Active then continue;
      //if FConfig.Entries[index].EntryBinary = cBaselineBinary then continue;
      line:= line + 'function ' + FConfig.Entries[index].EntryBinary + '() {' + LineEnding + LineEnding;
      line:= line + '  echo "===== '+ FConfig.Entries[index].Name +' ======"' + LineEnding;
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
          FConfig.Input,
          IntToStr(FConfig.Entries[index].Threads)
        ],
        [rfReplaceAll]
      );
      tmpStr:= Format('  %s > %s%s.output', [
        tmpStr,
        IncludeTrailingPathDelimiter(FConfig.ResultsFolder),
        FConfig.Entries[index].EntryBinary
      ]);
      line:= line + tmpStr + LineEnding;
      tmpStr:= Format('  sha256sum %s%s.output',[
        IncludeTrailingPathDelimiter(FConfig.ResultsFolder),
        FConfig.Entries[index].EntryBinary
      ]);
      line:= line + tmpStr + LineEnding;
      line:= line + Format('  echo "%s  Official Output Hash"',[
        FConfig.OutputHash
      ]) + LineEnding;
      line:= line + Format('  rm %s%s.output',[
        IncludeTrailingPathDelimiter(FConfig.ResultsFolder),
        FConfig.Entries[index].EntryBinary
      ]) + LineEnding;
      line:= line + '  echo "==========="' + LineEnding;
      line:= line + '  echo' + LineEnding + LineEnding + '}' + LineEnding + LineEnding;
    end;
    line:= line + 'if [ $1 == "" ];then'  + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      if not FConfig.Entries[index].Active then continue;
      line:= line + '  ' + FConfig.Entries[index].EntryBinary + LineEnding;
    end;
    line:= line + 'else'  + LineEnding;
    line:= line + '  case $1 in'  + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      if not FConfig.Entries[index].Active then continue;
      line:= line + '    ' + FConfig.Entries[index].EntryBinary + ')' + LineEnding;
      line:= line + '      ' + FConfig.Entries[index].EntryBinary + LineEnding;
      line:= line + '      ;;'  + LineEnding;
    end;
    line:= line + '    *)'  + LineEnding;
    line:= line + '      echo "Do not recognise $1"'  + LineEnding;
    line:= line + '      ;;'  + LineEnding;
    line:= line + '  esac'  + LineEnding;
    line:= line + 'fi'  + LineEnding;
    FScriptStream.WriteBuffer(line[1], Length(line));
  finally
    FScriptStream.Free;
  end;
  {$IFDEF UNIX}
  FpChmod(PChar(FScriptFile), &775);
  {$ENDIF}
end;

procedure TBuilder.BuildRunScriptBash;
var
  index: Integer;
  line, tmpStr: TJSONStringType;
begin
  FScriptFile:= IncludeTrailingPathDelimiter(FConfig.RootFolder) + cRunBash;
  FScriptStream:= TFileStream.Create(FScriptFile, fmCreate);
  try
    line:= '#!/bin/bash' + LineEnding + LineEnding;
    line:= line + 'echo "******** Run ********"' + LineEnding;
    line:= line + 'echo' + LineEnding + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      Write(GenerateProgressBar(index+1, FConfig.Entries.Count, 50), lineBreak);
      if not FConfig.Entries[index].Active then continue;
      if FConfig.Entries[index].EntryBinary = cBaselineBinary then continue;
      line:= line + 'function ' + FConfig.Entries[index].EntryBinary + '() {' + LineEnding + LineEnding;
      line:= line + '  echo "===== '+ FConfig.Entries[index].Name +' ======"' + LineEnding;
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
            FConfig.Entries[index].EntryBinary + '-1_000_000_000-' + cSSD + '.json'
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
          FConfig.Input,
          IntToStr(FConfig.Entries[index].Threads)
        ],
        [rfReplaceAll]
      );
      line:= line + '  echo "-- ' + cSSD + ' --"' + LineEnding + '  ' + tmpStr + LineEnding;
      line:= line + '  echo "==========="' + LineEnding;
      line:= line + '  echo' + LineEnding + LineEnding + '}' + LineEnding + LineEnding;
    end;
    line:= line + 'if [ $1 == "" ];then'  + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      if not FConfig.Entries[index].Active then continue;
      if FConfig.Entries[index].EntryBinary = cBaselineBinary then continue;
      line:= line + '  ' + FConfig.Entries[index].EntryBinary + LineEnding;
    end;
    line:= line + 'else'  + LineEnding;
    line:= line + '  case $1 in'  + LineEnding;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      if not FConfig.Entries[index].Active then continue;
      if FConfig.Entries[index].EntryBinary = cBaselineBinary then continue;
      line:= line + '    ' + FConfig.Entries[index].EntryBinary + ')' + LineEnding;
      line:= line + '      ' + FConfig.Entries[index].EntryBinary + LineEnding;
      line:= line + '      ;;'  + LineEnding;
    end;
    line:= line + '    *)'  + LineEnding;
    line:= line + '      echo "Do not recognise $1"'  + LineEnding;
    line:= line + '      ;;'  + LineEnding;
    line:= line + '  esac'  + LineEnding;
    line:= line + 'fi'  + LineEnding;
    FScriptStream.WriteBuffer(line[1], Length(line));
  finally
    FScriptStream.Free;
  end;
  {$IFDEF UNIX}
  FpChmod(PChar(FScriptFile), &775);
  {$ENDIF}
end;

end.
