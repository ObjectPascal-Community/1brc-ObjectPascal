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

    function GetHeader(const AHeader: String): String;
    function GetVariables: String;
    function GetFunctionCompile(const AEntry: TEntry): String;
    function GetFunctionTest(const AEntry: TEntry): String;
    function GetFunctionRun(const AEntry: TEntry): String;
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

  {$IFDEF UNIX}
  cLazbuildDefault     = '%s \'#10'    -B \'#10'    "%s"';
  cLazbuildRelease     = '%s \'#10'    -B \'#10'    --bm="Release" \'#10'    "%s"';
  {$ENDIF}

  cReplaceName         = '[[name]]';
  cReplaceJSONResults  = '[[results-json]]';
  cReplaceEntryBinary  = '[[entry-binary]]';
  cReplaceEntryInput   = '[[input]]';
  cReplaceEntryThreads = '[[threads]]';

  cBaselineBinary      = 'baseline';
  {$IFDEF UNIX}
  cCompilerFPC         = 'fpc';
  {$ELSE}
  cCompilerDelphi      = 'delphi';
  {$ENDIF}
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

function TBuilder.GetHeader(const AHeader: String): String;
begin
  Result:= '#!/bin/bash' + LineEnding + LineEnding;
  Result:= Result + 'echo "******** ' + AHeader + ' ********"' + LineEnding;
  Result:= Result + 'echo' + LineEnding + LineEnding;
end;

function TBuilder.GetVariables: String;
begin
  Result:= '';
  {$IFDEF UNIX}
  Result:= Result + 'BIN="' + FConfig.BinLinux + '"' + LineEnding;
  Result:= Result + 'ENTRIES="' + FConfig.EntriesLinux + '"' + LineEnding;
  Result:= Result + 'RESULTS="' + FConfig.ResultsFolder + '"' + LineEnding;
  Result:= Result + 'INPUT="' + FConfig.Input + '"' + LineEnding;
  {$ELSE}
  Result:= Result + 'BIN="' + FConfig.BinWindows + '"' + LineEnding;
  Result:= Result + 'ENTRIES="' + FConfig.EntriesWindows + '"' + LineEnding;
  Result:= Result + 'DELPHICC="' + FConfig.DelphiCompiler + '"' + LineEnding;
  Result:= Result + 'INCLUDE="c:\program files (x86)\embarcadero\studio\23.0\lib\Linux64\release;C:\Users\gcarr\Documents\Embarcadero\Studio\23.0\Imports;c:\program files (x86)\embarcadero\studio\23.0\Imports;C:\Users\Public\Documents\Embarcadero\Studio\23.0\Dcp\Linux64;c:\program files (x86)\embarcadero\studio\23.0\include;c:\program files (x86)\embarcadero\studio\23.0\redist\Linux64;C:\Users\Public\Documents\Embarcadero\Studio\23.0\Bpl\Linux64;C:\Users\gcarr\Documents\Embarcadero\Studio\23.0\CatalogRepository\ChatGPTWizard-2.2.0.9"' + LineEnding;
  Result:= Result + 'SDK="C:\Users\gcarr\Documents\Embarcadero\Studio\SDKs\ubuntu23.10.sdk"' + LineEnding;
  Result:= Result + 'LIBPATH="C:\Users\gcarr\Documents\Embarcadero\Studio\SDKs\ubuntu23.10.sdk\usr\lib\gcc\x86_64-linux-gnu\13;C:\Users\gcarr\Documents\Embarcadero\Studio\SDKs\ubuntu23.10.sdk\usr\lib\x86_64-linux-gnu;C:\Users\gcarr\Documents\Embarcadero\Studio\SDKs\ubuntu23.10.sdk\lib\x86_64-linux-gnu;C:\Users\gcarr\Documents\Embarcadero\Studio\SDKs\ubuntu23.10.sdk\lib64"' + LineEnding;
  Result:= Result + 'BPL="\Users\Public\Documents\Embarcadero\Studio\23.0\Bpl\Linux64"' + LineEnding;
  Result:= Result + 'DCP="\Users\Public\Documents\Embarcadero\Studio\23.0\Dcp\Linux64"' + LineEnding;
  Result:= Result + 'GENERICS="Generics.Collections=System.Generics.Collections;Generics.Defaults=System.Generics.Defaults"' + LineEnding;
  Result:= Result + 'HEADERS="\Users\Public\Documents\Embarcadero\Studio\23.0\hpp\Linux64"' + LineEnding;
  Result:= Result + 'DEFINES="RELEASE;LINUX"' + LineEnding;
  {$ENDIF}
  Result:= Result + LineEnding;
end;

function TBuilder.GetFunctionCompile(const AEntry: TEntry): String;
begin
  Result:= 'function ' + AEntry.EntryBinary + '() {' + LineEnding;
  Result:= Result + '  echo "===== '+ UTF8Encode(AEntry.Name) +' ======"' + LineEnding;
  {$IFDEF UNIX}
  if AEntry.HasRelease then
  begin
   Result:= Result + '  ' +
   Format(cLazbuildRelease, [
     FConfig.Lazbuild,
     '${ENTRIES}/' + AEntry.EntryFolder + '/' + AEntry.LPI
   ] ) +
   LineEnding;
  end
  else
  begin
    Result:= Result + '  ' +
    Format(cLazbuildDefault, [
      FConfig.Lazbuild,
      '${ENTRIES}/' + AEntry.EntryFolder + '/' + AEntry.LPI
    ] ) +
    LineEnding;
  end;
  {$ELSE}
  Result:= Result + Format(
    '  "${DELPHICC}" '+
    '-\$D0 '+
    '-\$L- '+
    '-\$Y- '+
    '--no-config '+
    '-B '+
    '-Q '+
    '"-A${GENERICS}" '+
    '"-D${DEFINES}" '+
    '"-E${BIN}" '+
    '"-I${INCLUDE}" '+
    '"-LEC:${BPL}" '+
    '"-LNC:${DCP}" '+
    '-NS '+
    '"-O${INCLUDE}" '+
    '"-R${INCLUDE}" '+
    '"-U${INCLUDE}" '+
    '"--syslibroot:${SDK}" '+
    '"--libpath:${LIBPATH}" '+
    '"-NHC:${HEADERS}" '+
    '"${ENTRIES}/%s/%s" && \' + LineEnding +
    '  scp ${BIN}/' + AEntry.EntryBinary + ' ' +
    'gcarreno@10.42.0.1:/home/gcarreno/Programming/1brc-ObjectPascal/bin'
    ,
  [
    AEntry.EntryFolder,
    AEntry.DPR
  ]) + LineEnding;
  {$ENDIF}
  Result:= Result + '  echo "==========="' + LineEnding;
  Result:= Result + '  echo' + LineEnding + '}' + LineEnding + LineEnding;
end;

function TBuilder.GetFunctionTest(const AEntry: TEntry): String;
var
  tmpStr: String;
begin
  Result:= 'function ' + AEntry.EntryBinary + '() {' + LineEnding;
  Result:= Result + '  echo "===== '+ UTF8Encode(AEntry.Name) +' ======"' + LineEnding;
  tmpStr:= Format('%s/%s %s', [
    '${BIN}',
    AEntry.EntryBinary,
    AEntry.RunParams
  ]);
  tmpStr:= StringsReplace(
    tmpStr,
    [
      cReplaceEntryInput,
      cReplaceEntryThreads
    ],
    [
      FConfig.Input,
      IntToStr(AEntry.Threads)
    ],
    [rfReplaceAll]
  );
  tmpStr:= Format('  %s > %s/%s.output', [
    tmpStr,
    '${RESULTS}',
    AEntry.EntryBinary
  ]);
  Result:= Result + tmpStr + LineEnding;
  tmpStr:= Format('  sha256sum %s/%s.output',[
    '${RESULTS}',
    AEntry.EntryBinary
  ]);
  Result:= Result + tmpStr + LineEnding;
  Result:= Result + Format('  echo "%s  Official Output Hash"',[
    FConfig.OutputHash
  ]) + LineEnding;
  Result:= Result + Format('  rm %s/%s.output',[
    '${RESULTS}',
    AEntry.EntryBinary
  ]) + LineEnding;
  Result:= Result + '  echo "==========="' + LineEnding;
  Result:= Result + '  echo' + LineEnding + '}' + LineEnding + LineEnding;
end;

function TBuilder.GetFunctionRun(const AEntry: TEntry): String;
var
  tmpStr: String;
begin
  Result:= 'function ' + AEntry.EntryBinary + '() {' + LineEnding;
  Result:= Result + '  echo "===== '+ UTF8Encode(AEntry.Name)  +' ======"' + LineEnding;
  // Run for SSD
  tmpStr:= StringsReplace(
    FConfig.Hyperfine,
    [
      cReplaceName,
      cReplaceJSONResults,
      cReplaceEntryBinary
    ],
    [
      AEntry.EntryBinary,
      Format('%s/%s', [
        '${RESULTS}',
        AEntry.EntryBinary + '-1_000_000_000-' + cSSD + '.json'
      ]),
      Format('%s/%s %s', [
        '${BIN}',
        AEntry.EntryBinary,
        AEntry.RunParams
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
      '${INPUT}',
      IntToStr(AEntry.Threads)
    ],
    [rfReplaceAll]
  );
  Result:= Result + '  echo "-- ' + cSSD + ' --"' + LineEnding + '  ' + tmpStr + LineEnding;
  Result:= Result + '  echo "==========="' + LineEnding;
  Result:= Result + '  echo' + LineEnding + '}' + LineEnding + LineEnding;
end;

procedure TBuilder.BuildCompileScriptBash;
var
  index: Integer;
  line
: TJSONStringType;
begin
  {$IFDEF UNIX}
  FScriptFile:= IncludeTrailingPathDelimiter(FConfig.RootLinux) + cCompileBash;
  {$ELSE}
  FScriptFile:= IncludeTrailingPathDelimiter(FConfig.RootWIndows) + cCompileBash;
  {$ENDIF}
  FScriptStream:= TFileStream.Create(FScriptFile, fmCreate);
  try
    line:= GetHeader('Compile');
    line:= line + GetVariables;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      Write(GenerateProgressBar(index+1, FConfig.Entries.Count, 50), lineBreak);
      if not FConfig.Entries[index].Active then continue;
      {$IFDEF UNIX}
      if FConfig.Entries[index].Compiler <> cCompilerFPC then continue;
      {$ELSE}
      if FConfig.Entries[index].Compiler <> cCompilerDelphi then continue;
      {$ENDIF}
      line:= line + GetFunctionCompile(FConfig.Entries[index]);
    end;
    line:= line + 'if [ "$1" == "" ];then'  + LineEnding;
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
  line: TJSONStringType;
begin
  FScriptFile:= IncludeTrailingPathDelimiter(FConfig.RootLinux) + cTestBash;
  FScriptStream:= TFileStream.Create(FScriptFile, fmCreate);
  try
    line:= GetHeader('Test');
    line:= line + GetVariables;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      Write(GenerateProgressBar(index+1, FConfig.Entries.Count, 50), lineBreak);
      if not FConfig.Entries[index].Active then continue;
      line:= line + GetFunctionTest(FConfig.Entries[index]);
    end;
    line:= line + 'if [ "$1" == "" ];then'  + LineEnding;
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
  line: TJSONStringType;
begin
  FScriptFile:= IncludeTrailingPathDelimiter(FConfig.RootLinux) + cRunBash;
  FScriptStream:= TFileStream.Create(FScriptFile, fmCreate);
  try
    line:= GetHeader('Run');
    line:= line + GetVariables;
    for index:= 0 to Pred(FConfig.Entries.Count) do
    begin
      Write(GenerateProgressBar(index+1, FConfig.Entries.Count, 50), lineBreak);
      if not FConfig.Entries[index].Active then continue;
      if FConfig.Entries[index].EntryBinary = cBaselineBinary then continue;
      line:= line + GetFunctionRun(FConfig.Entries[index]);
    end;
    line:= line + 'if [ "$1" == "" ];then'  + LineEnding;
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
