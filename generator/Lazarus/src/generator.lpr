program generator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes
, SysUtils
, CustApp
, Generate.Common
;

const
  cVersion = {$I version.inc};

  cShortOptHelp: Char    = 'h';
  cLongOptHelp           = 'help';
  cShortOptVersion: Char = 'v';
  cLongOptVersion        = 'version';
  cShortOptInput: Char   = 'i';
  cLongOptInput          = 'input-file';
  cShortOptOutput: Char  = 'o';
  cLongOptOutput         = 'output-file';
  cShortOptNumner: Char  = 'n';
  cLongOptNumber         = 'line-count';

var
  inputFilename: String = '';
  outputFilename: String = '';
  lineCount: Int64 = 0;

type

  { TOneBRCGenerator }

  TOneBRCGenerator = class(TCustomApplication)
  private
    FGenerator: TGenerator;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  published
  end;

{ TOneBRCGenerator }

procedure TOneBRCGenerator.DoRun;
var
  ErrorMsg: String;
  tmpLineCount: String;
begin
  // quick check parameters
  ErrorMsg:= CheckOptions(Format('%s%s%s:%s:%s:',[
      cShortOptHelp,
      cShortOptVersion,
      cShortOptInput,
      cShortOptOutput,
      cShortOptNumner
    ]),
    [
      cLongOptHelp,
      cLongOptVersion,
      cLongOptInput+':',
      cLongOptOutput+':',
      cLongOptNumber+':'
    ]
  );
  if ErrorMsg<>'' then
  begin
    //ShowException(Exception.Create(ErrorMsg));
    WriteLn('ERROR: ', ErrorMsg);
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption(cShortOptHelp, cLongOptHelp) then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption(cShortOptVersion, cLongOptVersion) then
  begin
    WriteLn('generator v', cVersion);
    Terminate;
    Exit;
  end;

  if HasOption(cShortOptInput, cLongOptInput) then
  begin
    inputFilename:= GetOptionValue(
      cShortOptInput,
      cLongOptInput
    );
  end
  else
  begin
    WriteLn('ERROR: Missing input file flag');
    Terminate;
    Exit;
  end;

  if HasOption(cShortOptOutput, cLongOptOutput) then
  begin
    outputFilename:= GetOptionValue(
      cShortOptOutput,
      cLongOptOutput
    );
  end
  else
  begin
    WriteLn('ERROR: Missing output file flag');
    Terminate;
    Exit;
  end;

  if HasOption(cShortOptNumner, cLongOptNumber) then
  begin
    tmpLineCount:=GetOptionValue(
      cShortOptNumner,
      cLongOptNumber
    );
    tmpLineCount:= StringReplace(tmpLineCount, '_', '', [rfReplaceAll]);
    if not TryStrToInt64(tmpLineCount, lineCount) then
    begin
      WriteLn('ERROR: Invalid integer "',tmpLineCount,'"');
      Terminate;
      Exit;
    end;
    if not (lineCount > 0) then
    begin
      WriteLn('ERROR: Number of lines should be a positive number, greater than 0.');
      Terminate;
      Exit;
    end;
  end
  else
  begin
    WriteLn('ERROR: Missing line count flag');
    Terminate;
    Exit;
  end;

  inputFilename:= ExpandFileName(inputFilename);
  outputFilename:= ExpandFileName(outputFilename);

  WriteLn('Input File : ', inputFilename);
  WriteLn('Output File: ', outputFilename);
  WriteLn('Line Count : ', Format('%.0n', [lineCount * 1.0]));
  WriteLn;

  FGenerator:= TGenerator.Create(inputFilename, outputFilename, lineCount);
  try
    try
      FGenerator.Generate;
    except
      on E: Exception do
      begin
        WriteLn('ERROR: ', E.Message);
      end;
    end;
  finally
    FGenerator.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TOneBRCGenerator.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TOneBRCGenerator.Destroy;
begin
  inherited Destroy;
end;

procedure TOneBRCGenerator.WriteHelp;
begin
  { add your help code here }
  WriteLn('Generates the measurement file with the specified number of lines');
  WriteLn;
  WriteLn('USAGE');
  WriteLn('  generator <flags>');
  WriteLn;
  WriteLn('FLAGS');
  WriteLn('  -h|--help                      Writes this help message and exits');
  WriteLn('  -v|--version                   Writes the version and exits');
  WriteLn('  -i|--input-file <filename>     The file containing the Weather Stations');
  WriteLn('  -o|--output-file <filename>    The file that will contain the generated lines');
  WriteLn('  -n|--line-count <number>       The amount of lines to be generated');
end;

var
  Application: TOneBRCGenerator;
begin
  Application:=TOneBRCGenerator.Create(nil);
  Application.Title:='One Billion Row Challenge Generator';
  Application.Run;
  Application.Free;
end.

