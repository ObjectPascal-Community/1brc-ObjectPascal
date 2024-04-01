unit Baseline.Console;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes
, SysUtils
;

const
  {$IFDEF FPC}
  cShortOptHelp: Char    = 'h';
  cLongOptHelp           = 'help';
  cShortOptVersion: Char = 'v';
  cLongOptVersion        = 'version';
  cShortOptInput: Char   = 'i';
  cLongOptInput          = 'input-file';
  {$ELSE}
  cOptionHelp:    array of string = ['-h', '--help'];
  cOptionVersion: array of string = ['-v', '--version'];
  cOptionInput:   array of string = ['-i', '--input-file'];
  {$ENDIF}

{$I version.inc}

resourcestring
  rsAppTitle         = 'One Billion Row Challenge Baseline';
  rsGeneratorVersion = 'baseline v%s';
  rsErrorMessage     = 'ERROR: %s';
  rsMissingInputFlag = 'Missing input file flag.';
  rsNoInputFile      = 'File "%s" not found.';

var
  inputFilename: String = '';

procedure WriteHelp;
{$IFNDEF FPC}
function ParseCmdLineParams(out aInputFile: string): Boolean;
{$ENDIF}


implementation

{$IFNDEF FPC}
uses
  System.IOUtils
;
{$ENDIF}


procedure WriteHelp;
begin
  WriteLn('Generates the output for the challenge');
  WriteLn;
  WriteLn('USAGE');
  WriteLn('  baseline <flags>');
  WriteLn;
  WriteLn('FLAGS');
  WriteLn('  -h|--help                      Writes this help message and exits');
  WriteLn('  -v|--version                   Writes the version and exits');
  WriteLn('  -i|--input-file <filename>     The file containing the Weather Stations');
end;

{$IFNDEF FPC}
function ArrayContains(const aArray: array of string; const aValue: string): Boolean;
var
  iValue: string;
begin
  Result := False;
  for iValue in aArray do
  begin
    if aValue.ToLower = iValue then
    begin
      Result := True;
      break;
    end;
  end;
end;

function ParseCmdLineParams(out aInputFile: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  aInputFile := '';

  // 0 is the exe path, so we start at 1
{$IFNDEF LINUX}
  for I := 1 to ParamCount do
{$ELSE}
  for I := 1 to ParamCount + 1 do
{$ENDIF}
  begin
    if ArrayContains(cOptionHelp, ParamStr(I)) then
    begin
      WriteHelp;
      Exit;
    end
    else
      if ArrayContains(cOptionVersion, ParamStr(I)) then
      begin
        WriteLn(Format(rsGeneratorVersion, [ cVersion ]));
        exit;
      end
      else
        if ArrayContains(cOptionInput, ParamStr(I)) then
        begin
          // must be followed by the user's specified input file
          if (I+1) <= ParamCount then
            aInputFile := ExpandFileName (ParamStr (I+1));
          if (aInputFile = '') or (not TFile.Exists (aInputFile)) then
          begin
            WriteLn(Format(rsErrorMessage, [ Format(rsNoInputFile, [aInputFile]) ]));
            Exit;
          end
          else
          begin
            Result := True;
            Exit;
          end;
        end
        else
          WriteLn(Format(rsErrorMessage, [ rsMissingInputFlag ]));
  end;
end;
{$ENDIF}

end.

