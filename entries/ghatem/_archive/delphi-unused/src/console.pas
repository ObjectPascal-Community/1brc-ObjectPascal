﻿unit Console;

interface

uses
  System.Classes, System.SysUtils;

const
  cOptionHelp:    array of string = ['-h', '--help'];
  cOptionVersion: array of string = ['-v', '--version'];
  cOptionInput:   array of string = ['-i', '--input-file'];

resourcestring
  rsAppTitle         = 'One Billion Row Challenge Baseline';
  rsGeneratorVersion = 'baseline v%s';
  rsErrorMessage     = 'ERROR: %s';
  rsMissingInputFlag = 'Missing input file flag.';
  rsNoInputFile      = 'File "%s" not found.';


procedure WriteHelp;
function ParseCmdLineParams(out aInputFile: string): Boolean;


implementation

uses
  System.IOUtils;

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
  for I := 1 to ParamCount do begin
{$ELSE}
  for I := 1 to ParamCount + 1 do begin
{$ENDIF}
    if ArrayContains(cOptionHelp, ParamStr(I)) then begin
      WriteHelp;
      Exit;
    end
    else if ArrayContains(cOptionVersion, ParamStr(I)) then begin
      WriteLn(Format(rsGeneratorVersion, [ 1.0 ]));
      exit;
    end
    else if ArrayContains(cOptionInput, ParamStr(I)) then begin
      // must be followed by the user's specified input file
      if (I+1) <= ParamCount then
        aInputFile := ExpandFileName (ParamStr (I+1));
      if (aInputFile = '') or (not TFile.Exists (aInputFile)) then begin
        WriteLn(Format(rsErrorMessage, [ Format(rsNoInputFile, [aInputFile]) ]));
        Exit;
      end
      else begin
        Result := True;
        Exit;
      end;
    end
    else
      WriteLn(Format(rsErrorMessage, [ rsMissingInputFlag ]));
  end;
end;

end.

