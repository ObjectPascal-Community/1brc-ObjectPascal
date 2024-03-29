unit Baseline.Console;

interface

uses
  System.Classes, SysUtils;

const
  cOptionHelp:    array of string = ['-h', '--help'];
  cOptionVersion: array of string = ['-v', '--version'];
  cOptionInput:   array of string = ['-i', '--input-file'];


resourcestring
  rsAppTitle         = 'One Billion Row Challenge Baseline';
  rsGeneratorVersion = 'baseline v%s';
  rsErrorMessage     = 'ERROR: %s';
  rsMissingInputFlag = 'Missing input file flag.';
  rsNoInputFile      = 'Input file does not exist.';

function ParseCmdLineParams (out aInputFile: string): Boolean;


implementation

uses
  System.IOUtils;

{$I version.inc}

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

function ArrayContains (const aArray: array of string; const aValue: string): Boolean;
var
  iValue: string;
begin
  Result := False;
  for iValue in aArray do begin
    if aValue.ToLower = iValue then begin
      Result := True;
      break;
    end;
  end;
end;

function ParseCmdLineParams (out aInputFile: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  aInputFile := '';

  // 0 is the exe path, so we start at 1
  for I := 1 to ParamCount do begin
    if ArrayContains (cOptionHelp, ParamStr(I)) then begin
      WriteHelp;
      exit;
    end
    else if ArrayContains (cOptionVersion, ParamStr(I)) then begin
      WriteLn(Format(rsGeneratorVersion, [ cVersion ]));
      exit;
    end
    else if ArrayContains (cOptionInput, ParamStr(I)) then begin
      // must be followed by the user's specified input file
      if (I+1) <= ParamCount then
        aInputFile := ExpandFileName (ParamStr (I+1));
      if not TFile.Exists (aInputFile) then
        WriteLn(Format(rsErrorMessage, [ rsNoInputFile ]))
      else begin
        Result := True;
        exit;
      end;
    end
    else
      WriteLn(Format(rsErrorMessage, [ rsMissingInputFlag ]));
  end;
end;

end.


