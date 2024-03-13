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
  cShortOptHelp: Char    = 'h';
  cLongOptHelp           = 'help';
  cShortOptVersion: Char = 'v';
  cLongOptVersion        = 'version';
  cShortOptInput: Char   = 'i';
  cLongOptInput          = 'input-file';
  {$IFNDEF FPC}
  cShortOptions: array of char = ['h', 'v'{, 'i', 'o', 'n'}];
  cLongOptions: array of string = ['help', 'version'{, 'input-file', 'output-file',
                                   'line-count'}];
  {$ENDIF}

resourcestring
  rsAppTitle = 'One Billion Row Challenge Baseline';
  rsGeneratorVersion = 'baseline v%s';
  rsErrorMessage = 'ERROR: %s';
  rsMissingInputFlag = 'Missing input file flag.';

var
  inputFilename: String = '';
  {outputFilename: String = '';
  lineCount: Integer = 0;}

procedure WriteHelp;


implementation

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

end.

