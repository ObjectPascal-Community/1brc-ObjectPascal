unit Generate.Console;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes
, SysUtils
;

const
  cShortOptHelp: Char     = 'h';
  cLongOptHelp            = 'help';
  cShortOptVersion: Char  = 'v';
  cLongOptVersion         = 'version';
  cShortOptInput: Char    = 'i';
  cLongOptInput           = 'input-file';
  cShortOptOutput: Char   = 'o';
  cLongOptOutput          = 'output-file';
  cShortOptNumber: Char   = 'n';
  cLongOptNumber          = 'line-count';
  cShortOptStations: Char = '4';
  cLongOptStations        = '400stations';
  cShortOptLineEnd: Char  = 'b';
  cLongOptLineEnd         = 'line-ending';
  {$IFNDEF FPC}
  cShortOptions: array of char = ['h', 'v', 'i', 'o', 'n', '4', 'b'];
  cLongOptions: array of string = ['help', 'version', 'input-file', 'output-file',
                                   'line-count', '400stations', 'line-ending'];
  {$ENDIF}

resourcestring
  rsAppTitle = 'One Billion Row Challenge Generator';
  rsGeneratorVersion = 'generator v%s';
  rsErrorMessage = 'ERROR: %s';
  rsMissingInputFlag = 'Missing input file flag.';
  rsMissingOutputFlag = 'Missing output file flag.';
  rsMissingLineCountFlag = 'Missing line count flag.';
  rsInvalidInteger = 'ERROR: Invalid integer "%s".';
  rsInvalidLineNumber = 'Number of lines should be a positive number, greater than 0.';
  rsInputFile = 'Input Filename: "%s"';
  rsOutputFile = 'Output Filename: "%s"';
  rsLineCount = 'Line Count: %.n';
  rsInvalidLineEnd = 'Line ending should be a CRLF or LF';

var
  inputFilename: String = '';
  outputFilename: String = '';
  lineCount: Integer = 0;  
  only400Stations: Boolean = False;
  lineEnding: String = '';

procedure WriteHelp;


implementation

procedure WriteHelp;
begin
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
  WriteLn('  -n|--line-count <number>       The amount of lines to be generated ( Can use 1_000_000_000 )');
  WriteLn('  -4|--400stations               Only 400 weather stations in output file');
  WriteLn('  -b|--line-ending               Line ending: LF (default) or CRLF');
end;

end.

