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

var
  inputFilename: String = '';
  outputFilename: String = '';
  lineCount: Int64 = 0;

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
  WriteLn('  -n|--line-count <number>       The amount of lines to be generated');
end;

end.

