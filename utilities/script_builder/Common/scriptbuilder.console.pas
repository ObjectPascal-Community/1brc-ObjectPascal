unit ScriptBuilder.Console;

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
  cShortOptConfig: Char  = 'c';
  cLongOptConfig         = 'config-file';
  {$IFNDEF FPC}
  cShortOptions: array of char = ['h', 'v'{, 'i', 'o', 'n'}];
  cLongOptions: array of string = ['help', 'version'{, 'input-file', 'output-file',
                                   'line-count'}];
  {$ENDIF}

resourcestring
  rsAppTitle = 'One Billion Row Challenge Script Builder';
  rsScriptBuilderVersion = 'scriptbuilder v%s';
  rsErrorMessage = 'ERROR: %s';
  rsMissingConfigFlag = 'Missing config file flag.';
  rsConfigFile = 'Config Filename: "%s"';

var
  configFilename: String = '';

procedure WriteHelp;


implementation

procedure WriteHelp;
begin
  WriteLn('Generates the scripts for the automated testing');
  WriteLn;
  WriteLn('USAGE');
  WriteLn('  sctiptbuilder <flags>');
  WriteLn;
  WriteLn('FLAGS');
  WriteLn('  -h|--help                      Writes this help message and exits');
  WriteLn('  -v|--version                   Writes the version and exits');
  WriteLn('  -c|--config-file <filename>    The file containing the configuration');
end;

end.

