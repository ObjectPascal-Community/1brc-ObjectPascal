program OneBRC;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Classes,
  ConsoleUnit in 'ConsoleUnit.pas',
  ProcessUnit in 'ProcessUnit.pas';

var
  keypress: String; // dummy for readln

begin
  try
    if ParseConsoleParams then
    begin
      inputFilename := ExpandFileName(inputFilename);
      outputFilename := ExpandFileName(outputFilename);
{$IFNDEF DEBUG}
      WriteLn(Format(rsInputFile, [inputFilename]));
      WriteLn(Format(rsOutputFile, [outputFilename]));
      WriteLn;

      start := Now();
{$IFEND}
      ProcessFile(inputFilename, outputFilename);
      DumpFile(outputFilename);

{$IFNDEF DEBUG}
      WriteLn(Format('Total Elapsed: %s', [FormatDateTime('n" min, "s" sec"',
        Now - start)]));
      WriteLn('Press ENTER to exit');
      readln(keypress);
{$IFEND}
    end;

  except
{$IFNDEF DEBUG}
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      WriteLn('Press ENTER to exit');
      readln(keypress);
    end;
{$IFEND}
  end;

end.
