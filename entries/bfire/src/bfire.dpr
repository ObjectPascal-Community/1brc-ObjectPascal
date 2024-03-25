program bfire;

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
      if outputFilename <> 'CONSOLE' then
      begin
        outputFilename := ExpandFileName(outputFilename);
        WriteLn(Format(rsInputFile, [inputFilename]));
        WriteLn(Format(rsOutputFile, [outputFilename]));
        WriteLn;
        start := Now();
      end;

      ProcessFile(inputFilename, outputFilename);
      DumpFile(outputFilename);

      if outputFilename <> 'CONSOLE' then
      begin
        WriteLn(Format('Total Elapsed: %s', [FormatDateTime('n" min, "s" sec"',
          Now - start)]));
        WriteLn('Press ENTER to exit');
        readln(keypress);
      end;
    end;

  except

    on E: Exception do
    begin
      if outputFilename <> 'CONSOLE' then
      begin
        WriteLn(E.ClassName, ': ', E.Message);
        WriteLn('Press ENTER to exit');
        readln(keypress);
      end;
    end;
  end;

end.
