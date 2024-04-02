program bfire;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  ConsoleUnit in 'ConsoleUnit.pas',
  ProcessUnit in 'ProcessUnit.pas';

var
  keypress: String; // dummy for readln
  UseStdOut: Boolean;  // True unless output file is defined

begin
  UseStdOut := True;
  try
    if ParseConsoleParams then
    begin
      inputFilename := ExpandFileName(inputFilename);
      if outputFilename <> '' then
      begin
        UseStdOut := False;
        outputFilename := ExpandFileName(outputFilename);
        WriteLn(Format(rsInputFile, [inputFilename]));
        WriteLn(Format(rsOutputFile, [outputFilename]));
        WriteLn;
        start := Now();
      end;

      ProcessFile(inputFilename, outputFilename, UseStdOut);
      DumpFile(outputFilename, UseStdOut);

      if Not (UseStdOut) then
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
      if Not (UseStdOut) then
      begin
        WriteLn(E.ClassName, ': ', E.Message);
        WriteLn('Press ENTER to exit');
        readln(keypress);
      end;
    end;
  end;

end.
