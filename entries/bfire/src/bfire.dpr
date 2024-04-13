program bfire;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Classes,
  ConsoleUnit in 'ConsoleUnit.pas',
  ProcessByHashUnit in 'ProcessByHashUnit.pas';

var
  keypress: String; // dummy for readln
  UseStdOut: Boolean; // True unless output file is defined

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

      // Read file byte-wise and digest to station name and temperature.
      // Hash station name, use hash as index into (initially unsorted)
      // TStringList that holds unsorted Unicode station name and
      // has linked objects for records holding accumulated data
      // for each station.
      // Sort station name TStringList, then output sorted data.

      // With TFileStream: 6 seconds for 1E7 records,
      // 1 min 1 sec for 1E8 records,  10 min 17 sec for 1E9 records

      // using global variables for arrays and lengths: 5 seconds for 1E7 records,
      // 56 sec for 1E8 records,  9 min 25 sec for 1E9 records

      FileToArrays(inputFilename, UseStdOut); // read
      SortArrays; // sort
      ArrayToFile(outputFilename, UseStdOut); // output

      if Not(UseStdOut) then
      begin
        WriteLn('Places: ' + IntToStr(PlaceCount));
        WriteLn(Format('Total Elapsed: %s', [FormatDateTime('n" min, "s" sec"',
          Now - start)]));
        WriteLn('Press ENTER to exit');
        readln(keypress);
      end;
    end;

  except

    on E: Exception do
    begin
      if Not(UseStdOut) then
      begin
        WriteLn(E.ClassName, ': ', E.Message);
        WriteLn('Press ENTER to exit');
        readln(keypress);
      end;
    end;
  end;

end.
