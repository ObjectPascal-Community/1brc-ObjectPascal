program bfire;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.Threading,
  System.SyncObjs,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  MultiThreadUnit in 'MultiThreadUnit.pas',
  ConsoleUnit in 'ConsoleUnit.pas';

var
  start: TDateTime; // for timing
  dummy: Integer; // for Val function

begin
  UseStdOut := True;
  try
    if ParseConsoleParams then
    begin
      inputFilename := ExpandFileName(inputFilename);
      Val(ReadThreadCountParam, ReadThreadCount, dummy);
      TabulateOn := Not(NoTabulate);
      if outputFilename <> '' then
      begin
        UseStdOut := False;
        outputFilename := ExpandFileName(outputFilename);
        WriteLn(Format(rsInputFile, [inputFilename]));
        WriteLn(Format(rsOutputFile, [outputFilename]));
        WriteLn('Reading Thread Count: ' + IntToStr(ReadThreadCount));
        if NoTabulate then
          WriteLn('Tabulation turned off');
        WriteLn;
        start := Now();
      end;

      // One thread for console (waits for tabulation, then sorts and
      // writes results), one, two or three threads to read file, five threads to
      // tabulate stations (split by section of alphabet).
      // File is read byte-wise into "classic" byte arrays for station
      // name and temperature. The arrays are passed to one of four
      // stacks, split by section of alphabet, for tabulation.
      // Tabulation threads hash station name, use hash as index into
      // a data array.  After all data is read and tabulated, the
      // four data arrays are added to an initially unsorted
      // TStringList that holds unsorted Unicode station name and
      // has linked pointers to tabulated data for each station.
      // Finally, the TStringList is sorted, and the data is output.

      FileToArrays(inputFilename, UseStdOut); // read

      if Not(UseStdOut) then // wait and report
      begin
        // while Not(ReadFile_Done1 and ReadFile_Done2 and ParseDataQ_Done1 and
        // ParseDataQ_Done2 and ParseDataQ_Done3 and ParseDataQ_Done4 and
        // ParseDataQ_Done5 and ParseDataQ_Done6) do
        while Not(ReadFile_Done1 and ReadFile_Done2 and ReadFile_Done3 and
          ParseData_Done) do
        begin
          Sleep(1000);
          WriteLn('Lines: ' + IntToStr(LineCount) + '  Stacks: ' +
            IntToStr(DataStackCount1) + ' / ' + IntToStr(DataStackCount2) +
            ' / ' + IntToStr(DataStackCount3) + ' / ' +
            IntToStr(DataStackCount4) + ' / ' + IntToStr(DataStackCount5));

          if DataStackCount1 > StackMax1 then
            StackMax1 := DataStackCount1;
          if DataStackCount2 > StackMax2 then
            StackMax2 := DataStackCount2;
          if DataStackCount3 > StackMax3 then
            StackMax3 := DataStackCount3;
          if DataStackCount4 > StackMax4 then
            StackMax4 := DataStackCount4;
          if DataStackCount5 > StackMax5 then
            StackMax5 := DataStackCount5;

          case ReadThreadCount of
            1:
              begin
                if ReadFile_Done1 then
                begin
                  WriteLn('Done with reading thread 1');
                  ReadFile_Done2 := True;
                  ReadFile_Done3 := True;
                  if Not(TabulateOn) then
                    ParseData_Done := True;
                end;
              end;
            2:
              begin
                if ReadFile_Done1 then
                  WriteLn('Done with reading thread 1');
                if ReadFile_Done2 then
                begin
                  WriteLn('Done with reading thread 2');
                  ReadFile_Done2 := True;
                  ReadFile_Done3 := True;
                  if Not(TabulateOn) then
                    ParseData_Done := True;
                end;
              end;
            3:
              begin
                if ReadFile_Done1 then
                  WriteLn('Done with reading thread 1');
                if ReadFile_Done2 then
                  WriteLn('Done with reading thread 2');
                if ReadFile_Done3 then
                  WriteLn('Done with reading thread 3');
                if Not(TabulateOn) then
                  ParseData_Done := True;
              end;
          end;

          if ParseData_Done and TabulateOn then
            WriteLn('Done with tabulating threads');

        end;

      end
      else // just wait
      begin
        // while Not(ReadFile_Done1 and ReadFile_Done2 and ParseDataQ_Done2 and
        // ParseDataQ_Done2 and ParseDataQ_Done3 and ParseDataQ_Done4 and
        // ParseDataQ_Done5 and ParseDataQ_Done6) do
        while Not(ReadFile_Done1 and ReadFile_Done2 and ReadFile_Done3 and
          ParseData_Done) do
        begin
          case ReadThreadCount of
            1:
              begin
                if ReadFile_Done1 then
                begin
                  ReadFile_Done2 := True;
                  ReadFile_Done3 := True;
                end;
              end;
            2:
              begin
                if ReadFile_Done2 then
                begin
                  ReadFile_Done2 := True;
                  ReadFile_Done3 := True;
                end;
              end;
          end;

          Sleep(100);
        end;
      end;

      Challenge.Free;

      SortArrays; // sort
      ArrayToFile(outputFilename, UseStdOut); // output

      if Not(UseStdOut) then
      begin
        WriteLn(Format('Total Elapsed: %s', [FormatDateTime('n" min, "s" sec"',
          Now - start)]));

        WriteLn('Stack Max: ' + IntToStr(StackMax1) + '/' + IntToStr(StackMax2)
          + '/' + IntToStr(StackMax3) + '/' + IntToStr(StackMax4) + '/' +
          IntToStr(StackMax5));

        WriteLn('Press ENTER to exit');
        readln;
      end;
    end;

  except

    on E: Exception do
    begin
      if Not(UseStdOut) then
      begin
        WriteLn(E.ClassName, ': ', E.Message);
        WriteLn('Press ENTER to exit');
        readln;
      end;
    end;
  end;

end.
