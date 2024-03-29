program Baseline;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils
, Baseline.Console
, Baseline.Delphi
;

var
  //vInputFilePath: string;
  vBaseline: TBaseline;
begin
  try
    if ParseCmdLineParams (inputFilename) then
    begin
      vBaseline := TBaseline.Create (inputFilename);
      try
        vBaseline.Generate;
      finally
        vBaseline.Free;
      end;
    end;

  except
    on E: Exception do
      Writeln(Format(rsErrorMessage, [ E.ClassName, ': ', E.Message ]));
  end;
end.
