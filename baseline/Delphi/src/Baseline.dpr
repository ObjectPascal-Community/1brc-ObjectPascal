program Baseline;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Baseline.Console in 'Baseline.Console.pas',
  OneBRC in 'OneBRC.pas';

var
  vInputFilePath: string;
  vBaseline: TBaseline;
begin
  try
    if ParseCmdLineParams (vInputFilePath) then begin
      vBaseline := TBaseline.Create (vInputFilePath);
      try
        vBaseline.Generate;
      finally
        vBaseline.Free;
      end;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
