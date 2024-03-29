program Baseline;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils
, Baseline.Console
, Baseline.Delphi
;

var
  vBaseline: TBaseline;
begin
  try
    if ParseCmdLineParams(inputFilename) then
    begin
      vBaseline := TBaseline.Create(inputFilename);
      try
        vBaseline.Generate;
      finally
        vBaseline.Free;
      end;
    end;

  except
    on E: Exception do
      WriteLn(Format(rsErrorMessage, [ E.ClassName, ': ', E.Message ]));
  end;
end.
