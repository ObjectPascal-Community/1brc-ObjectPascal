program ResultsGenerator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes
, SysUtils
, CustApp
, ResultsGenerator.Console
, ResultsGenerator.Common
;

{$I version.inc}

type

{ TResultsGenerator }
  TResultsGenerator = class(TCustomApplication)
  private
    FBuilder: TResults;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TResultsGenerator }

procedure TResultsGenerator.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:= CheckOptions(Format('%s%s%s:',[
      cShortOptHelp,
      cShortOptVersion,
      cShortOptConfig
    ]),
    [
      cLongOptHelp,
      cLongOptVersion,
      cLongOptConfig+':'
    ]
  );
  if ErrorMsg<>'' then begin
    //ShowException(Exception.Create(ErrorMsg));
    WriteLn(Format(rsErrorMessage, [ ErrorMsg ]));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption(cShortOptHelp, cLongOptHelp) then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption(cShortOptVersion, cLongOptVersion) then
  begin
    WriteLn(Format(rsScriptBuilderVersion, [ cVersion ]));
    Terminate;
    Exit;
  end;

  if HasOption(cShortOptConfig, cLongOptConfig) then
  begin
    configFilename:= GetOptionValue(
      cShortOptConfig,
      cLongOptConfig
    );
  end
  else
  begin
    WriteLn(Format(rsErrorMessage, [ rsMissingConfigFlag ]));
    Terminate;
    Exit;
  end;

  configFilename:= ExpandFileName(configFilename);

  WriteLn(Format(rsConfigFile, [ configFilename ]));

  FBuilder:= TResults.Create(configFilename);
  try
    FBuilder.Generate;
  finally
    FBuilder.Free;
  end;


  // stop program loop
  Terminate;
end;

constructor TResultsGenerator.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TResultsGenerator.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TResultsGenerator;
begin
  Application:=TResultsGenerator.Create(nil);
  Application.Title:='Results Table Generator';
  Application.Run;
  Application.Free;
end.

