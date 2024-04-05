program ScriptBuilder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes
, SysUtils
, CustApp
, ScriptBuilder.Console
, ScriptBuilder.Common
;

{$I version.inc}

type

{ TScriptBuilder }
  TScriptBuilder = class(TCustomApplication)
  private
    FBuilder: TBuilder;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TScriptBuilder }

procedure TScriptBuilder.DoRun;
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

  FBuilder:= TBuilder.Create(configFilename);
  try
    {$IFDEF UNIX}
    WriteLn('=== Compile Script ===');
    FBuilder.BuildCompileScriptBash;
    WriteLn;

    WriteLn('=== Test Script ===');
    FBuilder.BuildTestScriptBash;
    WriteLn;

    WriteLn('=== Run Script ===');
    FBuilder.BuildRunScriptBash;
    WriteLn;
    {$ELSE}
    WriteLn('=== Compile Script ===');
    FBuilder.BuildCompileScriptCmd;
    WriteLn;
    {$ENDIF}
  finally
    FBuilder.Free;
  end;


  // stop program loop
  Terminate;
end;

constructor TScriptBuilder.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TScriptBuilder.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TScriptBuilder;
begin
  Application:=TScriptBuilder.Create(nil);
  Application.Title:='Script Builder';
  Application.Run;
  Application.Free;
end.

