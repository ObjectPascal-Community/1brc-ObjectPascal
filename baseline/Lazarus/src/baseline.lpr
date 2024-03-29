program baseline;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes
, SysUtils
, CustApp
, Baseline.Console
, Baseline.Lazarus
;

type

{ TOneBRCBaseline }
  TOneBRCBaseline = class(TCustomApplication)
  private
    FBaseline: TBaseline;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TBaseline }

procedure TOneBRCBaseline.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:= CheckOptions(Format('%s%s%s:',[
      cShortOptHelp,
      cShortOptVersion,
      cShortOptInput
    ]),
    [
      cLongOptHelp,
      cLongOptVersion,
      cLongOptInput+':'
    ]
  );
  if ErrorMsg<>'' then
  begin
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
    WriteLn(Format(rsGeneratorVersion, [ cVersion ]));
    Terminate;
    Exit;
  end;

  if HasOption(cShortOptInput, cLongOptInput) then
  begin
    inputFilename:= GetOptionValue(
      cShortOptInput,
      cLongOptInput
    );
  end
  else
  begin
    WriteLn(Format(rsErrorMessage, [ rsMissingInputFlag ]));
    Terminate;
    Exit;
  end;

  inputFilename:= ExpandFileName(inputFilename);

  FBaseline:= TBaseline.Create(inputFilename);
  try
    try
      FBaseline.Generate;
    except
      on E: Exception do
      begin
        WriteLn(Format(rsErrorMessage, [ E.Message ]));
      end;
    end;
  finally
    FBaseline.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TOneBRCBaseline.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TOneBRCBaseline.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TOneBRCBaseline;
begin
  Application:=TOneBRCBaseline.Create(nil);
  Application.Title:='Baseline';
  Application.Run;
  Application.Free;
end.

