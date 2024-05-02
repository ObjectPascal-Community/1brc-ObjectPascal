program OneBRCproj;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, Lclintf, UTF8Process,
  Baseline.Console,
  OneBRC;

type

  { TOneBRCApp }

  TOneBRCApp = class(TCustomApplication)
  private
    FFileName: string;
    FThreadCount: Integer;
    procedure RunOneBRC;
  protected
    procedure DoRun; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TOneBRCApp }

procedure TOneBRCApp.RunOneBRC;
var
  vOneBRC: TOneBRC;
begin
  vOneBRC := TOneBRC.Create (FThreadCount);
  try
    try
      vOneBRC.mORMotMMF(FFileName);
      vOneBRC.DispatchThreads;
      vOneBRC.WaitAll;
      vOneBRC.MergeAll;
      vOneBRC.GenerateOutput;
    except
      on E: Exception do
      begin
        WriteLn(Format(rsErrorMessage, [ E.Message ]));
        ReadLn;
      end;
    end;
  finally
    vOneBRC.Free;
  end;
end;

{$REGION TOneBRCApp scaffolding}

procedure TOneBRCApp.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:= CheckOptions(Format('%s%s%s%s:',[
      cShortOptHelp,
      cShortOptThread,
      cShortOptVersion,
      cShortOptInput
    ]),
    [
      cLongOptHelp,
      cLongOptThread+':',
      cLongOptVersion,
      cLongOptInput+':'
    ]
  );
  if ErrorMsg<>'' then begin
    //ShowException(Exception.Create(ErrorMsg));
    WriteLn(Format(rsErrorMessage, [ ErrorMsg ]));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption(cShortOptHelp, cLongOptHelp) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption(cShortOptVersion, cLongOptVersion) then begin
    WriteLn(Format(rsGeneratorVersion, [ cVersion ]));
    Terminate;
    Exit;
  end;

  FThreadCount := GetSystemThreadCount;
  if HasOption(cShortOptThread, cLongOptThread) then begin
    FThreadCount := StrToInt (GetOptionValue(cShortOptThread, cLongOptThread));
  end;

  if HasOption(cShortOptInput, cLongOptInput) then begin
    FFileName := GetOptionValue(
      cShortOptInput,
      cLongOptInput
    );
  end
  else begin
    WriteLn(Format(rsErrorMessage, [ rsMissingInputFlag ]));
    Terminate;
    Exit;
  end;

  FFileName := ExpandFileName(FFileName);

  RunOneBRC;

  // stop program loop
  Terminate;
end;

constructor TOneBRCApp.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  StopOnException:=True;
end;

destructor TOneBRCApp.Destroy;
begin
  inherited Destroy;
end;

procedure TOneBRCApp.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TOneBRCApp;
begin
  Application:=TOneBRCApp.Create(nil);
  Application.Title:='1 BRC';
  Application.Run;
  Application.Free;
end.

{$ENDREGION}

