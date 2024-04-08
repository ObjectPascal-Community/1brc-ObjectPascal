program OneBRCproj;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, Lclintf,
  Baseline.Console,
  OneBRC;

type

  { TOneBRCApp }

  TOneBRCApp = class(TCustomApplication)
  private
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TOneBRCApp }

procedure TOneBRCApp.DoRun;
var
  ErrorMsg: String;
  vOneBRC: TOneBRC;
  vFileName: string;
  vStart: Int64;
  vTime: Int64;
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

  if HasOption(cShortOptInput, cLongOptInput) then begin
    vFileName := GetOptionValue(
      cShortOptInput,
      cLongOptInput
    );
  end
  else begin
    WriteLn(Format(rsErrorMessage, [ rsMissingInputFlag ]));
    Terminate;
    Exit;
  end;

  vFileName := ExpandFileName(vFileName);

  vOneBRC := TOneBRC.Create;
  try
    try
      //vOneBRC.mORMotMMF(vFileName);
      //vOneBRC.SingleThread;
      //vOneBRC.GenerateOutput;
      //vOneBRC.Free;

      vStart := GetTickCount;
      vOneBRC.mORMotMMF (vFileName);
      vTime := GetTickCount - vStart;
      WriteLn('read: ' + FloatToStr(vTime / 1000));
      WriteLn('-----------');
      WriteLn;

      vStart := GetTickCount;
      vOneBRC.SingleThread;
      vTime := GetTickCount - vStart;
      WriteLn('process: ' + FloatToStr(vTime / 1000));
      WriteLn('-----------');
      WriteLn;



      vStart := GetTickCount;
      vOneBRC.GenerateOutput;
      vTime := GetTickCount - vStart;
      WriteLn('generate: ' + FloatToStr(vTime / 1000));
      WriteLn('-----------');
      WriteLn;
      ReadLn;
    except
      on E: Exception do
      begin
        WriteLn(Format(rsErrorMessage, [ E.Message ]));
      end;
    end;
  finally
    vOneBRC.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TOneBRCApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
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

