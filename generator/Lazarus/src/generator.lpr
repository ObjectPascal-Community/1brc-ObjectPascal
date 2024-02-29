program generator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes
, SysUtils
, CustApp
, Generate.Common
;

const
  cVersion = {$I version.inc};
  cSeed: LongInt = 46668267; // '1BRC' in ASCII

  cShortOptHelp    = 'h';
  cLongOptHelp     = 'help';
  cShortOptVersion = 'v';
  cLongOptVersion  = 'version';
  cShortOptInput   = 'i:';
  cLongOptInput    = 'input-file:';
  cShortOptOutput  = 'o:';
  cLongOptOutput   = 'output-file:';
  cShortOptNumner  = 'n:';
  cLongOptNumber   = 'line-count:';

type

  { TOneBRCGenerator }

  TOneBRCGenerator = class(TCustomApplication)
  private
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  published
  end;

{ TOneBRCGenerator }

procedure TOneBRCGenerator.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:= CheckOptions(Format('%s%s%s%s%s',[
      cShortOptHelp,
      cShortOptVersion,
      cShortOptInput,
      cShortOptOutput,
      cShortOptNumner
    ]),
    [
      cLongOptHelp,
      cLongOptVersion,
      cLongOptInput,
      cLongOptOutput,
      cLongOptNumber
    ]
  );
  if ErrorMsg<>'' then begin
    //ShowException(Exception.Create(ErrorMsg));
    WriteLn(ErrorMsg);
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
    WriteLn('generator v', cVersion);
    Terminate;
    Exit;
  end;

  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TOneBRCGenerator.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TOneBRCGenerator.Destroy;
begin
  inherited Destroy;
end;

procedure TOneBRCGenerator.WriteHelp;
begin
  { add your help code here }
  WriteLn('Generates the measurement file with the specified number of lines');
  WriteLn;
  WriteLn('USAGE');
  WriteLn('  generator <flags>');
  WriteLn;
  WriteLn('FLAGS');
  WriteLn('  -h|--help                      Writes this help message and exits');
  WriteLn('  -v|--version                   Writes the version and exits');
  WriteLn('  -i|--input-file <filename>     The file containing the Weather Stations');
  WriteLn('  -o|--output-file <filename>    The file that will contain the generated lines');
  WriteLn('  -n|--line-count <number>       The amount of lines to be generated');
end;

var
  Application: TOneBRCGenerator;
begin
  Application:=TOneBRCGenerator.Create(nil);
  Application.Title:='One Billion Row Challenge Generator';
  Application.Run;
  Application.Free;
end.

