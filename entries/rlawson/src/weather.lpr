program weather;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  parser;

type

  { TWeatherProcessor }

  TWeatherProcessor = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TWeatherProcessor }

  procedure TWeatherProcessor.DoRun;
  var
    ErrorMsg, inputFile: string;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('hi:', ['help', 'input']);
    if ErrorMsg <> '' then
    begin
      WriteLn(ErrorMsg);
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if not HasOption('i', 'input') then
    begin
      WriteLn('Must specify an input file with -i or input parameter');
      Terminate;
      Exit;
    end;

    inputFile := GetOptionValue('i', 'input');
    { make sure file exists }
    if not FileExists(inputFile) then
    begin
      WriteLn('Sorry file ' + inputFile + ' does not exist');
      Terminate;
      Exit;
    end;

    { add your program here }
    parser.ReadMeasurements(inputFile);

    // stop program loop
    Terminate;
  end;

  constructor TWeatherProcessor.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TWeatherProcessor.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TWeatherProcessor.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -i /path/to/input/file');
  end;

var
  Application: TWeatherProcessor;
begin
  Application := TWeatherProcessor.Create(nil);
  Application.Title := 'Rick 1brc app';
  Application.Run;
  Application.Free;
end.
