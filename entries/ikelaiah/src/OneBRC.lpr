program OneBRC;

{
 ==Acknowledgments==

Inspiration, code snippets, libraries, etc.

 1. The FPC team, Lazarus team, fpcupdeluxe team, and other contributors.
      - For providing a usable programming language and a usable ecosystem.
 2. Gustavo 'Gus' Carreno.
      - For making this happen - 1BRC for Object Pascal.
      - Borrowed Gus' approach to use `TCustomApplication` and using `unit`s properly
        to make main code more readable.
      - Borrowed and modified Gus' `WriteHelp` from the `baseline.lpr`.
 3. A.Koverdyaev(avk)
      - For the amazing (LGenerics)[https://github.com/avk959/LGenerics] library.
 4. Székely Balázs.
      - Now I know what `Single` data type is!
      - I borrowed the custom `TStringList` comparer from the `baseline` program.
 5. Shraddha Agrawal - https://www.bytesizego.com/blog/one-billion-row-challenge-go.
      - The advice for not storing measurements for each station in a data structure.
 6. Arman Hajisafi - https://arman-hs.github.io
      - Encouragements and inspirations.
 }

{$mode objfpc}{$H+}{$J-}{$modeSwitch advancedRecords}
{$codepage utf8}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  WeatherStation;

const
  version = '1.5';

type

  { TOneBRC }

  TOneBRC = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TOneBRC }

  procedure TOneBRC.DoRun;
  var
    ErrorMsg: string;
    filename: string = '';
    weatherStation: TWeatherStation;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('hvi:', ['help', 'version', 'input:']);
    if ErrorMsg <> '' then
    begin
      // Commented out the default ShowException as the generated text is not user friendly.
      // ShowException(Exception.Create(ErrorMsg));
      WriteLn('Error: ', ErrorMsg);
      WriteHelp;
      Terminate;
      Exit;
    end;

    // Parse h
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    // Parse v
    if HasOption('v', 'version') then
    begin
      WriteLn('OneBRC version ', version);
      Terminate;
      Exit;
    end;

    // Parse i
    if HasOption('i', 'input') then
    begin
      filename := GetOptionValue('i', 'input');
    end;

    if not FileExists(fileName) then
    begin
      WriteLn('Input file seems invalid.');
      WriteHelp;
      Terminate;
      Exit;
    end;

    // Start the main algorithm ////////////////////////////////////////////////
    try
      weatherStation := TWeatherStation.Create(filename);
      try
        weatherStation.ProcessMeasurements;
      finally
        weatherStation.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error: ' + E.Message);
    end;

    // Stop program loop ///////////////////////////////////////////////////////
    Terminate;
  end;

  constructor TOneBRC.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TOneBRC.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TOneBRC.WriteHelp;
  begin
    WriteLn('OneBRC -- An entry to the One Billion Row Challenge for Object Pascal');
    WriteLn;
    WriteLn('Usage: OneBRC [-h] [-v] [-i input_file]');
    WriteLn;
    WriteLn('  -h | --help                      Show this help screen');
    WriteLn('  -v | --version                   Show the version number');
    WriteLn('  -i | --input-file <filename>     Input text file to process.');
    WriteLn('                                   Each row is one temperature measurement in the format <string: station name>;<double: measurement>');
  end;

var
  Application: TOneBRC;
begin
  Application := TOneBRC.Create(nil);
  Application.Title := 'OneBRC';
  Application.Run;
  Application.Free;
end.
