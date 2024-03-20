unit uChallengeCommon;
(* as: uChallengeCommon.pas
 * by: David Cornelius
 * on: March, 2024
 * in: Delphi 12 Athens
 * to: provide some common classes and methods for use by the rest of the OBRC program.
 *)

interface

type
  // a class that holds the loaded city/temperature data
  TWeatherCity = class
  public
    CityName: string;
    MinTemp: Single;
    MaxTemp: Single;
    DataCount: Int64;
    TotalTemp: Double;
    constructor Create(const NewCityName: string; const NewTemp: single);
    procedure AddNewTemp(const NewTemp: Single);
    function Mean: Double;
  end;

  // anonymous method type
  TWeatherCityProc = reference to procedure(const City: string; const Temp: Double);

  // a global class to provide standard open/close and other functions
  TChallengeCommon = class
  private
    FWeatherDataFile: TextFile;
    FInputFilename: string;
  public
    constructor Create(const NewInputFilename: string);
    procedure OpenWeatherData;
    procedure CloseWeatherData;
    procedure ReadAndParseAllData(WeatherCityProcessor: TWeatherCityProc);
    function PascalRound(x: Double): Double;
    function SplitCityTemp(const StationLine: string; var CityName: string; var CityTemp: Double): Boolean;
    property InputFilename: string read FInputFilename write FInputFilename;
    property WeatherDataFile: TextFile read FWeatherDataFile;
  end;

var
  ChallengeCommon: TChallengeCommon;

implementation

uses
  System.Classes, System.SysUtils, System.Math;

{ TChallengeCommon }

constructor TChallengeCommon.Create(const NewInputFilename: string);
begin
  inherited Create;

  if FileExists(NewInputFilename) then
    FInputFilename := NewInputFilename
  else
    raise EFileNotFoundException.Create(NewInputFilename + ' not found.');
end;

procedure TChallengeCommon.OpenWeatherData;
begin
  AssignFile(FWeatherDataFile, FInputFilename);
  Reset(WeatherDataFile);
  {$IFDEF DEBUG}
  Writeln('Reading from ' + ChallengeCommon.InputFilename);
  {$ENDIF}
end;

function TChallengeCommon.PascalRound(x: Double): Double;
var
  t: Double;
begin
  // round towards positive infinity
  t := Trunc(x);
  if (x < 0.0) and (t - x = 0.5) then
    Result := t
  else if Abs(x - t) >= 0.5 then
    Result := t + Sign(x)
  else
    Result := x;
end;

procedure TChallengeCommon.ReadAndParseAllData(WeatherCityProcessor: TWeatherCityProc);
var
  WeatherLine: string;
  WeatherCity: string;
  CityTemp: Double;
begin
  OpenWeatherData;
  try
    // read all rows
    while not Eof(WeatherDataFile) do begin
      // read a row into a temp string
      Readln(WeatherDataFile, WeatherLine);

      // parse the data and call a procedure to process it
      if SplitCityTemp(WeatherLine, WeatherCity, CityTemp) then
        WeatherCityProcessor(WeatherCity, CityTemp);
    end;
  finally
    ChallengeCommon.CloseWeatherData;
  end;
end;

procedure TChallengeCommon.CloseWeatherData;
begin
  CloseFile(FWeatherDataFile);
end;

function TChallengeCommon.SplitCityTemp(const StationLine: string; var CityName: string; var CityTemp: Double): Boolean;
var
  SemiPos: Integer;
  TempStr: string;
begin
  Result := False;

  if StationLine.IsEmpty then
    Exit;

  SemiPos := Pos(';', StationLine);
  if SemiPos = -1 then
    Exit;

  CityName := Copy(StationLine, 1, SemiPos - 1);
  TempStr := Copy(StationLine, SemiPos + 1, 50);

  Result := TryStrToFloat(TempStr, CityTemp);
end;

{ TWeatherCity }

procedure TWeatherCity.AddNewTemp(const NewTemp: Single);
begin
  Inc(DataCount);

  // check min/max
  if MinTemp > NewTemp then
    MinTemp := NewTemp
  else if MaxTemp < NewTemp then
    MaxTemp := NewTemp;

  // add up total for city
  TotalTemp := TotalTemp + NewTemp;
end;

constructor TWeatherCity.Create(const NewCityName: string; const NewTemp: single);
begin
  CityName  := NewCityName;
  DataCount := 1;
  MinTemp   := NewTemp;
  MaxTemp   := NewTemp;
  TotalTemp := NewTemp;
end;

function TWeatherCity.Mean: Double;
begin
  Result := ChallengeCommon.PascalRound(TotalTemp / DataCount);
end;

end.
