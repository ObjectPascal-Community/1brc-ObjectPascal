unit Baseline.Delphi;

interface

uses
  System.Classes
, System.Generics.Collections
;

type
// alias, to easily try various types (Int64, Currency, Double)
// due to rounding problems during the average calculation,
// and the mismatch between Delphi and Lazarus.
  TAmount = Int64;

{ TWeatherStation }
  PWeatherStation = ^TWeatherStation;
  TWeatherStation = record
    FStation: string;
    FMin: TAmount;
    FMax: TAmount;
    FTot: TAmount;
    FCnt: Integer;
  end;

{ TBaseline }
  TBaseline = class
  private
    FInputFile: String;
    FStationNames: TStringList;
    FHashStationList: TDictionary<string, PWeatherStation>;
    procedure AddToHashList (AStation: String; ATemp: TAmount);
    procedure BuildHashList;
  protected
  public
    constructor Create (AInputFile: String);
    destructor Destroy; override;

    procedure Generate;
end;


implementation

uses
  System.SysUtils
, System.StrUtils
, System.Math
, Baseline.Common
;


function Compare (AList: TStringList; AIndex1, AIndex2: Integer): Integer;
var
  Pos1, Pos2: Integer;
  Str1, Str2: String;
begin
  Result := 0;
  Str1 := AList.Strings[AIndex1];
  Str2 := AList.Strings[AIndex2];
  Pos1 := Pos('=', Str1);
  Pos2 := Pos('=', Str2);
  if (Pos1 > 0) and (Pos2 > 0) then
  begin
    Str1 := Copy(Str1, 1, Pos1 - 1);
    Str2 := Copy(Str2, 1, Pos2 - 1);
    Result := CompareStr(Str1, Str2);
  end;
end;

{ TBaseline }

constructor TBaseline.Create (AInputFile: String);
begin
  FInputFile := AInputFile;

  FHashStationList := TDictionary<string, PWeatherStation>.Create;

  FStationNames := TStringList.Create;
  FStationNames.DefaultEncoding := TEncoding.UTF8;
  FStationNames.UseLocale := True;
end;

destructor TBaseline.Destroy;
var
  iStation: PWeatherStation;
begin
  for iStation in FHashStationList.Values do
  begin
    Dispose(iStation);
  end;
  FStationNames.Free;
  FHashStationList.Free;
  inherited Destroy;
end;

procedure TBaseline.AddToHashList(AStation: String; ATemp: TAmount);
var
  vWeatherStation: PWeatherStation;
begin
// on Delphi, the below station returns a different average from the baseline.output provided
// this issue was resolved after
//  if aStation = 'Danau Kändimarg' then begin
//    FProblematicSum := FProblematicSum + aTemp;
//    Inc (FProblematicCount);
//  end;

  if not FHashStationList.TryGetValue(AStation, vWeatherStation) then
  begin
    New(vWeatherStation);
    vWeatherStation^.FStation := AStation;
    vWeatherStation^.FMin := ATemp;
    vWeatherStation^.FMax := ATemp;
    vWeatherStation^.FTot := ATemp;
    vWeatherStation^.FCnt := 1;
    FHashStationList.Add(AStation, vWeatherStation);
  end
  else
  begin
    vWeatherStation^.FMin := Min(vWeatherStation^.FMin, ATemp);
    vWeatherStation^.FMax := Max(vWeatherStation^.FMax, ATemp);
    vWeatherStation^.FTot := vWeatherStation^.FTot + ATemp;
    vWeatherStation^.FCnt := vWeatherStation^.FCnt + 1;
  end;
end;

procedure TBaseline.BuildHashList;
var
  inputFileStream: TFileStream;
  streamReader: TStreamReader;
  position, Code: Integer;
  strLine: String;
  strStation: String;
  strTemp: String;
  temperature: Int64;
begin
  inputFileStream := TFileStream.Create(FInputFile, fmOpenRead);
  try
    streamReader := TStreamReader.Create(inputFileStream);
    try
      while not streamReader.EndOfStream do
      begin
        strLine:= streamReader.ReadLine;
        position := Pos(';', strLine);
        if position > 0 then
        begin
          strStation := Copy(strLine, 1, position - 1);
          strTemp := Copy(strLine, position + 1, Length(strLine));
          strTemp := StringReplace(strTemp, '.', '', [rfReplaceAll]);
          Val(strTemp, temperature, Code);
          if Code <> 0 then
            Continue;
          AddToHashList(strStation, temperature);
        end;
      end;
    finally
      streamReader.Free;
    end;
  finally
    inputFileStream.Free;
  end;
end;

procedure TBaseline.Generate;
var
  index: Integer;
  strTemp: string;
  min: Double;
  max: Double;
  mean: Double;
  weatherStation: PWeatherStation;
  iStation: string;
  {$IFDEF DEBUG}
  vStream: TStringStream;
  {$ENDIF}
begin
  BuildHashList;
  FStationNames.BeginUpdate;
  for iStation in FHashStationList.Keys do
  begin
    FHashStationList.TryGetValue(iStation, weatherStation);
    Min := weatherStation^.FMin/10;
    Max := weatherStation^.FMax/10;
    Mean := RoundExDouble(weatherStation^.FTot/weatherStation^.FCnt/10);
    strTemp := weatherStation^.FStation + '=' + FormatFloat('0.0', Min) + '/' + FormatFloat('0.0', Mean) + '/' + FormatFloat('0.0', Max) + ',';
    FStationNames.Add(strTemp);
  end;
  FStationNames.EndUpdate;
  FStationNames.CustomSort(Compare);

  strTemp:= '';
  for index:= 0 to FStationNames.Count - 1 do
  begin
    strTemp:= strTemp + FStationNames[index] + ' ';
  end;
  SetLength(strTemp, Length(strTemp) - 2);

  // Windows will mess up the characters when outputting to STDOUT.
  // for debug purposes, we'll output it to a file instead.
{$IFDEF DEBUG}
  vStream := TStringStream.Create('', TEncoding.UTF8);
  try
    vStream.WriteString('{' + strTemp + '}' + #10);
    vStream.SaveToFile('output.txt');
  finally
    vStream.Free;
  end;
{$ENDIF}
{$IFDEF RELEASE}
  WriteLn ('{' + strTemp + '}' + #10);
{$ENDIF}
end;

end.
