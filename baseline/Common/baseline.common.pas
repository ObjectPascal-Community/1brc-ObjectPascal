unit Baseline.Common;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes
, SysUtils
{$IFDEF FPC}
  , Contnrs
{$ELSE}
{$ENDIF}

;

type
{ TWeatherStation }
PWeatherStation = ^TWeatherStation;
TWeatherStation = record
  FStation: String[100];
  FMin: Single;
  FMax: Single;
  FTot: Single;
  FCnt: Integer;

end;
{ TBaseline }
  TBaseline = class(TObject)
  private
    FInputFile: String;
    FStationNames: TStringList;
    FHashStationList: TFPHashList;

    procedure AddToHashList(AStation: String; ATemp: Single);
    procedure BuildHashList;
  protected
  public
    constructor Create(AInputFile: String);
    destructor Destroy; override;

    procedure Generate;
  published
  end;

  {$IFNDEF FPC}
  TStringArray = array of Utf8String;
  TWriteBufStream = TFileStream;
  {$ENDIF}

implementation

uses
  Math
{$IFDEF FPC}
, streamex
{$ELSE}
, System.Diagnostics 
{$IF defined(MSWINDOWS)}, Winapi.Windows{$ENDIF}
{$ENDIF}
;

const
  //lineEnding = #13#10;
  stationsCapacity = 50000;


function Compare(AList: TStringList; AIndex1, AIndex2: Integer): Integer;
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
    Result :=  CompareStr(Str1, Str2);
  end;
end;

{ TBaseline }

constructor TBaseline.Create(AInputFile: String);
begin
  FInputFile := AInputFile;

  FHashStationList:= TFPHashList.Create;
  FHashStationList.Capacity:= stationsCapacity;

  FStationNames := TStringList.Create;
  FStationNames.Capacity := stationsCapacity;
  FStationNames.UseLocale := False;
end;

destructor TBaseline.Destroy;
var
  index: Integer;
begin
  FStationNames.Free;
  for index:= 0 to FHashStationList.Count - 1 do
  begin
    Dispose(PWeatherStation(FHashStationList.Items[index]));
  end;
  FHashStationList.Free;
  inherited Destroy;
end;

procedure TBaseline.AddToHashList(AStation: String; ATemp: Single);
var
  weatherStation: PWeatherStation;
  Index: Integer;
begin
  Index := FHashStationList.FindIndexOf(AStation);
  if Index = -1 then
  begin
    New(weatherStation);
    weatherStation^.FStation := AStation;
    weatherStation^.FMin := ATemp;
    weatherStation^.FMax := ATemp;
    weatherStation^.FTot := ATemp;
    weatherStation^.FCnt := 1;
    FHashStationList.Add(AStation, weatherStation);
  end
  else
  begin
    weatherStation := FHashStationList.Items[Index];
    weatherStation^.FMin := Min(weatherStation^.FMin, ATemp);
    weatherStation^.FMax := Max(weatherStation^.FMax, ATemp);
    weatherStation^.FTot := weatherStation^.FTot + ATemp;
    weatherStation^.FCnt := weatherStation^.FCnt + 1;
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
  temparature: Single;
begin
  if FileExists(FInputFile) then
  begin
    inputFileStream:= TFileStream.Create(FInputFile, fmOpenRead);
    try
      streamReader:= TStreamReader.Create(inputFileStream);
      try
        while not streamReader.Eof do
        begin
          strLine:= streamReader.ReadLine;
          position := Pos(';', strLine);
          if position > 0 then
          begin
            strStation := Copy(strLine, 1, position - 1);
            strTemp := Copy(strLine, position + 1, Length(strLine));
            Val(strTemp, temparature, Code);
            if Code <> 0 then
              Continue;
            AddToHashList(strStation, temparature);
          end;
        end;
      finally
        streamReader.Free;
      end;
    finally
      inputFileStream.Free;
    end;
  end
  else
  begin
    raise Exception.Create(Format('File "%s" not found.', [FInputFile]));
  end;
end;

procedure TBaseline.Generate;
var
  index: Integer;
  strTemp: String;
  mean: Single;
  weatherStation: PWeatherStation;
begin

  BuildHashList;

  //FStationNames.DefaultEncoding := TEncoding.UTF8;
  FStationNames.BeginUpdate;
  for index := 0 to FHashStationList.Count - 1 do
  begin
    weatherStation := FHashStationList.Items[index];
    weatherStation^.FTot := Round(weatherStation^.FTot*10)/10;
    mean := weatherStation^.FTot/weatherStation^.FCnt;
    strTemp := weatherStation^.FStation + '=' + FormatFloat('0.0', weatherStation^.FMin) + '/' + FormatFloat('0.0', mean) + '/' + FormatFloat('0.0', weatherStation^.FMax) + ',';
    FStationNames.Add(strTemp);
  end;
  FStationNames.EndUpdate;
  FStationNames.CustomSort(@Compare);

  strTemp:= '';
  for index:= 0 to FStationNames.Count - 1 do
  begin
    strTemp:= strTemp + FStationNames[index] + ' ';
  end;
  SetLength(strTemp, Length(strTemp) - 2);
  WriteLn('{', strTemp, '}');

end;

end.
