unit Generate.Common;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes
, SysUtils
;

type

{ TGenerator }
  TGenerator = class(TObject)
  private
    FOnly400Stations: Boolean;
    rndState: Array [0..1] of Cardinal;
    FInputFile: String;
    FOutPutFile: String;
    FLineCount: Int64;
    FStationNames: TStringList;
    FLineEnding: String;

    procedure BuildStationNames;
    function GenerateProgressBar(APBPosition, APBMax, APBWIdth: Integer;
      AFileSize: Int64; ATimeElapsed: TDateTime): String;
    function Rng1brc(Range: Integer): Integer;
  public
    constructor Create(AInputFile, AOutputFile: String; ALineCount: Int64; AOnly400Stations: Boolean; ALineEnding: String);
    destructor Destroy; override;

    procedure Generate;
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
  cSeed = 46668267; // '1BRC' in ASCII
  linesPercent = 10;
  stationsCapacity = 50000;
  chunkBatch = 10000;
  chunkCapacity = 20 * 1024 * 1024;
  lineBreak = #13;

{ TGenerator }

constructor TGenerator.Create(AInputFile, AOutputFile: String; ALineCount: Int64; AOnly400Stations: Boolean; ALineEnding: String);
begin
  FInputFile := AInputFile;
  FOutPutFile := AOutputFile;
  FLineCount := ALineCount;
  FOnly400Stations := AOnly400Stations;
  FLineEnding := ALineEnding;

  FStationNames := TStringList.Create;
  FStationNames.Capacity := stationsCapacity;
  FStationNames.UseLocale := False;
  FStationNames.Duplicates := dupIgnore;
  FStationNames.Sorted := True;
end;

destructor TGenerator.Destroy;
begin
  FStationNames.Free;
  inherited Destroy;
end;

procedure TGenerator.BuildStationNames;
var
  inputStream: TFileStream;
  streamReader: TStreamReader;
  entry: String;
  count: Int64;
  start, stop: Int64;
begin
  count := 0; // cannot initialize vars in declaration (delphi complains)
  WriteLn('Building Weather Stations...');
  // Load the Weather Station names
  if FileExists(FInputFile) then
  begin
    inputStream := TFileStream.Create(FInputFile, fmOpenRead);
    try
      streamReader := TStreamReader.Create(inputStream);
      try
        {$IFDEF FPC}
        start := GetTickCount64;
        while not streamReader.Eof do
        begin
          entry := streamReader.ReadLine;
          if entry[1] <> '#' then
          begin
            entry := entry.Split(';')[0];
            FStationNames.Add(entry);
            Inc(count);
          end;
        end;
        stop := GetTickCount64;
        {$ELSE}
        start := {$IF defined(MSWINDOWS)}GetTickCount64{$ELSE}TStopwatch.GetTimeStamp{$ENDIF};
        while not streamReader.EndOfStream do
        begin
          entry := streamReader.ReadLine;
          if entry[1] <> '#' then
          begin
            entry := entry.Split([';'])[0];
            FStationNames.Add(entry);
            Inc(count);
          end;
        end;
        stop := {$IF defined(MSWINDOWS)}GetTickCount64{$ELSE}TStopwatch.GetTimeStamp{$ENDIF};
        {$ENDIF}
      finally
        streamReader.Free;
      end;
    finally
      inputStream.Free;
    end;
  end
  else
  begin
    raise Exception.Create(Format('File "%s" not found.', [FInputFile]));
  end;
  WriteLn(Format
    ('Done: Processed %.n entries from a total of %.n weather stations in %d ms',
    [Double(count), Double(FStationNames.count), stop - start]));
  WriteLn;
end;

function TGenerator.GenerateProgressBar(APBPosition, APBMax, APBWIdth: Integer; AFileSize: Int64; ATimeElapsed: TDateTime): String;
var
  percentDone: Double;
  filled: Integer;
begin
  percentDone := 100 * (APBPosition / APBMax);
  filled := trunc(APBWIdth * (percentDone / 100));
  Result := '[';
  Result := Result + StringOfChar('#', filled);
  Result := Result + StringOfChar('-', APBWIdth - filled);
  Result := Result + Format('] %5.2f %%', [percentDone]);
  Result := Result + Format(' lines: %.n, file size: %.n, elapsed: %s    ',
    [Double(APBPosition), Double(AFileSize), FormatDateTime('n" min, "s" sec"',
    ATimeElapsed)]);
end;

function TGenerator.Rng1brc(Range: Integer): Integer;
var
  s0, s1, s2: Cardinal;
  i0, i1: Int64;
begin
  s0 := rndState[0];
  s1 := rndState[1] xor s0;
  i0 := ((Int64(s1) * 3) xor 5) * 7;
  s2 := i0 and $FFFFFFFF;
  i1 := Int64(s2) * range;
  Result := Integer(i1 shr 32);
  rndState[0] := s2;
  i0 := s0 xor (Int64(s1) shl 9);
  rndState[1] := i0 and $FFFFFFFF;
end;

procedure TGenerator.Generate;
var
  index, progressCount, progressBatch: Integer;
  stationId, randomTemp: Integer;
  randomTempWord: Word;
  outputFileStream: TFileStream;
  chunkLine, randomTempFinal: Utf8String;
  stationArray, temperatureArray: TStringArray;
  stationTempOffsetArray, stationTempRangeArray, LenStationArray, LenTemperatureArray: Array of Word;
  chunkCount, chunkLen, stationsCount, temperaturesCount: Integer;
  start: TDateTime;
begin
  //random init
  rndState[0] := cSeed;
  rndState[1] := 7266;
  // Build list of station names
  BuildStationNames;

  //initialize line ending if not corrected
  if FLineEnding <> #13#10 then
    FLineEnding := #10;

  outputFileStream := TFileStream.Create(FOutPutFile, fmCreate);

  progressBatch := floor(FLineCount * (linesPercent / 100));
  start := Now;

  if FOnly400Stations then
  begin
    WriteLn('Only 400 weather stations in output file.');
    while FStationNames.Count > 400 do
      FStationNames.Delete(Rng1brc(FStationNames.Count));
  end;

  // This is all paweld magic:
  // From here
  // based on code @domasz from lazarus forum, github: PascalVault
  temperaturesCount := 1999;
  stationsCount := FStationNames.count;
  SetLength(stationArray, stationsCount);
  SetLength(LenStationArray, stationsCount);
  SetLength(stationTempOffsetArray, stationsCount);
  SetLength(stationTempRangeArray, stationsCount);
  for index := 0 to stationsCount - 1 do
  begin
    stationArray[index] := FStationNames[index] + ';';
    LenStationArray[index] := Length(stationArray[index]);
    stationTempRangeArray[index] := Rng1brc(501) + 350;
    stationTempOffsetArray[index] := Rng1brc(temperaturesCount - stationTempRangeArray[index]);
  end;

  temperaturesCount := 1999;
  SetLength(temperatureArray, temperaturesCount);
  SetLength(LenTemperatureArray, temperaturesCount);
  for index := 0 to High(temperatureArray) do
  begin
    randomTempFinal := FormatFloat('0"."0', index - 999);
    temperatureArray[index] := randomTempFinal + FLineEnding;
    LenTemperatureArray[index] := Length(temperatureArray[index]);
  end;

  chunkCount := chunkBatch;
  chunkLen := 0;
  SetLength(chunkLine, chunkCapacity);
  progressCount := progressBatch;
  // To here

  try
    // Print first state of the progress bar
    Write(GenerateProgressBar(1, FLineCount, 50, 0, Now - start), lineBreak);
    // Generate the file
    for index := 1 to FLineCount do
    begin
      stationId := Rng1brc(stationsCount);
      randomTemp := stationTempOffsetArray[stationId] + Rng1brc(stationTempRangeArray[stationId]);
      randomTempWord := randomTemp and $FFFF;
      Move(stationArray[stationId][1], chunkLine[chunkLen + 1],
        LenStationArray[stationId]);
      Inc(chunkLen, LenStationArray[stationId]);
      Move(temperatureArray[randomTempWord][1], chunkLine[chunkLen + 1],
        LenTemperatureArray[randomTempWord]);
      Inc(chunkLen, LenTemperatureArray[randomTempWord]);

      Dec(chunkCount);
      if chunkCount = 0 then
      begin
        outputFileStream.WriteBuffer(chunkLine[1], chunkLen);
        chunkCount := chunkBatch;
        chunkLen := 0;
      end;
      // To here
      Dec(progressCount);
      if progressCount = 0 then
      begin
        Write(GenerateProgressBar(index, FLineCount, 50, outputFileStream.Size,
          Now - start), lineBreak);
        progressCount := progressBatch;
      end;
    end;

    if chunkCount > 0 then
    begin
      outputFileStream.WriteBuffer(chunkLine[1], chunkLen);
    end;
  finally
    WriteLn;
    WriteLn;
    WriteLn(Format('Done: file size: %.n, elapsed: %s', [Double(outputFileStream.Size),
      FormatDateTime('n" min, "s" sec"', Now - start)]));
    outputFileStream.Free;
  end;
end;

end.
