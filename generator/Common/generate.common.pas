unit Generate.Common;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Math
  {$IFDEF FPC}
    , streamex, bufstream
  {$ELSE}
    , System.Diagnostics
  {$ENDIF}
    ;

const
  cSeed: LongInt = 46668267; // '1BRC' in ASCII
  cColdestTemp = -99.9;
  cHottestTemp = 99.9;
  cLineBreak = #13#10;

type
  { TGenerator }
  TGenerator = class(TObject)
  private
    FInputFile: String;
    FOutPutFile: String;
    FLineCount: Int64;
    FStationNames: TStringList;

    procedure BuildStationNames;
    function GenerateProgressBar(APBPosition, APBMax, APBWIdth, AFileSize: Int64;
      ATimeElapsed: TDateTime): String;
  protected
  public
    constructor Create(AInputFile, AOutputFile: String; ALineCount: Int64);
    destructor Destroy; override;

    procedure Generate;
  published
  end;

  {$IFNDEF FPC}

  TStringArray = array of string;
  TWriteBufStream = TFileStream;
  {$ENDIF}

implementation

const
  batchPercent = 10;

  { TGenerator }

constructor TGenerator.Create(AInputFile, AOutputFile: String; ALineCount: Int64);
begin
  FInputFile := AInputFile;
  FOutPutFile := AOutputFile;
  FLineCount := ALineCount;

  FStationNames := TStringList.Create;
  FStationNames.Capacity := 50000;
  // FStationNames.CaseSensitive:= False;
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
  start, stop: {$IFDEF FPC}QWord{$ELSE}Int64{$ENDIF};
begin
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
            // WriteLn('Got: ', entry);
            Inc(count);
          end;
        end;
        stop := GetTickCount64;
        {$ELSE}
        start := TStopwatch.GetTimeStamp;
        while not streamReader.EndOfStream do
        begin
          entry := streamReader.ReadLine;
          if entry[1] <> '#' then
          begin
            entry := entry.Split([';'])[0];
            FStationNames.Add(entry);
            // WriteLn('Got: ', entry);
            Inc(count);
          end;
        end;
        stop := TStopwatch.GetTimeStamp;
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

function TGenerator.GenerateProgressBar(APBPosition, APBMax, APBWIdth, AFileSize: Int64;
  ATimeElapsed: TDateTime): String;
var
  percentDone: Double;
  filled: Integer;
begin
  percentDone := (100 * APBPosition) / APBMax;
  filled := (APBWIdth * APBPosition) div APBMax;
  Result := '[';
  Result := Result + StringOfChar('#', filled);
  Result := Result + StringOfChar('-', APBWIdth - filled);
  Result := Result + Format('] %5.2f %%', [percentDone]);
  Result := Result + Format(' lines: %.n, file size: %.n, elapsed: %s    ',
    [Double(APBPosition), Double(AFileSize), FormatDateTime('n" min, "s" sec"',
    ATimeElapsed)]);
end;

procedure TGenerator.Generate;
var
  index, progressBatch: Int64;
  stationId: Int64;
  randomTemp: Integer;
  randomTempStr: String[4];
  outputFileStream: TFileStream;
  outputBufWriter: TWriteBufStream;
  line, randomTempFinal: String;
  stationArray, temperatureArray: TStringArray;
  i, stationsCount, temperaturesCount: Integer;
  start: TDateTime;
begin
  // Randomize sets this variable depending on the current time
  // We just set it to our own value
  RandSeed := cSeed;

  // Build list of station names
  BuildStationNames;

  outputFileStream := TFileStream.Create(FOutPutFile, fmCreate);

  progressBatch := floor(FLineCount * (batchPercent / 100));
  start := Now;

  // based on code @domasz from lazarus forum, github: PascalVault
  stationsCount := FStationNames.count;
  SetLength(stationArray, stationsCount);
  for i := 0 to stationsCount - 1 do
    stationArray[i] := FStationNames[i];

  temperaturesCount := 1999;
  SetLength(temperatureArray, temperaturesCount);
  temperatureArray[0] := '0.0';
  for i := 1 to 999 do
  begin
    randomTempStr := IntToStr(i);
    case Ord(randomTempStr[0]) of
      1:
        randomTempFinal := '0.' + randomTempStr;
      2:
        randomTempFinal := randomTempStr[1] + '.' + randomTempStr[2];
      3:
        randomTempFinal := randomTempStr[1] + randomTempStr[2] + '.' + randomTempStr[3];
      4:
        randomTempFinal := randomTempStr[1] + randomTempStr[2] + randomTempStr[3] + '.' +
          randomTempStr[4];
    end;
    temperatureArray[i * 2 - 1] := randomTempFinal;
    temperatureArray[i * 2] := '-' + randomTempFinal;
  end;
  //

  line := '';

  try
    // outputBufWriter:= TWriteBufStream.Create(outputFileStream, 4*1024);
    {$IFDEF FPC}
    outputBufWriter := TWriteBufStream.Create(outputFileStream, 64 * 1024);
    {$ENDIF}
    try
      Write(GenerateProgressBar(1, FLineCount, 50, 0, Now - start), cLineBreak);
      // Generate the file
      for index := 1 to FLineCount do
      begin
        stationId := Random(stationsCount);
        // This is all paweld magic:
        // From here
        randomTemp := Random(temperaturesCount);
        line := line + stationArray[stationId] + ';' + temperatureArray[randomTemp] +
          cLineBreak;
        // Write(line);
        if index mod 10000 = 0 then
        begin
          {$IFNDEF FPC}
          outputFileStream.WriteBuffer(line[1], Length(line));
          {$ELSE}
          outputBufWriter.WriteBuffer(line[1], Length(line));
          {$ENDIF}
          line := '';
        end;
        // To here
        Dec(progressBatch);
        if progressBatch = 0 then
        begin
          Write(GenerateProgressBar(index, FLineCount, 50, outputFileStream.Size,
            Now - start), cLineBreak);
          progressBatch := floor(FLineCount * (batchPercent / 100));
        end;
      end;
      if line <> '' then
      begin
        {$IFNDEF FPC}
        outputFileStream.WriteBuffer(line[1], Length(line));
        {$ELSE}
        outputBufWriter.WriteBuffer(line[1], Length(line));
        {$ENDIF}
      end;
    finally
      {$IFDEF FPC}
      outputBufWriter.Free;
      {$ENDIF}
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
