unit Generate.Common;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, streamex
;

const
  cSeed: LongInt = 46668267; // '1BRC' in ASCII
  cColdestTemp = -99.9;
  cHottestTemp = 99.9;

type
{ TGenerator }
  TGenerator = class(TObject)
  private
    FInputFile: String;
    FOutPutFile: String;
    FLineCount: Int64;
    FStationNames: TStringList;

    procedure BuildStationNames;
    function GenerateProgressBar(APosition, AMax, ALength:Int64):String;
  protected
  public
    constructor Create(AInputFile, AOutputFile: String; ALineCount: Int64);
    destructor Destroy; override;

    procedure Generate;
  published
  end;

implementation

uses
  Math,
  bufstream
;

const
  batchPercent = 10;

{ TGenerator }

constructor TGenerator.Create(
  AInputFile,
  AOutputFile: String;
  ALineCount: Int64
);
begin
  FInputFile:= AInputFile;
  FOutPutFile:= AOutputFile;
  FLineCount:= ALineCount;

  FStationNames:= TStringList.Create;
  FStationNames.Capacity:= 50000;
  //FStationNames.CaseSensitive:= False;
  FStationNames.UseLocale:= False;
  FStationNames.Duplicates:= dupIgnore;
  FStationNames.Sorted:= True;
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
  count: Int64 = 0;
  start, stop: QWord;
begin
  WriteLn('Building Weather Stations...');
  // Load the Weather Station names
  if FileExists(FInputFile) then
  begin
    inputStream:= TFileStream.Create(FInputFile, fmOpenRead);
    try
      streamReader:= TStreamReader.Create(inputStream);
      try
        start:= GetTickCount64;
        while not streamReader.Eof do
        begin
          entry:= streamReader.ReadLine;
          if entry[1] <> '#' then
          begin
            entry:= entry.Split(';')[0];
            FStationNames.Add(entry);
            //WriteLn('Got: ', entry);
            Inc(count);
          end;
        end;
        stop:= GetTickCount64;
      finally
        streamReader.Free;
      end;
    finally
      inputStream.Free;
    end;
  end
  else
  begin
    raise Exception.Create(Format('File "%s" not found.', [ FInputFile ]));
  end;
  WriteLn(Format('Done: Processed %.n entries from a total of %.n weather stations in %d ms', [
    Double(count),
    Double(FStationNames.Count),
    stop-start
  ]));
  WriteLn;
end;

function TGenerator.GenerateProgressBar(APosition, AMax, ALength: Int64
  ): String;
var
  percentDone: Double;
  filled: Integer;
begin
  percentDone:= (100 * APosition) / AMax;
  filled:= (ALength * APosition ) div AMax;
  Result:= '[';
  Result:= Result + StringOfChar('#', filled);
  Result:= Result + StringOfChar('-', ALength - filled);
  Result:= Result + Format('] %5.2f %%', [ percentDone ]);
end;

procedure TGenerator.Generate;
var
  index, progressBatch: Int64;
  stationId: Int64;
  randomTemp: Double;
  outputFileStream: TFileStream;
  outputBufWriter: TWriteBufStream;
  line: String;
begin
  // Randomize sets this variable depending on the current time
  // We just set it to our own value
  RandSeed:= cSeed;

  // Build list of station names
  BuildStationNames;

  outputFileStream:= TFileStream.Create(FOutPutFile, fmCreate);

  progressBatch:= floor(FLineCount * (batchPercent / 100));

  try
    //outputBufWriter:= TWriteBufStream.Create(outputFileStream, 4*1024);
    outputBufWriter:= TWriteBufStream.Create(outputFileStream, 64*1024);
    try
      Write(GenerateProgressBar(1, FLineCount, 50), #13);
      // Generate the file
      for index:= 1 to FLineCount do
      begin
        stationId:= Random(FStationNames.Count);
        randomTemp:= Random * (2 * cHottestTemp) - cHottestTemp;
        line:= Format('%s;%s'#13#10, [
          FStationNames[stationId],
          FormatFloat('#0.0', randomTemp)
        ]);
        //Write(line);
        outputBufWriter.WriteBuffer(line[1], Length(line));
        Dec(progressBatch);
        if progressBatch = 0 then
        begin
          Write(GenerateProgressBar(index, FLineCount, 50), #13);
          progressBatch:= floor(FLineCount * (batchPercent / 100));
        end;
      end;
    finally
      outputBufWriter.Free;
    end;
  finally
    outputFileStream.Free;
  end;
  WriteLn;
end;

end.

