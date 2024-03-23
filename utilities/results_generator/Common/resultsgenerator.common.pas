unit ResultsGenerator.Common;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes
, SysUtils
, Utilities.Data.Config
, Utilities.Data.Entries
, Utilities.Data.Hyperfine
;

type
{ TResults }
  TResults = class(TObject)
  private
    FConfig: TConfig;
    FResult: THyperfineResult;

    function GenerateProgressBar(APBPosition, APBMax, APBWIdth: Integer): String;
    function CompareResults(const elem1, elem2): Integer;
    function FormatTime(ATime: Double): String;
  protected
  public
    constructor Create(AConfigFile: String);
    destructor Destroy; override;

    procedure Generate;
  published
  end;

implementation

uses
  fpjson
, jsonparser
, Math
, Utilities.ArraySort
;

type
  //PResult = ^TResult;
  TResult = record
    Name: TJSONStringType;
    Notes: TJSONStringType;
    Compiler: TJSONStringType;
    Result: Double;
    Count: Integer;
  end;
  TResultsArray = array of TResult;

const
  lineBreak = #13;

  cTableHeader =
    '| # | Result (m:s.ms): SSD | Compiler | Submitter     | Notes     | Certificates |'#10 +
    '|--:|---------------------:|:---------|:--------------|:----------|:-------------|'#10;

{ TResults }

constructor TResults.Create(AConfigFile: String);
var
  configStream: TFileStream;
  configJSONData: TJSONData;
begin
  configStream:= TFileStream.Create(AConfigFile, fmOpenRead);
  try
    configJSONData:= GetJSON(configStream);
    try
      FConfig:= TConfig.Create(configJSONData);
    finally
      configJSONData.Free;
    end;
  finally
    configStream.Free;
  end;
end;

destructor TResults.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
end;

function TResults.CompareResults(const elem1, elem2): Integer;
var
  i1 : TResult absolute elem1;
  i2 : TResult absolute elem2;
begin
  if i1.Result = i2.Result then Result:= 0
  else if i1.Result < i2.Result then Result:= -1
  else Result:= 1;
end;

function TResults.FormatTime(ATime: Double): String;
var
  intPart, minutes: Integer;
  millis: String;
begin
  Result:= '';
  intPart:= Trunc(ATime);
  minutes:= 0;
  if intPart > 60 then
  begin
    repeat
      Inc(minutes);
      Dec(intPart, 60);
    until intPart < 60;
  end;
  millis:= Copy(
    FloatToStr(ATime),
    Pos('.', FloatToStr(ATime)) + 1,
    4
  );
  Result:= Format('%d:%d.%s',[
    minutes,
    intPart,
    millis
  ]);
end;

procedure TResults.Generate;
var
  index, index1, index2: Integer;
  content, hyperfineFile: String;
  results: TResultsArray;
  hyperfineStream: TFileStream;
  hyperfineJSON: TJSONData;
begin
  SetLength(results, 0);

  for index:= 0 to Pred(FConfig.Entries.Count) do
  begin
    Write(GenerateProgressBar(Succ(index), FConfig.Entries.Count, 50), lineBreak);
    hyperfineFile:= ExpandFileName(
        IncludeTrailingPathDelimiter(FConfig.ResultsFolder)+
        FConfig.Entries[index].EntryBinary+
        '-1_000_000_000-SSD.json'
      );
    if not FileExists(hyperfineFile) then continue;
    SetLength(results, Length(results) + 1);
    index2:= Length(results) - 1;
    hyperfineStream:= TFileStream.Create(
      hyperfineFile,
      fmOpenRead
    );
    try
      hyperfineJSON:= GetJSON(hyperfineStream);
      try
        FResult:= THyperfineResult.Create(
          hyperfineJSON.GetPath(cJSONHyperfineResult)
        );
        try
          if FConfig.Entries[index].Compiler = 'fpc' then
          begin
            results[index2].Compiler:= 'lazarus-3.0, fpc-3.2.2';
          end;

          results[index2].Name:= FConfig.Entries[index].Name;
          results[index2].Notes:= FConfig.Entries[index].Notes;
          results[index2].Result:= 0.0;
          results[index2].Count:= 0;
          for index1:= Low(FResult.Times) to High(FResult.Times) do
          begin
            if (time = FResult.Max) or (time = FResult.Max) then continue;
            results[index2].Result:= results[index2].Result + FResult.Times[index1];
            Inc(results[index2].Count);
          end;
          results[index2].Result:= results[index2].Result / results[index2].Count;
        finally
          FResult.Free;
        end;
      finally
        hyperfineJSON.Free;
      end;
    finally
      hyperfineStream.Free;
    end;
  end;

  WriteLn;
  WriteLn;

  //AnySort(results, Length(results), SizeOf(TResult), @CompareResults);

  content:= '';
  for index:= Low(results) to High(results) do
  begin
    content:= content + Format('| %d | %s | %s | %s | %s | |'+LineEnding, [
      index + 1,
      FormatTime(results[index].Result),
      results[index].Compiler,
      results[index].Name,
      results[index].Notes
    ]);
  end;

  Write(cTableHeader, content);
  WriteLn;
end;

function TResults.GenerateProgressBar(APBPosition, APBMax, APBWIdth: Integer
  ): String;
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
end;

end.
