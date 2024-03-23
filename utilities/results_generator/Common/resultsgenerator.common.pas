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
;

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

procedure TResults.Generate;
type
  TResult = record
    Result: Double;
    Count: Integer;
    Compiler: String;
  end;
  TResultsArray = array of TResult;

var
  index, index1: Integer;
  content, hyperfineFile: String;
  results: TResultsArray;
  hyperfineStream: TFileStream;
  hyperfineJSON: TJSONData;
begin
  content:= '';
  SetLength(results, FConfig.Entries.Count);

  for index:= 0 to Pred(FConfig.Entries.Count) do
  begin
    Write(GenerateProgressBar(Succ(index), FConfig.Entries.Count, 50), lineBreak);
    hyperfineFile:= ExpandFileName(
        IncludeTrailingPathDelimiter(FConfig.ResultsFolder)+
        FConfig.Entries[index].EntryBinary+
        '-1_000_000_000-SSD.json'
      );
    if not FileExists(hyperfineFile) then continue;
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
            results[index].Compiler:= 'lazarus-3.0, fpc-3.2.2';
          end;

          results[index].Result:= 0.0;
          results[index].Count:= 0;
          for index1:= Low(FResult.Times) to High(FResult.Times) do
          begin
            if (time = FResult.Max) or (time = FResult.Max) then continue;
            results[index].Result:= results[index].Result + FResult.Times[index1];
            Inc(results[index].Count);
          end;
          results[index].Result:= results[index].Result / results[index].Count;
          { #todo 99 -ogcarreno : needs to be done after sorting array by time }
          content:= content + Format('| %d | %.2f | %s | %s | %s | |'+LineEnding, [
            index,
            results[index].Result,
            results[index].Compiler,
            FConfig.Entries[index].Name,
            FConfig.Entries[index].Notes
          ]);
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
  Write(cTableHeader, content);
  // The results
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
