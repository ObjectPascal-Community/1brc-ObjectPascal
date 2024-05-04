unit ResultsGenerator.Common;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes
, SysUtils
, fgl
, Utilities.Data.Config
, Utilities.Data.Entries
, Utilities.Data.Hyperfine
;

type

{ TResult }
  TResult = class
    Name: String;
    Notes: String;
    Compiler: String;
    GoodHash: Boolean;
    Result: Double;
    Count: Integer;
    constructor Create;
  end;

{ TResultsList }
  TResultsList = specialize TFPGObjectList<TResult>;

{ TResults }
  TResults = class(TObject)
  private
    FConfig: TConfig;
    FResult: THyperfineResult;
    FList: TResultsList;

    function GenerateProgressBar(APBPosition, APBMax, APBWIdth: Integer): String;
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
;

const
  lineBreak = #13;

  cTableHeader =
    '| # | Result (m:s.ms): SSD | Compiler | Submitter     | Notes     | Certificates |'#10 +
    '|--:|---------------------:|:---------|:--------------|:----------|:-------------|'#10;

function CompareResults(const elem1, elem2: TResult): Integer;
begin
  if elem1.Result = elem2.Result then Result:= 0
  else if elem1.Result < elem2.Result then Result:= -1
  else Result:= 1;
end;

{ TResult }

constructor TResult.Create;
begin
  inherited Create;
  Name := '';
  Notes := '';
  Compiler := '';
  GoodHash := False;
  Result := 0;
  Count := 0;
end;

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
  FList := TResultsList.Create;
end;

destructor TResults.Destroy;
begin
  FConfig.Free;
  FList.Free;
  inherited Destroy;
end;

function TResults.FormatTime(ATime: Double): String;
var
  intPart, minutes: Integer;
  millis: String;
begin
  Result:= '';
  intPart:= Trunc(ATime);
  millis := FormatFloat('000', Trunc((ATime - intPart) * 1000));
  minutes := intPart div 60;
  intPart := intPart mod 60;
  Result:= Format('%d:%d.%s',[
    minutes,
    intPart,
    millis
  ]);
end;

procedure TResults.Generate;
var
  index, index2: Integer;
  content, hyperfineFile: String;
  resultitem: TResult;
  hyperfineStream: TFileStream;
  hyperfineJSON: TJSONData;
begin
  FList.Clear;

  for index:= 0 to Pred(FConfig.Entries.Count) do
  begin
    if not FConfig.Entries[index].Active then continue;
    Write(GenerateProgressBar(Succ(index), FConfig.Entries.Count, 50), lineBreak);
    hyperfineFile:= ExpandFileName(
        IncludeTrailingPathDelimiter(FConfig.ResultsFolder)+
        FConfig.Entries[index].EntryBinary+
        '-1_000_000_000-SSD.json'
      );
    if not FileExists(hyperfineFile) then continue;
    resultitem := TResult.Create;
    FList.Add(resultitem);
    index2 := FList.Count - 1;
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
            if FConfig.Entries[index].UseTrunk then
            begin
              FList[index2].Compiler:= 'lazarus-3.99, fpc-3.3.1';
            end
            else
            begin
              FList[index2].Compiler:= 'lazarus-3.0, fpc-3.2.2';
            end;
          end;
          if FConfig.Entries[index].Compiler = 'delphi' then
          begin
            FList[index2].Compiler:= 'delphi 12.1';
          end;

          FList[index2].Name:= UTF8Encode(FConfig.Entries[index].Name);
          FList[index2].Notes:= UTF8Encode(FConfig.Entries[index].Notes);
          FList[index2].Count := FResult.times.Count;
          FList[index2].GoodHash := FConfig.Entries[index].GoodHash;
          FList[index2].Result:= FResult.times.AvgValueWithOutMinMax;
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

  FList.Sort(@CompareResults);

  content:= '';
  for index:= 0 to FList.Count - 1 do
  begin
    if not FList[index].GoodHash then FList[index].Notes:= FList[index].Notes + ' **(failed hash)**';
    content:= content + Format('| %2d | %9s | %-23s | %-22s | %-34s | |'+LineEnding, [
      index + 1,
      FormatTime(FList[index].Result),
      FList[index].Compiler,
      FList[index].Name,
      FList[index].Notes
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
