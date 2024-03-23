unit Utilities.Data.Hyperfine;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, fpjson
;

const
  cJSONHyperfineResult = 'results[0]';

type
{ EResultNotAJSONObject }
  EResultNotAJSONObject = Exception;

{ THyperfineResult }
  TArrayOfTime = array of Double;
  THyperfineResult = class(TObject)
  private
    FCommand: String;
    FMean: Double;
    FStandardDeviation: Double;
    FMedian: Double;
    FUser: Double;
    FSystem: Double;
    FMin: Double;
    FMax: Double;
    FTimes: TArrayOfTime;

    procedure setFromJSONData(const AJSONData: TJSONData);
    procedure setFromJSONObject(const AJSONObject: TJSONObject);
  protected
  public
    constructor Create;
    constructor Create(const AJSONData: TJSONData);

    destructor Destroy; override;

    property Command: String
      read FCommand
      write FCommand;
    property Mean: Double
      read fMean
      write FMean;
    property StandardDeviation: Double
      read FStandardDeviation
      write FStandardDeviation;
    property Median: Double
      read FMedian
      write FMedian;
    property User: Double
      read FUser
      write FUser;
    property System: Double
      read FSystem
      write FSystem;
    property Min: Double
      read FMin
      write FMin;
    property Max: Double
      read FMax
      write FMax;
    property Times: TArrayOfTime
      read FTimes
      write FTimes;
  published
  end;

implementation

const
  cJSONCommand           = 'command';
  cJSONMean              = 'mean';
  cJSONStandardDeviation = 'stddev';
  cJSONMedian            = 'median';
  cJSONUser              = 'user';
  cJSONSystem            = 'system';
  cJSONMin               = 'min';
  cJSONMax               = 'max';
  cJSONTimes             = 'times';

resourcestring
  rsExceptionNotAJSONObject = 'JSON Data is not an object';

{ THyperfineResult }

constructor THyperfineResult.Create;
begin
  FCommand:= '';
  FMean:= 0.0;
  FStandardDeviation:= 0.0;
  FMedian:= 0.0;
  FUser:= 0.0;
  FSystem:= 0.0;
  FMin:= 0.0;
  FMax:= 0.0;
  SetLength(FTimes, 0);
end;

constructor THyperfineResult.Create(const AJSONData: TJSONData);
begin
  Create;
  setFromJSONData(AJSONData);
end;

destructor THyperfineResult.Destroy;
begin
  inherited Destroy;
end;

procedure THyperfineResult.setFromJSONData(const AJSONData: TJSONData);
begin
  if aJSONData.JSONType <> jtObject then
  begin
    raise EResultNotAJSONObject.Create(rsExceptionNotAJSONObject);
  end;
  setFromJSONObject(aJSONData as TJSONObject);
end;

procedure THyperfineResult.setFromJSONObject(const AJSONObject: TJSONObject);
var
  timesObject: TJSONArray;
  index: Integer;
begin
  FCommand:= AJSONObject.Get(cJSONCommand, FCommand);
  FMean:= AJSONObject.Get(cJSONMean, FMean);
  FStandardDeviation:= AJSONObject.Get(cJSONStandardDeviation, FStandardDeviation);
  FMedian:= AJSONObject.Get(cJSONMedian, FMedian);
  FUser:= AJSONObject.Get(cJSONUser, FUser);
  FSystem:= AJSONObject.Get(cJSONSystem, FSystem);
  FMin:= AJSONObject.Get(cJSONMin, FMin);
  FMax:= AJSONObject.Get(cJSONMax, FMax);
  timesObject:= AJSONObject.Arrays[cJSONTimes];
  SetLength(FTimes, timesObject.Count);
  for index:= 0 to Pred(timesObject.Count) do
  begin
    FTimes[index]:= timesObject[index].AsFloat;
  end;
end;

end.

