unit Utilities.Data.Hyperfine;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, fpjson
, fpjsonrtti
, fgl
, typinfo
;

const
  cJSONHyperfineResult = 'results[0]';

type

{ EResultNotAJSONObject }
  EResultNotAJSONObject = Exception;

{ TDoubleList }
  TDoubleList = class(specialize TFPGList<Double>)
  public
    function AvgValue: Double;
    function AvgValueWithOutMinMax: Double;
  end;

{ TIntegerList }
  TIntegerList = specialize TFPGList<Integer>;

{ THyperfineResult }
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
    FTimes: TDoubleList;
    FExitCodes: TIntegerList;

    procedure setFromJSONData(const AJSONData: TJSONData);
    procedure setFromJSONObject(const AJSONObject: TJSONObject);
    procedure OnRestoreProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
  protected
  public
    constructor Create;
    constructor Create(const AJSONData: TJSONData);

    destructor Destroy; override;
  published
    property command: String
      read FCommand
      write FCommand;
    property mean: Double
      read fMean
      write FMean;
    property stddev: Double
      read FStandardDeviation
      write FStandardDeviation;
    property median: Double
      read FMedian
      write FMedian;
    property user: Double
      read FUser
      write FUser;
    property system: Double
      read FSystem
      write FSystem;
    property min: Double
      read FMin
      write FMin;
    property max: Double
      read FMax
      write FMax;
    property times: TDoubleList
      read FTimes
      write FTimes;
    property exit_codes: TIntegerList
      read FExitCodes
      write FExitCodes;
  end;

implementation

resourcestring
  rsExceptionNotAJSONObject = 'JSON Data is not an object';

function CompareDouble(const d1, d2: Double): Integer;
begin
  if d1 = d2 then Result:= 0
  else if d1 < d2 then Result:= -1
  else Result:= 1;
end;

{ TDoubleList }

function TDoubleList.AvgValue: Double;
var
  i: Integer;
  sum: Double;
begin
  Result := 0;
  if Count = 0 then
    exit;
  sum := 0;
  for i := 0 to Count - 1 do
    sum := sum + Items[i];
  Result := sum / Count;
end;

function TDoubleList.AvgValueWithOutMinMax: Double;
var
  sortedlist: TDoubleList;
  i: Integer;
  sum: Double;
begin
  Result := 0;
  if Count <= 2 then
    exit;
  sum := 0;
  sortedlist := TDoubleList.Create;
  for i := 0 to Count - 1 do
    sortedlist.Add(Items[i]);
  sortedlist.Sort(@CompareDouble);
  for i := 1 to sortedlist.Count - 2 do
    sum := sum + sortedlist[i];
  Result := sum / (sortedlist.Count - 2);
  sortedlist.Free;
end;

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
  FTimes := TDoubleList.Create;
  FExitCodes := TIntegerList.Create;
end;

constructor THyperfineResult.Create(const AJSONData: TJSONData);
begin
  Create;
  setFromJSONData(AJSONData);
end;

destructor THyperfineResult.Destroy;
begin
  FTimes.Free;
  FExitCodes.Free;
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
  jds: TJSONDestreamer;
begin
  jds := TJSONDestreamer.Create(nil);
  jds.OnRestoreProperty := @OnRestoreProperty;
  try
    jds.JSONToObject(AJSONObject.AsJSON, Self);
  finally
    jds.Free;
  end;
end;

procedure THyperfineResult.OnRestoreProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
var
  i: Integer;
begin
  Handled := False;
  if (Info^.Name = 'times') then
  begin
    Handled := True;
    for i := 0 to AValue.Count - 1 do
      THyperfineResult(AObject).times.Add(Avalue.Items[i].AsFloat);
  end
  else if (Info^.Name = 'exit_codes') then
  begin
    Handled := True;
    for i := 0 to AValue.Count - 1 do
      THyperfineResult(AObject).exit_codes.Add(Avalue.Items[i].AsInteger);
  end;
end;

end.

