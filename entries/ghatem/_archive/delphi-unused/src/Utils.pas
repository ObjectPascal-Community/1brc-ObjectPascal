unit Utils;

interface

type
  TProcedure = reference to procedure;


function Ceiling(const ANumber: Double): integer; inline;
function RoundExDouble(const ATemp: Double): Double; inline;
function RoundExInteger(const ATemp: Double): Integer; inline;

procedure Timer (aCaption: string; aProcedure: TProcedure); overload;
procedure Timer (aCaption: string; aProcedure: TProcedure; var aDuration: Int64); overload;


implementation

uses
  System.Diagnostics,
  System.SysUtils;


function Ceiling(const ANumber: Double): integer;
begin
  Result := Trunc(ANumber) + Ord(Frac(ANumber) > 0);
end;

function RoundExDouble(const ATemp: Double): Double;
var
  tmp: Double;
begin
  tmp:= ATemp * 10;
  Result := Ceiling(tmp) / 10;
end;

function RoundExInteger(const ATemp: Double): Integer;
var
  tmp: Double;
begin
  tmp:= ATemp * 10;
  Result := Ceiling(tmp);
end;

procedure Timer (aCaption: string; aProcedure: TProcedure);
var
  vStopWatch: TStopWatch;
begin
  vStopWatch := TStopWatch.Create;
  vStopWatch.Start;
    aProcedure;
  vStopWatch.Stop;
  WriteLn(aCaption + ': ' + FloatToStr(vStopWatch.ElapsedMilliseconds / 1000));
  WriteLn('-----------');
  WriteLn;
end;

procedure Timer (aCaption: string; aProcedure: TProcedure; var aDuration: Int64); overload;
var
  vStopWatch: TStopWatch;
begin
  vStopWatch := TStopWatch.Create;
  vStopWatch.Start;
    aProcedure;
  vStopWatch.Stop;
  aDuration := aDuration + vStopWatch.ElapsedMilliseconds;
end;

end.
