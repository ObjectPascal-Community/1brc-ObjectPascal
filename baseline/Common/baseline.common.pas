unit Baseline.Common;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes
, SysUtils
;

function RoundExDouble(const ATemp: Double): Double;
function RoundExInteger(const ATemp: Double): Integer;

implementation

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

end.
