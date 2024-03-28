unit RoundEx.Common;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  SysUtils
, Math
;

function RoundExDouble(x: Double): Double;
function RoundExInteger(x: Double): Integer;
function GenerateProgressBar(APBPosition, APBMax, APBWIdth: Integer): String;

implementation

function RoundExDouble(x: Double): Double;
begin
  Result := Ceil(x * 10) / 10;
end;

function RoundExInteger(x: Double): Integer;
begin
  Result := Ceil(x * 10);
end;

function GenerateProgressBar(APBPosition, APBMax, APBWIdth: Integer): String;
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

