unit RoundEx.Common;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  SysUtils
, Math
;

function RoundExDouble(x: Currency): Double;
function RoundExInteger(x: Currency): Integer;
function GenerateProgressBar(APBPosition, APBMax, APBWIdth: Integer): String;

implementation

function RoundExDouble(x: Currency): Double;
begin
  Result := Ceil(x * 10) / 10;
end;

function RoundExInteger(x: Currency): Integer;
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

