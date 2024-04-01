unit RoundEx.Common;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  SysUtils;

function RoundExDouble(x: double): Double;
function RoundExInteger(x: double): Integer;
function GenerateProgressBar(APBPosition, APBMax, APBWIdth: Integer): String;

implementation

// we define our own "official" cross-compiler ceil() rounding method: the math
// unit seems not consistent between Delphi and FPC

function ceil(x: double): integer;
begin
  result := trunc(x) + ord(frac(x) > 0);  // using FPU is fast enough here
end;

function RoundExDouble(x: double): Double;
begin
  Result := Ceil(x * 10) / 10;
end;

function RoundExInteger(x: double): Integer;
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

