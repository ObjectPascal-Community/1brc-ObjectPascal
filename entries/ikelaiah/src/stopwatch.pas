unit Stopwatch;

{$mode objfpc}{$H+}{$J-}{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils;

procedure StartTimer;
procedure StopTimer;
procedure ResetTimer;
procedure DisplayTimer;

implementation

var
  startTime: QWord = 0;
  endTime: QWord = 0;
  elapsedMilliseconds: QWord = 0;
  hours, minutes, seconds, milliseconds: word;

procedure StartTimer;
begin
  startTime := GetTickCount64;
end;

procedure StopTimer;
begin
  endTime := GetTickCount64;
end;

procedure ResetTimer;
begin
  startTime := 0;
  endTime := 0;
  elapsedMilliseconds := 0;
end;

procedure DisplayTimer;
begin

  // Elapsed milliseconds
  elapsedMilliseconds := endTime - startTime;

  // Convert milliseconds to hours, minutes, seconds, and milliseconds
  hours := elapsedMilliseconds div 3600000;
  elapsedMilliseconds := elapsedMilliseconds mod 3600000;

  minutes := elapsedMilliseconds div 60000;
  elapsedMilliseconds := elapsedMilliseconds mod 60000;

  seconds := elapsedMilliseconds div 1000;
  milliseconds := elapsedMilliseconds mod 1000;

  WriteLn('------------------------------');
  WriteLn('Elapsed time: ', hours, ' hours ', minutes, ' minutes ',
    seconds, ' seconds ', milliseconds, ' milliseconds');
  WriteLn('------------------------------');
end;


end.
