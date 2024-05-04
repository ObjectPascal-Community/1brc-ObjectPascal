program obrc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  uWeatherStations;

var
  TC: Integer;
  FWSManager: TWSManager;

begin
  if (ParamStr(1) = '-h') or (ParamStr(1) = '/?') or (ParamStr(1) = '--help') then
  begin
    Writeln('USAGE: obrc <source_file> <threadCnt(optional-default 16)>');
    Halt;
  end;

  if (Trim(ParamStr(1)) = '') or (not FileExists(ParamStr(1))) then
  begin
    Writeln('Please specify a valid source file.');
    Halt;
  end;

  if (Trim(ParamStr(2)) <> '')  then
  begin
    TC := StrToIntDef(ParamStr(2), 8);
    if TC < 8 then
      TC := 8;
  end
  else
    TC := 8;

  FWSManager := TWSManager.Create(ParamStr(1), TC);
  FWSManager.WSThreadsWatcher.Start;
  while FWSManager.Done = False do
    Sleep(100);
  FWSManager.Free;
end.

