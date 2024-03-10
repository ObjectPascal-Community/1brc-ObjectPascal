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
  FTick: QWord;
  FWSManager: TWSManager;
  MStart, MFinish: TMethod;


procedure DoOnStart(Sender: TObject);
begin
  Writeln('Process started at: ' + FormatDateTime('hh:mm:ss:zzz', Now));
  FTick := GetTickCount64;
end;

procedure DoOnFinish(Sender: TObject);
begin
  FTick := GetTickCount64 - FTick;
  Writeln('Process finished at: ' + FormatDateTime('hh:mm:ss:zzz', Now));
  Writeln('Duration: ' + IntToStr(FTick) + ' ms');
  Halt;
end;

begin
  if (ParamStr(1) = '-h') or (ParamStr(1) = '/?') or (ParamStr(1) = '--help') then
  begin
    Writeln('USAGE: obrc <source_file> <destination_file> <threadCnt(optional-default 16)>');
    Halt;
  end;

  if (Trim(ParamStr(1)) = '') or (not FileExists(ParamStr(1))) then
  begin
    Writeln('Please specify a valid source file');
    Halt;
  end;

  {if (Trim(ParamStr(2)) = '')  then
  begin
    Writeln('Please specify a valid destination file');
    Halt;
  end;}

  if (Trim(ParamStr(2)) <> '')  then
    TC := StrToIntDef(ParamStr(3), 16)
  else
    TC := 16;

  FWSManager := TWSManager.Create(ParamStr(1), TC);
  MStart.Data := nil;
  MStart.Code := @DoOnStart;
  FWSManager.OnStart := TNotifyEvent(MStart);
  MFinish.Data := nil;
  MFinish.Code := @DoOnFinish;
  FWSManager.OnFinish := TNotifyEvent(MFinish);
  FWSManager.WSThreadsWatcher.Start;
  repeat
    Sleep(1500);
  until 0 = 1;
end.

