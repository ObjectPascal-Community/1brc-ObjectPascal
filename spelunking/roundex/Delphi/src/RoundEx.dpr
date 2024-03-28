program RoundEx;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils
, System.Classes
, RoundEx.Common in '../../Common/roundex.common.pas'
;

var
  index: Integer;
  temperature: Integer;
  output: AnsiString;
  outputStream: TFileStream;
  fs: TFormatSettings;

begin
  fs.DecimalSeparator := '.';
  WriteLn('Delphi RoundExDouble');
  index:= 0;
  output:= '';
  for temperature:= -999 to 999 do
  begin
    Inc(index);
    Write(GenerateProgressBar(index, 1999, 50), #13);
    output:= output + FloatToStr(RoundExDouble(temperature / 10), fs)  + ', ';
  end;
  SetLength(output, Length(output) - 2);
  {$IFDEF LINUX}
  outputStream:= TFileStream.Create('RoundExDouble-Linux.txt', fmCreate);
  {$ELSE}
  outputStream:= TFileStream.Create('RoundExDouble-Windows.txt', fmCreate);
  {$ENDIF}
  outputStream.WriteBuffer(output[1], Length(output));
  outputStream.Free;

  WriteLn;
  WriteLn;
  WriteLn('Delphi RoundExInteger');
  index:= 0;
  output:= '';
  for temperature:= -999 to 999 do
  begin
    Inc(index);
    Write(GenerateProgressBar(index, 1999, 50), #13);
    output:= output + IntToStr(RoundExInteger(temperature / 10))  + ', ';
  end;
  SetLength(output, Length(output) - 2);
  {$IFDEF LINUX}
  outputStream:= TFileStream.Create('RoundExInteger-Linux.txt', fmCreate);
  {$ELSE}
  outputStream:= TFileStream.Create('RoundExInteger-Windows.txt', fmCreate);
  {$ENDIF}
  outputStream.WriteBuffer(output[1], Length(output));
  outputStream.Free;

  WriteLn;

end.
