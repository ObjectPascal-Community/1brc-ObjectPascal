program official_obrc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  Math,
  Contnrs;

type
  PWS = ^TWS;
  TWS = record
    FStation: String[100];
    FMin: Single;
    FMax: Single;
    FTot: Single;
    FCnt: Integer;
  end;

var
  HashList: TFPHashList;

procedure AddToHashList(AStation: String; ATemp: Single);
var
  WS: PWS;
  Index: Integer;
begin
  Index := HashList.FindIndexOf(AStation);
  if Index = -1 then
  begin
    New(WS);
    WS^.FStation := AStation;
    WS^.FMin := ATemp;
    WS^.FMax := ATemp;
    WS^.FTot := ATemp;
    WS^.FCnt := 1;
    HashList.Add(AStation, WS);
  end
  else
  begin
    WS := HashList.Items[Index];
    WS^.FMin := Min(WS^.FMin, ATemp);
    WS^.FMax := Max(WS^.FMax, ATemp);
    WS^.FTot := WS^.FTot + ATemp;
    WS^.FCnt := WS^.FCnt + 1;
  end;
end;

function Compare(List: TStringList; Index1, Index2: Integer): Integer;
var
  P1, P2: Integer;
  Str1, Str2: String;
begin
  Result := 0;
  Str1 := List.Strings[Index1];
  Str2 := List.Strings[Index2];
  P1 := Pos('=', Str1);
  P2 := Pos('=', Str2);
  if (P1 > 0) and (P2 > 0) then
  begin
    Str1 := Copy(Str1, 1, P1 - 1);
    Str2 := Copy(Str2, 1, P2 - 1);
    Result :=  CompareStr(Str1, Str2);
  end;
end;

var
  P, I, Code: Integer;
  Str: String;
  Station: String;
  TempStr: String;
  Temp: Single;
  Mean: Single;
  TF: TextFile;
  SL: TStringList;
  WS: PWS;
begin
  if not FileExists(ParamStr(1)) then
  begin
    Writeln('File not exists bla bla!');
    Halt;
  end;
  HashList := TFPHashList.Create;
  try
    AssignFile(TF, ParamStr(1));
    Reset(TF);
    while not Eof(TF) do
    begin
      ReadLn(TF, Str);
      P := Pos(';', Str);
      if P > 0 then
      begin
        Station := Copy(Str, 1, P - 1);
        TempStr := Copy(Str, P + 1, Length(Str));
        Val(TempStr, Temp, Code);
        if Code <> 0 then
          Continue;
        AddToHashList(Station, Temp);
      end;
    end;
    CloseFile(TF);

    SL := TStringList.Create;
    try
      SL.DefaultEncoding := TEncoding.UTF8;
      SL.BeginUpdate;
      for I := 0 to HashList.Count - 1 do
      begin
        WS := HashList.Items[I];
        WS^.FTot := Round(WS^.FTot*10)/10;
        Mean := WS^.FTot/WS^.FCnt;
        Str := WS^.FStation + '=' + FormatFloat('0.0', WS^.FMin) + '/' + FormatFloat('0.0', Mean) + '/' + FormatFloat('0.0', WS^.FMax) + ',';
        SL.Add(Str);
      end;
      SL.EndUpdate;
      SL.CustomSort(@Compare);
      //Save SL to file write to console, etc...
    finally
      SL.Free;
    end;
  finally
    HashList.Free;
  end;
end.
