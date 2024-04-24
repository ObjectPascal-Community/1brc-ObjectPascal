program generator;

{$APPTYPE CONSOLE}

uses
  Classes,
  SysUtils,
  generate.common in '..\..\Common\generate.common.pas',
  generate.console in '..\..\Common\generate.console.pas';

{$I '..\..\Common\version.inc'}

type

  { TOneBRCGenerator }

  TOneBRCGenerator = class(TComponent)
  private
    FGenerator: TGenerator;
    FParams: TStringList;
    function CheckShortParams(const AParam: char): Boolean;
    function CheckLongParams(const AParam: string): Boolean;
  protected
    function ParseConsoleParams: boolean;
    procedure Run;
  public
    destructor Destroy; override;
  published
  end;

  { TOneBRCGenerator }

procedure TOneBRCGenerator.Run;
begin
  if ParseConsoleParams then
  begin
    inputFilename := ExpandFileName(inputFilename);
    outputFilename := ExpandFileName(outputFilename);

    WriteLn(Format(rsInputFile, [inputFilename]));
    WriteLn(Format(rsOutputFile, [outputFilename]));
    WriteLn(Format(rsLineCount, [Double(lineCount)]));
    WriteLn;

    FGenerator := TGenerator.Create(inputFilename, outputFilename, lineCount, only400Stations, lineEnding);
    try
      try
        FGenerator.generate;
      except
        on E: Exception do
        begin
          WriteLn(Format(rsErrorMessage, [E.Message]));
        end;
      end;
    finally
      FGenerator.Free;
    end;
  end;
end;

function TOneBRCGenerator.CheckLongParams(const AParam: string): Boolean;
var
  J: Integer;
begin
  Result := False;

  for J := 0 to Pred(Length(cLongOptions)) do
  begin
    if (AParam = cLongOptions[J]) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TOneBRCGenerator.CheckShortParams(const AParam: char): Boolean;
var
  J: Integer;
begin
  Result := False;

  for J := 0 to Pred(Length(cShortOptions)) do
  begin
    if (AParam = cShortOptions[J]) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

destructor TOneBRCGenerator.Destroy;
begin
  if Assigned(FParams) then
    FreeAndNil(FParams);
  inherited;
end;

function TOneBRCGenerator.ParseConsoleParams: boolean;
var
  I, J, invalid, valid: Integer;
  tmpLineCount, tmpLineEnd: String;
  ParamOK: Boolean;
  SkipNext: Boolean;
begin
  valid := 0;
  invalid := 0;

  // initialize the params list
  if not Assigned(FParams) then
    FParams := TStringList.Create(dupIgnore, false, false);

  J := 0;
  for I := 1 to ParamCount do
  begin
    if ParamStr(I).StartsWith('--') then
    begin
      FParams.Add(Copy(ParamStr(I), 3, ParamStr(I).Length));
      inc(J);
    end
    else if ParamStr(I).StartsWith('-') then
    begin
      FParams.Add(Copy(ParamStr(I), 2, ParamStr(I).Length));
      inc(J);
    end
    else
      FParams.Strings[J - 1] := FParams.Strings[J - 1] + '=' + ParamStr(I);
  end;

  // ************************************
  // parsing
  // ************************************
  // check for invalid input
  if FParams.Count > 0 then
  begin
    ParamOK := True;
    SkipNext := False;
    for I := 0 to FParams.Count - 1 do
    begin
      if SkipNext then
      begin
        SkipNext := False;
        Continue;
      end;

      if (Length(FParams[I]) = 1) or (FParams[I][2] = '=') then
        ParamOK := CheckShortParams(FParams[I][1])
      else if Pos('=', FParams[I]) > 0 then
        ParamOK := CheckLongParams(Copy(FParams[I], 1, Pos('=', FParams[I]) - 1))
      else
        ParamOK := CheckLongParams(FParams[I]);

      // if we found a bad parameter, don't need to check the rest of them
      if not ParamOK then
        Break;
    end;

    if not ParamOK then
    begin
      WriteLn(Format(rsErrorMessage, [FParams.CommaText]));
      Result := false;
      exit;
    end;
  end
  else
  begin
    Result := false;
    exit;
  end;

  // ************************************
  // check for valid inputs
  // check help
  if (FParams.Find(cShortOptHelp, J) or FParams.Find(cLongOptHelp, J)) then
  begin
    WriteHelp;
  end;

  // check version
  if (FParams.Find(cShortOptVersion, J) or FParams.Find(cLongOptVersion, J)) then
  begin
    WriteLn(Format(rsGeneratorVersion, [cVersion]));
  end;

  // check inputfilename
  J := FParams.IndexOfName(cShortOptInput);
  if J = -1 then
    J := FParams.IndexOfName(cLongOptInput);
  if J = -1 then
  begin
    WriteLn(Format(rsErrorMessage, [rsMissingInputFlag]));
    inc(invalid);
  end
  else
  begin
    inputFilename := FParams.ValueFromIndex[J];
    inc(valid);
  end;

  // check outputfilename
  J := FParams.IndexOfName(cShortOptOutput);
  if J = -1 then
    J := FParams.IndexOfName(cLongOptOutput);
  if J = -1 then
  begin
    WriteLn(Format(rsErrorMessage, [rsMissingOutputFlag]));
    inc(invalid);
  end
  else
  begin
    outputFilename := FParams.ValueFromIndex[J];
    inc(valid);
  end;

  // check linecount
  J := FParams.IndexOfName(cShortOptNumber);
  if J = -1 then
    J := FParams.IndexOfName(cLongOptNumber);
  if J = -1 then
  begin
    WriteLn(Format(rsErrorMessage, [rsMissingLineCountFlag]));
    inc(invalid);
  end
  else
  begin
    tmpLineCount := FParams.ValueFromIndex[J].Replace('_', '', [rfReplaceAll]);

    if not TryStrToInt(tmpLineCount, lineCount) then
    begin
      WriteLn(Format(rsInvalidInteger, [tmpLineCount]));
      inc(invalid);
    end;

    if not(lineCount > 0) then
    begin
      WriteLn(Format(rsErrorMessage, [rsInvalidLineNumber]));
      inc(invalid);
    end;
    inc(valid);
  end;

  only400Stations := (FParams.IndexOf(cShortOptStations) >= 0) or (FParams.IndexOf(cLongOptStations) >= 0);
  
  // check line ending
  J := FParams.IndexOfName(cShortOptLineEnd);
  if J = -1 then
    J := FParams.IndexOfName(cLongOptLineEnd);
  if J >= 0 then
  begin
    tmpLineCount := FParams.ValueFromIndex[J];
    
    if (UpperCase(tmpLineEnd) <> 'CRLF') and (UpperCase(tmpLineEnd) <> 'LF') then
    begin
      WriteLn(rsInvalidLineEnd);
      inc(invalid);
    end
    else 
    begin
      if UpperCase(tmpLineEnd) = 'CRLF' then
        lineEnding := #13#10;  
      inc(valid);
    end;    
  end;

  writeln(only400Stations);
  writeln(fparams.Text);

  // check if everything was provided
  Result := (valid = 3) and (invalid = 0);
end;

var
  Application: TOneBRCGenerator;

begin
  Application := TOneBRCGenerator.Create(nil);
  Application.Run;
  Application.Free;

  {$IFDEF DEBUG}
  Writeln('Press ENTER...');
  Readln;
  {$ENDIF}
end.
