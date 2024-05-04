program generator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes
, SysUtils
, CustApp
, Generate.Console
, Generate.Common
;

{$I version.inc}

type

{ TOneBRCGenerator }
  TOneBRCGenerator = class(TCustomApplication)
  private
    FGenerator: TGenerator;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

{ TOneBRCGenerator }

procedure TOneBRCGenerator.DoRun;
var
  ErrorMsg: String;
  tmpLineCount, tmpLineEnd: String;
begin
  // quick check parameters
  ErrorMsg:= CheckOptions(Format('%s%s%s:%s:%s:%s%s:',[
      cShortOptHelp,
      cShortOptVersion,
      cShortOptInput,
      cShortOptOutput,
      cShortOptNumber,
      cShortOptStations,
      cShortOptLineEnd
    ]),
    [
      cLongOptHelp,
      cLongOptVersion,
      cLongOptInput+':',
      cLongOptOutput+':',
      cLongOptNumber+':',
      cLongOptStations,
      cLongOptLineEnd+':'
    ]
  );
  if ErrorMsg<>'' then
  begin
    //ShowException(Exception.Create(ErrorMsg));
    WriteLn(Format(rsErrorMessage, [ ErrorMsg ]));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption(cShortOptHelp, cLongOptHelp) then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption(cShortOptVersion, cLongOptVersion) then
  begin
    WriteLn(Format(rsGeneratorVersion, [ cVersion ]));
    Terminate;
    Exit;
  end;

  if HasOption(cShortOptInput, cLongOptInput) then
  begin
    inputFilename:= GetOptionValue(
      cShortOptInput,
      cLongOptInput
    );
  end
  else
  begin
    WriteLn(Format(rsErrorMessage, [ rsMissingInputFlag ]));
    Terminate;
    Exit;
  end;

  if HasOption(cShortOptOutput, cLongOptOutput) then
  begin
    outputFilename:= GetOptionValue(
      cShortOptOutput,
      cLongOptOutput
    );
  end
  else
  begin
    WriteLn(Format(rsErrorMessage, [ rsMissingOutputFlag ]));
    Terminate;
    Exit;
  end;

  if HasOption(cShortOptNumber, cLongOptNumber) then
  begin
    tmpLineCount:=GetOptionValue(
      cShortOptNumber,
      cLongOptNumber
    );
    tmpLineCount:= StringReplace(tmpLineCount, '_', '', [rfReplaceAll]);
    if not TryStrToInt(tmpLineCount, lineCount) then
    begin
      WriteLn(Format(rsInvalidInteger, [ tmpLineCount ]));
      Terminate;
      Exit;
    end;
    if not (lineCount > 0) then
    begin
      WriteLn(Format(rsErrorMessage, [ rsInvalidLineNumber ]));
      Terminate;
      Exit;
    end;
  end
  else
  begin
    WriteLn(Format(rsErrorMessage, [ rsMissingLineCountFlag ]));
    Terminate;
    Exit;
  end;

  only400stations := HasOption(cShortOptStations, cLongOptStations);

  if HasOption(cShortOptLineEnd, cLongOptLineEnd) then
  begin
    tmpLineEnd:=GetOptionValue(
      cShortOptLineEnd,
      cLongOptLineEnd
    );
    if (UpperCase(tmpLineEnd) <> 'CRLF') and (UpperCase(tmpLineEnd) <> 'LF') then
    begin
      WriteLn(rsInvalidLineEnd);
      Terminate;
      Exit;
    end
    else if UpperCase(tmpLineEnd) = 'CRLF' then
      lineEnding := #13#10;
  end;

  inputFilename:= ExpandFileName(inputFilename);
  outputFilename:= ExpandFileName(outputFilename);

  WriteLn(Format(rsInputFile, [ inputFilename ]));
  WriteLn(Format(rsOutputFile, [ outputFilename ]));
  WriteLn(Format(rsLineCount, [ Double(lineCount) ]));
  WriteLn;

  FGenerator:= TGenerator.Create(inputFilename, outputFilename, lineCount, only400stations, lineEnding);
  try
    try
      FGenerator.Generate;
    except
      on E: Exception do
      begin
        WriteLn(Format(rsErrorMessage, [ E.Message ]));
      end;
    end;
  finally
    FGenerator.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TOneBRCGenerator.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TOneBRCGenerator.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TOneBRCGenerator;
begin
  Application:=TOneBRCGenerator.Create(nil);
  Application.Title:= rsAppTitle;
  Application.Run;
  Application.Free;
end.

