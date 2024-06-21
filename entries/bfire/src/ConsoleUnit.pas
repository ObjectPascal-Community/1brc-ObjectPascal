unit ConsoleUnit; // adapted from challenge data generator program

interface

uses
  System.SysUtils,
  Classes;

{$I 'version.inc'}

const
  cShortOptHelp: Char = 'h';
  cShortOptVersion: Char = 'v';
  cShortOptInput: Char = 'i';
  cShortOptOutput: Char = 'o';
  cShortOptReaders: Char = 'r';
  cShortOptNoTabulate: Char = 't';
  cShortOptions: array of Char = ['h', 'v', 'i', 'o', 'r', 't'];

resourcestring
  rsAppTitle = 'One Billion Row Challenge Entry';
  rsGeneratorVersion = 'OneBRC v%s';
  rsErrorMessage = 'ERROR: %s';
  rsMissingInputFlag = 'Missing input file flag.';
  rsMissingOutputFlag = 'Missing output file flag.';
  rsInvalidInteger = 'ERROR: Invalid integer "%s".';
  rsInputFile = 'Input Filename: "%s"';
  rsOutputFile = 'Output Filename: "%s"';

var
  inputFilename: String = '';
  outputFilename: String = '';
  FParams: TStringList;
  ReadThreadCountParam: String = '2'; // unless changed to 1, below
  NoTabulate: Boolean = True;
  // if true, file is read, but no data is sent to stacks

function ParseConsoleParams: Boolean;

implementation

procedure WriteHelp;
begin
  WriteLn('bfire -- An entry to the One Billion Row Challenge for Object Pascal');
  WriteLn;
  WriteLn('Usage');
  WriteLn('  bfire -h                       |  Write this help message and exit');
  WriteLn('  bfire -v                       |  Write the version and exit');
  WriteLn('  bfire -i <file_1>              |  <file_1> contains Weather Data');
  WriteLn('  bfire -i <file_1> -o <file_2>  |  <file_1> contains Weather Data');
  WriteLn('                                 |  <file_2> contains result');
  WriteLn('  If <file_2> is not defined, result goes to CONSOLE (STDOUT)');
  WriteLn;
  WriteLn('Experimental Options (use in addition to -o)');
  WriteLn('  bfire -i <file_1> -o <file_2> -r 1   | Use a single reading thread');
  WriteLn('  bfire -i <file_1> -o <file_2> -r 2   | Use two reading threads');
  WriteLn('  bfire -i <file_1> -o <file_2> -r 3   | Use three reading threads');
  WriteLn('  bfire -i <file_1> -o <file_2> -t 0   | Only read, do not tabulate');

end;

function CheckShortParams(const AParam: Char): Boolean;
var
  J: Integer;
begin
  Result := false; // default
  for J := 0 to Pred(Length(cShortOptions)) do
  begin
    if (AParam = cShortOptions[J]) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function ParseConsoleParams: Boolean;
var
  I, J, valid: Integer;
  ParamOK: Boolean;
  SkipNext: Boolean;
begin
  // initialize values
  valid := 0;
  ParamOK := false;
  ParseConsoleParams := false; // default
  inputFilename := '';
  outputFilename := '';
  // initialize the params list
  if not Assigned(FParams) then
    FParams := TStringList.Create(dupIgnore, false, false);

  J := 0;
  for I := 1 to ParamCount do
  begin
    if pos('-', ParamStr(I)) = 1 then
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
    SkipNext := false;
    for I := 0 to FParams.Count - 1 do
    begin
      if SkipNext then
      begin
        SkipNext := false;
        Continue;
      end;

      if (Length(FParams[I]) = 1) or (FParams[I][2] = '=') then
      begin
        ParamOK := CheckShortParams(FParams[I][1]);
      end;

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
    WriteHelp;
    exit;
  end;

  // ************************************
  // check for valid inputs
  // check help
  if (FParams.Find(cShortOptHelp, J)) then
  begin
    WriteHelp;
    exit;
  end;

  // check version
  if (FParams.Find(cShortOptVersion, J)) then
  begin
    WriteLn(Format(rsGeneratorVersion, [cVersion]));
    exit;
  end;

  // check inputfilename
  J := -1;
  J := FParams.IndexOfName(cShortOptInput);
  if J = -1 then
  begin
    WriteLn(Format(rsErrorMessage, [rsMissingInputFlag]));
  end
  else
  begin
    inputFilename := FParams.ValueFromIndex[J];
    inc(valid);
  end;

  // check outputfilename
  J := -1;
  J := FParams.IndexOfName(cShortOptOutput);
  if J = -1 then // send to console
  begin
    outputFilename := '';
    inc(valid);
  end
  else
  begin
    outputFilename := FParams.ValueFromIndex[J];
    inc(valid);
  end;

  // check for read thread count
  J := -1;
  J := FParams.IndexOfName(cShortOptReaders);
  if J > -1 then // test value
  begin
    if FParams.ValueFromIndex[J] = '1' then
    begin
      ReadThreadCountParam := '1';
    end
    else
    begin
      if FParams.ValueFromIndex[J] = '2' then
      begin
        ReadThreadCountParam := '2';
      end
      else
      begin
        if FParams.ValueFromIndex[J] = '3' then
        begin
          ReadThreadCountParam := '3';
        end
        else
        begin
          WriteLn('Invalid value for Read Thread Count: ' +
            FParams.ValueFromIndex[J]);
          exit;
        end;
      end;
    end;
  end;

  // check for no tabulate flag
  J := -1;
  J := FParams.IndexOfName(cShortOptNoTabulate);
  if J > -1 then // test value
  begin
    if FParams.ValueFromIndex[J] = '0' then
    begin
      NoTabulate := True;
    end
    else
    begin
      WriteLn('Invalid value for No Tabulate Flag: ' +
        FParams.ValueFromIndex[J]);
      exit;
    end;
  end;

  // check if everything was provided
  ParseConsoleParams := valid = 2;
end;

end.
