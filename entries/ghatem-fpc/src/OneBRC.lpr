program OneBRC;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, Lclintf, UTF8Process, syncobjs,
  mormot.core.os, mormot.core.base,
  Baseline.Console;

const
  cDictSize: Integer = 45007;

  c0ascii: ShortInt = 48;
  c9ascii: ShortInt = 57;
  cNegAscii: ShortInt = 45;

type
  //---------------------------------------
  { TOneBRCApp }

  TOneBRCApp = class(TCustomApplication)
  private
    FFileName: string;
    FThreadCount: Integer;
    procedure RunOneBRC;
  protected
    procedure DoRun; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  //---------------------------------------

  // record is packed to minimize its size
  // use pointers to avoid passing entire records around
  TStationData = packed record
    Min: SmallInt;
    Max: SmallInt;
    Count: UInt32;
    Sum: Integer;
  end;
  PStationData = ^TStationData;

  TKeys = array [0..45006] of Cardinal;
  TStationNames = array [0..45006] of AnsiString;
  TValues = array [0..45006] of PStationData;

  //---------------------------------------
  { TMyDictionary }

  TMyDictionary = class
  private
    FHashes: TKeys;
    FValues: TValues;
    FRecords: array [0..45006] of TStationData;
    // store the station names outside of the record as they are filled only upon first encounter
    FStationNames: TStationNames;
    procedure InternalFind(const aKey: Cardinal; out aFound: Boolean; out aIndex: Integer);
  public
    constructor Create;
    property Keys: TKeys read FHashes;
    property StationNames: TStationNames read FStationNames;
    property Values: TValues read FValues;
    function TryGetValue (const aKey: Cardinal; out aValue: PStationData): Boolean; inline;
    procedure Add (const aKey: Cardinal; const aValue: PStationData); inline;
    procedure AddName (const aKey: Cardinal; const aStationName: AnsiString); inline;
  end;

  //---------------------------------------
  { TOneBRC }

  TOneBRC = class
  private
    // mormot memory map for fast bytes read.
    // I tried using CreateFileMapping and failed, more details in unit FileReader
    FMemoryMap: TMemoryMap;
    FData: pAnsiChar;
    FDataSize: Int64;
    FCS: TCriticalSection;

    FThreadCount: UInt16;
    FThreads: array of TThread;
    FStationsDicts: array of TMyDictionary;

    FStationsNames: TMyDictionary;

    procedure ExtractLineData(const aStart: Int64; const aEnd: Int64; out aLength: ShortInt; out aTemp: SmallInt); inline;

  public
    constructor Create (const aThreadCount: UInt16);
    destructor Destroy; override;
    function mORMotMMF (const afilename: string): Boolean;
    procedure DispatchThreads;
    procedure WaitAll;
    procedure ProcessData (aThreadNb: UInt16; aStartIdx: Int64; aEndIdx: Int64);
    procedure Merge (aLeft: UInt16; aRight: UInt16);
    procedure MergeAll;
    procedure GenerateOutput;
    property DataSize: Int64 read FDataSize;
  end;

  //---------------------------------------
  { TBRCThread }

  TThreadProc = procedure (aThreadNb: UInt16; aStartIdx: Int64; aEndIdx: Int64) of object;

  TBRCThread = class (TThread)
  private
    FProc: TThreadProc;
    FThreadNb: UInt16;
    FStart: Int64;
    FEnd: Int64;
  protected
    procedure Execute; override;
  public
    constructor Create (aProc: TThreadProc; aThreadNb: UInt16; aStart: Int64; aEnd: Int64);
  end;

{ TOneBRCApp }

procedure TOneBRCApp.RunOneBRC;
var
  vOneBRC: TOneBRC;
begin
  vOneBRC := TOneBRC.Create (FThreadCount);
  try
    try
      vOneBRC.mORMotMMF(FFileName);
      vOneBRC.DispatchThreads;
      vOneBRC.WaitAll;
      vOneBRC.MergeAll;
      vOneBRC.GenerateOutput;
    except
      on E: Exception do
      begin
        WriteLn(Format(rsErrorMessage, [ E.Message ]));
        ReadLn;
      end;
    end;
  finally
    vOneBRC.Free;
  end;
end;

//---------------------------------------------------

function Ceiling (const ANumber: Double): Integer; inline;
begin
  Result := Trunc(ANumber) + Ord(Frac(ANumber) > 0);
end;

function RoundExDouble (const aTemp: Double): Double; inline;
var
  vTmp: Double;
begin
  vTmp := aTemp * 10;
  Result := Ceiling (vTmp) / 10;
end;

//---------------------------------------------------
{ TOneBRC }

function Compare(AList: TStringList; AIndex1, AIndex2: Integer): Integer;
var
  Str1, Str2: String;
begin
  Str1 := AList.Strings[AIndex1];
  Str2 := AList.Strings[AIndex2];
  Result := CompareStr(Str1, Str2);
end;

{ TMyDictionary }

procedure TMyDictionary.InternalFind(const aKey: Cardinal; out aFound: Boolean; out aIndex: Integer);
var vIdx: Integer;
    vOffset: Integer;
begin
  vIdx := aKey * cDictSize shr 32;

  if FHashes[vIdx] = aKey then begin
    aIndex := vIdx;
    aFound := True;
  end
  else begin
    vOffset := 1;

    while True do begin
      // quadratic probing, by incrementing vOffset
      Inc (vIdx, vOffset);
      Inc (vOffset);

      // exceeded boundary, loop back
      if vIdx >= cDictSize then
        Dec (vIdx, cDictSize);

      if FHashes[vIdx] = aKey then begin
        // found match
        aIndex := vIdx;
        aFound := True;
        break;
      end;
      if FHashes[vIdx] = 0 then begin
        // found empty bucket to use
        aIndex := vIdx;
        aFound := False;
        break;
      end;
    end;
  end;
end;

constructor TMyDictionary.Create;
var
  I: Integer;
begin
  for I := 0 to cDictSize - 1 do begin
    FValues[I] := @FRecords[I];
  end;
end;

function TMyDictionary.TryGetValue(const aKey: Cardinal; out aValue: PStationData): Boolean;
var
  vIdx: Integer;
begin
  InternalFind (aKey, Result, vIdx);
  aValue := FValues[vIdx];
end;

procedure TMyDictionary.Add(const aKey: Cardinal; const aValue: PStationData);
var
  vIdx: Integer;
  vFound: Boolean;
begin
  InternalFind (aKey, vFound, vIdx);
  if not vFound then begin
    FHashes[vIdx] := aKey;
    FValues[vIdx] := aValue;
  end
  else
    raise Exception.Create ('TMyDict: cannot add, duplicate key');
end;

procedure TMyDictionary.AddName (const aKey: Cardinal; const aStationName: AnsiString);
var
  vIdx: Integer;
  vFound: Boolean;
begin
  InternalFind (aKey, vFound, vIdx);
  if not vFound then begin
    FHashes[vIdx] := aKey;
    FStationNames[vIdx] := aStationName;
  end
  else
    raise Exception.Create ('TMyDict: cannot add, duplicate key');
end;

procedure TOneBRC.ExtractLineData(const aStart: Int64; const aEnd: Int64; out aLength: ShortInt; out aTemp: SmallInt);
// given a line of data, extract the length of station name, and temperature as Integer.
var
  I: Int64;
  vDigit: UInt8;
begin
  // we're looking for the semicolon ';', but backwards since there's fewer characters to traverse
  // a thermo measurement must be at least 3 characters long (one decimal, one period, and the units)
  // e.g. input: Rock Hill;-54.3
  // can safely skip 3:      ^^^
  I := aEnd - 3;

  while True do begin
    if FData[I] = ';' then
      break;
    Dec(I);
  end;
  // I is the position of the semi-colon, extract what's before and after it

  // length of the station name string
  aLength := i - aStart;

  // ASCII of 3 is 51.
  // subtract ASCII of 0 to get the digit 3
  // repeat with the remaining digits, multiplying by 10^x (skip the '.')
  // multiply by -1 upon reaching a '-'
  aTemp :=     (Ord(FData[aEnd])   - c0ascii)
         + 10 *(Ord(FData[aEnd-2]) - c0ascii);
  vDigit := Ord(FData[aEnd-3]);
  if (vDigit >= c0ascii) and (vDigit <= c9ascii) then begin
    aTemp := aTemp + 100*(Ord(FData[aEnd-3]) - c0ascii);
    vDigit := Ord(FData[aEnd-4]);
    if vDigit = cNegAscii then
      aTemp := -aTemp;
  end
  else if vDigit = cNegAscii then
    aTemp := -aTemp;
end;

//---------------------------------------------------

constructor TOneBRC.Create (const aThreadCount: UInt16);
var I: UInt16;
begin
  FThreadCount := aThreadCount;
  SetLength (FStationsDicts, aThreadCount);
  SetLength (FThreads, aThreadCount);

  for I := 0 to aThreadCount - 1 do begin
    FStationsDicts[I] := TMyDictionary.Create;
  end;

  FStationsNames := TMyDictionary.Create;
  FCS := TCriticalSection.Create;
end;

destructor TOneBRC.Destroy;
var I: UInt16;
begin
  FCS.Free;
  FStationsNames.Free;
  for I := 0 to FThreadCount - 1 do begin
    FStationsDicts[I].Free;
  end;
  inherited Destroy;
end;

//---------------------------------------------------

function TOneBRC.mORMotMMF (const afilename: string): Boolean;
begin
  Result := FMemoryMap.Map (aFilename);
  if Result then begin
    FData     := FMemoryMap.Buffer;
    FDataSize := FMemoryMap.Size;
  end;
end;

procedure TOneBRC.DispatchThreads;
var
  I: UInt16;
  vRange: Int64;
begin
  vRange := Trunc (FDataSize / FThreadCount);

  for I := 0 to FThreadCount - 1 do begin
    FThreads[I] := TBRCThread.Create (@ProcessData, I, I*vRange, (I+1)*vRange);
  end;
end;

procedure TOneBRC.WaitAll;
var
  I: UInt16;
begin
  for I := 0 to FThreadCount - 1 do begin
    FThreads[I].WaitFor;
  end;
end;

//---------------------------------------------------

procedure TOneBRC.ProcessData (aThreadNb: UInt16; aStartIdx: Int64; aEndIdx: Int64);
var
  i: Int64;
  vStation: AnsiString;
  vTemp: SmallInt;
  vData: PStationData;
  vLineStart: Int64;
  vHash: Cardinal;
  vLenStationName: ShortInt;
begin
  i := aStartIdx;

  // the given starting point might be in the middle of a line:
  // find the beginning of that line
  while i-1 >= 0 do begin
    if FData[i-1] <> #10 then
      Dec (I)
    else
      break;
  end;

  // the given ending point might be in the middle of a line:
  // find the beginning of that line (the last block works well)
  while True do begin
    if FData[aEndIdx] <> #10 then
      Dec (aEndIdx)
    else
      break;
  end;
  Inc (aEndIdx, 1);

  vLineStart := i;

  while i < aEndIdx do begin
    if FData[i] = #10 then begin
      // new line parsed, process its contents
      ExtractLineData (vLineStart, i - 1, vLenStationName, vTemp);

      // compute the hash starting at the station's first char, and its length
      // mORMot's crc32c is ~33% faster than the built-in one
      vHash := crc32c(0, @FData[vLineStart], vLenStationName);

      if FstationsDicts[aThreadNb].TryGetValue(vHash, vData) then begin
        if vTemp < vData^.Min then
          vData^.Min := vTemp;
        if vTemp > vData^.Max then
          vData^.Max := vTemp;
        vData^.Sum := vData^.Sum + vTemp;
        Inc (vData^.Count);
      end
      else begin
        // pre-allocated array of records instead of on-the-go allocation
        vData^.Min := vTemp;
        vData^.Max := vTemp;
        vData^.Sum := vTemp;
        vData^.Count := 1;
        FStationsDicts[aThreadNb].Add (vHash, vData);

        // SetString done only once per station name, for later sorting
        if not FStationsNames.TryGetValue (vHash, vData) then begin
          FCS.Acquire;
          if not FStationsNames.TryGetValue (vHash, vData) then begin
            SetString(vStation, pAnsiChar(@FData[vLineStart]), vLenStationName);
            FStationsNames.AddName (vHash, vStation);
          end;
          FCS.Release;
        end;
      end;

      // we're at a #10: next line starts at the next index
      vLineStart := i+1;

      // we're at a #10:
      // until the next #10 char, there will be:
      // - 1 semicolon
      // - 3 chars for the temp (min)
      // - 2 chars for the name (min)
      // - the usual Inc (I)
      // so we should be able to skip 7 chars until another #10 may appear
      Inc (i, 7);
      continue;
    end;

    Inc (i);
  end;
end;

procedure TOneBRC.Merge(aLeft: UInt16; aRight: UInt16);
var iHash: Cardinal;
    vDataR: PStationData;
    vDataL: PStationData;
    I: Integer;
begin
  for I := 0 to cDictSize - 1 do begin
    iHash := FStationsDicts[aRight].Keys[I];
    // zero means empty slot: skip
    if iHash = 0 then
      continue;

    FStationsDicts[aRight].TryGetValue(iHash, vDataR);

    if FStationsDicts[aLeft].TryGetValue(iHash, vDataL) then begin
      vDataL^.Count := vDataL^.Count + vDataR^.Count;
      vDataL^.Sum   := vDataL^.Sum + vDataR^.Sum;

      if vDataR^.Max > vDataL^.Max then
        vDataL^.Max := vDataR^.Max;
      if vDataR^.Min < vDataL^.Min then
        vDataL^.Min := vDataR^.Min;
    end
    else begin
      FStationsDicts[aLeft].Add (iHash, vDataR);
    end;
  end;
end;

procedure TOneBRC.MergeAll;
var
  I: UInt16;
begin
  for I := 1 to FThreadCount - 1 do begin
    Merge (0, I);
  end;
end;

//---------------------------------------------------

procedure TOneBRC.GenerateOutput;
var vMin, vMean, vMax: Double;
    vStream: TStringStream;
    I, N: Int64;
    vData: PStationData;
    vHash: Cardinal;
    vStations: TStringList;
begin
  vStream := TStringStream.Create;
  vStations := TStringList.Create;
  vStations.Capacity := cDictSize;
  vStations.UseLocale := False;
  try
    vStations.BeginUpdate;
    for I := 0 to cDictSize - 1 do begin
      if FStationsNames.Keys[I] <> 0 then begin
        // count = 0 means empty slot: skip
        vStations.Add(FStationsNames.StationNames[I]);
      end;
    end;
    vStations.EndUpdate;

    vStations.CustomSort (@Compare);

    I := 0;
    N := vStations.Count;

    vStream.WriteString('{');
    while I < N do begin
      // the stations are now sorted, but we need to locate the data: recompute hash
      // would it be more efficient to store the hash as well?
      // debatable, and the whole output generation is < 0.3 seconds, so not exactly worth it
      vHash := crc32c(0, @vStations[i][1], Length (vStations[i]));
      FStationsDicts[0].TryGetValue(vHash, vData);
      vMin := vData^.Min/10;
      vMax := vData^.Max/10;
      vMean := RoundExDouble(vData^.Sum/vData^.Count/10);

      vStream.WriteString(
        vStations[i] + '=' + FormatFloat('0.0', vMin)
                     + '/' + FormatFloat('0.0', vMean)
                     + '/' + FormatFloat('0.0', vMax) + ', '
      );
      Inc(I);
    end;

    vStream.SetSize(vStream.Size - 2);
    vStream.WriteString('}' + #10);
{$IFDEF DEBUG}
    vStream.SaveToFile('ghatem-out.txt');
{$ENDIF}
{$IFDEF RELEASE}
    Write(vStream.DataString);
{$ENDIF}
  finally
    vStream.Free;
    vStations.Free;
  end;
end;

{ TBRCThread }

procedure TBRCThread.Execute;
begin
  FProc (FThreadNb, FStart, FEnd);
  Terminate;
end;

constructor TBRCThread.Create(aProc: TThreadProc; aThreadNb: UInt16; aStart: Int64; aEnd: Int64);
begin
  inherited Create(False);
  FProc := aProc;
  FThreadNb := aThreadNb;
  FStart := aStart;
  FEnd := aEnd;
end;

//---------------------------------------------------



{$REGION TOneBRCApp scaffolding}

procedure TOneBRCApp.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:= CheckOptions(Format('%s%s%s%s:',[
      cShortOptHelp,
      cShortOptThread,
      cShortOptVersion,
      cShortOptInput
    ]),
    [
      cLongOptHelp,
      cLongOptThread+':',
      cLongOptVersion,
      cLongOptInput+':'
    ]
  );
  if ErrorMsg<>'' then begin
    WriteLn(Format(rsErrorMessage, [ ErrorMsg ]));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption(cShortOptHelp, cLongOptHelp) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption(cShortOptVersion, cLongOptVersion) then begin
    WriteLn(Format(rsGeneratorVersion, [ 1.0 ]));
    Terminate;
    Exit;
  end;

  FThreadCount := GetSystemThreadCount;
  if HasOption(cShortOptThread, cLongOptThread) then begin
    FThreadCount := StrToInt (GetOptionValue(cShortOptThread, cLongOptThread));
  end;

  if HasOption(cShortOptInput, cLongOptInput) then begin
    FFileName := GetOptionValue(
      cShortOptInput,
      cLongOptInput
    );
  end
  else begin
    WriteLn(Format(rsErrorMessage, [ rsMissingInputFlag ]));
    Terminate;
    Exit;
  end;

  FFileName := ExpandFileName(FFileName);

  RunOneBRC;

  // stop program loop
  Terminate;
end;

constructor TOneBRCApp.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  StopOnException:=True;
end;

destructor TOneBRCApp.Destroy;
begin
  inherited Destroy;
end;

procedure TOneBRCApp.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TOneBRCApp;
begin
  Application:=TOneBRCApp.Create(nil);
  Application.Title:='1 BRC';
  Application.Run;
  Application.Free;
end.

{$ENDREGION}

