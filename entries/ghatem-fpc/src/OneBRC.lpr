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
  cNumStations = 41343;  // as per the input file
  cDictSize    = 248071; // numstations * 6, next prime number
  cThreadCount = 32;

  c0ascii: ShortInt = 48;

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
    Count: UInt16; // will fail on 400 stations / 5B row tests, due to overflow
    Sum: Integer;
  end;
  PStationData = ^TStationData;

  TThreadCount  = UInt8;  // max number of threads = 256
  TStationCount = UInt16; // max number of stations = 65536 (enough for 42k)
  THashSize     = UInt32; // size of dictionary can exceed 2^16

  THashes = array [0..cDictSize-1] of Cardinal;
  TIndexes = array [0..cDictSize-1] of TStationCount;
  TStationNames = array [0..cNumStations-1] of AnsiString;


  { TBRCDictionary }

  TBRCDictionary = class
  private
    // keys/values for a Large dictionary:
    // values will be the index where the actual record is stored
    FHashes: THashes;
    FIndexes: TIndexes;

    // where the actual records are stored:
    // - each thread holds its own data
    // - for each thread, pre-allocate as much space as needed
    // - all threads store their data at the exact same index
    FThreadData: array [0..cThreadCount-1] of array [0..cNumStations-1] of TStationData;

    // station names are also shared, not lock-protected (in the worst case, the value is written twice)
    // stored separately as it is rarely needed
    FStationNames: TStationNames;

    // points to the next slot in FThreadData where we should fill a newly encountered station
    FCounter: TStationCount;

    // exclusively to protect FCounter from concurrent-writes
    FCS: TCriticalSection;

    // searches for a given key, returns if found the key and the storage index
    // (or, if not found, which index to use next)
    procedure InternalFind(const aKey: Cardinal; out aFound: Boolean; out aIndex: THashSize); {$IFNDEF VALGRIND} inline; {$ENDIF}

  public
    constructor Create;
    destructor Destroy; override;

    // simple wrapper to find station-record pointers
    function TryGetValue (const aKey: Cardinal; const aThreadNb: TThreadCount; out aValue: PStationData): Boolean; {$IFNDEF VALGRIND} inline; {$ENDIF}

    // multithread-unprotected: adds a firstly-encountered station-data (temp, name)
    procedure Add (const aHashIdx: THashSize; const aThreadNb: TThreadCount; const aTemp: SmallInt; const aStationName: AnsiString); {$IFNDEF VALGRIND} inline; {$ENDIF}

    // multithread-protected: safely assign a slot for a given key
    function AtomicRegisterHash (const aKey: Cardinal): THashSize;
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

    FThreadCount: TThreadCount;
    FThreads: array of TThread;
    FDictionary: TBRCDictionary;

    // for a line between idx [aStart; aEnd], returns the station-name length, and the integer-value of temperature
    procedure ExtractLineData(const aStart: Int64; const aEnd: Int64; out aLength: ShortInt; out aTemp: SmallInt); {$IFNDEF VALGRIND} inline; {$ENDIF}

  public
    constructor Create (const aThreadCount: TThreadCount);
    destructor Destroy; override;
    function mORMotMMF (const afilename: string): Boolean;

    // initial thread-spawn
    procedure DispatchThreads;

    // await for all threads to complete work
    procedure WaitAll;

    // executed by each thread to process data in the given range
    procedure ProcessData (aThreadNb: TThreadCount; aStartIdx: Int64; aEndIdx: Int64);

    // merge data from all threads
    procedure Merge (aLeft: TThreadCount; aRight: TThreadCount);
    procedure MergeAll;

    procedure GenerateOutput;
    property DataSize: Int64 read FDataSize;
  end;

  //---------------------------------------
  { TBRCThread }

  TThreadProc = procedure (aThreadNb: TThreadCount; aStartIdx: Int64; aEndIdx: Int64) of object;

  TBRCThread = class (TThread)
  private
    FProc: TThreadProc;
    FThreadNb: TThreadCount;
    FStart: Int64;
    FEnd: Int64;
  protected
    procedure Execute; override;
  public
    constructor Create (aProc: TThreadProc; aThreadNb: TThreadCount; aStart: Int64; aEnd: Int64);
  end;

{ TBRCDictionary }

constructor TBRCDictionary.Create;
begin
  FCS := TCriticalSection.Create;
  FCounter := 0;
end;

destructor TBRCDictionary.Destroy;
begin
  FCS.Free;
  inherited Destroy;
end;

function TBRCDictionary.TryGetValue(const aKey: Cardinal; const aThreadNb: TThreadCount; out aValue: PStationData): Boolean;
var
  vIdx: THashSize;
begin
  InternalFind (aKey, Result, vIdx);
  aValue := @FThreadData[aThreadNb][FIndexes[vIdx]];
end;

procedure TBRCDictionary.Add(const aHashIdx: THashSize; const aThreadNb: TThreadCount; const aTemp: SmallInt; const aStationName: AnsiString);
var
  vData: PStationData;
begin
  vData := @FThreadData[aThreadNb][FIndexes[aHashIdx]];
  vData^.Count := 1;
  vData^.Max   := aTemp;
  vData^.Min   := aTemp;
  vData^.Sum   := aTemp;

  FStationNames[FIndexes[aHashIdx]] := aStationName;
end;

function TBRCDictionary.AtomicRegisterHash(const aKey: Cardinal): THashSize;
var
  vFound: Boolean;
begin
  // must call InternalFind again, within the critical-section,
  // to ensure the slot was not taken by another thread
  // this function should execute only once per station, so at most 41343 times
  FCS.Acquire;
  try
    InternalFind (aKey, vFound, Result);
    if not vFound then begin
      FHashes[Result]  := aKey;
      FIndexes[Result] := FCounter;
      Inc (FCounter);
    end;
  finally
    FCS.Release;
  end;
end;

procedure TBRCDictionary.InternalFind(const aKey: Cardinal; out aFound: Boolean; out aIndex: THashSize);
var vIdx: Integer;
    vOffset: Integer;
begin
  // Lemire hashing: faster to ocmpute than modulus, but more collisions from trials
  // https://lemire.me/blog/2016/06/27/a-fast-alternative-to-the-modulo-reduction/
  // thanks Arnaud for the suggestion
  vIdx := aKey * cDictSize shr 32;

  if FHashes[vIdx] = 0 then begin
    // found match
    aIndex := vIdx;
    aFound := False;
    exit;
  end;
  if FHashes[vIdx] = aKey then begin
    // found empty bucket to use
    aIndex := vIdx;
    aFound := True;
    exit;
  end
  else begin
    vOffset := 1;

    while True do begin
      // linear (not quadratic) probing, with continous increments to minimize clusters
      Inc (vIdx, vOffset);
      Inc (vOffset);

      // exceeded boundary, loop back
      if vIdx >= cDictSize then
        Dec (vIdx, cDictSize);

      if FHashes[vIdx] = aKey then begin
        // found match
        aIndex := vIdx;
        aFound := True;
        exit;
      end;
      if FHashes[vIdx] = 0 then begin
        // found empty bucket to use
        aIndex := vIdx;
        aFound := False;
        exit;
      end;
    end;
  end;
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

function RoundExInteger (const aTemp: Double): Integer; inline;
var
  vTmp: Double;
begin
  vTmp := aTemp * 10;
  Result := Ceiling (vTmp);
end;

//---------------------------------------------------
{ TOneBRC }

function Compare(AList: TStringList; AIndex1, AIndex2: Integer): Integer;
begin
  Result := CompareStr(AList.Strings[AIndex1], AList.Strings[AIndex2]);
end;

procedure TOneBRC.ExtractLineData(const aStart: Int64; const aEnd: Int64; out aLength: ShortInt; out aTemp: SmallInt);
// given a line of data, extract the length of station name, and temperature as Integer.
var
  J: Int64;
  vIsNeg: Int8;
begin
  // we're looking for the semicolon ';', but backwards since there's fewer characters to traverse
  // a thermo measurement must be at least 3 characters long (one decimal, one period, and the units)
  // e.g. input: Rock Hill;-54.3
  // can safely skip 3:      ^^^
  J := aEnd - 3;

  if FData[J] <> ';' then begin
    Dec (J);
    Dec (J, Ord (FData[J] <> ';'));
  end;
  // I is the position of the semi-colon, extract what's before and after it

  // length of the station name string
  aLength := J - aStart;

  // ASCII of 3 is 51.
  // subtract ASCII of 0 to get the digit 3
  // repeat with the remaining digits, multiplying by 10^x (skip the '.')
  // multiply by -1 upon reaching a '-'

  {
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
  }

  //==========
  // entire computation is branchless (for readability, see version above)
  // no intermediary results also showed better performance

  // 0 if -
  // 1 if +
  // convert range [0;1] to [-1;1] for branchless negation when needed
  // if there is a 3rd digit (*100), add it, otherwise multiply by 0 to cancel it out
  vIsNeg := Ord (FData[J+1] <> '-');

  aTemp := (
                 (Ord(FData[aEnd])  - c0ascii)
           + 10 *(Ord(FData[aEnd-2]) - c0ascii)
           + Ord ((J+4 - vIsNeg < aEnd)) * 100*(Ord(FData[aEnd-3]) - c0ascii)
         ) * (vIsNeg * 2 - 1);
end;

//---------------------------------------------------

constructor TOneBRC.Create (const aThreadCount: TThreadCount);
begin
  FThreadCount := aThreadCount;
  SetLength (FThreads, aThreadCount);

  FDictionary := TBRCDictionary.Create;
end;

destructor TOneBRC.Destroy;
begin
  FDictionary.Free;
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
  I: TThreadCount;
  vRange: Int64;
begin
  // distribute input equally across available threads
  vRange := Trunc (FDataSize / FThreadCount);

  for I := 0 to FThreadCount - 1 do begin
    FThreads[I] := TBRCThread.Create (@ProcessData, I, I*vRange, (I+1)*vRange);
  end;
end;

procedure TOneBRC.WaitAll;
var
  I: TThreadCount;
begin
  for I := 0 to FThreadCount - 1 do begin
    FThreads[I].WaitFor;
  end;
end;

//---------------------------------------------------

procedure TOneBRC.ProcessData (aThreadNb: TThreadCount; aStartIdx: Int64; aEndIdx: Int64);
var
  i: Int64;
  vStation: AnsiString;
  vTemp: SmallInt;
  vData: PStationData;
  vHashIdx: THashSize;
  vLineStart: Int64;
  vHash: Cardinal;
  vLenStationName: ShortInt;
  vFound: Boolean;
begin
  // initialize min/max, else we may get zeroes (due to our Add that fires once per station across all threads)
  for I := 0 to cNumStations - 1 do begin
    FDictionary.FThreadData[aThreadNb][I].Max := -2000;
    FDictionary.FThreadData[aThreadNb][I].Min := 2000;
  end;

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

    // can still skip some chars
    if FData[i] > ';' then begin
      Inc (I, 5);
    end;

    // unroll a few seems to be improving?
    if FData[i] <> #10 then begin
      Inc (i);
      if FData[i] <> #10 then begin
        Inc (i);
        if FData[i] <> #10 then begin
          Inc (I);
          if FData[i] <> #10 then begin
            Inc (i);
            while FData[i] <> #10 do begin
              Inc (I);
            end;
          end;
        end;
      end;
    end;

    // new line parsed, process its contents
    ExtractLineData (vLineStart, i - 1, vLenStationName, vTemp);

    // compute the hash starting at the station's first char, and its length
    // mORMot's crc32c is ~33% faster than the built-in one
    vHash := crc32c(0, @FData[vLineStart], vLenStationName);

    FDictionary.InternalFind (vHash, vFound, vHashIdx);

    if vFound then begin
      vData := @FDictionary.FThreadData[aThreadNb][FDictionary.FIndexes[vHashIdx]];
      if vTemp < vData^.Min then
        vData^.Min := vTemp;
      if vTemp > vData^.Max then
        vData^.Max := vTemp;
      Inc (vData^.Sum, vTemp);
      Inc (vData^.Count);
    end
    else begin
      // pre-allocated array of records instead of on-the-go allocation
      vHashIdx := FDictionary.AtomicRegisterHash (vHash);

      // SetString done only once per station name, for later sorting
      SetString(vStation, pAnsiChar(@FData[vLineStart]), vLenStationName);

      // data can be safely added at the given index, without locking
      FDictionary.Add(vHashIdx, aThreadNb, vTemp, vStation);
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
  end;
end;

procedure TOneBRC.Merge(aLeft: TThreadCount; aRight: TThreadCount);
var vDataR: PStationData;
    vDataL: PStationData;
    I: Integer;
begin
  // accumulate data into Left
  for I := 0 to cNumStations - 1 do begin
    vDataR := @FDictionary.FThreadData[aRight][I];
    vDataL := @FDictionary.FThreadData[aLeft][I];

    vDataL^.Count := vDataL^.Count + vDataR^.Count;
    vDataL^.Sum   := vDataL^.Sum + vDataR^.Sum;

    if (vDataR^.Max > vDataL^.Max) then
      vDataL^.Max := vDataR^.Max;
    if (vDataR^.Min < vDataL^.Min) then
      vDataL^.Min := vDataR^.Min;
  end;
end;

procedure TOneBRC.MergeAll;
var
  I: TThreadCount;
begin
  // all thread-data is accumulated into index 0
  for I := 1 to FThreadCount - 1 do begin
    Merge (0, I);
  end;
end;

//---------------------------------------------------

function MyFormatInt (const aIn: SmallInt): AnsiString; inline;
begin
  // much faster than FormatFloat
  // oddly, IntToStr does not include leading zeroes for both pos and neg numbers
  Result := IntToStr(aIn);
  Insert ('.', Result, Length(Result));

  if Result[1] = '.' then begin
    Insert ('0', Result, 1);
    exit;
  end;

  if (Result[1] = '-') and (Result[2] = '.') then
    Insert('0', Result, 2);
end;

//---------------------------------------------------

procedure TOneBRC.GenerateOutput;
var vStream: TStringStream;
    I, N: Int32;
    vData: PStationData;
    vHash: Cardinal;
    vStations: TStringList;
    vIdx: THashSize;
    vRes: Boolean;
begin
  vStream := TStringStream.Create;
  vStations := TStringList.Create;
  vStations.Capacity := cNumStations;
  vStations.UseLocale := False;
  try
    vStations.BeginUpdate;
    for i := 0 to cNumStations - 1 do begin
      if FDictionary.FStationNames[i] <> '' then
        vStations.Add (FDictionary.FStationNames[i]);
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

      FDictionary.InternalFind (vHash, vRes, vIdx);
      vData := @FDictionary.FThreadData[0][FDictionary.FIndexes[vIdx]];

      vStream.WriteString(
        vStations[i] + '=' + MyFormatInt(vData^.Min)
                     + '/' + MyFormatInt(RoundExInteger(vData^.Sum/vData^.Count/10))
                     + '/' + MyFormatInt(vData^.Max) + ', '
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
end;

constructor TBRCThread.Create(aProc: TThreadProc; aThreadNb: TThreadCount; aStart: Int64; aEnd: Int64);
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
