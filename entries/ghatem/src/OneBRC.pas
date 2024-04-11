unit OneBRC;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  mormot.core.os,
  Utils;


const
  cNumStations: Integer = 45000;

type
  // record is packed to minimize its size
  // use pointers to avoid passing entire records around
  TStationData = packed record
    Min: SmallInt;
    Max: SmallInt;
    Count: UInt32;
    Sum: Integer;
    Name: AnsiString;
  end;
  PStationData = ^TStationData;

  TOneBRC = class
  private
    // mormot memory map for fast bytes read.
    // I tried using CreateFileMapping and failed, more details in unit FileReader
    FMemoryMap: TMemoryMap;
    FData: pAnsiChar;
    FDataSize: Int64;

    FThreadCount: UInt16;
    FThreads: array of TThread;
    FStationsDicts: array of TDictionary<Cardinal, PStationData>;

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

implementation

uses System.AnsiStrings,
     System.Generics.Defaults,
     System.ZLib;

//---------------------------------------------------
{ TOneBRC }

constructor TOneBRC.Create;
var I: UInt16;
begin
  FThreadCount := aThreadCount;
  SetLength (FStationsDicts, aThreadCount);
  SetLength (FThreads, aThreadCount);

  for I := 0 to aThreadCount - 1 do begin
    FStationsDicts[I] := TDictionary<Cardinal, PStationData>.Create;
    FStationsDicts[I].Capacity := 45000;
  end;
end;

//---------------------------------------------------

destructor TOneBRC.Destroy;
begin
  // TODO: free data structures
  inherited;
end;

procedure TOneBRC.DispatchThreads;
var
  I: UInt16;
  vRange: Int64;
begin
  vRange := Trunc (FDataSize / FThreadCount);

  for I := 0 to FThreadCount - 1 do begin
    FThreads[I] := TBRCThread.Create (ProcessData, I, I*vRange, (I+1)*vRange);
  end;
end;

//---------------------------------------------------

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
  aTemp :=     (Ord(FData[aEnd])   - 48)
         + 10 *(Ord(FData[aEnd-2]) - 48);
  vDigit := Ord(FData[aEnd-3]);
  if (vDigit >= 48) and (vDigit <= 57) then begin
    aTemp := aTemp + 100*(Ord(FData[aEnd-3]) - 48);
    vDigit := Ord(FData[aEnd-4]);
    if vDigit = 45 then
      aTemp := -1 * aTemp;
  end
  else if vDigit = 45 then
    aTemp := -1 * aTemp;
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
    if FData[aEndIdx] <> #13 then
      Dec (aEndIdx)
    else
      break;
  end;
  Inc (aEndIdx, 1);

  vLineStart := i;

  while i < aEndIdx do begin
    if FData[i] = #13 then begin
      // new line parsed, process its contents
      ExtractLineData (vLineStart, i - 1, vLenStationName, vTemp);

      // compute the hash starting at the station's first char, and its length
      vHash := crc32(0, @FData[vLineStart], vLenStationName);

      if FstationsDicts[aThreadNb].TryGetValue(vHash, vData) then begin
        if vTemp < vData^.Min then
          vData^.Min := vTemp;
        if vTemp > vData^.Max then
          vData^.Max := vTemp;
        vData^.Sum := vData^.Sum + vTemp;
        Inc (vData^.Count);
      end
      else begin
        // SetString done only once per station name (per thread), for later sorting
        // for 1-thread, I had a separate list to store station names
        // for N-threads, merging those lists became costly.
        // store the name directly in the record, we'll generate a stringlist at the end
        SetString(vStation, pAnsiChar(@FData[vLineStart]), vLenStationName);

        // pre-allocated array of records instead of on-the-go allocation
        new(vData);
        vData^.Min := vTemp;
        vData^.Max := vTemp;
        vData^.Sum := vTemp;
        vData^.Count := 1;
        vData^.Name := vStation;
        FStationsDicts[aThreadNb].Add (vHash, vData);
      end;

      // next char is #10, so we can skip 2 instead of 1
      vLineStart := i+2;
    end;

    Inc (i);
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

procedure TOneBRC.GenerateOutput;
var vMin, vMean, vMax: Double;
    vStream: TStringStream;
    I, N: Int64;
    vData: PStationData;
    iStation: AnsiString;
    vComparer: IComparer<AnsiString>;
    vHash: Cardinal;
    vStations: TList<AnsiString>;
begin
  vStream := TStringStream.Create;
  vStations := TList<AnsiString>.Create;
  try
    vComparer := TDelegatedComparer<AnsiString>.Create(
      function (const aLeft, aRight: AnsiString): Integer
      begin
        // use System.AnsiStrings.CompareStr to match the hash,
        // otherwise some Unicode chars will be out-of-order
        Result := CompareStr (aLeft, aRight);
      end);

    for vData in FStationsDicts[0].Values do begin
      vStations.Add(vData^.Name);
    end;

    vStations.Sort (vComparer);

    I := 0;
    N := vStations.Count;

    vStream.WriteString('{');
    while I < N do begin
      // the stations are now sorted, but we need to locate the data: recompute hash
      // would it be more efficient to store the hash as well?
      // debatable, and the whole output generation is < 0.3 seconds, so not exactly worth it
      vHash := crc32(0, @vStations[i][1], Length (vStations[i]));
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
{$ELSEIF defined(RELEASE)}
    Write (vStream.DataString);
{$ENDIF}
  finally
    vStations.Free;
    vStream.Free;
  end;
end;

//---------------------------------------------------

procedure TOneBRC.Merge(aLeft: UInt16; aRight: UInt16);
var iHash: Cardinal;
    vDataR: PStationData;
    vDataL: PStationData;
begin
  for iHash in FStationsDicts[aRight].Keys do begin
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

{ TBRCThread }

constructor TBRCThread.Create(aProc: TThreadProc; aThreadNb: UInt16; aStart, aEnd: Int64);
begin
  inherited Create(False);
  FProc := aProc;
  FThreadNb := aThreadNb;
  FStart := aStart;
  FEnd := aEnd;
end;

procedure TBRCThread.Execute;
begin
  inherited;
  FProc (FThreadNb, FStart, FEnd);
  Terminate;
end;

end.
