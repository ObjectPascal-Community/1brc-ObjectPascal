unit OneBRC;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections,
  mormot.core.os;

function RoundExDouble(const ATemp: Double): Double; inline;

type

  // record is packed to minimize its size
  // use pointers to avoid passing entire records around
  TStationData = packed record
    Min: SmallInt;
    Max: SmallInt;
    Count: UInt32;
    Sum: Integer;
  end;
  PStationData = ^TStationData;
  TStationsDict = specialize TDictionary<Cardinal, PStationData>;

  TOneBRC = class
  private
    // mormot memory map for fast bytes read.
    // I tried using CreateFileMapping and failed, more details in unit FileReader
    FMemoryMap: TMemoryMap;
    FData: pAnsiChar;
    FDataSize: Int64;

    FStationsDict: TStationsDict;
    FStations: TStringList;

    // pre-allocate space for N records at once (why can't I use a const in here??)
    FRecords: array[0..45000] of TStationData;

    procedure ExtractLineData(const aStart: Int64; const aEnd: Int64; out aLength: ShortInt; out aTemp: SmallInt); inline;

  public
    constructor Create;
    function mORMotMMF (const afilename: string): Boolean;
    procedure SingleThread;
    procedure GenerateOutput;
  end;


implementation

uses
  CRC;

const
  c0ascii: ShortInt = 48;
  c9ascii: ShortInt = 57;
  cNegAscii: ShortInt = 45;

function Ceiling (const ANumber: Double): integer; inline;
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

procedure TOneBRC.ExtractLineData(const aStart: Int64; const aEnd: Int64; out aLength: ShortInt; out aTemp: SmallInt);
// given a line of data, extract the length of station name, and temperature as Integer.
var
  I: Int64;
  vDigit: Integer;
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
      aTemp := -1 * aTemp;
  end
  else if vDigit = cNegAscii then
    aTemp := -1 * aTemp;
end;

//---------------------------------------------------

constructor TOneBRC.Create;
begin
  FStationsDict := TStationsDict.Create;
  FStationsDict.Capacity := 45000;

  FStations := TStringList.Create;
  FStations.Capacity := 45000;
  FStations.UseLocale := False;
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

procedure TOneBRC.SingleThread;
var
  i: Int64;
  vStation: AnsiString;
  vTemp: SmallInt;
  vData: PStationData;
  vLineStart: Int64;
  vRecIdx: Integer;
  vHash: Cardinal;
  vLenStationName: ShortInt;
begin
  vLineStart := 0;
  i := 0;
  vRecIdx := 0;

  while i < FDataSize - 1 do begin
    if FData[i] = #13 then begin
      // new line parsed, process its contents
      ExtractLineData (vLineStart, i - 1, vLenStationName, vTemp);

      // compute the hash starting at the station's first char, and its length
      vHash := crc32(0, @FData[vLineStart], vLenStationName);

      if Fstationsdict.TryGetValue(vHash, vData) then begin
        if vTemp < vData^.Min then
          vData^.Min := vTemp;
        if vTemp > vData^.Max then
          vData^.Max := vTemp;
        vData^.Sum := vData^.Sum + vTemp;
        Inc (vData^.Count);
      end
      else begin
        // pre-allocated array of records instead of on-the-go allocation
        vData := @FRecords[vRecIdx];
        vData^.Min := vTemp;
        vData^.Max := vTemp;
        vData^.Sum := vTemp;
        vData^.Count := 1;
        FStationsDict.Add (vHash, vData);

        // SetString done only once per station name, for later sorting
        SetString(vStation, pAnsiChar(@FData[vLineStart]), vLenStationName);
        FStations.Add (vStation);

        // point to the next pre-allocated record
        Inc (vRecIdx);
      end;

      // next char is #10, so we can skip 2 instead of 1
      vLineStart := i+2;
    end;

    Inc (i);
  end;
end;

//---------------------------------------------------

procedure TOneBRC.GenerateOutput;
var vMin, vMean, vMax: Double;
    vStream: TStringStream;
    I, N: Int64;
    vData: PStationData;
    vHash: Cardinal;
begin
  vStream := TStringStream.Create;

  try
    FStations.CustomSort (@Compare);

    I := 0;
    N := FStations.Count;

    vStream.WriteString('{');
    while I < N do begin
      // the stations are now sorted, but we need to locate the data: recompute hash
      // would it be more efficient to store the hash as well?
      // debatable, and the whole output generation is < 0.3 seconds, so not exactly worth it
      vHash := crc32(0, @FStations[i][1], Length (FStations[i]));
      FStationsDict.TryGetValue(vHash, vData);
      vMin := vData^.Min/10;
      vMax := vData^.Max/10;
      vMean := RoundExDouble(vData^.Sum/vData^.Count/10);

      vStream.WriteString(
        FStations[i] + '=' + FormatFloat('0.0', vMin)
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
    WriteLn (vStream.DataString);
{$ENDIF}
  finally
    vStream.Free;
  end;
end;

//---------------------------------------------------


end.

