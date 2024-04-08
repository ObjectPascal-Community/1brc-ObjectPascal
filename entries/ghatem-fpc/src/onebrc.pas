unit OneBRC;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  //Baseline.Common,
  mormot.core.os;

function RoundExDouble(const ATemp: Double): Double; inline;

type
  PSmallInt = ^SmallInt;

  // record is packed to minimize its size
  // use pointers to avoid passing entire records around
  TStationData = packed record
    Min: SmallInt;
    Max: SmallInt;
    Count: UInt32;
    Sum: Integer;
  end;
  PStationData = ^TStationData;

  TOneBRC = class
  private
    // mormot memory map for fast bytes read.
    // I tried using CreateFileMapping and failed, more details in unit FileReader
    FMemoryMap: TMemoryMap;
    FData: pAnsiChar;
    FDataSize: Int64;

    FTempArr: array [0..2000] of SmallInt;
    FTempIter: Integer;

    FStationsDict: TFPHashList;
    FStations: TStringList;

    // pre-allocate space for N records at once (why can't I use a const in here??)
    FRecords: array[0..45000] of TStationData;

    procedure ExtractLineData(const aStart: Int64; const aEnd: Int64; var aStation, aTempStr: AnsiString); inline;
    procedure DoTempsDict (var aDict: TFPHashList; var aStrVal: AnsiString; var aIntVal: SmallInt); inline;
  public
    constructor Create;
    function mORMotMMF (const afilename: string): Boolean;
    procedure SingleThread;
    procedure GenerateOutput;
  end;

implementation

function Ceiling(const ANumber: Double): integer; inline;
begin
  Result := Trunc(ANumber) + Ord(Frac(ANumber) > 0);
end;

function RoundExDouble(const ATemp: Double): Double; inline;
var
  tmp: Double;
begin
  tmp:= ATemp * 10;
  Result := Ceiling(tmp) / 10;
end;

//---------------------------------------------------
{ TProcesser }

function Compare(AList: TStringList; AIndex1, AIndex2: Integer): Integer;
var
  Str1, Str2: String;
begin
  Str1 := AList.Strings[AIndex1];
  Str2 := AList.Strings[AIndex2];
  Result := CompareStr(Str1, Str2);
end;

procedure TOneBRC.ExtractLineData(const aStart: Int64; const aEnd: Int64; var aStation, aTempStr: AnsiString);
// given a line of data, extract the station name and temperature, as strings.
var
  I: Int64;
begin
  // we're looking for the semicolon ';', but backwards since there's fewer characters to traverse
  // a thermo measurement must be at least 3 characters long (one decimal, one period, and the units)
  // e.g. input: Rock Hill;-54.3
  // can safely skip 3:      ^^^
  I := aEnd-3;

  while True do begin
    if FData[I] = ';' then
      break;
    Dec(I);
  end;

  // I is the position of the semi-colon, extract what's before and after it
  SetString(aStation, pAnsiChar(@FData[aStart]), i-aStart);
  SetString(aTempStr, pAnsiChar(@FData[i+1])   , aEnd-i);
end;

//---------------------------------------------------

procedure TOneBRC.DoTempsDict (var aDict: TFPHashList; var aStrVal: AnsiString;
                       var aIntVal: SmallInt);
// the parsed temperatures are as strings, we need them as smallInts.
// however, a few problems:
// - StrToInt is VERY expensive
// - Val is less expensive than StrToInt, but still very expensive
// solution:
// temperatures vary (between at most -100.0 and 100.0, that's 200*10=2000 possible different readings,
// regardless if there are 100M or 1B lines.
// practically in this input file, there are 1998 readings exactly:
// convert each one once, and store the resulting smallInt in a dictionary
{TODO: thread-safety}
var vSuccess: Integer;
    vIdx: Integer;
    pInt: PSmallInt;
begin
  // replicate the last char, then drop it
  aStrVal[Length(aStrVal) - 1] := aStrVal[Length(aStrVal)];
  SetLength (aStrVal, Length(aStrVal) - 1);

  //vIdx := aDict.FindIndexOf(aStrVal);
  //if vIdx >= 0 then begin
    //pInt := PSmallInt (aDict.Items[vIdx]);
    //aIntVal := pInt^;
  //end
  //else begin
    Val (aStrVal, aIntVal, vSuccess);
    if vSuccess <> 0 then
      raise Exception.Create('cannot decode value');
    //FTempArr[FTempIter] := aIntVal;
    //aDict.Add (aStrVal, @(FTempArr[FTempIter]));
    //Inc (FTempIter);
  //end;
end;

//---------------------------------------------------

constructor TOneBRC.Create;
begin
  FStationsDict := TFPHashList.Create;
  FStationsDict.Capacity := 45000;

  FStations := TStringList.Create;
  FStations.Capacity := 45000;
  FStations.UseLocale := False;

  FTempIter := 0;
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
  vTempDict: TFPHashList;
  i: Int64;
  vStation: AnsiString;
  vTempStr: AnsiString;
  vTemp: SmallInt;
  vData: PStationData;
  vLineStart: Int64;
  vRecIdx: Integer;
  vIdx: Integer;
begin
  // expecting 1998 different temperature measurements
  vTempDict := TFPHashList.Create;
  vTempDict.Capacity := 2000;

  vLineStart := 0;
  i := 0;
  vRecIdx := 0;

  while i < FDataSize - 1 do begin
    if FData[i] = #13 then begin
      // new line parsed, process its contents
      ExtractLineData (vLineStart, i -1, vStation, vTempStr);
      DoTempsDict (vTempDict, vTempStr, vTemp);

      // next char is #10, so we can skip 2 instead of 1
      vLineStart := i+2;

      // pre-allocated array of records instead of on-the-go allocation
      vIdx := FStationsDict.FindIndexOf(vStation);

      if vIdx >= 0 then begin
        vData := FStationsDict.Items[vIdx];
        if vTemp < vData^.Min then
          vData^.Min := vTemp;
        if vTemp > vData^.Max then
          vData^.Max := vTemp;
        vData^.Sum := vData^.Sum + vTemp;
        Inc (vData^.Count);
      end
      else begin
        vData := @FRecords[vRecIdx];
        vData^.Min := vTemp;
        vData^.Max := vTemp;
        vData^.Sum := vTemp;
        vData^.Count := 1;
        FStationsDict.Add (vStation, vData);
        FStations.Add (vStation);

        Inc (vRecIdx);
      end;
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
    vIdx: Integer;
begin
  vStream := TStringStream.Create;

  try
    FStations.CustomSort (@Compare);

    I := 0;
    N := FStations.Count;

    vStream.WriteString('{');
    while I < N do begin
      vIdx := FStationsDict.FindIndexOf(FStations[i]);
      vData := FStationsDict.Items[vIdx];
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
{$ELSEIF defined(RELEASE)}
    WriteLn (vStream.DataString);
{$ENDIF}
  finally
    vStream.Free;
  end;
end;

//---------------------------------------------------


end.

