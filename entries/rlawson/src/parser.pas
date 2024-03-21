unit parser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, bufstream, Contnrs, Math;

procedure ReadMeasurements(inputFile: string);

type
  PTCityTemp = ^TCityTemp;

  TCityTemp = record
    city: string;
    max: int64;
    min: int64;
    total: int64;
    numReadings: integer;
  end;

implementation

const
  READ_SIZE = 65536;
  // read size plus enough to hold one record if we split it
  BUFFER_SIZE = READ_SIZE + 1024;
  REC_SEP: char = ';';
  DECIMAL_POINT: char = '.';
  NEGATIVE_SIGN: char = '-';

procedure ProcessMeasurements(var buffer: array of char; bufferLength: integer;
  var results: TFPHashList);
var
  idx: integer = 0;
  currentTempSign: integer = 1;
  temp, cityStart: integer;
  city: shortstring;
  reading: PTCityTemp;
begin
  while idx < (bufferLength - 1) do
  begin
    // read the city until separator
    cityStart := idx;
    while buffer[idx] <> REC_SEP do Inc(idx);
    SetString(city, @buffer[cityStart], (idx - cityStart));
    // increment through temp reading and calculate integer temp (*100)
    Inc(idx); // move pointer past the ;
    currentTempSign := 1;
    // check for negative sign, if so flag the multiplier and then move past neg sign
    if buffer[idx] = NEGATIVE_SIGN then
    begin
      currentTempSign := -1;
      Inc(idx);
    end;
    // look ahead - is decimal point 2 spaces away then we have two digits
    if buffer[idx + 2] = DECIMAL_POINT then
    begin
      // this is the math that results from subtracting the byte value of ansi zero from each character
      temp := currentTempSign * (100 * byte(buffer[idx]) + 10 *
        byte(buffer[idx + 1]) + byte(buffer[idx + 2]) - 5328);
      // move past digits and CRLF and position pointer to first character of next record
      idx := idx + 6;
    end
    else
    begin
      temp := currentTempSign * (10 * byte(buffer[idx + 1]) +
        byte(buffer[idx + 2]) - 528);
      // move past digits and CRLF and position pointer to first character of next record
      idx := idx + 5;
    end;
    reading := results.Find(city);
    if reading = nil then
    begin
      reading := New(PTCityTemp);
      reading^.city := city;
      reading^.max := temp;
      reading^.min := temp;
      reading^.numReadings := 1;
      reading^.total := temp;
      results.Add(city, reading);
    end
    else
    begin
      reading^.total := reading^.total + temp;
      reading^.max := Max(reading^.max, temp);
      reading^.min := Min(reading^.min, temp);
      reading^.numReadings := reading^.numReadings + 1;
    end;
  end;
end;

function PascalRound(x: double): double;
var
  t: double;
begin
  //round towards positive infinity
  t := Trunc(x);
  if (x < 0.0) and (t - x = 0.5) then
  begin
    // Do nothing
  end
  else if Abs(x - t) >= 0.5 then
  begin
    t := t + Math.Sign(x);
  end;

  if t = 0.0 then
    Result := 0.0
  else
    Result := t;
end;


function RoundEx(x: double): double;
begin
  Result := PascalRound(x * 10.0) / 10.0;
end;

function Compare(AList: TStringList; AIndex1, AIndex2: integer): integer;
var
  Pos1, Pos2: integer;
  Str1, Str2: string;
begin
  Result := 0;
  Str1 := AList.Strings[AIndex1];
  Str2 := AList.Strings[AIndex2];
  Pos1 := Pos('=', Str1);
  Pos2 := Pos('=', Str2);
  if (Pos1 > 0) and (Pos2 > 0) then
  begin
    Str1 := Copy(Str1, 1, Pos1 - 1);
    Str2 := Copy(Str2, 1, Pos2 - 1);
    Result := CompareStr(Str1, Str2);
  end;
end;

procedure DumpMeasurements(results: TFPHashList);
var
  i: integer;
  reading: PTCityTemp;
  readingStr, ws: string;
  min: double;
  max: double;
  mean: double;
  weatherStationList: TStringList;
  isFirstKey: boolean = True;
begin
  weatherStationList := TStringList.Create;
  for i := 0 to results.Count - 1 do
  begin
    reading := results.Items[i];
    min := RoundEx(reading^.min / 10);
    max := RoundEx(reading^.max / 10);
    mean := RoundEx(reading^.total / reading^.numReadings / 10);
    readingStr := reading^.city + '=' + FormatFloat('0.0', min) +
      '/' + FormatFloat('0.0', mean) + '/' + FormatFloat('0.0', max);
    weatherStationList.Add(readingStr);
    Dispose(reading);
  end;
  weatherStationList.CustomSort(@Compare);
  Write('{');
  for ws in weatherStationList do
  begin
    if not isFirstKey then
      Write(', ');
    Write(ws);
    isFirstKey := False;
  end;
  WriteLn('}');
end;

procedure ReadMeasurements(inputFile: string);
type
  TCharArray = array of char;
var
  BytesRead: int64;
  buffer: TCharArray;
  FileStream: TFileStream;
  ReadBufferStream: TReadBufStream;
  results: TFPHashList;
  idx: integer;
  startOfNextRecord: string;
  startOfNextRecordLength: integer;
  bufferLength: integer = 0;
begin
  try
    buffer := Default(TCharArray);
    SetLength(buffer, BUFFER_SIZE);
    FileStream := TFileStream.Create(inputFile, fmOpenRead);
    FileStream.Position := 0;
    ReadBufferStream := TReadBufStream.Create(FileStream);
    results := TFPHashList.Create;
    startOfNextRecord := '';
    while True do
    begin
      startOfNextRecordLength := Length(startOfNextRecord);
      // if we have leftover from previous read then prepend it to this buffer
      if startOfNextRecordLength > 0 then
        Move(PChar(startOfNextRecord)^, buffer[0], startOfNextRecordLength);
      BytesRead := ReadBufferStream.Read(buffer[startOfNextRecordLength], READ_SIZE);
      if BytesRead < 1 then break;
      // now look in buffer backwards until we find the first LF
      bufferLength := startOfNextRecordLength + BytesRead;
      idx := bufferLength - 1;
      while (buffer[idx] <> REC_SEP) do Inc(idx);
      ProcessMeasurements(buffer, idx + 1, results);
      startOfNextRecordLength := bufferLength - idx - 1;
      if startOfNextRecordLength > 0 then
        SetString(startOfNextRecord, @buffer[idx + 1], startOfNextRecordLength);
    end;
    DumpMeasurements(results);
    ReadBufferStream.Free;
    FileStream.Free;
    results.Free;
  except
    on E: Exception do
    begin
      writeln('File ', inputFile, ' could not be read or written because: ', E.ToString);
    end;
  end;
end;

end.
