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
  LF: char = chr(10);
  ANSI_ZERO: integer = 48;
  DECIMAL_POINT: char = '.';
  NEGATIVE_SIGN: char = '-';

procedure ProcessMeasurements(var buffer: array of char; bufferLength: integer;
  var results: TFPHashList);
var
  currentChar: char;
  idx: integer = 0;
  currentTempSign: integer = 1;
  temp, cityStart: integer;
  city: shortstring;
  reading: PTCityTemp;
begin
  //Writeln('bufferLength: ', bufferLength);
  while idx < (bufferLength - 1) do
  begin
    {slurp up the city}
    city := '';
    cityStart := idx;
    currentChar := buffer[idx];
    while currentChar <> REC_SEP do
    begin
      Inc(idx);
      currentChar := buffer[idx];
    end;
    SetString(city, @buffer[cityStart], (idx - cityStart));
    {slurp up the temp reading}
    Inc(idx); // move pointer past the ;
    currentTempSign := 1;
    // check for negative sign, if so flag the multiplier and then move past neg sign
    if buffer[idx] = NEGATIVE_SIGN then
    begin
      currentTempSign := -1;
      Inc(idx);
    end;
    // look ahead - is decimal point 2 spaces away then we have two digits
    temp := 0;
    if buffer[idx + 2] = DECIMAL_POINT then
    begin
      temp := 100 * (byte(buffer[idx]) - ANSI_ZERO);
      Inc(idx);
    end;
    temp := temp + 10 * (byte(buffer[idx]) - ANSI_ZERO);
    idx := idx + 2;
    temp := currentTempSign * (temp + (byte(buffer[idx]) - ANSI_ZERO));
    if temp > 999 then
    begin
      WriteLn('Somethign wrong!', city, ' | ', temp, ' | ', buffer[idx - 3],
        ' | ', buffer[idx - 2],
        ' | ', buffer[idx - 1], ' | ', buffer[idx]);
      break;
    end;

    currentChar := buffer[idx];
    while currentChar <> LF do
    begin
      Inc(idx);
      currentChar := buffer[idx];
    end;
    Inc(idx);
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
  //WriteLn('results: ', results.Count);
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
  //WriteLn(results.Count);
  weatherStationList := TStringList.Create;
  for i := 0 to results.Count - 1 do
  begin
    reading := results.Items[i];
    min := RoundEx(reading^.min / 10);
    max := RoundEx(reading^.max / 10);
    mean := RoundEx(reading^.total / reading^.numReadings / 10);
    readingStr := reading^.city + '=' + FormatFloat('0.0', min) +
      '/' + FormatFloat('0.0', mean) + '/' + FormatFloat('0.0', max);
    {$IFDEF DEBUG}
       readingStr := reading^.city + '=' + FormatFloat('0.0', min) +
      '/' + FormatFloat('0.0', mean) + '/' + FormatFloat('0.0', max) +
      '/' + IntToStr(reading^.total) + '/' + IntToStr(reading^.numReadings);
    {$ENDIF}
    weatherStationList.Add(readingStr);
    Dispose(reading);
  end;
  weatherStationList.CustomSort(@Compare);
  Write('{');
  for ws in weatherStationList do
  begin
    // If it's not the first key, print a comma
    if not isFirstKey then
      Write(', ');
    // Print the weather station and the temp stat
    Write(ws);
    // Set isFirstKey to False after printing the first key
    isFirstKey := False;
  end;
  WriteLn('}');
end;


procedure DumpExceptionCallStack(E: Exception);
var
  I: integer;
  Frames: PPointer;
  Report: string;
begin
  Report := 'Program exception! ' + LineEnding + 'Stacktrace:' +
    LineEnding + LineEnding;
  if E <> nil then
  begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
      'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  WriteLn(Report);
  Halt; // End of program execution
end;


procedure ReadMeasurements(inputFile: string);
var
  totalBytesRead, BytesRead: int64;
  Buffer: array [0..BUFFER_SIZE] of char;
  FileStream: TFileStream;
  fileSize: int64;
  ReadBufferStream: TReadBufStream;
  starttime: uint64;
  elapsedTimeSec, MBRead: double;
  results: TFPHashList;
  currentChar: char;
  idx: integer;
  startOfNextRecord: string;
  startOfNextRecordLength: integer;
  bufferLength: integer = 0;
begin
  try
    FileStream := TFileStream.Create(inputFile, fmOpenRead);
    FileStream.Position := 0;  // Ensure you are at the start of the file
    ReadBufferStream := TReadBufStream.Create(FileStream);
    fileSize := FileStream.size;
    totalBytesRead := 0;
    starttime := GetTickCount64;
    results := TFPHashList.Create;
    startOfNextRecord := '';
    while totalBytesRead <= fileSize do
      // While the amount of data read is less than or equal to the size of the stream do
    begin
      startOfNextRecordLength := Length(startOfNextRecord);
      //WriteLn('startOfNextRecordLength: ', startOfNextRecordLength);
      // if we have leftover from previous read then prepend it to this buffer
      if startOfNextRecordLength > 0 then
        Move(PChar(startOfNextRecord)^, Buffer[0], startOfNextRecordLength);
      BytesRead := ReadBufferStream.Read(Buffer[startOfNextRecordLength], READ_SIZE);
      //WriteLn('Bytes read: ', BytesRead);
      if BytesRead < 1 then break;
      // now look in buffer backwards until we find the first LF
      bufferLength := startOfNextRecordLength + BytesRead;
      idx := bufferLength - 1;
      currentChar := buffer[idx];
      while (currentChar <> LF) do
      begin
        Dec(idx);
        currentChar := buffer[idx];
      end;
      ProcessMeasurements(Buffer, idx + 1, results);
      startOfNextRecord := '';
      startOfNextRecordLength := bufferLength - idx - 1;
      //WriteLn('startOfNextRecordLength: ', startOfNextRecordLength);
      if startOfNextRecordLength > 0 then
        SetString(startOfNextRecord, @buffer[idx + 1], startOfNextRecordLength);
      Inc(totalBytesRead, BytesRead);
    end;
    DumpMeasurements(results);
    elapsedTimeSec := (GetTickCount64() - starttime) / 1000;
    MBRead := (totalBytesRead / (1024 * 1024));
    {$IFDEF DEBUG}
    WriteLn(inputFile);
    WriteLn('Buffer size: ', SizeOf(Buffer));
    WriteLn('Read size: ', READ_SIZE);
    WriteLn('File size: ', FileStream.Size);
    WriteLn('Total Bytes Read: ', totalBytesRead);
    WriteLn(Format('%f MB read', [MBRead]));
    WriteLn(Format('%f secs', [elapsedTimeSec]));
    WriteLn(Format('%f MB/s processed', [MBRead / elapsedTimeSec]));
    {$ENDIF}
    ReadBufferStream.Free;
    FileStream.Free;
    results.Free;
  except
    on E: Exception do
    begin
      writeln('File ', inputFile, ' could not be read or written because: ', E.ToString);
      DumpExceptionCallStack(E);
    end;
  end;
end;

end.
