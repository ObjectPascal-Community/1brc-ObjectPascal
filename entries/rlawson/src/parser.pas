unit parser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, bufstream, Contnrs, Math, util, Baseline.Common;

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
    // read the city by looking for semicolon
    city := '';
    cityStart := idx;
    Inc(idx); // city has to be at least one character
    while buffer[idx] <> REC_SEP do Inc(idx);
    SetString(city, @buffer[cityStart], (idx - cityStart));
    // parse the temp reading
    Inc(idx); // move pointer past the ;
    currentTempSign := 1;
    // check for negative sign, if so flag the multiplier and then move past neg sign
    if buffer[idx] = NEGATIVE_SIGN then
    begin
      currentTempSign := -1;
      Inc(idx);
    end;
    // look ahead - is decimal point 2 spaces away then we have temp = dd.d
    // other wise d.d
    temp := 0;
    if buffer[idx + 2] = DECIMAL_POINT then
    begin
      temp := currentTempSign * (100 * Integer(buffer[idx]) + 10 *
        Integer(buffer[idx + 1]) + Integer(buffer[idx + 3]) - 5328);
      idx := idx + 6;
    end
    else
    begin
      temp := currentTempSign * (10 * Integer(buffer[idx]) + Integer(buffer[idx + 2]) - 528);
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
    min := RoundExDouble(reading^.min / 10);
    max := RoundExDouble(reading^.max / 10);
    mean := RoundExDouble(reading^.total / reading^.numReadings / 10);
    readingStr := reading^.city + '=' + FormatFloat('0.0', min) +
      '/' + FormatFloat('0.0', mean) + '/' + FormatFloat('0.0', max);
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

procedure ReadMeasurements(inputFile: string);
var
  totalBytesRead, BytesRead: int64;
  Buffer: array [0..BUFFER_SIZE] of char;
  FileStream: TFileStream;
  fileSize: int64;
  ReadBufferStream: TReadBufStream;
  results: TFPHashList;
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
    results := TFPHashList.Create;
    startOfNextRecord := '';
    while totalBytesRead <= fileSize do
      // While the amount of data read is less than or equal to the size of the stream do
    begin
      startOfNextRecordLength := Length(startOfNextRecord);
      // if we have leftover from previous read then prepend it to this buffer
      if startOfNextRecordLength > 0 then
        Move(PChar(startOfNextRecord)^, Buffer[0], startOfNextRecordLength);
      BytesRead := ReadBufferStream.Read(Buffer[startOfNextRecordLength], READ_SIZE);
      if BytesRead < 1 then break;
      // now look in buffer backwards until we find the first LF
      bufferLength := startOfNextRecordLength + BytesRead;
      idx := bufferLength - 1;
      while (buffer[idx] <> LF) do Dec(idx);
      ProcessMeasurements(Buffer, idx + 1, results);
      startOfNextRecord := '';
      startOfNextRecordLength := bufferLength - idx - 1;
      if startOfNextRecordLength > 0 then
        SetString(startOfNextRecord, @buffer[idx + 1], startOfNextRecordLength);
      Inc(totalBytesRead, BytesRead);
    end;
    DumpMeasurements(results);
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
