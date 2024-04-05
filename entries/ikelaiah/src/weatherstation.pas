unit WeatherStation;

{$mode objfpc}{$H+}{$J-}{$modeSwitch advancedRecords}

interface

uses
  Classes
  , SysUtils
  , Math
  , streamex
  , lgHashMap
  {$IFDEF DEBUG}
  , Stopwatch
  {$ENDIF}
  , Baseline.Common;

type
  { Create a record of temperature stats.

    Borrowed the concept from go's approach to improve performance, save floats as int64.
    This saved ~2 mins processing time for processing 1 billion rows.}
  TStat = record
  var
    min: int64;
    max: int64;
    sum: int64;
    cnt: int64;
  public
    constructor Create(const newMin: int64; const newMax: int64;
      const newSum: int64; const newCount: int64);
    function ToString: string;
  end;
  PStat = ^TStat;

type
  // Create a dictionary, now approx 4 mins faster than Generics.Collections.TDictionary
  TWeatherDictionaryLG = specialize TGHashMapQP<string, TStat>;

type
  // Create a class to encapsulate the temperature observations of each weather station.
  TWeatherStation = class
  private
    fname: string;
    weatherDictionary: TWeatherDictionaryLG;
    weatherStationList: TStringList;
    procedure ReadMeasurements;
    procedure ReadMeasurementsClassic;
    procedure ReadMeasurementsInChunks(const filename: string);
    procedure ParseStationAndTempFromChunk(const chunkData: pansichar;
      const dataSize: int64; const chunkIndex: int64);
    procedure ParseStationAndTemp(const line: string);
    procedure AddCityTemperatureLG(const cityName: string; const newTemp: int64);
    procedure SortWeatherStationAndStats;
    procedure PrintSortedWeatherStationAndStats;
  public
    constructor Create(const filename: string);
    destructor Destroy; override;
    // The main algorithm to process the temp measurements from various weather stations
    procedure ProcessMeasurements;
  end;


implementation

{
  A custom comparer for TStringList.

  The following procedure Written by Székely Balázs for the 1BRC for Object Pascal.
  URL: https://github.com/gcarreno/1brc-ObjectPascal/tree/main
}
function CustomTStringListComparer(AList: TStringList;
  AIndex1, AIndex2: integer): integer;
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

// Remove dots from a string
function RemoveDots(const line: string): string;
var
  index: integer;
begin
  Result := line;
  for index := Length(Result) downto 1 do
  begin
    if Result[index] = '.' then
      Delete(Result, index, 1);
  end;
end;

constructor TStat.Create(const newMin: int64; const newMax: int64;
  const newSum: int64; const newCount: int64);
begin
  self.min := newMin;
  self.max := newMax;
  self.sum := newSum;
  self.cnt := newCount;
end;

function TStat.ToString: string;
var
  minR, meanR, maxR: double; // Store the rounded values prior saving to TStringList.
begin
  minR := RoundExDouble(self.min / 10);
  maxR := RoundExDouble(self.max / 10);
  meanR := RoundExDouble(self.sum / self.cnt / 10);
  Result := FormatFloat('0.0', minR) + '/' + FormatFloat('0.0', meanR) +
    '/' + FormatFloat('0.0', maxR);
end;

constructor TWeatherStation.Create(const filename: string);
begin
  // Assign filename
  fname := filename;
  // Create a dictionary
  weatherDictionary := TWeatherDictionaryLG.Create;
  // Create a TStringList for sorting
  weatherStationList := TStringList.Create;
end;

destructor TWeatherStation.Destroy;
begin
  // Free TStringLIst dictionary
  weatherStationList.Free;
  // Free the dictionary
  weatherDictionary.Free;
end;

procedure TWeatherStation.PrintSortedWeatherStationAndStats;
var
  outputList: string;
  index: int64;
begin

  {$IFDEF DEBUG}
  // Display the line.
  WriteLn('Printing now: ', DateTimeToStr(Now));
  {$ENDIF DEBUG}

  if self.weatherStationList.Count = 0 then
  begin
    WriteLn('Nothing to print. The list is empty.');
    Exit;
  end;

  outputList := '';
  // Print the weather station and the temp stat
  for index := 0 to self.weatherStationList.Count - 1 do
    outputList := outputList + self.weatherStationList[index];

  // Remove last comma and space; ', ', a neat trick from Gus.
  SetLength(outputList, Length(outputList) - 2);
  WriteLn('{', outputList, '}');

  {$IFDEF DEBUG}
  // Display the line.
  WriteLn('Printing done: ', DateTimeToStr(Now));
  {$ENDIF DEBUG}
end;

procedure TWeatherStation.SortWeatherStationAndStats;
var
  wsKey: string;
begin

  {$IFDEF DEBUG}
  // Display the line.
  WriteLn('Sorting now: ', DateTimeToStr(Now));
  {$ENDIF DEBUG}

  wsKey := '';

  if self.weatherDictionary.GetCapacity = 0 then
  begin
    WriteLn('Nothing to Sort.');
    Exit;
  end;

  for wsKey in weatherDictionary.Keys do
  begin
    self.weatherStationList.Add(wsKey + '=' + weatherDictionary[wsKey].ToString + ', ');
  end;

  self.weatherStationList.CustomSort(@CustomTStringListComparer);


  {$IFDEF DEBUG}
  // Display the line.
  WriteLn('Sorting done: ', DateTimeToStr(Now));
  {$ENDIF DEBUG}
end;

procedure TWeatherStation.AddCityTemperatureLG(const cityName: string;
  const newTemp: int64);
var
  stat: TStat;
begin
  // If city name esxists, modify temp as needed
  if self.weatherDictionary.Contains(cityName) then
  begin
    // Get the temp record
    stat := self.weatherDictionary[cityName];

    // If the temp lower then min, set the new min.
    if newTemp < stat.min then
      stat.min := newTemp;

    // If the temp higher than max, set the new max.
    if newTemp > stat.max then
      stat.max := newTemp;

    // Add count for this city.
    stat.sum := stat.sum + newTemp;

    // Increase the counter
    stat.cnt := stat.cnt + 1;

    // Update the stat of this city
    self.weatherDictionary.AddOrSetValue(cityName, stat);
    {$IFDEF DEBUG}
    // Display the line.
    WriteLn('Updated: ', cityName);
    {$ENDIF DEBUG}
  end;

  // If city name doesn't exist add a new entry
  if not self.weatherDictionary.Contains(cityName) then
  begin
    self.weatherDictionary.Add(cityName, TStat.Create(newTemp, newTemp, newTemp, 1));

    {$IFDEF DEBUG}
    // Display the line.
    WriteLn('weatherDictionary count: ', inttostr(self.weatherDictionary.Count));
    WriteLn('Added: ', cityName);
    {$ENDIF DEBUG}
  end;
end;

procedure TWeatherStation.ParseStationAndTemp(const line: string);
var
  delimiterPos: integer;
  parsedStation, strTemp: string;
  parsedTemp, valCode: int64;
begin

  // Get position of the delimiter
  delimiterPos := Pos(';', line);
  if delimiterPos > 0 then
  begin
    // Get the weather station name
    // Using Copy and POS - as suggested by Gemini AI.
    // This part saves 3 mins faster when processing 1 billion rows.
    parsedStation := Copy(line, 1, delimiterPos - 1);

    // Get the temperature recorded, as string, remove '.' from string float
    // because we want to save it as int64.
    strTemp := Copy(line, delimiterPos + 1, Length(line));
    // strTemp := StringReplace(strTemp, '.', '', [rfReplaceAll]);
    // The above operation is a bit expensive.
    // Rewrote a simple function which prevents creation of new string
    // in each iteration. Saved approx 20-30 seconds for 1 billion row.
    // Remove dots turns a float into an int.
    strTemp := RemoveDots(strTemp);

    // Add the weather station and the recorded temp (as int64) in the TDictionary
    Val(strTemp, parsedTemp, valCode);
    if valCode <> 0 then Exit;

    // Add a record in TWeatherDictionary
    self.AddCityTemperatureLG(parsedStation, parsedTemp);
  end;
end;

procedure TWeatherStation.ReadMeasurements;
var
  fileStream: TFileStream;
  streamReader: TStreamReader;
  line: string;
begin

  // Open the file for reading
  fileStream := TFileStream.Create(self.fname, fmOpenRead or fmShareDenyNone);
  try
    streamReader := TStreamReader.Create(fileStream, 655360, False);
    try
      // Read and parse chunks of data until EOF -------------------------------
      while not streamReader.EOF do
      begin
        //line := streamReader.ReadLine;
        self.ParseStationAndTemp(streamReader.ReadLine);
      end;// End of read and parse chunks of data ------------------------------
    finally
      streamReader.Free;
    end;
  finally
    // Close the file
    fileStream.Free;
  end;
end;

procedure TWeatherStation.ReadMeasurementsClassic;
var
  inputFile: System.TextFile;
  textBuffer: array[1..131072] of byte;
  line: string;
begin

  // Open the file for reading
  AssignFile(inputFile, self.fname);
  SetTextBuf(inputFile, textBuffer);
  try
    Reset(inputFile);

    // Read and parse chunks of data until EOF -------------------------------
    while not EOF(inputFile) do
    begin
      ReadLn(inputFile, line);
      self.ParseStationAndTemp(line);
    end;// End of read and parse chunks of data ------------------------------

  finally
    // Close the file
    CloseFile(inputFile);
  end;
end;

{procedure TWeatherStation.ParseStationAndTempFromChunk(const chunkData: pansichar;
  const dataSize: int64; const chunkIndex: int64);
var
  mStream: TMemoryStream;
  streamReader: TStreamReader;
  currentString: string;
begin
  mStream:=TMemoryStream.Create;
  try
    mStream.WriteBuffer(chunkData^, dataSize);
    mStream.Position:=0;

    streamReader:=TStreamReader.Create(mStream, 1048576, False);
    try
      while not streamReader.Eof do
      begin
        currentString:=streamReader.ReadLine;
        self.ParseStationAndTemp(currentString);
      end;
    finally
      streamReader.Free;
    end;
  finally
    mStream.Free;
  end;
end;}

procedure TWeatherStation.ParseStationAndTempFromChunk(const chunkData: pansichar;
  const dataSize: int64; const chunkIndex: int64);
var
  index, lineStart, lineLength: int64;
begin
  lineStart := 0;

  // Check for Line Feed (LF)
  for index := 0 to dataSize - 1 do
  begin
    if chunkData[index] = #10 then
    begin

      lineLength := index - lineStart;

      // Remove potential CR before LF (for Windows)
      if (chunkData[index - 1] = #13) and (index < dataSize - 1) then
        Dec(LineLength);

      // The current line is now: Buffer[LineStart..LineStart+LineLength-1]
      // WriteLn(chunkData[lineStart..lineStart + lineLength - 1], '.');
      self.ParseStationAndTemp(chunkData[lineStart..lineStart + lineLength - 1]);
      // Skip to the next 'line' in the buffer
      lineStart := index + 1;
    end;
  end;
end;

procedure TWeatherStation.ReadMeasurementsInChunks(const filename: string);
const
  defaultChunkSize: int64 = 67108864; // 64MB in bytes
var
  fileStream: TFileStream;
  buffer: pansichar;
  bytesRead, totalBytesRead, chunkSize, lineBreakPos, chunkIndex: int64;
begin
  chunkSize := defaultChunkSize * 4 * 4; // Now 1GB in bytes ~ 5:53 :D
  // chunkSize := defaultChunkSize * 4; // Now 512GB in bytes ~ 5.50 :D

  // Open the file for reading
  fileStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    // Allocate memory buffer for reading chunks
    // Ref: https://www.freepascal.org/docs-html/rtl/system/getmem.html
    GetMem(buffer, chunkSize);
    try
      totalBytesRead := 0;
      chunkIndex := 0;

      // Read and parse chunks of data until EOF
      while totalBytesRead < fileStream.Size do
      begin
        {$IFDEF DEBUG}
        WriteLn('Processing chunk index: ', IntToStr(chunkIndex));
        {$ENDIF DEBUG}

        bytesRead := fileStream.Read(buffer^, chunkSize);

        // Update total bytes read
        Inc(totalBytesRead, bytesRead);

        // Find the position of the last newline character in the chunk
        lineBreakPos := BytesRead;
        while (lineBreakPos > 0) and (Buffer[lineBreakPos - 1] <> #10) do
          Dec(lineBreakPos);

        { Now, must ensure that if the last byte read in the current chunk
          is not a newline character, the file pointer is moved back to include
          that byte and any preceding bytes of the partial line in the next
          chunk's read operation.

          Also, no need to update the BytesRead variable in this context because
          it represents the actual number of bytes read from the file, including
          any partial line that may have been included due to moving the file
          pointer back.
          Ref: https://www.freepascal.org/docs-html/rtl/classes/tstream.seek.html}
        if lineBreakPos < bytesRead then
          fileStream.Seek(-(bytesRead - lineBreakPos), soCurrent);

        // Parse the buffer line by line here
        // This is to slow!
        self.ParseStationAndTempFromChunk(buffer, lineBreakPos, chunkIndex);

        // Increase chunk index - a counter
        Inc(chunkIndex);
      end;
    finally
      // Free the memory buffer
      FreeMem(buffer);
    end;
  finally
    // Close the file
    fileStream.Free;
  end;
end;

// The main algorithm
procedure TWeatherStation.ProcessMeasurements;
begin
  // self.ReadMeasurements;
  // self.ReadMeasurementsClassic;
  self.ReadMeasurementsInChunks(self.fname); {This method cuts ~ 30 - 40 seconds of processing time from ~6.45 to 6.00}
  self.SortWeatherStationAndStats;
  self.PrintSortedWeatherStationAndStats;
end;

end.
