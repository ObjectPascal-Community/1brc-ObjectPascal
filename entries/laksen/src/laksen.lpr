program laksen;

{$mode objfpc}{$h+}

uses
  cthreads,
  SysUtils,
  Classes,
  Contnrs,
  Math,
  Baseline.Common;

type

  TParser = class;
  TEntryList = class;

  { TChunk }

  TChunk = class
  public
    StartOffset, Size: qword;

    constructor Create(AStart, ALast: qword);

    procedure Execute(const AHashTable: array of TEntryList; AFileHandle: THandle; var ABuffer);
  end;

  { TEntry }

  TEntry = class
  public
    Hash: qword;
    Key: string;

    Min, Max, Sum, Count: longint;

    constructor Create(const AKey: string);
    procedure AddTemperature(ATemp: longint); inline;
    procedure Combine(AOther: TEntry);
  end;

  { TEntryList }

  TEntryList = class
  public
    Entries: array of TEntry;
    Count: longint;

    function Add(const AName: string): TEntry;
    constructor Create;
  end;

  { TParserWorker }

  TParserWorker = class(TThread)
  private
    fParser: TParser;
  protected
    procedure Execute; override;
  public
    constructor Create(AParser: TParser);
  end;

  { TParser }

  TParser = class
  private
    fJobList: TList;
    fFileName: string;
    fJobCS, fEntryCS: TRTLCriticalSection;
    Entries: TStringList;
    fMaxBuffer: Qword;

    procedure AddEntry(AEntry: TEntry);
  protected
    procedure ParseFunc;
  public
    procedure Parse(AThreads: longint);
    procedure Dump;

    constructor Create(const AFilename: string; AChunkSize: int64);
    destructor Destroy; override;
  end;

  function FindLast(const ABuf; ASize: longint; AChar: byte): longint;
  var
    pc: pbyte;
    i: longint;
  begin
    pc := pbyte(@ABuf);

    for i := ASize-1 downto 0 do
      if pc[i] = AChar then
        exit(i);
    Result := -1;
  end;

  function CalcHash(const AData; ABytes: longint): qword;
  var
    i: longint;
  begin
    {$push}
    {$OVERFLOWCHECKS OFF}
    {$RANGECHECKS OFF}
    Result := 6541009;
    for i := 0 to abytes-1 do
    begin
      Result := Result+pbyte(@AData)[i];
      Result := Result*123457;
    end;
    {$pop}
  end;

  function ParseTemp(AStr: PChar; ALen: longint): longint; inline;
  var
    Value: longword;
  begin
    Value := plongword(astr)^;

    if alen = 3 then
      Result := (((Value shr 16) and $FF)-$30)+10*((Value and $FF)-$30)
    else
      Result := (((Value shr 24) and $FF)-$30)+10*(((Value shr 8) and $FF)-$30)+100*((Value and $FF)-$30);
  end;

  function TEntryList.Add(const AName: string): TEntry;
  begin
    if Count>=Length(Entries) then
      setlength(entries, length(Entries)*4 div 3);

    Result := TEntry.Create(AName);
    entries[Count] := Result;
    Inc(Count);
  end;

  constructor TEntryList.Create;
  begin
    inherited Create;
    SetLength(Entries, 4);
    Count := 0;
  end;

  { TEntry }

  constructor TEntry.Create(const AKey: string);
  begin
    inherited Create;
    Max := -10000;
    Min := 10000;
    Sum := 0;
    Key := AKey;
  end;

  procedure TEntry.AddTemperature(ATemp: longint); inline;
  begin
    if ATemp>Max then Max := ATemp;
    if ATemp<Min then Min := ATemp;
    Inc(Sum, ATemp);
    Inc(Count);
  end;

  procedure TEntry.Combine(AOther: TEntry);
  begin
    if AOther.Max>Max then Max := AOther.Max;
    if AOther.Min<Min then Min := AOther.Min;

    Inc(Sum, AOther.Sum);
    Inc(Count, AOther.Count);
  end;

  { TChunk }

  constructor TChunk.Create(AStart, ALast: qword);
  begin
    inherited Create;
    StartOffset := AStart;
    Size := ALast-AStart+1;
  end;

  procedure TChunk.Execute(const AHashTable: array of TEntryList; AFileHandle: THandle; var ABuffer);
  var
    r: longint;
    offset, lineEnd: longint;
    remainder: QWord;
    ptr: pbyte;
    hash: qword;
    delim, tempLen: SizeInt;
    tempStart: PChar;
    tempNeg: boolean;
    temp: longint;
    i: longint;
    Name: string;
    entry, tmp: TEntry;
    bag: TEntryList;
  begin
    if FileSeek(AFileHandle, int64(StartOffset), fsFromBeginning)<>StartOffset then
      raise Exception.Create('Failed to seek');
    r := FileRead(AFileHandle, ABuffer, size);
    if r<>Size then
      raise Exception.Create('Failed to read');

    // Do actual parsing
    offset := 0;
    while True do
    begin
      remainder := size-offset;
      if remainder<2 then
        break;

      // Find end of line
      ptr := @pbyte(@ABuffer)[offset];
      lineEnd := IndexChar(ptr^, remainder, #$A);

      // Mark EOL
      ptr[lineEnd-1] := 0;

      // Find delimiter and add null terminator
      delim := IndexChar(ptr^, lineEnd-2, ';');
      ptr[delim] := 0;

      tempStart := PChar(@ptr[delim+1]);
      tempLen := lineEnd-2-delim;
      tempNeg := False;

      if tempStart^ = '-' then
      begin
        tempNeg := True;
        Inc(tempStart);
        Dec(tempLen);
      end;

      temp := ParseTemp(tempStart, tempLen);
      if tempNeg then temp := -temp;

      Name := PChar(@ptr[0]);
      hash := CalcHash(ptr^, delim);

      // Stuff into hash table
      entry := nil;
      bag := AHashTable[hash and (length(AHashTable)-1)];
      for i := 0 to bag.Count-1 do
      begin
        tmp := bag.Entries[i];
        if tmp.Hash<>hash then
          continue;
        if tmp.Key = Name then
        begin
          entry := tmp;
          break;
        end;
      end;

      if entry = nil then
      begin
        entry := bag.Add(Name);
        entry.hash := hash;
      end;

      entry.AddTemperature(temp);

      // On to next line
      offset := offset+lineEnd+1;
    end;
  end;

  procedure TParserWorker.Execute;
  begin
    fParser.ParseFunc;
  end;

  constructor TParserWorker.Create(AParser: TParser);
  begin
    fParser := AParser;
    inherited Create(False);
  end;

  procedure TParser.Parse(AThreads: longint);
  var
    threads: array of TParserWorker;
    i: longint;
  begin
    setlength(threads, athreads);

    for i := 0 to AThreads-1 do
      Threads[i] := TParserWorker.Create(self);

    for i := 0 to AThreads-1 do
      Threads[i].WaitFor;

    for i := 0 to AThreads-1 do
      Threads[i].Free;
  end;

  function AsNum(AValue, ACount: longint): string;
  begin
    Result := formatfloat('0.0', RoundExDouble(AValue/ACount)/10);
  end;

  procedure TParser.Dump;
  var
    entry: TEntry;
    i: longint;
  begin
    Write('{');

    for i := 0 to Entries.Count-1 do
    begin
      if i>0 then
        Write(', ');
      entry := TEntry(Entries.Objects[i]);

      Write(entry.Key, '=');
      Write(AsNum(entry.Min, 1), '/', AsNum(entry.Sum, entry.Count), '/', AsNum(entry.Max, 1));
    end;

    writeln('}');
  end;

  procedure TParser.AddEntry(AEntry: TEntry);
  var
    target: TEntry;
    idx: integer;
  begin
    if not Entries.Find(AEntry.Key, idx) then
    begin
      target := TEntry.Create(AEntry.Key);
      Entries.AddObject(AEntry.Key, target);
    end
    else
      target := TEntry(Entries.Objects[idx]);

    target.Combine(AEntry);
  end;

  procedure TParser.ParseFunc;
  var
    f: THandle;
    job: TChunk;
    buffer: array of byte;
    hashTables: array of TEntryList;
    i, i2: integer;
    bag: TEntryList;
  begin
    f := FileOpen(fFilename, fmOpenRead or fmShareDenyNone);
    if f<0 then
      raise Exception.Create('Failed to open file');
    setlength(buffer, fMaxBuffer+31); // Add extra to allow SIMD tricks

    setlength(hashTables, 65536);

    for i := 0 to high(hashTables) do
      hashTables[i] := TEntryList.Create;

    while True do
    begin
      EnterCriticalSection(fJobCS);
      try
        if fJobList.Count<=0 then
          break;
        job := TChunk(fJobList.First);
        fJobList.Delete(0);
      finally
        LeaveCriticalSection(fJobCS);
      end;

      job.Execute(hashTables, f, buffer[0]);
      job.Free;
    end;

    EnterCriticalSection(fEntryCS);
    for i := 0 to high(hashTables) do
    begin
      bag := hashTables[i];
      for i2 := 0 to bag.Count-1 do
        AddEntry(bag.Entries[i2]);
    end;
    LeaveCriticalSection(fEntryCS);

    for i := 0 to high(hashTables) do
    begin
      bag := hashTables[i];
      for i2 := 0 to bag.Count-1 do
        TEntry(bag.Entries[i2]).Free;
      hashTables[i].Free;
    end;
    FileClose(f);
  end;

  { TParser }
  constructor TParser.Create(const AFilename: string; AChunkSize: int64);
  var
    f: THandle;
    Size, Offset, LastOffset, Jobs: int64;
    i, r, ofs: longint;
    buffer: array[0..1023] of byte;
    chunk: TChunk;
  begin
    inherited Create;
    InitCriticalSection(fJobCS);
    fMaxBuffer := 0;

    InitCriticalSection(fEntryCS);
    Entries := TStringList.Create;
    Entries.CaseSensitive := True;
    Entries.Sorted := True;

    fJobList := TList.Create;
    fFileName := AFilename;

    f := FileOpen(AFilename, fmOpenRead);
    if f<0 then
      raise Exception.Create('Failed to open file');

    LastOffset := 0;
    Size := FileSeek(f, int64(0), fsFromEnd);

    Jobs := (Size+AChunkSize-1) div AChunkSize;

    Dec(Size, 16); // Ensure that the last chunk has 16 bytes to look in

    for i := 1 to Jobs do
    begin
      Offset := FileSeek(f, (i*Size) div Jobs, fsFromBeginning);
      r := FileRead(f, buffer[0], sizeof(buffer));
      ofs := FindLast(buffer[0], r, $0A);
      Offset := Offset+ofs;

      chunk := TChunk.Create(LastOffset, Offset);
      fMaxBuffer := Max(fMaxBuffer, chunk.Size);
      fJobList.Add(chunk);

      LastOffset := Offset+1;
    end;

    FileClose(f);
  end;

  destructor TParser.Destroy;
  begin
    Entries.Free;
    fJobList.Free;
    inherited Destroy;
  end;

var
  p: TParser;
  fn: string;
  ChunkSize, threads: longint;

  OutputBuffer: array[0..65535] of char;
begin
  SetTextBuf(output, OutputBuffer[0], sizeof(OutputBuffer));

  if ParamCount<>3 then
  begin
    Writeln('Usage: laksen <input-filename> <threads> <chunksize>');
    WriteLn();
    WriteLn('Threads: Number of threads to use');
    WriteLn('chunksize: Size is in megabytes. Approximate size of');
    WriteLn('           chunk that each thread processes at a time.');
    Halt(-1);
  end;

  fn := ParamStr(1);
  threads := StrToInt(ParamStr(2));
  ChunkSize := StrToInt(ParamStr(3));

  if threads<1 then
  begin
    writeln('Number of threads must be positive');
    Halt(-2);
  end;

  if ChunkSize<1 then
  begin
    writeln('Chunksize must be positive');
    Halt(-3);
  end;

  p := TParser.Create(fn, int64(ChunkSize)*1024*1024);
  p.Parse(threads);
  p.Dump();
  p.Free;
end.
