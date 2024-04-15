/// MIT code (c) Arnaud Bouchez, using the mORMot 2 framework
program brcmormot;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
{$endif OSWINDOWS}

uses
  {$ifdef UNIX}
  cthreads,
  {$endif UNIX}
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.data;

type
  // a weather station info, using 1/4rd of a CPU L1 cache line (64/4=16 bytes)
  TBrcStation = packed record
    NameHash: cardinal;   // crc32c perfect hash of the name
    Sum, Count: integer;  // we ensured no overflow occurs with 32-bit range
    Min, Max: SmallInt;   // 16-bit (-32767..+32768) temperatures * 10
  end;
  PBrcStation = ^TBrcStation;

  TBrcList = record
  public
    StationHash: array of word;      // store 0 if void, or Station[] index + 1
    Station: array of TBrcStation;
    StationName: array of PUtf8Char; // directly point to input memmap file
    Count: PtrInt;
    procedure Init(max: integer);
    function Add(h32: PtrUInt; name: PUtf8Char; h: PtrUInt): PBrcStation;
    function Search(name: pointer; h32: cardinal): PBrcStation; inline;
  end;

  TBrcMain = class
  protected
    fSafe: TOSLightLock;
    fEvent: TSynEvent;
    fRunning, fMax: integer;
    fCurrentChunk: PUtf8Char;
    fCurrentRemain, fChunkSize: PtrUInt;
    fList: TBrcList;
    fMem: TMemoryMap;
    procedure Aggregate(const another: TBrcList);
    function GetChunk(out start, stop: PUtf8Char): boolean;
  public
    constructor Create(const fn: TFileName; threads, chunkmb, max: integer;
      affinity: boolean);
    destructor Destroy; override;
    procedure WaitFor;
    function SortedText: RawUtf8;
  end;

  TBrcThread = class(TThread)
  protected
    fOwner: TBrcMain;
    fList: TBrcList; // each thread work on its own list
    procedure Execute; override;
  public
    constructor Create(owner: TBrcMain);
  end;


{ optimized crc32c-based hash function }

const
  HASHSIZE = 1 shl 17; // slightly oversized to avoid most collisions

{$ifdef OSLINUXX64}

function NameHash(buf, bufend: pointer): PtrUInt; nostackframe; assembler;
asm
        // rdi=buf rsi=bufend
        xor     eax, eax
        lea     rdx, [rsi - 8]            // rdx = last 8 bytes (may overlap)
        sub     rsi, rdi
        mov     ecx, esi                  // esi = ecx = buflen
        shr     esi, 3
        jz      @less8
        crc32   rax, qword ptr [rdi]
        crc32   rax, qword ptr [rdx]      // branchless for 8..16 bytes
        dec     esi
        ja      @more8
@end8:  ret
@more8: crc32   rax, qword ptr [rdi + 8]  // 17..23 bytes
        dec     esi
        jbe     @end8
        crc32   rax, qword ptr [rdi + 16] // 24..xx bytes
        ret
@less8: test    cl, 4
        jz      @less4
        crc32   eax, dword ptr [rdi]
        crc32   eax, dword ptr [rdx + 4]  // 4..7 bytes
        ret
@less4: crc32   eax, word ptr [rdi]
        crc32   eax, word ptr [rdx + 6]   // 2..3 bytes
end;

{$else}

function NameHash(buf, bufend: pointer): PtrUInt; inline;
begin
  result := crc32c(0, buf, PAnsiChar(bufend) - buf); // sse4.2/armv8 mormot code
end;

{$endif OSLINUXX64}


{ TBrcList }

procedure TBrcList.Init(max: integer);
begin
  assert(max <= high(StationHash[0]));
  SetLength(Station, max);
  SetLength(StationHash, HASHSIZE);
  SetLength(StationName, max);
end;

function TBrcList.Add(h32: PtrUInt; name: PUtf8Char; h: PtrUInt): PBrcStation;
begin
  assert(Count < length(Station));
  StationName[Count] := name;
  result := @Station[Count];
  inc(Count);
  StationHash[h] := Count;
  result^.NameHash := h32;
  result^.Min := high(result^.Min);
  result^.Max := low(result^.Max);
end;

function TBrcList.Search(name: pointer; h32: cardinal): PBrcStation;
var
  h, x: PtrUInt;
begin
  h := h32;
  repeat
    h := h and (HASHSIZE - 1);
    x := StationHash[h];
    if x = 0 then
      break; // void slot
    result := @Station[x - 1];
    if result^.NameHash = h32 then
      exit; // found this perfect hash = found this station name
    inc(h); // hash modulo collision: linear probing
  until false;
  result := Add(h32, name, h);
end;


{ TBrcThread }

constructor TBrcThread.Create(owner: TBrcMain);
begin
  fOwner := owner;
  FreeOnTerminate := true;
  fList.Init(fOwner.fMax);
  InterlockedIncrement(fOwner.fRunning);
  inherited Create({suspended=}false);
end;

procedure TBrcThread.Execute;
var
  start, stop: PUtf8Char;
  p: PByteArray;
  s: PBrcStation;
  neg: PtrInt;
  v: integer;
begin
  while fOwner.GetChunk(start, stop) do
  begin
    // parse this thread chunk
    p := @start[2];
    repeat
      // parse the name; and retrieve the corresponding station slot
      while p[0] <> ord(';') do
        inc(PByte(p));
      s := fList.Search(start, NameHash(start, p));
      inc(PByte(p));
      // parse the temperature (as -12.3 -3.4 5.6 78.9 patterns) into value * 10
      neg := 1;
      if p[0] = ord('-') then
      begin
        inc(PByte(p));
        neg := -1;
      end;
      if p[2] = ord('.') then // xx.x
      begin
        v := (p[0] * 100 + p[1] * 10 + p[3] - (ord('0') * 111)) * neg;
        inc(PByte(p), 8);
      end
      else
      begin
        v := (p[0] * 10 + p[2] - (ord('0') * 11)) * neg; // x.x
        inc(PByte(p), 7);
      end;
      start := PUtf8Char(p) - 2;
      // store the value
      inc(s^.Sum, v);
      inc(s^.Count);
      if v < s^.Min then
        s^.Min := v; // branchless cmovg/cmovl is not better
      if v > s^.Max then
        s^.Max := v;
    until PUtf8Char(p) >= stop;
  end;
  // aggregate this thread values into the main list
  fOwner.Aggregate(fList);
end;


{ TBrcMain }

constructor TBrcMain.Create(const fn: TFileName; threads, chunkmb, max: integer;
  affinity: boolean);
var
  i, cores, core: integer;
  one: TBrcThread;
begin
  fEvent := TSynEvent.Create;
  fSafe.Init;
  if not fMem.Map(fn) then
    raise ESynException.CreateUtf8('Impossible to find %', [fn]);
  fMax := max;
  fChunkSize := chunkmb shl 20;
  fCurrentChunk := pointer(fMem.Buffer);
  fCurrentRemain := fMem.Size;
  core := 0;
  cores := SystemInfo.dwNumberOfProcessors;
  for i := 0 to threads - 1 do
  begin
    one := TBrcThread.Create(self);
    if not affinity then
      continue;
    SetThreadCpuAffinity(one, core);
    inc(core, 2);
    if core >= cores then
      dec(core, cores - 1); // e.g. 0,2,1,3,0,2.. with 4 cpus
  end;
end;

destructor TBrcMain.Destroy;
begin
  inherited Destroy;
  fMem.UnMap;
  fSafe.Done;
  fEvent.Free;
end;

function TBrcMain.GetChunk(out start, stop: PUtf8Char): boolean;
var
  chunk: PtrUInt;
begin
  result := false;
  fSafe.Lock;
  chunk := fCurrentRemain;
  if chunk <> 0 then
  begin
    start := fCurrentChunk;
    if chunk > fChunkSize then
    begin
      stop := GotoNextLine(start + fChunkSize);
      chunk := stop - start;
    end
    else
    begin
      stop := @start[chunk];
      while stop[-1] <= ' ' do
        dec(stop); // ensure final chunk stops at meaningful char
    end;
    dec(fCurrentRemain, chunk);
    fCurrentChunk := @fCurrentChunk[chunk];
    result := true;
  end;
  fSafe.UnLock;
end;

function NameEnd(p: PUtf8Char): PUtf8Char; inline;
begin
  result := p;
  repeat
    inc(result);
  until result^ = ';';
end;

procedure TBrcMain.Aggregate(const another: TBrcList);
var
  n: integer;
  s, d: PBrcStation;
  p: PPUtf8Char;
begin
  fSafe.Lock; // several TBrcThread may finish at the same time
  if fList.Count = 0 then
    fList := another
  else
  begin
    n := another.Count;
    s := pointer(another.Station);
    p := pointer(another.StationName);
    repeat
      d := fList.Search(p^, NameHash(p^, NameEnd(p^)));
      inc(d^.Count, s^.Count);
      inc(d^.Sum, s^.Sum);
      if s^.Max > d^.Max then
        d^.Max := s^.Max;
      if s^.Min < d^.Min then
        d^.Min := s^.Min;
      inc(s);
      inc(p);
      dec(n);
    until n = 0;
  end;
  fSafe.UnLock;
  if InterlockedDecrement(fRunning) = 0 then
    fEvent.SetEvent; // all threads finished: release WaitFor method
end;

procedure TBrcMain.WaitFor;
begin
  fEvent.WaitForEver;
end;

procedure AddTemp(w: TTextWriter; sep: AnsiChar; val: PtrInt);
var
  d10: PtrInt;
begin
  w.Add(sep);
  if val < 0 then
  begin
    w.Add('-');
    val := -val;
  end;
  d10 := val div 10; // val as temperature * 10
  w.AddString(SmallUInt32Utf8[d10]); // in 0..999 range
  w.Add('.');
  w.Add(AnsiChar(val - d10 * 10 + ord('0')));
end;

function ByStationName(const A, B): integer; // = StrComp() but ending with ';'
var
  pa, pb: PByte;
  c: byte;
begin
  result := 0;
  pa := pointer(A);
  pb := pointer(B);
  dec(pa, {%H-}PtrUInt(pb));
  if pa = nil then
    exit;
  repeat
    c := PByteArray(pa)[{%H-}PtrUInt(pb)];
    if c <> pb^ then
      break
    else if c = ord(';') then
      exit; // Str1 = Str2
    inc(pb);
  until false;
  if (c = ord(';')) or
     ((pb^ <> ord(';')) and
      (c < pb^)) then
    result := -1
  else
    result := 1;
end;

function ceil(x: double): PtrInt; // "official" rounding method
begin
  result := trunc(x) + ord(frac(x) > 0);  // using FPU is fast enough here
end;

function TBrcMain.SortedText: RawUtf8;
var
  c: PtrInt;
  n: PCardinal;
  s: PBrcStation;
  p: PUtf8Char;
  st: TRawByteStringStream;
  w: TTextWriter;
  ndx: TSynTempBuffer;
  tmp: TTextWriterStackBuffer;
begin
  // compute the sorted-by-name indexes of all stations
  c := fList.Count;
  assert(c <> 0);
  DynArraySortIndexed(
    pointer(fList.StationName), SizeOf(PUtf8Char), c, ndx, ByStationName);
  try
    // generate output
    FastSetString(result, nil, 1200000); // pre-allocate result
    st := TRawByteStringStream.Create(result);
    try
      w := TTextWriter.Create(st, @tmp, SizeOf(tmp));
      try
        w.Add('{');
        n := ndx.buf;
        repeat
          s := @fList.Station[n^];
          assert(s^.Count <> 0);
          p := fList.StationName[n^];
          w.AddNoJsonEscape(p, NameEnd(p) - p);
          AddTemp(w, '=', s^.Min);
          AddTemp(w, '/', ceil(s^.Sum / s^.Count)); // average
          AddTemp(w, '/', s^.Max);
          dec(c);
          if c = 0 then
            break;
          w.Add(',', ' ');
          inc(n);
        until false;
        w.Add('}');
        w.FlushFinal;
        FakeLength(result, w.WrittenBytes);
      finally
        w.Free;
      end;
    finally
      st.Free;
    end;
  finally
    ndx.Done;
  end;
end;

var
  fn: TFileName;
  threads, chunkmb: integer;
  verbose, affinity, help: boolean;
  main: TBrcMain;
  res: RawUtf8;
  start, stop: Int64;
begin
  assert(SizeOf(TBrcStation) = 64 div 4); // 64 = CPU L1 cache line size
  // read command line parameters
  Executable.Command.ExeDescription := 'The mORMot One Billion Row Challenge';
  if Executable.Command.Arg(0, 'the data source #filename') then
    Utf8ToFileName(Executable.Command.Args[0], fn{%H-});
  verbose := Executable.Command.Option(
    ['v', 'verbose'], 'generate verbose output with timing');
  affinity := Executable.Command.Option(
    ['a', 'affinity'], 'force thread affinity to a single CPU core');
  Executable.Command.Get(
    ['t', 'threads'], threads, '#number of threads to run',
      SystemInfo.dwNumberOfProcessors);
  Executable.Command.Get(
    ['c', 'chunk'], chunkmb, 'size in #megabytes used for per-thread chunking', 16);
  help := Executable.Command.Option(['h', 'help'], 'display this help');
  if Executable.Command.ConsoleWriteUnknown then
    exit
  else if help or
     (fn = '') then
  begin
    ConsoleWrite(Executable.Command.FullDescription);
    exit;
  end;
  // actual process
  if verbose then
    ConsoleWrite(['Processing ', fn, ' with ', threads, ' threads, ',
      chunkmb, 'MB chunks and affinity=', affinity]);
  QueryPerformanceMicroSeconds(start);
  try
    main := TBrcMain.Create(fn, threads, chunkmb, {max=}45000, affinity);
    // note: current stations count = 41343 for 2.5MB of data per thread
    try
      main.WaitFor;
      res := main.SortedText;
      if verbose then
        ConsoleWrite(['result hash=',      CardinalToHexShort(crc32cHash(res)),
                      ', result length=',  length(res),
                      ', stations count=', main.fList.Count,
                      ', valid utf8=',     IsValidUtf8(res)])
      else
        ConsoleWrite(res);
    finally
      main.Free;
    end;
  except
    on E: Exception do
      ConsoleShowFatalException(E);
  end;
  // optional timing output
  if verbose then
  begin
    QueryPerformanceMicroSeconds(stop);
    dec(stop, start);
    ConsoleWrite(['done in ', MicroSecToString(stop), ' ',
      KB((FileSize(fn) * 1000000) div stop), '/s']);
  end;
end.

