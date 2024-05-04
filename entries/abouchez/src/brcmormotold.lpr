/// MIT code (c) Arnaud Bouchez, using the mORMot 2 framework
// - initial version, with full name comparison
program brcmormotold;

{$define CUSTOMHASH}
// a dedicated hash table is 40% faster than mORMot generic TDynArrayHashed

{$define CUSTOMASM}
// about 10% faster with some dedicated asm instead of mORMot code on x86_64

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
  // a weather station info, using a whole CPU L1 cache line (64 bytes)
  TBrcStation = packed record
    NameLen: byte; // name as first "shortstring" field for TDynArray
    NameText: array[1 .. 64 - 1 - 2 * 4 - 2 * 2] of byte; // maxlen = 51
    Sum, Count: integer;  // we ensured no overflow occurs with 32-bit range
    Min, Max: SmallInt;   // 16-bit (-32767..+32768) temperatures * 10
  end;
  PBrcStation = ^TBrcStation;
  TBrcStationDynArray = array of TBrcStation;

  TBrcStations = array[word] of TBrcStation;
  PBrcStations = ^TBrcStations;

  TBrcList = record
  public
    Count: integer;
    {$ifdef CUSTOMHASH}
    Station: PBrcStations; // perfectly aligned to 64 bytes from StationMem[]
    StationHash: array of word; // store 0 if void, or Station[] index + 1
    StationMem: TBrcStationDynArray;
    function Search(name: pointer; namelen: PtrInt): PBrcStation;
    {$else}
    Station: TBrcStationDynArray;
    Stations: TDynArrayHashed;
    function Search(name: PByteArray): PBrcStation;
    {$endif CUSTOMHASH}
    procedure Init(max: integer; align: boolean);
  end;

  TBrcMain = class
  protected
    fSafe: TLightLock;
    fEvent: TSynEvent;
    fRunning, fMax: integer;
    fCurrentChunk: PByteArray;
    fCurrentRemain: PtrUInt;
    fList: TBrcList;
    fMem: TMemoryMap;
    procedure Aggregate(const another: TBrcList);
    function GetChunk(out start, stop: PByteArray): boolean;
  public
    constructor Create(const fn: TFileName; threads, max: integer;
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


{ TBrcList }

{$ifdef CUSTOMHASH}

{$ifndef OSLINUXX64}
  {$undef CUSTOMASM} // asm below is for FPC + Linux x86_64 only
{$endif OSLINUXX64}

const
  HASHSIZE = 1 shl 18; // slightly oversized to avoid most collisions

procedure TBrcList.Init(max: integer; align: boolean);
begin
  assert(max <= high(StationHash[0]));
  SetLength(StationMem, max); // RTL won't align by 64 bytes
  Station := pointer(StationMem);
  if align then
    while {%H-}PtrUInt(Station) and 63 <> 0 do // manual alignment
      inc(PByte(Station));
  SetLength(StationHash, HASHSIZE);
end;

{$ifdef CUSTOMASM}

function dohash(buf: PAnsiChar; len: cardinal): PtrUInt; nostackframe; assembler;
asm
        xor     eax, eax // it is enough to hash up to 15 bytes for our purpose
        mov     ecx, len
        cmp     len, 8
        jb      @less8
        crc32   rax, qword ptr [buf]
        add     buf, 8
@less8: test    cl, 4
        jz      @less4
        crc32   eax, dword ptr [buf]
        add     buf, 4
@less4: test    cl, 2
        jz      @less2
        crc32   eax, word ptr [buf]
        add     buf, 2
@less2: test    cl, 1
        jz      @z
        crc32   eax, byte ptr [buf]
@z:
end;

function CompareMem(a, b: pointer; len: PtrInt): boolean; nostackframe; assembler;
asm
        add     a, len
        add     b, len
        neg     len
        cmp     len, -8
        ja      @less8
        align   8
@by8:   mov     rax, qword ptr [a + len]
        cmp     rax, qword ptr [b + len]
        jne     @diff
        add     len, 8
        jz      @eq
        cmp     len, -8
        jna     @by8
@less8: cmp     len, -4
        ja      @less4
        mov     eax, dword ptr [a + len]
        cmp     eax, dword ptr [b + len]
        jne     @diff
        add     len, 4
        jz      @eq
@less4: cmp     len, -2
        ja      @less2
        movzx   eax, word ptr [a + len]
        movzx   ecx, word ptr [b + len]
        cmp     eax, ecx
        jne     @diff
        add     len, 2
@less2: test    len, len
        jz      @eq
        mov     al, byte ptr [a + len]
        cmp     al, byte ptr [b + len]
        je      @eq
@diff:  xor     eax, eax
        ret
@eq:    mov     eax, 1 // = found (most common case of no hash collision)
end;

{$else}

function dohash(buf: PAnsiChar; len: cardinal): PtrUInt; inline;
begin
  if len > 16 then
    len := 16; // it is enough to hash up to 16 bytes for our purpose
  result := DefaultHasher(0, buf, len); // fast mORMot asm hasher (crc32c)
end;

{$endif CUSTOMASM}

function TBrcList.Search(name: pointer; namelen: PtrInt): PBrcStation;
var
  h, x: PtrUInt;
begin
  assert(namelen <= SizeOf(TBrcStation.NameText));
  h := dohash(name, namelen);
  repeat
    h := h and (HASHSIZE - 1);
    x := StationHash[h];
    if x = 0 then
      break; // void slot
    result := @Station[x - 1];
    if (result^.NameLen = namelen) and
       CompareMem(@result^.NameText, name, namelen) then
      exit; // found
    inc(h); // hash collision: try next slot
  until false;
  result := @Station[Count];
  inc(Count);
  StationHash[h] := Count;
  result^.NameLen := namelen;
  MoveFast(name^, result^.NameText, namelen);
  result^.Min := high(result^.Min);
  result^.Max := low(result^.Max);
end;

{$else}

function StationHash(const Item; Hasher: THasher): cardinal;
var
  s: TBrcStation absolute Item; // s.Name should be the first field
begin
  result := Hasher(0, @s.NameText, s.NameLen);
end;

function StationComp(const A, B): integer;
var
  sa: TBrcStation absolute A;
  sb: TBrcStation absolute B;
begin
  result := MemCmp(@sa.NameLen, @sb.NameLen, sa.NameLen + 1);
end;

procedure TBrcList.Init(max: integer; align: boolean);
begin
  // align is just ignored, because TDynArray requires natural alignment
  Stations.Init(
    TypeInfo(TBrcStationDynArray), Station, @StationHash, @StationComp, nil, @Count);
  Stations.Capacity := max;
end;

function TBrcList.Search(name: PByteArray): PBrcStation;
var
  i: PtrUInt;
  added: boolean;
begin
  assert(name^[0] < SizeOf(TBrcStation.NameText));
  i := Stations.FindHashedForAdding(name^, added);
  result := @Station[i]; // in two steps (Station[] may be reallocated)
  if not added then
    exit;
  MoveFast(name^, result^.NameLen, name^[0] + 1);
  result^.Min := high(result^.Min);
  result^.Max := low(result^.Max);
end;

{$endif CUSTOMHASH}


{ TBrcThread }

constructor TBrcThread.Create(owner: TBrcMain);
begin
  fOwner := owner;
  FreeOnTerminate := true;
  fList.Init(fOwner.fMax, {align=}true);
  InterlockedIncrement(fOwner.fRunning);
  inherited Create({suspended=}false);
end;

procedure TBrcThread.Execute;
var
  p, start, stop: PByteArray;
  v, m: integer;
  l, neg: PtrInt;
  s: PBrcStation;
  {$ifndef CUSTOMHASH}
  c: byte;
  name: array[0..63] of byte; // efficient map of a temp shortstring on FPC
  {$endif CUSTOMHASH}
begin
  while fOwner.GetChunk(start, stop) do
  begin
    // parse this thread chunk
    p := start;
    repeat
      // parse the name;
      l := 2;
      {$ifdef CUSTOMHASH}
      start := p;
      while p[l] <> ord(';') do
        inc(l); // small local loop is faster than SSE2 ByteScanIndex()
      {$else}
      repeat
        c := p[l];
        if c = ord(';') then
          break;
        inc(l);
        name[l] := c; // fill name[] as a shortstring
      until false;
      name[0] := l;
      {$endif CUSTOMHASH}
      p := @p[l + 1]; // + 1 to ignore ;
      // parse the temperature (as -12.3 -3.4 5.6 78.9 patterns) into value * 10
      if p[0] = ord('-') then
      begin
        neg := -1;
        p := @p[1];
      end
      else
        neg := 1;
      if p[2] = ord('.') then // xx.x
      begin
        // note: the PCardinal(p)^ + "shr and $ff" trick is actually slower
        v := (p[0] * 100 + p[1] * 10 + p[3] - (ord('0') * 111)) * neg;
        p := @p[5]; // also jump ending $10
      end
      else
      begin
        v := (p[0] * 10 + p[2] - (ord('0') * 11)) * neg; // x.x
        p := @p[4];
      end;
      // store the value
      {$ifdef CUSTOMHASH}
      s := fList.Search(start, l);
      {$else}
      s := fList.Search(@name);
      {$endif CUSTOMHASH}
      inc(s^.Sum, v);
      inc(s^.Count);
      m := s^.Min;
      if v < m then
        m := v; // branchless cmovl
      s^.Min := m;
      m := s^.Max;
      if v > m then
        m := v;
      s^.Max := m;
    until p >= stop;
  end;
  // aggregate this thread values into the main list
  fOwner.Aggregate(fList);
end;


{ TBrcMain }

constructor TBrcMain.Create(const fn: TFileName; threads, max: integer;
  affinity: boolean);
var
  i, cores, core: integer;
  one: TBrcThread;
begin
  fEvent := TSynEvent.Create;
  if not fMem.Map(fn) then
    raise ESynException.CreateUtf8('Impossible to find %', [fn]);
  fMax := max;
  fList.Init(fMax, {align=}false); // not aligned for TDynArray.Sort to work
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
  fEvent.Free;
end;

const
  CHUNKSIZE = 64 shl 20; // fed each TBrcThread with 64MB chunks
  // it is faster than naive parallel process of size / threads input because
  // OS thread scheduling is never fair so some threads will finish sooner

function TBrcMain.GetChunk(out start, stop: PByteArray): boolean;
var
  chunk: PtrUInt;
begin
  result := false;
  fSafe.Lock;
  chunk := fCurrentRemain;
  if chunk <> 0 then
  begin
    start := fCurrentChunk;
    if chunk > CHUNKSIZE then
    begin
      stop := pointer(GotoNextLine(pointer(@start[CHUNKSIZE])));
      chunk := PAnsiChar(stop) - PAnsiChar(start);
    end
    else
    begin
      stop := @start[chunk];
      while PAnsiChar(stop)[-1] <= ' ' do
        dec(PByte(stop)); // ensure final stop at meaningful char
    end;
    dec(fCurrentRemain, chunk);
    fCurrentChunk := @fCurrentChunk[chunk];
    result := true;
  end;
  fSafe.UnLock;
end;

procedure TBrcMain.Aggregate(const another: TBrcList);
var
  s, d: PBrcStation;
  n: integer;
begin
  fSafe.Lock; // several TBrcThread may finish at the same time
  n := another.Count;
  s := pointer(another.Station);
  repeat
    {$ifdef CUSTOMHASH}
    d := fList.Search(@s^.NameText, s^.NameLen);
    {$else}
    d := fList.Search(@s^.NameLen);
    {$endif CUSTOMHASH}
    inc(d^.Count, s^.Count);
    inc(d^.Sum, s^.Sum);
    if s^.Max > d^.Max then
      d^.Max := s^.Max;
    if s^.Min < d^.Min then
      d^.Min := s^.Min;
    inc(s);
    dec(n);
  until n = 0;
  fSafe.UnLock;
  if InterlockedDecrement(fRunning) = 0 then
    fEvent.SetEvent; // all threads finished: release main console thread
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

function ByStationName(const A, B): integer;
var
  sa: TBrcStation absolute A;
  sb: TBrcStation absolute B;
  la, lb: PtrInt;
begin
  la := sa.NameLen;
  lb := sb.NameLen;
  if la < lb then
    la := lb;
  result := MemCmp(@sa.NameText, @sb.NameText, la);
  if result = 0 then
    result := sa.NameLen - sb.NameLen;
end;

function ceil(x: double): PtrInt; // "official" rounding method
begin
  result := trunc(x) + ord(frac(x) > 0);  // using FPU is fast enough here
end;

function TBrcMain.SortedText: RawUtf8;
var
  n: integer;
  s: PBrcStation;
  st: TRawByteStringStream;
  w: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  {$ifdef CUSTOMHASH}
  DynArrayFakeLength(fList.Station, fList.Count);
  DynArray(TypeInfo(TBrcStationDynArray), fList.Station).Sort(ByStationName);
  {$else}
  fList.Stations.Sort(ByStationName);
  {$endif CUSTOMHASH}
  FastSetString(result, nil, 1200000); // pre-allocate result
  st := TRawByteStringStream.Create(result);
  try
    w := TTextWriter.Create(st, @tmp, SizeOf(tmp));
    try
      w.Add('{');
      s := pointer(fList.Station);
      n := fList.Count;
      if n > 0 then
        repeat
          assert(s^.Count <> 0);
          w.AddNoJsonEscape(@s^.NameText, s^.NameLen);
          AddTemp(w, '=', s^.Min);
          AddTemp(w, '/', ceil(s^.Sum / s^.Count)); // average
          AddTemp(w, '/', s^.Max);
          dec(n);
          if n = 0 then
            break;
          w.Add(',', ' ');
          inc(s);
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
end;

var
  fn: TFileName;
  threads: integer;
  verbose, affinity, help: boolean;
  main: TBrcMain;
  res: RawUtf8;
  start, stop: Int64;
begin
  assert(SizeOf(TBrcStation) = 64); // 64 bytes = CPU L1 cache line size
  // read command line parameters
  Executable.Command.ExeDescription := 'The mORMot One Billion Row Challenge';
  if Executable.Command.Arg(0, 'the data source #filename') then
    Utf8ToFileName(Executable.Command.Args[0], fn);
  verbose := Executable.Command.Option(
    ['v', 'verbose'], 'generate verbose output with timing');
  affinity := Executable.Command.Option(
    ['a', 'affinity'], 'force thread affinity to a single CPU core');
  Executable.Command.Get(
    ['t', 'threads'], threads, '#number of threads to run', 16);
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
    ConsoleWrite(['Processing ', fn, ' with ', threads, ' threads',
                  ' and affinity=', BOOL_STR[affinity]]);
  QueryPerformanceMicroSeconds(start);
  try
    main := TBrcMain.Create(fn, threads, {max=}45000, affinity);
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

