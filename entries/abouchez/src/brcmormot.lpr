/// MIT code (c) Arnaud Bouchez, using the mORMot 2 framework
// - fastest version with "perfect hash" trick and optional name comparison
program brcmormot;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
{$endif OSWINDOWS}

uses
  mormot.core.fpcx64mm,
  cthreads,
  baseunix, // low-level fpmmap with MAP_POPULATE
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.data;

type
  // a weather station info, using less than 1/4rd of a CPU L1 cache line
  TBrcStation = packed record
    Count, Sum: integer;  // we ensured no overflow occurs with 32-bit range
    Min, Max: SmallInt;   // 16-bit (-32767..+32768) temperatures * 10
  end;
  PBrcStation = ^TBrcStation;
  TBrcStations = array of TBrcStation;

  // state machine used for efficient per-thread line processing
  TBrcChunk = packed record
    NameHash: cardinal;
    Value: integer;
    Name, Start, Stop, MemMapBuf: PUtf8Char;
    MemMapSize: PtrInt;
    NameLen: byte;
  end;

  // main processing class, orchestrating all TBrcThread instances
  TBrcMain = class
  protected
    fSafe: TOsLightLock;
    fEvent: TSynEvent;
    fHashLookup: array of word;     // store 0 if void, or fStation[] index + 1
    fNameHash: array of cardinal;   // quick "perfect hash" search
    fNameLine: array of string[63]; // local copy of a whole line
    fStation: TBrcStations;         // aggregated list of result
    fCount: PtrInt;
    fRunning, fMax, fChunkSize, fCurrentChunk: integer;
    fFile: THandle;
    fFileSize: Int64;
    function StationSearch(var chunk: TBrcChunk): PtrInt; inline;
    function StationAdd(h: PtrUInt; var chunk: TBrcChunk): PtrInt;
    procedure Aggregate(const another: TBrcStations);
    function GetNext(var next: TBrcChunk): boolean;
  public
    constructor Create(const fn: TFileName; threads, chunkmb, max: integer;
      affinity, fullsearch: boolean);
    destructor Destroy; override;
    procedure WaitFor;
    function SortedText: RawUtf8;
  end;

  // per-thread execution
  TBrcThread = class(TThread)
  protected
    fOwner: TBrcMain;
    fStation: TBrcStations; // each thread work on its own resultset
    procedure Execute; override;
  public
    constructor Create(owner: TBrcMain);
  end;


{$ifdef OSLINUXX64}

procedure ParseLine(var chunk: TBrcChunk); nostackframe; assembler;
asm
         // 128-bit SSE2 ';" search with SSE4.2 crc32c hash
         mov      rsi, [rdi + TBrcChunk.Start]
         mov      r8, rsi
         xor      edx, edx
         movaps   xmm0, oword ptr [rip + @pattern]
         movups   xmm1, oword ptr [rsi]      // search in first 16 bytes
         pcmpeqb  xmm1, xmm0
         mov      [rdi + TBrcChunk.Name], rsi
         pmovmskb ecx, xmm1
         bsf      ecx, ecx                   // ecx = position
         jz       @by16
         lea      rax, [rsi + rcx]           // rax = found
         test     cl, 8
         jz       @less8
         crc32    rdx, qword ptr [rsi]       // branchless for 8..15 bytes
@ok8:    crc32    rdx, qword ptr [rax - 8]   // may overlap
@ok:     mov      rcx, rax
         sub      rcx, r8
         mov      [rdi + TBrcChunk.NameHash], edx
         mov      [rdi + TBrcChunk.NameLen], cl
         // branchless temperature parsing - same algorithm as pascal code below
         xor      ecx, ecx
         xor      edx, edx
         cmp      byte ptr [rax + 1], '-'
         setne    cl
         sete     dl
         lea      rsi, [rcx + rcx - 1]      // rsi = +1 or -1
         lea      r8, [rax + rdx]
         cmp      byte ptr [rax + rdx + 2], '.'
         sete     cl
         setne    dl
         mov      eax, dword ptr [r8 + 1]   // eax = xx.x or x.x
         shl      cl, 3
         lea      r8, [r8 + rdx + 5]        // r8 = next line (LF only)
         shl      eax, cl                   // normalized as xx.x
         and      eax, $0f000f0f            // from ascii to digit
         imul     rax, rax, 1 + 10 shl 16 + 100 shl 24
         shr      rax, 24                   // value is computed in high bits
         and      eax, 1023                 // truncate to 3 digits (0..999)
         imul     eax, esi                  // apply sign
         mov      [rdi + TBrcChunk.Value], eax
         mov      [rdi + TBrcChunk.Start], r8
         ret
@by16:   crc32    rdx, qword ptr [rsi]      // hash 16 bytes
         crc32    rdx, qword ptr [rsi + 8]
         jmp      @nxt16
@less8:  test     cl, 4
         jz       @less4
         crc32    edx, dword ptr [rsi]      // 4..7 bytes
         crc32    edx, dword ptr [rax - 4]  // may overlap
         jmp      @ok
@less4:  crc32    edx, word ptr [rsi]       // 2..3 bytes
         crc32    edx, word ptr [rax - 2]   // may overlap
         jmp      @ok
         align    16
@nxt16:  add      rsi,  16
         movups   xmm1, oword ptr [rsi]     // search in next 16 bytes
         pcmpeqb  xmm1, xmm0
         pmovmskb ecx,  xmm1
         bsf      ecx,  ecx
         jz       @nxt16
         lea      rax, [rsi + rcx]
         jmp      @ok8
         align    16
@pattern:dq       ';;;;;;;;'
         dq       ';;;;;;;;'
end;

function CompareMem(a, b: PAnsiChar; l: PtrInt): boolean; nostackframe; assembler;
asm
        // rdi=a rsi=b rdx=l
        mov     rax, -8                    // rax=-SizeOf(PtrInt)
        add     rdi, rdx
        add     rsi, rdx
        neg     rdx
        cmp     rax, rdx
        jl      @by1
        align   8
@by8:   mov     rcx, qword ptr [rdi + rdx] // branchless for 8..16 bytes
        cmp     rcx, qword ptr [rsi + rdx]
        jne     @set
        sub     rdx, rax
        jz      @ok
        cmp     rax, rdx
        jg      @by8
        mov     rcx, qword ptr [rdi + rax] // compare last 8 bytes - may overlap
        cmp     rcx, qword ptr [rsi + rax]
@set:   sete    al
        ret
@by1:   mov     cl, byte ptr [rdi+rdx]
        cmp     cl, byte ptr [rsi+rdx]
        jnz     @set
        add     rdx, 1
        jnz     @by1
@ok:    mov     al, 1
end;

{$else}

procedure ParseLine(var chunk: TBrcChunk); inline;
var
  p: PUtf8Char;
  neg, len: PtrInt;
begin
  // parse and hash the station name
  p := chunk.Start;
  chunk.Name := p;
  inc(p, 2);
  while p^ <> ';' do
    inc(p);
  len := p - chunk.Name;
  chunk.NameLen := len;
  chunk.NameHash := crc32c(0, chunk.Name, len); // intel/aarch64 asm
  // branchless parsing of the temperature
  neg := ord(p[1] <> '-') * 2 - 1;         // neg = +1 or -1
  inc(p, ord(p[1] = '-'));                 // ignore '-' sign
  chunk.Start := @p[ord(p[2] <> '.') + 5]; // next line (LF only)
  chunk.Value := PtrInt(cardinal((QWord((PCardinal(p + 1)^ shl
                   (byte(ord(p[2] = '.') shl 3))) and $0f000f0f) *
         (1 + 10 shl 16 + 100 shl 24)) shr 24) and cardinal(1023)) * neg;
end;

function CompareMem(a, b: PAnsiChar; l: PtrInt): boolean;
var
  ptrsiz: PtrInt;
begin
  ptrsiz := -SizeOf(PtrInt); // FPC will use a register for this constant
  inc(a, l);
  inc(b, l);
  l := -l;
  result := true;
  if l <= ptrsiz then
    repeat
      if PPtrUInt(@a[l])^ <> PPtrUInt(@b[l])^ then
        break;
      dec(l, ptrsiz);
      if l = 0 then
        exit
      else if l < ptrsiz then
        continue;
      result := PPtrUInt(@a[ptrsiz])^ = PPtrUInt(@b[ptrsiz])^; // may overlap
      exit;
    until false
  else
    repeat
      if a[l] <> b[l] then
        break;
      inc(l);
      if l <> 0 then
        continue;
      result := true;
      exit;
    until false;
  result := false;
end;

{$endif OSLINUXX64}



{ TBrcThread }

constructor TBrcThread.Create(owner: TBrcMain);
begin
  fOwner := owner;
  FreeOnTerminate := true;
  SetLength(fStation, fOwner.fMax);
  InterlockedIncrement(fOwner.fRunning);
  inherited Create({suspended=}false, {stacksize=}16384);
end;


{ TBrcMain }

const
  HASHSIZE = 1 shl 18; // slightly oversized to avoid most collisions
  // we tried with a prime constant for fast modulo mult-by-reciprocal: slower

constructor TBrcMain.Create(const fn: TFileName; threads, chunkmb, max: integer;
  affinity, fullsearch: boolean);
var
  i, cores, core: integer;
  one: TBrcThread;
begin
  // initialize the processing
  fSafe.Init;
  fEvent := TSynEvent.Create;
  fFile := FileOpenSequentialRead(fn);
  fFileSize := FileSize(fFile);
  if fFileSize <= 0 then
    raise ESynException.CreateUtf8('Impossible to find %', [fn]);
  fMax := max;
  fChunkSize := chunkmb shl 20;
  SetLength(fHashLookup, HASHSIZE);
  if not fullsearch then
    SetLength(fNameHash, fMax);
  SetLength(fNameLine, fMax);
  // (we tried pre-loading a first chunk here but it was not faster)
  // run the thread workers
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
  fEvent.Free;
  fSafe.Done;
end;

function TBrcMain.StationSearch(var chunk: TBrcChunk): PtrInt;
var
  h: PtrUInt;
  p: PCardinalArray;
  n: PAnsiChar;
begin
  h := chunk.NameHash;
  repeat
    repeat
      h := h and (HASHSIZE - 1);
      result := fHashLookup[h];
      if result = 0 then
        break;   // void slot: call StationAdd()
      p := pointer(fNameHash);
      dec(result);
      if p <> nil then
        if p[result] = chunk.NameHash then // perfect hash match
          exit
        else
          inc(h) // hash modulo collision: linear probing
      else
      begin
        n := @fNameLine[result]; // full name comparison
        if (n[0] = AnsiChar(chunk.NameLen)) and
           CompareMem(@n[1], chunk.Name, ord(n[0])) then
          exit
        else
          inc(h);
      end;
    until false;
    result := StationAdd(h, chunk);
  until result >= 0; // added
end;

function TBrcMain.StationAdd(h: PtrUInt; var chunk: TBrcChunk): PtrInt;
begin
  fSafe.Lock;
  if fHashLookup[h] <> 0 then
    result := -1 // race condition (at startup): try again
  else
  begin
    result := fCount;
    inc(fCount);
    fHashLookup[h] := result + 1;
    if fNameHash <> nil then
      fNameHash[result] := chunk.NameHash;
    assert(result < fMax);
    assert(chunk.NameLen < SizeOf(fNameLine[0]));
    SetString(fNameLine[result], chunk.Name, chunk.NameLen);
  end;
  fSafe.UnLock;
end;

const
  // read-ahead on the file to avoid page faults - need Linux kernel > 2.5.46
  MAP_POPULATE = $08000;

function TBrcMain.GetNext(var next: TBrcChunk): boolean;
var
  chunk, page, pos: Int64;
begin
  result := false;
  if next.MemMapBuf <> nil then
    fpmunmap(next.MemMapBuf, next.MemMapSize);
  pos := Int64(InterlockedIncrement(fCurrentChunk) - 1) {%H-}* fChunkSize;
  chunk := fFileSize - pos;
  if chunk <= 0 then
    exit; // reached end of file
  if chunk > fChunkSize then
    chunk := fChunkSize;
  // we include the previous 4KB memory page to parse full lines
  page := SystemInfo.dwPageSize;
  if pos = 0 then
    page := 0;
  next.MemMapSize := chunk + page;
  next.MemMapBuf := fpmmap(nil, next.MemMapSize, PROT_READ,
    MAP_SHARED or MAP_POPULATE, fFile, pos - page);
  if next.MemMapBuf = nil then
    exit; // invalid file
  result  := true;
  next.Start := next.MemMapBuf + page;
  if page <> 0 then
    next.Start := GotoNextLine(next.Start - 64); // = previous next.Stop
  next.Stop := next.MemMapBuf + page + chunk;
  if chunk = fChunkSize then
    next.Stop := GotoNextLine(next.Stop - 64)    // = following next.Start
  else
    while next.Stop[-1] <= ' ' do                // until end of last chunk
      dec(next.Stop);
end;

procedure TBrcMain.Aggregate(const another: TBrcStations);
var
  n: integer;
  s, d: PBrcStation;
begin
  fSafe.Lock; // several TBrcThread may finish at the same time
  n := fCount;
  s := pointer(another);
  d := pointer(fStation);
  if d = nil then
    fStation := another
  else
  repeat
    if s^.Count <> 0 then
    begin
      inc(d^.Count, s^.Count);
      inc(d^.Sum, s^.Sum);
      if s^.Min < d^.Min then
        d^.Min := s^.Min;
      if s^.Max > d^.Max then
        d^.Max := s^.Max;
    end;
    inc(s);
    inc(d);
    dec(n);
  until n = 0;
  fSafe.UnLock;
  if InterlockedDecrement(fRunning) = 0 then
    fEvent.SetEvent; // all threads finished: release WaitFor method
end;

procedure SetStationValue(s: PBrcStation; v: integer); // better not inlined
var
  m: integer;
begin
  if s^.Count <> 0 then
  begin
    inc(s^.Count);
    inc(s^.Sum, v);
    m := s^.Min;
    if v < m then   // cmovg
      m := v;
    s^.Min := m;
    m := s^.Max;
    if v > m then   // cmovl
      m := v;
    s^.Max := m;
    exit;
  end;
  s^.Count := 1;
  s^.Sum := v;
  s^.Min := v;
  s^.Max := v;
end;

procedure TBrcThread.Execute; // defined here for proper inlining
var
  chunk: TBrcChunk;
begin
  chunk.MemMapBuf := nil;
  while fOwner.GetNext(chunk) do
    repeat
      // parse next name;temp pattern into value * 10
      ParseLine(chunk);
      // store the value into the proper slot
      SetStationValue(@fStation[fOwner.StationSearch(chunk)], chunk.Value);
    until chunk.start >= chunk.stop;
  // aggregate this thread values into the main list
  fOwner.Aggregate(fStation);
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
  sa: shortstring absolute A;
  sb: shortstring absolute B;
  la, lb: PtrInt;
begin
  la := ord(sa[0]);
  lb := ord(sb[0]);
  if la < lb then
    la := lb;
  result := MemCmp(@sa[1], @sb[1], la);
  if result = 0 then
    result := ord(sa[0]) - ord(sb[0]);
end;

function ceil(x: double): PtrInt; // "official" rounding method
begin
  result := trunc(x) + ord(frac(x) > 0);  // using FPU is fast enough here
end;

function TBrcMain.SortedText: RawUtf8;
var
  c: integer;
  n: PCardinal;
  s: PBrcStation;
  st: TRawByteStringStream;
  w: TTextWriter;
  ndx: TSynTempBuffer;
  tmp: TTextWriterStackBuffer;
begin
  // compute the sorted-by-name indexes of all stations
  assert(fCount <> 0);
  DynArraySortIndexed(
    pointer(fNameLine), SizeOf(fNameLine[0]), fCount, ndx, ByStationName);
  try
    // generate output
    FastSetString(result, nil, 1200000); // pre-allocate result
    st := TRawByteStringStream.Create(result);
    try
      w := TTextWriter.Create(st, @tmp, SizeOf(tmp));
      try
        w.Add('{');
        c := fCount;
        n := ndx.buf;
        repeat
          w.AddShort(fNameLine[n^]);
          s := @fStation[n^];
          assert(s^.Count <> 0);
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
  verbose, affinity, full, help: boolean;
  main: TBrcMain;
  res: RawUtf8;
  start, stop: Int64;
begin
  assert(SizeOf(TBrcStation) <= 64 div 4); // 64 = CPU L1 cache line size
  // read command line parameters
  with Executable.Command do
  begin
    ExeDescription := 'The mORMot One Billion Row Challenge';
    if Arg(0, 'the data source #filename') then
      Utf8ToFileName(Executable.Command.Args[0], fn{%H-});
    verbose := Option(['v', 'verbose'], 'generate verbose output with timing');
    affinity := Option(['a', 'affinity'], 'force thread affinity to a single CPU core');
    full := Option(['f', 'full'], 'force full name lookup (disable "perfect hash" trick)');
    Get(['t', 'threads'], threads, '#number of thread workers', SystemInfo.dwNumberOfProcessors);
    Get(['c', 'chunk'], chunkmb, 'size in #megabytes for per-thread chunking', 16);
    help := Option(['h', 'help'], 'display this help');
    if ConsoleWriteUnknown then
      exit
    else if help or
       (fn = '') then
    begin
      ConsoleWrite(FullDescription);
      exit;
    end;
  end;
  // actual process
  if verbose then
    ConsoleWrite(['Processing ', fn, ' with ', threads, ' threads, ',
      chunkmb, 'MB chunks and affinity=', affinity, ' full=', full]);
  QueryPerformanceMicroSeconds(start);
  try
    main := TBrcMain.Create(fn, threads, chunkmb, {max=}45000, affinity, full);
    // note: current stations count = 41343 for 2.5MB of data per thread
    try
      main.WaitFor;
      res := main.SortedText;
      if verbose then
        ConsoleWrite(['hash=',      CardinalToHexShort(crc32cHash(res)),
                      ', length=',  length(res),
                      ', stations count=', main.fCount,
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

