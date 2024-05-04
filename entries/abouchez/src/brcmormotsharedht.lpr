/// MIT code (c) Arnaud Bouchez, using the mORMot 2 framework
// - shared hash table version, potentially with "perfect hash" trick
program brcmormotsharedht;

{.$define NOPERFECTHASH}
// you can define this conditional to force name comparison (slower)

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
  // a weather station info, using 12 bytes
  TBrcStation = packed record
    Sum, Count: integer;  // we ensured no overflow occurs with 32-bit range
    Min, Max: SmallInt;   // 16-bit (-32767..+32768) temperatures * 10
  end;
  PBrcStation = ^TBrcStation;
  TBrcStations = array of TBrcStation;

  TBrcMain = class
  protected
    fSafe: TOSLightLock;           // TLightLock seems to make no difference
    fHash: array of word;          // store 0 if void, or fStation[] index + 1
    fNameHash: array of cardinal;  // crc32c perfect hash of the name
    fNameText: array of PUtf8Char; // directly point to the input memmaped file
    fStation: TBrcStations;        // aggregated storage
    fCount: PtrInt;                // = length(fNameHash/fNameText/fStation)
    fEvent: TSynEvent;             // to wake up the main thread when finished
    fRunning, fCapacity: integer;
    fCurrentChunk: PByteArray;
    fCurrentRemain, fChunkSize: PtrUInt;
    fMem: TMemoryMap;
    function GetChunk(out start, stop: PByteArray): boolean;
    function Search(name: pointer; namelen: PtrUInt): PtrInt;
    procedure Aggregate(const another: TBrcStations);
  public
    constructor Create(const datafile: TFileName;
      threads, chunkmb, capacity: integer; affinity: boolean);
    destructor Destroy; override;
    procedure WaitFor;
    function SortedText: RawUtf8;
  end;

  TBrcThread = class(TThread)
  protected
    fOwner: TBrcMain;
    fStation: TBrcStations; // per-thread storage
    procedure Execute; override;
  public
    constructor Create(owner: TBrcMain);
  end;


{$ifdef FPC_CPUX64_disabled_slower}
function NameLen(p: PUtf8Char): PtrInt; assembler; nostackframe;
asm
         lea      rdx,  qword ptr [p + 2]
         movaps   xmm0, oword ptr [rip + @chr]
         movups   xmm1, oword ptr [rdx] // check first 16 bytes
         pcmpeqb  xmm1, xmm0
         pmovmskb eax,  xmm1
         bsf      eax,  eax
         jnz      @found
@by16:   add      rdx,  16
         movups   xmm1, oword ptr [rdx] // next 16 bytes
         pcmpeqb  xmm1, xmm0
         pmovmskb eax,  xmm1
         bsf      eax,  eax
         jz       @by16
@found:  add      rax,  rdx  // point to exact match
         sub      rax,  p    // return position
         ret
         align    16
@chr:    dq $3b3b3b3b3b3b3b3b // xmm0 of ';'
         dq $3b3b3b3b3b3b3b3b
end;
{$else}
function NameLen(p: PUtf8Char): PtrInt; inline;
begin
  result := 2;
  while true do
    if p[result] <> ';' then
      if p[result + 1] <> ';' then
        if p[result + 2] <> ';' then
          if p[result + 3] <> ';' then
            if p[result + 4] <> ';' then
              if p[result + 5] <> ';' then
                inc(result, 6)
              else
                exit(result + 5)
            else
              exit(result + 4)
          else
            exit(result + 3)
        else
          exit(result + 2)
      else
        exit(result + 1)
    else
      exit;
  // this small (unrolled) inlined loop is faster than a SSE2 function :)
end;
{$endif FPC_CPUX64}


{ TBrcMain }

const
  HASHSIZE = 1 shl 19; // slightly oversized to avoid most collisions

constructor TBrcMain.Create(const datafile: TFileName;
  threads, chunkmb, capacity: integer; affinity: boolean);
var
  i, cores, core: integer;
  one: TBrcThread;
begin
  // init thread-safety markers
  fSafe.Init;
  fEvent := TSynEvent.Create;
  // map the file into memory and prepare memory chunks
  if not fMem.Map(datafile) then
    raise ESynException.CreateUtf8('Impossible to find %', [datafile]);
  fChunkSize := chunkmb shl 20;
  fCurrentChunk := pointer(fMem.Buffer);
  fCurrentRemain := fMem.Size;
  // initialize the stations name hash table
  fCapacity := capacity;
  SetLength(fHash, HASHSIZE);
  SetLength(fNameHash, capacity);
  SetLength(fNameText, capacity);
  // launch the threads with optional thread affinity
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
  fSafe.Done;
end;

function TBrcMain.Search(name: pointer; namelen: PtrUInt): PtrInt;
var
  h32: cardinal;
  h: PtrUInt;
begin
  h32 := crc32c(0, name, namelen);
  h := h32;
  repeat
    repeat
      h := h and (HASHSIZE - 1);
      result := fHash[h];
      if result = 0 then
        break;  // void slot
      dec(result);
      if fNameHash[result] = h32 then
        {$ifdef NOPERFECTHASH}
        if MemCmp(pointer(fNameText[result]), name, namelen + 1) = 0 then
        {$endif NOPERFECTHASH}
          exit; // found this perfect hash = found this name
      inc(h);   // hash modulo collision: linear probing
    until false;
    // void slot: try to add now
    fSafe.Lock;
    if fHash[h] <> 0 then
      result := -1 // race condition
    else
    begin
      result := fCount;
      assert(result < fCapacity);
      inc(fCount);
      fNameHash[result] := h32;
      fNameText[result] := name;
      fHash[h] := fCount; // should be last
    end;
    fSafe.UnLock;
  until result >= 0;
end;

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
    if chunk > fChunkSize then
    begin
      stop := pointer(GotoNextLine(pointer(@start[fChunkSize])));
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

procedure TBrcMain.Aggregate(const another: TBrcStations);
var
  n: integer;
  s, d: PBrcStation;
begin
  fSafe.Lock; // several TBrcThread may finish at the same time
  if fStation = nil then
    fStation := another
  else
  begin
    n := fCount;
    s := pointer(another);
    d := pointer(fStation);
    repeat
      if s^.Count <> 0 then
      begin
        inc(d^.Count, s^.Count);
        inc(d^.Sum, s^.Sum);
        if s^.Max > d^.Max then
          d^.Max := s^.Max;
        if s^.Min < d^.Min then
          d^.Min := s^.Min;
      end;
      inc(s);
      inc(d);
      dec(n);
    until n = 0;
  end;
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
  c := fCount;
  assert(c <> 0);
  DynArraySortIndexed(
    pointer(fNameText), SizeOf(PUtf8Char), c, ndx, ByStationName);
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
          s := @fStation[n^];
          assert(s^.Count <> 0);
          p := fNameText[n^];
          w.AddNoJsonEscape(p, NameLen(p));
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


{ TBrcThread }

constructor TBrcThread.Create(owner: TBrcMain);
begin
  fOwner := owner;
  SetLength(fStation, fOwner.fCapacity);
  InterlockedIncrement(fOwner.fRunning);
  FreeOnTerminate := true;
  inherited Create({suspended=}false);
end;

procedure TBrcThread.Execute;
var
  p, start, stop: PByteArray;
  v, m: integer;
  l, neg: PtrInt;
  s: PBrcStation;
begin
  while fOwner.GetChunk(start, stop) do
  begin
    // parse this thread chunk
    p := start;
    repeat
      // parse the name; and find the corresponding station data
      l := NameLen(pointer(p));
      p := @p[l + 1]; // + 1 to ignore ;
      s := @fStation[fOwner.Search(start, l)];
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
        p := @p[5]; // also jump ending $13/$10
      end
      else
      begin
        v := (p[0] * 10 + p[2] - (ord('0') * 11)) * neg; // x.x
        p := @p[4];
      end;
      // store the value
      if s^.Count = 0 then
      begin
        s^.Min := v; // new entry
        s^.Max := v;
      end
      else
      begin
        m := s^.Min;
        if v < m then
          m := v; // branchless cmovg/cmovl
        s^.Min := m;
        m := s^.Max;
        if v > m then
          m := v;
        s^.Max := m;
      end;
      inc(s^.Sum, v);
      inc(s^.Count);
      start := p;
    until p >= stop;
  end;
  // aggregate this thread values into the main list
  fOwner.Aggregate(fStation);
end;


var
  fn: TFileName;
  threads, chunkmb: integer;
  verbose, affinity, help: boolean;
  main: TBrcMain;
  res: RawUtf8;
  start, stop: Int64;
begin
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
    ['c', 'chunk'], chunkmb, 'size in #megabytes used for per-thread chunking', 8);
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
    main := TBrcMain.Create(fn, threads, chunkmb, {capacity=}45000, affinity);
    // note: current stations count = 41343 for 484KB of data per thread
    try
      main.WaitFor;
      res := main.SortedText;
      if verbose then
        ConsoleWrite(['result hash=',      CardinalToHexShort(crc32cHash(res)),
                      ', result length=',  length(res),
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

