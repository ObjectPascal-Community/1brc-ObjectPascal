{1 billion row Challenge entry WITH threads by Hartmut Grosser, official version}

{$mode ObjFPC}{$H+}

{ $DEFINE XTEST}  {please keep this UNDEFINED / compile a safe or a fast program}
{$DEFINE XINLINE} {please keep this DEFINED / use INLINE for routines or not}
{$DEFINE XMORMOT} {please keep this DEFINED / use mORMot twice or not}
{$DEFINE XSTDOUT} {please keep this DEFINED / output result to STDOUT?}

{$IFDEF XTEST}
   {$R+} {$Q+} {$S+}                               {slow but safe}
{$ELSE}
   {$R-} {$Q-} {$S-} {$OPTIMIZATION REGVAR LEVEL4} {fast}
{$ENDIF}

{$IF DEFINED(LINUX) AND DEFINED(CPU64)}
   {$DEFINE LINUX64} {abbreviation}
{$ELSE}
   {$UNDEF LINUX64}
{$ENDIF}

{$INLINE ON} {allows INLINE procedures}

uses
{$IFDEF UNIX}
   cthreads,
{$ENDIF}
   sysutils, strutils, math, classes
{$IFNDEF LINUX64}
   ,baseline.Common  // only for func RoundExDouble()
{$ENDIF}
{$IFDEF XMORMOT}
   ,mormot.core.os   // only for type 'TMemoryMap'
   ,mormot.core.base // only for func crc32c()
{$ENDIF}
   ;

const
  M_version = '2.10'; {version number}

{------------------------------ Common routines: ------------------------------}

procedure debug(s: ansistring);
{outputs a debug message}
begin
{$IFNDEF XSTDOUT}
  writeln(s, #13);
{$ENDIF}
end;

type
  NumStr = string[30]; {string for numbers}

function komma3Str(s: NumStr): NumStr;
  {inserts a komma ',' for all 3 digits}
var
  i, m: integer;
begin
  i := pos('.', s);
  if i > 0 then Dec(i)
  else
    i := length(s);
  m := 3;
  if s[1] = '-' then m := 4;
  while i > m do
  begin
    insert(',', s, i - 2);
    Dec(i, 3);
  end;
  exit(s);
end;

function IntToStr3(i: int64): NumStr;
  {converts 'i' into a string with a komma ',' for all 3 digits}
var
  s: NumStr;
begin
  s := IntToStr(i);
  exit(komma3Str(s));
end;

function doubleStr(d: double; nks: integer): NumStr;
  {converts a 'double' to a string with 'nks' digits after the '.'}
var
  s: NumStr;
begin
  str(d: 0: nks, s);
  exit(s);
end;

function getSecTimeStr(sec: longint): NumStr;
  {converts a time in seconds to minutes and seconds}
var
  s: NumStr;
begin
  s := IntToStr(sec mod 60);
  if length(s) = 1 then s := '0' + s;
  s := IntToStr(sec div 60) + 'm' + s;
  exit(s);
end;

function outTimer(start: int64): NumStr;
  {returns the elapsed time since 'start'}
var
  s: NumStr;
  sec: double;
  stop: int64;
begin
  stop := GetTickCount64;
  sec := (stop - start) / 1000;
  s := doubleStr(sec, 3) + ' s';
  if sec >= 100 then s := s + ' = ' + getSecTimeStr(round(sec));
  exit(s);
end; {outTimer}

procedure error_halt(s: ansistring);
{outputs a Fatal Error and halt's the program}
begin
  writeln('FATAL ERROR!');
  writeln(s);
  halt(1);
end;

{$ASMMODE INTEL}

{$IFDEF LINUX64}
function FPHashX(var buf; len: dword): dword; assembler; nostackframe; inline;
   {returns a hash code similar to function FPHash() from FPC-Unit 'Contnrs',
    but with the difference, that all hash codes for all cities here are UNIQUE.
    I: len: MUST BE > 0!}
asm
         MOV     EAX,dword(-1)

         @LOOP:
         MOV     EDX,EAX       // save Result
         SHL     EAX,5         // eax := Result shl 5
         SUB     EAX,EDX       // eax := Result shl 5 - Result

         XOR     AL, byte ptr [buf]
         INC     buf
         DEC     len
         JNZ     @LOOP
end; {FPHashX}

{$ELSE}

function FPHashX(var buf; len: dword): dword; inline;
   {returns a hash code similar to function FPHash() from FPC-Unit 'Contnrs',
    but with the difference, that all hash codes for all cities here are UNIQUE}
var
  p, pmax: PChar;
begin
{$PUSH}
{$Q-} {Overflow checks off}
  Result := dword(-1);
  p := addr(buf);
  pmax := p + len;

  while (p < pmax) do
  begin
    Result := dword(longint(Result shl 5) - longint(Result)) xor dword(p^);
    Inc(p);
  end;
{$POP}
end; {FPHashX}
{$ENDIF LINUX64}

{----------------------------- consts and types: ------------------------------}

const
  MaxCities = 41500; {max. size for array WA[]}
  MaxLineLen = 56;   {string enough for 1 line incl. LF}
  OverlapLen = 60;   {extension to hash list array to avoid wrap-arounds.
                      Needs '54' for 'bitsHash=16'}
  LF = 10;           {line separator}

type
  cityStr = string[49];    {string for a cityname without temperature}
  cityTmpStr = string[55]; {string for a cityname with temperature}
  tempTyp = int16;         {numeric temperature, multiplied by 10}

  weatherRec = packed record  {collects weather data for 1 city: }
    wcity: cityStr; {city name without temperature}
    wmin: tempTyp;  {min. temperature}
    wmax: tempTyp;  {max. temperature}
    wsum: longint;  {sums all temperatures}
    wcount: word;   {number of all temperatures}
  end; {record}
  pWeatherRec = ^weatherRec;
                                                {weather data for all cities: }
  weatherArray = packed array[0..MaxCities - 1] of weatherRec;

  hashTyp = dword;    {my hash type}
  dataTyp = pointer;  {data type, returned by myHL_find_and_add()}

  hashRec = packed record {an entry in the hash list: }
    hhash: hashTyp;  {the hash code}
    hdata: dataTyp;  {pointer to corresponding array WA[]}
  end;

{--------------------------------- quicksort: ---------------------------------}

function strcmp(const s1, s2: cityStr): integer;
{$IFDEF XINLINE} inline;
{$ENDIF}
   {compares 2 strings alphabetically (case-sensitiv).
    O: -1: s1 < s2 / 0: s1 = s2 / +1: s1 > s2}
var
  m, i, d: integer;
begin
  if length(s1) < length(s2) then m := length(s1)
  else
    m := length(s2);
  for i := 1 to m do
  begin
    d := Ord(s1[i]) - Ord(s2[i]);
    if d > 0 then exit(+1)
    else if d < 0 then exit(-1);
  end;

  if length(s1) > length(s2) then exit(+1)
  else
  if length(s1) < length(s2) then exit(-1)
  else
    exit(0);
end; {strcmp}

type
  QS_item = weatherRec;    {quicksort item type}
  QS_array = weatherArray; {quicksort array type}
  QS_idx = longint;        {quicksort index type}

function QS_less(var x, y: QS_item): boolean;
   {checks the sort order of 'x' and 'y'.
    O: when TRUE is x < y / when FALSE is x >= y}
begin
  exit(strcmp(x.wcity, y.wcity) < 0);  {compares case-sensitiv}
end;

procedure quicksort(var A: QS_array; l, r: QS_idx);
{sorts A[l..r]}

  procedure sort(l, r: QS_idx);
  var
    MEL, ZSP: QS_item;
    i, j: QS_idx;
  begin
    i := l;
    j := r;
    MEL := A[(l + r) shr 1];
    repeat
      while QS_less(A[i], MEL) do Inc(i);
      while QS_less(MEL, A[j]) do Dec(j);
      if i <= j then
      begin
        ZSP := A[i];
        A[i] := A[j];
        A[j] := ZSP;
        Inc(i);
        Dec(j);
      end;
    until i > j;
    if l < j then sort(l, j);
    if i < r then sort(i, r);
  end; {sort}

begin {quicksort}
  if l < r then sort(l, r);
end; {quicksort}

{----------------------------- Class 'TMyThread': -----------------------------}

const
  threads: integer = 4; {number of used threads}

type
  TMyThread = class(TThread)
    fspecIn: ansistring; {file to read}
    city: cityTmpStr;    {cityname with temperature}
    tmp: tempTyp;        {numeric temperature, multiplied by 10}
    pos1, pos2: int64;   {file positions to read [pos1..pos2] in 'fspecIn'}
    buflen: longint;     {buffer size}

    HL: packed array of hashRec; {stores all entries of the hash list}
    NumHash: longint;    {number of hashes in hash list HL[]}
    AndHash: longint;    {AND-mask for all hash codes}
    hashBits: integer;   {bit-width for the hash list HL[]}

    WA: weatherArray;    {weather data for all cities}
    cntWA: word;         {number of valid entries in WA[]}

    jobNo: integer;      {job number [1..'threads'] = thread number}
    done: boolean;       {is this job finished?}

    constructor Create(job: integer; fspec: ansistring; p1, p2: int64;
      bufsiz: longint; bitsHash: integer);
    procedure Execute; override;
    procedure process_chunk;
{$IFDEF XMORMOT}
    procedure process_chunk_MM;
{$ENDIF}
    procedure myHL_init;
    function myHL_find_and_add(out new: boolean): dataTyp;
 {$IFDEF XINLINE} inline; {$ENDIF}
    procedure myHL_addCityTemp; {$IFDEF XINLINE} inline; {$ENDIF}

    procedure sort_WA;
  end; {class}

var
  TRA: array of TMyThread; {TRA[0] is unused}

constructor TMyThread.Create(job: integer; fspec: ansistring; p1, p2: int64;
  bufsiz: longint; bitsHash: integer);
const
  Susp = boolean(0); {True => wait, False => starts immediately}
begin
  // writeln('create: job=', job, ' pos1=', qi6Str(p1):12, ' pos2=', qi6Str(p2):12);
  fspecIn := fspec; {file to read}
  city := '';       {cityname with temperature}
  tmp := 0;         {numeric temperature, multiplied by 10}
  pos1 := p1;       {file positions to read [pos1..pos2] in 'fspecIn': }
  pos2 := p2;
  buflen := bufsiz; {buffer size}

  NumHash := 0;         {number of hashes in hash list HL[]}
  AndHash := 0;         {AND-mask for all hash codes}
  hashBits := bitsHash; {bit-width for the hash list HL[]}

  cntWA := 0;           {number of valid entries in WA[]}

  jobNo := job;         {job number [1..'threads'] = thread number}
  done := False;        {is this job finished?}

  inherited Create(Susp); {call the original constructor}
end; {Create}

var
  start0: int64; {main starting time}

procedure TMyThread.Execute;
begin
  myHL_init;         {initializes my hash list}
{$IFNDEF XMORMOT}
   process_chunk;    {reads a file chunk and processes all it's weather data}
{$ELSE}
   process_chunk_MM; {reads a file chunk and processes all it's weather data}
{$ENDIF}

  debug('Job ' + IntToStr(jobNo) + ' done ' + outTimer(start0) + #13);
  sort_WA;           {sorts array WA[0..cntWA-1] by city names}
  done := True;
end; {Execute}

procedure TMyThread.process_chunk;
   {reads positions [pos1..pos2] from file 'fspecIn' into a buffer of size
    'buflen' and processes all weather measurements. It is assured, that
    position 'pos1' is the 1st byte of a city name and that position 'pos2' is
    a LF}
var
  fi: file;
  s: ansistring;
  size, fpos: int64;
  take, p1, p2, len: longint;
  p, i: integer;
  neg: boolean;
begin
  Assign(fi, fspecIn);
  reset(fi, 1);
  seek(fi, pos1);

  size := succ(pos2 - pos1); {total number of bytes to read}
  SetLength(s, buflen);
  take := buflen;            {number of bytes to read in next chunk}

  while size > 0 do
  begin
    if size < buflen then take := size; {if last turn in this chunk}
    blockread(fi, s[1], take);

    p1 := 1;                                        {now process s[p1..take]: }
    repeat
      p2 := succ(IndexByte(s[p1], succ(take - p1), LF)); {seeks next 'LF'}

      if p2 > 0 then
      begin
        Inc(p2, pred(p1)); {=> p2:=position of 'LF'}
        len := p2 - p1;    {length for city name and temperature}
        city[0] := chr(len);
        move(s[p1], city[1], len);
                                                        {extract temperature: }
        p := succ(IndexByte(city[1], length(city), Ord(';'))); {seeks for ';'}
{$IFDEF XTEST}
        if p = 0 then error_halt('Internal Error: p=0 in city="' + city + '"');
{$ENDIF}
        i := succ(p);                               {get numeric temperature: }
        if city[i] = '-' then
        begin
          Inc(i);
          neg := True;
        end
        else
          neg := False;
        city[length(city) - 1] := city[length(city)]; {skip the '.': }
        Dec(city[0]);
        tmp := 0;

        while i <= length(city) do
        begin
          tmp := 10 * tmp + Ord(city[i]) - $30;
          Inc(i);
        end;
        if neg then tmp := -tmp;

        city[0] := chr(p - 1); {delete ';' and temperature from city name}
        myHL_addCityTemp;      {stores 'city' and 'tmp' in array WA[]}

        p1 := succ(p2);        {'p1' points to the 1. Byte after the LF}
      end;
    until (p2 = 0) or (p1 > take);

    Dec(size, pred(p1));                   {sub only really processed bytes}
    fpos := filepos(fi) - succ(take - p1); {adjust new filepos to be 'p1': }
    seek(fi, fpos);
  end; {while}

  Close(fi);
end; {process_chunk}

const
{$IFDEF CPU32}
  MaxArraySize = High(longint);; {max. allowed array-size: }
{$ELSE}
  MaxArraySize = High(int64);
{$ENDIF}

{$IFDEF XMORMOT}
type
  byteArray = array[0..MaxArraySize - 1] of byte; {file buffer}

var
  MM: TMemoryMap;

procedure TMyThread.process_chunk_MM;
   {reads positions [pos1..pos2] from file 'fspecIn' via Mormot-MemMap and
    processes all weather measurements; It is assured, that position 'pos1' is
    the 1st byte of a city name and that position 'pos2' is a LF}
var
  pBA: ^byteArray;
  p1, p2: int64;
  len: longint;
  p, i: integer;
  neg: boolean;
begin
  pBA := pointer(MM.Buffer); {buffer-startaddress}
  p1 := pos1;

  repeat
    p2 := succ(IndexByte(pBA^[p1], succ(pos2 - p1), LF)); {seeks next 'LF'}
{$IFDEF XTEST}
    if p2 = 0 then error_halt('p2=0 in process_chunk_MM()');
{$ENDIF}
    Inc(p2, pred(p1)); {=> p2:=position of 'LF'}
    len := p2 - p1;    {length for city name and temperature}
    city[0] := chr(len);
    move(pBA^[p1], city[1], len);
                                                        {extract temperature: }
    p := succ(IndexByte(city[1], length(city), Ord(';'))); {seeks for ';'}
{$IFDEF XTEST}
    if p = 0 then error_halt('Internal Error: p=0 in city="' + city + '"');
{$ENDIF}
    i := succ(p);                                   {get numeric temperature: }
    if city[i] = '-' then
    begin
      Inc(i);
      neg := True;
    end
    else
      neg := False;
    city[length(city) - 1] := city[length(city)]; {skip the '.': }
    Dec(city[0]);
    tmp := 0;

    while i <= length(city) do
    begin
      tmp := 10 * tmp + Ord(city[i]) - $30;
      Inc(i);
    end;
    if neg then tmp := -tmp;

    city[0] := chr(p - 1); {cityname without ';' and temperature}
    myHL_addCityTemp;      {stores 'city' and 'tmp' in array WA[]}
    p1 := succ(p2);        {'p1' points to the 1. Byte after the LF}

  until p1 > pos2;
end; {process_chunk_MM}
{$ENDIF XMORMOT}

procedure TMyThread.myHL_init;
{initializes my hash list}
begin
  if hashBits < 16 then error_halt('var "hashBits" is too small');

  NumHash := 1 shl hashBits; {max. number of hashes in array HL[]}
  AndHash := NumHash - 1;    {AND-mask for all hash codes}
  Inc(NumHash, OverlapLen);  {add extension to array HL[] to avoid wrap-arounds}
  SetLength(HL, NumHash);
  fillchar(HL[0], sizeof(HL[0]) * NumHash, 0); {fills field 'hdata'}

  if jobNo = 1 then debug('NumHash=' + IntToStr3(NumHash));
  if jobNo = 1 then debug('sizeof(HL) = ' + IntToStr3(sizeof(HL[0]) * NumHash));
end; {myHL_init}

function TMyThread.myHL_find_and_add(out new: boolean): dataTyp;
{$IFDEF XINLINE} inline;
{$ENDIF}
   {searches for city 'city' in the hash list HL[] and returns access to it's
    data in array WA[]. If the city is not found, it is added to the hash list.
    O: new: was a new entry in array WA[] added?
    ex I: WA[cntWA++]
    ATTENTION: this hash list requires, that all hash codes from function
               FPHashX() or crc32c() are UNIQUE for all city names!}
var
  p: ^hashRec;
  h: hashTyp;
  i: longint;
begin
{$IFNDEF XMORMOT}
  h := FPHashX(city[1],length(city));  {compute unique hash code for 'city': }
{$ELSE}
  h := crc32c(0,@city[1],length(city));
{$ENDIF}
  p := @HL[h and AndHash];             {get Index in HL[0..AndHash]}

  if not Assigned(p^.hdata) then        {if this entry in HL[] is still free: }
  begin
    new := True;
    p^.hhash := h;
    p^.hdata := @WA[cntWA]; {allocate a new entry in WA[]: }
    Inc(cntWA);
    exit(p^.hdata);         {return the new entry in WA[]}
  end;
                                             {if this entry in HL[] is valid: }
  if p^.hhash = h then {if hash matches: }
  begin
    new := False;
    exit(p^.hdata);
  end;

  {if hash not matches: }
  for i := 1 to OverlapLen do
  begin
    Inc(p);

    if not Assigned(p^.hdata) then      {if this entry in HL[] is still free: }
    begin
      new := True;
      p^.hhash := h;
      p^.hdata := @WA[cntWA]; {allocate a new entry in WA[]: }
      Inc(cntWA);
      exit(p^.hdata);         {return the new entry in WA[]}
    end;
                                             {if this entry in HL[] is valid: }
    if p^.hhash = h then {if hash matches: }
    begin
      new := False;
      exit(p^.hdata);
    end;
  end;

  error_halt('const "OverlapLen" is too small in myHL_find_and_add()');
end; {myHL_find_and_add}

procedure TMyThread.myHL_addCityTemp;
{$IFDEF XINLINE} inline; {$ENDIF}
{stores city 'city' with temperature 'tmp' to array WA[] using my hash list}
var
  pWR: pWeatherRec;
  new: boolean;
begin
  pWR := myHL_find_and_add(new); {returns a pointer to 'city' data in WA[]}

  if new then with pWR^ do                              {if it's a new city: }
    begin
      wcity := city;
      wmin := tmp;
      wmax := tmp;
      wsum := tmp;
      wcount := 1;
      exit;
    end;

  with pWR^ do                                     {if city already existed: }
  begin
    if tmp < wmin then wmin := tmp
    else
    if tmp > wmax then wmax := tmp;
    Inc(wsum, tmp);
    Inc(wcount);
  end;
end; {myHL_addCityTemp}

procedure TMyThread.sort_WA;
{sorts WA[0..cntWA-1] by city names}
var
  start1: int64;
begin
  start1 := GetTickCount64;
  quicksort(WA, 0, cntWA - 1);
  debug(' - sorting job ' + IntToStr(jobNo) + ' done in ' + outTimer(start1));
end;

function RoundExString(x: double): NumStr;
{$IFDEF XINLINE} inline; {$ENDIF}
  {new official rounding function}
var
  V, Q, R: integer;
begin
  V := Ceil(x * 10);

  if V < 0 then
  begin
    Result := '-';
    V := -V;
  end
  else
    Result := '';

  Q := V div 10;
  R := V - (Q * 10);
  Result := Result + IntToStr(Q) + '.' + IntToStr(R);
end; {RoundExString}

function myRound(sum, Count: longint): NumStr;
{$IFDEF XINLINE} inline; {$ENDIF}
  {using new official rounding function from 26.3.24}
{$IFNDEF LINUX64}
var
  s: NumStr;
  d: double;
{$ENDIF}
begin
{$IFDEF LINUX64}
  exit(RoundExString(sum / (Count * 10)));
{$ELSE}
  d := baseline.Common.RoundExDouble(sum / (Count * 10));
  str(d: 0: 1, s);
  exit(s);
{$ENDIF}
end; {myRound}

procedure create_output;
{creates the result of this program: either to STDOUT or into a textfile}
const
  FspecOut = '/media/H/tmp/1brc/xx.txt';
var
  fo: Text;
  start1: int64;
  i, sum: longint;
  Count: word;
  min, max: tempTyp;
  j: integer;
begin
  start1 := GetTickCount64;
  for j := 2 to threads do  if TRA[j].cntWA <> TRA[1].cntWA then
     error_halt('var "cntWA" is not equal in all jobs!');

{$IFDEF XSTDOUT}
  Assign(fo, '');
  rewrite(fo);
  Write(fo, '{');
{$ELSE}
  Assign(fo, FspecOut);
  rewrite(fo);
{$ENDIF}

  for i := 0 to TRA[1].cntWA - 1 do  {for all sorted cities: }
  begin
    with TRA[1].WA[i] do
    begin
      min := wmin;
      max := wmax;
      sum := wsum;
      Count := wcount;
    end;
    for j := 2 to threads do with TRA[j].WA[i] do
      begin
        if wmin < min then min := wmin;
        if wmax > max then max := wmax;
        Inc(sum, wsum);
        Inc(Count, wcount);
      end;
{$IFDEF XSTDOUT}
    if i > 0 then write(fo, ', ');
{$ENDIF}
    Write(fo, TRA[1].WA[i].wcity, '=', min / 10: 0: 1, '/', myRound(sum, Count),
      '/', max / 10: 0: 1);
{$IFNDEF XSTDOUT}
    writeln(fo);
{$ENDIF}
  end;

{$IFDEF XSTDOUT}
  writeln(fo, '}');
{$ENDIF}
  Close(fo);
  debug('Saved ' + IntToStr(TRA[1].cntWA) + ' items in ' + outTimer(start1));
end; {create_output}

procedure process_measurements(fspecIn: ansistring; buflen: longint;
  bitsHash: integer);
{starts all threads to do their jobs}
var
  fi: file;
  Buf: array[1..MaxLineLen] of byte;
  size, p1, p2, p: int64;
  j, dones: integer;
  oldFileMode: byte;
begin
  start0 := GetTickCount64; {start main Timer}

  oldFileMode := FileMode;  {save value}
  FileMode := fmShareDenyNone; {open file in read-only Mode, allow others}
  Assign(fi, fspecIn);
  reset(fi, 1);
  size := filesize(fi);

  debug('Threads=' + IntToStr(threads) + ' fspec=' +
    SysUtils.ExtractFileName(fspecIn) + ' filesize=' + IntToStr3(size) +
    ' buflen=' + IntToStr3(buflen) + ' bitsHash=' + IntToStr(bitsHash));

  SetLength(TRA, threads + 1); {TRA[0] is unused}

  p2 := -1;                                               {create all threads:}
  for j := 1 to threads do
  begin
    p1 := p2 + 1;
    p2 := j * round(size / threads);
    if j = threads then p2 := size - 1 {last job => read until end of file}
    else
    begin
      seek(fi, p2);                    {increase 'p2' until 1st 'LF': }
      blockread(fi, Buf, MaxLineLen);
      p := IndexByte(Buf, MaxLineLen, LF);
      if p < 0 then error_halt('p < 0 in process_measurements()');
      Inc(p2, p);
    end;
    TRA[j] := TMyThread.Create(j, fspecIn, p1, p2, buflen, bitsHash);
  end;
  Close(fi);
  FileMode := oldFileMode; {restore value}

  repeat
    dones := 0;                                {wait until all jobs are done: }
    sleep(1);
    for j := 1 to threads do if TRA[j].done then Inc(dones);
  until dones = threads;

  create_output; {creates + sends the result of this program to STDOUT}

  for j := 1 to threads do TRA[j].Free; {frees all threads}
  debug('Elapsed time: ' + outTimer(start0));
end; {process_measurements}

{$IFDEF XMORMOT}
procedure process_measurements_MM(fspecIn: ansistring; bitsHash: integer);
{starts all threads to do their jobs (with Mormot-MemMap function)}
var
  pBA: ^byteArray;
  s: string;
  size, p1, p2, p: int64;
  j, dones: integer;
  ok: boolean;
begin
  start0 := GetTickCount64; {start main Timer}
  ok := MM.Map(fspecIn);    {init Mormot-MemMap}
  if not ok then error_halt('TMemoryMap.Map() failed');

  pBA := pointer(MM.Buffer); {buffer-startaddress}
  size := MM.Size;           {filesize}
  debug('MM: fspec=' + SysUtils.ExtractFileName(fspecIn) + ' filesize=' +
    IntToStr3(size) + ' bitsHash=' + IntToStr(bitsHash));

  SetLength(TRA, threads + 1); {TRA[0] is unused}

  p2 := -1;
  for j := 1 to threads do
  begin
    p1 := p2 + 1;
    p2 := j * round(size / threads);
    if j = threads then p2 := size - 1 {last job => read until end of file}
    else
    begin
      p := IndexByte(pBA^[p2], MaxLineLen, LF); {seeks next LF}
      if p < 0 then error_halt('p < 0 in process_measurements_MM()');
      Inc(p2, p);
    end;
    TRA[j] := TMyThread.Create(j, fspecIn, p1, p2, 1, bitsHash);
  end;

  repeat                                       {wait until all jobs are done: }
    dones := 0;
    sleep(1);
    for j := 1 to threads do if TRA[j].done then Inc(dones);
  until dones = threads;

  MM.UnMap; {free 'MM'}

  create_output; {creates + sends the result of this program to STDOUT}

  for j := 1 to threads do TRA[j].Free;
  debug('Elapsed time: ' + outTimer(start0));
end; {process_measurements_MM}
{$ENDIF XMORMOT}

{-------------------------- command line parameters: --------------------------}

const
  ThreadsMin = 1;   {minimum allowed threads}
  ThreadsMax = 64;  {maximum allowed threads}
  HashBitsMin = 16; {Min. bit-width for the hash-array HL[]}
  HashBitsMax = 28; {Max. bit-width for the hash-array HL[]}
  HashBitsDef = 16; {Default bit-width for the hash-array HL[]}
  BufLenMin_KB = 1;       {Min. file buffer size in KB}
  BufLenMax_KB = 2000000; {Max. file buffer size in KB}
  BufLenDef_KB = 128;     {Default file buffer size in KB}

procedure syntax_halt;
{shows allowed syntax and halt's the program}
begin
  writeln('1 billion row Challenge program with threads by Hartmut Grosser, version ',
    M_version);
  writeln('Usage: <path to input file> <thread count> [<bit-width for hash-list>',
    {$IFNDEF XMORMOT} ' [buffer size in KB]', {$ENDIF} ']');
  writeln(' - thread count: allowed range = [', IntToStr(ThreadsMin), '..',
    IntToStr(ThreadsMax), ']');
  writeln(' - bit-width for hash-list: sets the size of the hash list, ',
    'e.g. ''16'' => 65536 entries,');
  writeln('   allowed range = [', IntToStr(HashBitsMin), '..', IntToStr(HashBitsMax),
    '], Default=', IntToStr(HashBitsDef));
{$IFNDEF XMORMOT}
  writeln(' - buffer size in KB: allowed range = [', IntToStr(BufLenMin_KB),
    '..', IntToStr3(BufLenMax_KB), ' KB], Default=', IntToStr(BufLenDef_KB), ' KB');
{$ENDIF}
  writeln('Example: ', SysUtils.ExtractFileName(ParamStr(0)),
    ' measurements.txt 32 16' {$IFNDEF XMORMOT} , ' 128' {$ENDIF});
  halt(1);
end; {syntax_halt}

var
  fspecInp: ansistring; {filespec of input file}
  buflen: longint;      {file buffer size to read file 'fspecInp'}
  hashBits: integer;    {bit-width for the hash-array HL[]}

procedure take_command_line_parameters;
{checks command line parameters. Errors => show message and halt}
const
{$IFNDEF XMORMOT}
  MaxP = 4; {with file buffer size}
{$ELSE}
  MaxP = 3; {without file buffer size}
{$ENDIF}
var
  p, code: integer;
begin
  p := ParamCount;
  if (p < 2) or (p > MaxP) then syntax_halt;

  fspecInp := ParamStr(1);
  hashBits := HashBitsDef;
  buflen := BufLenDef_KB * 1024;

  val(ParamStr(2), threads, code);                      {take 'thread count': }
  if code <> 0 then syntax_halt;
  if (threads < ThreadsMin) or (threads > ThreadsMax) then syntax_halt;

  if p >= 3 then                             {take 'bit-width for hash-list': }
  begin
    val(ParamStr(3), hashBits, code);
    if code <> 0 then syntax_halt;
    if (hashBits < HashBitsMin) or (hashBits > HashBitsMax) then syntax_halt;
  end;

  if p >= 4 then                              {take 'file buffer size in KB': }
  begin
    val(ParamStr(4), buflen, code);
    if code <> 0 then syntax_halt;
    if (buflen < BufLenMin_KB) or (buflen > BufLenMax_KB) then syntax_halt;
    buflen := buflen * 1024;
  end;

  if FileExists(fspecInp) then exit;

  writeln('Input file "', fspecInp, ' not found!');
  halt(1);
end; {take_command_line_parameters}

begin {main}
  take_command_line_parameters; {Errors => show message and halt}

{$IFDEF XTEST}
  debug('XTEST=safe');
{$ELSE}
  debug('XTEST=fast');
{$ENDIF}
{$IFDEF XINLINE}
  debug('with INLINE');
{$ELSE}
  debug('without INLINE');
{$ENDIF}
{$IFNDEF XMORMOT}
   debug('without mORMot');
{$ELSE}
   debug('with mORMot');
{$ENDIF}
{$IFDEF XSTDOUT}
  debug('Output=STDOUT');
{$ELSE}
  debug('Output=Textfile');
{$ENDIF}

{$IFNDEF XMORMOT}
  process_measurements(fspecInp, buflen, hashBits);
{$ELSE}
  process_measurements_MM(fspecInp, hashBits);
{$ENDIF}
  debug('');
end.

