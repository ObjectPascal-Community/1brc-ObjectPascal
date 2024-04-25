{1 billion row Challenge entry by Hartmut Grosser, official version}

{$mode ObjFPC}{$H+}

{ $DEFINE XTEST}   {please keep this UNDEFINED / compile a safe or a fast program}
{ $DEFINE XINLINE} {please keep this UNDEFINED / use INLINE for routines or not}
{$DEFINE noCR}     {neccessary for input-files without CR's (only LF's)}

{$IFDEF XTEST}
   {$R+} {$Q+} {$S+}                               {slow but safe}
{$ELSE}
   {$R-} {$Q-} {$S-} {$OPTIMIZATION REGVAR LEVEL4} {fast}
{$ENDIF}

{$INLINE ON} {allows INLINE procedures}

uses
  {$IFDEF UNIX}
     cthreads,
  {$ENDIF}
     sysutils, strutils, math;

const
  M_version = '1.61'; {version number}

{------------------------------ Common routines: ------------------------------}

procedure debug(s: ansistring);
{outputs a debug message}
begin
//  writeln(s);
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
  {formats a time in seconds to minutes and seconds}
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

function FPHashX_ASM(var buf; len: dword): dword; assembler; nostackframe; inline;
   {returns a hash code similar to function FPHash() from FPC-Unit 'Contnrs',
    but with the difference, that all hash codes for all cities here are UNIQUE.
    I: len: MUST BE > 0!}
asm
         MOV     EAX,dword(-1)

         @LOOP:
         MOV     EDX,EAX       // save Result
         SHL     EAX,5         // eax := Result shl 5
         SUB     EAX,EDX       // eax := Result shl 5 - Result

         XOR     AL, [buf]
         INC     buf
         DEC     len
         JNZ     @LOOP
end; {FPHashX_ASM}

{----------------------------- consts and types: ------------------------------}

const
  MaxCities = 42000;      {max. size for array WA[]}
  OverlapLen = 60;        {Extension to hash list array to avoid wrap-arounds}
  BufLenDefault_KB = 128; {Default buffer size in kb}

type
  cityStr    = string[49]; {string for a cityname without temperature}
  cityTmpStr = string[55]; {string for a cityname with temperature}
  tempTyp = int16;         {numeric temperature, multiplied by 10}

  weatherRec = packed record {collects weather data for 1 city: }
    wcity: cityStr; {city name}
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

var
  WA: weatherArray;            {weather data for all cities}
  HL: packed array of hashRec; {stores all entries of the hash list}

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
  QS_item = weatherRec;   {quicksort item type}
  QS_array = weatherArray; {quicksort array type}
  QS_idx = longint;      {quicksort index type}

function QS_less(var x, y: QS_item): boolean;
   {checks the sort order of 'x' and 'y';
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

{---------------------------- Class 'weatherClass': ---------------------------}

type
  weatherClass = class(TObject)
    fspecIn: ansistring; {file to read}
    buflen: longint;    {buffer size}
    cntWA: word;         {number of valid entries in WA[]}
    cntMulti: longint;   {counts cities when they occur a 2nd time}
    wsum_min, wsum_max: longint; {only for testing}

    NumHash: longint;    {number of hashes in hash array HL[]}
    AndHash: longint;    {AND-mask for all hash codes}

    constructor Create(fspec: ansistring; bufsize: longint; hashBits: integer);
    destructor Destroy; override;
    procedure myHL_init(hashBits: integer);
    function myHL_find_and_add(out new: boolean): dataTyp;
 {$IFDEF XINLINE} inline; {$ENDIF}
    procedure myHL_addCityTemp; {$IFDEF XINLINE} inline; {$ENDIF}

    procedure sort_WA;
    function RoundExString(x: double): NumStr; {$IFDEF XINLINE} inline; {$ENDIF}
    function myRound(sum, Count: longint): NumStr; {$IFDEF XINLINE} inline; {$ENDIF}
    procedure save_WA;
    procedure write_WA_STDOUT;
    procedure process_measurements;
  end; {Class}

constructor weatherClass.Create(fspec: ansistring; bufsize: longint;
  hashBits: integer);
   {I: - fspec: filespec to read.
       - bufsize: buffer size for above file.
       - hashBits: bit-width for the hash-array HL[]}
begin
  inherited Create;
  fspecIn := fspec;
  buflen := bufsize;

  cntWA := 0;            {number of valid entries in WA[]}
  cntMulti := 0;         {counts cities when they occur a 2nd time}
  wsum_min := High(wsum_min);
  wsum_max := Low(wsum_max);

  myHL_init(hashBits); {initializes my hash list}

  debug('sizeof(weatherRec)=' + IntToStr(sizeof(WA[0])));
end; {Create}

destructor weatherClass.Destroy;
begin
  inherited Destroy;
end;

procedure weatherClass.myHL_init(hashBits: integer);
   {initializes my hash list.
    I: hashBits: bit-width for the hash-array HL[]}
begin
  if hashBits < 16 then error_halt('var "hashBits" is too small');

  NumHash := 1 shl hashBits; {max. number of hashes in array HL[]}
  AndHash := NumHash - 1;      {AND-mask for all hash codes}
  Inc(NumHash, OverlapLen); {add extension to array HL[] to avoid wrap-arounds}
  SetLength(HL, NumHash);
  fillchar(HL[0], sizeof(HL[0]) * NumHash, 0); {fills field 'hdata'}

  debug('NumHash=' + IntToStr3(NumHash));
  debug('sizeof(HL) = ' + IntToStr3(sizeof(HL[0]) * NumHash));
end; {myHL_init}

var
  city: cityTmpStr; {global variables for use in myHL_find_and_add() and}
  tmp: tempTyp;     {in process_measurements() are faster than parameters}

function weatherClass.myHL_find_and_add(out new: boolean): dataTyp;
{$IFDEF XINLINE} inline;
{$ENDIF}
   {searches for city 'city' in the hash list HL[] and returns access to it's
    data in array WA[]. If the city is not found, it is added to the hash list.
    O: new: was a new entry in array WA[] added?
    ex I: WA[cntWA++];
    ATTENTION: this hash list requires, that all hash codes from function
               FPHashX_ASM() are UNIQUE for all city names!}
var
  p: ^hashRec;
  h: hashTyp;
  i: longint;
begin
  h := FPHashX_ASM(city[1], length(city)); {compute unique hash code for 'city'}
  p := @HL[h and AndHash];                 {get Index in HL[0..AndHash]}

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

  error_halt('const "OverlapLen" too small in myHL_find_and_add()');
end; {myHL_find_and_add}

procedure weatherClass.myHL_addCityTemp;
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
{$IFDEF XTEST}
    if wsum < wsum_min then wsum_min := wsum;
    if wsum > wsum_max then wsum_max := wsum;
{$ENDIF}
    Inc(wcount);
    Inc(cntMulti); {counts cities when they occur a 2nd time}
  end;
end; {myHL_addCityTemp}

procedure weatherClass.sort_WA;
{sorts WA[0..cntWA-1] by city names}
var
  start: int64;
begin
  start := GetTickCount64;
  quicksort(WA, 0, cntWA - 1);
  debug('Sorting done in ' + outTimer(start));
end;

function weatherClass.RoundExString(x: double): NumStr;
{$IFDEF XINLINE} inline;
{$ENDIF}
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

function weatherClass.myRound(sum, Count: longint): NumStr;
{$IFDEF XINLINE} inline;
{$ENDIF}
  {using new official rounding function from 26.3.24}
var
  x: double;
begin
  // x:=sum / (count*10);
  exit(RoundExString(sum / (Count * 10)));
end;

procedure weatherClass.save_WA;
   {only for testing: saves the results in array WA[] into a textfile with 1
    line per city}
const
  FspecOut = '/media/H/tmp/1brc/xx.txt';
var
  fo: Text;
  start: int64;
  i: longint;
begin
  start := GetTickCount64;
  Assign(fo, FspecOut);
  rewrite(fo);

  for i := 0 to cntWA - 1 do with WA[i] do
      writeln(fo, wcity, '=', wmin / 10: 0: 1, '/', myRound(wsum, wcount), '/',
        wmax / 10: 0: 1);

  Close(fo);
  debug('Saved ' + IntToStr(cntWA) + ' items via save_WA() = ' + outTimer(start));
end; {save_WA}

procedure weatherClass.write_WA_STDOUT;
{writes the results in array WA[] via STDOUT to the console}
var
  fo: Text;
  s: string;
  start: int64;
  i: longint;
begin
  start := GetTickCount64;
  Assign(fo, '');
  rewrite(fo);

  Write(fo, '{');
  for i := 0 to cntWA - 1 do with WA[i] do
    begin
      if i > 0 then Write(fo, ', ');
      Write(fo, wcity, '=', wmin / 10: 0: 1, '/', myRound(wsum, wcount), '/',
        wmax / 10: 0: 1);
    end;
  writeln(fo, '}');

  Close(fo);
  debug('Saved ' + IntToStr(cntWA) + ' items via write_WA_STDOUT() = ' +
    outTimer(start));
end; {write_WA_STDOUT}

procedure weatherClass.process_measurements;
{reads input file 'fspecIn' and processes all measurements}
const
  LF = #10; {line separator}
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
  size := filesize(fi);
  debug('fspec=' + SysUtils.ExtractFileName(fspecIn) + ' filesize=' +
    IntToStr3(size) + ' buflen=' + IntToStr3(buflen));

  SetLength(s, buflen);
  take := buflen; {number of bytes to read}

  while size > 0 do
  begin                                                    {if last turn: }
    if size < buflen then
    begin
      take := size;
      SetLength(s, take);
    end;
    blockread(fi, s[1], take);

    p1 := 1; {parse 's': }
    repeat
      p2 := succ(IndexByte(s[p1], succ(take - p1), byte(LF))); {seeks for 'LF'}
      if p2 > 0 then
      begin
        Inc(p2, pred(p1)); {p2:=position of 'LF'}
                                   {get cityname and temperature, without CR: }
        len := p2 - p1 {$IFNDEF noCR} -1 {$ENDIF} ; {'-1' skips the CR}
        city[0] := chr(len);
        move(s[p1], city[1], len);
                                                        {extract temperature: }
        p := 1 + IndexByte(city[1], length(city), Ord(';')); {seeks for ';'}
{$IFDEF XTEST}
        if p = 0 then error_halt('Internal Error: p=0');
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

        city[0] := chr(p - 1); {delete temperature from city name}
        myHL_addCityTemp;      {adds 'city' and 'tmp' to array WA[]}

        p1 := p2 + 1;
      end;
    until p2 = 0;

    Dec(size, pred(p1));                   {sub only really processed bytes}
    fpos := filepos(fi) - succ(take - p1); {adjust new filepos to 'p1': }
    seek(fi, fpos);
  end; {while}

  Close(fi);

  debug('cntWA=' + IntToStr(cntWA));         {=> 41343}
  debug('cntMulti=' + IntToStr3(cntMulti));  {=> 999,958,657}
{$IFDEF XTEST}
  debug('wsum_min=' + IntToStr3(wsum_min)    {=> -20,591,902} +
    ' wsum_max=' + IntToStr3(wsum_max));     {=> +20,415,562}
{$ENDIF}

  sort_WA;         {sorts array WA[0..cntWA-1] by city names}
//save_WA;         {for Tests: saves the results in array WA[] into a textfile}
  write_WA_STDOUT; {writes the results in array WA[] to STDOUT}
end; {process_measurements}

{------------------------- End of Class 'weatherClass' ------------------------}

const
  HashBitsMin = 16; {Min. bit-width for the hash-array HL[]}
  HashBitsMax = 28; {Max. bit-width for the hash-array HL[]}

procedure syntax_halt;
{shows allowed syntax and halt's the program}
begin
  writeln('Purpose: 1 billion row Challenge program by Hartmut Grosser, version ',
    M_version);
  writeln('Usage:   <path to input file> <bit-width for hash-list (',
    IntToStr(HashBitsMin), '..', IntToStr(HashBitsMax), ')>');
  writeln('Example: ', SysUtils.ExtractFileName(ParamStr(0)),
    ' measurements.txt 16');
  writeln(' - bit-width for hash-list: sets the size of the hash list, e.g. ''16'' => 65536 entries');
  halt(1);
end;

var
  fspecInp: ansistring; {filespec of input file}
  buflen: longint;      {buffer size to read file 'fspecInp'}
  hashBits: integer;    {bit-width for the hash-array HL[]}

procedure take_command_line_parameters;
{checks command line parameters. Errors => show message and halt}
var
  p, code: integer;
begin
  p := ParamCount;
  if p <> 2 then syntax_halt;

  fspecInp := ParamStr(1);
  buflen := BufLenDefault_KB * 1024;

  val(ParamStr(2), hashBits, code);
  if code <> 0 then syntax_halt;
  if (hashBits < HashBitsMin) or (hashBits > HashBitsMax) then syntax_halt;

  if FileExists(fspecInp) then exit;

  writeln('Input file "', fspecInp, ' not found!');
  halt(1);
end; {take_command_line_parameters}

var
  WC: weatherClass;
  start: int64;

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

  // fspecInp:='/media/H/tmp/1brc/measurements.txt';
  // buflen:=BufLenDefault_KB * 1024;
  // hashBits:=16;

  start := GetTickCount64;

  WC := weatherClass.Create(fspecInp, buflen, hashBits);
  try
    WC.process_measurements;
  finally
    WC.Free;
  end; {try}

  debug('Elapsed time = ' + outTimer(start));
  debug('');
end.
