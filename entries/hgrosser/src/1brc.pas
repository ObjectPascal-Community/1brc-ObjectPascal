{1 billion row Challenge entry by Hartmut Grosser, official version}

{$mode ObjFPC}{$H+}

{ $DEFINE XTEST}   {please keep this UNDEFINED / compile a safe or a fast program}
{ $DEFINE XINLINE} {please keep this UNDEFINED / use INLINE for routines or not}

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
     sysutils, strutils, math,
     Contnrs; {for type 'TFPHashList'}

const
  M_version = '1.0'; {version number}

procedure debug(s: ansistring);
  {outputs a debug message}
begin
  // writeln(s);
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

{----------------------------- consts and types: ------------------------------}

const
  MaxCities = 50000;    {max. size for array WA[]}
  HL_Capacity = 120000;   {initial capacity for the hash list}
  BufLenDefault_KB = 128; {Default buffer size in kb}

type
  cityStr = string[63]; {string for a cityname with temperature (maxlen=55)}
  tempStr = string[7];  {string for a temparature as string e.g. "-99.9"}
  tempTyp = int16;      {numeric temperature, multiplied by 10}

  weatherRec = record {collects weather data for 1 city: }
    wcity: cityStr; {city name}
    wmin: tempTyp; {min. temperature}
    wmax: tempTyp; {max. temperature}
    wsum: longint; {sums all temperatures}
    wcount: longint; {number of all temperatures}
  end; {record}
  pWeatherRec = ^weatherRec;

  weatherArray = array[0..MaxCities - 1] of weatherRec; {weather data for all cities}

{--------------------------------- quicksort: ---------------------------------}

function strcmp(const s1, s2: cityStr): integer;
{$IFDEF XINLINE} inline; {$ENDIF}
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
    WA: weatherArray;    {weather data for all cities}
    HLC: TFPHashList;    {hash list for city names}

    fspecIn: ansistring; {file to read}
    buflen: longint;     {buffer size}
    cntWA: word;         {number of valid entries in WA[]}
    cntMulti: longint;   {counts cities when they occur a 2nd time}
    wsum_min, wsum_max: longint; {only for testing}

    constructor Create(fspec: ansistring; bufsize: longint);
    destructor Destroy; override;
    procedure addCityTemp(var s: cityStr; t: tempTyp);
 {$IFDEF XINLINE} inline; {$ENDIF}
    procedure sort_WA;
    function RoundExString(x: double): NumStr; {$IFDEF XINLINE} inline; {$ENDIF}
    function myRound(sum, Count: longint): NumStr; {$IFDEF XINLINE} inline; {$ENDIF}
    procedure save_WA;
    procedure write_WA_STDOUT;
    procedure process_measurements;
  end; {Class}

constructor weatherClass.Create(fspec: ansistring; bufsize: longint);
begin
  inherited Create;
  fspecIn := fspec;
  buflen := bufsize;

  HLC := TFPHashList.Create;
  HLC.Capacity := HL_Capacity; {set initial capacity for the hash list}

  cntWA := 0;                  {number of valid entries in WA[]}
  cntMulti := 0;               {counts cities when they occur a 2nd time}
  wsum_min := High(wsum_min);
  wsum_max := Low(wsum_max);

  debug('sizeof=' + IntToStr(sizeof(WA[0])));
end; {Create}

destructor weatherClass.Destroy;
begin
  HLC.Free;
  inherited Destroy;
end;

procedure weatherClass.addCityTemp(var s: cityStr; t: tempTyp);
{$IFDEF XINLINE} inline; {$ENDIF}
{stores city 's' with temperature 't' to array WA[], using a hash list}
var
  pWR: pWeatherRec;
begin
  pWR := HLC.Find(s); {returns a pointer to the city data}

  if pWR = nil then with WA[cntWA] do                         {if a new city: }
    begin
      wcity := s;
      wmin := t;
      wmax := t;
      wsum := t;
      wcount := 1;
      HLC.Add(s, @WA[cntWA]); {adds a city with its data to the hash list}
      Inc(cntWA);
      exit;
    end;

  with pWR^ do                                      {if city already existed: }
  begin
    if t < wmin then wmin := t
    else
    if t > wmax then wmax := t;
    Inc(wsum, t);
{$IFDEF XTEST}
    if wsum < wsum_min then wsum_min := wsum;
    if wsum > wsum_max then wsum_max := wsum;
{$ENDIF}
    Inc(wcount);
    Inc(cntMulti); {counts cities when they occur a 2nd time}
  end;
end; {addCityTemp}

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

function weatherClass.myRound(sum, Count: longint): NumStr;
{$IFDEF XINLINE} inline; {$ENDIF}
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
  FspecOut = '/media/H/tmp/xx.txt';
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
  city: cityStr;
  temp: tempStr;
  size, fpos: int64;
  take, p1, p2, p, len: longint;
  i, t: integer;
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
      p2 := posEx(LF, s, p1);
      if p2 > 0 then
      begin                  {get cityname and temperature, without CR: }
        //          city:=copy(s,p1,p2-p1-1):
        len := p2 - p1 - 1;
        city[0] := chr(len);
        move(s[p1], city[1], len);

        p := pos(';', city);                           {extract temperature: }
        //          temp:=copy(city,p+1):
        len := length(city) - p;
        temp[0] := chr(len);
        move(city[p + 1], temp[1], len);
        {get numeric temperature: }
        if temp[1] = '-' then
        begin
          i := 2;
          neg := True;
        end
        else
        begin
          i := 1;
          neg := False;
        end;
        t := 0;
        temp[length(temp) - 1] := temp[length(temp)]; {skip the '.': }
        Dec(temp[0]);

        while i <= length(temp) do
        begin
          t := 10 * t + Ord(temp[i]) - $30;
          Inc(i);
        end;
        if neg then t := -t;

{$IFDEF XTEST}
        if p = 0 then error_halt('Internal Error: p=0');
{$ENDIF}
        city[0] := chr(p - 1);   {delete temperature from city name}
        addCityTemp(city, t); {stores city + temperature in array WA[]}

        p1 := p2 + 1;
      end;
    until p2 = 0;

    Dec(size, pred(p1));                {sub only really processed bytes}
    fpos := filepos(fi) - succ(take - p1); {adjust new filepos to 'p1': }
    seek(fi, fpos);
  end; {while}

  Close(fi);

  debug('cntWA=' + IntToStr(cntWA));         {=> 41343}
  debug('cntMulti=' + IntToStr3(cntMulti));  {=> 999,958,657}
{$IFDEF XTEST}
  debug('wsum_min=' + IntToStr3(wsum_min)    {=> -20,591,902} +
    ' wsum_max=' + IntToStr3(wsum_max)); {=> +20,415,562}
{$ENDIF}

  sort_WA;         {sorts array WA[0..cntWA-1] by city names}
  // save_WA;      {for Tests: saves the results in array WA[] into a textfile}
  write_WA_STDOUT; {writes the results in array WA[] to STDOUT}
end; {process_measurements}

{------------------------- End of Class 'weatherClass' ------------------------}

procedure syntax_halt;
{shows allowed syntax and halt's the program}
begin
  writeln('Purpose: 1 billion row Challenge program by Hartmut Grosser, version ',
    M_version);
  writeln('Usage:   <path to input file> [buffer size in kb (Default=',
    IntToStr(BufLenDefault_KB), ' kb)]');
  writeln('Example: ', SysUtils.ExtractFileName(ParamStr(0)),
    ' ./measurements.txt 128');
  halt(1);
end;

var
  fspecInp: ansistring; {filespec of input file}
  buflen: longint;      {buffer size to read file 'fspecInp'}

procedure take_command_line_parameters;
{checks command line parameters. Errors => show message and halt}
var
  p, code: integer;
begin
  p := ParamCount;
  if (p < 1) or (p > 2) then syntax_halt;

  fspecInp := ParamStr(1);
  buflen := BufLenDefault_KB * 1024;

  if p = 2 then
  begin
    val(ParamStr(2), buflen, code);
    if code <> 0 then syntax_halt;
    buflen := buflen * 1024;
  end;

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

  // fspecInp:='/media/H/tmp/measurements.txt';
  // buflen:=BufLenDefault_KB * 1024;

  start := GetTickCount64;

  WC := weatherClass.Create(fspecInp, buflen);
  try
    WC.process_measurements;
  finally
    WC.Free;
  end; {try}

  debug('Elapsed time = ' + outTimer(start));
  debug('');
end.
