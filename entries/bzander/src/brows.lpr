program brows;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,generics.Collections,sysutils, fasthashmap,mmaputils ,math
  { you can add units after this };
//{$define READ_FROM_STDIN}

var INPUTFILE: string = 'example';
const THREAD_COUNT = {$ifdef READ_FROM_STDIN}1{$else}32{$endif};
{$define USE_FAST_HASHMAP}

type
TTemperatureValue = int64;
TCounter = record
  count: integer;
  min,max: smallint;
  sum: TTemperatureValue;
end;
TCountNoOwning = record

end;
{$ifdef USE_FAST_HASHMAP}
TBRMap = specialize TXQBaseHashmapStr<TCounter>;
{$else}
TBRMap = specialize TDictionary<string, TCounter>;
{$endif}
TThreadResult = record
  map: TBRMap;
  names: array of string;
end;
TTextThreadResults = array[1..THREAD_COUNT] of TThreadResult;
PThreadResult = ^TThreadResult;


//\\\------------------modified from FPC sortbase (LGPL)---------------------------------////

procedure QuickSort_PtrList(ItemPtrs: PPointer; ItemCount: SizeUInt);
  procedure QuickSort(L, R : SizeUInt);
  var
    I, J, PivotIdx : SizeUInt;
    P, Q : Pointer;
  begin
    repeat
      I := L;
      J := R;
      PivotIdx := L + ((R - L) shr 1); { same as ((L + R) div 2), but without the possibility of overflow }
      P := ItemPtrs[PivotIdx];
      repeat
        while (I < PivotIdx) and (CompareStr(string(P), string(ItemPtrs[I])) > 0) do
          Inc(I);
        while (J > PivotIdx) and (CompareStr(string(P), string(ItemPtrs[J])) < 0) do
          Dec(J);
        if I < J then
        begin
          Q := ItemPtrs[I];
          ItemPtrs[I] := ItemPtrs[J];
          ItemPtrs[J] := Q;
          if PivotIdx = I then
          begin
            PivotIdx := J;
            Inc(I);
          end
          else if PivotIdx = J then
          begin
            PivotIdx := I;
            Dec(J);
          end
          else
          begin
            Inc(I);
            Dec(J);
          end;
        end;
      until I >= J;
      // sort the smaller range recursively
      // sort the bigger range via the loop
      // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
      if (PivotIdx - L) < (R - PivotIdx) then
      begin
        if (L + 1) < PivotIdx then
          QuickSort(L, PivotIdx - 1);
        L := PivotIdx + 1;
      end
      else
      begin
        if (PivotIdx + 1) < R then
          QuickSort(PivotIdx + 1, R);
        if (L + 1) < PivotIdx then
          R := PivotIdx - 1
        else
          exit;
      end;
    until L >= R;
  end;

begin
  if not Assigned(ItemPtrs) or (ItemCount < 2) then
    exit;
  QuickSort(0, ItemCount - 1);
end;
///------------------------------------------------------------------\\


type TProcessTextThread = class(TThread)
  fresult: PThreadResult;
  fstart,fend:pchar;
  procedure Execute; override;
end;
procedure TProcessTextThread.Execute;
var
  pend,p, namestart: PChar;
  name: THashMapString;
  value: TTemperatureValue;
  counter: TCounter;
  {$ifndef USE_FAST_HASHMAP}pair: specialize TPair<string, tcounter>;{$endif}
  names: array of string=nil;
  signed: boolean;
  i,namelen: Integer;
  result: TBRMap;
  {$ifdef USE_FAST_HASHMAP}ent: tBrmap.PHashMapEntity;
  entvalue: ^TCounter;
{$endif}
  {$ifdef READ_FROM_STDIN}seqBlock: TSequentialBlock;{$endif}
begin
  {$ifdef READ_FROM_STDIN}seqBlock.init(fstart, fend - fstart);{$endif}
  madviseSequential(fstart,fend-fstart);
  result := TBRMap.Create( 300000 );
  p := fstart;
  pend := fend;
  while (p < pend) {and (p^ <> #0) }do begin
    {$ifdef READ_FROM_STDIN}seqBlock.preload(p+PAGESIZE);{$endif}
    namestart := p;
    while p^ <> ';' do inc(p);
    namelen := p - namestart;
    inc(p);
    value := 0;
    signed := p^ = '-';
    if signed then inc(p);
    while p^ <> '.' do begin
      value := value * 10 + ord(p^) - ord('0');
      inc(p);
    end;
    inc(p);
    value := value * 10 + ord(p^) - ord('0');
    if signed then value := - value;
    //inc(p,2);
    while p^ <> #10 do inc(p);
    while (p^ = #10) or (p^ = #13) do inc(p);

    {$ifdef USE_FAST_HASHMAP}
    ent := result.findEntity(namestart, namelen);
   // if StrLComp(namestart, 'Abeokuta', namelen)=0 then writeln(ent <> nil, ' ',value);
    if ent <> nil then begin
      entvalue := @ent^.Value;
      inc(entvalue^.count);
      if value < entvalue^.min then entvalue^.min := value;
      if value > entvalue^.max then entvalue^.max := value;
      entvalue^.sum += value;
    end else begin
      counter.count := 1;
      counter.min := value;
      counter.max := value;
      counter.sum := value;
      SetString(name, namestart, namelen);
      result.include(name, counter);
    end;
    {$else}
    SetString(name, namestart, namelen);
    if result.TryGetValue(name, counter) then begin
      inc(counter.count);
      if value < counter.min then counter.min := value;
      if value > counter.max then counter.max := value;
      counter.sum += value;
      result.AddOrSetValue(name, counter);
    end else begin
      counter.count := 1;
      counter.min := value;
      counter.max := value;
      counter.sum := value;
      result.Add(name, counter);
    end;
    {$endif}

    {$ifdef READ_FROM_STDIN}seqBlock.forget(namestart);{$endif}
  end;

  setlength(names, result.Count);
  {$ifdef USE_FAST_HASHMAP}
  for i := 0 to result.Count - 1 do
    names[i] := result.Entities[i].Key;
  {$else}
  i:=0;
  for pair in result do begin
    names[i]:=pair.Key;
    inc(i);
  end;
  {$endif}
  QuickSort_PtrList(@names[0], length(names));
 // writeln('first: ',names[0], ' ',CompareStr(names[0],names[1]) ,' ', names[1]);

  fresult^.map := result;
  fresult^.names := names;
{
  for i := 0 to high(names) do begin
    counter := result[names[i]];
    writeln(GetThreadID, ' ', names[i], '=', counter.count, ' ',counter.sum / (counter.count*10) : 2:1);
  end;                                                                                                  }
end;
type TCombineCountersThread = class(TThread)
  ftextthreadResults: ^TTextThreadResults;
  fcombinednames: array of string;
  //fcombinedcounters: array of TCounter;
  foutput: array of shortstring;
  ffromname, ftoname: integer;
  procedure Execute; override;
end;
type T8String = string[8];


function Ceiling(const ANumber: Double): TTemperatureValue; inline;
begin
  Result := Trunc(ANumber) + Ord(Frac(ANumber) > 0);
end;

function RoundExDouble(const ATemp: Double): Double; inline;
var
  tmp: Double;
begin
  //writeln(atemp);
  tmp:= ATemp * 10;
  //writeln(tmp);
  Result := Ceiling(tmp) / 10;
end;
function roundedTenth(i: TTemperatureValue): T8String;
var res,rem: longint;
  signed: Boolean;
begin
  signed := i < 0;
  if signed then i := -i;
  DivMod(i, 10, res,rem);
  str(res, result);
  if signed then result := '-' + result;
  result +='.' + chr(ord('0')+rem);
end;
function roundedDiv(i,dividend: TTemperatureValue): T8String;inline;
//var
//  temp: Double;
begin
//  temp := RoundExDouble((extended(i)/dividend)/10);
//  temp := Ceiling((double(i)/dividend));///10;
//  result := FormatFloat('0.0', temp);
  result := roundedTenth(Ceiling((double(i)/dividend)));
end;

procedure TCombineCountersThread.Execute;
var
  i, j: Integer;
  combinedcounter,counter: TCounter;
  textthreadResults: ^TTextThreadResults;
  {$ifdef USE_FAST_HASHMAP}ent: tBrmap.PHashMapEntity;{$endif}
begin
  textthreadResults := ftextthreadResults;
  for i := ffromname to ftoname do begin
    combinedcounter := default(TCounter);
    combinedcounter.min := high(combinedcounter.min);
    combinedcounter.max := low(combinedcounter.max);
    for j := 1 to THREAD_COUNT do begin
      {$ifdef USE_FAST_HASHMAP}
      ent := textthreadResults^[j].map.findEntity(fcombinednames[i]);
    //writeln(fcombinednames[i],  ' ');
      if ent <> nil then begin
        counter := ent^.value;
      //  writeln(fcombinednames[i],  ' ', counter.count, ' ',counter.sum);
      {$else}
      if textthreadResults^[j].map.TryGetValue(fcombinednames[i], counter) then begin
      {$endif}
        if counter.min < combinedcounter.min then combinedcounter.min := counter.min;
        if counter.max > combinedcounter.max then combinedcounter.max := counter.max;
        combinedcounter.count := combinedcounter.count+counter.count;
        combinedcounter.sum := combinedcounter.sum+counter.sum;
      end;

    end;
    //fcombinedcounters[i] := combinedcounter;
    writestr(foutput[i], fcombinednames[i], '=',
            roundedTenth(combinedcounter.min),
            '/', roundedDiv(combinedcounter.sum, combinedcounter.count) ,
            '/', roundedTenth(combinedcounter.max));
  end;
end;


procedure processall(start,end_:pchar);
var split: array[0..THREAD_COUNT] of pchar;
    textthreads: array[1..THREAD_COUNT] of TProcessTextThread;
    textthreadResults: TTextThreadResults;
    combinethreads: array[1..THREAD_COUNT] of TCombineCountersThread;
  len: ptruint;
  i, l, nextname, i_for_combined_names, cmp: Integer;
  combinednames: array of string = nil;
  namepos: array[1..THREAD_COUNT] of integer;
  //combinedcounters: array of TCounter = nil;
  outputstrings: array of shortstring;
begin
  len := end_ - start;
  //writeln(len);
  for i := 0 to THREAD_COUNT do
    split[i] := start + (len div THREAD_COUNT) * i;
  for i := 1 to THREAD_COUNT - 1 do begin
    while split[i]^ <> #10 do inc(split[i]);
    if split[i]^ = #10 then inc(split[i]);
  end;
  split[THREAD_COUNT] := end_;
  for i := 1 to THREAD_COUNT do begin
    textthreads[i] := TProcessTextThread.Create(true);
    with textthreads[i] do begin
      fresult := @textthreadResults[i];
      fstart := split[i-1];
      fend := split[i];
   //   writeln(i, ' ',fend-fstart);
    end;
    textthreads[i].Start;
  end;
  for i := 1 to THREAD_COUNT do with textthreads[i] do begin
    WaitFor;
    free;
  end;
  l := 0;
  for i := 1 to THREAD_COUNT do inc(l, length(textthreadResults[i].names));
  SetLength(combinednames, l);
  for i := 1 to THREAD_COUNT do namepos[i] := 0;
  i_for_combined_names := 0;
  while true do begin
    nextname := 1;
    while (nextname <= THREAD_COUNT) and (namepos[nextname] > high(textthreadResults[nextname].names)) do
      inc(nextname);
    if nextname > THREAD_COUNT then break;
    for i := nextname+1 to THREAD_COUNT do
      if (namepos[i] <= high(textthreadResults[i].names)) then begin
        cmp := CompareStr(textthreadResults[i].names[namepos[i]], textthreadResults[nextname].names[namepos[nextname]]);
        if cmp = 0 then inc(namepos[i])
        else if cmp < 0 then nextname := i;
      end;
    if (i_for_combined_names = 0) or (combinednames[i_for_combined_names-1] <> textthreadResults[nextname].names[namepos[nextname]]) then begin
      combinednames[i_for_combined_names] := textthreadResults[nextname].names[namepos[nextname]];
      inc(i_for_combined_names);
    end;
    inc(namepos[nextname]);
  end;
  SetLength(outputstrings, i_for_combined_names);

 // writeln;
  //writeln;
  for i := 1 to THREAD_COUNT do begin
    combinethreads[i] := TCombineCountersThread.Create(true);
    with combinethreads[i] do begin
      ftextthreadResults := @textthreadResults;
      fcombinednames:=combinednames;
      foutput:=outputstrings;
      ffromname := (i - 1) * (i_for_combined_names div THREAD_COUNT);
      ftoname := (i) * (i_for_combined_names div THREAD_COUNT) - 1;
      if i = THREAD_COUNT then ftoname := i_for_combined_names - 1;
      start;
    end;
  end;
  for i := 1 to THREAD_COUNT do with combinethreads[i] do begin
    WaitFor;
    free;
  end;
  {
  for i := 0 to i_for_combined_names - 1 do begin
    combinedcounter := default(TCounter);
    combinedcounter.min := high(combinedcounter.min);
    for j := 1 to THREAD_COUNT do
      if textthreadResults[j].map.TryGetValue(combinednames[i], counter) then begin
        if counter.min < combinedcounter.min then combinedcounter.min := counter.min;
        if counter.max > combinedcounter.max then combinedcounter.max := counter.max;
        combinedcounter.count := combinedcounter.count+counter.count;
        combinedcounter.sum := combinedcounter.sum+counter.sum;
      end;
    combinedcounters[i] := combinedcounter;
  end;                                     }
  write('{');
  for i := 0 to i_for_combined_names - 1 do begin

    {combinedcounter := combinedcounters[i];
    writeln(combinednames[i], '=',
            combinedcounter.min/10:1:1,
            '/', combinedcounter.sum / (combinedcounter.count*10) : 1:1 ,
            '/', combinedcounter.max/10:1:1);
     }
    if i <> 0 then write(', ');
    write(outputstrings[i]);
  end;
  writeln('}');
end;



var mmap: TMemoryMappedFile;
begin
  if ParamCount = 0 then writeln('need input file as argument.');
  INPUTFILE := ParamStr(1);
  {$ifdef READ_FROM_STDIN}
  mmap := TMemoryMappedFile.prepareLoadFromStdin(StrToInt64(INPUTFILE));
  {$else}
  mmap := TMemoryMappedFile.loadFromFile(INPUTFILE);
  {$endif}

  processall(mmap.data, mmap.data+mmap.size);
  mmap.destroy;

//  writeln('??');
end.

