unit swizmap;

{$mode delphi}

interface

uses sysutils;
//
// https://github.com/LIMachi/swiss-table/tree/master
// https://faultlore.com/blah/hashbrown-tldr/

const
    SWT_CONTROL_SIZE = 16;
    SWT_GROUP_SIZE = 16;
    SWT_VALUE_SIZE = SWT_CONTROL_SIZE * SWT_GROUP_SIZE;
    SWT_LOAD_FACTOR = 0.75;
    SWT_EXPAND_FACTOR = 2;
    SWT_FULL_MASK = 127;
    SWT_EMPTY = 128;

type
  tkeystr=record
    s:pchar;
    l:word;
  end;

  t_swt_i128 = array[0..(SWT_CONTROL_SIZE)-1] of byte;

  { tswhash }

  tswhash = record
  private
    flag0 : uint64;
    function get_meta : uint64;
    function get_position: uint64;
  public
    property meta:SizeUInt read get_meta;
    property position:sizeuint read get_position;
   end;

  t_swt_hash = record
    case byte of
      0: ( h: tswhash );
      1: ( s: uint64 );
    end;

  { s_swt_group }

  t_swt_group = record
    control : t_swt_i128;
    key : array[0..SWT_CONTROL_SIZE-1] of tkeystr;
  end;
  pgroup=^t_swt_group;

  tgrouparray=array of t_swt_group;

  t_swt_hashfun = function(str:pchar; len:integer):uint64;

  { tswmap }

  tswpair<T>=record
    key:tkeystr;
    value:T;
  end;

  tswmap<T> = record
  type
    tvaluearray=array of T;
    pT=^T;
  private
    fcurgroup,fcurpos:integer;
    function getcurrent: tswpair<T>;
    function dofind(hash:t_swt_hash; s:pchar;len:integer; var val: pT):boolean;
    function inserths(hash:t_swt_hash;s: pchar; len: integer; val: T): pT;
    function isequal(s:tkeystr;p:pchar;len:integer):boolean;overload;
  public
    nb_groups : uint64;
    pair_count : uint64;
    hashfun : t_swt_hashfun;
    groups :tgrouparray;
    values : tvaluearray;
    procedure init(factor: integer=1; grps: integer=SWT_GROUP_SIZE);
    function findoradd(s: pchar; len: integer;out val: pT): boolean;
    procedure expand(factor:integer);
    property count:uint64 read pair_count;
    property current:tswpair<T> read getcurrent;
    function getenumerator:tswmap<T>;
    function movenext:boolean;
  end;
  function ft_basic_hash(str:pchar;len:integer):uint64;

  const SWTI128 : t_swt_i128=(128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128);

implementation

{ tswhash }

function tswhash.get_meta: uint64;
begin
  result:=flag0 and 127;
end;

function tswhash.get_position: uint64;
begin
  result:=flag0 >> 7;
end;

{$push}
{$R-}{$O-}
function ft_basic_hash(str:pchar;len:integer ):uint64;
var
  hash:sizeuint;
  prime:sizeuint;
begin
    hash := $cbf29ce484222325;
    prime := 1099511628211;
    while len>0 do begin
      hash := hash xor ord(str^);
      hash := hash * prime;
      str:=str+1;
      len:=len-1;
    end;
    result:=hash;
end;
{$pop}

function tswmap<T>.isequal(s:tkeystr;p:pchar;len:integer):boolean;
var
  i: Integer;
begin
  if s.l<>len then exit(false);
  for i:=1 to len do begin
    if s.s^<>p^ then exit(false);
    s.s:=s.s+1;
    p:=p+1;
  end;
  result:=true
end;

{ tswmap }

procedure tswmap<T>.init(factor: integer=1; grps: integer=SWT_GROUP_SIZE);
var
  i: uint64;
begin
  pair_count :=0;
  nb_groups := factor * grps;
  hashfun := @ft_basic_hash;
  setlength(groups, nb_groups);
  SetLength(values, nb_groups * SWT_GROUP_SIZE);
  for i:=0 to nb_groups-1 do
    groups[i].control:=SWTI128;
end;


 function tswmap<T>.findoradd(s: pchar; len: integer;out  val: pT): boolean;
 var  hash:t_swt_hash;
 begin
   if pair_count >= SWT_LOAD_FACTOR * nb_groups * SWT_CONTROL_SIZE then
     expand(SWT_EXPAND_FACTOR);

   hash.s := hashfun(s, len);
   if dofind(hash,s,len,val) then exit(true);
   val := inserths(hash, s, len, default(T));
   result:=false;
end;

function tswmap<T>.inserths(hash:t_swt_hash; s: pchar; len: integer; val: T): pT;
var
     gi :uint64;
     i, st:integer;
begin
  gi := hash.h.position mod nb_groups;
  st:=1;
  while true do begin
    for i := 0 to SWT_CONTROL_SIZE-1 do
      if groups[gi].control[i] and SWT_EMPTY<>0 then begin
 	groups[gi].control[i] := hash.h.meta;
        groups[gi].key[i].s:=s;
        groups[gi].key[i].l:=len;
 	result:=@values[gi * SWT_CONTROL_SIZE + i];
        result^:=val;
 	pair_count:=pair_count+1;
        exit;
      end;
    gi := (gi + st) and (nb_groups-1);
    st:=st+1;
  end;
end;

function tswmap<T>.dofind(hash:t_swt_hash; s: pchar; len: integer; var val: pT): boolean;
var
   gi:uint64;
   i, st:integer;
   meta:integer;
   g:^t_swt_group;
begin
  gi := hash.h.position and (nb_groups-1);
  st:=1;
  meta:=hash.h.meta;
  while true do begin
    g:=@groups[gi];
    for i := 0 to SWT_CONTROL_SIZE-1 do begin
      if g.control[i] and SWT_FULL_MASK=meta then
        if isequal(g.key[i], s, len) then begin
 	  val:=@values[gi * SWT_CONTROL_SIZE + i];
          exit(true);
        end;
    end;
    if g.control[SWT_CONTROL_SIZE-1] and SWT_EMPTY<>0 then exit(false);
    gi := (gi + st) and (nb_groups-1);
    st:=st+1
  end;
end;

procedure tswmap<T>.expand(factor: integer);
var
  tmp:tswmap<T>;
  i:uint64;
  j: integer;
  g:pgroup;
  hash:t_swt_hash;
begin
  tmp.init(factor, nb_groups);
  for i := 0 to nb_groups-1 do begin
    g := @groups[i];
    for j := 0 to SWT_CONTROL_SIZE-1 do
      if g^.control[j] and SWT_EMPTY=0 then begin
        hash.s := hashfun(g.key[j].s, g.key[j].l);
        tmp.inserths(hash, g.key[j].s, g.key[j].l, values[i * SWT_CONTROL_SIZE + j])
      end;
  end;
  pair_count:=tmp.pair_count;
  nb_groups:=tmp.nb_groups;
  groups:=tmp.groups;
  values:=tmp.values;
end;

function tswmap<T>.getenumerator: tswmap<T>;
begin
  fcurgroup:=0;
  fcurpos:=-1;
  result:=self;
end;

function tswmap<T>.movenext: boolean;
begin
  repeat
    fcurpos:=fcurpos+1;
    if fcurpos=SWT_CONTROL_SIZE then begin
      fcurpos:=0;
      fcurgroup:=fcurgroup+1;
      if fcurgroup=nb_groups then exit(false);
    end;
  until groups[fcurgroup].control[fcurpos] and ord(SWT_EMPTY)=0;
  result:=true
end;

function tswmap<T>.getcurrent: tswpair<T>;
begin
  result.key:=groups[fcurgroup].key[fcurpos];
  result.value:=values[fcurgroup* SWT_CONTROL_SIZE + fcurpos]
end;

end.

