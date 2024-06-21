unit fasthashmap;

{$mode ObjFPC}{$H+}{$ModeSwitch advancedrecords}{$ModeSwitch autoderef}

interface

uses
  Classes, SysUtils;


const
  ENT_EMPTY=-1;
type
THashMapString = string[50];
TXQHashCode = uint32;
TXQHashMapCellArray = array of int32;

TXQBaseTypeInfo = object
//    class procedure markKeyAsDeleted(var key: THashMapString); static;
//    class function isKeyDeleted(const p: ppointer): boolean; static; inline;
end;
TXQDefaultTypeInfo = object(TXQBaseTypeInfo)
  class function hash(data: pchar; len: SizeUInt): TXQHashCode; static;
  class function hash(const data: THashMapString): TXQHashCode; static;
end;
type THashMapHelper = record
  class function calcCellCandidate(logsize: int32; hashcode: TXQHashCode; out mask, step: uint32): uint32; inline; static;
  class function findEmptyCellWithHash(const cells: TXQHashMapCellArray; logsize: int32; hashcode: TXQHashCode): UInt32; static;
end;
{** Hashmap based on Bero's FLRECacheHashMap

    TXQHashmapStrOwning, e.g. TXQHashmapStrStr
}
generic TXQBaseHashmap<TKey, TBaseValue, TInfo> = class
  type THashMapEntity=record
    Key: TKey;
    Value: TBaseValue;
  end;
  PHashMapEntity = ^THashMapEntity;
  PValue = ^TBaseValue;
  PXQBaseHashmap = ^TXQBaseHashmap;

private
  //if a cell with key = Key exists, return that cell; otherwise return empty cell at expected position
  function findCell(const Key: TKey): UInt32; inline;
  function findCellWithHash(const Key: TKey; hashcode: TXQHashCode): UInt32;
  function findEmptyCell(const Key: TKey): UInt32; inline;
  procedure grow;
  procedure grow(NewSize: Int32);
protected
{$if FPC_FULLVERSION <= 30004} public{$endif}
  LogSize: int32;
  Size: int32;
public
  Entities:array of THashMapEntity;
  CellToEntityIndex: TXQHashMapCellArray;
  function getBaseValueOrDefault(const Key:TKey):TBaseValue;
  procedure setBaseValue(const Key:TKey;const Value:TBaseValue);
  function include(const Key:TKey; const Value:TBaseValue; allowOverride: boolean=true):PHashMapEntity;
public
  constructor create(cap: int32);
  destructor Destroy;override;
  procedure Clear;
  procedure reserve(minSize: Int32);
  function findEntity(const Key:TKey; CreateIfNotExist:boolean=false): PHashMapEntity;
  function contains(const key: TKey): boolean;
  property values[const Key:TKey]: TBaseValue read getBaseValueOrDefault write SetBaseValue; default;
  function isEmpty: boolean; inline;
  property Count: int32 read Size;
end;

generic TXQBaseHashmapStr<TValue> = class(specialize TXQBaseHashmap<THashMapString, TValue, TXQDefaultTypeInfo>)
  type TKeyInfo = TXQDefaultTypeInfo;
private
  class procedure keyToData(const key: THashMapString; out data: pchar; out datalen: SizeUInt); static; inline;
public
  function findCell(keydata: pchar; keylen: SizeUInt): UInt32; inline;
  function findCellWithHash(keydata: pchar; keylen: SizeUInt; HashCode: TXQHashCode): UInt32;
  function findEntity(data: pchar; keylen: SizeUInt): PHashMapEntity; overload;
  function findEntityWithHash(data: pchar; keylen: SizeUInt; ahash: TXQHashCode): PHashMapEntity; overload;
end;


implementation









constructor TXQBaseHashmap.create(cap: int32);
begin
 LogSize:=0;
 Size:=0;
 Entities:=nil;
 CellToEntityIndex:=nil;
 if cap > 0 then reserve(cap);
end;

destructor TXQBaseHashmap.Destroy;
begin
 clear;
 inherited;
end;

procedure TXQBaseHashmap.Clear;
begin
 LogSize:=0;
 Size:=0;
 SetLength(Entities,0);
 SetLength(CellToEntityIndex,0);
end;

procedure TXQBaseHashmap.reserve(minSize: Int32);
begin
  if minSize <= length(CellToEntityIndex) then exit;
  grow(minSize);
end;


function TXQBaseHashmap.findCell(const Key: TKey): UInt32;
begin
  result := findCellWithHash(key, TInfo.hash(key));
end;

class function THashMapHelper.calcCellCandidate(logsize: int32; hashcode: TXQHashCode; out mask, step: uint32): uint32;
begin
 result:=HashCode shr (32-LogSize);
 Mask:=(2 shl LogSize)-1;
 Step:=((HashCode shl 1)+1) and Mask;
end;

class function THashMapHelper.findEmptyCellWithHash(const cells: TXQHashMapCellArray; logsize: int32; hashcode: TXQHashCode): UInt32;
var Mask,Step:uint32;
    Entity:int32;
begin
 if LogSize<>0 then begin
   result := calcCellCandidate(LogSize, hashcode, mask, step);
 end else begin
  result:=0;
  exit
 end;
 repeat
  Entity:=cells[result];
  if (Entity=ENT_EMPTY) then begin
   exit;
  end;
  result:=(result+Step) and Mask;
 until false;
end;

function TXQBaseHashmap.findCellWithHash(const Key: TKey; hashcode: TXQHashCode): UInt32;
var Mask,Step:uint32;
    Entity:int32;
    keylen: SizeInt;
    EntityPtr: PHashMapEntity;
    keydata: Pointer;
begin
 if LogSize<>0 then begin
   result := THashMapHelper.calcCellCandidate(LogSize, hashcode, mask, step);
 end else begin
  result:=0;
  exit
 end;
 keydata := @key[1];
 keylen := length(key);
 repeat
  Entity:=CellToEntityIndex[result];
  if Entity=ENT_EMPTY then exit;
  EntityPtr := @Entities[Entity];
  if (length(EntityPtr.Key) = keylen) and  CompareMem( @EntityPtr.Key[1], keydata, keylen )  then
   exit;
  result:=(result+Step) and Mask;
 until false;
end;

function TXQBaseHashmap.findEmptyCell(const Key: TKey): UInt32;
begin
  result := THashMapHelper.findEmptyCellWithHash(CellToEntityIndex, LogSize, TInfo.hash(key));
end;

procedure TXQBaseHashmap.grow;
begin
  grow(Size);
end;


procedure TXQBaseHashmap.grow(NewSize: Int32);
var NewLogSize,OldSize,Counter, Entity:int32;
  Cell: UInt32;
begin
 OldSize := Size;
 //set NewLogSize to number of digits in binary representation of NewSize
 NewLogSize:=0;
 while NewSize<>0 do begin
  NewSize:=NewSize shr 1;
  inc(NewLogSize);
 end;
 if NewLogSize<1 then begin
  NewLogSize:=1;
 end;
 //resize CellToEntityIndex to 2^NewLogSize (two to four times larger than size e.g. size=7 -> log=3 -> new length 16; size=8 -> log=4 -> new length 32  )
 Size:=0;
 LogSize:=NewLogSize;
 SetLength(CellToEntityIndex,2 shl LogSize);
 for Counter:=0 to length(CellToEntityIndex)-1 do begin
  CellToEntityIndex[Counter]:=ENT_EMPTY;
 end;

 //quick reinsertation
 Entity := 0;
 for Counter:=0 to OldSize-1 do
  with Entities[counter] do begin
   {$ifdef HASHMAP_SUPPORTS_MARKING_DELETIONS}if not tinfo.isKeyDeleted(@Key) then begin{$endif}
     Cell := findEmptyCell(Key);
     CellToEntityIndex[Cell]:=Entity;
     {$ifdef HASHMAP_SUPPORTS_MARKING_DELETIONS}
     if Entity <> Counter then begin
       tempPtrSized := pointer(key);
       pointer(key) := pointer(Entities[Entity].Key);
       pointer(Entities[Entity].Key) := tempPtrSized;
       Entities[Entity].Value := Value;
     end;
     {$endif}
     inc(Entity);
   {$ifdef HASHMAP_SUPPORTS_MARKING_DELETIONS}end;{$endif}
  end;
 Size := Entity;
 SetLength(Entities,2 shl LogSize);
 {$ifdef HASHMAP_SUPPORTS_MARKING_DELETIONS}
 //remove old data (not really needed)
 for Counter:=Size to min(OldSize - 1, high(Entities)) do begin
   Entities[Counter].Key:=default(TKey);
   Entities[Counter].Value:=default(TBaseValue);
 end;
 {$endif}
end;

function TXQBaseHashmap.include(const Key: TKey; const Value: TBaseValue; allowOverride: boolean): PHashMapEntity;
var Entity:int32;
    Cell:uint32;
begin
 result:=nil;
 if Size+1>=(1 shl LogSize) then begin
   grow;
   assert(size < 1 shl LogSize);
 end;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];;
 if Entity>=0 then begin
  result:=@Entities[Entity];
  if not allowOverride then exit;
 end else begin
   Entity:=Size;
   inc(Size);
   assert(Entity<2 shl LogSize);
   CellToEntityIndex[Cell]:=Entity;
   result:=@Entities[Entity];
 end;
 result^.Key:=Key;
 result^.Value:=Value;
end;


function TXQBaseHashmap.findEntity(const Key:TKey;CreateIfNotExist:boolean=false):PHashMapEntity;
var Entity:int32;
    Cell:uint32;
begin
 Cell:=FindCell(Key);
 if CellToEntityIndex <> nil then begin
   Entity:=CellToEntityIndex[Cell];
   if Entity>=0 then
    exit(@Entities[Entity]);
 end;

 if CreateIfNotExist then begin
   if Size+1>=(1 shl LogSize) then result:=include(Key,default(TBaseValue))
   else begin
     Entity:=Size;
     inc(Size);
     assert(Entity<2 shl LogSize);
     CellToEntityIndex[Cell]:=Entity;
     result:=@Entities[Entity];
     result^.Key:=Key;
     result^.value:=default(TBaseValue)
   end;
  end else
   result:=nil;
end;


function TXQBaseHashmap.contains(const key: TKey): boolean;
begin
  result := findEntity(key) <> nil;
end;


function TXQBaseHashmap.isEmpty: boolean;
begin
  result := Count = 0;
end;

function TXQBaseHashmap.getBaseValueOrDefault(const Key: TKey): TBaseValue;
var Entity:int32;
    Cell:uint32;
begin
 Cell:=FindCell(Key);
 if CellToEntityIndex <> nil then begin
   Entity:=CellToEntityIndex[Cell];
   if Entity>=0 then
    exit(Entities[Entity].Value);
 end;
 result:=default(TBaseValue);
end;

procedure TXQBaseHashmap.setBaseValue(const Key: TKey; const Value: TBaseValue);
begin
  include(Key,Value);
end;


{$PUSH}{$RangeChecks off}{$OverflowChecks off}
class function TXQDefaultTypeInfo.hash(data: pchar; len: SizeUInt): uint32;
{var
  p, last: PByte;
begin
  if len = 0 then exit(1);
  p := pbyte(data);
  last := p + len;
  result := 0;
  while p < last do begin
    result := result + p^;
    result := result + (result shl 10);
    result := result xor (result shr 6);
    inc(p);
  end;

  result := result + (result shl 3);
  result := result xor (result shr 11);
  result := result + (result shl 15);
end;          }

type

     TFLREUInt8 = uint8;
     TFLREUInt16 = uint16;
     TFLREUInt32 = uint32;
     TFLREUInt64 = uint64;
     PFLRERawByteChar = pchar;
const m=TFLREUInt32($57559429);
      n=TFLREUInt32($5052acdb);
var b:PFLRERawByteChar;
    h,k:TFLREUInt32;
    o,p:TFLREUInt64;
begin
 h:=Len;
 k:=h+n+1;
  b:=Data;

 if len > 7 then begin
   o := TFLREUInt64(pointer(b)^);
   p:= TFLREUInt32(o and $ffffffff)*TFLREUInt64(n);
   h:=h xor TFLREUInt32(p and $ffffffff);
   k:=k xor TFLREUInt32(p shr 32);
   p:=TFLREUInt32(o shr 32)*TFLREUInt64(m);
   k:=k xor TFLREUInt32(p and $ffffffff);
   h:=h xor TFLREUInt32(p shr 32);
   if len > 8 then begin
     b := data + len - 8;
     o := TFLREUInt64(pointer(b)^);
     p:= TFLREUInt32(o and $ffffffff)*TFLREUInt64(n);
     h:=h xor TFLREUInt32(p and $ffffffff);
     k:=k xor TFLREUInt32(p shr 32);
     p:=TFLREUInt32(o shr 32)*TFLREUInt64(m);
     k:=k xor TFLREUInt32(p and $ffffffff);
     h:=h xor TFLREUInt32(p shr 32);
   end;
 end else begin
   if len > 3 then begin
   p:=TFLREUInt32(pointer(b)^)*TFLREUInt64(n);
   h:=h xor TFLREUInt32(p and $ffffffff);
   k:=k xor TFLREUInt32(p shr 32);
   inc(b,4);
   dec(Len,4);
   end;

  if Len>0 then begin
   if Len>1 then begin
    p:=TFLREUInt16(pointer(b)^);
    inc(b,2);
    dec(Len,2);
   end else begin
    p:=0;
   end;
   if Len>0 then begin
    p:=p or (TFLREUInt8(b^) shl 16);
   end;
   p:=p*TFLREUInt64(m);
   k:=k xor TFLREUInt32(p and $ffffffff);
   h:=h xor TFLREUInt32(p shr 32);
  end;
 end;
 result:=k xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;            {
type

     TFLREUInt8 = uint8;
     TFLREUInt16 = uint16;
     TFLREUInt32 = uint32;
     TFLREUInt64 = uint64;
     PFLRERawByteChar = pchar;
const m=TFLREUInt32($57559429);
      n=TFLREUInt32($5052acdb);
var b:PFLRERawByteChar;
    h,k:TFLREUInt32;
    p:TFLREUInt64;
begin
 h:=Len;
 k:=h+n+1;
 if Len>0 then begin
  b:=Data;
  while Len>7 do begin
   begin
    p:=TFLREUInt32(pointer(b)^)*TFLREUInt64(n);
    h:=h xor TFLREUInt32(p and $ffffffff);
    k:=k xor TFLREUInt32(p shr 32);
    inc(b,4);
   end;
   begin
    p:=TFLREUInt32(pointer(b)^)*TFLREUInt64(m);
    k:=k xor TFLREUInt32(p and $ffffffff);
    h:=h xor TFLREUInt32(p shr 32);
    inc(b,4);
   end;
   dec(Len,8);
  end;
  if Len>3 then begin
   p:=TFLREUInt32(pointer(b)^)*TFLREUInt64(n);
   h:=h xor TFLREUInt32(p and $ffffffff);
   k:=k xor TFLREUInt32(p shr 32);
   inc(b,4);
   dec(Len,4);
  end;
  if Len>0 then begin
   if Len>1 then begin
    p:=TFLREUInt16(pointer(b)^);
    inc(b,2);
    dec(Len,2);
   end else begin
    p:=0;
   end;
   if Len>0 then begin
    p:=p or (TFLREUInt8(b^) shl 16);
   end;
   p:=p*TFLREUInt64(m);
   k:=k xor TFLREUInt32(p and $ffffffff);
   h:=h xor TFLREUInt32(p shr 32);
  end;
 end;
 begin
  p:=(h xor (k+n))*TFLREUInt64(n);
  h:=h xor TFLREUInt32(p and $ffffffff);
  k:=k xor TFLREUInt32(p shr 32);
 end;
 result:=k xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
// }
class function TXQDefaultTypeInfo.hash(const data: THashMapString): uint32;
begin
  result := hash(@data[1], length(data));
end;


class procedure TXQBaseHashmapStr.keyToData(const key: THashMapString; out data: pchar; out datalen: SizeUInt);
begin
  data := @key[1];
  datalen := length(key)
end;

function TXQBaseHashmapStr.findCell(keydata: pchar; keylen: SizeUInt): UInt32;
begin
 result := findCellWithHash(keydata, keylen, TKeyInfo.hash(keydata, keylen));
end;

function TXQBaseHashmapStr.findCellWithHash(keydata: pchar; keylen: SizeUInt; HashCode: TXQHashCode): UInt32;
var Mask,Step:uint32;
    Entity:int32;
    EntityPtr: PHashMapEntity;
begin
 if LogSize<>0 then begin
   result := THashMapHelper.calcCellCandidate(LogSize, hashcode, mask, step);
 end else begin
  result:=0;
  exit
 end;
 repeat
  Entity:=CellToEntityIndex[result];
  if Entity=ENT_EMPTY then exit;
  EntityPtr := @Entities[Entity];
  if (length(EntityPtr.Key) = keylen) and  CompareMem( @EntityPtr.Key[1], keydata, keylen )  then
   exit;
  result:=(result+Step) and Mask;
 until false;
end;

function TXQBaseHashmapStr.findEntity(data: pchar; keylen: SizeUInt): PHashMapEntity;
begin
  result := findEntityWithHash(data, keylen, tkeyinfo.hash(data, keylen));
end;

function TXQBaseHashmapStr.findEntityWithHash(data: pchar; keylen: SizeUInt; ahash: UInt32): PHashMapEntity;
var Entity:int32;
    Cell:uint32;
begin
 Cell:=findCellWithHash(data, keylen, ahash);
 if CellToEntityIndex <> nil then begin
   Entity:=CellToEntityIndex[Cell];
   if Entity>=0 then
    exit(@Entities[Entity]);
 end;
 result:=nil;
end;
end.

