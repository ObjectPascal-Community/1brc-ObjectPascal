unit UHashes;

{$I SCL.inc}

interface

uses
  UNumber, UString, UxxHash, UxxHashLibraryStatic;

function xxHash32C(P: PChar; L: NChar): U32; inline;
function FNV1a32(P: PChar; L: NChar): U32; inline;
function FNV1a32Custom(P: PChar; L: NChar): U32; inline;
function crc32csse42(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
function crc32c(P: PChar; L: U32): U32;
function crc32c2(P: PChar; L: U32): U32;
function xxHash(P: PByte; L: U32): U32; inline;

implementation

function xxHash32C(P: PChar; L: NChar): U32;
begin
  Result := XXH3_64bits(P, L) shr 32;
end;

function FNV1a32(P: PChar; L: NChar): U32;
var
  C: Ind;
begin
  Result := 2166136261;
  for C := 0 to L - 1 do
    Result := (Result xor U8(P[C])) * 16777619;
end;

function FNV1a32Custom(P: PChar; L: NChar): U32;
begin
  Result := 2166136261;
  while L >= 4 do
  begin
    Result := (Result xor PU32(P)^) * 16777619;
    P += 3;
    L -= 3;
  end;
  while L >= 2 do
  begin
    Result := (Result xor PU16(P)^) * 16777619;
    P += 2;
    L -= 2;
  end;
  if L = 1 then
    Result := (Result xor PU8(P)^) * 16777619;
end;

//From the great tiny mORMot
function crc32csse42(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal; nostackframe; assembler;
asm
         mov     eax, crc
         test    len, len
         jz      @z
         test    buf, buf
         jz      @z
         not     eax
         mov     ecx, len
         shr     len, 3
         jnz     @by8 // no read alignment care here - but in crypto.core
         @0:
         test    cl, 4
         jz      @4
         crc32   eax, dword ptr [buf]
         add     buf, 4
         @4:
         test    cl, 2
         jz      @2
         crc32   eax, word ptr [buf]
         add     buf, 2
         @2:
         test    cl, 1
         jz      @1
         crc32   eax, byte ptr [buf]
         @1:
         not     eax
         @z:
         ret
         align   16
         @by8:
         crc32   rax, qword ptr [buf] // hash 8 bytes per loop
         add     buf, 8
         sub     len, 1
         jnz     @by8
         jmp     @0
end;

function crc32c(P: PChar; L: U32): U32; nostackframe; assembler;
asm
         MOV     EDX, EDX
         LEA     R9, [RCX + 8]
         LEA     R8, [RCX+RDX]
         CMP     R8, R9
         JB      @L7
         MOV     RCX, R9
         XOR     EAX, EAX
         @L3:
         CRC32   RAX, QWORD PTR [RCX - 8]
         ADD     RCX, 8
         CMP     R8, RCX
         JNB     @L3
         SUB     RDX, 8
         AND     RDX, -8
         LEA     RCX, [R9+RDX]
         @L2:
         LEA     RDX, [RCX + 4]
         CMP     R8, RDX
         JB      @L4
         CRC32   EAX, DWORD PTR [RCX]
         MOV     RCX, RDX
         @L4:
         CMP     RCX, R8
         JNB     @L1
         @L6:
         CRC32   EAX, BYTE PTR [RCX]
         INC     RCX
         CMP     R8, RCX
         JNE     @L6
         @L1:
         RET
         @L7:
         XOR     EAX, EAX
         JMP     @L2
end;

function crc32c2(P: PChar; L: U32): U32; nostackframe; assembler;
asm
         mov     r8d, edx
         and     r8d, 3
         xor     eax, eax
         cmp     r8d, 1
         je      @LBB0_5
         cmp     r8d, 2
         je      @LBB0_4
         cmp     r8d, 3
         jne     @LBB0_6
         xor     eax, eax
         crc32   eax, word ptr [rcx]
         crc32   eax, byte ptr [rcx + 2]
         add     rcx, 3
         cmp     edx, 4
         jae     @LBB0_7
         jmp     @LBB0_13
         @LBB0_5:
         crc32   eax, byte ptr [rcx]
         inc     rcx
         @LBB0_6:
         cmp     edx, 4
         jae     @LBB0_7
         jmp     @LBB0_13
         @LBB0_4:
         crc32   eax, word ptr [rcx]
         add     rcx, 2
         cmp     edx, 4
         jb      @LBB0_13
         @LBB0_7:
         shr     edx, 2
         lea     r8d, [rdx - 1]
         cmp     r8d, 7
         jb      @LBB0_10
         mov     r8d, edx
         and     r8d, -8
         @LBB0_9:
         crc32   eax, dword ptr [rcx]
         crc32   eax, dword ptr [rcx + 4]
         crc32   eax, dword ptr [rcx + 8]
         crc32   eax, dword ptr [rcx + 12]
         crc32   eax, dword ptr [rcx + 16]
         crc32   eax, dword ptr [rcx + 20]
         crc32   eax, dword ptr [rcx + 24]
         crc32   eax, dword ptr [rcx + 28]
         add     rcx, 32
         add     r8d, -8
         jne     @LBB0_9
         @LBB0_10:
         test    dl, 7
         je      @LBB0_13
         and     edx, 7
         xor     r8d, r8d
         @LBB0_12:
         crc32   eax, dword ptr [rcx + 4*r8]
         inc     r8
         cmp     edx, r8d
         jne     @LBB0_12
         @LBB0_13:
         ret
end;

const
  Prime1 = 2654435761;
  Prime2 = 2246822519;
  Prime3 = 3266489917;
  Prime4 = 668265263;
  Prime5 = 374761393;

function xxHashStep(V, D: U32): U32; inline;
begin
  V := V + (D * Prime2);
  V := (V shl 13) or (V shr (32 - 13));
  V := V * Prime1;
  Result := V;
end;

function xxHash(P: PByte; L: U32): U32;
var
  I, J: Ind;
  V1, V2, V3, V4: U32;
begin
  if L >= 16 then
  begin
    V1 := Prime1 + Prime2;
    V2 := Prime2;
    V3 := 0;
    V4 := Prime1;

    for I := 0 to (L shr 4) - 1 do
    begin
      V1 := xxHashStep(V1, PU32(P)[0]);
      V2 := xxHashStep(V2, PU32(P)[1]);
      V3 := xxHashStep(V3, PU32(P)[2]);
      V4 := xxHashStep(V4, PU32(P)[3]);
      P += 16;
    end;

    Result := ((V1 shl 1) or (V1 shr (32 - 1))) + ((V2 shl 7) or (V2 shr (32 - 7))) +
      ((V3 shl 12) or (V3 shr (32 - 12))) + ((V4 shl 18) or (V4 shr (32 - 18)));
    L := L and 15;
  end
  else
    Result := Prime5;

  Result += L;

  while L >= 4 do
  begin
    Result += PU32(P)^ * Prime3;
    Result := ((Result shl 17) or (Result shr (32 - 17))) * Prime4;
    P += 4;
    L -= 4;
  end;

  while L > 0 do
  begin
    Result += P^ * Prime5;
    Result := ((Result shl 11) or (Result shr (32 - 11))) * Prime1;
    P += 1;
    L -= 1;
  end;

  Result := Result xor (Result shr 15);
  Result := Result * Prime2;
  Result := Result xor (Result shr 13);
  Result := Result * Prime3;
  Result := Result xor (Result shr 16);
end;

end.
