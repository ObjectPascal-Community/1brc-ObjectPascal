unit mmaputils;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils;

type TMemoryMappedFile = record
  data: pointer;
  size: PtrUInt;
  class function loadFromFile(const filename: string): TMemoryMappedFile; static;
  class function prepareLoadFromStdin(asize: int64): TMemoryMappedFile; static;
  procedure destroy;
end;
  {$ifdef unix}
  TSequentialBlock = record
    data: pointer;
    size: PtrUInt;

    actualData: pointer;
    actualSize: PtrUInt;
    procedure init(adata: pointer; asize: PtrUInt);
    procedure preload(untilAddr: pointer);
    procedure forget(beforeAddr: pointer);
  end;
  {$endif}
const PAGESIZE = 4096;

procedure madviseSequential(data: pointer; size: ptruint);

implementation
uses {$ifdef unix}baseunix,syscall{$endif}
     {$ifdef windows}windows{$endif}
  ;
{$ifdef unix}
class function TMemoryMappedFile.loadFromFile(const filename: string): TMemoryMappedFile;
  function checkcerr(e: cint): cint;
  begin
    result := e;
    if e < 0 then begin
      raise EInOutError.create('error opening inputfile '+filename+ ' ' + inttostr(e)+ ' '+inttostr(errno));
    end;
  end;
var
  fd: cint;
  statbuf: stat;
begin
  result := default(TMemoryMappedFile);
  fd := checkcerr(fpOpen(filename, O_RDONLY));
  checkcerr(FpStat(filename, statbuf));
//  writeln(stderr, INPUTFILE, ' ',statbuf.st_size, ' bytes');
  result.data := Fpmmap(nil, statbuf.st_size, PROT_READ, MAP_SHARED, fd, 0);
  result.size := statbuf.st_size;
  if ptruint(result.data) = ptruint(-1) then checkcerr(-1);
  FpClose(fd);
end;

const STDIN_PASSING = 2*PAGESIZE;

class function TMemoryMappedFile.prepareLoadFromStdin(asize: int64): TMemoryMappedFile;
begin
  result.data := Fpmmap(nil, asize + STDIN_PASSING, PROT_READ or PROT_WRITE, MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
  if ptruint(result.data) = ptruint(-1) then writeln(stderr, 'mmap failed');
  result.size := asize;
end;

procedure TMemoryMappedFile.destroy;
begin
  Fpmunmap(data, size);
end;


procedure TSequentialBlock.init(adata: pointer; asize: PtrUInt);
begin
  data := adata;
  size := asize+ STDIN_PASSING;
  actualData := data;
  actualSize:= 0;
  preload(data+PAGESIZE);
end;

procedure madviseSequential(data: pointer; size: ptruint);
begin
  if Do_SysCall(syscall_nr_madvise, ptrint(data) and not ptruint(PAGESIZE - 1), size, {MADV_SEQUENTIAL} 2) < 0 then
    writeln(stderr, 'warning: madvise failed ',errno);
end;

procedure TSequentialBlock.preload(untilAddr: pointer);
var buffer: array[0..PAGESIZE] of Byte;
    count: int64;

begin
  while (actualData + actualSize <= untilAddr)  do begin
    count := PAGESIZE;
    count := FpRead(StdInputHandle, buffer, count);
//    writeln(stderr, count);
    if count > 0 then begin
      move(buffer[0], pchar(actualdata)[actualSize], count);
//      writeln('read: ',pchar(@buffer[0]));
      actualSize += count;
    end
    else if count = 0 then begin
//      writeln(stderr, 'skiP: ', (data  - actualdata) + (size - actualSize) );
      FillChar(pchar(actualData)[actualSize], (data  - actualdata) + (size - actualSize) , #10); // actualData + actualSize + filled = data + size
      actualSize := data - actualdata + size;
    end else if count < 0 then sleep(1);
  end;
end;

procedure TSequentialBlock.forget(beforeAddr: pointer);
begin
  while actualData + PAGESIZE < beforeAddr do begin
    Fpmunmap(actualData, PAGESIZE);
    actualData += PAGESIZE;
    actualSize -= PAGESIZE;
  end;
end;
{$endif}

{$ifdef windows}
class function TMemoryMappedFile.loadFromFile(const filename: string): TMemoryMappedFile;
var
  fs: TFileStream;
  fh: HANDLE;
  highsize: DWORD;
begin
 // fh := CreateFile(pchar(filename), GENERIC_READ, 0, nil, OPEN_ALWAYS  , 0, 0);
  fs := TFileStream.Create(filename, fmOpenRead);
  result.size := fs.Size;
  fh := CreateFileMapping(fs.Handle, nil, PAGE_READONLY, 0, 0,nil);
  //result.size := GetFileSize(fh, @highsize);
 // writeln(filename, ' ',result.size, ' ',fh);
  result.data := MapViewOfFile(fh, FILE_MAP_READ, 0,0,result.size);
  if result.data = nil then begin
    writeln('MapViewOfFile failed: ',GetLastError);
    halt;
  end;
//  writeln(pchar(result.data));
 // writeln(result.size);
end;
class function TMemoryMappedFile.prepareLoadFromStdin(asize: int64): TMemoryMappedFile;
begin
end;
procedure TMemoryMappedFile.destroy;
begin
  UnmapViewOfFile(data);
end;
procedure madviseSequential(data: pointer; size: ptruint);
begin

end;

{$endif}
end.


