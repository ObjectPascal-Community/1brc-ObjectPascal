program wcontest;
{$mode delphi}

uses
  cthreads,
  SysUtils,
  Classes,
  swizmap,
  Math,
  fpthreadpool,
  BaseUnix,
  Generics.Collections,
  Generics.Defaults;

const
  lineendinglength = 1;
type

  { tdataitem }

  tdataitem = record
    mx, mn, cnt, sm: integer;
  end;

  tsortitem = record
    key: tkeystr;
    Data: tdataitem;
  end;

  pdataitem = ^tdataitem;
  THashTable = tswmap<tdataitem>;

  tthreaddata = record
    a, e: PChar;
    x: THashTable;
  end;
  pthreaddata = ^tthreaddata;

  { tstreamhlp }

  tstreamhlp = class helper for tstream
    procedure writestr(s: string; sep: char = #0);
  end;

  { t1brtask }

  t1brtask = class(TThreadPoolTask)
    fdata: pthreaddata;
    procedure doExecute; override;
  end;

  { tcmp }

  tcmp = class(tComparer<tsortitem>)
    function Compare(const Left, Right: tsortitem): integer; override;
  end;


var
  results: array of pthreaddata;
  infile: string;
  threadc: longint;

  { tstreamhlp }

  procedure tstreamhlp.writestr(s: string; sep: char = #0);
  begin
    Write(s[1], Length(s));
    if sep <> #0 then WriteByte(Ord(sep));
  end;

  procedure threadfunc(par: pthreaddata);
  var
    nega: boolean;
    tmp: int64;
    n: PChar;
    it: pdataitem;
    bf, en: PChar;
    x: THashTable;
  begin
    bf := par.a;
    en := par.e;
    x.init;
    while True do
    begin
      n := strpos(bf, ';');
      if n = nil then break;
      if not x.FindOrAdd(bf, n - bf, it) then
      begin
        it.mn := 1000;
        it.mx := -1000;
      end;
      nega := n[1] = '-';
      if nega then n := n + 2
      else
        n := n + 1;
      if n[1] = '.' then
      begin
        tmp := (Ord(n^) - 48) * 10 + Ord(n[2]) - 48;
        bf := n + 3 + lineendinglength;
      end
      else
      begin
        tmp := (Ord(n^) - 48) * 100 + (Ord(n[1]) - 48) * 10 + Ord(n[3]) - 48;
        bf := n + 4 + lineendinglength;
      end;
      if nega then tmp := -tmp;
      it.mx := max(it.mx, tmp);
      it.mn := min(it.mn, tmp);
      it.sm := it.sm + tmp;
      it.cnt := it.cnt + 1;
      if bf >= en then break;
    end;
    par.x := x;
  end;

  { t1brtask }

  procedure t1brtask.doExecute;
  begin
    threadfunc(fdata);
  end;

  function tcmp.Compare(const Left, Right: tsortitem): integer;
  var
    i: integer;
    a, b: tkeystr;
  begin
    a := left.key;
    b := right.key;
    for i := 1 to min(a.l, b.l) do
    begin
      if a.s^ < b.s^ then exit(-1);
      if a.s^ > b.s^ then exit(1);
      Inc(a.s);
      Inc(b.s);
    end;
    Result := a.l - b.l;
  end;

  procedure output(sum: THashTable);
  var
    stream: tstream;
    keys: array of tsortitem;
    ditem: tdataitem;
    pair: tswpair<tdataitem>;
    eka: boolean;
    s: string;
    k: tsortitem;
    cmp: tcmp;
    i: integer;
  begin
    i := 0;
    keys:=nil;
    SetLength(keys, sum.Count);
    for pair in sum do
    begin
      keys[i].key := pair.key;
      keys[i].Data := pair.Value;
      i := i + 1;
    end;
    cmp := tcmp.Create;
    TArrayHelper<tsortitem>.sort(keys, cmp);
    cmp.Free;
    stream := THandleStream.Create(1);
    stream.WriteByte(Ord('{'));
    eka := True;
    for k in keys do
    begin
      if not eka then stream.writestr(', ')
      else
        eka := False;
      stream.Write(k.key.s^, k.key.l);
      stream.WriteByte(Ord('='));
      ditem := k.Data;
      s := formatfloat('0.0', ditem.mn / 10);
      stream.Writestr(s, '/');
      s := formatfloat('0.0', ceil(ditem.sm / ditem.cnt) / 10);
      stream.Writestr(s, '/');
      s := formatfloat('0.0', ditem.mx / 10);
      stream.Writestr(s);
    end;
    stream.WriteByte(Ord('}'));
    stream.WriteByte(10);
    stream.Free;
  end;

  procedure main(filin: string; threadc: integer);
  var
    i, bc, bs: PtrInt;
    sz: int64;
    sum: THashTable;
    f: TFileStream;
    en, rp, pmemmap, lp: PChar;
    Data: pthreaddata;
    final: boolean;
    pool: TFPSimpleThreadPool;
    tt: t1brtask;
    k: tswpair<tdataitem>;
    it: pdataitem;
  begin
    pool := TFPSimpleThreadPool.Create;
    pool.MinThreads:=1;
    if threadc > 0 then pool.MaxThreads := threadc;
    bc := 256;
    i := 0;
    f := TfileStream.Create(filin, fmOpenRead);
    sz := f.Size;
    pmemmap := fpmmap(nil, sz, PROT_READ, MAP_SHARED, f.handle, 0);
    rp := pmemmap;
    lp := rp + sz;
    f.Free;
    SetLength(results, bc + 1);
    bs := sz div bc;
    repeat
      final := rp + bs > lp;
      if final then en := lp - 1
      else
        en := strpos(rp + bs, LineEnding);
      if en = nil then break;
      Data := new(pthreaddata);
      results[i] := Data;
      Data.a := rp;
      Data.e := en + 1;
      tt := t1brtask.Create;
      tt.fdata := Data;
      pool.AddTask(tt);
      i := i + 1;
      rp := en + 1;
    until final;
    while pool.BusyThreadCount > 0 do ;
    pool.Free;
    sum := results[0].x;
    bc := i;
    for i := 1 to bc - 1 do
    begin
      for k in results[i].x do
      begin
        sum.findoradd(k.key.s, k.key.l, it);
        if k.Value.mx > it^.mx then it^.mx := k.Value.mx;
        it.cnt := it.cnt + k.Value.cnt;
        it.mn := min(k.Value.mn, it.mn);
        it.mx := max(k.Value.mx, it.mx);
        it.sm := it.sm + k.Value.sm;
      end;
      dispose(results[i]);
    end;
    output(sum);
    dispose(results[0]);
    Fpmunmap(pmemmap, sz);
  end;

  procedure printhelp;
  begin
    writeln(format('Usage: input_file [threadcount (default %d)]', [GetCPUCount]));
  end;

begin
  infile := '';
  threadc := GetCPUCount;
  repeat
    if ParamCount > 0 then
    begin
      infile := ParamStr(1);
      if not FileExists(infile) then
      begin
        writeln('Invalid filename ', ParamStr(1));
        break;
      end;
    end
    else
      break;
    if ParamCount > 1 then threadc := strtointdef(ParamStr(2), 0);
    if threadc < 1 then
    begin
      writeln('Invalid thread count ', ParamStr(2));
      break;
    end;
    main(infile, threadc);
    exit;
  until False;
  printhelp;
end.
