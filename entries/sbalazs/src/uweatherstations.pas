unit uWeatherStations;

{$mode ObjFPC}{$H+}
{$R-} {$Q-}
{$INLINE ON}

interface

uses
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}Classes, SysUtils, Math, syncobjs, Baseline.Common;

const
  HashBuckets = 1 shl 17;

type
  TWSManager = class;

  TData = record
    FMin: Int64;
    FMax: Int64;
    FTot: Int64;
    FCnt: Integer;
    FHash: QWord;
  end;

  TWS = record
    FStation: TBytes;
    FData: TData;
  end;

  TWSList = array[0..HashBuckets - 1] of TWS;

  TLock = class
  private
    FCS: TRTLCriticalSection;
    FLocked: Integer;
    function Lock: Boolean;
    procedure Unlock;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TLockList = class(TLock)
  private
    FList: TList;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TUpdateType = (utAdd, utRemove);
  TWSThreadBase = class(TThread)
  private
    FEvent: TEvent;
    FStarted: Boolean;
    FWSManager: TWSManager;
    procedure Wait(AMs: Integer);
    procedure AddLineBreak(var ABytes: TBytes);
    function GetThreadCnt: Integer;
    function GetNextLineBreak(AST: TStream): TBytes;
  protected
    procedure TerminatedSet; override;
  public
    constructor Create(AWSManager: TWSManager);
    destructor Destroy; override;
  end;

  TWSThread = class(TWSThreadBase)
  private
    FStartP: Int64;
    FEndP: Int64;
    FWSList: TWSList;
    procedure UpdateMainHashList;
    procedure ProcessBytes(ABytes: TBytes); inline;
    procedure AddToHashList(ABytes: TBytes; AStartP, AEndP: Int64; ATemp: Int64; AHash: QWord);
    procedure UpdateThreadList(AUpdateType: TUpdateType);
  protected
    procedure Execute; override;
  public
    constructor Create(AStartP, AEndP: Int64; AWSManager: TWSManager);
    destructor Destroy; override;
  end;

  TWSThreadsWatcher = class(TWSThreadBase)
  private
    procedure CreateFinalList;
  protected
    procedure Execute; override;
  public
    constructor Create(AWSManager: TWSManager);
  end;

  TWSManager = class
  private
    FDone: Boolean;
    FSrcFile: String;
    FThreadCnt: Integer;
    FTerminated: Boolean;
    FThreadList: TLockList;
    FWSListALL: TWSList;
    FWSThreadsWatcher: TWSThreadsWatcher;
  public
    constructor Create(ASrcFile: String; AThreadCnt: Integer);
    destructor Destroy; override;
  public
    property WSThreadsWatcher: TWSThreadsWatcher read FWSThreadsWatcher;
    property Done: Boolean read FDone;
  end;

implementation

{ TLock }
constructor TLock.Create;
begin
  FLocked := 0;
  InitCriticalSection(FCS);
end;

destructor TLock.Destroy;
begin
  DoneCriticalSection(FCS);
  inherited Destroy;
end;

function TLock.Lock: Boolean;
begin
  Result := False;
  if FLocked = 1 then
    Exit;
  FLocked := 1;
  EnterCriticalSection(FCS);
  Result := True;
end;

procedure TLock.Unlock;
begin
  LeaveCriticalSection(FCS);
  FLocked := 0;
end;

{ LockList }
constructor TLockList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TLockList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TWSThreadBase.Wait(AMs: Integer);
begin
  FEvent.WaitFor(AMs);
end;

procedure TWSThreadBase.AddLineBreak(var ABytes: TBytes);
var
  Len: LongInt;
begin
  Len := Length(ABytes);
  if ABytes[Len - 1] <> 10 then
  begin
    SetLength(ABytes, Len + 1);
    ABytes[Len] := 10;
  end;
end;

function TWSThreadBase.GetThreadCnt: Integer;
begin
  Result := -1;
  if FWSManager.FThreadList.Lock then
  try
    Result := FWSManager.FThreadList.FList.Count;
  finally
    FWSManager.FThreadList.Unlock;
  end;
end;

function TWSThreadBase.GetNextLineBreak(AST: TStream): TBytes;
var
  ReadCnt: LongInt;
  P, I: Integer;
begin
  Result := nil;
  if AST.Position = AST.Size then
    Exit;
  SetLength(Result, 100);
  ReadCnt := AST.Read(Result[0], 100);
  if ReadCnt > 0 then
  begin
    P := 0;
    AST.Position := AST.Position - ReadCnt;
    for I := Low(Result) to High(Result) do
    begin
      if (Result[I] = 10) or (Result[I] = 0) then
      begin
        P := I;
        Break;
      end;
    end;
    if P > 0 then
    begin
      SetLength(Result, P + 1);
      AST.Position := AST.Position + P + 1;
      if AST.Position > AST.Size then
         AST.Position := AST.Size;
    end
    else
      Result := nil;
  end;
end;

procedure TWSThreadBase.TerminatedSet;
begin
  if FStarted then
    FEvent.SetEvent;
end;

constructor TWSThreadBase.Create(AWSManager: TWSManager);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FWSManager := AWSManager;
  FEvent := TEvent.Create(nil, True, False, '');
end;

destructor TWSThreadBase.Destroy;
begin
  FEvent.Free;
  inherited Destroy;
end;

{ TWSThread }
constructor TWSThread.Create(AStartP, AEndP: Int64; AWSManager: TWSManager);
begin
  inherited Create(AWSManager);
  FStartP := AStartP;
  FEndP := AEndP;
  FStarted := False;
end;

destructor TWSThread.Destroy;
begin
  inherited Destroy;
end;

procedure TWSThread.UpdateThreadList(AUpdateType: TUpdateType);
begin
  while (not Terminated) do
  begin
    if FWSManager.FThreadList.Lock then
    begin
      try
        case AUpdateType of
          utAdd:
             FWSManager.FThreadList.FList.Add(Self);
          utRemove:
             begin
               UpdateMainHashList;
               FWSManager.FThreadList.FList.Remove(Self);
            end;
        end;
      finally
        FWSManager.FThreadList.Unlock;
      end;
      Break;
    end;
    Wait(Random(100));
  end;
end;

procedure TWSThread.ProcessBytes(ABytes: TBytes);
var
  PCur: Int64;
  PBeg: Int64;
  PDel: Int64;
  Len: Int64;
  Temp: Int64;
  DoHash: Boolean;
  DoTemp: Boolean;
  IsNeg: Boolean;
  Hash: QWord;
begin
  PCur := -1;
  PBeg := 0;
  PDel := 0;
  Temp := 1000;
  DoHash := True;
  DoTemp := False;
  Len := Length(ABytes);
  Hash := 14695981039346656037; //FNV (Fowler-Noll-Vo)
  repeat
    Inc(PCur);
    if DoTemp then
    begin
      IsNeg := False;
      if ABytes[PCur] = 45 then
      begin
        Inc(PCur);
        IsNeg := True;
      end;
      Temp := Ord(ABytes[PCur]) - Ord('0');
      Inc(PCur);
      if ABytes[PCur] <> 46 then
      begin
        Temp := Temp*10 + (Ord(ABytes[PCur]) - Ord('0'));
        Inc(PCur)
      end;
      Inc(PCur);
      Temp := Temp*10 +  Ord(ABytes[PCur]) - Ord('0');
      if IsNeg then
        Temp := -Temp;
      Inc(PCur);
      DoTemp := False;
    end;
    if ABytes[PCur] = 59 then
    begin
      PDel := PCur;
      DoHash := False;
      DoTemp := True;
      IsNeg := False;
    end;
    if DoHash and (ABytes[PCur] <> 10) then
    begin
      Hash := Hash xor QWord(ABytes[PCur]); // FNV-1a is XOR then *
      Hash := Hash * 1099511628211;
    end;
    if (ABytes[PCur] = 10) and (PCur < Len) then
    begin
      if (Temp < 1000) then
        AddToHashList(ABytes, PBeg, PDel, Temp, Hash);
      Hash := 14695981039346656037;
      DoHash := True;
      Temp := 1000;
      DoTemp := False;
      PBeg := PCur + 1;
    end;
  until (PCur >= Len);
end;

procedure TWSThread.Execute;
var
  I: Integer;
  Bytes: TBytes;
  ReadCnt: LongInt;
  FS: TFileStream;
begin
  UpdateThreadList(utAdd);
  try
    FStarted := True;
    for I := Low(FWSList) to High(FWSList) do
    begin
      FWSList[I].FStation := nil;
      FWSList[I].FData.FMin := 0;
      FWSList[I].FData.FMax := 0;
      FWSList[I].FData.FTot := 0;
      FWSList[I].FData.FCnt := 0;
    end;
    Bytes := nil;
    FS := TFileStream.Create(FWSManager.FSrcFile, fmOpenRead or fmShareDenyWrite);
    try
      FS.Position := FStartP;
      SetLength(Bytes, FEndP - FStartP);
      ReadCnt := FS.Read(Bytes[0], FEndP - FStartP);
      if ReadCnt > 0 then
        ProcessBytes(Bytes);
    finally
      FS.Free;
    end;
  finally
    UpdateThreadList(utRemove);
  end;
end;

procedure TWSThread.AddToHashList(ABytes: TBytes; AStartP, AEndP: Int64;
  ATemp: Int64; AHash: QWord);
var
  Index: Integer;
begin
  Index := AHash and QWord(HashBuckets - 1);
  while True do
  begin
    if FWSList[Index].FStation = nil then
    begin
      SetLength(FWSList[Index].FStation, AEndP - AStartP);
      Move(ABytes[AStartP], FWSList[Index].FStation[0], AEndP - AStartP);
      FWSList[Index].FData.FMin := ATemp;
      FWSList[Index].FData.FMax := ATemp;
      FWSList[Index].FData.FTot := ATemp;
      FWSList[Index].FData.FCnt := 1;
      FWSList[Index].FData.FHash := AHash;
      Break;
    end;
    if CompareMem(@FWSList[Index].FStation[0], @ABytes[AStartP], AEndP - AStartP) then
    begin
      FWSList[Index].FData.FMin := min(FWSList[Index].FData.FMin, ATemp);
      FWSList[Index].FData.FMax := max(FWSList[Index].FData.FMax, ATemp);
      FWSList[Index].FData.FTot := FWSList[Index].FData.FTot + ATemp;
      Inc(FWSList[Index].FData.FCnt);
      Break;
    end;
    Inc(Index);
    if Index >= HashBuckets then
      Index := 0;
  end;
end;

procedure TWSThread.UpdateMainHashList;
var
  I, Index: Integer;
begin
  for I := Low(FWSList) to High(FWSList) do
  begin
    if FWSList[I].FStation = nil then
      Continue;
    Index := FWSList[I].FData.FHash and QWord(HashBuckets - 1);
    while True do
    begin
      if FWSManager.FWSListAll[Index].FStation = nil then
      begin
        SetLength(FWSManager.FWSListAll[Index].FStation, Length(FWSList[I].FStation));
        Move(FWSList[I].FStation[0], FWSManager.FWSListAll[Index].FStation[0], Length(FWSList[I].FStation));
        FWSManager.FWSListAll[Index].FData.FMin := FWSList[I].FData.FMin;
        FWSManager.FWSListAll[Index].FData.FMax := FWSList[I].FData.FMax;
        FWSManager.FWSListAll[Index].FData.FTot := FWSList[I].FData.FTot;
        FWSManager.FWSListAll[Index].FData.FCnt := FWSList[I].FData.FCnt;
        Break;
      end;
      if CompareMem(@FWSManager.FWSListAll[Index].FStation[0], @FWSList[I].FStation[0], Length(FWSList[I].FStation)) then
      begin
        FWSManager.FWSListAll[Index].FData.FMin := min(FWSManager.FWSListAll[Index].FData.FMin, FWSList[I].FData.FMin);
        FWSManager.FWSListAll[Index].FData.FMax := max(FWSManager.FWSListAll[Index].FData.FMax, FWSList[I].FData.FMax);
        FWSManager.FWSListAll[Index].FData.FTot := FWSManager.FWSListAll[Index].FData.FTot + FWSList[I].FData.FTot;
        FWSManager.FWSListAll[Index].FData.FCnt := FWSManager.FWSListAll[Index].FData.FCnt + FWSList[I].FData.FCnt;
        Break;
      end;
      Inc(Index);
      if Index >= HashBuckets then
        Index := 0;
    end;
  end;
end;

{ TWSThreadWatcher }
constructor TWSThreadsWatcher.Create(AWSManager: TWSManager);
begin
  inherited Create(AWSManager);
  FWSManager := AWSManager;
end;

function Compare(List: TStringList; Index1, Index2: Integer): Integer;
var
  P1, P2: Integer;
  Str1, Str2: String;
begin
  Result := 0;
  Str1 := List.Strings[Index1];
  Str2 := List.Strings[Index2];
  P1 := Pos('=', Str1);
  P2 := Pos('=', Str2);
  if (P1 > 0) and (P2 > 0) then
  begin
    Str1 := Copy(Str1, 1, P1 - 1);
    Str2 := Copy(Str2, 1, P2 - 1);
    Result := CompareStr(Str1, Str2);
  end;
end;

procedure TWSThreadsWatcher.CreateFinalList;
var
  I: Integer;
  WS: TWS;
  Str: String;
  Name: RawByteString;
  Min, Max: Double;
  Mean: Double;
  SL: TStringList;
  //MS: TMemoryStream;
begin
  SL := TStringList.Create;
  try
    SL.DefaultEncoding := TEncoding.UTF8;
    SL.BeginUpdate;
    I := High(FWSManager.FWSListALL);
    for I := Low(FWSManager.FWSListALL) to High(FWSManager.FWSListALL) do
    begin
      WS := FWSManager.FWSListALL[I];
      if WS.FStation = nil then
        Continue;
      SetString(Name, Pointer(@WS.FStation[0]), Length(WS.FStation));
      SetCodePage(Name, CP_UTF8, True);
      Min := WS.FData.FMin/10;
      Max := WS.FData.FMax/10;
      Mean := RoundExDouble(WS.FData.FTot/WS.FData.FCnt/10);
      Str := Name + '=' + FormatFloat('0.0', Min) + '/' + FormatFloat('0.0', Mean) + '/' + FormatFloat('0.0', Max) + ',';
      SL.Add(Str);
    end;
    SL.CustomSort(@Compare);
    SL.EndUpdate;
    Str := SL.Text;
  finally
    SL.Free;
  end;
  Str := Trim(Str);
  Delete(Str, Length(Str), 1);
  Str := '{' + Str + '}';
  Str := StringReplace(Str, sLineBreak, ' ', [rfReplaceAll]);
  //write to console
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  SetString(Name, Pointer(@Str[1]), Length(Str));
  SetCodePage(Name, CP_UTF8, False);
  Writeln(Name);
  {$ELSE}
  Writeln(Str);
  {$ENDIF}

  //save to file
  {MS := TMemoryStream.Create;
  try    
    MS.Write(Pointer(Str)^, Length(Str) div SizeOf(Char));
    MS.Position := 0;
    MS.SaveToFile('summary.txt');
  finally
    MS.Free
  end;}
end;

procedure TWSThreadsWatcher.Execute;
var
  ThreadCnt: Integer;
  Size: Int64;
  FS: TFileStream;
  WSThread: TWSThread;
  StartP, EndP: Int64;
begin
  FStarted := True;
  FS := TFileStream.Create(FWSManager.FSrcFile, fmOpenRead or fmShareDenyNone);
  try
    Size := Round(FS.Size/(FWSManager.FThreadCnt + 1));
    FS.Position := 0;
    StartP := 0;
    EndP := 0;
    repeat
      if Size > FS.Size - FS.Position then
        Size := FS.Size - FS.Position;
      if Size > High(LongInt) then
        Size := High(LongInt) - 100;
      StartP := FS.Position;
      FS.Position :=FS.Position + Size;
      GetNextLineBreak(FS);
      EndP := FS.Position;
      WSThread := TWSThread.Create(StartP, EndP, FWSManager);
      WSThread.Start;
    until (FS.Position >= FS.Size);
  finally
    FS.Free
  end;
  ThreadCnt := -1;
  repeat
    ThreadCnt := GetThreadCnt;
    Sleep(100);
  until ThreadCnt = 0;
  if (not Terminated) then
    CreateFinalList;
  FWSManager.FDone := True;
end;

{ TWSManager }
constructor TWSManager.Create(ASrcFile: String; AThreadCnt: Integer);
var
  I: Integer;
begin
  FSrcFile := ASrcFile;
  FDone := False;
  FThreadCnt := AThreadCnt;
  FTerminated := False;
  for I := Low(FWSListAll) to High(FWSListAll) do
  begin
    FWSListAll[I].FStation := nil;
    FWSListAll[I].FData.FMin := 0;
    FWSListAll[I].FData.FMax := 0;
    FWSListAll[I].FData.FTot := 0;
    FWSListAll[I].FData.FCnt := 0;
  end;
  FThreadList := TLockList.Create;
  FWSThreadsWatcher := TWSThreadsWatcher.Create(Self);
end;

destructor TWSManager.Destroy;
begin
  FThreadList.Free;
  inherited Destroy;
end;

end.

