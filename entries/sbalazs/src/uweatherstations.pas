unit uWeatherStations;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, contnrs;

type
  TWSManager = class;

  PWS = ^TWS;
  TWS = record
    FName: String[100];
    FMin: Single;
    FMax: Single;
    FMean: Single;
    FTot: Single;
    FCnt: Integer;
  end;

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
    function GetLeftOver(AST: TStream): String;
    function GetThreadCnt: Integer;
  protected
    procedure TerminatedSet; override;
  public
    constructor Create(AWSManager: TWSManager);
    destructor Destroy; override;
  end;

  TWSThread = class(TWSThreadBase)
  private
    FStr: String;
    FHashList: TFPHashList;
    FMS: TMemoryStream;
    procedure ProcessBuffer(ABuffer: String);
    procedure UpdateMainHashList;
    procedure AddToHashList(AStation, ATemp: String);
    procedure UpdateThreadList(AUpdateType: TUpdateType);
  protected
    procedure Execute; override;
  public
    constructor Create(AStr: String; AWSManager: TWSManager);
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
    FSrcFile: String;
    FDstFile: String;
    FThreadCnt: Integer;
    FTerminated: Boolean;
    FThreadList: TLockList;
    FHashListAll: TFPHashList;
    FWSThreadsWatcher: TWSThreadsWatcher;
    FOnStart: TNotifyEvent;
    FOnFinish: TNotifyEvent;
  public
    constructor Create(ASrcFile, ADstFile: String; AThreadCnt: Integer);
    destructor Destroy; override;
  public
    property WSThreadsWatcher: TWSThreadsWatcher read FWSThreadsWatcher;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
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

function TWSThreadBase.GetLeftOver(AST: TStream): String;
var
  P, Len: Integer;
  ReadCnt: LongInt;
begin
  Result := '';
  Len := 100 div SizeOf(Char);
  SetLength(Result, Len);
  ReadCnt := AST.Read(Pointer(Result)^, Len);
  if ReadCnt > 0 then
  begin
    AST.Position := AST.Position - ReadCnt;
    P := Pos(sLineBreak, Result);
    if P > 0 then
    begin
      Result := Copy(Result, 1, P - 1);
      AST.Position := AST.Position + Length(Result)*SizeOf(Char);
      Result := Result + sLineBreak + ' ';
    end;
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
constructor TWSThread.Create(AStr: String; AWSManager: TWSManager);
begin
  inherited Create(AWSManager);
  FStr := AStr;
  FMS := TMemoryStream.Create;
  FMS.Position := 0;
  FStarted := False;
  FHashList := TFPHashList.Create;
end;

destructor TWSThread.Destroy;
var
  I: Integer;
  WS: PWS;
begin
  for I := 0 to FHashList.Count - 1 do
  begin
    WS := FHashList.Items[I];
    Dispose(WS);
  end;
  FHashList.Clear;
  FHashList.Free;
  FMS.Free;
  inherited Destroy;
end;

procedure TWSThread.UpdateMainHashList;
var
  I: Integer;
  WS: PWS;
  WSAll: PWS;
  Hash: ShortString;
  Index: Integer;
begin
  for I := 0 to FHashList.Count - 1 do
  begin
    if FWSManager.FTerminated then
      Break;
    Hash := FHashList.NameOfIndex(I);
    WS := FHashList.Items[I];
    Index := FWSManager.FHashListAll.FindIndexOf(Hash);
    if Index = -1 then
    begin
       New(WSAll);
       WSAll^.FName := WS^.FName;
       WSAll^.FMin := WS^.FMin;
       WSAll^.FMax := WS^.FMax;
       WSAll^.FTot := WS^.FTot;
       WSAll^.FCnt := WS^.FCnt;
       FWSManager.FHashListAll.Add(Hash, WSAll);
    end
    else
    begin
      WSAll := FWSManager.FHashListAll.Items[Index];
      if WS^.FMin < WSAll^.FMin then
        WSAll^.FMin := WS^.FMin;
      if WS^.FMax > WSALL^.FMax then
        WSAll^.FMax := WS^.FMax;
      WSAll^.FTot := WSAll^.FTot + WS^.FTot;
      WSAll^.FCnt := WSAll^.FCnt + WS^.FCnt;
    end;
  end;
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

procedure TWSThread.AddToHashList(AStation, ATemp: String);
var
  WS: PWS;
  Index: Integer;
  Temp: Single;
begin
  Index := FHashList.FindIndexOf(AStation);
  if Index = -1 then
  begin
    ATemp := ATemp + '000000001';
    if not TryStrToFloat(ATemp, Temp) then
      Exit;
    New(WS);
    WS^.FName := AStation;
    WS^.FMin := Temp;
    WS^.FMax := Temp;
    WS^.FTot := Temp;
    WS^.FCnt := 1;
    FHashList.Add(AStation, WS);
  end
  else
  begin
    if not TryStrToFloat(ATemp, Temp) then
      Exit;
    WS := FHashList.Items[Index];
    if Temp < WS^.FMin then
      WS^.FMin := Temp;
    if Temp > WS^.FMax then
      WS^.FMax := Temp;
    WS^.FTot := WS^.FTot + Temp;
    WS^.FCnt := WS^.FCnt + 1;
  end;
end;

procedure TWSThread.ProcessBuffer(ABuffer: String);
var
  SL: TStringList;
  Str: String;
  P, I: Integer;
  Station: String;
  Temp: String;
begin
  SL := TStringList.Create;
  try
    SL.DefaultEncoding := TEncoding.UTF8;
    SL.BeginUpdate;
    SL.Text := ABuffer;
    for I := 0 to SL.Count - 1 do
    begin
      Str := SL.Strings[I];
      P := Pos(';', Str);
      if P > 0 then
      begin
        Station := Copy(Str, 1, P - 1);
        Temp := Copy(Str, P + 1, Length(Str));
        AddToHashList(Trim(Station), Trim(Temp));
      end;
    end;
    SL.EndUpdate;
  finally
    SL.Free;
  end;
end;

procedure TWSThread.Execute;
const
  BufferSize =  1048576;
var
  Buffer: string;
  ReadCnt: LongInt;
begin
  UpdateThreadList(utAdd);
  try
    FMS.Write(Pointer(FStr)^, Length(FStr) div SizeOf(Char));
    FMS.Position := 0;
    FStr := '';
    repeat
       if Terminated then
         Break;
       Buffer := '';
       SetLength(Buffer, BufferSize div SizeOf(Char));
       ReadCnt := FMS.Read(Pointer(Buffer)^, BufferSize div SizeOf(Char));
       if (ReadCnt > 0) then
       begin
         if Terminated then Break;
         Buffer := Buffer + GetLeftOver(FMS);
         ProcessBuffer(Buffer);
       end;
    until (ReadCnt = 0);
  finally
    UpdateThreadList(utRemove);
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
    Result :=  CompareText(Str1, Str2);
  end;
end;

procedure TWSThreadsWatcher.CreateFinalList;
var
  I: Integer;
  Str: String;
  WS: PWS;
  SL: TStringList;
  MS: TMemoryStream;
begin
  SL := TStringList.Create;
  try
    SL.DefaultEncoding := TEncoding.UTF8;
    SL.BeginUpdate;
    SL.CustomSort(@Compare);
    for I := 0 to FWSManager.FHashListAll.Count - 1 do
    begin
      WS := FWSManager.FHashListAll.Items[I];
      WS^.FMean := WS^.FTot/WS^.FCnt;
      WS^.FMean := Round(WS^.FMean*1000)/1000;
      Str := WS^.FName + '=' + FormatFloat('0.0', WS^.FMin) + '/' + FormatFloat('0.0', WS^.FMean) + '/' + FormatFloat('0.0', WS^.FMax) + ',';
      SL.Add(Str);
    end;
    SL.SortStyle := sslUser;
    SL.Sorted := True;
    SL.EndUpdate;
    Str := SL.Text;
  finally
    SL.Free;
  end;

  Str := Trim(Str);
  Delete(Str, Length(Str), 1);
  Str := '{' + Str + '}';
  MS := TMemoryStream.Create;
  try
    MS.Write(Pointer(Str)^, Length(Str) div SizeOf(Char));
    MS.Position := 0;
    MS.SaveToFile(FWSManager.FDstFile);
  finally
    MS.Free;
  end;
end;

procedure TWSThreadsWatcher.Execute;
var
  ThreadCnt: Integer;
  WSThread: TWSThread;
  Buffer: String;
  BufferSize: Int64;
  FS: TFileStream;
  ReadCnt: LongInt;
begin
  FStarted := True;
  if (not Terminated) and (FWSManager.FOnStart <> nil) then
    FWSManager.FOnStart(Self);
  FS := TFileStream.Create(FWSManager.FSrcFile, fmOpenRead or fmShareDenyNone);
  try
    BufferSize := Round(FS.Size/(FWSManager.FThreadCnt + 1));
    FS.Position := 0;
    repeat
      Buffer := '';
      if BufferSize > FS.Size - FS.Position then
        BufferSize := FS.Size - FS.Position;
      SetLength(Buffer, BufferSize div SizeOf(Char));
      ReadCnt := FS.Read(Pointer(Buffer)^, BufferSize div SizeOf(Char));
      if (ReadCnt > 0) then
      begin
        Buffer := Buffer + GetLeftOver(FS);
        WSThread := TWSThread.Create(Buffer, FWSManager);
        WSThread.Start;
      end;
    until ReadCnt = 0;
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
  if (not Terminated) and (FWSManager.FOnFinish <> nil) then
    FWSManager.FOnFinish(Self);
end;

{ TWSManager }
constructor TWSManager.Create(ASrcFile, ADstFile: String; AThreadCnt: Integer);
begin
  FSrcFile := ASrcFile;
  FDstFile := ADstFile;
  FThreadCnt := AThreadCnt;
  FTerminated := False;
  FThreadList := TLockList.Create;
  FHashListAll := TFPHashList.Create;
  FWSThreadsWatcher := TWSThreadsWatcher.Create(Self);
end;

destructor TWSManager.Destroy;
var
  I: Integer;
  WS: PWS;
begin
  for I := 0 to FHashListAll.Count - 1 do
  begin
    WS := FHashListAll.Items[I];
    Dispose(WS);
  end;
  FHashListAll.Clear;
  FHashListAll.Free;
  FThreadList.Free;
  inherited Destroy;
end;

end.

