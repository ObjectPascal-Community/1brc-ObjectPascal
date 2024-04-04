program Project1;

{$I SCL.inc}

uses
  UProgram,
  UProgramCommandLine,
  UConsole,
  USystemCPU,
  UNumber,
  UNumberHelp,
  UString,
  UStringHelp,
  UArraySort,
  UFile,
  UFileHelp,
  UFileHandleHelp,
  UFilePathHelp,
  UMemory,
  UMemoryBlock,
  UThread,
  UThreadGroup,
  UThreadHelp,
  UxxHash,
  UxxHashLibraryStatic;

const
  MaxCount: UPS = 48 * 1024;
  ReadMargin: UPS = 512;

var
  JumperCount: UPS;
  PartSize: UPS;
  ProcessorCount: U8;

type
  THash = U32;
  THashArray = TArray<THash>;
  TJumper = U16;
  TJumpers = TArray<TJumper>;

  TCoordinator = record
    Hashes: THashArray;
    Names: TStrArray;
    LastAdded: Ind;
    Jumpers: TJumpers;
    CriticalSection: TCriticalSection;
  end;

  TTemperatureSummary = packed record
    Min, Max: I16;
    Count: U16;
    Sum: I32;
  end;
  TTemperatureSummaryArray = TArray<TTemperatureSummary>;

  TContext = record
    SourceFile: TFile;
    LastPickedPart: Ind;
    Coordinator: TCoordinator;
    ProcessorTemperatures: TArray<TTemperatureSummaryArray>;
  end;
  PContext = ^TContext;

  TStationSummary = record
    Name: Str;
    Count: U16;
    Min, Mean, Max: I16;
    Sum: I32;
    class operator LessThan(const A, B: TStationSummary): Bool; inline;
    class operator GreaterThan(const A, B: TStationSummary): Bool; inline;
  end;
  TStationResultArray = TArray<TStationSummary>;

  class operator TStationSummary.LessThan(const A, B: TStationSummary): Bool;
  begin
    Result := A.Name < B.Name;
  end;

  class operator TStationSummary.GreaterThan(const A, B: TStationSummary): Bool;
  begin
    Result := A.Name > B.Name;
  end;

  function FindOrAdd(P: PChar; NS, NE: NChar; var ACoordinator: TCoordinator): Ind; inline; overload;
  var
    H: THash;
    I: Ind;
    JN: TJumper;
  begin
    H := XXH3_64bits(P + NS, NE - NS + 1) shr 32; //Good quality, no repeat for this dataset
    I := H and (JumperCount - 1); //Index in Jumpers

    with ACoordinator do
      repeat
        JN := Jumpers[I];
        if JN <> 0 then //Not empty
        begin
          Result := JN - 1; //JN starts from 1
          if Hashes[Result] = H then
            Exit
          else
          begin
            I += 1; //Collision, check next
            if I = Length(Jumpers) then //End, back to start
              I := 0;
          end;
        end
        else //Empty, try to set
        begin
          Enter(CriticalSection);
          try
            if Jumpers[I] = 0 then //If it is still 0
            begin //Start of write
              LastAdded += 1; //Add one
              Result := LastAdded;
              Hashes[Result] := H;
              JN := Result + 1;
              Jumpers[I] := JN;
            end; //End of write
          finally
            Leave(CriticalSection);
          end;
          if Jumpers[I] = JN then //Added by this
          begin
            Names[Result] := Create(P, NS, NE);
            Exit;
          end;
        end;
      until False;
  end;

  procedure Add(P: PChar; NS, NE: NChar; ATemperature: I16; var ATemperatures: TTemperatureSummaryArray;
  var ACoordinator: TCoordinator); inline; overload;
  var
    I: Ind;
  begin
    I := FindOrAdd(P, NS, NE, ACoordinator);
    with ATemperatures[I] do
    begin
      if Count <> 0 then
      begin
        if ATemperature < Min then
          Min := ATemperature
        else if ATemperature > Max then
          Max := ATemperature;
      end
      else
      begin
        Min := ATemperature;
        Max := ATemperature;
      end;
      Count += 1;
      Sum += ATemperature;
    end;
  end;

  procedure ProcessPart(P: PChar; var C, L: NChar; var ATemperatures: TTemperatureSummaryArray;
  var ACoordinator: TCoordinator); overload;
  var
    T: IPS;
  var
    NS, NE: NChar;
  begin
    if (C > 0) and (P[C - 1] <> #10) then //If it is not the first one and if it is not start
    begin
      if P[C] <> #10 then
        C += IndexChar(P[C], L, #10);
      C += 1; //Find start
    end;

    repeat
      //Name
      NS := C;
      C += IndexChar(P[C], L, ';') + 1;
      NE := C - 2;

      //Temperature
      T := 1 + I8(P[C] = '-') * -2;
      C += U8(P[C] = '-');
      if P[C + 1] = '.' then //X.X
      begin
        T *= (U8(P[C]) - 48) * 10 + U8(P[C + 2]) - 48;
        C += 5; //X.XCRLF
      end
      else //XX.X
      begin
        T *= (U8(P[C]) - 48) * 100 + (U8(P[C + 1]) - 48) * 10 + U8(P[C + 3]) - 48;
        C += 6; //XX.XCRLF
      end;

      Add(P, NS, NE, T, ATemperatures, ACoordinator);
    until C >= L;
  end;

  procedure Run(AProcessorIndex: Ind; AParameter: Ptr); overload;
  var
    H: TFileHandler;
    I, RM: IPS;
    FS: Siz;
    FP, PS, C, L: NChar;
    RB: TCharArray;
  begin
    SetLength(RB, PartSize + ReadMargin);
    with PContext(AParameter)^ do
      if Open(SourceFile, [ofoOpen, ofoRead, ofoShareRead], H) then
      try
        FS := Size(H);
        while True do
        begin
          I := InterlockedIncrement(LastPickedPart);
          FP := PartSize * I; //Position in file need to be processed
          RM := FS - FP; //Remaining of file
          if RM <= 0 then
            Break; //End of file

          if FP > 0 then
            FP -= 1; //Read one more from previous
          if FP > 0 then
            C := 1 //0 is #10 to fake a previous ending
          else
            C := 0;
          L := C + PartSize;
          if L > RM then //Clamp L to not over process
            L := RM;

          PS := 1 + PartSize + ReadMargin;
          if PS > RM then //Clamp PS to not over read
            PS := RM;

          SeekTo(H, FP);
          Read(H, PU8(@RB[0]), PS);
          ProcessPart(@RB[0], C, L, ProcessorTemperatures[AProcessorIndex], Coordinator);
        end;
      finally
        Close(H);
      end;
  end;

  procedure Run(const AFile: TFile); overload;

    procedure Process(out AStations: TStationResultArray); overload;

      procedure Initialize(out AContext: TContext);
      var
        I: Ind;
      begin
        AContext := Default(TContext);
        with AContext do
        begin
          SourceFile := AFile;
          LastPickedPart := -1;
          with Coordinator do
          begin
            SetLength(Hashes, MaxCount);
            SetLength(Names, MaxCount);
            LastAdded := -1;
            SetLength(Jumpers, JumperCount);
            Create(CriticalSection);
          end;
          SetLength(ProcessorTemperatures, ProcessorCount);
          for I := 0 to High(ProcessorTemperatures) do
            SetLength(ProcessorTemperatures[I], MaxCount);
        end;
      end;

      procedure Finalize(var AContext: TContext);
      begin
        Destroy(AContext.Coordinator.CriticalSection);
      end;

      procedure Process(var AContext: TContext); overload;
      var
        PG: TThreadGroup;
      begin
        Create(PG, ProcessorCount, Run, @AContext);
        StackSize(PG, PartSize * 2);
        Execute(PG);
        WaitFor(PG);
        Close(PG);
      end;

      procedure Aggregate(const AContext: TContext);
      var
        I, J: Ind;
      begin
        with AContext do
          for I := 0 to High(ProcessorTemperatures) do
            for J := 0 to High(AStations) do
              with ProcessorTemperatures[I][J] do
                if Count <> 0 then //Skipping empty ones as not all Processors may see all the stations
                begin
                  if AStations[J].Count <> 0 then
                  begin
                    AStations[J].Min := UNumberHelp.Min(AStations[J].Min, Min);
                    AStations[J].Max := UNumberHelp.Max(AStations[J].Max, Max);
                  end
                  else
                  begin
                    AStations[J].Min := Min;
                    AStations[J].Max := Max;
                  end;
                  AStations[J].Sum += Sum;
                  AStations[J].Count += Count;
                end;

        //Name and Mean
        for I := 0 to High(AStations) do
          with AStations[I] do
            if Count <> 0 then //Skipping empty ones if MaxCount is bigger than real count
            begin
              Name := AContext.Coordinator.Names[I];
              Mean := Ceil(Sum / Count);
            end;
      end;

    var
      CX: TContext;
    begin
      SetLength(AStations, MaxCount);
      Initialize(CX);
      Process(CX);
      Aggregate(CX);
      QuickSortMiddlePivot<TStationSummary>(AStations);
      Finalize(CX);
    end;

    function ToStr(constref AStations: TStationResultArray): RStr; overload;

      function ToOneFractionalStr(V: I16): Str; inline; overload;
      var
        Q, R: I16;
      begin
        if V < 0 then
        begin
          Result := '-';
          V *= -1;
        end
        else
          Result := '';
        Divide<I16>(V, 10, Q, R);
        Result += ToStr(Q) + '.' + ToStr(R);
      end;

    var
      I: Ind;
    begin
      Result := '{';
      for I := 0 to High(AStations) do
        with AStations[I] do
          if Count <> 0 then //Skipping empty ones if MaxCount is bigger than real count
          begin
            Result += Name + '=' + ToOneFractionalStr(Min) + '/' + ToOneFractionalStr(Mean) + '/' + ToOneFractionalStr(Max);
            if I <> High(AStations) then
              Result += ', ';
          end;
      Result += '}';
    end;

  var
    RST: TStationResultArray;
  begin
    Process(RST);
    WriteLn(ToStr(RST));
  end;

var
  I: Ind;
  N: Str;
  V: I64;
begin
  if Length(Parameters) = 0 then
    Exit;

  ProcessorCount := LogicalProcessorCount;
  JumperCount := 256 * 1024;
  PartSize := 192 * 1024 - ReadMargin;

  for I := 1 to High(Parameters) do
  begin
    N := Name(Parameters[I]);
    ToI64(Value(Parameters[I]), V);

    if N = 'jumper-count' then
      JumperCount := V * 1024
    else if N = 'part-size' then
      PartSize := (V * 1024) - ReadMargin
    else if N = 'processor-count' then
      ProcessorCount := V;
  end;

  Run(&File(Fix(Value(Parameters[0]))));
end.
