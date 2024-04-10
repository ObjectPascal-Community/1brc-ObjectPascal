program dtoepfl;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.StrUtils,
  System.IOUtils,
  System.Classes,
  System.TimeSpan,
  System.Generics.Collections,
  System.Diagnostics,
  generate.console in '..\..\..\generator\Common\generate.console.pas',
  Baseline.Common in '..\..\..\Baseline\Common\Baseline.Common.pas';

type TAlgorithm = (v1, v2, Count);
var algorithm: TAlgorithm;
    FormatSettings: TFormatSettings;
const paramPrefix = '--';
      paramPrefixShort = '-';

{$REGION 'v1'}

type TStationEntry = record
  min, max, sum: Double;
  count: Integer;
end;

/// <summary>Simple algorithm, single-thread</summary>
procedure runV1;
var FileStream: TFileStream;
    StreamReader: TStreamReader;
    CityTemperatures: TDictionary<String, TStationEntry>;  //Station, min, avrg, max
    Line: String;
    Parts: TArray<String>;
    entry: TStationEntry;
    value: Double;
begin
  CityTemperatures := TDictionary<String, TStationEntry>.Create;

  try
    FileStream := TFileStream.Create(inputFilename, fmOpenRead or fmShareDenyWrite);
    try
      StreamReader := TStreamReader.Create(FileStream, TEncoding.UTF8);

      // Read the first three bytes to check for UTF-8 BOM
//      var NumRead := FileStream.Read(BOM, Length(BOM));
      // If the file starts with the UTF-8 BOM, continue as is, otherwise reset the stream position.
//      if (NumRead <> Length(BOM)) or (BOM[0] <> $EF) or (BOM[1] <> $BB) or (BOM[2] <> $BF) then
//      begin
//        FileStream.Position := 0; // Reset the position if no BOM or not a UTF-8 BOM
//      end;

      try
        while not StreamReader.EndOfStream do
        begin
          Line := StreamReader.ReadLine;
          Parts := SplitString(Line, ';');
          if (TryStrToFloat(Parts[1], value, FormatSettings)) then
          begin
            if (CityTemperatures.ContainsKey(Parts[0])) then
            begin
              entry := CityTemperatures[Parts[0]];

              if (value < entry.min) then   //min
                entry.min := value;

              entry.sum := (entry.sum + value);  //average
              entry.count := entry.count + 1;

              if (value > entry.max) then //max
                entry.max := value;

              CityTemperatures[Parts[0]] := entry;
            end
            else
            begin
              entry.min := value;
              entry.count := 1;
              entry.sum := value;
              entry.max := value;
              CityTemperatures.Add(Parts[0], entry);
            end;
          end;
        end;


        //Sorted output
        var SortedKeys := TList<String>.Create(CityTemperatures.Keys);
            SortedKeys.Sort;
        var i := 0;
        {$IFDEF DEBUG}
             var vStream := TStringStream.Create('', TEncoding.UTF8);
        {$ENDIF}
        try
          for var Key in SortedKeys do
          begin

             // Windows will mess up the characters when outputting to STDOUT.
             // for debug purposes, we'll output it to a file instead.
             //https://github.com/gcarreno/1brc-ObjectPascal/blob/main/baseline/Common/baseline.delphi.pas @https://github.com/georges-hatem
             {$IFDEF DEBUG}
               if (i = 0) then
                 vStream.WriteString('{');

               vStream.WriteString(Format('%s=%.1f/%.1f/%.1f', [key, CityTemperatures[key].min, RoundExDouble(CityTemperatures[key].sum / CityTemperatures[key].count), CityTemperatures[key].max], FormatSettings));

               if (i = CityTemperatures.Count-1) then
                 vStream.WriteString('}' + #10)
               else
                 vStream.WriteString(', ');

             {$ELSE}
               if (i = 0) then
                Write('{');

               Write(Format('%s=%.1f/%.1f/%.1f', [key, CityTemperatures[key].min, RoundExDouble(CityTemperatures[key].sum / CityTemperatures[key].count), CityTemperatures[key].max], FormatSettings));

               if (i = CityTemperatures.Count-1) then
                 Write('}' + #10)
               else
                 Write(', ');
             {$ENDIF}

            Inc(i);
          end;

          {$IFDEF DEBUG}
            vStream.SaveToFile('output.txt');
          {$ENDIF}

        finally
          {$IFDEF DEBUG}
            vStream.Free;
          {$ENDIF}
        end;

      finally
        StreamReader.Free;
      end;
    finally
      FileStream.Free;
    end;
  finally
    CityTemperatures.Free;
  end;
end;
{$ENDREGION}

{$REGION 'v2'}
procedure runV2;
begin
  raise Exception.Create('v2 not implemented yet!');
end;
{$ENDREGION}

function TryGetParamValue(index: Int8): String;
begin
  try
    Result := ParamStr(index + 1).ToLower
  except on e:exception do
    raise Exception.Create('Invalid parameter for ' + ParamStr(index) + sLineBreak + e.Message);
  end;
end;

begin
  try
    FormatSettings := TFormatSettings.Create;
    FormatSettings.DecimalSeparator := '.';

    {$REGION 'Application Params'}
    var Arg: String;
    for var i := 1 to ParamCount do
    begin
      Arg := ParamStr(i).ToLower;

      if (Arg = paramPrefixShort + cShortOptions[2]) or
         (Arg = paramPrefix + cLongOptions[2]) then //input-file
        inputFilename := TryGetParamValue(i);

      if (Arg = paramPrefixShort + 'a') or
         (Arg = paramPrefix + 'algorithm') then //algorithm
      begin
        const v = StringReplace(TryGetParamValue(i), 'v', '', [rfReplaceAll]);

        var version: Integer;
        if (TryStrToInt(v, version)) then
        begin

          if (Ord(TAlgorithm.Count) = version) then
          begin
            case version of
              1: algorithm := TAlgorithm.v1;
              2: algorithm := TAlgorithm.v2;
            end;
          end
          else
            raise Exception.Create('Invalid algorithm version!');

        end;
      end;
    end;

    //Check if values are valid
    if (inputFilename.IsEmpty) then
      raise Exception.Create(Format(rsErrorMessage, [ rsMissingInputFlag ]));

    inputFilename  := ExpandFileName(inputFilename);
    {$ENDREGION}

    {$IFDEF DEBUG}
    WriteLn('The One Billion Row Challenge in Object Pascal');
    WriteLn('Source: https://github.com/dtpfl, https://github.com/gcarreno/1brc-ObjectPascal');
    WriteLn;
    WriteLn(Format(rsInputFile, [ inputFilename ]));
    WriteLn(Format('Algorithm: v%d', [Ord(algorithm)+1]));
    WriteLn;

    var Stopwatch := TStopwatch.StartNew;
    {$ENDIF}

    if (algorithm = TAlgorithm.v1) then
      runV1
    else if (algorithm = TAlgorithm.v2) then
      runv2;

    {$IFDEF DEBUG}
    var Elapsed: TTimeSpan := Stopwatch.Elapsed;
    Writeln;
    Writeln('Elapsed time: ' + FormatFloat('0.000', Elapsed.TotalSeconds) + ' seconds');
    {$ENDIF}
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
