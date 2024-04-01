program obrc_dcornelius;
(* as: OBRC_DCornelius.dpr
 * by: David Cornelius
 * on: March, 2024
 * in: Delphi 12 Athens
 * to: submit an entry in the One Billion Row Challenge
 *
 * Development/Testing was done on a 3.8 GHz Intel i7 desktop computer running Windows 11.
 *
 * NOTE: Build with 'Debug' configuration for messages, automatic timing, and a pause at the end.
 *       Build with 'Release' configuration for the official submission.
 *)

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Classes,
  {$IFDEF DEBUG}
  System.Diagnostics,
  {$ENDIF }
  uChallengeWithDictionary in 'uChallengeWithDictionary.pas',
  uChallengeCommon in 'uChallengeCommon.pas',
  uChallengeWithStringList in 'uChallengeWithStringList.pas',
  udmChallengeWithFireDAC in 'udmChallengeWithFireDAC.pas' {dmChallengeWithFireDAC: TDataModule};

procedure DisplaySyntax;
begin
  Writeln('SYNTAX: ' + ExtractFileName(ParamStr(0)) + ' <filename> <method>');
  Writeln('  where <filename> is a text file with weather station data');
  Writeln('    and <method> is the algorytm for summarizing the data:');
  Writeln('        TSL = read in all data to a TStringList (lots of memory needed)');
  Writeln('        DIC = build a Dictionary, then sort after built');
  Writeln('        TBL = load a FireDAC in-memory table');
  Writeln;
  Writeln('Press ENTER...');
  Readln;
end;

begin
  try
    if ParamCount <> 2 then
      DisplaySyntax
    else begin
      {$IFDEF DEBUG}
      var StopWatch := TStopwatch.StartNew;
      {$ENDIF}
      ChallengeCommon := TChallengeCommon.Create(ParamStr(1));
      try
        var Method := ParamStr(2);
        if SameText(Method, 'TSL') then
          ChallengeWithStringList
        else if SameText(Method, 'DIC') then
          ChallengeWithDictionary
        else if SameText(Method, 'TBL') then
          ChallengeWithFireDAC
        else
          raise EArgumentException.Create('Invalid method');
      finally
        ChallengeCommon.Free;
      end;
      {$IFDEF DEBUG}
      StopWatch.Stop;
      Writeln('Elapsed Time (seconds/milliseconds): ', StopWatch.Elapsed.Seconds, ' / ', StopWatch.ElapsedMilliseconds);
      Readln;
      {$ENDIF}
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
