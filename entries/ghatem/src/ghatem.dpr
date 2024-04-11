program ghatem;

{$APPTYPE CONSOLE}

{$R *.res}

{$IFDEF RELEASE}
{$INLINE ON}
{$OPTIMIZATION ON}
{$ASSERTIONS OFF}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}
{$ENDIF}

uses
  OneBRC in 'OneBRC.pas',
  Utils in 'Utils.pas',
  console in 'console.pas';

//const
//  cInput: string = 'C:\DelphiLabs\1BRC2\data\measurements_1m.txt';
//  cInput: string = 'C:\DelphiLabs\1BRC2\data\measurements_100m.txt';
//  cInput: string = 'C:\DelphiLabs\1BRC2\data\measurements_1b.txt';

var
  vOneBRC: TOneBRC;
  vFileName: string;
begin
  if ParseCmdLineParams (vFileName) then begin

    vOneBRC := TOneBRC.Create (32);
    try
      vOneBRC.mORMotMMF (vFileName);
      vOneBRC.DispatchThreads;
      vOneBRC.WaitAll;
      vOneBRC.MergeAll;
      vOneBRC.GenerateOutput;
    finally
      vOneBRC.Free;
    end;

{$REGION 'debug'}
    {Timer ('read mmfile',
      procedure
      begin
        vOneBRC.mORMotMMF (vFileName);
      end
    );

    Timer ('process all',
      procedure
      begin
        vOneBRC.DispatchThreads;
        vOneBRC.WaitAll;
      end
    );

    Timer ('merge all',
      procedure
      begin
        vOneBRC.MergeAll;
      end
    );

    Timer ('generate output',
      procedure
      begin
        vOneBRC.GenerateOutput;
      end
    );

    WriteLn ('DONE!');
    Readln;   }
{$ENDREGION}

  end;
end.
