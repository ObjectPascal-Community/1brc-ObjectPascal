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

    vOneBRC := TOneBRC.Create;
    try
      vOneBRC.mORMotMMF (vFileName);
      vOneBRC.SingleThread;
      vOneBRC.GenerateOutput;
    finally
      vOneBRC.Free;
    end;

{$REGION 'debug'}
//    Timer ('read mmfile',
//      procedure
//      begin
//
//      end
//    );
//
//    for I := 0 to 4 do begin
//      Timer ('process single thread',
//        procedure
//        begin
//
//        end
//      );
//
//      Timer ('generate output',
//        procedure
//        begin
//
//        end
//      );
//    end;
//
//    WriteLn ('DONE!');
//    Readln;
{$ENDREGION}

  end;
end.
