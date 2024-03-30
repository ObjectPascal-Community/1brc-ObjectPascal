unit Utilities.Data.Config;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes
, SysUtils
  {$IFDEF FPC}
, fpjson
  {$ELSE}
  {$ENDIF}
, Utilities.Data.Entries
;

type
{ EConfigNotAJSONObject }
  EConfigNotAJSONObject = Exception;
{ ENodeStatusEmptyString }
//  ENodeStatusEmptyString = Exception;
{ ENodeStatusCannotParse }
//  ENodeStatusCannotParse = Exception;
{ ENodeStatusMissingMember }
//  ENodeStatusMissingMember = Exception;
{ ENodeStatusParamsWrongType }
//  ENodeStatusParamsWrongType = Exception;

{ TConfig }
  TConfig = class(TObject)
  private
    FRootLinux: TJSONStringType;
    FRootWindows: TJSONStringType;

    FEntriesLinux: TJSONStringType;
    FEntriesWindows: TJSONStringType;

    FResultsFolder: TJSONStringType;

    FBinLinux: TJSONStringType;
    FBinWindows: TJSONStringType;

    FInput: TJSONStringType;
    FHyperfine: TJSONStringType;

    FLazbuild: TJSONStringType;
    FDelphiCompiler: TJSONStringType;

    FOutputHash: TJSONStringType;
    FEntries: TEntries;

    procedure setFromJSONData(const AJSONData: TJSONData);
    procedure setFromJSONObject(const AJSONObject: TJSONObject);
  protected
  public
    constructor Create;
    constructor Create(const AJSONData: TJSONData);

    destructor Destroy; override;

    property RootLinux: TJSONStringType
      read FRootLinux
      write FRootLinux;
    property RootWindows: TJSONStringType
      read FRootWindows
      write FRootWindows;

    property EntriesLinux: TJSONStringType
      read FEntriesLinux
      write FEntriesLinux;
    property EntriesWindows: TJSONStringType
      read FEntriesWindows
      write FEntriesWindows;

    property ResultsFolder: TJSONStringType
      read FResultsFolder
      write FResultsFolder;

    property BinLinux: TJSONStringType
      read FBinLinux
      write FBinLinux;
    property BinWindows: TJSONStringType
      read FBinWindows
      write FBinWindows;

    property Input: TJSONStringType
      read FInput
      write FInput;
    property Hyperfine: TJSONStringType
      read FHyperfine
      write FHyperfine;

    property Lazbuild: TJSONStringType
      read FLazbuild
      write FLazbuild;
    property DelphiCompiler: TJSONStringType
      read FDelphiCompiler
      write FDelphiCompiler;

    property OutputHash: TJSONStringType
      read FOutputHash
      write FOutputHash;
    property Entries: TEntries
      read FEntries;
  published
  end;

implementation

const
  cJSONRootLinux      = 'root-linux';
  cJSONRootWIndows    = 'root-windows';
  cJSONEntriesLinux   = 'entries-linux';
  cJSONEntriesWindows = 'entries-windows';
  cJSONResultsFolder  = 'results-folder';
  cJSONBinLinux       = 'bin-linux';
  cJSONBinWindows     = 'bin-windows';
  cJSONInput          = 'input';
  cJSONHyperfine      = 'hyperfine';
  cJSONLazbuild       = 'lazbuild';
  cJSONDelphiCompiler = 'delphi-compiler';
  cJSONOutpuHash      = 'output-hash';
  cJSONEntries        = 'entries';

resourcestring
  rsExceptionNotAJSONObject = 'JSON Data is not an object';
//  rsExceptionEmptyString = 'MUST not be and empty string';
//  rsExceptionCannotParse = 'Cannot parse: %s';
//  rsExceptionMissingMember = 'Missing member: %s';

  { TConfig }

constructor TConfig.Create;
begin
  FRootLinux:= '';
  FRootWindows:= '';
  FEntriesLinux:= '';
  FEntriesWindows:= '';
  FResultsFolder:= '';
  FBinLinux:= '';
  FBinWindows:= '';
  FInput:= '';
  FHyperfine:= '';
  FLazbuild:= '';
  FDelphiCompiler:= '';
  FOutputHash:= '';
end;

constructor TConfig.Create(const AJSONData: TJSONData);
begin
  Create;
  setFromJSONData(AJSONData);
end;

destructor TConfig.Destroy;
begin
  FEntries.Free;
  inherited Destroy;
end;


procedure TConfig.setFromJSONData(const AJSONData: TJSONData);
begin
  if aJSONData.JSONType <> jtObject then
  begin
    raise EConfigNotAJSONObject.Create(rsExceptionNotAJSONObject);
  end;
  setFromJSONObject(aJSONData as TJSONObject);
end;

procedure TConfig.setFromJSONObject(const AJSONObject: TJSONObject);
begin
  FRootLinux:= AJSONObject.Get(cJSONRootLinux, FRootLinux);
  FRootWindows:= AJSONObject.Get(cJSONRootWIndows, FRootWindows);

  FEntriesLinux:= AJSONObject.Get(cJSONEntriesLinux, FEntriesLinux);
  FEntriesWindows:= AJSONObject.Get(cJSONEntriesWindows, FEntriesWindows);

  FResultsFolder:= AJSONObject.Get(cJSONResultsFolder, FResultsFolder);

  FBinLinux:= AJSONObject.Get(cJSONBinLinux, FBinLinux);
  FBinWindows:= AJSONObject.Get(cJSONBinWindows, FBinWindows);

  FInput:= AJSONObject.Get(cJSONInput, FInput);
  FHyperfine:= AJSONObject.Get(cJSONHyperfine, FHyperfine);

  FLazbuild:= AJSONObject.Get(cJSONLazbuild, FLazbuild);
  FDelphiCompiler:= AJSONObject.Get(cJSONDelphiCompiler, FDelphiCompiler);

  FOutputHash:= AJSONObject.Get(cJSONOutpuHash, FOutputHash);
  FEntries:= TEntries.Create(AJSONObject.Find(cJSONEntries));
end;

end.
