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
    FRootFolder: String;
    FEntriesFolder: String;
    FResultsFolder: String;
    FBinFolder: String;
    FInputSSD: String;
    FInputHDD: String;
    FHyperfine: String;
    FLazbuild: String;
    FOutputHash: String;
    FEntries: TEntries;

    //procedure setFromJSON(const AJSON: TJSONStringType);
    procedure setFromJSONData(const AJSONData: TJSONData);
    procedure setFromJSONObject(const AJSONObject: TJSONObject);
  protected
  public
    constructor Create;
    //constructor Create(const AJSON: TJSONStringType);
    constructor Create(const AJSONData: TJSONData);

    destructor Destroy; override;

    property RootFolder: String
      read FRootFolder
      write FRootFolder;
    property EntriesFolder: String
      read FEntriesFolder
      write FEntriesFolder;
    property ResultsFolder: String
      read FResultsFolder
      write FResultsFolder;
    property BinFolder: String
      read FBinFolder
      write FBinFolder;
    property InputSSD: String
      read FInputSSD
      write FInputSSD;
    property InputHDD: String
      read FInputHDD
      write FInputHDD;
    property Hyperfine: String
      read FHyperfine
      write FHyperfine;
    property Lazbuild: String
      read FLazbuild
      write FLazbuild;
    property OutputHash: String
      read FOutputHash
      write FOutputHash;
    property Entries: TEntries
      read FEntries;
  published
  end;

implementation

const
  cJSONRootFolder    = 'root-folder';
  cJSONEntriesFolder = 'entries-folder';
  cJSONResultsFolder = 'results-folder';
  cJSONBinFolder     = 'bin-folder';
  cJSONInputSSD      = 'input-ssd';
  cJSONInputHDD      = 'input-hdd';
  cJSONHyperfine     = 'hyperfine';
  cJSONLazbuild      = 'lazbuild';
  cJSONOutpuHash     = 'output-hash';
  cJSONEntries       = 'entries';

resourcestring
  rsExceptionNotAJSONObject = 'JSON Data is not an object';
//  rsExceptionEmptyString = 'MUST not be and empty string';
//  rsExceptionCannotParse = 'Cannot parse: %s';
//  rsExceptionMissingMember = 'Missing member: %s';

  { TConfig }

constructor TConfig.Create;
begin
  FRootFolder:= '';
  FEntriesFolder:= '';
  FResultsFolder:= '';
  FBinFolder:= '';
  FInputSSD:= '';
  FInputHDD:= '';
  FHyperfine:= '';
  FLazbuild:= '';
  FOutputHash:= '';
  //FEntries:= TEntries.Create;
end;

{constructor TConfig.Create(const AJSON: TJSONStringType);
begin
  Create;
  setFromJSON(AJSON);
end;}

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

{procedure TConfig.setFromJSON(const AJSON: TJSONStringType);
var
  jData: TJSONData;
begin
  if trim(AJSON) = EmptyStr then
  begin
    raise ENodeStatusEmptyString.Create(rsExceptionEmptyString);
  end;
  try
    jData:= GetJSON(AJSON);
  except
    on E: Exception do
    begin
      raise ENodeStatusCannotParse.Create(Format(rsExceptionCannotParse, [E.Message]));
    end;
  end;
  try
    setFromJSONData(jData);
  finally
    jData.Free;
  end;
end;}

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
  FRootFolder:= AJSONObject.Get(cJSONRootFolder, FRootFolder);
  FEntriesFolder:= AJSONObject.Get(cJSONEntriesFolder, FEntriesFolder);
  FResultsFolder:= AJSONObject.Get(cJSONResultsFolder, FResultsFolder);
  FBinFolder:= AJSONObject.Get(cJSONBinFolder, FBinFolder);
  FInputSSD:= AJSONObject.Get(cJSONInputSSD, FInputSSD);
  FInputHDD:= AJSONObject.Get(cJSONInputHDD, FInputHDD);
  FHyperfine:= AJSONObject.Get(cJSONHyperfine, FHyperfine);
  FLazbuild:= AJSONObject.Get(cJSONLazbuild, FLazbuild);
  FOutputHash:= AJSONObject.Get(cJSONOutpuHash, FOutputHash);
  FEntries:= TEntries.Create(AJSONObject.Find(cJSONEntries));
end;

end.
