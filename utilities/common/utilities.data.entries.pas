unit Utilities.Data.Entries;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes
, SysUtils
  {$IFDEF FPC}
, fpjson
, Contnrs
  {$ELSE}
  {$ENDIF}
;

type
{ EEntryNotAJSONObject }
  EEntryNotAJSONObject = Exception;
{ EEntriessNotAJSONArray }
  EEntriessNotAJSONArray = Exception;
{ ENodeStatusEmptyString }
//  ENodeStatusEmptyString = Exception;
{ ENodeStatusCannotParse }
//  ENodeStatusCannotParse = Exception;
{ ENodeStatusMissingMember }
//  ENodeStatusMissingMember = Exception;
{ ENodeStatusParamsWrongType }
//  ENodeStatusParamsWrongType = Exception;

{ TEntry }
  TEntry = class(TObject)
  private
    FActive: Boolean;
    FName: TJSONStringType;
    FNotes: TJSONStringType;
    FCompiler: TJSONStringType;
    FUseTrunk: Boolean;
    FEntryFolder: TJSONStringType;
    FEntryBinary: TJSONStringType;
    FLPI: TJSONStringType;
    FDPROJ: TJSONStringType;
    FHasRelease: Boolean;
    FGoodHash: Boolean;
    FThreads: Integer;
    FRunParams: TJSONStringType;

    //procedure setFromJSON(const AJSON: TJSONStringType);
    procedure setFromJSONData(const AJSONData: TJSONData);
    procedure setFromJSONObject(const AJSONObject: TJSONObject);
  protected
  public
    constructor Create;
    //constructor Create(const AJSON: TJSONStringType);
    constructor Create(const AJSONData: TJSONData);

    destructor Destroy; override;

    property Active: Boolean
      read FActive
      write FActive;
    property Name: TJSONStringType
      read FName
      write FName;
    property Notes: TJSONStringType
      read FNotes
      write FNotes;
    property Compiler: TJSONStringType
      read FCompiler
      write FCompiler;
    property UseTrunk: Boolean
      read FUseTrunk
      write FUseTrunk;
    property EntryFolder: TJSONStringType
      read FEntryFolder
      write FEntryFolder;
    property EntryBinary: TJSONStringType
      read FEntryBinary
      write FEntryBinary;
    property LPI: TJSONStringType
      read FLPI
      write FLPI;
    property DPROJ: TJSONStringType
      read FDPROJ
      write FDPROJ;
    property HasRelease: Boolean
      read FHasRelease
      write FHasRelease;
    property GoodHash: Boolean
      read FGoodHash
      write FGoodHash;
    property Threads: Integer
      read FThreads
      write FThreads;
    property RunParams: TJSONStringType
      read FRunParams
      write FRunParams;
  published
  end;

{ TEntries }
  TEntriesEnumerator = class; //Forward
  TEntries = class(TObject)
  private
    FEntries: TFPObjectList;

    //procedure setFromJSON(const AJSON: TJSONStringType);
    procedure setFromJSONData(const AJSONData: TJSONData);
    procedure setFromJSONArray(const AJSONArray: TJSONArray);

    function GetCount: Integer;
    function GetEntry(Index: Integer): TEntry;
    procedure SetEntry(Index: Integer; AValue: TEntry);
  protected
  public
    constructor Create;
    constructor Create(AJSONData: TJSONData);

    destructor Destroy; override;

    function GetEnumerator: TEntriesEnumerator;

    property Count: Integer
      read GetCount;
    property Items[Index: Integer]: TEntry
      read GetEntry
      write SetEntry; default;
  published
  end;
  //TEntriesClass = class of TEntries;

{ TEntriesEnumerator }
  TEntriesEnumerator = class(TObject)
  private
    FEntries: TEntries;
    FPosition: Integer;
  protected
  public
    constructor Create(const AEntries: TEntries);
    function GetCurrent: TEntry;
    function MoveNext: Boolean;

    property Current: TEntry
      read GetCurrent;
  published
  end;

implementation

const
  cJSONActive      = 'active';
  cJSONName        = 'name';
  cJSONNotes       = 'notes';
  cJSONCompiler    = 'compiler';
  cJSONUseTrunk    = 'use-trunk';
  cJSONEntryFolder = 'entry-folder';
  cJSONEntryBinary = 'entry-binary';
  cJSONLPI         = 'lpi';
  cJSONDPROJ       = 'dproj';
  cJSONHasRelease  = 'has-release';
  cJSONGoodHash    = 'good-hash';
  cJSONThreads     = 'threads';
  cJSONRunParams   = 'run-params';

resourcestring
  rsExceptionNotAJSONObject = 'JSON Data is not an object';
  rsExceptionNotAJSONArray = 'JSON data is not an array';
//  rsExceptionEmptyString = 'MUST not be and empty string';
//  rsExceptionCannotParse = 'Cannot parse: %s';
//  rsExceptionMissingMember = 'Missing member: %s';

{ TEntry }

constructor TEntry.Create;
begin
  FActive:= false;
  FName:= '';
  FNotes:= '';
  FCompiler:= '';
  FUseTrunk:= False;
  FEntryFolder:= '';
  FEntryBinary:= '';
  FLPI:= '';
  FDPROJ:= '';
  FHasRelease:= True;
  FThreads:= 1;
  FRunParams:= '';
end;

constructor TEntry.Create(const AJSONData: TJSONData);
begin
  Create;
  setFromJSONData(AJSONData);
end;

destructor TEntry.Destroy;
begin
  inherited Destroy;
end;

procedure TEntry.setFromJSONData(const AJSONData: TJSONData);
begin
  if aJSONData.JSONType <> jtObject then
  begin
    raise EEntryNotAJSONObject.Create(rsExceptionNotAJSONObject);
  end;
  setFromJSONObject(aJSONData as TJSONObject);
end;

procedure TEntry.setFromJSONObject(const AJSONObject: TJSONObject);
begin
  FActive:= AJSONObject.Get(cJSONActive, FActive);
  FName:= AJSONObject.Get(cJSONName, FName);
  FNotes:= AJSONObject.Get(cJSONNotes, FNotes);
  FCompiler:= AJSONObject.Get(cJSONCompiler, FCompiler);
  FUseTrunk:= AJSONObject.Get(cJSONUseTrunk, FUseTrunk);
  FEntryFolder:= AJSONObject.Get(cJSONEntryFolder, FEntryFolder);
  FEntryBinary:= AJSONObject.Get(cJSONEntryBinary, FEntryBinary);
  FLPI:= AJSONObject.Get(cJSONLPI, FLPI);
  FDPROJ:= AJSONObject.Get(cJSONDPROJ, FDPROJ);
  FHasRelease:= AJSONObject.Get(cJSONHasRelease, FHasRelease);
  FGoodHash:= AJSONObject.Get(cJSONGoodHash, FGoodHash);
  FThreads:= AJSONObject.Get(cJSONThreads, FThreads);
  FRunParams:= AJSONObject.Get(cJSONRunParams, FRunParams);
end;

{ TEntries }

procedure TEntries.setFromJSONData(const AJSONData: TJSONData);
begin
  if aJSONData.JSONType <> jtArray then
  begin
    raise EEntriessNotAJSONArray.Create(rsExceptionNotAJSONArray);
  end;
  setFromJSONArray(aJSONData as TJSONArray);
end;

procedure TEntries.setFromJSONArray(const AJSONArray: TJSONArray);
var
  Index: Integer;
begin
  for Index:= 0 to Pred(AJSONArray.Count) do
  begin
    FEntries.Add(TEntry.Create(AJSONArray[Index]));
  end;
end;

function TEntries.GetCount: Integer;
begin
  Result:= FEntries.Count;
end;

function TEntries.GetEntry(Index: Integer): TEntry;
begin
  Result:= FEntries[Index] as TEntry;
end;

procedure TEntries.SetEntry(Index: Integer; AValue: TEntry);
begin
  FEntries[Index]:= AValue;
end;

constructor TEntries.Create;
begin
  FEntries:= TFPObjectList.Create(True);
end;

constructor TEntries.Create(AJSONData: TJSONData);
begin
  Create;
  setFromJSONData(AJSONData);
end;

destructor TEntries.Destroy;
begin
  FEntries.Free;
  inherited Destroy;
end;

function TEntries.GetEnumerator: TEntriesEnumerator;
begin
    Result:= TEntriesEnumerator.Create(Self);
end;

{ TEntriesEnumerator }

constructor TEntriesEnumerator.Create(const AEntries: TEntries);
begin
  FEntries := AEntries;
  FPosition := -1;
end;

function TEntriesEnumerator.GetCurrent: TEntry;
begin
  Result:= FEntries.Items[FPosition] as TEntry;
end;

function TEntriesEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FEntries.Count;
end;

end.
