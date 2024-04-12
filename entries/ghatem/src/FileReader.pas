unit FileReader;

interface

uses
  System.Classes, System.SysUtils, System.Types,
  Winapi.Windows;

type
  TInputReader = class
    class procedure MMF(const AFilename: string; var aOut: TArray<AnsiChar>);
  end;

implementation


class procedure TInputReader.MMF (const AFilename: string; var aOut: TArray<AnsiChar>);
// procedure found online while searching for memory-mappped files

// PROBLEM-1: GetFileSize was returning ~3.9B, which is incorrect.
//            GetFileSizeEx was correctly returning 16B instead.
// PROBLEM-2: CreateFileMapping only accepts Cardinals, which are not enough to fit 16B,
//            thus RangeCheck error.
// One way would be to call CreateFileMapping in chunks of acceptable size,
// but a chunk of 3.9B was taking ~1.7 seconds to load.
// so 4 times that was pretty slow, especially that it is a single-threaded operation
var
  hFile: THandle;
  hFileMap: THandle;
  hiSize: Int64;
  loSize: Int64;
  view: pointer;
begin
  if AFilename = '' then
    Exit;
  if not FileExists(AFilename) then
    Exit;
  {Open the file}
  hFile := CreateFile(
    PChar(AFilename), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
  );
  if hFile <> INVALID_HANDLE_VALUE then
  begin
    loSize := GetFileSize(hFile, @hiSize);
    {File was opened successfully, now map it:}
    hFileMap := CreateFileMapping(
      hFile, nil, PAGE_READONLY, hiSize, loSize, 'TextForString'
    );
    if (hFileMap <> 0) then
    begin
      if (GetLastError() = ERROR_ALREADY_EXISTS) then
      begin
        Writeln('Mapping already exists - not created.');
        CloseHandle(hFileMap)
      end
      else
      begin
        view := nil;
        try
          {File mapped successfully, now map a view of the file into the
          address space:}
          view := MapViewOfFile(hFileMap, FILE_MAP_READ, 0, 0, 0);
          if (view <> nil) then
          begin {View mapped successfully}
            {Close file handle - as long is view is open it will persist}
            CloseHandle(hFile);
            SetLength(aOut, loSize);
            Move(view^, aOut[0], loSize);
          end
          else
            WriteLn('Unable to map view of file.');
        finally
          UnmapViewOfFile(view);  {Close view}
          CloseHandle(hFileMap);  {Close mapping}
        end
      end
    end
    else
    begin
      WriteLn('Unable to create file mapping.');
    end;
  end
  else
  begin
    WriteLn('Unable to open file.');
  end;
end;

end.
