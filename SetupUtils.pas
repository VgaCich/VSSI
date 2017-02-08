unit SetupUtils;

interface

uses
  Windows, AvL, avlUtils, avlCRC32;

type
  TSpecialFolder = (
    sfDesktop = 16,
    sfCommonDesktop = 25,
    sfStartMenu = 11,
    sfCommonStartMenu = 22,
    sfPrograms = 2,
    sfCommonPrograms = 23,
    sfStartup = 7,
    sfCommonStartup = 24,
    sfMyDocuments = 5,
    sfCommonDocuments = 46,
    sfAppData = 26,
    sfLocalAppData = 28,
    sfCommonAppData = 35,
    sfSendTo = 9,
    sfTemplates = 21,
    sfCommonTemplates = 45,
    sfFavorites = 6,
    sfCommonFavorites = 31,
    sfProgramFiles = 38,
    sfCommonProgramFiles = 43,
    sfSystem = 37,
    sfWindows = 36,
    sfFonts = 20
  );
  TScriptStatus = (ssError, ssOK, ssNoScript, ssCRCFail);

procedure ReportError(const Error: string);
function GetSpecialFolder(FolderID: TSpecialFolder): string;
procedure SearchFolders(const Path: string; List: TStringList);
function CheckScript: TScriptStatus;
function LoadScript: TMemoryStream;
function CopyToTemp: string;
procedure SetMacro(const Name, Value: string);
function GetMacro(const Name: string): string;
function SubstituteMacro(const S: string): string;
function nrv2e_decompress(src: Pointer; src_len: Cardinal; dst: Pointer; var dst_len: Cardinal; wrkmem: Pointer): Integer; cdecl;

implementation

var
  Macros: array of record
    Name, Value: string;
  end;

procedure ReportError(const Error: string);
begin
  MessageBox(0, PChar(Error), PChar(string(ExtractFileName(ExeName))), MB_ICONERROR or MB_OK);
  ExitCode := -1;
end;

{function GetSpecialFolder(FolderID: TSpecialFolder): string;
begin
  SetLength(Result, MAX_PATH);
  if SHGetSpecialFolderPath(0, PChar(Result), Integer(FolderID), false) then
    SetLength(Result, FirstDelimiter(#0, Result) - 1)
  else Result := '';
end;}

const
  shell32 = 'shell32.dll';

function SHGetSpecialFolderLocation(hwndOwner: HWND; nFolder: Integer; var ppidl: Pointer): HResult; stdcall; external shell32 name 'SHGetSpecialFolderLocation';
function SHGetPathFromIDList(pidl: Pointer; pszPath: PChar): BOOL; stdcall; external shell32 name 'SHGetPathFromIDListA';

function GetSpecialFolder(FolderID: TSpecialFolder): string;
var
  PIDL: Pointer;
begin
  SetLength(Result, MAX_PATH);
  if (SHGetSpecialFolderLocation(0, Integer(FolderID), PIDL) = NOERROR) and
     (SHGetPathFromIDList(PIDL, PChar(Result))) then
  begin
    SetLength(Result, FirstDelimiter(#0, Result) - 1);
    CoTaskMemFree(PIDL);
  end
  else
    Result := '';
end;

procedure SearchFolders(const Path: string; List: TStringList);
var
  SR: TSearchRec;
begin
  if FindFirst(AddTrailingBackslash(Path) + '*', faDirectory, SR) = 0 then
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') and (SR.Attr and faDirectory <> 0) and (List.IndexOf(SR.Name) < 0) then
        List.Add(SR.Name);
    until FindNext(SR) <> 0;
  FindClose(SR);
end;

{$IFDEF DEBUG_DUMMY}
function FullExeName: string;
begin
  Result := AddTrailingBackslash(ExePath) + 'TestSetup.exe';
end;
{$ENDIF}

function CheckScript: TScriptStatus;
var
  F: TFileStream;
  Offset: Integer;
  CRC: Cardinal;
begin
  Result := ssError;
  F := TFileStream.Create(FullExeName, fmOpenRead or fmShareDenyWrite);
  try
    F.Seek(-(SizeOf(CRC) + SizeOf(Offset)), soFromEnd);
    F.ReadBuffer(Offset, SizeOf(Offset));
    if Offset = 0 then
      Result := ssNoScript
    else if Offset < F.Position then
    begin
      F.ReadBuffer(CRC, SizeOf(CRC));
      F.Position := Offset;
      if StreamCRC32(F, F.Size - Offset - SizeOf(CRC)) = CRC then
        Result := ssOK
      else
        Result := ssCRCFail;
    end;
  finally
    F.Free;
  end;
end;

function LoadScript: TMemoryStream;
var
  F: TFileStream;
  Offset, CSize: Integer;
  USize: Cardinal;
begin
  Result := nil;
  {$IFDEF DEBUG}
  Result := TMemoryStream.Create;
  Result.LoadFromFile('script.txt');
  {$ELSE}
  F := TFileStream.Create(FullExeName, fmOpenRead or fmShareDenyWrite);
  try
    F.Seek(-2 * SizeOf(Offset), soFromEnd);
    F.ReadBuffer(Offset, SizeOf(Offset));
    if Offset >= F.Position then Exit;
    F.Position := Offset;
    F.ReadBuffer(USize, SizeOf(USize));
    CSize := F.Size - F.Position - 3 * SizeOf(Offset);
    Result := TMemoryStream.Create;
    Result.SetSize(USize + CSize);
    F.ReadBuffer(IncPtr(Result.Memory, USize)^, CSize);
    if nrv2e_decompress(IncPtr(Result.Memory, USize), CSize, Result.Memory, USize, nil) <> 0 then
      FreeAndNil(Result)
    else
      Result.SetSize(USize);
  finally
    F.Free;
  end;
  {$ENDIF}
end;

function CopyToTemp: string;
begin
  Result := UniTempFile;
  CopyFile(PChar(FullExeName), PChar(Result), false);
end;

function FindMacro(const Name: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(Macros) do
    if SameText(Macros[i].Name, Name) then
    begin
      Result := i;
      Exit;
    end;
end;

procedure SetMacro(const Name, Value: string);
var
  Index: Integer;
begin
  if Name = '' then Exit;
  Index := FindMacro(Name);
  if Index < 0 then
  begin
    SetLength(Macros, Length(Macros) + 1);
    Index := High(Macros);
  end;
  Macros[Index].Name := Name;
  Macros[Index].Value := Value;
end;

function GetMacro(const Name: string): string;
var
  Index: Integer;
begin
  if Name <> '' then
  begin
    Index := FindMacro(Name);
    if Index >= 0 then
      Result := Macros[Index].Value
    else
      Result := '';
  end
    else Result := '%';
end;

function SubstituteMacro(const S: string): string;
var
  i: Integer;
  IsMacro: Boolean;
  Buf: string;
begin
  IsMacro := false;
  Result := '';
  Buf := '';
  for i := 1 to Length(S) do
    if S[i] = '%' then
    begin
      if IsMacro then
        Result := Result + GetMacro(Buf)
      else
        Result := Result + Buf;
      IsMacro := not IsMacro;
      Buf := '';
    end
      else Buf := Buf + S[i];
  if not IsMacro then
    Result := Result + Buf;
end;

{$DEFINE NRV2E_ASM}
{$IFDEF NRV2E_ASM}
function nrv2e_decompress(src: Pointer; src_len: Cardinal; dst: Pointer; var dst_len: Cardinal; wrkmem: Pointer): Integer;
asm
  pop EBP
  db 85,87,86,83,81,82,131,236,8,137,227,252,139,115,36,139
  db 123,44,137,248,139,83,48,3,2,15,130,137,1,0,0,137
  db 3,137,240,3,67,40,15,130,124,1,0,0,137,67,4,131
  db 205,255,49,201,235,24,59,116,36,4,15,131,58,1,0,0
  db 59,60,36,15,131,81,1,0,0,164,0,219,117,15,59,116
  db 36,4,15,131,34,1,0,0,138,30,70,16,219,114,215,49
  db 192,64,0,219,117,15,59,116,36,4,15,131,10,1,0,0
  db 138,30,70,16,219,17,192,15,136,36,1,0,0,0,219,117
  db 15,59,116,36,4,15,131,239,0,0,0,138,30,70,16,219
  db 114,30,72,0,219,117,15,59,116,36,4,15,131,217,0,0
  db 0,138,30,70,16,219,17,192,15,136,243,0,0,0,235,178
  db 61,2,0,0,1,15,135,230,0,0,0,131,232,3,114,58
  db 193,224,8,59,116,36,4,15,131,173,0,0,0,172,131,240
  db 255,15,132,152,0,0,0,15,137,196,0,0,0,209,248,137
  db 197,115,40,0,219,117,15,59,116,36,4,15,131,137,0,0
  db 0,138,30,70,16,219,17,201,235,74,0,219,117,11,59,116
  db 36,4,115,118,138,30,70,16,219,114,216,65,0,219,117,11
  db 59,116,36,4,115,100,138,30,70,16,219,114,198,0,219,117
  db 11,59,116,36,4,115,83,138,30,70,16,219,17,201,120,106
  db 0,219,117,11,59,116,36,4,115,64,138,30,70,16,219,115
  db 220,131,193,2,129,253,0,251,255,255,131,209,2,137,242,137
  db 254,1,206,114,69,59,52,36,119,64,137,254,1,238,115,65
  db 59,116,36,44,114,59,243,164,137,214,233,219,254,255,255,59
  db 60,36,119,38,59,116,36,4,118,7,184,55,255,255,255,235
  db 5,116,3,72,176,51,43,124,36,44,139,84,36,48,137,58
  db 131,196,8,90,89,91,94,95,93,195,184,54,255,255,255,235
  db 229,184,53,255,255,255,235,222,131,200,255,235,217,144,144,144
end;
{$ELSE}
function nrv2e_decompress(src: Pointer; src_len: Cardinal; dst: Pointer; var dst_len: Cardinal; wrkmem: Pointer): Integer; cdecl;
var
  BB, ILen: Cardinal;

  function GetByte: Byte;
  begin
    Result := PByteArray(Src)[ILen];
    Inc(ILen);
  end;

  function GetBit: Cardinal;
  begin
    if (BB and $7f) = 0 then
      BB := 2 * GetByte + 1
    else
      BB := 2 * BB;
    Result := (BB shr 8) and 1;
  end;

var
  OLen, OEnd, LastMOff, MOff, MLen: Cardinal;

  function Check(Condition: Boolean; ErrorCode: Integer): Boolean;
  begin
    Result := Condition;
    if Result then
    begin
      nrv2e_decompress := ErrorCode;
      dst_len := OLen;
    end;
  end;

begin
  BB := 0;
  ILen := 0;
  OLen := 0;
  OEnd := dst_len;
  LastMOff := 1;
  while true do
  begin
    while GetBit > 0 do
    begin
      if Check(ILen >= src_len, -201) then Exit;
      if Check(OLen >= OEnd, -202) then Exit;
      PByteArray(dst)[OLen] := GetByte;
      Inc(OLen);
    end;
    MOff := 1;
    while true do
    begin
      MOff := 2 * MOff + GetBit;
      if Check(ILen >= src_len, -201) then Exit;
      if Check(MOff > $1000002, -203) then Exit;
      if GetBit > 0 then break;
      MOff := 2 * (MOff - 1) + GetBit;
    end;
    if MOff = 2 then
    begin
      MOff := LastMOff;
      MLen := GetBit;
    end
    else begin
      if Check(ILen >= src_len, -201) then Exit;
      MOff := 256 * (MOff - 3) + GetByte;
      if MOff = $FFFFFFFF then break;
      MLen := (not MOff) and 1;
      MOff := (MOff shr 1) + 1;
      LastMOff := MOff;
    end;
    if MLen > 0 then
      MLen := GetBit + 1
    else if GetBit > 0 then
      MLen := GetBit + 3
    else begin
      Inc(MLen);
      repeat
        MLen := 2 * MLen + GetBit;
        if Check(ILen >= src_len, -201) then Exit;
        if Check(MLen >= OEnd, -202) then Exit;
      until GetBit > 0;
      Inc(MLen, 3);
    end;
    if MOff > $500 then
      Inc(MLen);
    if Check(OLen + MLen > OEnd, -202) then Exit;
    if Check(MOff > OLen, -203) then Exit;
    PByteArray(dst)[OLen] := PByteArray(dst)[OLen - MOff];
    Inc(OLen);
    repeat
      PByteArray(dst)[OLen] := PByteArray(dst)[OLen - MOff];
      Inc(OLen);
      Dec(MLen);
    until MLen = 0;
  end;
  dst_len := OLen;
  if ILen = src_len then
    Result := 0
  else if ILen < src_len then
    Result := -205
  else
    Result := -201;
end;
{$ENDIF}

initialization

finalization

  Finalize(Macros);

end.
