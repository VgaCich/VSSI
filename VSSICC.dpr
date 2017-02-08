program VSSICC;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Windows, AvL, avlUtils, SetupScript, UCLAPI, avlCRC32;

var
  Script: TSetupScript = nil;
  OutFile: TFileStream = nil;
  ArchiveOffset, ScriptOffset: Integer;

procedure PrepareSetup;
var
  OutFileName: string;
begin
  WriteLn('Writing stub');
  OutFileName := Script[SSOptions, SKOutFile, ChangeFileExt(ParamStr(1), '.exe')];
  if not CopyFile('SetupHdr.exe', PChar(OutFileName), false) then
    begin
      WriteLn('Error: Can''t create output file:');
      WriteLn(SysErrorMessage(GetLastError));
      Halt(1);
    end;
  OutFile := TFileStream.Create(OutFileName, fmOpenReadWrite);
  OutFile.Seek(0, soFromEnd);
end;

procedure ArchiveFiles;
begin
  WriteLn('Archiving files');
  ArchiveOffset := OutFile.Position;
end;

procedure LoadWelcomeText(const FileName, SectionName: string);
var
  i: Integer;
begin
  if FileExists(FileName) then
  begin
    if not Assigned(Script.Section[SectionName]) then
      Script.AddSection(SectionName);
    with Script.Section[SectionName] do
    begin
      LoadFromFile(FileName);
      for i := 0 to Count - 1 do
        if ((Strings[i] <> '') and (Strings[i][1] = ';')) or
           ((Trim(Strings[i]) <> '') and (Trim(Strings[i])[1] = '[')) then
          Strings[i] := ';' + Strings[i];
    end;
  end;
end;

procedure PrepareScript;
var
  Temp: TStringList;
  i, j: Integer;
begin
  WriteLn('Preparing script');
  LoadWelcomeText(Script[SSOptions, SKWelcomeText, ''], SSWelcomeText);
  LoadWelcomeText(Script[SSOptions, SKUninstallWelcomeText, ''], SSUninstallWelcomeText);
  Script.DeleteSection(SSFiles);
  with Script.Section[SSOptions] do
  begin
    Values[SKOutFile] := '';
    Values[SKWelcomeText] := '';
    Values[SKUninstallWelcomeText] := '';
  end;
  Temp := Script.GetSectionNames;
  try
    for i := 0 to Temp.Count - 1 do
      if not (SameText(Temp[i], SSWelcomeText) or SameText(Temp[i], SSUninstallWelcomeText)) then //TODO: filter other non-KV-pair sections
        with Script.Section[Temp[i]] do
          for j := Count - 1 downto 0 do
            if (Trim(Strings[j]) = '') or (Trim(Strings[j])[1] = ';') then
              Delete(j);
  finally
    Temp.Free;
  end;
end;

procedure WriteScript;
var
  Temp: TMemoryStream;
  ScriptSize, OutSize: Cardinal;
begin
  WriteLn('Writing script');
  ScriptOffset := OutFile.Position;
  Temp := TMemoryStream.Create;
  try
    Script.Save(Temp);
    Temp.Position := 0;
    ScriptSize := Temp.Size;
    Temp.Size := ScriptSize + UCLOutputBlockSize(ScriptSize);
    Move(Temp.Memory^, IncPtr(Temp.Memory, Temp.Size - ScriptSize)^, ScriptSize);
    OutSize := Temp.Size;
    if ucl_nrv2e_99_compress(IncPtr(Temp.Memory, Temp.Size - ScriptSize), ScriptSize, Temp.Memory, OutSize, nil, 10, nil, nil) <> UCL_E_OK then
    begin
      WriteLn('Script compression error');
      Halt(1);
    end;
    OutFile.WriteBuffer(ScriptSize, SizeOf(ScriptSize));
    OutFile.CopyFrom(Temp, OutSize);
  finally
    Temp.Free;
  end;
end;

procedure FinalizeSetup;
var
  CRC: Cardinal;
begin
  WriteLn('Finalizing setup');
  OutFile.WriteBuffer(ArchiveOffset, SizeOf(ArchiveOffset));
  OutFile.WriteBuffer(ScriptOffset, SizeOf(ScriptOffset));
  OutFile.Seek(ScriptOffset, soFromBeginning);
  CRC := StreamCRC32(OutFile, OutFile.Size - OutFile.Position);
  OutFile.WriteBuffer(CRC, SizeOf(CRC));
end;

begin
  CRC32Initialization;
  WriteLn('VSSI command-line compiler 1.0');
  if ParamCount < 1 then
  begin
    WriteLn('Usage: VSSICC <setup script>');
    Halt(1);
  end;
  if not FileExists(ParamStr(1)) then
  begin
    WriteLn('File "' + ParamStr(1) + '" don''t exists');
    Halt(1);
  end;
  Script := TSetupScript.Create;
  try
    Script.Load(ParamStr(1));
    PrepareSetup;
    ArchiveFiles;
    PrepareScript;
    WriteScript;
    FinalizeSetup;
    WriteLn('Done');
  finally
    FreeAndNil(OutFile);
    FreeAndNil(Script);
  end;
end.