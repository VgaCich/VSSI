unit SetupScript;

interface

uses
  {$IFDEF USE_VCL}SysUtils, Classes{$ELSE}AvL{$ENDIF};

type
  TSetupScript = class
  private
    FSections: array of record
      Name: string;
      Data: TStringList;
    end;
    function FindSection(const Name: string): Integer;
    function GetSection(const Name: string): TStringList;
    function GetValue(const Section, Key, Def: string): string;
    procedure SetValue(const Section, Key, Def: string; const Value: string);
  public
    destructor Destroy; override;
    procedure Clear;
    procedure Load(const FileName: string); overload;
    procedure Load(Stream: TStream); overload;
    procedure Save(Stream: TStream);
    function AddSection(const Name: string): TStringList;
    procedure DeleteSection(const Name: string);
    function GetSectionNames: TStringList;
    procedure AddListEntry(const Section: string; const Keys, Values: array of string);
    procedure RenameValue(const Section, OldName, NewName: string);
    property Value[const Section, Key, Def: string]: string read GetValue write SetValue; default;
    property Section[const Name: string]: TStringList read GetSection;
  end;

const
  SSOptions = 'Options';
  SKOutFile = 'OutFile';
  SKUIFont = 'UIFont';
  SKProgramName = 'ProgramName';
  SKWelcomeText = 'WelcomeText';
  SKUninstallWelcomeText = 'UninstallWelcomeText';
  SKProgramVer = 'ProgramVer';
  SKInstallPath = 'InstallPath';
  SKShortcutsFolder = 'ShortcutsFolder';
  SSLanguage = 'Language';
  SSFiles = 'Files';
  SSWelcomeText = 'WelcomeText';
  SSUninstallWelcomeText = 'UninstallWelcomeText';
  SSShortcuts = 'Shortcuts';
  SSRun = 'Run';
  SSInstallIni = 'InstallIni';
  SSUninstallIni = 'UninstallIni';
  SSInstallRegistry = 'InstallRegistry';
  SSUninstallRegistry = 'UninstallRegistry';
  SSInstallDelete = 'InstallDelete';
  SSUninstallDelete = 'UninstallDelete';
  SSInstallRun = 'InstallRun';
  SSUninstallRun = 'UninstallRun';
  SKCount = 'Count';

implementation

{ TSetupScript }

procedure TSetupScript.DeleteSection(const Name: string);
var
  Index: Integer;
begin
  Index := FindSection(Name);
  if Index >= 0 then
    with FSections[Index] do
    begin
      Name := '';
      FreeAndNil(Data);
    end;
end;

destructor TSetupScript.Destroy;
begin
  Clear;
  inherited;
end;

function TSetupScript.AddSection(const Name: string): TStringList;
var
  i: Integer;
begin
  if FindSection(Name) < 0 then
  begin
    i := 0;
    while i <= High(FSections) do
    begin
      if FSections[i].Name = '' then Break;
      Inc(i);
    end;
    if i > High(FSections) then SetLength(FSections, Length(FSections) + 1);
    FSections[i].Name := Name;
    FSections[i].Data := TStringList.Create;
    Result := FSections[i].Data;
  end
    else Result := GetSection(Name);
end;

procedure TSetupScript.Clear;
var
  i: Integer;
begin
  for i := 0 to High(FSections) do
    with FSections[i] do
    begin
      Name := '';
      FreeAndNil(Data);
    end;
  SetLength(FSections, 0);
end;

function TSetupScript.FindSection(const Name: string): Integer;
begin
  for Result := 0 to High(FSections) do
    if (Name <> '') and SameText(FSections[Result].Name, Name) then Exit;
  Result := -1;
end;

function TSetupScript.GetSection(const Name: string): TStringList;
var
  Index: Integer;
begin
  Result := nil;
  Index := FindSection(Name);
  if Index >= 0 then
    Result := FSections[Index].Data;
end;

procedure TSetupScript.Load(const FileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmOpenRead);
  try
    Load(F);
  finally
    F.Free;
  end;
end;

procedure TSetupScript.Load(Stream: TStream);
var
  Temp: TStringList;
  CurSection: TStringList;
  S: string;
  i: Integer;
begin
  Clear;
  CurSection := nil;
  Temp := TStringList.Create;
  try
    Temp.LoadFromStream(Stream);
    for i := 0 to Temp.Count - 1 do
    begin
      S := Trim(Temp[i]);
      if (S <> '') and (S[1] = '[') and (S[Length(S)] = ']') then
        CurSection := AddSection(Copy(S, 2, Length(S) - 2))
      else if Assigned(CurSection) then
        CurSection.Add(Temp[i]);
    end;
  finally
    Temp.Free;
  end;
end;

procedure TSetupScript.Save(Stream: TStream);
var
  i: Integer;
  Temp: string;
begin
  for i := 0 to High(FSections) do
    with FSections[i] do
      if Name <> '' then
      begin
        Temp := '[' + Name + ']'#13#10;
        Stream.WriteBuffer(Temp[1], Length(Temp));
        Data.SaveToStream(Stream);
      end;
end;

function TSetupScript.GetValue(const Section, Key, Def: string): string;
begin
  if Assigned(Self.Section[Section]) and (Self.Section[Section].IndexOfName(Key) >= 0) then
    Result := Self.Section[Section].Values[Key]
  else
    Result := Def;
end;

procedure TSetupScript.SetValue(const Section, Key, Def: string; const Value: string);
begin
  if not Assigned(Self.Section[Section]) then
    AddSection(Section);
  Self.Section[Section].Values[Key] := Value;
end;

function TSetupScript.GetSectionNames: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to High(FSections) do
    if FSections[i].Name <> '' then
      Result.Add(FSections[i].Name);
end;

procedure TSetupScript.AddListEntry(const Section: string; const Keys, Values: array of string);
var
  i: Integer;
  Index: string;
begin
  if High(Keys) <> High(Values) then Exit;
  i := StrToInt(Value[Section, SKCount, '0']);
  Index := IntToStr(i);
  Value[Section, SKCount, ''] := IntToStr(i + 1);
  for i := 0 to High(Keys) do
    Value[Section, Keys[i] + Index, ''] := Values[i];
end;

procedure TSetupScript.RenameValue(const Section, OldName, NewName: string);
begin
  if Assigned(Self.Section[Section]) then
    with Self.Section[Section] do
    begin
      Values[NewName] := Values[OldName];
      Values[OldName] := '';
    end;
end;

end.
