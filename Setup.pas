unit Setup;

interface

uses
  Windows, AvL, avlUtils, SetupScript, LZMADecAPI;

type
  TProcessEntry = function(const Section: string; Index: Integer): Boolean of object;
  TSetupEngine = class
  private
    FLog: TSetupScript;
    procedure InitLog;
    function ProcessEntries(const Section: string; ProcessEntry: TProcessEntry): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

const
  MaxRunPrograms = 7;

var
  InitSetupError: string;
  IsInstaller: Boolean;
  Script: TSetupScript;
  SetupEngine: TSetupEngine;

function InitSetup: Boolean;
procedure FinalizeSetup;

implementation

uses
  SetupUtils, Localize;

const
  SMInstallSize = 'InstallSize';
  SMTempDir = 'TempDir';

procedure InitMacros;
const
  DirMacros: array[0..22] of record
    Name: string; ID: TSpecialFolder
  end = (
    (Name: 'Desktop'; ID: sfDesktop),
    (Name: 'CommonDesktop'; ID: sfCommonDesktop),
    (Name: 'StartMenu'; ID: sfStartMenu),
    (Name: 'CommonStartMenu'; ID: sfCommonStartMenu),
    (Name: 'Programs'; ID: sfPrograms),
    (Name: 'CommonPrograms'; ID: sfCommonPrograms),
    (Name: 'Startup'; ID: sfStartup),
    (Name: 'CommonStartup'; ID: sfCommonStartup),
    (Name: 'MyDocuments'; ID: sfMyDocuments),
    (Name: 'CommonDocuments'; ID: sfCommonDocuments),
    (Name: 'AppData'; ID: sfAppData),
    (Name: 'LocalAppData'; ID: sfLocalAppData),
    (Name: 'CommonAppData'; ID: sfCommonAppData),
    (Name: 'SendTo'; ID: sfSendTo),
    (Name: 'Templates'; ID: sfTemplates),
    (Name: 'CommonTemplates'; ID: sfCommonTemplates),
    (Name: 'Favorites'; ID: sfFavorites),
    (Name: 'CommonFavorites'; ID: sfCommonFavorites),
    (Name: 'ProgramFiles'; ID: sfProgramFiles),
    (Name: 'CommonProgramFiles'; ID: sfCommonProgramFiles),
    (Name: 'System'; ID: sfSystem),
    (Name: 'Windows'; ID: sfWindows),
    (Name: 'Fonts'; ID: sfFonts));
  ScriptMacros: array[0..3] of record
    Name, Def: string
  end = (
    (Name: SKProgramName; Def: ''),
    (Name: SKProgramVer; Def: ''),
    (Name: SKInstallPath; Def: '%ProgramFiles%\%' + SKProgramName + '%'),
    (Name: SKShortcutsFolder; Def: '%' + SKProgramName + '%'));
var
  i: Integer;
begin
  SetMacro('br', #13#10);
  for i := 0 to High(DirMacros) do
    SetMacro(DirMacros[i].Name, GetSpecialFolder(DirMacros[i].ID));
  SetMacro('Temp', TempDir);
  SetMacro(SMTempDir, UniTempFile);
  SetMacro(SMInstallSize, SizeToStr(FlSize(FullExeName))); //TODO: calculate actual setup size
  for i := 0 to High(ScriptMacros) do
    SetMacro(ScriptMacros[i].Name, SubstituteMacro(Script[SSOptions, ScriptMacros[i].Name, ScriptMacros[i].Def]));
end;

function InitSetup: Boolean;
var
  ScriptData: TStream;
begin
  Result := false;
  Script := TSetupScript.Create;
  if IsInstaller then
    ScriptData := LoadScript
  else if not FileExists(ParamStr(2)) then
  begin
    ReportError('Uninstallation data not found');
    Exit;
  end
    else ScriptData := TFileStream.Create(ParamStr(2), fmOpenRead);
  try
    if not Assigned(ScriptData) or (ScriptData.Size <= 0) then
    begin
      ReportError('No script data'); 
      Exit;
    end;
    Script.Load(ScriptData);
  finally
    ScriptData.Free;
  end;
  InitMacros;
  SetupEngine := TSetupEngine.Create;
  Result := true;
end;

procedure FinalizeSetup;
var
  TempDir: string;
begin
  FreeAndNil(SetupEngine);
  FreeAndNil(Script);
  TempDir := SubstituteMacro('%' + SMTempDir + '%');
  if DirectoryExists(TempDir) then
    DeleteDir(TempDir);
end;

{ TSetupEngine }

constructor TSetupEngine.Create;
begin
  inherited;
  if IsInstaller then
  begin
    Script[SSFiles, SKCount, ''] := '1';
    InitLog;
  end;
end;

destructor TSetupEngine.Destroy;
begin
  FreeAndNil(FLog);
  inherited;
end;

procedure TSetupEngine.InitLog;

  procedure CopySection(const SrcSection, DestSection: string);
  begin
    if not Assigned(Script.Section[SrcSection]) then Exit;
    FLog.AddSection(DestSection);
    FLog.Section[DestSection].Assign(Script.Section[SrcSection]);
  end;

var
  i: Integer;
begin
  FLog := TSetupScript.Create;
  CopySection(SSOptions, SSOptions);
  //TODO: Remove unnecessary options;
  //TODO: Replace constants like InstallPath
  CopySection(SSLanguage, SSLanguage);
  for i := 0 to High(UninstallRenames) do
    FLog.RenameValue(SSLanguage, IntToStr(Ord(UninstallRenames[i].Old)), IntToStr(Ord(UninstallRenames[i].New)));
  CopySection(SSUninstallIni, SSUninstallIni);
  CopySection(SSUninstallRegistry, SSUninstallRegistry);
  CopySection(SSUninstallDelete, SSUninstallDelete);
  CopySection(SSUninstallRun, SSUninstallRun);
  CopySection(SSUninstallWelcomeText, SSWelcomeText);
end;

function TSetupEngine.ProcessEntries(const Section: string; ProcessEntry: TProcessEntry): Boolean;
var
  i, Count: Integer;
begin
  Result := false;
  Count := StrToInt(Script[Section, SKCount, '0']);
  for i := 0 to Count - 1 do
    if not ProcessEntry(Section, i) then Exit; //TODO: Callbacks
  Result := true;
end;

end.
