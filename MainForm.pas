unit MainForm;

//TODO: Solve "program installed incorrectly..." problem

interface

uses
  Windows, Messages, AvL, avlUtils, Setup;

type
  TSetupPages = (spWelcome, spInstallPath, spShortcutsPath, spProgress, spFinish);
  TImageEx = class(TImage)
  public
    property BkMode;
  end;
  TRichEditEx = class(TRichEdit)
  private
  public
    constructor Create(Parent: TWinControl);
    procedure Add(const Line: string);
    procedure Insert(const Line: string; Index: Integer);
    function Count: Integer;
  end;
  THeader = class(TSimplePanel)
    Caption, Text: TLabel;
    Image: TImageEx;
  public
    constructor Create(Parent: TWinControl);
  end;
  TSetupPage = class(TSimplePanel)
  public
    constructor Create(Parent: TWinControl); virtual;
    procedure Show; virtual;
    procedure Apply; virtual;
    function Skip: Boolean; virtual;
    function Cancel: Boolean; virtual;
  end;
  TMainForm = class(TForm)
    BtnPrev, BtnNext, BtnCancel: TButton;
    Header: THeader;
    Pages: array[TSetupPages] of TSetupPage;
  private
    FCurPage: TSetupPages;
    procedure CancelClick(Sender: TObject);
    function FormClose(Sender: TObject): Boolean;
    procedure PrevNextClick(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetHeaderText(const Caption, Text: string);
    function SetPage(Page: TSetupPages): Boolean;
  end;
  TPageWelcome = class(TSetupPage)
    InfoText: TRichEditEx;
  public
    constructor Create(Parent: TWinControl); override;
    procedure Show; override;
    procedure Apply; override;
  end;
  TPageInstallPath = class(TSetupPage)
    InstallPath: TEdit;
    BtnBrowse: TButton;
  private
    procedure BrowseClick(Sender: TObject);
  public
    constructor Create(Parent: TWinControl); override;
    procedure Show; override;
    procedure Apply; override;
    function Skip: Boolean; override;
  end;
  TPageShortcutsPath = class(TSetupPage)
    ShortcutsPath: TEdit;
    MenuFolders: TListBox;
    CreateShortcuts, CreateOnDesktop: TCheckBox;
  private
    procedure MenuFoldersChange(Sender: TObject);
  public
    constructor Create(Parent: TWinControl); override;
    procedure Show; override;
    procedure Apply; override;
    function Skip: Boolean; override;
  end;
  TPageProgress = class(TSetupPage)
    Progress: TProgressBar;
    Log: TRichEditEx;
  public
    constructor Create(Parent: TWinControl); override;
    procedure Show; override;
  end;
  TPageFinish = class(TSetupPage)
    RunChecks: array[0 .. MaxRunPrograms - 1] of TCheckBox;
    LeftoversLabel: TLabel;
    Leftovers: TListBox;
  public
    constructor Create(Parent: TWinControl); override;
    procedure Show; override;
  end;

var
  FormMain: TMainForm;

implementation

uses
  SetupScript, SetupUtils, Localize;

type
  TSetupPageClass = class of TSetupPage;
  TCharRange = record
    cpMin: Longint;
    cpMax: LongInt;
  end;

const
  SetupPages: array[TSetupPages] of TSetupPageClass =
    (TPageWelcome, TPageInstallPath, TPageShortcutsPath, TPageProgress, TPageFinish);
  EM_EXSETSEL = WM_USER + 55; 

{ TMainForm }

constructor TMainForm.Create;
var
  Page: TSetupPages;
begin
  inherited Create(nil, L(lsSetupCaption));
  BorderStyle := bsSingle;
  BorderIcons := [biSystemMenu, biMinimize];
  SetSize(480, 350);
  Position := poScreenCenter;
  OnClose := FormClose;
  Header := THeader.Create(Self);
  TSimplePanel.Create(Self, '').SetBounds(5, ClientHeight - 36, ClientWidth - 10, 2);
  BtnPrev := TButton.Create(Self, L(lsBtnPrev));
  BtnPrev.SetPosition(ClientWidth - 250, ClientHeight - 30);
  BtnPrev.Tag := -1;
  BtnPrev.OnClick := PrevNextClick;
  BtnNext := TButton.Create(Self, L(lsBtnNext));
  BtnNext.SetPosition(ClientWidth - 175, ClientHeight - 30);
  BtnNext.Default := true;
  BtnNext.Tag := 1;
  BtnNext.OnClick := PrevNextClick;
  BtnCancel := TButton.Create(Self, L(lsBtnCancel));
  BtnCancel.SetPosition(ClientWidth - 80, ClientHeight - 30);
  BtnCancel.OnClick := CancelClick;
  for Page := Low(TSetupPages) to High(TSetupPages) do
  begin
    Pages[Page] := SetupPages[Page].Create(Self);
    Pages[Page].BringToFront;
    Pages[Page].Visible := false;
  end;
  //TODO: System Menu -> About
end;

destructor TMainForm.Destroy;
begin

  inherited;
end;

procedure TMainForm.CancelClick(Sender: TObject);
begin
  Close;
end;

function TMainForm.FormClose(Sender: TObject): Boolean;
begin
  Result := Pages[FCurPage].Cancel;
end;

procedure TMainForm.PrevNextClick(Sender: TObject);
var
  Page: TSetupPages;
begin
  Page := FCurPage;
  repeat
    Page := TSetupPages(Integer(Page) + (Sender as TWinControl).Tag);
  until (Page < Low(TSetupPages)) or (Page > High(TSetupPages)) or SetPage(Page);
end;

function TMainForm.SetPage(Page: TSetupPages): Boolean;
begin
  Result := not Pages[Page].Skip;
  if Result then
  begin
    BtnPrev.Enabled := Page <> Low(TSetupPages);
    BtnNext.Enabled := Page <> High(TSetupPages);
    Pages[FCurPage].Apply;
    Pages[Page].Show;
    FCurPage := Page;
  end;
end;

procedure TMainForm.SetHeaderText(const Caption, Text: string);
begin
  Header.Caption.Caption := Caption;
  Header.Text.Caption := Text;
end;

{ TRichEditEx }

procedure TRichEditEx.Insert(const Line: string; Index: Integer);
var
  L: Integer;
  Selection: TCharRange;
  Str: string;
begin
  if Index >= 0 then
  begin
    Selection.cpMin := Perform(EM_LINEINDEX, Index, 0);
    if Selection.cpMin >= 0 then
      Str := Line + #13#10
    else begin
      Selection.cpMin := Perform(EM_LINEINDEX, Index - 1, 0);
      if Selection.cpMin < 0 then Exit;
      L := Perform(EM_LINELENGTH, Selection.cpMin, 0);
      if L = 0 then Exit;
      Inc(Selection.cpMin, L);
      Str := #13#10 + Line;
    end;
    Selection.cpMax := Selection.cpMin;
    Perform(EM_EXSETSEL, 0, Longint(@Selection));
    Perform(EM_REPLACESEL, 0, LongInt(PChar(Str)));
  end;
end;

function TRichEditEx.Count: Integer;
begin
  Result := Perform(EM_GETLINECOUNT, 0, 0);
  if Perform(EM_LINELENGTH, Perform(EM_LINEINDEX, Result - 1, 0), 0) = 0 then
    Dec(Result);
end;

constructor TRichEditEx.Create(Parent: TWinControl);
begin
  inherited Create(Parent, '', true);
  ReadOnly := true;
  MaxLength := -1;
  ExStyle := (ExStyle and not WS_EX_CLIENTEDGE) or WS_EX_STATICEDGE;
end;

procedure TRichEditEx.Add(const Line: string);
begin
  Insert(Line, Count);
end;

{ THeader }

constructor THeader.Create(Parent: TWinControl);
begin
  inherited Create(Parent, '');
  Border := 2;
  ExStyle := 0;
  Width := Parent.ClientWidth;
  Height := 64;
  Self.Color := clWhite;
  Caption := TLabel.Create(Self, '');
  Caption.BkMode := bk_Transparent;
  Caption.Font.Style := [fsBold];
  Caption.SetBounds(5, 10, ClientWidth - 70, Caption.Height);
  Text := TLabel.Create(Self, '');
  Text.BkMode := bk_Transparent;
  Text.SetBounds(5, 30, ClientWidth - 70, 30);
  Image := TImageEx.Create(Self);
  Image.SetBounds(ClientWidth - 48, 16, 32, 32);
  Image.BkMode := bk_Transparent;
  Image.IconHandle := LoadIcon(hInstance, 'MAINICON');
  TSimplePanel.Create(Self, '').SetBounds(0, ClientHeight - 2, ClientWidth, 2);
end;

{ TSetupPage }

constructor TSetupPage.Create(Parent: TWinControl);
begin
  inherited Create(Parent, '');
  Border := 2;
  ExStyle := 0;
  Top := 64;
  Width := Parent.ClientWidth;
  Height := Parent.ClientHeight - 100;
end;

procedure TSetupPage.Show;
begin
  Visible := true;
end;

procedure TSetupPage.Apply;
begin
  Visible := false;
end;

function TSetupPage.Skip: Boolean;
begin
  Result := FormMain.FCurPage = spFinish;
end;

function TSetupPage.Cancel: Boolean;
begin
  Result := true; //TODO: Ask user
end;

{ TPageWelcome }

constructor TPageWelcome.Create(Parent: TWinControl);
var
  i: Integer;
begin
  inherited;
  Caption := L(lsWelcomeText);
  InfoText := TRichEditEx.Create(Self);
  InfoText.SetBounds(5, 5, ClientWidth - 10, ClientHeight - 10);
  InfoText.Visible := IsInstaller;
  if Assigned(Script.Section[SSWelcomeText]) then
    with Script.Section[SSWelcomeText] do
    begin
      for i := 0 to Count - 1 do
        if (Length(Strings[i]) > 0) and (Strings[i][1] = ';') then
          Strings[i] := Copy(Strings[i], 2, MaxInt);
      InfoText.Text := Text;
      InfoText.Visible := true
    end
  else
    InfoText.Visible := false;
end;

procedure TPageWelcome.Show;
begin
  inherited;
  FormMain.SetHeaderText(L(lsWelcomeHeaderCaption), L(lsWelcomeHeaderText));
  FormMain.BtnNext.Caption := L(lsWelcomeBtnNext);
end;

procedure TPageWelcome.Apply;
begin
  inherited;
  FormMain.BtnNext.Caption := L(lsBtnNext);
end;

{ TPageInstallPath }

constructor TPageInstallPath.Create(Parent: TWinControl);
begin
  inherited;
  InstallPath := TEdit.Create(Self, '');
  InstallPath.SetBounds(5, ClientHeight div 2 - 40, ClientWidth - 90, 25);
  BtnBrowse := TButton.Create(Self, L(lsInstallPathBrowse));
  BtnBrowse.SetPosition(ClientWidth - 80, InstallPath.Top);
  BtnBrowse.OnClick := BrowseClick;
end;

procedure TPageInstallPath.BrowseClick(Sender: TObject);
var
  Path: string;
begin
  Path := InstallPath.Text;
  if OpenDirDialog(Parent.Handle, L(lsInstallPathBrowseTitle), true, Path) then
    InstallPath.Text := Path;
  //TODO: Check for free space
end;

procedure TPageInstallPath.Show;
begin
  inherited;
  Caption := L(lsInstallPathText);
  FormMain.SetHeaderText(L(lsInstallPathHeaderCaption), L(lsInstallPathHeaderText));
  InstallPath.Text := SubstituteMacro('%' + SKInstallPath + '%');
end;

procedure TPageInstallPath.Apply;
begin
  inherited;
  SetMacro(SKInstallPath, InstallPath.Text);
end;

function TPageInstallPath.Skip: Boolean;
begin
  Result := inherited Skip or not IsInstaller;
end;

{ TPageShortcutsPath }

constructor TPageShortcutsPath.Create(Parent: TWinControl);
var
  FoldersList: TStringList;
  i: Integer;
begin
  inherited;
  ShortcutsPath := TEdit.Create(Self, '');
  ShortcutsPath.SetBounds(5, 5, ClientWidth - 10, ShortcutsPath.Height);
  MenuFolders := TListBox.Create(Self, lbSorted);
  MenuFolders.SetBounds(5, 35, ClientWidth - 10, ClientHeight - 60);
  MenuFolders.OnChange := MenuFoldersChange;
  CreateShortcuts := TCheckBox.Create(Self, L(lsShortcutsPathCreateShortcuts));
  CreateShortcuts.SetBounds(5, ClientHeight - 20, ClientWidth div 2 - 10, CreateShortcuts.Height);
  CreateShortcuts.Checked := true; //TODO: Disable controls when unchecked
  CreateOnDesktop := TCheckBox.Create(Self, L(lsShortcutsPathCreateOnDesktop));
  CreateOnDesktop.SetBounds(ClientWidth div 2, ClientHeight - 20, ClientWidth div 2 - 5, CreateOnDesktop.Height);
  CreateOnDesktop.Checked := true;
  FoldersList := TStringList.Create;
  try
    SearchFolders(GetSpecialFolder(sfPrograms), FoldersList);
    SearchFolders(GetSpecialFolder(sfCommonPrograms), FoldersList);
    for i := 0 to FoldersList.Count - 1 do
      MenuFolders.ItemAdd(FoldersList[i]);
  finally
    FoldersList.Free;
  end;
end;

procedure TPageShortcutsPath.MenuFoldersChange(Sender: TObject);
begin
  ShortcutsPath.Text := MenuFolders.Items[MenuFolders.ItemIndex] + '\' + ExtractFileName(ShortcutsPath.Text);
end;

procedure TPageShortcutsPath.Show;
begin
  inherited;
  FormMain.SetHeaderText(L(lsShortcutsPathHeaderCaption), L(lsShortcutsPathHeaderText));
  ShortcutsPath.Text := SubstituteMacro('%' + SKShortcutsFolder + '%');
end;

procedure TPageShortcutsPath.Apply;
begin
  inherited;
  SetMacro(SKShortcutsFolder, ShortcutsPath.Text);
end;

function TPageShortcutsPath.Skip: Boolean;
begin
  Result := inherited Skip or not IsInstaller;
end;

{ TPageProgress }

constructor TPageProgress.Create(Parent: TWinControl);
begin
  inherited;
  Progress := TProgressBar.Create(Self);
  Progress.SetBounds(5, 5, ClientWidth - 10, 20);
  Log := TRichEditEx.Create(Self);
  Log.SetBounds(5, 30, ClientWidth - 10, ClientHeight - 35);
end;

procedure TPageProgress.Show;
begin
  inherited;
  FormMain.SetHeaderText(L(lsProgressHeaderCaption), L(lsProgressHeaderText));
end;

{ TPageFinish }

constructor TPageFinish.Create(Parent: TWinControl);
var
  i: Integer;
begin
  inherited;
  if IsInstaller then
  begin
    for i := 0 to MaxRunPrograms - 1 do
    begin
      RunChecks[i] := TCheckBox.Create(Self, SubstituteMacro('Run %ProgramName% #' + IntToStr(i + 1)));
      RunChecks[i].SetBounds(5, 5 + i * 30, ClientWidth - 10, RunChecks[i].Height);
    end;
  end
  else begin
    LeftoversLabel := TLabel.Create(Self, L(lsFinishLeftoverLabel));
    LeftoversLabel.SetBounds(5, 5, ClientWidth - 10, LeftoversLabel.Height);
    Leftovers := TListBox.Create(Self, lbMultipleSel);
    Leftovers.SetBounds(5, 25, ClientWidth - 10, ClientHeight - 30);
    Leftovers.Perform(LB_DIR, 0, Integer(PChar(ExePath + '\*')));
  end;
end;

procedure TPageFinish.Show;
begin
  inherited;
  FormMain.SetHeaderText(L(lsFinishHeaderCaption), L(lsFinishHeaderText));
  FormMain.BtnPrev.Enabled := false;
  FormMain.BtnCancel.Caption := L(lsBtnFinish);
end;

end.
