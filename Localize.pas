unit Localize;

interface

uses
  AvL;

type
  TLocalizeStringID = (lsSetupCaption, lsUninstallCaption, lsBtnPrev, lsBtnNext, lsBtnCancel, lsBtnFinish,
    lsWelcomeText, lsWelcomeHeaderCaption, lsWelcomeHeaderText, lsWelcomeBtnNext,
    lsUninstallWelcomeText, lsUninstallWelcomeHeaderCaption, lsUninstallWelcomeHeaderText, lsUninstallWelcomeBtnNext,
    lsInstallPathHeaderCaption, lsInstallPathHeaderText, lsInstallPathText, lsInstallPathBrowse, lsInstallPathBrowseTitle,
    lsShortcutsPathHeaderCaption, lsShortcutsPathHeaderText, lsShortcutsPathCreateShortcuts, lsShortcutsPathCreateOnDesktop,
    lsProgressHeaderCaption, lsProgressHeaderText,
    lsUninstallProgressHeaderCaption, lsUninstallProgressHeaderText,
    lsFinishHeaderCaption, lsFinishHeaderText,
    lsUninstallFinishHeaderCaption, lsUninstallFinishHeaderText, lsFinishLeftoverLabel);
//TODO: Rename lsUninstall* to ls* on uninstaller script generation

const
  UninstallRenames: array[0..8] of record Old, New: TLocalizeStringID end = (
    (Old: lsUninstallCaption; New: lsSetupCaption),
    (Old: lsUninstallWelcomeText; New: lsWelcomeText),
    (Old: lsUninstallWelcomeHeaderCaption; New: lsWelcomeHeaderCaption),
    (Old: lsUninstallWelcomeHeaderText; New: lsWelcomeHeaderText),
    (Old: lsUninstallWelcomeBtnNext; New: lsWelcomeBtnNext),
    (Old: lsUninstallProgressHeaderCaption; New: lsProgressHeaderCaption),
    (Old: lsUninstallProgressHeaderText; New: lsProgressHeaderText),
    (Old: lsUninstallFinishHeaderCaption; New: lsFinishHeaderCaption),
    (Old: lsUninstallFinishHeaderText; New: lsFinishHeaderText));

function L(ID: TLocalizeStringID): string;

implementation

uses
  Setup, SetupScript, SetupUtils;

const
  DefLocalization: array[TLocalizeStringID] of string = (
    '%ProgramName% %ProgramVer% Setup', 'Uninstall %ProgramName% %ProgramVer%', '< Back', 'Next >', 'Cancel', 'Finish',
    'This program will install %ProgramName% %ProgramVer% to your computer', 'Welcome', '%ProgramName% setup', 'Next >',
    'This program will uninstall %ProgramName% %ProgramVer% from your computer', 'Welcome', 'Uninstall %ProgramName%', 'Uninstall',
    'Install path', 'Select install path', '%InstallSize% of space needed', 'Browse...', 'Select destination directory',
    'Shortcuts', 'Configure shortcuts', 'Create shortcuts', 'Create shortcut on desktop',
    'Install', 'Installing %ProgramName%...',
    'Uninstall', 'Uninstalling %ProgramName%...',
    'Finish', '%ProgramName% successfully installed',
    'Finish', '%ProgramName% successfully uninstalled', 'Delete leftovers:'
  );

function L(ID: TLocalizeStringID): string;
begin
  Result := SubstituteMacro(Script[SSLanguage, IntToStr(Ord(ID)), DefLocalization[ID]]);
end;

end.