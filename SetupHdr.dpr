program SetupHdr;

uses
  Windows, AvL, avlUtils, avlCRC32, MainForm, Setup, SetupScript, SetupUtils;

{$R *.res}
{$R Manifest.res}

const
  DoUninstallCmd = '/DoUninstall';
  ole32 = 'ole32.dll';

function CoInitialize(pvReserved: Pointer): HResult; stdcall; external ole32 name 'CoInitialize';
procedure CoUninitialize; stdcall; external ole32 name 'CoUninitialize';

begin
  InitCommonControls;
  CoInitialize(nil);
  try
    CRC32Initialization;
    {$IFDEF DEBUG}
    IsInstaller := true;
    {$ELSE}
    case CheckScript of
      ssOK: IsInstaller := true;
      ssNoScript: IsInstaller := false;
    else
      ReportError('Installation data is damaged');
      Exit;
    end;
    {$ENDIF}
    if IsInstaller or (ParamStr(1) = DoUninstallCmd) then
    try
      if not InitSetup then Exit;
      DefaultFont := Script[SSOptions, SKUIFont, DefaultFont];
      FormMain := TMainForm.Create;
      FormMain.SetPage(spWelcome);
      FormMain.Run;
      FormMain.Free;
    finally
      FinalizeSetup;
      if not IsInstaller then
        SelfDelete;
    end
      else WinExec(PChar(CopyToTemp + ' ' + DoUninstallCmd + ' "' + ChangeFileExt(FullExeName, '.ini') + '"'), SW_NORMAL);
  finally
    CoUninitialize;
  end;
end.
