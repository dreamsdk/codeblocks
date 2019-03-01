program CBSplash;

{$mode objfpc}{$H+}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Main
  { you can add units after this };

{$R *.res}

const
  CLOSE_SWITCH = '/CLOSE';
  INSTALL_SWITCH = '/INSTALL';

  FORM_TITLE = 'Code::Blocks Splash Application for DreamSDK';
  PANEL_INSTALL_CAPTION = 'Setting up Code::Blocks... Please wait.';
  PANEL_UNINSTALL_CAPTION = 'Restoring Code::Blocks... Please wait.';

var
  FormHandle: THandle;
  IsCloseAction: Boolean = False;
  IsInstallMode: Boolean = False;

function ParseCommandLine: Boolean;
begin
  Result := (ParamCount > 0);
  IsInstallMode := (UpperCase(ParamStr(1)) = INSTALL_SWITCH);
  IsCloseAction := (UpperCase(ParamStr(1)) = CLOSE_SWITCH);
end;

begin
  if not ParseCommandLine then
    Exit;

  if IsCloseAction then
  begin
{$IFDEF WINDOWS}
    FormHandle := FindWindow(nil, PChar(FORM_TITLE));
    if FormHandle <> INVALID_HANDLE_VALUE then
      SendMessage(FormHandle, WM_CLOSE, 0, 0);
{$ENDIF}
  end
  else
  begin
    RequireDerivedFormResource := True;
  Application.Scaled:=True;
    Application.Initialize;
    frmMain := TfrmMain.Create(Application);
    frmMain.Caption := FORM_TITLE;
    frmMain.pnlSplash.Caption := PANEL_INSTALL_CAPTION;
    if not IsInstallMode then
      frmMain.pnlSplash.Caption := PANEL_UNINSTALL_CAPTION;
    frmMain.Show;
    Application.Run;
  end;
end.

