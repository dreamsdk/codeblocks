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
  REINSTALL_SWITCH = '/REINSTALL';
  UNINSTALL_SWITCH = '/UNINSTALL';

  FORM_TITLE = 'Code::Blocks Splash Application for DreamSDK';
  PANEL_INSTALL_CAPTION = 'Setting up Code::Blocks';
  PANEL_REINSTALL_CAPTION = 'Reinstalling Code::Blocks patch';
  PANEL_UNINSTALL_CAPTION = 'Restoring Code::Blocks';

type
  TCodeBlocksSplashOperation = (soUnknown, soInstall, soUninstall, soReinstall, soClose);

var
  FormHandle: THandle;
  Operation: TCodeBlocksSplashOperation;

function ParseCommandLine: Boolean;
var
  Parameter: string;

begin
  Operation := soUnknown;
  Result := (ParamCount > 0);
  if Result then
  begin
    Parameter := UpperCase(ParamStr(1));
    if SameText(Parameter, CLOSE_SWITCH) then
      Operation := soClose
    else if SameText(Parameter, INSTALL_SWITCH) then
      Operation := soInstall
    else if SameText(Parameter, REINSTALL_SWITCH) then
      Operation := soReinstall
    else if SameText(Parameter, UNINSTALL_SWITCH) then
      Operation := soUninstall;
  end;
end;

begin
  Operation := soUnknown;
  if not ParseCommandLine then
    Exit;

  if (Operation = soClose) then
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
    Application.Scaled := True;
    Application.Initialize;
    frmMain := TfrmMain.Create(Application);
    frmMain.Caption := FORM_TITLE;

    case Operation of
      soInstall:
        SetPanelText(PANEL_INSTALL_CAPTION);
      soUninstall:
        SetPanelText(PANEL_UNINSTALL_CAPTION);
      soReinstall:
        SetPanelText(PANEL_REINSTALL_CAPTION);
    end;

    frmMain.Show;
    Application.Run;
  end;
end.

