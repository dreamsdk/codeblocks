program CodeBlocksPatcher;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, CBPatch, Version, SysTools;

type
  { TCodeBlocksPatcherApplication }
  TCodeBlocksPatcherApplication = class(TCustomApplication)
  private
    fCodeBlocksPatcher: TCodeBlocksPatcher;
    procedure WriteHeader;
  protected
    procedure DoInitialize;
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    property Patcher: TCodeBlocksPatcher
      read fCodeBlocksPatcher;
  end;

{ TCodeBlocksConfigurationInjector }

procedure TCodeBlocksPatcherApplication.WriteHeader;
var
  ReleaseType: string;

begin
  ReleaseType := EmptyStr;
  if IsDebugBuild then
    ReleaseType := '-DEBUG';

  WriteLn(Title, ', Ver. ', GetFileVersion, ReleaseType);
  WriteLn('Created by ', GetCompanyName, '.', sLineBreak);
end;

procedure TCodeBlocksPatcherApplication.DoRun;
const
  ERR_HELP_PASSED = 1;
  ERR_INVALID_OPTIONS = 2;
  ERR_PATCH_FAILED = 3;

var
  ErrorMsg: string;
  TaskResult, Result: Boolean;
  ProgramExitCode: Integer;

  procedure WriteBool(const B: Boolean);
  begin
    if B then
      WriteLn('OK!')
    else
      WriteLn('Failed!');
  end;

begin
  WriteHeader;

  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <> EmptyStr then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate(ERR_HELP_PASSED);
    Exit;
  end;

  // initialize the patcher engine
  ProgramExitCode := 0;
  DoInitialize;

  // execute it!
  with Patcher do
    if Ready then
    begin
      // Extracting the patch...
      Write('Patching Code::Blocks installation directory... ');
      TaskResult := InstallFilesPatch;
      WriteBool(TaskResult);
      Result := TaskResult;

      // Modifying the configuration...
      Write('Patching Code::Blocks configuration file... ');
      TaskResult := PatchConfiguration;
      WriteBool(TaskResult);
      Result := Result and TaskResult;

      // Updating the IDE configuration file...
      Write('Updating the DreamSDK configuration file... ');
      TaskResult := UpdateConfiguration;
      WriteBool(TaskResult);
      Result := Result and TaskResult;

      // Displaying the result...
      WriteLn(EmptyStr);
      if Result then
        WriteLn('Code::Blocks is now patched!')
      else
      begin
        WriteLn('Code::Blocks was NOT successfully patched!');
        ProgramExitCode := ERR_PATCH_FAILED;
      end;
    end
    else
    begin
      WriteLn('Error: Missing or invalid parameters.', sLineBreak);
      WriteHelp;
      ProgramExitCode := ERR_INVALID_OPTIONS;
    end;

  // stop program loop
  Terminate(ProgramExitCode);
end;

procedure TCodeBlocksPatcherApplication.DoInitialize;
const
  DEFAULT_SOURCE_DIR = 'data';

begin
  if ParamCount >= 3 then
    with Patcher do
    begin
      CodeBlocksInstallationDirectory := ParamStr(1);
      CodeBlocksConfigurationFileName := ParamStr(2);
      SoftwareDevelopmentKitHomeDirectory := ParamStr(3);
      SourceDirectory := GetApplicationPath + DEFAULT_SOURCE_DIR;
      if ParamCount > 3 then
        SourceDirectory := ParamStr(4);
    end;

{$IFDEF DEBUG}
  with Patcher do
  begin
    WriteLn('CodeBlocksInstallationDirectory: ', CodeBlocksInstallationDirectory);
    WriteLn('CodeBlocksConfigurationFileName: ', CodeBlocksConfigurationFileName);
    WriteLn('SoftwareDevelopmentKitHomeDirectory: ', SoftwareDevelopmentKitHomeDirectory);
    WriteLn('SourceDirectory: ', SourceDirectory);
  end;
{$ENDIF}
end;

constructor TCodeBlocksPatcherApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StopOnException := True;
  fCodeBlocksPatcher := TCodeBlocksPatcher.Create;
end;

destructor TCodeBlocksPatcherApplication.Destroy;
begin
  fCodeBlocksPatcher.Free;
  inherited Destroy;
end;

procedure TCodeBlocksPatcherApplication.WriteHelp;
var
  CmdName: string;

begin
  CmdName := ExtractFileName(ChangeFileExt(ExeName, EmptyStr));
  WriteLn('Usage:', sLineBreak,
    '  ', CmdName, ' <CB_INST_DIR> <CB_CONF_FILE> <DREAMSDK_DIR> [SOURCE_DIR] ', sLineBreak,
    sLineBreak,
    'Description:', sLineBreak,
    '  CB_INST_DIR  : The Code::Blocks installation directory (typically ', sLineBreak,
    '                 ''%ProgramFiles%\CodeBlocks\'' on 32-bits systems or', sLineBreak,
    '                 ''%ProgramFiles(x86)%\CodeBlocks\'' on 64-bits systems).', sLineBreak,
    '  CB_CONF_FILE : The Code::Blocks configuration file to patch (typically ', sLineBreak,
    '                 ''%AppData%\CodeBlocks\default.conf'').', sLineBreak,
    '  DREAMSDK_DIR : The DreamSDK Home directory (typically ''C:\DreamSDK\'').', sLineBreak,
    '  SOURCE_DIR   : Optional. The directory where are stored the required', sLineBreak,
    '                 files for the patcher. Default value is ''.\data\''.', sLineBreak,
    sLineBreak,
    'Note:', sLineBreak,
    '  This patcher was made for the Code::Blocks 17.12 stable release only.', sLineBreak,
    '  It was made for DreamSDK and it is not affiliated with the Code::Blocks Team', sLineBreak,
    '  in any way.');
end;

var
  Application: TCodeBlocksPatcherApplication;

{$R *.res}

begin
  Application := TCodeBlocksPatcherApplication.Create(nil);
  try
    Application.Title:='Code::Blocks Patcher';
    Application.Run;
  finally
    Application.Free;
  end;
end.

