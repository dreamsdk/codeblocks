program CodeBlocksPatcher;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  FSTools,
  RefBase,
  Version,
  SysTools,
  CBPatch, xmltools;

const
  ERR_SUCCESS = 0;
  ERR_HELP_PASSED = 1;
  ERR_INVALID_OPTIONS = 2;
  ERR_PATCH_INSTALL_FAILED = 3;
  ERR_PATCH_UNINSTALL_FAILED = 5;
  ERR_NO_OPERATION_PASSED = 4;
  ERR_PATCH_ALREADY_INSTALLED = 6;
  ERR_PATCH_NOT_UNINSTALLABLE = 7;

type
  TCodeBlocksPatcherCommandOperation = (coUnknown, coInstall, coUninstall,
    coPrintCodeBlocksUsers);
  TCodeBlocksPatcherOperationSet = set of TCodeBlocksPatcherCommandOperation;

  { TCodeBlocksPatcherApplication }
  TCodeBlocksPatcherApplication = class(TCustomApplication)
  private
    fVerbose: Boolean;
    fProgramExitCode: Integer;
    fErrorTerminate: Boolean;
    fCodeBlocksPatcher: TCodeBlocksPatcher;
    fDisplayBanner: Boolean;
    fCommandOperationText: string;
    fCommandOperation: TCodeBlocksPatcherCommandOperation;
    function ConvertCommandOperationToPatcherOperation:
      TCodeBlocksPatcherOperation;
    procedure WriteHeader;
    procedure PrintParameters;
    procedure OnPatcherProcessBegin(Sender: TObject);
    procedure OnPatcherProcessTaskBegin(Sender: TObject; const Message: string);
    procedure OnPatcherProcessTaskEnd(Sender: TObject; const Message: string;
      const Success: Boolean);
    procedure OnPatcherProcessEnd(Sender: TObject; const Success: Boolean);
    procedure WriteBool(const B: Boolean);
    function ParseOperation(
      const OperationValue: string): TCodeBlocksPatcherCommandOperation;
  protected
    function DoInitialize: Boolean;
    procedure DoRun; override;
    procedure DoErrorTerminate(AExitCode: Integer; const Message: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    property DisplayBanner: Boolean read fDisplayBanner;
    property Patcher: TCodeBlocksPatcher
      read fCodeBlocksPatcher;
    property CommandOperation: TCodeBlocksPatcherCommandOperation
      read fCommandOperation;
    property CommandOperationText: string
      read fCommandOperationText;
  end;

var
  ProgramName: string;

{ TCodeBlocksPatcherApplication }

function TCodeBlocksPatcherApplication.ConvertCommandOperationToPatcherOperation:
  TCodeBlocksPatcherOperation;
begin
  Result := pmUndefined;
  case CommandOperation of
    coInstall:
      Result := pmInstall;
    coUninstall:
      Result := pmUninstall;
  end;
end;

procedure TCodeBlocksPatcherApplication.WriteHeader;
var
  ReleaseType: string;

begin
  ReleaseType := EmptyStr;
{$IFDEF DEBUG}
  ReleaseType := '-DEBUG';
{$ENDIF}

  WriteLn(Title, ', Ver. ', GetFileVersion, ReleaseType);
  WriteLn('Created by ', GetCompanyName, '.', sLineBreak);
end;

procedure TCodeBlocksPatcherApplication.PrintParameters;

  function ToScreen(FileList: TFileList): string;
  var
    i: Integer;

  begin
    Result := EmptyStr;
    for i := 0 to FileList.Count - 1 do
      Result := Result + sLineBreak + '    * ' + FileList[i];
  end;

  procedure PrintParam(const Message, Value: string);
  begin
    WriteLn('  ', Message, ': ', Value);
  end;

begin
  if fVerbose then
  begin
    WriteLn('Parameters:');
    with Patcher do
    begin
      PrintParam('Code::Blocks Installation Directory', CodeBlocksInstallationDirectory);
      PrintParam('Code::Blocks Backup Directory', CodeBlocksBackupDirectory);
      PrintParam('Code::Blocks Configuration File Names', ToScreen(CodeBlocksConfigurationFileNames));
      PrintParam('DreamSDK Home Directory', HomeDirectory);
      PrintParam('DreamSDK Configuration File Name', ConfigurationFileName);
    end;
    WriteLn;
  end;
end;

procedure TCodeBlocksPatcherApplication.OnPatcherProcessBegin(Sender: TObject);
begin
  // Nothing
end;

procedure TCodeBlocksPatcherApplication.OnPatcherProcessTaskBegin(Sender: TObject;
  const Message: string);
begin
  Write(Message, '... ');
end;

procedure TCodeBlocksPatcherApplication.OnPatcherProcessTaskEnd(Sender: TObject;
  const Message: string; const Success: Boolean);
begin
  WriteBool(Success);
end;

procedure TCodeBlocksPatcherApplication.OnPatcherProcessEnd(
  Sender: TObject; const Success: Boolean);
var
  FinalExitCode: Integer;
  FinalStr: string;

begin
  WriteLn;

  FinalStr := EmptyStr;
  FinalExitCode := ERR_SUCCESS;

  case CommandOperation of
    coInstall:
      begin
        FinalExitCode := ERR_PATCH_INSTALL_FAILED;
        FinalStr := 'patched';
      end;
    coUninstall:
      begin
        FinalExitCode := ERR_PATCH_UNINSTALL_FAILED;
        FinalStr := 'restored';
      end;
  end;

  if Success then
    WriteLn(Format('Code::Blocks is now %s!', [FinalStr]))
  else
  begin
    WriteLn(Format('Code::Blocks was NOT successfully %s!', [FinalStr]));
    fProgramExitCode := FinalExitCode;
  end;
end;

procedure TCodeBlocksPatcherApplication.WriteBool(const B: Boolean);
begin
  if B then
    WriteLn('OK!')
  else
    WriteLn('Failed!');
end;

function TCodeBlocksPatcherApplication.ParseOperation(
  const OperationValue: string): TCodeBlocksPatcherCommandOperation;
var
  Buffer: string;

begin
  Result := coUnknown;

  Buffer := UpperCase(Trim(OperationValue));
  if (Buffer = 'INSTALL') or (Buffer = 'I') then
    Result := coInstall
  else if (Buffer = 'UNINSTALL') or (Buffer = 'U') then
    Result := coUninstall
  else if (Buffer = 'PRINT-CODEBLOCKS-USERS') or (Buffer = 'C') then
    Result := coPrintCodeBlocksUsers;

  if Result <> coUnknown then
    fCommandOperationText := LowerCase(OperationValue);
end;

procedure TCodeBlocksPatcherApplication.DoRun;
var
  ErrorMsg, OperationSwitch: string;

  procedure DoPrintCodeBlocksConfigurationFiles;
  var
    i: Integer;

  begin
    WriteLn('Code::Blocks available user profiles:');
    for i := 0 to Patcher.CodeBlocksAvailableUsers.Count - 1 do
      WriteLn('  ', Patcher.CodeBlocksAvailableUsers[i]);
  end;

begin
  // Display application banner
  fDisplayBanner := not HasOption('n', 'no-logo');
  if DisplayBanner then
    WriteHeader;

  // Quick check parameters
  ErrorMsg := CheckOptions(
    'hni:c:m:o:b:se',
    'help no-logo install-dir: config-files: home-dir: operation: backup-dir: show-splash verbose'
  );
  if ErrorMsg <> EmptyStr then
  begin
    DoErrorTerminate(ERR_INVALID_OPTIONS, ErrorMsg);
    Exit;
  end;

  // Help passed
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate(ERR_HELP_PASSED);
    Exit;
  end;

  // Handle verbose
  fVerbose := HasOption('e', 'verbose');

  // Check if an operation is specified (mandatory)
  if not HasOption('o', 'operation') then
  begin
    WriteHelp;
    Terminate(ERR_NO_OPERATION_PASSED);
    Exit;
  end;

  // Parse requested operation
  OperationSwitch := GetOptionValue('o', 'operation');
  fCommandOperation := ParseOperation(OperationSwitch);
  if fCommandOperation = coUnknown then
  begin
    DoErrorTerminate(ERR_NO_OPERATION_PASSED, Format('Invalid operation: %s', [OperationSwitch]));
    Exit;
  end;
  Patcher.Operation := ConvertCommandOperationToPatcherOperation;

  // initialize the patcher engine and parse the parameters
  fProgramExitCode := ERR_SUCCESS;
  if not DoInitialize then
    Exit; // Errors already processed in the DoInitialize func

  // Display parameters
  PrintParameters;

  // Check if the patch is already installed
  if (Patcher.Installed) and (CommandOperation = coInstall) then
  begin
    DoErrorTerminate(ERR_PATCH_ALREADY_INSTALLED,
      'Patch is already installed.');
    Exit;
  end;

  // Check if the patch is already uninstalled
  if (not Patcher.Installed) and (CommandOperation = coUninstall) then
  begin
    DoErrorTerminate(ERR_PATCH_NOT_UNINSTALLABLE,
      'Patch is not installed, nothing to uninstall.');
    Exit;
  end;

  // Check input parameters
  if not Patcher.Ready then
  begin
    DoErrorTerminate(ERR_INVALID_OPTIONS, 'Missing or invalid parameters.');
    Exit;
  end;

  // Handle Splash
  Patcher.VisibleSplash := HasOption('s', 'show-splash');

  // Events
  Patcher.OnProcessBegin := @OnPatcherProcessBegin;
  Patcher.OnProcessTaskBegin := @OnPatcherProcessTaskBegin;
  Patcher.OnProcessTaskEnd := @OnPatcherProcessTaskEnd;
  Patcher.OnProcessEnd := @OnPatcherProcessEnd;

  // Execute it!
  if CommandOperation in ([coInstall, coUninstall]) then
    Patcher.Execute
  else if (CommandOperation = coPrintCodeBlocksUsers) then
    DoPrintCodeBlocksConfigurationFiles;

  // stop program loop
  Terminate(fProgramExitCode);
end;

procedure TCodeBlocksPatcherApplication.DoErrorTerminate(AExitCode: Integer;
  const Message: string);
var
  RowHeader: string;

begin
  fErrorTerminate := True;

  RowHeader := EmptyStr;
  if not DisplayBanner then
    RowHeader := ProgramName + ': ';

  WriteLn(RowHeader, 'Error: ', Message);
  Terminate(AExitCode);
end;

function TCodeBlocksPatcherApplication.DoInitialize: Boolean;
var
  TempParam: string;

  function GetParam(Modes: TCodeBlocksPatcherOperationSet;
    ShortOption: Char; LongOption: string;
    var OutputValue: string): Boolean;
  var
    Value: string;

  begin
    OutputValue := EmptyStr;

    Value := GetOptionValue(ShortOption, LongOption);
    Result := (Value <> EmptyStr);

    if Result then
      if CommandOperation in Modes then
        OutputValue := Value
      else
        DoErrorTerminate(ERR_INVALID_OPTIONS,
          Format('Supplied parameter "-%s, --%s" is invalid in "%s" mode', [
            ShortOption, LongOption, CommandOperationText]));
  end;

begin
  Result := True;
  TempParam := EmptyStr;
  with Patcher do
  begin
    // Code::Blocks Installation Directory
    if GetParam([coInstall], 'i', 'install-dir', TempParam) then
      CodeBlocksInstallationDirectory := TempParam;

    // Code::Blocks Backup Directory
    if GetParam([coInstall], 'b', 'backup-dir', TempParam) then
      CodeBlocksBackupDirectory := TempParam;

    // Code::Blocks Configuration File Names
    if GetParam([coInstall], 'c', 'config-files', TempParam) then
      CodeBlocksConfigurationFileNames.SetItems(TempParam, FILENAMES_SEPARATOR);

    // DreamSDK Home Directory
    if GetParam([coInstall, coUninstall], 'm', 'home-dir', TempParam) then
      HomeDirectory := TempParam;

    Result := not fErrorTerminate;
  end;
end;

constructor TCodeBlocksPatcherApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StopOnException := True;
  fCodeBlocksPatcher := TCodeBlocksPatcher.Create;
  ProgramName := GetProgramName;
  fErrorTerminate := False;
end;

destructor TCodeBlocksPatcherApplication.Destroy;
begin
  fCodeBlocksPatcher.Free;
  inherited Destroy;
end;

procedure TCodeBlocksPatcherApplication.WriteHelp;
var
  DefaultBackupDirectory: TFileName;

begin
  DefaultBackupDirectory := Format(DEFAULT_CODEBLOCKS_BACKUP_DIR, [
    '%' + GetBaseEnvironmentVariableName + '%']);

  WriteLn('Usage:', sLineBreak,
    '  ', ProgramName, ' --operation=<install, uninstall> [options]', sLineBreak,
    sLineBreak,
    'Description:', sLineBreak,
    '  --operation, -o    : The operation the patch need to perform. It can be', sLineBreak,
    '                       ''install'', ''uninstall'' or ''print-codeblocks-users'' to', sLineBreak,
    '                       list the users that have a valid Code::Blocks profile.', sLineBreak,
    '                       To give eligibility to an user to get the patch, just', sLineBreak,
    '                       run Code::Blocks one time with that user to create all', sLineBreak,
    '                       the required files.', sLineBreak,
    sLineBreak,
    'Options:', sLineBreak,
    '  --install-dir, -i  : The Code::Blocks installation directory. Default is ', sLineBreak,
    '                       ''%ProgramFiles%\CodeBlocks\'' on 32-bits systems or', sLineBreak,
    '                       ''%ProgramFiles(x86)%\CodeBlocks\'' on 64-bits systems.', sLineBreak,
    sLineBreak,
    '  --backup-dir, -b   : The directory where the original, unpatched Code::Blocks', sLineBreak,
    '                       files will be stored. Default backup path is:', sLineBreak,
    '                       ''', DefaultBackupDirectory, '''.', sLineBreak,
    sLineBreak,
    '  --config-files, -c : The Code::Blocks configuration files to patch.', sLineBreak,
    '                       It can be a single file or multiple files separated by', sLineBreak,
    '                       the '';'' character. If not specified, it will try to', sLineBreak,
    '                       determine itself the patchable files (typically', sLineBreak,
    '                       ''%AppData%\CodeBlocks\default.conf'' for each users', sLineBreak,
    '                       on this system). You may use the --operation', sLineBreak,
    '                       ''print-codeblocks-users'' to list these patchable files.', sLineBreak,
    sLineBreak,
    '  --home-dir, -m     : The DreamSDK Home directory (typically ', sLineBreak,
    '                       ''C:\DreamSDK\''). If not specified, the value is read', sLineBreak,
    '                       from the ''', GetBaseEnvironmentVariableName, ''' environment variable.', sLineBreak,
    sLineBreak,
    '  --show-splash, -s  : If enabled, display a progress screen while working.', sLineBreak,
    sLineBreak,
    '  --verbose, -e      : Print detailed information while working.', sLineBreak,
    sLineBreak,
    '  --no-logo, -n      : Don''t print application banner.', sLineBreak,
    sLineBreak,
    '  --help, -h         : Print this help.', sLineBreak,
    sLineBreak,
    'Note:', sLineBreak,
    '  This patcher was made for the Code::Blocks 17.12 stable release only.', sLineBreak,
    '  It is not affiliated with the Code::Blocks Team in any way.');
end;

var
  Application: TCodeBlocksPatcherApplication;

{$R *.res}

begin
  Application := TCodeBlocksPatcherApplication.Create(nil);
  try
    Application.Title := 'Code::Blocks Patcher';
    Application.Run;
  finally
    Application.Free;
  end;
end.

