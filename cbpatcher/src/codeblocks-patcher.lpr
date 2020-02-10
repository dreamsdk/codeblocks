program CodeBlocksPatcher;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  UniqueInstanceRaw,
  FSTools,
  RefBase,
  Version,
  SysTools,
  CBPatch,
  XmlTools,
  Settings;

const
  ERR_SUCCESS = 0;
  ERR_HELP_PASSED = 1;
  ERR_INVALID_OPTIONS = 2;
  ERR_PATCH_INSTALL_FAILED = 3;
  ERR_PATCH_UNINSTALL_FAILED = 5;
  ERR_NO_OPERATION_PASSED = 4;
  ERR_PATCH_ALREADY_INSTALLED = 6;
  ERR_PATCH_NOT_UNINSTALLABLE = 7;
  ERR_PATCH_REINSTALL_FAILED = 8;
  ERR_PATCH_NOT_REINSTALLABLE = 9;
  ERR_PATCH_RUNNING = 10;
  ERR_CODEBLOCKS_RUNNING = 11;
  ERR_INVALID_CONFIGURATION_FILE_NAMES = 12;
  ERR_REGISTRY_NOT_REFRESHED = 13;

type
  TCodeBlocksPatcherCommandOperation = (coUnknown, coInstall, coUninstall,
    coPrintCodeBlocksUsers, coPrintStatus, coReinstall, coInternalRefresh);
  TCodeBlocksPatcherOperationSet = set of TCodeBlocksPatcherCommandOperation;

  { TCodeBlocksPatcherApplication }
  TCodeBlocksPatcherApplication = class(TCustomApplication)
  private
    fErrorMessages: TStringList;
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
    procedure OnPatcherError(Sender: TObject; const ErrorMessage: string);
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
    coReinstall:
      Result := pmReinstall;
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

  function FileListToScreen(FileList: TFileList): string;
  var
    i: Integer;

  begin
    Result := EmptyStr;
    for i := 0 to FileList.Count - 1 do
      Result := Result + sLineBreak + '    * "' + FileList[i] + '"';
  end;

  procedure PrintParam(const Message, Value: string; const UseQuote: Boolean = True);
  var
    QuoteChar: string;

  begin
    QuoteChar := EmptyStr;
    if UseQuote then
      QuoteChar := '"';
    WriteLn('  ', Message, ': ', QuoteChar, Value, QuoteChar);
  end;

  function UsersFromFileListToScreen(InstalledUsers: TStringList): string;
  var
    i: Integer;

  begin
    Result := EmptyStr;
    for i := 0 to InstalledUsers.Count - 1 do
      Result := Result + sLineBreak + '    * ' + InstalledUsers[i];
  end;

begin
  if fVerbose then
  begin
    WriteLn('Parameters:');
    with Patcher.Settings do
    begin
      PrintParam('Code::Blocks Installation Directory', InstallationDirectory);
      PrintParam('Code::Blocks Configuration Files',
        FileListToScreen(ConfigurationFileNames), False);
      PrintParam('DreamSDK Home Directory', IncludeTrailingPathDelimiter(HomeDirectory));
      PrintParam('Code::Blocks Backup Directory', BackupDirectory);
      PrintParam('DreamSDK Registry File', RegistryFileName);
      if Installed and (InstalledUsers.Count > 0) then
        PrintParam('Code::Blocks Users List',
          UsersFromFileListToScreen(InstalledUsers), False);
    end;
    WriteLn;
  end;
end;

procedure TCodeBlocksPatcherApplication.OnPatcherError(Sender: TObject;
  const ErrorMessage: string);
begin
  fErrorMessages.Add(ErrorMessage);
end;

procedure TCodeBlocksPatcherApplication.OnPatcherProcessBegin(Sender: TObject);
begin
  // Nothing
end;

procedure TCodeBlocksPatcherApplication.OnPatcherProcessTaskBegin(Sender: TObject;
  const Message: string);
begin
  fErrorMessages.Clear;
  Write(Message, '... ');
end;

procedure TCodeBlocksPatcherApplication.OnPatcherProcessTaskEnd(Sender: TObject;
  const Message: string; const Success: Boolean);
var
  i: Integer;
  Line: string;

begin
  WriteBool(Success);
  if fErrorMessages.Count > 0 then
    for i := 0 to fErrorMessages.Count - 1 do
    begin
      Line := Trim(fErrorMessages[i]);
      if not IsEmpty(Line) then
        WriteLn('  * ', Line);
    end;
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
    coReinstall:
      begin
        FinalExitCode := ERR_PATCH_REINSTALL_FAILED;
        FinalStr := 'reinstalled';
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
    Result := coPrintCodeBlocksUsers
  else if (Buffer = 'PRINT-STATUS') or (Buffer = 'STATUS') or (Buffer = 'S') then
    Result := coPrintStatus
  else if (Buffer = 'REINSTALL') or (Buffer = 'R') then
    Result := coReinstall
  else if (Buffer = 'INTERNAL-REFRESH') then
    Result := coInternalRefresh; // for DreamSDK Manager only

  if Result <> coUnknown then
    fCommandOperationText := LowerCase(OperationValue);
end;

procedure TCodeBlocksPatcherApplication.DoRun;
const
  CODEBLOCKS_FILE_NAME = 'codeblocks.exe';

var
  ErrorMsg, OperationSwitch: string;

  procedure DoPrintCodeBlocksConfigurationFiles;
  var
    i: Integer;

  begin
    WriteLn('Code::Blocks Available User Profiles:');
    for i := 0 to Patcher.Settings.AvailableUsers.Count - 1 do
      WriteLn('  * ', Patcher.Settings.AvailableUsers[i]);
  end;

  procedure DoPrintStatus;
  begin
    if Patcher.Settings.Installed then
      WriteLn('Patch is currently installed')
    else
      WriteLn('Patch is NOT currently installed');
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

  // Check if the patch is already running
  if InstanceRunning then
  begin
    DoErrorTerminate(ERR_PATCH_RUNNING, 'Patcher is already running, can''t continue.');
    Exit;
  end;

  // Check if Code::Blocks is already running
  if IsProcessRunning(CODEBLOCKS_FILE_NAME) then
  begin
    DoErrorTerminate(ERR_CODEBLOCKS_RUNNING, 'Code::Blocks is running, can''t continue.');
    Exit;
  end;

  // Check if the patch is already installed
  if (Patcher.Settings.Installed) and (CommandOperation = coInstall) then
  begin
    DoErrorTerminate(ERR_PATCH_ALREADY_INSTALLED,
      'Patch is already installed.');
    Exit;
  end;

  // Check if the patch is already uninstalled
  if (not Patcher.Settings.Installed) and (CommandOperation = coUninstall) then
  begin
    DoErrorTerminate(ERR_PATCH_NOT_UNINSTALLABLE,
      'Patch is not installed, nothing to uninstall.');
    Exit;
  end;

  // Check if the patch cannot be reinstalled
  if (not Patcher.Settings.Installed) and (CommandOperation = coReinstall) then
  begin
    DoErrorTerminate(ERR_PATCH_NOT_REINSTALLABLE,
      'Patch is not installed, nothing to reinstall.');
    Exit;
  end;

  // Check input parameters
  if not Patcher.Ready then
  begin
    DoErrorTerminate(ERR_INVALID_OPTIONS, 'Missing or invalid parameters.');
    Exit;
  end;

  // Check C::B configuration files when installing
  if (CommandOperation = coInstall) and (not Patcher.ConfigurationFilesNamesReady) then
  begin
    DoErrorTerminate(ERR_INVALID_CONFIGURATION_FILE_NAMES,
      'Some supplied configuration file names were not found.');
    Exit;
  end;

  // Handle Splash
  Patcher.VisibleSplash := HasOption('s', 'show-splash');

  // Events
  Patcher.OnError := @OnPatcherError;
  Patcher.OnProcessBegin := @OnPatcherProcessBegin;
  Patcher.OnProcessTaskBegin := @OnPatcherProcessTaskBegin;
  Patcher.OnProcessTaskEnd := @OnPatcherProcessTaskEnd;
  Patcher.OnProcessEnd := @OnPatcherProcessEnd;

  // Execute it!
  if CommandOperation in ([coInstall, coUninstall, coReinstall]) then
    Patcher.Execute
  else if (CommandOperation = coPrintCodeBlocksUsers) then
    DoPrintCodeBlocksConfigurationFiles
  else if (CommandOperation = coPrintStatus) then
    DoPrintStatus
  else if (CommandOperation = coInternalRefresh) then
  begin
    if Patcher.Settings.Refresh then
      WriteLn('Registry refreshed!')
    else
    begin
      DoErrorTerminate(ERR_REGISTRY_NOT_REFRESHED, 'Registry NOT refreshed!');
      Exit;
    end;
  end;

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
  with Patcher.Settings do
  begin
    // Code::Blocks Installation Directory
    if GetParam([coInstall], 'i', 'install-dir', TempParam) then
      InstallationDirectory := TempParam;

    // Code::Blocks Backup Directory
    if GetParam([coInstall], 'b', 'backup-dir', TempParam) then
      BackupDirectory := TempParam;

    // Code::Blocks Configuration File Names
    if GetParam([coInstall], 'c', 'config-files', TempParam) then
      ConfigurationFileNames.SetItems(TempParam, ArraySeparator);

    // DreamSDK Home Directory
    if GetParam([coInstall, coUninstall, coReinstall, coPrintStatus, coInternalRefresh],
      'm', 'home-dir', TempParam) then
        HomeDirectory := TempParam;

    Result := not fErrorTerminate;
  end;
end;

constructor TCodeBlocksPatcherApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StopOnException := True;
  fCodeBlocksPatcher := TCodeBlocksPatcher.Create;
  fErrorMessages := TStringList.Create;
  ProgramName := GetProgramName;
  fErrorTerminate := False;
end;

destructor TCodeBlocksPatcherApplication.Destroy;
begin
  fErrorMessages.Free;
  fCodeBlocksPatcher.Free;
  inherited Destroy;
end;

procedure TCodeBlocksPatcherApplication.WriteHelp;
var
  DefaultBackupDirectory: TFileName;

begin
  DefaultBackupDirectory := GetDefaultCodeBlocksBackupDirectory;

  WriteLn('Usage:', sLineBreak,
    '  ', ProgramName, ' --operation=<install, uninstall, ...> [options]', sLineBreak,
    sLineBreak,
    'Description:', sLineBreak,
    '  --operation, -o    : The operation the patch need to perform. It can be:', sLineBreak,
    sLineBreak,
    '    * install, i                : Install the Code::Blocks patch for DreamSDK.', sLineBreak,
    '    * uninstall, u              : Uninstall the patch. ', sLineBreak,
    '    * reinstall, r              : Reinstall the patch (basically, it''s ', sLineBreak,
    '                                  ''uninstall'' then ''install'' commands chained).', sLineBreak,
    '    * print-codeblocks-users, c : List the users that have a valid Code::Blocks', sLineBreak,
    '                                  profile. To give eligibility to an user to', sLineBreak,
    '                                  get the patch, just run Code::Blocks one', sLineBreak,
    '                                  time with that user to create all the', sLineBreak,
    '                                  required files, then run the patch again.', sLineBreak,
    '    * print-status, s           : Get the installation status of the patch.', sLineBreak,
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
    '                       the ''', ArraySeparator, ''' character. If not specified, it will try to', sLineBreak,
    '                       determine itself the patchable files (typically', sLineBreak,
    '                       ''%AppData%\CodeBlocks\default.conf'' for each user', sLineBreak,
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
    'Examples:', sLineBreak,
    '  ', ProgramName, ' --operation=install', sLineBreak,
    '    Install the Code::Blocks patch for DreamSDK using the defaults.', sLineBreak,
    sLineBreak,
    '  ', ProgramName, ' --operation=install --show-splash', sLineBreak,
    '    Do the same thing as above but display the splash screen.', sLineBreak,
    sLineBreak,
    '  ', ProgramName, ' --operation=install --install-dir="E:\CodeBlocks\"', sLineBreak,
    '    Install the patch by changing the Code::Blocks installation directory.', sLineBreak,
    sLineBreak,
    '  ', ProgramName, ' --operation=reinstall', sLineBreak,
    '    Reinstall the Code::Blocks patch, by using the previous parameters.', sLineBreak,
    sLineBreak,
    '  ', ProgramName, ' --operation=uninstall', sLineBreak,
    '    Remove completely the Code::Blocks patch for DreamSDK.', sLineBreak,
    sLineBreak,
    'Note:', sLineBreak,
    '  This patcher was made for the Code::Blocks 17.12 stable release only.', sLineBreak,
    '  It is NOT affiliated with the Code::Blocks Team in any way.', sLineBreak,
    sLineBreak,
    'Hint: ', sLineBreak,
    '  The help is too big to fit in your screen? Don''t hesitate to use this tip:', sLineBreak,
    '    ', ProgramName, ' --help | more'
  );
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

