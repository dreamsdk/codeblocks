unit CBPatch;

{$mode objfpc}{$H+}

{$IFNDEF LITE_VERSION}
{$R embedded.rc}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  DOM,
  XMLRead,
  XMLWrite,
  FSTools;

const
  FILENAMES_SEPARATOR = ';';

  DEFAULT_CODEBLOCKS_DIR_64 = '%ProgramFiles(x86)%\CodeBlocks';
  DEFAULT_CODEBLOCKS_DIR_32 = '%ProgramFiles%\CodeBlocks';
  DEFAULT_CODEBLOCKS_CONFIGURATION_FILE = '%sCodeBlocks\default.conf';
  DEFAULT_CODEBLOCKS_BACKUP_DIR = '%s\support\ide\codeblocks\';

type
  ECodeBlocksPatcher = class(Exception);
  ECodeBlocksPatcherLiteVersion = class(ECodeBlocksPatcher);

  TTaskProc = function: Boolean of object;

  TCodeBlocksPatcherOperation = (pmUndefined, pmInstall, pmUninstall,
    pmReinstall);

  TTerminateEvent = procedure(Sender: TObject;
    const Success: Boolean) of object;
  TTaskBeginEvent = procedure(Sender: TObject;
    const Message: string) of object;
  TTaskEndEvent = procedure(Sender: TObject; const Message: string;
    const Success: Boolean) of object;

  TCodeBlocksBackupRestoreOperation = (cbBackup, cbRestore);
  TCodeBlocksSplashOperation = (soInstall, soUninstall, soReinstall, soClose);
  TSourceKind = (skDebugger, skGlobalVariables, skTools);

  { TCodeBlocksPatcher }
  TCodeBlocksPatcher = class(TObject)
  private
    fDesignFileNameDebugger: TFileName;
    fDesignFileNameGlobalVariables: TFileName;
    fDesignFileNameTools: TFileName;
    fFragmentFileNameDebugger: TFileName;
    fFragmentFileNameGlobalVariables: TFileName;
    fFragmentFileNameTools: TFileName;
    fFragmentFileNameToolsSeparator: TFileName;
    fNodeValueDebugger: string;
    fNodeValueGlobalVariables: string;
    fCodeBlocksAvailableUsers: TStringList;
    fCodeBlocksInstalledUsers: TStringList;
    fExecuteResult: Boolean;
    fProcessBegin: TNotifyEvent;
    fCodeBlocksBackupRestoreFileName: TFileName;
    fCodeBlocksSplashFileName: TFileName;
    fCodeBlocksPatchFileName: TFileName;
    fCodeBlocksBackupDirectory: TFileName;
    fCodeBlocksConfigurationFileNames: TFileList;
    fCodeBlocksInstallationDirectory: TFileName;
    fHomeDirectoryForDreamSDK: TFileName;
    fProcessEnd: TTerminateEvent;
    fOperation: TCodeBlocksPatcherOperation;
    fSourceDirectory: TFileName;
    fProcessTaskBegin: TTaskBeginEvent;
    fProcessTaskEnd: TTaskEndEvent;
    fVisibleSplash: Boolean;
    procedure ExtractEmbeddedFiles;
    procedure FixTools(const CodeBlocksConfigurationFileName: TFileName);
    procedure FixDebugger(const CodeBlocksConfigurationFileName: TFileName);
    function GetCodeBlocksAvailableUsers: TStringList;
    function GetCodeBlocksInstalledUsers: TStringList;
    function GetConfigurationFileName: TFileName;
    function GetRegisteredString(const Section, Key: string): string;
    function GetGlobalVariablesActiveSet(XMLDocument: TXMLDocument): string;
    function GetReady: Boolean;
    function GetFriendlyUserName(const UserName: string): string;
    procedure InjectDebugger(SourceXML, TargetXML: TXMLDocument);
    procedure InjectGlobalVariables(SourceXML, TargetXML: TXMLDocument);
    procedure InjectTools(SourceXML, TargetXML: TXMLDocument);
    procedure RemoveDebugger(XML: TXMLDocument);
    procedure RemoveGlobalVariables(XML: TXMLDocument);
    procedure RemoveTools(XML: TXMLDocument);
    function IsCodeBlocksPatchInstalled: Boolean;
    procedure SetCodeBlocksBackupDirectory(AValue: TFileName);
    procedure SetCodeBlocksInstallationDirectory(AValue: TFileName);
    procedure SetHomeDirectory(AValue: TFileName);
    procedure SetOperation(AValue: TCodeBlocksPatcherOperation);
    procedure FixSection(const CodeBlocksConfigurationFileName: TFileName;
      const SectionName, ItemName, ItemFormat: string; const StartIndex: Integer);
    procedure HandleBaseDirectory(
      const CodeBlocksConfigurationFileName: TFileName);
    function Merge(const CodeBlocksConfigurationFileName: TFileName;
      SourceKind: TSourceKind): Boolean;
    function Cleanup(const CodeBlocksConfigurationFileName: TFileName;
      SourceKind: TSourceKind): Boolean;
    function UpdateConfiguration(
      const Operation: TCodeBlocksPatcherOperation): Boolean;

    procedure ExecuteTask(const StepMessage: string; TaskFunction: TTaskProc);

    function InstallCodeBlocksPatch: Boolean;
    function PatchCodeBlocksConfiguration: Boolean;
    function UpdateConfigurationInstall: Boolean;

    function UninstallCodeBlocksPatch: Boolean;
    function CleanCodeBlocksConfiguration: Boolean;
    function UpdateConfigurationUninstall: Boolean;

    function PatcherOperationToSplashOperation: TCodeBlocksSplashOperation;
  protected
    function BackupFiles: Boolean;
    function RunCodeBlocksBackupRestore(
      const Operation: TCodeBlocksBackupRestoreOperation): Boolean;
    function GetSourcePackageDirectory: TFileName;
    function SourceKindToFragmentFileName(const SourceKind: TSourceKind): TFileName;

    procedure DoPatchInstall;
    procedure DoPatchUninstall;

    function RunCodeBlocksSplash(
      const Operation: TCodeBlocksSplashOperation): Boolean;

    {$IFDEF DEBUG}procedure IterateNodes(Node: TDOMNode);{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    function Execute: Boolean;

    // Code::Blocks Configuration Files (default.conf)
    property CodeBlocksConfigurationFileNames: TFileList
      read fCodeBlocksConfigurationFileNames;

    // Code::Blocks Available Users
    property CodeBlocksAvailableUsers: TStringList
      read GetCodeBlocksAvailableUsers;

    // Code::Blocks Installed Users (if any)
    property CodeBlocksInstalledUsers: TStringList
      read GetCodeBlocksInstalledUsers;

    // Code::Blocks Installation Directory
    property CodeBlocksInstallationDirectory: TFileName
      read fCodeBlocksInstallationDirectory
      write SetCodeBlocksInstallationDirectory;

    // Backup Directory for Code::Blocks files (in DreamSDK tree)
    property CodeBlocksBackupDirectory: TFileName
      read fCodeBlocksBackupDirectory
      write SetCodeBlocksBackupDirectory;

    // Is the Patcher ready to execute?
    property Ready: Boolean read GetReady;

    // Mode: Install or Uninstall
    property Operation: TCodeBlocksPatcherOperation
      read fOperation
      write SetOperation;

    property VisibleSplash: Boolean
      read fVisibleSplash
      write fVisibleSplash;

    property Installed: Boolean
      read IsCodeBlocksPatchInstalled;

    // DreamSDK Home
    property HomeDirectory: TFileName
      read fHomeDirectoryForDreamSDK
      write SetHomeDirectory;

    // DreamSDK Configuration File
    property ConfigurationFileName: TFileName
      read GetConfigurationFileName;

    // Events
    property OnProcessBegin: TNotifyEvent
      read fProcessBegin
      write fProcessBegin;
    property OnProcessTaskBegin: TTaskBeginEvent
      read fProcessTaskBegin
      write fProcessTaskBegin;
    property OnProcessTaskEnd: TTaskEndEvent
      read fProcessTaskEnd
      write fProcessTaskEnd;
    property OnProcessEnd: TTerminateEvent
      read fProcessEnd
      write fProcessEnd;
  end;

implementation

uses
  IniFiles,
  SysTools,
  RefBase,
  Version,
  RunTools,
  XmlTools;

const
  DREAMSDK_HOME_VARIABLE = '{app}';

  INI_SECTION_IDE = 'IDE';
  INI_SECTION_SETUP = 'Setup';

  INI_KEY_IDE_KIND = 'Kind';
  INI_KEY_EXPORT_LIBRARY_INFORMATION_ENABLED = 'ExportLibraryInformation';
  INI_KEY_EXPORT_LIBRARY_INFORMATION_PATH = 'ExportLibraryInformationPath';
  INI_KEY_CONFIGURATION_FILENAMES = 'ConfigurationFileNames';
  INI_KEY_INSTALLATION_PATH = 'InstallationPath';
  INI_KEY_BACKUP_PATH = 'BackupPath';

  SEPARATOR_NAME = '---separator---';

constructor TCodeBlocksPatcher.Create;

  // Define this to debug this procedure
  // {$DEFINE DEBUG_INITIALIZE_DEFAULTS}
  procedure InitializeDefaults;
  var
    UsersAppData: TStringList;
    i: Integer;
    CodeBlocksConfigurationFileName: TFileName;

  begin
    // Code::Blocks Installation Directory
    CodeBlocksInstallationDirectory := DEFAULT_CODEBLOCKS_DIR_32;
    if IsWindows64 then
      CodeBlocksInstallationDirectory := DEFAULT_CODEBLOCKS_DIR_64;

    // Code::Blocks Configuration Files
    UsersAppData := TStringList.Create;
    try
      GetAppDataListFromUsers(UsersAppData);

      for i := 0 to UsersAppData.Count - 1 do
      begin
        CodeBlocksConfigurationFileName := Format(
          DEFAULT_CODEBLOCKS_CONFIGURATION_FILE,
          [ IncludeTrailingPathDelimiter(UsersAppData[i]) ]
        );
  {$IFDEF DEBUG}
  {$IFDEF DEBUG_INITIALIZE_DEFAULTS}
        Write(CodeBlocksConfigurationFileName, ' ... ');
  {$ENDIF}
  {$ENDIF}
        if FileExists(CodeBlocksConfigurationFileName) then
        begin
  {$IFDEF DEBUG}
  {$IFDEF DEBUG_INITIALIZE_DEFAULTS}
          WriteLn('exist!');
  {$ENDIF}
  {$ENDIF}
          CodeBlocksConfigurationFileNames.Add(CodeBlocksConfigurationFileName);
        end
  {$IFDEF DEBUG}
  {$IFDEF DEBUG_INITIALIZE_DEFAULTS}
        else
          WriteLn('doesn''t exist!')
  {$ENDIF}
  {$ENDIF};
      end;
    finally
      UsersAppData.Free;
    end;

    // Home Directory
    HomeDirectory := GetInstallationBaseDirectory;

    // Code::Blocks Backup Directory
    CodeBlocksBackupDirectory := Format(DEFAULT_CODEBLOCKS_BACKUP_DIR,
      [HomeDirectory]);
  end;

begin
  fCodeBlocksConfigurationFileNames := TFileList.Create;
  fCodeBlocksAvailableUsers := TStringList.Create;
  fCodeBlocksInstalledUsers := TStringList.Create;
  fOperation := pmUndefined;
  InitializeDefaults;
  ExtractEmbeddedFiles;
end;

destructor TCodeBlocksPatcher.Destroy;
begin
  fCodeBlocksInstalledUsers.Free;
  fCodeBlocksAvailableUsers.Free;
  fCodeBlocksConfigurationFileNames.Free;
{$IFNDEF LITE_VERSION}
  Sleep(500);
  KillProcessByName(fCodeBlocksSplashFileName);
{$ENDIF}
  inherited Destroy;
end;

function TCodeBlocksPatcher.Execute: Boolean;
begin
{$IFNDEF LITE_VERSION}
  fExecuteResult := False;

  if Ready then
  begin
    // Notify the process start
    if Assigned(fProcessBegin) then
      fProcessBegin(Self);

    // Show Splash (if needed)
    if fVisibleSplash then
      RunCodeBlocksSplash(PatcherOperationToSplashOperation);

    // Execute the operations
    fExecuteResult := True;
    case Operation of
      pmInstall:
        DoPatchInstall;
      pmUninstall:
        DoPatchUninstall;
      pmReinstall:
        begin
          DoPatchUninstall;
          Sleep(500);
          DoPatchInstall;
        end;
    end;

    // Notify the process end
    if Assigned(fProcessEnd) then
      fProcessEnd(Self, fExecuteResult);

    // Hide the Splash
    if fVisibleSplash then
      RunCodeBlocksSplash(soClose);
  end;

  Result := fExecuteResult;
{$ELSE}
  Result := False;
  raise ECodeBlocksPatcherLiteVersion.Create(
    'Unable to execute the Patcher engine in Lite version mode.');
{$ENDIF}
end;

procedure TCodeBlocksPatcher.FixSection(
  const CodeBlocksConfigurationFileName: TFileName; const SectionName, ItemName,
  ItemFormat: string; const StartIndex: Integer);
var
  Buffer: TStringList;
  i, SectionPositionIndex, ItemIndex: Integer;
  Done: Boolean;
  Line, TagName: string;

begin
  TagName := Format('%s%s', [ItemName, ItemFormat]);
  Buffer := TStringList.Create;
  try
    Buffer.LoadFromFile(CodeBlocksConfigurationFileName);
    SectionPositionIndex := StringListSubstringIndexOf(Buffer, '<' + SectionName + '>');
{$IFDEF DEBUG}
    WriteLn('SectionPositionIndex: ', SectionPositionIndex);
{$ENDIF}
    if SectionPositionIndex <> -1 then
    begin
      i := SectionPositionIndex + 1;
      ItemIndex := StartIndex;
      Done := False;

      while (i < Buffer.Count) and (not Done) do
      begin
        Line := Buffer[i];
        Done := IsInString('</' + SectionName + '>', Line);

        if not Done then
        begin
          if IsInString('<' + ItemName, Line) then
          begin
{$IFDEF DEBUG}
            WriteLn('  Processing (Start): ', Line);
{$ENDIF}
            Buffer[i] := Format('<' + TagName + '>', [ItemIndex]);
          end;

          if IsInString('</' + ItemName, Line) then
          begin
{$IFDEF DEBUG}
            WriteLn('  Processing (End): ', Line);
{$ENDIF}
            Buffer[i] := Format('</' + TagName + '>', [ItemIndex]);
            Inc(ItemIndex);
          end;
        end; // not Done

        Inc(i);
      end; // while

      Buffer.SaveToFile(CodeBlocksConfigurationFileName);
    end;
  finally
    Buffer.Free;
  end;
end;

procedure TCodeBlocksPatcher.HandleBaseDirectory(
  const CodeBlocksConfigurationFileName: TFileName);
var
  Buffer: TStringList;

begin
  Buffer := TStringList.Create;
  try
    Buffer.LoadFromFile(CodeBlocksConfigurationFileName);
    Buffer.Text := StringReplace(Buffer.Text, DREAMSDK_HOME_VARIABLE,
      HomeDirectory, [rfReplaceAll]);
    Buffer.SaveToFile(CodeBlocksConfigurationFileName);
  finally
    Buffer.Free;
  end;
end;

function TCodeBlocksPatcher.InstallCodeBlocksPatch: Boolean;
const
  COMPILER_FILE = 'share\CodeBlocks\compilers\compiler_dc-gcc.xml';
  OPTIONS_FILE = 'share\CodeBlocks\compilers\options_dc-gcc.xml';

begin
  Result := BackupFiles;
  Result := Result and UncompressLzmaFile(fCodeBlocksPatchFileName,
    CodeBlocksInstallationDirectory);
  Result := Result and PatchTextFile(CodeBlocksInstallationDirectory + COMPILER_FILE,
    DREAMSDK_HOME_VARIABLE, HomeDirectory);
  Result :=  Result and PatchTextFile(CodeBlocksInstallationDirectory + OPTIONS_FILE,
    DREAMSDK_HOME_VARIABLE, HomeDirectory);
end;

procedure TCodeBlocksPatcher.InjectDebugger(SourceXML,
  TargetXML: TXMLDocument);
var
  Node: TDOMNode;

  function SourceNode: TDOMNode;
  begin
    Result := SelectSingleNode('/debugger_common/sets/gdb_debugger/conf1', SourceXML);
  end;

  function IsSegaDreamcastDebuggerProfileExists: Boolean;
  begin
    Result := NodeExists(TargetXML, '/debugger_common/sets/gdb_debugger/*/NAME/str/text()',
      fNodeValueDebugger);
  end;

begin
  if not IsSegaDreamcastDebuggerProfileExists then
  begin
    Node := SourceNode;
    if Assigned(Node) then
      ImportNode(TargetXML, Node, '/debugger_common/sets/gdb_debugger', True);
  end;
end;

procedure TCodeBlocksPatcher.InjectGlobalVariables(SourceXML,
  TargetXML: TXMLDocument);
var
  Node: TDOMNode;
  ActiveSet: string;
  i: Integer;

begin
  ActiveSet := GetGlobalVariablesActiveSet(TargetXML);
{$IFDEF DEBUG}
  WriteLn('GlobalVariables Active: ', ActiveSet);
{$ENDIF}
  Node := SourceXML.DocumentElement.GetChildNodes[0].GetChildNodes[0].GetChildNodes[0]; // <default>
  for i := 0 to Node.ChildNodes.Count - 1 do
    ImportNode(TargetXML, Node.GetChildNodes[i], '/gcv/sets/' + ActiveSet); // <dreamsdk_home> at least
end;

procedure TCodeBlocksPatcher.InjectTools(SourceXML,
  TargetXML: TXMLDocument);
var
  Node, ToolNode: TDOMNode;
  i: Integer;

  function IsToolExists: Boolean;
  var
    ToolNameNode: TDOMNode;
    ToolName: string;

  begin
    Result := False;
    ToolNameNode := SelectSingleNode(Format('/tools/tool%0.2d/NAME/str/text()', [i]),
      SourceXML);
    if Assigned(ToolNameNode) then
    begin
      ToolName := AnsiString(ToolNameNode.NodeValue);
{$IFDEF DEBUG}
      WriteLn('Checking for ToolName: ', ToolName);
{$ENDIF}
      Result := NodeExists(TargetXML, '/tools/*/NAME/str/text()', ToolName);
    end;
  end;

begin
  Node := SourceXML.DocumentElement.GetChildNodes[0]; // <tools>
  for i := 0 to Node.ChildNodes.Count - 1 do
  begin
    ToolNode := Node.GetChildNodes[i];
    if not IsToolExists then
      ImportNode(TargetXML, ToolNode, '/tools', True);
  end;
end;

procedure TCodeBlocksPatcher.RemoveDebugger(XML: TXMLDocument);
var
  Node: TDOMNode;

begin
  Node := SelectSingleNode('/debugger_common/sets/gdb_debugger/*/NAME/str/text()',
    fNodeValueDebugger, XML);
  if Assigned(Node) then
    DeleteNode(Node, 3);
end;

procedure TCodeBlocksPatcher.RemoveGlobalVariables(XML: TXMLDocument);
var
  Node: TDOMNode;
  ActiveSet: string;

begin
  ActiveSet := GetGlobalVariablesActiveSet(XML);
  Node := SelectSingleNode(Format('/gcv/sets/%s/%s',
    [ActiveSet, fNodeValueGlobalVariables]), XML);
  if Assigned(Node) then
    DeleteNode(Node);
end;

procedure TCodeBlocksPatcher.RemoveTools(XML: TXMLDocument);
var
  ToolsDesign: TStringList;
  i: Integer;
  ToolDesignNodeValue: string;
  ToolNode: TDOMNode;

  procedure RemoveUselessToolsSeparators;
  var
    j, NodeCount: Integer;
    RootNode,
    CurrentNode,
    CurrentTextNode,
    PreviousNode: TDOMNode;
    CurrentTextNodeValue: WideString;

  begin
{$IFDEF DEBUG}
    DebugLog('RemoveUselessToolsSeparators');
{$ENDIF}
    PreviousNode := nil;
    RootNode := SelectSingleNode('/tools', XML);
    if Assigned(RootNode) then
    begin
      NodeCount := RootNode.ChildNodes.Count;
      for j := RootNode.ChildNodes.Count - 1 downto 0 do
      begin
        CurrentNode := RootNode.ChildNodes[j];
{$IFDEF DEBUG}
        DebugLog(AnsiString(CurrentNode.NodeName));
{$ENDIF}
        CurrentTextNode := SelectSingleNode('NAME/str/text()', CurrentNode);
        if Assigned(CurrentTextNode) then
        begin
          CurrentTextNodeValue := CurrentTextNode.NodeValue;
          if WideSameText(CurrentTextNodeValue, SEPARATOR_NAME) then
          begin
{$IFDEF DEBUG}
            DebugLog('  IsSeparator!');
{$ENDIF}
            if Assigned(PreviousNode) then
            begin
              DeleteNode(PreviousNode);
              Dec(NodeCount);
{$IFDEF DEBUG}
              DebugLog('  Deleted!');
{$ENDIF}
            end;
            PreviousNode := CurrentNode;
          end;
        end;
      end; // for

      // If the tools section contains only one separator and nothing else, delete it
      if Assigned(PreviousNode) and (NodeCount = 1) then
      begin
        DeleteNode(PreviousNode);
{$IFDEF DEBUG}
        DebugLog('Removing the only separator from the tools section!');
{$ENDIF}
      end;

    end; // Assigned RootNode
  end;

begin
{$IFDEF DEBUG}
  DebugLog('RemoveTools:');
{$ENDIF}

  // Remove all tools added by DreamSDK
  if NodeExists(XML, '/tools') then
  begin
    ToolsDesign := TStringList.Create;
    try
      ToolsDesign.LoadFromFile(fDesignFileNameTools);
      for i := 0 to ToolsDesign.Count - 1 do
      begin
        ToolDesignNodeValue := ToolsDesign[i];
        if not SameText(ToolDesignNodeValue, SEPARATOR_NAME) then
        begin
{$IFDEF DEBUG}
          DebugLog('  ' + ToolDesignNodeValue);
{$ENDIF}
          ToolNode := SelectSingleNode('/tools/*/NAME/str/text()', ToolDesignNodeValue, XML);
          if Assigned(ToolNode) then
            DeleteNode(ToolNode, 3); // delete the found tool
        end;
      end;
    finally
      ToolsDesign.Free;
    end;

    // Remove useless separators
    RemoveUselessToolsSeparators;
  end;
end;

function TCodeBlocksPatcher.IsCodeBlocksPatchInstalled: Boolean;
begin
  Result := SameText(GetRegisteredString(INI_SECTION_IDE, INI_KEY_IDE_KIND), '1');
end;

procedure TCodeBlocksPatcher.SetCodeBlocksBackupDirectory(AValue: TFileName);
begin
  if fCodeBlocksBackupDirectory <> AValue then
    fCodeBlocksBackupDirectory := IncludeTrailingPathDelimiter(
      ParseInputFileSystemObject(AValue))
end;

procedure TCodeBlocksPatcher.SetCodeBlocksInstallationDirectory(
  AValue: TFileName);
begin
  if fCodeBlocksInstallationDirectory <> AValue then
    fCodeBlocksInstallationDirectory := IncludeTrailingPathDelimiter(
      ParseInputFileSystemObject(AValue));
end;

procedure TCodeBlocksPatcher.SetHomeDirectory(AValue: TFileName);
begin
  if fHomeDirectoryForDreamSDK <> AValue then
    fHomeDirectoryForDreamSDK := ExcludeTrailingPathDelimiter(
      ParseInputFileSystemObject(AValue));
end;

procedure TCodeBlocksPatcher.SetOperation(AValue: TCodeBlocksPatcherOperation);

  function GetRegisteredCodeBlocksInstallationDirectory: TFileName;
  begin
    Result := GetRegisteredString(INI_SECTION_SETUP, INI_KEY_INSTALLATION_PATH);
  end;

  function GetRegisteredCodeBlocksBackupDirectory: TFileName;
  begin
    Result := GetRegisteredString(INI_SECTION_SETUP, INI_KEY_BACKUP_PATH);
  end;

  function GetRegisteredCodeBlocksConfigurationFileNames: string;
  begin
    Result := GetRegisteredString(INI_SECTION_SETUP, INI_KEY_CONFIGURATION_FILENAMES);
  end;

begin
  if (fOperation <> AValue) then
  begin
    fOperation := AValue;

    // When uninstalling, override supplied parameters with the proper one from
    // the DreamSDK configuration (if possible)
    if (Operation <> pmInstall) and IsCodeBlocksPatchInstalled then
    begin
      CodeBlocksInstallationDirectory :=
        GetRegisteredCodeBlocksInstallationDirectory;
      CodeBlocksBackupDirectory :=
        GetRegisteredCodeBlocksBackupDirectory;
      CodeBlocksConfigurationFileNames.SetItems(
        GetRegisteredCodeBlocksConfigurationFileNames, FILENAMES_SEPARATOR);
    end;
  end;
end;

function TCodeBlocksPatcher.SourceKindToFragmentFileName(
  const SourceKind: TSourceKind): TFileName;
begin
  Result := EmptyStr;
  case SourceKind of
    skDebugger:
      Result := fFragmentFileNameDebugger;
    skGlobalVariables:
      Result := fFragmentFileNameGlobalVariables;
    skTools:
      Result := fFragmentFileNameTools;
  end;
end;

procedure TCodeBlocksPatcher.DoPatchInstall;
begin
  ExecuteTask('Patching Code::Blocks installation directory', @InstallCodeBlocksPatch);
  ExecuteTask('Patching Code::Blocks configuration file', @PatchCodeBlocksConfiguration);
  ExecuteTask('Updating the DreamSDK configuration file', @UpdateConfigurationInstall);
end;

procedure TCodeBlocksPatcher.DoPatchUninstall;
begin
  ExecuteTask('Restoring Code::Blocks installation directory', @UninstallCodeBlocksPatch);
  ExecuteTask('Cleaning up Code::Blocks configuration file', @CleanCodeBlocksConfiguration);
  ExecuteTask('Updating the DreamSDK configuration file', @UpdateConfigurationUninstall);
end;

procedure TCodeBlocksPatcher.FixTools(
  const CodeBlocksConfigurationFileName: TFileName);

  procedure FixToolsSeparators(const CodeBlocksConfigurationFileName: TFileName);
  var
    Buffer, ToolsDesign: TStringList;
    i, SectionPositionIndex, ToolIndex, DesignToolIndex: Integer;
    Done: Boolean;
    ProcessedBuffer, Line, ToolName, ToolSeparatorTag: string;
    IsPreviousToolSeparator: Boolean;

    function IsToolsSectionAlreadyFilled: Boolean;
    begin
      Result := (ToolIndex <> 0) and (DesignToolIndex = 0);
    end;

    function IsSeparatorNeededByDesign: Boolean;
    begin
      Result := (DesignToolIndex > 0)
        and (SameText(ToolsDesign[DesignToolIndex - 1], SEPARATOR_NAME));
    end;

    procedure AddSeparator;
    begin
      Buffer[i] := ToolSeparatorTag + Line;
    end;

    function ExtractCurrentToolName: string;
    var
      PartialToolName: string;

    begin
      PartialToolName := ExtractStr(ProcessedBuffer + Line, ']]></str>', Buffer.Text);
      Result := Trim(Right('<![CDATA[', PartialToolName));
    end;

    procedure InitializeAlgorithm;
    begin
      i := SectionPositionIndex + 1;
      Done := False;
      ToolIndex := 0;
      ProcessedBuffer := EmptyStr;
      IsPreviousToolSeparator := False;
    end;

  begin
    ToolSeparatorTag := LoadFileToString(fFragmentFileNameToolsSeparator) + sLineBreak;
    Buffer := TStringList.Create;
    ToolsDesign := TStringList.Create;
    try
      ToolsDesign.LoadFromFile(fDesignFileNameTools);
      Buffer.LoadFromFile(CodeBlocksConfigurationFileName);
      SectionPositionIndex := StringListSubstringIndexOf(Buffer, '<tools>');
      if SectionPositionIndex <> -1 then
      begin
        InitializeAlgorithm;

        while (i < Buffer.Count) and (not Done) do
        begin
          Line := Buffer[i];
          Done := IsInString('</tools>', Line);

          if not Done then
          begin
            if IsInString('<tool', Line) then
            begin
              ToolName := ExtractCurrentToolName;
{$IFDEF DEBUG}
              WriteLn('Detected Tool: ', ToolName);
{$ENDIF}
              DesignToolIndex := ToolsDesign.IndexOf(ToolName);
              if DesignToolIndex <> -1 then
              begin
{$IFDEF DEBUG}
                WriteLn('Processing Tool #', ToolIndex, ': ', ToolName);
{$ENDIF}
                // Process the first separator...
                if IsToolsSectionAlreadyFilled and (not IsPreviousToolSeparator) then
                begin
{$IFDEF DEBUG}
                  WriteLn('* Adding first separator, as the tools section is NOT empty...');
{$ENDIF}
                  // Adding separator...
                  AddSeparator;
                end;

                // Process other separators
                if (IsSeparatorNeededByDesign) and (not IsPreviousToolSeparator) then
                begin
{$IFDEF DEBUG}
                  WriteLn('* Adding separator by design for ', ToolName);
{$ENDIF}
                  AddSeparator;
                end;

              end; // DesignToolIndex

              // Handle control variables...
              IsPreviousToolSeparator := SameText(ToolName, SEPARATOR_NAME);
              Inc(ToolIndex);
            end; // IsInString

            ProcessedBuffer := ProcessedBuffer + Buffer[i] + sLineBreak;
          end; // not Done

          Inc(i);
        end; // while

        Buffer.SaveToFile(CodeBlocksConfigurationFileName);
      end;
    finally
      ToolsDesign.Free;
      Buffer.Free;
    end;
  end;

begin
  FixToolsSeparators(CodeBlocksConfigurationFileName);
  FixSection(CodeBlocksConfigurationFileName, 'tools', 'tool', '%0.2d', 0);
end;

procedure TCodeBlocksPatcher.FixDebugger(
  const CodeBlocksConfigurationFileName: TFileName);
begin
  FixSection(CodeBlocksConfigurationFileName, 'gdb_debugger', 'conf', '%d', 1);
end;

function TCodeBlocksPatcher.GetCodeBlocksAvailableUsers: TStringList;
var
  i: Integer;
  UsersDirectory: TFileName;
  CurrentUserName: string;

begin
  fCodeBlocksAvailableUsers.Clear;
  UsersDirectory := GetUsersDirectory;
  for i := 0 to CodeBlocksConfigurationFileNames.Count - 1 do
  begin
    CurrentUserName := ExtractStr(UsersDirectory, DirectorySeparator,
      CodeBlocksConfigurationFileNames[i]);
    fCodeBlocksAvailableUsers.Add(GetFriendlyUserName(CurrentUserName));
  end;
  Result := fCodeBlocksAvailableUsers;
end;

function TCodeBlocksPatcher.GetCodeBlocksInstalledUsers: TStringList;
var
  i: Integer;
  UserName: string;

begin
  fCodeBlocksInstalledUsers.Clear;
  for i := 0 to CodeBlocksConfigurationFileNames.Count - 1 do
  begin
    UserName := GetUserFromAppDataDirectory(CodeBlocksConfigurationFileNames[i]);
    if not IsEmpty(UserName) then
      UserName := GetFriendlyUserName(UserName)
    else
      UserName := Format('<%s>', [ExtractFileName(CodeBlocksConfigurationFileNames[i])]);
    fCodeBlocksInstalledUsers.Add(UserName);
  end;
  Result := fCodeBlocksInstalledUsers;
end;

function TCodeBlocksPatcher.GetConfigurationFileName: TFileName;
const
  IDE_CONFIGURATION_FILE = 'msys\1.0\etc\dreamsdk\ide.conf';
begin
  Result := IncludeTrailingPathDelimiter(HomeDirectory)
    + IDE_CONFIGURATION_FILE;
end;

function TCodeBlocksPatcher.GetRegisteredString(const Section, Key: string): string;
var
  IniFile: TIniFile;

begin
  Result := EmptyStr;
  if FileExists(ConfigurationFileName) then
  begin
    IniFile := TIniFile.Create(ConfigurationFileName);
    try
      Result := IniFile.ReadString(Section, Key, EmptyStr);
    finally
      IniFile.Free;
    end;
  end;
end;

function TCodeBlocksPatcher.GetGlobalVariablesActiveSet(
  XMLDocument: TXMLDocument): string;
var
  Node: TDOMNode;

begin
  Result := 'default';
  Node := SelectSingleNode('/gcv/ACTIVE/str/text()', XMLDocument);
  if Assigned(Node) then
    Result := AnsiString(Node.NodeValue);
end;

function TCodeBlocksPatcher.GetReady: Boolean;
var
  CodeBlocksConfigurationIndex: Integer;

begin
  Result := DirectoryExists(GetWorkingPath)
    and DirectoryExists(HomeDirectory)
    and DirectoryExists(CodeBlocksInstallationDirectory);

  for CodeBlocksConfigurationIndex := 0 to CodeBlocksConfigurationFileNames.Count - 1 do
    Result := Result and FileExists(CodeBlocksConfigurationFileNames[CodeBlocksConfigurationIndex]);

  if (Operation = pmUninstall) then
    Result := Result and IsCodeBlocksPatchInstalled;
end;

function TCodeBlocksPatcher.GetFriendlyUserName(const UserName: string): string;
var
  CurrentUserFullName: string;

begin
  Result := UserName;
  CurrentUserFullName := GetUserFullNameFromUserName(UserName);
  if not IsEmpty(CurrentUserFullName) then
    Result := Format('%s (%s)', [CurrentUserFullName, UserName]);
end;

function TCodeBlocksPatcher.Merge(
  const CodeBlocksConfigurationFileName: TFileName;
  SourceKind: TSourceKind): Boolean;
var
  TargetFileStream, SourceFileStream: TFileStream;
  TargetXML, SourceXML: TXMLDocument;
  SourceFileName: TFileName;

begin
  Result := True;
  try
    // Inject the DreamSDK fragment into the CodeBlocksConfig XML file.
    TargetFileStream := TFileStream.Create(CodeBlocksConfigurationFileName, fmOpenReadWrite);
    SourceFileName := SourceKindToFragmentFileName(SourceKind);
    SourceFileStream := TFileStream.Create(SourceFileName, fmOpenRead);
    try
      ReadXMLFile(TargetXML, TargetFileStream);
      ReadXMLFile(SourceXML, SourceFileStream);

      case SourceKind of
        skDebugger:
          InjectDebugger(SourceXML, TargetXML);
        skGlobalVariables:
          InjectGlobalVariables(SourceXML, TargetXML);
        skTools:
          InjectTools(SourceXML, TargetXML);
      end;

      TargetFileStream.Size := 0;
      TargetFileStream.Seek(0, soBeginning);
      WriteXMLFile(TargetXML, TargetFileStream);
    finally
      TargetXML.Free;
      TargetFileStream.Free;
      SourceXML.Free;
      SourceFileStream.Free;
    end;

    // Fix the numbered sections if needed.
    case SourceKind of
      skDebugger:
        FixDebugger(CodeBlocksConfigurationFileName);
      skTools:
        FixTools(CodeBlocksConfigurationFileName);
    end;

    // Handle the DREAMSDK_HOME_VARIABLE variable representing the DreamSDK home base directory.
    HandleBaseDirectory(CodeBlocksConfigurationFileName);

    // Reformat the indentation of the XML.
    ReformatXML(CodeBlocksConfigurationFileName);
  except
    on E:Exception do
    begin
      Result := False;
      WriteLn('Error: Unable to merge: ', E.Message);
    end;
  end;
end;

function TCodeBlocksPatcher.Cleanup(
  const CodeBlocksConfigurationFileName: TFileName;
  SourceKind: TSourceKind): Boolean;
var
  TargetFileStream: TFileStream;
  TargetXML: TXMLDocument;

begin
  Result := True;
  try
    TargetFileStream := TFileStream.Create(CodeBlocksConfigurationFileName, fmOpenReadWrite);
    try
      ReadXMLFile(TargetXML, TargetFileStream);

      case SourceKind of
        skDebugger:
          RemoveDebugger(TargetXML);
        skGlobalVariables:
          RemoveGlobalVariables(TargetXML);
        skTools:
          RemoveTools(TargetXML);
      end;

      TargetFileStream.Size := 0;
      TargetFileStream.Seek(0, soBeginning);
      WriteXMLFile(TargetXML, TargetFileStream);
    finally
      TargetXML.Free;
      TargetFileStream.Free;
    end;

    // Fix the numbered sections if needed.
    case SourceKind of
      skDebugger:
        FixDebugger(CodeBlocksConfigurationFileName);
      skTools:
        FixTools(CodeBlocksConfigurationFileName);
    end;

    // Reformat the indentation of the XML.
    ReformatXML(CodeBlocksConfigurationFileName);
  except
    on E:Exception do
    begin
      Result := False;
      WriteLn('Error: Unable to cleanup: ', E.Message);
    end;
  end;
end;

function TCodeBlocksPatcher.UpdateConfiguration(
  const Operation: TCodeBlocksPatcherOperation): Boolean;
const
  IDE_EXPORT_LIB_INFO_DIR = 'share\CodeBlocks\templates\wizard\dc\libinfo\';

var
  ExportLibraryInformationPath,
  InstallationPath,
  BackupPath: TFileName;
  IniFile: TIniFile;
  Kind: Integer;
  ExportLibraryInformation: Boolean;

begin
  case Operation of

    pmInstall:
      begin
        Kind := 1; // Code::Blocks
        ExportLibraryInformation := True;
        ExportLibraryInformationPath := CodeBlocksInstallationDirectory
          + IDE_EXPORT_LIB_INFO_DIR;
        InstallationPath := CodeBlocksInstallationDirectory;
        BackupPath := CodeBlocksBackupDirectory;

        if not DirectoryExists(ExportLibraryInformationPath) then
          ForceDirectories(ExportLibraryInformationPath);

        SetDirectoryRights(ExportLibraryInformationPath, GetEveryoneName,
          ACL_RIGHT_FULL);
      end;

    pmUninstall:
      begin
        Kind := 0; // None
        ExportLibraryInformation := False;
        ExportLibraryInformationPath := EmptyStr;
        InstallationPath := EmptyStr;
        BackupPath := EmptyStr;
      end;

  end;

  IniFile := TIniFile.Create(ConfigurationFileName);
  try
    IniFile.WriteInteger(INI_SECTION_IDE, INI_KEY_IDE_KIND, Kind);
    IniFile.WriteBool(INI_SECTION_IDE,
      INI_KEY_EXPORT_LIBRARY_INFORMATION_ENABLED, ExportLibraryInformation);
    IniFile.WriteString(INI_SECTION_IDE,
      INI_KEY_EXPORT_LIBRARY_INFORMATION_PATH, ExportLibraryInformationPath);

    IniFile.WriteString(INI_SECTION_SETUP, INI_KEY_INSTALLATION_PATH,
      InstallationPath);
    IniFile.WriteString(INI_SECTION_SETUP, INI_KEY_BACKUP_PATH, BackupPath);
    IniFile.WriteString(INI_SECTION_SETUP, INI_KEY_CONFIGURATION_FILENAMES,
      CodeBlocksConfigurationFileNames.GetItems(FILENAMES_SEPARATOR));
  finally
    IniFile.Free;
  end;

  Result := FileExists(ConfigurationFileName);
end;

procedure TCodeBlocksPatcher.ExecuteTask(const StepMessage: string;
  TaskFunction: TTaskProc);
var
  TaskResult: Boolean;

begin
  if Assigned(fProcessTaskBegin) then
    fProcessTaskBegin(Self, StepMessage);

  TaskResult := TaskFunction();

  if Assigned(fProcessTaskEnd) then
    fProcessTaskEnd(Self, StepMessage, TaskResult);

  fExecuteResult := fExecuteResult and TaskResult;
end;

function TCodeBlocksPatcher.BackupFiles: Boolean;
begin
  Result := ForceDirectories(CodeBlocksBackupDirectory);
  Result := Result and RunCodeBlocksBackupRestore(cbBackup);
end;

function TCodeBlocksPatcher.RunCodeBlocksBackupRestore(
  const Operation: TCodeBlocksBackupRestoreOperation): Boolean;
var
  Switch,
  Buffer: String;

begin
  Switch := '/B';
  if Operation = cbRestore then
    Switch := '/R';

  Buffer := Run(Format('"%s" %s "%s" "%s"', [
    fCodeBlocksBackupRestoreFileName,
    Switch,
    ExcludeTrailingPathDelimiter(CodeBlocksInstallationDirectory),
    ExcludeTrailingPathDelimiter(CodeBlocksBackupDirectory)
  ]));

  Result := (Pos('done!', LowerCase(Buffer)) > 0);
end;

function TCodeBlocksPatcher.RunCodeBlocksSplash(
  const Operation: TCodeBlocksSplashOperation): Boolean;
var
  AdditionalSwitch: string;
  ExecWait: Boolean;

begin
  AdditionalSwitch := '';
  ExecWait := False;

  case Operation of
    soInstall: AdditionalSwitch := '/install';
    soUninstall: AdditionalSwitch := '/uninstall';
    soReinstall: AdditionalSwitch := '/reinstall';
    soClose:
      begin
        AdditionalSwitch := '/close';
        ExecWait := True;
      end;
  end;

  if ExecWait then
  begin
{$IFDEF DEBUG}
    DebugLog('RunAndWait for ' + AdditionalSwitch);
{$ENDIF}
    Result := RunAndWait(fCodeBlocksSplashFileName, AdditionalSwitch)
  end
  else
  begin
{$IFDEF DEBUG}
    DebugLog('RunNoWait for ' + AdditionalSwitch);
{$ENDIF}
    Result := RunNoWait(fCodeBlocksSplashFileName, AdditionalSwitch);
  end;

{$IFDEF DEBUG}
  DebugLog('  ' + BoolToStr(Result));
{$ENDIF}
end;

procedure TCodeBlocksPatcher.ExtractEmbeddedFiles;
const
  FILE_BACKUP_RESTORE = 'codeblocks-backup-restore.cmd';
  FILE_SPLASH = 'codeblocks-splash.exe';
  FILE_PACKAGE = 'codeblocks-17.12-dreamsdk-addon-bin.7z';
  FILE_CONFIG = 'codeblocks-patcher-data.zip';
  FILE_7ZIP = '7za.exe';

  DIRECTORY_SOURCE = 'data';

  EMBEDDED_BACKUP_RESTORE = 'BACKUPRESTORE';
  EMBEDDED_SPLASH = 'SPLASH';
  EMBEDDED_PACKAGE = 'PACKAGE';
  EMBEDDED_CONFIG = 'CONFIG';

var
  DataFileName: TFileName;

  procedure InitializeDataFiles;
  var
    DesignDirectory,
    FragmentsDirectory: TFileName;

  begin
    DesignDirectory := fSourceDirectory + 'design'
      + DirectorySeparator;
    FragmentsDirectory := fSourceDirectory + 'fragments'
      + DirectorySeparator;

    // Design
    fDesignFileNameDebugger := DesignDirectory + 'debugger.dat';
    fNodeValueDebugger := LoadFileToString(fDesignFileNameDebugger);
    fDesignFileNameGlobalVariables := DesignDirectory + 'gcv.dat';
    fNodeValueGlobalVariables := LoadFileToString(fDesignFileNameGlobalVariables);
    fDesignFileNameTools := DesignDirectory + 'tools.dat';

    // Fragments
    fFragmentFileNameDebugger := FragmentsDirectory + 'debugger.xml';
    fFragmentFileNameGlobalVariables := FragmentsDirectory + 'gcv.xml';
    fFragmentFileNameTools := FragmentsDirectory + 'tools.xml';
    fFragmentFileNameToolsSeparator := FragmentsDirectory + 'tools-separator.xml';
  end;

begin
{$IFNDEF LITE_VERSION}

  // Code::Blocks Backup/Restore
  fCodeBlocksBackupRestoreFileName :=
    ExtractEmbeddedFileToWorkingPath(EMBEDDED_BACKUP_RESTORE, FILE_BACKUP_RESTORE);

  // 7-Zip for Code::Blocks Backup/Restore
{$IFDEF LZMA_SUPPORT}
  ExtractEmbeddedFileToWorkingPath(EMBEDDED_7ZIP, FILE_7ZIP);
{$ENDIF}

  // Code::Blocks Splash
  fCodeBlocksSplashFileName :=
    ExtractEmbeddedFileToWorkingPath(EMBEDDED_SPLASH, FILE_SPLASH);

  // Code::Blocks Patch Data
  fCodeBlocksPatchFileName := ExtractEmbeddedFileToWorkingPath(
    EMBEDDED_PACKAGE, FILE_PACKAGE);

  // Code::Blocks Configuration Data
  fSourceDirectory := GetWorkingPath + DIRECTORY_SOURCE + DirectorySeparator;
  DataFileName := ExtractEmbeddedFileToWorkingPath(EMBEDDED_CONFIG, FILE_CONFIG);
  if FileExists(DataFileName) then
  begin
    UncompressZipFile(DataFileName, fSourceDirectory);
    InitializeDataFiles;
    KillFile(DataFileName);
  end;

{$ENDIF}
end;

function TCodeBlocksPatcher.GetSourcePackageDirectory: TFileName;
begin
  Result := fSourceDirectory + 'package' + DirectorySeparator;
end;

function TCodeBlocksPatcher.PatchCodeBlocksConfiguration: Boolean;
var
  i: Integer;
  CodeBlocksConfigurationFileName: TFileName;

begin
  Result := True;
  for i := 0 to CodeBlocksConfigurationFileNames.Count - 1 do
  begin
    CodeBlocksConfigurationFileName := CodeBlocksConfigurationFileNames[i];
    Result := Result and Merge(CodeBlocksConfigurationFileName, skDebugger);
    Result := Result and Merge(CodeBlocksConfigurationFileName, skGlobalVariables);
    Result := Result and Merge(CodeBlocksConfigurationFileName, skTools);
  end;
end;

function TCodeBlocksPatcher.UpdateConfigurationInstall: Boolean;
begin
  Result := UpdateConfiguration(pmInstall);
end;

function TCodeBlocksPatcher.UninstallCodeBlocksPatch: Boolean;
begin
  Result := False;
  if DirectoryExists(CodeBlocksBackupDirectory) then
    Result := RunCodeBlocksBackupRestore(cbRestore);
end;

function TCodeBlocksPatcher.CleanCodeBlocksConfiguration: Boolean;
var
  i: Integer;
  CodeBlocksConfigurationFileName: TFileName;

begin
  Result := True;
  for i := 0 to CodeBlocksConfigurationFileNames.Count - 1 do
  begin
    CodeBlocksConfigurationFileName := CodeBlocksConfigurationFileNames[i];
    Result := Result and Cleanup(CodeBlocksConfigurationFileName, skDebugger);
    Result := Result and Cleanup(CodeBlocksConfigurationFileName, skGlobalVariables);
    Result := Result and Cleanup(CodeBlocksConfigurationFileName, skTools);
  end;
end;

function TCodeBlocksPatcher.UpdateConfigurationUninstall: Boolean;
begin
  Result := UpdateConfiguration(pmUninstall);
end;

function TCodeBlocksPatcher.PatcherOperationToSplashOperation: TCodeBlocksSplashOperation;
begin
  Result := soClose;
  case fOperation of
    pmInstall:
      Result := soInstall;
    pmUninstall:
      Result := soUninstall;
    pmReinstall:
      Result := soReinstall;
  end;
end;

{$IFDEF DEBUG}
procedure TCodeBlocksPatcher.IterateNodes(Node: TDOMNode);
var
  NodeList: TDOMNodeList;
  i, j: Integer;
  CurrentNode, Attribute: TDOMNode;

begin
//  WriteLn(Node.NodeName, ': ', Node.NodeValue);
  NodeList := Node.GetChildNodes;
  for i := 0 to NodeList.Count - 1 do
  begin
    CurrentNode := NodeList[i];
    WriteLn(CurrentNode.NodeName, ': ', CurrentNode.NodeValue);

    if CurrentNode.HasAttributes then
      for j := 0 to CurrentNode.Attributes.Length - 1 do
      begin
        Attribute := CurrentNode.Attributes[j];
        Write(Attribute.NodeName, ' = "', Attribute.NodeValue, '" ');
      end;

    if CurrentNode.NodeType = ELEMENT_NODE then
      IterateNodes(CurrentNode);
  end;
end;
{$ENDIF}

end.

