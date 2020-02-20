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
  FSTools,
  Settings;

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
  TErrorEvent = procedure(Sender: TObject; const ErrorMessage: string) of object;

  TCodeBlocksBackupRestoreOperation = (cbBackup, cbRestore);
  TCodeBlocksSplashOperation = (soInstall, soUninstall, soReinstall, soClose);
  TSourceKind = (skDebugger, skGlobalVariables, skTools);

  { TCodeBlocksPatcher }
  TCodeBlocksPatcher = class(TObject)
  private
{$IFNDEF LITE_VERSION}
    fDesignFileNameDebugger: TFileName;
    fDesignFileNameGlobalVariables: TFileName;
    fCodeBlocksPatchFileName: TFileName;
{$ENDIF}
    fError: TErrorEvent;
    fSettings: TDreamcastSoftwareDevelopmentCodeBlocksPatcherSettings;
    fSourceDirectory: TFileName;
    fDesignFileNameTools: TFileName;
    fFragmentFileNameDebugger: TFileName;
    fFragmentFileNameGlobalVariables: TFileName;
    fFragmentFileNameTools: TFileName;
    fFragmentFileNameToolsSeparator: TFileName;
    fNodeValueDebugger: string;
    fExecuteResult: Boolean;
    fProcessBegin: TNotifyEvent;
    fCodeBlocksBackupRestoreFileName: TFileName;
    fCodeBlocksSplashFileName: TFileName;
    fProcessEnd: TTerminateEvent;
    fOperation: TCodeBlocksPatcherOperation;
    fProcessTaskBegin: TTaskBeginEvent;
    fProcessTaskEnd: TTaskEndEvent;
    fVisibleSplash: Boolean;
    procedure ExtractEmbeddedFiles;
    procedure FixTools(const CodeBlocksConfigurationFileName: TFileName);
    procedure FixDebugger(const CodeBlocksConfigurationFileName: TFileName);
    function GetConfigurationFilesNamesReady: Boolean;
    function GetGlobalVariablesActiveSet(XMLDocument: TXMLDocument): string;
    function GetReady: Boolean;
    procedure InjectDebugger(SourceXML, TargetXML: TXMLDocument);
    procedure InjectGlobalVariables(SourceXML, TargetXML: TXMLDocument);
    procedure InjectTools(SourceXML, TargetXML: TXMLDocument);
    procedure RemoveDebugger(XML: TXMLDocument);
    procedure RemoveGlobalVariables(XML: TXMLDocument);
    procedure RemoveTools(XML: TXMLDocument);
    function IsCodeBlocksPatchInstalled: Boolean;
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

    function TaskInstallCodeBlocksPatch: Boolean;
    function TaskPatchCodeBlocksConfiguration: Boolean;
    function TaskUpdateConfigurationInstall: Boolean;

    function TaskUninstallCodeBlocksPatch: Boolean;
    function TaskCleanCodeBlocksConfiguration: Boolean;
    function TaskUpdateConfigurationUninstall: Boolean;

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

    procedure LogError(const Message: string);

    {$IFDEF DEBUG}procedure IterateNodes(Node: TDOMNode);{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    function Execute: Boolean;

    // Is the Patcher ready to execute?
    property Ready: Boolean read GetReady;

    property ConfigurationFilesNamesReady: Boolean
      read GetConfigurationFilesNamesReady;

    // Mode: Install or Uninstall
    property Operation: TCodeBlocksPatcherOperation
      read fOperation
      write SetOperation;

    property VisibleSplash: Boolean
      read fVisibleSplash
      write fVisibleSplash;

    property Settings: TDreamcastSoftwareDevelopmentCodeBlocksPatcherSettings
      read fSettings;

    // Events
    property OnError: TErrorEvent
      read fError
      write fError;
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

function SourceKindToString(const SourceKind: TSourceKind): string;

implementation

uses
  SysTools,
  RefBase,
  Version,
  RunTools,
  XmlTools;

const
  DREAMSDK_HOME_VARIABLE = '{app}';
  SEPARATOR_NAME = '---separator---';

function SourceKindToString(const SourceKind: TSourceKind): string;
begin
  Result := EmptyStr;
  case SourceKind of
    skDebugger:
      Result := 'Debugger';
    skGlobalVariables:
      Result := 'Global Variables';
    skTools:
      Result := 'Tools';
  end;
end;

constructor TCodeBlocksPatcher.Create;
begin
  // Initializing settings
  fSettings := TDreamcastSoftwareDevelopmentCodeBlocksPatcherSettings.Create;

  fOperation := pmUndefined;
  ExtractEmbeddedFiles;
end;

destructor TCodeBlocksPatcher.Destroy;
begin
  fSettings.Free;
{$IFNDEF LITE_VERSION}
  Sleep(500);
  KillProcessByName(fCodeBlocksSplashFileName);
{$ENDIF}
  inherited Destroy;
end;

function TCodeBlocksPatcher.Execute: Boolean;
{$IFNDEF LITE_VERSION}
var
  SettingsCopy: TDreamcastSoftwareDevelopmentCodeBlocksPatcherSettings;
{$ENDIF}

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
          SettingsCopy := TDreamcastSoftwareDevelopmentCodeBlocksPatcherSettings.Create;
          try
            SettingsCopy.Assign(Settings);
            DoPatchUninstall;

            Sleep(500);

            Settings.Assign(SettingsCopy);
            DoPatchInstall;
          finally
            SettingsCopy.Free;
          end;
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
      Settings.HomeDirectory, [rfReplaceAll]);
    Buffer.SaveToFile(CodeBlocksConfigurationFileName);
  finally
    Buffer.Free;
  end;
end;

function TCodeBlocksPatcher.TaskInstallCodeBlocksPatch: Boolean;
{$IFNDEF LITE_VERSION}
const
  COMPILER_FILE = 'share\CodeBlocks\compilers\compiler_dc-gcc.xml';
  OPTIONS_FILE = 'share\CodeBlocks\compilers\options_dc-gcc.xml';

begin
  Result := BackupFiles;
  Result := Result and UncompressLzmaFile(fCodeBlocksPatchFileName,
    Settings.InstallationDirectory);
  Result := Result and PatchTextFile(Settings.InstallationDirectory
    + COMPILER_FILE,
    DREAMSDK_HOME_VARIABLE, Settings.HomeDirectory);
  Result :=  Result and PatchTextFile(Settings.InstallationDirectory
    + OPTIONS_FILE,
    DREAMSDK_HOME_VARIABLE, Settings.HomeDirectory);
{$ELSE}
begin
  Result := False;
{$ENDIF}
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
  GlobalVariables: TStringList;
  i: Integer;

begin
  ActiveSet := GetGlobalVariablesActiveSet(XML);
  GlobalVariables := TStringList.Create;
  try
    GlobalVariables.LoadFromFile(fDesignFileNameGlobalVariables);
    for i := 0 to GlobalVariables.Count - 1 do
    begin
      Node := SelectSingleNode(Format('/gcv/sets/%s/%s', [
        ActiveSet,
        GlobalVariables[i]
      ]), XML);
      if Assigned(Node) then
        DeleteNode(Node);
    end;
  finally
    GlobalVariables.Free;;
  end;
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
  Result := Settings.Installed;
end;

procedure TCodeBlocksPatcher.SetOperation(AValue: TCodeBlocksPatcherOperation);
begin
  if (fOperation <> AValue) then
  begin
    fOperation := AValue;

    // When uninstalling, override supplied parameters with the proper one from
    // the DreamSDK configuration (if possible)
    if (Operation <> pmInstall) then
      Settings.LoadConfiguration;
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
  ExecuteTask('Patching Code::Blocks installation directory', @TaskInstallCodeBlocksPatch);
  ExecuteTask('Patching Code::Blocks configuration file', @TaskPatchCodeBlocksConfiguration);
  ExecuteTask('Updating the DreamSDK configuration file', @TaskUpdateConfigurationInstall);
end;

procedure TCodeBlocksPatcher.DoPatchUninstall;
begin
  ExecuteTask('Restoring Code::Blocks installation directory', @TaskUninstallCodeBlocksPatch);
  ExecuteTask('Cleaning up Code::Blocks configuration file', @TaskCleanCodeBlocksConfiguration);
  ExecuteTask('Updating the DreamSDK configuration file', @TaskUpdateConfigurationUninstall);
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

function TCodeBlocksPatcher.GetConfigurationFilesNamesReady: Boolean;
var
  CodeBlocksConfigurationIndex: Integer;

begin
  Result := True;
  for CodeBlocksConfigurationIndex := 0 to
    Settings.ConfigurationFileNames.Count - 1 do
      Result := Result and FileExists(
        Settings.ConfigurationFileNames[CodeBlocksConfigurationIndex]);
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
begin
  Result := DirectoryExists(GetWorkingPath)
    and DirectoryExists(Settings.HomeDirectory)
    and DirectoryExists(Settings.InstallationDirectory);

  if (Operation = pmUninstall) then
    Result := Result and IsCodeBlocksPatchInstalled;
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
begin
  case Operation of
    pmInstall:
      Settings.Save(True);
    pmUninstall:
      Settings.Save(False);
  end;
  Result := FileExists(Settings.RegistryFileName);
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
  Result := ForceDirectories(Settings.BackupDirectory);
  Result := Result and RunCodeBlocksBackupRestore(cbBackup);
end;

function TCodeBlocksPatcher.RunCodeBlocksBackupRestore(
  const Operation: TCodeBlocksBackupRestoreOperation): Boolean;
var
  Switch,
  Buffer: string;

begin
  Switch := '/B';
  if Operation = cbRestore then
    Switch := '/R';

  Buffer := Run(Format('"%s" %s "%s" "%s"', [
    fCodeBlocksBackupRestoreFileName,
    Switch,
    ExcludeTrailingPathDelimiter(Settings.InstallationDirectory),
    ExcludeTrailingPathDelimiter(Settings.BackupDirectory)
  ]));

{$IFDEF DEBUG}
  DebugLog('RunCodeBlocksBackupRestore: ' + Buffer);
{$ENDIF}

  Result := (Pos('done!', LowerCase(Buffer)) > 0);

  if not Result then
    LogError(Buffer);
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

procedure TCodeBlocksPatcher.LogError(const Message: string);
begin
  if Assigned(fError) then
    fError(Self, Message);
end;

procedure TCodeBlocksPatcher.ExtractEmbeddedFiles;
{$IFNDEF LITE_VERSION}
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
    fDesignFileNameTools := DesignDirectory + 'tools.dat';

    // Fragments
    fFragmentFileNameDebugger := FragmentsDirectory + 'debugger.xml';
    fFragmentFileNameGlobalVariables := FragmentsDirectory + 'gcv.xml';
    fFragmentFileNameTools := FragmentsDirectory + 'tools.xml';
    fFragmentFileNameToolsSeparator := FragmentsDirectory + 'tools-separator.xml';
  end;

begin
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
{$ELSE}
begin
{$ENDIF}
end;

function TCodeBlocksPatcher.GetSourcePackageDirectory: TFileName;
begin
  Result := fSourceDirectory + 'package' + DirectorySeparator;
end;

function TCodeBlocksPatcher.TaskPatchCodeBlocksConfiguration: Boolean;
const
  LOCAL_ERROR_MESSAGE = 'Unable to merge the source section "%s" in the file "%s"!';

var
  i: Integer;
  CodeBlocksConfigurationFileName: TFileName;

  procedure ProcessConfiguration(SourceKind: TSourceKind);
  begin
    Result := Result and Merge(CodeBlocksConfigurationFileName, SourceKind);
    if not Result then
      LogError(Format(LOCAL_ERROR_MESSAGE, [SourceKindToString(SourceKind),
        CodeBlocksConfigurationFileName]));
  end;

begin
  Result := True;
  for i := 0 to Settings.ConfigurationFileNames.Count - 1 do
  begin
    CodeBlocksConfigurationFileName := Settings.ConfigurationFileNames[i];
    if FileExists(CodeBlocksConfigurationFileName) then
    begin
      ProcessConfiguration(skDebugger);
      ProcessConfiguration(skGlobalVariables);
      ProcessConfiguration(skTools);
    end;
  end;
end;

function TCodeBlocksPatcher.TaskUpdateConfigurationInstall: Boolean;
begin
  Result := UpdateConfiguration(pmInstall);
end;

function TCodeBlocksPatcher.TaskUninstallCodeBlocksPatch: Boolean;
begin
  Result := False;
  if DirectoryExists(Settings.BackupDirectory) then
    Result := RunCodeBlocksBackupRestore(cbRestore)
  else
    LogError('Backup directory doesn''t exists!');
end;

function TCodeBlocksPatcher.TaskCleanCodeBlocksConfiguration: Boolean;
const
  LOCAL_ERROR_MESSAGE = 'Unable to remove the section "%s" in the file "%s"!';

var
  i: Integer;
  CodeBlocksConfigurationFileName: TFileName;

  procedure ProcessConfiguration(SourceKind: TSourceKind);
  begin
    Result := Result and Cleanup(CodeBlocksConfigurationFileName, SourceKind);
    if not Result then
      LogError(Format(LOCAL_ERROR_MESSAGE, [SourceKindToString(SourceKind),
        CodeBlocksConfigurationFileName]));
  end;

begin
  Result := True;
  for i := 0 to Settings.ConfigurationFileNames.Count - 1 do
  begin
    CodeBlocksConfigurationFileName := Settings.ConfigurationFileNames[i];
    if FileExists(CodeBlocksConfigurationFileName) then
    begin
      ProcessConfiguration(skDebugger);
      ProcessConfiguration(skGlobalVariables);
      ProcessConfiguration(skTools);
    end;
  end;
end;

function TCodeBlocksPatcher.TaskUpdateConfigurationUninstall: Boolean;
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

