unit CBPatch;

{$mode objfpc}{$H+}
{$R embedded.rc}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, XPath, FSTools;

type
  TTaskProc = function: Boolean of object;

  TCodeBlocksPatcherOperation = (pmUndefined, pmInstall, pmUninstall);

  TTerminateEvent = procedure(Sender: TObject;
    const Success: Boolean) of object;
  TTaskBeginEvent = procedure(Sender: TObject;
    const Message: string) of object;
  TTaskEndEvent = procedure(Sender: TObject; const Message: string;
    const Success: Boolean) of object;

  TCodeBlocksBackupRestoreOperation = (cbBackup, cbRestore);
  TCodeBlocksSplashOperation = (soInstall, soUninstall, soClose);
  TSourceKind = (skDebugger, skGlobalVariables, skTools);

  { TCodeBlocksPatcher }
  TCodeBlocksPatcher = class(TObject)
  private
    fExecuteResult: Boolean;
    fProcessBegin: TNotifyEvent;
    fCodeBlocksBackupRestoreFileName: TFileName;
    fCodeBlocksSplashFileName: TFileName;
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
    procedure CleanEmbeddedFiles;
    procedure ExtractEmbeddedFiles;
    procedure FixTools(const CodeBlocksConfigurationFileName: TFileName);
    procedure FixDebugger(const CodeBlocksConfigurationFileName: TFileName);
    function GetConfigurationFileName: TFileName;
    function GetRegisteredString(const Key: string): string;
    function GetRegisteredCodeBlocksInstallationDirectory: TFileName;
    function GetRegisteredCodeBlocksBackupDirectory: TFileName;
    function GetGlobalVariablesActiveSet(XMLDocument: TXMLDocument): string;
    function GetReady: Boolean;
    procedure InjectDebugger(SourceXML, TargetXML: TXMLDocument);
    procedure InjectGlobalVariables(SourceXML, TargetXML: TXMLDocument);
    procedure InjectTools(SourceXML, TargetXML: TXMLDocument);
    function IsCodeBlocksPatchInstalled: Boolean;
    procedure SetCodeBlocksBackupDirectory(AValue: TFileName);
    procedure SetCodeBlocksInstallationDirectory(AValue: TFileName);
    procedure SetHomeDirectory(AValue: TFileName);
    procedure SetOperation(AValue: TCodeBlocksPatcherOperation);
    procedure SetSourceDirectory(AValue: TFileName);
    procedure FixSection(const CodeBlocksConfigurationFileName: TFileName;
      const SectionName, ItemName, ItemFormat: string; const StartIndex: Integer);
    procedure ReformatXML(const CodeBlocksConfigurationFileName: TFileName);
    procedure HandleBaseDirectory(
      const CodeBlocksConfigurationFileName: TFileName);
    function Merge(const CodeBlocksConfigurationFileName: TFileName;
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
    function GetSourceConfigurationDirectory: TFileName;
    function GetSourceFragmentsDirectory: TFileName;
    function GetSourcePackageDirectory: TFileName;
    function SelectSingleNode(XMLDoc: TXMLDocument; Path: string): TDOMNode;
    function NodeExists(XMLDocument: TXMLDocument; XPath: string; NodeValue: string): Boolean;
    function ForceNodes(XMLDoc: TXMLDocument; Path: string): TDOMNode;
    procedure ImportNode(TargetXMLDocument: TXMLDocument;
      SourceNode: TDOMNode; Path: string; AppendNode: Boolean); overload;
    procedure ImportNode(TargetXMLDocument: TXMLDocument;
      SourceNode: TDOMNode; Path: string); overload;
    function SourceKindToFileName(const SourceKind: TSourceKind): TFileName;

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

    // Sources files used for the patcher
    property SourceDirectory: TFileName
      read fSourceDirectory
      write SetSourceDirectory;

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
  RunTools;

const
  DREAMSDK_HOME_VARIABLE = '{app}';
  DOM_ROOT_PATH = '/CodeBlocksConfig';

  EMBEDDED_BACKUP_RESTORE = 'BACKUPRESTORE';
  EMBEDDED_SPLASH = 'SPLASH';

function SanitizeXPath(XPathExpression: string): string;
begin
  Result := XPathExpression;
  if not StartsWith('/', XPathExpression) then
    XPathExpression := '/' + XPathExpression;
  if not StartsWith(DOM_ROOT_PATH, XPathExpression) then
    Result := DOM_ROOT_PATH + XPathExpression;
end;

constructor TCodeBlocksPatcher.Create;

  // Define this to debug this procedure
  // {$DEFINE DEBUG_INITIALIZE_DEFAULTS}
  procedure InitializeDefaults;
  const
    DEFAULT_CODEBLOCKS_DIR_64 = '%ProgramFiles(x86)%\CodeBlocks';
    DEFAULT_CODEBLOCKS_DIR_32 = '%ProgramFiles%\CodeBlocks';
    DEFAULT_CODEBLOCKS_CONFIGURATION_FILE = '%sCodeBlocks\default.conf';
    DEFAULT_CODEBLOCKS_BACKUP_DIR = '%s\support\ide\codeblocks\';
    DEFAULT_SOURCE_DIR = 'data';

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
      GetUserAppDataList(UsersAppData);

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

    // Source Directory
    SourceDirectory := GetApplicationPath + DEFAULT_SOURCE_DIR;
  end;

begin
  fCodeBlocksConfigurationFileNames := TFileList.Create;
  fOperation := pmUndefined;
  InitializeDefaults;
  ExtractEmbeddedFiles;
end;

destructor TCodeBlocksPatcher.Destroy;
begin
  CleanEmbeddedFiles;
  fCodeBlocksConfigurationFileNames.Free;
  Sleep(500);
  KillProcessByName(fCodeBlocksSplashFileName);
  inherited Destroy;
end;

function TCodeBlocksPatcher.Execute: Boolean;
begin
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
    end;

    // Notify the process end
    if Assigned(fProcessEnd) then
      fProcessEnd(Self, fExecuteResult);

    // Hide the Splash
    if fVisibleSplash then
      RunCodeBlocksSplash(soClose);
  end;

  Result := fExecuteResult;
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

function TCodeBlocksPatcher.ForceNodes(XMLDoc: TXMLDocument;
  Path: string): TDOMNode;
var
  Buffer: TStringList;
  Node, NewNode: TDOMNode;
  i: Integer;
  NodeName: string;

begin
  Result := nil;
  Buffer := TStringList.Create;
  try
    Path := StringReplace(Path, DOM_ROOT_PATH, EmptyStr, [rfIgnoreCase]);
    StringToStringList(Path, '/', Buffer);

    Node := XMLDoc.DocumentElement;
    for i := 0 to Buffer.Count - 1 do
    begin
      NodeName := Trim(Buffer[i]);
      if not SameText(NodeName, EmptyStr) then
      begin
{$IFDEF DEBUG}
        WriteLn('NodeName: ', NodeName, ' on ', Node.NodeName);
{$ENDIF}
        NewNode := Node.FindNode(WideString(NodeName));
        if not Assigned(NewNode) then
        begin
{$IFDEF DEBUG}
          WriteLn('  Creating: ', NodeName);
{$ENDIF}
          NewNode := XMLDoc.CreateElement(WideString(NodeName));
          Node := Node.AppendChild(NewNode);
        end
        else
        begin
{$IFDEF DEBUG}
          WriteLn('  Existing: ', NodeName);
{$ENDIF}
          Node := NewNode;
        end;
      end;
    end;
    Result := Node;
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
  PACKAGE_FILE = 'codeblocks-17.12-dreamsdk-addon-bin.zip';
  COMPILER_FILE = 'share\CodeBlocks\compilers\compiler_dc-gcc.xml';
  OPTIONS_FILE = 'share\CodeBlocks\compilers\options_dc-gcc.xml';

begin
  Result := BackupFiles;
  Result := Result and UncompressZipFile(GetSourcePackageDirectory + PACKAGE_FILE,
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
    Result := SelectSingleNode(SourceXML, '/debugger_common/sets/gdb_debugger/conf1');
  end;

  function IsSegaDreamcastDebuggerProfileExists: Boolean;
  begin
    Result := NodeExists(TargetXML, '/debugger_common/sets/gdb_debugger/*/NAME/str/text()', 'Sega Dreamcast');
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
    ToolNameNode := SelectSingleNode(SourceXML, Format('/tools/tool%0.2d/NAME/str/text()', [i]));
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

function TCodeBlocksPatcher.IsCodeBlocksPatchInstalled: Boolean;
begin
  Result := SameText(GetRegisteredString('Kind'), '1');
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
begin
  if (fOperation <> AValue) then
  begin
    fOperation := AValue;

    // When uninstalling, override supplied parameters with the proper one from
    // the DreamSDK configuration (if possible)
    if (Operation = pmUninstall) and IsCodeBlocksPatchInstalled then
    begin
      CodeBlocksInstallationDirectory :=
        GetRegisteredCodeBlocksInstallationDirectory;
      CodeBlocksBackupDirectory :=
        GetRegisteredCodeBlocksBackupDirectory;
    end;
  end;
end;

procedure TCodeBlocksPatcher.SetSourceDirectory(AValue: TFileName);
begin
  if fSourceDirectory <> AValue then
    fSourceDirectory := IncludeTrailingPathDelimiter(ParseInputFileSystemObject(AValue));
end;

procedure TCodeBlocksPatcher.ReformatXML(
  const CodeBlocksConfigurationFileName: TFileName);
var
  TargetFileStream: TFileStream;
  TargetXML: TXMLDocument;

begin
  TargetFileStream := TFileStream.Create(CodeBlocksConfigurationFileName, fmOpenReadWrite);
  try
    ReadXMLFile(TargetXML, TargetFileStream);
    TargetFileStream.Size := 0;
    TargetFileStream.Seek(0, soBeginning);
    WriteXMLFile(TargetXML, TargetFileStream);
  finally
    TargetXML.Free;
    TargetFileStream.Free;
  end;
end;

function TCodeBlocksPatcher.SelectSingleNode(XMLDoc: TXMLDocument;
  Path: string): TDOMNode;
var
  XPathResult: TXPathVariable;

begin
  // Find DestinationNode
  Result := nil;

  Path := SanitizeXPath(Path);
  XPathResult := EvaluateXPathExpression(WideString(Path), XMLDoc.DocumentElement);
  if XPathResult.AsNodeSet.Count > 0 then
    Result := TDOMNode(XPathResult.AsNodeSet.Items[0]);

  // Destroying the XPathResult object...
  FreeAndNil(XPathResult);
end;

function TCodeBlocksPatcher.NodeExists(XMLDocument: TXMLDocument;
  XPath: string; NodeValue: string): Boolean;
var
  XPathResult: TXPathVariable;
  i, NodesCount: Integer;
  Node: TDOMNode;

begin
  Result := False;

  XPath := SanitizeXPath(XPath);
  XPathResult := EvaluateXPathExpression(WideString(XPath), XMLDocument.DocumentElement);

  i := 0;
  NodesCount := XPathResult.AsNodeSet.Count;
  while (i < NodesCount) and (not Result) do
  begin
    Node := TDOMNode(XPathResult.AsNodeSet.Items[i]);
    if Assigned(Node) then
    begin
{$IFDEF DEBUG}
      WriteLn(Node.NodeName, ': ', Node.NodeValue);
{$ENDIF}
      Result := (AnsiString(Node.NodeValue) = NodeValue);
    end;
    Inc(i);
  end;

  // Destroying the XPathResult object...
  FreeAndNil(XPathResult);
end;

procedure TCodeBlocksPatcher.ImportNode(
  TargetXMLDocument: TXMLDocument; SourceNode: TDOMNode; Path: string;
  AppendNode: Boolean);
var
  ImportedNode, DestinationNode, WorkingNode: TDOMNode;

begin
{$IFDEF DEBUG}
  WriteLn('Importing: ', SourceNode.NodeName);
{$ENDIF}

  ImportedNode := TargetXMLDocument.ImportNode(SourceNode, True);

  DestinationNode := SelectSingleNode(TargetXMLDocument, Path);

  // Create the Destination SourceNode if not detected.
  if not Assigned(DestinationNode) then
    DestinationNode := ForceNodes(TargetXMLDocument, Path);

  if not AppendNode then
  begin
    // Remove the existing SourceNode if needed.
    WorkingNode := DestinationNode.FindNode(SourceNode.NodeName);
    if Assigned(WorkingNode) then
      DestinationNode.RemoveChild(WorkingNode);
  end;

  // Appending the SourceNode.
  DestinationNode.AppendChild(ImportedNode);
end;

procedure TCodeBlocksPatcher.ImportNode(
  TargetXMLDocument: TXMLDocument; SourceNode: TDOMNode; Path: string);
begin
  ImportNode(TargetXMLDocument, SourceNode, Path, False);
end;

function TCodeBlocksPatcher.SourceKindToFileName(
  const SourceKind: TSourceKind): TFileName;
begin
  Result := EmptyStr;
  case SourceKind of
    skDebugger:
      Result := 'debugger.conf';
    skGlobalVariables:
      Result := 'gcv.conf';
    skTools:
      Result := 'tools.conf';
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
  const
    SEPARATOR_NAME = '---separator---';
    TOOLS_SEPARATOR_FILE = 'tools-separator.dat';
    TOOLS_DESIGN_FILE = 'tools-design.dat';

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
    ToolSeparatorTag := LoadFileToString(GetSourceConfigurationDirectory
      + TOOLS_SEPARATOR_FILE) + sLineBreak;
    Buffer := TStringList.Create;
    ToolsDesign := TStringList.Create;
    try
      ToolsDesign.LoadFromFile(GetSourceConfigurationDirectory + TOOLS_DESIGN_FILE);
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

function TCodeBlocksPatcher.GetConfigurationFileName: TFileName;
const
  IDE_CONFIGURATION_FILE = 'msys\1.0\etc\dreamsdk\ide.conf';
begin
  Result := IncludeTrailingPathDelimiter(HomeDirectory)
    + IDE_CONFIGURATION_FILE;
end;

function TCodeBlocksPatcher.GetRegisteredString(const Key: string): string;
var
  ConfigurationFileName: TFileName;
  IniFile: TIniFile;

begin
  Result := EmptyStr;
  ConfigurationFileName := GetConfigurationFileName;

  if FileExists(ConfigurationFileName) then
  begin
    IniFile := TIniFile.Create(ConfigurationFileName);
    try
      Result := IniFile.ReadString('IDE', Key, EmptyStr);
    finally
      IniFile.Free;
    end;
  end;
end;

function TCodeBlocksPatcher.GetRegisteredCodeBlocksInstallationDirectory: TFileName;
begin
  Result := GetRegisteredString('InstallationPath');
end;

function TCodeBlocksPatcher.GetRegisteredCodeBlocksBackupDirectory: TFileName;
begin
  Result := GetRegisteredString('BackupPath');
end;

function TCodeBlocksPatcher.GetGlobalVariablesActiveSet(
  XMLDocument: TXMLDocument): string;
var
  Node: TDOMNode;

begin
  Result := 'default';
  Node := SelectSingleNode(XMLDocument, '/gcv/ACTIVE/str/text()');
  if Assigned(Node) then
    Result := AnsiString(Node.NodeValue);
end;

function TCodeBlocksPatcher.GetReady: Boolean;
var
  CodeBlocksConfigurationIndex: Integer;

begin
  Result := DirectoryExists(SourceDirectory)
    and DirectoryExists(HomeDirectory)
    and DirectoryExists(CodeBlocksInstallationDirectory);

  for CodeBlocksConfigurationIndex := 0 to CodeBlocksConfigurationFileNames.Count - 1 do
    Result := Result and FileExists(CodeBlocksConfigurationFileNames[CodeBlocksConfigurationIndex]);

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
    SourceFileName := GetSourceFragmentsDirectory + SourceKindToFileName(SourceKind);
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

function TCodeBlocksPatcher.UpdateConfiguration(
  const Operation: TCodeBlocksPatcherOperation): Boolean;
const
  IDE_EXPORT_LIB_INFO_DIR = 'share\CodeBlocks\templates\wizard\dc\libinfo\';

var
  ConfigurationFileName,
  ExportLibraryInformationPath,
  InstallationPath,
  BackupPath: TFileName;
  IniFile: TIniFile;
  Kind: Integer;
  ExportLibraryInformation: Boolean;

begin
  ConfigurationFileName := GetConfigurationFileName;

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
    IniFile.WriteInteger('IDE', 'Kind', Kind);
    IniFile.WriteBool('IDE', 'ExportLibraryInformation', ExportLibraryInformation);
    IniFile.WriteString('IDE', 'ExportLibraryInformationPath', ExportLibraryInformationPath);
    IniFile.WriteString('IDE', 'InstallationPath', InstallationPath);
    IniFile.WriteString('IDE', 'BackupPath', BackupPath);
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

  function ExtractFileName(const ResourceName: string;
    const FileName: TFileName): TFileName;
  begin
    Result := ChangeFileExt(LowerCase(SysUtils.GetTempFileName), '-' + FileName);
    ExtractEmbeddedResourceToFile(ResourceName, Result);
  end;

begin
  // Code::Blocks Backup/Restore
  fCodeBlocksBackupRestoreFileName :=
    ExtractFileName(EMBEDDED_BACKUP_RESTORE, 'codeblocks-backup-restore.cmd');

  // Code::Blocks Splash
  fCodeBlocksSplashFileName :=
    ExtractFileName(EMBEDDED_SPLASH, 'codeblocks-splash.exe');
end;

procedure TCodeBlocksPatcher.CleanEmbeddedFiles;
begin
  KillFile(fCodeBlocksBackupRestoreFileName);
  KillFile(fCodeBlocksSplashFileName);
end;

function TCodeBlocksPatcher.GetSourceConfigurationDirectory: TFileName;
begin
  Result := SourceDirectory + 'config' + DirectorySeparator;
end;

function TCodeBlocksPatcher.GetSourceFragmentsDirectory: TFileName;
begin
  Result := SourceDirectory + 'fragments' + DirectorySeparator;
end;

function TCodeBlocksPatcher.GetSourcePackageDirectory: TFileName;
begin
  Result := SourceDirectory + 'package' + DirectorySeparator;
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
begin
  Result := True; // TODO
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

