unit CBPatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, XPath;

type
  TSourceKind = (skDebugger, skGlobalVariables, skTools);

  { TCodeBlocksPatcher }
  TCodeBlocksPatcher = class(TObject)
  private
    fCodeBlocksInstallationDirectory: TFileName;
    fDreamSDKHomeDirectory: TFileName;
    fSourceDirectory: TFileName;
    fCodeBlocksConfigurationFileName: TFileName;
    procedure FixTools;
    procedure FixDebugger;
    function GetGlobalVariablesActiveSet(XMLDocument: TXMLDocument): string;
    function GetReady: Boolean;
    procedure InjectDebugger(SourceXML, TargetXML: TXMLDocument);
    procedure InjectGlobalVariables(SourceXML, TargetXML: TXMLDocument);
    procedure InjectTools(SourceXML, TargetXML: TXMLDocument);
    procedure SetCodeBlocksInstallationDirectory(AValue: TFileName);
    procedure SetDreamSDKHomeDirectory(AValue: TFileName);
    procedure SetSourceDirectory(AValue: TFileName);
    procedure SetCodeBlocksConfigurationFileName(AValue: TFileName);
    procedure FixSection(const SectionName, ItemName, ItemFormat: string;
      const StartIndex: Integer);
    procedure ReformatXML;
    procedure HandleBaseDirectory;
    procedure Merge(SourceKind: TSourceKind);
  protected
    function SelectSingleNode(XMLDoc: TXMLDocument; Path: string): TDOMNode;
    function NodeExists(XMLDocument: TXMLDocument; XPath: string; NodeValue: string): Boolean;
    function ForceNodes(XMLDoc: TXMLDocument; Path: string): TDOMNode;
    procedure ImportNode(TargetXMLDocument: TXMLDocument;
      SourceNode: TDOMNode; Path: string; AppendNode: Boolean); overload;
    procedure ImportNode(TargetXMLDocument: TXMLDocument;
      SourceNode: TDOMNode; Path: string); overload;
    function SourceKindToFileName(const SourceKind: TSourceKind): TFileName;
{$IFDEF DEBUG}
    procedure IterateNodes(Node: TDOMNode);
{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    procedure InstallPatch;
    procedure ApplyConfiguration;

    property CodeBlocksConfigurationFileName: TFileName
      read fCodeBlocksConfigurationFileName
      write SetCodeBlocksConfigurationFileName;
    property CodeBlocksInstallationDirectory: TFileName
      read fCodeBlocksInstallationDirectory
      write SetCodeBlocksInstallationDirectory;
    property Ready: Boolean read GetReady;
    property SoftwareDevelopmentKitHomeDirectory: TFileName
      read fDreamSDKHomeDirectory
      write SetDreamSDKHomeDirectory;
    property SourceDirectory: TFileName
      read fSourceDirectory
      write SetSourceDirectory;
  end;

implementation

uses
  SysTools;

const
  DREAMSDK_HOME_VARIABLE = '{app}';
  DOM_ROOT_PATH = '/CodeBlocksConfig';

function SanitizeXPath(XPathExpression: string): string;
begin
  Result := XPathExpression;
  if not StartsWith('/', XPathExpression) then
    XPathExpression := '/' + XPathExpression;
  if not StartsWith(DOM_ROOT_PATH, XPathExpression) then
    Result := DOM_ROOT_PATH + XPathExpression;
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

constructor TCodeBlocksPatcher.Create;
begin

end;

destructor TCodeBlocksPatcher.Destroy;
begin
  inherited Destroy;
end;

procedure TCodeBlocksPatcher.InstallPatch;
const
  PACKAGE_FILE = 'codeblocks-17.12-dreamsdk-addon-bin.zip';
  COMPILER_FILE = 'share\CodeBlocks\compilers\compiler_dc-gcc.xml';
  OPTIONS_FILE = 'share\CodeBlocks\compilers\options_dc-gcc.xml';

begin
  UncompressZipFile(SourceDirectory + PACKAGE_FILE,
    CodeBlocksInstallationDirectory);
  PatchTextFile(CodeBlocksInstallationDirectory + COMPILER_FILE, DREAMSDK_HOME_VARIABLE,
    SoftwareDevelopmentKitHomeDirectory);
  PatchTextFile(CodeBlocksInstallationDirectory + OPTIONS_FILE, DREAMSDK_HOME_VARIABLE,
    SoftwareDevelopmentKitHomeDirectory);
end;

{$ENDIF}

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

procedure TCodeBlocksPatcher.SetCodeBlocksInstallationDirectory(
  AValue: TFileName);
begin
  fCodeBlocksInstallationDirectory := IncludeTrailingPathDelimiter(ExpandFileName(AValue));
end;

procedure TCodeBlocksPatcher.SetDreamSDKHomeDirectory(AValue: TFileName);
begin
  if fDreamSDKHomeDirectory <> AValue then
    fDreamSDKHomeDirectory := ExcludeTrailingPathDelimiter(AValue);
end;

procedure TCodeBlocksPatcher.SetSourceDirectory(AValue: TFileName);
begin
  if fSourceDirectory <> AValue then
    fSourceDirectory := IncludeTrailingPathDelimiter(ExpandFileName(AValue));
end;

procedure TCodeBlocksPatcher.SetCodeBlocksConfigurationFileName(AValue: TFileName);
begin
  if fCodeBlocksConfigurationFileName <> AValue then
    fCodeBlocksConfigurationFileName := ExpandFileName(AValue);
end;

procedure TCodeBlocksPatcher.FixSection(const SectionName,
  ItemName, ItemFormat: string; const StartIndex: Integer);
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

procedure TCodeBlocksPatcher.ReformatXML;
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

procedure TCodeBlocksPatcher.HandleBaseDirectory;
var
  Buffer: TStringList;

begin
  Buffer := TStringList.Create;
  try
    Buffer.LoadFromFile(CodeBlocksConfigurationFileName);
    Buffer.Text := StringReplace(Buffer.Text, DREAMSDK_HOME_VARIABLE,
      SoftwareDevelopmentKitHomeDirectory, [rfReplaceAll]);
    Buffer.SaveToFile(CodeBlocksConfigurationFileName);
  finally
    Buffer.Free;
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

procedure TCodeBlocksPatcher.FixTools;

  procedure FixToolsSeparators;
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
    ToolSeparatorTag := LoadFileToString(SourceDirectory + TOOLS_SEPARATOR_FILE)
      + sLineBreak;
    Buffer := TStringList.Create;
    ToolsDesign := TStringList.Create;
    try
      ToolsDesign.LoadFromFile(SourceDirectory + TOOLS_DESIGN_FILE);
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
  FixToolsSeparators;
  FixSection('tools', 'tool', '%0.2d', 0);
end;

procedure TCodeBlocksPatcher.FixDebugger;
begin
  FixSection('gdb_debugger', 'conf', '%d', 1);
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
begin
  Result := DirectoryExists(SourceDirectory)
    and DirectoryExists(SoftwareDevelopmentKitHomeDirectory)
    and FileExists(CodeBlocksConfigurationFileName)
    and DirectoryExists(CodeBlocksInstallationDirectory);
end;

procedure TCodeBlocksPatcher.Merge(SourceKind: TSourceKind);
var
  TargetFileStream, SourceFileStream: TFileStream;
  TargetXML, SourceXML: TXMLDocument;
  SourceFileName: TFileName;

begin
  // Inject the DreamSDK fragment into the CodeBlocksConfig XML file.
  TargetFileStream := TFileStream.Create(CodeBlocksConfigurationFileName, fmOpenReadWrite);
  SourceFileName := SourceDirectory + SourceKindToFileName(SourceKind);
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
      FixDebugger;
    skTools:
      FixTools;
  end;

  // Handle the DREAMSDK_HOME_VARIABLE variable representing the DreamSDK home base directory.
  HandleBaseDirectory;

  // Reformat the indentation of the XML.
  ReformatXML;
end;

procedure TCodeBlocksPatcher.ApplyConfiguration;
begin
  Merge(skDebugger);
  Merge(skGlobalVariables);
  Merge(skTools);
end;


end.

