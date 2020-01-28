unit XmlTools;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DOM,
  XMLRead,
  XMLWrite,
  XPath;

procedure DeleteNode(SourceNode: TDOMNode; ParentDeepLevel: Integer); overload;
procedure DeleteNode(SourceNode: TDOMNode); overload;
function ForceNodes(XMLDocument: TXMLDocument; Path: string): TDOMNode;
procedure ImportNode(TargetXMLDocument: TXMLDocument; SourceNode: TDOMNode;
  Path: string; AppendNode: Boolean); overload;
procedure ImportNode(TargetXMLDocument: TXMLDocument; SourceNode: TDOMNode;
  Path: string); overload;
function NodeExists(XMLDocument: TXMLDocument;
  XPath, NodeValue: string): Boolean; overload;
function NodeExists(XMLDocument: TXMLDocument; XPath: string): Boolean; overload;
function SelectSingleNode(XPath: string;
  XMLDocument: TXMLDocument): TDOMNode; overload;
function SelectSingleNode(XPath: string;
  ParentNode: TDOMNode): TDOMNode; overload;
function SelectSingleNode(XPath, NodeValue: string;
  XMLDocument: TXMLDocument): TDOMNode; overload;
function SelectSingleNode(XPath, NodeValue: string;
  ParentNode: TDOMNode): TDOMNode; overload;
procedure ReformatXML(const FileName: TFileName);
function SanitizeXPath(const DomRootPath, XPathExpression: string): string;

implementation

uses
  SysTools;

const
  DOM_ROOT_PATH = '/CodeBlocksConfig';

function SanitizeXPath(const DomRootPath, XPathExpression: string): string;
var
  WorkXPathExpression: string;

begin
  Result := XPathExpression;
  WorkXPathExpression := XPathExpression;
  if not StartsWith('/', WorkXPathExpression) then
    WorkXPathExpression := '/' + WorkXPathExpression;
  if not StartsWith(DomRootPath, WorkXPathExpression) then
    Result := DomRootPath + WorkXPathExpression;
end;

procedure DeleteNode(SourceNode: TDOMNode; ParentDeepLevel: Integer);
var
  RootNode,
  Node: TDOMNode;
  i: Integer;

begin
{$IFDEF DEBUG}
  WriteLn(SourceNode.NodeName);
{$ENDIF}
  for i := 0 to ParentDeepLevel - 1 do
    if Assigned(SourceNode) then
    begin
      SourceNode := SourceNode.ParentNode;
{$IFDEF DEBUG}
      WriteLn(SourceNode.NodeName);
{$ENDIF}
    end;

  RootNode := SourceNode.FirstChild;
  while Assigned(RootNode) do
  begin
    Node := RootNode;
{$IFDEF DEBUG}
    WriteLn('  > ', Node.NodeName);
{$ENDIF}
    RootNode := RootNode.NextSibling;
    Node := SourceNode.RemoveChild(Node);
    FreeAndNil(Node);
  end;

  SourceNode.ParentNode.RemoveChild(SourceNode);
end;

procedure DeleteNode(SourceNode: TDOMNode);
begin
  DeleteNode(SourceNode, 0);
end;

procedure ReformatXML(const FileName: TFileName);
var
  TargetFileStream: TFileStream;
  TargetXML: TXMLDocument;

begin
  TargetFileStream := TFileStream.Create(FileName, fmOpenReadWrite);
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

function SelectSingleNode(XPath: string; ParentNode: TDOMNode): TDOMNode;
var
  XPathResult: TXPathVariable;

begin
  // Find DestinationNode
  Result := nil;

  XPathResult := EvaluateXPathExpression(WideString(XPath), ParentNode);
  if XPathResult.AsNodeSet.Count > 0 then
    Result := TDOMNode(XPathResult.AsNodeSet.Items[0]);

  // Destroying the XPathResult object...
  FreeAndNil(XPathResult);
end;

function SelectSingleNode(XPath: string; XMLDocument: TXMLDocument): TDOMNode;
begin
  Result := SelectSingleNode(SanitizeXPath(DOM_ROOT_PATH, XPath),
    XMLDocument.DocumentElement);
end;

function SelectSingleNode(XPath, NodeValue: string;
  ParentNode: TDOMNode): TDOMNode;
var
  XPathResult: TXPathVariable;
  i, NodesCount: Integer;
  Node: TDOMNode;
  NodeFound: Boolean;

begin
  Result := nil;
  NodeFound := False;

  XPathResult := EvaluateXPathExpression(WideString(XPath), ParentNode);

  i := 0;
  NodesCount := XPathResult.AsNodeSet.Count;
  while (i < NodesCount) and (not NodeFound) do
  begin
    Node := TDOMNode(XPathResult.AsNodeSet.Items[i]);
    if Assigned(Node) then
    begin
{$IFDEF DEBUG}
      WriteLn(Node.NodeName, ': ', Node.NodeValue);
{$ENDIF}
      NodeFound := (AnsiString(Node.NodeValue) = NodeValue);
      if NodeFound then
        Result := Node;
    end;
    Inc(i);
  end;

  // Destroying the XPathResult object...
  FreeAndNil(XPathResult);
end;

function SelectSingleNode(XPath, NodeValue: string;
  XMLDocument: TXMLDocument): TDOMNode;
begin
  Result := SelectSingleNode(SanitizeXPath(DOM_ROOT_PATH, XPath), NodeValue,
    XMLDocument.DocumentElement);
end;

function NodeExists(XMLDocument: TXMLDocument; XPath, NodeValue: string): Boolean;
begin
  Result := Assigned(SelectSingleNode(XPath, NodeValue, XMLDocument));
end;

function NodeExists(XMLDocument: TXMLDocument; XPath: string): Boolean;
begin
  Result := Assigned(SelectSingleNode(XPath, XMLDocument));
end;

function ForceNodes(XMLDocument: TXMLDocument; Path: string): TDOMNode;
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

    Node := XMLDocument.DocumentElement;
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
          NewNode := XMLDocument.CreateElement(WideString(NodeName));
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

procedure ImportNode(TargetXMLDocument: TXMLDocument; SourceNode: TDOMNode; Path: string;
  AppendNode: Boolean);
var
  ImportedNode, DestinationNode, WorkingNode: TDOMNode;

begin
{$IFDEF DEBUG}
  WriteLn('Importing: ', SourceNode.NodeName);
{$ENDIF}

  ImportedNode := TargetXMLDocument.ImportNode(SourceNode, True);

  DestinationNode := SelectSingleNode(Path, TargetXMLDocument);

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

procedure ImportNode(TargetXMLDocument: TXMLDocument; SourceNode: TDOMNode; Path: string);
begin
  ImportNode(TargetXMLDocument, SourceNode, Path, False);
end;

end.

