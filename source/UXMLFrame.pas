// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UXMLFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, StdCtrls, ImgList, XMLIntf, XMLDoc, UCommonDM,
  UCommonTabbedEditor, UCommonTabbedEditorFrame, JvTabBar, JvPageList,
  JvExControls, ExtCtrls, SynEdit, SynEditHighlighter, SynHighlighterXML,
  SynEditKeyCmds, JvSpeedButton, Menus, DKLang;

type
  RNodeData = record
    NodeType: TNodeType;
    Text, Value: string;
  end;
  PNodeData = ^RNodeData;

  TFilterOption = (foElements, foValues, foElementsAndValues, foXPath);

  TXMLFrame = class(TCommonTabbedEditorFrame)
    JvTabBar1: TJvTabBar;
    JvPageList1: TJvPageList;
    jvstdpgXMLTree: TJvStandardPage;
    jvstdpgXMLText: TJvStandardPage;
    GridPanel1: TGridPanel;
    Label1: TLabel;
    edtFilter: TEdit;
    vstData: TVirtualStringTree;
    synedtXMLText: TSynEdit;
    SynXMLSyn1: TSynXMLSyn;
    JvModernTabBarPainter1: TJvModernTabBarPainter;
    ppmnuFilterOptions: TPopupMenu;
    N1231: TMenuItem;
    N2341: TMenuItem;
    N3451: TMenuItem;
    N5671: TMenuItem;
    JvSpeedButton1: TJvSpeedButton;
    DKLanguageController1: TDKLanguageController;
    
    procedure edtFilterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vstDataGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure vstDataGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vstDataPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure vstDataFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FilterOptionsClick(Sender: TObject);
    procedure vstDataFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure FrameResize(Sender: TObject);

  private
    FFileName: string;
    FXMLDocument: IXMLDocument;

  private
    FFilterText: string;
    FFilterType: TFilterOption;

    procedure FilterTree(filterText: String);
    procedure ProcessContent;

    procedure expandAllFiltered();

    function CheckNodeData(const nodeData: RNodeData): boolean;

  public
    destructor Destroy(); override;

    function SaveChanges(): boolean; override;

    function getFullFileName(): string; override;
    function getImageIndex(): integer; override;
    function getModified(): boolean; override;

    procedure InitWith(AFileName: string; DataStream: TStream);
  end;

implementation

{$R *.dfm}

uses
  StrUtils;

procedure TXMLFrame.ProcessContent;
begin
  FFilterType := foElements;
  FilterTree('');
end;

function TXMLFrame.SaveChanges: boolean;
begin
  Result := false;
end;

function TXMLFrame.CheckNodeData(const nodeData: RNodeData): boolean;
begin
  Result := false;
  if (FFilterText = '') then
    Result := true
  else
  if (FFilterType = foElements) and
      AnsiContainsText(nodeData.Text, FFilterText) then
    Result := true
  else
  if (FFilterType = foValues) and
      AnsiContainsText(nodeData.Value, FFilterText) then
    Result := true
  else
  if (FFilterType = foElementsAndValues) and
     (AnsiContainsText(nodeData.Text, FFilterText) or
      AnsiContainsText(nodeData.Value, FFilterText)) then
    Result := true
  else
  if FFilterType = foXPath then
    Result := true;
end;

procedure TXMLFrame.FilterOptionsClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := true;
  case (Sender as TMenuItem).Tag of
    1: FFilterType := foElements;
    2: FFilterType := foValues;
    3: FFilterType := foElementsAndValues;
    4: FFilterType := foXPath;
    else FFilterType := foElements;
  end;
  FilterTree(edtFilter.Text);
end;

procedure TXMLFrame.FilterTree(filterText: String);

function addNode(parentNode: PVirtualNode; nodeData: RNodeData; isChecked: boolean = false): PVirtualNode;
var
  xNodeData: PNodeData;
begin
  if isChecked or checkNodeData(nodeData) then
    begin
      Result := vstData.AddChild(parentNode);
      xNodeData := vstData.GetNodeData(Result);
      xNodeData^ := nodeData;
    end
  else
    Result := nil;
end;

function addAttributes(const treeNode: PVirtualNode; const xmlNode: IXMLNode; isChecked: boolean = false): boolean;
var
  i: integer;
  nodeData: RNodeData;
begin
  Result := isChecked;

  for i := 0 to xmlNode.AttributeNodes.Count - 1 do
    begin
      nodeData.Text := xmlNode.AttributeNodes[i].NodeName;
      nodeData.Value := xmlNode.AttributeNodes[i].Text;
      nodeData.NodeType := xmlNode.AttributeNodes[i].NodeType;

      if isChecked then
        addNode(treeNode, nodeData, true)
      else
      if addNode(treeNode, nodeData, false) <> nil then
        Result := true;
    end;
end;

function addNodes(const treeNode: PVirtualNode; const xmlNode: IXMLNode; isChecked: boolean = false): boolean;
var
  node: PVirtualNode;
  nodeData: RNodeData;
  I: Integer;
  childXmlNode: IXMLNode;
  nodePassed: boolean;
  subNodePassed: boolean;
begin
  Result := isChecked;

  for I := 0 to xmlNode.ChildNodes.Count - 1 do
    begin
      childXmlNode := xmlNode.ChildNodes.Get(I);

      if (childXmlNode.NodeType = ntProcessingInstr) then
        begin
          // skip
        end
      else if (childXmlNode.NodeType = ntText) then
        begin
          // skip
        end
      else if childXmlNode.IsTextElement then
        begin
          nodeData.Text := childXmlNode.NodeName;
          nodeData.Value := childXmlNode.Text;
          nodeData.NodeType := childXmlNode.NodeType;

          node := addNode(treeNode, nodeData, true);
          
          nodePassed := isChecked or checkNodeData(nodeData);

          subNodePassed := false;
          if childXmlNode.AttributeNodes.Count > 0 then
            subNodePassed := addAttributes(node, childXmlNode, nodePassed) or subNodePassed;

          nodePassed := nodePassed or subNodePassed;

          if not nodePassed then
            vstData.DeleteNode(node)
          else
            Result := true;
        end
      else
        begin
          nodeData.Text := childXmlNode.NodeName;
          nodeData.Value := '';
          nodeData.NodeType := childXmlNode.NodeType;

          node := addNode(treeNode, nodeData, true);

          nodePassed := isChecked or checkNodeData(nodeData);

          subNodePassed := false;
          if childXmlNode.AttributeNodes.Count > 0 then
            subNodePassed := addAttributes(node, childXmlNode, nodePassed) or subNodePassed;

          if childXmlNode.HasChildNodes then
            subNodePassed := addNodes(node, childXmlNode, nodePassed) or subNodePassed;
            
          nodePassed := nodePassed or subNodePassed;

         if not nodePassed then
            vstData.DeleteNode(node)
          else
            Result := true;
        end;

      childXmlNode := nil;
    end;
end;

begin
  FFilterText := Trim(filterText);

  vstData.BeginUpdate;
  try
    vstData.Clear;
    addNodes(nil, FXMLDocument.DocumentElement, false);
    expandAllFiltered();
  finally
    vstData.EndUpdate;
  end;
end;

procedure TXMLFrame.FrameResize(Sender: TObject);
begin
  GridPanel1.Realign;
end;

function TXMLFrame.getFullFileName: string;
begin
  Result := FFileName;
end;

function TXMLFrame.getImageIndex: integer;
begin
  Result := 3;
end;

function TXMLFrame.getModified: boolean;
begin
  Result := false;
end;

destructor TXMLFrame.Destroy;
begin
  inherited;
end;

procedure TXMLFrame.edtFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    FilterTree(edtFilter.Text);
end;

procedure TXMLFrame.expandAllFiltered;
var
  node: PVirtualNode;
  nodeData: PNodeData;
begin
  node := vstData.GetFirst;
  while node <> nil do
    begin
      nodeData := vstData.GetNodeData(node);

      if (nodeData <> nil) and CheckNodeData(nodeData^) then
        vstData.VisiblePath[node] := true;

      node := vstData.GetNext(node);
    end;
end;

procedure TXMLFrame.InitWith(AFileName: string; DataStream: TStream);
begin
  FFileName := AFileName;

  vstData.NodeDataSize := sizeof(RNodeData);

  FXMLDocument := TXMLDocument.Create(nil);
  FXMLDocument.Options := [doNodeAutoIndent];
  FXMLDocument.ParseOptions := [poValidateOnParse, poPreserveWhiteSpace];
  FXMLDocument.LoadFromStream(DataStream);

  ProcessContent();

//  synedtXMLText.Text := FXMLDocument.XML.Text;
  DataStream.Position := 0;
  synedtXMLText.Lines.LoadFromStream(DataStream);
end;

procedure TXMLFrame.vstDataFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  inherited;
//
end;

procedure TXMLFrame.vstDataFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  nodeData: PNodeData;
begin
  nodeData := vstData.GetNodeData(Node);
  if nodeData<>nil then
    begin
      nodeData.Text := EmptyStr;
      nodeData.Value := EmptyStr;
    end;
end;

procedure TXMLFrame.vstDataGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  nodeData: PNodeData;
begin
  if Kind = ikOverlay then
    exit;
  if (Column = -1) or (Column = 0) then
    begin
      nodeData := vstData.GetNodeData(Node);
      if nodeData.NodeType = ntElement then
        ImageIndex := 0
      else
        ImageIndex := 1;
    end;
end;

procedure TXMLFrame.vstDataGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  nodeData: PNodeData;
begin
  nodeData := vstData.GetNodeData(Node);
  if (Column = -1) or (Column = 0) then
    CellText := nodeData.Text
  else
    CellText := nodeData.Value;
end;

procedure TXMLFrame.vstDataPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  nodeData: PNodeData;
begin
  nodeData := vstData.GetNodeData(Node);
  if nodeData = nil then
    exit;
  if (FFilterText<>'') and CheckNodeData(nodeData^) then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
end;

end.
