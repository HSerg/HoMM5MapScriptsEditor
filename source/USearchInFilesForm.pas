// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit USearchInFilesForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DKLang, ActnList, StdCtrls, ExtCtrls, ComCtrls, VirtualTrees,
  USearchReplaceOptions, UMapManager, SynEditRegexSearch, SynEditMiscClasses,
  SynEditSearch;

type
  TSearchInFilesForm = class(TForm)
    ActionList1: TActionList;
    acFind: TAction;
    DKLanguageController1: TDKLanguageController;
    StatusBar1: TStatusBar;
    pnlSettings: TPanel;
    Label1: TLabel;
    cboSearchText: TComboBox;
    gbSearchOptions: TGroupBox;
    chkSearchCaseSensitive: TCheckBox;
    chkSearchWholeWords: TCheckBox;
    chkRegularExpression: TCheckBox;
    btnFind: TButton;
    gbFilesFilter: TGroupBox;
    btnClose: TButton;
    synSearch: TSynEditSearch;
    synSearchRegex: TSynEditRegexSearch;
    GridPanel1: TGridPanel;
    chkLUA: TCheckBox;
    chkXDB: TCheckBox;
    chkTXT: TCheckBox;
    chkOthers: TCheckBox;
    acClose: TAction;
    vstData: TVirtualDrawTree;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acFindExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vstDataDblClick(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure vstDataDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure vstDataGetNodeWidth(Sender: TBaseVirtualTree; HintCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; var NodeWidth: Integer);

  private
    type
      RNodeData = record
        FileName: string;
        Text: string;
        Line: integer;
        Offset: integer;
        Size: integer;
      end;
      PNodeData = ^RNodeData;
    function GetNodeData(ANode: PVirtualNode): PNodeData;

  private
    FH5MManager: TMapManager;

    procedure CustomSearch(ASubString: string; AFileName: string; AText: TStrings; ASearchEngine: TSynEditSearchCustom);

  private
    FSearchInFilesOptions: TSearchInFilesOptions;

    procedure DisableUserControls();
    procedure EnableUserControls();

  public
    SearchText: String;
    function IsCaseSensitive: Boolean;
    function IsWholeWordOnly: Boolean;
    function IsResgularExpression: Boolean;

    procedure LoadFromOptions(const ASearchOptions: TSearchInFilesOptions);
    procedure SaveToOptions(var ASearchOptions: TSearchInFilesOptions);

    procedure vstDataGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: UnicodeString);

    property H5MManager: TMapManager read FH5MManager write FH5MManager;
  end;

var
  SearchInFilesForm: TSearchInFilesForm;

implementation

{$R *.dfm}

uses
  StrUtils, SynEditTypes, SynEdit, UAppController, UXMLFrame,
  SynCompletionProposal, Math, UDKConsts;

procedure TSearchInFilesForm.acCloseExecute(Sender: TObject);
begin
  SearchText := '';
  ModalResult := mrOk;
end;

procedure TSearchInFilesForm.acFindExecute(Sender: TObject);
var
  i: integer;
  xBodyText: string;
  xSearchEngine: TSynEditSearchCustom;
  xLines: TStrings;
  xFileName: string;
begin
  vstData.Clear;

  SearchText := cboSearchText.Text;
  if SearchText = '' then
    exit;

  DisableUserControls();
  Application.ProcessMessages;

  SaveToOptions(FSearchInFilesOptions);

  if not FSearchInFilesOptions.RegularExpression then
    xSearchEngine := synSearch
  else
    xSearchEngine := synSearchRegex;
  xSearchEngine.Options := FSearchInFilesOptions.BuildSynSearchOptions();
  xSearchEngine.Pattern := SearchText;

  xLines := TStringList.Create;
  for i := 0 to FH5MManager.ItemsCount - 1 do
    begin
      try
        xFileName := FH5MManager.ItemFileName[i];

        if EndsText('.lua', xFileName) then
          begin
            if not FSearchInFilesOptions.ScanLUAFiles then
              continue;
          end
        else
        if EndsText('.txt', xFileName) then
          begin
            if not FSearchInFilesOptions.ScanTXTFiles then
              continue;
          end
        else
        if EndsText('.xdb', xFileName) then
          begin
            if not FSearchInFilesOptions.ScanXDBFiles then
              continue
          end
        else
        if not FSearchInFilesOptions.ScanOthersFiles then
          continue;

        StatusBar1.SimpleText := xFileName;
        Application.ProcessMessages;

        xBodyText := FH5MManager.GetFileAsUnicodeText(xFileName);
        xLines.Text := xBodyText;
        CustomSearch(SearchText, xFileName, xLines, xSearchEngine);
      except
        // nothing
      end;
      Application.ProcessMessages;
    end;
  xLines.Free;

  if cboSearchText.Items.IndexOf(SearchText) = -1 then
    cboSearchText.Items.Insert(0, SearchText);
  StatusBar1.SimpleText := '';

  EnableUserControls();
end;

procedure TSearchInFilesForm.CustomSearch(ASubString: string; AFileName: string;
  AText: TStrings; ASearchEngine: TSynEditSearchCustom);
const
  SBLOCK_FMT = '%s %u: %s\style{+B}%s\style{-B}%s';
var
  nInLine: integer;
  i, j: Integer;
  fileNode: PVirtualNode;
  dataNode: PVirtualNode;
  fileNodeData: PNodeData;
  dataNodeData: PNodeData;
  text_before, text_after, text: string;
  srcLine: string;
  pos: integer;
begin
  fileNode := nil;

  for i := 0 to AText.Count - 1 do
    begin
      nInLine := ASearchEngine.FindAll(AText[i]);
      for j := 0 to nInLine - 1 do
        begin
          if fileNode = nil then
            begin
              fileNode := vstData.AddChild(nil);
              fileNodeData := GetNodeData(fileNode);
              fileNodeData.FileName := AFileName;
            end;

          srcLine := AText[i];
          pos := Max(1, ASearchEngine.Results[j]-1024);
          text_before := Copy(srcLine, pos, ASearchEngine.Results[j]-pos);
          text := Copy(srcLine, ASearchEngine.Results[j], ASearchEngine.Lengths[j]);
          text_after := Copy(srcLine, ASearchEngine.Results[j]+ASearchEngine.Lengths[j], 1024);

          dataNode := vstData.AddChild(fileNode);
          dataNodeData := GetNodeData(dataNode);
          dataNodeData.FileName := AFileName;
          dataNodeData.Text := Format(SBLOCK_FMT,
            [DKConsts.SSEARCH_IN_FILES_LINE, i+1, text_before, text, text_after]);
          dataNodeData.Line := i;
          dataNodeData.Offset := ASearchEngine.Results[j];
          dataNodeData.Size := ASearchEngine.Lengths[j];
        end;
    end;

  if fileNode <> nil then
    vstData.Expanded[fileNode] := true;
end;

procedure TSearchInFilesForm.DisableUserControls;
begin
  acFind.Enabled := false;
  acClose.Enabled := false;
  gbSearchOptions.Enabled := false;
  gbFilesFilter.Enabled := false;
  cboSearchText.Enabled := false;
end;

procedure TSearchInFilesForm.EnableUserControls;
begin
  cboSearchText.Enabled := true;
  acFind.Enabled := true;
  acClose.Enabled := true;
  gbSearchOptions.Enabled := true;
  gbFilesFilter.Enabled := true;
end;

procedure TSearchInFilesForm.FormCreate(Sender: TObject);
begin
  vstData.NodeDataSize := sizeof(RNodeData);

  FSearchInFilesOptions := TSearchInFilesOptions.Create;

  SearchText := '';
end;

procedure TSearchInFilesForm.FormDestroy(Sender: TObject);
begin
  FSearchInFilesOptions.Free;
end;

procedure TSearchInFilesForm.FormShow(Sender: TObject);
begin
  cboSearchText.SetFocus;
  vstData.Header.Columns[0].Text := DKConsts.SSEARCH_TREE_HEADER;
end;

function TSearchInFilesForm.GetNodeData(ANode: PVirtualNode): PNodeData;
begin
  Result := vstData.GetNodeData(ANode);
end;

function TSearchInFilesForm.IsCaseSensitive: Boolean;
begin
  Result := chkSearchCaseSensitive.Checked;
end;

function TSearchInFilesForm.IsWholeWordOnly: Boolean;
begin
  Result := chkSearchWholeWords.Checked;
end;

function TSearchInFilesForm.IsResgularExpression: Boolean;
begin
  Result := chkRegularExpression.Checked;
end;

procedure TSearchInFilesForm.LoadFromOptions(
  const ASearchOptions: TSearchInFilesOptions);
begin
  chkRegularExpression.Checked := ASearchOptions.RegularExpression;
  chkSearchCaseSensitive.Checked := ASearchOptions.CaseSensitive;
  chkSearchWholeWords.Checked := ASearchOptions.WholeWordsOnly;

  chkLUA.Checked := ASearchOptions.ScanLUAFiles;
  chkTXT.Checked := ASearchOptions.ScanTXTFiles;
  chkXDB.Checked := ASearchOptions.ScanXDBFiles;
  chkOthers.Checked := ASearchOptions.ScanOthersFiles;

  cboSearchText.Items.Clear;
  cboSearchText.Items.Add(ASearchOptions.SearchString);
  if ASearchOptions.SearchStringHistory.Count > 0 then
    cboSearchText.Items.AddStrings(ASearchOptions.SearchStringHistory);
end;

procedure TSearchInFilesForm.SaveToOptions(
  var ASearchOptions: TSearchInFilesOptions);
begin
  ASearchOptions.RegularExpression := IsResgularExpression;
  ASearchOptions.CaseSensitive := IsCaseSensitive;
  ASearchOptions.WholeWordsOnly := IsWholeWordOnly;

  ASearchOptions.ScanLUAFiles := chkLUA.Checked;
  ASearchOptions.ScanTXTFiles := chkTXT.Checked;
  ASearchOptions.ScanXDBFiles := chkXDB.Checked;
  ASearchOptions.ScanOthersFiles := chkOthers.Checked;

  ASearchOptions.SearchString := SearchText;
  if ASearchOptions.SearchStringHistory.IndexOf(ASearchOptions.SearchString) = -1 then
    ASearchOptions.SearchStringHistory.Insert(0, ASearchOptions.SearchString);
end;

procedure TSearchInFilesForm.vstDataDrawNode(Sender: TBaseVirtualTree;
  const PaintInfo: TVTPaintInfo);

procedure InitializeTextProperties(var APaintInfo: TVTPaintInfo);
begin
  APaintInfo.Canvas.Font := Sender.Font;
  if poDrawSelection in APaintInfo.PaintOptions then
    if (APaintInfo.Column = Sender.FocusedColumn) then
      if vsSelected in APaintInfo.Node.States then
        if Sender.Focused then
          APaintInfo.Canvas.Font.Color := clHighlightText;
end;

var
  S: UnicodeString;
  DrawFormat: Cardinal;
  CellRect: TRect;
  xPaintInfo: TVTPaintInfo;
begin
  xPaintInfo := PaintInfo;
  InitializeTextProperties(xPaintInfo);
  vstDataGetText(Sender, xPaintInfo.Node, xPaintInfo.Column, ttNormal, S);
  DrawFormat := DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE or DT_LEFT;
  CellRect := xPaintInfo.ContentRect;
  InflateRect(CellRect, vstData.TextMargin, 0);
  CellRect.Left := CellRect.Left + vstData.TextMargin;
  CellRect.Left := CellRect.Left + 3;
  CellRect.Top := CellRect.Top + 2;
  if xPaintInfo.Canvas.TextFlags and ETO_OPAQUE = 0 then
    SetBkMode(xPaintInfo.Canvas.Handle, TRANSPARENT)
  else
    SetBkMode(xPaintInfo.Canvas.Handle, OPAQUE);

//  Windows.DrawTextW(xPaintInfo.Canvas.Handle, PWideChar(S), Length(S),
//    CellRect, DrawFormat);

  FormattedTextOut(xPaintInfo.Canvas, CellRect, S, false, nil, nil);
end;

procedure TSearchInFilesForm.vstDataDblClick(Sender: TObject);
var
  node: PVirtualNode;
  nodeData: PNodeData;
  xCursor, xBefore, xEnd: TBufferCoord;
  xEditor: TSynEdit;
  xEditorIndex: Integer;
  xEditorFrame: TFrame;
begin
  node := vstData.FocusedNode;
  nodeData := GetNodeData(node);
  if nodeData = nil then
    exit;

  AppController.OpenFileEvent(nodeData.FileName, nodeData.FileName);

  xEditorIndex := AppController.ActiveEditor;
  if xEditorIndex <> -1 then
    begin
      xEditorFrame := AppController.OpenEditors[xEditorIndex].Editor.getFrame;
      if xEditorFrame is TXMLFrame then
        TXMLFrame(xEditorFrame).JvPageList1.ActivePageIndex := 1;
     end;

  if edoSynEditCompatible in AppController.GetActivePageEditorOptions() then
    begin
      xBefore.Line := nodeData.Line+1;
      xBefore.Char := nodeData.Offset;
      xEnd.Line := nodeData.Line+1;
      xEnd.Char := nodeData.Offset+nodeData.Size;
      xCursor := xBefore;

      xEditor := AppController.GetActivePageEditor();
      xEditor.ActiveSelectionMode := smNormal;
      xEditor.SetCaretAndSelection(xCursor, xBefore, xEnd);
      xEditor.EnsureCursorPosVisibleEx(True);
    end;
end;

procedure TSearchInFilesForm.vstDataGetNodeWidth(Sender: TBaseVirtualTree;
  HintCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  var NodeWidth: Integer);
var
  S: UnicodeString;
  xVTree: TVirtualDrawTree;
  xMargin: Integer;
begin
  vstDataGetText(Sender, Node, Column, ttNormal, S);
  xVTree := Sender as TVirtualDrawTree;
  xMargin := xVTree.TextMargin;
  NodeWidth := FormattedTextWidth(xVTree.Canvas, S, nil, nil) + 2 * xMargin;
end;

procedure TSearchInFilesForm.vstDataGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: UnicodeString);
const
  SFILE_FMT = '\style{+B} %s \style{-B}';
var
  nodeData: PNodeData;
begin
  nodeData := GetNodeData(Node);
  if nodeData = nil then
    exit;

  if Node.Parent = vstData.RootNode then
    CellText := Format(SFILE_FMT, [nodeData.FileName])
  else
    CellText := nodeData.Text;
end;

end.
