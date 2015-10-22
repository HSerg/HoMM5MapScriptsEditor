// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit ULuaFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit, SynCompletionProposal, ImgList, ComCtrls, UCommonDM,
  ToolWin, StdCtrls, ActnList, SynEditHighlighter, Contnrs, ExtCtrls,
  VirtualTrees, ULuaScriptFunctionsScaner, UH5MManager, UCommonTabbedEditor,
  UCommonTabbedEditorFrame, Menus, DKLang, JvComponentBase, JvBalloonHint,
  UCommonLibDefs, UMapManager, SynEditKeyCmds, SynEditTextBuffer,
  USelectionLights, ULuaSrcLibFileParser, UExtLuaLibs;

const
  ecUserCommentBlock = ecUserFirst + 10;
  ecUserUnCommentBlock = ecUserFirst + 11;

type
  TScriptLineStatus = (slsError, slsWarning, slsInfo, slsNone);

  TLuaFrame = class(TCommonTabbedEditorFrame)
    synedtScript: TSynEdit;
    synCompletion: TSynCompletionProposal;
    ActionList1: TActionList;
    acCheckSyntax: TAction;
    acScriptReload: TAction;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    synCompletionParams: TSynCompletionProposal;
    acSave: TAction;
    ToolButton3: TToolButton;
    imglGutterGlyphs: TImageList;                                        
    splScript: TSplitter;
    vstMessages: TVirtualStringTree;
    ToolButton2: TToolButton;
    acInsertTXTFileName: TAction;
    pppmnuInsertFileName: TPopupMenu;
    a11: TMenuItem;
    a21: TMenuItem;
    acInsertLUAFileName: TAction;
    acInsertXDBFileName: TAction;
    xdb1: TMenuItem;
    acInsertANYFileName: TAction;
    pnlRightOutline: TPanel;
    vstOutline: TVirtualStringTree;
    vstLibs: TVirtualStringTree;
    splOutline: TSplitter;
    DKLanguageController1: TDKLanguageController;
    JvBalloonHint1: TJvBalloonHint;
    procedure synCompletionExecute(Kind: TSynCompletionType;
      Sender: TObject; var CurrentInput: string; var x, y: Integer;
      var CanExecute: Boolean);
    procedure acCheckSyntaxExecute(Sender: TObject);
    procedure acScriptReloadExecute(Sender: TObject);
    procedure synCompletionParamsExecute(Kind: TSynCompletionType;
      Sender: TObject; var CurrentInput: string; var x, y: Integer;
      var CanExecute: Boolean);
    procedure vstMessagesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vstMessagesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstMessagesDblClick(Sender: TObject);
    procedure synedtScriptPaintTransient(Sender: TObject; Canvas: TCanvas;
      TransientType: TTransientType);
    procedure synedtScriptChange(Sender: TObject);
    procedure vstOutlineGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vstOutlineDblClick(Sender: TObject);
    procedure synCompletionAfterCodeCompletion(Sender: TObject;
      const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
    procedure acSaveExecute(Sender: TObject);
    procedure acInsertTXTFileNameExecute(Sender: TObject);
    procedure acInsertLUAFileNameExecute(Sender: TObject);
    procedure acInsertXDBFileNameExecute(Sender: TObject);
    procedure acInsertANYFileNameExecute(Sender: TObject);
    procedure vstLibsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vstOutlineKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure synedtScriptMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure JvBalloonHint1Close(Sender: TObject);
    procedure JvBalloonHint1DblClick(Sender: TObject);
    procedure synedtScriptKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure synCompletionParamsPaintItem(Sender: TObject; Index: Integer;
      TargetCanvas: TCanvas; ItemRect: TRect; var CustomDraw: Boolean);
    procedure synedtScriptCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
    procedure synedtScriptProcessUserCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
    procedure synedtScriptStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);

  private
    type
      RMessageItem = record
        Status: TScriptLineStatus;
        LineNo: integer;
        Text: string;
      end;

  private
    FMessages: array of RMessageItem;
    procedure AddMessage(Status: TScriptLineStatus; LineNo: integer; Text: string);
    procedure ClearMessages;

    procedure CheckSyntax();

  private
    LuaScriptFunctionsScaner: TLuaScriptFunctionsScaner;
    procedure RefreshOutline();

  private
    SynLuaSyn: TSynCustomHighlighter;

    FScriptName: string;
    FScriptText: string;

    FExtLuaLibs: TExtLuaLibs;

  private
    FLocalLib: TLuaSrcLibFileParser;
    procedure RefreshLocalLib();

  private
    FSelectionLights: TSelectionLights;

  private
    function getParametersCount(const functionName: string): integer;

  private
    procedure CommentLuaBlock(ASynEdit: TSynEdit);
    procedure UnCommentLuaBlock(ASynEdit: TSynEdit);

  private
    type
      TXFuncItemArray = array of TXFuncItem;
    function findFunctionDefinitions(const functionName: string): TXFuncItemArray;

  private
    FH5MManager: TMapManager;

  private
    procedure RefreshLibs();

  private
    procedure ShowTxtHint();

  private
    procedure FillLookUpList(AUsedLuaLibs: TLuaLibs; ALookupList: TStrings);

  public
    destructor Destroy(); override;

    procedure InitWith(AH5MManager: TMapManager; AScriptName: string;
      DataStream: TStream; AExtLuaLibs: TExtLuaLibs);

    procedure SelectAndInsertFileName(AFileMask: string);

    function GetLineStatus(Line: integer): TScriptLineStatus;

    function getFullFileName(): string; override;
    function getImageIndex(): integer; override;
    function getModified(): boolean; override;

    function SaveChanges(): boolean; override;

    property Editor: TSynEdit read synedtScript;

  public
    function getParameterCategories(const functionName: string;
      const parameterIndex: Integer): TStrings;
    function CheckLibIsChecked(AIndex: integer): boolean;

    // поиск "головы" при вызове функции (парсинг скобок и запятых)
    function findFunctionCalling(AUsedLuaLibs: TLuaLibs;
      var TmpLocation: Integer; var Lookup: string): boolean;

  private
    FLastHint: string;
  end;

implementation

{$R *.dfm}

uses
  lua4.checker, lua5.checker, SynHighlighterLua, LuaSyntax, SynEditTypes,
  ULuaPaintPlugin, UXLibFileParser, StrUtils, UOptionsUnit, UConsts,
  UEmbFileChooseDialog, Clipbrd, UAbstractLibParser, UAppController,
  UDKConsts, UBracketsLights, ULuaFrameEditorCompletion,
  ULuaFrameEditorCompletionParams;

procedure TLuaFrame.ClearMessages();
begin
  vstMessages.RootNodeCount := 0;
  SetLength(FMessages, 0);
end;

procedure TLuaFrame.CommentLuaBlock(ASynEdit: TSynEdit);
var
  xEditor: TSynEdit;
  x, SelStart, SelEnd, AddedChars: Integer;
  UndoStart, UndoEnd: TBufferCoord;
  xResult: boolean;
  xEndLine: integer;
begin
  xResult := False;
  xEditor := ASynEdit;

  // Retrieve selection
  SelStart := xEditor.SelStart;
  SelEnd := xEditor.SelEnd;
  xEditor.BeginUndoBlock;
  AddedChars := 0;

  if xEditor.BlockEnd.Char <> 1 then
    xEndLine := xEditor.BlockEnd.Line
  else
    xEndLine := xEditor.BlockEnd.Line - 1;

  for x := xEditor.BlockBegin.Line - 1 to xEndLine - 1 do
    begin
      xResult := True;

      // Commenting line
      xEditor.Lines.Strings[x] := COMMENT_PREFIX + xEditor.Lines.Strings[x];
      xEditor.Modified := True;
      if Assigned(xEditor.OnChange) then
        xEditor.OnChange(xEditor);
      AddedChars := AddedChars + Length(COMMENT_PREFIX);

      // Notify change to synedit's undo list
      UndoStart.Char := 1;
      UndoStart.Line := x+1;
      UndoEnd.Char := 3;
      UndoEnd.Line := x+1;
      xEditor.UndoList.AddChange(crInsert, UndoStart, UndoEnd, '', smNormal);
    end;

  // Reset selection
  if xResult then
    begin
      xEditor.SelStart := SelStart;
      xEditor.SelEnd := SelEnd + AddedChars;
    end;

  xEditor.EndUndoBlock;
end;

destructor TLuaFrame.Destroy;
begin
  FreeAndNil(LuaScriptFunctionsScaner);
  FreeAndNil(FSelectionLights);
  FreeAndNil(FLocalLib);

  inherited;
end;

procedure TLuaFrame.acCheckSyntaxExecute(Sender: TObject);
begin
  CheckSyntax();
  RefreshOutline();
end;

procedure TLuaFrame.acInsertANYFileNameExecute(Sender: TObject);
begin
  SelectAndInsertFileName('');
end;

procedure TLuaFrame.acInsertLUAFileNameExecute(Sender: TObject);
begin
  SelectAndInsertFileName('*.lua');
end;

procedure TLuaFrame.acInsertTXTFileNameExecute(Sender: TObject);
begin
  SelectAndInsertFileName('*.txt');
end;

procedure TLuaFrame.acInsertXDBFileNameExecute(Sender: TObject);
begin
  SelectAndInsertFileName(MASK_XDB);
end;

procedure TLuaFrame.acSaveExecute(Sender: TObject);
var
  xStream: TMemoryStream;
begin
  xStream := TMemoryStream.Create;
  try
    synedtScript.Lines.SaveToStream(xStream);
    FH5MManager.SetFileAsStream(FScriptName, xStream);
    FScriptText := synedtScript.Text;
  finally
    xStream.Free;
  end;
  DoChangeModified;
end;

procedure TLuaFrame.acScriptReloadExecute(Sender: TObject);
begin
  if MessageDlg(DKConsts.SFILE_UNDO_CHANGES, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      synedtScript.Text := FScriptText;
      acCheckSyntax.Execute();
    end;
  DoChangeModified;
end;

procedure TLuaFrame.InitWith(AH5MManager: TMapManager; AScriptName: string;
  DataStream: TStream; AExtLuaLibs: TExtLuaLibs);
begin
  FH5MManager := AH5MManager;

  FScriptName := AScriptName;

  SynLuaSyn := SynHighlighterLua.TSynLuaSyn.Create(Self);
  synedtScript.Highlighter := SynLuaSyn;

  synedtScript.Clear;
  synedtScript.Lines.LoadFromStream(DataStream);

  FScriptText := synedtScript.Text;

  TLuaPaintPlugin.Create(Self);

  LuaScriptFunctionsScaner := TLuaScriptFunctionsScaner.Create;

  FSelectionLights := TSelectionLights.Create;

  FLocalLib := TLuaSrcLibFileParser.Create;
  FExtLuaLibs := AExtLuaLibs;

  RefreshLibs();

  RefreshOutline();

  CheckSyntax();
end;

procedure TLuaFrame.JvBalloonHint1Close(Sender: TObject);
begin
  FLastHint := '';
end;

procedure TLuaFrame.JvBalloonHint1DblClick(Sender: TObject);
begin
  AppController.OpenFileEvent(FH5MManager.GetFileRevFullPath(FLastHint), FLastHint);
  JvBalloonHint1.CancelHint;
end;

procedure TLuaFrame.synCompletionAfterCodeCompletion(Sender: TObject;
  const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
var
  xFunctionName: string;
begin
  if Copy(Value, Length(Value)-1, 2) = '()' then
    begin
      xFunctionName := Copy(Value, 1, Length(Value)-2);
      if getParametersCount(xFunctionName) <> 0 then
        begin
          synedtScript.CaretX := synedtScript.CaretX - 1;
          synCompletionParams.ActivateCompletion();
        end;
    end;
end;

procedure TLuaFrame.synCompletionExecute(Kind: TSynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
var
  luaLibs: TLuaLibs;
  i: Integer;
begin
  FExtLuaLibs.RefreshLibs();
  RefreshLocalLib();

  luaLibs := TLuaLibs.Create;
  try
    for i := 0 to FExtLuaLibs.Count - 1 do
      if CheckLibIsChecked(i) then
        luaLibs.AddLib(FExtLuaLibs.libs[i]);

    LuaFrameEditorCompletionExecute(Self, FLocalLib, luaLibs);
  finally
    luaLibs.Free;
  end;
end;

procedure TLuaFrame.synCompletionParamsExecute(Kind: TSynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
var
  luaLibs: TLuaLibs;
  i: Integer;
begin
  FExtLuaLibs.RefreshLibs();
  RefreshLocalLib();

  luaLibs := TLuaLibs.Create;
  try
    for i := 0 to FExtLuaLibs.Count - 1 do
      if CheckLibIsChecked(i) then
        luaLibs.AddLib(FExtLuaLibs.libs[i]);

      ExtractedSynCompletionParamsExecute(Self, FLocalLib, luaLibs,
        (Sender as TSynCompletionProposal), CanExecute);
  finally
    luaLibs.Free;
  end;
end;

procedure TLuaFrame.synCompletionParamsPaintItem(Sender: TObject;
  Index: Integer; TargetCanvas: TCanvas; ItemRect: TRect;
  var CustomDraw: Boolean);

function FormatParamList(const S: String; CurrentIndex: Integer): string;
var
  i: Integer;
  List: TStrings;
begin
  Result := '';
  List := TStringList.Create;
  try
    List.CommaText := S;
    for i := 0 to List.Count - 1 do
    begin
      if i = CurrentIndex then
        Result := Result + '\style{~B}' + List[i] + '\style{~B}'
      else
        Result := Result + List[i];

      if i < List.Count - 1 then
//        Result := Result + ', ';
        Result := Result + ' ';
    end;
  finally
    List.Free;
  end;
end;

const
  TEXT_HEIGHT_STRING = 'CompletionProposalW';
var
  TmpString: string;
  xForm: TSynBaseCompletionProposalForm;
  xEffectiveItemHeight: integer;
  xFontHeight: integer;
begin
  if not AppIniOptions.LuaEditor.UseLongParamsDesc then
    begin
      CustomDraw := false;
      exit;
    end;

  xForm := Sender as TSynBaseCompletionProposalForm;

  if (xForm.AssignedList.Count < 2) or (xForm.AssignedList[1] <> '') then
    begin
      CustomDraw := false;
      exit;
    end;

  xFontHeight := TargetCanvas.TextHeight(TEXT_HEIGHT_STRING);
  if xForm.ItemHeight > 0 then
    xEffectiveItemHeight := xForm.ItemHeight
  else
    xEffectiveItemHeight := xFontHeight;

  if Index = 0 then
    TmpString := FormatParamList(xForm.AssignedList[Index], xForm.CurrentIndex)
  else
  if (Trunc((Index - 4) / 2) = xForm.CurrentIndex) and (Index mod 2 = 0) then
    TmpString := '\style{~B}' + '  ' + xForm.AssignedList[Index] + '\style{~B}'
  else
    TmpString := '  ' + xForm.AssignedList[Index];

//  ItemRect.Left := xForm.Margin+1;
//  ItemRect.Top := xEffectiveItemHeight*Index+((xEffectiveItemHeight-xFontHeight) div 2)+xForm.Margin;

  FormattedTextOut(TargetCanvas, ItemRect, TmpString, False, nil, xForm.Images);

  CustomDraw := true;
end;

procedure TLuaFrame.synedtScriptChange(Sender: TObject);
begin
  FSelectionLights.TextChanged := true;

  CheckSyntax();
  RefreshOutline();

  DoChangeModified();
end;

procedure TLuaFrame.synedtScriptCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
begin
  if Command = ecPaste then
    synedtScript.InvalidateLines(-1, -1);
end;

procedure TLuaFrame.synedtScriptKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_CONTROL then
    ShowTxtHint();
end;

procedure TLuaFrame.synedtScriptMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if not (ssCtrl in Shift) then
    begin
      JvBalloonHint1.CancelHint();
      exit;
    end;

  ShowTxtHint();
end;

procedure TLuaFrame.synedtScriptPaintTransient(Sender: TObject; Canvas: TCanvas;
  TransientType: TTransientType);
var
  synEditor: TSynEdit;
begin
  synEditor := TSynEdit(Sender);
  FSelectionLights.Paint(synEditor, Canvas, TransientType);
  PaintBracketsLights(synEditor, Canvas, TransientType);
end;

procedure TLuaFrame.synedtScriptProcessUserCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
begin
  if Command = ecUserCommentBlock then
    CommentLuaBlock(Sender as TSynEdit)
  else
  if Command = ecUserUnCommentBlock then
    UnCommentLuaBlock(Sender as TSynEdit);
end;

procedure TLuaFrame.synedtScriptStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if (scModified in Changes) or (scAll in Changes) then
    FSelectionLights.TextChanged := true;

  if (scSelection in Changes) or (scModified in Changes) or (scAll in Changes) then
    Editor.InvalidateLines(-1, -1);
end;

procedure TLuaFrame.UnCommentLuaBlock(ASynEdit: TSynEdit);
var
  xEditor: TSynEdit;
  x, SelStart, SelEnd, RemovedChars: Integer;
  UndoStart, UndoEnd: TBufferCoord;
  xResult: boolean;
  xEndLine: Integer;
begin
  xResult := False;
  xEditor := ASynEdit;
  if xEditor<>nil then
    begin
      // Retrieve selection
      SelStart := xEditor.SelStart;
      SelEnd := xEditor.SelEnd;
      xEditor.BeginUndoBlock;
      RemovedChars := 0;

      if xEditor.BlockEnd.Char <> 1 then
        xEndLine := xEditor.BlockEnd.Line
      else
        xEndLine := xEditor.BlockEnd.Line - 1;

      for x := xEditor.BlockBegin.Line - 1 to xEndLine - 1 do
        begin
          if Copy(xEditor.Lines.Strings[x], 1, Length(COMMENT_PREFIX)) = COMMENT_PREFIX then
          begin
            xResult := True;

            // Uncommenting line
            xEditor.Lines.Strings[x] := Copy(xEditor.Lines.Strings[x], 3, Length(xEditor.Lines.Strings[x]) - Length(COMMENT_PREFIX));
            xEditor.Modified := True;
            if Assigned(xEditor.OnChange) then
              xEditor.OnChange(xEditor);
            RemovedChars := RemovedChars + Length(COMMENT_PREFIX);

            // Notify change to synedit's undo list
            UndoStart.Char := 1;
            UndoStart.Line := x+1;
            UndoEnd.Char := 3;
            UndoEnd.Line := x+1;
            xEditor.UndoList.AddChange(crDelete, UndoStart, UndoEnd, COMMENT_PREFIX, smNormal);
          end;
        end;

      // Reset selection
      if xResult then
        begin
          xEditor.SelStart := SelStart;
          xEditor.SelEnd := SelEnd - RemovedChars;
        end;

      xEditor.EndUndoBlock;
    end;
end;

procedure TLuaFrame.vstLibsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
begin
  if Node.Index < FExtLuaLibs.Count then
    CellText := FExtLuaLibs.Libs[Node.Index].LibName;
end;

procedure TLuaFrame.vstMessagesDblClick(Sender: TObject);
var
  treeNode: PVirtualNode;
begin
  treeNode := vstMessages.GetFirstSelected;
  if (treeNode <> nil) and (FMessages[treeNode.Index].LineNo <> -1) then
    begin
      synedtScript.GotoLineAndCenter(FMessages[treeNode.Index].LineNo);
      synedtScript.SetFocus;
    end;
end;

procedure TLuaFrame.vstMessagesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if Kind = ikOverlay then
    exit;

  if (Node <> nil) then
    begin
      if slsError = FMessages[Node.Index].Status then
        ImageIndex := 4
      else
      if slsWarning = FMessages[Node.Index].Status then
        ImageIndex := 5;
    end;
end;

procedure TLuaFrame.vstMessagesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  if Node<> nil then
    CellText := FMessages[Node.Index].Text;
end;

procedure TLuaFrame.vstOutlineDblClick(Sender: TObject);
var
  treeNode: PVirtualNode;
  xFunction: TXIntFuncItem;
begin
  treeNode := vstOutline.GetFirstSelected;
  if (treeNode <> nil) then
    begin
      xFunction := LuaScriptFunctionsScaner.Functions[treeNode.Index];
      if (xFunction.SrcFileLine <> -1) then
        begin
          synedtScript.GotoLineAndCenter(synedtScript.CharIndexToRowCol(xFunction.SrcFileLine).Line);
          synedtScript.SetFocus;
        end;
    end;
end;

procedure TLuaFrame.vstOutlineGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  xFunction: TXIntFuncItem;
begin
  if Node<> nil then
    begin
      xFunction := LuaScriptFunctionsScaner.Functions[Node.Index];
      CellText := Format('%s(%s)', [xFunction.name, xFunction.GetParamsAsCommaSeparatedString()]);
    end;
end;

procedure TLuaFrame.vstOutlineKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  treeNode: PVirtualNode;
  xFunction: TXIntFuncItem;
begin
  if Key = VK_RETURN then
    begin
      treeNode := vstOutline.GetFirstSelected;
      if (treeNode <> nil) then
        begin
          xFunction := LuaScriptFunctionsScaner.Functions[treeNode.Index];
          if (xFunction.SrcFileLine <> -1) then
            begin
              synedtScript.GotoLineAndCenter(synedtScript.CharIndexToRowCol(xFunction.SrcFileLine).Line);
            end;
        end;
    end;
end;

function TLuaFrame.findFunctionDefinitions(
  const functionName: string): TXFuncItemArray;

  function intProcess(const pLibItem: TXLibRootItem): TXFuncItem;
  var
    xmlFuncItem: TXFuncItem;
  begin
    Result := nil;
    if (pLibItem is TXFuncItem) then
      begin
        xmlFuncItem := pLibItem as TXFuncItem;
        if AnsiSameText(xmlFuncItem.fullName, functionName) then
          Result := xmlFuncItem;
      end;
  end;

var
  i, j: Integer;
  xLib: TAbstractLibParser;
  functionListParser: TLuaScriptFunctionsScaner;
  xFunctionsList: TObjectList;
  xFunctionDef: TXFuncItem;
begin
  xFunctionsList := TObjectList.Create(false);
  try
    // looking for Libs definitions
    for i:= 0 to FExtLuaLibs.Count - 1 do
      if CheckLibIsChecked(i) then
        begin
          xLib := FExtLuaLibs.libs[i];
          for j := 0 to xLib.Count - 1 do
            begin
              xFunctionDef := intProcess(xLib.Items[j]);
              if xFunctionDef <> nil then
                xFunctionsList.Add(xFunctionDef);
            end;
        end;

    // looking for local definitions
    functionListParser := TLuaScriptFunctionsScaner.Create;
    try
      functionListParser.Scan(synedtScript.Text);
      for j := 0 to functionListParser.FunctionsCount - 1 do
        begin
          xFunctionDef := intProcess(functionListParser.Functions[j]);
           if xFunctionDef <> nil then
             xFunctionsList.Add(xFunctionDef);
        end;
    finally
      functionListParser.Free;
    end;

    SetLength(Result, xFunctionsList.Count);
    for i := 0 to xFunctionsList.Count - 1 do
      Result[i] := xFunctionsList[i] as TXFuncItem;
  finally
    xFunctionsList.Free;
  end;
end;

function TLuaFrame.getFullFileName: string;
begin
  Result := FScriptName;
end;

function TLuaFrame.getImageIndex: integer;
begin
  Result := 2;
end;

function TLuaFrame.GetLineStatus(Line: integer): TScriptLineStatus;
var
  i: Integer;
begin
  Result := slsNone;
  for i := 0 to Length(FMessages) - 1 do
    begin
      if FMessages[i].LineNo = Line then
        Result := FMessages[i].Status;
    end;
end;

function TLuaFrame.getParameterCategories(const functionName: string;
  const parameterIndex: Integer): TStrings;

  function intProcess(const pLibItem: TXFuncItem): TStrings;
  begin
    Result := nil;
    if (pLibItem.ParamDefs.Count > 0) and
      (pLibItem.ParamDefs.Count >= parameterIndex) then
      Result := pLibItem.Params[parameterIndex].categoryList;
  end;

var
  i: Integer;
  functions: TXFuncItemArray;
begin
  Result := nil;

  functions := findFunctionDefinitions(functionName);
  for i := 0 to Length(functions) - 1 do
    begin
      Result := intProcess(functions[i]);
      if Result <> nil then
        exit;
    end;
end;

function TLuaFrame.getParametersCount(const functionName: string): integer;
var
  xFD: TXFuncItemArray;
begin
  xFD := findFunctionDefinitions(functionName);
  if Length(xFD) = 0 then
    Result := -1
  else
  if Length(xFD) = 1 then
    Result := xFD[0].ParamDefs.Count
  else
    Result := -1
end;

procedure TLuaFrame.AddMessage(Status: TScriptLineStatus; LineNo: integer; Text: string);
begin
  SetLength(FMessages, Length(FMessages)+1);

  FMessages[Length(FMessages)-1].Status := Status;
  FMessages[Length(FMessages)-1].LineNo := LineNo;
  FMessages[Length(FMessages)-1].Text := Text;

  vstMessages.RootNodeCount := Length(FMessages);
end;

procedure TLuaFrame.RefreshLibs;
var
  node: PVirtualNode;
begin
  vstLibs.BeginUpdate;
  try
    FExtLuaLibs.RefreshLibs(false);
    vstLibs.RootNodeCount := FExtLuaLibs.Count;
    node := vstLibs.GetFirst;
    while node <> nil do
      begin
        node.CheckType := ctCheckBox;
        node.CheckState := csCheckedNormal;
        node := node.NextSibling;
      end;
  finally
    vstLibs.EndUpdate;
    vstLibs.Invalidate;
  end;
end;

procedure TLuaFrame.RefreshLocalLib;
var
  xStream: TStringStream;
begin
  xStream := TStringStream.Create(synedtScript.Text);
  try
    FLocalLib.Parse(xStream, getFullFileName());
  finally
    xStream.Free;
  end;
end;

procedure TLuaFrame.RefreshOutline();
begin
  vstOutline.BeginUpdate;
  try
    LuaScriptFunctionsScaner.Scan(synedtScript.Text);
    vstOutline.RootNodeCount := LuaScriptFunctionsScaner.FunctionsCount;
  finally
    vstOutline.EndUpdate;
    vstOutline.Invalidate;
    if vstOutline.FocusedNode<>nil then
      vstOutline.InvalidateNode(vstOutline.FocusedNode);
  end;
end;

function TLuaFrame.SaveChanges: boolean;
begin
  Result := acSave.Execute();
end;

procedure TLuaFrame.SelectAndInsertFileName(AFileMask: string);
var
  xFileName: string;
begin
  // показывает файл диалога с файлами по фильтру и, если осуществлён выбор, то
  // вставляет имя файла в текущую позицию
  xFileName := ChooseEmbFileDialog(FH5MManager, AFileMask);
  if xFileName <> '' then
    begin
      if AppIniOptions.LuaEditor.UseGetMapDataPath and
         (FH5MManager.MapPath <> '') and
         AnsiStartsText(FH5MManager.MapPath, xFileName) then
        begin
          Delete(xFileName, 1, Length(FH5MManager.MapPath));
          xFileName := 'GetMapDataPath().."' + xFileName + '"';
        end
      else
        begin
          xFileName := '"/' + xFileName + '"';
        end;
      Clipboard.SetTextBuf(PAnsiChar(xFileName));
      synedtScript.PasteFromClipboard;
    end;
  DoChangeModified;
end;

procedure TLuaFrame.ShowTxtHint();
var
  xCoord: TBufferCoord;
  xToken: string;
  xTokenType: integer;
  xStart: integer;
  xAttr: TSynHighlighterAttributes;
  xText: string;
  xPoint: TPoint;
begin
  // всплывающий хинт с текстом файла, если курсор над строкой с *.txt

  if not synedtScript.GetPositionOfMouse(xCoord) then
    begin
      JvBalloonHint1.CancelHint();
      exit;
    end;

  if not synedtScript.GetHighlighterAttriAtRowColEx(xCoord, xToken, xTokenType, xStart, xAttr) then
    begin
      JvBalloonHint1.CancelHint();
      exit;
    end;

  if xTokenType <> 7 then
    begin
      JvBalloonHint1.CancelHint();
      exit;
    end;

  Delete(xToken, Length(xToken), 1);
  xToken := Trim(xToken);
  if not AnsiEndsText(EXT_TXT, xToken) then
    begin
      JvBalloonHint1.CancelHint();
      exit;
    end;

  Delete(xToken, 1, 1);

  if xToken = FLastHint then
    exit;

  JvBalloonHint1.CancelHint();
  if FH5MManager.IsFilePresentRev(xToken) then
    begin
      xText := FH5MManager.GetFileAsUnicodeText(xToken);
      GetCursorPos(xPoint);
      xPoint.X := xPoint.X + 5;
      xPoint.Y := xPoint.Y - 5;
      xPoint := Application.MainForm.ScreenToClient(xPoint);
      JvBalloonHint1.MaxWidth := Trunc(synedtScript.Width * 0.75);
      JvBalloonHint1.ActivateHintPos(Application.MainForm, xPoint, xToken, xText);
      FLastHint := xToken; 
    end;
end;

function TLuaFrame.getModified: boolean;
begin
  getModified := (synedtScript.Text <> FScriptText);
end;

function TLuaFrame.CheckLibIsChecked(AIndex: integer): boolean;
var
  pNode: PVirtualNode;
begin
  Result := false;

  pNode := vstLibs.GetFirst;
  while pNode <> nil do
    begin
      if pNode.Index = AIndex then
        begin
          Result := pNode.CheckState = csCheckedNormal;
          break;
        end;
      pNode := pNode.NextSibling;
    end;
end;

procedure TLuaFrame.CheckSyntax();
var
  code: string;
  lua4checker: TLua4SyntaxChecker;
  lua5checker: TLua5SyntaxChecker;
  errorFound: boolean; 
  errorMsg: string;
  errorLine: integer;
begin
  vstMessages.BeginUpdate;
  try
    code := synedtScript.Text;
    if code <> '' then
      begin
        ClearMessages();

        lua4checker := TLua4SyntaxChecker.Create;
        try
          errorFound := not lua4checker.Check(code, errorMsg, errorLine);
          if errorFound then
            AddMessage(slsError, errorLine, DKConsts.SLUA_ERROR_FMT([errorMsg, errorLine, DateTimeToStr(Now)]));
        finally
          lua4checker.Free;
        end;

        if not errorFound then
          begin
            lua5checker := TLua5SyntaxChecker.Create;
            try
              errorFound := not lua5checker.Check(code, errorMsg, errorLine);
              if errorFound then
                AddMessage(slsWarning, errorLine, DKConsts.SLUA_LUA5_WARN_FMT([errorMsg, errorLine, DateTimeToStr(Now)]));
            finally
              lua5checker.Free;
            end;
          end;

      if not errorFound then
        AddMessage(slsInfo, -1, DKConsts.SLUA_HINT_FMT([DateTimeToStr(Now)]));
    end;
  finally
    vstMessages.EndUpdate;
  end;

  vstMessages.Invalidate;
  synedtScript.InvalidateGutter();
end;

procedure TLuaFrame.FillLookUpList(AUsedLuaLibs: TLuaLibs; ALookupList: TStrings);
var
  i, j: integer;
  xLib: TAbstractLibParser;
  xLibItem: TXLibRootTableItem;
begin
  ALookupList.Clear;

  for i := 0 to AUsedLuaLibs.Count - 1 do
    begin
      xLib := AUsedLuaLibs.libs[i];
      for j := 0 to xLib.Count - 1 do
        begin
          xLibItem := xLib.Items[j];
          if (xLibItem is TXFuncItem) then
            ALookupList.Add(xLibItem.fullName);
        end;
    end;

  for j := 0 to FLocalLib.Count - 1 do
    begin
      xLibItem := FLocalLib.Items[j];
      if (xLibItem is TXFuncItem) then
        ALookupList.Add(xLibItem.fullName);
    end;
end;

function TLuaFrame.findFunctionCalling(AUsedLuaLibs: TLuaLibs;
  var TmpLocation: Integer; var Lookup: string): boolean;
var
  locline: String;
  TmpX, savepos, StartX, ParenCounter: Integer;
  FoundMatch: Boolean;
  i: Integer;
  lookupList: TStrings;
begin
  lookupList := TStringList.Create;
  try
    FillLookUpList(AUsedLuaLibs, lookupList);

    locLine := '';
    for i := synedtScript.CaretY - 2 downto 0 do
      locLine := synedtScript.Lines[i] +  ' ' + locLine;

    //go back from the cursor and find the first open paren
    TmpX := synedtScript.CaretX;
    if TmpX > length(locLine) then
      TmpX := length(locLine)
    else
      dec(TmpX);

    TmpX := TmpX + Length(locLine);
    locLine := locLine + synedtScript.LineText;
    
    FoundMatch := False;
    TmpLocation := 0;
    while (TmpX > 0) and not(FoundMatch) do
    begin
      if LocLine[TmpX] = ',' then
        begin
          inc(TmpLocation);
          dec(TmpX);
        end
      else if LocLine[TmpX] = ')' then
        begin
          //We found a close, go till it's opening paren
          ParenCounter := 1;
          dec(TmpX);
          while (TmpX > 0) and (ParenCounter > 0) do
            begin
              if LocLine[TmpX] = ')' then
                inc(ParenCounter)
              else if LocLine[TmpX] = '(' then
                dec(ParenCounter);
              dec(TmpX);
            end;
          if TmpX > 0 then dec(TmpX);  //eat the open paren
        end
      else if locLine[TmpX] = '(' then
        begin
          //we have a valid open paren, lets see what the word before it is
          StartX := TmpX;
          while (TmpX > 0) and not(locLine[TmpX] in TSynValidStringChars) do
            Dec(TmpX);
          if TmpX > 0 then
            begin
              SavePos := TmpX;
              While (TmpX > 0) and (locLine[TmpX] in TSynValidStringChars+['.']) do
                dec(TmpX);
              inc(TmpX);
              lookup := UpperCase(Copy(LocLine, TmpX, SavePos - TmpX + 1));
              FoundMatch := LookupList.IndexOf(Lookup) > -1;
              if not(FoundMatch) then
                begin
                  TmpX := StartX;
                  dec(TmpX);
                end;
            end;
        end
      else
        dec(TmpX)
    end;

  finally
    FreeAndNil(lookupList);
  end;

  Result := FoundMatch;
end;

end.
