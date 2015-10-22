// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VirtualTrees, ImgList, ExtCtrls, ComCtrls,
  UH5MFrame, UMapXDB, UH5MManager, JvExComCtrls, ActnList, Menus, UCommonDM,
  SynEdit, SynEditSearch, SynEditRegexSearch, SynEditMiscClasses,
  USearchReplaceOptions, DKLang, Contnrs, ToolWin, UResourcesCache,
  UCommonTabbedEditor, JvComCtrls, UMapManager, UAppController, UExtLuaLibs;

type
  TMainForm = class(TForm)
    pnlClientArea: TPanel;
    Splitter1: TSplitter;
    jvpgctrlEditors: TJvPageControl;
    actListMain: TActionList;
    acExit: TAction;
    acFindInFiles: TAction;
    acPrint: TAction;
    acUndo: TAction;
    acRedo: TAction;
    acSelectAll: TAction;
    acCut: TAction;
    acCopy: TAction;
    acPaste: TAction;
    acFind: TAction;
    acFindAgain: TAction;
    acFindReplace: TAction;
    acGoToLine: TAction;
    acEditorSettings: TAction;
    acBlockUnindent: TAction;
    acBlockIndent: TAction;
    acBlockComment: TAction;
    acBlockUncomment: TAction;
    acUpperCase: TAction;
    acLowerCase: TAction;
    ppmnuEditor: TPopupMenu;
    Undo2: TMenuItem;
    Redo2: TMenuItem;
    N17: TMenuItem;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
    N19: TMenuItem;
    IndentSelection2: TMenuItem;
    UnindentSelection2: TMenuItem;
    CommentSelection2: TMenuItem;
    UncommentSelection2: TMenuItem;
    N16: TMenuItem;
    oggleBookmark1: TMenuItem;
    GotoBookmark12: TMenuItem;
    GotoBookmark13: TMenuItem;
    GotoBookmark14: TMenuItem;
    GotoBookmark15: TMenuItem;
    GotoBookmark16: TMenuItem;
    GotoBookmark17: TMenuItem;
    GotoBookmark18: TMenuItem;
    GotoBookmark81: TMenuItem;
    GotoBookmark91: TMenuItem;
    GotoBookmark11: TMenuItem;
    GotoBookmark1: TMenuItem;
    Bookmark11: TMenuItem;
    Bookmark12: TMenuItem;
    Bookmark13: TMenuItem;
    Bookmark14: TMenuItem;
    Bookmark15: TMenuItem;
    Bookmark16: TMenuItem;
    Bookmark17: TMenuItem;
    Bookmark18: TMenuItem;
    Bookmark19: TMenuItem;
    Bookmark110: TMenuItem;
    N24: TMenuItem;
    GotoLine2: TMenuItem;
    acClosePage: TAction;
    MainMenu1: TMainMenu;
    miFiles: TMenuItem;
    N2: TMenuItem;
    Ghfdrf1: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    acOpenH5M: TAction;
    h5m1: TMenuItem;
    miWindows: TMenuItem;
    acClosePage1: TMenuItem;
    N6: TMenuItem;
    Exit1: TMenuItem;
    N5: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    N18: TMenuItem;
    N20: TMenuItem;
    N21: TMenuItem;
    N22: TMenuItem;
    N23: TMenuItem;
    synSearch: TSynEditSearch;
    synSearchRegex: TSynEditRegexSearch;
    Replace1: TMenuItem;
    acCreateTXTFile: TAction;
    miMRUStart: TMenuItem;
    N26: TMenuItem;
    N27: TMenuItem;
    N28: TMenuItem;
    DKLanguageController1: TDKLanguageController;
    acCloseOtherPages: TAction;
    N29: TMenuItem;
    acOpenEmbFile: TAction;
    acCreateLUAFile: TAction;
    acDeleteFolderOrFile: TAction;
    acCreateLUAFile1: TMenuItem;
    acDeleteFile1: TMenuItem;
    N30: TMenuItem;
    pnlFilesTree: TPanel;
    H5MFrame1: TH5MFrame;
    acCreateFolder: TAction;
    opndlgH5MSelect: TOpenDialog;
    acNextPage: TAction;
    acPrevPage: TAction;
    N31: TMenuItem;
    N32: TMenuItem;
    N33: TMenuItem;
    miMRUEnd: TMenuItem;
    acReOpenH5M: TAction;
    acAbout: TAction;
    N1: TMenuItem;
    N25: TMenuItem;
    acCloseH5M: TAction;
    N36: TMenuItem;
    acSettings: TAction;
    N38: TMenuItem;
    acCheckGrammar: TAction;
    N34: TMenuItem;
    N35: TMenuItem;
    acOpenHelp: TAction;
    N39: TMenuItem;
    pnlHeader: TPanel;
    lblFileName: TLabel;
    pnlToolbar: TPanel;
    tlbrFileOperations: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ppmnuFilesFilter: TPopupMenu;
    txt1: TMenuItem;
    xdb1: TMenuItem;
    lua1: TMenuItem;
    dds1: TMenuItem;
    bin1: TMenuItem;
    N42: TMenuItem;
    tlbrFilters: TToolBar;
    acFilterFiles: TAction;
    ToolButton6: TToolButton;
    N40: TMenuItem;
    miOtherFiles: TMenuItem;
    ppmnuPages: TPopupMenu;
    N41: TMenuItem;
    N43: TMenuItem;
    acOpenExtFile: TAction;
    opndlgExtFileSelect: TOpenDialog;
    N44: TMenuItem;
    lualua1: TMenuItem;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    acRefresh: TAction;
    StatusBar1: TStatusBar;
    FindinFiles1: TMenuItem;
    N45: TMenuItem;
    acSavePageEditorContent: TAction;
    acOpenHomePage: TAction;
    N37: TMenuItem;
    procedure acClosePageExecute(Sender: TObject);
    procedure acUndoExecute(Sender: TObject);
    procedure acOpenH5MExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure actListMainUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acRedoExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acCutExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure acBlockIndentExecute(Sender: TObject);
    procedure acBlockUnindentExecute(Sender: TObject);
    procedure acBlockCommentExecute(Sender: TObject);
    procedure acBlockUncommentExecute(Sender: TObject);
    procedure acGoToLineExecute(Sender: TObject);
    procedure miWindowsClick(Sender: TObject);
    procedure ToggleBookmarkClick(Sender: TObject);
    procedure GotoBookmarkClick(Sender: TObject);
    procedure jvpgctrlEditorsChange(Sender: TObject);
    procedure acFindExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure acFindAgainExecute(Sender: TObject);
    procedure acFindReplaceExecute(Sender: TObject);
    procedure acSelectAllExecute(Sender: TObject);
    procedure acCloseOtherPagesExecute(Sender: TObject);
    procedure acOpenEmbFileExecute(Sender: TObject);
    procedure acCreateTXTFileExecute(Sender: TObject);
    procedure acCreateLUAFileExecute(Sender: TObject);
    procedure acCreateFolderExecute(Sender: TObject);
    procedure acDeleteFolderOrFileExecute(Sender: TObject);
    procedure acNextPageExecute(Sender: TObject);
    procedure acPrevPageExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acReOpenH5MExecute(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure acCloseH5MExecute(Sender: TObject);
    procedure acFilterFilesExecute(Sender: TObject);
    procedure acSettingsExecute(Sender: TObject);
    procedure H5MFrame1vstH5MContentIncrementalSearch(Sender: TBaseVirtualTree;
        Node: PVirtualNode; const SearchText: WideString; var Result: Integer);
    procedure H5MFrame1vstH5MContentStateChange(Sender: TBaseVirtualTree; Enter,
        Leave: TVirtualTreeStates);
    procedure FilterChangeClick(Sender: TObject);
    procedure jvpgctrlEditorsContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure acOpenExtFileExecute(Sender: TObject);
    procedure DKLanguageController1LanguageChanged(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acFindInFilesExecute(Sender: TObject);
    procedure acSavePageEditorContentExecute(Sender: TObject);
    procedure acOpenHomePageExecute(Sender: TObject);

  private
    procedure OpenH5MFile(AFileName: string);
    procedure OpenExtFile(AFileName: string);

  private
    FOpenEditors: TObjectList;
    FOpenEditorsGen: integer;
    function GetOpenEditor(Index: integer): TEditorPage;
    function GetOpenEditorsCount: integer;
    function FindEditorByTab(TabSheet: TTabSheet): integer;
    procedure CloseEditor(Index: integer; AModPromt: boolean = false);
    function GetActiveEditor: integer;

  public
    property OpenEditors[Index: integer]: TEditorPage read GetOpenEditor;
    property OpenEditorsCount: integer read GetOpenEditorsCount;
    property ActiveEditor: integer read GetActiveEditor;

    procedure SetStatusbarText(const S: string);

  protected
    FH5MManager: TMapManager;
    FResourcesCache: TResourcesCache;
    FExtLuaLibs: TExtLuaLibs;

    FMapXDB: TMapXDB;

    procedure ProcessH5M();
    procedure ReInitHMM5Res();

    function OpenLUAFile(AFullFilePath: string): TTabSheet;
    function OpenXMLFile(AFullFilePath: string): TTabSheet;
    function OpenTXTFile(AFullFilePath: string): TTabSheet;
    function OpenUnkFile(AFullFilePath: string): TTabSheet;

    procedure OpenXDBMapFile();

    function FindFilePage(AFileName: string): TTabSheet;

    function GenerateUniqControlName(AObject: TObject): string;

  protected
    FSearchReplaceOptions: TSearchReplaceOptions;
    procedure synedtEditorReplaceText(Sender: TObject; const ASearch,
      AReplace: string; Line, Column: Integer; var Action: TSynReplaceAction);

  protected
    procedure WindowsMenuItemClick(Sender : TObject);
    procedure AddWindowsMenuItem(Obj: TTabSheet);

  protected
    procedure EditorChangeModified(Sender: TObject);

  protected
    procedure BuildFilterFromMenu(var Filter: string; var ExcludeMode: boolean);

  public
    procedure OpenFileEvent(AFullFilePath, AFileName: string);

  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  StrUtils, XMLIntf, ShellAPI, ULuaFrame, UXMLFrame, UTxtFrame, SynEditKeyCmds,
  SynEditTypes, SynEditTextBuffer, USearchForm, UReplaceForm, UConsts,
  UReplaceQueryFrom, UEmbFileChooseDialog, UOptionsUnit, JclShell,
  UZipFileUtils, UUnkFrame, JclFileUtils, USettingsForm, UAboutForm,
  UMapManager.MapEntry, UDKConsts, USearchInFilesForm;

procedure TMainForm.acBlockIndentExecute(Sender: TObject);
var
  xEditor: TSynEdit;
begin
  xEditor := AppController.GetActivePageEditor();
  if xEditor<>nil then
    xEditor.ExecuteCommand(ecBlockIndent, #0, nil);
end;

procedure TMainForm.acCloseH5MExecute(Sender: TObject);
var
  xIsMod: boolean;
  i: Integer;
begin
  xIsMod := false;
  for i := 0 to OpenEditorsCount - 1 do
    if OpenEditors[i].Editor.getModified then
      begin
        xIsMod := true;
        break;
      end;

  if xIsMod then
    if MessageDlg(DKConsts.SCAN_CLOSE_MAP, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
      exit;

  for i := OpenEditorsCount - 1 downto 0 do
    CloseEditor(i, false);

  lblFileName.Caption := DKConsts.SHEADER_MAP_LABEL;

  H5MFrame1.OpenFileEvent := nil;
  H5MFrame1.InitWith(nil);

  FMapXDB.Free;
  FMapXDB := nil;

  FH5MManager.Free;
  FH5MManager := nil;

  acOpenH5M.Enabled := true;
  acReOpenH5M.Enabled := true;
end;

procedure TMainForm.acCloseOtherPagesExecute(Sender: TObject);
var
  xIndex: integer;
  i: Integer;
begin
  if AppController.OpenEditorsCount = 0 then
    exit;

  xIndex := AppController.ActiveEditor;
  if xIndex = -1 then
    exit;

  FOpenEditors.Move(xIndex, 0);
  for i := OpenEditorsCount - 1 downto 1 do
    CloseEditor(i, true);
end;

procedure TMainForm.acClosePageExecute(Sender: TObject);
var
  xIndex: integer;
begin
  if AppController.OpenEditorsCount = 0 then
    exit;

  xIndex := AppController.ActiveEditor;
  if xIndex = -1 then
    exit;

  CloseEditor(xIndex, true);
end;

procedure TMainForm.acExitExecute(Sender: TObject);
begin
  Application.MainForm.Close;
end;

procedure TMainForm.acFindAgainExecute(Sender: TObject);
var
  Options: TSynSearchOptions;
  xResult: Boolean;
  xEditor: TSynEdit;
  xMsg: string;
begin
  xEditor := AppController.GetActivePageEditor();
  if xEditor = nil then
    exit;

  xResult := True;

  Options := FSearchReplaceOptions.BuildSynSearchOptions();
  if not FSearchReplaceOptions.srSearchRegularExpression then
    xEditor.SearchEngine := synSearch
  else
    xEditor.SearchEngine := synSearchRegex;

  if xEditor.SearchReplace(FSearchReplaceOptions.sSearchString, '', Options) = 0 then
    begin
      xResult := False;
      xMsg := DKConsts.SSTRING_NOT_FOUND_FMT([FSearchReplaceOptions.sSearchString]);
      Application.MessageBox(PAnsiChar(xMsg), APPLICATION_NAME, MB_OK+MB_ICONINFORMATION);
    end;
end;

procedure TMainForm.acFindExecute(Sender: TObject);
var
  Options: TSynSearchOptions;
  xResult: boolean;
  xEditor: TSynEdit;
  xMsg: string;
  xSearchForm: TSearchForm;
begin
  xEditor := AppController.GetActivePageEditor();
  if xEditor = nil then
    exit;

  xResult := True;

  xSearchForm := TSearchForm.Create(Application);
  try
    xSearchForm.LoadFromOptions(FSearchReplaceOptions);

    if (xEditor.SelLength > 0) and (xEditor.BlockBegin.Line = xEditor.BlockEnd.Line) then
      xSearchForm.cboSearchText.Text := xEditor.SelText
    else if xEditor.GetWordAtRowCol(xEditor.CaretXY) <> '' then
      xSearchForm.cboSearchText.Text := xEditor.GetWordAtRowCol(xEditor.CaretXY);

    // вызов формы
    xSearchForm.ShowModal();

    if xSearchForm.SearchText <> '' then
      begin
        xSearchForm.SaveToOptions(FSearchReplaceOptions);

        Options := FSearchReplaceOptions.BuildSynSearchOptions();
        if not FSearchReplaceOptions.srSearchRegularExpression then
          xEditor.SearchEngine := synSearch
        else
          xEditor.SearchEngine := synSearchRegex;

        if xEditor.SearchReplace(FSearchReplaceOptions.sSearchString, '', Options) = 0 then
          begin
            xResult := False;
            xMsg := DKConsts.SSTRING_NOT_FOUND_FMT([FSearchReplaceOptions.sSearchString]);
            Application.MessageBox(PAnsiChar(xMsg), APPLICATION_NAME, MB_OK+MB_ICONINFORMATION);
          end;
      end;
  finally
    xSearchForm.Release;
  end;
end;

procedure TMainForm.acFindInFilesExecute(Sender: TObject);
begin
  if FH5MManager = nil then
    exit;

  SearchInFilesForm.H5MManager := FH5MManager;
  SearchInFilesForm.ShowModal();
end;

procedure TMainForm.acFindReplaceExecute(Sender: TObject);
var
  Options: TSynSearchOptions;
  xResult: Boolean;
  xEditor: TSynEdit;
  xMsg: string;
begin
  xEditor := AppController.GetActivePageEditor();
  if xEditor<>nil then
    begin
      xResult := True;

      ReplaceForm.LoadFromOptions(FSearchReplaceOptions);

      if ((xEditor.SelAvail) and (xEditor.BlockBegin.Line = xEditor.BlockEnd.Line)) then
        ReplaceForm.cboSearchText.Text := xEditor.SelText
      else if xEditor.GetWordAtRowCol(xEditor.CaretXY) <> '' then
        ReplaceForm.cboSearchText.Text := xEditor.GetWordAtRowCol(xEditor.CaretXY);

      // вызов формы
      ReplaceForm.ShowModal;

      if ReplaceForm.SearchText <> '' then
      begin
        ReplaceForm.SaveToOptions(FSearchReplaceOptions);

        Options := [ssoReplace];
        if FSearchReplaceOptions.srReplaceAll then
          Options := Options + [ssoReplaceAll];
        if FSearchReplaceOptions.srPromptForReplace then
          Options := Options + [ssoPrompt];
        if FSearchReplaceOptions.srSearchSensitive then
          Options := Options + [ssoMatchCase];
        if FSearchReplaceOptions.srSearchWholeWords then
          Options := Options + [ssoWholeWord];
        if FSearchReplaceOptions.srSearchOrigin = SR_ENTIRESCOPE then
          Options := Options + [ssoEntireScope];
        if FSearchReplaceOptions.srSearchScope = SR_SELECTED then
          Options := Options + [ssoSelectedOnly];
        if FSearchReplaceOptions.srSearchDirection = SR_BACKWARD then
          Options := Options + [ssoBackwards];
        if not FSearchReplaceOptions.srSearchRegularExpression then
          xEditor.SearchEngine := synSearch
        else
          xEditor.SearchEngine := synSearchRegEx;

        if xEditor.SearchReplace(FSearchReplaceOptions.sSearchString, FSearchReplaceOptions.sReplaceString, Options) = 0 then
          begin
            xResult := False;
            xMsg := DKConsts.SSTRING_NOT_FOUND_FMT([FSearchReplaceOptions.sSearchString]);
            Application.MessageBox(PAnsiChar(xMsg), APPLICATION_NAME, MB_OK+MB_ICONINFORMATION);
          end;
      end;
    end;
end;

procedure TMainForm.acOpenEmbFileExecute(Sender: TObject);
var
  xFileName: string;
begin
  xFileName := ChooseEmbFileDialog(FH5MManager, MASK_XDB);
  if xFileName<>'' then
    ShowMessage(xFileName);
end;

procedure TMainForm.acOpenH5MExecute(Sender: TObject);
begin
  if opndlgH5MSelect.Execute then
    OpenH5MFile(opndlgH5MSelect.FileName);
end;

procedure TMainForm.acOpenHomePageExecute(Sender: TObject);
const
  HOME_PAGE = 'http://hmm5.sklabs.ru';
begin
  ShellExecute(0, 'open', PChar(HOME_PAGE), nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.acOpenExtFileExecute(Sender: TObject);
begin
  if FH5MManager = nil then
    exit;
  if opndlgExtFileSelect.Execute then
    OpenExtFile(opndlgExtFileSelect.FileName);
end;

procedure TMainForm.acCopyExecute(Sender: TObject);
begin
  if AppController.GetActivePageEditor()<>nil then
    AppController.GetActivePageEditor().CommandProcessor(ecCopy, #0, nil);
end;

procedure TMainForm.acCutExecute(Sender: TObject);
begin
  if AppController.GetActivePageEditor() <> nil then
    AppController.GetActivePageEditor().CommandProcessor(ecCut, #0, nil);
end;

procedure TMainForm.acDeleteFolderOrFileExecute(Sender: TObject);
var
  xPath: string;
  xIndex: Integer;
  xMsg: string;
begin
  xPath := H5MFrame1.GetSelectedPath(false);
  if (xPath = '') then
    exit;
    
  xMsg := DKConsts.SCONF_DEL_FILE_FMT([xPath]);
  if MessageDlg(xMsg, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    exit;

  xIndex := AppController.FindEditorByFileName(xPath);
  if xIndex <> -1 then
    CloseEditor(xIndex, false);

  try
    FH5MManager.DeleteDirectoryFile(xPath);
  finally
    H5MFrame1.Refresh();
  end;
end;

procedure TMainForm.acAboutExecute(Sender: TObject);
var
  xForm: TAboutForm;
begin
  xForm := TAboutForm.Create(Self.Owner);
  try
    xForm.ShowModal();
  finally
    xForm.Free;
  end;
end;

procedure TMainForm.acBlockCommentExecute(Sender: TObject);
begin
  if AppController.GetActivePageEditor() <> nil then
    AppController.GetActivePageEditor().CommandProcessor(ecUserCommentBlock, #0, nil);
end;

procedure TMainForm.acBlockUncommentExecute(Sender: TObject);
begin
  if AppController.GetActivePageEditor() <> nil then
    AppController.GetActivePageEditor().CommandProcessor(ecUserUnCommentBlock, #0, nil);
end;

procedure TMainForm.acBlockUnindentExecute(Sender: TObject);
begin
  if AppController.GetActivePageEditor() <> nil then
    AppController.GetActivePageEditor().ExecuteCommand(ecBlockUnindent, #0, nil);
end;

procedure TMainForm.acGoToLineExecute(Sender: TObject);
var
  xLineNumber: string;
  xEditor: TSynEdit;
begin
  xEditor := AppController.GetActivePageEditor();
  if xEditor<>nil then
    begin
      if InputQuery(DKConsts.SGOTO_TITLE, DKConsts.SGOTO_TEXT, xLineNumber) and
         (StrToIntDef(xLineNumber, -1)>0) then
        begin
          xEditor.GotoLineAndCenter(StrToIntDef(xLineNumber, -1));
        end;
    end;
end;

procedure TMainForm.acNextPageExecute(Sender: TObject);
begin
  jvpgctrlEditors.ActivePageIndex := (jvpgctrlEditors.ActivePageIndex + 1) mod jvpgctrlEditors.PageCount;
end;

procedure TMainForm.acCreateFolderExecute(Sender: TObject);

function intCheckFileName(AFullFileName, AFileName: string): boolean;
begin
  Result := not FH5MManager.IsFilePresent(AFullFileName);
  Result := Result and checkFileName(AFileName);
end;

var
  xFullFileName: string;
  xFileName: string;
  xActiveFolder: string;
begin
  if FH5MManager=nil then
    exit;

  xActiveFolder := H5MFrame1.GetSelectedFolderPath();

  xFullFileName := '';
  xFileName := DEFAULT_DIR_FILENAME;
  while InputQuery(DKConsts.SCAPTION_DIR_FILENAME, DKConsts.SPROMT_DIR_FILENAME, xFileName) do
    begin
      xFullFileName := xActiveFolder + xFileName + '/';
      if (intCheckFileName(xFullFileName, xFileName)) then
        break;
      xFullFileName := '';
      MessageDlg(DKConsts.SERROR_DIR_FILENAME, mtError, [mbOK], 0);
    end;

  if xFullFileName <> '' then
    begin
      FH5MManager.CreateEmptyDirectory(xFullFileName);
      H5MFrame1.Refresh();
      H5MFrame1.SetSelected(xFullFileName);
    end;
end;

procedure TMainForm.acCreateLUAFileExecute(Sender: TObject);

function intCheckFileName(AFullFileName, AFileName: string): boolean;
begin
  Result := not FH5MManager.IsFilePresent(AFullFileName);
  Result := Result and AnsiSameText(ExtractFileExt(AFileName), EXT_LUA);
  Result := Result and checkFileName(AFileName);
end;

var
  xFullFileName: string;
  xFileName: string;
  xActiveFolder: string;
begin
  if FH5MManager=nil then
    exit;

  xActiveFolder := H5MFrame1.GetSelectedFolderPath();

  xFullFileName := '';
  xFileName := DEFAULT_LUA_FILENAME;
  while InputQuery(DKConsts.SCAPTION_LUA_FILENAME, DKConsts.SPROMT_LUA_FILENAME, xFileName) do
    begin
      xFullFileName := xActiveFolder + xFileName;
      if (intCheckFileName(xFullFileName, xFileName)) then
        break;
      xFullFileName := '';
      MessageDlg(DKConsts.SERROR_LUA_FILENAME, mtError, [mbOK], 0);
    end;

  if xFullFileName <> '' then
    begin
      FH5MManager.CreateEmptyFile(xFullFileName);
      H5MFrame1.Refresh();
      H5MFrame1.SetSelected(xFullFileName);
      OpenFileEvent(xFullFileName, xFileName);
    end;
end;

procedure TMainForm.acCreateTXTFileExecute(Sender: TObject);

function intCheckFileName(AFullFileName, AFileName: string): boolean;
begin
  Result := not FH5MManager.IsFilePresent(AFullFileName);
  Result := Result and AnsiSameText(ExtractFileExt(AFileName), EXT_TXT);
  Result := Result and checkFileName(AFileName);
end;

var
  xFullFileName: string;
  xFileName: string;
  xActiveFolder: string;
begin
  if FH5MManager = nil then
    exit;

  xActiveFolder := H5MFrame1.GetSelectedFolderPath();

  xFullFileName := '';
  xFileName := DEFAULT_TXT_FILENAME;
  while InputQuery(DKConsts.SCAPTION_TXT_FILENAME, DKConsts.SPROMT_TXT_FILENAME, xFileName) do
    begin
      xFullFileName := xActiveFolder + xFileName;
      if (intCheckFileName(xFullFileName, xFileName)) then
        break;
      xFullFileName := '';
      MessageDlg(DKConsts.SERROR_TXT_FILENAME, mtError, [mbOK], 0);
    end;

  if xFullFileName <> '' then
    begin
      FH5MManager.CreateEmptyFile(xFullFileName);
      H5MFrame1.Refresh();
      H5MFrame1.SetSelected(xFullFileName);
      OpenFileEvent(xFullFileName, xFileName);
    end;
end;

procedure TMainForm.acFilterFilesExecute(Sender: TObject);
begin
  if (Sender is TToolButton) then
    (Sender as TToolButton).CheckMenuDropdown;
end;

procedure TMainForm.actListMainUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  xEditor: TSynEdit;
  xEditorOptions: TEditorOptions;
  xHMMFileOpened: boolean;
  xEditorActive: boolean;
  xZIPItemSelected: boolean;
  xMoreThenOnePage: Boolean;
  xIndex: integer;
  xMapItemLocation: TMapItemLocation; 
begin
  xMapItemLocation := ilUnknown;
  
  xHMMFileOpened := FH5MManager<>nil;
  if xHMMFileOpened then
    begin
      xIndex := FH5MManager.FindFile(H5MFrame1.GetSelectedPath());
      if xIndex <> -1 then
        xMapItemLocation := FH5MManager.ItemLocation[xIndex]
    end;

  acCloseH5M.Enabled := xHMMFileOpened;

  acOpenExtFile.Enabled := xHMMFileOpened;
  acRefresh.Enabled := xHMMFileOpened;
  acFindInFiles.Enabled := xHMMFileOpened;

  acCreateTXTFile.Enabled := xHMMFileOpened and not (xMapItemLocation in [ilExtFile]);
  acCreateLUAFile.Enabled := xHMMFileOpened and not (xMapItemLocation in [ilExtFile]);
  acCreateFolder.Enabled := xHMMFileOpened and not (xMapItemLocation in [ilExtFile]);

  xZIPItemSelected := xHMMFileOpened and
    (H5MFrame1.vstH5MContent.GetFirstSelected()<>nil) and
    (H5MFrame1.vstH5MContent.GetFirstSelected().ChildCount = 0);

  acDeleteFolderOrFile.Enabled := xZIPItemSelected and
    not (xMapItemLocation in [ilExtFile, ilGameResource]);

  xEditor := AppController.GetActivePageEditor();
  xEditorOptions := AppController.GetActivePageEditorOptions();
  xEditorActive := xHMMFileOpened and (xEditor<>nil);

  if xEditorActive then
    begin
      acUndo.Enabled := (xEditor.UndoList.ItemCount > 0) and (edoEditable in xEditorOptions);
      acRedo.Enabled := (xEditor.RedoList.ItemCount > 0) and (edoEditable in xEditorOptions);
    end
  else
    begin
      acUndo.Enabled := false;
      acRedo.Enabled := false;
    end;
    
  acCopy.Enabled := xEditorActive;
  acPaste.Enabled := xEditorActive and (edoEditable in xEditorOptions);
  acCut.Enabled := xEditorActive and (edoEditable in xEditorOptions);

  acBlockIndent.Enabled := xEditorActive and (edoEditable in xEditorOptions);
  acBlockUnindent.Enabled := xEditorActive and (edoEditable in xEditorOptions);
  acBlockComment.Enabled := xEditorActive and (edoEditable in xEditorOptions) and (edoLuaComment in xEditorOptions);
  acBlockUncomment.Enabled := xEditorActive and (edoEditable in xEditorOptions) and (edoLuaComment in xEditorOptions);

  acSavePageEditorContent.Enabled := xEditorActive and (edoEditable in xEditorOptions); 

  acGoToLine.Enabled := xEditorActive;

  acFind.Enabled := xEditorActive;
  acFindReplace.Enabled := xEditorActive and (edoEditable in xEditorOptions);
  acFindAgain.Enabled := xEditorActive;

  xMoreThenOnePage := jvpgctrlEditors.PageCount > 1;

  acNextPage.Enabled := xMoreThenOnePage;
  acPrevPage.Enabled := xMoreThenOnePage;
  acClosePage.Enabled := (jvpgctrlEditors.PageCount > 0);
  acCloseOtherPages.Enabled := xMoreThenOnePage;

  acFilterFiles.Enabled := true;
end;

procedure TMainForm.acSavePageEditorContentExecute(Sender: TObject);
var
  xIndex: integer;
begin
  xIndex := ActiveEditor;
  if xIndex = -1 then
    exit;
  if OpenEditors[xIndex].Editor.getModified then
    OpenEditors[xIndex].Editor.SaveChanges();
end;

procedure TMainForm.acSelectAllExecute(Sender: TObject);
begin
  if AppController.GetActivePageEditor()<>nil then
    AppController.GetActivePageEditor().CommandProcessor(ecSelectAll, #0, nil);
end;

procedure TMainForm.acSettingsExecute(Sender: TObject);
var
  xSettingsForm: TSettingsForm;
begin
  xSettingsForm := TSettingsForm.Create(Application);
  try
    xSettingsForm.Init();
    if xSettingsForm.ShowModal = mrOk then
      begin
        H5MFrame1.StartUpOpenFolders := AppIniOptions.StartUp.OpenFolders;
        if not AnsiSameText(FResourcesCache.HMM5Dir, AppIniOptions.Path.HMM5Path) then
          ReInitHMM5Res();
      end;
  finally
    xSettingsForm.Release;
  end;
end;

procedure TMainForm.acPasteExecute(Sender: TObject);
var
  xEditor: TSynEdit;
begin
  xEditor := AppController.GetActivePageEditor();
  if xEditor<>nil then
    begin
      xEditor.CommandProcessor(ecPaste, #0, nil);
    end;
end;

procedure TMainForm.acPrevPageExecute(Sender: TObject);
begin
  if (jvpgctrlEditors.ActivePageIndex = 0) then
    jvpgctrlEditors.ActivePageIndex := jvpgctrlEditors.PageCount - 1
  else
    jvpgctrlEditors.ActivePageIndex := (jvpgctrlEditors.ActivePageIndex - 1);
end;

procedure TMainForm.acRedoExecute(Sender: TObject);
begin
  if AppController.GetActivePageEditor()<>nil then
    AppController.GetActivePageEditor().CommandProcessor(ecRedo, #0, nil);
end;

procedure TMainForm.acRefreshExecute(Sender: TObject);
begin
  FH5MManager.Refresh();
  H5MFrame1.Refresh();

  //TODO: проверить открытые окна
end;

procedure TMainForm.acReOpenH5MExecute(Sender: TObject);
begin
  OpenH5MFile(AppIniOptions.MRUOpenFiles.Items[((Sender as TCustomAction).ActionComponent as TMenuItem).Tag]);
end;

procedure TMainForm.acUndoExecute(Sender: TObject);
begin
  if AppController.GetActivePageEditor<>nil then
    AppController.GetActivePageEditor().CommandProcessor(ecUndo, #0, nil);
end;

function TMainForm.GenerateUniqControlName(AObject: TObject): string;
begin
  Result := AObject.ClassName + '_' + IntToStr(FOpenEditorsGen);
  Inc(FOpenEditorsGen);
end;

procedure TMainForm.ProcessH5M;
var
  xFilter: string;
  ExcludeMode: Boolean;
begin
  BuildFilterFromMenu(xFilter, ExcludeMode);
  H5MFrame1.InitWith(FH5MManager, xFilter, ExcludeMode);
  H5MFrame1.OpenFileEvent := OpenFileEvent;
end;

procedure TMainForm.SetStatusbarText(const S: string);
begin
  StatusBar1.SimpleText := S;
end;

function TMainForm.FindEditorByTab(TabSheet: TTabSheet): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FOpenEditors.Count - 1 do
    if OpenEditors[i].Page = TabSheet then
      begin
        Result := i;
        break;
      end;
end;

function TMainForm.FindFilePage(AFileName: string): TTabSheet;
var
  xIndex: Integer;
begin
  xIndex := AppController.FindEditorByFileName(AFileName);
  if xIndex <> -1 then
    Result := OpenEditors[xIndex].Page
  else
    Result := nil;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  xIsMod: boolean;
  i: Integer;
begin
  xIsMod := false;
  for i := 0 to OpenEditorsCount - 1 do
    if OpenEditors[i].Editor.getModified then
      begin
        xIsMod := true;
        break;
      end;

  if xIsMod then
    CanClose := (MessageDlg(DKConsts.SCAN_CLOSE_ALL_FILES, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FSearchReplaceOptions := TSearchReplaceOptions.Create;
  FOpenEditors := TObjectList.Create(true);
  FOpenEditorsGen := 0;

  AppIniOptions.MRUOpenFiles.RebuildMenuItems(miFiles, acReOpenH5M);

  H5MFrame1.StartUpOpenFolders := AppIniOptions.StartUp.OpenFolders;

  FResourcesCache := TResourcesCache.Create;

  ReInitHMM5Res();

  FExtLuaLibs := TExtLuaLibs.Create;
  FExtLuaLibs.LibDir := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)+ 'Libs'));
  FExtLuaLibs.RefreshLibs(true);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FH5MManager) then
    FreeAndNil(FH5MManager);

  FreeAndNil(FResourcesCache);

  if Assigned(FMapXDB) then
    FreeAndNil(FMapXDB);

  FreeAndNil(FOpenEditors);
  FreeAndNil(FSearchReplaceOptions);
  FreeAndNil(FExtLuaLibs);
end;

function TMainForm.GetActiveEditor: integer;
begin
  Result := -1;
  if jvpgctrlEditors.PageCount > 0 then
    Result := FindEditorByTab(jvpgctrlEditors.ActivePage);
end;

function TMainForm.GetOpenEditor(Index: integer): TEditorPage;
begin
  Result := FOpenEditors[Index] as TEditorPage;
end;

function TMainForm.GetOpenEditorsCount: integer;
begin
  Result := FOpenEditors.Count;
end;

procedure TMainForm.ToggleBookmarkClick(Sender: TObject);
var
  iBookmark: Integer;
  xEditor: TSynEdit;
begin
  xEditor := AppController.GetActivePageEditor();
  if xEditor<>nil then
    begin
      iBookmark := TMenuItem(Sender).Tag;

      case iBookmark of
        1: xEditor.ExecuteCommand(ecSetMarker1, #0, nil);
        2: xEditor.ExecuteCommand(ecSetMarker2, #0, nil);
        3: xEditor.ExecuteCommand(ecSetMarker3, #0, nil);
        4: xEditor.ExecuteCommand(ecSetMarker4, #0, nil);
        5: xEditor.ExecuteCommand(ecSetMarker5, #0, nil);
        6: xEditor.ExecuteCommand(ecSetMarker6, #0, nil);
        7: xEditor.ExecuteCommand(ecSetMarker7, #0, nil);
        8: xEditor.ExecuteCommand(ecSetMarker8, #0, nil);
        9: xEditor.ExecuteCommand(ecSetMarker9, #0, nil);
        0: xEditor.ExecuteCommand(ecSetMarker0, #0, nil);
      end;
    end;
end;

procedure TMainForm.AddWindowsMenuItem(Obj: TTabSheet);
var
  mi : TMenuItem;
  xEditorPage: TEditorPage;
begin
  xEditorPage := OpenEditors[FindEditorByTab(Obj)];

  mi := TMenuItem.Create(Self);
  if xEditorPage.Editor.getModified then
    mi.Caption := '*' + xEditorPage.Editor.getTitle
  else
    mi.Caption := xEditorPage.Editor.getTitle;
  mi.ImageIndex := xEditorPage.Editor.getImageIndex;
  mi.Tag := Integer(Obj);
  mi.OnClick := WindowsMenuItemClick;
  miWindows.Insert(0, mi);
end;

procedure TMainForm.BuildFilterFromMenu(var Filter: string;
  var ExcludeMode: boolean);
var
  i: Integer;
begin
  ExcludeMode := miOtherFiles.Checked;
  Filter := '';
  for i := 0 to ppmnuFilesFilter.Items.Count - 1 do
    if ppmnuFilesFilter.Items[i].Caption <> '-' then
      if (not ExcludeMode and ppmnuFilesFilter.Items[i].Checked) or
        (ExcludeMode and not ppmnuFilesFilter.Items[i].Checked) then
        Filter := Filter + '|' + Trim(ppmnuFilesFilter.Items[i].Caption);
end;

procedure TMainForm.EditorChangeModified(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to OpenEditorsCount - 1 do
    begin
      if OpenEditors[i].Editor.getModified then
        OpenEditors[i].Page.Caption := '*' + OpenEditors[i].Editor.getTitle
      else
        OpenEditors[i].Page.Caption := OpenEditors[i].Editor.getTitle;
    end;
end;

procedure TMainForm.CloseEditor(Index: integer; AModPromt: boolean = false);
begin
  if (Index >= 0) and (Index<OpenEditorsCount) then
    begin
      if AModPromt and OpenEditors[Index].Editor.getModified then
        begin
          if (MessageDlg(DKConsts.SMOD_FILE_CLOSE_FMT([OpenEditors[Index].Editor.getTitle]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
            begin
              OpenEditors[Index].Page.Free;
              FOpenEditors.Delete(Index);
            end;
        end
      else
        begin
          OpenEditors[Index].Page.Free;
          FOpenEditors.Delete(Index);
        end;
    end;
end;

procedure TMainForm.DKLanguageController1LanguageChanged(Sender: TObject);
begin
  if Assigned(FH5MManager) then
    lblFileName.Caption := DKConsts.SHEADER_MAP_LABEL + ' ' + FH5MManager.H5MFileName
  else
    lblFileName.Caption := DKConsts.SHEADER_MAP_LABEL;
end;

procedure TMainForm.GotoBookmarkClick(Sender: TObject);
var
  iBookmark: Integer;
  xEditor: TSynEdit;
begin
  xEditor := AppController.GetActivePageEditor();
  if xEditor<>nil then
    begin
      iBookmark := TMenuItem(Sender).Tag;

      case iBookmark of
        1: xEditor.ExecuteCommand(ecGotoMarker1, #0, nil);
        2: xEditor.ExecuteCommand(ecGotoMarker2, #0, nil);
        3: xEditor.ExecuteCommand(ecGotoMarker3, #0, nil);
        4: xEditor.ExecuteCommand(ecGotoMarker4, #0, nil);
        5: xEditor.ExecuteCommand(ecGotoMarker5, #0, nil);
        6: xEditor.ExecuteCommand(ecGotoMarker6, #0, nil);
        7: xEditor.ExecuteCommand(ecGotoMarker7, #0, nil);
        8: xEditor.ExecuteCommand(ecGotoMarker8, #0, nil);
        9: xEditor.ExecuteCommand(ecGotoMarker9, #0, nil);
        0: xEditor.ExecuteCommand(ecGotoMarker0, #0, nil);
      end;
    end;
end;

procedure TMainForm.H5MFrame1vstH5MContentIncrementalSearch(Sender:
    TBaseVirtualTree; Node: PVirtualNode; const SearchText: WideString; var
    Result: Integer);
begin
  SetStatusbarText(DKConsts.STREE_INCSEARCH + ' ' + SearchText);

  H5MFrame1.vstH5MContentIncrementalSearch(Sender, Node, SearchText, Result);
end;

procedure TMainForm.H5MFrame1vstH5MContentStateChange(Sender: TBaseVirtualTree;
    Enter, Leave: TVirtualTreeStates);
begin
  if tsIncrementalSearching in Enter then
    SetStatusbarText(DKConsts.STREE_INCSEARCH + ' ' + Sender.SearchBuffer);
  if tsIncrementalSearching in Leave then
    SetStatusbarText('');
end;

procedure TMainForm.ReInitHMM5Res;
var
  xHMM5Dir: string;
begin
  xHMM5Dir := AppIniOptions.Path.HMM5Path;
  if DirectoryExists(xHMM5Dir) then
    xHMM5Dir := IncludeTrailingPathDelimiter(xHMM5Dir);

  while not FResourcesCache.Ready do
    begin
      // nothing
    end;

  FResourcesCache.AsyncInit(xHMM5Dir, DetectHMM5Version(xHMM5Dir));
end;

procedure TMainForm.jvpgctrlEditorsChange(Sender: TObject);
var
  xEditor: TSynEdit;
begin
  xEditor := AppController.GetActivePageEditor();
  if xEditor<>nil then
    begin
      xEditor.SetFocus;
    end;
end;

procedure TMainForm.jvpgctrlEditorsContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  ppPoint: TPoint;
begin
  Handled := true;
  if jvpgctrlEditors.PageCount = 0 then
    exit;

  if (MousePos.Y > (jvpgctrlEditors.ClientHeight - jvpgctrlEditors.Pages[0].ClientHeight)) then
    exit;

  ppPoint := jvpgctrlEditors.ClientToScreen(MousePos);
  jvpgctrlEditors.PopupMenu.Popup(ppPoint.X, ppPoint.Y);
end;

procedure TMainForm.miWindowsClick(Sender: TObject);
var
  i: integer;
begin
  miWindows.SubMenuImages := CommonDM.imglstFileIcons;
  for i := 0 to miWindows.Count - 6 - 1 do
    miWindows.Remove(miWindows.Items[0]);
  for i := jvpgctrlEditors.PageCount - 1 downto 0 do
    begin
      AddWindowsMenuItem(jvpgctrlEditors.Pages[i]);
    end;
end;

procedure TMainForm.WindowsMenuItemClick(Sender: TObject);
begin
  jvpgctrlEditors.ActivePage := TTabSheet((Sender as TMenuItem).Tag);
end;

procedure TMainForm.OpenExtFile(AFileName: string);
begin
  if not FileExists(AFileName) then
    begin
      MessageDlg(DKConsts.SFILE_NOT_FOUND, mtError, [mbOK], 0);
      exit;
    end;

  FH5MManager.AddExtFile(AFileName);
  H5MFrame1.Refresh();
  H5MFrame1.SetSelected(AFileName);
  OpenFileEvent(AFileName, AFileName);
end;

procedure TMainForm.OpenFileEvent(AFullFilePath, AFileName: string);
var
  xPage: TTabSheet;
begin
  xPage := FindFilePage(AFullFilePath);
  if xPage = nil then
    begin
      if AnsiSameText(ExtractFileExt(AFileName), EXT_XDB) then
        xPage := OpenXMLFile(AFullFilePath)
      else
      if AnsiSameText(ExtractFileExt(AFileName), EXT_LUA) then
        xPage := OpenLUAFile(AFullFilePath)
      else
      if AnsiSameText(ExtractFileExt(AFileName), EXT_TXT) then
        xPage := OpenTXTFile(AFullFilePath)
      else
        xPage := OpenUnkFile(AFullFilePath);
    end;
  if xPage <> nil then
    jvpgctrlEditors.ActivePage := xPage;
end;

procedure TMainForm.OpenH5MFile(AFileName: string);
var
  xLUAFileName: string;
begin
  if not FileExists(AFileName) then
    begin
      MessageDlg(DKConsts.SFILE_NOT_FOUND, mtError, [mbOK], 0);
      exit;
    end;

  lblFileName.Caption := DKConsts.SHEADER_MAP_LABEL + ' ' + AFileName;

  FH5MManager := TMapManager.Create;
  FH5MManager.InitHMM5Res(FResourcesCache);
  FH5MManager.InitWith(AFileName);

  ProcessH5M();

  OpenXDBMapFile();

  if AppIniOptions.StartUp.OpenMapXDB then
    if FH5MManager.LocateMapXDB() <> '' then
      OpenXMLFile(FH5MManager.LocateMapXDB());

  if AppIniOptions.StartUp.OpenMapScriptLUA then
    if Assigned(FMapXDB) then
      begin
        xLUAFileName := Trim(FMapXDB.getMapScriptFilename());
        if (xLUAFileName <> '') then
          begin
            if xLUAFileName[1] = '/' then
              Delete(xLUAFileName, 1, 1);
            OpenLUAFile(xLUAFileName);
          end;
      end;

  acOpenH5M.Enabled := false;
  acReOpenH5M.Enabled := false;

  AppIniOptions.MRUOpenFiles.AppendItem(AFileName);
  AppIniOptions.MRUOpenFiles.RebuildMenuItems(miFiles, acReOpenH5M);
end;

function TMainForm.OpenLUAFile(AFullFilePath: string): TTabSheet;
var
  xTab: TTabSheet;
  xLuaFrame: TLuaFrame;
  xStream: TStream;
begin
  Result := nil;

  if not FH5MManager.IsFilePresent(AFullFilePath) then
    begin
      ShowMessage(DKConsts.SFILE_NOT_FOUND_FMT([AFullFilePath]));
      exit;
    end;

  xStream := FH5MManager.GetFileAsStream(AFullFilePath);
  if xStream = nil then
    begin
      ShowMessage(DKConsts.SOPEN_FILE_ERROR_FMT([AFullFilePath]));
      exit;
    end;

  try
    xTab := TTabSheet.Create(Self);
    xTab.PageControl := jvpgctrlEditors;

    xLuaFrame := TLuaFrame.Create(Self);
    xLuaFrame.Name := GenerateUniqControlName(xLuaFrame);
    xLuaFrame.Align := alClient;
    xLuaFrame.Parent := xTab;
    xLuaFrame.InitWith(FH5MManager, AFullFilePath, xStream, FExtLuaLibs);
    xLuaFrame.OnChangeModified := EditorChangeModified;

    xLuaFrame.synedtScript.PopupMenu := ppmnuEditor;
    xLuaFrame.synedtScript.OnReplaceText := synedtEditorReplaceText;

    xTab.Caption := xLuaFrame.getTitle();
    xTab.ImageIndex := xLuaFrame.getImageIndex();

    FOpenEditors.Add(TEditorPage.Create(xTab, xLuaFrame));

    Result := xTab;
  finally
    xStream.Free;
  end;
end;

function TMainForm.OpenTXTFile(AFullFilePath: string): TTabSheet;
var
  xTab: TTabSheet;
  xTxtFrame: TTxtFrame;
  xText: string;
begin
  Result := nil;

  if not FH5MManager.IsFilePresent(AFullFilePath) then
    begin
      ShowMessage(DKConsts.SFILE_NOT_FOUND_FMT([AFullFilePath]));
      exit;
    end;

  try
    xText := FH5MManager.GetFileAsUnicodeText(AFullFilePath);
  except
    ShowMessage(DKConsts.SOPEN_FILE_ERROR_FMT([AFullFilePath]));
    exit;
  end;

  xTab := TTabSheet.Create(Self);
  xTab.PageControl := jvpgctrlEditors;

  xTxtFrame := TTxtFrame.Create(Self);
  xTxtFrame.Name := GenerateUniqControlName(xTxtFrame);
  xTxtFrame.Align := alClient;
  xTxtFrame.Parent := xTab;
  xTxtFrame.InitWith(FH5MManager, AFullFilePath, xText);
  xTxtFrame.OnChangeModified := EditorChangeModified;

  xTxtFrame.synedtText.PopupMenu := ppmnuEditor;
  xTxtFrame.synedtText.OnReplaceText := synedtEditorReplaceText;

  xTab.Caption := xTxtFrame.getTitle();
  xTab.ImageIndex := xTxtFrame.getImageIndex();

  FOpenEditors.Add(TEditorPage.Create(xTab, xTxtFrame));

  Result := xTab;
end;

function TMainForm.OpenUnkFile(AFullFilePath: string): TTabSheet;
var
  xTab: TTabSheet;
  xUnkFrame: TUnkFrame;
  xStream: TStream;
begin
  Result := nil;

  if not FH5MManager.IsFilePresent(AFullFilePath) then
    begin
      ShowMessage(DKConsts.SFILE_NOT_FOUND_FMT([AFullFilePath]));
      exit;
    end;

  try
    xStream := FH5MManager.GetFileAsStream(AFullFilePath);
  except
    ShowMessage(DKConsts.SOPEN_FILE_ERROR_FMT([AFullFilePath]));
    exit;
  end;
  try
    xTab := TTabSheet.Create(Self);
    xTab.PageControl := jvpgctrlEditors;

    xUnkFrame := TUnkFrame.Create(Self);
    xUnkFrame.Name := GenerateUniqControlName(xUnkFrame);
    xUnkFrame.Align := alClient;
    xUnkFrame.Parent := xTab;
    xUnkFrame.InitWith(FH5MManager, AFullFilePath, xStream);
    xUnkFrame.OnChangeModified := EditorChangeModified;

  finally
    xStream.Free;
  end;

  xTab.Caption := xUnkFrame.getTitle();
  xTab.ImageIndex := xUnkFrame.getImageIndex();

  FOpenEditors.Add(TEditorPage.Create(xTab, xUnkFrame));

  Result := xTab;
end;

procedure TMainForm.OpenXDBMapFile;
var
  xXMLDoc: IXMLDocument;
begin
  xXMLDoc := FH5MManager.GetFileAsXML(FH5MManager.LocateMapXDB());
  if xXMLDoc = nil then
    ShowMessage(DKConsts.SOPEN_FILE_ERROR_FMT([MAP_XDB]))
  else
    begin
      FMapXDB := TMapXDB.Create(FH5MManager);
      FMapXDB.XMLBody := xXMLDoc;
    end;
end;

function TMainForm.OpenXMLFile(AFullFilePath: string): TTabSheet;
var
  xTab: TTabSheet;
  xXMLFrame: TXMLFrame;
  xStream: TStream;
begin
  Result := nil;

  if not FH5MManager.IsFilePresent(AFullFilePath) then
    begin
      ShowMessage(DKConsts.SFILE_NOT_FOUND_FMT([AFullFilePath]));
      exit;
    end;

  xStream := FH5MManager.GetFileAsStream(AFullFilePath);
  if xStream = nil then
    ShowMessage(DKConsts.SOPEN_FILE_ERROR_FMT([AFullFilePath]))
  else
    try
      xTab := TTabSheet.Create(Self);
      xTab.PageControl := jvpgctrlEditors;

      xXMLFrame := TXMLFrame.Create(Self);
      xXMLFrame.Name := GenerateUniqControlName(xXMLFrame);
      xXMLFrame.Align := alClient;
      xXMLFrame.Parent := xTab;
      xXMLFrame.InitWith(AFullFilePath, xStream);
      xXMLFrame.OnChangeModified := EditorChangeModified;

      xXMLFrame.synedtXMLText.PopupMenu := ppmnuEditor;

      xTab.Caption := xXMLFrame.getTitle();
      xTab.ImageIndex := xXMLFrame.getImageIndex();

      FOpenEditors.Add(TEditorPage.Create(xTab, xXMLFrame));

      Result := xTab;
  finally
    xStream.Free;
  end;
end;

procedure TMainForm.synedtEditorReplaceText(Sender: TObject; const ASearch,
  AReplace: string; Line, Column: Integer; var Action: TSynReplaceAction);
var
  Pos: TPoint;
  EditRect: TRect;
  synEdit: TSynEdit;
begin
  synEdit := TSynEdit(Sender);

  if FSearchReplaceOptions.srPromptForReplace then
    begin
      Pos := synEdit.ClientToScreen(synEdit.RowColumnToPixels(synEdit.BufferToDisplayPos(BufferCoord(Column, Line))));
      EditRect := ClientRect;
      EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
      EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);
      ReplaceQueryForm.Prepare(EditRect, Pos.X, Pos.Y, Pos.Y + synEdit.LineHeight, ASearch, AReplace);
      case ReplaceQueryForm.ShowModal of
        mrYes: Action := raReplace;
        mrYesToAll: Action := raReplaceAll;
        mrNo: Action := raSkip;
        else Action := raCancel;
      end;
    end
  else
  if FSearchReplaceOptions.srReplaceAll then
    begin
      Action := raReplaceAll;
    end;
end;

procedure TMainForm.FilterChangeClick(Sender: TObject);
var
  item: TMenuItem;
  xFilter: string;
  ExcludeMode: boolean;
begin
  if (Sender is TMenuItem) then
    begin
      item := (Sender as TMenuItem);
      item.Checked := not item.Checked;
    end;

  BuildFilterFromMenu(xFilter, ExcludeMode);

  H5MFrame1.Filter := xFilter;
  H5MFrame1.ExcludeFilter := ExcludeMode;
  H5MFrame1.Refresh;
end;

end.
