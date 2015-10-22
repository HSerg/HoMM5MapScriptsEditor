// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UTxtFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit, ComCtrls, ToolWin, ActnList, UCommonDM, UH5MManager,
  UCommonTabbedEditor, UCommonTabbedEditorFrame, StdCtrls, DKLang,
  UMapManager, Menus, SynEditKeyCmds;

type
  TTxtFrame = class(TCommonTabbedEditorFrame)
    synedtText: TSynEdit;
    ActionList1: TActionList;
    acTextReload: TAction;
    acSave: TAction;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    acWordWrap: TAction;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    DKLanguageController1: TDKLanguageController;
    ppmnuSuggestions: TPopupMenu;
    piAddToDictionary: TMenuItem;
    N1: TMenuItem;
    acAddToDictionary: TAction;
    procedure acSaveExecute(Sender: TObject);
    procedure acTextReloadExecute(Sender: TObject);
    procedure synedtTextChange(Sender: TObject);
    procedure acWordWrapExecute(Sender: TObject);
    procedure acAddToDictionaryExecute(Sender: TObject);
    procedure synedtTextCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
    
  private
    FH5MManager: TMapManager;

  private
    FSuggList: TStringList;
    FCommonPopupMenu: TPopupMenu;

  private
    FFileName: string;
    FText: string;

  protected
    procedure SuggestionOnClick(Sender: TObject);
    procedure EditorContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);

  public
    destructor Destroy(); override;

    function getFullFileName(): string; override;
    function getImageIndex(): integer; override;
    function getModified(): boolean; override;

    function SaveChanges(): boolean; override;

    procedure InitWith(AH5MManager: TMapManager; AFileName: string; AText: string);

    property Editor: TSynEdit read synedtText;
  end;

implementation

{$R *.dfm}

uses
  SynSpellCheck, UConsts, UDKConsts, USynEditHelper;

var
  synSpellCheck: TSynSpellCheck;

procedure TTxtFrame.acAddToDictionaryExecute(Sender: TObject);
var
  word: string;
begin
  word := synedtText.GetWordAtRowCol(synedtText.CaretXY);
  synSpellCheck.AddDictWord(word);
  synedtText.Invalidate;
end;

procedure TTxtFrame.acSaveExecute(Sender: TObject);
begin
  FH5MManager.SetFileAsUnicodeText(FFileName, Editor.Text);
  FText := Editor.Text;
  DoChangeModified;
end;

procedure TTxtFrame.acTextReloadExecute(Sender: TObject);
begin
  if MessageDlg(DKConsts.SFILE_UNDO_CHANGES, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Editor.Text := FText;
    end;
  DoChangeModified;
end;

procedure TTxtFrame.acWordWrapExecute(Sender: TObject);
begin
  synedtText.WordWrap := acWordWrap.Checked;
end;

destructor TTxtFrame.Destroy;
begin
  FreeAndNil(FSuggList);
  synSpellCheck.RemoveEditor(synedtText);

  inherited;
end;

procedure TTxtFrame.EditorContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  word: string;
  mnu: TMenuItem;
  i: Integer;
  suggList: TStringList;
  xVisible: boolean;
begin
  if not Assigned(FCommonPopupMenu) then
    FCommonPopupMenu := synedtText.PopupMenu;

  word := Trim(synedtText.GetWordAtRowCol(synedtText.CaretXY));
  xVisible := (word <> '') and (not synSpellCheck.CheckWord(word));

  if not xVisible then
    begin
      if Assigned(synedtText.PopupMenu) then
        synedtText.PopupMenu := FCommonPopupMenu;
      exit;
    end;

  while ppmnuSuggestions.Items.Count > 2 do
    ppmnuSuggestions.Items[ppmnuSuggestions.Items.Count - 1].Free;

  suggList := TStringList.Create;
  try
    suggList.Clear;
    if synSpellCheck.GetSuggestions(word, suggList) > 0 then
      for i := 0 to suggList.Count - 1 do
        if suggList.Strings[i] <> word then
          begin
            mnu := TMenuItem.Create(Self);
            ppmnuSuggestions.Items.Add(mnu);
            mnu.Caption := suggList.Strings[i];
            mnu.OnClick := SuggestionOnClick;
          end
  finally
    FreeAndNil(suggList);
  end;

  acAddToDictionary.Caption := DKConsts.SADD_WORD_TO_DICT_FMT([word]);

  // бывают исключения с апострофами

  if ppmnuSuggestions.Items.Count = 2 then
    begin
      if Assigned(synedtText.PopupMenu) then
        synedtText.PopupMenu := FCommonPopupMenu;
      exit;
    end;

  synedtText.PopupMenu := ppmnuSuggestions;
end;

function TTxtFrame.getFullFileName: string;
begin
  Result := FFileName;
end;

function TTxtFrame.getImageIndex: integer;
begin
  Result := 1;
end;

function TTxtFrame.getModified: boolean;
begin
  getModified := (synedtText.Text <> FText);
end;

procedure TTxtFrame.InitWith(AH5MManager: TMapManager; AFileName: string; AText: string);
begin
  FH5MManager := AH5MManager;

  FFileName := AFileName;

  synedtText.Clear;
  synedtText.Text := AText;
  synedtText.OnContextPopupHack := EditorContextPopup;

  FText := synedtText.Text;

  FSuggList := TStringList.Create;

  synSpellCheck.AddEditor(synedtText);
end;

procedure TTxtFrame.synedtTextChange(Sender: TObject);
begin
  DoChangeModified();
end;

procedure TTxtFrame.synedtTextCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
begin
  if Command = ecPaste then
    synedtText.InvalidateLines(-1, -1);
end;

function TTxtFrame.SaveChanges: boolean;
begin
  Result := acSave.Execute();
end;

procedure TTxtFrame.SuggestionOnClick(Sender: TObject);
begin
  synedtText.SetSelWord();
//  synedtText.BlockBegin := synedtText.WordStartEx(synedtText.CaretXY);
//  synedtText.BlockEnd := synedtText.WordEndEx(synedtText.CaretXY);
//  synSpellCheck.SelectWordAtCursor;
  synedtText.SelText := StringReplace(TMenuItem(Sender).Caption, '&', '', []);
end;

initialization
  synSpellCheck := TSynSpellCheck.Create(nil);
  synSpellCheck.Options := [sscoAutoSpellCheck, sscoIgnoreSingleChars, sscoIgnoreWordsWithNumbers, sscoSuggestWords];
  synSpellCheck.UseUserDictionary := true;
  synSpellCheck.DictionaryPath := ExtractFilePath(Application.ExeName) + DEFAULT_DICTIONARIES_DIR;
  synSpellCheck.UserDirectory := ExtractFilePath(Application.ExeName) + DEFAULT_DICTIONARIES_DIR;
  synSpellCheck.LoadDictionary('ru_RU');

finalization
  synSpellCheck.SaveUserDictionary();
  FreeAndNil(synSpellCheck);

end.
