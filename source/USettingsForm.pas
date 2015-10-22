// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit USettingsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, JvExControls, JvPageList, ComCtrls, JvExComCtrls,
  JvPageListTreeView, ActnList, StdCtrls, TntStdCtrls, DKLang, Mask, JvExMask,
  JvToolEdit;

type
  TSettingsForm = class(TForm)
    JvPageListTreeView1: TJvPageListTreeView;
    JvPageList1: TJvPageList;
    pnlButtons: TPanel;
    Splitter1: TSplitter;
    Button1: TButton;
    Button2: TButton;
    ActionList1: TActionList;
    acOK: TAction;
    acCancel: TAction;
    Bevel1: TBevel;
    jvspCommons: TJvStandardPage;
    jvspEmpty: TJvStandardPage;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    RadioGroup1: TRadioGroup;
    jvspLangs: TJvStandardPage;
    GroupBox2: TGroupBox;
    tntlstbxLang: TTntListBox;
    DKLanguageController1: TDKLanguageController;
    jvspLua: TJvStandardPage;
    GroupBox3: TGroupBox;
    CheckBox4: TCheckBox;
    jvspPath: TJvStandardPage;
    GroupBox4: TGroupBox;
    JvDirectoryEdit1: TJvDirectoryEdit;
    lblHMM5Path: TLabel;
    lblHMMVersion: TLabel;
    CheckBox3: TCheckBox;
    procedure acOKExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JvDirectoryEdit1Change(Sender: TObject);

  private
    procedure RegisterPage(AText: string; APage: integer);

  private
    FLangs: array of record Name: string; ID: integer; end;
    FLangIndex: integer;

  public
    procedure Init();
    
  end;

implementation

{$R *.dfm}

uses
  UOptionsUnit, UResourcesCache, UDKConsts;

procedure TSettingsForm.acCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSettingsForm.acOKExecute(Sender: TObject);
begin
  AppIniOptions.Path.HMM5Path := JvDirectoryEdit1.Text;

  AppIniOptions.StartUp.OpenMapXDB := CheckBox1.Checked;
  AppIniOptions.StartUp.OpenMapScriptLUA := CheckBox2.Checked;
  AppIniOptions.StartUp.OpenFolders := TStartUpOpenFolders(RadioGroup1.ItemIndex);

  AppIniOptions.Common.LANG := FLangs[tntlstbxLang.ItemIndex].ID;
  LangManager.LanguageID := AppIniOptions.Common.LANG;

  AppIniOptions.LuaEditor.UseGetMapDataPath := CheckBox4.Checked;
  AppIniOptions.LuaEditor.UseLongParamsDesc := CheckBox3.Checked;

  ModalResult := mrOk;
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  RegisterPage(DKConsts.SOPTIONS_PATHS, jvspPath.PageIndex);
  RegisterPage(DKConsts.SOPTIONS_COMMONS, jvspCommons.PageIndex);
  RegisterPage(DKConsts.SOPTIONS_EDITORS, jvspEmpty.PageIndex);
  RegisterPage(DKConsts.SOPTIONS_EDITORS+'\*.lua', jvspLua.PageIndex);
  RegisterPage(DKConsts.SOPTIONS_EDITORS+'\*.txt', jvspEmpty.PageIndex);
  RegisterPage(DKConsts.SOPTIONS_EDITORS+'\*.xdb, *.xml', jvspEmpty.PageIndex);
  RegisterPage(DKConsts.SOPTIONS_EDITORS+'\*.*', jvspEmpty.PageIndex);
  RegisterPage(DKConsts.SOPTIONS_LANG, jvspLangs.PageIndex);

  JvPageListTreeView1.FullExpand();
end;

procedure TSettingsForm.Init;
var
  i: Integer;
begin
  FLangIndex := -1;
  SetLength(FLangs, LangManager.LanguageCount);
  for i := 0 to LangManager.LanguageCount - 1 do
    begin
      FLangs[i].Name := LangManager.LanguageNames[i];
      FLangs[i].ID := LangManager.LanguageIDs[i];
      if FLangs[i].ID = LangManager.LanguageID then
        FLangIndex := i;
    end;
  if FLangIndex = -1 then
    FLangIndex := 0;

  JvDirectoryEdit1.Text := AppIniOptions.Path.HMM5Path;

  CheckBox1.Checked := AppIniOptions.StartUp.OpenMapXDB;
  CheckBox2.Checked := AppIniOptions.StartUp.OpenMapScriptLUA;
  RadioGroup1.ItemIndex := Integer(AppIniOptions.StartUp.OpenFolders);

  tntlstbxLang.Clear;
  for i := 0 to Length(FLangs) - 1 do
    tntlstbxLang.Items.Add(FLangs[i].Name);
  tntlstbxLang.ItemIndex := FLangIndex;

  CheckBox4.Checked := AppIniOptions.LuaEditor.UseGetMapDataPath;
  CheckBox3.Checked := AppIniOptions.LuaEditor.UseLongParamsDesc;
end;

procedure TSettingsForm.JvDirectoryEdit1Change(Sender: TObject);
var
  hmm5Version: THMM5Version;
  xVersionTitle: string;
begin
  hmm5Version := DetectHMM5Version(JvDirectoryEdit1.Text);
  case hmm5Version of
    hmmOriginal :
      xVersionTitle := DKConsts.SHMM5_TITLE;
    hmmAddon1   :
      xVersionTitle := DKConsts.SHMM5A1_TITLE;
    hmmAddon2   :
      xVersionTitle := DKConsts.SHMM5A2_TITLE;
    hmmUnknown  :
      xVersionTitle := DKConsts.SUNKNOWN;
  end;
  lblHMMVersion.Caption := Format(' * %s: %s', [DKConsts.SVERSION, xVersionTitle]);
end;

procedure TSettingsForm.RegisterPage(AText: string; APage: integer);
var
  xString: TStringList;
  node: TJvPageIndexNode;
begin
  xString := TStringList.Create;
  try
    xString.StrictDelimiter := true;
    xString.Delimiter := '\';
    xString.DelimitedText := AText;

    if xString.Count = 2 then
      begin
        node := TJvPageIndexNode(JvPageListTreeView1.Items.GetFirstNode);
        while node <> nil do
          begin
            if xString[0] = node.Text then
              break;
            node := TJvPageIndexNode(node.GetNext);
          end;
        TJvPageIndexNode(JvPageListTreeView1.Items.AddChild(node, xString[1])).PageIndex := APage
      end
    else
      begin
        TJvPageIndexNode(JvPageListTreeView1.Items.AddChild(nil, AText)).PageIndex := APage;  
      end;
  finally
    xString.Free;
  end;
end;

end.

