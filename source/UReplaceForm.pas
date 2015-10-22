// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UReplaceForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, USearchReplaceOptions, ActnList, DKLang;

type
  TReplaceForm = class(TForm)
    Label1: TLabel;
    cboSearchText: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    Label2: TLabel;
    cboReplaceText: TComboBox;
    optScope: TRadioGroup;
    optOrigin: TRadioGroup;
    optDirection: TRadioGroup;
    gbSearchOptions: TGroupBox;
    chkSearchCaseSensitive: TCheckBox;
    chkSearchWholeWords: TCheckBox;
    chkRegularExpression: TCheckBox;
    btnReplaceAll: TButton;
    chkPrompt: TCheckBox;
    ActionList1: TActionList;
    acOk: TAction;
    acReplaceAll: TAction;
    DKLanguageController1: TDKLanguageController;
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acOkExecute(Sender: TObject);
    procedure acReplaceAllExecute(Sender: TObject);
    procedure acOkUpdate(Sender: TObject);
    procedure acReplaceAllUpdate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SearchText: String;
    ReplaceText: String;
    function IsCaseSensitive: Boolean;
    function IsWholeWordOnly: Boolean;
    function IsResgularExpression: Boolean;
    function IsReplaceAll: Boolean;
    function IsPromptForReplace: Boolean;
    function GetDirection: Integer;
    function GetScope: Integer;
    function GetOrigin: Integer;

    procedure LoadFromOptions(const ASearchOptions: TSearchReplaceOptions);
    procedure SaveToOptions(var ASearchOptions: TSearchReplaceOptions);
  end;

var
  ReplaceForm: TReplaceForm;
  bReplaceAll: Boolean;

implementation

{$R *.dfm}

uses
  UConsts, UDKConsts;

function TReplaceForm.IsCaseSensitive: Boolean;
begin
  Result := chkSearchCaseSensitive.Checked;
end;

function TReplaceForm.IsWholeWordOnly: Boolean;
begin
  Result := chkSearchWholeWords.Checked;
end;

procedure TReplaceForm.LoadFromOptions(
  const ASearchOptions: TSearchReplaceOptions);
begin
  chkRegularExpression.Checked := ASearchOptions.srSearchRegularExpression;
  chkSearchCaseSensitive.Checked := ASearchOptions.srSearchSensitive;
  chkSearchWholeWords.Checked := ASearchOptions.srSearchWholeWords;
  optOrigin.ItemIndex := ASearchOptions.srSearchOrigin;
  optScope.ItemIndex := ASearchOptions.srSearchScope;
  optDirection.ItemIndex := ASearchOptions.srSearchDirection;

  cboSearchText.Items.Clear;
  cboSearchText.Items.AddStrings(ASearchOptions.SearchedText);

  cboReplaceText.Clear;
  cboReplaceText.Items.AddStrings(ASearchOptions.ReplacedText);

  if ASearchOptions.SearchedText.Count > 0 then
    cboSearchText.Text := ASearchOptions.SearchedText.Strings[ASearchOptions.SearchedText.Count - 1];
end;

procedure TReplaceForm.SaveToOptions(var ASearchOptions: TSearchReplaceOptions);
var
  Index: integer;
begin
  ASearchOptions.srSearchRegularExpression := IsResgularExpression;
  ASearchOptions.srReplaceAll := IsReplaceAll;
  ASearchOptions.srPromptForReplace := IsPromptForReplace;
  ASearchOptions.srSearchSensitive := IsCaseSensitive;
  ASearchOptions.srSearchWholeWords := IsWholeWordOnly;
  ASearchOptions.srSearchOrigin := GetOrigin;
  ASearchOptions.srSearchScope := GetScope;
  ASearchOptions.srSearchDirection := GetDirection;
  ASearchOptions.sSearchString := SearchText;
  ASearchOptions.sReplaceString := ReplaceText;

  ASearchOptions.ReplacedText.Sort;
  if not ASearchOptions.ReplacedText.Find(ASearchOptions.sReplaceString, Index) then
    ASearchOptions.ReplacedText.Add(ASearchOptions.sReplaceString);

  ASearchOptions.SearchedText.Sort;
  if not ASearchOptions.SearchedText.Find(ASearchOptions.sSearchString, Index) then
    ASearchOptions.SearchedText.Add(ASearchOptions.sSearchString);
end;

function TReplaceForm.IsResgularExpression: Boolean;
begin
  Result := chkRegularExpression.Checked;
end;

function TReplaceForm.IsReplaceAll: Boolean;
begin
  Result := bReplaceAll;
end;

function TReplaceForm.IsPromptForReplace: Boolean;
begin
  Result := chkPrompt.Checked
end;

function TReplaceForm.GetDirection: Integer;
begin
  Result := optDirection.ItemIndex;
end;

function TReplaceForm.GetScope: Integer;
begin
  Result := optScope.ItemIndex;
end;

function TReplaceForm.GetOrigin: Integer;
begin
  Result := optOrigin.ItemIndex;
end;

procedure TReplaceForm.acOkExecute(Sender: TObject);
begin
  if cboSearchText.Text <> '' then
    begin
      SearchText := cboSearchText.Text;
      ReplaceText := cboReplaceText.Text;
      Self.Close;
    end
  else
    begin
      Application.MessageBox(PAnsiChar(DKConsts.SBLANK_SEARCH_STRING_WARNING), APPLICATION_NAME, MB_OK+MB_ICONERROR);
    end;
end;

procedure TReplaceForm.acOkUpdate(Sender: TObject);
begin
  acOK.Enabled := (cboSearchText.Text <> '');
end;

procedure TReplaceForm.acReplaceAllUpdate(Sender: TObject);
begin
  acReplaceAll.Enabled := (cboSearchText.Text <> '');
end;

procedure TReplaceForm.acReplaceAllExecute(Sender: TObject);
begin
  bReplaceAll := True;
  acOk.Execute();
end;

procedure TReplaceForm.btnCancelClick(Sender: TObject);
begin
  ReplaceText := '';
  SearchText := '';
  Self.Close;
end;

procedure TReplaceForm.FormCreate(Sender: TObject);
begin
  bReplaceAll := False;
  ReplaceText := '';
  SearchText := '';
end;

procedure TReplaceForm.FormShow(Sender: TObject);
begin
  cboSearchText.SetFocus;
end;

end.
