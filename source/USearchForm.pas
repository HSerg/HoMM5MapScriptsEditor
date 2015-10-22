// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit USearchForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, USearchReplaceOptions, ActnList, DKLang;

type
  TSearchForm = class(TForm)
    Label1: TLabel;
    cboSearchText: TComboBox;
    gbSearchOptions: TGroupBox;
    chkSearchCaseSensitive: TCheckBox;
    chkSearchWholeWords: TCheckBox;
    chkRegularExpression: TCheckBox;
    optDirection: TRadioGroup;
    btnOK: TButton;
    btnCancel: TButton;
    optScope: TRadioGroup;
    optOrigin: TRadioGroup;
    ActionList1: TActionList;
    acOK: TAction;
    DKLanguageController1: TDKLanguageController;
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acOKUpdate(Sender: TObject);
    procedure acOKExecute(Sender: TObject);

  private

  public
    SearchText: String;
    function IsCaseSensitive: Boolean;
    function IsWholeWordOnly: Boolean;
    function IsResgularExpression: Boolean;
    function GetDirection: Integer;
    function GetScope: Integer;
    function GetOrigin: Integer;

    procedure LoadFromOptions(const ASearchOptions: TSearchReplaceOptions);
    procedure SaveToOptions(var ASearchOptions: TSearchReplaceOptions);
  end;

implementation

{$R *.dfm}

uses
  UConsts, UDKConsts;

procedure TSearchForm.acOKExecute(Sender: TObject);
begin
  if cboSearchText.Text <> '' then
    begin
      SearchText := cboSearchText.Text;
      ModalResult := mrOk;
    end
  else
    Application.MessageBox(PAnsiChar(DKConsts.SBLANK_SEARCH_STRING_WARNING), APPLICATION_NAME, MB_OK+MB_ICONERROR);
end;

procedure TSearchForm.acOKUpdate(Sender: TObject);
begin
  acOK.Enabled := (cboSearchText.Text <> '');
end;

procedure TSearchForm.btnCancelClick(Sender: TObject);
begin
  SearchText := '';
  ModalResult := mrCancel;
end;

function TSearchForm.IsCaseSensitive: Boolean;
begin
  Result := chkSearchCaseSensitive.Checked;
end;

function TSearchForm.IsWholeWordOnly: Boolean;
begin
  Result := chkSearchWholeWords.Checked;
end;

function TSearchForm.IsResgularExpression: Boolean;
begin
  Result := chkRegularExpression.Checked;
end;

function TSearchForm.GetDirection: Integer;
begin
  Result := optDirection.ItemIndex;
end;

function TSearchForm.GetScope: Integer;
begin
  Result := optScope.ItemIndex;
end;

function TSearchForm.GetOrigin: Integer;
begin
  Result := optOrigin.ItemIndex;
end;

procedure TSearchForm.LoadFromOptions(const ASearchOptions: TSearchReplaceOptions);
begin
  chkRegularExpression.Checked := ASearchOptions.srSearchRegularExpression;
  chkSearchCaseSensitive.Checked := ASearchOptions.srSearchSensitive;
  chkSearchWholeWords.Checked := ASearchOptions.srSearchWholeWords;
  optOrigin.ItemIndex := ASearchOptions.srSearchOrigin;
  optScope.ItemIndex := ASearchOptions.srSearchScope;
  optDirection.ItemIndex := ASearchOptions.srSearchDirection;
  cboSearchText.Items.Clear;
  cboSearchText.Items.AddStrings(ASearchOptions.SearchedText);
  if ASearchOptions.SearchedText.Count > 0 then
    cboSearchText.Text := ASearchOptions.SearchedText.Strings[ASearchOptions.SearchedText.Count - 1];
end;

procedure TSearchForm.SaveToOptions(var ASearchOptions: TSearchReplaceOptions);
var
  Index: integer;
begin
  ASearchOptions.srSearchRegularExpression := IsResgularExpression;
  ASearchOptions.srSearchSensitive := IsCaseSensitive;
  ASearchOptions.srSearchWholeWords := IsWholeWordOnly;
  ASearchOptions.srSearchOrigin := GetOrigin;
  ASearchOptions.srSearchScope := GetScope;
  ASearchOptions.srSearchDirection := GetDirection;
  ASearchOptions.sSearchString := SearchText;
  ASearchOptions.SearchedText.Sort;
  if not ASearchOptions.SearchedText.Find(ASearchOptions.sSearchString, Index) then
    ASearchOptions.SearchedText.Add(ASearchOptions.sSearchString);
end;

procedure TSearchForm.FormCreate(Sender: TObject);
begin
  SearchText := '';
end;

procedure TSearchForm.FormShow(Sender: TObject);
begin
  cboSearchText.SetFocus;
end;

end.
