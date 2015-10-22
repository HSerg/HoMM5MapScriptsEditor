// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UEmbFileChooseDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, UH5MFrame, SynEdit, UH5MManager,
  SynHighlighterLua, SynHighlighterXML, DKLang, UMapManager;

type
  TEmbFileChooseDialog = class(TForm)
    FlowPanel1: TFlowPanel;
    btnCancel: TButton;
    btnChoose: TButton;
    H5MFrame1: TH5MFrame;
    Splitter1: TSplitter;
    Panel1: TPanel;
    lblPreview: TLabel;
    synedtPreview: TSynEdit;
    DKLanguageController1: TDKLanguageController;
    procedure FormCreate(Sender: TObject);
    procedure btnChooseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    synLua: SynHighlighterLua.TSynLuaSyn;
    synXml: SynHighlighterXML.TSynXMLSyn;

  private
    procedure OpenFileEvent(AFullFilePath, AFileName: string);
    procedure ChangeFileEvent(AFullFilePath, AFileName: string);

  public
    H5MManager: TMapManager;
    FileName: string;
    Filter: string;

    procedure InitWith(AH5MManager: TMapManager; AFilter: string);
  end;

function ChooseEmbFileDialog(AH5MManager: TMapManager; AFilter: string): string;

implementation

{$R *.dfm}

function ChooseEmbFileDialog(AH5MManager: TMapManager; AFilter: string): string;
var
  xChooseDLG: TEmbFileChooseDialog;
begin
  Result := '';

  if AH5MManager = nil then
    exit;
  xChooseDLG := TEmbFileChooseDialog.Create(Application);
  try
    xChooseDLG.InitWith(AH5MManager, AFilter);
    if xChooseDLG.ShowModal = mrOk then
      Result := xChooseDLG.FileName;
  finally
    xChooseDLG.Free;
  end;
end;

procedure TEmbFileChooseDialog.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TEmbFileChooseDialog.btnChooseClick(Sender: TObject);
begin
  if (H5MFrame1.GetSelectedPath <> '') then
    begin
      FileName := H5MFrame1.GetSelectedPath;
      ModalResult := mrOk;
    end;
end;

procedure TEmbFileChooseDialog.ChangeFileEvent(AFullFilePath,
  AFileName: string);
begin
  if (H5MFrame1.GetSelectedPath <> '') and H5MManager.IsFilePresent(AFullFilePath) then
    begin
      synedtPreview.Clear;

      if ExtractFileExt(AFileName) = '.lua' then
        synedtPreview.Highlighter := synLua
      else
      if ExtractFileExt(AFileName) = '.xdb' then
        synedtPreview.Highlighter := synXml
      else
        synedtPreview.Highlighter := nil;

      if ExtractFileExt(AFileName) = '.txt' then
        synedtPreview.WordWrap := true
      else
        synedtPreview.WordWrap := false;

      synedtPreview.Text := H5MManager.GetFileAsUnicodeText(AFullFilePath);
    end
  else
    begin
      synedtPreview.Clear;
    end;
end;

procedure TEmbFileChooseDialog.FormCreate(Sender: TObject);
begin
  H5MFrame1.OpenFileEvent := OpenFileEvent;
  H5MFrame1.ChangeFileEvent := ChangeFileEvent;
end;

procedure TEmbFileChooseDialog.FormShow(Sender: TObject);
begin
  if Self.ActiveControl = nil then
    Self.FocusControl(H5MFrame1.vstH5MContent);
end;

procedure TEmbFileChooseDialog.InitWith(AH5MManager: TMapManager; AFilter: string);
begin
  if synLua = nil then
    synLua := SynHighlighterLua.TSynLuaSyn.Create(Self);
  if synXml = nil then
    synXml := SynHighlighterXML.TSynXMLSyn.Create(Self);

  H5MManager := AH5MManager;
  H5MFrame1.InitWith(AH5MManager, AFilter);

  H5MFrame1.vstH5MContent.Selected[H5MFrame1.vstH5MContent.GetFirst] := true;
  H5MFrame1.vstH5MContent.FocusedNode := H5MFrame1.vstH5MContent.GetFirst;
end;

procedure TEmbFileChooseDialog.OpenFileEvent(AFullFilePath, AFileName: string);
begin
  FileName := AFullFilePath;
  ModalResult := mrOk; 
end;

end.
