// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UAboutForm;

interface

uses
  Forms, Classes, StdCtrls, ExtCtrls, Controls, Graphics, DKLang;

type
  TAboutForm = class(TForm)
    GridPanel1: TGridPanel;
    lblTitle: TLabel;
    lblVersion: TLabel;
    lblAuthor: TLabel;
    lblSK: TLabel;
    lblCoAuthors: TLabel;
    lblLE: TLabel;
    lblRO: TLabel;
    Image1: TImage;
    Button1: TButton;
    DKLanguageController1: TDKLanguageController;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DKLanguageController1LanguageChanged(Sender: TObject);

  private
    procedure UpdateVersionInfo();

  public

  end;

implementation

{$R *.dfm}

uses
  SysUtils, JclFileUtils, UDKConsts;

procedure TAboutForm.Button1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TAboutForm.DKLanguageController1LanguageChanged(Sender: TObject);
const
  HSerg_RU = '    Сергей А. Клочков (aka HSerg)';
  HSerg_EN = '    Sergey A. Klochkov (aka HSerg)';
  Valex_RU = '    Алексей Назаров (aka Valex)';
  Valex_EN = '    Alexey Nazarov (aka Valex)';
  Romzes_RU = '    Роман Бурлаченко (aka Romzes)';
  Romzes_EN = '    Roman Burlachenko (aka Romzes)';
begin
  UpdateVersionInfo();

  if DKLang.LangManager.LanguageID = 1049 then
    begin
      lblSK.Caption := HSerg_RU;
      lblLE.Caption := Valex_RU;
      lblRO.Caption := Romzes_RU;
    end
  else
    begin
      lblSK.Caption := HSerg_EN;
      lblLE.Caption := Valex_EN;
      lblRO.Caption := Romzes_EN;
    end;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  UpdateVersionInfo();
end;

procedure TAboutForm.UpdateVersionInfo;
var
  xVersion: string;
begin
  xVersion := VersionFixedFileInfoString(Application.ExeName);
  lblVersion.Caption := Format('    %s: %s', [DKConsts.SVERSION, xVersion]);
end;

end.
