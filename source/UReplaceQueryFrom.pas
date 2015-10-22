// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UReplaceQueryFrom;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DKLang;

type
  TReplaceQueryForm = class(TForm)
    lblConfirmation: TLabel;
    imgIcon: TImage;
    btnReplace: TButton;
    btnSkip: TButton;
    btnCancel: TButton;
    btnReplaceAll: TButton;
    DKLanguageController1: TDKLanguageController;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Prepare(EditorRect: TRect; X, Y1, Y2: Integer; sSearchText, sReplaceText: string);
  end;

var
  ReplaceQueryForm: TReplaceQueryForm;

implementation

{$R *.dfm}

uses
  UDKConsts;

procedure TReplaceQueryForm.Prepare(EditorRect: TRect; X, Y1, Y2: Integer; sSearchText, sReplaceText: string);
var
  nW, nH: integer;
begin
  imgIcon.Picture.Icon.Handle := LoadIcon(0, IDI_QUESTION);
  lblConfirmation.Caption := DKConsts.SASK_REPLACE_TEXT_FMT([sSearchText, sReplaceText]);
  nW := EditorRect.Right - EditorRect.Left;
  nH := EditorRect.Bottom - EditorRect.Top;

  if nW <= Width then
    X := EditorRect.Left - (Width - nW) div 2
  else
    begin
      if X + Width > EditorRect.Right then
        X := EditorRect.Right - Width;
    end;

  if Y2 > EditorRect.Top + MulDiv(nH, 2, 3) then
    Y2 := Y1 - Height - 4
  else
    Inc(Y2, 4);
    
  SetBounds(X, Y2, Width, Height);
end;

end.
