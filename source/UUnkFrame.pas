// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UUnkFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, UCommonTabbedEditor, UCommonDM, UH5MManager, StdCtrls, ExtCtrls,
  ATBinHex, UCommonTabbedEditorFrame, DKLang, UMapManager;

type
  TUnkFrame = class(TCommonTabbedEditorFrame)
    GroupBox2: TGroupBox;
    FlowPanel1: TFlowPanel;
    chkModeText: TRadioButton;
    chkModeBinary: TRadioButton;
    chkModeHex: TRadioButton;
    chkModeUnicode: TRadioButton;
    GroupBox1: TGroupBox;
    Viewer: TATBinHex;
    DKLanguageController1: TDKLanguageController;
    procedure chkModeTextClick(Sender: TObject);
  private
    FH5MManager: TMapManager;
  private
    FFileName: string;
    FContent: TStream;

  public
    destructor Destroy(); override;

    function getFullFileName(): string; override;
    function getImageIndex(): integer; override;
    function getModified(): boolean; override;

    function SaveChanges(): boolean; override;

    procedure InitWith(AH5MManager: TMapManager; AFileName: string; DataStream: TStream);
  end;

implementation

{$R *.dfm}

{ TUnkFrame }

function TUnkFrame.getModified: boolean;
begin
  Result := false;
end;

procedure TUnkFrame.chkModeTextClick(Sender: TObject);
begin
  case (Sender as TControl).Tag of
    0: Viewer.Mode := vbmodeText;
    1: Viewer.Mode := vbmodeBinary;
    2: Viewer.Mode := vbmodeHex;
    3: Viewer.Mode := vbmodeUnicode;
  end;
end;

destructor TUnkFrame.Destroy;
begin
  inherited;
  FContent.Free;
end;

function TUnkFrame.getFullFileName: string;
begin
  Result := FFileName;
end;

function TUnkFrame.getImageIndex: integer;
begin
  Result := 4;
end;

procedure TUnkFrame.InitWith(AH5MManager: TMapManager; AFileName: string;
  DataStream: TStream);
begin
  FH5MManager := AH5MManager;

  FFileName := AFileName;

  FContent := TMemoryStream.Create;
  FContent.CopyFrom(DataStream, 0);

  Viewer.OpenStream(FContent, true);

  chkModeText.Checked := true;
end;

function TUnkFrame.SaveChanges: boolean;
begin
  Result := false;
end;

end.
