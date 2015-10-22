// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UCommonTabbedEditorFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, UCommonTabbedEditor;

type
  TCommonTabbedEditorFrame = class(TFrame, ICommonTabbedEditor)
  protected
    FOnChangeModified: TNotifyEvent;
    function getChangeModified(): TNotifyEvent; virtual;
    procedure setChangeModified(ANotifyEvent: TNotifyEvent); virtual;

    procedure DoChangeModified(); virtual;

  public
    function SaveChanges(): boolean; virtual; abstract;

    function getTitle(): string; virtual;
    function getFullFileName(): string; virtual; abstract;
    function getImageIndex(): integer; virtual; 
    function getModified(): boolean; virtual; abstract;
    function getFrame: TFrame; virtual;

    property OnChangeModified: TNotifyEvent read getChangeModified write setChangeModified;
  end;

implementation

{$R *.dfm}

uses
  StrUtils;

{ TCommonTabbedEditorFrame }

procedure TCommonTabbedEditorFrame.DoChangeModified;
begin
  if Assigned(FOnChangeModified) then
    FOnChangeModified(Self);
end;

function TCommonTabbedEditorFrame.getChangeModified: TNotifyEvent;
begin
  Result := FOnChangeModified;
end;

function TCommonTabbedEditorFrame.getFrame: TFrame;
begin
  Result := Self;
end;

function TCommonTabbedEditorFrame.getImageIndex: integer;
begin
  Result := -1;
end;

procedure TCommonTabbedEditorFrame.setChangeModified(
  ANotifyEvent: TNotifyEvent);
begin
  FOnChangeModified := ANotifyEvent;
end;

function TCommonTabbedEditorFrame.getTitle: string;
begin
  Result := ExtractFileName(StringReplace(getFullFileName(), '/', '\', [rfReplaceAll]));
end;

end.
