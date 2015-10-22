// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UAppController;

interface

uses
  ComCtrls, UCommonTabbedEditor, SynEdit;

type
  TEditorOption = (edoLuaComment, edoEditable, edoSynEditCompatible);
  TEditorOptions = set of TEditorOption;

  TEditorPage = class
    Page: TTabSheet;
    Editor: ICommonTabbedEditor;
    constructor Create(APage: TTabSheet; AEditor: ICommonTabbedEditor);
  end;

  TAppController = class
  protected
    function GetOpenEditor(Index: integer): TEditorPage;
    function GetOpenEditorsCount: integer;
    function GetActiveEditor: integer;

  public
    property OpenEditors[Index: integer]: TEditorPage read GetOpenEditor;
    property OpenEditorsCount: integer read GetOpenEditorsCount;
    property ActiveEditor: integer read GetActiveEditor;

    function GetActivePageEditor(): TSynEdit;
    function GetActivePageEditorOptions(): TEditorOptions;

    function FindEditorByFileName(AFileName: string): integer;

    procedure OpenFileEvent(AFullFilePath, AFileName: string);
  end;

var
  AppController: TAppController;

implementation

uses
  UMainForm, Forms, ULuaFrame, UTxtFrame, UXMLFrame;

function TAppController.FindEditorByFileName(AFileName: string): integer;
var
  i: integer;
  xEditor: ICommonTabbedEditor;
begin
  Result := -1;
  for i := 0 to OpenEditorsCount - 1 do
    begin
      xEditor := OpenEditors[i].Editor as ICommonTabbedEditor;
      if xEditor.getFullFileName() = AFileName then
        begin
          Result := i;
          break;
        end;
    end;
end;

function TAppController.GetActiveEditor: integer;
begin
  Result := MainForm.ActiveEditor;
end;

function TAppController.GetActivePageEditor: TSynEdit;
var
  xIndex: integer;
  xEditor: TFrame;
begin
  Result := nil;
  
  if OpenEditorsCount = 0 then
    exit;

  xIndex := ActiveEditor;
  if xIndex = -1 then
    exit;

  xEditor := OpenEditors[xIndex].Editor.getFrame;
  if xEditor is TLuaFrame then
    Result := (xEditor as TLuaFrame).synedtScript
  else
  if xEditor is TTxtFrame then
    Result := (xEditor as TTxtFrame).synedtText
  else
  if xEditor is TXMLFrame then
    begin
      if (xEditor as TXMLFrame).JvPageList1.ActivePageIndex = 1 then
        Result := (xEditor as TXMLFrame).synedtXMLText;
    end;
end;

function TAppController.GetActivePageEditorOptions: TEditorOptions;
var
  xIndex: integer;
  xEditor: TFrame;
begin
  Result := [];
  
  if OpenEditorsCount = 0 then
    exit;

  xIndex := ActiveEditor;
  if xIndex = -1 then
    exit;

  xEditor := OpenEditors[xIndex].Editor.getFrame;
  if xEditor is TLuaFrame then
    Result := [edoLuaComment, edoEditable, edoSynEditCompatible]
  else
  if xEditor is TTxtFrame then
    Result := [edoEditable, edoSynEditCompatible]
  else
  if xEditor is TXMLFrame then
    begin
      if (xEditor as TXMLFrame).JvPageList1.ActivePageIndex = 1 then
        Result := [edoSynEditCompatible];
    end;
end;

function TAppController.GetOpenEditor(Index: integer): TEditorPage;
begin
  Result := MainForm.OpenEditors[Index];
end;

function TAppController.GetOpenEditorsCount: integer;
begin
  Result := MainForm.OpenEditorsCount;
end;

procedure TAppController.OpenFileEvent(AFullFilePath, AFileName: string);
begin
  MainForm.OpenFileEvent(AFullFilePath, AFileName);
end;

{ TEditorPage }

constructor TEditorPage.Create(APage: TTabSheet; AEditor: ICommonTabbedEditor);
begin
  Page := APage;
  Editor := AEditor;
end;

initialization
  AppController := TAppController.Create;

finalization
  AppController.Free;
  AppController := nil;

end.
