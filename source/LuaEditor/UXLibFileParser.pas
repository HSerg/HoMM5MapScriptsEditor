// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UXLibFileParser;

interface

uses
  Classes, Contnrs, XMLIntf, UCommonLibDefs, UAbstractLibParser;

type
  TXLibRootTableItemsArray = array of TXLibRootTableItem;
  TXLibFileParser = class(TAbstractLibParser)
  protected
    FTokenClassList: array of record
                                TokenName: string;
                                TokenClass: TXLibRootTableItemClass;
                              end;
    FItems: TObjectList;
    FLibTable: TStringList;
    FXMLDocument: IXMLDocument;
    FLibName: string;

    function checkTokenNode(node: IXMLNode): boolean;
    function getTokenNodeClass(node: IXMLNode): TXLibRootTableItemClass;
    function createLibItemsByTokenNode(node: IXMLNode): TXLibRootTableItemsArray;

  protected
    function GetItems(Index: integer): TXLibRootTableItem; override;
    function GetLibName: string; override;
    function GetLibTable: TStringList; override;

  public
    constructor Create;
    destructor Destroy; override;

  public
    procedure Parse(ADataStream: TStream; ADefaultLibName: string); override;
    function Count: integer; override;

    property XMLDocument: IXMLDocument read FXMLDocument;
  end;

implementation

uses
  Windows, StrUtils, SysUtils, XMLDoc, ActiveX;

const
  FUNCTION_NODE = 'function';
  VARIABLE_NODE = 'variable';
  LIBRARY_NODE = 'library';

function TXLibFileParser.checkTokenNode(node: IXMLNode): boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to Length(FTokenClassList) - 1 do
    if AnsiSameText(node.NodeName, FTokenClassList[i].TokenName) then
      begin
        Result := true;
        break;
      end;
end;

function TXLibFileParser.getTokenNodeClass(node: IXMLNode): TXLibRootTableItemClass;
var
  i: Integer;
  nodeName: string;
begin
  Result := nil;
  nodeName := node.NodeName;
  for i := 0 to Length(FTokenClassList) - 1 do
    if AnsiSameText(nodeName, FTokenClassList[i].TokenName) then
      begin
        Result := FTokenClassList[i].TokenClass;
        break;
      end;
end;

function TXLibFileParser.Count: integer;
begin
  Result := FItems.Count;
end;

constructor TXLibFileParser.Create;
begin
  FLibType := lltXLib;
  FLibTable := TStringList.Create;
  FItems := TObjectList.Create(true);
  FXMLDocument := nil;
  
  SetLength(FTokenClassList, 3);
  FTokenClassList[0].TokenName := FUNCTION_NODE;
  FTokenClassList[0].TokenClass := TXFuncItem;
  FTokenClassList[1].TokenName := VARIABLE_NODE;
  FTokenClassList[1].TokenClass := TXVarItem;
  FTokenClassList[2].TokenName := LIBRARY_NODE;
  FTokenClassList[2].TokenClass := TXLibItem;
end;

function TXLibFileParser.createLibItemsByTokenNode(node: IXMLNode): TXLibRootTableItemsArray;
var
  tokenNodeClass: TXLibRootTableItemClass;
  item: TXLibRootTableItem;
  aliases: TStrings;
  i: Integer;
begin
  Result := nil;
  tokenNodeClass := getTokenNodeClass(node);
  if tokenNodeClass<>nil then
    begin
      item := tokenNodeClass.Create;
      item.LoadFromNode(node);
      if item.GetAliases() = false then
        begin
          SetLength(Result, 1);
          Result[0] := item;
          exit;
        end;
      aliases := TStringList.Create;
      try
        item.GetAliases(aliases);
        SetLength(Result, aliases.Count+1);
        Result[0] := item;
        for i := 0 to aliases.Count - 1 do
          begin
            item := tokenNodeClass.Create;
            item.usedAlias := aliases[i];
            item.LoadFromNode(node);
            Result[i+1] := item;
          end;
      finally
        aliases.Free;
      end;
    end;
end;

destructor TXLibFileParser.Destroy;
begin
  FXMLDocument := nil;
  FItems.Free;
  FLibTable.Free;
end;

procedure TXLibFileParser.Parse(ADataStream: TStream; ADefaultLibName: string);
var
  entryNode: IXMLNode;
  i, j: Integer;
  items: TXLibRootTableItemsArray;
begin
  FLibTable.Clear;
  FItems.Clear;

  FXMLDocument := TXMLDocument.Create(nil);
  FXMLDocument.Options := [doNodeAutoIndent];
  FXMLDocument.ParseOptions := [poValidateOnParse, poPreserveWhiteSpace];
  FXMLDocument.LoadFromStream(ADataStream);

  FLibName := FXMLDocument.DocumentElement.Attributes['name'];
  if FLibName = '' then
      FLibName := ADefaultLibName;
//    FLibName := ChangeFileExt(ExtractFileName(AFileName), '');

  for i := 0 to FXMLDocument.DocumentElement.ChildNodes.Count - 1 do
    begin
      entryNode := FXMLDocument.DocumentElement.ChildNodes[i];

      if checkTokenNode(entryNode) then
        begin
          items := createLibItemsByTokenNode(entryNode);
          for j := 0 to Length(items) - 1 do
            FItems.Add(items[j]);
        end;
    end;
end;

function TXLibFileParser.GetItems(Index: integer): TXLibRootTableItem;
begin
  Result := FItems[Index] as TXLibRootTableItem;
end;

function TXLibFileParser.GetLibName: string;
begin
  Result := FLibName;
end;

function TXLibFileParser.GetLibTable: TStringList;
begin
  Result := FLibTable;
end;

initialization
// -- портит диалоги выбора файла :\
//  CoInitializeEx(NIL, 0); (или COINIT_MULTITHREADED)
  CoInitializeEx(NIL, COINIT_APARTMENTTHREADED or COINIT_SPEED_OVER_MEMORY);

finalization
  CoUninitialize();

end.
