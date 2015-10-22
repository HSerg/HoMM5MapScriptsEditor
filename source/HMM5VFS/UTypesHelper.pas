// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UTypesHelper;

interface

uses
  XmlIntf;

function FindTableNodeByXPointerSubstring(xmlTypes: IXMLDocument; xPointerSubstring: string): IXMLNode;

implementation

uses
  StrUtils;

function FindTableNodeByXPointerSubstring(xmlTypes: IXMLDocument; xPointerSubstring: string): IXMLNode;
var
  itemsNodeList: IXMLNodeList;
    i: Integer;
begin
  Result := nil;
  itemsNodeList := xmlTypes.DocumentElement.ChildNodes.FindNode('Tables').ChildNodes;
  for i := 0 to itemsNodeList.Count - 1 do
    if AnsiContainsText(itemsNodeList.Get(i).ChildNodes.FindNode('dbid').ChildNodes.FindNode('XPointer').Text, xPointerSubstring) then
      begin
        Result := itemsNodeList.Get(i);
        break;
      end;
end;

end.
