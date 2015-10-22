// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UXPathUnit;

interface

uses
  XMLIntf, xmldom;

function LocateXMLNode(const pXML: IXMLDocument; const pXPath: string): IDOMNode; overload;
function LocateXMLNode(const pDOMNode: IDOMNode; const pXPath: string): IDOMNode; overload;

function LocateXMLNodes(const pXML: IXMLDocument; const pXPath: string): IDOMNodeList;

implementation

uses
  SysUtils, ActiveX;

const
  SNoDOMNodeEx = 'Selected DOM Vendor does not support IDOMNodeSelect interface';

function LocateXMLNode(const pXML: IXMLDocument; const pXPath: string): IDOMNode;
var
  xNode: IDOMNodeSelect;
begin
  if not Supports(pXML.DOMDocument.documentElement, IDOMNodeSelect, xNode) then
    raise DOMException.Create(SNoDOMNodeEx);

  Result := xNode.selectNode(pXPath);
end;

function LocateXMLNodes(const pXML: IXMLDocument; const pXPath: string): IDOMNodeList;
var
  xNode: IDOMNodeSelect;
begin
  if not Supports(pXML.DOMDocument.documentElement, IDOMNodeSelect, xNode) then
    raise DOMException.Create(SNoDOMNodeEx);

  Result := xNode.selectNodes(pXPath);
end;

function LocateXMLNode(const pDOMNode: IDOMNode; const pXPath: string): IDOMNode;
var
  xNode: IDOMNodeSelect;
begin
  if not Supports(pDOMNode, IDOMNodeSelect, xNode) then
    raise DOMException.Create(SNoDOMNodeEx);

  Result := xNode.selectNode(pXPath);
end;

initialization
  CoInitializeEx(NIL, COINIT_APARTMENTTHREADED or COINIT_SPEED_OVER_MEMORY);

finalization
  CoUninitialize();

end.
