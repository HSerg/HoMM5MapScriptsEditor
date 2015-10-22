// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UMapXDB;

interface

uses
  XMLIntf, XMLDoc, UH5MManager, UMapManager;

type
  TMapXDB = class
  private
    FH5MManager: TMapManager;
  public
    XMLBody: IXMLDocument;
    function getMapScriptFilename(): string;
    constructor Create(AH5MManager: TMapManager);
  end;

implementation

uses
  UXPathUnit, xmldom, SysUtils;

constructor TMapXDB.Create(AH5MManager: TMapManager);
begin
  FH5MManager := AH5MManager;
end;

function TMapXDB.getMapScriptFilename: string;
var
  xScriptHREFNode: IDOMNode;
  xXDBScriptFilename: string;
  xScriptXDB: IXMLDocument;
  xLastDelimIndex: integer;
begin
  Result := '';

  try
    xScriptHREFNode := LocateXMLNode(XMLBody, '/AdvMapDesc/MapScript/@href');
    if xScriptHREFNode = nil then
      exit;

    xXDBScriptFilename := Copy(xScriptHREFNode.nodeValue, 1, Pos('#', xScriptHREFNode.nodeValue)-1);

    xScriptXDB := FH5MManager.GetFileAsXML(xXDBScriptFilename);
    if xScriptXDB = nil then
      exit;

    xScriptHREFNode := LocateXMLNode(xScriptXDB, '/Script/FileName/@href');
    if xScriptHREFNode = nil then
      exit;

    Result := xScriptHREFNode.nodeValue;

    // нет файла
    if Result = '' then
      exit;

    // абсолютный путь
    if Result[1] = '/' then
      exit;

    // абсолютный путь
    if xXDBScriptFilename[1] = '/' then
      begin
        xLastDelimIndex := LastDelimiter('/', xXDBScriptFilename);
        Result := Copy(xXDBScriptFilename, 1, xLastDelimIndex) + Result;
        exit;
      end;

    // используется основной путь
    Result := FH5MManager.MapPath + Result;

  except
    // nothing
  end;
end;

end.
