// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UH5MFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, VirtualTrees, UH5MManager, UMapManager, ActnList, Menus, UCommonDM,
  DKLang, UOptionsUnit, ImgList;

type
  TOpenFileEvent = procedure (AFullFilePath, AFileName: string) of object;

  TH5MFrame = class(TFrame)
    vstH5MContent: TVirtualStringTree;
    DKLanguageController1: TDKLanguageController;
    procedure vstH5MContentGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vstH5MContentDblClick(Sender: TObject);
    procedure vstH5MContentFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstH5MContentChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstH5MContentCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstH5MContentIncrementalSearch(Sender: TBaseVirtualTree;
      Node: PVirtualNode; const SearchText: WideString; var Result: Integer);
    procedure vstH5MContentPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure vstH5MContentGetImageIndexEx(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
    procedure vstH5MContentKeyPress(Sender: TObject; var Key: Char);

  private
    type
      RNodeData = record
        FileName: string;
        AText: string;
        IsDirectory: boolean;
      end;
      PNodeData = ^RNodeData;
    function GetNodeData(ANode: PVirtualNode): PNodeData;

  private
    FH5MManager: TMapManager;
    FOpenFileEvent: TOpenFileEvent;
    FChangeFileEvent: TOpenFileEvent;
    FFilter: string;
    FUseShortRootNode: boolean;
    FStartUpOpenFolders: TStartUpOpenFolders;
    FExcludeFilter: boolean;

    procedure SetFilter(const Value: string);
    procedure SetExcludeFilter(const Value: boolean);
    function GetPathNode(APath: string; ACreate: boolean = true): PVirtualNode;
    function ParsePath(APath: string): TStrings;

  public
    destructor Destroy(); override;

    procedure InitWith(H5MManager: TMapManager; Filter: string = '';
        AExcludeFilter: boolean = false);

    procedure Refresh();

    function GetSelectedPath(AFilesOnly: boolean = true): string;
    function GetSelectedFolderPath(): string;

    procedure SetSelected(const AFullPath: string);

    property UseShortRootNode: boolean read FUseShortRootNode write FUseShortRootNode;
    property Filter: string read FFilter write SetFilter;
    property ExcludeFilter: boolean read FExcludeFilter write SetExcludeFilter;
    property OpenFileEvent: TOpenFileEvent read FOpenFileEvent write FOpenFileEvent;
    property ChangeFileEvent: TOpenFileEvent read FChangeFileEvent write FChangeFileEvent;
    property StartUpOpenFolders: TStartUpOpenFolders read FStartUpOpenFolders write FStartUpOpenFolders;
  end;

implementation

{$R *.dfm}

uses
  UConsts, Masks, StrUtils, UMapManager.MapEntry;

{ TH5MFrame }

destructor TH5MFrame.Destroy;
begin
  FOpenFileEvent := nil;

  inherited;
end;

procedure TH5MFrame.SetExcludeFilter(const Value: boolean);
begin
  FExcludeFilter := Value;
  Refresh();
end;

procedure TH5MFrame.SetFilter(const Value: string);
begin
  FFilter := Value;
  Refresh();
end;

procedure TH5MFrame.SetSelected(const AFullPath: string);
var
  node: PVirtualNode;
  nodeData: PNodeData;
begin
  node := vstH5MContent.GetFirst;
  while node <> nil do
    begin
      nodeData := vstH5MContent.GetNodeData(node);
      if nodeData.FileName = AFullPath then
        begin
          vstH5MContent.ClearSelection;
          vstH5MContent.Selected[node] := true;
          vstH5MContent.FocusedNode := node;
          break;
        end;
      node := vstH5MContent.GetNext(node);
    end;
end;

function TH5MFrame.GetNodeData(ANode: PVirtualNode): PNodeData;
begin
  Result := vstH5MContent.GetNodeData(ANode);
end;

function TH5MFrame.GetPathNode(APath: string; ACreate: boolean = true): PVirtualNode;

function findNode(parentNode: PVirtualNode; AFullPath, AText: string): PVirtualNode;
var
  node: PVirtualNode;
  nodeData: PNodeData;
begin
  Result := nil;

  node := parentNode.FirstChild;
  while node <> nil do
    begin
      nodeData := vstH5MContent.GetNodeData(node);

      if AnsiSameText(nodeData.AText, AText) then
        begin
          Result := node;
          break;
        end;

      node := node.NextSibling;
    end;

  if (Result = nil) and ACreate then
    begin
      node := vstH5MContent.AddChild(parentNode);
      nodeData := vstH5MContent.GetNodeData(node);
      nodeData.FileName := AFullPath;
      nodeData.AText := AText;
      nodeData.IsDirectory := true;
      
      Result := node;
    end;
end;

var
  node: PVirtualNode;
  xPath: TStrings;
  xLevel: integer;
  xFullPath: string;
begin
  xPath := ParsePath(APath);
  try
    node := vstH5MContent.RootNode;
    for xLevel := 0 to xPath.Count - 1 do
      begin
        if xFullPath = '' then
          xFullPath := xPath[0] + '/'
        else
          xFullPath := xFullPath + xPath[xLevel] + '/';

        node := findNode(node, xFullPath, xPath[xLevel]);

        if (node = nil) then
          break;
      end;
    Result := node;
  finally
    xPath.Free;
  end;
end;

function TH5MFrame.GetSelectedFolderPath: string;
var
  node: PVirtualNode;
  nodeData: PNodeData;
begin
  node := vstH5MContent.GetFirstSelected();
  nodeData := vstH5MContent.GetNodeData(Node);
  if (nodeData<>nil) then
    if nodeData.IsDirectory then
      Result := nodeData.FileName
    else
      begin
        node := node.Parent;
        nodeData := vstH5MContent.GetNodeData(Node);
        if (nodeData<>nil) then
          Result := nodeData.FileName;
      end;

  if Result = '' then
    Result := FH5MManager.MapPath;
end;

function TH5MFrame.GetSelectedPath(AFilesOnly: boolean = true): string;
var
  node: PVirtualNode;
  nodeData: PNodeData;
begin
  node := vstH5MContent.GetFirstSelected();
  nodeData := vstH5MContent.GetNodeData(Node);
  if (nodeData<>nil) then
    if not (AFilesOnly and nodeData.IsDirectory) then
      Result := nodeData.FileName;
end;

procedure TH5MFrame.InitWith(H5MManager: TMapManager; Filter: string = '';
  AExcludeFilter: boolean = false);

function intMatchMasks(const aValue: string; const xFilters: TStrings;
  const AExcludeMode: boolean): boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to xFilters.Count - 1 do
    if MatchesMask(aValue, xFilters[i]) then
      begin
        Result := true;
        break;
      end;

  if AExcludeMode then
    Result := not Result;
end;

var
  i: Integer;

  pathNode: PVirtualNode;
  node: PVirtualNode;
  nodeData: PNodeData;
  xWinFileName: string;
  treeNodeText: string;

  xFilters: TStrings;
begin
  vstH5MContent.NodeDataSize := sizeof(RNodeData);

  FH5MManager := H5MManager;
  FUseShortRootNode := true;

  FFilter := Trim(Filter);
  FExcludeFilter := AExcludeFilter;

  xFilters := TStringList.Create;
  xFilters.Delimiter := '|';
  xFilters.DelimitedText := FFilter;

  vstH5MContent.BeginUpdate;
  try
    vstH5MContent.Clear;

    if H5MManager <> nil then
      begin
        for i := 0 to H5MManager.ItemsCount - 1 do
          if (Filter = '') or (intMatchMasks(H5MManager.ItemFileName[i], xFilters, FExcludeFilter))  then
            begin
              xWinFileName := StringReplace(H5MManager.ItemFileName[i], '/', '\', [rfReplaceAll, rfIgnoreCase]);

              if (H5MManager.ItemLocation[i] = ilExtFile) then
                begin
                  pathNode := nil;
                  treeNodeText := xWinFileName;
                end
              else
                begin
                  pathNode := GetPathNode(ExtractFilePath(xWinFileName));
                  treeNodeText := ExtractFileName(xWinFileName);
                end;
                
              if ExtractFileName(xWinFileName) <> '' then
                begin
                  node := vstH5MContent.AddChild(pathNode);
                  nodeData := vstH5MContent.GetNodeData(node);
                  nodeData.FileName := H5MManager.ItemFileName[i];
                  nodeData.AText := treeNodeText;
                  case H5MManager.ItemLocation[i] of
                    ilUnknown: nodeData.AText := nodeData.AText;
                    ilExtFile: nodeData.AText := nodeData.AText;
                    ilRootH5M: nodeData.AText := nodeData.AText;
                    ilGameResource: nodeData.AText := nodeData.AText;
                  end;
                  nodeData.IsDirectory := false;
                end;
            end;
        vstH5MContent.SortTree(0, sdAscending);

        if H5MManager.MapPath = '' then
          node := vstH5MContent.GetFirst
        else
        if H5MManager.MapPath[Length(H5MManager.MapPath)] = '/' then
          node := GetPathNode(Copy(H5MManager.MapPath, 1, Length(H5MManager.MapPath)-1), false)
        else
          node := GetPathNode(H5MManager.MapPath, false);

        if node <> nil then
          case FStartUpOpenFolders of
            sofNothing:
              { nothing } ;
            sofMapFolder:
              begin
                vstH5MContent.Expanded[node] := true;
                vstH5MContent.FullyVisible[node] := true;
              end;
            sofMapFolderAndSubFolders:
              begin
                vstH5MContent.FullExpand(node);
                vstH5MContent.FullyVisible[node] := true;
              end;
            sofAll:
              vstH5MContent.FullExpand();
          end;

      end;
  finally
    vstH5MContent.EndUpdate;

    xFilters.Free;
  end;
end;

function TH5MFrame.ParsePath(APath: string): TStrings;
var
  xPos: integer;
begin
  Result := TStringList.Create;
  xPos := AnsiPos('\', APath);
  while xPos>0 do
    begin
      Result.Add(Copy(APath, 1, xPos-1));
      Delete(APath, 1, xPos);

      xPos := AnsiPos('\', APath);
    end;
  if APath<>'' then
    Result.Add(APath);

  if (FUseShortRootNode) and (Result.Count > 2) and
    AnsiSameText(Result[0] + '/' + Result[1] + '/' + Result[2] + '/', FH5MManager.MapPath) then
    begin
      Result[0] := Result[0] + '/' + Result[1] + '/' + Result[2];
      Result.Delete(2);
      Result.Delete(1);
    end;
end;

procedure TH5MFrame.Refresh;
var
  expandedNodes: TStrings;
  node: PVirtualNode;
  nodeData: PNodeData;
  tmp: TStartUpOpenFolders;
  firstVisibleNodeData: string;
  lastVisibleNodeData: string;
begin
  if vstH5MContent.NodeDataSize <= 0 then
    exit;

  nodeData := GetNodeData(vstH5MContent.GetFirstVisible());
  if nodeData<>nil then
    firstVisibleNodeData := nodeData.FileName
  else
    firstVisibleNodeData := '';

  nodeData := GetNodeData(vstH5MContent.GetLastVisible());
  if nodeData<>nil then
    lastVisibleNodeData := nodeData.FileName
  else
    lastVisibleNodeData := '';

  expandedNodes := TStringList.Create;
  try
    node := vstH5MContent.GetFirst;
    while node <> nil do
      begin
        if vstH5MContent.Expanded[node] then
          begin
            nodeData := vstH5MContent.GetNodeData(node);
            expandedNodes.Add(nodeData.FileName);
          end;
        node := vstH5MContent.GetNext(node);
      end;

    tmp := FStartUpOpenFolders;
    try
      FStartUpOpenFolders := sofNothing;
      InitWith(FH5MManager, FFilter, FExcludeFilter);
    finally
      FStartUpOpenFolders := tmp;
    end;

    node := vstH5MContent.GetFirst;
    while node <> nil do
      begin
        nodeData := vstH5MContent.GetNodeData(node);
        if expandedNodes.IndexOf(nodeData.FileName) <> -1 then
          vstH5MContent.Expanded[node] := true;
        node := vstH5MContent.GetNext(node);
      end;

    node := vstH5MContent.GetFirst;
    while node <> nil do
      begin
        nodeData := vstH5MContent.GetNodeData(node);
        if (firstVisibleNodeData <> '') and (nodeData.FileName = firstVisibleNodeData) or
           (lastVisibleNodeData <> '') and (nodeData.FileName = lastVisibleNodeData) then
            begin
              vstH5MContent.ScrollIntoView(node, false, false);
              firstVisibleNodeData := '';
              lastVisibleNodeData := '';
            end;
        node := vstH5MContent.GetNext(node);
      end;

  finally
    expandedNodes.Free;
  end;
end;

procedure TH5MFrame.vstH5MContentChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  nodeData: PNodeData;
begin
  if Assigned(FChangeFileEvent) then
    begin
      nodeData := vstH5MContent.GetNodeData(Node);
      if (nodeData<>nil) then
        begin
          if (nodeData.FileName <> '') then
            FChangeFileEvent(nodeData.FileName, nodeData.AText)
          else
            FChangeFileEvent('', nodeData.AText);
        end;
    end;
end;

procedure TH5MFrame.vstH5MContentCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  node1Data, node2Data: PNodeData;
begin
  node1Data := vstH5MContent.GetNodeData(Node1);
  node2Data := vstH5MContent.GetNodeData(Node2);

  if (node1Data.IsDirectory = node2Data.IsDirectory) then
    Result := CompareText(node1Data.AText, node2Data.AText)
  else
  if node1Data.IsDirectory then
    Result := -1
  else
    Result := 1;
end;

procedure TH5MFrame.vstH5MContentDblClick(Sender: TObject);
var
  node: PVirtualNode;
  nodeData: PNodeData;
begin
  if Assigned(FOpenFileEvent) then
    begin
      node := vstH5MContent.GetFirstSelected();
      nodeData := vstH5MContent.GetNodeData(Node);
      if (nodeData<>nil) and (not nodeData.IsDirectory) then
        FOpenFileEvent(nodeData.FileName, nodeData.AText);
    end;
end;

procedure TH5MFrame.vstH5MContentFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  nodeData: PNodeData;
begin
  nodeData := vstH5MContent.GetNodeData(Node);
  if nodeData<>nil then
    begin
      nodeData.FileName := EmptyStr;
      nodeData.AText := EmptyStr;
    end;
end;

procedure TH5MFrame.vstH5MContentGetImageIndexEx(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var
  nodeData: PNodeData;
  fileExt: string;
  index: Integer;
begin
  ImageIndex := -1;
  nodeData := vstH5MContent.GetNodeData(Node);
  if nodeData<>nil then
    begin
      if nodeData.IsDirectory then
        begin
          if Kind <> ikOverlay then
            ImageIndex := 0
        end
      else
        begin
          if Kind <> ikOverlay then
            begin
              fileExt := ExtractFileExt(nodeData.FileName);
              if AnsiSameText(fileExt, EXT_TXT) then
                ImageIndex := 1
              else
              if AnsiSameText(fileExt, EXT_LUA) then
                ImageIndex := 2
              else
              if AnsiSameText(fileExt, EXT_XML) or AnsiSameText(fileExt, EXT_XDB)then
                ImageIndex := 3
              else
                ImageIndex := 4;
            end
          else
            begin
              index := FH5MManager.FindFile(nodeData.FileName);
              if index = -1 then
                exit;
              ImageList := CommonDM.imglstFileOverlayIcons;
              case FH5MManager.ItemLocation[index] of
                ilUnknown : ImageIndex := -1;
                ilExtFile : ImageIndex := 17;
                ilRootH5M : ImageIndex := -1;
                ilGameResource : ImageIndex := 16;
              end;
            end;
        end;
    end;
end;

procedure TH5MFrame.vstH5MContentGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  nodeData: PNodeData;
begin
  nodeData := vstH5MContent.GetNodeData(Node);
  CellText := nodeData.AText;
end;

procedure TH5MFrame.vstH5MContentIncrementalSearch(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: WideString; var Result: Integer);
var
  nodeData: PNodeData;
begin
  nodeData := vstH5MContent.GetNodeData(Node);
  if AnsiStartsText(SearchText, nodeData.AText) then
    Result := 0
  else
    Result := -1;
end;

procedure TH5MFrame.vstH5MContentKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    vstH5MContentDblClick(Sender);
end;

procedure TH5MFrame.vstH5MContentPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  nodeData: PNodeData;
begin
  nodeData := vstH5MContent.GetNodeData(Node);
  if not nodeData.IsDirectory and EndsText('.lua', nodeData.AText) then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
end;

end.
