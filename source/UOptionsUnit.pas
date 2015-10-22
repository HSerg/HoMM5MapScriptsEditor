// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UOptionsUnit;

interface

uses
  IniFiles, Menus, Classes, ActnList;

type
  { MRU список }
  TMRUOpenFiles = class
  private
    FItems: TStrings;
    FSize: integer;

    function GetItem(Index: integer): string;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AppendItem(AItem: string);
    procedure Write(AIniFile: TCustomIniFile; ASection: string);
    procedure Read(AIniFile: TCustomIniFile; ASection: string);

    procedure RebuildMenuItems(AParentMenuItem: TMenuItem; AMenuAction: TCustomAction);

    property Items[Index: integer]: string read GetItem;
  end;

  TStartUpOpenFolders = (sofNothing = 0,
                         sofMapFolder = 1,
                         sofMapFolderAndSubFolders = 2,
                         sofAll = 3);
    
  { Настройки приложение }
  TAppIniOptions = class
  private
    FIniFile: TMemIniFile;
    FMRUOpenFiles: TMRUOpenFiles;

  public
    constructor Create;
    destructor Destroy; override;

  public
    Common: record
      LANG: integer;
    end;
    const COMMON_SECTION = 'COMMON';

  public
    StartUp: record
      OpenMapXDB: boolean;
      OpenMapScriptLUA: boolean;
      OpenFolders: TStartUpOpenFolders;
    end;
    const STARTUP_SECTION = 'STARTUP';

  public
    Path: record
      HMM5Path: string;
    end;
    const PATH_SECTION = 'PATH';

  public
    LuaEditor: record
      UseGetMapDataPath: boolean;
      UseLongParamsDesc: boolean;
    end;
    const LUA_EDITOR_SECTION = 'LUA_EDITOR';

  public
    property MRUOpenFiles: TMRUOpenFiles read FMRUOpenFiles;
  end;

var
  AppIniOptions: TAppIniOptions;

implementation

uses
  SysUtils, Forms;

//------------------------------------------------------------------------------
constructor TAppIniOptions.Create;
var
  xOptionsIni: string;
begin
  xOptionsIni := ChangeFileExt(Application.ExeName, '.ini');
  FIniFile := TMemIniFile.Create(xOptionsIni);

  Common.LANG := FIniFile.ReadInteger(COMMON_SECTION, 'LANG', 0);

  Path.HMM5Path := FIniFile.ReadString(PATH_SECTION, 'HMM5Path', '');

  StartUp.OpenMapXDB := FIniFile.ReadBool(STARTUP_SECTION, 'OpenMapXDB', true);
  StartUp.OpenMapScriptLUA := FIniFile.ReadBool(STARTUP_SECTION, 'OpenMapScriptLUA', true);
  StartUp.OpenFolders := TStartUpOpenFolders(FIniFile.ReadInteger(STARTUP_SECTION, 'OpenFolders', 0));

  LuaEditor.UseGetMapDataPath := FIniFile.ReadBool(LUA_EDITOR_SECTION, 'UseGetMapDataPath', false);
  LuaEditor.UseLongParamsDesc := FIniFile.ReadBool(LUA_EDITOR_SECTION, 'UseLongParamsDesc', true);

  FMRUOpenFiles := TMRUOpenFiles.Create;
  FMRUOpenFiles.Read(FIniFile, 'FILES');
end;

//------------------------------------------------------------------------------
destructor TAppIniOptions.Destroy;
begin
  FMRUOpenFiles.Write(FIniFile, 'FILES');
  FMRUOpenFiles.Free;

  FIniFile.WriteInteger(COMMON_SECTION, 'LANG', Common.LANG);
  
  FIniFile.WriteString(PATH_SECTION, 'HMM5Path', Path.HMM5Path);

  FIniFile.WriteBool(STARTUP_SECTION, 'OpenMapXDB', StartUp.OpenMapXDB);
  FIniFile.WriteBool(STARTUP_SECTION, 'OpenMapScriptLUA', StartUp.OpenMapScriptLUA);
  FIniFile.WriteInteger(STARTUP_SECTION, 'OpenFolders', Integer(StartUp.OpenFolders));

  FIniFile.WriteBool(LUA_EDITOR_SECTION, 'UseGetMapDataPath', LuaEditor.UseGetMapDataPath);
  FIniFile.WriteBool(LUA_EDITOR_SECTION, 'UseLongParamsDesc', LuaEditor.UseLongParamsDesc);

  FIniFile.UpdateFile;
  FIniFile.Free;
end;

//------------------------------------------------------------------------------
procedure TMRUOpenFiles.AppendItem(AItem: string);
var
  localIndex: integer;
begin
  localIndex := FItems.IndexOf(AItem);
  if localIndex = -1 then
    begin
      if FItems.Count >= FSize then
        FItems.Delete(FSize-1);
      FItems.Insert(0, AItem);
    end
  else
  if localIndex > 0 then
    FItems.Move(localIndex, 0);
end;

//------------------------------------------------------------------------------
constructor TMRUOpenFiles.Create;
begin
  FItems := TStringList.Create;
  FSize := 10;
end;

//------------------------------------------------------------------------------
destructor TMRUOpenFiles.Destroy;
begin
  FItems.Free;
end;

//------------------------------------------------------------------------------
procedure TMRUOpenFiles.Write(AIniFile: TCustomIniFile; ASection: string);
var
  i: integer;
begin
  AIniFile.EraseSection(ASection);
  for i := 0 to FItems.Count - 1 do
    AIniFile.WriteString(ASection, IntToStr(i), FItems[i]);
end;

//------------------------------------------------------------------------------
procedure TMRUOpenFiles.Read(AIniFile: TCustomIniFile; ASection: string);
var
  i: integer;
begin
  for i := FSize - 1 downto 0 do
    if AIniFile.ValueExists(ASection, IntToStr(i)) then
      AppendItem(AIniFile.ReadString(ASection, IntToStr(i), ''));
end;

//------------------------------------------------------------------------------
procedure TMRUOpenFiles.RebuildMenuItems(AParentMenuItem: TMenuItem; AMenuAction: TCustomAction);
var
  i: integer;
  startIndex, endIndex: integer;
  mi: TMenuItem;
begin
  for i := 0 to AParentMenuItem.Count - 1 do
    begin
      if AParentMenuItem.Items[i].Name = 'miMRUStart' then
        startIndex := i
      else
      if AParentMenuItem.Items[i].Name = 'miMRUEnd' then
        endIndex := i;
    end;

  for i := startIndex+1 to endIndex - 1 do
    AParentMenuItem.Remove(AParentMenuItem.Items[startIndex+1]);

  for i := FItems.Count-1 downto 0 do
    begin
      mi := TMenuItem.Create(AParentMenuItem.Owner);
      mi.Action := AMenuAction;
      mi.Caption := FItems[i];
      mi.Tag := i;
      AParentMenuItem.Insert(startIndex+1, mi);
    end;
end;

//------------------------------------------------------------------------------
function TMRUOpenFiles.GetItem(Index: integer): string;
begin
  Result := FItems[Index];
end;

initialization
  AppIniOptions := TAppIniOptions.Create;

finalization
  AppIniOptions.Free;

end.
