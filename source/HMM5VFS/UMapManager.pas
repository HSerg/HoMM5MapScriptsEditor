// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UMapManager;

interface

uses
  UH5MManager, Classes, XMLIntf, UResourcesCache, UMapManager.MapEntry;

type
  TMapManager = class
  protected
    FBaseH5MManager: TH5MManager;
    FResourcesCache: TResourcesCache;
    FMapEntries: TMapEntryCollection;

  protected
    function GetH5MItemsCount: integer;
    function GetH5MItemFileName(AIndex: integer): string;
    function GetH5MItemLocation(AIndex: integer): TMapItemLocation;
    function GetMapPath: string;

  protected
    procedure InternalOpen();
    procedure InternalRefresh();

  public
    function GetFileAsStream(FileName: string): TStream;
    function GetFileAsXML(FileName: string): IXMLDocument;
    function GetFileAsUnicodeText(FileName: string): string;

    procedure SetFileAsStream(FileName: string; AStream: TStream);
    procedure SetFileAsUnicodeText(FileName: string; AText: string);

    // создаёт папку в базовом h5m-файле
    procedure CreateEmptyDirectory(DirectoryName: string);
    // создаёт файл в базовом h5m-файле
    procedure CreateEmptyFile(AFileName: string);
    // удаляет файл/папку среди открытых в MapManager
    procedure DeleteDirectoryFile(APath: string);

  public
    constructor Create();
    destructor Destroy; override;

    // инициализирует указатель на кэш ресурсов игры
    procedure InitHMM5Res(AResourcesCache: TResourcesCache);
    // инициализирует менеджер файлом-картой (*.h5m)
    procedure InitWith(AFileName: string);

    // возвращает map.xdb карты в базовом h5m-файле
    function LocateMapXDB(): string;

    // проверка наличия файла с указанным путём/именем (совпадение от начала)
    function IsFilePresent(const AFullFileName: string): boolean;
    // проверка наличия файла с указанным путём/именем (совпадение с конца)
    function IsFilePresentRev(const AFullFileName: string): boolean;

    // ищет файл по имени с конца и если такой есть, то возращает его полный путь
    function GetFileRevFullPath(const AFileName: string): string;

    // поиск файла с указанным путём/именем (полное совпадение)
    function FindFile(AFileName: string): integer;

    //добавляет к карте внешний файл
    procedure AddExtFile(AFileName: string);

    // базовый H5M файл
    function H5MFileName: string;

    // обновление данных о карте
    procedure Refresh();

    // путь в базовом h5m-файле
    property MapPath: string read GetMapPath;

    // расположение файла
    property ItemLocation[AIndex: integer]: TMapItemLocation read GetH5MItemLocation;

    // число MapEntry
    property ItemsCount: integer read GetH5MItemsCount;

    // имя MapEntry по индексу
    property ItemFileName[AIndex: integer]: string read GetH5MItemFileName;
  end;

implementation

uses
  StrUtils, SysUtils, XMLDoc, JclUnicode, DKLang, UZipFileUtils, UDKConsts;

procedure TMapManager.AddExtFile(AFileName: string);

  function iPlainFile2RME(APath: string): RMapEntry;
  begin
    Result.Location := ilExtFile;
    Result.FSPath := APath;
    Result.LastModDate := 0;
    Result.IsArchived := false;
    Result.FullFileName := APath;
  end;

var
  index: integer;
begin
  index := FMapEntries.FindItemByFileName(AFileName);
  if index = -1 then
    FMapEntries.AppendMapEntry(iPlainFile2RME(AFileName))
  else
    FMapEntries.ReplaceMapEntry(index, iPlainFile2RME(AFileName));
end;

constructor TMapManager.Create;
begin
  FMapEntries := TMapEntryCollection.Create;
  FResourcesCache := nil;
  FBaseH5MManager := nil;
end;

procedure TMapManager.CreateEmptyDirectory(DirectoryName: string);
begin
  FBaseH5MManager.CreateEmptyDirectory(DirectoryName);

  InternalRefresh();
end;

procedure TMapManager.CreateEmptyFile(AFileName: string);
begin
  FBaseH5MManager.CreateEmptyFile(AFileName);

  InternalRefresh();
end;

procedure TMapManager.DeleteDirectoryFile(APath: string);
begin
  FBaseH5MManager.DeleteDirectoryFile(APath);

  InternalRefresh();
end;

destructor TMapManager.Destroy;
begin
  FResourcesCache := nil;
  FreeAndNil(FBaseH5MManager);
  FreeAndNil(FMapEntries);
end;

function TMapManager.FindFile(AFileName: string): integer;
begin
  Result := FMapEntries.FindMapEntryByFileName(AFileName);
end;

function TMapManager.GetFileAsStream(FileName: string): TStream;
var
  index: integer; 
begin
  Result := nil;

  index := FMapEntries.FindMapEntryByFileName(FileName);
  if index = -1 then
    exit;

  case FMapEntries.Items[index].Location of
    ilExtFile:
      Result := TFileStream.Create(FMapEntries.Items[index].FSPath, fmOpenRead);

    ilRootH5M:
      Result := FBaseH5MManager.GetFileAsStream(FileName);

    ilGameResource:
      begin
        Result := TMemoryStream.Create;
        if Assigned(FResourcesCache) then
          if not FResourcesCache.GetFileAsStream(FileName, Result) then
            FreeAndNil(Result);
      end;
  end;
end;

function TMapManager.GetFileAsUnicodeText(FileName: string): string;
var
  xStream: TStream;
  xStrings: TWideStringList;
begin
  Result := '';

  xStream := GetFileAsStream(FileName);
  if (xStream = nil) then
    raise Exception.Create(DKConsts.SOPEN_ZIPFILE_ERROR_FMT([FileName]))
  else
    try
      xStream.Position := 0;
      xStrings := TWideStringList.Create();
      try
        xStrings.LoadFromStream(xStream);
        Result := xStrings.Text;
      finally
        xStrings.Free;
      end;
    finally
      xStream.Free;
    end;
end;

function TMapManager.GetFileAsXML(FileName: string): IXMLDocument;
var
  xStream: TStream;
  xXMLDoc: IXMLDocument;
begin
  Result := nil;

  xStream := GetFileAsStream(FileName);
  if xStream = nil then
    exit
  else
    try
      xXMLDoc := TXMLDocument.Create(nil);
      xXMLDoc.Options := [doNodeAutoIndent];
      xXMLDoc.ParseOptions := [poValidateOnParse, poPreserveWhiteSpace];
      xXMLDoc.LoadFromStream(xStream);

      Result := xXMLDoc;
    finally
      xStream.Free;
    end;
end;

function TMapManager.GetFileRevFullPath(const AFileName: string): string;
var
  index: Integer;
begin
  index := FMapEntries.FindMapEntryByFileNameRev(AFileName);
  if index = -1 then
    Result := ''
  else
    Result := FMapEntries.Items[index].FullFileName;
end;

function TMapManager.GetH5MItemFileName(AIndex: integer): string;
begin
  Result := FMapEntries.Items[AIndex].FullFileName;
end;

function TMapManager.GetH5MItemLocation(AIndex: integer): TMapItemLocation;
//var
//  xBSPath: string;
//  xFileInfo: RFileInfo;
begin
//  Result := ilUnknown;
//
//  xBSPath := ReplaceText(ItemFileName[AIndex], '/', '\');
//  if FResourcesCache.IsFilePresent(xBSPath) then
//    begin
//      xFileInfo := FResourcesCache.GetFileInfo(xBSPath);
//      Result := ilGameResource;
//    end;
//
//  if (Result = ilUnknown) and IsFilePresent(ItemFileName[AIndex]) then
//    Result := ilRootH5M;
//
//  if (Result = ilUnknown) and FileExists(ItemFileName[AIndex]) then
//    Result := ilExtFile;
  Result := FMapEntries.Items[AIndex].Location;
end;

function TMapManager.GetH5MItemsCount: integer;
begin
  Result := FMapEntries.Count;
end;

function TMapManager.GetMapPath: string;
begin
  Result := FBaseH5MManager.H5MRootPath;
end;

function TMapManager.H5MFileName: string;
begin
  if Assigned(FBaseH5MManager) then
    Result := FBaseH5MManager.FileName
  else
    Result := '';
end;

procedure TMapManager.InitHMM5Res(AResourcesCache: TResourcesCache);
begin
  FResourcesCache := AResourcesCache;
end;

procedure TMapManager.InitWith(AFileName: string);
begin
  FBaseH5MManager := TH5MManager.Create;
  FBaseH5MManager.InitWith(AFileName);
  
  InternalOpen();
end;

procedure TMapManager.InternalOpen;

  function iRFI2RME(APath: string; AFileInfo: RFileInfo): RMapEntry;
  begin
    Result.Location := ilGameResource;
    Result.FSPath := AFileInfo.FSPath;
    Result.LastModDate := AFileInfo.LastModDateTime;
    Result.IsArchived := AFileInfo.Archived;
    Result.FullFileName := APath;
  end;

  function iH5M2RME(APath: string): RMapEntry;
  begin
    Result.Location := ilRootH5M;
    Result.FSPath := FBaseH5MManager.FileName;
    Result.LastModDate := 0;
    Result.IsArchived := true;
    Result.FullFileName := APath;
  end;

var
  i: Integer;
  xResFiles: TStrings;
  xFileInfo: RFileInfo;
  index: integer;
begin
  FMapEntries.Clean();

  for i := 0 to FBaseH5MManager.H5MItemsCount - 1 do
    FMapEntries.AppendMapEntry(iH5M2RME(FBaseH5MManager.H5MItemFileName[i]));

  if Assigned(FResourcesCache) then
    begin
      xResFiles := FResourcesCache.GetAllFilesFromPath(FBaseH5MManager.H5MRootPath);
      try
        for i := 0 to xResFiles.Count - 1 do
          begin
            xFileInfo := FResourcesCache.GetFileInfo(xResFiles[i]);
            index := FMapEntries.FindItemByFileName(xResFiles[i]);
            if index = -1 then
              FMapEntries.AppendMapEntry(iRFI2RME(xResFiles[i], xFileInfo))
            else
            if not xFileInfo.Archived then
              FMapEntries.ReplaceMapEntry(index, iRFI2RME(xResFiles[i], xFileInfo));
          end;
      finally
        xResFiles.Free;
      end;
    end;
end;

procedure TMapManager.InternalRefresh;
begin
  //TODO: fix me !
  
  FBaseH5MManager.Refresh();
  if Assigned(FResourcesCache) then
    FResourcesCache.Refresh();
  InternalOpen();
end;

function TMapManager.IsFilePresent(const AFullFileName: string): boolean;
begin
  Result := FMapEntries.IsFilePresent(AFullFileName);
end;

function TMapManager.IsFilePresentRev(const AFullFileName: string): boolean;
begin
  Result := FMapEntries.IsFilePresentRev(AFullFileName);
end;

function TMapManager.LocateMapXDB: string;
begin
  Result := FBaseH5MManager.LocateMapXDB;
end;

procedure TMapManager.Refresh;
begin
  InternalRefresh();
end;

procedure TMapManager.SetFileAsStream(FileName: string; AStream: TStream);

  procedure intSaveAsPlainFile(FSPath: string; AStream: TStream);
  var
    xStream: TStream;
  begin
    xStream := TFileStream.Create(FSPath, fmOpenWrite);
    try
      xStream.Size := AStream.Size;
      xStream.Seek(0, soFromBeginning);
      xStream.CopyFrom(AStream, 0);
    finally
      xStream.Free;
    end;
  end;

var
  index: integer;
begin
  index := FMapEntries.FindMapEntryByFileName(FileName);
  if index = -1 then
    exit;

  case FMapEntries.Items[index].Location of
    ilExtFile:
      intSaveAsPlainFile(FMapEntries.Items[index].FSPath, AStream);

    ilRootH5M:
      FBaseH5MManager.SetFileAsStream(FileName, AStream);

    ilGameResource:
      begin
        if not FMapEntries.Items[index].IsArchived then
          intSaveAsPlainFile(FMapEntries.Items[index].FSPath, AStream)
        else
          saveAsZIPFile(FMapEntries.Items[index].FSPath,
            FMapEntries.Items[index].FullFileName, AStream);
      end;
  end;
end;

procedure TMapManager.SetFileAsUnicodeText(FileName, AText: string);
var
  xStream: TStream;
  xStrings: TWideStringList;
begin
  xStream := TMemoryStream.Create;
  try
    xStrings := TWideStringList.Create();
    try
      xStrings.Text := AText;
      xStrings.SaveUnicode := true;
      xStrings.SaveFormat := sfUTF16LSB;
      xStrings.SaveToStream(xStream, true);
    finally
      xStrings.Free;
    end;
    SetFileAsStream(FileName, xStream);
  finally
    xStream.Free;
  end;
end;

end.
