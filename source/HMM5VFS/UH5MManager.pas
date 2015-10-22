// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UH5MManager;

interface

uses
  AbZipKit, AbArcTyp, Classes, XMLIntf;

type
  {  Класс менеджер H5M-файла  }
  TH5MManager = class
  protected
    FZipFile: TAbZipKit;
    FH5MRootPath: string;

    function FindFile(AFileName: string): Integer;

  private
    procedure AbArchiveItemInsertFromStreamEvent(Sender: TObject; Item: TAbArchiveItem; OutStream, InStream: TStream);
    function GetH5MItemFileName(AIndex: integer): string;
    procedure ReOpenH5M();
    function GetH5MItemsCount: integer;
    function GetFileName: string;

  public
    destructor Destroy(); override;

    procedure InitWith(AFileName: string);

    function GetFileAsStream(FileName: string): TStream;
    procedure SetFileAsStream(FileName: string; AStream: TStream);

    function LocateMapXDB(): string;

    procedure Refresh();

    procedure CreateEmptyDirectory(DirectoryName: string);
    procedure CreateEmptyFile(AFileName: string);
    procedure DeleteDirectoryFile(APath: string);

    function IsFilePresent(const AFullFileName: string): boolean;
    function IsFilePresentRev(const AFileName: string): boolean;

    // ищет файл по имени с конца и если такой есть, то возращает его полный путь
    function GetFileRevFullPath(const AFileName: string): string;

    property FileName: string read GetFilename; 
    property H5MRootPath: string read FH5MRootPath;
    property H5MItemsCount: integer read GetH5MItemsCount;
    property H5MItemFileName[AIndex: integer]: string read GetH5MItemFileName;
  end;

implementation

uses
  AbUtils, StrUtils, SysUtils, XMLDoc, DKLang, AbZipTyp, UConsts, Windows;

procedure TH5MManager.AbArchiveItemInsertFromStreamEvent(Sender: TObject;
  Item: TAbArchiveItem; OutStream, InStream: TStream);
begin
// nothing
end;

procedure TH5MManager.CreateEmptyDirectory(DirectoryName: string);
var
  xAbArchiveItem: TAbZipItem;
  xSavedEvent: TAbArchiveItemInsertFromStreamEvent;
  xSavedAutoSave: boolean;
begin
  xSavedEvent := (FZipFile.ZipArchive as TAbZipArchive).InsertFromStreamHelper;
  xSavedAutoSave := FZipFile.ZipArchive.AutoSave;
  try
    try
      (FZipFile.ZipArchive as TAbZipArchive).InsertFromStreamHelper := AbArchiveItemInsertFromStreamEvent;
      FZipFile.ZipArchive.AutoSave := false;

      xAbArchiveItem := TAbZipItem.Create;
      xAbArchiveItem.FileName := DirectoryName;
      xAbArchiveItem.ExternalFileAttributes := faDirectory;
      FZipFile.ZipArchive.Add(xAbArchiveItem);
      xAbArchiveItem.Action := aaStreamAdd;

      FZipFile.Save;
      
    except
      FZipFile.ZipArchive.IsDirty := false;
      ReOpenH5M();

      raise;
    end;
  finally
    FZipFile.ZipArchive.AutoSave := xSavedAutoSave;
    (FZipFile.ZipArchive as TAbZipArchive).InsertFromStreamHelper := xSavedEvent;
  end;

  ReOpenH5M();
end;

procedure TH5MManager.CreateEmptyFile(AFileName: string);
var
  xStream: TMemoryStream;
begin
  xStream := TMemoryStream.Create;
  try
    SetFileAsStream(AFileName, xStream);
  finally
    xStream.Free;
  end;
end;

destructor TH5MManager.Destroy;
begin
  FreeAndNil(FZipFile);
end;

function TH5MManager.FindFile(AFileName: string): Integer;
var
  i: Integer;
  useFullPath: Boolean;
  xCurrentFileName: string;
begin
  Result := -1;
  if FileName = '' then
    exit;

  useFullPath := (AFileName[1] = '/');
  if useFullPath then
    Delete(AFileName, 1, 1);

  for i := 0 to FZipFile.Count - 1 do
    begin
      xCurrentFileName := FZipFile.Items[i].FileName;
      if (useFullPath and AnsiSameText(AFileName, xCurrentFileName)) or
         ((not useFullPath) and AnsiEndsText(AFileName, xCurrentFileName)) then
        begin
          Result := i;
          break;
        end;
    end;
end;

function TH5MManager.GetFileAsStream(FileName: string): TStream;
var
  xFileIndex: integer;
  xStream: TStream;
begin
  Result := nil;
  
  xFileIndex := FindFile(FileName);

  if xFileIndex = -1 then
    exit;

  xStream := TMemoryStream.Create;

  FZipFile.ExtractToStream(FZipFile.Items[xFileIndex].FileName, xStream);

  Result := xStream;
end;

function TH5MManager.GetFileName: string;
begin
  Result := FZipFile.FileName;
end;

function TH5MManager.GetFileRevFullPath(const AFileName: string): string;
var
  index: integer;
begin
  Result := '';
  index := FindFile(AFileName);
  if index <> -1 then
    Result := H5MItemFileName[index];
end;

function TH5MManager.GetH5MItemFileName(AIndex: integer): string;
begin
  Result := FZipFile.Items[AIndex].FileName;
end;

function TH5MManager.GetH5MItemsCount: integer;
begin
  Result := FZipFile.Count;
end;

procedure TH5MManager.InitWith(AFileName: string);
var
  xZipFile: TAbZipKit;
  xWinFileName: string;
  xMapXDBPath: string;
begin
  xZipFile := TAbZipKit.Create(nil);
  xZipFile.ForceType := true;
  xZipFile.ArchiveType := atZip;
  xZipFile.ExtractOptions := [];
  xZipFile.StoreOptions := xZipFile.StoreOptions + [soReplace];
  xZipFile.OpenArchive(AFileName);

  FZipFile := xZipFile;

  xMapXDBPath := LocateMapXDB();
  if xMapXDBPath<>'' then
    begin
      xWinFileName := StringReplace(xMapXDBPath, '/', '\', [rfReplaceAll, rfIgnoreCase]);
      FH5MRootPath := StringReplace(ExtractFilePath(xWinFileName), '\', '/', [rfReplaceAll, rfIgnoreCase]);
    end;
end;

function TH5MManager.IsFilePresent(const AFullFileName: string): boolean;
begin
  Result := FZipFile.ZipArchive.ItemList.IsActiveDupe(AFullFileName);
end;

function TH5MManager.IsFilePresentRev(const AFileName: string): boolean;
begin
  Result := FindFile(AFileName) <> -1;
end;

function TH5MManager.LocateMapXDB: string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to FZipFile.Count - 1 do
    begin
      if AnsiStartsText(SINGLE_DIR, FZipFile.Items[i].FileName) then
        if AnsiEndsText('/'+MAP_XDB, FZipFile.Items[i].FileName) then
          begin
            Result := FZipFile.Items[i].FileName;
            break;
          end;
    end;
  if Result = '' then
    begin
      for i := 0 to FZipFile.Count - 1 do
        begin
          if AnsiStartsText(MULTI_DIR, FZipFile.Items[i].FileName) then
            if AnsiEndsText('/'+MAP_XDB, FZipFile.Items[i].FileName) then
              begin
                Result := FZipFile.Items[i].FileName;
                break;
              end;
        end;
    end;
end;

procedure TH5MManager.Refresh;
begin
  ReOpenH5M();
end;

procedure TH5MManager.ReOpenH5M;
var
  xFileName: string;
begin
  xFileName := FZipFile.FileName;
  FZipFile.CloseArchive;
  FZipFile.OpenArchive(xFileName);
end;

procedure TH5MManager.SetFileAsStream(FileName: string; AStream: TStream);
begin
  try
    FZipFile.AddFromStream(FileName, AStream);
  except
    FZipFile.ZipArchive.IsDirty := false;
    ReOpenH5M();

    raise;
  end;

  ReOpenH5M();
end;

procedure TH5MManager.DeleteDirectoryFile(APath: string);
begin
  FZipFile.DeleteFiles(APath);
  FZipFile.Save;

  ReOpenH5M();
end;

end.

