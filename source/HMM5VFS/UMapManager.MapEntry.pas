// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UMapManager.MapEntry;

interface

type
  { расположение файла
     ilUnknown ~ неизвестно;
     ilExtFile ~ внешний plain-файл;
     ilRootH5M ~ базовый h5m карты;
     ilGameResource ~ ресурс игры }
  TMapItemLocation = (ilUnknown, ilExtFile, ilRootH5M, ilGameResource);

  RMapEntry = record
    Location: TMapItemLocation;   // расположение файла

    FSPath: string;           // пусть в файловой системе до архива или файла
    LastModDate: TDateTime;   // абстрактные дата-время для определения самой новой версии
    IsArchived: boolean;      // запакован в архив или нет

    FullFileName: string;  // полное имя файла [ZIP '/'] [Path + Filename]
  end;

  TMapEntryCollection = class
  protected
    FMapEntries: array of RMapEntry;
    function GetMapEntry(const AIndex: integer): RMapEntry;
  public
    procedure AppendMapEntry(const AMapEntry: RMapEntry); overload;
    procedure AppendMapEntry(FullFileName: string; Location: TMapItemLocation); overload;
    procedure RemoveMapEntry(AIndex: integer);
    procedure ReplaceMapEntry(AIndex: integer; const AMapEntry: RMapEntry);
    procedure Clean();

    function FindMapEntryByFileNameRev(FileName: string): integer;
    function FindMapEntryByFileName(FileName: string): integer;

    function IsFilePresent(const AFullFileName: string): boolean;
    function IsFilePresentRev(const AFullFileName: string): boolean;

    function FindItemByFileName(const AFullFileName: string): integer;

    function Count(): integer;
    property Items[const AIndex: integer]: RMapEntry read GetMapEntry;
  end;
  
implementation

uses
  SysUtils, StrUtils, Windows;

procedure TMapEntryCollection.AppendMapEntry(FullFileName: string;
  Location: TMapItemLocation);
var
  xMapEntry: RMapEntry;
begin
  xMapEntry.Location := Location;
  xMapEntry.FullFileName := FullFileName;
  AppendMapEntry(xMapEntry);
end;

procedure TMapEntryCollection.AppendMapEntry(const AMapEntry: RMapEntry);
var
  index: integer;
begin
  index := Length(FMapEntries);
  SetLength(FMapEntries, index + 1);
  FMapEntries[index] := AMapEntry;
end;

procedure TMapEntryCollection.Clean;
begin
  SetLength(FMapEntries, 0);
end;

function TMapEntryCollection.Count: integer;
begin
  Result := Length(FMapEntries);
end;

procedure TMapEntryCollection.RemoveMapEntry(AIndex: integer);
var
  i: Integer;
begin
  for i := AIndex + 1 to Length(FMapEntries) - 1 do
    FMapEntries[i-1] := FMapEntries[i];
  SetLength(FMapEntries, Length(FMapEntries) - 1);
end;

procedure TMapEntryCollection.ReplaceMapEntry(AIndex: integer;
  const AMapEntry: RMapEntry);
begin
  FMapEntries[AIndex] := AMapEntry;
end;

function TMapEntryCollection.FindItemByFileName(
  const AFullFileName: string): integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FMapEntries) - 1 do
    if AnsiSameText(AFullFileName, FMapEntries[i].FullFileName) then
      begin
        Result := i;
        break;
      end;
end;

function TMapEntryCollection.FindMapEntryByFileName(FileName: string): integer;
var
  i: Integer;
  useFullPath: boolean;
  xCurrentFileName: string;
begin
  Result := -1;

  if FileName = '' then
    exit;

  useFullPath := (FileName[1] = '/');
  if useFullPath then
    Delete(FileName, 1, 1);

  for i := 0 to Length(FMapEntries) - 1 do
    begin
      xCurrentFileName := FMapEntries[i].FullFileName;
      if (useFullPath and AnsiSameText(FileName, xCurrentFileName)) or
         ((not useFullPath) and AnsiEndsText(FileName, xCurrentFileName)) then
        begin
          Result := i;
          break;
        end;
    end;
end;

function TMapEntryCollection.FindMapEntryByFileNameRev(
  FileName: string): integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FMapEntries) - 1 do
    begin
      if AnsiEndsText(FileName, FMapEntries[i].FullFileName) then
        begin
          Result := i;
          break;
        end;
    end;
end;

function TMapEntryCollection.GetMapEntry(const AIndex: integer): RMapEntry;
begin
  Result := FMapEntries[AIndex];
end;

function TMapEntryCollection.IsFilePresent(const AFullFileName: string): boolean;
begin
  Result := FindItemByFileName(AFullFileName) <> -1;
end;

function TMapEntryCollection.IsFilePresentRev(
  const AFullFileName: string): boolean;
var
  i: Integer;
begin
  Result := false;

  if AFullFileName = '' then
    exit;

  for i := 0 to Length(FMapEntries) - 1 do
    if AnsiEndsText(AFullFileName, FMapEntries[i].FullFileName) then
      begin
        Result := true;
        exit;
      end;

  if AFullFileName[1] = '/' then
    Result := IsFilePresent(Copy(AFullFileName, 2, Length(AFullFileName)-1));
end;

end.
