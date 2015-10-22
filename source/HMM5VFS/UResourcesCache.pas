// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UResourcesCache;

interface

uses
  Classes, Contnrs, AsyncCalls, UConsts;

type
  THMM5Version = (hmmUnknown, hmmOriginal, hmmAddon1, hmmAddon2);

  RFileInfo = record
    Filename: string;   // имя файла
    Path: string;       // путь к файлу в идеологии HMM
    Archived: boolean;  // запакован в архив или нет
    FSPath: string;     // пусть в файловой система до архива или файла
    LastModDateTime: TDateTime; // абстрактные дата-время для определения самой новой версии
  end;

  //TODO: Добавить обработку путей с начальным '/'
  //[или переработать все пути, или просто отсекать во всех функциях]
  TResourcesCache = class
  protected
    type
      TFileEntry = class
        Filename: string;   // имя файла
        Path: string;       // путь к файлу в идеологии HMM
        Archived: boolean;  // запакован в архив или нет
        FSPath: string;     // пусть в файловой система до архива или файла
        LastModDateTime: TDateTime; // абстрактные дата-время для определения самой новой версии
      end;

  protected
    FHMM5Dir: string;
    FHMM5Version: THMM5Version;
    FPackIgnore: boolean;
    FFiles: TStringList;
    FReady: boolean;
    FAsyncCall: IAsyncCall;

    procedure ScanDataDir(const ADir: string);
    procedure ScanPak(const AFileName: string);
    procedure ScanFile(const AFSPath, APath, AFileName: string);
    procedure RecursiveScanDataDir(const ADir, CurrentHMM5Path: string);

    function BuildDirFileList(const Path: string; const DirList, FileList:
        TStrings): Boolean;

    procedure InternalInit();
    procedure WaitForInit();

    function FindFileIndex(AFileName: string): integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(const AHMM5Dir: string; const AHMM5Version: THMM5Version);
    procedure AsyncInit(const AHMM5Dir: string; const AHMM5Version: THMM5Version);

    procedure Refresh();

    // возвращает содержимое файла
    function GetFileAsStream(AFileName: string; pStream: TStream): boolean;
    // проверяет наличие файла
    function IsFilePresent(const AFileName: string): boolean;
    // возращает информацию о файле или кидает Exception
    function GetFileInfo(AFileName: string): RFileInfo;
    // возращает список файлов по указанному пути 
    function GetAllFilesFromPath(APath: string): TStrings;

    procedure Clear;

    property Ready: boolean read FReady;
    property HMM5Dir: string read FHMM5Dir;
  end;

function DetectHMM5Version(ADir: string): THMM5Version;

implementation

uses
  SysUtils, JclFileUtils, JclDateTime, Math, Windows, StrUtils,
  AbZipKit, AbUtils, AbArcTyp;

function DetectHMM5Version(ADir: string): THMM5Version;
begin
  Result := hmmUnknown;
  if not DirectoryExists(ADir) then
    exit;

  ADir := IncludeTrailingPathDelimiter(ADir);

  if FileExists(ADir + 'profiles\autoexec.cfg') then
    Result := hmmOriginal;
  if FileExists(ADir + 'profiles\autoexec_a1.cfg') then
    Result := hmmAddon1;
  if FileExists(ADir + 'profiles\autoexec_a2.cfg') then
    Result := hmmAddon2;
end;

function AsyncInitCaller(Arg: TObject): Integer;
begin
  try
    (Arg as TResourcesCache).InternalInit();
    Result := 0;
  except
    Result := -1;
  end;
end;

{ TResourcesCache }

procedure TResourcesCache.Clear;
var
  i: Integer;
  obj: TObject;
begin
  WaitForInit();

  FHMM5Dir := '';
  FHMM5Version := hmmUnknown;

  for i := 0 to FFiles.Count - 1 do
    begin
      obj := FFiles.Objects[i];
      FFiles.Objects[i] := nil;
      obj.Free;
    end;

  FFiles.Clear;
end;

constructor TResourcesCache.Create;
begin
  FAsyncCall := nil;
  FReady := true;
  FFiles := TStringList.Create();
  FFiles.Capacity := 100000;
  FFiles.Sorted := true;
  FFiles.CaseSensitive := false;
  FHMM5Version := hmmUnknown;
  FHMM5Dir := '';
  FPackIgnore := true;
end;

destructor TResourcesCache.Destroy;
begin
  Clear();
  
  FFiles.Free;
end;

function TResourcesCache.FindFileIndex(AFileName: string): integer;
begin
  Result := FFiles.IndexOf(AFileName);
end;

function TResourcesCache.GetFileAsStream(AFileName: string;
  pStream: TStream): boolean;
var
  FOutStream: TMemoryStream;
  index: integer;
  xEntry: TFileEntry;
  xZipFile: TAbZipKit;
  xSearchFileName: string;
  xIndex: Integer;
begin
  WaitForInit();

  Result := false;

  index := FindFileIndex(AFileName);
  if index = -1 then
    exit;

  xEntry := TFileEntry(FFiles.Objects[index]);
  FOutStream := TMemoryStream.Create;
  try
    if xEntry.Archived then
      begin
        xZipFile := TAbZipKit.Create(nil);
        try
          try
            xZipFile.ForceType := true;
            xZipFile.ArchiveType := atZip;
            xZipFile.ExtractOptions := [];
            xZipFile.StoreOptions := xZipFile.StoreOptions + [soReplace];

            xZipFile.OpenArchive(xEntry.FSPath);

            xSearchFileName := StringReplace(AFileName, '\', '/', [rfReplaceAll, rfIgnoreCase]);
            xIndex := xZipFile.FindFile(xSearchFileName);

            if xIndex <> -1 then
              begin
                FOutStream.Size := 0;
                xZipFile.ExtractToStream(xZipFile.Items[xIndex].FileName, FOutStream);
                Result := true;
              end;

            xZipFile.CloseArchive;
          except
            // nothing
          end;
        finally
          xZipFile.Free;
        end;
      end
    else
      begin
        FOutStream.LoadFromFile(xEntry.FSPath);
        Result := true;
      end;

    pStream.CopyFrom(FOutStream, 0);
  finally
    FOutStream.Free;
  end;
end;

function TResourcesCache.GetFileInfo(AFileName: string): RFileInfo;
var
  xEntry: TFileEntry;
  index: Integer;
begin
  WaitForInit();

  index := FindFileIndex(AFileName);
  if index = -1 then
    raise EInOutError.Create('File not found :: ' + AFileName);

  xEntry := TFileEntry(FFiles.Objects[index]);
  Result.Filename := xEntry.Filename;
  Result.Path := xEntry.Path;
  Result.Archived := xEntry.Archived;
  Result.FSPath := xEntry.FSPath;
  Result.LastModDateTime := xEntry.LastModDateTime;
end;

procedure TResourcesCache.Init(const AHMM5Dir: string; const AHMM5Version:
    THMM5Version);
begin
  Clear();

  FHMM5Dir := IncludeTrailingPathDelimiter(AHMM5Dir);
  FHMM5Version := AHMM5Version;

  InternalInit();
end;

procedure TResourcesCache.InternalInit;
begin
  FReady := false;
  try
    ScanDataDir(FHMM5Dir + 'data' + PathDelim);
    if FHMM5Version = hmmAddon1 then
      ScanDataDir(FHMM5Dir + 'dataa1' + PathDelim);
  finally
    FReady := true;
  end;
end;

function TResourcesCache.IsFilePresent(const AFileName: string): boolean;
begin
  WaitForInit();

  Result := FindFileIndex(AFileName) <> -1;
end;

procedure TResourcesCache.RecursiveScanDataDir(const ADir, CurrentHMM5Path:
    string);
var
  xDirList, xFileList: TStringList;
  i: integer;
  xDirPath: string;
begin
  xDirList := TStringList.Create;
  xFileList := TStringList.Create;
  try
    BuildDirFileList(ADir, xDirList, xFileList);
    
    for i := 0 to xFileList.Count-1 do
      ScanFile(ADir + xFileList[i], CurrentHMM5Path, xFileList[i]);

    for i := 0 to xDirList.Count-1 do
      begin
        xDirPath := Trim(xDirList[i]);
        RecursiveScanDataDir(ADir+xDirPath+PathDelim, CurrentHMM5Path+xDirPath+HMM5_PATH_DELIM);
      end;
  finally
    xDirList.Free;
    xFileList.Free;
  end;
end;

procedure TResourcesCache.Refresh;
var
  xHMM5Dir: string;
  xHMM5Version: THMM5Version;
begin
  xHMM5Dir := FHMM5Dir;
  xHMM5Version := FHMM5Version;

  Init(xHMM5Dir, xHMM5Version);
end;

procedure TResourcesCache.ScanDataDir(const ADir: string);
var
  xDirList, xFileList: TStringList;
  i: integer;
  xDirPath: string;
begin
  xDirList := TStringList.Create;
  xFileList := TStringList.Create;
  try
    BuildDirFileList(ADir, xDirList, xFileList);

    for i := 0 to xFileList.Count-1 do
      if AnsiEndsText('.pak', xFileList[i]) then
        ScanPak(ADir + xFileList[i])
      else
        ScanFile(ADir + xFileList[i], '', xFileList[i]);

    for i := 0 to xDirList.Count-1 do
      begin
        xDirPath := Trim(xDirList[i]);
        RecursiveScanDataDir(ADir+xDirPath+PathDelim, xDirPath+HMM5_PATH_DELIM);
      end;
  finally
    xDirList.Free;
    xFileList.Free;
  end;
end;

procedure TResourcesCache.ScanFile(const AFSPath, APath, AFileName: string);
var
  index: integer;
  xLastModDateTime: TDateTime;
  xFileEntry: TFileEntry;
begin
  xLastModDateTime := FileTimeToLocalDateTime(GetFileLastWrite(AFSPath));

  index := FindFileIndex(APath + AFileName);
  if (index <> -1) then
    begin
      xFileEntry := TFileEntry(FFiles.Objects[index]);
      if (not xFileEntry.Archived) or (xFileEntry.LastModDateTime < xLastModDateTime) then
        begin
          xFileEntry.Archived := false;
          xFileEntry.FSPath := AFSPath;
          xFileEntry.LastModDateTime := xLastModDateTime;
        end;
    end
  else
    begin
      xFileEntry := TFileEntry.Create;
      xFileEntry.Filename := AFileName;
      xFileEntry.Path := APath;
      xFileEntry.Archived := false;
      xFileEntry.FSPath := AFSPath;
      xFileEntry.LastModDateTime := xLastModDateTime;
      FFiles.AddObject(xFileEntry.Path+xFileEntry.Filename, xFileEntry);
    end;
end;

procedure TResourcesCache.ScanPak(const AFileName: string);
var
  xZipFile: TAbZipKit;
  xLastModDateTime: TDateTime;
  i: Integer;
  index: Integer;
  xFileEntry: TFileEntry;
  xZIPFileName: string;
  xZIPFilesList: TStrings;
begin
  if FPackIgnore then
    exit;

  xLastModDateTime := FileTimeToLocalDateTime(GetFileLastWrite(AFileName));

  xZIPFilesList := TStringList.Create;
  try
    xZipFile := TAbZipKit.Create(nil);
    try
      try
        xZipFile.ForceType := true;
        xZipFile.ArchiveType := atZip;
        xZipFile.ExtractOptions := [];
        xZipFile.StoreOptions := xZipFile.StoreOptions + [soReplace];

        xZipFile.OpenArchive(AFileName);

        for i := 0 to xZipFile.Count - 1 do
          begin
            // проверка файл или папка
            if AnsiEndsText('/', xZipFile.Items[i].FileName) then
              continue;

            xZIPFileName := StringReplace(xZipFile.Items[i].FileName, '/', HMM5_PATH_DELIM, [rfReplaceAll, rfIgnoreCase]);
            index := FindFileIndex(xZIPFileName);
            if (index = -1) then
              xZIPFilesList.Add(xZIPFileName)
            else
              begin
                xFileEntry := TFileEntry(FFiles.Objects[index]);
                if xFileEntry.Archived and (xFileEntry.LastModDateTime < xLastModDateTime) then
                  begin
                    xFileEntry.FSPath := AFileName;
                    xFileEntry.LastModDateTime := xLastModDateTime;
                  end;
              end;
          end;

        xZipFile.CloseArchive;
      except
        // nothing
      end;

    finally
      xZipFile.Free;
    end;

    for i := 0 to xZIPFilesList.Count - 1 do
      begin
        xFileEntry := TFileEntry.Create;
        xFileEntry.Filename := ExtractFileName(xZIPFilesList[i]);
        xFileEntry.Path := ExtractFilePath(xZIPFilesList[i]);
        xFileEntry.Archived := true;
        xFileEntry.FSPath := AFileName;
        xFileEntry.LastModDateTime := xLastModDateTime;
        FFiles.AddObject(xZIPFilesList[i], xFileEntry);
      end;
      
  finally
    xZIPFilesList.Free;
  end;
end;

procedure TResourcesCache.WaitForInit;
begin
  if Assigned(FAsyncCall) then
    FAsyncCall.Sync();
    
//  while not Ready do
//    begin
//      Sleep(500);
//    end;
end;

procedure TResourcesCache.AsyncInit(const AHMM5Dir: string;
  const AHMM5Version: THMM5Version);
var
  xProc: TAsyncCallArgObjectProc;
begin
  Clear();

  FHMM5Dir := IncludeTrailingPathDelimiter(AHMM5Dir);
  FHMM5Version := AHMM5Version;

  xProc := AsyncInitCaller;
  FAsyncCall := AsyncCall(xProc, Self);
end;

function TResourcesCache.BuildDirFileList(const Path: string;
  const DirList, FileList: TStrings): Boolean;
var
  SearchRec: TSearchRec;
begin
  Assert(DirList <> nil);
  Assert(FileList <> nil);

  Result := FindFirst(IncludeTrailingPathDelimiter(Path) + '*', faAnyFile, SearchRec) = 0;

  DirList.BeginUpdate;
  FileList.BeginUpdate;
  try
    while Result do
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          begin
            if (SearchRec.Attr and faDirectory) <> 0 then
              DirList.Add(SearchRec.Name)
            else
              FileList.Add(SearchRec.Name);
          end;

        case FindNext(SearchRec) of
          0 : ;
          ERROR_NO_MORE_FILES :
            Break;
          else
            Result := False;
        end;
      end;
  finally
    SysUtils.FindClose(SearchRec);
    DirList.EndUpdate;
    FileList.EndUpdate
  end;
end;

function TResourcesCache.GetAllFilesFromPath(APath: string): TStrings;
var
  i: Integer;
begin
  WaitForInit();

  Result := TStringList.Create;
  for i := 0 to FFiles.Count - 1 do
    if AnsiStartsText(APath, FFiles[i]) then
      Result.Add(FFiles[i]);
end;

end.
