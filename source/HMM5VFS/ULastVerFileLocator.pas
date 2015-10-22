// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit ULastVerFileLocator;

interface

uses
  Classes;

type
  TLastVerFileLocator = class
  private
    FUseAddonData: boolean;
    FHMM5Dir: string;

    FOutStream: TMemoryStream;
    FCurrentFileDateTime: TDateTime;
    FSearchFileName: string;
    FUnpackedFileFind: boolean;

    procedure SetHMM5Dir(const Value: string);

    function ScanPak(FileName: string; UseCurrentFileDateTime: boolean): boolean;
    function ScanFile(FileName: string; UseCurrentFileDateTime: boolean): boolean;
    function ScanDataDir(ADir: string; UseCurrentFileDateTime: boolean): boolean;

  public
    function GetFileAsStream(FileName: string; pStream: TStream): boolean;

    property HMM5Dir: string read FHMM5Dir write SetHMM5Dir;
    property UseAddonData: boolean read FUseAddonData write FUseAddonData;

  end;

implementation

uses
  Windows, AbZipKit, AbArcTyp, AbUtils, JclFileUtils, JclDateTime, SysUtils,
  Math;

function CompareFileAges(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := Sign(FileTimeToLocalDateTime(GetFileLastWrite(List[Index2])) -
                 FileTimeToLocalDateTime(GetFileLastWrite(List[Index1])));
end;

{ TLastVerFileLocator }

function TLastVerFileLocator.GetFileAsStream(FileName: string;
  pStream: TStream): boolean;
begin
  Result := false;

  FSearchFileName := FileName;
  FUnpackedFileFind := false;

  FOutStream := TMemoryStream.Create;
  try
    if FUseAddonData then
      Result := ScanDataDir(IncludeTrailingPathDelimiter(FHMM5Dir + 'dataa1'), false);

    if FUnpackedFileFind = false then
      Result := ScanDataDir(IncludeTrailingPathDelimiter(FHMM5Dir + 'data'), Result) or Result;

    if Result then
      pStream.CopyFrom(FOutStream, 0);
  finally
    FOutStream.Free;
  end;
end;

function TLastVerFileLocator.ScanDataDir(ADir: string;
  UseCurrentFileDateTime: boolean): boolean;
var
  xPackFileList: TStringList;
  i: integer;
begin
  Result := ScanFile(ADir + FSearchFileName, false);
  if Result then
    exit;

  Result := UseCurrentFileDateTime;
  xPackFileList := TStringList.Create;
  try
    BuildFileList(ADir + '*.pak', faAnyFile, xPackFileList);

    for i := xPackFileList.Count - 1 downto 0 do
      if not FileExists(ADir + xPackFileList[i]) then
        xPackFileList.Delete(i)
      else
        xPackFileList[i] := ADir + xPackFileList[i];

    xPackFileList.CustomSort(CompareFileAges);

    for i := 0 to xPackFileList.Count - 1 do
      Result := ScanPak(xPackFileList[i], Result) or Result;
  finally
    xPackFileList.Free;
  end;
end;

function TLastVerFileLocator.ScanFile(FileName: string;
  UseCurrentFileDateTime: boolean): boolean;
var
  xFileLastModDateTime: TDateTime;
begin
  Result := false;

  if not FileExists(FileName) then
    exit;
    
  xFileLastModDateTime := FileTimeToLocalDateTime(GetFileLastWrite(FileName));
  if UseCurrentFileDateTime and (FCurrentFileDateTime > xFileLastModDateTime) then
    exit;

  FOutStream.Size := 0;
  FOutStream.LoadFromFile(FileName);

  FCurrentFileDateTime := xFileLastModDateTime;
  FUnpackedFileFind := true;

  Result := true;
end;

function TLastVerFileLocator.ScanPak(FileName: string;
  UseCurrentFileDateTime: boolean): boolean;
var
  xZipFile: TAbZipKit;
  xIndex: integer;
  xSearchFileName: string;
  xFileLastModDateTime: TDateTime;
begin
  Result := false;

  if not FileExists(FileName) then
    exit;
    
  xFileLastModDateTime := FileTimeToLocalDateTime(GetFileLastWrite(FileName));
  if UseCurrentFileDateTime and (FCurrentFileDateTime > xFileLastModDateTime) then
    exit;

  xZipFile := TAbZipKit.Create(nil);
  try
    try
      xZipFile.ForceType := true;
      xZipFile.ArchiveType := atZip;
      xZipFile.ExtractOptions := [];
      xZipFile.StoreOptions := xZipFile.StoreOptions + [soReplace];

      xZipFile.OpenArchive(FileName);

      xSearchFileName := StringReplace(FSearchFileName, '\', '/', [rfReplaceAll, rfIgnoreCase]);
      xIndex := xZipFile.FindFile(xSearchFileName);

      if xIndex <> -1 then
        begin
          FOutStream.Size := 0;
          xZipFile.ExtractToStream(xZipFile.Items[xIndex].FileName, FOutStream);

          FCurrentFileDateTime := xFileLastModDateTime;

          Result := true;
        end;

      xZipFile.CloseArchive;
    except
      // nothing
    end;

  finally
    xZipFile.Free;
  end;
end;

procedure TLastVerFileLocator.SetHMM5Dir(const Value: string);
begin
  FHMM5Dir := IncludeTrailingPathDelimiter(Value);
end;

end.
