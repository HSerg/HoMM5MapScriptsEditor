// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UZipFileUtils;

interface

uses
  Classes;

function checkFileName(AFileName: string): boolean;
procedure saveAsZIPFile(AZIPFileName, AItemFileName: string; ASrcStream: TStream);

implementation

uses
  AbZipKit, AbUtils, AbArcTyp;

function checkFileName(AFileName: string): boolean;
var
  i: integer;
begin
  Result := true;
  for i := 1 to Length(AFileName) do
    if not (AFileName[i] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', ' ', '.', '!']) then
      begin
        Result := false;
        break;
      end;
end;

procedure saveAsZIPFile(AZIPFileName, AItemFileName: string; ASrcStream: TStream);
var
  xZipFile: TAbZipKit;
begin
  xZipFile := TAbZipKit.Create(nil);
  try
    xZipFile.ForceType := true;
    xZipFile.ArchiveType := atZip;
    xZipFile.ExtractOptions := [];
    xZipFile.StoreOptions := xZipFile.StoreOptions + [soReplace];
    xZipFile.OpenArchive(AZIPFileName);
    xZipFile.AddFromStream(AItemFileName, ASrcStream);
    xZipFile.CloseArchive;
  finally
    xZipFile.Free;
  end;
end;

end.
