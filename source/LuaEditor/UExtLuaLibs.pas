// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UExtLuaLibs;

interface

uses
  Windows, Contnrs, UAbstractLibParser;

type
  TLuaLibs = class
  protected
    FLibs: TObjectList;
    function GetLibItem(Index: integer): TAbstractLibParser;
    function GetOwnsLibs: boolean; virtual;
    procedure SetOwnsLibs(const Value: boolean); virtual;

  public
    constructor Create();
    destructor Destroy(); override;

    procedure AddLib(ALib: TAbstractLibParser);

    function Count: integer;
    property OwnsLibs: boolean read GetOwnsLibs write SetOwnsLibs;
    property Libs[Index: integer]: TAbstractLibParser read GetLibItem;
  end;

  TExtLuaLibs = class(TLuaLibs)
  protected
    FLibDir: string;
    FLastScanTime: TDateTime;
    procedure SetOwnsLibs(const Value: boolean); override;
    procedure SetLibDir(const Value: string);

  public
    constructor Create();
    destructor Destroy(); override;

    procedure RefreshLibs(AForce: boolean = false);

    property LastScanTime: TDateTime read FLastScanTime;
    property LibDir: string read FLibDir write SetLibDir;
  end;

implementation

uses
  UXLibFileParser, SysUtils, Forms, Classes;

constructor TExtLuaLibs.Create;
begin
  inherited;
  FLibs.OwnsObjects := true;
  FLastScanTime := 0;
end;

destructor TExtLuaLibs.Destroy;
begin
  inherited;
end;

procedure TExtLuaLibs.RefreshLibs(AForce: boolean = false);
const
  RESCAN_PERIOD = 0.00015;
var
  hFileSearch: TSearchRec;
  xXMLLib: TXLibFileParser;
  xStream: TMemoryStream; 
begin
  if not AForce then
    if Now()-FLastScanTime < RESCAN_PERIOD then
      exit;

  FLibs.Clear;

  if FindFirst(FLibDir+'*.xlib', faAnyFile, hFileSearch) = 0 then
    begin
      repeat
        xXMLLib := TXLibFileParser.Create;
        xStream := TMemoryStream.Create();
        try
          xStream.LoadFromFile(FLibDir+hFileSearch.Name);
          xXMLLib.Parse(xStream, ChangeFileExt(hFileSearch.Name, ''));
          if xXMLLib.Count>0 then
            FLibs.Add(xXMLLib);
        finally
          xStream.Free;
        end;
      until (FindNext(hFileSearch) <> 0);
      FindClose(hFileSearch);
    end;

  FLastScanTime := Now();
end;

procedure TExtLuaLibs.SetLibDir(const Value: string);
begin
  FLastScanTime := 0;
  FLibs.Clear;
  FLibDir := Value;
end;

procedure TExtLuaLibs.SetOwnsLibs(const Value: boolean);
begin
  // nothing
end;

{ TLuaLibs }

procedure TLuaLibs.AddLib(ALib: TAbstractLibParser);
begin
  if FLibs.IndexOf(ALib) = -1 then
    FLibs.Add(ALib);
end;

function TLuaLibs.Count: integer;
begin
  Result := FLibs.Count;
end;

constructor TLuaLibs.Create;
begin
  FLibs := TObjectList.Create(false);
end;

destructor TLuaLibs.Destroy;
begin
  FreeAndNil(FLibs);
end;

function TLuaLibs.GetLibItem(Index: integer): TAbstractLibParser;
begin
  Result := FLibs[Index] as TAbstractLibParser;
end;

function TLuaLibs.GetOwnsLibs: boolean;
begin
  Result := FLibs.OwnsObjects;
end;

procedure TLuaLibs.SetOwnsLibs(const Value: boolean);
begin
  FLibs.OwnsObjects := Value;
end;

end.

