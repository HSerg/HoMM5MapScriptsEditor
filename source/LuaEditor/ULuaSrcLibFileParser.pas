// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit ULuaSrcLibFileParser;

interface

uses
  Classes, Contnrs, UCommonLibDefs, UAbstractLibParser;

type
  TLuaSrcLibFileParser = class(TAbstractLibParser)
  protected
    FItems: TObjectList;
    FLibTable: TStringList;
    FLuaSrc: string;
    FLibName: string;

  protected
    function GetItems(Index: integer): TXLibRootTableItem; override;
    function GetLibName: string; override;
    function GetLibTable: TStringList; override;

  public
    constructor Create;
    destructor Destroy; override;

  public
    procedure Parse(ADataStream: TStream; ADefaultLibName: string); override;
    function Count: integer; override;

    property LuaSrc: string read FLuaSrc;
  end;

implementation

uses
  Windows, StrUtils, SysUtils, ULuaScriptFunctionsScaner;

function TLuaSrcLibFileParser.Count: integer;
begin
  Result := FItems.Count;
end;

constructor TLuaSrcLibFileParser.Create;
begin
  FLibType := lltLuaSrcLib;
  FLibTable := TStringList.Create;
  FItems := TObjectList.Create(true);
  FLuaSrc := '';
end;

destructor TLuaSrcLibFileParser.Destroy;
begin
  FLuaSrc := '';
  FItems.Free;
  FLibTable.Free;
end;

procedure TLuaSrcLibFileParser.Parse(ADataStream: TStream; ADefaultLibName: string);
var
  j: Integer;
  functionListParser: TLuaScriptFunctionsScaner;
  xStream: TStringStream;
  libItem: TXLibItem;
begin
  FLibTable.Clear;
  FItems.Clear;

  FLibName := ADefaultLibName;

  if ADataStream is TStringStream then
    begin
      FLuaSrc := (ADataStream as TStringStream).DataString;
    end
  else
    begin
      xStream := TStringStream.Create('');
      try
        xStream.CopyFrom(ADataStream, 0);
        FLuaSrc := xStream.DataString;
      finally
        xStream.Free;
      end;
    end;

  // добавление в список локальные функции и библиотеки (массивы ?)
  functionListParser := TLuaScriptFunctionsScaner.Create;
  try
    functionListParser.Scan(FLuaSrc);
    for j := 0 to functionListParser.FunctionsCount - 1 do
      begin
        FItems.Add(functionListParser.Functions[j].Clone);
      end;
    for j := 0 to functionListParser.LibTable.Count - 1 do
      begin
        libItem := TXLibItem.Create;
        libItem.name := functionListParser.LibTable[j];
        FItems.Add(libItem);
      end;
  finally
    functionListParser.Free;
  end;
end;

function TLuaSrcLibFileParser.GetItems(Index: integer): TXLibRootTableItem;
begin
  Result := FItems[Index] as TXLibRootTableItem;
end;

function TLuaSrcLibFileParser.GetLibName: string;
begin
  Result := FLibName;
end;

function TLuaSrcLibFileParser.GetLibTable: TStringList;
begin
  Result := FLibTable;
end;

end.
