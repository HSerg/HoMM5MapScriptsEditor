// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit ULuaScriptTokensScaner;


interface

uses
  Classes, SynEditHighlighter, Contnrs, ULuaLibTable, UCommonLibDefs,
  UAbstractLibParser;

type
  TLuaScriptTokensScaner = class
  public
    const LUA_KEYS: array [0..20] of string = ('if', 'do', 'and', 'in', 'end',
      'or', 'nil', 'for', 'break', 'else', 'local', 'false', 'then', 'not',
      'while', 'elseif', 'true', 'repeat', 'until', 'return', 'function' );
    const END_OF_TOKEN_CHR = '"''{}()[],;+-/*<>=.~ '#9#13#10;

  private
    FItems: TObjectList;
    FSource: string;
    FSourceChanged: boolean;
    FIgnoreSingleCharTokens: boolean;
    FIgnoreLuaKeys: boolean;

    FLuaKeys: TStringList;
    FPlainTextNames: TStringList;
    FLibTable: TLuaLibTable;

    function AddToken(AVarItem: TXTokenItem): boolean;
    function GetTokensCount: integer;
    function GetToken(Index: integer): TXTokenItem;

    procedure Parse();
    procedure FillLibTable();
    procedure FillTokens();

  public
    constructor Create;
    destructor Destroy; override;

    procedure Scan(LuaScript: string);
    procedure ExcludeLibDefs(ALib: TAbstractLibParser);

    property IgnoreLuaKeys: boolean read FIgnoreLuaKeys write FIgnoreLuaKeys;
    property IgnoreSingleCharTokens: boolean read FIgnoreSingleCharTokens write FIgnoreSingleCharTokens;
    property LibTable: TLuaLibTable read FLibTable;
    property Vars[Index: integer]: TXTokenItem read GetToken;
    property VarsCount: integer read GetTokensCount;
  end;

implementation

uses
  Windows, SysUtils, LuaSyntax;

constructor TLuaScriptTokensScaner.Create;
var
  i: Integer;
begin
  FLibTable := TLuaLibTable.Create;
  FItems := TObjectList.Create(true);
  FIgnoreSingleCharTokens := true;
  FIgnoreLuaKeys := true;
  FLuaKeys := TStringList.Create;
  for i := 0 to Length(LUA_KEYS) - 1 do
    FLuaKeys.Add(LUA_KEYS[i]);
  FLuaKeys.Sorted := true;
  FPlainTextNames := TStringList.Create;
  FPlainTextNames.Sorted := true;
  FPlainTextNames.Duplicates := dupIgnore;
end;

destructor TLuaScriptTokensScaner.Destroy;
begin
  FPlainTextNames.Free;
  FLuaKeys.Free;
  FItems.Free;
  FLibTable.Free;
end;

procedure TLuaScriptTokensScaner.ExcludeLibDefs(ALib: TAbstractLibParser);
var
  i: Integer;
  libContent: TStringList;
begin
  libContent := TStringList.Create;
  try
    for i := 0 to ALib.Count - 1 do
      libContent.Add(ALib.Items[i].name);
    libContent.Sorted := true;
    for i := VarsCount - 1 downto 0 do
      if libContent.IndexOf(Vars[i].name) <> -1 then
        FItems.Delete(i);
  finally
    libContent.Free;
  end;
end;

procedure TLuaScriptTokensScaner.FillLibTable;
var
  i: Integer;
begin
  for i := 0 to VarsCount - 1 do
    FLibTable.AddFromFunctionDef(Vars[i].name);
end;

procedure TLuaScriptTokensScaner.FillTokens;
var
  i: Integer;
  xTokenItem: TXTokenItem;
begin
  xTokenItem := nil;
  for i := 0 to FPlainTextNames.Count - 1 do
    begin
      if xTokenItem = nil then
        xTokenItem := TXTokenItem.Create;
      xTokenItem.name := FPlainTextNames[i];
      if AddToken(xTokenItem) then
        xTokenItem := nil;
    end;
end;

function TLuaScriptTokensScaner.GetToken(Index: integer): TXTokenItem;
begin
  Result := FItems[Index] as TXTokenItem;
end;

function TLuaScriptTokensScaner.GetTokensCount: integer;
begin
  Result := FItems.Count;
end;

function TLuaScriptTokensScaner.AddToken(AVarItem: TXTokenItem): boolean;
begin
  Result := false;
  if FIgnoreLuaKeys and (FLuaKeys.IndexOf(AVarItem.name) <> -1) then
    exit;
  if FIgnoreSingleCharTokens and (Length(AVarItem.name) = 1) then
    exit;
  FItems.Add(AVarItem);
  Result := true;
end;

procedure TLuaScriptTokensScaner.Parse;
var
  line: integer;
  textLine: string;
  token: string;
  i: Integer;
  xLines: TStrings;
begin
  xLines := TStringList.Create;
  try
    xLines.Text := FSource;
    for line := 0 to xLines.Count-1 do
      begin
        textLine := xLines[line];
        token := '';
        for i := 1 to Length(textLine) do
          begin
            if pos(textLine[i], END_OF_TOKEN_CHR) = 0 then
              begin
                token := token + textLine[i];
                continue;
              end;

            if token <> '' then
              FPlainTextNames.Add(token);

            token := '';
          end;
        if token <> '' then
          FPlainTextNames.Add(token);
      end;
  finally
    xLines.Free;
  end;
end;

procedure TLuaScriptTokensScaner.Scan(LuaScript: string);
begin
  FSource := LuaScript;
  FSourceChanged := false;
  FItems.Clear;
  FPlainTextNames.Clear;

  Parse();

  FillTokens();
  FillLibTable();
end;

end.
