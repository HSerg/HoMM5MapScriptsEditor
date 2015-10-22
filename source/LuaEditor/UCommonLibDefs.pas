// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UCommonLibDefs;

interface

uses
  Windows, Classes, Contnrs, XMLIntf;

type
  // список категорий
  TXCategoryList = TStringList;

  // мета-класс для корневого класса
  TXLibRootItemClass = class of TXLibRootItem;

  // общий предок
  TXLibRootItem = class
  public
    categoryList: TXCategoryList;

    name: string;
    short_description: string;
    long_description: string;

    constructor Create; virtual;
    destructor Destroy; override;

    // чтение данных из XML-ноды
    procedure LoadFromNode(node: IXMLNode); virtual;

    // получение списка алиасов (для функций)
    function GetAliases(AValues: TStrings = nil): boolean; virtual;

    procedure Assign(Source: TXLibRootItem); virtual;

    function Clone(): TXLibRootItem;

    class function ClazzType(): TXLibRootItemClass; virtual;
  end;

  // мета-класс для корневого класса пакетных типов
  TXLibRootTableItemClass = class of TXLibRootTableItem;

  // общий предок для пакетных типов
  TXLibRootTableItem = class(TXLibRootItem)
  private
    FAliases: TStrings;
    function getFullName: string;

  public
    constructor Create; override;
    destructor Destroy; override;

  public
    nestedTable: string;
    usedAlias: string;

    // используется для отображения в synEdit
    function GetAsDecoratedText(): string; virtual; abstract;

    procedure LoadFromNode(node: IXMLNode); override;

    // список алиасов
    function GetAliases(AValues: TStrings = nil): boolean; override;

    property fullName: string read getFullName;

    procedure Assign(Source: TXLibRootItem); override;

    class function ClazzType(): TXLibRootItemClass; override;
  end;

  // переменная
  TXVarItem = class(TXLibRootTableItem)
    function GetAsDecoratedText(): string; override;

    class function ClazzType(): TXLibRootItemClass; override;
  end;

  // переменная
  TXTokenItem = class(TXLibRootTableItem)
    function GetAsDecoratedText(): string; override;

    class function ClazzType(): TXLibRootItemClass; override;
  end;

  // библиотека
  TXLibItem = class(TXLibRootTableItem)
    function GetAsDecoratedText(): string; override;

    class function ClazzType(): TXLibRootItemClass; override;
  end;

  // параметр функции
  TXFuncParam = class(TXLibRootItem)
    class function ClazzType(): TXLibRootItemClass; override;
  end;

  // функция
  TXFuncItem = class(TXLibRootTableItem)
  protected
    FParams: TObjectList;
    function GetItem(Index: integer): TXFuncParam;

  public
    constructor Create; override;
    destructor Destroy; override;

    function GetAsDecoratedText(): string; override;

    procedure SetParamsAsCommaSeparatedString(pParameters: string);
    function GetParamsAsCommaSeparatedString(): string;

    procedure LoadFromNode(node: IXMLNode); override;

    property Params[Index: integer]: TXFuncParam read GetItem;
    property ParamDefs: TObjectList read FParams;

    procedure Assign(Source: TXLibRootItem); override;

    class function ClazzType(): TXLibRootItemClass; override;
  end;

  // функция с известным метоположением заголовка и флагом успешности разбора (для игнорирования)
  TXIntFuncItem = class(TXFuncItem)
  public
    SrcFileLine: Integer;
    CompleteDeclaration: boolean;

    constructor Create; override; 

    procedure Assign(Source: TXLibRootItem); override;

    class function ClazzType(): TXLibRootItemClass; override;
  end;

implementation

uses
  SysUtils, StrUtils, JclStrings;

const
  COMMENT_TAB_SPACING = 4;
  COMMENT_STYLE = '\color{$3596D0}\style{+B} ';

{ TXFuncItem }


procedure TXFuncItem.Assign(Source: TXLibRootItem);
var
  i: integer;
begin
  inherited;

  if Source is TXFuncItem then
    begin
      for i := 0 to TXFuncItem(Source).FParams.Count - 1 do
        FParams.Add(TXFuncItem(Source).Params[i].Clone());
    end;
end;

class function TXFuncItem.ClazzType: TXLibRootItemClass;
begin
  Result := TXFuncItem;
end;

constructor TXFuncItem.Create;
begin
  inherited;

  FParams := TObjectList.Create(true);
end;

destructor TXFuncItem.Destroy;
begin
  FParams.Free;

  inherited;
end;

function TXFuncItem.GetAsDecoratedText: string;
begin
  Result := '\color{clBlue}function\color{clBlack}   \column{}\style{+B}' + name + '\style{-B}';
  Result := Result + '(' + GetParamsAsCommaSeparatedString() + ')';
  if short_description <> '' then
    begin
      Result := Result + ' ';
      if (Length(Result) mod COMMENT_TAB_SPACING) <> 0 then
        Result := StrPadRight(Result, Length(Result) + COMMENT_TAB_SPACING - (Length(Result) mod COMMENT_TAB_SPACING), ' ');
      Result := Result + COMMENT_STYLE + short_description;
    end;
end;

function TXFuncItem.GetItem(Index: integer): TXFuncParam;
begin
  Result := FParams[Index] as TXFuncParam;
end;

function TXFuncItem.GetParamsAsCommaSeparatedString: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to FParams.Count - 1 do
    begin
      if Result = '' then
        Result := Params[i].name
      else
        Result := Result + ',' + Params[i].name;
    end;
end;

procedure TXFuncItem.LoadFromNode(node: IXMLNode);
var
  subNode: IXMLNode;
  funcParam: TXFuncParam;
begin
  inherited LoadFromNode(node);

  if node.HasChildNodes then
    begin
      subNode := node.ChildNodes[0];
      while subNode<>nil do
        begin
          if (subNode.NodeType = ntElement) and AnsiSameText(subNode.NodeName, 'param') then
            begin
              funcParam := TXFuncParam.Create();
              funcParam.LoadFromNode(subNode);
              FParams.Add(funcParam);
            end;
          subNode := subNode.NextSibling;
        end;
    end;
end;

procedure TXFuncItem.SetParamsAsCommaSeparatedString(pParameters: string);
var
  xParamList: TStringList;
  i: Integer;
  xFuncParam: TXFuncParam;
begin
  xParamList := TStringList.Create;
  try
    xParamList.CommaText := pParameters;
    for i := 0 to xParamList.Count - 1 do
      begin
        xFuncParam := TXFuncParam.Create();
        xFuncParam.name := xParamList[i];
        ParamDefs.Add(xFuncParam);
      end;
  finally
    xParamList.Free;
  end;
end;

{ TXLibRootItem }

procedure TXLibRootItem.Assign(Source: TXLibRootItem);
begin
  categoryList.Assign(Source.categoryList);
  name := Source.name;
  short_description := Source.short_description;
  long_description := Source.long_description;
end;

class function TXLibRootItem.ClazzType: TXLibRootItemClass;
begin
  Result := TXLibRootItem;
end;

function TXLibRootItem.Clone: TXLibRootItem;
begin
  Result := ClazzType.Create;
  Result.Assign(Self);
end;

constructor TXLibRootItem.Create;
begin
  categoryList := TXCategoryList.Create;
end;

destructor TXLibRootItem.Destroy;
begin
  categoryList.Free;
end;

function TXLibRootItem.GetAliases(AValues: TStrings = nil): boolean;
begin
  Result := false;
end;

procedure TXLibRootItem.LoadFromNode(node: IXMLNode);
var
  subNode: IXMLNode;
begin
  categoryList.CommaText := Trim(node.Attributes['category']);
  name := Trim(node.Attributes['name']);
  short_description := Trim(node.Attributes['short_description']);

  long_description := '';
  if node.HasChildNodes then
    begin
      subNode := node.ChildNodes[0];
      while subNode<>nil do
        begin
          if subNode.NodeType = ntText then
            long_description := long_description + subNode.Text;
          subNode := subNode.NextSibling;
        end;
    end;
end;

{ TXLibRootTableItem }

procedure TXLibRootTableItem.Assign(Source: TXLibRootItem);
begin
  inherited;

  if Source is TXLibRootTableItem then
    begin
      FAliases.Assign(TXLibRootTableItem(Source).FAliases);
      nestedTable := TXLibRootTableItem(Source).nestedTable;
      usedAlias := TXLibRootTableItem(Source).usedAlias;
    end;
end;

class function TXLibRootTableItem.ClazzType: TXLibRootItemClass;
begin
  Result := TXLibRootTableItem;
end;

constructor TXLibRootTableItem.Create;
begin
  inherited;

  usedAlias := '';
  FAliases := TStringList.Create;
end;

destructor TXLibRootTableItem.Destroy;
begin
  FAliases.Free;

  inherited;
end;

function TXLibRootTableItem.GetAliases(AValues: TStrings = nil): boolean;
begin
  if Assigned(AValues) then
    AValues.Assign(FAliases);
  Result := FAliases.Count > 0;
end;

function TXLibRootTableItem.getFullName: string;
begin
  if nestedTable<>'' then
    Result := nestedTable + '.' + name
  else
    Result := name;
end;

procedure TXLibRootTableItem.LoadFromNode(node: IXMLNode);
const
  ALIAS_DELIMETER = '|';
  TABLE_DELIMETER = '.'; 
var
  sTemp: string;
  sTable: string;
  sNestedTable: string;
  xAliases: TStrings;
  i: Integer;
begin
  inherited LoadFromNode(node);

  if usedAlias <> '' then
    name := usedAlias;

  if Pos(ALIAS_DELIMETER, name) <> 0 then
    begin
      xAliases := TStringList.Create;
      try
        xAliases.Delimiter := ALIAS_DELIMETER;
        xAliases.DelimitedText := name;
        for i := xAliases.Count - 1 downto 0 do
          begin
            sTemp := Trim(xAliases[i]);
            if sTemp = '' then
              xAliases.Delete(i)
            else
              xAliases[i] := sTemp;
          end;
        if xAliases.Count > 1 then
          begin
            name := xAliases[0];
            xAliases.Delete(0);
            FAliases.Assign(xAliases);
          end;
      finally
        xAliases.Free;
      end;
    end;

  sTemp := name;

  while (Pos(TABLE_DELIMETER, sTemp) <> 0) do
    begin
      sTable := Copy(sTemp, 1, Pos(TABLE_DELIMETER, sTemp) - 1);
      sTemp := StringReplace(sTemp, sTable + TABLE_DELIMETER, '', [rfReplaceAll, rfIgnoreCase]);
      if sNestedTable = '' then
        sNestedTable := Trim(sTable)
      else
        sNestedTable := sNestedTable + TABLE_DELIMETER + Trim(sTable);
    end;

  nestedTable := sNestedTable;
  name := Trim(sTemp);
end;

{ TXVarItem }

class function TXVarItem.ClazzType: TXLibRootItemClass;
begin
  Result := TXVarItem;
end;

function TXVarItem.GetAsDecoratedText: string;
begin
  Result := '\color{clMaroon}global var\color{clBlack} \column{}\style{+B}' + name + '\style{-B}';
  if short_description <> '' then
    begin
      Result := Result + ' ';
      if (Length(Result) mod COMMENT_TAB_SPACING) <> 0 then
        Result := StrPadRight(Result, Length(Result) + COMMENT_TAB_SPACING - (Length(Result) mod COMMENT_TAB_SPACING), ' ');
      Result := Result + COMMENT_STYLE + short_description;
    end;
end;

{ TXLibItem }

class function TXLibItem.ClazzType: TXLibRootItemClass;
begin
  Result := TXLibItem;
end;

function TXLibItem.GetAsDecoratedText: string;
begin
  Result := '\color{clGreen}library\color{clBlack}       \column{}\style{+B}' + name + '\style{-B}';
  if short_description <> '' then
    begin
      Result := Result + ' ';
      if (Length(Result) mod COMMENT_TAB_SPACING) <> 0 then
        Result := StrPadRight(Result, Length(Result) + COMMENT_TAB_SPACING - (Length(Result) mod COMMENT_TAB_SPACING), ' ');
      Result := Result + COMMENT_STYLE + short_description;
    end;
end;

{ TXIntFuncItem }

procedure TXIntFuncItem.Assign(Source: TXLibRootItem);
begin
  inherited;

  if Source is TXIntFuncItem then
    begin
      SrcFileLine := TXIntFuncItem(Source).SrcFileLine;
      CompleteDeclaration := TXIntFuncItem(Source).CompleteDeclaration;
    end;
end;

class function TXIntFuncItem.ClazzType: TXLibRootItemClass;
begin
  Result := TXIntFuncItem;
end;

constructor TXIntFuncItem.Create;
begin
  inherited;

  SrcFileLine := -1;
  CompleteDeclaration := true;
end;

{ TXFuncParam }

class function TXFuncParam.ClazzType: TXLibRootItemClass;
begin
  Result := TXFuncParam;
end;

{ TXTokenItem }

class function TXTokenItem.ClazzType: TXLibRootItemClass;
begin
  Result := TXTokenItem;
end;

function TXTokenItem.GetAsDecoratedText: string;
begin
  Result := '\color{clGray}local def\color{clBlack} \column{}\style{+B}' + name + '\style{-B}';
  if short_description <> '' then
    begin
      Result := Result + ' ';
      if (Length(Result) mod COMMENT_TAB_SPACING) <> 0 then
        Result := StrPadRight(Result, Length(Result) + COMMENT_TAB_SPACING - (Length(Result) mod COMMENT_TAB_SPACING), ' ');
      Result := Result + COMMENT_STYLE + short_description;
    end;
end;

end.
