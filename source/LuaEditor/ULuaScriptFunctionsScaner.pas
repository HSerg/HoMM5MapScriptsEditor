// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit ULuaScriptFunctionsScaner;

interface

uses
  Classes, SynEditHighlighter, Contnrs, ULuaLibTable, UCommonLibDefs;

type
  TLuaScriptFunctionsScaner = class
  private
    FHighlighter: TSynCustomHighlighter;
    FItems: TObjectList;
    FSource: string;
    FSourceChanged: boolean;

    FLibTable: TLuaLibTable;

    procedure AddFunction(AFunctionItem: TXIntFuncItem);
    function GetFunctionsCount: integer;
    function GetFunction(Index: integer): TXIntFuncItem;

    procedure SkipSpaces(const preNext: boolean = false);
    function IsTokenChar(AChr: char): boolean;
    function ReadDottedIdentifier(var pDottedIdentifier: string): boolean;

    procedure Parse();
    procedure FillLibTable();

  public
    constructor Create;
    destructor Destroy; override;

    procedure Scan(LuaScript: string);

    property LibTable: TLuaLibTable read FLibTable;
    property Functions[Index: integer]: TXIntFuncItem read GetFunction;
    property FunctionsCount: integer read GetFunctionsCount;
  end;

implementation

uses
  Windows, SysUtils, LuaSyntax;
  
constructor TLuaScriptFunctionsScaner.Create;
begin
  FLibTable := TLuaLibTable.Create;
  FHighlighter := TSynLuaSyn.Create(nil);
  FItems := TObjectList.Create(true);
end;

destructor TLuaScriptFunctionsScaner.Destroy;
begin
  FItems.Free;
  FHighlighter.Free;
  FLibTable.Free;
end;

procedure TLuaScriptFunctionsScaner.FillLibTable;
var
  i: Integer;
begin
  for i := 0 to FunctionsCount - 1 do
    begin
      FLibTable.AddFromFunctionDef(Functions[i].name);
    end;
end;

function TLuaScriptFunctionsScaner.GetFunction(Index: integer): TXIntFuncItem;
begin
  Result := FItems[Index] as TXIntFuncItem;
end;

function TLuaScriptFunctionsScaner.GetFunctionsCount: integer;
begin
  Result := FItems.Count;
end;

procedure TLuaScriptFunctionsScaner.AddFunction(AFunctionItem: TXIntFuncItem);
begin
  FItems.Add(AFunctionItem);
end;

procedure TLuaScriptFunctionsScaner.SkipSpaces(const preNext: boolean = false);
begin
  if preNext then
    FHighlighter.Next;
  while not FHighlighter.GetEol and (FHighlighter.GetTokenKind = Ord(LuaSyntax.tkSpace)) do
    FHighlighter.Next;
end;

function TLuaScriptFunctionsScaner.IsTokenChar(AChr: char): boolean;
begin
  Result := (FHighlighter.GetTokenKind = Ord(LuaSyntax.tkUnknown)) and (FHighlighter.GetToken = AChr);
end;

procedure TLuaScriptFunctionsScaner.Parse;
var
  s: string;
  xFunctionItem: TXIntFuncItem;
  xParamList: TStrings;
  state: (stNone, stParam, stDelim, stPass, stExit);
  completed: (cUnk, cTrue, cFalse);
  mayBeFunctionName: string;
  mayBeFunctionNameWithErrors: boolean;
  stdFunctionName: string; 
begin
  xParamList := TStringList.Create;
  try
    // scan the source text for the keywords, cancel if the source in the
    // editor has been changed again
    FHighlighter.ResetRange;
    FHighlighter.SetLine(FSource, 1);
    mayBeFunctionName := '';
    while not FSourceChanged and not FHighlighter.GetEol do
      begin
        mayBeFunctionNameWithErrors := not readDottedIdentifier(mayBeFunctionName);

        if FHighlighter.GetEol() then
          continue;

        if mayBeFunctionName <> '' then
          begin
            if IsTokenChar('=') then
              begin
                SkipSpaces(true);
              end
            else
              begin
                mayBeFunctionName := '';
                continue;
              end;
          end;
          
        if FHighlighter.GetTokenKind = Ord(LuaSyntax.tkKey) then begin
          s := FHighlighter.GetToken;
          if AnsiSameText(s, 'FUNCTION') then
            begin
              completed := cUnk;
              xFunctionItem := TXIntFuncItem.Create;
              try
                xParamList.Clear;

                xFunctionItem.SrcFileLine := FHighlighter.GetTokenPos;

                SkipSpaces(true);
                if FHighlighter.GetEol then
                  continue;
                
                // имя функции
                if readDottedIdentifier(stdFunctionName) then
                  begin
                    xFunctionItem.name := stdFunctionName;
                  end
                else
                  begin
                    if stdFunctionName <> '' then
                      begin
                        xFunctionItem.name := stdFunctionName;
                        completed := cFalse;
                      end
                    else
                      begin
                        if mayBeFunctionName = '' then
                          begin
                            xFunctionItem.name := '???';
                            completed := cFalse;
                          end
                        else
                          begin
                            xFunctionItem.name := mayBeFunctionName;
                            if mayBeFunctionNameWithErrors then
                              completed := cFalse;
                          end;
                      end
                  end;

                // скобка '('
                if IsTokenChar('(') then
                  begin
                    state := stNone;

                    while not (state in [stExit, stPass]) do
                      begin
                        // 'параметр ,' или 'параметр )' или ')'
                        FHighlighter.Next; SkipSpaces();
                        if FHighlighter.GetEol then
                          begin
                            state := stExit;
                          end
                        else
                        if IsTokenChar(')') then
                          begin
                            if state in [stNone, stParam] then
                              state := stPass
                            else
                              state := stExit;
                          end
                        else
                        if IsTokenChar(',') then
                          begin
                            if state in [stParam] then
                              state := stDelim
                            else
                              state := stExit;
                          end
                        else
                        if (FHighlighter.GetTokenKind = Ord(LuaSyntax.tkIdentifier)) then
                          begin
                            if state in [stNone, stDelim] then
                              begin
                                state := stParam;
                                xParamList.Add(FHighlighter.GetToken);
                              end
                            else
                              state := stExit;
                          end
                        else
                          begin
                            state := stExit;
                          end;
                      end;

                    if state = stPass then
                      begin
                        xFunctionItem.SetParamsAsCommaSeparatedString(xParamList.CommaText);
                        if completed = cUnk then
                          completed := cTrue;
                      end
                    else
                      begin
                        xFunctionItem.SetParamsAsCommaSeparatedString('???');
                        completed := cFalse;
                      end;
                  end
                else
                  begin
                    xFunctionItem.SetParamsAsCommaSeparatedString('???');
                    completed := cFalse;
                  end;

              finally
                if xFunctionItem.name = '' then
                  begin
                    if completed = cUnk then
                      completed := cFalse;
                  end;

                xFunctionItem.CompleteDeclaration := (completed = cTrue);

                AddFunction(xFunctionItem);
              end;
            end;
      end;

      mayBeFunctionName := '';

      FHighlighter.Next;
    end;
  finally
    xParamList.Free;
  end;
end;

function TLuaScriptFunctionsScaner.ReadDottedIdentifier(var pDottedIdentifier: string): boolean;
begin
  pDottedIdentifier := '';

  while (FHighlighter.GetTokenKind = Ord(LuaSyntax.tkIdentifier)) do
    begin
      pDottedIdentifier := pDottedIdentifier + FHighlighter.GetToken;

      SkipSpaces(true);
      if FHighlighter.GetEol() then
        break;

      if IsTokenChar('.') then
        pDottedIdentifier := pDottedIdentifier + '.'
      else
        break;

      SkipSpaces(true);
      if FHighlighter.GetEol() then
        break;
    end;

  if (pDottedIdentifier = '') then
    begin
      Result := false;
    end
  else
  if (pDottedIdentifier[Length(pDottedIdentifier)-1]='.') then
    begin
      pDottedIdentifier := pDottedIdentifier + '???';
      Result := false;
    end
  else
    begin
      Result := true;
    end;
end;

procedure TLuaScriptFunctionsScaner.Scan(LuaScript: string);
begin
  FSource := LuaScript;
  FSourceChanged := false;
  FItems.Clear;

  Parse();

  FillLibTable();
end;

end.
