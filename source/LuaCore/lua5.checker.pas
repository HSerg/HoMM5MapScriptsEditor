// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit lua5.checker;

interface

uses
  SysUtils;

type
  TLua5SyntaxChecker = class
  private
    procedure ProcessErrorMessage(const ErrMsg: string; var Title: string; var Line: Integer; var Msg: string);
  public
    function Check(const ACode: string; var ErrorMsg: string; var ErrorLine: integer): boolean;
  end;

implementation

const
  LUA5_DLL = 'lua5.dll';
  LUA_TSTRING = 4;

type
  lua_State = record
  end;
  Plua_State = ^lua_State;

  size_t = Integer;

function luaL_newstate: Plua_State; cdecl external LUA5_DLL;
function luaL_loadbuffer(L: Plua_State; const Buff: PAnsiChar; SZ: size_t; const Name: PAnsiChar): Integer; cdecl external LUA5_DLL;
function lua_type(L: Plua_State; Idx: Integer): Integer; cdecl external LUA5_DLL;
function lua_tolstring(L: Plua_State; Idx: Integer): PAnsiChar; cdecl external LUA5_DLL;
function lua_objlen(L: Plua_State; Idx: Integer): size_t; cdecl external LUA5_DLL;
function lua_gettop(L: Plua_State): Integer;  cdecl external LUA5_DLL;

{ TLuaSyntaxChecker }

function TLua5SyntaxChecker.Check(const ACode: string; var ErrorMsg: string;
  var ErrorLine: integer): boolean;
var
  L: Plua_State;
  Title, Msg: string;
  Line: Integer;
  index: integer;
  errorMessageLenght: Integer;
  errorMessage: AnsiString;
  code: AnsiString;
  name: AnsiString;
begin
  code := ACode;
  name := 'script';

  L := luaL_newstate();

  if (luaL_loadbuffer(L, PAnsiChar(code), Length(code), PAnsiChar(name)) = 0) then
    begin
      Result := true;
      exit;
    end;

  index := lua_gettop(L);
  if lua_type(L, Index) = LUA_TSTRING then
    begin
      errorMessageLenght := lua_objlen(L, index);
      SetLength(errorMessage, errorMessageLenght);
      if (errorMessageLenght > 0) then
        Move(lua_tolstring(L, Index)^, errorMessage[1], errorMessageLenght);
    end;

  ProcessErrorMessage(errorMessage, Title, Line, Msg);

  ErrorMsg := Msg;
  ErrorLine := Line;
  Result := false;
end;

procedure TLua5SyntaxChecker.ProcessErrorMessage(const ErrMsg: string;
  var Title: string; var Line: Integer; var Msg: string);
const
  Term = #$00;

  function S(const Index: Integer): Char;
  begin
    if (Index <= Length(ErrMsg)) then
      Result := ErrMsg[Index]
    else
      Result := Term;
  end;

  function IsDigit(const C: Char): Boolean;
  begin
    Result := ('0' <= C) and (C <= '9');
  end;

  function PP(var Index: Integer): Integer;
  begin
    Inc(Index);
    Result := Index;
  end;

var
  I, Start, Stop: Integer;
  LS: string;
  Find: Boolean;
begin
  // ErrMsg = Title:Line:Message
  Title := '';
  Line := 0;
  Msg := ErrMsg;
  Find := False;
  I := 1 - 1;
  Stop := 0;
  repeat
    while (S(PP(I)) <> ':') do
      if (S(I) = Term) then
        Exit;
    Start := I;
    if (not IsDigit(S(PP(I)))) then
      Continue;
    while (IsDigit(S(PP(I)))) do
      if (S(I - 1) = Term) then
        Exit;
    Stop := I;
    if (S(I) = ':') then
      Find := True;
  until (Find);
  Title := Copy(ErrMsg, 1, Start - 1);
  LS := Copy(ErrMsg, Start + 1, Stop - Start - 1);
  Line := StrToIntDef(LS, 0);
  Msg := Copy(ErrMsg, Stop + 1, Length(ErrMsg));
end;

end.
