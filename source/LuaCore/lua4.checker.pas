// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit lua4.checker;

interface

uses
  JclPCRE;

type
  TLua4SyntaxChecker = class
  private
    const BUFFER_NAME: AnsiString = 'script';
    const PREFIX_CODE: AnsiString = '__stop()'#$0D#$0A;

  private
    FRegex: TJclRegEx;

  public
    constructor Create();
    destructor Destroy(); override;

    function Check(const ACode: string; var ErrorMsg: string; var ErrorLine: integer): boolean;
  end;

implementation

uses
  SysUtils, StrUtils;

const
  LUA4_DLL = 'lua4.dll';
  LUA_ERRORMESSAGE = '_ERRORMESSAGE';
  LUA_TSTRING = 3;
  LUA_IDSIZE = 60;

  LUA_OK        = 0;
  LUA_ERRRUN    = 1;
  LUA_ERRFILE   = 2;
  LUA_ERRSYNTAX = 3;
  LUA_ERRMEM    = 4;
  LUA_ERRERR    = 5;

type
  lua_State = record
  end;
  Plua_State = ^lua_State;

  lua_CFunction = function (L: Plua_State): Integer; cdecl;

  size_t = Integer;

  lua_Debug = record
    event: PAnsiChar;
    currentline: Integer;
    name: PAnsiChar;
    namewhat: PAnsiChar;
    nups: Integer;
    linedefined: Integer;
    what: PAnsiChar;
    source: PAnsiChar;
    short_src: array [0..LUA_IDSIZE - 1] of AnsiChar;
    (* private part *)
    i_ci: Integer;  (* active function *)
  end;
  Plua_Debug = ^lua_Debug;

function lua_open(AStackSize: Integer): Plua_State; cdecl external LUA4_DLL;
procedure lua_close(L: Plua_State); cdecl external LUA4_DLL;
function lua_dobuffer(L: Plua_State; const Buff: PAnsiChar; SZ: size_t; const Name: PAnsiChar): Integer; cdecl external LUA4_DLL;
function lua_type(L: Plua_State; Idx: Integer): Integer; cdecl external LUA4_DLL;
function lua_tostring(L: Plua_State; Idx: Integer): PChar; cdecl external LUA4_DLL;
function lua_strlen(L: Plua_State; Idx: Integer): size_t; cdecl external LUA4_DLL;
function lua_gettop(L: Plua_State): Integer;  cdecl external LUA4_DLL;
procedure lua_setglobal(L: Plua_State; const S: PAnsiChar); cdecl external LUA4_DLL;
procedure lua_pushcclosure(L: Plua_State; fn: lua_CFunction; n: integer); cdecl external LUA4_DLL;
function lua_getstack(L: Plua_State; Level: Integer; AR: Plua_Debug): Integer; cdecl external LUA4_DLL;
function lua_getinfo(L: Plua_State; const What: PAnsiChar; AR: Plua_Debug): Integer; cdecl external LUA4_DLL;

type
  TErrors = class
    FList: array of record L: Plua_State; Text: string; Line: integer; end;
    procedure Add(L: Plua_State; Text: string; Line: integer);
    function GetLast(L: Plua_State): integer;
    procedure Clear;
  end;

var
  errors: TErrors;

procedure TErrors.Add(L: Plua_State; Text: string; Line: integer);
var
  index: integer;
begin
  index := Length(FList);
  SetLength(FList, index+1);
  FList[index].L := L;
  FList[index].Text := Text;
  FList[index].Line := Line;
end;

function TErrors.GetLast(L: Plua_State): integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Length(FList)-1 downto 0 do
    if FList[i].L = L then
      begin
        Result := i;
        break;
      end;
end;

procedure TErrors.Clear;
begin
  SetLength(FList, 0);
end;

{ TLuaSyntaxChecker }

function LuaErrorMessageHandler(L: Plua_State): integer; cdecl;
var
  index: Integer;
  errorMsgLength: size_t;
  errorMsg: AnsiString;
  errorLine: integer;
  dbg: lua_Debug;
begin
  index := lua_gettop(L);
  if lua_type(L, index) = LUA_TSTRING then
    begin
      errorMsgLength := lua_strlen(L, index);
      SetLength(errorMsg, errorMsgLength);
      if (errorMsgLength > 0) then
        Move(lua_tostring(L, Index)^, errorMsg[1], errorMsgLength);
    end;

  errorLine := -1;
  if lua_getstack(L, 1, @dbg) <> 0 then
    begin
      lua_getinfo(L, 'Sl', @dbg);
      errorLine := dbg.currentline;
    end;

  errors.Add(L, errorMsg, errorLine);

  Result := 0;
end;

function TLua4SyntaxChecker.Check(const ACode: string; var ErrorMsg: string;
  var ErrorLine: integer): boolean;
const 
  SUFFIX_FN = ' in string "script"';
  SUCCESS_ERROR = 'attempt to call global `__stop'' (a nil value)';
var
  L: Plua_State;
  Msg: string;
  code: AnsiString;
  name: AnsiString;
  exitCode: integer;
  lastErrorIndex: integer;
  lastError: boolean;
  lastErrorText: string;
  lastErrorLine: integer;
begin
  code := PREFIX_CODE + ACode;
  name := BUFFER_NAME;

  lastErrorText := '';
  lastErrorLine := -1;

  L := lua_open(0);
  try
    lua_pushcclosure(L, LuaErrorMessageHandler, 0);
    lua_setglobal(L, LUA_ERRORMESSAGE);

    exitCode := lua_dobuffer(L, PAnsiChar(code), Length(code), PAnsiChar(name));

    lastErrorIndex := errors.GetLast(L);
    lastError := (lastErrorIndex <> -1); 
    if lastError then
      begin
        lastErrorText := errors.FList[lastErrorIndex].Text;
        lastErrorLine := errors.FList[lastErrorIndex].Line;
      end;

    errors.Clear();
  finally
    lua_close(L);
  end;

  if (exitCode = LUA_OK) then
    begin
      Result := true;
      exit;
    end;

  if not lastError then
    begin
      ErrorMsg := 'Internal error ('+IntToStr(exitCode)+').';
      ErrorLine := -1;
      Result := false;
      exit;
    end;

  if (exitCode = LUA_ERRRUN) and (lastErrorLine = 1) and (lastErrorText = SUCCESS_ERROR) then
    begin
      Result := true;
      exit;
    end;

  Msg := StringReplace(lastErrorText, #$0A' ', ' ', [rfReplaceAll]);
  if lastErrorLine <> -1 then
    ErrorLine := lastErrorLine - 1
  else
    ErrorLine := -1;

  if exitCode = LUA_ERRSYNTAX then
    begin
      if FRegex.Match(Msg) then
        begin
          Msg := FRegex.Captures[1];
          ErrorLine := StrToIntDef(FRegex.Captures[2], -1) - 1;
        end;
    end;

  ErrorMsg := Msg;
  Result := false;
end;

constructor TLua4SyntaxChecker.Create;
begin
  FRegex := TJclRegEx.Create;
  FRegex.Options := FRegex.Options + [roMultiLine, roDotAll];
  FRegex.Compile('(.+ )at line ([0-9]+) in string "'+BUFFER_NAME+'"', false, false);
end;

destructor TLua4SyntaxChecker.Destroy;
begin
  FreeAndNil(FRegex);

  inherited;
end;

initialization
  errors := TErrors.Create;

finalization
  errors.Free;
  errors := nil;

end.
