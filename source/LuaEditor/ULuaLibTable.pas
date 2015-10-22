// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit ULuaLibTable;

interface

uses
  Classes;

type
  TLuaLibTable = class(TStringList)
    function AddFromFunctionDef(AFunctionDef: string): string;
  end;

implementation

uses
  StrUtils, SysUtils;

{ TLuaLibTable }

function TLuaLibTable.AddFromFunctionDef(AFunctionDef: string): string;
var
  sTemp: string;
  sTable: string;
  sNestedTable: string;
  Index: integer;
begin
  sTemp := AFunctionDef;

  while ((Pos('.', sTemp) <> 0) and ((Pos('(', sTemp) <> 0) and (Pos('.', sTemp) < Pos('(', sTemp)))) do
    begin
      sTable := Copy(sTemp, 1, Pos('.', sTemp) - 1);
      sTemp := StringReplace(sTemp, sTable + '.', '', [rfReplaceAll, rfIgnoreCase]);
      if sNestedTable = '' then
        sNestedTable := sTable
      else
        sNestedTable := sNestedTable + '.' + sTable;

      Self.Sort();

      if not Self.Find(sNestedTable, Index) then
        Self.Add(sNestedTable);
    end;

  Result := sTemp;
end;

end.
