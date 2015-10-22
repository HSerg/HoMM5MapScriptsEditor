(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Copyright (C) 2006
 * Miha Vrhovnik (http://simail.sf.net, http://xcollect.sf.net)
 * All Rights Reserved.
 *
 * Contributor(s):
 * 
 *  
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** **)
unit uHunSpellLib;

interface

uses
  Windows, SysUtils, Classes, Contnrs, JclWideStrings;

type
  THunSpell = class
  protected
    FHunSpell: Pointer;

    function get_dic_encoding(): string;

  public
    constructor Create();
    destructor Destroy(); override;

    procedure initialize(aff_file, dict_file: string);
    procedure uninitialize();

    function spell(AWord: WideString): Boolean;
    function suggest(AWord: WideString): TWStrings;
    function put_word(AWord: WideString): Integer;

    property DicEncoding: string read get_dic_encoding;
  end;

  THunSpellExt = class
  protected
    FHunSpellCollection: TObjectList;
    FEnabledItems: array of boolean;

    function GetCount: integer;

    function GetEnabled(Index: integer): boolean;
    function GetItem(Index: integer): THunSpell;
    procedure SetEnabled(Index: integer; const Value: boolean);
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Append(aff_file, dict_file: string);
    procedure Clear();

    function put_word(AWord: WideString): Integer;
    function spell(AWord: WideString): Boolean;

    property Count: integer read GetCount; 
    property Item[Index: integer]: THunSpell read GetItem;
    property Enabled[Index: integer]: boolean read GetEnabled write SetEnabled;
  end;

implementation

var
  hunspell_initialize: function(aff_file: PChar; dict_file: PChar): Pointer; cdecl;
  hunspell_uninitialize: procedure(sspel: Pointer); cdecl;
  hunspell_spell: function(spell: Pointer; word: PChar): Boolean; cdecl;
  hunspell_suggest: function(spell: Pointer; word: PChar; var suggestions: PPChar): Integer; cdecl;
  hunspell_suggest_free: procedure(spell: Pointer; suggestions: PPChar; suggestLen: Integer); cdecl;
  hunspell_get_dic_encoding: function(spell: Pointer): PChar; cdecl;
  hunspell_put_word: function(spell: Pointer; word: PChar): Integer; cdecl;

  LibsLoaded: Boolean = False;
  DLLHandle: THandle;

function LoadLibHunspell(libraryName: String = ''): Boolean;
begin
  if libraryName = '' then
    libraryName := 'hunspell.dll';

  Result := LibsLoaded;
  if Result then //already loaded.
    exit;

  DLLHandle := LoadLibrary(PAnsiChar(libraryName));
  if DLLHandle <> 0 then begin
    Result := True; //assume everything ok unless..

    @hunspell_initialize := GetProcAddress(DLLHandle, '_hunspell_initialize');
    if not Assigned(@hunspell_initialize) then Result := False;
    @hunspell_uninitialize := GetProcAddress(DLLHandle, '_hunspell_uninitialize');
    if not Assigned(@hunspell_uninitialize) then Result := False;
    @hunspell_spell := GetProcAddress(DLLHandle, '_hunspell_spell');
    if not Assigned(@hunspell_spell) then Result := False;
    @hunspell_suggest := GetProcAddress(DLLHandle, '_hunspell_suggest');
    if not Assigned(@hunspell_suggest) then Result := False;
    @hunspell_suggest_free := GetProcAddress(DLLHandle, '_hunspell_suggest_free');
    if not Assigned(@hunspell_suggest_free) then Result := False;
    @hunspell_get_dic_encoding := GetProcAddress(DLLHandle, '_hunspell_get_dic_encoding');
    if not Assigned(@hunspell_get_dic_encoding) then Result := False;
    @hunspell_put_word := GetProcAddress(DLLHandle, '_hunspell_put_word');
    if not Assigned(@hunspell_put_word) then Result := False;
  end;
end;

{ THunSpell }

constructor THunSpell.Create;
begin
  FHunSpell := nil;
end;

destructor THunSpell.Destroy;
begin
  if FHunSpell <> nil then
    uninitialize();
end;

function THunSpell.get_dic_encoding(): string;
begin
  Result := hunspell_get_dic_encoding(FHunSpell);
end;

function THunSpell.put_word(AWord: WideString): Integer;
begin
  Result := hunspell_put_word(FHunSpell, PChar(UTF8Encode(AWord)));
end;

function THunSpell.spell(AWord: WideString): Boolean;
begin
  Result := hunspell_spell(FHunSpell, PChar(UTF8Encode(AWord)));
end;

function THunSpell.suggest(AWord: WideString): TWStrings;
var
  i: integer;
  length: integer;
  orig_wrds: PPChar;
  wrds: PPChar;
begin
  Result := TWStringList.Create;
  try
    wrds := nil;
    length := hunspell_suggest(FHunSpell, PChar(UTF8Encode(AWord)), wrds);
    orig_wrds := wrds;
    try
      for i := 0 to length-1 do begin
        Result.Add(UTF8Decode(wrds^));
        Inc(Integer(wrds), sizeOf(Pointer));
      end;
    finally
      hunspell_suggest_free(FHunSpell, orig_wrds, length);
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure THunSpell.uninitialize();
begin
  hunspell_uninitialize(FHunSpell);
  FHunSpell := nil;
end;

procedure THunSpell.initialize(aff_file: string; dict_file: string);
begin
  FHunSpell := hunspell_initialize(PChar(aff_file), PChar(dict_file));
end;

{ THunSpellExt }

procedure THunSpellExt.Append(aff_file, dict_file: string);
var
  xHunSpell: THunSpell;
begin
  xHunSpell := THunSpell.Create;
  xHunSpell.initialize(aff_file, dict_file);
  FHunSpellCollection.Add(xHunSpell);
  SetLength(FEnabledItems, Length(FEnabledItems)+1);
  FEnabledItems[Length(FEnabledItems)-1] := true;
end;

procedure THunSpellExt.Clear;
begin
  FEnabledItems := nil;
  FHunSpellCollection.Clear;
end;

constructor THunSpellExt.Create;
begin
  FEnabledItems := nil;
  FHunSpellCollection := TObjectList.Create(true);
end;

destructor THunSpellExt.Destroy;
begin
  FEnabledItems := nil;
  FHunSpellCollection.Free;
end;

function THunSpellExt.GetCount: integer;
begin
  Result := FHunSpellCollection.Count;
end;

function THunSpellExt.GetEnabled(Index: integer): boolean;
begin
  Result := FEnabledItems[Index];
end;

function THunSpellExt.GetItem(Index: integer): THunSpell;
begin
  Result := FHunSpellCollection[Index] as THunSpell;
end;

function THunSpellExt.put_word(AWord: WideString): Integer;
var
  i: integer;
begin
  for i := 0 to Count-1 do
    Item[i].put_word(AWord);
  // TODO: Result ?
  Result := 0;
end;

procedure THunSpellExt.SetEnabled(Index: integer; const Value: boolean);
begin
  FEnabledItems[Index] := Value;
end;

function THunSpellExt.spell(AWord: WideString): Boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to Count-1 do
    if Item[i].spell(AWord) then
      begin
        Result := true;
        break;
      end;
end;

initialization
  LoadLibHunspell()

finalization
  if DLLHandle <> 0 then
      FreeLibrary(DLLHandle);
end.
