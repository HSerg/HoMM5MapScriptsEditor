// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UAbstractLibParser;

interface

uses
  Classes, Contnrs, UCommonLibDefs;

type
  TLuaLibType = (lltLib, lltXLib, lltLuaSrcLib);

  TAbstractLibParser = class
  protected
    FLibType: TLuaLibType;
    function GetItems(Index: integer): TXLibRootTableItem; virtual; abstract;
    function GetLibName: string; virtual; abstract;
    function GetLibTable: TStringList; virtual; abstract;
    
  public
    procedure Parse(ADataStream: TStream; ADefaultLibName: string); virtual; abstract;
    function Count: integer; virtual; abstract;

    property LibType: TLuaLibType read FLibType;
    property LibName: string read GetLibName;
    property Items[Index: integer]: TXLibRootTableItem read GetItems;
    property LibTable: TStringList read GetLibTable;
  end;

implementation

end.
