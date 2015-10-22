// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit ULuaFrameEditorCompletion;

interface

uses
  Classes, SynCompletionProposal, SynEdit, ULuaFrame, UExtLuaLibs,
  UAbstractLibParser;

procedure LuaFrameEditorCompletionExecute(LuaFrame: TLuaFrame;
  ALocalLib: TAbstractLibParser; ALuaLibs: TLuaLibs);

implementation

uses
  UCommonLibDefs, UDKConsts, SysUtils, ULuaScriptTokensScaner;

type
  TLuaFrameEditorCompletion = class
  protected
    const TOKEN_DELIM = ' (,[;+-/*={'#09;

  protected
    FSynEdtScript: TSynEdit;
    FSynCompletion: TSynCompletionProposal;
    FLuaFrame: TLuaFrame;
    FLuaSource: string;
    FLookup: string;
    FLookupTable: string;
    FLuaLibs: TLuaLibs;
    FLocalLib: TAbstractLibParser;

    procedure intProcess(const Lookup, LookupTable: string;
      const pLibItem: TXLibRootTableItem);
    procedure intProcessWithCategory(const Lookup, LookupTable: string;
      const pLibItem: TXLibRootTableItem; const pCategories: TStrings;
      const pCategoryIndex: integer);
    procedure intProcessWithoutCategory(const Lookup, LookupTable: string;
      const pLibItem: TXLibRootTableItem; const pCategories: TStrings);
    procedure intProcessLibTable(const pLibTable: TStrings;
      lstLocalTable: TStringList);

    procedure Process();

    procedure LocateLookups();

    procedure FillSynCompletion(lstLocalTable: TStringList;
      const Lookup, LookupTable: String);

  public
    procedure Execute(ALuaFrame: TLuaFrame; ALocalLib: TAbstractLibParser;
      ALuaLibs: TLuaLibs);
  end;

procedure LuaFrameEditorCompletionExecute(LuaFrame: TLuaFrame;
  ALocalLib: TAbstractLibParser; ALuaLibs: TLuaLibs);
var
  obj: TLuaFrameEditorCompletion;
begin
  obj := TLuaFrameEditorCompletion.Create;
  try
    obj.Execute(LuaFrame, ALocalLib, ALuaLibs);
  finally
    obj.Free;
  end;
end;

{ TLuaFrameEditorCompletion }

procedure TLuaFrameEditorCompletion.Execute(ALuaFrame: TLuaFrame;
  ALocalLib: TAbstractLibParser; ALuaLibs: TLuaLibs);
begin
  FLuaFrame := ALuaFrame;
  FSynEdtScript := FLuaFrame.synedtScript;
  FSynCompletion := FLuaFrame.synCompletion;
  FLuaSource := FLuaFrame.synedtScript.Text;
  FLuaLibs := ALuaLibs;
  FLocalLib := ALocalLib;
  
  Process();
end;

procedure TLuaFrameEditorCompletion.intProcess(const Lookup, LookupTable: string;
  const pLibItem: TXLibRootTableItem);
var
  synCompletionItem: string;
begin
  if (pLibItem is TXIntFuncItem) and (not (pLibItem as TXIntFuncItem).CompleteDeclaration) then
    exit;

  if ((LookupTable = Copy(pLibItem.nestedTable, 1, Length(Lookup))) and (LookupTable <> '')) then
    synCompletionItem := pLibItem.GetAsDecoratedText()
  else
  if (Lookup = Copy(pLibItem.name, 1, Length(Lookup))) and (pLibItem.nestedTable = '') then
    synCompletionItem := pLibItem.GetAsDecoratedText()
  else
    synCompletionItem := '';

  if synCompletionItem <> '' then
    begin
      FSynCompletion.ItemList.Add(synCompletionItem);

      if pLibItem is TXFuncItem then
        FSynCompletion.InsertList.Add(pLibItem.name+'()')
      else
        FSynCompletion.InsertList.Add(pLibItem.name);
    end;
end;

procedure TLuaFrameEditorCompletion.intProcessWithCategory(
  const Lookup, LookupTable: string; const pLibItem: TXLibRootTableItem;
  const pCategories: TStrings; const pCategoryIndex: integer);
var
  i: integer;
begin
  if pCategories=nil then
    exit;
  if pLibItem.categoryList.IndexOf(pCategories[pCategoryIndex]) = -1 then
    exit;
  for i := 0 to pCategoryIndex - 1 do
    if pLibItem.categoryList.IndexOf(pCategories[i]) <> -1 then
      exit;
  intProcess(Lookup, LookupTable, pLibItem);
end;

procedure TLuaFrameEditorCompletion.intProcessWithoutCategory(
  const Lookup, LookupTable: string; const pLibItem: TXLibRootTableItem;
  const pCategories: TStrings);
var
  i: integer;
begin
  if pCategories<>nil then
    for i := 0 to pCategories.Count - 1 do
      if pLibItem.categoryList.IndexOf(pCategories[i]) <> -1 then
        exit;
  intProcess(Lookup, LookupTable, pLibItem);
end;

procedure TLuaFrameEditorCompletion.LocateLookups;
var
  j: Integer;
  xCurrentLine: string;
  GotTable: Boolean;
begin
  j := FSynEdtScript.CaretX - 1;
  xCurrentLine := FSynEdtScript.Lines[FSynEdtScript.CaretY - 1];
  GotTable := false;

  // Getting lookup text and lookup table at cursor
  if (j > 1) and (Copy(xCurrentLine, j-1, 2) = '..') then
   // nothing
  else
    begin
      while ((AnsiPos(Copy(xCurrentLine, j, 1), TOKEN_DELIM) = 0) and
             (Copy(xCurrentLine, j, 1) <> '') and
             (j > 0)) do
        begin
          // Retreive lookup text at cursor
          FLookup := Copy(xCurrentLine, j, 1) + FLookup;

          // Retreive lookup table at cursor if any
          if (j > 2) and (Copy(xCurrentLine, j-2, 2) = '..') then
            begin
              break;
            end
          else if ((Copy(xCurrentLine, j, 1) = '.') or GotTable) then
            begin
              if GotTable then
                FLookupTable := Copy(xCurrentLine, j, 1) + FLookupTable;

              GotTable := True;
            end;

          Dec(j);
        end;
    end;
end;

procedure TLuaFrameEditorCompletion.intProcessLibTable(
  const pLibTable: TStrings; lstLocalTable: TStringList);
var
  i: integer;
  Index: Integer;
begin
  lstLocalTable.Sort;
  for i := 0 to pLibTable.Count - 1 do
    if not lstLocalTable.Find(pLibTable[i], Index) then
      begin
        lstLocalTable.Add(pLibTable[i]);
        lstLocalTable.Sort;
      end;
end;

procedure TLuaFrameEditorCompletion.Process;
var
  lstLocalTable: TStringList;
  k, j, l: Integer;

  xLib: TAbstractLibParser;
  varListParser: TLuaScriptTokensScaner;

  xFillParams: boolean;
  xTmpLocation: integer;
  xLookup: string;

  xParamCategories: TStrings;
begin
  FSynCompletion.ItemList.Clear;
  FSynCompletion.ClearList;
  FSynCompletion.InsertList.Clear;

  xFillParams := FLuaFrame.findFunctionCalling(FLuaLibs, xTmpLocation, xLookup);
  xParamCategories := FLuaFrame.getParameterCategories(xLookup, xTmpLocation);

  LocateLookups();

  lstLocalTable := TStringList.Create;
  try
    if xParamCategories<>nil then
      for k := 0 to xParamCategories.Count - 1 do
        begin
          for l := 0 to FLuaLibs.Count - 1 do
            begin
              xLib := FLuaLibs.libs[l];
              intProcessLibTable(xLib.LibTable, lstLocalTable);
              for j := 0 to xLib.Count - 1 do
                intProcessWithCategory(FLookup, FLookupTable, xLib.Items[j], xParamCategories, k);
            end;

          intProcessLibTable(FLocalLib.LibTable, lstLocalTable);
          for j := 0 to FLocalLib.Count - 1 do
            intProcessWithCategory(FLookup, FLookupTable, FLocalLib.Items[j], xParamCategories, k);
        end;

    for l := 0 to FLuaLibs.Count - 1 do
      begin
        xLib := FLuaLibs.libs[l];
        intProcessLibTable(xLib.LibTable, lstLocalTable);
        for j := 0 to xLib.Count - 1 do
          intProcessWithoutCategory(FLookup, FLookupTable, xLib.Items[j], xParamCategories);
      end;

    intProcessLibTable(FLocalLib.LibTable, lstLocalTable);
    for j := 0 to FLocalLib.Count - 1 do
      intProcessWithoutCategory(FLookup, FLookupTable, FLocalLib.Items[j], xParamCategories);

    varListParser := TLuaScriptTokensScaner.Create;
    try
      varListParser.Scan(FLuaSource);
      for l := 0 to FLuaLibs.Count - 1 do
        varListParser.ExcludeLibDefs(FLuaLibs.libs[l]);
      varListParser.ExcludeLibDefs(FLocalLib);
      intProcessLibTable(varListParser.LibTable, lstLocalTable);
      for j := 0 to varListParser.VarsCount - 1 do
        intProcessWithoutCategory(FLookup, FLookupTable, varListParser.Vars[j], xParamCategories);
    finally
      varListParser.Free;
    end;

    FillSynCompletion(lstLocalTable, FLookup, FLookupTable);

  finally
    lstLocalTable.Free;
  end;

  FSynCompletion.Title := DKConsts.SAUTOCOMPLETE_TITLE;
  FSynCompletion.ResetAssignedList;
end;

procedure TLuaFrameEditorCompletion.FillSynCompletion(
  lstLocalTable: TStringList; const Lookup, LookupTable: String);
var
  i: Integer;
  sTemp: string;
  j: Integer;
  item: string;
begin
  for i := 0 to lstLocalTable.Count - 1 do
    begin
      item := lstLocalTable.Strings[i]; 
      if ((LookupTable = Copy(item, 1, Length(LookupTable))) and
          (LookupTable <> '') and
          (item <> LookupTable) and
          (Pos('.', Copy(item, Length(Lookup) + 1, Length(item) - Length(Lookup) + 1)) = 0)) then
        begin
          j := Length(item);
          sTemp := '';
          while ((Copy(item, j, 1) <> '.') and (j > 0)) do
            begin
              sTemp := Copy(item, j, 1) + sTemp;
              Dec(j);
            end;

          FSynCompletion.InsertList.Add(sTemp);
          FSynCompletion.ItemList.Add(
            '\color{clGreen}library\color{clBlack}       \column{}\style{+B}' +
            sTemp +
            '\style{-B}');
        end
      else if ((Lookup = Copy(item, 1, Length(Lookup))) and
               (Pos('.', Copy(item, Length(Lookup) + 1, Length(item) - Length(Lookup) + 1)) = 0)) then
        begin
          //TODO: фигня какая-то (может PChar ?)
          FSynCompletion.InsertList.Add(Copy(item, 1, Length(item)));
          FSynCompletion.ItemList.Add(
            '\color{clGreen}library\color{clBlack}       \column{}\style{+B}' +
            Copy(item, 1, Length(item)) +
            '\style{-B}');
        end;
    end;
end;

end.
