// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit ULuaFrameEditorCompletionParams;

interface

uses
  Windows, Classes, SynCompletionProposal, SynEdit, ULuaFrame, UExtLuaLibs,
  UAbstractLibParser;

procedure ExtractedSynCompletionParamsExecute(ALuaFrame: TLuaFrame;
  ALocalLib: TAbstractLibParser; ALuaLibs: TLuaLibs;
  Sender: TSynCompletionProposal; var CanExecute: Boolean);

implementation

uses
  UCommonLibDefs, UDKConsts, SysUtils, ULuaScriptTokensScaner, UOptionsUnit,
  SynEditTypes;

type
  TLuaFrameEditorCompletionParams = class
  protected
    FLuaFrame: TLuaFrame;
    FLuaLibs: TLuaLibs;
    FLocalLib: TAbstractLibParser;
    FSynCompletionParams: TSynCompletionProposal;
    
    procedure AddToSynCompletionParams(const pLookup: String;
      const pLibItem: TXLibRootItem; var pLastDef: TXFuncItem);

    procedure Process(var CanExecute: Boolean);
      
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Execute(ALuaFrame: TLuaFrame; ALocalLib: TAbstractLibParser;
      ALuaLibs: TLuaLibs; Sender: TSynCompletionProposal;
      var CanExecute: Boolean);
  end;

constructor TLuaFrameEditorCompletionParams.Create;
begin
  // nothing
end;

destructor TLuaFrameEditorCompletionParams.Destroy;
begin
  // nothing
end;

procedure TLuaFrameEditorCompletionParams.Execute(ALuaFrame: TLuaFrame;
  ALocalLib: TAbstractLibParser; ALuaLibs: TLuaLibs;
  Sender: TSynCompletionProposal; var CanExecute: Boolean);
begin
  FLuaFrame := ALuaFrame;
  FLuaLibs := ALuaLibs;
  FSynCompletionParams := Sender;
  FLocalLib := ALocalLib;

  Process(CanExecute);
end;

procedure TLuaFrameEditorCompletionParams.AddToSynCompletionParams(
  const pLookup: String; const pLibItem: TXLibRootItem; var pLastDef: TXFuncItem);
var
  sFunctionName, sParameters, sProposition: String;
begin
  if (pLibItem is TXIntFuncItem) and (not (pLibItem as TXIntFuncItem).CompleteDeclaration) then
    exit;

  if (pLibItem is TXFuncItem) then
    begin
      sFunctionName := TXFuncItem(pLibItem).fullName;

      if AnsiSameText(pLookup, sFunctionName) then
        begin
          pLastDef := pLibItem as TXFuncItem;

          sParameters := TXFuncItem(pLibItem).GetParamsAsCommaSeparatedString;

          if sParameters = '' then
            sProposition := DKConsts.SWITHOUT_PARAMS
          else
            sProposition := StringReplace(sParameters, ',', '", ", ', [rfReplaceAll, rfIgnoreCase]);

          sProposition := Format('"(%s)"', [sProposition]);
          FSynCompletionParams.ItemList.Add(sProposition);
        end;
    end;
end;

procedure TLuaFrameEditorCompletionParams.Process(var CanExecute: Boolean);
var
  xLastDef: TXFuncItem;
  Lookup: String;
  TmpLocation, i, j: Integer;
  FoundMatch: Boolean;
  xLib: TAbstractLibParser;
  tmp: string;
begin
  FoundMatch := FLuaFrame.findFunctionCalling(FLuaLibs, TmpLocation, Lookup);

  CanExecute := FoundMatch;

  if not CanExecute then
    begin
      FSynCompletionParams.ItemList.Clear;
      exit;
    end;

  // установка подсветки параметра с соотв. порядковым номером
  FSynCompletionParams.Form.CurrentIndex := TmpLocation;

  if Lookup <> FSynCompletionParams.PreviousToken then
    begin
      FSynCompletionParams.ItemList.Clear;

      // добавление в список библиотечные функции
      for i := 0 to FLuaLibs.Count - 1 do
        begin
          xLib := FLuaLibs.libs[i];
          for j := 0 to xLib.Count - 1 do
            AddToSynCompletionParams(Lookup, xLib.Items[j], xLastDef)
        end;

      // добавление в список локальные функции
      for j := 0 to FLocalLib.Count - 1 do
        AddToSynCompletionParams(Lookup, FLocalLib.Items[j], xLastDef);
    end;

  // добавление описания параметров функции (только в случае однозначного определения функции)
  if AppIniOptions.LuaEditor.UseLongParamsDesc and (FSynCompletionParams.ItemList.Count = 1) then
    begin
      FSynCompletionParams.ItemList.Add('');
      FSynCompletionParams.ItemList.Add(xLastDef.short_description+'   ');
      FSynCompletionParams.ItemList.Add('');
      for i := 0 to xLastDef.ParamDefs.Count - 1 do
        begin
          tmp := Format('\style{+I}%s\style{-I}   ', [xLastDef.Params[i].name]);
          FSynCompletionParams.ItemList.Add(tmp);
          tmp := Format('  %s   ', [xLastDef.Params[i].short_description]);
          FSynCompletionParams.ItemList.Add(tmp);
        end;
    end;
end;

procedure ExtractedSynCompletionParamsExecute(ALuaFrame: TLuaFrame;
  ALocalLib: TAbstractLibParser; ALuaLibs: TLuaLibs;
  Sender: TSynCompletionProposal; var CanExecute: Boolean);
var
  obj: TLuaFrameEditorCompletionParams;
begin
  obj := TLuaFrameEditorCompletionParams.Create;
  try
    obj.Execute(ALuaFrame, ALocalLib, ALuaLibs, Sender, CanExecute);
  finally
    obj.Free;
  end;
end;

end.
