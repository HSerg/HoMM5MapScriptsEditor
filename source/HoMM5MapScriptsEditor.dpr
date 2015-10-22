// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

program HoMM5MapScriptsEditor;

uses
  FastCode,
  FastMove,
  Forms,
  DKLang,
  SysUtils,
  TntSystem,
  TntSysUtils,
  UMainForm in 'UMainForm.pas' {MainForm},
  ULuaFrame in 'LuaEditor\ULuaFrame.pas' {LuaFrame: TFrame},
  UXMLFrame in 'UXMLFrame.pas' {XMLFrame: TFrame},
  UH5MFrame in 'UH5MFrame.pas' {H5MFrame: TFrame},
  UCommonDM in 'UCommonDM.pas' {CommonDM: TDataModule},
  UMapXDB in 'HMM5VFS\UMapXDB.pas',
  UTxtFrame in 'UTxtFrame.pas' {TxtFrame: TFrame},
  USearchForm in 'USearchForm.pas' {SearchForm},
  UReplaceForm in 'UReplaceForm.pas' {ReplaceForm},
  USearchReplaceOptions in 'USearchReplaceOptions.pas',
  UConsts in 'UConsts.pas',
  UReplaceQueryFrom in 'UReplaceQueryFrom.pas' {ReplaceQueryForm},
  ULuaPaintPlugin in 'LuaEditor\ULuaPaintPlugin.pas',
  ULuaScriptFunctionsScaner in 'LuaEditor\ULuaScriptFunctionsScaner.pas',
  UXLibFileParser in 'LuaEditor\UXLibFileParser.pas',
  UCommonLibDefs in 'LuaEditor\UCommonLibDefs.pas',
  UExtLuaLibs in 'LuaEditor\UExtLuaLibs.pas',
  ULuaLibTable in 'LuaEditor\ULuaLibTable.pas',
  UCommonTabbedEditor in 'UCommonTabbedEditor.pas',
  UEmbFileChooseDialog in 'UEmbFileChooseDialog.pas' {EmbFileChooseDialog},
  UUnkFrame in 'UUnkFrame.pas' {UnkFrame: TFrame},
  UCommonTabbedEditorFrame in 'UCommonTabbedEditorFrame.pas' {CommonTabbedEditorFrame: TFrame},
  UAbstractLibParser in 'LuaEditor\UAbstractLibParser.pas',
  UOptionsUnit in 'UOptionsUnit.pas',
  USettingsForm in 'USettingsForm.pas' {SettingsForm},
  UAboutForm in 'UAboutForm.pas' {AboutForm},
  UMapManager in 'HMM5VFS\UMapManager.pas',
  ULastVerFileLocator in 'HMM5VFS\ULastVerFileLocator.pas',
  UResourcesCache in 'HMM5VFS\UResourcesCache.pas',
  UTypesHelper in 'HMM5VFS\UTypesHelper.pas',
  UXPathUnit in 'HMM5VFS\UXPathUnit.pas',
  UZipFileUtils in 'HMM5VFS\UZipFileUtils.pas',
  UH5MManager in 'HMM5VFS\UH5MManager.pas',
  UMapManager.MapEntry in 'HMM5VFS\UMapManager.MapEntry.pas',
  UDKConsts in 'UDKConsts.pas',
  USynEditHelper in 'USynEditHelper.pas',
  ULangHelper in 'ULangHelper.pas',
  USearchInFilesForm in 'USearchInFilesForm.pas' {SearchInFilesForm},
  UAppController in 'UAppController.pas',
  UVFSFileUtils in 'HMM5VFS\UVFSFileUtils.pas',
  USelectionLights in 'LuaEditor\USelectionLights.pas',
  UBracketsLights in 'LuaEditor\UBracketsLights.pas',
  ULuaFrameEditorCompletion in 'LuaEditor\ULuaFrameEditorCompletion.pas',
  ULuaScriptTokensScaner in 'LuaEditor\ULuaScriptTokensScaner.pas',
  ULuaFrameEditorCompletionParams in 'LuaEditor\ULuaFrameEditorCompletionParams.pas',
  ULuaSrcLibFileParser in 'LuaEditor\ULuaSrcLibFileParser.pas',
  lua4.checker in 'LuaCore\lua4.checker.pas',
  lua5.checker in 'LuaCore\lua5.checker.pas';

{$R *.res}
{$R WindowsXP.res}
{$R HoMM5MapScriptsEditor.dkl_const.res}

begin
  Application.Initialize;
  Application.Title := 'HoMM5 Map Scripts Editor';
  
  LangManager.DefaultLanguageID := DEFAULT_LANGUAGE_ID;
  LangManager.LanguageID := DEFAULT_LANGUAGE_ID;
  LangManager.ScanForLangFiles(WideExtractFileDir(WideParamStr(0)) + DEFAULT_LANGUAGE_DIR, '*.lng', False);
  if AppIniOptions.Common.LANG = 0 then
    begin
      if LangManager.IndexOfLanguageID(GetOSNativeLANGID()) > 0 then
        LangManager.LanguageID := GetOSNativeLANGID()
      else
        LangManager.LanguageID := ALIEN_LANGUAGE_ID;
    end
  else
    LangManager.LanguageID := AppIniOptions.Common.LANG;

  AppIniOptions.Common.LANG := LangManager.LanguageID;

  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TCommonDM, CommonDM);
  Application.CreateForm(TReplaceForm, ReplaceForm);
  Application.CreateForm(TReplaceQueryForm, ReplaceQueryForm);
  Application.CreateForm(TSearchInFilesForm, SearchInFilesForm);
  Application.Run;
end.
