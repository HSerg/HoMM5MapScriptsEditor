// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit USearchReplaceOptions;

interface

uses
  Classes, SynEditTypes;

const
  SR_FORWARD     = 0;
  SR_BACKWARD    = 1;
  SR_FROMCURSOR  = 0;
  SR_ENTIRESCOPE = 1;
  SR_GLOBAL      = 0;
  SR_SELECTED    = 1;

type
  TSearchReplaceOptions = class
    sSearchString: String;
    sReplaceString: String;
    SearchedText: TStringList;
    ReplacedText: TStringList;
    srSearchDirection: Integer;
    srSearchScope: Integer;
    srSearchOrigin: Integer;
    srReplaceAll: Boolean;
    srPromptForReplace: Boolean;
    srSearchSensitive: Boolean;
    srSearchRegularExpression: Boolean;
    srSearchWholeWords: Boolean;

    function BuildSynSearchOptions(): TSynSearchOptions;

    constructor Create();
    destructor Destroy(); override;
  end;

  RSearchedBlock = record
    FileName: string;
    BlockBegin, BlockEnd: integer;
  end;

  TSearchInFilesOptions = class
    SearchedBlocks: array of RSearchedBlock;

    SearchString: String;
    CaseSensitive: Boolean;
    RegularExpression: Boolean;
    WholeWordsOnly: Boolean;
    SearchStringHistory: TStringList;

    ScanLUAFiles: boolean;
    ScanTXTFiles: boolean;
    ScanXDBFiles: boolean;
    ScanOthersFiles: boolean;

    function BuildSynSearchOptions(): TSynSearchOptions;

    constructor Create();
    destructor Destroy(); override;
  end;

implementation

uses
  SysUtils;

{ TSearchFormOptions }

function TSearchReplaceOptions.BuildSynSearchOptions: TSynSearchOptions;
begin
  Result := [];
  if srSearchSensitive then
    Result := Result + [ssoMatchCase];
  if srSearchWholeWords then
    Result := Result + [ssoWholeWord];
  if srSearchOrigin = SR_ENTIRESCOPE then
    Result := Result + [ssoEntireScope];
  if srSearchScope = SR_SELECTED then
    Result := Result + [ssoSelectedOnly];
  if srSearchDirection = SR_BACKWARD then
    Result := Result + [ssoBackwards];
end;

constructor TSearchReplaceOptions.Create;
begin
  SearchedText := TStringList.Create;
  ReplacedText := TStringList.Create;
end;

destructor TSearchReplaceOptions.Destroy;
begin
  FreeAndNil(SearchedText);
  FreeAndNil(ReplacedText);
end;

{ TSearchInFilesOptions }

function TSearchInFilesOptions.BuildSynSearchOptions: TSynSearchOptions;
begin
  Result := [];
  if CaseSensitive then
    Result := Result + [ssoMatchCase];
  if WholeWordsOnly then
    Result := Result + [ssoWholeWord];
  Result := Result + [ssoEntireScope];
end;

constructor TSearchInFilesOptions.Create;
begin
  SearchStringHistory := TStringList.Create;
end;

destructor TSearchInFilesOptions.Destroy;
begin
  SearchStringHistory.Free;
end;

end.
