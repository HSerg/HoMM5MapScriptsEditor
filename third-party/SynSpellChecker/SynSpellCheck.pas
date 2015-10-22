////////////////////////////////////////////////////////////////////////////////
// TSynSpellCheck 1.30
//
// Copyright (c) 2002 Jacob Dybala a.k.a. "m3Rlin". All rights reserved.
//
// E-Mail: jacobdybala@synspellcheck.prv.pl
// WWW   : http://www.synspellcheck.prv.pl/ SynSpellCheck Home
//         http://www.delphifaq.net/        Merlin's Delphi Forge
//
// Elf hash algorithm
//   Copyright (c) 1998-2002 Scalabium
//   <http://www.scalabium.com/faq/dct0136.htm>
// SoundEx algorithm
//   Copyright (c) 1995-2001 Borland Software Corporation
// Metaphone Phonetic Hash Algorithm
//   Copyright (c) Tom White <wcstom@yahoo.com>
// Word differences algorithm JHCMP...
//   Copyright (c) Josef Hampl
//
// Created : Jan-10-2002
// Modified: Aug-31-2002
////////////////////////////////////////////////////////////////////////////////
// All dictionaries are located in the 'Program Files\Common\SynSpell' folder.
// This is to limit the number of copies of the same dictionary on a single
// computer to one file.
//
// Dictionaries are flat text files with a single word in each line. All words
// MUST be lowercase. The dictionaries are case insensitive.
//
// Available dictionaries:
// * czech.1-0-0.dic     - 21,177  words [197 Kb]
// * danish.1-0-0.dic    - 339,207 words [4.31 Mb]
// * dutch.1-0-0.dic     - 176,800 words [2.03 Mb]
// * english.1-1-4.dic   - 73,403  words [762 Kb]
// * german.1-0-0.dic    - 88,566  words [1.16 Mb]
// * italian.1-0-0.dic   - 60,453  words [607 Kb]
// * japanese.1-0-0.dic  - 115,524 words [1 Mb]
// * latin.1-0-0.dic     - 77,107  words [905 Kb]
// * norwegian.1-0-0.dic - 61,847  words [636 Kb]
// * polish.1-0-3.dic    - 3,925   words [36 Kb]
// * russian.1-0-0.dic   - 39,412  words [407 Kb]
// * spanish.1-0-0.dic   - 59,167  words [554 Kb]
// * turkish.1-0-0.dic   - 26,123  words [237 Kb]
////////////////////////////////////////////////////////////////////////////////
// Changes:
//
// 1.30 (Contributed in large by Jan Fiala)
//   * Many, many minor adjustments, optimizations.
//   * Rewritten SetApostrophes().
//   + New word suggestion algorithm: haDiff. Finds words based on differences.
//     haSoundex and haMetaphone *may* be removed in upcoming versions.
//   + New action added: ACTION_UNDO.
//   + New function: GetDictionaryDir(). This allows users to specify their own
//     paths.
//   + Dutch (compiled by Arno Verhoeven) dictionary added.
//
// 1.24 Released privately to certain users.
//   * Bug Fix: PChar and string incompatiblity. Fixed.
//
// 1.23 Released privately to certain users.
//   * Minor code adjustments.
//   + New dictionaries: Norwegian and Spanish.
//
// 1.22
//   * Bug Fix: The Apostrophes property did not allow changing. Fixed.
//     Submitted by R.K. Wittye.
//   * Bug Fix: ClearDictWords did not properly dispose of words creating a
//     rather large memory leak. Fixed. Submitted by Ascher Stefan.
//   * English and Polish dictionaries updated.
//   + Added Value field to TWordRec record. Each word is assigned an Elf value
//     and is checked by it. Major speed optimization. Suggested by Jan Fiala
//     (CRC32).
//
// 1.21
//   * Bug Fix: %ProgramFilesDir%\Common Files was read instead of %CommonFilesDir%.
//     This created problems on non-English versions of Windows. The directory
//     was not found. Fixed.
//   * English and Polish dictionaries updated.
//
// 1.20
//   * FindWord() routine rewritten to make use of cache array. Other functions
//     have only been slightly modified yet no functions have been broken.
//   * LoadDictionary() routine now converts all words to lowercase.
//   * LoadSkipList() does not add the words one-by-one any more. They are
//     assigned in whole.
//   * FSkipList is now cleared when a dictionary is closed.
//   * SaveSkipList() now removes all empty lines before saving to file.
//   + Added cache array to speed up word checks.
//   + ENoDictionaryLoaded is now thrown when no dictionary has been loaded.
//
// 1.19
//   * Bug Fix: Word underlining would also draw on gutter when the word was
//     partially scrolled under it. Fixed.
//   * SoundexLength property converted to HashLength.
//   * PaintUnderLine() code modified to directly color pixels instead of drawing
//     lines.
//   * Dictionary updates: English (1.1.2), Polish (1.1.1). The Polish word list
//     has been *significantly* reduced due to the fact that this word list is
//     being started all over to include words with non-latin characters.
//   + New option: sscoTrimApostrophes.
//   + New properties: Busy and UnderlineStyle (to mimic Corel Wordperfect
//     auto spell checking).
//   + MetaPhone algorithm has been finally implemented. In beta stage (works,
//     but slow on big lists).
//   + AddDictSkipList(), AddDictWordList() routines added.
//   + New dictionaries: German (by Ascher Stefan) and Russian.
//
// 1.18
//   * Bug Fix: OnSkipWord event did not return proper ASkipAll value. Fixed.
//   * Bug Fix: GetDictionaryList() included all copies of dictionaries for a
//     specific language instead of newest. Fixed.
//   * DupeString() has been corrected with proper compiler conditional.
//   * Minor code changes to always pass lowercase words to FindWord().
//   * English dictionary updated to version 1.1.0.
//   * Updated component demo.
//   + New option: sscoMaintainCase. Idea suggested by Jeff Rafter.
//   + New event: OnAddWord.
//   + Added support for words with apostrophes. Idea by Stefan van As.
//   + GetDictionaryList() now returns a sorted list.
//
// 1.17
//   * SelectWordAtCursor() made public.
//   + Added support for localized languages and numbers.
//
// 1.16
//   * Bug Fix: Compiler conditional around SoundEx() routines was broken.
//     Fixed.
//   * Bug Fix: sscoSelectWord did not work when set to False. Fixed.
//   + SelectWordAtCursor() routine added. Contributed by Stefan van As.
//
// 1.15
//   * Bug Fix: PenColor property did not work. Fixed by Jeff Corbets.
//   * Bug Fix: OnAbort event was not called when spell checking was aborted.
//     Fixed.
//   * TSoundEx class has been removed in favor of Borland implementation of
//     SoundEx() function.
//   * Minor code modifications.
//   + Added support for dashed words.
//   + New option: sscoGoUp.
//   + New property: SoundExLength.
//
// 1.14
//   * Bug Fix: If the editor had no text and sscoHourGlass was set the cursor
//     did not revert to it's previous value. Fixed by Jeff Corbets.
//
// 1.13
//   * Bug Fix: When empty lines in base dictionary and user dictionary were
//     added to word list and raised AV when attempting to calculate word hash.
//     Fixed.
//
// 1.12
//   * Bug Fix: GetSuggestions did not properly support words with uppercase
//     characters. Fixed. Found by Jeff Rafter.
//   + Added Metaphon algorithm for word hashes. Not working, just skeleton for
//     now.
//
// 1.11
//   + Added support for multiple editors: AddEditor() and RemoveEditor().
//
// 1.10 (code contributed by Ricardo Cardona)
//   * Bug Fix: When not highlighter was selected and sscoAutoSpellCheck was set
//     in Options the component generated an AV. Fixed.
//   * New property: CheckAttribs.
//   * Improved code for underlining unknown words.
//
// 1.09
//   * Bug Fix: FWordList did not free memory when the component was destroyed.
//     It just cleared the word and hash lists. Fixed.
//
// 1.08
//   * Bug Fix: FindWord() function was case sensitive. Fix contributed by
//     Gerald Nunn.
//   + New events: OnDictClose and OnDictLoad.
//   + New options: sscoAutoSpellCheck (contributed by Ricardo Cardona),
//     sscoIgnoreWordsWithNumbers and sscoIgnoreSingleChars.
//   + New property: PenColor.
//   + Added support for Java documentation.
//
// 1.07
//   * Bug Fix: When spell checking the last word under certain conditions the
//     component would enter an infinite loop. Fixed.
//
// 1.06
//   * Bug Fix: When correcting words in OnCheckWord event the word would not be
//     replaced but added to the beginning of the old one. Fixed.
//   + New dictionary: Danish.
//   + New property: OpenDictionary.
//   + New option: sscoSelectWord.
//
// 1.05
//   + New events: OnCorrectWord and OnSkipWord.
//   + Demo added.
//
// 1.04
//   * Bug Fix: Would not compile under Delphi 6 due to duplicate resource
//     error. Fixed.
//   * GetDictionaryList() now searches for file that match the correct naming
//     scheme - name.major-minor-revision.dic, where major, minor and revision
//     are single characters.
//   + New dictionaries: Italian, Latin, Japanese, Polish, Spanish (Thanks to
//     Ricardo Cardona), and Turkish.
//   + New routines: CloseDictionary(), GetWordCount().
//   + New property: Dictionary.
//   - Removed {$IFDEF SYN_WIN32} directive from GetDictionaryList(). The
//     routines are available under Kylix also.
//   - Removed Version parameter from LoadDictionary.
//
// 1.03
//   + Added /usr/local/SynSpell dir under Linux as the default dictionary
//     directory.
//   + Added Language property.
//   + %ProgramFiles%\Common Files\SynSpell is now dynamically read from system
//     Registry.
//   + Added user dictionary.
//   + Added GetDictionaryList().
//
// 1.02
//   * Bug Fix: When the word list was cleared, the SoundEx list still hogged up
//     the memory =) Fixed.
//   * Bug Fix: When a word was deleted from the dictionary, the SoundEx hash
//     remained undeleted. Therefor, after deleting a word the whole SoundEx
//     hash list after the deleted word was wrong (1 up).
//   * Bug Fix: Suggestions were not passed in ASuggestions in OnCheckWord
//     event. Fixed.
//   * Bug Fix: DeleteSkipWord() fixed to delete form skip word list, not word
//     list ;-)
//   * Bug Fix: editor did not update when searching for words, the screen
//     would "blur". Fixed.
//   * GetSuggestions() changed from procedure to function to return number or
//     words in list.
//   * FWordList is now type of TList instead of TStringList.
//   + If no AAction is specified in the OnCheckWord event, then ACTION_SKIP is
//     default.
//   + Now double words are automatically ignored in FWordList.
//   + Added sscoSuggestWords option.
//   + Added OnAbort event.
//   + Added support for HTML Text.
//   - Removed unsupported options from Options property.
//
// 1.01
//   + Added Options property (support for selecting unknown words in editor,
//     spell checking from cursor, and spell checking only selection, hour glass
//     cursor during spell check, removing cursor visibility during spell
//     check).
//   + Added word suggestion list.
////////////////////////////////////////////////////////////////////////////////

unit SynSpellCheck;

{$i synedit.inc}

interface

uses
  Classes,
  Graphics,
  Windows,
  Controls,
  SysUtils,
  SynEdit,
  SynEditTypes,
  uHunSpellLib,
  JclWideStrings;
  
type
  TLanguageRec = record
    Name,
    Version: string;
  end;

  TSynSpellCheck  = class;
  TUnderlineStyle = (usCorelWordPerfect, usMicrosoftWord);

  TDrawAutoSpellCheckPlugin = class(TSynEditPlugin)
  private
    FPenColor      : TColor;
    FUnderlineStyle: TUnderlineStyle;
    { Procedures }
    procedure SetPenColor(const Value: TColor);
    procedure SetUnderlineStyle(const Value: TUnderlineStyle);
  protected
    FSynSpellCheck: TSynSpellCheck;
    { Procedures }
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect; FirstLine,
      LastLine: Integer); override;
    procedure LinesInserted(FirstLine, Count: Integer); override;
    procedure LinesDeleted(FirstLine, Count: Integer); override;
  public
    constructor Create(AOwner: TCustomSynEdit);
    { Properties }
    property PenColor: TColor read FPenColor write SetPenColor default clRed;
    property UnderlineStyle: TUnderlineStyle read FUnderlineStyle
      write SetUnderlineStyle default usMicrosoftWord;
  end;

  { Procedure types }
  TOnAddWord = procedure(Sender: TObject; AWord: string) of object;
  TOnCheckWord = procedure(Sender: TObject; AWord: string;
    ASuggestions: TStringList; var ACorrectWord: string; var AAction: Integer;
    const AUndoEnabled: Boolean = True) of object;
  TOnCorrectWord = procedure(Sender: TObject; AWord, ACorrectWord: string)
    of object;
  TOnSkipWord = procedure(Sender: TObject; AWord: string; ASkipAll: Boolean)
    of object;

  { Sets }
  SynSpellCheckOptions = (
    sscoAutoSpellCheck,
    sscoGoUp,
    sscoHideCursor,
    sscoHourGlass,
    sscoIgnoreSingleChars,
    sscoIgnoreWordsWithNumbers,
    sscoMaintainCase,
    sscoSelectWord,
    sscoStartFromCursor,
    sscoSuggestWords,
    sscoTrimApostrophes
  );
  TSynSpellCheckOptions = set of SynSpellCheckOptions;

  TSynSpellCheck = class(TComponent)
  private
    FHunSpellExt: THunSpellExt;
  private
    FBusy,
    FModified,
    FOpenDictionary,
    FUseUserDictionary : Boolean;
    FApostrophes,
    FDictionary,
    FDictPath,
    FUserFileName,
    FUserDictPath      : string;
    FPenColor          : TColor;
    FCursor            : TCursor;
    FEditor            : TCustomSynEdit;
    FDrawAutoSpellCheck: TDrawAutoSpellCheckPlugin;
    FOnAddWord         : TOnAddWord;
    FLanguage          : TLanguageRec;
    FEditors           : TList;
    FPlugins           : TList;
    FWordList          : TWStringList;
    FOnAbort,
    FOnDictClose,
    FOnDictLoad,
    FOnDone,
    FOnStart           : TNotifyEvent;
    FOnCheckWord       : TOnCheckWord;
    FOnCorrectWord     : TOnCorrectWord;
    FOnSkipWord        : TOnSkipWord;
    FCheckAttribs      : TStringList;
    FSkipList          : TStringList;
    FOptions           : TSynSpellCheckOptions;
    FUnderlineStyle    : TUnderlineStyle;
    { Functions }
    function GetDefaultDictionaryDir: string;
    function GetDictionaryDir: string;
    function GetUserDictionaryDir: string;    
    { Procedures }
    procedure SetSkipList(Value: TStringList);
    procedure SetApostrophes(const Value: string);
    procedure SetCheckAttribs(const Value: TStringList);
    procedure SetEditor(const Value: TCustomSynEdit);
    procedure SetPenColor(const Value: TColor);
    procedure SetUnderlineStyle(const Value: TUnderlineStyle);
  public
    FIdentChars: TSynIdentChars;
    FWhiteChars: TSynIdentChars;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Functions }
    function AddEditor(AEditor: TCustomSynEdit): Integer;
    function CheckWord(Word: string): Boolean;
    function DictionaryExists(Language: string; Path: string = ''): Boolean;
    function GetNewestDictionary(Language: string): string;
    function GetSuggestions(Word: string; SuggestionList: TStringList): Integer;
    function GetWordCount: Integer;
    function IsDictWord(Word: string): Boolean;
    function RemoveEditor(AEditor: TCustomSynEdit): Boolean;
    { Procedures }
    procedure AddDictWord(Word: string);
    procedure AddDictWordList(WordList: TStringList);
    procedure AddSkipWord(Word: string);
    procedure AddSkipWordList(WordList: TStringList);
    procedure ClearDictWords;
    procedure ClearSkipWords;
    procedure CloseDictionary;
    procedure DeleteDictWord(Word: string);
    procedure DeleteSkipWord(Word: string);
    procedure FixLists;
    procedure GetDictionaryList(var tslList: TStringList);
    procedure LoadDictionary(Language: string; FileName: string = '');
    procedure LoadSkipList(FileName: string);
    procedure SaveSkipList(FileName: string);
    procedure SaveUserDictionary;
    procedure SelectWordAtCursor;
    procedure SpellCheck;
  published
    { Properties }
    property Apostrophes: string read FApostrophes write SetApostrophes;
    property Busy: Boolean read FBusy default False;
    property CheckAttribs: TStringList read FCheckAttribs write SetCheckAttribs;
    property Dictionary: string read FDictionary;
    property DictionaryPath: string read GetDictionaryDir write FDictPath;
    property Editor: TCustomSynEdit read FEditor write SetEditor;
    property Language: TLanguageRec read FLanguage;
    property Modified: Boolean read FModified write FModified default False;
    property OpenDictionary: Boolean read FOpenDictionary;
    property Options: TSynSpellCheckOptions read FOptions write FOptions;
    property PenColor: TColor read FPenColor write SetPenColor default clRed;
    property SkipList: TStringList read FSkipList write SetSkipList;
    property UnderlineStyle: TUnderlineStyle read FUnderlineStyle
      write SetUnderlineStyle default usMicrosoftWord;
    property UserDirectory: string read GetUserDictionaryDir write FUserDictPath;
    property UseUserDictionary: Boolean read FUseUserDictionary write FUseUserDictionary default True;
    { Events }
    property OnAbort: TNotifyEvent read FOnAbort write FOnAbort;
    property OnAddWord: TOnAddWord read FOnAddWord write FOnAddWord;
    property OnCheckWord: TOnCheckWord read FOnCheckWord write FOnCheckWord;
    property OnCorrectWord: TOnCorrectWord read FOnCorrectWord
      write FOnCorrectWord;
    property OnDictClose: TNotifyEvent read FOnDictClose write FOnDictClose;
    property OnDictLoad: TNotifyEvent read FOnDictLoad write FOnDictLoad;
    property OnDone: TNotifyEvent read FOnDone write FOnDone;
    property OnSkipWord: TOnSkipWord read FOnSkipWord write FOnSkipWord;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
  end;

  ENoDictionaryLoaded = class(EExternal);
  resourcestring SNoDictionaryLoaded = 'No dictionary is loaded.';

function TrimEx(const sWord: string; const chChar: Char): string;

const
  //////////////////////////////////////////////////////////////////////////////
  // Action constants
  //////////////////////////////////////////////////////////////////////////////
  ACTION_ABORT   = -1;
  ACTION_SKIP    = 0;
  ACTION_SKIPALL = 1;
  ACTION_CORRECT = 2;
  ACTION_ADD     = 3;
  ACTION_UNDO    = -2;

procedure Register;

implementation

uses
  Math,
  Forms,
  StrUtils,
  SynEditHighlighter,
  SynEditMiscProcs;

type
  TSynEditHelper = class helper for TCustomSynEdit
  public
    function GetWordAtRowColHelper(XY: TBufferCoord; cIdentChars: TSynIdentChars;
      OverrideHighlighterChars: Boolean): string;
    function NextWordPosHelper(cIdentChars, cWhiteChars: TSynIdentChars): TBufferCoord;
    function PrevWordPosHelper(cIdentChars, cWhiteChars: TSynIdentChars): TBufferCoord;
  end;

procedure Register;
begin
  RegisterComponents('SynEdit', [TSynSpellCheck]);
end;

function ContainsNumbers(sWord: string): Boolean;
var
  iI: Integer;
begin
  Result := False;
  for iI := 1 to Length(sWord) do
    if sWord[iI] in ['1'..'9', '0'] then begin
      Result := True;
      Break;
    end;
end;

{ TSynSpellCheck }

function TSynSpellCheck.GetDefaultDictionaryDir: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
end;

function TSynSpellCheck.GetDictionaryDir: string;
begin
  if FDictPath <> '' then
    Result := IncludeTrailingPathDelimiter(FDictPath)
  else
    Result := IncludeTrailingPathDelimiter(GetDefaultDictionaryDir);
end;

function TSynSpellCheck.GetUserDictionaryDir;
begin
  if FUserDictPath <> '' then
    Result := IncludeTrailingPathDelimiter(FUserDictPath)
  else
    Result := IncludeTrailingPathDelimiter(GetDefaultDictionaryDir);
end;

function TrimEx(const sWord: string; const chChar: Char): string;
var
  iI, iLength: Integer;
begin
  iLength := Length(sWord);
  iI := 1;
  while (iI <= iLength) and (sWord[iI] <= chChar) do
    Inc(iI);
  if iI > iLength then
    Result := ''
  else begin
    while sWord[iLength] = chChar do
      Dec(iLength);
    Result := Copy(sWord, iI, iLength - iI + 1);
  end;
end;

{ TDrawAutoSpellCheckPlugin }

constructor TDrawAutoSpellCheckPlugin.Create(AOwner: TCustomSynEdit);
begin
  inherited;
  FPenColor       := clRed;
  FUnderlineStyle := usMicrosoftWord;
end;

procedure TDrawAutoSpellCheckPlugin.AfterPaint(ACanvas: TCanvas; const AClip: TRect;
  FirstLine, LastLine: Integer);
var
  LH, CX     : Integer;
  CurrentWord: string;
  CurrentXY  : TBufferCoord;
  TP         : TPoint;

  procedure PaintUnderLine;
  var
    MaxX,
    NewPoint,
    NewY: Integer;

    procedure DrawPoint;
    begin
      // Do not draw on gutter.
      // This happens when a word is underlined and part of it is "hidden" under
      // the gutter.
      if TP.X <= Editor.Gutter.RealGutterWidth(Editor.CharWidth) then
        Exit;
      with ACanvas do begin
        if NewY = TP.Y - 1 then
          Pen.Color := Editor.Color
        else
          Pen.Color := FPenColor;
        Pixels[TP.X, NewY] := Pen.Color;
      end;
    end;

  const
    // Microsoft Word style
//  MW_POINTS: array[0..6] of ShortInt = (1, 2, 2, 1, 0, 0, 0);
    MW_POINTS: array[0..3] of ShortInt = (0, 1, 2, 1);
    // Corel Word Perfect style
//  WP_POINTS: array[0..4] of ShortInt = (3, 2, 1, -1, -1);
    WP_POINTS: array[0..3] of ShortInt = (2, 1, 0, -1);

  begin
    Inc(TP.Y, LH - 3);
    NewPoint := 0;
    if FUnderlineStyle = usMicrosoftWord then
      NewY := TP.Y + MW_POINTS[NewPoint]
    else
      NewY := TP.Y + WP_POINTS[NewPoint];
    DrawPoint;
    MaxX := TP.X + ACanvas.TextWidth(CurrentWord);
    while TP.X <= MaxX do begin
      DrawPoint;
      Inc(NewPoint);
      if FUnderlineStyle = usMicrosoftWord then begin
        if NewPoint > High(MW_POINTS) then
          NewPoint := 0
      end else begin
        if NewPoint > High(WP_POINTS) then
          NewPoint := 0;
      end;
      DrawPoint;
      Inc(TP.X);
      if FUnderlineStyle = usMicrosoftWord then
        NewY := TP.Y + MW_POINTS[NewPoint]
      else
        NewY := TP.Y + WP_POINTS[NewPoint];
    end;
  end;

var
  sToken: string;
  Attri : TSynHighlighterAttributes;
begin
  if (not Assigned(FSynSpellCheck)) or (not Assigned(Editor)) or
    (not (sscoAutoSpellCheck in FSynSpellCheck.Options)) then
    Exit;
  LH     := Editor.LineHeight;
  ACanvas.Font.Assign(Editor.Font);
  while FirstLine <= LastLine do begin
    // Paint "Bad Words"
    CX := 1;
    while CX < Length(Editor.Lines[FirstLine - 1]) do begin
      CurrentXY   := BufferCoord(CX, FirstLine);
      CurrentWord := Editor.GetWordAtRowColHelper(CurrentXY, FSynSpellCheck.FIdentChars, True);
      TP := Editor.RowColumnToPixels(Editor.BufferToDisplayPos(CurrentXY));
      if TP.X > ACanvas.ClipRect.Right - ACanvas.ClipRect.Left then
        Break;
      if Assigned(Editor.Highlighter) then begin
        if Editor.GetHighlighterAttriAtRowCol(CurrentXY, sToken, Attri) = False then
          Attri := Editor.Highlighter.WhitespaceAttribute;
        if Assigned(Attri) and (FSynSpellCheck.FCheckAttribs.IndexOf(Attri.Name) <> -1) and
          (CurrentWord <> '') then
          if FSynSpellCheck.CheckWord(CurrentWord) = False then
            PaintUnderLine;
      end else if FSynSpellCheck.CheckWord(CurrentWord) = False then
        PaintUnderLine;
      Inc(CX, Length(CurrentWord));
      Inc(CX);
    end;
    Inc(FirstLine);
  end;
end;

procedure TDrawAutoSpellCheckPlugin.LinesDeleted(FirstLine, Count: Integer);
begin
  // This is only for the compiler hint
end;

procedure TDrawAutoSpellCheckPlugin.LinesInserted(FirstLine, Count: Integer);
begin
  // This is only for the compiler hint
end;

procedure TDrawAutoSpellCheckPlugin.SetPenColor(const Value: TColor);
begin
  if FPenColor <> Value then begin
    FPenColor := Value;
    if Editor <> nil then
      Editor.Repaint;
  end;
end;

procedure TDrawAutoSpellCheckPlugin.SetUnderlineStyle(
  const Value: TUnderlineStyle);
begin
  if FUnderlineStyle <> Value then begin
    FUnderlineStyle := Value;
    if Editor <> nil then
      Editor.Repaint;
  end;
end;

{ TSynSpellCheck }

constructor TSynSpellCheck.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHunSpellExt := THunSpellExt.Create;
  FPenColor := clRed;
  FBusy     := False;
  FModified := False;
  FUnderlineStyle := usMicrosoftWord;
  FUseUserDictionary := True;
  FApostrophes := '''`´';
  //////////////////////////////////////////////////////////////////////////////
  // Lists
  //////////////////////////////////////////////////////////////////////////////
  FEditors  := TList.Create;
  FPlugins  := TList.Create;
  FWordList := TWStringList.Create;
  FWordList.Duplicates := dupIgnore;
  FSkipList := TStringList.Create;
  FSkipList.Duplicates := dupIgnore;

  FCheckAttribs := TStringList.Create;
  FCheckAttribs.Add('Comment');
  FCheckAttribs.Add('Text');
  FCheckAttribs.Add('String');
  FCheckAttribs.Add('Documentation');

  SetApostrophes(FApostrophes); // Generate character set
end;

destructor TSynSpellCheck.Destroy;
begin
  CloseDictionary;
  FreeAndNil(FHunSpellExt);
  //////////////////////////////////////////////////////////////////////////////
  // Free used memory
  //////////////////////////////////////////////////////////////////////////////
  FCheckAttribs.Free;
  FEditors.Free;
  FPlugins.Free;
  FSkipList.Free;
  FWordList.Free;
  inherited;
end;

function TSynSpellCheck.DictionaryExists(Language: string; Path: string = ''): Boolean;
var
  sTemp : string;
  srTemp: TSearchRec;
begin
  if Trim(Path) = '' then
    sTemp := GetDictionaryDir // Search in shared dictionary directory
  else
    sTemp := Path;            // Search in user specified directory
  Result := (FindFirst(sTemp + Language + '.?-?-?.dic', faAnyFile, srTemp) = 0);
end;

function TSynSpellCheck.GetNewestDictionary(Language: string): string;
var
  srDict : TSearchRec;
  tslTemp: TStringList;
begin
  tslTemp := TStringList.Create;
  if FindFirst(GetDictionaryDir + Language + '.?-?-?.dic', faAnyFile,
    srDict) = 0 then begin
    if Pos('.user.', srDict.Name) = 0 then
      tslTemp.Add(AnsiLowerCase(srDict.Name));
    while FindNext(srDict) = 0 do begin
      if Pos('.user.', srDict.Name) = 0 then
        tslTemp.Add(AnsiLowerCase(srDict.Name));
    end;
  end;
  with tslTemp do begin
    if Count > 0 then begin
      Sort;
      Result := Strings[Count - 1];
    end;
    Free;
  end;
  SysUtils.FindClose(srDict);
end;

function TSynSpellCheck.GetWordCount: Integer;
begin
  Result := FWordList.Count;
end;

procedure TSynSpellCheck.AddDictWord(Word: string);
begin
  if Trim(Word) = '' then
    Exit;
  Word := AnsiLowerCase(Word);
  if not CheckWord(Word) then begin
    FWordList.Add(Word);
    FHunSpellExt.put_word(Word);
    FModified := True;
    if Assigned(FOnAddWord) then
      FOnAddWord(Self, Word);
  end;
end;

procedure TSynSpellCheck.AddDictWordList(WordList: TStringList);
var
  word: string;
begin
  for word in WordList do
    AddDictWord(word);
end;

function TSynSpellCheck.AddEditor(AEditor: TCustomSynEdit): integer;
var
  iI    : Integer;
  Plugin: TDrawAutoSpellCheckPlugin;
begin
  // Adds an Editor and returns its index in the list
  Result := -1;
  if AEditor <> nil then begin
    iI := FEditors.IndexOf(AEditor);
    if iI = -1 then begin
      Plugin := TDrawAutoSpellCheckPlugin.Create(AEditor);
      with Plugin do begin
        FSynSpellCheck := Self;
        PenColor       := Self.FPenColor;
        UnderlineStyle := Self.FUnderlineStyle;
      end;
      iI := FEditors.Add(AEditor);
      FPlugIns.Add(Plugin);
      Result := iI;
    end else
      Result := iI;
  end;
end;

procedure TSynSpellCheck.AddSkipWord(Word: string);
begin
  if Trim(Word) <> '' then
    FSkipList.Add(AnsiLowerCase(Word));
end;

procedure TSynSpellCheck.AddSkipWordList(WordList: TStringList);
var
  iI: Integer;
begin
  for iI := 0 to WordList.Count - 1 do
    AddSkipWord(WordList.Strings[iI]);
end;

function TSynSpellCheck.CheckWord(Word: string): Boolean;
var
  iI: Integer;
begin
  Word := Trim(Word);
  if (Word = '') or (sscoIgnoreSingleChars in FOptions) and (Length(Word) = 1) then begin
    Result := True;
    Exit;
  end;

  // It's quicker to check before checking word list
  if sscoIgnoreWordsWithNumbers in FOptions then
    for iI := 1 to Length(Word) do
      if Word[iI] in ['0'..'9'] then begin
        Result := True;
        Exit;
      end;

  //////////////////////////////////////////////////////////////////////////////
  // Check if word consists only of dashes or apostrophes. Quite often these
  // are used when dividing sections in ASCII text files.
  //////////////////////////////////////////////////////////////////////////////
  if (TrimEx(Word, '-') = '') or (TrimEx(Word, '''') = '') then begin
    Result := True;
    Exit;
  end;

  if sscoTrimApostrophes in FOptions then
    Word := TrimEx(Word, ''''); // Trim apostrophes
  //////////////////////////////////////////////////////////////////////////////
  // Main Searching Routine
  //////////////////////////////////////////////////////////////////////////////

  Result := FHunSpellExt.spell(Word);
end;

procedure TSynSpellCheck.ClearDictWords;
begin
  FWordList.Clear;
end;

procedure TSynSpellCheck.ClearSkipWords;
begin
  FSkipList.Clear;
end;

procedure TSynSpellCheck.CloseDictionary;
begin
  ClearDictWords;
  FSkipList.Clear;
  FOpenDictionary := False;
  if Assigned(FOnDictClose) then
    FOnDictClose(Self);
  FHunSpellExt.Clear();
end;

procedure TSynSpellCheck.DeleteDictWord(Word: string);
var
  xIndex: integer; 
begin
  xIndex := FWordList.IndexOf(AnsiLowerCase(Word));
  if xIndex <> -1 then
    FWordList.Delete(xIndex);
end;

procedure TSynSpellCheck.DeleteSkipWord(Word: string);
begin
  with FSkipList do
    Delete(IndexOf(AnsiLowerCase(Word)));
end;

function TSynSpellCheck.IsDictWord(Word: string): Boolean;
begin
  Result := CheckWord(Word);
end;

procedure TSynSpellCheck.FixLists;
var
  iI: Integer;
begin
  for iI := 0 to FSkipList.Count - 1 do
    FSkipList.Strings[iI] := AnsiLowerCase(FSkipList.Strings[iI]);
end;

procedure TSynSpellCheck.GetDictionaryList(var tslList: TStringList);
var
  srDics: TSearchRec;

  procedure AddDictionary;
  var
    sLanguage: string;
  begin
    sLanguage := Copy(srDics.Name, 1, Pos('.', srDics.Name) - 1);
    if (tslList.IndexOf(sLanguage) = -1) and (Pos('.user.', srDics.Name) = 0) then
      tslList.Add(sLanguage);
  end;

begin
  if FindFirst(GetDictionaryDir + '*.aff', faAnyFile, srDics) = 0 then
  begin
    AddDictionary;
    while FindNext(srDics) = 0 do
      AddDictionary;
  end;
  SysUtils.FindClose(srDics);
  tslList.Sort;
end;

function TSynSpellCheck.GetSuggestions(Word: string;
  SuggestionList: TStringList): Integer;
var
  wrds: TWStrings;
  i: integer;
begin
  Result := 0;
  if not (sscoSuggestWords in FOptions) then
    Exit;
  if Assigned(SuggestionList) then begin
    for i := 0 to FHunSpellExt.Count - 1 do
      if FHunSpellExt.Enabled[i] then
        begin
          wrds := FHunSpellExt.Item[i].suggest(Word);
          wrds.AddStringsTo(SuggestionList);
//          SuggestionList.Add('-');
        end;
      SuggestionList.Delete(SuggestionList.Count-1);

    Result := SuggestionList.Count;
  end;
end;

procedure TSynSpellCheck.LoadDictionary(Language: string; FileName: string = '');
var
  sLine    : WideString;
  sName    : string;
  i: Integer;
begin
  FDictionary := Language;
  FHunSpellExt.Append(GetDictionaryDir + Language + '.aff', GetDictionaryDir + Language + '.dic');

  // always load additional en_US
  FHunSpellExt.Append(GetDictionaryDir + 'en_US' + '.aff', GetDictionaryDir + 'en_US' + '.dic');

//  FUserFileName := FLanguage.Name + 'user.dic';
  FUserFileName := 'user.dic';
  //////////////////////////////////////////////////////////////////////////////
  // Load user's dictionary if present
  //////////////////////////////////////////////////////////////////////////////
  FModified := False;
  if FUseUserDictionary then begin
    if FUserDictPath = '' then
      FUserDictPath := GetUserDictionaryDir;
    sName := IncludeTrailingPathDelimiter(FUserDictPath) + FUserFileName;
    if FileExists(sName) then begin
      FWordList.LoadFromFile(sName, [foUnicodeLB]);
      for i := 0 to FWordList.Count - 1 do
        begin
          sLine := Trim(FWordList[i]);
          if sLine <> '' then
            FHunSpellExt.put_word(sLine);
        end;
      FModified := False;
    end;
  end;

  FOpenDictionary := True;
  if (sscoAutoSpellCheck in FOptions) and (Assigned(FEditor)) then
    FEditor.Invalidate;
  if Assigned(FOnDictLoad) then
    FOnDictLoad(Self);
end;

procedure TSynSpellCheck.LoadSkipList(FileName: string);
begin
  if FileExists(FileName) then
    FSkipList.LoadFromFile(FileName);
end;

function TSynSpellCheck.RemoveEditor(AEditor: TCustomSynEdit): Boolean;
var
  iI: Integer;
begin
  Result := False;
  if AEditor <> nil then begin
    iI := FEditors.IndexOf(AEditor);
    if iI > -1 then begin
      if FEditor = AEditor then begin
        FEditor             := nil;
        FDrawAutoSpellCheck := nil;
      end;
      FEditors.Delete(iI);
      FPlugIns.Delete(iI);
      Result := True;
    end;
  end;
end;

procedure TSynSpellCheck.SaveSkipList(FileName: string);
var
  iI: Integer;
begin
  for iI := 0 to FSkipList.Count -1 do
    if Trim(FSkipList.Strings[iI]) = '' then
      FSkipList.Delete(iI);
  FSkipList.SaveToFile(FileName);
end;

procedure TSynSpellCheck.SaveUserDictionary;
var
  xDirPath: string;
begin
  xDirPath := IncludeTrailingPathDelimiter(ExtractFileDir(FUserDictPath));
  if not DirectoryExists(xDirPath) then
    if not ForceDirectories(xDirPath) then
      Exit;
  FWordList.SaveToFile(xDirPath + FUserFileName, [foUnicodeLB] );
  FModified := False;
end;

procedure TSynSpellCheck.SelectWordAtCursor;
begin
  if FEditor = nil then
    Exit;
  FEditor.BlockBegin := FEditor.WordStartEx(FEditor.CaretXY);
  FEditor.BlockEnd := FEditor.WordEndEx(FEditor.CaretXY);
end;

procedure TSynSpellCheck.SetCheckAttribs(const Value: TStringList);
begin
  FCheckAttribs.Assign(Value);
end;

procedure TSynSpellCheck.SetEditor(const Value: TCustomSynEdit);
var
  iI: Integer;
begin
  if Value <> FEditor then begin
    iI := AddEditor(Value);
    if iI > -1 then begin
      FEditor := FEditors[iI];
      FDrawAutoSpellCheck := FPlugIns[iI];
      with FDrawAutoSpellCheck do begin
        FSynSpellCheck := Self;
        PenColor       := Self.FPenColor;
      end;
    end else begin
      FEditor             := nil;
      FDrawAutoSpellCheck := nil;
    end;
  end;
end;

procedure TSynSpellCheck.SetPenColor(const Value: TColor);
begin
  FPenColor := Value;
  if FDrawAutoSpellCheck <> nil then
    FDrawAutoSpellCheck.PenColor := Value;
end;

procedure TSynSpellCheck.SetSkipList(Value: TStringList);
begin
  SkipList.Assign(Value);
end;

procedure TSynSpellCheck.SetUnderlineStyle(const Value: TUnderlineStyle);
begin
  FUnderlineStyle := Value;
  if FDrawAutoSpellCheck <> nil then
    FDrawAutoSpellCheck.UnderlineStyle := Value;
end;

procedure TSynSpellCheck.SpellCheck;
var
  bAborted,
  bUndoEnabled  : Boolean;
  sToken,
  sWord         : string;
  pLastWord,
  pNextWord     : TBufferCoord;
  tslSuggestions: TStringList;
  Attri         : TSynHighlighterAttributes;

  function InternalCheckWord(Word: string): Boolean;
  var
    iAction     : Integer;
    sCorrectWord: string;
  begin
    Result := True;
    if not CheckWord(AnsiLowerCase(Word)) then begin
      if sscoHideCursor in FOptions then
        FEditor.EndUpdate;
      with FEditor do begin
        Update;
        EnsureCursorPosVisible;
      end;
      if sscoHourGlass in FOptions then
        Screen.Cursor := FCursor;
      if Assigned(FOnCheckWord) then begin
        // Get suggestions
        if sscoSuggestWords in FOptions then
          GetSuggestions(Word, tslSuggestions);
        if sscoSelectWord in FOptions then
          SelectWordAtCursor;
        FOnCheckWord(Self, Word, tslSuggestions, sCorrectWord, iAction);
        tslSuggestions.Clear; // Remove items to free some memory
        case iAction of
          ACTION_ABORT: begin
            Result   := False;
            bAborted := True;
            with FEditor do begin
              BlockBegin := CaretXY;
              BlockEnd   := BlockBegin;
            end;
          end;
          ACTION_ADD: AddDictWord(sWord);
          ACTION_CORRECT: begin
            SelectWordAtCursor;
            FEditor.SelText := sCorrectWord;
            if Assigned(FOnCorrectWord) then
              FOnCorrectWord(Self, sWord, sCorrectWord);
          end;
          ACTION_SKIPALL: begin
            AddSkipWord(sWord);
            if Assigned(FOnSkipWord) then
              FOnSkipWord(Self, sWord, True);
          end;
          ACTION_SKIP: if Assigned(FOnSkipWord) then
            FOnSkipWord(Self, sWord, False);
          ACTION_UNDO: begin
            with FEditor do begin
              Undo;
              CaretXY := pLastWord;
              if sscoGoUp in FOptions then
                CaretXY := NextWordPosHelper(FIdentChars, FWhiteChars)
              else
                CaretXY := PrevWordPosHelper(FIdentChars, FWhiteChars);
              bUndoEnabled := False;
            end;
          end;
        end;
      end;
      if sscoHourGlass in FOptions then
        Screen.Cursor := crHourGlass;
      if sscoHideCursor in FOptions then
        FEditor.BeginUpdate;
    end;
  end;

begin
  bUndoEnabled := False;
  // If no dictionary if loaded and spell checking is requested and Exception
  // is thrown.
  if not FOpenDictionary then
    raise ENoDictionaryLoaded.CreateRes(@SNoDictionaryLoaded);

  FBusy := True;
  if Assigned(FOnStart) then
    FOnStart(Self);
  bAborted := False;
  if sscoHourGlass in FOptions then begin
    FCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
  end;
  tslSuggestions := TStringList.Create;

  with FEditor do begin
    if Trim(Lines.Text) = '' then begin
      Screen.Cursor := FCursor;
      if Assigned(FOnDone) then
        FOnDone(Self);
      FBusy := False;
      tslSuggestions.Free;
      Exit;
    end;
    if not (sscoStartFromCursor in FOptions) then
      CaretXY := BufferCoord(1, 1);
    if sscoHideCursor in FOptions then
      BeginUpdate;
    if sscoGoUp in FOptions then
      pNextWord := PrevWordPosHelper(FIdentChars, FWhiteChars)
    else
      pNextWord := NextWordPosHelper(FIdentChars, FWhiteChars);
    pLastWord := pNextWord;
    while pNextWord.Char > 0 do begin
      Attri := nil;
      //////////////////////////////////////////////////////////////////////////
      // Check if the word is the last word
      // Is cursor at end of text?
      //////////////////////////////////////////////////////////////////////////
      if sscoGoUp in FOptions then begin
        if (PrevWordPosHelper(FIdentChars, FWhiteChars).Char = CaretX) and
          (Lines.Count = CaretY) then
        Break;
      end else begin
        if (NextWordPosHelper(FIdentChars, FWhiteChars).Char = CaretX) and
          (Lines.Count = CaretY) then
        Break;
      end;
      //////////////////////////////////////////////////////////////////////////
      // Make sure we do not get any 'blank' words
      //////////////////////////////////////////////////////////////////////////
      while Trim(GetWordAtRowColHelper(CaretXY, FIdentChars, True)) = '' do begin
        { Just move to next word }
        if sscoGoUp in FOptions then
          pNextWord := PrevWordPosHelper(FIdentChars, FWhiteChars)
        else
          pNextWord := NextWordPosHelper(FIdentChars, FWhiteChars);
        CaretXY := pNextWord;
        { If it the last word then exit loop }
        if pNextWord.Char = 0 then
          Break;
      end;
      if pNextWord.Char = 0 then
        Break;
      sWord := GetWordAtRowColHelper(CaretXY, FIdentChars, True);
      //////////////////////////////////////////////////////////////////////////
      // Check if the word is in the dictionary
      //////////////////////////////////////////////////////////////////////////
      if Highlighter = nil then begin
        if InternalCheckWord(sWord) = False then
          Break;
      end else begin
        if GetHighlighterAttriAtRowCol(CaretXY, sToken, Attri) = False then
          Attri := Highlighter.WhitespaceAttribute;
        if Assigned(Attri) and (FCheckAttribs.IndexOf(Attri.Name) <> -1) and
          (not InternalCheckWord(sWord)) then
          Break;
      end;
      //////////////////////////////////////////////////////////////////////////
      // Prepare next word position
      //////////////////////////////////////////////////////////////////////////
      if sscoGoUp in FOptions then
        pNextWord := PrevWordPosHelper(FIdentChars, FWhiteChars)
      else
        pNextWord := NextWordPosHelper(FIdentChars, FWhiteChars);
      CaretXY  := pNextWord;
    end;
    if sscoHideCursor in FOptions then
      EndUpdate;
  end;
  tslSuggestions.Free;
  if sscoHourGlass in FOptions then
    Screen.Cursor := FCursor;
  //////////////////////////////////////////////////////////////////////////////
  // Remove last word selection
  //////////////////////////////////////////////////////////////////////////////
  with FEditor do begin
    BlockBegin := CaretXY;
    BlockEnd   := BlockBegin;
  end;
  if bAborted then begin
    if Assigned(FOnAbort) then
      FOnAbort(Self)
  end else if Assigned(FOnDone) then
    FOnDone(Self);
  FBusy := False;
end;

procedure TSynSpellCheck.SetApostrophes(const Value: string);
var
  chChar: Char;
  i    : Integer;
begin
  for chChar := #0 to #255 do
    if IsCharAlphaNumeric(chChar) then
      Include(FIdentChars, chChar);
  for i := 1 to Length(FApostrophes) do
    Include(FIdentChars, FApostrophes[i]);
  FWhiteChars := [#1..#255] - FIdentChars;
end;

{ TSynEditEx }

function TSynEditHelper.GetWordAtRowColHelper(XY: TBufferCoord;
  cIdentChars: TSynIdentChars; OverrideHighlighterChars: Boolean): string;
var
  Line: string;
  IdChars: TSynIdentChars;
  Len, Stop: integer;
begin
  Result := '';
  if (XY.Line >= 1) and (XY.Line <= Lines.Count)  then begin
    Line := Lines[XY.Line - 1];
    Len := Length(Line);
    if (XY.Char >= 1) and (XY.Char <= Len + 1) then begin
      if Assigned(Highlighter) and not OverrideHighlighterChars then
        IdChars := Highlighter.IdentChars
      else
        IdChars := cIdentChars;
      Stop := XY.Char;
      while (Stop <= Len) and (Line[Stop] in IdChars) do
        Inc(Stop);
      while (XY.Char > 1) and (Line[XY.Char - 1] in IdChars) do
        Dec(XY.Char);
      if Stop > XY.Char then
        Result := Copy(Line, XY.Char, Stop - XY.Char);
    end;
  end;
end;

function TSynEditHelper.NextWordPosHelper(cIdentChars, cWhiteChars: TSynIdentChars): TBufferCoord;
var
  CX, CY, LineLen, MultiPos: integer;
  Line: string;

  procedure CheckOnNextLine;
  begin
    // find first IdentChar or multibyte char in the next line
    if CY < Lines.Count then begin
      Line := Lines[CY];
      Inc(CY);
{$IFDEF SYN_MBCSSUPPORT}
      MultiPos := StrScanForMultiByteChar(Line, 1);
{$ELSE}
      MultiPos := 0;
{$ENDIF}
      CX := StrScanForCharInSet(Line, 1, cIdentChars);
      // stop on any multibyte chars
      if (MultiPos > 0) and ((CX = 0) or (CX > MultiPos)) then
        CX := MultiPos;
      if CX = 0 then
        CheckOnNextLine;
    end;
  end;

begin
  CX := CaretX;
  CY := CaretY;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then begin
    Line    := Lines[CY - 1];
    LineLen := Length(Line);
    if CX >= LineLen then begin
      CheckOnNextLine;
    end else begin
{$IFDEF SYN_MBCSSUPPORT}
      MultiPos := StrScanForMultiByteChar(Line, CX + 1);
      // find next "whitespace" if current char is an IdentChar
      if (Line[CX] in cIdentChars) and (ByteType(Line, CX) = mbSingleByte) then
        CX := StrScanForCharInSet(Line, CX, cWhiteChars);
{$ELSE}
      MultiPos := 0;
      // find next "whitespace" if current char is an IdentChar
      if Line[CX] in IdentChars then
        CX := StrScanForCharInSet(Line, CX, cWhiteChars);
{$ENDIF}
      // if "whitespace" found, find the next IdentChar
      if (CX > 0) and (CX < LineLen) then begin
          CX := StrScanForCharInSet(Line, CX, cIdentChars);
        // stop on any multibyte chars
        if (MultiPos > 0) and ((CX = 0) or (CX > MultiPos)) then
          CX := MultiPos;
        // if one of those failed just position at the end of the line
        if CX = 0 then
          CheckOnNextLine;
      end else CheckOnNextLine;
    end;
  end;
  Result := BufferCoord(CX, CY);
end;

function TSynEditHelper.PrevWordPosHelper(cIdentChars, cWhiteChars: TSynIdentChars): TBufferCoord;
var
  CX, CY, MultiPos: integer;
  Line: string;

  procedure CheckForIdentChar;
  begin
    if CX <= 1 then
      Exit;
{$IFDEF SYN_MBCSSUPPORT}
    MultiPos := StrRScanForMultiByteChar(Line, CX - 1);
    // If previous char is a "whitespace" search for the last IdentChar
    if (Line[CX - 1] in cWhiteChars) and (ByteType(Line, CX - 1) = mbSingleByte) then
      CX := StrRScanForCharInSet(Line, CX - 1, cIdentChars);
{$ELSE}
    MultiPos := 0;
    // If previous char is a "whitespace" search for the last IdentChar
    if Line[CX - 1] in cWhiteChars then
      CX := StrRScanForCharInSet(Line, CX - 1, cIdentChars);
{$ENDIF}
    if CX > 0 then
      // Search for the first IdentChar of this "word"
      CX := StrRScanForCharInSet(Line, CX - 1, cWhiteChars) + 1;
    // Stop on any multibyte chars
    if (MultiPos > 0) and ((CX = 0) or (CX < MultiPos)) then
      CX := MultiPos;

    if CX = 0 then begin
      // Same as CheckOnPrevLine, but we can't have a circular reference
      //  find last cIdentChar in the previous line
      if CY > 1 then begin
        Dec(CY);
        Line := Lines[CY - 1];
        while (CY > 1) and (Line = '') do begin
          Dec(CY);
          Line := Lines[CY - 1];
        end;
        if Line = '' then
          CX := 1
        else begin
          CX := Length(Line) + 1;
          CheckForIdentChar;
        end;
      end else
        CX := 1;
    end;
  end;

  procedure CheckOnPrevLine;
  begin
    // Find last IdentChar in the previous line
    if CY > 1 then begin
      Dec(CY);
      Line := Lines[CY - 1];
      CX := Length(Line) + 1;
      CheckForIdentChar;
    end else
      CX := 1;
  end;

begin
  CX := CaretX;
  CY := CaretY;
  // Valid line?
  if (CY >= 1) and (CY <= Lines.Count) then begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);
    if CX <= 1 then
      CheckOnPrevLine
    else
      CheckForIdentChar;
  end;
  Result := BufferCoord(CX, CY);
end;

end.
