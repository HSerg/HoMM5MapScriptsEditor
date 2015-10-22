// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UDKConsts;

interface

type
  TDKConsts = class
  protected

  public
    // Добавить '%s' в словарь
    // function SADD_WORD_TO_DICT_FMT(): string; overload;
    function SADD_WORD_TO_DICT_FMT(const Args: array of const): string; overload;

    // Заменить текcт "%s" на "%s"?
    // function SASK_REPLACE_TEXT_FMT(): string; overload;
    function SASK_REPLACE_TEXT_FMT(const Args: array of const): string; overload;

    // Список подходящих функций и переменных
    function SAUTOCOMPLETE_TITLE(): string;

    // Не заполнена строка поиска
    function SBLANK_SEARCH_STRING_WARNING(): string;

    // Закрытие файлов приведёт к потере изменений. Вы уверены, что хотите выйти из программы ?
    function SCAN_CLOSE_ALL_FILES(): string;

    // Закрытие файла карты приведёт к потере изменений. Вы уверены, что хотите продолжить ?
    function SCAN_CLOSE_MAP(): string;

    // Ввод имени папки
    function SCAPTION_DIR_FILENAME(): string;

    // Ввод имени нового файла
    function SCAPTION_LUA_FILENAME(): string;

    // Ввод имени файла
    function SCAPTION_TXT_FILENAME(): string;

    // Вы уверены, что хотите удалить "%s" ?
    // function SCONF_DEL_FILE_FMT(): string; overload;
    function SCONF_DEL_FILE_FMT(const Args: array of const): string; overload;

    // Неправильно задано имя папки.
    function SERROR_DIR_FILENAME(): string;

    // Неправильно задано имя lua-файла.
    function SERROR_LUA_FILENAME(): string;

    // Неправильно задано имя текстового файла.
    function SERROR_TXT_FILENAME(): string;

    // Указанный файл не найден.
    function SFILE_NOT_FOUND(): string;

    // Указанный файл "%s" не найден.
    // function SFILE_NOT_FOUND_FMT(): string; overload;
    function SFILE_NOT_FOUND_FMT(const Args: array of const): string; overload;

    // Вы уверены, что хотите отменить все изменения и вернуть содержимое файла в исходное состояние ?
    function SFILE_UNDO_CHANGES(): string;

    // Номер строки:
    function SGOTO_TEXT(): string;

    // Перейти к строке
    function SGOTO_TITLE(): string;

    // Файл карты:
    function SHEADER_MAP_LABEL(): string;

    // help_ru.chm
    function SHELP_FILENAME(): string;

    // Heroes of Might and Magic V - Владыки Севера
    function SHMM5A1_TITLE(): string;

    // Heroes of Might and Magic V - Повелители Орды
    function SHMM5A2_TITLE(): string;

    // Heroes of Might and Magic V
    function SHMM5_TITLE(): string;

    // [Ошибка]: %s (в строке: %u) - %s
    // function SLUA_ERROR_FMT(): string; overload;
    function SLUA_ERROR_FMT(const Args: array of const): string; overload;

    // [Несовместимость с LUA 5]: %s (в строке: %u) - %s
    // function SLUA_ERROR_FMT(): string; overload;
    function SLUA_LUA5_WARN_FMT(const Args: array of const): string; overload;

    // [Подсказка]:  Синтаксис успешно проверен - %s
    // function SLUA_HINT_FMT(): string; overload;
    function SLUA_HINT_FMT(const Args: array of const): string; overload;

    // Закрытие файла "%s" приведёт к потере изменений. Вы уверены, что хотите продолжить ?
    // function SMOD_FILE_CLOSE_FMT(): string; overload;
    function SMOD_FILE_CLOSE_FMT(const Args: array of const): string; overload;

    // Ошибка обработки %s
    // function SOPEN_FILE_ERROR_FMT(): string; overload;
    function SOPEN_FILE_ERROR_FMT(const Args: array of const): string; overload;

    // Open file "%s" error.
    // function SOPEN_ZIPFILE_ERROR_FMT(): string; overload;
    function SOPEN_ZIPFILE_ERROR_FMT(const Args: array of const): string; overload;

    // Общие
    function SOPTIONS_COMMONS(): string;

    // Редакторы
    function SOPTIONS_EDITORS(): string;

    // Язык
    function SOPTIONS_LANG(): string;

    // Пути
    function SOPTIONS_PATHS(): string;

    // Имя новой папки:
    function SPROMT_DIR_FILENAME(): string;

    // Имя нового lua-файла:
    function SPROMT_LUA_FILENAME(): string;

    // Имя нового текстового файла:
    function SPROMT_TXT_FILENAME(): string;

    // Строка
    function SSEARCH_IN_FILES_LINE(): string;

    // Имя файла
    function SSEARCH_TREE_HEADER(): string;

    // Строка "%s" не найдена.
    // function SSTRING_NOT_FOUND_FMT(): string; overload;
    function SSTRING_NOT_FOUND_FMT(const Args: array of const): string; overload;

    // Поиск:
    function STREE_INCSEARCH(): string;

    // Неизвестна
    function SUNKNOWN(): string;

    // Версия
    function SVERSION(): string;

    // * Без параметров *
    function SWITHOUT_PARAMS(): string;

  end;

var
  DKConsts: TDKConsts;

implementation

uses
  DKLang, SysUtils;

//function TDKConsts.SADD_WORD_TO_DICT_FMT(): string;
//begin
//  Result := DKLangConstW('SADD_WORD_TO_DICT_FMT');
//end;

function TDKConsts.SADD_WORD_TO_DICT_FMT(const Args: array of const): string;
begin
  Result := DKLangConstW('SADD_WORD_TO_DICT_FMT', Args);
end;

//function TDKConsts.SASK_REPLACE_TEXT_FMT(): string;
//begin
//  Result := DKLangConstW('SASK_REPLACE_TEXT_FMT');
//end;

function TDKConsts.SASK_REPLACE_TEXT_FMT(const Args: array of const): string;
begin
  Result := DKLangConstW('SASK_REPLACE_TEXT_FMT', Args);
end;

function TDKConsts.SAUTOCOMPLETE_TITLE: string;
begin
  Result := DKLangConstW('SAUTOCOMPLETE_TITLE');
end;

function TDKConsts.SBLANK_SEARCH_STRING_WARNING: string;
begin
  Result := DKLangConstW('SBLANK_SEARCH_STRING_WARNING');
end;

function TDKConsts.SCAN_CLOSE_ALL_FILES: string;
begin
  Result := DKLangConstW('SCAN_CLOSE_ALL_FILES');
end;

function TDKConsts.SCAN_CLOSE_MAP: string;
begin
  Result := DKLangConstW('SCAN_CLOSE_MAP');
end;

function TDKConsts.SCAPTION_DIR_FILENAME: string;
begin
  Result := DKLangConstW('SCAPTION_DIR_FILENAME');
end;

function TDKConsts.SCAPTION_LUA_FILENAME: string;
begin
  Result := DKLangConstW('SCAPTION_LUA_FILENAME');
end;

function TDKConsts.SCAPTION_TXT_FILENAME: string;
begin
  Result := DKLangConstW('SCAPTION_TXT_FILENAME');
end;

//function TDKConsts.SCONF_DEL_FILE_FMT(): string;
//begin
//  Result := DKLangConstW('SCONF_DEL_FILE_FMT');
//end;

function TDKConsts.SCONF_DEL_FILE_FMT(const Args: array of const): string;
begin
  Result := DKLangConstW('SCONF_DEL_FILE_FMT', Args);
end;

function TDKConsts.SERROR_DIR_FILENAME: string;
begin
  Result := DKLangConstW('SERROR_DIR_FILENAME');
end;

function TDKConsts.SERROR_LUA_FILENAME: string;
begin
  Result := DKLangConstW('SERROR_LUA_FILENAME');
end;

function TDKConsts.SERROR_TXT_FILENAME: string;
begin
  Result := DKLangConstW('SERROR_TXT_FILENAME');
end;

function TDKConsts.SFILE_NOT_FOUND: string;
begin
  Result := DKLangConstW('SFILE_NOT_FOUND');
end;

//function TDKConsts.SFILE_NOT_FOUND_FMT(): string;
//begin
//  Result := DKLangConstW('SFILE_NOT_FOUND_FMT');
//end;

function TDKConsts.SFILE_NOT_FOUND_FMT(const Args: array of const): string;
begin
  Result := DKLangConstW('SFILE_NOT_FOUND_FMT', Args);
end;

function TDKConsts.SFILE_UNDO_CHANGES: string;
begin
  Result := DKLangConstW('SFILE_UNDO_CHANGES');
end;

function TDKConsts.SGOTO_TEXT: string;
begin
  Result := DKLangConstW('SGOTO_TEXT');
end;

function TDKConsts.SGOTO_TITLE: string;
begin
  Result := DKLangConstW('SGOTO_TITLE');
end;

function TDKConsts.SHEADER_MAP_LABEL: string;
begin
  Result := DKLangConstW('SHEADER_MAP_LABEL');
end;

function TDKConsts.SHELP_FILENAME: string;
begin
  Result := DKLangConstW('SHELP_FILENAME');
end;

function TDKConsts.SHMM5A1_TITLE: string;
begin
  Result := DKLangConstW('SHMM5A1_TITLE');
end;

function TDKConsts.SHMM5A2_TITLE: string;
begin
  Result := DKLangConstW('SHMM5A2_TITLE');
end;

function TDKConsts.SHMM5_TITLE: string;
begin
  Result := DKLangConstW('SHMM5_TITLE');
end;

//function TDKConsts.SLUA_ERROR_FMT(): string;
//begin
//  Result := DKLangConstW('SLUA_ERROR_FMT');
//end;

function TDKConsts.SLUA_ERROR_FMT(const Args: array of const): string;
begin
  Result := DKLangConstW('SLUA_ERROR_FMT', Args);
end;

//function TDKConsts.SLUA_HINT_FMT(): string;
//begin
//  Result := DKLangConstW('SLUA_HINT_FMT');
//end;

function TDKConsts.SLUA_HINT_FMT(const Args: array of const): string;
begin
  Result := DKLangConstW('SLUA_HINT_FMT', Args);
end;

function TDKConsts.SLUA_LUA5_WARN_FMT(const Args: array of const): string;
begin
  Result := DKLangConstW('SLUA_LUA5_WARN_FMT', Args);
end;

//function TDKConsts.SMOD_FILE_CLOSE_FMT(): string;
//begin
//  Result := DKLangConstW('SMOD_FILE_CLOSE_FMT');
//end;

function TDKConsts.SMOD_FILE_CLOSE_FMT(const Args: array of const): string;
begin
  Result := DKLangConstW('SMOD_FILE_CLOSE_FMT', Args);
end;

//function TDKConsts.SOPEN_FILE_ERROR_FMT(): string;
//begin
//  Result := DKLangConstW('SOPEN_FILE_ERROR_FMT');
//end;

function TDKConsts.SOPEN_FILE_ERROR_FMT(const Args: array of const): string;
begin
  Result := DKLangConstW('SOPEN_FILE_ERROR_FMT', Args);
end;

//function TDKConsts.SOPEN_ZIPFILE_ERROR_FMT(): string;
//begin
//  Result := DKLangConstW('SOPEN_ZIPFILE_ERROR_FMT');
//end;

function TDKConsts.SOPEN_ZIPFILE_ERROR_FMT(const Args: array of const): string;
begin
  Result := DKLangConstW('SOPEN_ZIPFILE_ERROR_FMT', Args);
end;

function TDKConsts.SOPTIONS_COMMONS: string;
begin
  Result := DKLangConstW('SOPTIONS_COMMONS');
end;

function TDKConsts.SOPTIONS_EDITORS: string;
begin
  Result := DKLangConstW('SOPTIONS_EDITORS');
end;

function TDKConsts.SOPTIONS_LANG: string;
begin
  Result := DKLangConstW('SOPTIONS_LANG');
end;

function TDKConsts.SOPTIONS_PATHS: string;
begin
  Result := DKLangConstW('SOPTIONS_PATHS');
end;

function TDKConsts.SPROMT_DIR_FILENAME: string;
begin
  Result := DKLangConstW('SPROMT_DIR_FILENAME');
end;

function TDKConsts.SPROMT_LUA_FILENAME: string;
begin
  Result := DKLangConstW('SPROMT_LUA_FILENAME');
end;

function TDKConsts.SPROMT_TXT_FILENAME: string;
begin
  Result := DKLangConstW('SPROMT_TXT_FILENAME');
end;

function TDKConsts.SSEARCH_IN_FILES_LINE: string;
begin
  Result := DKLangConstW('SSEARCH_IN_FILES_LINE');
end;

function TDKConsts.SSEARCH_TREE_HEADER: string;
begin
  Result := DKLangConstW('SSEARCH_TREE_HEADER');
end;

//function TDKConsts.SSTRING_NOT_FOUND_FMT(): string;
//begin
//  Result := DKLangConstW('SSTRING_NOT_FOUND_FMT');
//end;

function TDKConsts.SSTRING_NOT_FOUND_FMT(const Args: array of const): string;
begin
  Result := DKLangConstW('SSTRING_NOT_FOUND_FMT', Args);
end;

function TDKConsts.STREE_INCSEARCH: string;
begin
  Result := DKLangConstW('STREE_INCSEARCH');
end;

function TDKConsts.SUNKNOWN: string;
begin
  Result := DKLangConstW('SUNKNOWN');
end;

function TDKConsts.SVERSION: string;
begin
  Result := DKLangConstW('SVERSION');
end;

function TDKConsts.SWITHOUT_PARAMS: string;
begin
  Result := DKLangConstW('SWITHOUT_PARAMS');
end;



initialization
  DKConsts := TDKConsts.Create;

finalization
  FreeAndNil(DKConsts);

end.
