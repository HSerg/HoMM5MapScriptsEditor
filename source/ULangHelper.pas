// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit ULangHelper;

interface

uses
  Windows, SysUtils, JclSysInfo, JclCOM, JclFileUtils, JclRegistry;

function GetOSNativeLANGID: LANGID;

implementation

function GetOSNativeLANGID: LANGID;
const
  FUNCTION_NAME = 'GetSystemDefaultUILanguage';
var
  xHandle: HMODULE;
  lpFunction: function: LANGID; stdcall;
begin
  if not IsWinNT then
    Result := StrToInt('$' + RegReadString(HKCU, 'Control Panel\desktop\ResourceLocale', ''))
  else
  if IsWinNT4 then
    Result := StrToInt('$' + RegReadString(HKUS, '.DEFAULT\Control Panel\International', 'Locale'))
  else
    begin
      xHandle := LoadLibrary(PAnsiChar('kernel32'));
      try
        lpFunction := GetProcAddress(xHandle, PAnsiChar(FUNCTION_NAME));
        if Assigned(lpFunction) then
          Result := lpFunction()
        else
          Result := 0;
      finally
        FreeLibrary(xHandle);
      end;
    end;
end;

end.
