DEL /F /S /Q .\bin\win32.debug\*.exe .\bin\win32.debug\*.dll .\bin\win32.debug\*.ini
rmdir /S /Q .\bin\win32.debug\Dictionaries
rmdir /S /Q .\bin\win32.debug\Languages
rmdir /S /Q .\bin\win32.debug\Libs

DEL /F /S /Q .\bin\win32.release\*.exe .\bin\win32.release\*.dll .\bin\win32.release\*.ini
rmdir /S /Q .\bin\win32.release\Dictionaries
rmdir /S /Q .\bin\win32.release\Languages
rmdir /S /Q .\bin\win32.release\Libs

DEL /F /S /Q .\dcu\win32.debug\*.dcu
DEL /F /S /Q .\dcu\win32.release\*.dcu
