call "%ProgramFiles%\CodeGear\RAD Studio\5.0\bin\rsvars.bat"
msbuild /target:Build /p:Configuration=Release source\HoMM5MapScriptsEditor.dproj
