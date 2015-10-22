call "%ProgramFiles%\CodeGear\RAD Studio\5.0\bin\rsvars.bat"
msbuild /target:Build /p:Configuration=Debug source\HoMM5MapScriptsEditor.dproj
