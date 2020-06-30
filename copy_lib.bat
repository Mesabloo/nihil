@echo off

set in=%cd%\src\runtime\target\release\runtime.dll

for /D %%a in ("%cd%\.stack-work\install\*") do (
    echo Copying runtime DLL located at '%in%' to '%%a\bin'...

    copy /y /d "%in%" "%%a\bin"
)