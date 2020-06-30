@echo off

setlocal enabledelayedexpansion

set in=%cd%\src\runtime\target\release

for /F "delims=;" %%e in ("dll;so;dylib") do (
    set l=!in!\runtime.%%~e
    if exist !l! (
        call :CP !l!
    )
    set l=!in!\libruntime.%%~e
    if exist !l! (
        call :CP !l!
    )
)
goto END

:CP
set l=%1

echo Found library at '!l!'.
echo.

for /D %%a in ("%cd%\.stack-work\install\*") do (
    echo Copying library located at '!l!'
    echo                         to '%%~a\bin'

    copy /y /d "!l!" "%%~a\bin"

    echo.
)

goto :eof


:END