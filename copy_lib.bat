@echo off

setlocal enabledelayedexpansion

for /F "delims=;" %%e in ("dll;so;dylib") do (
    set l=%cd%\src\runtime\target\release\runtime.%%~e
    if exist !l! (
        echo Found library at '!l!'.
        echo.

        for /D %%a in ("%cd%\.stack-work\install\*") do (
            echo Copying library located at '!l!'
            echo                         to '%%~a\bin'

            copy /y /d "!l!" "%%~a\bin"

            echo.
        )
    )
)