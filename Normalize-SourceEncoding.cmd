@echo off
rem Double-click to normalise source files (UTF-8 BOM + CRLF) in this folder and
rem all subfolders, skipping thirdparty / 3rdparty. You can also drag a folder
rem onto this file to normalise that folder instead.
setlocal

set "SCRIPT=%~dp0Normalize-SourceEncoding.ps1"

if "%~1"=="" (
    set "TARGET=%~dp0."
) else (
    set "TARGET=%~1"
)

rem Prefer PowerShell 7 (pwsh); fall back to Windows PowerShell 5.1.
where pwsh >nul 2>nul && (set "PS=pwsh") || (set "PS=powershell")

"%PS%" -NoProfile -ExecutionPolicy Bypass -File "%SCRIPT%" -Root "%TARGET%"

echo.
pause
endlocal
