@echo off
set TITLE=Code::Blocks Patcher Release Maker
title %TITLE%
cls

echo %TITLE%
echo.

set BASE_DIR=%~dp0
set BASE_DIR=%BASE_DIR:~0,-1%

set SOURCE_DIR=%BASE_DIR%\..\cbpatcher\bin
set DESTINATION_DIR=%BASE_DIR%\dist

set CB_PATCHER_FILE=%SOURCE_DIR%\codeblocks-patcher.exe
if not exist %CB_PATCHER_FILE% goto error

set CB_SPLASH_FILE=%SOURCE_DIR%\codeblocks-splash.exe
if not exist %CB_SPLASH_FILE% goto error

:release
if not exist %DESTINATION_DIR% mkdir %DESTINATION_DIR%
xcopy %SOURCE_DIR%\data %DESTINATION_DIR%\data\ /E
copy %CB_PATCHER_FILE% %DESTINATION_DIR%\
copy %CB_SPLASH_FILE% %DESTINATION_DIR%\
copy %SOURCE_DIR%\codeblocks-backup-restore.cmd %DESTINATION_DIR%

:clean
set GITKEEP_FILE=%DESTINATION_DIR%\data\package\.gitkeep
if exist %GITKEEP_FILE% del %GITKEEP_FILE%
goto end

:error
echo Please Build the Code::Blocks Patcher/Splash binaries first.
goto end

:end
pause
