@echo off
set TITLE=Code::Blocks Backup/Restore
title %TITLE%
cls

echo %TITLE%
echo.

set BASE_DIR=%~dp0
set BASE_DIR=%BASE_DIR:~0,-1%

rem Checking parameters
if "%~1"=="" goto error
if "%~2"=="" goto error
if "%~3"=="" goto error

rem Assigning parameters
set OPERATION=%~1
set CB_INSTALL_DIR=%~2
set CB_BACKUP_DIR=%~3

rem If the source directory doesn't exists, then stop.
if not exist "%CB_INSTALL_DIR%" goto error

rem Files definition
:filesdef
set CB_SHARE_DIR=share\CodeBlocks
set CB_SHARE_PLUGINS_DIR=%CB_SHARE_DIR%\plugins
set CB_SHARE_TEMPLATES_DIR=%CB_SHARE_DIR%\templates\wizard

set CB_EXE_FILE=codeblocks.exe
set CB_SDK_DLL_FILE=codeblocks.dll
set CB_COMPILER_FRM_FILE=compiler.zip
set CB_DEBUGGER_FRM_FILE=debugger.zip
set CB_CONFIG_FILE=config.script

rem Which operation
if "%OPERATION%"=="/B" goto backup
if "%OPERATION%"=="/b" goto backup
if "%OPERATION%"=="/R" goto restore
if "%OPERATION%"=="/r" goto restore
goto error

:backup
echo Performing Backup...

if not exist "%CB_BACKUP_DIR%" mkdir "%CB_BACKUP_DIR%"
if not exist "%CB_BACKUP_DIR%\%CB_SHARE_DIR%" mkdir "%CB_BACKUP_DIR%\%CB_SHARE_DIR%"
if not exist "%CB_BACKUP_DIR%\%CB_SHARE_PLUGINS_DIR%" mkdir "%CB_BACKUP_DIR%\%CB_SHARE_PLUGINS_DIR%"
if not exist "%CB_BACKUP_DIR%\%CB_SHARE_TEMPLATES_DIR%" mkdir "%CB_BACKUP_DIR%\%CB_SHARE_TEMPLATES_DIR%"

copy /Y /B "%CB_INSTALL_DIR%\%CB_EXE_FILE%" "%CB_BACKUP_DIR%"
copy /Y /B "%CB_INSTALL_DIR%\%CB_SDK_DLL_FILE%" "%CB_BACKUP_DIR%"
copy /Y /B "%CB_INSTALL_DIR%\%CB_SHARE_DIR%\%CB_COMPILER_FRM_FILE%" "%CB_BACKUP_DIR%\%CB_SHARE_DIR%"
copy /Y /B "%CB_INSTALL_DIR%\%CB_SHARE_DIR%\%CB_DEBUGGER_FRM_FILE%" "%CB_BACKUP_DIR%\%CB_SHARE_DIR%"
copy /Y /B "%CB_INSTALL_DIR%\%CB_SHARE_PLUGINS_DIR%\*.dll" "%CB_BACKUP_DIR%\%CB_SHARE_PLUGINS_DIR%"
copy /Y /B "%CB_INSTALL_DIR%\%CB_SHARE_TEMPLATES_DIR%\%CB_CONFIG_FILE%" "%CB_BACKUP_DIR%\%CB_SHARE_TEMPLATES_DIR%"

echo Done!
goto end

:restore
echo Performing Restore...
move /Y "%CB_BACKUP_DIR%\%CB_EXE_FILE%" "%CB_INSTALL_DIR%"
move /Y "%CB_BACKUP_DIR%\%CB_SDK_DLL_FILE%" "%CB_INSTALL_DIR%"
move /Y "%CB_BACKUP_DIR%\%CB_SHARE_DIR%\%CB_COMPILER_FRM_FILE%" "%CB_INSTALL_DIR%\%CB_SHARE_DIR%\%CB_COMPILER_FRM_FILE%"
move /Y "%CB_BACKUP_DIR%\%CB_SHARE_DIR%\%CB_DEBUGGER_FRM_FILE%" "%CB_INSTALL_DIR%\%CB_SHARE_DIR%\%CB_DEBUGGER_FRM_FILE%"
move /Y "%CB_BACKUP_DIR%\%CB_SHARE_PLUGINS_DIR%\*.dll" "%CB_INSTALL_DIR%\%CB_SHARE_PLUGINS_DIR%"
move /Y "%CB_INSTALL_DIR%\%CB_SHARE_TEMPLATES_DIR%\%CB_CONFIG_FILE%" "%CB_INSTALL_DIR%\%CB_SHARE_TEMPLATES_DIR%\%CB_CONFIG_FILE%"
if exist "%CB_BACKUP_DIR%" rmdir /S "%CB_BACKUP_DIR%" /Q

set COMPILER_FILE=share\CodeBlocks\compilers\compiler_dc-gcc.xml
if exist "%CB_INSTALL_DIR%\%COMPILER_FILE%" del "%CB_INSTALL_DIR%\%COMPILER_FILE%"

set OPTIONS_FILE=share\CodeBlocks\compilers\options_dc-gcc.xml
if exist "%CB_INSTALL_DIR%\%OPTIONS_FILE%" del "%CB_INSTALL_DIR%\%OPTIONS_FILE%"

set WIZARD_DC_DIR=share\CodeBlocks\templates\wizard\dc
if exist "%CB_INSTALL_DIR%\%WIZARD_DC_DIR%" rmdir /S "%CB_INSTALL_DIR%\%WIZARD_DC_DIR%" /Q

echo Done!
goto end

:error
echo Usage: %~n0 ^<OPERATION^> ^<CB_INSTALL_DIR^> ^<CB_BACKUP_DIR^> 
echo.
echo Operation can be:
echo   /B    - Backup Code::Blocks files.
echo   /R    - Restore Code::Blocks files.
goto end

:end
