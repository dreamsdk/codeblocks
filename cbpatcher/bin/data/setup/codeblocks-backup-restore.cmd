@echo off
set TITLE=Code::Blocks Backup/Restore
title %TITLE%
cls

echo %TITLE%
echo.

set BASE_DIR=%~dp0
set BASE_DIR=%BASE_DIR:~0,-1%

set CB_INSTALL_DIR=%ProgramFiles(x86)%\CodeBlocks
set CB_BACKUP_DIR=%BASE_DIR%\backup

rem Files definition
:filesdef
set CB_SHARE_DIR=share\CodeBlocks
set CB_SHARE_PLUGINS_DIR=%CB_SHARE_DIR%\plugins
set CB_SHARE_TEMPLATES_DIR=%CB_SHARE_DIR%\templates\wizard

set CB_SDK_DLL_FILE=codeblocks.dll
set CB_COMPILER_FRM_FILE=compiler.zip
set CB_DEBUGGER_FRM_FILE=debugger.zip
set CB_COMPILER_DLL_FILE=compiler.dll
set CB_DEBUGGER_DLL_FILE=debugger.dll
set CB_CONFIG_FILE=config.script

rem Which operation
if "%1"=="/B" goto backup
if "%1"=="/b" goto backup
if "%1"=="/R" goto restore
if "%1"=="/r" goto restore
goto error

:backup
echo Performing Backup...
if not exist "%CB_BACKUP_DIR%" mkdir "%CB_BACKUP_DIR%"

copy /Y /B "%CB_INSTALL_DIR%\%CB_SDK_DLL_FILE%" "%CB_BACKUP_DIR%"
copy /Y /B "%CB_INSTALL_DIR%\%CB_SHARE_DIR%\%CB_COMPILER_FRM_FILE%" "%CB_BACKUP_DIR%"
copy /Y /B "%CB_INSTALL_DIR%\%CB_SHARE_DIR%\%CB_DEBUGGER_FRM_FILE%" "%CB_BACKUP_DIR%"
copy /Y /B "%CB_INSTALL_DIR%\%CB_SHARE_PLUGINS_DIR%\%CB_COMPILER_DLL_FILE%" "%CB_BACKUP_DIR%"
copy /Y /B "%CB_INSTALL_DIR%\%CB_SHARE_PLUGINS_DIR%\%CB_DEBUGGER_DLL_FILE%" "%CB_BACKUP_DIR%"
copy /Y /B "%CB_INSTALL_DIR%\%CB_SHARE_TEMPLATES_DIR%\%CB_CONFIG_FILE%" "%CB_BACKUP_DIR%"

echo Done!
goto end

:restore
echo Performing Restore...
move /Y "%CB_BACKUP_DIR%\%CB_SDK_DLL_FILE%" "%CB_INSTALL_DIR%"
move /Y "%CB_BACKUP_DIR%\%CB_COMPILER_FRM_FILE%" "%CB_INSTALL_DIR%\%CB_SHARE_DIR%\%CB_COMPILER_FRM_FILE%"
move /Y "%CB_BACKUP_DIR%\%CB_DEBUGGER_FRM_FILE%" "%CB_INSTALL_DIR%\%CB_SHARE_DIR%\%CB_DEBUGGER_FRM_FILE%"
move /Y "%CB_BACKUP_DIR%\%CB_COMPILER_DLL_FILE%" "%CB_INSTALL_DIR%\%CB_SHARE_PLUGINS_DIR%\%CB_COMPILER_DLL_FILE%"
move /Y "%CB_BACKUP_DIR%\%CB_DEBUGGER_DLL_FILE%" "%CB_INSTALL_DIR%\%CB_SHARE_PLUGINS_DIR%\%CB_DEBUGGER_DLL_FILE%"
move /Y "%CB_BACKUP_DIR%\%CB_CONFIG_FILE%" "%CB_INSTALL_DIR%\%CB_SHARE_TEMPLATES_DIR%\%CB_CONFIG_FILE%"
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
echo /B    - Backup Code::Blocks files.
echo /R    - Restore Code::Blocks files.
goto end

:end
