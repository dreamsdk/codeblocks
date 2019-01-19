@echo off
title Code::Blocks for DreamSDK Packager
cls

set BASE_DIR=%~dp0
set BASE_DIR=%BASE_DIR:~0,-1%

set PACKAGE_FILE=codeblocks_dreamsdk_addon.7z
set PACKAGE_DONE_DIR=__DONE__

if exist %BASE_DIR%\%PACKAGE_DONE_DIR% goto error
if exist %PACKAGE_FILE% goto error

set CB_OUTPUT_HOME=..\codeblocks\src\output\
set PACKAGE_DIR=%BASE_DIR%\package
set CB_SHARE_DIR=%PACKAGE_DIR%\share\CodeBlocks
set CB_SHARE_COMPILERS_DIR=%CB_SHARE_DIR%\compilers
set CB_SHARE_PLUGINS_DIR=%CB_SHARE_DIR%\plugins
set CB_SHARE_TMPL_DIR=%CB_SHARE_DIR%\templates\wizard
set SEVENZIP="C:\Program Files\7-Zip\7z.exe"

:mkdirtree
if not exist %PACKAGE_DIR% mkdir %PACKAGE_DIR%
if not exist %PACKAGE_DIR%\share mkdir %PACKAGE_DIR%\share
if not exist %CB_SHARE_DIR% mkdir %CB_SHARE_DIR%
if not exist %CB_SHARE_COMPILERS_DIR% mkdir %CB_SHARE_COMPILERS_DIR%
if not exist %CB_SHARE_PLUGINS_DIR% mkdir %CB_SHARE_PLUGINS_DIR%
if not exist %CB_SHARE_DIR%\templates mkdir %CB_SHARE_DIR%\templates
if not exist %CB_SHARE_TMPL_DIR% mkdir %CB_SHARE_TMPL_DIR%

:copyfiles
cd %CB_OUTPUT_HOME%

rem SDK
copy codeblocks.dll %PACKAGE_DIR%

rem Plugins
copy share\CodeBlocks\compiler.zip %CB_SHARE_DIR%
copy share\CodeBlocks\plugins\compiler.dll %CB_SHARE_PLUGINS_DIR%
copy share\CodeBlocks\debugger.zip %CB_SHARE_DIR%
copy share\CodeBlocks\plugins\debugger.dll %CB_SHARE_PLUGINS_DIR%

rem Compilers
copy share\CodeBlocks\compilers\compiler_dc-gcc.xml %CB_SHARE_COMPILERS_DIR%
copy share\CodeBlocks\compilers\options_dc-gcc.xml %CB_SHARE_COMPILERS_DIR%

rem Project Wizard
copy share\CodeBlocks\templates\wizard\config.script %CB_SHARE_TMPL_DIR%
xcopy share\CodeBlocks\templates\wizard\dc %CB_SHARE_TMPL_DIR%\dc\ /E
if exist %CB_SHARE_TMPL_DIR%\dc\libinfo\.gitkeep del %CB_SHARE_TMPL_DIR%\dc\libinfo\.gitkeep

:mkpack
cd %PACKAGE_DIR%
%SEVENZIP% a -mx9 %PACKAGE_FILE% .
move %PACKAGE_FILE% ..\
cd ..
ren %PACKAGE_DIR% %PACKAGE_DONE_DIR%
goto end

:error
echo Please delete the %PACKAGE_DONE_DIR% and %PACKAGE_FILE% objects.

:end
pause
