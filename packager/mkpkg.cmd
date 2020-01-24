@echo off
set TITLE=Code::Blocks for DreamSDK Packager
title %TITLE%
cls

echo %TITLE%
echo.

set PACKAGE_FILE=codeblocks-17.12-dreamsdk-addon-bin.7z
set PACKAGE_PATH=..\..\cbpatcher\src\engine\embedded\

if exist %PACKAGE_PATH%%PACKAGE_FILE% del %PACKAGE_PATH%%PACKAGE_FILE%

echo Preparing: %PACKAGE_FILE%...

rem This should match the drive specified in:
rem   .\codeblocks\src\plugins\compilergcc\resources\compilers\compiler_dc-gcc.xml
rem   .\codeblocks\src\plugins\compilergcc\resources\compilers\options_dc-gcc.xml
set DREAMSDK_HOME_DEBUG_DRIVE=E:

set BASE_DIR=%~dp0
set BASE_DIR=%BASE_DIR:~0,-1%
set JREPL=%BASE_DIR%\tools\jrepl.bat

set CB_OUTPUT_HOME=..\codeblocks\src\output\
set PACKAGE_DIR=%BASE_DIR%\.package
set CB_SHARE_DIR=%PACKAGE_DIR%\share\CodeBlocks
set CB_SHARE_COMPILERS_DIR=%CB_SHARE_DIR%\compilers
set CB_SHARE_PLUGINS_DIR=%CB_SHARE_DIR%\plugins
set CB_SHARE_TMPL_DIR=%CB_SHARE_DIR%\templates\wizard
set SEVENZIP="C:\Program Files\7-Zip\7z.exe"

:mkdirtree
if not exist %PACKAGE_DIR% mkdir %PACKAGE_DIR%
attrib +h %PACKAGE_DIR%
if not exist %PACKAGE_DIR%\share mkdir %PACKAGE_DIR%\share
if not exist %CB_SHARE_DIR% mkdir %CB_SHARE_DIR%
if not exist %CB_SHARE_COMPILERS_DIR% mkdir %CB_SHARE_COMPILERS_DIR%
if not exist %CB_SHARE_PLUGINS_DIR% mkdir %CB_SHARE_PLUGINS_DIR%
if not exist %CB_SHARE_DIR%\templates mkdir %CB_SHARE_DIR%\templates
if not exist %CB_SHARE_TMPL_DIR% mkdir %CB_SHARE_TMPL_DIR%

:copyfiles
cd %CB_OUTPUT_HOME%

rem SDK
copy *.exe %PACKAGE_DIR% 
copy codeblocks.dll %PACKAGE_DIR%
copy wx*.dll %PACKAGE_DIR%

rem Plugins
copy share\CodeBlocks\compiler.zip %CB_SHARE_DIR%
copy share\CodeBlocks\debugger.zip %CB_SHARE_DIR%
copy share\CodeBlocks\plugins\*.dll %CB_SHARE_PLUGINS_DIR%

rem Compilers
set COMPILER_FILE=share\CodeBlocks\compilers\compiler_dc-gcc.xml
copy %COMPILER_FILE% %CB_SHARE_COMPILERS_DIR%
call %JREPL% "\b%DREAMSDK_HOME_DEBUG_DRIVE%\\DreamSDK\b" "{app}" /f %PACKAGE_DIR%\%COMPILER_FILE% /o -

rem Compilers Options
set OPTIONS_FILE=share\CodeBlocks\compilers\options_dc-gcc.xml
copy %OPTIONS_FILE% %CB_SHARE_COMPILERS_DIR%
call %JREPL% "\b%DREAMSDK_HOME_DEBUG_DRIVE%\\DreamSDK\b" "{app}" /f %PACKAGE_DIR%\%OPTIONS_FILE% /o -
call %JREPL% "\bT%DREAMSDK_HOME_DEBUG_DRIVE%\\DreamSDK\b" "T{app}" /f %PACKAGE_DIR%\%OPTIONS_FILE% /o -

rem Project Wizard
copy share\CodeBlocks\templates\wizard\config.script %CB_SHARE_TMPL_DIR%
xcopy share\CodeBlocks\templates\wizard\dc %CB_SHARE_TMPL_DIR%\dc\ /E

rem Resetting the libinfo directory
set LIBINFO_DIR=%CB_SHARE_TMPL_DIR%\dc\libinfo
if exist %LIBINFO_DIR% (
  rmdir /S %LIBINFO_DIR% /Q
  mkdir %LIBINFO_DIR%
)

:mkpack
cd %PACKAGE_DIR%
%SEVENZIP% a -mx9 %PACKAGE_FILE% .
move %PACKAGE_FILE% %PACKAGE_PATH%
cd ..
rmdir /S %PACKAGE_DIR% /Q
echo.
echo %TITLE% done!
goto end

:end
pause
