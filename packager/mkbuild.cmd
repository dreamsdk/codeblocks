@echo off
set TITLE=Code::Blocks for DreamSDK Packager
title %TITLE%
cls

echo %TITLE%
echo.

set PACKAGE_FILE=codeblocks-17.12-dreamsdk-addon-bin.zip
if exist %PACKAGE_FILE% goto error

echo Preparing: %PACKAGE_FILE%...

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
copy codeblocks.dll %PACKAGE_DIR%

rem Plugins
copy share\CodeBlocks\compiler.zip %CB_SHARE_DIR%
copy share\CodeBlocks\plugins\compiler.dll %CB_SHARE_PLUGINS_DIR%
copy share\CodeBlocks\debugger.zip %CB_SHARE_DIR%
copy share\CodeBlocks\plugins\debugger.dll %CB_SHARE_PLUGINS_DIR%

rem Compilers
set COMPILER_FILE=share\CodeBlocks\compilers\compiler_dc-gcc.xml
copy %COMPILER_FILE% %CB_SHARE_COMPILERS_DIR%
call %JREPL% "\bC:\\DreamSDK\b" "{app}" /f %PACKAGE_DIR%\%COMPILER_FILE% /o -

rem Compilers Options
set OPTIONS_FILE=share\CodeBlocks\compilers\options_dc-gcc.xml
copy %OPTIONS_FILE% %CB_SHARE_COMPILERS_DIR%
call %JREPL% "\bC:\\DreamSDK\b" "{app}" /f %PACKAGE_DIR%\%OPTIONS_FILE% /o -
call %JREPL% "\bTC:\\DreamSDK\b" "T{app}" /f %PACKAGE_DIR%\%OPTIONS_FILE% /o -

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
move %PACKAGE_FILE% ..\
cd ..
rmdir /S %PACKAGE_DIR% /Q
echo.
echo %TITLE% done!
goto end

:error
echo Error: File exists: %PACKAGE_FILE%

:end
pause
