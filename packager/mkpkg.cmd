@echo off
set TITLE=Code::Blocks for DreamSDK Packager
title %TITLE%
cls

echo %TITLE%
echo.

rem Initialize internal stuff
set BASE_DIR=%~dp0
set BASE_DIR=%BASE_DIR:~0,-1%
set PACKAGE_DIR=%BASE_DIR%\.package
set CB_SHARE_DIR=%PACKAGE_DIR%\share\CodeBlocks
set CB_SHARE_COMPILERS_DIR=%CB_SHARE_DIR%\compilers
set CB_SHARE_IMAGES_DIR=%CB_SHARE_DIR%\images
set CB_SHARE_PLUGINS_DIR=%CB_SHARE_DIR%\plugins
set CB_SHARE_TMPL_DIR=%CB_SHARE_DIR%\templates\wizard
set CB_SHARE_TMPL_WIZARD_LIBINFO_DIR=%CB_SHARE_TMPL_DIR%\dc\libinfo
set JREPL=%BASE_DIR%\tools\jrepl.bat

rem Read configuration
set CONFIG_FILE=%BASE_DIR%\mkpkg.ini
for /F "tokens=*" %%i in (%CONFIG_FILE%) do (
	set %%i 2> nul
)

rem Sanitize configuration entries
call :trim TOOLCHAIN32_HOME
call :trim TOOLCHAIN64_HOME
call :trim SEVENZIP
call :trim CB_VERSION
call :trim CB_SOURCE32_DIR_NAME
call :trim CB_SOURCE64_DIR_NAME
call :trim DREAMSDK_HOME_DEBUG_DRIVE
call :trim OUTPUT_DIR

rem Doing some checks
if not exist %TOOLCHAIN32_HOME% goto errenv
if not exist %TOOLCHAIN64_HOME% goto errenv
if not exist %SEVENZIP% goto errenv
if "%CB_VERSION%"=="" goto errenv
if "%CB_SOURCE32_DIR_NAME%"=="" goto errenv
if "%CB_SOURCE64_DIR_NAME%"=="" goto errenv
if not exist %DREAMSDK_HOME_DEBUG_DRIVE% goto errenv

rem Startup!
:start
pushd .

call :makepack 64
call :makepack 86

rem Done!
:finish
popd
echo.
echo ---
echo %TITLE% is done!
echo Check the output directory: %OUTPUT_DIR%
pause
goto :eof

:errenv
echo.
echo There is some configuration errors.
echo Please verify the configuration file.
pause
goto :eof

:makepack
setlocal

set _arch=x%1
set _package_file=codeblocks-%CB_VERSION%-dreamsdk-addon-bin-%_arch%.7z
set _cb_source_dir=%CB_SOURCE32_DIR_NAME%
if "%_arch%"=="x64" set _cb_source_dir=%CB_SOURCE64_DIR_NAME%
set _cb_source_path=..\codeblocks\src\%_cb_source_dir%
set _toolchain=%TOOLCHAIN32_HOME%
if "%_arch%"=="x64" set _toolchain=%TOOLCHAIN64_HOME%
set _strip="%_toolchain%\bin\strip.exe"

if not exist %OUTPUT_DIR% mkdir %OUTPUT_DIR%
if exist %OUTPUT_DIR%%_package_file% del %OUTPUT_DIR%%_package_file%

echo Preparing: %_package_file%...

:makepack_mkdirtree
if not exist %PACKAGE_DIR% mkdir %PACKAGE_DIR%
attrib +h %PACKAGE_DIR%
if not exist %PACKAGE_DIR%\share mkdir %PACKAGE_DIR%\share
if not exist %CB_SHARE_DIR% mkdir %CB_SHARE_DIR%
if not exist %CB_SHARE_COMPILERS_DIR% mkdir %CB_SHARE_COMPILERS_DIR%
if not exist %CB_SHARE_IMAGES_DIR% mkdir %CB_SHARE_IMAGES_DIR%
if not exist %CB_SHARE_PLUGINS_DIR% mkdir %CB_SHARE_PLUGINS_DIR%
if not exist %CB_SHARE_DIR%\templates mkdir %CB_SHARE_DIR%\templates
if not exist %CB_SHARE_TMPL_DIR% mkdir %CB_SHARE_TMPL_DIR%

:makepack_copyfiles
cd %_cb_source_path%

rem Base files
copy *.exe %PACKAGE_DIR% 
copy *.dll %PACKAGE_DIR%

rem Updated plugins
copy share\CodeBlocks\compiler.zip %CB_SHARE_DIR%
copy share\CodeBlocks\debugger.zip %CB_SHARE_DIR%
copy share\CodeBlocks\resources.zip %CB_SHARE_DIR%
copy share\CodeBlocks\start_here.zip %CB_SHARE_DIR%
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

rem Splash file
set SPLASH_FILE=share\CodeBlocks\images\splash_1312.png
copy %SPLASH_FILE% %CB_SHARE_IMAGES_DIR%

rem All files were copied!
cd %PACKAGE_DIR%

:makepack_fixes
rem Resetting the libinfo directory
if exist %CB_SHARE_TMPL_WIZARD_LIBINFO_DIR% (
  rmdir /S %CB_SHARE_TMPL_WIZARD_LIBINFO_DIR% /Q
  mkdir %CB_SHARE_TMPL_WIZARD_LIBINFO_DIR%
)

rem Stripping all produced binaries (except wxMSW)
ren wxmsw*.dll wxmsw*.upx
%_strip% *.exe *.dll
ren wxmsw*.upx wxmsw*.dll
%_strip% share\CodeBlocks\plugins\*.dll

:makepack_create
rem Finally, create the package!
%SEVENZIP% a -mx9 %_package_file% .
move %_package_file% ..\%OUTPUT_DIR%
cd ..
rmdir /S %PACKAGE_DIR% /Q

:makepack_exit
rem Package was created!
endlocal
goto :eof

:trim
rem Thanks to: https://stackoverflow.com/a/19686956/3726096
setlocal EnableDelayedExpansion
call :trimsub %%%1%%
endlocal & set %1=%tempvar%
goto :eof
:trimsub
set tempvar=%*
goto :eof
