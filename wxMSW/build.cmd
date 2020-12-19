@echo off

set TITLE=wxWidgets MinGW Builder
title %TITLE%
cls

rem Initialization
set BASE_DIR=%~dp0
set BASE_DIR=%BASE_DIR:~0,-1%
set BACKUP_PATH=%PATH%
set OUTPUT_DIR=%BASE_DIR%\bin
set LOGS_DIR=%BASE_DIR%\logs

rem Read Configuration
set CONFIG_FILE=%BASE_DIR%\build.ini
for /F "tokens=*" %%i in (%CONFIG_FILE%) do (
	set %%i 2> nul
)

rem Sanitize configuration entries
call :trim TOOLCHAIN32_HOME
call :trim TOOLCHAIN64_HOME
call :trim UPX32
call :trim UPX64
call :trim FLAGS
call :trim WINDRES32_FLAGS
call :trim WINDRES64_FLAGS
set UPX32="%UPX32%"
set UPX64="%UPX64%"

rem Doing some checks
if not exist %TOOLCHAIN32_HOME% goto errenv
if not exist %TOOLCHAIN64_HOME% goto errenv
if not exist %UPX32% goto errenv
if not exist %UPX64% goto errenv

rem Startup!
:start
pushd .
cd %BASE_DIR%\src\build\msw\

rem Prepare build
call :cleandir %OUTPUT_DIR%
call :cleandir %LOGS_DIR%

rem Build x64
call :build 64 debug
call :build 64 release

rem Build x86
call :build 86 debug
call :build 86 release

rem Done!
:finish
popd
goto :eof

:errenv
echo.
echo There is some configuration errors.
echo Please verify the configuration file.
pause
goto :eof

:cleandir
setlocal

set _dir=%1
if exist %_dir% rmdir /q /s %_dir%
mkdir %_dir%

endlocal
goto :eof

:build
setlocal

set _arch=x%1
set _build_type=%2
set _debug_flag=0
if "%_build_type%"=="debug" set _debug_flag=1
set _suffix=
if "%_arch%"=="x64" set _suffix=64
set _logfile="%LOGS_DIR%\%_arch%-%_build_type%.log"
set _output_dir=%OUTPUT_DIR%\%_arch%\%_build_type%
mkdir "%_output_dir%"

set _build_ver=wxWidgets for %_arch% (%_build_type%)
echo Building %_build_ver%...
echo --- %_build_ver% --- >> %_logfile% 2>&1

if "%_arch%"=="x64" set PATH=%TOOLCHAIN64_HOME%\bin;%PATH%
if "%_arch%"=="x86" set PATH=%TOOLCHAIN32_HOME%\bin;%PATH%

echo --- Clean --- >> %_logfile% 2>&1
mingw32-make -f makefile.gcc clean >> %_logfile% 2>&1

echo --- Build --- >> %_logfile% 2>&1
if "%_arch%"=="x64" mingw32-make -f makefile.gcc MONOLITHIC=1 SHARED=1 BUILD=%_build_type% DEBUG_FLAG=%_debug_flag% VENDOR=cb CFLAGS="%FLAGS%" CXXFLAGS="%FLAGS%" CPPFLAGS="%FLAGS%" WINDRES="windres %WINDRES64_FLAGS%" CFG=64 >> %_logfile% 2>&1
if "%_arch%"=="x86" mingw32-make -f makefile.gcc MONOLITHIC=1 SHARED=1 BUILD=%_build_type% DEBUG_FLAG=%_debug_flag% VENDOR=custom CFLAGS="%FLAGS% -m32" CXXFLAGS="%FLAGS% -m32" CPPFLAGS="%FLAGS% -m32" LDFLAGS="-m32" WINDRES="windres %WINDRES32_FLAGS% -F pe-i386" >> %_logfile% 2>&1

if "%errorlevel%"=="0" goto build_success
goto build_fail

:build_success
echo --- Install --- >> %_logfile% 2>&1
rem Copy libraries
mkdir %_output_dir%\lib
move %BASE_DIR%\src\lib\gcc_dll%_suffix% %_output_dir%\lib\gcc_dll >> %_logfile% 2>&1

rem Copy binaries
set _output_bin_dir=%_output_dir%\bin
mkdir %_output_bin_dir%
xcopy %_output_dir%\lib\gcc_dll\*.dll %_output_bin_dir% >> %_logfile% 2>&1
if "%_build_type%"=="release" (
	strip %_output_bin_dir%\*.dll >> %_logfile% 2>&1
	if "%_arch%"=="x64" %UPX64% -9 %_output_bin_dir%\*.dll >> %_logfile% 2>&1
	if "%_arch%"=="x86" %UPX32% -9 %_output_bin_dir%\*.dll >> %_logfile% 2>&1
)

rem Copy headers
xcopy %BASE_DIR%\src\include %_output_dir%\include /E /H /C /I >> %_logfile% 2>&1

echo --- End --- >> %_logfile% 2>&1
goto build_exit

:build_fail
echo Build failed. Please check the log file.
pause
goto build_exit

:build_exit
set PATH=%BACKUP_PATH%
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
