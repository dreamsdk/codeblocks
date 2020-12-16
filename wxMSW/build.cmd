@echo off

set TITLE=wxWidgets MinGW Builder
title %TITLE%
echo %TITLE%

set TOOLCHAIN_HOME=C:\Nuwen
if not exist %TOOLCHAIN_HOME% goto errenv

set PATH=%TOOLCHAIN_HOME%\bin;%PATH%

set SCRIPT_PATH=%~dp0
set SCRIPT_PATH=%SCRIPT_PATH:~0,-1%

rem -D_WIN32_IE=0x0603
set FLAGS=-fno-keep-inline-dllexport -Wno-unused-local-typedefs -Wno-deprecated-declarations

set OUTPUT_DIR=%SCRIPT_PATH%\bin
set LOGS_DIR=%SCRIPT_PATH%\logs

pushd
cd %SCRIPT_PATH%\src\build\msw\

rem Prepare build
call :cleandir %OUTPUT_DIR%
call :cleandir %LOGS_DIR%

rem Build x64
call :build 64 debug
call :build 64 release

rem Build x86
call :build 86 debug
call :build 86 release

popd
goto :eof

:errenv
echo.
echo The MinGW toolchain was not found.
echo Please install it and/or update the TOOLCHAIN_HOME variable in this script.
pause
goto :eof

:cleandir
setlocal

set _dir=%1
if exist %_dir% rmdir /q /s %_dir%
mkdir %_dir%

endlocal
goto:eof

:build
setlocal

set _arch=x%1
set _build_type=%2
set _debug_flag=0
if "%_build_type%"=="debug" set _debug_flag=1
set _suffix=
if "%_arch%"=="x64" set _suffix=64
set _logfile=%LOGS_DIR%\%_arch%-%_build_type%.log
set _output_dir=%OUTPUT_DIR%\%_build_type%\%_arch%
mkdir %_output_dir%

set _build_ver=wxWidgets for %_arch% (%_build_type%)
echo Building %_build_ver%...
echo --- %_build_ver% --- >> %_logfile% 2>&1

echo --- Clean --- >> %_logfile% 2>&1
mingw32-make -f makefile.gcc clean >> %_logfile% 2>&1

echo --- Build --- >> %_logfile% 2>&1
if "%_arch%"=="x64" mingw32-make -f makefile.gcc MONOLITHIC=1 SHARED=1 BUILD=%_build_type% DEBUG_FLAG=%_debug_flag% VENDOR=cb CFLAGS="%FLAGS%" CXXFLAGS="%FLAGS%" CPPFLAGS="%FLAGS%" CFG=64 >> %_logfile% 2>&1
if "%_arch%"=="x86" mingw32-make -f makefile.gcc MONOLITHIC=1 SHARED=1 BUILD=%_build_type% DEBUG_FLAG=%_debug_flag% VENDOR=custom CFLAGS="%FLAGS% -m32" CXXFLAGS="%FLAGS% -m32" CPPFLAGS="%FLAGS% -m32" LDFLAGS="-m32" WINDRES="windres --use-temp-file -F pe-i386" >> %_logfile% 2>&1

echo --- Install --- >> %_logfile% 2>&1
mkdir %_output_dir%\lib
move %SCRIPT_PATH%\src\lib\gcc_dll%_suffix% %_output_dir%\lib\gcc_dll >> %_logfile% 2>&1
mkdir %_output_dir%\bin
xcopy %_output_dir%\lib\gcc_dll\*.dll %_output_dir%\bin >> %_logfile% 2>&1
xcopy %SCRIPT_PATH%\src\include %_output_dir%\include /E /H /C /I >> %_logfile% 2>&1

echo --- End --- >> %_logfile% 2>&1

endlocal
goto:eof
