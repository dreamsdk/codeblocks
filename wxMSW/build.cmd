@echo off

set TITLE=wxWidgets MinGW-w64 Builder
title %TITLE%
echo %TITLE%

set TDM_GCC_64_HOME=C:\TDM-GCC-64

set PATH=%TDM_GCC_64_HOME%\bin;%PATH%

set SCRIPT_PATH=%~dp0
set SCRIPT_PATH=%SCRIPT_PATH:~0,-1%

rem -D_WIN32_IE=0x0603
set FLAGS=-fno-keep-inline-dllexport -Wno-unused-local-typedefs -Wno-deprecated-declarations

set OUTPUT_DIR=%SCRIPT_PATH%\bin
set LOGS_DIR=%SCRIPT_PATH%\logs

pushd
cd %SCRIPT_PATH%\src\build\msw\

call :cleandir %OUTPUT_DIR%
call :cleandir %LOGS_DIR%

call :build 86
call :build 64

popd
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
set _suffix=
if "%_arch%"=="x64" set _suffix=64
set _logfile=%LOGS_DIR%\%_arch%-release.log
set _output_dir=%OUTPUT_DIR%\%_arch%

echo Building %_arch%...

echo --- Clean --- >> %_logfile% 2>&1
mingw32-make -f makefile.gcc clean >> %_logfile% 2>&1

echo --- Build --- >> %_logfile% 2>&1
if "%_arch%"=="x64" mingw32-make -f makefile.gcc MONOLITHIC=1 SHARED=1 BUILD=release VENDOR=cb CFLAGS="%FLAGS%" CXXFLAGS="%FLAGS%" CPPFLAGS="%FLAGS%" CFG=64 >> %_logfile% 2>&1
if "%_arch%"=="x86" mingw32-make -f makefile.gcc MONOLITHIC=1 SHARED=1 BUILD=release VENDOR=custom CFLAGS="%FLAGS% -m32" CXXFLAGS="%FLAGS% -m32" CPPFLAGS="%FLAGS% -m32" LDFLAGS="-m32" WINDRES="windres --use-temp-file -F pe-i386" >> %_logfile% 2>&1

echo --- Install --- >> %_logfile% 2>&1
mkdir %_output_dir%\lib
move %SCRIPT_PATH%\src\lib\gcc_dll%_suffix% %_output_dir%\lib\gcc_dll >> %_logfile% 2>&1
xcopy %SCRIPT_PATH%\src\include %_output_dir%\include /E /H /C /I >> %_logfile% 2>&1

endlocal
goto:eof
