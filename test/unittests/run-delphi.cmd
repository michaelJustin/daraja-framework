@echo off
rem Build and run the unit tests with Delphi (dcc32), headless (DUnit text runner).
rem
rem   run-delphi.cmd            runs the whole suite
rem   run-delphi.cmd -text-mode extra switches are forwarded to the runner
rem
rem Override these if your installation differs:
rem   BDS   - RAD Studio / Delphi root (contains bin\dcc32.exe and lib\)
rem   INDY  - Indy "Lib" directory (Core, Protocols, System)
setlocal
if "%BDS%"=="" set "BDS=C:\Program Files (x86)\CodeGear\RAD Studio\6.0"
if "%INDY%"=="" set "INDY=%~dp0..\..\..\Indy\Lib"

set "DCU=%~dp0lib\delphi"
if not exist "%DCU%" mkdir "%DCU%"

"%BDS%\bin\dcc32.exe" -B -Q ^
  "-U%BDS%\lib;%INDY%\Core;%INDY%\Protocols;%INDY%\System;%~dp0..\..\source;%~dp0..\..\source\optional;%~dp0..\..\..\slf4p\src\main" ^
  "-I%INDY%\Core;%INDY%\System" ^
  "-N0%DCU%" "-E%~dp0." "%~dp0Unittests.dpr"
if errorlevel 1 exit /b 1

set ARGS=%*
if "%ARGS%"=="" set ARGS=-text-mode

"%~dp0Unittests.exe" %ARGS%
exit /b %ERRORLEVEL%
