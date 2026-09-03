@echo off
rem Build and run the unit tests with Free Pascal / Lazarus, headless.
rem
rem   run-fpc.cmd                     runs the whole suite, plain text output
rem   run-fpc.cmd --format=xml --file=results.xml   custom FPCUnit options
rem
rem Set LAZBUILD to your lazbuild.exe if it is not on PATH.
setlocal
if "%LAZBUILD%"=="" set LAZBUILD=lazbuild

"%LAZBUILD%" -B --build-mode=Console "%~dp0Unittests.lpi"
if errorlevel 1 exit /b 1

set ARGS=%*
if "%ARGS%"=="" set ARGS=--all --format=plain

"%~dp0UnittestsConsole.exe" %ARGS%
exit /b %ERRORLEVEL%
