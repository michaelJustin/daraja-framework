@echo off
rem Legacy entry point - kept for convenience. Prefer run-fpc.cmd, which also
rem builds the Console build mode first.
UnittestsConsole.exe --all --format=plain
exit /b %ERRORLEVEL%
