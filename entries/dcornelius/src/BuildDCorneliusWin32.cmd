@echo off
echo Build 1BRC Entry for DCornelius - Delphi 12 Athens, Win32, Release mode.
echo Assumes Delphi 12 is in the path and can find the compiler for Win32 (dcc32.exe).
pause

REM RSVars is a batch supplied with Delphi and sets environment variables used in the compilation
call RSVars

REM -$L- : no debug symbols
REM -$Y- : no symbol reference info
REM -B   : build all units
REM -Q   : quiet compile
REM -TX  : set extension
REM -D   : define compiler symbol
REM -E   : output folder
REM -CC  : console target
REM -U   : unit folders
dcc32.exe -$L- -$Y- --no-config -B -Q -TX.exe -DRELEASE -E..\..\..\bin -CC -U"%BDS%\lib\win32\release" dcornelius.dpr 
