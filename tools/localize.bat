@echo off
REM
REM Usage: localize.bat
REM
REM This script should be executed after adding new resource strings and after
REM updating the translated .po files.
REM
REM This script
REM - builds the tools if required
REM - converts all compiled .rst files to .po files
REM - updates all translated xx.po files
REM

rem Make sure this is set correctly for your system
set FPCTARGET=i386-win32


if exist updatepofiles.exe goto SkipTools
echo The updatepofiles tool was not found, compiling tools...
echo.
rem build_tools.bat
fpc -O2 -Xs -XX -Sh -FUunits -oupdatepofiles.exe updatepofiles.pas
fpc -O2 -Xs -XX -Sh -FUunits -ogenerateincfiles.exe generateincfiles.pas

if not exist updatepofiles.exe goto Exit_Error

:SkipTools

echo Updating language files...
@set Constants_RST=..\lib\%FPCTARGET%\fpg_constants.rst
rstconv -i %Constants_RST% -o ..\languages\fpgui.po

updatepofiles.exe ..\languages\fpgui.po


REM generate new include files from the updated .po file.
generateincfiles.exe ..

goto Exit


:Exit_Error
echo Unable to compile the updatepofiles tool

:Exit

