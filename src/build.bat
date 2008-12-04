@echo off
; I don't know how to store the output to a variable:  fpctarget=`fpc -iTP`-`fpc -iTO`
if exist ..\lib\i386-win32\nul.x goto exists
echo "Creating missing directory ..\lib\i386-win32"
mkdir ..\lib\i386-win32
goto end

:exists
echo "You've got the correct output lib directory"

:end

fpc -dRELEASE -dGDI @extrafpc.cfg corelib\gdi\fpgui_toolkit.pas

