@echo off

rem We use FPC to found out the Platform and OS to create a lib output path
fpc -Parm -Twince -iTP > tmpvar
set /p myplatform= < tmpvar
fpc -Parm -Twince -iTO > tmpvar
set /p myos= < tmpvar
del tmpvar

if exist ..\lib\%myplatform%-%myos%\nul.x goto exists

echo Creating missing directory ..\lib\%myplatform%-%myos%
mkdir ..\lib\%myplatform%-%myos%
goto end

:exists
echo "You've got the correct output lib directory"

:end

fpc -v0 -dGDI -Twince -Parm @extrafpc.cfg corelib\gdi\fpgui_toolkit.pas

