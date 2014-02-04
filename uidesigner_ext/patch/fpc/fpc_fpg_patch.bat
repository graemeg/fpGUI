@ECHO OFF
cls
title Patch for fpc.cfg
echo.
echo This is a patch to define fpGUI-path in fpc.cfg config file. 
echo.
echo WARNING: Be sure that the patch is in fpGUI root-directory...
echo.

SET /P x="Do you want to patch fpc.cfg (y/n) ? "%
echo.

if "%x%" == "y" goto GO_PATCHIT
if "%x%" == "Y" goto GO_PATCHIT
if "%x%" == "n" goto GO_END
if "%x%" == "N" goto GO_END

:GO_PATCHIT
echo ------------------------------------------------------------------------
echo.
echo The patch will add fpc-path in Windows PATH Environment Variable
echo              and compile fpc_fpg_patch.pas.
echo.
echo ------------------------------------------------------------------------

SET ifpatch=0

IF EXIST c:\fpc\bin\fpc.exe (
SET ifpatch=1
setx path ";c:\fpc\bin"
echo fpc-path added in Windows PATH Environment Variable
c:\fpc\bin\fpc.exe fpc_fpg_patch.pas
)

if %ifpatch% == 0 (
IF EXIST c:\fpc\2.6.2\bin\i386-win32\fpc.exe (
SET ifpatch=1
setx path ";c:\fpc\2.6.2\bin\i386-win32"
echo fpc-path added in Windows PATH Environment Variable
c:\fpc\2.6.2\bin\i386-win32\fpc.exe fpc_fpg_patch.pas
))

if %ifpatch% == 0 (
IF EXIST c:\fpc\2.6.2\bin\x86_64-win64\fpc.exe (
SET ifpatch=1
setx path ";c:\fpc\2.6.2\bin\x86_64-win64"
echo fpc-path added in Windows PATH Environment Variable
c:\fpc\2.6.2\bin\x86_64-win64\fpc.exe fpc_fpg_patch.pas
))

if %ifpatch% == 0 (
IF EXIST c:\fpc\2.7.1\bin\i386-win32\fpc.exe (
SET ifpatch=1
setx path ";c:\fpc\2.7.1\bin\i386-win32"
echo fpc-path added in Windows PATH Environment Variable
c:\fpc\2.7.1\bin\i386-win32\fpc.exe fpc_fpg_patch.pas
))

if %ifpatch% == 0 (
IF EXIST c:\fpc\2.7.1\bin\x86_64-win64\fpc.exe (
SET ifpatch=1
setx path ";c:\fpc\2.7.1\bin\x86_64-win64"
echo fpc-path added in Windows PATH Environment Variable
c:\fpc\2.7.1\bin\x86_64-win64\fpc.exe fpc_fpg_patch.pas
))

if %ifpatch% == 0 (
IF EXIST c:\fpc\bin\i386-win32\fpc.exe (
SET ifpatch=1
setx path ";c:\fpc\bin\i386-win32"
echo fpc-path added in Windows PATH Environment Variable
c:\fpc\bin\i386-win32\fpc.exe fpc_fpg_patch.pas
))

if %ifpatch% == 0 (
IF EXIST c:\fpc\bin\x86_64-win64\fpc.exe (
SET ifpatch=1
setx path ";c:\fpc\bin\x86_64-win64" 
echo fpc-path added in Windows PATH Environment Variable
c:\fpc\bin\x86_64-win64\fpc.exe fpc_fpg_patch.pas
))

if %ifpatch% == 0 (
IF EXIST c:\freepascal\bin\i386-win32\fpc.exe (
SET ifpatch=1
setx path ";c:\freepascal\bin\i386-win32"
echo fpc-path added in Windows PATH Environment Variable
c:\freepascal\bin\i386-win32\fpc.exe fpc_fpg_patch.pas
))

if %ifpatch% == 0 (
IF EXIST c:\freepascal\bin\x86_64-win64\fpc.exe (
SET ifpatch=1
setx path ";c:\freepascal\bin\x86_64-win64" 
echo fpc-path added in Windows PATH Environment Variable
c:\freepascal\bin\x86_64-win64\fpc.exe fpc_fpg_patch.pas
))

if %ifpatch% == 0 (
IF EXIST c:\freepascal-master\bin\i386-win32\fpc.exe (
SET ifpatch=1
setx path ";c:\freepascal-master\bin\i386-win32"
echo fpc-path added in Windows PATH Environment Variable
c:\freepascal-master\bin\i386-win32\fpc.exe fpc_fpg_patch.pas
))

if %ifpatch% == 0 (
IF EXIST c:\freepascal-master\bin\x86_64-win64\fpc.exe (
SET ifpatch=1
setx path ";c:\freepascal-master\bin\x86_64-win64" 
echo fpc-path added in Windows PATH Environment Variable
c:\freepascal-master\bin\x86_64-win64\fpc.exe fpc_fpg_patch.pas
))

if %ifpatch% == 0 (
IF EXIST C:\codetyphon\fpc\bin\i386-win32\fpc.exe (
SET ifpatch=1
setx path ";C:\codetyphon\fpc\bin\i386-win32" 
echo fpc-path added in Windows PATH Environment Variable
C:\codetyphon\fpc\bin\i386-win32\fpc.exe fpc_fpg_patch.pas
))

if  %ifpatch% == 0 (
IF EXIST C:\codetyphon\fpc\bin\x86_64-win64\fpc.exe (
SET ifpatch=1
setx path ";C:\codetyphon\fpc\bin\x86_64-win64"
echo fpc-path added in Windows PATH Environment Variable
C:\codetyphon\fpc\bin\x86_64-win64\fpc.exe fpc_fpg_patch.pas 
))

if  %ifpatch% == 0 (
IF EXIST C:\lazarus\fpc\2.6.2\bin\i386-win32\fpc.exe (
SET ifpatch=1
setx path ";C:\lazarus\fpc\2.6.2\bin\i386-win32"
echo fpc-path added in Windows PATH Environment Variable
C:\lazarus\fpc\2.6.2\bin\i386-win32\fpc.exe fpc_fpg_patch.pas
))

if  %ifpatch% == 0 (
IF EXIST C:\lazarus\fpc\2.6.2\bin\x86_64-win64\fpc.exe (
SET ifpatch=1
setx path ";C:\lazarus\fpc\2.6.2\bin\x86_64-win64"
echo fpc-path added in Windows PATH Environment Variable
C:\lazarus\fpc\2.6.2\bin\x86_64-win64\fpc.exe fpc_fpg_patch.pas
))

if  %ifpatch% == 0 (
IF EXIST C:\lazarus\fpc\2.7.1\bin\i386-win32\fpc.exe (
SET ifpatch=1
setx path ";C:\lazarus\fpc\2.7.1\bin\i386-win32"
echo fpc-path added in Windows PATH Environment Variable
C:\lazarus\fpc\2.7.1\bin\i386-win32\fpc.exe fpc_fpg_patch.pas)
)
if  %ifpatch% == 0 (
IF EXIST C:\lazarus\fpc\2.7.1\bin\x86_64-win64\fpc.exe (
SET ifpatch=1
setx path ";C:\lazarus\fpc\2.7.1\bin\x86_64-win64"
echo fpc-path added in Windows PATH Environment Variable
C:\lazarus\fpc\2.7.1\bin\x86_64-win64\fpc.exe fpc_fpg_patch.pas)
)
if  %ifpatch% == 0 (
IF EXIST C:\lazarus32\fpc\2.6.2\bin\i386-win32\fpc.exe (
SET ifpatch=1
setx path ";C:\lazarus32\fpc\2.6.2\bin\i386-win32"
echo fpc-path added in Windows PATH Environment Variable
C:\lazarus32\fpc\2.6.2\bin\i386-win32\fpc.exe fpc_fpg_patch.pas)
)
if  %ifpatch% == 0 (
IF EXIST C:\lazarus32\fpc\2.6.2\bin\x86_64-win64\fpc.exe (
SET ifpatch=1
setx path ";C:\lazarus32\fpc\2.6.2\bin\x86_64-win64"
echo fpc-path added in Windows PATH Environment Variable
C:\lazarus32\fpc\2.6.2\bin\x86_64-win64\fpc.exe fpc_fpg_patch.pas)
)
if  %ifpatch% == 0 (
IF EXIST C:\lazarus32\fpc\2.7.1\bin\i386-win32\fpc.exe (
SET ifpatch=1
setx path ";C:\lazarus32\fpc\2.7.1\bin\i386-win32"
echo fpc-path added in Windows PATH Environment Variable
C:\lazarus32\fpc\2.7.1\bin\i386-win32\fpc.exe fpc_fpg_patch.pas)
)
if  %ifpatch% == 0 (
IF EXIST C:\lazarus32\fpc\2.7.1\bin\x86_64-win64\fpc.exe (
SET ifpatch=1
setx path ";C:\lazarus32\fpc\2.7.1\bin\x86_64-win64"
echo fpc-path added in Windows PATH Environment Variable
C:\lazarus32\fpc\2.7.1\bin\x86_64-win64\fpc.exe fpc_fpg_patch.pas)
)
if  %ifpatch% == 0 (
IF EXIST C:\lazarus64\fpc\2.6.2\bin\i386-win32\fpc.exe (
SET ifpatch=1
setx path ";C:\lazarus64\fpc\2.6.2\bin\i386-win32"
echo fpc-path added in Windows PATH Environment Variable
C:\lazarus64\fpc\2.6.2\bin\i386-win32\fpc.exe fpc_fpg_patch.pas)
)
if  %ifpatch% == 0 (
IF EXIST C:\lazarus64\fpc\2.6.2\bin\x86_64-win64\fpc.exe (
SET ifpatch=1
setx path ";C:\lazarus64\fpc\2.6.2\bin\x86_64-win64"
echo fpc-path added in Windows PATH Environment Variable
C:\lazarus64\fpc\2.6.2\bin\x86_64-win64\fpc.exe fpc_fpg_patch.pas)
)
if  %ifpatch% == 0 (
IF EXIST C:\lazarus64\fpc\2.7.1\bin\i386-win32\fpc.exe (
SET ifpatch=1
setx path ";C:\lazarus64\fpc\2.7.1\bin\i386-win32"
echo fpc-path added in Windows PATH Environment Variable
C:\lazarus64\fpc\2.7.1\bin\i386-win32\fpc.exe fpc_fpg_patch.pas)
)
if  %ifpatch% == 0 (
IF EXIST C:\lazarus64\fpc\2.7.1\bin\x86_64-win64\fpc.exe (
SET ifpatch=1
setx path ";C:\lazarus64\fpc\2.7.1\bin\x86_64-win64"
echo fpc-path added in Windows PATH Environment Variable
C:\lazarus64\fpc\2.7.1\bin\x86_64-win64\fpc.exe fpc_fpg_patch.pas)
)
echo.
echo ------------------------------------------------------------------------
echo.
echo Ok, let's run compiled fpc_fpg_patch...
echo.
fpc_fpg_patch.exe 1
echo.
echo ------------------------------------------------------------------------
echo.
del /f fpc_fpg_patch.exe
del /f fpc_fpg_patch.o
echo Patch applied...
echo.
echo ------------------------------------------------------------------------
echo.
echo       Do you want to delete
echo fpc_fpg_patch.pas and fpc_fpg_patch.bat
SET /P x="   in fpGUI-root directory (y/n) ?"%
echo.
if "%x%" == "y" goto GO_DELIT
if "%x%" == "Y" goto GO_DELIT
if "%x%" == "n" goto GO_END
if "%x%" == "N" goto GO_END

:GO_DELIT
echo ------------------------------------------------------------------------
echo.
echo Do not worry about the next message...
echo It is normal, fpc_fpg_patch.bat was deleted...
echo.
del /f fpc_fpg_patch.pas
del /f fpc_fpg_patch.bat

:GO_END
echo ------------------------------------------------------------------------
echo.
echo Exiting...
echo.