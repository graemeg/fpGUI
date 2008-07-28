if not exist updatestdimgs.exe fpc -O2 -Xs -XX -Sh -FUunits -oupdatestdimgs.exe updatestdimgs.pas
if exist updatestdimgs.exe updatestdimgs.exe --prefix=stdimg > ../src/corelib/stdimages.inc
pause
