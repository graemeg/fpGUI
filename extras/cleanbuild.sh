./cleanall.sh
cd ../src
./build.sh
cd ../uidesigner
fpc @extrafpc.cfg uidesigner.lpr
cd ../docview/src
fpc @extrafpc.cfg docview.lpr
cd ../..


