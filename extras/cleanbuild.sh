./cleanall.sh
cd src
./build.sh
cd ../examples/apps/uidesigner
fpc @extrafpc.cfg uidesigner.lpr

