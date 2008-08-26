./cleanall.sh
cd ../src
./build.sh
cd ../uidesigner
fpc @extrafpc.cfg uidesigner.lpr

