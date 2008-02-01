cd ..
find lib/ examples/ \( -name '*.o' -o -name '*.ppu' -o -name '*.a' \) -exec rm {} \+
cd src
./build.sh
cd ../examples/apps/uidesigner
fpc @extrafpc.cfg uidesigner.lpr

