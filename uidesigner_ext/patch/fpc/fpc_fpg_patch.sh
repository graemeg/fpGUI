#!/bin/bash

patchit() 
{

echo "-------------------------------------------------------------"
echo "The patch will compile fpc_fpg_patch.pas"
echo "-------------------------------------------------------------"
fpc fpc_fpg_patch.pas
echo "----------------------------------------------"

echo "Ok, let's run compiled fpc_fpg_patch..."
echo "-------------------------------------------------------------"
sudo ./fpc_fpg_patch 1
echo "-------------------------------------------------------------"
sudo rm fpc_fpg_patch.o
sudo rm fpc_fpg_patch

echo ""
echo "-------------------------------------------------------------"
echo "      Do you want to delete"
echo "fpc_fpg_patch.pas and fpc_fpg_patch.sh"
echo "   in fpGUI-root directory (y/n) ? "

read var_read
case $var_read in

#------------------- OK, delete it -------------------
y)  
sudo rm fpc_fpg_patch.pas
sudo rm fpc_fpg_patch.sh

;;
Y)
sudo rm fpc_fpg_patch.pas
sudo rm fpc_fpg_patch.sh
;;
#------------------- else exit -------------------
*) 
echo "Exiting..."
exit
;;
esac 
}

# =================== Init =============================

echo "-------------------------------------------------------------"
echo "This is a patch to define fpGUI path in fpc.cfg config file." 
echo ""
echo "WARNING: Be sure that the patch is in fpGUI root-directory..." 
echo ""
echo -n "Do you want to patch fpc.cfg (y/n) ? "

read var_read
case $var_read in

#------------------- OK, patch it -------------------
y)  patchit
;;
Y)  patchit
;;
# ------------Exiting----------------------------
*) 
echo "Exiting..."
exit
;;

esac
# ------------------------------------------------
echo " "




