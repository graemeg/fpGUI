#!/bin/bash
###########################################################################
# NOTE:  
#  Cross compiling is from Linux 64-bit to Linux 32-bit only.
#
#  This is really only for my testing purposes so I can quickly test 
#  other platforms and targets
#
###########################################################################

CROSSFPC=/opt/fpc_2.4.1/i386-linux/bin/fpc

#fpctarget=`$CROSSFPC -iTP`-`fpc -iTO`
#echo $fpctarget

#libpath='../lib/'$fpctarget
unitpath='../lib/i386-linux'
# Must we create the output directory?
if [ ! -d $unitpath ]; then
  echo 'creating directory: '$unitpath
  mkdir $unitpath
  echo ' '
fi
# compile fpGUI Toolkit itself
echo 'compiling fpGUI Toolkit library'
$CROSSFPC -Tlinux -Pi386 -dRELEASE -dX11 @extrafpc.cfg corelib/x11/fpgui_toolkit.pas
echo ' '

unitpath='../docview/src/units/i386-linux'
# Must we create the output directory for DocView?
if [ ! -d $unitpath ]; then
  echo 'creating directory: '$unitpath
  mkdir $unitpath
  echo ' '
fi
# compile the DocView (documentation viewer) application
echo 'compiling DocView'
$CROSSFPC -Tlinux -Pi386 -dRELEASE -dX11 @extrafpc.cfg -Fu../docview/components/richtext/ -FE$unitpath ../docview/src/docview.lpr
echo ' '

unitpath='../uidesigner/units/i386-linux'
# Must we create the output directory for DocView?
if [ ! -d $unitpath ]; then
  echo 'creating directory: '$unitpath
  mkdir $unitpath
  echo ' '
fi
# compile the UI Designer (visual form designer) application
echo 'compiling UIDesigner'
$CROSSFPC -Tlinux -Pi386 -dRELEASE -dX11 @extrafpc.cfg -FE$unitpath ../uidesigner/uidesigner.lpr
echo ' '
