#!/bin/bash

fpctarget=`fpc -iTP`-`fpc -iTO`
#echo $fpctarget
libpath='../lib/'$fpctarget

# Must we create the output directory?
if [ ! -d $libpath ]; then
  echo 'creating directory: '$libpath
  mkdir $libpath
  echo ' '
fi

# Default build
fpc -dRELEASE -dX11 @extrafpc.cfg corelib/x11/fpgui_toolkit.pas
# experimental AggPas-enabled Canvas under X11
#fpc -dRELEASE -dX11 -dAGGCanvas @extrafpc.cfg corelib/x11/fpgui_toolkit.pas

