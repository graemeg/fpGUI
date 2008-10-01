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

fpc -dRELEASE -dX11 @extrafpc.cfg corelib/x11/fpgui_toolkit.pas

