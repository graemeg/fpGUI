#!/bin/sh

fpcbin=fpc
fpctarget=`$fpcbin -iTP`-`$fpcbin -iTO`
#echo $fpctarget
libpath='../lib/'$fpctarget

# Must we create the output directory?
if [ ! -d $libpath ]; then
  echo 'creating directory: '$libpath
  mkdir $libpath
  echo ' '
fi

# LINUX & FREEBSD
#$fpcbin -dX11 @extrafpc.cfg corelib/x11/fpgui_toolkit.pas

# OSX
$fpcbin -dCocoa @extrafpc.cfg corelib/cocoa/fpgui_toolkit.pas
