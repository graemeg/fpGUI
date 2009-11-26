#!/bin/bash

# NOTE:  Cross compiling is from Linux to Windows 32-bit only

CROSSFPC=/opt/fpc_2.3.1/i386-win32/lib/fpc/2.3.1/ppcross386

#fpctarget=`$CROSSFPC -iTP`-`fpc -iTO`
#echo $fpctarget

#libpath='../lib/'$fpctarget
libpath='../lib/i386-win32'

# Must we create the output directory?
if [ ! -d $libpath ]; then
  echo 'creating directory: '$libpath
  mkdir $libpath
  echo ' '
fi

$CROSSFPC -Twin32 dRELEASE -dGDI @extrafpc.cfg corelib/gdi/fpgui_toolkit.pas
