#!/bin/sh

echo "Which platform are you building for?"
echo " "
echo "  L - Linux"
echo "  F - FreeBSD"
echo "  M - Mac OSX"
echo " "

read -p "Enter a letter or Ctrl+C to quit: " OSinput



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


case $OSinput in
	"L"|"l")
		echo "Compiling for Linux"
		$fpcbin -dX11 @extrafpc.cfg corelib/x11/fpgui_toolkit.pas
		;;
	"F"|"f")
		echo "Compiling for FreeBSD"
		$fpcbin -dX11 @extrafpc.cfg corelib/x11/fpgui_toolkit.pas
		;;
	"M"|"m")
		echo "Compiling for OSX Cocoa"
		$fpcbin -dCocoa @extrafpc.cfg corelib/cocoa/fpgui_toolkit.pas
		;;
	*)
		echo "Unknown option - doing nothing!"
		;;
esac

# LINUX & FREEBSD
#$fpcbin -dX11 @extrafpc.cfg corelib/x11/fpgui_toolkit.pas

# OSX
#$fpcbin -dCocoa @extrafpc.cfg corelib/cocoa/fpgui_toolkit.pas
