#!/bin/sh

# Check if platform argument provided on command line
if [ -n "$1" ]; then
  OSinput="$1"
else
  echo "Which platform are you building for?"
  echo " "
  echo "  L - Linux"
  echo "  1 - Linux + AggCanvas"
  echo "  F - FreeBSD"
  echo "  2 - FreeBSD + AggCanvas"
  echo "  M - Mac OSX"
  echo " "

  read -p "Enter a letter or Ctrl+C to quit: " OSinput
fi



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
		$fpcbin -dX11 @extrafpc.cfg fpgui_unittests_console.lpr
		;;
	"1")
		echo "Compiling for Linux (AggCanvas)"
		$fpcbin -dX11 -dAGGCanvas @extrafpc.cfg fpgui_unittests_console.lpr
		;;
	"F"|"f")
		echo "Compiling for FreeBSD"
		$fpcbin -dX11 @extrafpc.cfg fpgui_unittests_console.lpr
		;;
	"2")
		echo "Compiling for FreeBSD + AggCanvas"
		$fpcbin -dX11 -dAGGCanvas @extrafpc.cfg fpgui_unittests_console.lpr
		;;
    "M"|"m")
		echo "Compiling for OSX Cocoa"
		$fpcbin -dX11 -dAGGCanvas @extrafpc.cfg fpgui_unittests_console.lpr
		;;
	*)
		echo "Unknown option - doing nothing!"
		;;
esac

# LINUX & FREEBSD
#$fpcbin -dX11 @extrafpc.cfg fpgui_unittests_console.lpr

# OSX
#$fpcbin -dCocoa @extrafpc.cfg fpgui_unittests_console.lpr
