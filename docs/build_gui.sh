#!/bin/sh
# fpc/bin must be in your PATH
fpdoc --package=fpgui \
  --format=html \
  --output=html/gui/  \
  --content=html/gui.cnt \
  --import=html/gfx.cnt,../gfx/ \
  --descr=xml/gui/fpgui.xml --input='-Fi../gui ../gui/fpgui.pas' \
  --descr=xml/gui/stylemanager.xml --input='-Fi../gui ../gui/stylemanager.pas' \
  --descr=xml/gui/windowsstyle.xml --input='-Fi../gui ../gui/windowsstyle.pas' \
  --descr=xml/gui/motifstyle.xml --input='-Fi../gui ../gui/motifstyle.pas' \
  --descr=xml/gui/opensoftstyle.xml --input='-Fi../gui ../gui/opensoftstyle.pas' 

