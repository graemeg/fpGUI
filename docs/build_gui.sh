#!/bin/sh
# fpc/bin must be in your PATH
fpdoc --package=GUI \
  --format=html \
  --output=html/gui/  \
  --content=html/gui.cnt \
  --html-search=/fpgui/docs/search.html \
  --import=html/corelib.cnt,../corelib/ \
  --descr=xml/gui/fpg_dialogs.xml --input='-Fi../src/gui ../src/gui/fpg_dialogs.pas' \
  --descr=xml/gui/fpg_hyperlink.xml --input='-Fi../src/gui ../src/gui/fpg_hyperlink.pas'


#  --descr=xml/gui/fpg_.xml --input='-Fi../src/gui ../src/gui/fpg_.pas' \

