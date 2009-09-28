#!/bin/sh
# fpc/bin must be in your PATH
fpdoc --package=GUI \
  --format=html \
  --output=html/gui/  \
  --content=html/gui.cnt \
  --charset=UTF-8 \
  --footer-date='yyyy-mm-dd' \
  --html-search=/fpgui/docs/search.html \
  --import=html/corelib.cnt,../corelib/ \
  --descr=xml/gui/fpg_dialogs.xml --input='-Fi../src/gui ../src/gui/fpg_dialogs.pas' \
  --descr=xml/gui/fpg_hyperlink.xml --input='-Fi../src/gui ../src/gui/fpg_hyperlink.pas' \
  --descr=xml/gui/fpg_colormapping.xml --input='-Fi../src/gui ../src/gui/fpg_colormapping.pas' \
  --descr=xml/gui/fpg_colorwheel.xml --input='-Fi../src/gui ../src/gui/fpg_colorwheel.pas'


#  --descr=xml/gui/fpg_.xml --input='-Fi../src/gui ../src/gui/fpg_.pas' \

