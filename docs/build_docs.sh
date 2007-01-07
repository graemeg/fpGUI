#!/bin/sh
# fpc/bin must be in your PATH
fpdoc --package=fpgui --format=html --output=html/  \
  --descr=xml/gui/fpgui.xml --input='-Fi../gui ../gui/fpgui.pas' \
  --descr=xml/gui/stylemanager.xml --input='-Fi../gui ../gui/stylemanager.pas'

