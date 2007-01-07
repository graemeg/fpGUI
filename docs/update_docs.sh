#!/bin/sh
# fpc/bin must be in your PATH
makeskel --update --package=fpgui \
  --input='-Fi../gui ../gui/fpgui.pas' --descr="xml/gui/fpgui.xml" --output="xml/gui/fpgui.new.xml" \
  --input='-Fi../gui ../gui/stylemanager.pas' --descr="xml/gui/stylemanager.xml" --output="xml/gui/stylemanager.new.xml"
