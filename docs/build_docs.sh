#!/bin/sh
# fpc/bin must be in your PATH
fpdoc --package=fpgui --format=html --output=html/  \
  --descr=xml/fpgui.xml --input='-Fi../src ../src/fpgui.pp'

