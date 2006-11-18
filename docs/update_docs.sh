#!/bin/sh
# fpc/bin must be in your PATH
makeskel --update --package=fpgui \
  --input='-Fi../src ../src/fpgui.pp' --descr="xml/fpgui.xml" --output="xml/fpgui.new.xml"
