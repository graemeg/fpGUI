#!/bin/sh
# fpc/bin must be in your PATH
fpdoc --package=fpgfx \
  --format=html \
  --output=html/gfx/  \
  --content=html/gfx.cnt \
  --import=html/gui.cnt,../gui/ \
  --input='-Fi../gfx ../gfx/fpgfx.pas' --descr=xml/gfx/fpgfx.xml \
  --input='-Fi../gfx ../gfx/gfxbase.pas' --descr=xml/gfx/gfxbase.xml \
  --input='-Fi../gfx ../gfx/geldirty.pas' --descr=xml/gfx/geldirty.xml \
  --input='-Fi../gfx ../gfx/gelimage.pas' --descr=xml/gfx/gelimage.xml \
  --input='-Fi../gfx ../gfx/x11/gfxinterface.pas' --descr=xml/gfx/gfxinterface.xml \
  --input='-Fi../gfx ../gfx/x11/gfx_x11.pas' --descr=xml/gfx/gfx_x11.xml \
  --input='-Fi../gfx ../gfx/x11/unitxft.pas' --descr=xml/gfx/unitxft.xml
