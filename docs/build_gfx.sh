#!/bin/sh
# fpc/bin must be in your PATH
#   --html-search=../search.html \
fpdoc --package=CoreLib \
  --format=html \
  --output=html/corelib/  \
  --content=html/corelib.cnt \
  --html-search=/fpgui/docs/search.html \
  --import=html/gui.cnt,../gui/ \
  --input='-Fi../src/corelib ../src/corelib/gfxbase.pas' --descr=xml/corelib/gfxbase.xml \
  --input='-Fi../src/corelib ../src/corelib/x11/gfx_x11.pas' --descr=xml/corelib/x11/gfx_x11.xml \
  --input='-Fi../src/corelib ../src/corelib/gdi/gfx_gdi.pas' --descr=xml/corelib/gdi/gfx_gdi.xml \
  --input='-Fi../src/corelib ../src/corelib/fpgfx.pas' --descr=xml/corelib/fpgfx.xml \
  --input='-Fi../src/corelib ../src/corelib/gfx_cmdlineparams.pas' --descr=xml/corelib/gfx_cmdlineparams.xml \
  --input='-Fi../src/corelib ../src/corelib/gfx_extinterpolation.pas' --descr=xml/corelib/gfx_extinterpolation.xml \
  --input='-Fi../src/corelib ../src/corelib/gfx_imgfmt_bmp.pas' --descr=xml/corelib/gfx_imgfmt_bmp.xml \
  --input='-Fi../src/corelib ../src/corelib/gfx_stdimages.pas' --descr=xml/corelib/gfx_stdimages.xml \
  --input='-Fi../src/corelib ../src/corelib/gfx_utf8utils.pas' --descr=xml/corelib/gfx_utf8utils.xml \
  --input='-Fi../src/corelib ../src/corelib/gfx_widget.pas' --descr=xml/corelib/gfx_widget.xml \
  --input='-Fi../src/corelib ../src/corelib/gfx_utils.pas' --descr=xml/corelib/gfx_utils.xml \
  --input='-Fi../src/corelib ../src/corelib/gfx_popupwindow.pas' --descr=xml/corelib/gfx_popupwindow.xml \
  --input='-Fi../src/corelib ../src/corelib/gfx_wuline.pas' --descr=xml/corelib/gfx_wuline.xml \
  --input='-Fi../src/corelib ../src/corelib/gfx_imagelist.pas' --descr=xml/corelib/gfx_imagelist.xml \
  --input='-Fi../src/corelib ../src/corelib/gfx_constants.pas' --descr=xml/corelib/gfx_constants.xml \
  --input='-Fi../src/corelib ../src/corelib/gfx_pofiles.pas' --descr=xml/corelib/gfx_pofiles.xml \
  --input='-Fi../src/corelib ../src/corelib/gfx_translations.pas' --descr=xml/corelib/gfx_translations.xml \
  --input='-Fi../src/corelib ../src/corelib/gfx_stringhashlist.pas' --descr=xml/corelib/gfx_stringhashlist.xml 


#  --input='-Fi../src/corelib ../src/corelib/x11/gfx_.pas' --descr=xml/corelib/gfx_.xml \

