#!/bin/sh
# fpc/bin must be in your PATH
#   --html-search=../search.html \
fpdoc --package=CoreLib \
  --format=ipf \
  --output=fpgui.ipf  \
  --input='-Fi../src/corelib ../src/corelib/fpg_base.pas' --descr=xml/corelib/fpg_base.xml \
  --input='-Fi../src/corelib ../src/corelib/x11/fpg_x11.pas' --descr=xml/corelib/x11/fpg_x11.xml \
  --input='-Fi../src/corelib ../src/corelib/gdi/fpg_gdi.pas' --descr=xml/corelib/gdi/fpg_gdi.xml \
  --input='-Fi../src/corelib ../src/corelib/fpg_main.pas' --descr=xml/corelib/fpg_main.xml \
  --input='-Fi../src/corelib ../src/corelib/fpg_cmdlineparams.pas' --descr=xml/corelib/fpg_cmdlineparams.xml \
  --input='-Fi../src/corelib ../src/corelib/fpg_extinterpolation.pas' --descr=xml/corelib/fpg_extinterpolation.xml \
  --input='-Fi../src/corelib ../src/corelib/fpg_imgfmt_bmp.pas' --descr=xml/corelib/fpg_imgfmt_bmp.xml \
  --input='-Fi../src/corelib ../src/corelib/fpg_stdimages.pas' --descr=xml/corelib/fpg_stdimages.xml \
  --input='-Fi../src/corelib ../src/corelib/fpg_stringutils.pas' --descr=xml/corelib/fpg_stringutils.xml \
  --input='-Fi../src/corelib ../src/corelib/fpg_widget.pas' --descr=xml/corelib/fpg_widget.xml \
  --input='-Fi../src/corelib -Fi../src/corelib/x11 ../src/corelib/fpg_utils.pas' --descr=xml/corelib/fpg_utils.xml \
  --input='-Fi../src/corelib ../src/corelib/fpg_popupwindow.pas' --descr=xml/corelib/fpg_popupwindow.xml \
  --input='-Fi../src/corelib ../src/corelib/fpg_wuline.pas' --descr=xml/corelib/fpg_wuline.xml \
  --input='-Fi../src/corelib ../src/corelib/fpg_imagelist.pas' --descr=xml/corelib/fpg_imagelist.xml \
  --input='-Fi../src/corelib ../src/corelib/fpg_constants.pas' --descr=xml/corelib/fpg_constants.xml \
  --input='-Fi../src/corelib ../src/corelib/fpg_pofiles.pas' --descr=xml/corelib/fpg_pofiles.xml \
  --input='-Fi../src/corelib ../src/corelib/fpg_translations.pas' --descr=xml/corelib/fpg_translations.xml \
  --input='-Fi../src/corelib ../src/corelib/fpg_stringhashlist.pas' --descr=xml/corelib/fpg_stringhashlist.xml 


#  --input='-Fi../src/corelib ../src/corelib/fpg_.pas' --descr=xml/corelib/fpg_.xml \

