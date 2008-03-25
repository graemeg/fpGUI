#!/bin/sh
# fpc/bin must be in your PATH
makeskel --disable-private --update --package=CoreLib \
  --input='-Fi../src/corelib ../src/corelib/gfxbase.pas' --descr="xml/corelib/gfxbase.xml" --output="xml/corelib/gfxbase.new.xml"
makeskel --disable-private --update --package=CoreLib \
  --input='-Fi../src/corelib ../src/corelib/x11/gfx_x11.pas' --descr="xml/corelib/x11/gfx_x11.xml" --output="xml/corelib/x11/gfx_x11.new.xml"
makeskel --disable-private --update --package=CoreLib \
  --input='-Fi../src/corelib ../src/corelib/gdi/gfx_gdi.pas' --descr="xml/corelib/gdi/gfx_gdi.xml" --output="xml/corelib/gdi/gfx_gdi.new.xml"
makeskel --disable-private --update --package=CoreLib \
  --input='-Fi../src/corelib ../src/corelib/fpgfx.pas' --descr="xml/corelib/fpgfx.xml" --output="xml/corelib/fpgfx.new.xml"
makeskel --disable-private --update --package=CoreLib \
  --input='-Fi../src/corelib ../src/corelib/gfx_cmdlineparams.pas' --descr="xml/corelib/gfx_cmdlineparams.xml" --output="xml/corelib/gfx_cmdlineparams.new.xml"
makeskel --disable-private --update --package=CoreLib \
  --input='-Fi../src/corelib ../src/corelib/gfx_extinterpolation.pas' --descr="xml/corelib/gfx_extinterpolation.xml" --output="xml/corelib/gfx_extinterpolation.new.xml"
makeskel --disable-private --update --package=CoreLib \
  --input='-Fi../src/corelib ../src/corelib/gfx_imgfmt_bmp.pas' --descr="xml/corelib/gfx_imgfmt_bmp.xml" --output="xml/corelib/gfx_imgfmt_bmp.new.xml"
makeskel --disable-private --update --package=CoreLib \
  --input='-Fi../src/corelib ../src/corelib/gfx_stdimages.pas' --descr="xml/corelib/gfx_stdimages.xml" --output="xml/corelib/gfx_stdimages.new.xml"
makeskel --disable-private --update --package=CoreLib \
  --input='-Fi../src/corelib ../src/corelib/gfx_utf8utils.pas' --descr="xml/corelib/gfx_utf8utils.xml" --output="xml/corelib/gfx_utf8utils.new.xml"
makeskel --disable-private --update --package=CoreLib \
  --input='-Fi../src/corelib ../src/corelib/gfx_widget.pas' --descr="xml/corelib/gfx_widget.xml" --output="xml/corelib/gfx_widget.new.xml"
makeskel --disable-private --update --package=CoreLib \
  --input='-Fi../src/corelib ../src/corelib/gfx_utils.pas' --descr="xml/corelib/gfx_utils.xml" --output="xml/corelib/gfx_utils.new.xml"
makeskel --disable-private --update --package=CoreLib \
  --input='-Fi../src/corelib ../src/corelib/gfx_popupwindow.pas' --descr="xml/corelib/gfx_popupwindow.xml" --output="xml/corelib/gfx_popupwindow.new.xml"
makeskel --disable-private --update --package=CoreLib \
  --input='-Fi../src/corelib ../src/corelib/gfx_wuline.pas' --descr="xml/corelib/gfx_wuline.xml" --output="xml/corelib/gfx_wuline.new.xml"


