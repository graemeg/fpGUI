makeskel --package=fpGFX --output=xml/gfx/fpgfx.xml --input='-Fi../gfx ../gfx/fpgfx.pas'
makeskel --package=fpGFX --output=xml/gfx/geldirty.xml --input='-Fi../gfx ../gfx/geldirty.pas'
makeskel --package=fpGFX --output=xml/gfx/gelimage.xml --input='-Fi../gfx ../gfx/gelimage.pas'
makeskel --package=fpGFX --output=xml/gfx/gfxbase.xml --input='-Fi../gfx ../gfx/gfxbase.pas'

# X11 interface
makeskel --package=fpGFX --output=xml/gfx/gfxinterface.xml --input='-Fi../gfx ../gfx/x11/gfxinterface.pas'
makeskel --package=fpGFX --output=xml/gfx/gfx_x11.xml --input='-Fi../gfx ../gfx/x11/gfx_x11.pas'
makeskel --package=fpGFX --output=xml/gfx/unitxft.xml --input='-Fi../gfx ../gfx/x11/unitxft.pas'

