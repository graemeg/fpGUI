
Compiling:

- Run-time package: 
  lclfpreport.lpk
- Demos: demos/fcldemo.lpr

fpReport Demos can be found in the "demos" directory.

Check the various defines in udapp.pp and demos.inc:

EXPORTPDF - enable export to PDF
EXPORTFPIMAGE - enable export to image
EXPORTHTML - enable export to html
EXPORTFPGUI - enable export to fpGUI (preview window)
EXPORTAGGPAS - enable exporting to image using high quality AggPas rendering


Running on windows:
If EXPORTFPIMAGE is defined, the freetype.dll is needed. If using EXPORTAGGPAS then
you have the option to use GDI font rendering or FreeType font rendering - the latter
requires the freetype.dll to be available.

TODO:
  * An initial stand-alone report designer was created using LCL. I have plans to port
    that to fpGUI, or extend fpGUI's UIDesigner with a Report Design module. Either way,
    it is pretty easy to design reports via source code alone (by design), so not having
    a designer immediately will not prevent you from using fpReport.

