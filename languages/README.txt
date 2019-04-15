Quick Start for translators:
----------------------------
For example finnish translation:
Search for all *.fi.po files. They are simple text files, with an easy format.
Edit them with programs like kbabel or gtranslator.
Run 'localize.sh' script in the tools directory to update translations.

Send the updated xxx.fi.po file in a zip archive to graemeg@gmail.com.
Do NOT send diffs for .po files!


Now the background:
-------------------
The <fpGUIdir>/languages directory contains all the stuff for
internationalization of the fpGUI Toolkit.

All language files can easily be updated with the
<fpGUIdir>/tools/localize.sh or <fpGUIdir>\tools\localize.bat
script.

All text and messages used in the toolkit should be placed into the  
<fpGUIdir/src/corelib/lang_en.inc (English) include file. This unit uses a
resourcestring section, so that the compiler will create the
<fpGUIdir>/lib/fpg_constants.rst file.
Since this is a fpc-only format it must be converted with the rstconv program:

cd <fpGUIdir>/languages
rstconv -i ../lib/fpg_constants.rst -o fpgui.po

HINT:
  This is automatically done by localize.sh or localize.bat scripts.

This will create the file fpgui.po, which should be translated in all
required languages to a fpgui.xx.po file. For the xx see the gettext unit
in the procedure TranslateResourceStrings. All translation .po files are 
in UTF-8 encoding.


fpGUI Toolkit currently supports the following translations:
------------------------------------------------------------
afrikaans:              fpgui.af.po
german:                 fpgui.de.po
english:                fpgui.en.po
spanish:                fpgui.es.po
estonian:               fpgui.et.po
french:                 fpgui.fr.po
hebrew:                 fpgui.he.po
italian:                fpgui.it.po
lao:                    fpgui.lo.po
malay:                  fpgui.ms.po
polish:                 fpgui.pl.po
portuguese(Brazil):     fpgui.pt_BR.po
portuguese(Portugal):   fpgui.pt.po
russian:                fpgui.ru.po
ukrainian:              fpgui.uk.po
chinese:                fpgui.zh_CN.po
