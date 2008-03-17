Quick Start for translators:

For example finnish translation:
Search for all *.fi.po files. They are simple text files, with an easy format.
Edit them with programs like kbabel or gtranslator.
Run 'sh localize.sh' script in the tools directory to update translations.

Send the updated xxx.fi.po files to graemeg.lists@gmail.com.
Do not send diffs for .po files.


Now the background:

The <fpGUIdir>/languages directory contains all the stuff for
internationalization of the fpGUI Toolkit.

All language files can easily be updated with the
<fpGUIdir>/tools/localize.sh or <fpGUIdir>\tools\localize.bat (coming soon) 
script.

All text and messages used in the toolkit should be placed into the  
<fpGUIdir/src/corelib/lang_english.inc include file. This unit uses a
resourcestring section, so that the compiler will create the
<fpGUIdir>/lib/gfx_constants.rst file.
Since this is a fpc-only format it must be converted with the rstconv program:

cd <fpGUIdir>/languages
rstconv -i ../lib/gfx_constants.rst -o fpgui.po

Hint: 
This is done by <fpGUIdir>/tools/localize.sh or <fpGUIdir>\tools\localize.bat.

This will create the file fpgui.po, which should be translated in all
required languages to a fpgui.xx.po file. For the xx see the gettext unit
in the procedure TranslateResourceStrings.

german:            fpgui.de.po
russian (KOI8-R):  fpgui.ru.po
spanish:           fpgui.es.po
french:            fpgui.fr.po
italian:           fpgui.it.po
afrikaans:         fpgui.af.po


