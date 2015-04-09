
Name:    ats (alternative translation strings) editor
Author:  Nagy Viktor <nvitya@freemail.hu>
Date:    around 2006
Description:
This uses a single file for all language translations. You can then
use the ats editor to edit that *.ats file. The editor was planned
to support *.ats, *.inc and *.csv file formats, but not all of them
are implemented.


Usage (2015-04-02):
I modified the project a bit to make it more user friendly and to show
how it is used. Basically it maintains a list of Resource/Text ID's and
then the translation for each of those in a language column. Any number
of languages can be added. The editor is optimised for quick keyboard
usage, so translation can be done very quickly. You will have to include
a few of the ats units in your own project to use it - just like the
ats_editor project does itself.

The editor maintains an "atstable.inc" file which is just an array of
text strings. To add a new language, it still needs to be done manually
by editing the atstable.inc file (improving the editor to do this should
be easy) and just adding one or more new language codes.

I did the following change in the atstable.inc:

   ,'  RU="Русский" HU="Magyar"'

After that you run the ats_editor and edit the Resource ID's for those
new languages.

You set the current language by calling atstable.SelectLang(<LangID>)

You retrieve the translated text for a specific Resource ID by calling

    atsText(<ResourceID>)

eg:  ShowMessage(atsText('rsCancel');

                     --------[ end ]---------
