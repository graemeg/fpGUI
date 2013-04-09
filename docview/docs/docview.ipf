:userdoc.
:docprof toc=123456.
:title.fpGUI DocView Help

.nameit symbol=dv text='DocView'

.* ************************************************************
.* Introduction
.* ************************************************************
:h1 res=30000 id='Introduction'.Introduction
:i1 id=30001.support
.*  :artwork runin name='images\DocView.bmp'.
:hp2.Welcome to &dv.!:ehp2.
:p.
&dv. is a program for reading fpGUI or OS/2 (aka eComStation) Help
Files. Both the INF and OS/2 HLP file formats can be read.
:p.:link reftype=hd refid='Support'.Support and Licensing:elink.
:p.:link reftype=hd refid='Using'.Using &dv.:elink.
:p.:hp1.To stop this file appearing when you start &dv. without specifying a
help file, see "Settings - Options - General" tab.:ehp1.
:p.
:hp2.History:ehp2.
:p.
&dv. was originally based on the OS/2 NewView program. It was ported from
the Sibyl development environment, to the fpGUI Toolkit and the Free Pascal Compiler. Since then the code
and features of &dv. has diverged.
:p.
&dv. improves upon many aspects of the origin IBM VIEW program, with a more modern and
easy to use interface, more options, and new features that VIEW simply
didn't have. And don't forget, &dv. is also cross platform - currently available
as native executables for Linux, FreeBSD and Windows. &dv. is also available in
32-bit and 64-bit.


.* ************************************************************
.* Support and Licensing
.* ************************************************************
:h2 res=1 id='Support'.Support and Licensing
:i1 id=30002.license
:i2 refid=30001.Support and Licensing
:i2 refid=30001.Introduction
:i1 id=30003.bugs
:i1 id=30004.source code
:i1 id=30007.features planned
:hp2.Support and Licensing:ehp2.
:p.&dv. is Copyright 2009-2011 by Graeme Geldenhuys. It is also licensed
under the GNU Public License v2, which means you have the right to obtain the
source code.
:p.
&dv. is part of the fpGUI Toolkit project and is the default help viewer for
any fpGUI based applications. Source code for &dv. is available in the fpGUI
git repository on SourceForge.net, inside the :hp1.docview:ehp1. directory.
:p.
If you find &dv. helpful, please email me and/or make a
donation to support further development. It's nice to hear from
you!
:ul.
:li.Project is hosted on SourceForge.net at http://sourceforge.net/projects/fpgui/
:li.Suggestions, compliments or bug reports can be left on the fpGUI support newsgroups.
The news server is accessible via NNTP at news://opensoft.homeip.net/
.br
There is also a web interface to the NNTP news server for those behind company
firewalls. http://opensoft.homeip.net/webnews/
:li.Translate &dv. to your language.
:li.A donation to fpGUI development. You can contact me via email at: graemeg@gmail.com
:eul.
:p.:hp2.Reporting Bugs:ehp2.
:p.If you need to report a crash or other problem, then be as specific as
possible about what files were being used, what you were doing,
etc. If at all possible, compile &dv. with debug information and supply a
backtrace. This will greatly help me fix problems.
:p.
If it is specific to a particular help file, then send them to me via email.
:p.
It would be helpful if you could include the following information with your
bug report:
:ul.
:li.&dv. version (Help - Product Information)
:li.The file names of the help files you have problems with.
:li.A screenshot may be useful, if the problem is an incorrect or corrupt
display.
:eul.

:p.:hp2.Why doesn't my help file work properly?:ehp2.
:p.
Some of the less used features of the original IBM VIEW program are not
implemented. This is either because I have not got around to it, or
because they are simply not worth the time. Examples include
metafiles, index synonyms, application control API (like toolbar button customisation),
MDI style window management and so on.
:p.
Unfortunately, IBM seems to have used every one of those features in their INF
documents, so you may find an occasional file that doesn't
display or navigate properly.

:p.:hp2.Features still planned?:ehp2.
:ul.
:li.Image support. The Rich Text View component can already handle images, I am only struggling
to decode the images inside INF help files. This should hopefully be fixed soon.
:li.Printing support
:li.Cross-file linking. Clicking a hyperlink that automatically opens another
INF help file and goes to the correct topic. &dv. already supports loading
multiple files, so this is almost complete.
:li.Global searching of all help files on your help folder specified by the
environment variables BOOKSHELF or HELP.
:eul.

.* ----------------------------------------------------------------------
:h2.Disclaimers
:p.
Since this program is free, it is supplied with no warranty, either expressed or
implied. 
:p.
I disclaim all warranties for any damages, including, but not limited to, incidental
or consequential damage caused directly or indirectly by this software. 
:p.
All software is supplied AS IS. You may use the program package only at your own
risk. 
:p.
This program must not be used in states that do not allow the above limitation of
liability.


.* ----------------------------------------------------------------------
:h2.Trademarks
:p.
The following trademarks are used in this online help file:
:ul compact.
:li.OS/2 is a registered trademark of IBM Corporation.
:li.eComStation is a registered trademark of Serenity Systems, Inc.
:li.Windows is a registered trademark of Microsoft Corporation
:li.Linux is a registered trademark of Linus Torvalds
:li.FreeBSD is a registered trademark of The FreeBSD Foundation
:eul.

.* ************************************************************
.* Using DocView
.* ************************************************************
:h1 res=2 id='Using'.Using &dv.
:hp2.Using &dv.:ehp2.
:p.
Once you have :link reftype=hd refid='OpeningFiles'.opened a
file:elink., you can read it in various ways.
:p.
You can read the :link reftype=hd refid='contents'.table of
contents:elink., use the :link reftype=hd refid='Index'.alphabetical
index:elink., or :link reftype=hd refid='search'.search:elink..
:p.
To simply read the help file like a paper book, use the "Previous"
.*  :artwork runin name='images\previous.bmp'.
 and "Next"
.*  :artwork runin name='images\next.bmp'.
 buttons to work your way through all the topics. They are the Up and Down arrow
 buttons on the toolbar.
:p.You can also use the help file like web pages, using "Back"
.*  :artwork runin name='images\back.bmp'.
 and "Forward"
.*  :artwork runin name='images\forward.bmp'.
 buttons to go back to wherever you were before, or to retrace your
steps. They are the Left and Right arrow buttons on the toolbar.
:p.
Colours and some of the behaviour of &dv. can be adjusted from the "Tools
- Options" menu.
.* TODO
.* :p.You can also :link reftype=hd refid='notes'.annotate:elink. or :link
.*reftype=hd refid='bookmarks'.bookmark:elink. topics.


.* ************************************************************
.* Opening Help File
.* ************************************************************
:h1 res=3 id='OpeningFiles'.
Opening Files
:i1 id=30005.open
:p.:hp2.Opening Help Files:ehp2.
:p.
To open a help file, you can use any of the following:
:ul.
:li.Double-click a :link reftype=hd refid='HelpIcons'.help icon:elink. that is
already set up. The &dv. executable needs to be associated with INF files in
your desktop environment.
:li.Type "docview :hp1.filename:ehp1." from the :link reftype=hd
refid='CommandLine'.command line:elink..
:li.Click the Open button
.*  :artwork runin name='images\open.bmp'.
 from within &dv., or the "File - Open" menu item.
:li.Click the "File - Open additional file" menu item to open more files, without
closing the already opened files.
:li.Click the "File - Open Special" to load help files based on an Environmnt Variable
that was previous set.
:li.Reload a recently viewed file from the "File - Open Recent" menu.
:eul.
:p.
Once the file is loaded, you should see the :link reftype=hd
refid='contents'.table of contents:elink. and the first help topic.
:p.
:hp2.Loading Multiple Files Together:ehp2.
:p.
&dv. can load multiple files at once, presenting them as if they
were one book, and read environment variables for filenames.
:p.
For example, on my system I can do the following:
:xmp.
  docview FPCHELP
:exmp.
:p.
which loads the whole Free Pascal help library on my system. FPCHELP is an environment
variable set in ~/.profile (Linux) or config.sys (Windows) consisting of "rtl+fcl+ref"
which tells &dv. to load the help files rtl.inf, fcl.inf and ref.inf. The
files are searched for in the path specified by two :link
reftype=hd refid='L_EnvironmentVariables'.environment
variables:elink..
:p.
The files are all loaded and effectively appended to each other.
:p.
Being able to load multiple files like this can be helpful for various
reasons. For example, I use this method to integrate &dv. with the various IDE's
(Integrated Development Environments) I use. I can then easily search for help on
language keywords, source code classes or units etc.
:p.
You can also open additional help files by selecting the "File - Open additional file"
menu option.
.* TODO
.* :p.
.* When you click a link to a different help file&comma. &dv. loads the other
.* file without closing your current files&per.
:p.
At any time, you can find out what files are open by using the "Help - Show help
file header info" menu option.
:p.
:hp2.Loading Additional Files:ehp2.
:p.
You can select the "File - Open additional file" menu option, and &dv. will
open the file you have selected without closing the currently opened files.
.*  TODO
.*  :p.:hp2.Drag and Drop:ehp2.
.*  :p.You can drag and drop &per.INF or &per.HLP files onto &dv. and they will
.*  be opened&per. If you hold down the Shift key&comma. they will be opened without
.*  closing the current files&per.
.*  :p.You can drop files onto any of the main content areas&comma. such as the
.*  Contents or Index windows&comma. or an existing topic window&per.
.*  :note text='Note:'.Some links that go across files&comma. will only work if the correct set of
.*  files is loaded&per.


.* ************************************************************
.* Help Icons
.* ************************************************************
:h2 res=17 id='HelpIcons'.Help Icons
:hp2.Help Icons:ehp2.
:p.
Help Icons on the desktop are usually "program shortcuts" or "filesystem objects".
Details on how to create a program shortcut for &dv., or how to associate INF
files to be opened with &dv., is beyond the scope of this document. Please see the
help of your operating system for more information.
:p.
Saying that, with the fpGUI source code, in the "docview/install" directory, there is some
information on how to setup a Gnome desktop with &dv.. Unfortunately those
instructions don't seem to work on all Linux or FreeBSD systems, so you again need
to refer to your desktop system's help for guidance.
.*  :p.Some programs create these icons automatically at install time&per.
.*  :p.You can create these icons yourself by using the desktop program
.*  template&per. See desktop help for more information&per.
.*  :p.If you create icons by dragging help files to the desktop&comma. then you
.*  cannot give them a meaningful title&comma. because that would change the name of
.*  the file&comma. which might prevent programs from finding the help file&per.
.*  Therefore program objects are currently the recommended means of creating help
.*  icons&per.


.* ************************************************************
.* Navigation Panel Tabs
.* ************************************************************
:h1 res=200 id='NavigationPanel'.Navigation Panel Tabs
:hp2.Navigation Panel Tabs:ehp2.
:p.
The left hand panel contains several tabs for moving through the current help
file in different ways.
:ul.
:li.:link reftype=hd refid='contents'.Contents:elink.
:li.:link reftype=hd refid='index'.Index:elink.
:li.:link reftype=hd refid='search'.Search:elink.
:li.:link reftype=hd refid='notes'.Notes:elink.
:li.:link reftype=hd refid='history'.History:elink.
:eul.
:p.You can hide this panel to get more space, by draging the splitter bar
.*  :artwork runin name='images\navigator.bmp'. or by selecting View
to the left, or simply double-clicking on the splitter bar. To show the Left Panel
again, drag the splitter bar to the right.
:p.
The last location of the splitter bar (width of the Left Panel) is remembered
between &dv. sessions. So when you open &dv., the Left Panel is the same
width as the last time you used &dv..
.*  TODO
.*  :p.You can stop the navigation panel from appearing when a help file is opened
.*  in Tools &endash. Options &endash. General tab&per.


.* ************************************************************
.* Contents View
.* ************************************************************
:h2 res=4 id='contents'.Contents View
:i1 id=30006.Table of Contents
:hp2.Table of Contents:ehp2.
:p.
Most help files have a table of contents that shows you the topics in the
file, in a hierarchy or "tree". This is usually the first view you see
when you open a file.
:p.
You can expand or collapse branches on the tree by clicking the [+] or
[-] buttons, or using the left and right arrow keys on the keyboard.
:p.
To view a topic from the contents, just click on it. You can also
move through the contents by using the arrow keys.
:p.
To move through :hp2.all:ehp2. topics in the contents tree, in
order, you can use the
.*  TODO
.*  Ctrl &plus. Up and Ctrl &plus. Down&comma. or the
"Previous"
.*  :artwork runin name='images\previous.bmp'.
 and "Next"
.*  :artwork runin name='images\next.bmp'.
 buttons on the toolbar. This is one way to treat the help file as a normal book, reading
through each page.
:p.
You can also review the whole table of contents by using "Expand All" in the
tree view's popup menu. This expands all the branches of the contents table so you can
quickly look through it. However, it's usually easier to use
:link reftype=hd refid='search'.Search:elink. or :link reftype=hd
refid='Index'.Index:elink. for this purpose.

.* ************************************************************
.* Index
.* ************************************************************
:h2 res=5 id='index'.Index
:hp2.About the Index:ehp2.
:p.
The Index tab contains an alphabetical listing of topics or keywords in the
help file. You can quickly search through it just by typing the first few
characters of the word you want to look up. &dv. jumps to the first match
in the index automatically. To view the highlighted topic, press
enter.
:nt.
Help files may or may not include an "official" index. The index is
manually created by the author, so (for the original OS/2 View program) it's
usefulness is strictly dependent on how much work the author put into it.
There may not even be Index information in the help file.
:ent.
:p.
However, a useful index can be provided simply by listing the titles of
each topic alphabetically, and this is what &dv. does. It then
merges the original index (if any) with the list of topic titles.
:p.
If for some reason you don't like this, you can change this behaviour in
"Settings - Options - General" tab. There are three options to choose from.
:dl tsize=16.
:dt.Alphabetical
:dd.All topics from the table of content are sorted and displayed as an Index.
:dt.File Only
:dd.Only the "official" index created by the author of the help file is used. Some
help files don't include an official index, so then the Index listbox will be empty.
:dt.Full
:dd.Both the topics and the official index are combined to display a larger and
more useful Index to the user. This is the default option.
:edl.

.* ************************************************************
.* Search
.* ************************************************************
:h2 res=6 id='search'.Search
:hp2.Searching:ehp2.
:p.
Searching is a quick way to find information, when you don't know
where to start. Simply go to the Search tab, type some related words
and press Enter, or click the Go button.
You'll see a listing of all topics containing that word, or words
like it, with the best matches at the top. The best match will be
displayed automatically.
:p.
Words that matches your search are highlighted in the topic.
.*  TODO
.*  :p.
.*  :hp2.Global search:ehp2.
.*  :p.You can also search all help files on your system using the :link reftype=hd
.*  refid='GlobalSearch'.global search:elink. in "Settings - Search all Help
.*  Files".
:p.
:hp2.Phrase search:ehp2.
:p.
If you want to search for a phrase made up of more than one word, put
double quotes around it, for example "OS/2 warp".
:p.
:hp2.Matching features:ehp2.
:p.
&dv. allows you finer control of searching. You can add either of the two symbols
directly infront of a word (no spaces between symbol and word), to narrow down
a search even more.
:dl compact tsize=20.
:dt.+ (plus sign)
:dd.indicates a word that :hp2.must:ehp2. be matched
:dt.- (minus sign)
:dd.indicates a word that must :hp2.not:ehp2. be matched
:edl.
:p.
To make searching even more useful, &dv. implements a special search algorithm
which does partial word matches. That is, if you search for "win", &dv. will
also find "window", "following", "showing" etc. However, the better the match is the higher
the rank will be. The search rank is displayed in square brackets behind each
search result. Searches are :hp2.not:ehp2. case-sensitive.
:p.
:hp2.How &dv. ranks results:ehp2.
:p.&dv. ranks matching topics by various means:
:ul compact.
:li.a closer match to a full word
:li.number of matching words in a topic
:li.matches within the title
:li.matches within an index entry
:eul.

.* ************************************************************
.* Notes
.* ************************************************************
:h2 res=7 id='notes'.Notes
:i1.Annotate
:hp2.Adding and Using Notes:ehp2.
:p.
&dv. allows you to add notes (annotations) to your help
files.
:p.
To add a note, simply left click where you want to make a note and click the
"Note" toolbar button
.*  :artwork runin name='images\note.bmp'.
, then type your text and click OK. The text will be inserted into the
help topic with a different color (default is green; you can change
this in "Settings - Options - Fonts &amp. Colors".
:p.
To edit or delete a note, click on the colored note text; you can
then edit the note text, or click on delete to get rid of it.
:p.
You can also review all the notes that you've made in the current help
file(s) by going to the Notes tab; this allows you to add,
edit and delete, and also jump to the topics containing your notes.
:p.
Notes are saved in a file with the extension "&per.notes", in same directory
as the help file. This makes it ideal to keep your private notes with the help
files in question. For example: storing help files on a removable usb drive. If
you made any annotations, your annotations will be avaiable, no matter what
machine you use to read your help. In future this behaviour will become user
selectable, so you can select your preferred storage location.

.* the DocView config
.* directory. This is in the user's home profile directory where there is read/write
.* access. Under Linux it is normally "~/.config/docview/" and under Windows it is
.* normally "C:\Documents and Settings\<user>\Local Settings\Application Data\docview".
.* I will probably add a setting in &dv., so the user can configure a
.* preferred storage location for notes (eg: some users prefer it like OS/2's View program did,
.* by storing notes in the same directory as the help file).

:nt.If a help file is changed (for example a program is
upgraded) then notes will no longer appear in the correct place;
however, you can still read them from the Notes tab.
:ent.

.* ************************************************************
.* History
.* ************************************************************
:h2 res=21 id='history'.History
:p.:hp2.About the History:ehp2.
:p.
&dv. keeps track of every topic you visit. This allows you to view a help file
like a web browser does with web pages. You can use the "Back" and "Forward"
buttons on the toolbar to move back and forward in your history list.
:p.
In the History tab, is a list of all previously visited topics. To jump to any
of them, simply highlight the topic and press enter. Or simply double-click a
topic.
:p.
The history is not saved when you exit &dv.. The history is also cleared if
you open a new or additional help file.

.*
.*
.* ************************************************************
.* Global Search
.* ************************************************************
.*  TODO
.*  :h1 res=18 hide id='GlobalSearch'.Global Search
.*  :hp2.Global search:ehp2.
.*  :note.:hp8.*** This feature is not implemented yet. ***:ehp8.
.*  :p.You can search all help files on your system, by clicking the Search
.*  All toolbar button
.*  .*  :artwork runin name='images\search.bmp'.
.*  , or by using "Tools - Search all Help Files" in the menu, or
.*  pressing Ctrl+S.
.*  :p.
.*  This search works similarly to :link reftype=hd refid='search'.searching
.*  within a file:elink., but it also tells you what help file the results
.*  were found in.
.*  :p.
.*  These searches may take some time, depending on what you search
.*  for. You can stop the search at any time.
.*  :p.
.*  :hp2.Where &dv. searches:ehp2.
.*  :p.
.*  The default is to search for help files in the help paths, which are
.*  specified by the BOOKSHELF and HELP :link reftype=hd
.*  refid='L_EnvironmentVariables'.environment variables:elink..
.*  :p.
.*  You can choose other places to search by choosing from the drop-down
.*  list, or customise the list by clicking the Select button.
.*  :ul.
.*  :li.:hp2.Search in: Standard Help Paths:ehp2.
.*  :p.
.*  This is the default and will search BOOKSHELF and HELP as specified
.*  above.
.*  :p.
.*  Clicking the select button will allow you to choose which of the directories
.*  in the help paths will be used. Click each item in the list to select or
.*  deselect it. After you choose this, the location will show as
.*  "Selected Help Paths".
.*  :li.:hp2.Search in: All Hard Drives:ehp2.
.*  :p.
.*  This option will search all hard drives on
.*  your system. You can click "Select..." to customise the
.*  location.
.*  :p.
.*  Searching drives can find more help files, but might be much slower
.*  than just the help paths.
.*  :li.:hp2.Search in: Selected Help Paths:ehp2.
.*  :p.
.*  If you have already selected particular help paths to search, you can
.*  click "Select..." to customise again.
.*  :li.:hp2.Search in: Directory List:ehp2.
.*  :p.
.*  In the "Select Directories" dialog, clicking the "Add..."
.*  button will allow you to add one or more directories to the search list.
.*  :p.
.*  Select the drive and directory using the controls that appear, then
.*  click "< Add Directory" to add the chosen directory. You can do this as
.*  many times as you like. Choose "With sub-directories" if you want
.*  sub-directories of the selected directory to be searched as well. In
.*  this case, ... will show on the end of the directory.
.*  :p.
.*  After you add a custom directory like this, the location for searching
.*  will show as "Directory List".
.*  :note.If you add a custom directory to standard or selected help
.*  paths, then the list will become a custom list, and you can no
.*  longer re-select help paths. To get back to the original help
.*  paths, choose "Standard Help Paths" then click "Select..."
.*  again.
.*  :li.:hp2.Search in: Typing a location:ehp2.
.*  :p.
.*  You can type a drive or directory into the "Search in:" entry
.*  field. Add "..." on the end of the directory if you want to
.*  search subdirectories as well.
.*  :p.
.*  Example:
.*  :p.
.*  Search in: [ E:\mydocs\... ]
.*  :p.
.*  This will search help files in E:\mydocs\ and any
.*  subdirectories.
.*  :eul.


.* ************************************************************
.* Bookmarks
.* ************************************************************
:h1 res=8 id='bookmarks'.Bookmarks
:hp2.Bookmarks:ehp2.
:p.
&dv. allows you to bookmark particular topics within the current help
file. Simply click the bookmark toolbar button
.*  :artwork runin name='images\bookmark.bmp'.
to add the current topic as a bookmark.
:p.
To jump to a bookmark, go to the "Bookmarks" menu, and click on
the bookmark you want to open.
:p.
You can view or delete all your bookmarks by clicking on "Edit..." in
the "Bookmarks" menu.
:p.
Bookmarks are saved in a file with the extension ".bookmarks", in the &dv. config
directory. This is in the user's home profile directory where there is read/write
access. Under Linux it is normally "~/.config/docview/" and under Windows it is
normally "C:\Documents and Settings\<user>\Local Settings\Application Data\docview".
.* TODO
.* I will probably add a setting in &dv., so the user can configure a
.* preferred storage location for bookmarks (eg: some users prefer it like OS/2's View program did,
.* by storing notes in the same directory as the help file).


.* ************************************************************
.* Internet Links
.* ************************************************************
:h1 res=100 id='InternetLinks'.Internet Links
:hp2.Internet Links:ehp2.
:p.
When you click a web URL like
http://fpgui.sourceforge.net , &dv. launches your
default web browser.
:p.
This web browser is specified by operating system settings, not &dv.
itself. Please see your operating system or desktop environment help for further
details on how to configure it.
:p.
For more technical details about what browser is used, have a look at the fpgOpenURL()
function in fpGUI Toolkit source code.
:p.
Email, Newsgroups and FTP links are also passed to the operating system or desktop
environment, the same as web addresses.
:p.
:hp2.Note to help file authors:ehp2.
:p.The original OS/2 View program had no understanding of URL or email links, so the
only way to implement them was a link to, for example,
"netscape.exe" with the correct parameters.
:p.
&dv. translates program links to "netscape", "explorer" or "mozilla"
into links to the default browser.
:p.
It also auto-detects URLs in the forms:
:lm margin=5.
:p.http&colon.//xxx
:p.https&colon.//xxx
:p.ftp&colon.//xxx
:p.mailto&colon.xxx
:p.news&colon.xxx
:lm margin=1.
:p.
Things that look like URLs are also detected, even without the protocol
prefix:
:lm margin=5.
:p.www&per.aaa&per.bbb - browser
:p.ftp&per.aaa&per.bbb - ftp
:p.aaa&atsign.bbb&per.ccc - email
:lm margin=1.
:p.
where aaa, bbb and ccc are any alphanumeric string.
:p.
You don't need to do anything for &dv. to recognise these.


.* ************************************************************
.* Command Line
.* ************************************************************
:h1 res=9 id='CommandLine'.Command Line Parameters
:hp2.Command Line Parameters:ehp2.
:p.
When you run &dv. from the command line you can supply various
parameters. None of them are required. For a quick summary, select
"Help - Command line parameters".
:xmp.
  docview [<filename>] [[option] [option parameter]]
:exmp.

:p.
:link reftype=hd refid='CommandLineExamples'.:hp2.Examples:ehp2.:elink.
:p.
:hp2.<filename>:ehp2.
:p.
The file for &dv. to load. You can load multiple files at once by
using filename1+filename2 format.
:p.
If a path isn't specified, then the files are searched for in the
:link reftype=hd refid='L_EnvironmentVariables'.BOOKSHELF and HELP:elink.
environment variable locations.
:p.
If a path and/or filename contains special characters (like blank) then
you have to enclose the filename in double qoutes.

:p.
:hp2.[option]:ehp2.
:p.The following options are available:
:parml tsize=6.
:pt.-h
:pd.Show the command line summary when &dv. starts up.
:pt.-k <searchtext>
:pd.After opening the file, it performs a :link reftype=hd
refid='search'.search:elink. for a keyword <searchtext>. This does a full text
search look at topic titles, index entries and topic content. The result is the
same as performing a search from the :link reftype=hd refid='search'.search panel:elink..
:p.
Example:
.br
:lm margin=8.
To search for TfpgApplication in the whole fpGUI Toolkit class document you can call

:xmp.
  docview fpgui.inf -k TfpgApplication
:exmp.

:p.
&dv. is clever enough to handle multiple words (like the :link reftype=hd
refid='search'.search panel:elink.). This is a OR search.

:xmp.
  docview fpgui.inf -k net access
:exmp.

:p.
To perform an AND search enclose the search phrase in double quotes.
:xmp.
  docview fpgui.inf -k "net access"
:exmp.
:lm margin=1.

:pt.-n <id>
:pd.Open a topic with the numeric ID equal to <id>.
:pt.-s <id>
:pd.Open a topic with string ID equal to <id>.
:pt.-debuglog <filename>
:pd.Log debug information to a file.
:eparml.

.*  :p.See also&colon. :link reftype=hd refid='AdvancedParameters'.Advanced
.*  Parameters:elink.

.* ************************************************************
.* Advanded Parameters
.* ************************************************************
.*  :h2 res=14 id='AdvancedParameters'.
.*  Advanced Parameters
.*  :p.:hp2.Advanced Parameters:ehp2.
.*  :p.The following command line parameters are intended mainly for software
.*  developers&comma. but can be used for any purpose&per.
.*  :p.:hp2.&slash.lang&colon.&lt.language spec&gt.:ehp2.
.*  :p.Loads the specified language&per. Overrides the default chosen based on the
.*  LANG environment variable&per. For example&comma.
.*  :xmp.
.*    DocView cmdref &slash.lang&colon.en
.*  :exmp.
.*  loads English&per. See readme&per.txt for more information&per.
.*  :p.:hp2.&slash.pos&colon.&lt.left&gt.&comma.&lt.bottom&gt.&comma.&lt.width&gt.&comma.&lt.height&gt.:ehp2.
.*  :p.Set the main program window to the given position and size&per. All values
.*  must be given&per. Put a :hp2.P:ehp2. after a number to specify a
.*  percentage&per. For example&colon.
.*  :xmp.
.*    DocView &slash.pos&colon.10P&comma.10P&comma.80P&comma.80P
.*  :exmp.
.*  :p.makes the window centered and 80&percent. of the screen size&per.
.*  :p.:hp2.&slash.title&colon.&lt.window title&gt.:ehp2.
.*  :p.Sets the title of the &dv. window to the specified text&comma. overriding
.*  whatever appears in the help file&per. The text &odq.Help &endash.  &cdq. will always be
.*  inserted in front of the specified text&comma. unless the specified text is
.*  &odq.help&cdq.&comma. in which case the title will simply become &odq.Help&cdq.&per. This is to
.*  make sure that help windows are always obvious as such in the window list&per.
.*  :p.If you need to specify multiple words&comma. surround the entire option with
.*  quotes&comma. for example&colon.
.*  :xmp.
.*    DocView cmdref &odq.&slash.title&colon.Command Line Help&cdq.
.*  :exmp.

.* ************************************************************
.* Command Line Examples
.* ************************************************************
:h2 res=15 id='CommandLineExamples'.Command Line Examples
:hp2.Command Line Examples:ehp2.
:p.
The following examples assume that &dv. is installed and is accessible from
the command line.
:p.
:hp2.docview fpgui:ehp2.
:lm margin=4.
:p.
Open the file fpgui.inf (fpGUI Toolkit's class documentation) from
the help path.
:lm margin=1.
:p.
:hp2.docview fpgui+rtl:ehp2.
:lm margin=4.
:p.
Open two files, fpgui.inf and rtl.inf (Free Pascal's Runtime Library documentation),
in the same window. The table of contents from rtl.inf is added to the end of the
contents from fpgui.inf. The indexes are combined alphabetically.
:lm margin=1.
:p.
:hp2.docview c:\programming\books\rtl.inf:ehp2.
:lm margin=4.
:p.
Open the file rtl.inf in the c:\programming\books\ directory.
:lm margin=1.
:p.
:hp2.docview "c:\programming books\rtl.inf":ehp2.
:lm margin=4.
:p.
Surround <path>\<file> with double quotes if they contain special characters
(like spaces).
:lm margin=1.
:p.
:hp2.docview fpgui -k themes:ehp2.
:lm margin=4.
:p.Open the file fpgui.inf and search for the word "themes". The best
match is shown. The search tab will also be active, showing any other search
results.
:lm margin=1.
:p.
Linux example:
.br
:hp2.export MYHELP=rtl+fcl+fpgui:ehp2.
.br
:hp2.docview MYHELP:ehp2.
:p.
Windows example:
.br
:hp2.set MYHELP=rtl+fcl+fpgui:ehp2.
.br
:hp2.docview MYHELP:ehp2.
:lm margin=4.
:p.The first line sets an environment variable MYHELP to contain the names of
three help files. The second line opens the three files.
:note.Under Linux, environment variable names are also case sensitive (like
file names), so the environment variable "MYHELP" is not equal to "myhelp".
:lm margin=1.


.* ************************************************************
.* Keyboard Shortcuts
.* ************************************************************
:h1 res=10 id='KeyboardShortcuts'.Keyboard Shortcuts
:p.:hp2.Keyboard Shortcuts:ehp2.
:p.Most keyboard shortcuts are visible in the menu, but a few are not.
Here is a list of keyboard shortcuts supported by &dv.:

:table rules=vert frame=box cols='45 20'.
:row.
:c.:hp2.TASK:ehp2.
:c.:hp2.PRESS:ehp2.

:row.
:c.----------
:c.----------

:row.
:c.Open File
:c.Ctrl+O

:row.
:c.Open Additional Files
:c.Ctrl+Shift+O

:row.
:c.Open Special (eg: environment variable)
:c.Ctrl+L

:row.
:c.Save Current Top
:c.Ctrl+S

:row.
:c.Quit &dv.
:c.Ctrl+Q

:row.
:c.Switch to Contents tab
:c.F5

:row.
:c.Switch to Index tab
:c.F6

:row.
:c.Switch to Search tab
:c.F7

:row.
:c.Switch to Notes tab
:c.F8

:row.
:c.Switch to History tab
:c.F9

:row.
:c.Navigate Back
:c.Ctrl+left arrow

:row.
:c.Navigate Forward
:c.Ctrl+right arrow

:row.
:c.Navigate to previous topic in contents
:c.Ctrl+up arrow

:row.
:c.Navigate to next topic in contents
:c.Ctrl+down arrow

:row.
:c.Bookmark current topic
:c.Ctrl+B

:row.
:c.Edit Bookmarks
:c.Ctrl+M

:row.
:c.Add note (annotations) at cursor position
:c.Ctrl+M

:etable.



.* ************************************************************
.* Environment Variables
.* ************************************************************
:h1 res=11 id='L_EnvironmentVariables'.Environment Variables
:hp2.Environment Variables:ehp2.
:p.
Both the :hp2.BOOKSHELF:ehp2. and :hp2.HELP:ehp2. environment variables
define paths (lists of directories) for searching for help files.
&dv. uses both paths without distinction.
:p.
These paths are searched when you:
:ul.
:li.specify a help file without a path on the command line
:li.use the "File - Open Special..." menu item
.* TODO
.* :li.do a :link reftype=hd refid='GlobalSearch'.global search:elink.
:eul.
:p.You can permanently add directories of help files to the :hp2.HELP:ehp2. or
:hp2.BOOKSHELF:ehp2. paths by modifying the CONFIG&per.SYS (Windows) or
~/.profile (Linux &amp. FreeBSD) file.
:p.
:hp2.Other environment variables:ehp2.
:p.
The :hp2.LANG:ehp2. environment variable is examined under Linux or FreeBSD to
decide the default language that &dv. will be displayed in. Windows doesn't
use an environment variable - instead you need to use the Region applet in
the Windows Control Panel.


.* ************************************************************
.* For Authors and Developers
.* ************************************************************
:h1 res=20 id='ForAuthors'.For Authors and Developers
:hp2.For Authors and Developers:ehp2.
:p.
This section contains some notes for document authors and software
developers.
:p.
Also see the section about URL recognition in the :link reftype=hd
refid='InternetLinks'.Internet Links :elink.topic.

.* ************************************************************
.* Writing Help Files
.* ************************************************************
:h2 res=12 id='WritingHelpFiles'.Writing Help Files
:hp2.Writing fpGUI Toolkit and OS/2 Help Files:ehp2.
:p.
fpGUI Toolkit Help Files are produced using the IPF Compiler. IPF stands for
Information Presentation Facility and was originally designed by IBM.
:p.
The IPF Compiler takes a text file written in a markup language that tells
it about things like headings, links, text and images, and produces
either a .INF or .HLP file.
:p.
The official way to obtain the IPF compiler (ipfc.exe). is from
the OS/2 Developers Toolkit. This is included free with eComStation
(http://www.ecomstation.com). fpGUI doesn't support the OS/2 platform, so we
can't really use the origin IPF Compiler. Luckily for us, open source software
comes to the rescue! A developer from the OpenWatcom (http://www.openwatcom.org) project
implemented a clean room implementation of the IPF Compiler, by simply using the
origin IPF Compiler documentation as a guide. This new compiler is called WIPC, and
is 100% compatible with the original IBM version.

:p.
For your convenience I included the OpenWatcom IPF Compiler as part of fpGUI Toolkit.
You can find pre-built Windows and Linux binaries in the <fpgui>/tools/wipfc/ directory.

:p.
One huge benefit of the WIPFC compiler over the original IBM IPFC version, is that it
is much easier to author help files. With the original IBM version the help text source
was very tedious to write. Every symbol or punctuation like a comma, semi-colon, hyphen etc. had to be
written as special "escaped" symbol. This requirement, which was very labour intensive, was
luckily removed from the WIPFC version.

:p.
fpGUI also includes the IPF Language Reference document (IPFREF_v4.INF) as part
of fpGUI Toolkit. You can find this INF document in the
<fpgui>/docview/docs/ directory. The IPF language is not hard to learn. There are
45 tags in total, but you don't need all of them to start authoring your own help
files. All markup tags (with the exception of 5 tags) start with a colon and end
with a fullstop. The tags are mnemonic, making it easy to assocciate them with
their function.

:p.
There are still some GUI IPF editors out there, but most of them are only for
OS/2 (and maybe for Windows). The IPF Language is actually really easy to learn
and understand, so you don't really need a special editor. I used my programming IDE
as my editor, and simply supplied it with a syntax highlighter for the IPF language.
No extra tools are needed. Just so you know, that is what I used to author this help
document. :-)

:p.
Here is an example of a small IPF document to get you started. I'll also show you
how to compile it into a binary INF file - viewable with &dv. of course. ;-)

:p.
Lets create a text file and call it "sample.ipf". Then type in the following using
your favourite text editor.

:xmp.
  &colon.userdoc.
  &colon.title. Sample INF file...
  &colon.h1.Header One
  &colon.p.This is a sample file with one header in the table of content.
  &colon.p.This is paragraph two of this single header.
  &colon.euserdoc.
:exmp.

:p.
Save the document and close your editor. Now we have the source of our first help
file. Now all that remains is to compile it into a binary INF file.

:p.
To use the WIPC compiler, we need to setup an environment variable so the compiler
can find some of it's support files.

:p.
We do this as follows under Linux.

:xmp.
  export WIPFC=~/apps/wipfc/
:exmp.

:p.
 ...or as follows under Windows.

:xmp.
  set WIPFC=C:\apps\wipfc\
:exmp.

:p.
I normally setup my computer so that those settings are permantantly set when my
computer boots up. I also make sure the "wipfc" executable is setup in the system
search path so I can run it form anywhere on my system. Now we are ready to compile
our help file!

:xmp.
  wipfc -i sample.ipf
:exmp.

:p.
If you typed the document correctly, you should now have
a :hp2.sample.inf:ehp2. file in the current directory. Now you simply
run &dv. and open your first INF help file. Well done!

.* ************************************************************
.* Topic Resource IDs
.* ************************************************************
:h2 res=16 id='TopicResourceIDs'.Topic Resource IDs
:hp2.Topic Resource IDs:ehp2.
:p.
Resource IDs are used by authors of online help for applications, to
identify help topics. When you create fpGUI base applications, the fpGUI
framework will call &dv. with the resource ID as parameter, to display the
correct topic for that part of your application. The resource ID is stored in
a table inside the help file. For a developer to use these, they needs to set
the component's HelpType to htContext.
:p.
For document authors, &dv. offers the ability to see and find
resource IDs.
:p.
:hp2.Finding by Resource ID:ehp2.
:p.
Use "Tools - Find topic by resource ID" to search for a specified resource ID in
all opened files.
:p.
:hp2.Displaying Resource IDs:ehp2.
:p.
Use topic properties (right mouse click in the Contents tree view - "Topic Properties")
to see which resource IDs are associated with a topic.

.* ************************************************************
.* TopicNames
.* ************************************************************
:h2 res=19 id='TopicNames'.Topic Names
:hp2.Topic Names:ehp2.
:p.
Like resource IDs, topic names can be used by developers to link to
help topics from within their application. The developers needs to set the component's
HelpType to htKeyword.
:p.
&dv. can find a particular topic name, using "Tools - Find
topic by resource name".

:euserdoc.
