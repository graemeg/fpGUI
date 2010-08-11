:userdoc.
:docprof toc=123456.
:title.NewView Help
.* ************************************************************
.* Subject: Help for NewView
.* Version:
.* Copyright: Copyright 2004 Aaron Lawrence
.* Copyright: Copyright 2006-2007 Ronald Brill
.* Author: Aaron Lawrence
.* ************************************************************
.*
.*
.* ************************************************************
.* Introduction
.* ************************************************************
:h1 res=30000 id='Introduction'.Introduction
:i1 id=30001.support
:p.:artwork runin name='images\NewView.bmp'.
:hp2.Welcome to NewView&xclm.:ehp2.
:p.
:p.NewView is a program for reading OS&slash.2 &lpar.or eComStation&rpar. Help
Files&per.
:p.:link reftype=hd refid='Support'.Support and Licensing:elink.
:p.:link reftype=hd refid='Using'.Using NewView:elink.
:p.:hp1.To stop this file appearing when you start NewView without specifying a
help file&comma. see Tools &endash. Options &endash. General tab&per.:ehp1.
:p.:hp2.History:ehp2.
:p.NewView replaces the original IBM program supplied with OS&slash.2&per.
:p.It improves upon many aspects of View&comma. with a modern&comma. easy to use
interface&comma. more options&comma. and new features that View simply
didn&apos.t have&per.
:p.
.* ************************************************************
.* Support and Licensing
.* ************************************************************
:h2 res=1 id='Support'.
Support and Licensing
:i1 id=30002.license
:i2 refid=30001.Support and Licensing
:i2 refid=30001.Introduction
:i1 id=30003.bugs
:i1 id=30004.source code
:p.:hp2.Support and Licensing:ehp2.
:p.
:p.NewView is Copyright 1999&endash.2006 Aaron Lawrence&per. It is also licensed
under the GNU Public License&comma. which means you have the right to obtain the
source code&per.
:p.Since 2006 Ronald Brill maintains the product&per.
.br
NewView is a Netlabs project&per.
:p.See the Readme&per.txt for more technical details&per.
:p.See the Changes&per.txt file for a history of changes to NewView&per.
:p.If you find NewView helpful&comma. please email me and&slash.or make a
donation to support further development&per. It&apos.s nice to hear from
you&xclm.
:ul.
:li.Suggestions&comma. compliments or bug reports http&colon.&slash.&slash.svn&per.netlabs&per.org&slash.newview
:li.Translate NewView to your language&per.
:li.A donation to Netlabs http&colon.&slash.&slash.www&per.mensys&per.nl
:eul.
:p.:hp2.Reporting Bugs:ehp2.
:p.If you need to report a crash or other problem&comma. then be as specific as
possible about what files were being used&comma. what you were doing&comma.
etc&per. If one is available&comma. PLEASE include the newview&per.log&per. The
log file will be EITHER
.br
 &endash. in the same directory as NewView itself
.br
 &endash. in the folder set by LOGFILES environment variable &lpar.typically eCS
1&per.1&plus.&rpar.
.br
If it is specific to a particular help file&comma. then send them to me&comma.
unless they&apos.re large &lpar.bigger than 1MB&rpar.&per.
:p.Most of the following information is in the newview&per.log file&comma. but
it would be helpful if you could include it for verification&colon.
:ul.
:li.NewView version &lpar.Help &endash. Product Information&rpar.
:li.The filenames of the help files&per.
:li.A screenshot may be useful&comma. if the problem is an incorrect or corrupt
display&per.
:eul.
:p.:hp2.Why doesn&apos.t my help file work properly?:ehp2.
:p.Some of the less used features of the original View program are not
implemented&per. This is either because I have not got around to it&comma. or
because they are simply not worth the time&per. Examples include
metafiles&comma. index synonyms&comma. the entire application control API&comma.
and so on&per.
:p.Unfortunately&comma. it seems that at least one developer has used every one
of these features&comma. so you may find an occasional file that doesn&apos.t
load or doesn&apos.t work properly&per.
.*
.*
.* ************************************************************
.* Using NewView
.* ************************************************************
:h1 res=2 id='Using'.
Using NewView
:p.:hp2.Using NewView:ehp2.
:p.Once you have :link reftype=hd refid='OpeningFiles'.opened a
file:elink.&comma. you can read it in various ways&per.
:p.You can read the :link reftype=hd refid='contents'.table of
contents:elink.&comma. use the :link reftype=hd refid='Index'.alphabetical
index:elink.&comma. or :link reftype=hd refid='search'.search:elink.&per.
:p.To simply read the help file like a paper book&comma. use the &odq.Previous&cdq.
:artwork runin name='images\previous.bmp'.
 and &odq.Next&cdq.
:artwork runin name='images\next.bmp'.
 buttons to work your way through all the topics&per.
:p.You can also use the help file like web pages&comma. using &odq.Back&cdq.
:artwork runin name='images\back.bmp'.
 and &odq.Forward&cdq.
:artwork runin name='images\forward.bmp'.
 buttons to go back to wherever you were before&comma. or to retread your
steps&per.
:p.Colours and some of the behaviour of NewView can be adjusted from the Tools
&endash. Options menu&per.
:p.You can also :link reftype=hd refid='notes'.annotate:elink. or :link
reftype=hd refid='bookmarks'.bookmark:elink. topics&per.
.*
.*
.* ************************************************************
.* Opening Help File
.* ************************************************************
:h1 res=3 id='OpeningFiles'.
Opening Files
:i1 id=30005.open
:p.:hp2.Opening Help Files:ehp2.
:p.
:p.To open a help file&comma. you can use any of the following&colon.
:p.&endash. Double&endash.click a :link reftype=hd refid='HelpIcons'.help
icon:elink. that is already set up
:p.&endash. Type &odq.view :hp1.filename:ehp1.&cdq. from the :link reftype=hd
refid='CommandLine'.command line:elink.
:p.&endash. Click the Open button
:artwork runin name='images\open.bmp'.
 from within NewView
:p.&endash. Reload a recently viewed file from the &odq.File&cdq. menu
:p.&endash. Drag and drop a Help file from the desktop
:p.Once the file is loaded&comma. you should see the :link reftype=hd
refid='contents'.table of contents:elink. and the first topic&per.
:note text='Note:'.This assumes you installed NewView as a replacement
for original View&per. If you didn&apos.t then existing help icons and the
command line may behave differently&per.
:p.:hp2.Loading Multiple Files Together:ehp2.
:p.NewView can load multiple files at once&comma. presenting them as if they
were one book&comma. and read environment variables for filenames&per.
:p.For example&comma. with the OS&slash.2 Developer&apos.s Toolkit
documentation&colon.
.br
  NewView cpref
.br
loads the &odq.Control Program Guide and Reference&cdq.&per. CPREF is an environment
variable set in config&per.sys&comma. consisting of &odq.CP1&plus.CP2&plus.CP3&cdq.
which tells NewView &lpar.or View&rpar. to load the help files CP1&comma. CP2
and CP3&per. The files are searched for in the path specified by two :link
reftype=hd refid='L_EnvironmentVariables'.environment
variables:elink.&per.&asterisk.
:p.The files are all loaded and effectively appended to each other&per.
:p.Being able to load multiple files like this can be helpful for various
reasons&per. For example&comma. 4OS&slash.2 &lpar.a CMD&per.EXE
replacement&rpar. uses it to add it&apos.s own help on top of the original CMD
help&per. You can do it yourself with any files you like&per.
:p.You can load multiple files in the Open File dialog by using Ctrl or Shift to
select multiple files&per.
:p.When you click a link to a different help file&comma. NewView loads the other
file without closing your current files&per.
:p.At any time&comma. you can find out what files are open by using File
&endash. Information&per.
:p.:hp2.Loading Additional Files:ehp2.
:p.You can tick the &odq.Keep current files open&cdq. checkbox in the Open File
dialog&comma. and NewView will open the files you have selected without closing
the currently opened files&per.
:p.:hp2.Drag and Drop:ehp2.
:p.You can drag and drop &per.INF or &per.HLP files onto NewView and they will
be opened&per. If you hold down the Shift key&comma. they will be opened without
closing the current files&per.
:p.You can drop files onto any of the main content areas&comma. such as the
Contents or Index windows&comma. or an existing topic window&per.
:note text='Note:'.Some links that go across files&comma. will only work if the correct set of
files is loaded&per.
.* ************************************************************
.* Help Icons
.* ************************************************************
:h2 res=17 id='HelpIcons'.
Help Icons
:p.:hp2.Help Icons:ehp2.
:p.Help Icons on the desktop are usually &odq.program objects&cdq. with the program name
set to &odq.view&per.exe&cdq. and the parameters set to the name of the help files&per.
:p.Some programs create these icons automatically at install time&per.
:p.You can create these icons yourself by using the desktop program
template&per. See desktop help for more information&per.
:p.If you create icons by dragging help files to the desktop&comma. then you
cannot give them a meaningful title&comma. because that would change the name of
the file&comma. which might prevent programs from finding the help file&per.
Therefore program objects are currently the recommended means of creating help
icons&per.
.*
.*
.* ************************************************************
.* Navigation Panel Tabs
.* ************************************************************
:h1 res=200 id='NavigationPanel'.
Navigation Panel Tabs
:p.:hp2.Navigation Panel Tabs:ehp2.
:p.The left hand panel contains several tabs for moving through the current help
file in different ways&per.
:p.:link reftype=hd refid='contents'.Contents:elink.
:p.:link reftype=hd refid='Index'.Index:elink.
:p.:link reftype=hd refid='search'.Search:elink.
:p.:link reftype=hd refid='notes'.Notes:elink.
:p.You can turn this panel off to get more space&comma. with the button
:artwork runin name='images\navigator.bmp'. or by selecting View
&endash. Show Left Panel from the menu&comma. or pressing Alt&plus.P&per. Do the
same to turn it on again&per.
:p.You can stop the navigation panel from appearing when a help file is opened
in Tools &endash. Options &endash. General tab&per.
:note text='Note:'.Many programs choose to show the table of contents
when they open their help file&semi. in this case&comma. the panel is
automatically shown&comma. overriding your setting&per.
.* ************************************************************
.* Contents View
.* ************************************************************
:h2 res=4 id='contents'.
Contents View
:i1 id=30006.Table of Contents
:p.:hp2.Table of Contents:ehp2.
:p.
:p.Most help files have a table of contents that shows you the topics in the
file&comma. in a hierarchy or &odq.tree&cdq.&per. This is usually the first view you see
when you open a file&per.
:p.You can expand or collapse branches on the tree by clicking the &plus. or
&endash. buttons&comma. or using the space bar&per.
:p.To view a topic from the contents&comma. just click on it&per. You can also
move through the contents by using the arrow keys&per.
:p.To move through :hp2.all:ehp2. topics in the contents tree&comma. in
order&comma. you can use Ctrl &plus. Up and Ctrl &plus. Down&comma. or the
&odq.Previous&cdq.
:artwork runin name='images\previous.bmp'.
 and &odq.Next&cdq.
:artwork runin name='images\next.bmp'.
 buttons&per. This is one way to treat the file as a normal book&comma. reading
through each page&per.
:p.You can also review the whole table of contents by using &odq.Expand All&cdq. in the
&odq.View&cdq. menu&per. This expands all the branches of the contents table so you can
quickly look through it&per. However&comma. it&apos.s usually easier to use
:link reftype=hd refid='search'.Search:elink. or :link reftype=hd
refid='Index'.Index:elink. for this purpose&per.
.* ************************************************************
.* Index
.* ************************************************************
:h2 res=5 id='Index'.
Index
:p.:hp2.About the Index:ehp2.
:p.
:p.The Index tab contains an alphabetical listing of topics or keywords in the
help file&per. You can quickly search through it just by typing the first few
characters of the word you want to look up&per. NewView jumps to the first match
in the index automatically&per. To view the highlighted topic&comma. press
enter&per.
:note text='Note:'.
:p.Help files may or may not include an &odq.official&cdq. index&per. The index is
manually created by the author&comma. so &lpar.for original View&rpar. it&apos.s
usefulness is strictly dependent on how much work the author put into it&per.
There may not even be one&per.
:p.However&comma. a useful index can be provided simply by listing the titles of
each topic alphabetically&comma. and this is what NewView does&per. It then
merges the original index &lpar.if any&rpar. with the list of topic titles&per.
:p.If for some reason you don&apos.t like this&comma. you can turn it off in
Tools &endash. Options &endash. Index tab&per.
.* ************************************************************
.* Search
.* ************************************************************
:h2 res=6 id='search'.
Search
:p.:hp2.Searching:ehp2.
:p.
:p.Searching is a quick way to find information&comma. when you don&apos.t know
where to start&per. Simply go to the Search tab&comma. type some related words
and click the Search button&per.
You&apos.ll see a listing of all topics containing that word&comma. or words
like it&comma. with the best matches at the top&per. The best match will be
displayed automatically&per.
:p.Words that matches your search are highlighted in the topic&per.
:p.:hp2.Global search:ehp2.
:p.You can also search all help files on your system using the :link reftype=hd
refid='GlobalSearch'.global search:elink. in Tools &endash. Search all Help
Files&per.
:p.:hp2.Phrase search:ehp2.
:p.If you want to search for a phrase made up of more than one word&comma. put
double quotes around it&comma. for example &odq.os&slash.2 warp&cdq.&per.
:p.:hp2.Matching features:ehp2.
:p.NewView allows you finer control of searching&per.
:p.&plus. indicates a word that :hp2.must:ehp2. be matched
:p.&endash. indicates a word that must :hp2.not:ehp2. be matched
:p.NewView always does partial word matches&per. That is&comma. if you search
for &odq.win&cdq. NewView will also find &odq.window&cdq. and &odq.showing&cdq.&per. However&comma. the
better the match is the higher the rank will be&per.
:p.:hp2.How NewView ranks results:ehp2.
:p.NewView ranks matching topics by various means&colon.
:p.&endash. a closer match to a full word
:p.&endash. number of matching words in a topic
:p.&endash. matches within the title
:p.&endash. matches within an index entry
.* ************************************************************
.* Notes
.* ************************************************************
:h2 res=7 id='notes'.
Notes
:p.:hp2.Adding and Using Notes:ehp2.
:p.
:p.NewView allows you to add notes &lpar.annotations&rpar. to your help
files&per.
:p.To add a note&comma. simply click where you want to make a note and click the
&odq.Note&cdq. button
:artwork runin name='images\note.bmp'.
&comma. then type your text and click OK&per. The text will be inserted into the
help topic with a different color &lpar.default is green&semi. you can change
this in Tools &endash. Options &endash. Colors&rpar.&per.
:p.To edit or delete a note&comma. click on the colored note text&semi. you can
then edit the note text&comma. or click on delete to get rid of it&per.
:p.You can also review all the notes that you&apos.ve made in the current help
file&lpar.s&rpar. by going to the Notes tab&semi. this allows you to add&comma.
edit and delete&comma. and also jump to the topics containing your notes&per.
:p.
:p.Notes are saved in a file with the extension &per.nte&comma. in the same
directory as the help file they are for&per.
:note text='Note:'.If a help file is changed &lpar.for example a program is
upgraded&rpar. then notes will no longer appear in the correct place&semi.
however&comma. you can still read them from the Notes tab&per.
.*
.*
.* ************************************************************
.* Global Search
.* ************************************************************
:h1 res=18 id='GlobalSearch'.
Global Search
:p.:hp2.Global search:ehp2.
:p.You can search all help files on your system&comma. by clicking the Search
All button
:artwork runin name='images\search.bmp'.
&comma. using Tools &endash. Search all Help Files in the menu&comma. or
pressing Ctrl&plus.S&per.
:p.This search works similarly to :link reftype=hd refid='search'.searching
within a file:elink.&comma. but it also tells you what help file the results
were found in&per.
:p.These searches may take some time&comma. depending on what you search
for&per. You can stop the search at any time&per.
:p.:hp2.Where NewView searches:ehp2.
:p.The default is to search for help files in the help paths&comma. which are
specified by the BOOKSHELF and HELP :link reftype=hd
refid='L_EnvironmentVariables'.environment variables:elink.&per.
:p.You can choose other places to search by choosing from the drop&endash.down
list&comma. or customise the list by clicking the Select button&per.
:p.:hp2.Search in&colon. Standard Help Paths:ehp2.
:p.This is the default and will search BOOKSHELF and HELP as specified
above&per.
:p.Clicking the select button will allow you to choose which of the directories
in the help paths will be used&per. Click each item in the list to select or
de&endash.select it&per. After you choose this&comma. the location will show as
&odq.Selected Help Paths&cdq.
:p.:hp2.Search in&colon. All Hard Drives:ehp2.
:p.This option will search all hard &lpar.non&endash.removable&rpar. drives on
your system&per. You can click &odq.Select&per.&per.&per.&cdq. to customise the
location&per.
:p.Searching drives can find more help files&comma. but might be much slower
than just the help paths&per.
:p.:hp2.Search in&colon. Selected Help Paths:ehp2.
:p.If you have already selected particular help paths to search&per.&comma. you can
click &odq.Select&per.&per.&per.&cdq. to customise again&per.
:p.:hp2.Search in&colon. Directory List:ehp2.
:p.In the &odq.Select Directories&cdq. dialog&comma. clicking the &odq.Add&per.&per.&per.&cdq.
button will allow you to add one or more directories to the search list&per.
:p.Select the drive and directory using the controls that appear&comma. then
click &odq.&lt. Add Directory&cdq. to add the chosen directory&per. You can do this as
many times as you like&per. Choose &odq.With sub&endash.directories&cdq. if you want
sub&endash.directories of the selected directory to be searched as well&per. In
this case&comma. &per.&per.&per. will show on the end of the directory&per.
:p.After you add a custom directory like this&comma. the location for searching
will show as &odq.Directory List&cdq.&per.
:note text='Note:'.If you add a custom directory to standard or selected help
paths&comma. then the list will become a custom list&comma. and you can no
longer re&endash.select help paths&per. To get back to the original help
paths&comma. choose &odq.Standard Help Paths&cdq. then click &odq.Select&per.&per.&per.&cdq.
again&per.
:p.:hp2.Search in&colon. Typing a location:ehp2.
:p.You can type a drive or directory into the &odq.Search in&colon.&cdq. entry
field&per. Add &odq.&per.&per.&per.&cdq. on the end of the directory if you want to
search subdirectories as well&per.
:p.Example&colon.
:p. Search in&colon. &lbracket. E&colon.&bsl.mydocs&bsl.&per.&per.&per.
 &rbracket.
:p.This will search help files in E&colon.&bsl.mydocs&bsl. and any
subdirectories&per.
.*
.*
.* ************************************************************
.* Bookmarks
.* ************************************************************
:h1 res=8 id='bookmarks'.
Bookmarks
:p.:hp2.Bookmarks:ehp2.
:p.
:p.NewView allows you to bookmark particular topics within the current help
file&per. Simply click the bookmark button
:artwork runin name='images\bookmark.bmp'.
 to add the current topic as a bookmark&per.
:p.To jump to a bookmark&comma. go to the &odq.Bookmarks&cdq. menu&comma. and click on
the bookmark you want to open&per.
:p.You can view or delete all your bookmarks by clicking on &odq.Edit&per.&per.&cdq. in
the &odq.Bookmarks&cdq. menu&per. This window can remain open while you read&comma. so
that you can quickly look through your bookmarks&per.
:p.
:note text='Note:'.
:p.NewView bookmarks remember all the topic windows that are open&comma. if
there is more than one&per.
:p.Bookmarks are saved in a file with the extension &per.bmk&comma. in the same
directory as the help file they are for&per.
.*
.*
.* ************************************************************
.* Internet Links
.* ************************************************************
:h1 res=100 id='InternetLinks'.
Internet Links
:p.:hp2.Internet Links:ehp2.
:p.When you click a web URL like
http&colon.&slash.&slash.www&per.google&per.com&comma. NewView launches your
default web browser&per.
:p.This web browser is specified by operating system settings&comma. not NewView
itself&per. To configure it&comma. you can open a URL object on the
desktop&comma. edit the browser path in the :hp2.Browser:ehp2. tab&comma. then
click Set Default&per. Alternatively&comma. download the utility ConfigApps from
Hobbes
:p.
 http&colon.&slash.&slash.hobbes&per.nmsu&per.edu&slash.cgi&endash.bin&slash.h&endash.search?key&eq.configapps
.br
:p.Browsers may also have the ability to make themselves the default&comma.
either at install time or in preferences&per.
:p.Email&comma. Newsgroups and FTP links are also passed to the web browser&per.
At this time&comma. it isn&apos.t possible to choose a different program for
these links&per.
:p.:hp2.Note to help file authors:ehp2.
:p.The original View had no understanding of URL or email links&comma. so the
only way to implement them was a link to&comma. for example&comma.
&odq.netscape&per.exe&cdq. with the correct parameters&per.
:p.NewView translates program links to &odq.netscape&cdq.&comma. &odq.explore&cdq. or &odq.mozilla&cdq.
into links to the default browser&per.
:p.It also auto&endash.detects URLs in the forms&colon.
:p. http&colon.&slash.&slash.x  https&colon.&slash.&slash.x
 ftp&colon.&slash.&slash.x
:p. mailto&colon.x  news&colon.x
:p.Things that look like URLs are also detected&comma. even without the protocol
prefix&colon.
:p. www&per.a&per.b &endash. browser
:p. ftp&per.a&per.b &endash. ftp
:p. a&atsign.b&per.c &endash. email
:p.where a&comma. b and c are any alphanumeric string&per.
:p.You don&apos.t need to do anything for NewView to recognise these&per.
.*
.*
.* ************************************************************
.* Command Line
.* ************************************************************
:h1 res=9 id='CommandLine'.
Command Line Parameters
:p.:hp2.Command Line Parameters:ehp2.
:p.When you run NewView from the command line you can supply various
parameters&per. None of them are required&per.
:p.
:p.:hp2.NewView &lbracket.options&rbracket. &lbracket.&lt.filename&gt.
&lbracket.&lt.search text&gt.&rbracket.&rbracket.:ehp2.
:p.If NewView is installed as a replacement for view&comma. then the command
starts with view instead of newview&per.
:p.:link reftype=hd refid='CommandLineExamples'.Examples:elink.
.*
:p.:hp2.&lt.filename&gt.:ehp2.
:p.The file for NewView to load&per. You can load multiple files at once by
using filename1&plus.filename2 etc&per.
.br
If a path isn&apos.t specified&comma. then the files are searched for in the
:link reftype=hd refid='L_EnvironmentVariables'.BOOKSHELF and HELP
paths:elink.&per.
.br
If you path and&slash.or filename contains special characters (like blank) then
you have to enclose the filename in double qoutes&per.
.*
:p.:hp2.&lt.search text&gt.:ehp2.
:p.Search topic titles and index entries for this text&per. This is
:hp2.not:ehp2. the same as a normal search&comma. for compatibility with
original View&per. To do a proper search use the &slash.s option &lpar.see
below&rpar.&per. For more details&comma. see :link reftype=hd
refid='CommandLineTopicSearch'.Command Line Topic Search:elink.&per.
:p.:hp2.Options:ehp2.
:p.:hp2.&slash.s:ehp2.
:p.After opening the file&comma. performs a :link reftype=hd
refid='search'.search:elink. for the given text (do a real full text search intead of
the default topic titles search)&per. The result is the same as performing the search from
the :link reftype=hd
refid='search'.serach navigation panel:elink.&per.
:p.Example&colon.
.br
:lm margin=4.
To search for copy in the whole cmdref document you can call
:xmp.
  newview &slash.s cmdref copy
:exmp.
NewView is clever enought to handle multiple words (like the :link reftype=hd
refid='search'.serach navigation panel:elink.)&per. This is a OR search&per.
:xmp.
  newview &slash.s cmdref net access
:exmp.
To perform a AND search enclose the search phrase in double quotes&per.
:xmp.
  newview &slash.s cmdref &odq.net access&cdq.
:exmp.
:lm margin=1.
.*
:p.:hp2.&slash.g:ehp2.
:p.Performs a :link reftype=hd refid='GlobalSearch'.global search:elink. for the
given text&comma. on all the help files in your system&per.
:p.Example&colon.
.br
:lm margin=4.
To search for copy in all help files use
:xmp.
  newview &slash.g copy
:exmp.
Provide the file name as first parameter if you like to open a help file
before the search starts.
:xmp.
  newview &slash.g cmdref copy
:exmp.
:lm margin=1.
.*
:p.:hp2.&slash.?:ehp2. or :hp2.&slash.h:ehp2. or :hp2.&slash.help:ehp2.
:p.Show command line help
:p.See also&colon. :link reftype=hd refid='AdvancedParameters'.Advanced
Parameters:elink.
.* ************************************************************
.* Command Line Topic Search
.* ************************************************************
:h2 res=13 id='CommandLineTopicSearch'.
Command Line Topic Search
:p.:hp2.Command line topic search:ehp2.
:p. view &lt.filename&gt. &lt.topic&gt.
:p.The topic search parameter specified on the command line&comma. copies the
behaviour of old view&per.
:p.Text within topics is not searched&comma. only titles and index entries&per.
This makes it less useful to humans&comma. but is used by some programs to
reference help topics in a predictable way&per.
:p.You can use multiple words here&per.
:p.The search performed is&colon.
:p.&endash. topic title starts with search text
:p.&endash. index entry starts with search text
:p.&endash. topic title contains search text
:p.&endash. index entry contains search text&per.
:p.Developers should make sure that the expected document will be found if using
this technique to identify topics when starting New or Old view&per.
.* ************************************************************
.* Advanded Parameters
.* ************************************************************
:h2 res=14 id='AdvancedParameters'.
Advanced Parameters
:p.:hp2.Advanced Parameters:ehp2.
:p.The following command line parameters are intended mainly for software
developers&comma. but can be used for any purpose&per.
:p.:hp2.&slash.lang&colon.&lt.language spec&gt.:ehp2.
:p.Loads the specified language&per. Overrides the default chosen based on the
LANG environment variable&per. For example&comma.
:xmp.
  newview cmdref &slash.lang&colon.en
:exmp.
loads English&per. See readme&per.txt for more information&per.
:p.:hp2.&slash.pos&colon.&lt.left&gt.&comma.&lt.bottom&gt.&comma.&lt.width&gt.&comma.&lt.height&gt.:ehp2.
:p.Set the main program window to the given position and size&per. All values
must be given&per. Put a :hp2.P:ehp2. after a number to specify a
percentage&per. For example&colon.
:xmp.
  newview &slash.pos&colon.10P&comma.10P&comma.80P&comma.80P
:exmp.
:p.makes the window centered and 80&percent. of the screen size&per.
:p.:hp2.&slash.title&colon.&lt.window title&gt.:ehp2.
:p.Sets the title of the NewView window to the specified text&comma. overriding
whatever appears in the help file&per. The text &odq.Help &endash.  &cdq. will always be
inserted in front of the specified text&comma. unless the specified text is
&odq.help&cdq.&comma. in which case the title will simply become &odq.Help&cdq.&per. This is to
make sure that help windows are always obvious as such in the window list&per.
:p.If you need to specify multiple words&comma. surround the entire option with
quotes&comma. for example&colon.
:xmp.
  newview cmdref &odq.&slash.title&colon.Command Line Help&cdq.
:exmp.
.* ************************************************************
.* Command Line Examples
.* ************************************************************
:h2 res=15 id='CommandLineExamples'.
Command Line Examples
:p.:hp2.Command Line Examples:ehp2.
.*
:p.The following examples assume that newview is installed as a complete
replacement and therefore view is actually newview&per.
:p.:hp2.view cmdref:ehp2.
:lm margin=4.
:p.Open the file cmdref&per.inf &lpar.OS&slash.2 Command Reference&rpar. from
the help path&per.
:lm margin=1.
.*
:p.:hp2.view cmdref&plus.os2ug:ehp2.
:lm margin=4.
:p.Open two files&comma. cmdref&per.inf and os2ug&per.inf &lpar.OS&slash.2 User
Guide&rpar.&comma. in the same window&per.
.br
The table of contents from os2ug&per.inf is added to the end of the contents
from cmdref&per.inf&per. The indexes are combined alphabetically&per.
:lm margin=1.
.*
:p.:hp2.view c&colon.&bsl.os2&bsl.book&bsl.os2ug&per.inf:ehp2.
:lm margin=4.
:p.Open the file os2ug&per.inf in the c&colon.&bsl.os2&bsl.book directory&per.
:lm margin=1.
.*
:p.:hp2.view &odq.c&colon.&bsl.os2 book&bsl.os2ug&per.inf&cdq.:ehp2.
:lm margin=4.
:p.Surround path&bsl.file with double quotes if they contain special characters (like blank)&per.
:lm margin=1.
.*
:p.:hp2.view cmdref dir:ehp2.
:lm margin=4.
:p.Open the file cmdref &lpar.OS&slash.2 command reference&rpar. and look in
titles and index for the word &odq.dir&cdq.&per. Will show the help page for the DIR
command&per.
:lm margin=1.
.*
:p.:hp2.view &slash.s os2ug desktop:ehp2.
:lm margin=4.
:p.Open the file os2ug&per.inf and search for the word &odq.desktop&cdq.&per. The best
match is shown&per.
:lm margin=1.
.*
:p.:hp2.view &slash.g permissions:ehp2.
:lm margin=4.
:p.Performs a search of all help files for the word &odq.permissions&cdq.&per.
:lm margin=1.
.*
:p.:hp2.set myhelp&eq.cmdref&plus.os2ug&plus.rexx:ehp2.
.br
:hp2.view myhelp:ehp2.
:lm margin=4.
:p.The first line sets an environment variable MYHELP to contain the names of
three help files&per. The second line opens the three files&per.
:lm margin=1.
.*
.*
.* ************************************************************
.* Keyboard Shortcuts
.* ************************************************************
:h1 res=10 id='KeyboardShortcuts'.
Keyboard Shortcuts
:p.:hp2.Keyboard Shortcuts:ehp2.
:p.Most keyboard shortcuts are visible in the menu&comma. but a few are not&per.
The additional shortcuts are&colon.
:p.:hp2.Alt&plus.F4:ehp2. Exit
:p.:hp2.Ctrl&plus.C:ehp2. Copy selected text to clipboard
:p.:hp2.F7:ehp2. Back
:p.:hp2.F8:ehp2. Forward
:p.:hp2.Ctrl&plus.Left:ehp2. Back
:p.:hp2.F11:ehp2. Previous in contents
:p.:hp2.F12:ehp2. Next in contents
:p.
:p.:hp2.Shortcuts visible in the menu:ehp2.
:p.:hp2.Ctrl&plus.O:ehp2. Open files
:p.:hp2.Ctrl&plus.E:ehp2. Open files from help paths
:p.:hp2.Ctrl&plus.N:ehp2. Open a new window
:p.:hp2.Ctrl&plus.P:ehp2. Print topic
:p.:hp2.F3:ehp2. Exit
:p.
:p.
:p.:hp2.Ctrl&plus.A:ehp2. Select all text in topic
:p.:hp2.Ctrl&plus.Ins:ehp2. Copy selected text to clipboard
:p.
:p.:hp2.Ctrl&plus.F:ehp2. Find within current topic
:p.:hp2.Ctrl&plus.G:ehp2. Repeat last find
:p.
:p.:hp2.Ctrl&plus.S:ehp2. Open global search tool
:p.
:p.:hp2.Alt&plus.C:ehp2. Change to the contents tab
:p.:hp2.Alt&plus.I:ehp2. Change to the index tab
:p.:hp2.Alt&plus.S:ehp2. Change to the search tab
:p.:hp2.Alt&plus.N:ehp2. Change to the notes tab
:p.:hp2.Alt&plus.P:ehp2. Toggle the left panel &lpar.tabs&rpar. on and off
:p.:hp2.F5:ehp2. Expand all contents
:p.:hp2.F6:ehp2. Collapse all contents
:p.
:p.:hp2.Esc:ehp2. Back
:p.:hp2.Ctrl&plus.Right:ehp2. Forward
:p.:hp2.Ctrl&plus.Up:ehp2. Previous topic in contents
:p.:hp2.Ctrl&plus.Down:ehp2. Next topic in contents
:p.
:p.:hp2.Ctrl&plus.D:ehp2. Edit bookmarks
:p.:hp2.Ctrl&plus.B:ehp2. Bookmark current topic
:p.
:p.:hp2.Ctrl&plus.M:ehp2. Add note at cursor position
:p.
:p.:hp2.F1:ehp2. Help for NewView
.*
.*
.* ************************************************************
.* Environment Variables
.* ************************************************************
:h1 res=11 id='L_EnvironmentVariables'.
Environment Variables
:p.:hp2.Environment Variables:ehp2.
:p.
:p.Both the :hp2.BOOKSHELF:ehp2. and :hp2.HELP:ehp2. environment variables
define paths &lpar.lists of directories&rpar. for searching for help files&per.
NewView uses both paths without distinction&per.
:p.These paths are searched when you&colon.
:p.o specify a help file without a path on the command line
:p.o use the File &endash. Open Special&per.&per.&per. menu item
:p.o do a :link reftype=hd refid='GlobalSearch'.global search:elink.
:p.You can permanently add directories of help files to the :hp2.HELP:ehp2. or
:hp2.BOOKSHELF:ehp2. paths by modifying the CONFIG&per.SYS file&per. Add to both
paths&comma. if you also want old view to be able to find the files&per.
:p.:hp2.Other environment variables:ehp2.
:p.The :hp2.LANG:ehp2. environment variable is examined to decide the default
language that NewView will be displayed in&per. &lpar.Overridden by the
&slash.lang :link reftype=hd refid='AdvancedParameters'.command line
parameter:elink.&per.&rpar. See the newview readme&per.txt for more information
about languages&per.
:p.The directory defined in :hp2.LOGFILES:ehp2. is used for logging crashes or
other information&per.
:p.The subdirectory &odq.lang&cdq. under the directory defined by :hp2.OSDIR:ehp2. is
searched for language files at startup&per.
:p.The path :hp2.ULSPATH:ehp2. is also searched for language files&per.
.*
.*
.* ************************************************************
.* For Authors and Developers
.* ************************************************************
:h1 res=20 id='ForAuthors'.
For Authors and Developers
:p.:hp2.For Authors and Developers:ehp2.
:p.
:p.This section contains some notes for document authors and software
developers&per.
:p.Also see the section about URL recognition in the :link reftype=hd
refid='InternetLinks'.Internet Links :elink.topic&per.
.* ************************************************************
.* Writing Help Files
.* ************************************************************
:h2 res=12 id='WritingHelpFiles'.
Writing Help Files
:p.:hp2.Writing OS&slash.2 Help Files :ehp2.
:p.
:p.OS&slash.2 Help Files are produced using the IPF Compiler&per. IPF stands for
Information Presentation Facility&per.
:p.The IPF Compiler takes a text file written in a language that tells it about
things like headings&comma. links&comma. text and images&comma. and produces
either a &per.INF or &per.HLP file&per.
:p.The official way to obtain the IPF compiler &lpar.ipfc&per.exe&rpar. is from
the OS&slash.2 Developers Toolkit&per. This is included free with eComStation
&lpar.http&colon.&slash.&slash.www&per.ecomstation&per.com&rpar.&per.
:p.Since the language for IPFC is tedious &lpar.for example all punctuation must
be typed as special keywords&comma. like &amp.comma&per.&rpar. many people use
other tools besides the IPF compiler itself&per.
:p.I use Vyperhelp
&lpar.http&colon.&slash.&slash.www&per.vyperhelp&per.com&rpar. since it is
simple and graphical&per. It can also export to Windows Help&comma. HTML and
others&comma. though it only runs on OS&slash.2&per. Not free&per.
:p.Some other popular options are&colon.
:p.o HyperText&slash.2 IPF Preprocessor
&lpar.http&colon.&slash.&slash.www&per.clanganke&per.de&slash.os2&slash.sw&slash.htext&slash.
.br
&rpar. &endash. preprocesses a simpler starting language into the very difficult
IPF format&per. Free&per.
:p.o HyperMake
&lpar.http&colon.&slash.&slash.www&per.hypermake&per.com&rpar.&per.
Similar&comma. but can also produce Windows Help and HTML&per.
:p.o Sibyl &lpar.which NewView was created with&rpar. comes with an IPF
preprocessor&per.
:p.o IPFEditor from PCS
&lpar.http&colon.&slash.&slash.www&per.pcs&endash.soft&per.com&slash.productipfe212&per.htm
.br
&rpar.&per. Probably the most complete&comma. but significant cost&per.
Note&colon. NewView doesn&apos.t support everything IPFE can do&xclm.
.br
In the past there were many other options&per. Those listed should still be
available and have some support&per.
.* ************************************************************
.* Topic Resource IDs
.* ************************************************************
:h2 res=16 id='TopicResourceIDs'.
Topic Resource IDs
:p.:hp2.Topic Resource IDs:ehp2.
:p.Resource IDs are used by authors of online help for applications&comma. to
identify help topics&per. Applications call the Help Manager specifying a
resource ID&comma. either directly using the HM&us.DISPLAY&us.HELP
message&comma. or indirectly via help tables added to their resources&comma.
which PM automatically handles&per. The resource ID is stored in a table inside
the help file&per.
:p.For document authors&comma. NewView offers the ability to see and find
resource IDs&per.
:p.:hp2.Finding by Resource ID:ehp2.
:p.Use Tools &endash. Find Resource ID to search for a specified resource ID in
all opened files&per.
:p.:hp2.Displaying Resource IDs:ehp2.
:p.Use topic properties &lpar.right mouse click &endash. Properties&rpar. to see
which resource IDs are associated with a topic&per.
.* ************************************************************
.* TopicNames
.* ************************************************************
:h2 res=19 id='TopicNames'.
Topic Names
:p.:hp2.Topic Names:ehp2.
:p.
:p.Like resource IDs&comma. topic names can be used by developers to link to
help topics from within their application&comma. using the HM&us.DISPLAY&us.HELP
message with parameter 2 being HM&us.PANELNAME&per.
:p.These are not so often used&per.
:p.NewView can find a particular topic name&comma. using Tools &endash. Find
Topic Name&per.
:euserdoc.
