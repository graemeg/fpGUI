.* This file is encoded using IBM850 encoding
.* :encoding=IBM850:
:userdoc.
:docprof toc=1234.
:title.A Quick Guide to fpGUI

.* This as a marco definition
.nameit symbol=dv text='DocView'

.* This is a comment
:h1.A Quick Guide to fpGUI
:hp2.A Quick Guide to fpGUI:ehp2.
:p.By Graeme Geldenhuys
:artwork align=left name='qguide_images/wizard_pink.bmp'.

:table cols="13 20".
:row.
:c.Version
:c.v0.1
:row.
:c.Pub Date
:c.August 2015
:etable.



:h2.Preface
I've been meaning to start this book back in 2013, but always seemed to find
other things to do. I've now finally decided to put pen to paper and get
going - even if it is just a draft document for the time being.
:p.
:hp2.Who This Book Is For:ehp2.
:p.I've written this book for programmers who want to learn to use the fpGUI
toolkit, and that might not have used it, or similar tooolkits, before.
:p.
This book covers using fpGUI Toolkit directly (no LCL involved). The Lazarus
LCL-fpGUI widgetset is not feature complete, and not nearly ready for real usage. My hope is that somebody will eventually take control of that aspect of
LCL and make it into a usable LCL target.
:p.
But for now, lets look at fpGUI itself!

:h2.Colophon
This is the first book that I wrote using IPF and related technologies. The
master text was written primarily with the EditPad Pro text editor and my
custom IPF syntax colour scheme and file navigation.

:h2.A Brief History of fpGUI
After developing many cross platform applications with Kylix and Delphi
I started getting very frustrated with the differences between the look and
behavior of the applications under Linux and Windows. The code was also
riddled with IFDEF statements.
:p.
Then I stumbled across the Free Pascal and Lazarus projects. I thought this
is it, the answer to all my cross platform development problems. Unfortu-
nately after working with Lazarus for a few months I started finding more
and more issues with the widget sets, though the IDE was great.
:p.
The Lazarus LCL is a wrapper for each platforms native widget set. This
brought with it the same issues I experienced with Kylix and Delphi. This
got me thinking about how I could resolve this issue.
:p.
Then it hit me - implement the widget set myself using Free Pascal! Painting
the widgets myself to get a consistent look and implementing a consistent
behaviour. Instead of reinventing the wheel, I thought I would do some
searching to see if there is another project I can contribute to or help by
giving me a head start.
:p.
The first version of my widget set was based around a heavily modified
version of the Light Pascal Toolkit [http://sourceforge.net/projects/lptk]. I then discovered the discontinued
fpGUI and fpGFX projects. I tried to contact the original author to no
avail. The fpGUI code hasn?t been touched in four years (since early 2002).
After studying the code for a few weeks, I came to the conclusion that fpGUI
is much closer to what I strived to accomplish with my modified LPTK. A
lot was still missing from fpGUI though.
:p.
After thinking long and hard, I decided to start my widget set again, but
this time based on the work done in fpGUI and fpGFX. I also added to
the mix some good ideas I saw in Qt 4.1. So far I have completed quite a
few things missing in fpGUI, but I still need to do a lot to get to the point
where I can test drive it in a commercial application. I set myself a list of
things outstanding which should get it to a usable level. I also added a lot of
documentation as I went as there was no documentation included with the
original fpGUI and fpGFX projects. Documentation is important to attract
other developers in using the widget set.

:h2.Part I: Basic fpGUI
:h3.Getting Started
:h3.Creating Dialogs
:h3.Creating Main Windows
:h3.Implementing Application Functionality
:h3.Creating Custom Widgets
:h2.Part II: Intermediate fpGUI
:h3.Layout Management
:h3.Event Processing
:h3.2D Graphics with AggPas
:h3.Drag and Drop
:h3.Databases
:h3.Internationalization
:h3.Providing Online Help
:h3.Multithreading
:h2.Installing fpGUI
:h3.Standalone Setup

:h3.Integration with Lazarus IDE
For that you follow these simple steps:

:p.
:hp2.Lets configure Lazarus to have a new project type::ehp2.
:ol.
:li.Run Lazarus and open the fpgui_toolkit.lpk package found in the
.br
fpGUI code: <fpgui>/src/corelib/[x11|gdi]/fpgui_toolkit.lpk
.br
Click "Compile".

:li.Now open the IDE add-on package, compile and install.
.br
Open <fpgui>/extras/lazarus_ide/fpgui_ide.lpk
.br
Click "Compile" & "Install".
.br
Lazarus will rebuild and restart itself.
:eol.

:p.
:hp2.Now to create a fpGUI application::ehp2.
:ol.
:li.Run Lazarus
:li.Select "Project -> New Project..." and select "fpGUI Toolkit Application".
:eol.

:p.
All done, you have create your first fpGUI Toolkit application using
Lazarus IDE as your editor! :)
:h4.UIDesigner
 This page describes how fpGUI's forms designer application, UIDesigner, can be integrated with Lazarus IDE. Thus allowing you to launch the forms designer with a simple keyboard shortcut, and automatically load the current file being
edited by Lazarus IDE.
:p.
For this to work, we opted to use the External Tools feature of Lazarus.
Alternative methods exist, but that requires recompiling the IDE - and that seems like just to much effort.
:p.
The process is quite simple. While in Lazarus, simply select the menu items
":hp2.Tools:ehp2. -> :hp2.Configure External Tools...:ehp2.". A dialog will
appear. Select the :hp2.Add:ehp2. button, and then fill in the details as
shown in the screen below. Please use the correct path to the UIDesigner
executable.
:p.
Once you have entered all the details, click the OK button, and you have setup the UIDesigner inside Lazarus IDE.

:cgraphic.
ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
³ Edit Tool                                       X ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³ Title:              fpGUI UIDesigner              ³
³ Program Filename:   /path/to/uidesigner           ³
³ Parameters:         $EdFile()                     ³
³                                                   ³
³ ÄÄOptionsÄÄ                                       ³
³ [ ]                      [ ]                      ³
³ [ ]                                               ³
³                                                   ³
³ ÄÄKeyÄÄ                                           ³
³ [x] Shift  [ ] Alt  [x] Ctrl     [ F12         ]  ³
³                                                   ³
³  ÚÄÄÄÄ¿                          ÚÄÄÄÄÄÄ¿ ÚÄÄÄÄ¿  ³
³  ³Help³                          ³Cancel³ ³ OK ³  ³
³  ÀÄÄÄÄÙ                          ÀÄÄÄÄÄÄÙ ÀÄÄÄÄÙ  ³
ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
:ecgraphic.

.* :artwork align=left name='qguide_images/laz_uidesigner_setup.bmp'.
:h4.DocView
:cgraphic.
ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
³ Edit Tool                                       X ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³ Title:              DocView                       ³
³ Program Filename:   /path/to/docview              ³
³ Parameters:         fpgui+fcl+rtl -k $CurToken()  ³
³                                                   ³
³ ÄÄOptionsÄÄ                                       ³
³ [ ]                      [ ]                      ³
³ [ ]                                               ³
³                                                   ³
³ ÄÄKeyÄÄ                                           ³
³ [ ] Shift  [ ] Alt  [ ] Ctrl     [ F1          ]  ³
³                                                   ³
³  ÚÄÄÄÄ¿                          ÚÄÄÄÄÄÄ¿ ÚÄÄÄÄ¿  ³
³  ³Help³                          ³Cancel³ ³ OK ³  ³
³  ÀÄÄÄÄÙ                          ÀÄÄÄÄÄÄÙ ÀÄÄÄÄÙ  ³
ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
:ecgraphic.

.* :artwork align=left name='qguide_images/laz_docview_setup.bmp'.
:h3.Integration with MSEide
:h3.Integration with EditPad Pro
:h3.Integration with FP Text IDE

:euserdoc.
