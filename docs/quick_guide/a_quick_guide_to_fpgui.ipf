.* This file is encoded using IBM850 encoding
.* :encoding=IBM850:wrap=hard:tabSize=2:noTabs=true:

.* =============================================
.* Copyright (c) 2015 by Graeme Geldenhuys
.* =============================================

:userdoc.
:docprof toc=1234.
:title.A Quick Guide to fpGUI

.* This as a marco definition
.nameit symbol=pubdate text='August 2015'
.nameit symbol=dv text='Docview'
.nameit symbol=fpg text='fpGUI Toolkit'
.nameit symbol=fpc text='Free Pascal Compiler'

.* Footnotes referenced throughout the book.
:fn id=fpc_docs.
All official Free Pascal documentation can be found at:
http://www.freepascal.org/docs.var
:efn.

:h1.A Quick Guide to using &fpg.
:font facename='Helvetica-20:antialias=true'.
.* :artwork name='img0.bmp' align=center.
:lines align=center.
A Quick Guide to using
.br
fpGUI Toolkit
.br
:elines.
:font facename=default.
:lines align=center.
Written by Graeme Geldenhuys
:artwork align=center name='images/wizard_pink.bmp'.
.br
.br
All Rights Reserved. Copyright (c) 2015 by Graeme Geldenhuys
.br
:elines.

:p.
:p.
:table cols='13 20'.
:row.
:c.Version
:c.v0.1
:row.
:c.Pub Date
:c.&pubdate.
:etable.


:h2.License
:cgraphic.
şşşşşşşşş
 License
şşşşşşşşş
:ecgraphic.
:p.
This work is licensed under the Creative Commons Attribution-NonCommercial-
ShareAlike 3.0 Unported License. To view a copy of this license, visit
http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter
to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

:h2.Preface
:cgraphic.
şşşşşşşşş
 Preface
şşşşşşşşş
:ecgraphic.
:p.
I've been meaning to start this book back in 2013, but always seemed to find
other things to do. I've finally decided to put pen-to-paper (in a digital
way) and get started.
:p.
:hp2.What is fpGUI:ehp2.
:p.
fpGUI Toolkit (or the Free Pascal GUI Toolkit) is a multi-platform toolkit for
creating graphical user interfaces. Offering a complete set of custom drawn
widgets, fpGUI is suitable for projects ranging from small one-off tools to
complete application suites.
:p.
fpGUI is a 32bit and 64bit 2D graphics toolkit that doesn't rely on any huge
third party graphics libraries like GTK+ or Qt (and its dependencies). It
talks directly to the underlying graphics system (GDI, X11 etc). fpGUI is
supported on Unix, Linux, BSD, OpenSolaris, Mac OS X (via X11), Windows, and
embedded systems like ARM-Linux and WinCE.
:p.
fpGUI is open source and free software, licensed under a Modified LGPL
license. The toolkit has been implemented using the Object Pascal language,
and is meant for use with the Free Pascal compiler.
:p.
:hp2.Who This Book Is For:ehp2.
:p.
I've written this book for programmers who want to learn to use the &fpg.
toolkit, and that might not have used it, or similar toolkits before. I am
assuming that you have some basic understanding of the Object Pascal language.
:p.
This book covers using &fpg. directly (no LCL involved). The Lazarus
LCL-fpGUI widgetset is not feature complete, and not nearly ready for real
usage. My hope is that somebody will eventually take control of that aspect of
LCL and make it into a usable LCL target.
:p.
But for now, lets look at &fpg. itself!

:h2.Conventions
:cgraphic.
şşşşşşşşşşşşş
 Conventions
şşşşşşşşşşşşş
:ecgraphic.
:p.
The conventions used in this book should be pretty straight forward, but here
is the general gist of it.

:dl tsize=15.
:dthd.:hp2.TYPEFACE:ehp2.
:ddhd.:hp2.MEANING:ehp2.
:dt.:font facename='System Monospaced' size=0x0.Monospace type:font
facename=default size=0x0.
:dd.Monospace text represents text as it appears on screen or in code. It also
represents anything you need to type.
:dt.:hp2.Boldface:ehp2.
:dd.Boldface words in code listings represent reserved words or compiler
options.
:dt.:color fc=darkred.Dark Red text:color fc=default.
:dd.Darkred words in text represent Object Pascal identifiers, such as
variable or type names.
:dt.:hp1.Italic text:ehp1.
:dd.Italic text might be used to emphasise certain words in the text.
:edl.


:h2.Colophon
:cgraphic.
şşşşşşşşşş
 Colophon
şşşşşşşşşş
:ecgraphic.
:p.
This is the first book that I wrote using IPF and related technologies, though
I have written many shorter articles or README-style documents with it.
:p.
The master text was written primarily with the EditPad Pro v7 text editor and
using my own IPF syntax and file navigation schemes.
:p.
You can download the IPF colour scheme from here:
.br
  http://www.editpadpro.com/cgi-bin/cscsdl4.pl?id=243
:p.
You can download the IPF file navigation scheme from here:
.br
  http://www.editpadpro.com/cgi-bin/fnsdl2.pl?id=71
:p.
The source code examples are processed with a small custom written tool I
created. I can simply copy some source code into the program and it will
generate the same source code prefixed with line numbers and some IPF markup
added.
:p.
The final binary INF file is generated using the cross-platform OpenWatcom IPF
Compiler (WIPFC) - included with fpGUI.


:h2.A Brief History of &fpg.
:cgraphic.
şşşşşşşşşşşşşşşşşşşşşşşşşş
 A Brief History of fpGUI
şşşşşşşşşşşşşşşşşşşşşşşşşş
:ecgraphic.
:p.
circa 2006.
:p.
After developing many cross platform applications with Kylix and Delphi
I started getting very frustrated with the differences between the look and
behaviour of the applications under Linux and Windows. The code was also
riddled with IFDEF statements.
:p.
Then I stumbled across the Free Pascal and Lazarus projects. I thought this
is it, the answer to all my cross platform development problems. Unfortunately
after working with Lazarus for a few months I started finding more and more
issues with the Lazarus Component Library (LCL), though the IDE was great.
:p.
The Lazarus LCL is a wrapper for each platform's native widget set. This
brought with it the same issues I experienced with Kylix and Delphi - inconsistencies
between platforms. This got me thinking again about how I could resolve this
problem once and for all.
:p.
Then it hit me - implement the GUI toolkit myself using Free Pascal! Painting
the widgets myself to get a consistent look, and implement a consistent
behaviour. Instead of reinventing the wheel, I thought I would do some
searching to see if there is another project I could contribute to, or that
could give me a head start.
:p.
The first version of my GUI tolkit was based around a heavily modified
version of the Light Pascal Toolkit [http://sourceforge.net/projects/lptk]. I
then discovered the discontinued
fpGUI and fpGFX projects. I tried to contact the original author to no
avail. The &fpg. code hasn't been touched in four years (since early 2002).
After studying the code for a few weeks, I came to the conclusion that fpGUI
is much closer to what I strived to accomplish with my modified LPTK. A
lot was still missing from &fpg. though.
:p.
After thinking long and hard, I decided to start my widgetset again, but
this time based on the work done in fpGUI and fpGFX. I also added to
the mix some good ideas I saw in Qt 4.1. After a few weeks I completed quite a
few things missing in fpGUI, but I still needed to do a lot more to get it
to the point where I could test drive it in a commercial application.
I set myself a list of
outstanding things which should get it to a usable level. I also added a lot of
documentation as there was no documentation included with the
original fpGUI and fpGFX projects. Documentation is important to attract
other developers in using the widgetset.
:p.
And so fpGUI Toolkit was (re)born. ;-)

:h2.Part I: Basic fpGUI
:h3 id=ch_getting_started.Getting Started
:cgraphic.
şşşşşşşşşşşşşşşşş
 Getting Started
şşşşşşşşşşşşşşşşş
:ecgraphic.
:p.
In this chapter we will start small, and introduce some basic concepts and
terminology. In later chapters we will start with a realistic application.
:p.
I'll also assume that you have setup your development environment as
described in the :link reftype=hd refid=ch_installing_fpgui.Installing
fpGUI:elink. chapter. Because there are multiple ways to set up your build
environment, I'll make no assumption what environment you are using. So for
the rest of the book I'll simply say you must rebuild and run the application
when needed.

:p.
:hp2.See Also:ehp2.
:ul compact.
:li.:link reftype=hd refid=ch_hello_world.Hello World:elink.
:li.:link reftype=hd refid=ch_making_connections.Making Connections:elink.
:li.:link reftype=hd refid=ch_using_documentation.Using the Class
Documentation:elink.
:eul.

:h4 id=ch_hello_world.Hello World
:cgraphic.
şşşşşşşşşşşşş
 Hello World
şşşşşşşşşşşşş
:ecgraphic.
:p.
Here is a very simple fpGUI application. We will study it line by line.
:xmp.
  1   program helloworld;
  2
  3   {$mode objfpc}{$H+}
  4
  5   uses
  6     Classes, fpg_base, fpg_main, fpg_form, fpg_label;
  7
  8   type
  9     TMainForm = class(TfpgForm)
 10     public
 11       procedure AfterCreate; override;
 12     end;
 13
 14   procedure TMainForm.AfterCreate;
 15   begin
 16     Name := 'MainForm';
 17     SetPosition(316, 186, 170, 30);
 18     WindowTitle := 'MainForm';
 19     CreateLabel(self, 40, 4, 'Hello World!');
 20   end;
 21
 22   procedure MainProc;
 23   var
 24     frm: TMainForm;
 25   begin
 26     fpgApplication.Initialize;
 27     fpgApplication.CreateForm(TMainForm, frm);
 28     try
 29       frm.Show;
 30       fpgApplication.Run;
 31     finally
 32       frm.Free;
 33     end;
 34   end;
 35
 36   begin
 37     MainProc;
 38   end.
:exmp.

:p.
Line 1 tells the compiler this unit is the program unit - the start of an
application source code.

:p.
Line 3 tells the compiler what Pascal syntax we are using for this unit. The
&fpc. Compiler supports multiple pascal syntax modes, like Turbo Pascal,
Delphi, Object FPC  (the one we used above) etc. The $H+ directive notifies
the compiler that when we use the generic :color fc=darkred.String:color
fc=default. data type, we actually mean the :color fc=darkred.AnsiString:color
fc=default. data type. You can read more about these in the :link reftype=fn
refid=fpc_docs.Free Pascal Programmers Guide:elink..

:p.
Lines 5 and 6 specifies which other source code units to include, for the
compiler to be able to successfully compile our application. This is because
we use objects or classes defined in other units. For example the :color
fc=darkred.fpgApplication:color fc=default. object is defined in the :color
fc=darkred.fpg_main:color fc=default. unit, the :color
fc=darkred.TfpgForm:color fc=default. class is defined in the :color
fc=darkred.fpg_form:color fc=default. unit.

:p.
Lines 8 to 12 we define a new class, descending from the fpGUI :color
fc=darkred.TfpgForm:color fc=default. class.
A :color fc=darkred.TfpgForm:color fc=default. is a basic representation of a
window in our application. We define a descending class, named :color
fc=darkred.TMainForm:color fc=default., so we can add our own customisations
or functionality. In Line 11 we override the :color
fc=darkred.AfterCreate:color fc=default. method, as that is a good place to
add our own customisations like the window size, window title etc. This will
be explained in more detail later in the book.

:p.
Lines 14 to 20 is the implementation of the :color
fc=darkred.AfterCreate:color fc=default. method. Line 16 sets
the :color fc=darkred.Name:color fc=default. property of the :color
fc=darkred.TMainForm:color fc=default.. Every
widget in fpGUI needs to have a name
assigned to it. In fpGUI terminology, a widget is a visual element displayed
in the user interface. Examples of widgets are a Button, ComboBox, Form etc.
Some widgets can also act as a container, containing other widgets. A Form and
Panel widgets are examples of container widgets - as they often contain other
widgets like a MenuBar, Buttons, Labels etc. Line 17 sets the Top, Left, Width
and Height of the main form. Line 18 sets the title (often also referred to as
a caption) of our main form.

:p.
Line 19 creates an instance of a Label widget. It also assigns the parent
widget, the position and the text to be displayed by the label widget. All
widgets in fpGUI has such a convenience CreateXXX() function. This makes in
very easy to write a single line of code to create a widget instance, and set
the most popular properties of that widget. Later in this book you will see a
different way of creating widgets - normally generated by the UI Designer
application (fpGUI's stand alone visual forms designer). The code generated by
the UI Designer is also very easy to read, and as close as possible to what a
human would have typed - thus making in very easy to read and understand.

:p.
Lines 22 to 34 we define a procedure called :color fc=darkred.MainProc:color
fc=default.. We did this simply to
organise our source code a little better. In Lines 23 and 24 we define a local
variable that will represent our application's main window. The :color
fc=darkred.frm:color fc=default. variable will hold an instance of the
TMainForm class we defined earlier.

:p.
Line 26 initialises the fpGUI framework so it is ready to run our application.

:p.
Line 27 we ask the fpgApplication to create a instance of the :color
fc=darkred.TMainForm:color fc=default. class,
and save that instance in the :color fc=darkred.frm:color fc=default. variable.

:p.
Line 29 makes the main window visible. In fpGUI all widgets are created hidden
so it gives as a chance to make customisation (eg: changing the displayed
text, or size of the widget) without causing any flicker.

:p.
Line 30 tells the fpGUI framework to start running our program, and process
any system or user generated events. Behind the scenes an application
generates many events. They are defined in two categories. User generated
events would be generated when the user interacts with the application. For
example, when clicking on a button.
Operating System events would be clipboard handling, placing windows in a
specific location on the desktop, telling the framework when it should repaint
the form etc. You don't need to worry about system events, as the fpGUI
framework takes care of all this for you. User events (or widget events) will
be covered in the next topic.

:p.
Line 32 frees our :color fc=darkred.TMainForm:color fc=default. instance,
referenced by the :color fc=darkred.frm:color fc=default. variable,
releasing the memory used back to the operating system. In Object Pascal you
normally need to free all class instances you create.

:p.
Line 36 to 38 is the main code block for the program unit. We simply call the
MainProc procedure which kickstarts our application.

:p.
You can now build the program, and when you run it, it should display a
windows as can be seen in Figure 1. Your first fpGUI application!

:p.
:artwork align=center name='images/ch1_hello_world.bmp'.
:lines align=center.:hp2.Figure 1::ehp2. Hello World application under
FreeBSD:elines.

:p.
Other that the window borders and window icon, the content of the window
should be identical on all fpGUI supported platforms.


:h4 id=ch_making_connections.Handling User Events
:cgraphic.
şşşşşşşşşşşşşşşşşşşşşş
 Handling User Events
şşşşşşşşşşşşşşşşşşşşşş
:ecgraphic.
:p.
The big difference between modern GUI applications and console applications are
that modern GUI applications tend to be event based applications. When you run
a small console utility application, they tend to start up, do their thing,
and then exit.

:p.
Most GUI applications on the other hand start up, then waits for user
interaction, then react to whatever the user did, then wait for the next
interaction etc. An example of such an event would be when the user clicks a
Button, which would then trigger the :color fc=darkred.OnClick:color
fc=default. event for that button. The :color fc=darkred.OnClick:color
fc=default. event handler would then execute a block of code &emdash. maybe
calling one or more functions &emdash. then wait for more user input.

:xmp.
  1   :hp2.program:ehp2. closeme;
  2
  3   {$mode objfpc}{$H+}
  4
  5   :hp2.uses:ehp2.
  6     Classes, fpg_main, fpg_form, fpg_button;
  7
  8   :hp2.type:ehp2.
  9     TMainForm = :hp2.class:ehp2.(TfpgForm)
 10     :hp2.private:ehp2.
 11       :hp2.procedure:ehp2. btnQuitClicked(Sender: TObject);
 12     :hp2.public:ehp2.
 13       :hp2.procedure:ehp2. AfterCreate; override;
 14     :hp2.end:ehp2.;
 15
 16   :hp2.procedure:ehp2. TMainForm.btnQuitClicked(Sender: TObject);
 17   :hp2.begin:ehp2.
 18     Close;
 19   :hp2.end:ehp2.;
 20
 21   :hp2.procedure:ehp2. TMainForm.AfterCreate;
 22   :hp2.begin:ehp2.
 23     Name := 'MainForm';
 24     SetPosition(316, 186, 170, 30);
 25     WindowTitle := 'MainForm';
 26     CreateButton(self, 40, 4, 85, 'Quit', @btnQuitClicked);
 27   :hp2.end:ehp2.;
 28
 29   :hp2.procedure:ehp2. MainProc;
 30   :hp2.var:ehp2.
 31     frm: TMainForm;
 32   :hp2.begin:ehp2.
 33     fpgApplication.Initialize;
 34     fpgApplication.CreateForm(TMainForm, frm);
 35     :hp2.try:ehp2.
 36       frm.Show;
 37       fpgApplication.Run;
 38     :hp2.finally:ehp2.
 39       frm.Free;
 40     :hp2.end:ehp2.;
 41   :hp2.end:ehp2.;
 42
 43   :hp2.begin:ehp2.
 44     MainProc;
 45   :hp2.end:ehp2..
:exmp.

:p.
Essentially most of this program is identical to the previous example. What we
have changed is replacing the Label widget with a Button widget. We also
implemented an :color fc=darkred.OnClick:color fc=default. event handler for
the button, so that when you click the
button, the application quits. I'll now step through the changes in more
detail.

:p.On line 6 we had to specify the fpGUI unit that contains the declaration of
the :color fc=darkred.TfpgButton:color fc=default. class.

:p.Line 11 declares the method which will become our Button's :color
fc=darkred.OnClick:color fc=default. event
handler. In Lines 16 to 19 we have the implementation of that method. On line
18 we simply call the form's :color fc=darkred.Close:color fc=default. method.
In fpGUI, when the main form of the
application closes, then the program terminates.

:p.In the :color fc=darkred.AfterCreate:color fc=default. method, line 26, we
removed the earlier :color fc=darkred.CreateLabel():color fc=default. call and
replaced it with a :color fc=darkred.CreateButton():color fc=default. call.

:nt.The parameters for the :color fc=darkred.CreateButton():color fc=default.
call are slightly different to the earlier :color
fc=darkred.CreateLabel():color fc=default. call. As was mentioned before,
these are convenience functions, and each of them have different parameters
&emdash. assigning values to the most used properties of the widget in
question.:ent.

The :color fc=darkred.CreateButton():color fc=default. can also assign the
:color fc=darkred.OnClick:color fc=default. event handler for our button.
After all, what is the point of having a button if it doesn't do something
when clicked. So the last parameter we passed to :color
fc=darkred.CreateButton():color fc=default. is a reference to such method.
This method must have the same signature as what the :color
fc=darkred.TfpgButton.OnClick:color fc=default. definition specifies &emdash.
so we can't simply assign any method as the event handler.

:p.
That's it! The rest of the program is exactly the same as before. Now compile
and run this new program. The main form should now look like Figure 2. When
you click the button (or press the Spacebar key - which also "clicks" the
button), the application terminates.

:p.
:artwork align=center name='images/ch1_closeme.bmp'.
:lines align=center.:hp2.Figure 2::ehp2. CloseMe application under
FreeBSD:elines.

:p.
Now that we have the basics down, lets add two more widgets to our program. In
this case the two new widgets will keep each other in sync - always
representing the same value. We will be adding a SpinEdit and a TrackBar widget
to our current program. We also need to enlarge our main form to make space
for these new widgets. The end result will look like the program in Figure 3.

:p.
:artwork align=center name='images/ch1_closeme2.bmp'.
:lines align=center.:hp2.Figure 3::ehp2. CloseMe application with two extra
widgets:elines.

:p.
In summary, here are the changes we are going to make, and the order in which
we are going to make them.
:ol compact.
:li.enlarged the form by changing the height
:li.add new units to uses clause
:li.create the SpinEdit widget and store its reference
:li.define spnEdit's OnChange event
:li.create trackbar, but in a slightly different way. results is the same
:li.implementation of both OnChange events
:eol.

:p.
:hp2.Step 1 - enlarged the form by changing the height:ehp2.
:p.
In the existing :color fc=darkred.AfterCreate:color fc=default. method, we
change the height value from 30 to 65 in the :color
fc=darkred.SetPossition:color fc=default. call, so it look as follows:
:xmp.
  SetPosition(316, 186, 170, 65);
:exmp.

:p.
:hp2.Step 2 - add new units to uses clause:ehp2.
:p.
At the beginning of our program we modify the uses clause line by adding the
:color fc=darkred.fpg_spinedit:color fc=default. and :color
fc=darkred.fpg_trackbar:color fc=default. units to the existing list of units.

:p.
:hp2.Step 3 - create the SpinEdit widget and store its reference:ehp2.
:p.
The :color fc=darkred.CreateSpinEdit:color fc=default. helper function doesn't
take an :color fc=darkred.OnChange:color fc=default. event handler, so we need
to store the reference of the newly created widget, so we can then use that
reference to assign the :color fc=darkred.OnChange:color fc=default. event
handler.

:p.
Because we will need to reference the SpinEdit widget later again in our
program, we define a field variable - named :color fc=darkred.spnEdit:color
fc=default. -  inside :color fc=darkred.TMainForm:color fc=default.'s private
section. This field variable will hold a reference to our SpinEdit widget. We
create the SpinEdit widget as shown below. Note in the second line of code how
we use the reference variable, and then assign the OnChange event handler.

:xmp.
  spnEdit := CreateSpinEdit(self, 5, 32, 40, 24);
  spnEdit.OnChange := @SpinEditChanged;
:exmp.

:p.
:hp2.Step 4 - define spnEdit's OnChange event:ehp2.
:p.
In the previous step we assigned the method :color
fc=darkred.SpinEditChanged:color fc=default. as the OnChange event handler of
our SpinEdit widget. But at this point we don't have such a method defined
yet. So lets do that now.

:p.
Declare the :color fc=darkred.SpinEditChanged:color fc=default. method in the
:color fc=darkred.TMainForm:color fc=default.'s private section, and then
create an empty implementation for now. The implementation looks as follows:

:xmp.
  :hp2.procedure:ehp2. TMainForm.SpinEditChanged(Sender: TObject);
  :hp2.begin:ehp2.
    :color fc=darkcyan.// code to be added here:color fc=default.
  :hp2.end:ehp2.;
:exmp.



:p.
:hp2.Step 5 - create the trackbar widget:ehp2.
:p.
This step is near identical to what we did in Steps 3 & 4. We define a TrackBar
field variable which will hold a reference of our trackbar. Then we create our
TrackBar instance with the :color fc=darkred.CreateTrackBar():color fc=default. helper function.
Next we assign our OnChange event handler. Again, all very simalar to what was done in Step 3.

:p.
We then define our event handler method in the Private section of our form, and
define a empty implementation. We will implement this in the next step.

:p.
:hp2.Step 6 - implementation of both OnChange events:ehp2.
:p.
Inside the :color fc=darkred.SpinEditChanged:color fc=default. method - which
gets triggered when the SpinEdit is changed - we are going to assign the
:color fc=darkred.SpinEdit.Value:color fc=default. to the TrackBar widget, so
they both display the same value.

:p.
Then inside the :color fc=darkred.TrackBarChanged:color fc=default. method, we do
a similar thing - assign the :color fc=darkred.TrackBar.Value:color fc=default. to the SpinEdit.

:p.
And that is it! All our changes are now complete, and our application should
behave as we intended.





:h4 id=ch_using_documentation.Using the Class Documentation
:cgraphic.
şşşşşşşşşşşşşşşşşşşşşşşşşşşşşşş
 Using the Class Documentation
şşşşşşşşşşşşşşşşşşşşşşşşşşşşşşş
:ecgraphic.
:p.
It is not expected for any developer to know the complete class model of &fpg.
or the &fpc. RTL (runtime library). Neither does this ebook cover all classes
or functions. Not to mention that there are hundreds of classes, maybe even
thousands! So as you will quickly realise, these frameworks are huge.

:p.
Luckily there is help on hand - excuse the pun! :-) The &fpg. project includes
class reference documentation in the form on INF files. These files are viewable
with a tool called &dv. (part of fpGUI - and the program you are using to view
this ebook). &dv. is a very fast and feature complete help viewer. Incidentally
the INF help format is also the official help format recommended for your
applications created with fpGUI.

:p.
For convenience the reference documentation is also available in HTML format,
and is what is also made available on the project website. The INF format is
preferred though, as it is available offline, indexed and supports advanced
searching via &dv..

:p.
Documenting every part of fpGUI is a ongoing process, but with every release there
are more and more documentation added.

:p.
:artwork align=left name='images/ch1_docview.bmp'.
:lines align=center.:hp2.Figure 4::ehp2. &dv. main window with INF help loaded:elines.

:p.
&dv. also supports many other useful features like being able to concatenate
INF files at runtime, inline annotations (user comments) and bookmarks.



:h3.Creating Dialogs
:cgraphic.
şşşşşşşşşşşşşşşşşş
 Creating Dialogs
şşşşşşşşşşşşşşşşşş
:ecgraphic.
:p.
This chapter will teach you how to create a secondary window for your application,
or as is more well known, a dialog. Dialogs are often used by applications to
ask for more information before the application can continue with its next task.

:p.
We will create our first dialog purely by code, then later we will use the
fpGUI Visual Forms Designer (known as UIDesigner). Using the visual forms designer
is a lot faster, and allows you to quickly experiment with different designs
and widget layouts. All done by using the mouse to drop widegts on a designer
form, set properties and drag or resize them as needed.

:h4.Subclassing TfpgForm
:cgraphic.
şşşşşşşşşşşşşşşşşşşşşş
 Subclassing TfpgForm
şşşşşşşşşşşşşşşşşşşşşş
:ecgraphic.
:p.
Our first example will be to create a Find dialog, which will look like the
following screenshot.

:p.
[ add Find dialog screenshot here ]

:p.
We will also add events to this dialog - treating it just like a normal class - which
in terms of Object Pascal, is exactly what it is.



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

.* Ideas to implement multithreading with GUI updates:

.* Schedule a method that pulls other data from a threadsafe queue
.* based on tthreadlist. The threads populate the threadsafe queue,
.* and the main gui pulls data from there when it can.

.* fpGUI's event queue is thread safe, but is it really a good idea to use
.* that for this purpose?

:h3.Providing Help for Your Components
.* Explain fpdoc usage.

:h2 id=ch_installing_fpgui.Installing fpGUI




:h3.Building fpGUI from the Command Line
:cgraphic.
şşşşşşşşşşşşşşşşşşşşşşşşşşşşşşşşşşşşşş
 Building fpGUI from the Command Line
şşşşşşşşşşşşşşşşşşşşşşşşşşşşşşşşşşşşşş
:ecgraphic.
:p.
This is still a work in progress until I can find a suitable solution. I'm
not 100% satisfied with this, but it's a quick and easy way to get things
to compile. I'll assume you have the :color fc=darkred.fpc:color fc=default.
executable setup in your system's
PATH environment variable so it
can be run from any location on your computer. I'll also assume your global
:color fc=darkred.fpc.cfg:color fc=default. file has been setup correctly so
the &fpc. can find the RTL and
FCL units.

:p.
Under Linux, *BSD, OpenSolaris and Mac OS X run:
:xmp.
 cd <fpgui>/src
 build.sh
:exmp.

:p.
Under Windows run:
:xmp.
 cd <fpgui>\src
 build.bat
:exmp.
:p.
The :color fc=darkred.extrafpc.cfg:color fc=default. file located in the
:color fc=darkred.src:color fc=default. directory is combined with your global
fpc.cfg file. The local extrafpc.cfg file sets up all the required search and
include paths to compile the :color fc=darkred.corelib:color fc=default. and
:color fc=darkred.gui:color fc=default. directories.
:p.
All compiled units (*.o and *.ppu) are saved in the <fpgui>/lib directory. This
makes the search paths for you applications a little easier to setup.

:h3.Integration with Lazarus IDE
:cgraphic.
şşşşşşşşşşşşşşşşşşşşşşşşşşşşşş
 Integration with Lazarus IDE
şşşşşşşşşşşşşşşşşşşşşşşşşşşşşş
:ecgraphic.
:p.
Lazarus IDE is a very versatile IDE and allows you to create many types of
projects with it. Those include LCL desktop applications, web applications,
console applications, and more importantly, fpGUI Toolkit applications.
:p.
To accomplish the latter, follow these simple steps:
:p.
:hp2.Lets configure Lazarus to have a new project type::ehp2.
:ol.
:li.Run Lazarus and open the :color fc=darkred.fpgui_toolkit.lpk:color
fc=default. package found in the
following location:
:xmp.
 <fpgui>/src/corelib/[x11|gdi]/fpgui_toolkit.lpk
:exmp.
:note.The [x11|gdi] means you need to choose the "x11" or "gdi" directory
based on
your platform.
:p.
Click "Compile".

:li.Now you need to install the IDE add-on package which registers a new
project
type with Lazarus IDE. Do that by opening the :color
fc=darkred.fpgui_ide.lpk:color fc=default. package, found at:
:xmp.
 <fpgui>/extras/lazarus_ide/fpgui_ide.lpk
:exmp.
:p.
Click "Compile" and then "Install". Lazarus will rebuild and restart itself.
:eol.

:p.
:hp2.Now to create a &fpg. application::ehp2.
:ol.
:li.Run Lazarus
:li.Select ":hp2.Project:ehp2. ¯ :hp2.New Project...:ehp2." and select "fpGUI
Toolkit Application".
:eol.

:p.
All done, you have created your first fpGUI application using
Lazarus IDE as your editor! :)

:h4 id=laz_uidesigner.UI Designer
:cgraphic.
şşşşşşşşşşşşş
 UI Designer
şşşşşşşşşşşşş
:ecgraphic.

:p.
This page describes how &fpg.'s forms designer application, UIDesigner, can be
integrated with Lazarus IDE. Thus allowing you to launch the forms designer
with a simple keyboard shortcut, and automatically load the current file being
edited by Lazarus IDE.
:p.
For this to work, we opted to use the External Tools feature of Lazarus.
Alternative methods exist, but that requires recompiling the IDE - and that
seems like just to much effort.
:p.
The process is quite simple. While in Lazarus, simply select the menu items
":hp2.Tools:ehp2. ¯ :hp2.Configure External Tools...:ehp2.". A dialog will
appear. Select the "Add" button, and then fill in the details as
shown in the screen below. Please use the correct path to the UIDesigner
executable.
:p.
Once you have entered all the details, click the OK button, and you have setup
the UIDesigner inside Lazarus IDE.

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

:h4.Docview
:cgraphic.
şşşşşşşşş
 Docview
şşşşşşşşş
:ecgraphic.
:p.
Integrating Docview with Lazarus IDE is the exact same process as was done
setting up Lazarus IDE with the :link reftype=hd refid=laz_uidesigner.UI
Designer:elink..
Please refer to that page for detailed instructions, and then apply the
following
settings to the External Tools item you are setting up.

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
