
 Building fpGUI from the Command Line
 ====================================

This is still a work in progress until I can find a suitable solution. I'm
not 100% satisfied with this, but it's a quick and dirty way to get things
to compile. I'll assume you have the 'fpc' executable setup in your PATH so it
can be run from any location on your computer. I'll also assume you global 
fpc.cfg file has been setup correctly so the FPC compiler can find the RTL and
FCL units.

Under Linux run:  build.sh

Under Windows run:  build.bat


The extrafpc.cfg file located in this directory is combined with your global 
fpc.cfg file. The local extrafpc.cfg file sets up all the required search and 
include paths to compile CoreLib and GUI directories.

All compiled units (*.o and *.ppu) are saved in the ../lib directory. This 
makes the search paths for you applications a little easier to setup.


  Building fpGUI using Lazarus
  ============================

I use a Lazarus feature call Packages that compiles the required 
units and keeps track of the compiled units and paths when creating 
applications.  

 * Start Lazarus
 * Select Components->Open Package File (*.lpk) and select the
    src/corelib/<your platform>/fpgfx_package.lpk
   In you case the .lpk file will be in the x11 directory. A new
   dialog will appear - click Compile.
 * Select Components->Open Package File (*.lpk) again and select
   the
     src/gui/fpgui_package.lpk
   A new dialog will appear - click Compile.
 * Lazarus has now compiled both packages and will keep track of
   all the compiled units and paths.
 * Now lets open a project. Select Project->Open Project and select
   any project in the examples/gui directory. Select the *.lpi file.
 * Now select Run->Build and Lazarus will compile the project for
   you. The executable will be located in the same directory as the
   source. The compiled units will be placed in the <project>/units
   directory.

When you create you own project, all you need to do is tell Lazarus to 
associate the 'fpgui_package.lpk' with your project and it will automatically 
find all the fpGUI compiled units and source for you.

 * Creating a new project. Select Project->New Project.  Select
   Program and click the Create button.
 * Save the project in your preferred directory.
 * Associate fpGUI with your project. Select Project->Project Inspector.
   An new dialog will appear. Select Add then New Requirements. In the
   Package Name combobox, select the 'fpgui_package' package and
   click OK.
 * You can now write your program and use any fpGUI units. Lazarus will
   automatically include the paths to the fpGUI compiled units for you.


 Building fpGUI from the Free Pascal Text IDE
 ============================================
 
First you would need to setup the 'fp' IDE to find the related files.
As far as I understand the text mode IDE has it's own built-in compiler
so doesn't read the standard fpc.cfg file.

 * Run the text mode IDE from the command line:  fp
 * Navigate the menus to: Options|Directories and select the 'Units'
   tab.
 * Now enter the following directories replacing the relevant parts with
   you actual paths. The example below is valid on my system only.
   I was using FPC 2.2.0 under Linux and the X11 corelib backend.
  
    /opt/fpc_2.2.0/lib/fpc/2.2.0/units/i386-linux/*
    /opt/fpc_2.2.0/lib/fpc/2.2.0/units/i386-linux/rtl
    /home/graemeg/programming/fpGUI/src/corelib
    /home/graemeg/programming/fpGUI/src/corelib/x11
    /home/graemeg/programming/fpGUI/src/gui

 * Now select the 'Include files' tab and enter the following paths.
   Again change the paths to point to your actual directories and
   X11 or GDI corelib backend.
  
    /home/graemeg/programming/fpGUI/src/corelib  
    /home/graemeg/programming/fpGUI/src/corelib/x11
  
 * Now changes to 'Miscellaneous' tab, PPU output directory. Type in
   the edit box:  units
  
   NOTE:
   This will place all the compiled *.ppu and *.o files into a 'units'
   directory inside you current directory. So make sure you create it
   before you try to compile for the first time. FPC doesn't create
   directories for you!
  
 * Now you are ready to open your projects main program unit (F3) and
   compiling it by pressing (F9). 
  
  
 Compiling any of the examples from the Command Line
 ===================================================

You need to compile fpGUI first as mentioned above!
Every project in the ../examples directory has it's own extrafpc.cfg file.
You only need to specify that config file and the project unit to compile
it.  

fpc @extrafpc.cfg <project unit>

Example:
  fpc @extrafpc.cfg docedit.lpr




Regards,
  - Graeme -

                ===========================================



