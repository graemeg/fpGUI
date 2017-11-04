
                           Compiling the fpGUI Demos                          

              Compiling any of the examples from the Command Line              
===============================================================================

You need to compile the fpGUI toolkit first as mentioned in the
README.txt file located in the <fpgui>/docs/ directory.

I'll assume the fpc executable path has been setup so you can execute 'fpc'
from any directory. Every project in the 'examples' directory has its
own extrafpc.cfg file. You only need to specify that config file and the
project's main unit to compile it. The executable is written in the 'bin'
sub-directory of the project. If this sub-directory doesn't exist, create it before.

The format is as follows:

  fpc @extrafpc.cfg <project main unit>

Example:

    fpc @extrafpc.cfg docedit.lpr
  or
    fpc @extrafpc.cfg helloworld.pas


You can also compile all examples with compileall.bat (for Windows cmd)
or compileall.sh script (for bash). For example:

    $ cd fpgui/examples/gui
    $ ./compileall.sh

or

    > cd fpgui/examples/gui
    > compileall.bat


NB:

 - The compilation of gui/video_vlc example will fail if you have not compiled
   fpgui with the third party vlc and fpg_vlc units.
 - apps/gui is not supported by compileall.(sh|bat) scripts. You have to compile
   it by hand.

===============================================================================
