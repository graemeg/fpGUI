
            Compiling the fpGUI Demos


 Compiling any of the examples from the Command Line
 ===================================================

You need to compile the fpGUI toolkit first as mentioned in the
README.txt file located in the <fpgui>/docs/ directory.

I'll assume the fpc executable path has been setup so you can execute 'fpc'
from any directory. Every project in the 'examples' directory has its
own extrafpc.cfg file. You only need to specify that config file and the
project's main unit to compile it.

The format is as follows:
  fpc @extrafpc.cfg <project main unit>

Example:
    fpc @extrafpc.cfg docedit.lpr
  or
    fpc @extrafpc.cfg helloworld.pas


      =======================================================



