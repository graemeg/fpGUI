
fpGUI information
=================
 This version of fpGUI is based on a implementation where every
 widget has a window handle. In other words every widget is actually
 an embedded window in the Form (yet another window).
  
 Revision 127 was the last revision which still had the old design - one
 handle per Form.
 It has been tagged in SubVersion as /tags/single_handle_fpgui
 
 From revision 227, the new design (multi-handle implementation) has
 become the new default implementation of fpGUI. It was a complete
 rewrite of the code.



To install FPC under Debian/Ubuntu
==================================
 Select the fpc.deb metapackage, which depends on a number of sub-packages
 containing the compiler, the units and so on. The 'libc' unit provided by
 FPC is included in the fp-units-i386.deb package, which is however marked
 as "deprecated" by the Ubuntu package manager and is therefore *not*
 installed by default using fpc.deb.

 The following command will set up FPC under Ubuntu in order to be used with
 fpGUI:

   sudo apt-get install fpc fp-units-i386



How to compile fpGUI
====================
 Please see the readme.txt file in the "src" directory.


System requirements under Linux
===============================
To be able to compile and link fpGUI based applications you need to install
the following library dependencies. The packages will pull in all the other
required packages too.

  $ sudo apt-get install libX11-dev
  $ sudo apt-get install libXft-dev



-----
 Graeme Geldenhuys

 
