
fpGUI information
=================
 This version of fpGUI is based on a implementation where every
 widget has a window handle. In other words every widget is actually
 an embedded window inside a top-level window (aka Form).

 If you wanted to look at the old design of fpGUI - the one based on
 a single handle per Form - then create a local branch based on the
 tag 'single_handle_fpgui', or the slightly newer branch called 
 'v0.4_fixes'.

 Release v0.5 and later is the new design (multi-handle implementation).
 It was a complete rewrite of the code.



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
 Please see the INSTALL.txt file for detailed instructions.



System requirements under Linux
===============================
To be able to compile and link fpGUI based applications you need to install
the following library dependencies. The packages will pull in all the other
required packages too.

  $ sudo apt-get install libX11-dev
  $ sudo apt-get install libXft-dev



System requirements under *BSD, OpenSolaris
===========================================
Pretty much the same as under Linux.


Regards,
 - Graeme Geldenhuys -

                   ============================================
