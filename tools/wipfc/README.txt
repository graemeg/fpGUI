
  Open Watcom IPF Compiler
  ========================

This directory contains the "wipfc" binary files for Windows,
Linux and FreeBSD. It also contains the helper language
files.


Unix-like systems
-----------------
Unpack the wipfc-* archive for your platform. If the unpacked
"wipfc" binary is not marked executable, then run the
following from the command line:

    chmod +x wipfc


Windows systems
---------------
Simply unpack the wipfc.exe.zip archive.


Configuration
-------------
For the wipfc executable to work, it needs to have a WIPFC
environment variable set up. The WIPFC environment variable
needs to specify the fully path to the  <fpgui>/tools/wipfc/
directory.


Usage
-----
   wipfc -i my_help_file.ipf



Compiling your own wipfc
------------------------

  git clone https://github.com/open-watcom/open-watcom-v2.git
  cd open-watcom-v2/bld/wipfc
  ./configure
  make
  sudo make install


             -----------[ end ]------------
