Title:   nanoedit
Author:  Graeme Geldenhuys <graemeg@gmail.com>
Description:
------------
I often need a Notepad like (very basic) editor on non-Windows platforms,
for some of my other projects. I created nanoedit for that reason. I'm
also using nanoedit as a testing platform for the fpg_textedit unit
(part of the Maximus IDE example project), and for my experimental
"elastic tabstops" [http://nickgravgaard.com/elastictabstops/]
implementation.

Some of Nanoedit's features thus far:

 * Multiple tabs (editors windows) support. Edit more than
   one file at a time.
 * Single instance support. Launch nanoedit from multiple
   file manager locations. Instead of opening multiple
   copies of nanoedit, the new files will be opened in tabs,
   using the currently running nanoedit instance.
 * Drag-and-drop loading of files. I nice fast way to open
   files. Combine this with single instance and it's very
   useful. eg: I have a nanoedit icon on my desktop. Drop a
   file on it, and nanoedit starts editing that text file
   file for me.
 * It uses the fpg_textedit widget, so has some nice features
   like dynamic scrolling speed (with mouse wheel), line
   numbers, right margin line etc. Syntax highlighting can
   also be enabled if needed.
 * Some basic text editor features like Find, Find Next,
   Goto Line etc.

                ----------[ end ]-----------

