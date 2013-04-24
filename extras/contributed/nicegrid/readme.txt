Name:    NiceGrid
Author:  "Jean Pierre Anghel" <jean-pierre.anghel@orange.fr>
Date:    2012-01-21
Description:
This grid widget is a port of the VCL NiceGrid component found at 
[http://www.priyatna.org]. The original author gave permission to port this
component to Free Pascal and fpGUI. Below is the emails giving permission.



********************************************************************************
Dear Jean-Pierre,

Sure, I will be glad if NiceGrid can be ported to FreePascal.

There's however an important bug that needs to be solved. It's when 
handling scrollbar messages. Current version can only handle 2 bytes 
(word) scrollbar offset, due to Windows limitation in the past. This 
will limit NiceGrid to only able to handle only a few thousands rows. 
Current Windows OS-es use 4 bytes (integer) scrollbar offsets. It should 
be easy to fix, I can help.

Also I prefer to have one source code for NiceGrid using IFDEFs, instead 
a version for Delphi and another for FreePascal. I don't have much 
experience in LCL or fpGUI. Is it possible?

Regards,
Priyatna


On 8/10/2011 2:52 PM, jean-pierre anghel wrote:
> Hello,
> I participate, modestly, in the fpGui project which works under FreePascal and allows to have,
> with the same source code, executables files under Linux and Windows : http://fpgui.sourceforge.net/
> I was seduced by your NiceGrid and I adapted it to FreePascal.
> Before passing on the result to Graeme Geldenhuys I wanted at first to present it to you and to have
> your agreement.
> Regards.
> Jean Pierre ANGHEL
>
********************************************************************************

