.* This file is encoded using IBM850 encoding
.* :encoding=IBM850:
:userdoc.
:docprof toc=123456.
:title.Adding a new Canvas class

.* This as a marco definition
.nameit symbol=pubdate text='December 2016'
.nameit symbol=dv text='Docview'
.nameit symbol=fpg text='fpGUI Toolkit'
.nameit symbol=fpc text='Free Pascal Compiler'


:h1.Adding a new Canvas class
:font facename='Helvetica-20:antialias=true'.
.* :artwork name='img0.bmp' align=center.
:lines align=center.
Adding a new Canvas class
.br
:elines.
:font facename=default.
:lines align=center.
Written by Andrew Haines
.br
:elines.

:p.
:p.
:table cols='13 20'.
:row.
:c.Version
:c.v1.0
:row.
:c.Pub Date
:c.&pubdate.
:etable.

:h2.Abstact
:cgraphic.
þþþþþþþþþþ
 Abstract
þþþþþþþþþþ
:ecgraphic.
:p.This will hopefully help anyone in the future implementing a new canvas 
class in &fpg..

:h2.Steps
:cgraphic.
þþþþþþþ
 Steps
þþþþþþþ
:ecgraphic.
:ol.
:li.Create a new unit that uses :color fc=darkred.fpg_base:color fc=default..
:li.Add a new canvas class i.e. :color fc=darkred.TfpgCairoCanvas:color fc=default. derived from :color fc=darkred.TfpgCanvasBase:color fc=default..
:li.Copy/Paste the :hp2.virtual; abstract;:ehp2. methods to the new type.
:li.Start implementing those methods.
:eol.

:h2.Further Reading
:cgraphic.
þþþþþþþþþþþþþþþþþ
 Further Reading
þþþþþþþþþþþþþþþþþ
:ecgraphic.
:p.To get a better understanding of how &fpg.'s painting process works, what
methods get called, and how the various widget Canvas's work together, take a
look at 
the :link reftype=hd refid=1 database='alienwindow_design.inf'.alienwindow_design.inf:elink. file.

:euserdoc.
