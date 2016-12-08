.* This file is encoded using IBM850 encoding
.* :encoding=IBM850:
:userdoc.
:docprof toc=123456.
:title.Alien-Window Design

.* This as a marco definition
.nameit symbol=pubdate text='December 2016'
.nameit symbol=dv text='Docview'
.nameit symbol=fpg text='fpGUI Toolkit'
.nameit symbol=fpc text='Free Pascal Compiler'


:h1.Alien-Window Design
:font facename='Helvetica-20:antialias=true'.
.* :artwork name='img0.bmp' align=center.
:lines align=center.
Alien-Window Design
.br
:elines.
:font facename=default.
:lines align=center.
Written by Andrew Haines
.br
with modifications by Graeme Geldenhuys
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
:p.This document will explain a bit how the process of painting works
in &fpg., and how the different canvases work together. This information
only applies to &fpg. v1.5 and later.

:h2.Canvas Basics / Background
:cgraphic.
þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ
 Canvas Basics / Background
þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ
:ecgraphic.
:p.
Currently &fpg. uses a single native window surface for each window with it's 
child widgets. We call these 'alien' widgets because they are completely 
virtual and the events are managed by the top level native window class.

:p.
As a result, when a widget/canvas draws, it is actually drawing on the window 
surface that all widgets share and care must be taken to not draw in another 
widgets space. Previously, using a window for each widget would do this for us 
but caused other difficulties not relevant to this topic.

:p.
:hp2.The Solution:ehp2.
:p.
Each Widget has it's own canvas object, however the drawing commands it 
receives are performed on a shared window surface or buffer. The canvas acts 
like a context and has it's own colors/properties/fonts which don't affect 
other canvases.

:h2.Life Cycle of a Canvas
:cgraphic.
þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ
 Life Cycle of a Canvas
þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ
:ecgraphic.
:p.
:hp2.Created in the widgets constructor:ehp2.
:p.
When a Widget is created, it creates a Canvas instance derived from the global 
variable in fpg_main: :color fc=darkred.DefaultCanvasClass:color fc=default..

:xmp.
  FCanvas := DefaultCanvasClass.Create(Self);
:exmp.

:p.
:hp2.A paint event begins:ehp2.
:p.
As its properties are set, its inner implementation is updating its context 
for when a paint event is trigger. Usually these properties are set 
or changed in response to a paint event.

:p.
:color fc=darkred.DoBeginDraw:color fc=default. is called. It has two arguments, the widget and the 
:color fc=darkred.CanvasTarget:color fc=default.. The :color fc=darkred.CanvasTarget:color fc=default. is
the canvas of the top level widget that 
has a native window. So in the case of a simple form with a button, the button 
is drawing on the canvas of the parent form. :color fc=darkred.CanvasTarget:color fc=default. for
the button would 
be the same as :color fc=darkred.Button.Parent.Canvas:color fc=default.. This is a
good place to instantiate the 
implementation specific resources like a context.

:p.
If the canvas is :hp2.not:ehp2. the top level canvas then the base class asks the 
top level canvas to start drawing by calling :color fc=darkred.BeginDraw:color fc=default.. This
is done for us 
and need not be done by our child class.

:p.
:hp2.Painting:ehp2.
:p.
The :hp2.virtual; abstract;:ehp2. methods that the canvas class implements, receive 
commands with coordinates relative to the widget. So if a button is drawing a 
rectangle around the button, :color fc=darkred.DoDrawRectangle:color fc=default. might
have the arguments 
DoDrawRectangle(0,0,Button.Width,Button.Height). Likely the button is not 
located in the extreme top/left of the form (0,0) so the commands must be 
translated to the top/left of the widget inside the top level canvas. 

:p.
:color fc=darkred.FDeltaX:color fc=default. and :color fc=darkred.FDeltaY:color fc=default. are
the two variables used to store the deltas of the 
points the canvas needs to translate where it is actually painted on the 
top level window. They are set in :color fc=darkred.TfpgCanvasBase.BeginDraw:color fc=default. and
are ready to 
use before :color fc=darkred.DoBeginDraw:color fc=default. is called.

:p.
:hp2.A painting ends:ehp2.
:p.
When the painting is done the method :color fc=darkred.DoPutBufferToScreen:color fc=default. is
called, but only 
for the top level canvas. Since there is only one buffer shared among all the 
widgets, the painting is completed on the top level widget and it only needs to 
move that buffer to the target window surface once.

:p.
:color fc=darkred.DoEndDraw:color fc=default. is called in response 
to :color fc=darkred.FreeResources:color fc=default.. :color fc=darkred.Canvas.EndDraw:color fc=default. calls
:color fc=darkred.DoPutBufferToScreen:color fc=default..

:note.FreeResources appears to never be called. A bug? 

:p.
:hp2.The canvas is destroyed:ehp2.
:p.
In the canvas destructor any remaining allocated implementation specific 
resources should be freed.

:h2.Clipping
:cgraphic.
þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ
 Clipping
þþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþþ
:ecgraphic.
:p.
With each child sharing a single surface clipping is a little more difficult. 
If a child widget sets a cliprect that is outside of it visible area it must 
be further clipped to prevent unwanted painting. For instance if a button is 
placed as a child outside the bounds, either partially or fully, of a panel it 
should not be draw outside of its parent. The protected method 
:color fc=darkred.TfpgCanvasBase.GetWidgetWindowRect:color fc=default. retrieves the valid area in top level 
coordinates of the visible area of a widget. It should be combined with the 
argument value of :color fc=darkred.DoSetClipRect:color fc=default. for the desired result.

:p.
When :color fc=darkred.DoClearCliprect:color fc=default. is called a cliprect equal to :color fc=darkred.GetWidgetWindowRect:color fc=default. is 
still required for a non top level canvas.




:euserdoc.
