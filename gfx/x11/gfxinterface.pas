{
    fpGUI  -  Free Pascal Graphical User Interface

    GFXInterface  -  Default target selection unit for X11 target

    Copyright (C) 2000 - 2006 See the file AUTHORS, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit gfxinterface;

interface

uses
  GFX_X11;

type

  TDefCanvas = TX11Canvas;

  TDefFont = TX11Font;

  TDefScreen = TX11Screen;

  TDefApplication = TX11Application;
  
  TDefWindow = TX11Window;
  
  TDefBitmap = TX11Bitmap;


implementation

end.

