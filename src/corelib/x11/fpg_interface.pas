{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit defines alias types to bind each backend graphics library
      to fpg_main without the need for IFDEF's
}

unit fpg_interface;

{$mode objfpc}{$H+}

interface

uses
  fpg_x11;

type
  TfpgFontResourceImpl  = TfpgX11FontResource;
  TfpgImageImpl         = TfpgX11Image;
  TfpgCanvasImpl        = TfpgX11Canvas;
  TfpgWindowImpl        = TfpgX11Window;
  TfpgApplicationImpl   = TfpgX11Application;
  TfpgClipboardImpl     = TfpgX11Clipboard;
  TfpgFileListImpl      = TfpgX11FileList;
  TfpgMimeDataImpl      = TfpgX11MimeData;
  TfpgDragImpl          = TfpgX11Drag;
  TfpgTimerImpl         = TfpgX11Timer;

implementation

end.

