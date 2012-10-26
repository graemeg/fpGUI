{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2012 See the file AUTHORS.txt, included in this
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
  TfpgFontResourceImpl  = class(TfpgX11FontResource);
  TfpgImageImpl         = class(TfpgX11Image);
  TfpgCanvasImpl        = class(TfpgX11Canvas);
  TfpgWindowImpl        = class(TfpgX11Window);
  TfpgApplicationImpl   = class(TfpgX11Application);
  TfpgClipboardImpl     = class(TfpgX11Clipboard);
  TfpgFileListImpl      = class(TfpgX11FileList);
  TfpgMimeDataImpl      = class(TfpgX11MimeData);
  TfpgDragImpl          = class(TfpgX11Drag);
  TfpgTimerImpl         = class(TfpgX11Timer);
  TfpgSystemTrayHandler = class(TfpgX11SystemTrayHandler);

implementation

end.

