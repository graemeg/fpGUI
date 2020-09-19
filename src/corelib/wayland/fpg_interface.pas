{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2018 by Graeme Geldenhuys.

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

{$I fpg_defines.inc}

interface

uses
  fpg_wayland;

type
  TfpgFontResourceImpl  = class(TfpgWaylandFontResource);
  TfpgImageImpl         = class(TfpgWaylandImage);
  TfpgCanvasImpl        = class(TfpgWaylandCanvas);
  TfpgWindowImpl        = class(TfpgWaylandWindow);
  TfpgApplicationImpl   = class(TfpgWaylandApplication);
  TfpgClipboardImpl     = class(TfpgWaylandClipboard);
  TfpgFileListImpl      = class(TfpgWaylandFileList);
  TfpgMimeDataImpl      = class(TfpgWaylandMimeData);
  TfpgDragImpl          = class(TfpgWaylandDrag);
  TfpgDropImpl          = class(TfpgWaylandDrop);
  TfpgTimerImpl         = class(TfpgWaylandTimer);
  TfpgSystemTrayHandler = class(TfpgWaylandSystemTrayHandler);

implementation

end.

