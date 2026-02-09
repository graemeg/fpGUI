{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2019 by Graeme Geldenhuys.

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
  fpg_cocoa;

type
  TfpgFontResourceImpl  = class(TfpgCocoaFontResource);
  TfpgImageImpl         = class(TfpgCocoaImage);
  TfpgCanvasImpl        = class(TfpgCocoaCanvas);
  TfpgWindowImpl        = class(TfpgCocoaWindow);
  TfpgApplicationImpl   = class(TfpgCocoaApplication);
  TfpgClipboardImpl     = class(TfpgCocoaClipboard);
  TfpgFileListImpl      = class(TfpgCocoaFileList);
  TfpgMimeDataImpl      = class(TfpgCocoaMimeData);
  TfpgDragImpl          = class(TfpgCocoaDrag);
  TfpgDropImpl          = class(TfpgCocoaDrop);
  TfpgTimerImpl         = class(TfpgCocoaTimer);
  TfpgSystemTrayHandler = class(TfpgCocoaSystemTrayHandler);

implementation

end.

