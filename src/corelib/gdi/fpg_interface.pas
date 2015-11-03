{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2015 by Graeme Geldenhuys.

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
  fpg_gdi;

type
  TfpgFontResourceImpl  = class(TfpgGDIFontResource);
  TfpgImageImpl         = class(TfpgGDIImage);
  TfpgCanvasImpl        = class(TfpgGDICanvas);
  TfpgWindowImpl        = class(TfpgGDIWindow);
  TfpgApplicationImpl   = class(TfpgGDIApplication);
  TfpgClipboardImpl     = class(TfpgGDIClipboard);
  TfpgFileListImpl      = class(TfpgGDIFileList);
  TfpgMimeDataImpl      = class(TfpgGDIMimeDataBase);
  TfpgDragImpl          = class(TfpgGDIDrag);
  TfpgTimerImpl         = class(TfpgGDITimer);
  TfpgSystemTrayHandler = class(TfpgGDISystemTrayIcon);

implementation

end.

