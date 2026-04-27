{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2026 by Graeme Geldenhuys.

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
  fpg_gdi
  {$ifdef AGGCanvas}
  , fpg_gdi_buffer_manager
  {$endif}
  ;

type
  TfpgFontResourceImpl  = class(TfpgGDIFontResource);
  TfpgImageImpl         = class(TfpgGDIImage);
  { Suppress deprecation note: TfpgCanvasImpl retains the native canvas
    during the transition period. Remove once THybridCanvas is the sole base. }
  {$NOTES OFF}
  TfpgCanvasImpl        = class(TfpgGDICanvas);
  {$NOTES ON}
  TfpgWindowImpl        = class(TfpgGDIWindow);
  TfpgApplicationImpl   = class(TfpgGDIApplication);
  TfpgClipboardImpl     = class(TfpgGDIClipboard);
  TfpgFileListImpl      = class(TfpgGDIFileList);
  TfpgMimeDataImpl      = class(TfpgGDIMimeDataBase);
  TfpgDragImpl          = class(TfpgGDIDrag);
  TfpgDropImpl          = class(TfpgGDIDrop);
  TfpgTimerImpl         = class(TfpgGDITimer);
  TfpgSystemTrayHandler = class(TfpgGDISystemTrayIcon);

implementation

{$ifdef AGGCanvas}
uses
  fpg_hybrid_canvas,
  fpg_fontmanager,
  fpg_gdi_agg_fontresource;

initialization
  CreateBufferManager  := @CreateGDIBufferManager;
  AggFontResourceClass := TfpgGDIAggFontResource;
{$endif}

end.

