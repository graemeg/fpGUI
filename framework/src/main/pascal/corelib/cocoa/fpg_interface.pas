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
  fpg_cocoa
  {$ifdef AGGCanvas}
  , fpg_cocoa_buffer_manager
  {$endif}
  ;

type
  TfpgFontResourceImpl  = class(TfpgCocoaFontResource);
  TfpgImageImpl         = class(TfpgCocoaImage);
  { Suppress deprecation note: TfpgCanvasImpl retains the native canvas
    during the transition period. Remove once THybridCanvas is the sole base. }
  {$NOTES OFF}
  TfpgCanvasImpl        = class(TfpgCocoaCanvas);
  {$NOTES ON}
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

{$ifdef AGGCanvas}
uses
  fpg_hybrid_canvas,
  fpg_fontmanager,
  fpg_freetype_agg_fontresource;

initialization
  CreateBufferManager  := @CreateCocoaBufferManager;
  AggFontResourceClass := TfpgFreeTypeFontResource;
{$endif}

end.

