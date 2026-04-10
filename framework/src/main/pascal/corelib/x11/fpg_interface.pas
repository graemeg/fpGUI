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
  fpg_x11
  {$ifdef AGGCanvas}
  , fpg_x11_buffer_manager
  {$endif}
  ;

type
  TfpgFontResourceImpl  = class(TfpgX11FontResource);
  TfpgImageImpl         = class(TfpgX11Image);
  { Suppress deprecation hint: TfpgCanvasImpl retains the native canvas
    during the transition period. Remove once THybridCanvas is the sole base. }
  {$NOTES OFF}
  TfpgCanvasImpl        = class(TfpgX11Canvas);
  {$NOTES ON}
  TfpgWindowImpl        = class(TfpgX11Window);
  TfpgApplicationImpl   = class(TfpgX11Application);
  TfpgClipboardImpl     = class(TfpgX11Clipboard);
  TfpgFileListImpl      = class(TfpgX11FileList);
  TfpgMimeDataImpl      = class(TfpgX11MimeData);
  TfpgDragImpl          = class(TfpgX11Drag);
  TfpgDropImpl          = class(TfpgX11Drop);
  TfpgTimerImpl         = class(TfpgX11Timer);
  TfpgSystemTrayHandler = class(TfpgX11SystemTrayHandler);

implementation

{$ifdef AGGCanvas}
uses
  fpg_hybrid_canvas,
  fpg_fontmanager,
  fpg_freetype_agg_fontresource;

initialization
  CreateBufferManager  := @CreateX11BufferManager;
  AggFontResourceClass := TfpgFreeTypeFontResource;
{$endif}

end.

