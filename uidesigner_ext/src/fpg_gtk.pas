{ Gtk header for loading system palette

  Copyright (C) 2013 Krzysztof Dibowski dibowski_at_interia.pl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit fpg_Gtk;

{$mode objfpc}{$H+}

interface

uses
  dynlibs, ctypes;

const
  LIB_GTK = 'libgtk-x11-2.0.so';

const
  GTK_STATE_NORMAL      = 0; // The normal color, nothing special happening
  GTK_STATE_ACTIVE      = 1; // This state is used for buttons while the mouse is pressed
  GTK_STATE_PRELIGHT    = 2; // Prelight means mouse over effects. Usually the background will be slightly lighter
  GTK_STATE_SELECTED    = 3; // This state is used for example for selected text
  GTK_STATE_INSENSITIVE = 4; // This is the state when a widget is 'greyed out'. It is not active, and cannot be clicked on

  // Description of GtkStyle colors:
  // fg - Foreground color. Used for text on buttons. Also used for the button borders in some engines
  // bg - Background color. This is the background color of windows and buttons
  // text - Text color for text input widgets and lists (/GtkTreeView)
  // base - Background color of text widgets and lists

type
  PGtkWidget = Pointer;
  PGtkWindow = PGtkWidget;
  PGtkInvisible = Pointer;
  PGtkStyle = ^TGtkStyle;
  Pgchar = ^gchar;
  gchar = char;
  gint  = cint;
  PGdkColor = ^TGdkColor;
  gboolean = Boolean32;
  TGType = csize_t;
  gulong = culong;
  guint = cuint;
  guint32 = dword;
  PGData    =  pointer;
  GType = gulong;
  TGtkWindowType =  Longint;
  TGdkColor = record
       pixel : dword;
       red : word;
       green : word;
       blue : word;
    end;
  PGTypeClass = ^TGTypeClass;
  TGTypeClass = record
    g_type : GType;
  end;
  TGTypeInstance = record
    g_class : PGTypeClass;
  end;
  TGObject = record
       g_type_instance : TGTypeInstance;
       ref_count : guint;
       qdata : PGData;
    end;
  TGtkStyle = record
       parent_instance : TGObject;
       fg : array[0..4] of TGdkColor;
       bg : array[0..4] of TGdkColor;
       light : array[0..4] of TGdkColor;
       dark : array[0..4] of TGdkColor;
       mid : array[0..4] of TGdkColor;
       text : array[0..4] of TGdkColor;
       base : array[0..4] of TGdkColor;
       text_aa : array[0..4] of TGdkColor;
       black : TGdkColor;
       white : TGdkColor;
       font_desc : Pointer;
       xthickness : gint;
       ythickness : gint;
       fg_gc : array[0..4] of Pointer;
       bg_gc : array[0..4] of Pointer;
       light_gc : array[0..4] of Pointer;
       dark_gc : array[0..4] of Pointer;
       mid_gc : array[0..4] of Pointer;
       text_gc : array[0..4] of Pointer;
       base_gc : array[0..4] of Pointer;
       text_aa_gc : array[0..4] of Pointer;
       black_gc : Pointer;
       white_gc : Pointer;
       bg_pixmap : array[0..4] of Pointer;
       attach_count : gint;
       depth : gint;
       colormap : Pointer;
       private_font : Pointer;
       private_font_desc : Pointer;
       rc_style : Pointer;
       styles : Pointer;
       property_cache : Pointer;
       icon_factories : Pointer;
    end;

var
  LibGtkHandler: TLibHandle=0;
  gtk_rc_get_style: function (widget: PGtkWidget): PGtkStyle; cdecl = nil;
  gtk_style_lookup_color: function(AStyle: PGtkStyle; color_name: Pgchar; color: PGdkColor): gboolean; cdecl;
  gtk_invisible_new: function : PGtkWidget; cdecl;
  gtk_widget_destroy: procedure (AWidget: PGtkWidget); cdecl;
  gtk_widget_new: function (type_: TGType; first_property_name: Pgchar; args: array of const): PGtkWidget; cdecl;
  gtk_button_new: function :PGtkWidget; cdecl;
  gtk_widget_get_style: function (widget:PGtkWidget):PGtkStyle; cdecl=nil;
  gtk_init: procedure (argc:Plongint; argv:PPPchar); cdecl;
  gtk_window_new: function (_type:TGtkWindowType):PGtkWidget; cdecl;
  gtk_widget_show: procedure (widget:PGtkWidget); cdecl;
  gtk_window_resize: procedure (window:PGtkWindow; width:gint; height:gint); cdecl;
  gtk_widget_hide: procedure (widget:PGtkWidget); cdecl;

  function LoadGtkLib: Boolean;
  procedure UnloadGtkLib;

implementation

function LoadGtkLib: Boolean;
begin
  if (LibGtkHandler<>0) then Exit(True);

  LibGtkHandler := LoadLibrary(LIB_GTK);

  Result := LibGtkHandler<>0;

  if Result then
  begin
    Pointer(gtk_rc_get_style) := GetProcedureAddress(LibGtkHandler, 'gtk_rc_get_style');
    Pointer(gtk_style_lookup_color) := GetProcedureAddress(LibGtkHandler, 'gtk_style_lookup_color');
    Pointer(gtk_invisible_new) := GetProcedureAddress(LibGtkHandler, 'gtk_invisible_new');
    Pointer(gtk_widget_destroy) := GetProcedureAddress(LibGtkHandler, 'gtk_widget_destroy');
    Pointer(gtk_widget_new) := GetProcedureAddress(LibGtkHandler, 'gtk_widget_new');
    Pointer(gtk_button_new) := GetProcedureAddress(LibGtkHandler, 'gtk_button_new');
    Pointer(gtk_widget_get_style) := GetProcedureAddress(LibGtkHandler, 'gtk_widget_get_style');
    Pointer(gtk_init) := GetProcedureAddress(LibGtkHandler, 'gtk_init');
    Pointer(gtk_window_new) := GetProcedureAddress(LibGtkHandler, 'gtk_window_new');
    Pointer(gtk_widget_show) := GetProcedureAddress(LibGtkHandler, 'gtk_widget_show');
    Pointer(gtk_window_resize) := GetProcedureAddress(LibGtkHandler, 'gtk_window_resize');
    Pointer(gtk_widget_hide) := GetProcedureAddress(LibGtkHandler, 'gtk_widget_hide');
  end;
end;

procedure UnloadGtkLib;
begin
  if LibGtkHandler>0 then
    UnloadLibrary(LibGtkHandler);
end;


end.

