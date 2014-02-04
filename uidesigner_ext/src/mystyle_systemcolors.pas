
{ This is a demo style using the native system palette
Copyright (C) 2013 Krzysztof Dibowski dibowski_at_interia.pl and 
the MyStyle demo of fpGUI. 
  Fred van Stappen fiens@hotmail.com} 

{ fpGUI style which load native system palette on Windows and Linux

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

unit MyStyle_SystemColors;

{$mode objfpc}{$H+}

interface

uses
 // Classes, fpg_main, fpg_base;
Classes, fpg_main, fpg_base;

type

  { Tmystyle_SystemColors }

  Tmystyle_SystemColors = class(TfpgStyle)
  private
    {$IFDEF LINUX}
    procedure LoadGtkPalette;
    {$ENDIF}
    {$IFDEF WINDOWS}
    procedure LoadWindowsPalette;
    {$ENDIF}
    { General }
    procedure   DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord); override;
    { Buttons }
    procedure   DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; AFlags: TfpgButtonFlags); override;
    { Menus }
    procedure   DrawMenuRow(ACanvas: TfpgCanvas; r: TfpgRect; AFlags: TfpgMenuItemFlags); override;
    procedure   DrawMenuBar(ACanvas: TfpgCanvas; r: TfpgRect; ABackgroundColor: TfpgColor); override;

  public
    constructor Create; override;
  end;


implementation

uses
  fpg_stylemanager, {$IFDEF LINUX}fpg_gtk{$ELSE}{$IFDEF WINDOWS}fpg_WinAPI{$ENDIF}{$ENDIF};

{ Tmystyle_SystemColors }

{$IFDEF LINUX}
procedure Tmystyle_SystemColors.LoadGtkPalette;
var
  w: fpg_gtk.PGtkWidget=nil;
  st: fpg_gtk.PGtkStyle=nil;
begin
  try
    if not LoadGtkLib then Exit;

    fpg_gtk.gtk_init(@argc, @argv);
    w := fpg_gtk.gtk_window_new(0);
    if w=nil then Exit;
    fpg_gtk.gtk_window_resize(w,1,1);
    fpg_gtk.gtk_widget_show(w);
    st := fpg_gtk.gtk_widget_get_style(w);
    fpg_gtk.gtk_widget_hide(w);

    if st<>nil then
    begin
      fpgSetNamedColor(clWindowBackground,
        fpgColor(st^.bg[GTK_STATE_NORMAL].red div 256, st^.bg[GTK_STATE_NORMAL].green div 256, st^.bg[GTK_STATE_NORMAL].blue div 256));
      fpgSetNamedColor(clButtonFace,
        fpgColor(st^.bg[GTK_STATE_INSENSITIVE].red div 256, st^.bg[GTK_STATE_INSENSITIVE].green div 256, st^.bg[GTK_STATE_INSENSITIVE].blue div 256));
      fpgSetNamedColor(clShadow1,
        fpgColor(st^.fg[GTK_STATE_INSENSITIVE].red div 256, st^.fg[GTK_STATE_INSENSITIVE].green div 256, st^.fg[GTK_STATE_INSENSITIVE].blue div 256));
      fpgSetNamedColor(clShadow2,
        fpgColor(st^.dark[GTK_STATE_INSENSITIVE].red div 256, st^.dark[GTK_STATE_INSENSITIVE].green div 256, st^.dark[GTK_STATE_INSENSITIVE].blue div 256));
      fpgSetNamedColor(clSelection,
        fpgColor(st^.base[GTK_STATE_SELECTED].red div 256, st^.base[GTK_STATE_SELECTED].green div 256, st^.base[GTK_STATE_SELECTED].blue div 256));
      fpgSetNamedColor(clSelectionText,
        fpgColor(st^.text[GTK_STATE_SELECTED].red div 256, st^.text[GTK_STATE_SELECTED].green div 256, st^.text[GTK_STATE_SELECTED].blue div 256));
      fpgSetNamedColor(clText1,
        fpgColor(st^.text[GTK_STATE_NORMAL].red div 256, st^.text[GTK_STATE_NORMAL].green div 256, st^.text[GTK_STATE_NORMAL].blue div 256));
      fpgSetNamedColor(clGridSelection,
        fpgColor(st^.base[GTK_STATE_SELECTED].red div 256, st^.base[GTK_STATE_SELECTED].green div 256, st^.base[GTK_STATE_SELECTED].blue div 256));
      fpgSetNamedColor(clGridSelectionText,
        fpgColor(st^.text[GTK_STATE_SELECTED].red div 256, st^.text[GTK_STATE_SELECTED].green div 256, st^.text[GTK_STATE_SELECTED].blue div 256));
      fpgSetNamedColor(clGridInactiveSel,
        fpgColor(st^.text_aa[GTK_STATE_SELECTED].red div 256, st^.text_aa[GTK_STATE_SELECTED].green div 256, st^.text_aa[GTK_STATE_SELECTED].blue div 256));
      fpgSetNamedColor(clGridInactiveSelText,
        fpgColor(st^.text[GTK_STATE_NORMAL].red div 256, st^.text[GTK_STATE_NORMAL].green div 256, st^.text[GTK_STATE_NORMAL].blue div 256));
      fpgSetNamedColor(clInactiveSel,
        fpgColor(st^.text_aa[GTK_STATE_SELECTED].red div 256, st^.text_aa[GTK_STATE_SELECTED].green div 256, st^.text_aa[GTK_STATE_SELECTED].blue div 256));
      fpgSetNamedColor(clInactiveSelText,
        fpgColor(st^.text[GTK_STATE_NORMAL].red div 256, st^.text[GTK_STATE_NORMAL].green div 256, st^.text[GTK_STATE_NORMAL].blue div 256));
      fpgSetNamedColor(clBoxColor,
        fpgColor(st^.base[GTK_STATE_NORMAL].red div 256, st^.base[GTK_STATE_NORMAL].green div 256, st^.base[GTK_STATE_NORMAL].blue div 256));
      fpgSetNamedColor(clListBox,
        fpgColor(st^.base[GTK_STATE_NORMAL].red div 256, st^.base[GTK_STATE_NORMAL].green div 256, st^.base[GTK_STATE_NORMAL].blue div 256));
      fpgSetNamedColor(clGridLines,
        fpgColor(st^.mid[GTK_STATE_NORMAL].red div 256, st^.mid[GTK_STATE_NORMAL].green div 256, st^.mid[GTK_STATE_NORMAL].blue div 256));
      fpgSetNamedColor(clSplitterGrabBar,
        fpgColor(st^.bg[GTK_STATE_ACTIVE].red div 256, st^.bg[GTK_STATE_ACTIVE].green div 256, st^.bg[GTK_STATE_ACTIVE].blue div 256));
      fpgSetNamedColor(clHilite2,
        fpgColor(st^.light[GTK_STATE_NORMAL].red div 256, st^.light[GTK_STATE_NORMAL].green div 256, st^.light[GTK_STATE_NORMAL].blue div 256));
      fpgSetNamedColor(clHilite1,
        fpgColor(st^.mid[GTK_STATE_NORMAL].red div 256, st^.mid[GTK_STATE_NORMAL].green div 256, st^.mid[GTK_STATE_NORMAL].blue div 256));
      fpgSetNamedColor(clScrollBar,
        fpgColor(st^.bg[GTK_STATE_ACTIVE].red div 256, st^.bg[GTK_STATE_ACTIVE].green div 256, st^.bg[GTK_STATE_ACTIVE].blue div 256));
      fpgSetNamedColor(clHintWindow,
        fpgColor(st^.dark[GTK_STATE_NORMAL].red div 256, st^.dark[GTK_STATE_NORMAL].green div 256, st^.dark[GTK_STATE_NORMAL].blue div 256));
      fpgSetNamedColor(clTextCursor,
        fpgColor(st^.text[GTK_STATE_NORMAL].red div 256, st^.text[GTK_STATE_NORMAL].green div 256, st^.text[GTK_STATE_NORMAL].blue div 256));
    end;
  finally
    if w<>nil then
      fpg_gtk.gtk_widget_destroy(w);
    UnloadGtkLib;
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
procedure Tmystyle_SystemColors.LoadWindowsPalette;
var
  c: DWord;

  function GetRValue(rgb : longint) : BYTE;
  begin
    Result := BYTE(rgb);
  end;
  function GetGValue(rgb : longint) : BYTE;
  begin
    Result := BYTE((WORD(rgb)) shr 8);
  end;
  function GetBValue(rgb : longint) : BYTE;
  begin
    Result := BYTE(rgb shr 16);
  end;

begin
  try
    if not LoadUser32Lib then Exit;

    c := fpg_WinAPI.GetSysColor(COLOR_BTNFACE);
    fpgSetNamedColor(clWindowBackground,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    fpgSetNamedColor(clButtonFace,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_GRAYTEXT);
    fpgSetNamedColor(clShadow1,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_BTNSHADOW);
    fpgSetNamedColor(clShadow2,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_HIGHLIGHT);
    fpgSetNamedColor(clSelection,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_HIGHLIGHTTEXT);
    fpgSetNamedColor(clSelectionText,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_CAPTIONTEXT);
    fpgSetNamedColor(clText1,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_HIGHLIGHT);
    fpgSetNamedColor(clGridSelection,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_HIGHLIGHTTEXT);
    fpgSetNamedColor(clGridSelectionText,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_GRAYTEXT); // could not find correct color for this one
    fpgSetNamedColor(clGridInactiveSel,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_HIGHLIGHTTEXT);
    fpgSetNamedColor(clGridInactiveSelText,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_GRAYTEXT); // could not find correct color for this one
    fpgSetNamedColor(clInactiveSel,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_HIGHLIGHTTEXT);
    fpgSetNamedColor(clInactiveSelText,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_WINDOW);
    fpgSetNamedColor(clBoxColor,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_WINDOW);
    fpgSetNamedColor(clListBox,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_BTNHIGHLIGHT);
    fpgSetNamedColor(clGridLines,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_BTNHIGHLIGHT);
    fpgSetNamedColor(clSplitterGrabBar,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_BTNHIGHLIGHT);
    fpgSetNamedColor(clHilite2,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_BTNFACE);
    fpgSetNamedColor(clHilite1,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_SCROLLBAR);
    fpgSetNamedColor(clScrollBar,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_INFOBK);
    fpgSetNamedColor(clHintWindow,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
    c := fpg_WinAPI.GetSysColor(COLOR_CAPTIONTEXT);
    fpgSetNamedColor(clTextCursor,
      fpgColor(GetRValue(c), GetGValue(c), GetBValue(c)));
  finally
    UnloadUser32Lib;
  end;
end;
{$ENDIF}


procedure Tmystyle_SystemColors.DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord);
var
  r: TfpgRect;
begin
  r.SetRect(x, y, w, h);
  ACanvas.SetColor(clShadow1);
  ACanvas.Clear(clsilver);
  ACanvas.DrawRectangle(r);
end;

procedure Tmystyle_SystemColors.DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; AFlags: TfpgButtonFlags);
var
  r: TfpgRect;
begin
  r.SetRect(x, y, w, h);

  if btfIsDefault in AFlags then
  begin
    ACanvas.SetColor(TfpgColor($7b7b7b));
    ACanvas.SetLineStyle(1, lsSolid);
    ACanvas.DrawRectangle(r);
    InflateRect(r, -1, -1);
    Exclude(AFlags, btfIsDefault);
    fpgStyle.DrawButtonFace(ACanvas, r.Left, r.Top, r.Width, r.Height, AFlags);
    Exit; //==>
  end;

  // Clear the canvas
  ACanvas.SetColor(clWindowBackground);
  ACanvas.FillRectangle(r);

  if (btfFlat in AFlags) and not (btfIsPressed in AFlags) then
    Exit; // no need to go further

  // outer rectangle
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.SetColor(TfpgColor($a6a6a6));
  ACanvas.DrawRectangle(r);

  // so we don't paint over the border
  InflateRect(r, -1, -1);
  // now paint the face of the button
  if (btfIsPressed in AFlags) then
  begin
    ACanvas.GradientFill(r, TfpgColor($cccccc), TfpgColor($e4e4e4), gdVertical);
  end
  else
  begin
    ACanvas.GradientFill(r, TfpgColor($fafafa), TfpgColor($e2e2e2), gdVertical);
    ACanvas.SetColor(TfpgColor($cccccc));
    ACanvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);   // right
    ACanvas.DrawLine(r.Right, r.Bottom, r.Left, r.Bottom);   // bottom
  end;
end;

procedure Tmystyle_SystemColors.DrawMenuRow(ACanvas: TfpgCanvas; r: TfpgRect; AFlags: TfpgMenuItemFlags);
begin
  inherited DrawMenuRow(ACanvas, r, AFlags);
  if (mifSelected in AFlags) and not (mifSeparator in AFlags) then
    ACanvas.GradientFill(r, TfpgColor($fec475), TfpgColor($fb9d24), gdVertical);
end;

procedure Tmystyle_SystemColors.DrawMenuBar(ACanvas: TfpgCanvas; r: TfpgRect; ABackgroundColor: TfpgColor);
var
  FLightColor: TfpgColor;
  FDarkColor: TfpgColor;
begin
  // a possible future theme option
  FLightColor := TfpgColor($f0ece3);  // color at top of menu bar
  FDarkColor  := TfpgColor($beb8a4);  // color at bottom of menu bar
  ACanvas.GradientFill(r, FLightColor, FDarkColor, gdVertical);

  // inner bottom line
  ACanvas.SetColor(clShadow1);
  ACanvas.DrawLine(r.Left, r.Bottom-1, r.Right+1, r.Bottom-1);   // bottom
  // outer bottom line
  ACanvas.SetColor(clWhite);
  ACanvas.DrawLine(r.Left, r.Bottom, r.Right+1, r.Bottom);   // bottom
end;


constructor Tmystyle_SystemColors.Create;
begin
  inherited Create;
 fpgSetNamedColor(clWindowBackground, TfpgColor($eeeeec));

  {$IFDEF LINUX}
  LoadGtkPalette;
  {$ENDIF}
  {$IFDEF WINDOWS}
  LoadWindowsPalette;
  {$ENDIF}
end;


initialization
  fpgStyleManager.RegisterClass('My Style System Colors', Tmystyle_SystemColors);

end.

