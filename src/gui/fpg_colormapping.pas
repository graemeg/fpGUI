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
      This unit contains some color conversion/mapping functions.
}

unit fpg_ColorMapping;

{$mode objfpc}{$H+}

interface

uses
  Classes, fpg_base;

 // IN these functions the following definitions apply:
 // Hue: 0 - 1535
 //   red = 0  green = 512  blue = 1024
 // Saturation: 0 - 1
 //   grey (no color) = 0  max color = 1
 // Value: 0 - 1
 //   black =0   max brightness = 1

procedure RGBToHSV(C: TfpgColor; out Hue: longint; out Saturation, Value: double);
function HSVToRGB(const H: longint; const S, V: double): TfpgColor;

implementation

uses
  Math;

procedure RGBToHSV(C: TfpgColor; out Hue: longint; out Saturation, Value: double);
var
  r, g, b: longint;
  hi, lo: longint;
  d:   longint;
  rgb: TFPColor;
begin
  rgb   := fpgColorToFPColor(C);
  r     := rgb.Red;
  g     := rgb.Green;
  b     := rgb.Blue;
  hi    := max(max(r, g), b);
  lo    := min(min(r, g), b);
  d     := hi - lo;
  Value := hi / 256;
  if d > 0 then
  begin
    if r = hi then
      Hue := 256 * (g - b) div d
    else if g = hi then
      Hue := 512 + 256 * (b - r) div d
    else
      Hue := 1024 + 256 * (r - g) div d;
    if Hue < 0 then
      Hue := Hue + 1536;
  end
  else
    Hue := 0; // doesn't matter (grey: Sat = 0)

  if hi > 0 then
    Saturation := d / hi
  else
    Saturation := 0; // doesn't matter (black: Val = 0
end;

function HSVToRGB(const H: longint; const S, V: double): TfpgColor;
var
  r, g, b: longint;
  rgb: TFPColor;
begin
  if (h < 0) or (h > 1535) or (S < 0) or (S > 1) or (V < 0) or (V > 1) then
  begin
    // Invalid value, use black
    Result := 0;
    exit;
  end;
  case h div 256 of
    0:
    begin
      r := 255;
      g := h;
      b := 0;
    end;
    1:
    begin
      r := 511 - h;
      g := 255;
      b := 0;
    end;
    2:
    begin
      r := 0;
      g := 255;
      b := h - 512;
    end;
    3:
    begin
      r := 0;
      g := 1023 - h;
      b := 255;
    end;
    4:
    begin
      r := h - 1024;
      g := 0;
      b := 255;
    end;
    5:
    begin
      r := 255;
      g := 0;
      b := 1535 - h;
    end;
  end;
  r      := Trunc(V * (255 - S * (255 - r)));
  g      := Trunc(V * (255 - S * (255 - g)));
  b      := Trunc(V * (255 - S * (255 - b)));
  rgb.Red := r;
  rgb.Green := g;
  rgb.Blue := b;
  Result := FPColorTofpgColor(rgb);
end;


end.

