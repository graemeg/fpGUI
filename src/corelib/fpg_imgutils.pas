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
      Some handly image manipulation functions to use with TfpgImage class.
      Included is a gray color conversion matrix.
}

unit fpg_imgutils;

{$mode objfpc}{$H+}

{ TODO : Make the conversion matrix a plugable architecture. Similar to the
         interpolation handling in TfpgCanvas. }

interface

uses
  fpg_base,
  fpg_main;

type
  TGrayConvMatrix = record
    red: single;
    green: single;
    blue: single;
  end;

var
  GrayConvMatrix: TGrayConvMatrix;
  GrayBrightness: Boolean;
  GrayBrightnessPercentage: integer;

const
  GCM_NTSC: TGrayConvMatrix = (red:0.299; green:0.587; blue:0.114);                 // NTSC method
  GCM_Mathematical: TGrayConvMatrix = (red:0.334; green:0.333; blue:0.333);         // Intensity method
  GCM_Photoshop: TGrayConvMatrix = (red:0.212671; green:0.715160; blue:0.072169);   // Y of YUV from B/W TV's


procedure fpgApplyGreyFilter(var AImg: TfpgImage);
function fpgCalculateGray(const AFrom: TfpgColor; const ABrighter: boolean = False; const APercent: integer = 0): TfpgColor;


implementation


procedure fpgApplyGreyFilter(var AImg: TfpgImage);
var
  x, y: integer;
  c: TfpgColor;
begin
  for x := 0 to AImg.Width-1 do
  begin
    for y := 0 to AImg.Height-1 do
    begin
      c := AImg.Colors[x, y];
      AImg.Colors[x, y] := fpgCalculateGray(c, GrayBrightness, GrayBrightnessPercentage);
    end;
  end;
  AImg.UpdateImage;
end;


{ AFrom is the original color we want to change
  ABrighter = True goes to direction of White. False goes to direction of Black
  APercent =  0 zero is straight conversion to gray. 100% is pure black or
              white, depending on ABrighter value. }
function fpgCalculateGray(const AFrom: TfpgColor; const ABrighter: boolean = False; const APercent: integer = 0): TfpgColor;
var
  g: integer;
  rgb: TFPColor;
begin
  with GrayConvMatrix do
  begin
    rgb := fpgColorToFPColor(AFrom);
    g := round(red*rgb.red + green*rgb.green + blue*rgb.blue);

    if ABrighter then
      g := trunc(255 - ((255 - g) * (100 - APercent) / 100))
    else
      g := trunc(g * (100 - APercent) / 100);

    if (g < 0) then g := 0;
    if (g > 255) then g := 255;

    rgb.Red := g;
    rgb.Green := g;
    rgb.Blue := g;
  end;
  Result := FPColorTofpgColor(rgb);
end;


initialization
  GrayConvMatrix := GCM_NTSC;
  GrayBrightness := True;
  GrayBrightnessPercentage := 40;

end.

