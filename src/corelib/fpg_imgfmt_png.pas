{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2012 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      PNG image loading using the Free Pascal library: fcl-image
}

unit fpg_imgfmt_png;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  fpg_base,
  fpg_main,
  FPImage,
  FPReadPNG;

function LoadImage_PNG(const AFileName: TfpgString): TfpgImage;
function LoadImage_PNGcrop(const AMaxWidth, AMaxHeight: integer; const AFileName: TfpgString): TfpgImage;


implementation

uses
  fpg_utils;

function LoadImage_PNG(const AFileName: TfpgString): TfpgImage;
var
  i, j: integer;
  colorA: TFPColor;   // struct Red, Green, Blue, Alpha: word
  colorB: TfpgColor;  // ONE long 32-bit-word
  imga: TFPCustomImage;
  imgb: TfpgImage;
  xlocal, ylocal: integer;
begin
  Result := nil;
  if not fpgFileExists(AFileName) then
    Exit; //==>

  imga := TFPMemoryImage.Create(0, 0);
  try
    imga.LoadFromFile(AFileName, TFPReaderPNG.Create); // auto size image
  except
    imga := nil;
    imgb := nil;
  end;
  if imga <> nil then
  begin
    xlocal := imga.Width;
    ylocal := imga.Height;
    imgb   := TfpgImage.Create;
    imgb.AllocateImage(32, xlocal, ylocal); // 32=colordepth
    for i := 0 to ylocal - 1 do
      for j := 0 to xlocal - 1 do
      begin
        colorA := imga.Colors[j, i];
        colorB := (colorA.Blue shr 8) or (colorA.Green and $FF00) or ((colorA.Red and $FF) shl 16) or ((colorA.Alpha and $FF) shl 24);
        imgb.Colors[j, i] := colorB;
      end;
    imgb.UpdateImage;
  end;
  imga.Free;
  Result := imgb;
end;

function LoadImage_PNGcrop(const AMaxWidth, AMaxHeight: integer; const AFileName: TfpgString): TfpgImage;
var
  i, j: integer;
  colorA: TFPColor;   // struct Red, Green, Blue, Alpha: word
  colorB: TfpgColor;  // ONE long 32-bit-word
  imga: TFPCustomImage;
  imgb: TfpgImage;
  xlocal, ylocal: integer;
begin
  Result := nil;
  if not fpgFileExists(AFileName) then
    Exit; //==>

          // Maximum image size of AMaxWidth by AMaxHeight.
          // Actual image imga.Width (AMaxWidth) and imga.Height (AMaxHeight).
          // Calculated to fit the image within required size: xlocal, ylocal
  imga := TFPMemoryImage.Create(0, 0);
  try
    imga.LoadFromFile(AFileName, TFPReaderPNG.Create); // auto size image
  except
    imga := nil;
    imgb := nil;
  end;
  if imga <> nil then
  begin
    if imga.Width > AMaxWidth then
      xlocal := AMaxWidth
    else
      xlocal := imga.Width;
    if imga.Height > AMaxHeight then
      ylocal := AMaxHeight
    else
      ylocal := imga.Height;
    imgb := TfpgImage.Create;
    imgb.AllocateImage(32, xlocal, ylocal); // 32=colordepth
    for i := 0 to ylocal - 1 do
      for j := 0 to xlocal - 1 do
      begin
        colorA := imga.Colors[j, i];
        colorB := (colorA.Blue shr 8) or (colorA.Green and $FF00) or ((colorA.Red and $FF00) shl 8);
        imgb.Colors[j, i] := colorB;
      end;
    imgb.UpdateImage;
  end;
  imga.Free;
  Result := imgb;
end;


end.

