{
    fpGUI  -  Free Pascal GUI Library
    
    fpIMG interface declarations. This is the main glue code between 
    different fpIMG submodules such as ImageIO and fpGFX.
    
    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit fpImg;

{$IFDEF Debug}
{$ASSERTIONS On}
{$ENDIF}

interface

uses
  Classes, gfxbase, ImageIO, fpgfx;


function CreateImageFromFile(AScreen: TFScreen; AReader: TImageReaderClass;
  const AFilename: String): TFBitmap;

function CreateImageFromStream(AScreen: TFScreen; AReader: TImageReaderClass;
  AStream: TStream): TFBitmap;



implementation


function CreateImageFromFile(AScreen: TFScreen; AReader: TImageReaderClass;
  const AFilename: String): TFBitmap;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFilename, fmOpenRead);
  try
    Result := CreateImageFromStream(AScreen, AReader, Stream);
  finally
    Stream.Free;
  end;
end;


function CreateImageFromStream(AScreen: TFScreen; AReader: TImageReaderClass;
  AStream: TStream): TFBitmap;
var
  Reader: TImageReader;
  Data: Pointer;
  Stride: LongWord;
  Palette: TGfxPalette;
begin
  Reader := AReader.Create;
  try
    Reader.ProcessHeaderData(AStream);
    Result := TFBitmap.Create(Reader.Width, Reader.Height, Reader.PixelFormat);
    if Reader.PaletteSize > 0 then
    begin
      Palette := TGfxPalette.create(Reader.PaletteSize, Reader.Palette);
      try
        Result.Palette := Palette;
      finally
        Palette.Release;
      end;
    end;
    try
      Result.Lock(Data, Stride);
      try
        Reader.SetImageSegmentBuffer(Data, Stride, Reader.Height);
        Reader.ProcessImageData(AStream);
      finally
        Result.Unlock;
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    Reader.Free;
  end;
end;

end.

