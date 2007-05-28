{
    fpGUI  -  Free Pascal GUI Library

    Example: Display BMP file with monochrome mask

    Copyright (C) 2000 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

program MaskTest;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

uses
  Classes, GFXBase, fpGFX, fpImg, BMPReader;

type
  TMainWindow = class(TFWindow)
  private
    Image2, Image4, Image8, Image24, Mask: TFBitmap;
    Image2Canvas, Image4Canvas, Image8Canvas,
      Image24Canvas, MaskCanvas: TFCanvas;
  protected
    procedure   Paint(Sender: TObject; const ARect: TRect);
  public
    constructor Create; overload;
    destructor  Destroy; override;
  end;

constructor TMainWindow.Create;
begin
  inherited Create(nil, [woWindow]);

  // Load and prepare the images
  Image2 := CreateImageFromFile(GFScreen, TBMPReader, 'image2.bmp');
  Image2Canvas := TFCanvas(GFScreen.CreateBitmapCanvas(Image2.Width, Image2.Height));
  Image2Canvas.DrawImage(Image2, Point(0, 0));

  Image4 := CreateImageFromFile(GFScreen, TBMPReader, 'image4.bmp');
  Image4Canvas := TFCanvas(GFScreen.CreateBitmapCanvas(Image4.Width, Image4.Height));
  Image4Canvas.DrawImage(Image4, Point(0, 0));

  Image8 := CreateImageFromFile(GFScreen, TBMPReader, 'image8.bmp');
  Image8Canvas := TFCanvas(GFScreen.CreateBitmapCanvas(Image8.Width, Image8.Height));
  Image8Canvas.DrawImage(Image8, Point(0, 0));

  Image24 := CreateImageFromFile(GFScreen, TBMPReader, 'image24.bmp');
  Image24Canvas := TFCanvas(GFScreen.CreateBitmapCanvas(Image24.Width, Image24.Height));
  Image24Canvas.DrawImage(Image24, Point(0, 0));

  // Load and prepare the image mask
  Mask := CreateImageFromFile(GFScreen, TBMPReader, 'mask.bmp');
  MaskCanvas := TFCanvas(GFScreen.CreateMonoBitmapCanvas(Mask.Width, Mask.Height));
//  MaskCanvas := Display.DefaultScreen.CreateMonoBitmap(Mask.Width, Mask.Height);
  MaskCanvas.DrawImage(Mask, Point(0, 0));

  Title := 'fpImg Blitting Mask Test';
  OnPaint := @Paint;
  SetClientSize(Size(Image2.Width * 2 + 64, Image2.Height * 2 + 64));
end;

destructor TMainWindow.Destroy;
begin
  MaskCanvas.Free;
  Mask.Free;
  Image24Canvas.Free;
  Image24.Free;
  Image8Canvas.Free;
  Image8.Free;
  Image4Canvas.Free;
  Image4.Free;
  Image2Canvas.Free;
  Image2.Free;
  inherited Destroy;
end;

procedure TMainWindow.Paint(Sender: TObject; const ARect: TRect);
var
  Color: TGfxColor;
  r: TRect;
  i, x1, y1, x2, y2: Integer;
begin
  Color.Red := 0;
  Color.Green := 0;
  Color.Alpha := 0;
  r.Left := ARect.Left;
  r.Right := ARect.Right;
  for i := ARect.Top to ARect.Bottom - 1 do
  begin
    Color.Blue := $ffff - (i * $ffff) div Height;
    Color.Red := Color.Blue shr 1;
    Canvas.SetColor(Color);
    r.Top := i;
    r.Bottom := i + 1;
    Canvas.FillRect(r);
  end;

  x1 := ClientWidth div 4 - Image2.Width div 2;
  y1 := ClientHeight div 4 - Image2.Height div 2;
  x2 := x1 + ClientWidth div 2;
  y2 := y1 + ClientHeight div 2;

  Canvas.SetColor(colWhite);
  Canvas.MaskedCopy(Image2Canvas, MaskCanvas, Point(x1, y1));
  Canvas.TextOut(Point(x1, y1 + Image2.Height), 'monochrome');
  Canvas.MaskedCopy(Image4Canvas, MaskCanvas, Point(x2, y1));
  Canvas.TextOut(Point(x2, y1 + Image2.Height), '4bpp palettized');
  Canvas.MaskedCopy(Image8Canvas, MaskCanvas, Point(x1, y2));
  Canvas.TextOut(Point(x1, y2 + Image2.Height), '8bpp palettized');
  Canvas.MaskedCopy(Image24Canvas, MaskCanvas, Point(x2, y2));
  Canvas.TextOut(Point(x2, y2 + Image2.Height), '24bpp true color');
end;

var
  MainWindow: TMainWindow;
begin
  GFApplication.Initialize;
  MainWindow := TMainWindow.Create;
  MainWindow.Show;
  GFApplication.Run;
end.

