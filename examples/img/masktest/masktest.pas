{
    $Id: masktest.pp,v 1.3 2001/02/14 23:08:59 sg Exp $

    fpImg  -  Free Pascal Imaging Library
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Example: Display BMP file with monochrome mask

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program MaskTest;

uses Classes, GFXBase, fpGFX, fpImg, BMPReader;

type
  TMainWindow = class
    procedure Paint(Sender: TObject; const ARect: TRect);
  private
    Window: TFWindow;
    Image2, Image4, Image8, Image24, Mask: TFBitmap;
    Image2Canvas, Image4Canvas, Image8Canvas,
      Image24Canvas, MaskCanvas: TFCanvas;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TMainWindow.Create;
begin
  inherited Create;

  // Load and prepare the images
  Image2 := CreateImageFromFile(GFScreen, TBMPReader, 'image2.bmp');
  Image2Canvas := TFBitmap.Create(Image2.Width, Image2.Height);
  Image2Canvas.DrawImage(Image2, Point(0, 0));

  Image4 := CreateImageFromFile(GFScreen, TBMPReader, 'image4.bmp');
  Image4Canvas :=
    GFScreen.CreateBitmap(Image4.Width, Image4.Height);
  Image4Canvas.DrawImage(Image4, Point(0, 0));

  Image8 := CreateImageFromFile(GFScreen, TBMPReader, 'image8.bmp');
  Image8Canvas :=
    Display.DefaultScreen.CreateBitmap(Image8.Width, Image8.Height);
  Image8Canvas.DrawImage(Image8, Point(0, 0));

  Image24 := CreateImageFromFile(GFScreen, TBMPReader, 'image24.bmp');
  Image24Canvas :=
    Display.DefaultScreen.CreateBitmap(Image24.Width, Image24.Height);
  Image24Canvas.DrawImage(Image24, Point(0, 0));

  // Load and prepare the image mask
  Mask := CreateImageFromFile(GFScreen, TBMPReader, 'mask.bmp');
  MaskCanvas := Display.DefaultScreen.CreateMonoBitmap(Mask.Width, Mask.Height);
  MaskCanvas.DrawImage(Mask, Point(0, 0));

  Window := ADisplay.DefaultScreen.CreateWindow;
  Window.Title := 'fpImg Blitting Mask Test';
  Window.OnPaint := @Paint;
  Window.SetClientSize(Size(Image2.Width * 2 + 64, Image2.Height * 2 + 64));
  Window.Show;
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
    Color.Blue := $ffff - (i * $ffff) div Window.Height;
    Color.Red := Color.Blue shr 1;
    Window.Canvas.SetColor(Color);
    r.Top := i;
    r.Bottom := i + 1;
    Window.Canvas.FillRect(r);
  end;

  x1 := Window.ClientWidth div 4 - Image2.Width div 2;
  y1 := Window.ClientHeight div 4 - Image2.Height div 2;
  x2 := x1 + Window.ClientWidth div 2;
  y2 := y1 + Window.ClientHeight div 2;

  Window.Canvas.MaskedCopy(Image2Canvas, MaskCanvas, Point(x1, y1));
  Window.Canvas.TextOut(Point(x1, y1 + Image2.Height), 'monochrome');
  Window.Canvas.MaskedCopy(Image4Canvas, MaskCanvas, Point(x2, y1));
  Window.Canvas.TextOut(Point(x2, y1 + Image2.Height), '4bpp palettized');
  Window.Canvas.MaskedCopy(Image8Canvas, MaskCanvas, Point(x1, y2));
  Window.Canvas.TextOut(Point(x1, y2 + Image2.Height), '8bpp palettized');
  Window.Canvas.MaskedCopy(Image24Canvas, MaskCanvas, Point(x2, y2));
  Window.Canvas.TextOut(Point(x2, y2 + Image2.Height), '24bpp true color');
end;

var
  MainWindow: TMainWindow;
begin
  GFApplication.Initialize;
  MainWindow := TMainWindow.Create;
  GFApplication.AddWindow(MainWindow);
  MainWindow.Show;
  GFApplication.Run;
end.

