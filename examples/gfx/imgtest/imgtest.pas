{
    fpGUI  -  Free Pascal GUI Library

    Image Test example

    Copyright (C) 2000 - 2006 See the file AUTHORS, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

program ImgTest;

uses
  Classes,
  GFXBase,
  fpGFX;

type
  TMainWindow = class(TFWindow)
    procedure Paint(Sender: TObject; const Rect: TRect);
  private
    Image: TFImage;
  public
    constructor Create;
    destructor  Destroy; override;
  end;


constructor TMainWindow.Create;
var
  Data: Pointer;
  Stride: LongWord;
  i, j: Integer;
begin
  inherited Create(nil, [woWindow]);
  
  Title := 'fpGFX Bitmap Test';
  OnPaint := @Paint;
  SetClientSize(Size(256, 256));
  SetMinMaxClientSize(Size(256, 256), Size(256, 256));

  Image := TFImage.Create(256, 256, PixelFormatRGB32);
  Image.Lock(Data, Stride);
  for j := 0 to 255 do
    for i := 0 to 255 do
      PLongWord(Data)[j * 256 + i] := (i shl 16) or (j shl 8);
  Image.Unlock;
end;

destructor TMainWindow.Destroy;
begin
  Image.Free;
  inherited Destroy;
end;

procedure TMainWindow.Paint(Sender: TObject; const Rect: TRect);
var
  r: TRect;
begin
  Canvas.SetColor(colBlue);
  r.Left    := 0;
  r.Top     := 0;
  r.Right   := Width;
  r.Bottom  := Height;
  Canvas.FillRect(r);
  Canvas.DrawImage(Image, Point(0, 0));
end;


var
  MainWindow: TMainWindow;
  
begin
  gApplication.Initialize;
  MainWindow := TMainWindow.Create;
  gApplication.AddWindow(MainWindow);
  MainWindow.Show;
  gApplication.Run;
end.


