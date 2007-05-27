{
    fpGUI  -  Free Pascal GUI Library

    Image Test example

    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

program ImgTest;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

uses
  cmem,
  Classes,
  GFXBase,
  fpGFX;

type
  TMainWindow = class(TFWindow)
  private
    Bitmap: TFBitmap;
    procedure   Paint(Sender: TObject; const Rect: TRect);
  public
    constructor Create; overload;
    destructor  Destroy; override;
  end;


constructor TMainWindow.Create;
var
  Data: Pointer;
  Stride: LongWord;
  i, j: Integer;
begin
  inherited Create(nil, [woWindow]);
  Data := nil;
  Stride := 0;
  
  Title := 'fpGFX Bitmap Test';
  OnPaint := @Paint;
  SetClientSize(Size(256, 256));
  SetMinMaxClientSize(Size(256, 256), Size(256, 256));

  Bitmap := TFBitmap.Create(256, 256, PixelFormatRGB32);
  Bitmap.Lock(Data, Stride);
  for j := 0 to 255 do
    for i := 0 to 255 do
      PLongWord(Data)[j * 256 + i] := (i shl 16) or (j shl 8);
  Bitmap.Unlock;
end;

destructor TMainWindow.Destroy;
begin
  Bitmap.Free;
  inherited Destroy;
end;

procedure TMainWindow.Paint(Sender: TObject; const Rect: TRect);
begin
  Canvas.SetColor(colBlue);
  Canvas.FillRect(Rect);
  Canvas.DrawImage(Bitmap, Point(0, 0));
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


