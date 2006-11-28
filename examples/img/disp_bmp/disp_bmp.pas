{
    fpGUI  -  Free Pascal GUI Library

    Example: Display BMP file using the fpImg - Free Pascal Imaging Library

    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program Disp_BMP;

uses
  Classes
  ,GFXBase
  ,GFXImpl
  ,fpImg
  ,BMPReader
  ,fpgfxpackage
  ;

type
  TMainWindow = class
    procedure Paint(Sender: TObject; const Rect: TRect);
  private
    Display: TDefDisplay;
    Window: TGfxWindow;
    Image: TGfxImage;
  public
    constructor Create(ADisplay: TDefDisplay);
    destructor  Destroy; override;
  end;

constructor TMainWindow.Create(ADisplay: TDefDisplay);
begin
  inherited Create;
  Display         := ADisplay;
  Image           := CreateImageFromFile(Display.DefaultScreen, TBMPReader, ParamStr(1));
  Window          := ADisplay.DefaultScreen.CreateWindow;
  Window.Title    := 'fpImg Bitmap Test';
  Window.OnPaint  := @Paint;
  Window.SetFixedClientSize(Size(Image.Width, Image.Height));
  Window.Show;
end;

destructor TMainWindow.Destroy;
begin
  Image.Free;
  inherited Destroy;
end;

procedure TMainWindow.Paint(Sender: TObject; const Rect: TRect);
begin
  Window.Canvas.SetColor(colRed);
  Window.Canvas.FillRect(Rect);
  Window.Canvas.SetColor(colYellow);
  Window.Canvas.DrawImage(Image, Point(0, 0));
end;

var
  Display: TDefDisplay;
  MainWindow: TMainWindow;
begin
  if ParamCount <> 1 then
  begin
    WriteLn(StdErr, 'Please give the name of a BMP file as argument');
    Halt(2);
  end;

  Display     := TDefDisplay.Create;
  MainWindow  := TMainWindow.Create(Display);
  Display.Run;
  MainWindow.Free;
  Display.Free;
end.

