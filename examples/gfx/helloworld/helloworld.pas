{
    fpGUI  -  Free Pascal GUI Library

    HelloWorld  -  GFX Hello World application

    Copyright (C) 2000 - 2006 See the file AUTHORS, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
program HelloWorld;

uses
  Classes,
  Types,
  fpGFX,
  GFXBase
  ;

const
  HelloWorldString: String = 'Hello, world!';

type

  { TMainWindow }

  TMainWindow = class(TFWindow)
  public
    Font: TFCustomFont;
    TextSize: TSize;
    procedure Paint(Sender: TObject; const Rect: TRect);
    constructor Create;
  end;
  
procedure TMainWindow.Paint(Sender: TObject; const Rect: TRect);
var
  Color: TGfxColor;
  r: TRect;
  i: Integer;
begin
  Color.Red := 0;
  Color.Green := 0;
  Color.Alpha := 0;
  r.Left := Rect.Left;
  r.Right := Rect.Right;
  for i := Rect.Top to Rect.Bottom - 1 do
  begin
    Color.Blue := $ffff - (i * $ffff) div ClientHeight;
    Canvas.SetColor(Color);
    r.Top := i;
    r.Bottom := i + 1;
    Canvas.FillRect(r);
  end;

  Canvas.SetColor(colBlack);
  Canvas.SetFont(Font);
  Canvas.TextOut(Point((ClientWidth - TextSize.cx) div 2 + 1,
    (ClientHeight - TextSize.cy) div 2 + 1), HelloWorldString);

  Canvas.SetColor(colWhite);
  Canvas.TextOut(Point((ClientWidth - TextSize.cx) div 2 - 1,
    (ClientHeight - TextSize.cy) div 2 - 1), HelloWorldString);
end;

constructor TMainWindow.Create;
begin
  inherited Create(nil, [woWindow]);

  { Possible font classes:
    fcSerif, fcSansSerif, fcTypewriter, fcDingbats }
  Font := TFFont.Create('-*-'
      + TFFont.GetDefaultFontName(fcSerif)
      + '-*-r-*-*-34-*-*-*-*-*-*-*');

  Title := 'fpGFX Hello World example';
  OnPaint := @Paint;
  Canvas.SetFont(Font);
  TextSize.cx := Canvas.TextWidth(HelloWorldString);
  TextSize.cy := Canvas.FontCellHeight;
  SetClientSize(Size((TextSize.cx * 3) div 2, TextSize.cy * 2));
  SetMinMaxClientSize(TextSize, Size(0, 0));
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

