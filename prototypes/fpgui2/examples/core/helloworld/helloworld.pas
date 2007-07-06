{
    fpGUI  -  Free Pascal GUI Library

    HelloWorld  -  GFX Hello World application

    Copyright (C) 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}



//   Graeme:         I'm still busy porting this!!!!!


program HelloWorld;

{$mode objfpc}{$H+}

uses
  Classes
  ,fpGFX
  ,GFXBase
  ,gfx_widget
  ,gui_form
  ;

const
  HelloWorldString: String = 'Hello, world!';

const
  // predefined colors RRGGBB format
  colWhite = $FFFFFF;
  colBlack = $000000;
  
type

  { TMainWindow }

  TMainWindow = class(TfpgWindow)
  protected
    procedure   HandlePaint; virtual;
  public
    constructor Create(aowner: TComponent); override;
    procedure   Show;
    procedure   MsgPaint(var msg: TfpgMessageRec); message FPGM_PAINT;
    procedure   MsgClose(var msg: TfpgMessageRec); message FPGM_CLOSE;
  end;

procedure TMainWindow.HandlePaint;
begin
end;

constructor TMainWindow.Create(aowner: TComponent);
begin
  inherited Create(aowner);
  FWidth    := 350;
  FHeight   := 200;
  DoSetWindowTitle('fpGFX Hello World');
end;

procedure TMainWindow.Show;
begin
  AllocateWindowHandle;
end;

{
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
}

procedure TMainWindow.MsgPaint(var msg: TfpgMessageRec);
var
  Color: TfpgColor;
  r: TfpgRect;
  i: Integer;
begin
  Canvas.BeginDraw;
  Canvas.Clear(colWhite);
  
  Color     := 0;
  r.Left    := 0;
  r.Width   := FWidth;
  for i := 0 to FHeight-1 do
  begin
    Color := $ffff - (i * $ffff) div FHeight;
    Canvas.SetColor(Color);
    r.Top := i;
    r.Height := i + 1;
    Canvas.DrawRect(r);
  end;
{
  Canvas.SetColor(colBlack);
  Canvas.SetFont(Font);
  Canvas.TextOut(Point((ClientWidth - TextSize.cx) div 2 + 1,
    (ClientHeight - TextSize.cy) div 2 + 1), HelloWorldString);

  Canvas.SetColor(colWhite);
  Canvas.TextOut(Point((ClientWidth - TextSize.cx) div 2 - 1,
    (ClientHeight - TextSize.cy) div 2 - 1), HelloWorldString);
}
  Canvas.EndDraw(0, 0, FWidth, FHeight);
end;

procedure TMainWindow.MsgClose(var msg: TfpgMessageRec);
begin
  ReleaseWindowHandle;
  Halt(0);
end;

(*
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
*)

var
  MainWindow: TMainWindow;
begin
  fpgApplication.Initialize;
  MainWindow := TMainWindow.Create(nil);
  MainWindow.Show;
  fpgApplication.Run;
  MainWindow.Free;
end.

