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
  private
    procedure   MsgPaint(var msg: TfpgMessageRec); message FPGM_PAINT;
    procedure   MsgClose(var msg: TfpgMessageRec); message FPGM_CLOSE;
    procedure   MsgResize(var msg: TfpgMessageRec); message FPGM_RESIZE;
  protected
    procedure   HandlePaint; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   Show;
  end;
  

procedure TMainWindow.HandlePaint;
begin
end;

constructor TMainWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth    := 350;
  FHeight   := 200;
  WindowAttributes := [waSizeable, waScreenCenterPos];
end;

procedure TMainWindow.Show;
begin
  AllocateWindowHandle;
  // We can't set a title if we don't have a window handle. So we do that here
  // and not in the constructor.
  DoSetWindowTitle('fpGFX Hello World');
end;

procedure TMainWindow.MsgPaint(var msg: TfpgMessageRec);
var
  Color: TfpgColor;
  r: TfpgRect;
  i: Integer;
begin
  Canvas.BeginDraw;

  Color     := 0;
  r.Left    := 0;
  r.Width   := FWidth;
  for i := 0 to FHeight-1 do
  begin
    Color := $ff - (i * $ff) div FHeight;
    Canvas.SetColor(Color);
    r.Top := i;
    r.Height := i + 1;
    Canvas.DrawRectangle(r);
  end;

  Canvas.Font := fpgGetFont('Arial-30');

  Canvas.SetTextColor(colBlack);
  Canvas.DrawString((Width - Canvas.Font.TextWidth(HelloWorldString)) div 2 + 1,
    (Height - Canvas.Font.Height) div 2 + 1, HelloWorldString);

  Canvas.SetTextColor(colWhite);
  Canvas.DrawString((Width - Canvas.Font.TextWidth(HelloWorldString)) div 2 - 1,
    (Height - Canvas.Font.Height) div 2 - 1, HelloWorldString);

  Canvas.EndDraw(0, 0, FWidth, FHeight);
end;

procedure TMainWindow.MsgClose(var msg: TfpgMessageRec);
begin
  ReleaseWindowHandle;
  Halt(0);
end;

procedure TMainWindow.MsgResize(var msg: TfpgMessageRec);
begin
  FWidth := msg.Params.rect.Width;
  FHeight := msg.Params.rect.Height;
end;



var
  MainWindow: TMainWindow;
begin
  fpgApplication.Initialize;
  MainWindow := TMainWindow.Create(nil);
  MainWindow.Show;
  fpgApplication.Run;
  MainWindow.Free;
end.

