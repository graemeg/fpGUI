{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      A simple hello world application that only uses canvas painting.
}

program HelloWorld;

{$mode objfpc}{$H+}

uses
  Classes,
  fpg_base,
  fpg_main;

const
  HelloWorldString: String = 'Hello, world!';
  ClickToClose: String = 'click to close';


type

  TMainWindow = class(TfpgWindow)
  private
    procedure   MsgPaint(var msg: TfpgMessageRec); message FPGM_PAINT;
    procedure   MsgClose(var msg: TfpgMessageRec); message FPGM_CLOSE;
    procedure   MsgResize(var msg: TfpgMessageRec); message FPGM_RESIZE;
    procedure   MsgMouseUp(var msg: TfpgMessageRec); message FPGM_MOUSEUP;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   Show;
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
  DoSetWindowVisible(True);
  // We can't set a title if we don't have a window handle. So we do that here
  // and not in the constructor.
  SetWindowTitle('fpGUI Hello World');
end;

procedure TMainWindow.MsgPaint(var msg: TfpgMessageRec);
var
  r: TfpgRect;
  i: Integer;
  fnt: TfpgFont;
begin
  Canvas.BeginDraw;  // begin double buffering

  r.SetRect(0, 0, Width, Height);
  Canvas.GradientFill(r, clBlue, clBlack, gdVertical);

  fnt := fpgGetFont('Arial-20');
  try
    Canvas.Font := fnt;

    Canvas.SetTextColor(clBlack);
    Canvas.DrawString((Width - Canvas.Font.TextWidth(HelloWorldString)) div 2 + 1,
      (Height - Canvas.Font.Height) div 2 + 1, HelloWorldString);

    Canvas.SetTextColor(clWhite);
    Canvas.DrawString((Width - Canvas.Font.TextWidth(HelloWorldString)) div 2 - 1,
      (Height - Canvas.Font.Height) div 2 - 1, HelloWorldString);
  finally
    fnt.Free;
  end;

  fnt := fpgGetFont('Arial-10');
  try
    Canvas.Font := fnt;
    Canvas.DrawString((Width - Canvas.Font.TextWidth(ClickToClose)) div 2 - 1,
      Height - (Canvas.Font.Height*2), ClickToClose);
  finally
    fnt.Free;
  end;

  Canvas.EndDraw;
end;

procedure TMainWindow.MsgClose(var msg: TfpgMessageRec);
begin
  ReleaseWindowHandle;
  fpgApplication.Terminate;
end;

procedure TMainWindow.MsgResize(var msg: TfpgMessageRec);
begin
  FWidth  := msg.Params.rect.Width;
  FHeight := msg.Params.rect.Height;
end;

procedure TMainWindow.MsgMouseUp(var msg: TfpgMessageRec);
begin
  MsgClose(msg);
end;



var
  MainWindow: TMainWindow;
begin
  fpgApplication.Initialize;
  MainWindow := TMainWindow.Create(nil);
  fpgApplication.MainForm := MainWindow;
  MainWindow.Show;
  fpgApplication.Run;
  MainWindow.Free;
end.

