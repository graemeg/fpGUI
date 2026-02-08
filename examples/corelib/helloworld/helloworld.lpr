{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2017 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      A simple hello world application that only uses canvas painting.
      NOTE: This is not how you would normally create an application. This
            is simply to test some core fpGUI functionality.
}

program HelloWorld;

{$mode objfpc}{$H+}

uses
  Classes,
  fpg_base,
  fpg_window,
  fpg_main;

const
  HelloWorldString: String = 'Hello, world!';
  ClickToClose: String = 'click to close';


type

  TMainWindow = class(TfpgWindow)
  private
    FLargeFont: TfpgFontResourceBase;
    FSmallFont: TfpgFontResourceBase;
    procedure   MsgPaint(var msg: TfpgMessageRec); message FPGM_PAINT;
    procedure   MsgClose(var msg: TfpgMessageRec); message FPGM_CLOSE;
    procedure   MsgResize(var msg: TfpgMessageRec); message FPGM_RESIZE;
    procedure   MsgMouseUp(var msg: TfpgMessageRec); message FPGM_MOUSEUP;
  protected
    procedure DoAllocateWindowHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure   Show;
  end;
  

constructor TMainWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth    := 350;
  FHeight   := 200;
  FLargeFont := fpgApplication.FontManager.GetFont(FPG_DEFAULT_SANS + '-20');
  FSmallFont := fpgApplication.FontManager.GetFont(FPG_DEFAULT_SANS + '-10');
end;

destructor TMainWindow.Destroy;
begin
  FLargeFont := nil;
  FSmallFont := nil;
  inherited Destroy;
end;

procedure TMainWindow.Show;
begin
  HandleShow;
  WindowTitle := 'fpGUI Hello World';
end;

procedure TMainWindow.MsgPaint(var msg: TfpgMessageRec);
var
  r: TfpgRect;
begin
  Canvas.BeginDraw;  // begin double buffering

  r.SetRect(0, 0, ActualWidth, ActualHeight);
  Canvas.GradientFill(r, clBlue, clBlack, gdVertical);

  Canvas.SetFont(FLargeFont);
  Canvas.SetTextColor(clBlack);
  Canvas.DrawString((ActualWidth - FLargeFont.GetTextWidth(HelloWorldString)) div 2 + 1,
    (ActualHeight - FLargeFont.GetHeight) div 2 + 1, HelloWorldString);

  Canvas.SetTextColor(clWhite);
  Canvas.DrawString((ActualWidth - FLargeFont.GetTextWidth(HelloWorldString)) div 2 - 1,
    (ActualHeight - FLargeFont.GetHeight) div 2 - 1, HelloWorldString);


  Canvas.SetFont(FSmallFont);
  Canvas.DrawString((ActualWidth - FSmallFont.GetTextWidth(ClickToClose)) div 2 - 1,
    ActualHeight - (FSmallFont.GetHeight*2), ClickToClose);

  Canvas.EndDraw;
end;

procedure TMainWindow.MsgClose(var msg: TfpgMessageRec);
begin
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

procedure TMainWindow.DoAllocateWindowHandle;
var
  WindowAttributes: TWindowAttributes;
begin
  inherited DoAllocateWindowHandle;
  WindowAttributes := Window.WindowAttributes;
  Include(WindowAttributes, waOneThirdDownPos);
  Window.WindowAttributes := WindowAttributes;
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

