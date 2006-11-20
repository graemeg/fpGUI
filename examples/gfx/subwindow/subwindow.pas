{
    fpGUI  -  Free Pascal GUI Library

    SubWindow  -  Shows how to create a Sub-Window on GFX

    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
program subwindow;

uses
  SysUtils, Classes,
  fpGFX, GFXBase, fpgfxpackage;

type

  { TBoxWindow }

  TBoxWindow = class(TFWindow)
  public
    procedure Paint(Sender: TObject; const Rect: TRect);
    constructor Create(AParent: TFCustomWindow);
    procedure MouseReleased(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
  end;

  { TMainWindow }

  TMainWindow = class(TFWindow)
  public
    ABox: TBoxWindow;
    constructor Create;
    procedure MouseReleased(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
  end;
  
constructor TBoxWindow.Create(AParent: TFCustomWindow);
begin
  inherited Create(AParent, []);

  OnMouseReleased := @MouseReleased;
  OnPaint := @Paint;

  SetClientSize(Size(125, 125));
  SetMinMaxClientSize(Size(125, 125), Size(125, 125));
end;

procedure TBoxWindow.MouseReleased(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  WriteLn('Mouse released on child window');
end;

procedure TBoxWindow.Paint(Sender: TObject; const Rect: TRect);
var
  r: TRect;
begin
  Canvas.SetColor(colBlue);
  r.Left    := 0;
  r.Top     := 0;
  r.Right   := Width;
  r.Bottom  := Height;
  Canvas.FillRect(r);
end;

constructor TMainWindow.Create;
begin
  inherited Create(nil, [woWindow]);

  Title := 'fpGFX Sub-Window example';
  SetClientSize(Size(256, 256));
  SetMinMaxClientSize(Size(256, 256), Size(256, 256));

  OnMouseReleased := @MouseReleased;

  ABox := TBoxWindow.Create(Self);
  ABox.Show;
end;

procedure TMainWindow.MouseReleased(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  WriteLn('Mouse released on main window');
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

