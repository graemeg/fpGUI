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
  
  { Disable this to hide Paint event writeln's. Move another Application over
    SubWindow application, to see when Paint event fires for TBoxWindow or
    TMainWindow. }
  {$Define DEBUG}

type

  { TBoxWindow }

  TBoxWindow = class(TFWindow)
  public
    constructor Create(AParent: TFCustomWindow);
    procedure   Paint(Sender: TObject; const Rect: TRect);
    procedure   MouseReleased(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
  end;

  { TMainWindow }

  TMainWindow = class(TFWindow)
  public
    ABox: TBoxWindow;
    constructor Create;
    procedure   Paint(Sender: TObject; const Rect: TRect);
    procedure   MouseReleased(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
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
  tw: integer;
begin
  {$IFDEF DEBUG} Writeln(ClassName + '.Paint'); {$ENDIF}
  Canvas.SetColor(colBlue);
  r.Left    := 0;
  r.Top     := 0;
  r.Right   := Width;
  r.Bottom  := Height;
  Canvas.FillRect(r);
  tw := Canvas.TextWidth('Window 2');
  Canvas.SetColor(colWhite);
  Canvas.TextOut(Point((Width div 2) - (tw div 2), Height - 20), 'Window 2');
end;

constructor TMainWindow.Create;
begin
  inherited Create(nil, [woWindow]);

  Title := 'fpGFX Sub-Window example';
  SetClientSize(Size(256, 256));
  SetMinMaxClientSize(Size(256, 256), Size(256, 256));

  OnMouseReleased := @MouseReleased;
  OnPaint := @Paint;
  
  ABox := TBoxWindow.Create(Self);
  ABox.Show;
end;

procedure TMainWindow.Paint(Sender: TObject; const Rect: TRect);
var
  r: TRect;
  tw: integer;
begin
  {$IFDEF DEBUG} Writeln(ClassName + '.Paint'); {$ENDIF}
  Canvas.SetColor(colLtGray);
  r.Left    := 0;
  r.Top     := 0;
  r.Right   := Width;
  r.Bottom  := Height;
  Canvas.FillRect(r);
  tw := Canvas.TextWidth('Window 1');
  Canvas.SetColor(colBlack);
  Canvas.TextOut(Point((Width div 2) - (tw div 2), Height - 20), 'Window 1');
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

