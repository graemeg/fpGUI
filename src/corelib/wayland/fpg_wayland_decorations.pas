{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2020 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit implements Wayland window decorations for fpGUI.
}
unit fpg_wayland_decorations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_button, fpg_label, fpg_widget, fpg_panel, fpg_window, fpg_base, fpg_wayland,fpg_agg2d_canvas_wayland, fpg_wayland_classes;

type

  { TfpgWaylandDecorator }

  TfpgWaylandDecorator = class
  protected
    class var FBorderWidthIncrease: Integer;
    class var FBorderHeightIncrease: Integer;
  public
    class property BorderWidthIncrease: Integer read FBorderWidthIncrease;
    class property BorderHeightIncrease: Integer read FBorderHeightIncrease;
    class constructor Create;
  private
    FBorderLeft: Integer;
    FBorderTop: Integer;
    FSavedWidth,
    FSavedHeight: Integer;
    FCanvas: TAgg2dWaylandBufferCanvas;
    FDecorator: TfpgwWindowDecorator;
    procedure CloseClicked(Sender: TObject);
    procedure HandleLMouseDown(x, y: integer; shiftstate: TShiftState); {override;}
    procedure HandlePaint; {override;}
    procedure HandleResize(AWidth, AHeight: TfpgCoord); {override;}
    procedure MinimizeClicked(Sender: TObject);
    procedure SetWinTitle(AValue: String);
    procedure ToggleMaximizedClicked(Sender: TObject);
  protected
    FWindow: TfpgWaylandWindow;
    FTitle: String;
    FFont: TfpgFontBase;
    FCloseButton,
    FToggleButton,
    FMinimizeButton: TfpgButton;
    FContent: TfpgPanel;
    procedure MoveParentsChildren;
  public
    constructor Create(AWindow: TfpgWaylandWindow; AWin: TfpgwWindow);
    destructor Destroy; override;
    procedure Draw(ABuffer: TfpgwBuffer);
    property Title: String read FTitle write SetWinTitle;
    property BorderTop: Integer read FBorderTop;
    property BorderLeft: Integer read FBorderLeft;
  end;

implementation
uses
  fpg_main, fpg_fontcache;

{ TfpgWaylandDecorator }

class constructor TfpgWaylandDecorator.Create;
begin
  FBorderWidthIncrease:=10;
  FBorderHeightIncrease:=30;
end;

procedure TfpgWaylandDecorator.HandleLMouseDown(x, y: integer;
  shiftstate: TShiftState);
begin
  {inherited HandleLMouseDown(x, y, shiftstate);
  Window.WinHandle.SurfaceShell.Move(WApplication.Display.EventSerial);}
end;

procedure TfpgWaylandDecorator.HandlePaint;
var
  lWidth: Integer;
begin
  {inherited HandlePaint;
  Canvas.Font := FFont;
  Canvas.Color:=clDarkGray;
  Canvas.FillRectangle(0,0,Width, 20);
  Canvas.FillRectangle(0,20,5, Height);
  Canvas.FillRectangle(Width-5,20,5, Height);
  Canvas.FillRectangle(0,Height-5,Width, 5);
  Canvas.TextColor:=clwhite;
  lWidth := Canvas.Font.TextWidth(FTitle);
  Canvas.DrawString((Width div 2 - lWidth div 2), 1, FTitle);}
end;

procedure TfpgWaylandDecorator.HandleResize(AWidth, AHeight: TfpgCoord);
begin
{  inherited HandleResize(AWidth, AHeight);
  if Window.HasHandle and Assigned(FContent) then
    Window.WinHandle.SurfaceShell.SetOpaqueRegion(TRect(FContent.GetBoundsRect));}
end;

procedure TfpgWaylandDecorator.MinimizeClicked(Sender: TObject);
begin
  //Window.WinHandle.SurfaceShell.SetMinimized;
end;

procedure TfpgWaylandDecorator.SetWinTitle(AValue: String);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
  //InvalidateRect(fpgrect(0,0,Width,20));
end;

procedure TfpgWaylandDecorator.ToggleMaximizedClicked(Sender: TObject);
var
  lIsMaximized: Boolean;
  msgp: TfpgMessageParams;
begin
  {lIsMaximized := Window.WinHandle.SurfaceShell.IsMaximized;

  // toggle maximized
  Window.WinHandle.SurfaceShell.SetMaximized(not lIsMaximized);

  // now save or restore our old size.
  if not lIsMaximized then
  begin
    FSavedHeight:=Height;
    FSavedWidth:=Width;
  end
  else
  begin
    Window.WinHandle.SetSize(FSavedWidth, FSavedHeight);
    Window.WinHandle.Display.Dispatch;
  end;}
end;

procedure TfpgWaylandDecorator.CloseClicked(Sender: TObject);
begin
  //fpgPostMessage(nil, Window, FPGM_CLOSE);
end;

procedure TfpgWaylandDecorator.MoveParentsChildren;
var
  i: Integer;
  c: TComponent;
begin
{  for i := 0 to Owner.ComponentCount-1 do
  begin
    if (Owner.Components[0] <> self) and (Owner.Components[0].InheritsFrom(TfpgWidgetBase)) then
    begin
      c := Owner.Components[0];
      TfpgWidget(c).Parent := FContent;
      TfpgWidget(c).UpdatePosition;
      TfpgWidget(c).Realign;
      //Owner.RemoveComponent(c);
      //FContent.InsertComponent(c);
    end;
  end;}
end;

constructor TfpgWaylandDecorator.Create(AWindow: TfpgWaylandWindow; AWin: TfpgwWindow);
var
  lParentForm: TfpgWindow;
begin
  FWindow := AWindow;
  FDecorator := TfpgwWindowDecorator.Create(Self, WApplication.Display, 5,5,25,5);
  FDecorator.Host := AWin;

  FBorderTop := 25;
  FBorderLeft:= 5;
  //inherited Create(AOwner);
  //Align := alClient;
  FTitle := ExtractFileName(ParamStr(0));
  FCanvas := TAgg2dWaylandBufferCanvas.Create(AWin);
  //FIsContainer:=True;
{  lParentForm := AOwner as TfpgWindow;}

  {FCloseButton := TfpgButton.Create(Self);
  FCloseButton.Anchors:=[anRight, anTop];
  FCloseButton.SetPosition(Width-20, 5, 15, 15);
  FCloseButton.OnClick:=@CloseClicked;
  FCloseButton.Text:='X';

  FToggleButton := TfpgButton.Create(Self);
  FToggleButton.Anchors:=[anRight, anTop];
  FToggleButton.SetPosition(Width-35, 5, 15, 15);
  FToggleButton.OnClick:=@ToggleMaximizedClicked;
  FToggleButton.Text:='o';

  FMinimizeButton := TfpgButton.Create(Self);
  FMinimizeButton.Anchors:=[anRight, anTop];
  FMinimizeButton.SetPosition(Width-50, 5, 15, 15);
  FMinimizeButton.OnClick:=@MinimizeClicked;
  FMinimizeButton.Text:='_';

  FContent := TfpgPanel.Create(Self);
  FContent.Style:=bsFlat;
  FContent.Anchors:=AllAnchors;
  FContent.SetPosition(5,20,Width-10, Height-25);
  FContent.UpdatePosition;
  FContent.Realign;
  //Fcontent.Visible:=False;
  FFont := fpgGetFont(FPG_DEFAULT_SANS+'-12:bold');
  Canvas.Color:=clBlue;


  PArent := TfpgWidget(AOwner);

  MoveParentsChildren;}
end;

destructor TfpgWaylandDecorator.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TfpgWaylandDecorator.Draw(ABuffer: TfpgwBuffer);
begin
  FCanvas.SetBuffer(ABuffer);
  FCanvas.BeginDraw(nil, 0, 0);
  FCanvas.Color:=clRed;
  FCanvas.FillRectangle(0,0, ABuffer.Width, BorderTop);
  FCanvas.Color := clWhite;
  FCanvas.DrawString(0,0, FTitle);
  FCanvas.EndDraw(0,0, ABuffer.Width, ABuffer.Height);
end;

end.

