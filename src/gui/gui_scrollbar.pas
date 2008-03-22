{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2008 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines Scrollbar controls.
}

unit gui_scrollbar;

{$mode objfpc}{$H+}

{
  TODO:
    * Set slider button to minimum length (default setting)
    * Create property to enable dynamic sizing of slider button length.
    * Paint scroll area between arrow buttons and slider button a different
      color on click.
}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_widget;

type
  TScrollNotifyEvent = procedure(Sender: TObject; position: integer) of object;

  TfpgScrollStyle = (ssNone, ssHorizontal, ssVertical, ssAutoBoth);
  
  TfpgScrollBarPart = (sbpNone, sbpUpBack, sbpPageUpBack, sbpSlider, sbpDownForward, sbpPageDownForward);

  TfpgScrollBar = class(TfpgWidget)
  private
    FLargeChange: Integer;
    FScrollbarDownPart: TfpgScrollBarPart;
    procedure   SetMax(const AValue: integer);
    procedure   SetMin(const AValue: integer);
    procedure   SetPosition(const AValue: integer);
  protected
    FMax: integer;
    FMin: integer;
    FPosition: integer;
    FScrollStep: integer;
    FSliderPos: TfpgCoord;
    FSliderLength: TfpgCoord;
    FSliderDragPos: TfpgCoord;
    FSliderDragStart: TfpgCoord;
    FScrollTimer: TfpgTimer;
    FActiveButtonRect: TfpgRect;
    FMousePosition: TPoint;
    FOnScroll: TScrollNotifyEvent;
    procedure   ScrollTimer(Sender: TObject);
    procedure   DrawButton(x, y, w, h: TfpgCoord; const imgname: string; Pressed: Boolean = False); virtual;
    procedure   DrawSlider(recalc: boolean); virtual;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandlePaint; override;
    procedure   PositionChange(d: integer);
  public
    Orientation: TOrientation;
    SliderSize: double;  // 0-1
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   RepaintSlider;
    property    Position: integer read FPosition write SetPosition default 10;
    property    ScrollStep: integer read FScrollStep write FScrollStep default 1;
//    property    LargeChange: Integer read FLargeChange write FLargeChange default 0;
    property    Min: integer read FMin write SetMin default 0;
    property    Max: integer read FMax write SetMax default 100;
    property    OnScroll: TScrollNotifyEvent read FOnScroll write FOnScroll;
  end;


implementation

{ TfpgScrollBar }

constructor TfpgScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScrollTimer := TfpgTimer.Create(500);
  FScrollTimer.Enabled := False;
  FScrollTimer.OnTimer := @ScrollTimer;
  Orientation   := orVertical;
  FMin          := 0;
  FMax          := 100;
  FPosition     := 10;
  SliderSize    := 0.5;
  FOnScroll     := nil;
  FSliderPos    := 0;
  FSliderLength := 10;
  FScrollStep   := 1;
  FLargeChange  := 0;
end;

destructor TfpgScrollBar.Destroy;
begin
  FScrollTimer.Free;
  inherited Destroy;
end;

procedure TfpgScrollBar.HandlePaint;
begin
  // Do NOT localize
  Canvas.BeginDraw;
  
  if Orientation = orVertical then
  begin
    DrawButton(0, 0, Width, Width, 'sys.sb.up', FScrollbarDownPart = sbpPageUpBack);
    DrawButton(0, Height-Width, Width, Width, 'sys.sb.down', FScrollbarDownPart = sbpPageDownForward);
  end
  else
  begin
    DrawButton(0, 0, Height, Height, 'sys.sb.left', FScrollbarDownPart = sbpPageUpBack);
    DrawButton(Width-Height, 0, Height, Height, 'sys.sb.right', FScrollbarDownPart = sbpPageDownForward);
  end;

  DrawSlider(True);
  Canvas.EndDraw;
end;

procedure TfpgScrollBar.RepaintSlider;
begin
  if not HasHandle then
    Exit; //==>
  DrawSlider(True);
end;

procedure TfpgScrollBar.SetMax(const AValue: integer);
begin
  if AValue = FMax then
    Exit;
  if AValue < FMin then
    FMax := FMin
  else
    FMax := AValue;
  if FPosition > FMax then
    SetPosition(FMax);
end;

procedure TfpgScrollBar.SetMin(const AValue: integer);
begin
  if AValue = FMin then
    Exit;
  if AValue > FMax then
    FMin := FMax
  else
    FMin := AValue;
  if FPosition < FMin then
    SetPosition(FMin);
end;

procedure TfpgScrollBar.SetPosition(const AValue: integer);
begin
  if AValue < FMin then
    FPosition := FMin
  else if AValue > FMax then
    FPosition := FMax
  else
    FPosition := AValue;

  if HasHandle then
    DrawSlider(False);
end;

procedure TfpgScrollBar.ScrollTimer(Sender: TObject);
   function WithinActiveButton: Boolean;
   begin
     Result := (FMousePosition.X < FActiveButtonRect.Right)
           and (FMousePosition.X > FActiveButtonRect.Left)
           and (FMousePosition.Y < FActiveButtonRect.Bottom)
           and (FMousePosition.Y > FActiveButtonRect.Top);
   end;
   function WithinPageArea(IsBefore: Boolean): Boolean;
   begin
     case Orientation of
       orVertical:
         if IsBefore then
           Result := (FMousePosition.X > -1)
                 and (FMousePosition.X < Width)
                 and (FMousePosition.Y < FSliderPos + Width)
                 and (FMousePosition.Y > Width)
         else
           Result := (FMousePosition.X > -1)
                 and (FMousePosition.X < Width)
                 and (FMousePosition.Y < Height - Width)
                 and (FMousePosition.Y > Width + FSliderPos + FSliderLength);
       orHorizontal:
         if IsBefore then
           Result := (FMousePosition.X > Height)
                 and (FMousePosition.X < FSliderPos + Height)
                 and (FMousePosition.Y < Height)
                 and (FMousePosition.Y > -1)
         else
           Result := (FMousePosition.X > Height + FSliderPos + FSliderLength)
                 and (FMousePosition.X < Width - Height)
                 and (FMousePosition.Y < Height)
                 and (FMousePosition.Y > -1);
     end;
   end;
begin

  FScrollTimer.Interval := 25;
  case FScrollbarDownPart of
    sbpUpBack:
      begin
        if WithinActiveButton then
          PositionChange(-FScrollStep);
        if Position = FMin then
          FScrollTimer.Enabled := False;
      end;
    sbpDownForward:
      begin
        if WithinActiveButton then
          PositionChange(FScrollStep);
        if Position = FMax then
          FScrollTimer.Enabled := False;
      end;
    sbpPageUpBack:
      begin
        if (Position = FMin) or not WithinPageArea(True) then
          FScrollTimer.Enabled := False
        else
          PositionChange(-(FScrollStep * 5));
      end;
    sbpPageDownForward:
      begin
        if (Position = FMax) or not WithinPageArea(False) then
          FScrollTimer.Enabled := False
        else
          PositionChange(FScrollStep * 5);
      end;
  else
    FScrollTimer.Enabled := False;
  end;
end;

procedure TfpgScrollBar.DrawButton(x, y, w, h: TfpgCoord; const imgname: string; Pressed: Boolean = False);
var
  img: TfpgImage;
  dx: integer;
  dy: integer;
begin
  if Pressed then
  begin
    Canvas.DrawButtonFace(x, y, w, h, [btnIsEmbedded, btnIsPressed]);
    dx := 1;
    dy := 1;
  end
  else
  begin
    Canvas.DrawButtonFace(x, y, w, h, [btnIsEmbedded]);
    dx := 0;
    dy := 0;
  end;
  Canvas.SetColor(clText1);
  img := fpgImages.GetImage(imgname);
  if img <> nil then
    Canvas.DrawImage(x + w div 2 - (img.Width div 2) + dx, y + h div 2 - (img.Height div 2) + dy, img);
end;

procedure TfpgScrollBar.DrawSlider(recalc: boolean);
var
  area: TfpgCoord;
  mm: TfpgCoord;
begin
  Canvas.BeginDraw;

  if SliderSize > 1 then
    SliderSize := 1;

  Canvas.SetColor(clScrollBar);

  if Orientation = orVertical then
  begin
    Canvas.FillRectangle(0, Width, Width, Height-Width-Width);
    area := Height - (Width shl 1);
  end
  else
  begin
    Canvas.FillRectangle(Height, 0, Width-Height-Height, Height);
    area := Width - (Height shl 1);
  end;

  if recalc then
  begin
    if FPosition > FMax then
      FPosition := FMax;
    if FPosition < FMin then
      FPosition := FMin;

    FSliderLength := Trunc(area * SliderSize);
    //FSliderLength := Trunc((width/area) * (fmax /area  ));
    if FSliderLength < 20 then
      FSliderLength := 20;
    if FSliderLength > area then
      FSliderLength := area;
    area := area - FSliderLength;
    mm   := FMax - FMin;
    if mm = 0 then
      FSliderPos := 0
    else
      FSliderPos := Trunc(area * ((FPosition - FMin) / mm));
  end;

  // Paint the area between the buttons and the Slider
  if Orientation = orVertical then
  begin
    if FScrollbarDownPart in [sbpUpBack, sbpPageUpBack] then
    begin
      Canvas.SetColor(clShadow1);
      Canvas.FillRectangle(0, Width, Width, FSliderPos);
      Canvas.SetColor(clScrollBar);
    end
    else if FScrollbarDownPart in [sbpDownForward, sbpPageDownForward] then
    begin
      Canvas.SetColor(clShadow1);
      Canvas.FillRectangle(0, FSliderPos + FSliderLength, Width, Height - Width - (FSliderPos + FSliderLength));
      Canvas.SetColor(clScrollBar);
    end;
  end
  else
  begin
    if FScrollbarDownPart in [sbpUpBack, sbpPageUpBack] then
    begin
      Canvas.SetColor(clShadow1);
      Canvas.FillRectangle(Height, 0, FSliderPos, Height);
      Canvas.SetColor(clScrollBar);
    end
    else if FScrollbarDownPart in [sbpDownForward, sbpPageDownForward] then
    begin
      Canvas.SetColor(clShadow1);
      Canvas.FillRectangle(FSliderPos + FSliderLength, 0, Width - Height - (FSliderPos + FSliderLength), Height);
      Canvas.SetColor(clScrollBar);
    end;
  end;

  // Paint the slider button
  if Orientation = orVertical then
  begin
    Canvas.DrawButtonFace(0, Width + FSliderPos, Width, FSliderLength, [btnIsEmbedded]);
    Canvas.EndDraw(0, Width, Width, Height - Width - Width);
  end
  else
  begin
    Canvas.DrawButtonFace(Height + FSliderPos, 0, FSliderLength, Height, [btnIsEmbedded]);
    Canvas.EndDraw(Height, 0, Width - Height - Height, Height);
  end;
end;

procedure TfpgScrollBar.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  lPos: TfpgCoord;
begin
  inherited;

  if Orientation = orVertical then
  begin
    if y <= Width then
    begin
      // Up button has been pressed
      PositionChange(-FScrollStep);
      FScrollbarDownPart := sbpUpBack;
      FActiveButtonRect.SetRect(0, 0, Width, Width);
    end
    else if y >= Height - Width then
    begin
      // Down button has been pressed
      PositionChange(FScrollStep);
      FScrollbarDownPart := sbpDownForward;
      FActiveButtonRect.SetRect(0,Height-Width, Width, Height);
    end
    else if (y >= (Width + FSliderPos)) and (y <= Width + FSliderPos + FSliderLength) then
    begin
      FScrollbarDownPart := sbpSlider;
      FSliderDragPos  := y;
    end
    else if (y > Width) and (y < (Width + FSliderPos)) then
    begin
      // Clicked between Up button and Slider
      FScrollbarDownPart := sbpPageUpBack;
      PositionChange(-(FScrollStep * 5))
    end
    else if (y < (Height - Width)) and (y > (Width + FSliderPos + FSliderLength)) then
    begin
      // Clicked between Down button and Slider
      FScrollbarDownPart := sbpPageDownForward;
      PositionChange(FScrollStep * 5);
    end;
  end
  else
  begin
    if x <= Height then
    begin
      // Left button has been pressed
      PositionChange(-FScrollStep);
      FScrollbarDownPart := sbpUpBack;
      FActiveButtonRect.SetRect(0, 0, Height, Height);
    end
    else if x >= Width - Height then
    begin
      // Right button has been pressed
      PositionChange(FScrollStep);
      FScrollbarDownPart := sbpDownForward;
      FActiveButtonRect.SetRect(Width-Height, 0, Width, Height);
    end
    else if (x >= (Height + FSliderPos)) and (x <= Height + FSliderPos + FSliderLength) then
    begin
      FScrollbarDownPart := sbpSlider;
      FSliderDragPos  := x;
    end
    else if (x > Height) and (x < (Height + FSliderPos)) then
    begin
      // Clicked between Left button and Slider
      FScrollbarDownPart := sbpPageUpBack;
      PositionChange(-(FScrollStep * 5));
    end
    else if (x < (Width - Height)) and (x > (Height + FSliderPos + FSliderLength)) then
    begin
      // Clicked between the Right button and Slider
      FScrollbarDownPart := sbpPageDownForward;
      PositionChange(FScrollStep * 5);
    end;
  end;
  
  if FScrollbarDownPart = sbpSlider then
  begin
    FSliderDragStart := FSliderPos;
    DrawSlider(False);
  end
  else if FScrollbarDownPart <> sbpNone then
  begin
    FScrollTimer.Interval := 500;
    FScrollTimer.Enabled := True;

    HandlePaint;
  end;
end;

procedure TfpgScrollBar.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  WasPressed: Boolean;
begin
  inherited;
  WasPressed := FScrollbarDownPart <> sbpNone;
  FScrollTimer.Enabled := False;

  FScrollbarDownPart := sbpNone;
  
  if WasPressed then
    HandlePaint;
end;

procedure TfpgScrollBar.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  d: integer;
  area: integer;
  newp: integer;
  ppos: integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  FMousePosition.X := x;
  FMousePosition.Y := y;
  
  if (FScrollbarDownPart <> sbpSlider) or ((btnstate and MOUSE_LEFT) = 0) then
    Exit;

  if Orientation = orVertical then
  begin
    d    := y - FSliderDragPos;
    area := Height - (Width shl 1) - FSliderLength;
  end
  else
  begin
    d    := x - FSliderDragPos;
    area := Width - (Height shl 1) - FSliderLength;
  end;

  ppos       := FSliderPos;
  FSliderPos := FSliderDragStart + d;

  if FSliderPos < 0 then
    FSliderPos := 0;
  if FSliderPos > area then
    FSliderPos := area;

  if ppos <> FSliderPos then
    DrawSlider(False);

  if area <> 0 then
    newp := FMin + Trunc((FMax - FMin)  * (FSliderPos / area))
  else
    newp := FMin;

  if newp <> FPosition then
  begin
    Position := newp;
    if Assigned(FOnScroll) then
      FOnScroll(self, FPosition);
  end;
end;

procedure TfpgScrollBar.HandleMouseScroll(x, y: integer; shiftstate: TShiftState;
  delta: smallint);
begin
  inherited HandleMouseScroll(x, y, shiftstate, delta);
  if delta < 0 then
    PositionChange(-FScrollStep);
  if delta > 0 then
    PositionChange( FScrollStep);
end;

procedure TfpgScrollBar.PositionChange(d: integer);
begin
  FPosition := FPosition + d;
  if FPosition < FMin then
    FPosition := FMin;
  if FPosition > FMax then
    FPosition := FMax;

  if Visible then
    DrawSlider(True);

  if Assigned(FOnScroll) then
    FOnScroll(self, FPosition);
end;

end.

