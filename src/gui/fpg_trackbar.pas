{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2012 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines two types of TrackBar controls. Also known as Slider controls.
}

unit fpg_trackbar;

{$mode objfpc}{$H+}

{
  TODO:
   - TfpgTrackBarExtra
    * Tick line orientation (top, bottom, left or right)
    * Slide the slider with the mouse button down (like a scrollbar)
    * Slider button style (rectangle, pointer, double pointer)
    * Tick captions

   - TfpgTrackBar
    * Vertical orientation
    * show ticks property
}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget;
  
type
  TTrackBarChange = procedure(Sender: TObject; APosition: integer) of object;


  TfpgBaseTrackBar = class(TfpgWidget)
  private
    FPosition: integer;
    FMax: integer;
    FMin: integer;
    FOrientation: TOrientation;
    FOnChange: TTrackBarChange;
    procedure   DoChange;
    procedure   SetTBPosition(AValue: integer);
    procedure   SetMax(const AValue: integer);
    procedure   SetMin(const AValue: integer);
  protected
    property    Max: integer read FMax write SetMax default 100;
    property    Min: integer read FMin write SetMin default 0;
    property    Orientation: TOrientation read FOrientation write FOrientation default orHorizontal;
    property    Position: integer read FPosition write SetTBPosition default 0;
    property    OnChange: TTrackBarChange read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TfpgTrackBarExtra = class(TfpgBaseTrackBar)
  private
    FSliderSize: integer;
    procedure   SetSliderSize(const AValue: integer);
    procedure   FixMinMaxOrder;
    procedure   FixPositionLimits;
    procedure   DrawSlider(p: integer);
  protected
    procedure   HandlePaint; override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Align;
    property    BackgroundColor;
    property    Hint;
    property    Max default 10;
    property    Min;
    property    Orientation;
    property    Position;
    property    ShowHint;
    property    SliderSize: integer read FSliderSize write SetSliderSize default 11;
    property    TabOrder;
    property    OnChange;
    property    OnShowHint;
  end;
  
  
  TfpgTrackBar = class(TfpgBaseTrackBar)
  private
    FScrollStep: integer;
    FShowPosition: boolean;
    FSliderLength: TfpgCoord;
    FSliderDragging: boolean;
    FSliderDragPos: TfpgCoord;
    FSliderDragStart: TfpgCoord;
    FMousePosition: TPoint;
    procedure   SetShowPosition(const AValue: boolean);
    function    GetTextWidth: TfpgCoord;
    procedure   SetSliderLength(AValue: integer);
  protected
    FFont: TfpgFont;
    FSliderPos: TfpgCoord;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandlePaint; override;
    procedure   DrawSlider(recalc: boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property    Align;
    property    BackgroundColor;
    property    Enabled;
    property    Hint;
    property    Max;
    property    Min;
    property    Orientation;
    property    ParentShowHint;
    property    Position;
    property    ScrollStep: integer read FScrollStep write FScrollStep default 1;
    property    ShowHint;
    property    ShowPosition: boolean read FShowPosition write SetShowPosition default False;
    property    SliderLength: integer read FSliderLength write SetSliderLength default 11;
    property    TabOrder;
    property    TextColor;
    property    OnChange;
    property    OnEnter;
    property    OnExit;
    property    OnShowHint;
  end;
  

implementation

{ TfpgBaseTrackBar }

procedure TfpgBaseTrackBar.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self, FPosition);
end;

procedure TfpgBaseTrackBar.SetTBPosition(AValue: integer);
begin
  if FPosition = AValue then
    Exit; //==>

  if AValue < FMin then
    FPosition := FMin
  else if AValue > FMax then
    FPosition := FMax
  else
    FPosition := AValue;

  RePaint;
  DoChange;
end;

procedure TfpgBaseTrackBar.SetMax(const AValue: integer);
begin
  if AValue = FMax then
    Exit;
  if AValue < FMin then
    FMax := FMin
  else
    FMax := AValue;
  if FPosition > FMax then
    SetTBPosition(FMax);
  Repaint;
end;

procedure TfpgBaseTrackBar.SetMin(const AValue: integer);
begin
  if AValue = FMin then
    Exit;
  if AValue > FMax then
    FMin := FMax
  else
    FMin := AValue;
  if FPosition < FMin then
    SetTBPosition(FMin);
  Repaint;
end;

constructor TfpgBaseTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFocusable := True;
  FMin := 0;
  FMax := 100;
  FPosition := 0;
  FOrientation := orHorizontal;
  FTextColor := Parent.TextColor;
  FBackgroundColor := Parent.BackgroundColor;
  FOnChange := nil;
end;

{ TfpgTrackBarExtra }

procedure TfpgTrackBarExtra.SetSliderSize(const AValue: integer);
begin
  if FSliderSize = AValue then
    Exit; //==>
  if AValue > 11 then
  begin
    FSliderSize := AValue;
    RePaint;
  end;
end;

procedure TfpgTrackBarExtra.FixMinMaxOrder;
var
  lmin: integer;
  lmax: integer;
begin
  if FMax < FMin then
  begin
    lmin := FMax; // change order
    lmax := FMin;
    FMax := lmax; // reassign values
    FMin := lmin;
  end;
end;

procedure TfpgTrackBarExtra.FixPositionLimits;
begin
  if FPosition < FMin then
    FPosition := FMin;
  if FPosition > FMax then
    FPosition := FMax;
end;

procedure TfpgTrackBarExtra.DrawSlider(p: integer);
var
  h: integer;
begin
  if Orientation = orHorizontal then
  begin
    h := Height div 2 - 1;
    Canvas.SetColor(clHilite1);
    Canvas.DrawLine(p - FSliderSize div 2,5, p + FSliderSize div 2, 5);
    Canvas.DrawLine(p - FSliderSize div 2,5, p - FSliderSize div 2, h - FSliderSize div 2);
    Canvas.DrawLine(p - FSliderSize div 2, h - FSliderSize div 2, p, h + FSliderSize div 2);
    Canvas.SetColor(clHilite2);
    Canvas.DrawLine(p - FSliderSize div 2 + 1,6, p + FSliderSize div 2 - 1, 6);
    Canvas.DrawLine(p - FSliderSize div 2 + 1,6, p - FSliderSize div 2 + 1, h - FSliderSize div 2);
    Canvas.DrawLine(p - FSliderSize div 2 + 1, h - FSliderSize div 2, p, h + FSliderSize div 2 - 1);
    Canvas.SetColor(clShadow2);
    Canvas.DrawLine(p + FSliderSize div 2, 6, p + FSliderSize div 2, h - FSliderSize div 2);
    Canvas.DrawLine(p + FSliderSize div 2, h - FSliderSize div 2, p + 1, h + FSliderSize div 2 - 1);
    Canvas.SetColor(clShadow1);
    Canvas.DrawLine(p + FSliderSize div 2 - 1, 7, p + FSliderSize div 2 - 1, h - FSliderSize div 2);
    Canvas.DrawLine(p + FSliderSize div 2 - 1, h - FSliderSize div 2, p + 1, h + FSliderSize div 2 - 2);
  end
  else
  begin
    h := Width div 2 - 1;
    Canvas.SetColor(clHilite1);
    Canvas.DrawLine(5,p - FSliderSize div 2, 5, p + FSliderSize div 2);
    Canvas.DrawLine(5,p - FSliderSize div 2, h - FSliderSize div 2, p - FSliderSize div 2);
    Canvas.DrawLine( h - FSliderSize div 2, p - FSliderSize div 2, h + FSliderSize div 2,p);
    Canvas.SetColor(clHilite2);
    Canvas.DrawLine(6,p - FSliderSize div 2 + 1, 6, p + FSliderSize div 2 - 1);
    Canvas.DrawLine(6,p - FSliderSize div 2 + 1, h - FSliderSize div 2, p - FSliderSize div 2 + 1);
    Canvas.DrawLine(h - FSliderSize div 2,p - FSliderSize div 2 + 1, h + FSliderSize div 2 - 1,p);
    Canvas.SetColor(clShadow2);
    Canvas.DrawLine( 6,p + FSliderSize div 2, h - FSliderSize div 2, p + FSliderSize div 2);
    Canvas.DrawLine( h - FSliderSize div 2,p + FSliderSize div 2, h + FSliderSize div 2 - 1, p + 1);
    Canvas.SetColor(clShadow1);
    Canvas.DrawLine( 7, p + FSliderSize div 2 - 1, h - FSliderSize div 2,p + FSliderSize div 2 - 1);
    Canvas.DrawLine( h - FSliderSize div 2, p + FSliderSize div 2 - 1, h + FSliderSize div 2 - 2, p + 1);
  end;
end;

procedure TfpgTrackBarExtra.HandlePaint;
var
  r: TfpgRect;
  linepos: double;
  drawwidth: integer;
  i: integer;
begin
//  inherited HandlePaint;
  r.SetRect(0, 0, Width, Height);
  Canvas.Clear(FBackgroundColor);

  if FFocused then
    Canvas.SetColor(clWidgetFrame)
  else
    Canvas.SetColor(clInactiveWgFrame);
  Canvas.DrawRectangle(r);
  
  FixMinMaxOrder;
  FixPositionLimits;

  if Orientation = orHorizontal then
  begin
    drawwidth := Width - 5 - FSliderSize;
    linepos   := FMax - FMin;
    if linepos <> 0 then
    begin
      linepos := drawwidth / linepos;
      Canvas.SetColor(clWidgetFrame);
      for i := 0 to (FMax - FMin) do
        Canvas.DrawLine(round(2 + (FSliderSize div 2) + (linepos * i)), Height div 2 + FSliderSize * 2, round(2 + FSliderSize div 2 + linepos * i), Height - 5);
      DrawSlider(round(2 + FSliderSize div 2 + linepos * position));
    end;
  end
  else
  begin
    drawwidth := Height - 5 - FSliderSize;
    linepos   := FMax - FMin;
    if linepos <> 0 then
    begin
      linepos := drawwidth / linepos;
      Canvas.SetColor(clWidgetFrame);
      for i := 0 to (FMax - FMin) do
        Canvas.DrawLine(Width div 2 + FSliderSize * 2, round(2 + (FSliderSize div 2) + (linepos * i)), Width - 5, round(2 + FSliderSize div 2 + linepos * i));
      DrawSlider(round(2 + FSliderSize div 2 + linepos * position));
    end;
  end;  { if/else }
end;

procedure TfpgTrackBarExtra.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  linepos: double;
  drawwidth: integer;
  OldPos: integer;
begin
  OldPos := Position;
  FixMinMaxOrder;
  linepos := FMax - FMin;
  
  if Orientation = orHorizontal then
  begin
    drawwidth := Width - 5 - FSliderSize;
    linepos   := drawwidth / linepos;
    FPosition  := round((x - 2 - FSliderSize div 2) / linepos) + FMin;
  end
  else
  begin
    drawwidth := Height - 5 - FSliderSize;
    linepos   := drawwidth / linepos;
    FPosition  := round((y - 2 - FSliderSize div 2) / linepos) + FMin;
  end;
  RePaint;
  
  if Position <> OldPos then
    DoChange;

//  inherited HandleLMouseUp(x, y, shiftstate);
end;

procedure TfpgTrackBarExtra.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  OldPos: integer;
begin
  consumed := True;
  OldPos := FPosition;
  
  if Orientation = orHorizontal then
  begin
    case keycode of
      keyLeft:      Position := Position - 1;
      keyRight:     Position := Position + 1;
      keyPageUp:    Position := FMin;
      keyPageDown:  Position := FMax;
      else
        consumed := False;
    end;
  end
  else
  begin
    case keycode of
      keyUp:        Position := Position - 1;
      keyDown:      Position := Position + 1;
      keyPageUp:    Position := FMin;
      keyPageDown:  Position := FMax;
      else
        consumed := False;
    end;
  end;  { if/else }

  inherited HandleKeyPress(keycode, shiftstate, consumed);
  if OldPos <> Position then
    DoChange;
end;

constructor TfpgTrackBarExtra.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMax          := 10;
  FSliderSize   := 11;
end;

{ TfpgTrackBar }

procedure TfpgTrackBar.SetShowPosition(const AValue: boolean);
begin
  if FShowPosition = AValue then
    Exit; //==>
  FShowPosition := AValue;
  RePaint;
end;

function TfpgTrackBar.GetTextWidth: TfpgCoord;
begin
  if FShowPosition then
    Result := FFont.TextWidth(IntToStr(Max)) + 4
  else
    Result := 0;
end;

procedure TfpgTrackBar.SetSliderLength(AValue: integer);
begin
  if FSliderLength = AValue then
    Exit;
  FSliderLength := AValue;
  RePaint;
end;

procedure TfpgTrackBar.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  tw: TfpgCoord;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  if Orientation = orVertical then
  begin
    if (y >= Width + FSliderPos) and (y <= Width + FSliderPos + FSliderLength) then
    begin
      FSliderDragging := True;
      FSliderDragPos  := y;
    end;
  end
  else
  begin
    tw := GetTextWidth;
    if (x >= FSliderPos) and (x <= (FSliderPos + FSliderLength + tw)) then
    begin
      FSliderDragging := True;
      FSliderDragPos  := x;
    end;
  end;

  if FSliderDragging then
  begin
    FSliderDragStart := FSliderPos;
    RePaint;
  end;
end;

procedure TfpgTrackBar.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  FSliderDragging   := False;
  RePaint;
end;

procedure TfpgTrackBar.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  d: integer;
  area: integer;
  newp: integer;
  ppos: integer;
  tw: TfpgCoord;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  FMousePosition.X := x;
  FMousePosition.Y := y;

  if (not FSliderDragging) or ((btnstate and MOUSE_LEFT) = 0) then
  begin
    FSliderDragging := False;
    Exit;
  end;

  if Orientation = orVertical then
  begin
    d     := y - FSliderDragPos;
    area  := Height - FSliderLength-4;
  end
  else
  begin
    d     := x - FSliderDragPos;
    tw    := GetTextWidth;
    area  := Width - FSliderLength-4-tw;
  end;

  ppos       := FSliderPos;
  FSliderPos := FSliderDragStart + d;

  if FSliderPos < 0 then
    FSliderPos := 0;
  if FSliderPos > area then
    FSliderPos := area;

  if area <> FMin then
    newp := FMin + Trunc((FMax - FMin)  * (FSliderPos / area))
  else
    newp := FMin;

  if newp <> FPosition then
  begin
    Position := newp;
    RePaint;
    DoChange;
  end;
end;

procedure TfpgTrackBar.HandlePaint;
var
  r: TfpgRect;
begin
  DrawSlider(True);
  { dont't draw focus rect while dragging - it flickers }
  if Focused and (not FSliderDragging) then
  begin
    r.SetRect(0, 0, Width, Height);
    Canvas.DrawFocusRect(r);
  end;
end;

procedure TfpgTrackBar.DrawSlider(recalc: boolean);
var
  area: TfpgCoord;
  mm: TfpgCoord;
  r: TfpgRect;
  tw: TfpgCoord;
begin
  Canvas.Clear(FBackgroundColor);
  Canvas.SetColor(FBackgroundColor);

  if Orientation = orVertical then
    area := Height-4
  else
  begin
    tw := GetTextWidth;
    area := Width-4-tw;
  end;

  if recalc then
  begin
    if FPosition > FMax then
      FPosition := FMax;
    if FPosition < FMin then
      FPosition := FMin;

    mm   := FMax - FMin;
    area := area - FSliderLength;
    if mm = 0 then
      FSliderPos := FMin
    else
      FSliderPos := Trunc(area * ((FPosition - FMin) / mm));
    if FPosition = FMin then
      inc(FSliderPos, 2);
  end;

  if Orientation = orVertical then
  begin
    Canvas.DrawButtonFace(0, Width + FSliderPos, Width, FSliderLength, [btfIsEmbedded]);
  end
  else
  begin
    r.SetRect(1, (Height-4) div 2, Width - tw - 4, 4);
    Canvas.DrawControlFrame(r);
    r.SetRect(FSliderPos, (Height-20) div 2, FSliderLength, 21);
    Canvas.DrawButtonFace(r, []);
    if FShowPosition then
    begin
      Canvas.SetTextColor(TextColor);
      fpgStyle.DrawString(Canvas, Width - tw, (Height - FFont.Height) div 2, IntToStr(Position), Enabled);
    end;
  end;
end;

constructor TfpgTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height        := 30;
  Width         := 100;
  FSliderPos    := 0;
  FSliderDragging := False;
  FSliderLength := 11;
  FScrollStep   := 1;
  FShowPosition := False;
  FFont         := fpgGetFont('#Grid');
end;

destructor TfpgTrackBar.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

end.

