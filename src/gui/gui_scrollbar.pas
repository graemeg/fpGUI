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

  { TfpgScrollBar }

  TfpgScrollBar = class(TfpgWidget)
  private
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
    FSliderDragging: boolean;
    FStartBtnPressed: Boolean;
    FEndBtnPressed: Boolean;
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
  FSliderDragging := False;
  FSliderLength := 10;
  FScrollStep   := 1;
end;

destructor TfpgScrollBar.Destroy;
begin
  FScrollTimer.Free;
  inherited Destroy;
end;

procedure TfpgScrollBar.HandlePaint;
begin
  Canvas.BeginDraw;
  
  if Orientation = orVertical then
  begin
    DrawButton(0, 0, Width, Width, 'sys.sb.up', FStartBtnPressed);
    DrawButton(0, Height-Width, Width, Width, 'sys.sb.down', FEndBtnPressed);
  end
  else
  begin
    DrawButton(0, 0, Height, Height, 'sys.sb.left', FStartBtnPressed);
    DrawButton(Width-Height, 0, Height, Height, 'sys.sb.right', FEndBtnPressed);
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
begin
  FScrollTimer.Interval := 25;
  if  (FMousePosition.X < FActiveButtonRect.Right)
      and (FMousePosition.X > FActiveButtonRect.Left)
      and (FMousePosition.Y < FActiveButtonRect.Bottom)
      and (FMousePosition.Y > FActiveButtonRect.Top) then
  begin
    if FStartBtnPressed then
    begin
      PositionChange(-FScrollStep);
      if Position = FMin then
        FScrollTimer.Enabled := False;
    end;
    if FEndBtnPressed then
    begin
      PositionChange(FScrollStep);
      if Position = FMax then
        FScrollTimer.Enabled := False;
    end;
  end
  else
    FScrollTimer.Enabled := False;
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
begin
  inherited;

  if Orientation = orVertical then
  begin
    if y <= Width then
    begin
      PositionChange(-FScrollStep);
      FStartBtnPressed := True;
      FActiveButtonRect.SetRect(0, 0, Width, Width);
    end
    else if y >= Height - Width then
    begin
      PositionChange(FScrollStep);
      FEndBtnPressed := True;
      FActiveButtonRect.SetRect(0,Height-Width, Width, Height);
    end
    else if (y >= Width + FSliderPos) and (y <= Width + FSliderPos + FSliderLength) then
    begin
      FSliderDragging := True;
      FSliderDragPos  := y;
    end;
  end
  else
  begin
    if x <= Height then
    begin
      PositionChange(-FScrollStep);
      FStartBtnPressed := True;
      FActiveButtonRect.SetRect(0, 0, Height, Height);
    end
    else if x >= Width - Height then
    begin
      PositionChange(FScrollStep);
      FEndBtnPressed := True;
      FActiveButtonRect.SetRect(Width-Height, 0, Width, Height);
    end
    else if (x >= Height + FSliderPos) and (x <= Height + FSliderPos + FSliderLength) then
    begin
      FSliderDragging := True;
      FSliderDragPos  := x;
    end;
  end;
  
  if FSliderDragging then
  begin
    FSliderDragStart := FSliderPos;
    DrawSlider(False);
  end
  else if FStartBtnPressed or FEndBtnPressed then
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
  WasPressed := FStartBtnPressed or FEndBtnPressed;
  FScrollTimer.Enabled := False;
  FStartBtnPressed  := False;
  FEndBtnPressed    := False;
  FSliderDragging   := False;
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
  
  if (not FSliderDragging) or ((btnstate and MOUSE_LEFT) = 0) then
  begin
    FSliderDragging := False;
    Exit;
  end;

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

