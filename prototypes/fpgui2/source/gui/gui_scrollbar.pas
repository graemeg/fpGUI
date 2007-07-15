unit gui_scrollbar;

{$mode objfpc}{$H+}

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
    FScrollStep: integer;
  protected
    FSliderPos: TfpgCoord;
    FSliderLength: TfpgCoord;
    FSliderDragging: boolean;
    FStartBtnPressed,
    FEndBtnPressed: Boolean;
    FSliderDragPos: TfpgCoord;
    FSliderDragStart: TfpgCoord;
    FScrollTimer: TfpgTimer;
    FActiveButtonRect: TRect;
    FMousePosition: TPoint;
    procedure   ScrollTimer(Sender: TObject);
    procedure   DrawButton(x, y, w, h: TfpgCoord; const imgname: string; Pressed: Boolean = False);
    procedure   DrawSlider(recalc: boolean);
    procedure   HandleLMouseDown(x, y: integer; shiftstate: word); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: word); override;
    procedure   HandleMouseMove(x, y: integer; btnstate, shiftstate: word); override;
    procedure   HandlePaint; override;
    procedure   PositionChange(d: integer);
  public
    OnScroll: TScrollNotifyEvent;
    Orientation: TOrientation;
    Min: integer;
    Max: integer;
    SliderSize: double;  // 0-1
    Position: integer;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   RepaintSlider;
    property    ScrollStep: integer read FScrollStep write FScrollStep default 1;
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
  Min           := 0;
  Max           := 100;
  Position      := 10;
  SliderSize    := 0.5;
  OnScroll      := nil;
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
    DrawButton(0, 0, Width, Width, 'sys.sb.up' ,FStartBtnPressed);
    DrawButton(0, Height - Width, Width, Width, 'sys.sb.down', FEndBtnPressed);
  end
  else
  begin
    DrawButton(0, 0, Height, Height, 'sys.sb.left', FStartBtnPressed);
    DrawButton(Width - Height, 0, Height, Height, 'sys.sb.right', FEndBtnPressed);
  end;

  DrawSlider(True);
  Canvas.EndDraw;
end;

procedure TfpgScrollBar.RepaintSlider;
begin
  if not HasHandle then
    Exit;
  DrawSlider(True);
end;

procedure TfpgScrollBar.ScrollTimer(Sender: TObject);
begin
  FScrollTimer.Interval := 25;
  if  (FMousePosition.X < FActiveButtonRect.Right)
  and (FMousePosition.X > FActiveButtonRect.Left)
  and (FMousePosition.Y < FActiveButtonRect.Bottom)
  and (FMousePosition.Y > FActiveButtonRect.Top)
  then begin
    if FStartBtnPressed then PositionChange(-FScrollStep);
    if FEndBtnPressed then PositionChange(FScrollStep);
  end
  else begin
    FScrollTimer.Enabled := False;
  end;
end;

procedure TfpgScrollBar.DrawButton(x, y, w, h: TfpgCoord; const imgname: string; Pressed: Boolean = False);
var
  img: TfpgImage;
begin
  if Pressed then
    Canvas.DrawButtonFace(x, y, w, h, [btnIsEmbedded, btnIsPressed])
  else
    Canvas.DrawButtonFace(x, y, w, h, [btnIsEmbedded]);
  Canvas.SetColor(clText1);
  img := fpgImages.GetImage(imgname);
  if img <> nil then
    Canvas.DrawImage(x + w div 2 - (img.Width div 2), y + h div 2 - (img.Height div 2), img);
end;

procedure TfpgScrollBar.DrawSlider(recalc: boolean);
var
  area: TfpgCoord;
  mm: TfpgCoord;
  rpr: TfpgRect;
begin
  Canvas.BeginDraw;

  if SliderSize > 1 then
    SliderSize := 1;

  Canvas.SetColor(clScrollBar);

  if Orientation = orVertical then
  begin
    Canvas.FillRectangle(0, Width, Width, Height - Width - Width);
    area := Height - (Width shl 1);
  end
  else
  begin
    Canvas.FillRectangle(Height, 0, Width - Height - Height, Height);
    area := Width - (Height shl 1);
  end;

  if recalc then
  begin
    if Position > Max then
      Position := Max;
    if Position < min then
      Position := Min;

    FSliderLength := trunc(area * SliderSize);
    if FSliderLength < 8 then
      FSliderLength := 8;
    area := area - FSliderLength;
    mm   := Max - Min;
    if mm = 0 then
      FSliderPos := 0
    else
      FSliderPos := Trunc(area * ((Position - min) / mm));
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

procedure TfpgScrollBar.HandleLMouseDown(x, y: integer; shiftstate: word);
begin
  inherited;

  if Orientation = orVertical then
  begin
    if y <= Width then begin
      PositionChange(-FScrollStep);
      FStartBtnPressed := True;
      FActiveButtonRect := Rect(0, 0, Width, Width);
    end
    else if y >= Height - Width then begin
      PositionChange(FScrollStep);
      FEndBtnPressed := True;
      FActiveButtonRect := Rect(0,Height-Width, Width, Height);
    end
    else if (y >= Width + FSliderPos) and (y <= Width + FSliderPos + FSliderLength) then
    begin
      FSliderDragging := True;
      FSliderDragPos  := y;
    end;
  end
  else begin
    if x <= Height then begin
      PositionChange(-FScrollStep);
      FStartBtnPressed := True;
      FActiveButtonRect := Rect(0, 0, Height, Height);
    end
    else if x >= Width - Height then begin
      PositionChange(FScrollStep);
      FEndBtnPressed := True;
      FActiveButtonRect := Rect(Width-Height, 0, Width, Height);
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
  else if FStartBtnPressed or FEndBtnPressed then begin
    FScrollTimer.Interval := 500;
    FScrollTimer.Enabled := True;

    HandlePaint;
  end;
  
end;

procedure TfpgScrollBar.HandleLMouseUp(x, y: integer; shiftstate: word);
var
  WasPressed: Boolean;
begin
  inherited;
  WasPressed := FStartBtnPressed or FEndBtnPressed;
  FScrollTimer.Enabled := False;
  FStartBtnPressed := False;
  FEndBtnPressed   := False;
  FSliderDragging := False;
  if WasPressed then HandlePaint;
end;

procedure TfpgScrollBar.HandleMouseMove(x, y: integer; btnstate, shiftstate: word);
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
    newp := Min + trunc((Max - Min) * FSliderPos / area)
  else
    newp := Min;

  if newp <> Position then
  begin
    Position := newp;
    if Assigned(OnScroll) then
      OnScroll(self, Position);
  end;
end;

procedure TfpgScrollBar.PositionChange(d: integer);
begin
  Position := Position + d;
  if Position < Min then
    Position := Min;
  if Position > Max then
    Position := Max;

  DrawSlider(True);

  if Assigned(OnScroll) then
    OnScroll(self, Position);
end;

end.

