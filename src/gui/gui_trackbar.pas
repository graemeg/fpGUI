unit gui_trackbar;

{$mode objfpc}{$H+}

{
  TODO:
    * Tick line orientation (top, bottom, left or right)
    * Slide the slider with the mouse button down (like a scrollbar)
    * Slider button style (rectangle, pointer, double pointer)
    * Tick captions
}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_widget;
  
type
  TTrackBarChange = procedure(Sender: TObject; APosition: integer) of object;
  
  
  TfpgTrackBar = class(TfpgWidget)
  private
    FBackgroundColor: TfpgColor;
    FMax: integer;
    FMin: integer;
    FOnChange: TTrackBarChange;
    FOrientation: TOrientation;
    FPosition: integer;
    FSliderSize: integer;
    procedure   DoChange;
    procedure   SetBackgroundColor(const AValue: TfpgColor);
    procedure   SetMax(const AValue: integer);
    procedure   SetMin(const AValue: integer);
    procedure   SetPosition(const AValue: integer);
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
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor;
    property    Min: integer read FMin write SetMin default 0;
    property    Max: integer read FMax write SetMax default 10;
    property    Position: integer read FPosition write SetPosition default 0;
    property    SliderSize: integer read FSliderSize write SetSliderSize default 11;
    property    Orientation: TOrientation read FOrientation write FOrientation default orHorizontal;
    property    OnChange: TTrackBarChange read FOnChange write FOnChange;
  end;
  

implementation

{ TfpgTrackBar }

procedure TfpgTrackBar.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self, FPosition);
end;

procedure TfpgTrackBar.SetBackgroundColor(const AValue: TfpgColor);
begin
  if FBackgroundColor = AValue then
    Exit; //==>
  FBackgroundColor := AValue;
  RePaint;
end;

procedure TfpgTrackBar.SetMax(const AValue: integer);
begin
  if FMax = AValue then
    Exit; //==>
  FMax := AValue;
  RePaint;
end;

procedure TfpgTrackBar.SetMin(const AValue: integer);
begin
  if FMin = AValue then
    Exit; //==>
  FMin := AValue;
  RePaint;
end;

procedure TfpgTrackBar.SetPosition(const AValue: integer);
begin
  if FPosition = AValue then
    Exit; //==>
  FPosition := AValue;
  RePaint;
  // OnChange only fired on keyboard or mouse input.
end;

procedure TfpgTrackBar.SetSliderSize(const AValue: integer);
begin
  if FSliderSize = AValue then
    Exit; //==>
  if AValue > 11 then
  begin
    FSliderSize := AValue;
    RePaint;
  end;
end;

procedure TfpgTrackBar.FixMinMaxOrder;
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

procedure TfpgTrackBar.FixPositionLimits;
begin
  if FPosition < FMin then
    FPosition := FMin;
  if FPosition > FMax then
    FPosition := FMax;
end;

procedure TfpgTrackBar.DrawSlider(p: integer);
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

procedure TfpgTrackBar.HandlePaint;
var
  r: TRect;
  linepos: double;
  drawwidth: integer;
  i: integer;
begin
  Canvas.BeginDraw;
//  inherited HandlePaint;
  r := Rect(0, 0, Width-1, Height-1);
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

  Canvas.EndDraw;
end;

procedure TfpgTrackBar.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  p: integer;
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

procedure TfpgTrackBar.HandleKeyPress(var keycode: word;
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

constructor TfpgTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackgroundColor := clWindowBackground;
  FFocusable    := True;
  FMin          := 0;
  FMax          := 10;
  FPosition     := 0;
  FSliderSize   := 11;
  FOrientation  := orHorizontal;
  FOnChange     := nil;
end;

end.

