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
      This unit implements color selectors using a ColorWheel and
      a ValueBar. Color results are in HSV or TfpgColor format.
}

unit fpg_ColorWheel;

{$mode objfpc}{$H+}

interface

uses
  Classes, fpg_base, fpg_main, fpg_widget;

type
  // forward declaration
  TfpgValueBar = class;

  TfpgColorWheel = class(TfpgWidget)
  protected
    FValueBar: TfpgValueBar;
    FHue: longint;
    FSaturation: double;
    FMarginWidth: longint;
    FCursorSize: longint;
    FWhiteAreaPercent: longint; // 0 to 50 percent of circle radius that is pure white
    FOnChange: TNotifyEvent;
    FImage: TfpgImage; // cached colorwheel image
    FRecalcWheel: Boolean; // should chached FImage be recalculated
    procedure   HSFromPoint(X, Y: longint; out H: longint; out S: double);
    procedure   DrawCursor;
    procedure   SetMarginWidth(NewWidth: longint);
    procedure   SetCursorSize(NewSize: longint);
    procedure   SetValueBar(AValueBar: TfpgValueBar);
    procedure   SetWhiteAreaPercent(WhiteAreaPercent: longint);
    procedure   SetBackgroundColor(const AValue: TfpgColor); override;
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    function    DrawWidth: longint;
    function    DrawHeight: longint;
    procedure   Change;
    procedure   HandlePaint; override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Hue: longint Read FHue;
    property    Saturation: double Read FSaturation;
    procedure   SetSelectedColor(const NewColor: TfpgColor);
  published
    property    Align;
    property    BackgroundColor;
    property    Enabled;
    property    ValueBar: TfpgValueBar Read FValueBar Write SetValueBar;
    property    MarginWidth: longint Read FMarginWidth Write SetMarginWidth default 5;
    property    CursorSize: longint Read FCursorSize Write SetCursorSize default 5;
    property    WhiteAreaPercent: longint Read FWhiteAreaPercent Write SetWhiteAreaPercent default 10;
    property    OnChange: TNotifyEvent Read FOnChange Write FOnChange;
  end;


  TfpgValueBar = class(TfpgWidget)
  protected
    FColorWheel: TfpgColorWheel;
    FHue: longint;
    FSaturation: double;
    FValue: double;
    FMarginWidth: longint;
    FCursorHeight: longint;
    FOnChange: TNotifyEvent;
    procedure   DrawCursor;
    procedure   SetMarginWidth(NewWidth: longint);
    procedure   SetValue(Value: double);
    procedure   SetCursorHeight(CursorHeight: longint);
    function    GetSelectedColor: TfpgColor;
    procedure   Change;
    function    DrawWidth: longint;
    function    DrawHeight: longint;
    function    ValueFromY(Y: longint): real;
    procedure   DrawLine(Y: longint);
    procedure   HandlePaint; override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   SetHS(Hue: longint; Sat: double);
  published
    property    Align;
    property    BackgroundColor;
    property    Enabled;
    property    Value: double Read FValue Write SetValue;
    property    SelectedColor: TfpgColor Read GetSelectedColor;
    property    MarginWidth: longint Read FMarginWidth Write SetMarginWidth default 5;
    property    CursorHeight: longint Read FCursorHeight Write SetCursorHeight default 10;
    property    OnChange: TNotifyEvent Read FOnChange Write FOnChange;
  end;


implementation

uses
  fpg_ColorMapping;

const
  RadToHue: double = 1536 / (2 * pi);


function AngleFrom(x, y: double): double;
  // Quadrants are laid out as follows:
  //
  //     1|0
  //   ---+----
  //     2|3
begin
  if X = 0 then
  begin
    if Y > 0 then
      Result := pi / 2
    else
      Result := 3 * pi / 2;
  end
  else
  begin
    Result := arctan(abs(y) / abs(x));
    if (x < 0) and (y >= 0) then
      // quadrant 1
      Result := pi - Result
    else if (x < 0) and (y < 0) then
      // quadrant 2
      Result := Result + pi
    else if (x >= 0) and (y < 0) then
      // quadrant 3
      Result := 2 * pi - Result;
  end;
end;


{ TfpgColorWheel }

function TfpgColorWheel.DrawWidth: longint;
begin
  Result := Width - FMarginWidth * 2;
end;

function TfpgColorWheel.DrawHeight: longint;
begin
  Result := Height - FMarginWidth * 2;
end;

procedure TfpgColorWheel.SetSelectedColor(const NewColor: TfpgColor);
var
  Value: double;
begin
  RGBToHSV(NewColor, FHue, FSaturation, Value);
  Change;
  if FValueBar <> nil then
    FValueBar.Value := Value;
end;

procedure TfpgColorWheel.Change;
begin
  if FValueBar <> nil then
    FValueBar.SetHS(FHue, FSaturation);
  if FOnChange <> nil then
    FOnChange(self);
  Invalidate;
end;

procedure TfpgColorWheel.HandlePaint;
var
  x, y: longint;
  lHue: longint;
  lsaturation: double;
  c:    TfpgColor;
begin
  // clear background rectangle
  Canvas.Clear(BackgroundColor);

  // margins too big
  if (Width < MarginWidth * 2) or (Height < MarginWidth * 2) then
    Exit;  //==>

  if csDesigning in ComponentState then
  begin
    // When designing, don't draw colors
    // but draw an outline
    Canvas.SetLineStyle(1, lsDash);
    Canvas.DrawRectangle(GetClientRect);
    Canvas.SetLineStyle(1, lsSolid);
    Canvas.Color := clUIDesignerGreen;
    Canvas.FillArc(FMarginWidth, FMarginWidth, DrawWidth, DrawHeight, 0, 360);
    Canvas.Color := clHilite1;
    Canvas.DrawArc(FMarginWidth, FMarginWidth, DrawWidth, DrawHeight, 45, 180);
    Canvas.Color := clShadow1;
    Canvas.DrawArc(FMarginWidth, FMarginWidth, DrawWidth, DrawHeight, 225, 180);
    Canvas.TextColor := clShadow1;
    Canvas.DrawText(5, 5, Name + ': ' + ClassName);
    DrawCursor;
    Exit;  //==>
  end;

  if (FImage = nil) or FRecalcWheel then
  begin
    // we must only do this when needed, because it's very slow
    if FImage <> nil then
      FImage.Free;
    FImage := TfpgImage.Create;
    FImage.AllocateImage(32, DrawWidth, DrawHeight);
    for X := 0 to DrawWidth - 1 do
    begin
      for Y := 0 to DrawHeight - 1 do
      begin
        // work out hue and saturation for point
        HSFromPoint(X, Y, lHue, lSaturation);
        if lSaturation <= 1.0 then
        begin
          // point is within wheel
          C := HSVToRGB(lHue, lSaturation, 1.0);
          // draw the pixel
          Canvas.Pixels[X + FMarginWidth, Y + FMarginWidth] := C;
          FImage.Colors[x, y] := c;
        end
        else
          // point is outside wheel. Also incase color is alias, lookup the RGB values.
          FImage.Colors[x, y] := fpgColorToRGB(BackgroundColor);
      end;
      FImage.UpdateImage;
    end;
    FRecalcWheel := False;
  end
  else
  begin
    // paint buffer image seeing that we have it
    Canvas.DrawImage(FMarginWidth, FMarginWidth, FImage);
  end;

  DrawCursor;
end;

procedure TfpgColorWheel.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  Dec(X, FMarginWidth);
  Dec(Y, FMarginWidth);
  HSFromPoint(X, Y, FHue, FSaturation);
  if FSaturation > 1.0 then
    FSaturation := 1.0;
  Change;
  CaptureMouse;
end;

procedure TfpgColorWheel.HandleMouseMove(x, y: integer; btnstate: word;
  shiftstate: TShiftState);
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);
  // is mouse still captured (LButton down)?
  if ((btnstate and MOUSE_LEFT) = 0) then
    Exit;  //==>
  Dec(X, FMarginWidth);
  Dec(Y, FMarginWidth);
  HSFromPoint(X, Y, FHue, FSaturation);
  if FSaturation > 1.0 then
    FSaturation := 1.0;
  Change;
end;

procedure TfpgColorWheel.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  ReleaseMouse;
end;

constructor TfpgColorWheel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMarginWidth := 5;
  FCursorSize := 5;
  Width  := 100;
  Height := 100;
  Name   := 'ColorWheel';
  FWhiteAreaPercent := 10;
  FRecalcWheel := True;
end;

destructor TfpgColorWheel.Destroy;
begin
  if FImage <> nil then
    FImage.Free;
  inherited Destroy;
end;

// Calculate hue and saturation for a given point in the color wheel
procedure TfpgColorWheel.HSFromPoint(X, Y: longint; out H: longint; out S: double);
var
  xp, yp: double;
  halfw, halfh: longint;
begin
  halfw := DrawWidth div 2;
  halfh := DrawHeight div 2;
  xp    := (x - halfw) / halfw; // x as -1..1
  yp    := (y - halfh) / halfh; // y as -1..1
  H     := Trunc(RadToHue * AngleFrom(xp, -yp));
  S     := sqrt(xp * xp + yp * yp);
  // scale saturation and limit to white, for white area
  S     := S * (1 + (FWhiteAreaPercent / 100.0)) - (FWhiteAreaPercent / 100.0);
  if S < 0 then
    S := 0;
end;

procedure TfpgColorWheel.DrawCursor;
var
  Angle: Double;
  X, Y: longint;
  S: Double;
  a: Double;
  len: longint;
begin
  Angle := FHue/RadToHue;

  // Scale distance from centre for white area
  S := FSaturation;
  if S > 0 then
  begin
    a := FWhiteAreaPercent / 100.0;
    S := (S * (1 - a)) + a;
  end;

  // work out point for selected hue and saturation
  X := Trunc(Width div 2+cos(Angle) * S * (DrawWidth div 2));
  Y := Trunc(Height div 2+sin(Angle) * S * -(DrawHeight div 2));

  // draw a crosshair with centre at mouse cursor position
  len := FCursorSize*2 + 2; // length of crosshair lines
  Canvas.XORFillRectangle($FFFFFF, X-FCursorSize, Y, len, 2);
  Canvas.XORFillRectangle($FFFFFF, X, Y-FCursorSize, 2, len);
end;

procedure TfpgColorWheel.SetMarginWidth(NewWidth: longint);
begin
  FMarginWidth := NewWidth;
  if WinHandle = 0 then
    Exit; //==>
  Invalidate;
end;

procedure TfpgColorWheel.SetCursorSize(NewSize: longint);
begin
  FCursorSize := NewSize;
  if WinHandle = 0 then
    Exit; //==>
  Invalidate;
end;

procedure TfpgColorWheel.SetValueBar(AValueBar: TfpgValueBar);
begin
  if FValueBar <> nil then
    // tell the old value bar it's no longer controlled by this wheel
    FValueBar.FColorWheel := nil;
  FValueBar := AValueBar;
  if FValueBar <> nil then
  begin
    // Tell value bar it is controlled by this component
    FValueBar.FColorWheel := Self;
    // request notification when other is freed
    FValueBar.FreeNotification(Self);
  end;
end;

procedure TfpgColorWheel.SetWhiteAreaPercent(WhiteAreaPercent: longint);
begin
  if WhiteAreaPercent > 50 then
    WhiteAreaPercent := 50;

  if WhiteAreaPercent < 0 then
    WhiteAreaPercent := 0;

  FWhiteAreaPercent := WhiteAreaPercent;
  Invalidate;
end;

procedure TfpgColorWheel.SetBackgroundColor(const AValue: TfpgColor);
begin
  FRecalcWheel := True;
  inherited SetBackgroundColor(AValue);
end;

procedure TfpgColorWheel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FValueBar then
      FValueBar := nil;
  end;
end;


{ TfpgValueBar }

procedure TfpgValueBar.DrawLine(Y: longint);
var
  DrawVal: double;
  c: TfpgColor;
begin
  DrawVal := ValueFromY(Y);
  C := HSVToRGB(FHue, FSaturation, DrawVal);
  Canvas.Color := c;
  Canvas.DrawLine(FMarginWidth, Y, Width - FMarginWidth - 1, Y);
end;

procedure TfpgValueBar.HandlePaint;
var
  y: longint;
  r: TfpgRect;
begin
  //  inherited HandlePaint;
  Canvas.Clear(BackgroundColor);

  if csDesigning in ComponentState then
  begin
    // when designing just draw
    // a rectangle to indicate
    Canvas.Color := clBlack;
    Canvas.SetLineStyle(1, lsDash);
    Canvas.DrawRectangle(GetClientRect);
    if (Width < MarginWidth * 2) or (Height < MarginWidth * 2) then
      Exit;  //==>
    r := GetClientRect;
    InflateRect(r, -FMarginWidth, -FMarginWidth);
    Canvas.Color := clShadow1;
    Canvas.SetLineStyle(1, lsSolid);
    Canvas.DrawRectangle(r);
    Canvas.TextColor := clShadow1;
    Canvas.DrawText(5, 5, Width, Height, Name + ': ' + ClassName, TextFlagsDflt + [txtWrap]);
    DrawCursor;
    exit;
  end;

  // Draw margins
  r.left := 0;
  r.setbottom(0);
  r.setright(FMarginWidth - 1);
  r.top := Height - 1;
  Canvas.Color := BackgroundColor;
  Canvas.FillRectangle(r);  // left
  r.left := Width - FMarginWidth;
  r.setright(Width - 1);
  Canvas.FillRectangle(r); // right
  r.left := FMarginWidth;
  r.setright(Width - FMarginWidth - 1);
  r.setbottom(Height - FMarginWidth);
  r.top := Height - 1;
  Canvas.FillRectangle(r);  // top
  r.setbottom(0);
  r.top := FMarginWidth - 1;
  Canvas.FillRectangle(r); // bottom

  if (Width < MarginWidth * 2) or (Height < MarginWidth * 2) then
    Exit; //==>

  for Y := 0 to DrawHeight - 1 do
    DrawLine(Y + FMarginWidth);

  DrawCursor;
end;

procedure TfpgValueBar.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  FValue := ValueFromY(Y);
  Change;
  CaptureMouse;
end;

procedure TfpgValueBar.HandleMouseMove(x, y: integer; btnstate: word;
  shiftstate: TShiftState);
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);
  if ((btnstate and MOUSE_LEFT) = 0) then
    Exit; //==>
  FValue := ValueFromY(Y);
  Change;
end;

procedure TfpgValueBar.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  ReleaseMouse;
end;

constructor TfpgValueBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMarginWidth := 5;
  FValue  := 1.0;
  Width   := 80;
  Height  := 100;
  Name    := 'ValueBar';
  FCursorHeight := 10;
end;

procedure TfpgValueBar.SetHS(Hue: longint; Sat: double);
begin
  FHue := Hue;
  FSaturation := Sat;
  Invalidate;
  Change;
end;

procedure TfpgValueBar.SetValue(Value: double);
begin
  FValue := Value;
  Change;
end;

function TfpgValueBar.DrawWidth: longint;
begin
  Result := Width - FMarginWidth * 2;
end;

function TfpgValueBar.DrawHeight: longint;
begin
  Result := Height - FMarginWidth * 2;
end;

procedure TfpgValueBar.DrawCursor;
var
  Y: longint;
  r: TfpgRect;
begin
  if (Width < MarginWidth * 2) or
     (Height < MarginWidth * 2) then
    Exit;  //==>

  Y := Trunc((FValue * DrawHeight) + FMarginWidth);

  r.SetRect(FMarginWidth-1, Y - (FCursorHeight div 2), DrawWidth+1, FCursorHeight);
  Canvas.Color := GetSelectedColor;
  Canvas.FillRectangle(r);

  Canvas.Color := clBlack;
  Canvas.DrawControlFrame(r);
end;

procedure TfpgValueBar.SetMarginWidth(NewWidth: longint);
begin
  if MarginWidth < 0 then
    MarginWidth := 0;
  FMarginWidth := NewWidth;
  Invalidate;
end;

procedure TfpgValueBar.SetCursorHeight(CursorHeight: longint);
begin
  if CursorHeight < 3 then
    CursorHeight := 3;
  FCursorHeight := CursorHeight;
  Invalidate;
end;

function TfpgValueBar.GetSelectedColor: TfpgColor;
begin
  Result := HSVToRGB(FHue, FSaturation, FValue);
end;

function TfpgValueBar.ValueFromY(Y: longint): real;
begin
  Result := (Y - MarginWidth) / (DrawHeight - 1);
  if Result < 0 then
    Result := 0;
  if Result > 1.0 then
    Result := 1.0;
end;

procedure TfpgValueBar.Change;
begin
  Invalidate;
  if FOnChange <> nil then
    FOnChange(self);
end;


end.

