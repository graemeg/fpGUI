{%mainunit fpgui.pas}

{$ifndef READ_IMPLEMENTATION}

type

  TFButtonFlag = (fbfIsPressed);
  TFButtonFlags = set of TFButtonFlag;

  { TFStyle }

  TFStyle = class(TObject)
  public
    constructor Create; virtual;
    procedure   DrawButtonFace(ACanvas: TFCustomCanvas; ARect: TRect; AFlags: TFButtonFlags); virtual;
    procedure   DrawControlFrame(ACanvas: TFCustomCanvas; ARect: TRect); virtual;
    procedure   DrawDirectionArrow(ACanvas: TFCustomCanvas; ARect: TRect; direction: Integer); virtual;
    procedure   DrawEdit(ACanvas: TFCustomCanvas; ARect: TRect; AText: string); virtual;
  end;

const
  colShadow1: TGfxColor	       = (Red: $80; Green: $80; Blue: $80; Alpha: $00);
  colShadow2: TGfxColor	       = (Red: $40; Green: $40; Blue: $40; Alpha: $00);
  colHilite1: TGfxColor	       = (Red: $E0; Green: $E0; Blue: $E0; Alpha: $00);
  colHilite2: TGfxColor	       = (Red: $FF; Green: $FF; Blue: $FF; Alpha: $00);
  colGrayBlue: TGfxColor       = (Red: $7F; Green: $9D; Blue: $B9; Alpha: $00);

{$else}

constructor TFStyle.Create;
begin
  inherited Create;

end;

procedure TFStyle.DrawButtonFace(ACanvas: TFCustomCanvas; ARect: TRect; AFlags: TFButtonFlags);
var
  x, y, w, h: Integer;
begin
  x := ARect.Left;
  y := ARect.Top;
  w := ARect.Right - ARect.Left;
  h := ARect.Bottom - ARect.Top;

  { Background }
  ACanvas.SetColor(colLtGray);
  ACanvas.FillRect(ARect);

  // Left and Top (outer)
  if (fbfIsPressed in AFlags) then ACanvas.SetColor(colShadow2)
  else ACanvas.SetColor(colHilite1);

  ACanvas.DrawLine(x, y + h - 2, x, y);  // left
  ACanvas.DrawLine(x, y, x + w - 1, y);  // top

  // Left and Top (inner)
  if (fbfIsPressed in AFlags) then
  begin
    ACanvas.SetColor(colShadow1);
    ACanvas.DrawLine(x + 1, y + h - 3, x + 1, y + 1);  // left
    ACanvas.DrawLine(x + 1, y + 1, x + w - 2, y + 1);  // top
  end;

  // Right and Bottom (outer)
  if (fbfIsPressed in AFlags) then
    ACanvas.SetColor(colShadow2)
  else
    ACanvas.SetColor(colShadow2);

  ACanvas.DrawLine(x + w - 1, y + 1, x + w - 1, y + h - 1);   // right
  ACanvas.DrawLine(x, y + h - 1, x + w - 1, y + h - 1);       // bottom

  // Right and Bottom (inner)
  if fbfIsPressed in AFlags then
    ACanvas.SetColor(colHilite1)
  else
    ACanvas.SetColor(colShadow1);
    
  ACanvas.DrawLine(x + w - 2, y + 2, x + w - 2, y + h - 2);   // right
  ACanvas.DrawLine(x + 1, y + h - 2, x + w - 2, y + h - 2);   // bottom
end;

procedure TFStyle.DrawControlFrame(ACanvas: TFCustomCanvas; ARect: TRect);
begin
end;

procedure TFStyle.DrawDirectionArrow(ACanvas: TFCustomCanvas; ARect: TRect; direction: Integer);
begin
end;

procedure TFStyle.DrawEdit(ACanvas: TFCustomCanvas; ARect: TRect; AText: string);
begin
  { Inner area }
  ACanvas.SetColor(colWhite);
  ACanvas.FillRect(ARect);

  { The text }
  
  ACanvas.TextOut(Point(5, 0), AText);

  { Outer border }
  ACanvas.SetColor(colGrayBlue);
  ACanvas.DrawRect(ARect);
end;

{$endif}

