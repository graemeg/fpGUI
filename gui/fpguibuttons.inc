{
    fpGUI  -  Free Pascal GUI Library
    
    Button class declarations
    
    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pas}


{$IFDEF read_interface}

  TFGenericButton = class(TFWidget)
  protected
    FEmbedded: Boolean;
    procedure   Paint(Canvas: TFCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    property    Embedded: Boolean read FEmbedded write FEmbedded default False;
  end;


  TFCustomButton = class(TFGenericButton)
  protected
    procedure   Paint(Canvas: TFCanvas); override;
    procedure   CalcSizes; override;
  public
    constructor Create(const pText: string; pOwner: TComponent); overload;
  end;


  TFButton = class(TFCustomButton)
  published
    property    CanExpandWidth;
    property    CanExpandHeight;
    property    Enabled;
    property    Text;
    property    OnClick;
  end;

{
  TCustomImageButton = class(TFGenericButton)
  protected
    procedure Paint(Canvas: TGfxCanvas); override;
    procedure EvRecalcLayout; override;
  end;

  TImageButton = class(TCustomImageButton)
  published
    property Enabled;
    property Image;
    property OnClick;
  end;
}

{$ENDIF read_interface}



{$IFDEF read_implementation}

// ===================================================================
//   TFGenericButton
// ===================================================================

constructor TFGenericButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  // default size
  FDefSize := Size(18, 18);
end;


procedure TFGenericButton.Paint(Canvas: TFCanvas);
var
  Flags: TFButtonFlags;
begin
  if Embedded then
    Flags := [btnIsEmbedded]
  else
    Flags := [];
  if (wsClicked in WidgetState) and (wsMouseInside in WidgetState) then
    Include(Flags, btnIsPressed);
  if (wsHasFocus in WidgetState) and not Embedded then
  begin
    Include(Flags, btnIsSelected);
    if FindForm.IsActive then
      Include(Flags, btnHasFocus);
  end;

  Style.DrawButtonFace(Canvas, Rect(0, 0, BoundsSize.cx, BoundsSize.cy), Flags);
end;

// ===================================================================
//   TFCustomButton
// ===================================================================

procedure TFCustomButton.Paint(Canvas: TFCanvas);
var
  Pt: TPoint;
  Borders: TRect;
begin
  inherited Paint(Canvas);

  Borders := Style.GetButtonBorders;
  Canvas.IntersectClipRect(Rect(Borders.Left, Borders.Top,
    BoundsSize.cx - Borders.Right, BoundsSize.cy - Borders.Bottom));

  Canvas.SetColor(Style.GetUIColor(clBtnText));
  Pt.x := (BoundsSize.cx - Canvas.TextWidth(Text)) div 2;
  Pt.y := (BoundsSize.cy - Canvas.FontCellHeight) div 2;
  if (wsClicked in WidgetState) and (wsMouseInside in WidgetState) then
    Pt := Pt + Point(1, 1);
  Style.DrawText(Canvas, Pt, Text, WidgetState);
end;


procedure TFCustomButton.CalcSizes;
var
  Borders: TRect;
begin
  LAYOUTTRACE('TFCustomButton.CalcSizes for %s:%s', [Name, ClassName]);
  Borders := Style.GetButtonBorders;
  with FindForm.Wnd.Canvas do
  begin
    FMinSize.cx := Borders.Left + Borders.Right + TextWidth(Text);
    if FMinSize.cx < 75 then
      FMinSize.cx := 75;    // apply default button width
    FMinSize.cy := Borders.Top + Borders.Bottom + FontCellHeight;
  end;
  FDefSize := FMinSize + gfxbase.Size(20, 2);
end;

constructor TFCustomButton.Create(const pText: string; pOwner: TComponent);
begin
  Create(pOwner);
  // default size
  FDefSize := Size(75, 25);
  Text := pText;
end;


{$ENDIF read_implementation}

