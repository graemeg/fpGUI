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

  TGenericButton = class(TWidget)
  protected
    FEmbedded: Boolean;
    procedure   Paint(Canvas: TFCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    property    Embedded: Boolean read FEmbedded write FEmbedded default False;
  end;


  TCustomButton = class(TGenericButton)
  protected
    procedure   Paint(Canvas: TFCanvas); override;
    procedure   CalcSizes; override;
  public
    constructor Create(const pText: string; pOwner: TComponent); overload;
  end;


  TButton = class(TCustomButton)
  published
    property    CanExpandWidth;
    property    CanExpandHeight;
    property    Enabled;
    property    Text;
    property    OnClick;
  end;

{
  TCustomImageButton = class(TGenericButton)
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
//   TGenericButton
// ===================================================================

constructor TGenericButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
end;


procedure TGenericButton.Paint(Canvas: TFCanvas);
var
  Flags: TButtonFlags;
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
//   TCustomButton
// ===================================================================

procedure TCustomButton.Paint(Canvas: TFCanvas);
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


procedure TCustomButton.CalcSizes;
var
  Borders: TRect;
begin
  LAYOUTTRACE('TCustomButton.CalcSizes for %s:%s', [Name, ClassName]);
  Borders := Style.GetButtonBorders;
  with FindForm.Wnd.Canvas do
  begin
    FMinSize.cx := Borders.Left + Borders.Right + TextWidth(Text);
    if FMinSize.cx < 75 then
      FMinSize.cx := 75;    // apply default button width
    FMinSize.cy := Borders.Left + Borders.Right + FontCellHeight;
  end;
  FDefSize := FMinSize + gfxbase.Size(20, 2);
end;

constructor TCustomButton.Create(const pText: string; pOwner: TComponent);
begin
  Create(pOwner);
  Text := pText;
end;


{$ENDIF read_implementation}

