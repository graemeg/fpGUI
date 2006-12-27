{
    fpGUI  -  Free Pascal GUI Library
    
    Panel class declarations
    
    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pp}

{
  Panel implementation. I mixture of Delphi's TPanel and TBevel. The class
  name might change to Frame.
}

{$IFDEF read_interface}

  TCustomPanel = class(TBinWidget)
  private
    FBevelStyle: TBevelStyle;
    procedure   SetBevelStyle(const AValue: TBevelStyle);
  protected
    procedure   Paint(Canvas: TFCanvas); override;
    procedure   CalcSizes; override;
    procedure   Resized; override;
    property    BevelStyle: TBevelStyle read FBevelStyle write SetBevelStyle;
    {$Note Still outstanding is the Shape and Color properties for starters }
  public
    constructor Create(const pText: string; pOwner: TComponent); overload;
  end;
  

  TPanel = class(TCustomPanel)
  published
    property    CanExpandWidth;
    property    CanExpandHeight;
    property    Enabled;
    property    Text;
    property    BevelStyle;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}

procedure TCustomPanel.SetBevelStyle(const AValue: TBevelStyle);
begin
  if FBevelStyle = AValue then exit;
  FBevelStyle := AValue;
  Redraw;
end;

procedure TCustomPanel.Paint(Canvas: TFCanvas);
var
  Pt: TPoint;
begin
  inherited Paint(Canvas);
  Style.DrawPanel(Canvas, Rect(0, 0, Width, Height), FBevelStyle);

  if Text <> '' then
  begin
    Canvas.SetColor(Style.GetUIColor(clBtnText));
    Pt.x := (BoundsSize.cx - Canvas.TextWidth(Text)) div 2;
    Pt.y := (BoundsSize.cy - Canvas.FontCellHeight) div 2;
    Style.DrawText(Canvas, Pt, Text, WidgetState);
  end;
end;

procedure TCustomPanel.CalcSizes;
var
  Borders: TRect;
  LabelWidth: Integer;
begin
  Borders := Style.GetGroupBoxBorders(TFCanvas(FindForm.Wnd.Canvas), Text, LabelWidth);
  FMinSize.cx := Borders.Left + Borders.Right + LabelWidth;
  FMinSize.cy := Borders.Top + Borders.Bottom;
  if Assigned(Child) then
  begin
    if Child.MinSize.cx > LabelWidth then
      FMinSize.cx := Borders.Left + Borders.Right + Child.MinSize.cx;
    Inc(FMinSize.cy, Child.MinSize.cy);
    if Child.DefSize.cx > LabelWidth then
      FDefSize.cx := Borders.Left + Borders.Right + Child.MinSize.cx;
    Inc(FDefSize.cy, Child.DefSize.cy);
    if Child.MaxSize.cx > LabelWidth then
      FMaxSize.cx := Min(Borders.Left + Borders.Right + Child.MaxSize.cx, InfiniteSize);
    FMaxSize.cy := Min(MaxSize.cy + Child.MaxSize.cy, InfiniteSize);
  end;
end;

procedure TCustomPanel.Resized;
var
  LabelWidth: Integer;
  Borders: TRect;
begin
  LabelWidth := 0;
  if Assigned(Child) then
  begin
    Borders := Style.GetGroupBoxBorders(TFCanvas(FindForm.Wnd.Canvas), Text, LabelWidth);
    Child.SetBounds(Borders.TopLeft, Size(Width - Borders.Left - Borders.Right,
        Height - Borders.Top - Borders.Bottom));
  end;
end;

constructor TCustomPanel.Create(const pText: string; pOwner: TComponent);
begin
  Create(pOwner);
  FCanExpandWidth   := True;
  FCanExpandHeight  := True;
  Text              := pText;
  FBevelStyle       := bsRaised;
end;


{$ENDIF read_implementation}

