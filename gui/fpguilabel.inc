{
    fpGUI  -  Free Pascal GUI Library
    
    Label class declarations
    
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


  { TCustomLabel }

  TCustomLabel = class(TWidget)
  private
    FFontColor: TColor;
    procedure   SetAlignment(AAlignment: TAlignment);
    procedure   SetFontColor(const AValue: TColor);
  protected
    FAlignment: TAlignment;
    procedure   Paint(Canvas: TFCanvas); override;
    procedure   CalcSizes; override;
    property    Alignment: TAlignment read FAlignment write SetAlignment;
    property    FontColor: TColor read FFontColor write SetFontColor;
  public
    constructor Create(const pText: string; pOwner: TComponent); overload;
  end;
  

  TLabel = class(TCustomLabel)
  public
  published
    property CanExpandWidth;
    property Enabled;
    property FontColor;
    property Text;
    property Alignment default taLeftJustify;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}


// ===================================================================
//   TCustomLabel
// ===================================================================

procedure TCustomLabel.Paint(Canvas: TFCanvas);
var
  x: Integer;
begin
  Canvas.SetColor(Style.GetUIColor(FFontColor));
//  Canvas.SetColor(FFontColor);
  case Alignment of
    taLeftJustify: x := 0;
    taCenter: x := (BoundsSize.cx - Canvas.TextWidth(Text)) div 2;
    taRightJustify: x := BoundsSize.cx - Canvas.TextWidth(Text);
  end;
  Style.DrawText(Canvas, Point(x,
    (BoundsSize.cy - Canvas.FontCellHeight) div 2), Text, WidgetState);
end;


procedure TCustomLabel.CalcSizes;
begin
  with FindForm.Wnd.Canvas do
    FMinSize := gfxbase.Size(TextWidth(Text), FontCellHeight);
end;


constructor TCustomLabel.Create(const pText: string; pOwner: TComponent);
begin
  Create(pOwner);
  Text := pText;
  FFontColor := clWindowText;
end;


procedure TCustomLabel.SetAlignment(AAlignment: TAlignment);
begin
  if AAlignment <> Alignment then
  begin
    FAlignment := AAlignment;
    Redraw;
  end;
end;


procedure TCustomLabel.SetFontColor(const AValue: TColor);
begin
  if FFontColor = AValue then exit;
  FFontColor := AValue;
end;


{$ENDIF read_implementation}

