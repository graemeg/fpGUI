{
    fpGUI  -  Free Pascal GUI Library
    
    GroupBox class declarations
    
    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pas}

{$IFDEF read_interface}

  { TCustomGroupBox }

  TFCustomGroupBox = class(TFBinWidget)
  protected
    procedure   Paint(Canvas: TFCanvas); override;
    procedure   CalcSizes; override;
    procedure   Resized; override;
  public
    constructor Create(const pText: string; pOwner: TComponent); overload;
  end;


  TFGroupBox = class(TFCustomGroupBox)
  published
    property    CanExpandWidth;
    property    CanExpandHeight;
    property    Enabled;
    property    Text;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}



// ===================================================================
//   TCustomGroupBox
// ===================================================================

procedure TFCustomGroupBox.Paint(Canvas: TFCanvas);
begin
  Style.DrawGroupBox(Canvas, Rect(0, 0, Width, Height), Text, WidgetState);
end;


procedure TFCustomGroupBox.CalcSizes;
var
  Borders: TRect;
  LabelWidth: Integer;
begin
  LabelWidth := 0;
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


procedure TFCustomGroupBox.Resized;
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


constructor TFCustomGroupBox.Create(const pText: string; pOwner: TComponent);
begin
  Create(pOwner);
  Text := pText;
end;


{$ENDIF read_implementation}

