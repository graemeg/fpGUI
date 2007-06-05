{
    fpGUI  -  Free Pascal GUI Library
    
    RadioButton class declarations
    
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

  { TFCustomRadioButton }

  TFCustomRadioButton = class(TFWidget)
  private
    procedure   SetChecked(AChecked: Boolean);
  protected
    FChecked: Boolean;
    FLabelPos: TPoint;
    procedure   Click; override;
    procedure   Paint(Canvas: TFCanvas); override;
    procedure   CalcSizes; override;
    property    Checked: Boolean read FChecked write SetChecked;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(const pText: string; pOwner: TComponent); overload;
  end;


  TFRadioButton = class(TFCustomRadioButton)
  published
    property CanExpandWidth;
    property CanExpandHeight;
    property Enabled;
    property Checked;
    property Text;
    property OnClick;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}


// ===================================================================
//   TFCustomRadioButton
// ===================================================================

constructor TFCustomRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable];
end;


constructor TFCustomRadioButton.Create(const pText: string; pOwner: TComponent);
begin
  Create(pOwner);
  Text := pText;
end;


procedure TFCustomRadioButton.Click;
begin
  if not Checked then
    SetChecked(True);
  inherited Click;
end;


procedure TFCustomRadioButton.Paint(Canvas: TFCanvas);
var
  FontHeight: Integer;
  LabelRect: TRect;
  Flags: TFCheckboxFlags;
begin
  FontHeight := Canvas.FontCellHeight;
  LabelRect.Left := FLabelPos.x;
  LabelRect.Top := FLabelPos.y + (Height - MinSize.cy) div 2;
  LabelRect.Right := LabelRect.Left + Canvas.TextWidth(Text);
  LabelRect.Bottom := LabelRect.Top + FontHeight;

  Flags := [];
  if (wsClicked in WidgetState) and (wsMouseInside in WidgetState) then
    Include(Flags, cbIsPressed);
  if (wsHasFocus in WidgetState) and FindForm.IsActive then
    Include(Flags, cbHasFocus);
  if wsEnabled in WidgetState then
    Include(Flags, cbIsEnabled);
  if Checked then
    Include(Flags, cbIsChecked);

  Style.DrawRadioButton(Canvas, Rect(0, 0, Width, Height), LabelRect, Flags);
  Canvas.SetColor(Style.GetUIColor(clWindowText));
  Style.DrawText(Canvas, LabelRect.TopLeft, Text, WidgetState);
end;

procedure TFCustomRadioButton.CalcSizes;
begin
  with FindForm.Wnd.Canvas do
    Style.GetRadioButtonLayout(gfxbase.Size(TextWidth(Text), FontCellHeight),
      FMinSize, FLabelPos);
end;

procedure TFCustomRadioButton.SetChecked(AChecked: Boolean);
var
  i: Integer;
  Child: TFWidget;
begin
  if AChecked <> Checked then
  begin
    FChecked := AChecked;
    Redraw;

    if Checked and Assigned(Parent) and
      Parent.InheritsFrom(TFContainerWidget) then
      for i := 0 to TFContainerWidget(Parent).ChildCount - 1 do
      begin
        Child := TFContainerWidget(Parent).Children[i];
	      if (Child <> Self) and Child.InheritsFrom(TFCustomRadioButton) then
	        TFCustomRadioButton(Child).Checked := False;
      end;
  end;
end;


{$ENDIF read_implementation}

