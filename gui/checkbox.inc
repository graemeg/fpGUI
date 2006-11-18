{
    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther
    Copyright (C) 2006 by Graeme Geldenhuys 
      member of the fpGUI development team.

    Checkbox class declarations

    See the file COPYING.fpGUI, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{%mainunit fpgui.pp}

{$IFDEF read_interface}

  { TCustomCheckbox }

  TCustomCheckbox = class(TWidget)
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


  TCheckbox = class(TCustomCheckbox)
  published
    property    Enabled;
    property    CanExpandHeight;
    property    CanExpandWidth;
    property    Checked;
    property    Text;
    property    OnClick;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}

// ===================================================================
//   TCustomCheckBox
// ===================================================================

constructor TCustomCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
end;


constructor TCustomCheckbox.Create(const pText: string; pOwner: TComponent);
begin
  Create(pOwner);
  Text := pText;
end;


procedure TCustomCheckBox.Click;
begin
  FChecked := not FChecked;
  inherited Click;
end;


procedure TCustomCheckBox.Paint(Canvas: TFCanvas);
var
  FontHeight: Integer;
  LabelRect: TRect;
  Flags: TCheckboxFlags;
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

  Style.DrawCheckbox(Canvas, Rect(0, 0, Width, Height), LabelRect, Flags);
  Canvas.SetColor(Style.GetUIColor(clWindowText));
  Style.DrawText(Canvas, LabelRect.TopLeft, Text, WidgetState);
end;


procedure TCustomCheckBox.CalcSizes;
begin
  with FindForm.Wnd.Canvas do
    Style.GetCheckBoxLayout(gfxbase.Size(TextWidth(Text), FontCellHeight),
      FMinSize, FLabelPos);
end;


procedure TCustomCheckBox.SetChecked(AChecked: Boolean);
begin
  if AChecked <> Checked then
  begin
    FChecked := AChecked;
    Redraw;
  end;
end;


{$ENDIF read_implementation}

