{
    fpGUI  -  Free Pascal GUI Library
    
    ComboBox class declarations
    
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

  { Combobox widget declarations }

  TFComboBoxPopup = class(TFPopupWindow)
  private
    FLayout: TFBoxLayout;
    FListBox: TFListBox;
  public
    constructor Create(AOwner: TComponent); override;
    property    ListBox: TFListBox read FListBox;
  end;
  

  TFCustomComboBox = class(TFWidget)
  private
    FItemIndex: Integer;
    FItems: TStrings;
    FOnChange: TNotifyEvent;
    procedure   ComboBoxButtonClick(Sender: TObject);
    procedure   DropDownDeactivate(Sender: TObject);
    procedure   DropDownDestroy(Sender: TObject);
    procedure   SetItemIndex(const AValue: Integer);
  protected
    ComboBoxButton: TFGenericButton;
    FDropDown: TFComboBoxPopup;
    lbl: TFLabel;
    procedure   Click; override;
    procedure   Paint(Canvas: TFCanvas); override;
    procedure   CalcSizes; override;
    procedure   Resized; override;
    function    DistributeEvent(Event: TEventObj): Boolean; override;
    property    CanExpandWidth default True;
//    property    DropDownCount: integer read FDropDownCount write FDropDownCount;
    property    ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Items: TStrings read FItems write FItems;
  end;


  TFComboBox = class(TFCustomComboBox)
  published
    property    CanExpandWidth;
    property    CanExpandHeight;
    property    Enabled;
    property    Text;
    property    ItemIndex;
    property    OnChange;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}


{ Combobox widget implementation }

type
  TFArrowButton = class(TFGenericButton)
  protected
    procedure Paint(Canvas: TFCanvas); override;
    procedure CalcSizes; override;
  end;


procedure TFArrowButton.Paint(Canvas: TFCanvas);
begin
  inherited Paint(Canvas);
  Style.DrawComboBoxArrow(Canvas, Rect(0, 0, Width, Height),
    (wsClicked in WidgetState) and (wsMouseInside in WidgetState),
    wsEnabled in WidgetState);
end;

procedure TFArrowButton.CalcSizes;
begin
  FMinSize := Style.GetComboBoxBtnSize;
end;

constructor TFComboBoxPopup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  BorderWidth := 1;
  Color       := clBlack;
  Name        := '_ComboBoxPopup';

  FLayout := TFBoxLayout.Create(self);
  FLayout.Name        := '_VBoxLayout';
  FLayout.Orientation := Vertical;
  FLayout.Spacing     := 0;
  InsertChild(FLayout);

  FListBox := TFListBox.Create(self);
  FListBox.Name       := '_Listbox';
  FListBox.HotTrack   := True;
  FLayout.InsertChild(FListBox);
end;


// -------------------------------------------------------------------
//   TCustomComboBox
// -------------------------------------------------------------------

constructor TFCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanExpandWidth := True;
  WidgetStyle     := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  FItems := TStringList.Create;
  FItemIndex := -1;

  ComboBoxButton := TFArrowButton.Create(Self);
  ComboBoxButton.Name             := '_ComboBoxButton';
  ComboBoxButton.Embedded         := True;
  ComboBoxButton.CanExpandWidth   := False;
  ComboBoxButton.CanExpandHeight  := False;
  ComboBoxButton.OnClick          := @ComboBoxButtonClick;
  ComboBoxButton.SetEmbeddedParent(Self);
end;

destructor TFCustomComboBox.Destroy;
begin
  FDropDown.Free;
  inherited Destroy;
end;

procedure TFCustomComboBox.Paint(Canvas: TFCanvas);
var
  Pt: TPoint;
  ItemRect: TRect;
  ItemFlags: TItemFlags;
  c: TFCanvas;
  r: TRect;
begin
  ItemFlags := [];
  Style.DrawEditBox(Canvas, Rect(0, 0, Width, Height));
  
  if Text <> '' then
  begin
    Style.SetUIColor(Canvas, clWindowText);
    Pt.x := 4;
    Pt.y := (BoundsSize.cy - Canvas.FontCellHeight) div 2;
    
    if (wsHasFocus in WidgetState) and FindForm.IsActive then
    begin
      Include(ItemFlags, ifFocused);
      Include(ItemFlags, ifSelected);
    end;

    ItemRect := Rect(0, 0, (Width - ComboBoxButton.Width), Height);
//    InflateRect(ItemRect, -1, -1);
    ItemRect.TopLeft := ItemRect.TopLeft + 1;
    ItemRect.BottomRight := ItemRect.BottomRight - 2;

    { Text must be clipped before reaching the button }
//    try
//      Canvas.SaveState;
//      writeln(Format('Canvas size Before %d:%d', [Canvas.Width, Canvas.Height]));
//      r := Canvas.Transform(ComboBoxButton.BoundsRect);
//      writeln(Format('  Bounding rectangle (%d:%d)x(%d:%d)', [BoundsRect.Top, BoundsRect.Left, BoundsRect.Bottom, BoundsRect.Right]));
//      writeln(Format('  Canvas rectangle (%d:%d)x(%d:%d)', [Canvas.GetClipRect.Top, Canvas.GetClipRect.Left, Canvas.GetClipRect.Bottom, Canvas.GetClipRect.Right]));
//      writeln(Format('  ComboButton rectangle (%d:%d)x(%d:%d)', [r.Top, r.Left, r.Bottom, r.Right]));
//      Canvas.IntersectClipRect(r);
//      writeln(Format('Canvas size After %d:%d', [Canvas.Width, Canvas.Height]));

      Style.DrawItemBefore(Canvas, ItemRect, ItemFlags);
      Style.DrawText(Canvas, Pt, Text, WidgetState);
      Style.DrawItemAfter(Canvas, ItemRect, ItemFlags);
//    finally
//      Canvas.RestoreState;
//    end;
  end
  else
  begin
    if (wsHasFocus in WidgetState) and FindForm.IsActive then
      Style.DrawFocusRect(Canvas, Rect(0, 0, Width, Height));
  end;
end;

procedure TFCustomComboBox.CalcSizes;
begin
  with Style.GetEditBoxBorders do
    FMinSize := Size(ComboBoxButton.MinSize.cx,
      Max(FindForm.Wnd.Canvas.FontCellHeight, ComboBoxButton.MinSize.cy)) +
      TopLeft + BottomRight;
end;

procedure TFCustomComboBox.Resized;
begin
  with Style.GetEditBoxBorders do
    ComboBoxButton.SetBounds(
      Point(Width - Right - ComboBoxButton.MinSize.cx, Top),
      ComboBoxButton.MinSize);
end;

function TFCustomComboBox.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := Event.SendToChild(ComboBoxButton);
//   or inherited DistributeEvent(Event);
end;

procedure TFCustomComboBox.ComboBoxButtonClick(Sender: TObject);
begin
  if Assigned(FDropDown) and FDropDown.Visible then
  begin
    FDropDown.Close;
    Exit; //==>
  end;
  
  if not Assigned(FDropDown) then
  begin
    FDropDown := TFComboBoxPopup.Create(Self);
    FDropDown.OnDestroy           := @DropDownDestroy;
    FDropDown.ListBox.Items.Text  := FItems.Text;
    FDropDown.ListBox.FItemIndex  := FItemIndex;
    FDropDown.ListBox.OnClick     := @DropDownDeactivate;
  end;

  FDropDown.SetPosition(ClientToScreen(Point(0, Height)));
  FDropDown.Show;
  FDropDown.Wnd.SetMinMaxClientSize(MaxSize, MaxSize);
end;

procedure TFCustomComboBox.DropDownDeactivate(Sender: TObject);
begin
  LAYOUTTRACE('TCustomComboBox.DropDownDestroy for %s:%s', [Name, ClassName]);
  ItemIndex := FDropDown.ListBox.ItemIndex;
  FDropDown.Close;
  SetFocus;
end;

procedure TFCustomComboBox.DropDownDestroy(Sender: TObject);
begin
  LAYOUTTRACE('TCustomComboBox.DropDownDestroy for %s:%s', [Name, ClassName]);
  FDropDown := nil;
end;

procedure TFCustomComboBox.SetItemIndex(const AValue: Integer);
begin
  if FItemIndex <> AValue then
  begin
    if AValue < FItems.Count then
      FItemIndex := AValue;
    if FItemIndex = -1 then
      Text := ''
    else
      Text := FItems[FItemIndex];

    // fire event
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

{ This event causes the combobox to drop open when you click anywhere in the
  component, or press the spacebar key. }
procedure TFCustomComboBox.Click;
begin
  ComboBoxButtonClick(nil);
  inherited Click;
end;


{$ENDIF read_implementation}

