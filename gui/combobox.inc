{
    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther
    Copyright (C) 2006 by Graeme Geldenhuys 
      member of the fpGUI development team.

    Combobox class declarations

    See the file COPYING.fpGUI, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{%mainunit fpgui.pp}

{$IFDEF read_interface}

  { Combobox widget declarations }

  { TCustomComboBox }
  
  TComboBoxPopup = class(TPopupWindow)
  private
    FListBox: TListBox;
    procedure   ItemSelected(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property    ListBox: TListBox read FListBox;
  end;
  

  TCustomComboBox = class(TWidget)
  private
    FItemIndex: Integer;
    FItems: TStrings;
    FOnChange: TNotifyEvent;
    procedure   ButtonClick(Sender: TObject);
    procedure   DropDownDeactivate(Sender: TObject);
    procedure   DropDownDestroy(Sender: TObject);
    procedure   SetItemIndex(const AValue: Integer);
  protected
    FButton: TGenericButton;
//    FDropDown: TCustomForm;
    FDropDown: TComboBoxPopup;
    lbl: TLabel;
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


  TComboBox = class(TCustomComboBox)
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
  TArrowButton = class(TGenericButton)
  protected
    procedure Paint(Canvas: TFCanvas); override;
    procedure CalcSizes; override;
  end;


procedure TArrowButton.Paint(Canvas: TFCanvas);
begin
  inherited Paint(Canvas);
  Style.DrawComboBoxArrow(Canvas, Rect(0, 0, Width, Height),
    (wsClicked in WidgetState) and (wsMouseInside in WidgetState),
    wsEnabled in WidgetState);
end;


procedure TArrowButton.CalcSizes;
begin
  FMinSize := Style.GetComboBoxArrowSize;
end;


procedure TComboBoxPopup.ItemSelected(Sender: TObject);
begin
  Close;
end;


constructor TComboBoxPopup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];

  FListBox := TListBox.Create(self);
  FListBox.Name       := 'listbox';
  FListBox.Parent     := self;
  FListBox.HotTrack   := True;
//  FListBox.OnClick    := @ItemSelected;   // Listbox needs OnSelect event
end;


// -------------------------------------------------------------------
//   TCustomComboBox
// -------------------------------------------------------------------

constructor TCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanExpandWidth := True;
  WidgetStyle     := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  FItems := TStringList.Create;
  FItemIndex := -1;

  FButton := TArrowButton.Create(Self);
  FButton.Name := 'FButton';
  FButton.Embedded        := True;
  FButton.CanExpandWidth  := False;
  FButton.OnClick         := @ButtonClick;
  FButton.Parent          := Self;
end;


destructor TCustomComboBox.Destroy;
begin
  FDropDown.Free;
  inherited Destroy;
end;


procedure TCustomComboBox.Paint(Canvas: TFCanvas);
var
  Pt: TPoint;
  ItemRect: TRect;
  ItemFlags: TItemFlags;
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

    ItemRect := Rect(0, 0, (Width - FButton.Width), Height);
//    InflateRect(ItemRect, -1, -1);
    ItemRect.TopLeft := ItemRect.TopLeft + 1;
    ItemRect.BottomRight := ItemRect.BottomRight - 2;

    Style.DrawItemBefore(Canvas, ItemRect, ItemFlags);
    Style.DrawText(Canvas, Pt, Text, WidgetState);
    Style.DrawItemAfter(Canvas, ItemRect, ItemFlags);
  end
  else
  begin
    if (wsHasFocus in WidgetState) and FindForm.IsActive then
      Style.DrawFocusRect(Canvas, Rect(0, 0, Width, Height));
  end;
end;


procedure TCustomComboBox.CalcSizes;
begin
  with Style.GetEditBoxBorders do
    FMinSize := gfxBase.Size(FButton.MinSize.cx,
      Max(FindForm.Wnd.Canvas.FontCellHeight, FButton.MinSize.cy)) +
      TopLeft + BottomRight;
end;


procedure TCustomComboBox.Resized;
begin
  with Style.GetEditBoxBorders do
    FButton.SetBounds(Point(Width - Right - FButton.MinSize.cx, Top),
      FButton.MinSize);
end;


function TCustomComboBox.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := Event.SendToChild(FButton) or
    inherited DistributeEvent(Event);
end;


procedure TCustomComboBox.ButtonClick(Sender: TObject);
var
  l: TCustomListBox;
begin
  if Assigned(FDropDown) and FDropDown.Visible then
  begin
    FDropDown.Close;
    Exit; //==>
  end;
  
  if not Assigned(FDropDown) then
  begin
    FDropDown := TComboBoxPopup.Create(Self);
    FDropDown.OnDeactivate        := @DropDownDeactivate;
    FDropDown.OnDestroy           := @DropDownDestroy;
    FDropDown.ListBox.Items.Text  := FItems.Text;
    FDropDown.ListBox.FItemIndex  := FItemIndex;
    FDropDown.ListBox.OnClick     := @DropDownDeactivate;
  end;

  FDropDown.SetPosition(ClientToScreen(Point(0, Height)));
  FDropDown.Show;
end;


procedure TCustomComboBox.DropDownDeactivate(Sender: TObject);
begin
  LAYOUTTRACE('TCustomComboBox.DropDownDestroy for %s:%s', [Name, ClassName]);
  ItemIndex := FDropDown.ListBox.ItemIndex;
  FDropDown.Close;
  SetFocus;
end;


procedure TCustomComboBox.DropDownDestroy(Sender: TObject);
begin
  LAYOUTTRACE('TCustomComboBox.DropDownDestroy for %s:%s', [Name, ClassName]);
  FDropDown := nil;
end;


procedure TCustomComboBox.SetItemIndex(const AValue: Integer);
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
procedure TCustomComboBox.Click;
begin
  ButtonClick(nil);
  inherited Click;
end;


{$ENDIF read_implementation}

