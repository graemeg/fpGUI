{
    fpGUI  -  Free Pascal GUI Library

    Edit class declarations

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


  { TFCustomEdit }
  TBorderStyle = (bsNone, bsSingle);

  TFCustomEdit = class(TFWidget)
  private
    FFontColor: TColor;
    FReadOnly: Boolean;
    FSelStart: integer;
    FSelOffset: integer;
    FDrawOffset: integer;
    FCursorPos: Integer;
    FPasswordChar: Char;
    FOnChange: TNotifyEvent;
    FBorderStyle: TBorderStyle;
    procedure   SetFontColor(const AValue: TColor);
    procedure   SetPasswordChar(APasswordChar: Char);
    procedure   SetCursorPos(ACursorPos: Integer);
    procedure   SetBorderStyle(ABorderStyle: TBorderStyle);
    procedure   DoMousePressed(pEvent: TMousePressedEventObj);
    procedure   SetReadOnly(const AValue: Boolean);
    procedure   AdjustCursor;
    function    GetDrawText: string;
  protected
    procedure   Paint(Canvas: TFCanvas); override;
    function    ProcessEvent(Event: TEventObj): Boolean; override;
    procedure   CalcSizes; override;
    procedure   EvKeyPressed(Key: Word; Shift: TShiftState); override;
    procedure   EvKeyChar(KeyChar: Char); override;
    procedure   EvTextChanged; override;
    property    CanExpandWidth default True;
    property    Cursor default crIBeam;
    property    PasswordChar: Char read FPasswordChar write SetPasswordChar default #0;
    property    CursorPos: Integer read FCursorPos write SetCursorPos;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    property    FontColor: TColor read FFontColor write SetFontColor;
    property    BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property    ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    procedure   SetText(const AText: String); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(const pText: string; pOwner: TComponent); overload;
  end;


  TFEdit = class(TFCustomEdit)
  published
    property CanExpandWidth;
    property Enabled;
    property PasswordChar;
    property Text;
    property OnChange;
    property FontColor;
    property BorderStyle;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}

// ===================================================================
//   TFCustomEdit
// ===================================================================

constructor TFCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WidgetStyle     := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  FCanExpandWidth := True;
  FCursor         := crIBeam;
  FFontColor      := clWindowText;
  FCursorPos      := 0;
  FDrawOffset     := 0;
  FBorderStyle    := bsSingle;
  FReadOnly       := False;
end;

constructor TFCustomEdit.Create(const pText: string; pOwner: TComponent);
begin
  Create(pOwner);
  Text := pText;
end;

procedure TFCustomEdit.SetBorderStyle(ABorderStyle: TBorderStyle);
begin
  if FBorderStyle <> ABorderStyle then
  begin
    FBorderStyle := ABorderStyle;
    Redraw;
  end;
end;

procedure TFCustomEdit.Paint(Canvas: TFCanvas);
var
  Borders: TRect;
  s: string;
  c: TGfxColor;
  ItemRect: TRect;
  tw: integer;
  lSideMargins: integer;
begin
  Borders := Style.GetEditBoxBorders;
  lSideMargins := Borders.Left + Borders.Right;
  
  ItemRect := Rect(0, 0, BoundsSize.cx, BoundsSize.cy);
  case FBorderStyle of
    bsNone:
      begin
        c := Canvas.GetColor;
        Style.SetUIColor(Canvas, clWindow);
        Style.DrawWindowBackground(Canvas, ItemRect);
        Canvas.SetColor(c);
      end;
    bsSingle:
      Style.DrawEditBox(Canvas, ItemRect, ReadOnly);
  end;

  // setup the clip rectangle
  if not Canvas.IntersectClipRect(Rect(Borders.Left + 1, Borders.Top + 1,
    BoundsSize.cx - Borders.Right - 1, BoundsSize.cy - Borders.Bottom - 1)) then
    exit;

  // setup the correct font color
  if wsEnabled in WidgetState then
    Canvas.SetColor(Style.GetUIColor(FFontColor))
  else
    Canvas.SetColor(Style.GetUIColor(clGrayText));

  // paint the text
  s := GetDrawText;
  Canvas.TextOut(Point(-FDrawOffset + Borders.Left+2, 3), s);

  if wsHasFocus in WidgetState then
  begin
(*
    // drawing selection
    if (FSelOffset <> 0) then
    begin
      if (wsHasFocus in WidgetState) and FindForm.IsActive then
      begin
        Include(ItemFlags, ifFocused);
        Include(ItemFlags, ifSelected);
      end;

      ItemRect.Left := Canvas.TextWidth(Copy(s, 1, CursorPos - FSelOffset));
      ItemRect.Top := 0;
      ItemRect.Right := Canvas.TextWidth(Copy(s, 1, CursorPos));
      ItemRect.Bottom := Height;

      Style.DrawItemBefore(Canvas, ItemRect, ItemFlags);
      Style.DrawText(Canvas, (Borders.TopLeft + Point(1, 1)), s, WidgetState);
      Style.DrawItemAfter(Canvas, ItemRect, ItemFlags);
    end;
*)

    // drawing cursor
    Canvas.SetColor(Style.GetUIColor(clWindowText));
//    Style.SetUIColor(Canvas, clWindowText);
    tw := Canvas.TextWidth(UTF8Copy(s, 1, FCursorPos));

    // 2 pixel cursor line
//    ItemRect.Top    := Borders.Top;
//    ItemRect.Left   := -FDrawOffset + lSideMargins + tw;
//    ItemRect.Bottom := BoundsSize.cy - Borders.Bottom;
//    ItemRect.Right  := ItemRect.Left + 2;
//    Canvas.FillRect(ItemRect);

    // 1 pixel cursor line
    Canvas.DrawLine(
        Point(-FDrawOffset + lSideMargins + tw, Borders.Top),
        Point(-FDrawOffset + lSideMargins + tw, BoundsSize.cy - Borders.Bottom));
  end;
end;

function TFCustomEdit.ProcessEvent(Event: TEventObj): Boolean;
begin
  if Event.InheritsFrom(TMousePressedEventObj) then
  begin
    DoMousePressed(TMousePressedEventObj(Event));
  end;
  Result := inherited ProcessEvent(Event);
end;

procedure TFCustomEdit.EvKeyPressed(Key: Word; Shift: TShiftState);
begin
  if Shift * [ssShift, ssAlt, ssCtrl, ssMeta, ssSuper, ssHyper, ssAltGr] = [] then
  begin
    // Normal typing - no selections
    case Key of
      keyLeft, keyUp:
        if CursorPos > 0 then
	        CursorPos := CursorPos - 1;
      keyRight, keyDown:
	      if CursorPos < UTF8Length(FText) then
	        CursorPos := CursorPos + 1;
      keyHome:
        CursorPos := 0;
      keyEnd:
        CursorPos := UTF8Length(FText);
      else
        inherited EvKeyPressed(Key, Shift);
    end;
  end
{  else if Shift * [ssShift, ssAlt, ssCtrl, ssMeta, ssSuper, ssHyper, ssAltGr] = [ssShift] then
  begin
    Writeln('2');
    case Key of
      keyHome:
        begin
          FSelOffset := CursorPos;
          CursorPos := 0;
        end;
      keyEnd:
        begin
          FSelOffset := CursorPos;
          CursorPos := Length(Text);
        end;
      else
        EvKeyPressed(Key, Shift);
    end;
  end
}
  else
  begin
    inherited EvKeyPressed(Key, Shift);
  end;
  AdjustCursor;
  Redraw;
end;


procedure TFCustomEdit.EvKeyChar(KeyChar: Char);
begin
  case KeyChar of
    #8:      { Backspace }
      if FCursorPos > 0 then
      begin
        FText := UTF8Copy(FText, 1, FCursorPos - 1) + UTF8Copy(FText, FCursorPos + 1, UTF8Length(FText));
        FCursorPos := FCursorPos - 1;
      end;
    #127:    { Del }
      if FCursorPos < UTF8Length(FText) then
      begin
        FText := UTF8Copy(FText, 1, FCursorPos) + UTF8Copy(FText, FCursorPos + 2, UTF8Length(FText));
        Redraw;
      end;
    #32..#126, #128..#255:
      begin
        FText := UTF8Copy(FText, 1, FCursorPos) + KeyChar + UTF8Copy(FText, CursorPos + 1, UTF8Length(FText));
        FCursorPos := FCursorPos + 1;
      end;
    else
      inherited EvKeyChar(KeyChar);
  end;
  AdjustCursor;
end;

procedure TFCustomEdit.CalcSizes;
var
  Borders: TRect;
begin
  Borders := Style.GetEditBoxBorders;
  FMinSize := Size(50, Borders.Top + Borders.Bottom +
    FindForm.Wnd.Canvas.FontCellHeight + 2);
end;

procedure TFCustomEdit.EvTextChanged;
begin
  Redraw;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TFCustomEdit.SetText(const AText: String);
begin
  inherited SetText(AText);
  FSelOffset    := 0;
  FCursorPos    := UTF8Length(FText);
  FSelStart     := FCursorPos;
  FDrawOffset   := 0;
  AdjustCursor;
end;

procedure TFCustomEdit.SetPasswordChar(APasswordChar: Char);
begin
  if APasswordChar <> PasswordChar then
  begin
    FPasswordChar := APasswordChar;
    Redraw;
  end;
end;

procedure TFCustomEdit.SetFontColor(const AValue: TColor);
begin
  if FFontColor = AValue then exit;
  FFontColor := AValue;
end;

procedure TFCustomEdit.SetCursorPos(ACursorPos: Integer);
begin
  if ACursorPos <> CursorPos then
  begin
    FCursorPos := ACursorPos;
    Redraw;
  end;
end;

procedure TFCustomEdit.DoMousePressed(pEvent: TMousePressedEventObj);
var
  Borders: TRect;
  cp: integer;
  cpx: integer;
  lSideMargin: integer;
  n: integer;
  cx: integer;
  lText: string;
begin
  if (pEvent.Button = mbLeft) then
  begin
    // searching for the appropriate character position
    Borders     := Style.GetEditBoxBorders;
    lSideMargin := Borders.Left + 1;

    // Make sure we work with the correct displayed text
    lText := GetDrawText;

    cp    := FCursorPos;
    cpx   := FindForm.Wnd.Canvas.TextWidth(UTF8Copy(lText, 1, FCursorPos)) - FDrawOffset + lSideMargin;

    for n := 0 to UTF8Length(lText) do
    begin
      cx := FindForm.Wnd.Canvas.TextWidth(UTF8Copy(lText, 1, n)) - FDrawOffset + lSideMargin;
      if abs(cx - pEvent.Position.x) < abs(cpx - pEvent.Position.x) then
      begin
        cpx := cx;
        cp  := n;
      end;
    end;

    FCursorPos := cp;

    if (ssShift in pEvent.Shift) then
    begin
      FSelOffset := FCursorPos - FSelStart;
    end
    else
    begin
      FSelStart   := cp;
      FSelOffset  := 0;
    end;
  end;
end;

procedure TFCustomEdit.SetReadOnly(const AValue: Boolean);
begin
  if FReadOnly <> AValue then
  begin
    FReadOnly := AValue;
    Redraw;
  end;
end;

procedure TFCustomEdit.AdjustCursor;
var
  tw: integer;
  VisibleWidth: integer;
  Canvas: TFCustomCanvas;
  lBorders: TRect;
  lSideMargins: integer;
begin
  // This is not pretty and needs to change, but if these two tests are not
  // here it throws a AV when loading forms
  if not Assigned(FindForm) then
    Exit; //==>
  if not Assigned(FindForm.FWnd) then
    Exit; //==>

  Canvas := FindForm.Wnd.Canvas;
  tw := Canvas.TextWidth(UTF8Copy(GetDrawText, 1, FCursorPos));

  lBorders := Style.GetEditBoxBorders;
  lSideMargins := lBorders.Left + lBorders.Right;
  VisibleWidth := (Width - lSideMargins);

  if tw - FDrawOffset > VisibleWidth - 2 then
  begin
    FDrawOffset := tw - VisibleWidth + 2;
  end
  else if tw - FDrawOffset < 0 then
  begin
    FDrawOffset := tw;
    if tw <> 0 then
      dec(FDrawOffset, 2);
  end;
end;

// Return the correct text to be displayed
function TFCustomEdit.GetDrawText: string;
begin
  if FPasswordChar = #0 then
    Result := FText
  else
  begin
    Result := StringOfChar(FPasswordChar, UTF8Length(FText));
  end;
end;

{$ENDIF read_implementation}
