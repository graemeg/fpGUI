{
    fpGUI  -  Free Pascal GUI Library

    Edit class declarations

    Copyright (C) 2000 - 2007 See the file AUTHORS.txt, included in this
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
    FOldVisibleIndex: Integer;
    FFontColor: TColor;
    FSelStart: integer;
    FSelOffset: integer;
    FCursorPos: Integer;
    FPasswordChar: Char;
    FOnChange: TNotifyEvent;
    FBorderStyle: TBorderStyle;
    procedure   SetFontColor(const AValue: TColor);
    procedure   SetPasswordChar(APasswordChar: Char);
    procedure   SetCursorPos(ACursorPos: Integer);
    procedure   SetBorderStyle(ABorderStyle: TBorderStyle);
    procedure   DoMousePressed(pEvent: TMousePressedEventObj);
    function    GetFirstVisibleIndex(AText: string): Integer;
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
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];
  FCanExpandWidth := True;
  FCursor := crIBeam;
  FFontColor := clWindowText;
  FCursorPos := 0;
  FBorderStyle := bsSingle;
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
  x, y: Integer;
  ItemRect: TRect;
  ItemFlags: TItemFlags;
begin
  ItemFlags := [];
  Borders := Style.GetEditBoxBorders;

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
      Style.DrawEditBox(Canvas, ItemRect);
  end;


  if not Canvas.IntersectClipRect(Rect(Borders.Left + 1, Borders.Top + 1,
    BoundsSize.cx - Borders.Right - 1, BoundsSize.cy - Borders.Bottom - 1)) then
    exit;

  if wsEnabled in WidgetState then
    Canvas.SetColor(Style.GetUIColor(FFontColor))
  else
    Canvas.SetColor(Style.GetUIColor(clGrayText));

  // If PasswordChar is set we need to use different text
  if PasswordChar = #0 then
    s := PChar(Text)
  else
  begin
    SetLength(s, Length(Text));
    if Length(Text) > 0 then
      FillChar(s[1], Length(Text), PasswordChar);
  end;


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
  end
  else
  begin
    x := 1;
    y := GetFirstVisibleIndex(s);
    {$ifdef debug}
    writeln(y);
    {$endif debug}
    while (x <= Length(Text)) and (Canvas.TextWidth(Copy(s, y, x)) < Width - 8) do
      Inc(x);
    s := Copy(s, y, x);
    {$ifdef debug}
    writeln(s);
    writeln(FCursorPos, ':', y);
    {$endif debug}
    Canvas.TextOut(Borders.TopLeft + Point(1, 1), s);
  end;

  // cursor
  if wsHasFocus in WidgetState then
  begin
    Canvas.SetColor(Style.GetUIColor(clWindowText));
    x := Canvas.TextWidth(Copy(s, 1, CursorPos-y+1));
    if CursorPos = 0 then
      x := 0;
    x := Borders.Left + 1 + x;
    Canvas.DrawLine(Point(x, Borders.Top), Point(x, BoundsSize.cy - Borders.Bottom));
  end;
end;

function TFCustomEdit.ProcessEvent(Event: TEventObj): Boolean;
begin
  if Event.InheritsFrom(TMousePressedEventObj) then
  begin
//    Result := True;
    DoMousePressed(TMousePressedEventObj(Event));
//    Writeln('>>> TMousePressedEventObj in ' + Classname);
  end;
  Result := inherited ProcessEvent(Event);
end;

procedure TFCustomEdit.EvKeyPressed(Key: Word; Shift: TShiftState);
begin
  if Shift * [ssShift, ssAlt, ssCtrl, ssMeta, ssSuper, ssHyper, ssAltGr] = [] then
  begin
//    Writeln('1');
    case Key of
      keyLeft, keyUp:
        if CursorPos > 0 then
	        CursorPos := CursorPos - 1;
      keyRight, keyDown:
	      if CursorPos < Length(Text) then
	        CursorPos := CursorPos + 1;
      keyHome:
        CursorPos := 0;
      keyEnd:
        CursorPos := Length(Text);
      else
        inherited EvKeyPressed(Key, Shift);
    end;
    Redraw;
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
    inherited EvKeyPressed(Key, Shift);
end;


procedure TFCustomEdit.EvKeyChar(KeyChar: Char);
begin
  case KeyChar of
    #8:      { Backspace }
      if CursorPos > 0 then
      begin
        Text := Copy(Text, 1, CursorPos - 1) + Copy(Text, CursorPos + 1, Length(Text));
        CursorPos := CursorPos - 1;
      end;
    #127:    { Del }
      if CursorPos < Length(Text) then
      begin
        Text := Copy(Text, 1, CursorPos) + Copy(Text, CursorPos + 2, Length(Text));
        Redraw;
      end;
    #32..#126, #128..#255:
      begin
        Text := Copy(Text, 1, CursorPos) + KeyChar + Copy(Text, CursorPos + 1, Length(Text));
        CursorPos := CursorPos + 1;
      end;
    else
      inherited EvKeyChar(KeyChar);
  end;
end;


procedure TFCustomEdit.CalcSizes;
var
  Borders: TRect;
begin
  Borders := Style.GetEditBoxBorders;
  FMinSize := gfxbase.Size(50, Borders.Top + Borders.Bottom +
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
  FSelOffset := 0;
  inherited SetText(AText);
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

    // replicates pStrValue a set of pRepCount times.
    function lReplicate(const pStrValue : string; pRepCount : Word) : string;
    var
      pResult, pValue: PChar;
      lenValue: cardinal;
    begin
      if (pRepCount = 0) or (Pointer(pStrValue) = nil) then
        exit;

      lenValue  := Length(pStrValue);
      SetLength(Result, lenValue * pRepCount);
      pResult   := Pointer(Result);
      pValue    := Pointer(pStrValue);

      while pRepCount <> 0 do
      begin
        Move(pValue^, pResult^, lenValue);
        Inc(pResult, lenValue);
        Dec(pRepCount);
      end;
    end;

begin
  if (pEvent.Button = mbLeft) then
  begin
    // searching for the appropriate character position
    Borders := Style.GetEditBoxBorders;
    lSideMargin := Borders.Left + 1;
    cp := CursorPos;

    // Make sure we work with the correct displayed text
    if FPasswordChar = #0 then
      lText := Text
    else
      lText := lReplicate(FPasswordChar, Length(Text));

    cpx := FindForm.Wnd.Canvas.TextWidth(Copy(lText, 1, CursorPos)) + lSideMargin;

    for n := FOldVisibleIndex to Length(Text) do
    begin
      cx := FindForm.Wnd.Canvas.TextWidth(Copy(lText, FOldVisibleIndex, n-FOldVisibleIndex+1)) + lSideMargin;
      if abs(cx - pEvent.Position.x) < abs(cpx - pEvent.Position.x) then
      begin
        cpx := cx;
        cp := n;
      end;
    end;

    {cp := cp + FOldVisibleIndex - 1;

    if cp < 0 then
      cp := 0
    else
    if cp > Length(Text) then
      cp := Length(Text);}

    FCursorPos := cp;

    if (ssShift in pEvent.Shift) then
    begin
      FSelOffset := FCursorPos - FSelStart;
    end
    else
    begin
      FSelStart := cp;
      FSelOffset := 0;
    end;
  end;
end;

function TFCustomEdit.GetFirstVisibleIndex(AText: string): Integer;
var
  Canvas: TFCustomCanvas;

  function GetFirstPosition(maxlength: Integer): Integer;
  begin
    if Canvas.TextWidth(Copy(AText, 1, maxlength)) < Width-8 then
      Result := 1
    else
    begin
      Result := maxlength;
      while (Result > 1) and (Canvas.TextWidth(Copy(AText,
        Result-1, maxlength-Result+2)) < Width-8) do
        Dec(Result);
    end;
  end;

begin
  Canvas := FindForm.Wnd.Canvas;
  Result := GetFirstPosition(Length(AText));
  if Result > FCursorPos then
    Result := FCursorPos;
  FOldVisibleIndex := Result;
end;
{$ENDIF read_implementation}
