{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2015 by Graeme Geldenhuys.
    Copyright (c) 2015 by Andrew Haines.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      A hexadecimal control and a panel control that displays the current
      position as signed and unsigned 8, 16, 32, 64 bit integers, 8 bit binary
      8 bit octal, 32 and 64 bit floats.
}
  unit fpg_hexview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_widget, fpg_base, fpg_main, fpg_scrollbar, fpg_label, fpg_edit, fgl;

type
  TfpgHexView = class;

  IfpgHexEventListener = interface
    ['{257CE50F-22D4-4C75-A2A5-805972094CEE}']
    procedure HexCursorChanged(Sender: TfpgHexView; Data: QWord);
  end;

  IfpgHexView = interface
    ['{DB19EE55-080F-450D-A596-65F528C04A70}']
    procedure   AddEventListener(AListener: IfpgHexEventListener);
    procedure   RemoveEventListener(AListener: IfpgHexEventListener);
    procedure   MoveCursor(APos: Int64);
  end;

  { TfpgHexView }

  TfpgHexView = class(TfpgWidget, IfpgHexView)
  private
  const
    AddressSpace   = 2; // use an even number
    CharSpacing    = 5;
  type
    TEventList = specialize TFPGList<IfpgHexEventListener>;
  private
    FCursor: Int64;
    FOwnsStream: Boolean;
    FStream: TStream;
    FFont: TfpgFont;
    FVScroll: TfpgScrollBar;
    FEventListenerList: TEventList;
    function    GetFont: TfpgFont;
    procedure   SetCursor(AValue: Int64);
    procedure   SetFont(AValue: TfpgFont);
    procedure   SetStream(AValue: TStream);
    function    GetHexCharSize: TfpgSize;
    function    GetHexAreaWidth: Integer;
    function    GetCharsPerRow: Integer;
    function    GetRowsPerPage: Integer;
    function    GetAddressChars: Integer;
    function    GetAddressWidth: Integer;
    function    GetTextWidth: Integer;
    function    GetTextLeft: Integer;
    procedure   PaintAddressGutter(AStartAddress: QWord);
    procedure   PaintHexArea(AStartAddress: QWord);
    function    GetTextChar(B: Byte): Char;
    procedure   DoScroll(Sender: TObject; position: integer);
    procedure   UpdateScrollbar;
    procedure   MakeCursorVisible;
    procedure   DoCursorChange;
  protected
    procedure   HandlePaint; override;
    procedure   HandleResize(AWidth, AHeight: TfpgCoord); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    // IfpgHexView
    procedure   AddEventListener(AListener: IfpgHexEventListener);
    procedure   RemoveEventListener(AListener: IfpgHexEventListener);
    procedure   MoveCursor(APos: Int64);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Stream: TStream read FStream write SetStream;
    property    Font: TfpgFont read GetFont write SetFont; // Fixed width
    property    Cursor: Int64 read FCursor write SetCursor;
  published
    property    Align;
    property    TabOrder;
    property    OwnsStream: Boolean read FOwnsStream write FOwnsStream;
  end;

  { TfpgHexPanel }

  TfpgHexPanel = class(TfpgWidget, IfpgHexEventListener)
  private
  type
    TIntType = (bcS8, bcU8, bcS16, bcU16, bcS32, bcU32, bcS64, bcU64, bcBinary, bcOctal, bcFloat32, bcFloat64);
    function IntTypeToString(AType: TIntType): String;
    function GetValueString(AType: TIntType; const AValue): String;
  private
    FCurrentVal: QWord;
    FFont: TfpgFont;
    FHexView: IfpgHexView;
    FLabels: array[TIntType] of TfpgLabel;
    FEdits: array[TIntType] of TfpgEdit;
    FReverseEndian: Boolean;
    procedure SetHexView(AValue: IfpgHexView);
    procedure SetReverseEndian(AValue: Boolean);
  protected
    // IfpgHexEvent
    procedure HexCursorChanged(Sender: TfpgHexView; Data: QWord);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property    Align;
    property    TabOrder;
    property    ReverseEndian: Boolean read FReverseEndian write SetReverseEndian;
    property    HexView: IfpgHexView read FHexView write SetHexView;
  end;

implementation

{ TfpgHexPanel }

function TfpgHexPanel.IntTypeToString(AType: TIntType): String;
begin
  case AType of
    bcS8: Result := 'Signed 8 bit';
    bcU8: Result := 'Unsigned 8 bit';
    bcS16: Result := 'Signed 16 bit';
    bcU16: Result := 'Unsigned 16 bit';
    bcS32: Result := 'Signed 32 bit';
    bcU32: Result := 'Unsigned 32 bit';
    bcS64: Result := 'Signed 64 bit';
    bcU64: Result := 'Unsigned 64 bit';
    bcBinary: Result :=  'Binary';
    bcOctal: Result :=  'Octal';
    bcFloat32: Result := 'Float 32';
    bcFloat64: Result := 'Float 64';
  end;
  Result := Result + ':';
end;

function TfpgHexPanel.GetValueString(AType: TIntType; const AValue): String;
var
  SInt8: ShortInt absolute AValue;
  UInt8: Byte absolute AValue;
  SInt16: Smallint absolute AValue;
  UInt16: Word absolute AValue;
  SInt32: LongInt absolute AValue;
  UInt32: LongWord absolute AValue;
  SInt64: Int64 absolute AValue;
  UInt64: QWord absolute AValue;
  Float32: Single absolute AValue;
  Float64: Double absolute AValue;
begin
  try
    if not FReverseEndian then
    case AType of
      bcS8: Result := IntToStr(SInt8);
      bcU8: Result := IntToStr(UInt8);
      bcS16: Result := IntToStr(SInt16);
      bcU16: Result := IntToStr(UInt16);
      bcS32: Result := IntToStr(SInt32);
      bcU32: Result := IntToStr(UInt32);
      bcS64: Result := IntToStr(SInt64);
      bcU64: Result := IntToStr(UInt64);
      bcBinary: Result := binStr(UInt8, 8);
      bcOctal: Result:= OctStr(UInt8, 3);
      bcFloat32: Result := FloatToStr(Float32);
      bcFloat64: Result := FloatToStr(Float64);
    end
    else // swap endian
    case AType of
      bcS8: Result := IntToStr(SInt8);
      bcU8: Result := IntToStr(UInt8);
      bcS16: Result := IntToStr(swap(SInt16));
      bcU16: Result := IntToStr(swap(UInt16));
      bcS32: Result := IntToStr(swap(SInt32));
      bcU32: Result := IntToStr(swap(UInt32));
      bcS64: Result := IntToStr(swap(SInt64));
      bcU64: Result := IntToStr(swap(UInt64));
      bcBinary: Result := binStr(UInt8, 8);
      bcOctal: Result:= OctStr(UInt8, 3);
      bcFloat32: Result := FloatToStr(Single(swap(UInt32)));
      bcFloat64: Result := FloatToStr(Single(swap(UInt64)));
    end
  except
    Result := 'Err';
  end;
end;

procedure TfpgHexPanel.SetReverseEndian(AValue: Boolean);
begin
  if FReverseEndian=AValue then Exit;
  FReverseEndian:=AValue;
  HexCursorChanged(nil, FCurrentVal);
end;

procedure TfpgHexPanel.SetHexView(AValue: IfpgHexView);
begin
  if FHexView=AValue then Exit;

  if Assigned(FormDesigner) then
  begin
    FHexView:=AValue;
    Exit;
  end;

  if Assigned(FHexView) then
    FHexView.RemoveEventListener(Self as IfpgHexEventListener);

  FHexView:=AValue;
  if Assigned(FHexView) then
    FHexView.AddEventListener(Self as IfpgHexEventListener);
end;

procedure TfpgHexPanel.HexCursorChanged(Sender: TfpgHexView; Data: QWord);
var
  i: TIntType;
begin
  FCurrentVal := Data;
  for i := Low(TIntType) to High(TIntType) do
  begin
    FEdits[i].Text:=GetValueString(i, Data);
  end;
end;

constructor TfpgHexPanel.Create(AOwner: TComponent);
var
  i: TIntType;
  X, Y: Integer;
  LabelWidths: Integer;
  EditWidths: Integer;
begin
  inherited Create(AOwner);
  FWidth := 300;
  FHeight := 100;
  FFont := fpgGetFont('#Label1');

  LabelWidths := FFont.TextWidth(IntTypeToString(bcU64));

  X := 0;
  Y := 0;
  for i := Low(TIntType) to High(TIntType) do
  begin
    case Ord(i) div 4 of
      0: EditWidths := 50;
      1: EditWidths :=150;
      2: begin
           EditWidths := 150;
           LabelWidths:= 60;
         end;
    end;
    FLabels[i] := TfpgLabel.Create(Self);
    FLabels[i].Text:=IntTypeToString(i);
    FLabels[i].Left:=X;
    FLabels[i].Top:=Y+5;
    FLabels[i].Width:= LabelWidths;
    FLabels[i].FontDesc:=FFont.FontDesc;
    FLabels[i].Alignment:=taRightJustify;

    FEdits[i] := TfpgEdit.Create(Self);
    FEdits[i].ReadOnly:=True;
    FEdits[i].Left:=FLabels[i].Left+FLabels[i].Width;
    FEdits[i].Top:=Y;
    FEdits[i].Width:= EditWidths;

    Inc(Y, FEdits[i].Height);

    if Ord(i) mod 4 = 3 then
    begin
      X := FEdits[i].Left+FEdits[i].Width;
      Y := 0;
    end;
  end;
  HexCursorChanged(nil, 0);
end;

destructor TfpgHexPanel.Destroy;
begin
  inherited Destroy;
  FFont.Free;
end;

{ TfpgHexView }

procedure TfpgHexView.SetStream(AValue: TStream);
begin
  if FStream=AValue then Exit;

  if Assigned(FStream) and FOwnsStream then
    FStream.Free;
  FStream:=AValue;
  FCursor:=0;

  if Assigned(FStream) then
    MakeCursorVisible;

  if not (csDestroying in ComponentState) then
  begin
    UpdateScrollbar;
    Invalidate;
  end;

  DoCursorChange;
end;

procedure TfpgHexView.DoScroll(Sender: TObject; position: integer);
begin
  Invalidate;
end;

function TfpgHexView.GetFont: TfpgFont;
begin
  if FFont = nil then
    FFont := fpgGetFont('Courier New-12');

  Result := FFont;
end;

procedure TfpgHexView.SetCursor(AValue: Int64);
var
  Data: QWord = 0;
begin
  if AValue < 0 then
    Avalue := 0;

  if FCursor=AValue then Exit;

  if not Assigned(FStream) then
    AValue := 0
  else if AValue > FStream.Size-1 then
    AValue := FStream.Size-1;
  FCursor:=AValue;
  // assume we have clicked so that is why the cursor changed
  MakeCursorVisible;
  Invalidate;

  DoCursorChange;
end;

procedure TfpgHexView.SetFont(AValue: TfpgFont);
begin
  if FFont <> AValue then
    FFont := AValue;
end;

function TfpgHexView.GetHexCharSize: TfpgSize;
begin
  Result.H := Font.Height;
  Result.W := Font.TextWidth('AZ');
end;

function TfpgHexView.GetHexAreaWidth: Integer;
begin
  Result := (GetHexCharSize.W + CharSpacing) * GetCharsPerRow - CharSpacing;
end;

function TfpgHexView.GetCharsPerRow: Integer;
var
  CSize: TfpgSize;
  BevelsWidth: Integer;
  BordersWidth: Integer;
  W: Integer;
  CharWidth: Integer;
begin

  CSize := GetHexCharSize;
  BevelsWidth:=fpgStyle.GetBevelWidth * 4;
  BordersWidth:=15; // 5 outer borders and 5 between hex and char
  W := Width - GetAddressWidth - BevelsWidth - BordersWidth;
  Result := W div (CSize.W + CharSpacing);

  CharWidth:=Font.TextWidth('X');

  while (Result * (CSize.W + CharSpacing)) + (Result * CharWidth) > W do
    Dec(Result);
end;

function TfpgHexView.GetRowsPerPage: Integer;
var
  H: Integer;
begin
  // H is not affected by a scrollbar. We only scroll up and down
  H := Height - fpgStyle.GetBevelWidth * 2;
  Result := H div Font.Height;
end;

function TfpgHexView.GetAddressChars: Integer;
var
  S: QWord;
begin
  Result := 1;
  if Assigned(FStream) then
  begin
    S := FStream.Size;
    while S > $F do
    begin
      // each nibble is a hex char
      S := S shr 4;
      Inc(Result);
    end;
  end;
end;

function TfpgHexView.GetAddressWidth: Integer;
begin
  Result := GetAddressChars * Font.TextWidth('X')+AddressSpace;
end;

function TfpgHexView.GetTextWidth: Integer;
begin
  Result := GetCharsPerRow * Font.TextWidth('X');
end;

function TfpgHexView.GetTextLeft: Integer;
begin
  Result := Width - fpgStyle.GetBevelWidth - FVScroll.Width - GetTextWidth;
end;

procedure TfpgHexView.PaintAddressGutter(AStartAddress: QWord);
var
  X, Y: Integer;
  CharsPerRow: Integer;
  AddressWidth: Integer;
  Address: String;
  AddressChars: Integer;
  CharWidth: Integer;
  Pos: QWord;
begin
  Y := fpgStyle.GetBevelWidth;
  CharsPerRow:=GetCharsPerRow;
  Pos := AStartAddress;
  CharWidth:=Font.TextWidth('X');
  AddressChars:=GetAddressChars;

  Canvas.Color:=clDarkGray;
  Canvas.FillRectangle(fpgStyle.GetBevelWidth,fpgStyle.GetBevelWidth, GetAddressWidth, Height - (fpgStyle.GetBevelWidth*2));

  if not Assigned(FStream) then
    Exit; // ==>

  while (Y < Height) and (Pos <= FStream.Size) do
  begin
    Address := hexStr(Pos, AddressChars);
    AddressWidth := Font.TextWidth(Address);
    X := AddressChars * CharWidth - AddressWidth + (AddressSpace div 2);
    Canvas.DrawText(X,Y, Address);

    Inc(Y, Font.Height);
    Inc(Pos, CharsPerRow);
  end;
end;

procedure TfpgHexView.PaintHexArea(AStartAddress: QWord);
var
  X, Y: Integer;
  CharSize: TfpgSize;
  CharWidth: Integer;
  RCount: Integer;
  B: Byte = 0;
  HexLeft: Integer;
  PaintTop: Integer;
  TextLeft: Integer;
  CharPos: TfpgRect;
  TextPos: TfpgRect;
begin
  CharSize := GetHexCharSize;
  CharWidth:=Font.TextWidth('X');

  // get some useful values
  PaintTop := fpgStyle.GetBevelWidth;
  HexLeft := GetAddressWidth+fpgStyle.GetBevelWidth;
  TextLeft:=GetTextLeft;

  // fill background
  Canvas.Color:=clLightGray;
  Canvas.FillRectangle(HexLeft,fpgStyle.GetBevelWidth, Width-HexLeft, Height - (fpgStyle.GetBevelWidth*2));

  // draw line separator
  Canvas.Color:=clDarkGray;
  Canvas.DrawLine(GetTextLeft-5, fpgStyle.GetBevelWidth, GetTextLeft-5, Height - fpgStyle.GetBevelWidth * 2);

  if not Assigned(FStream) then
    Exit; // ==>

  FStream.Position:=AStartAddress;

  for Y := 0 to GetRowsPerPage do
  begin
    for X := 0 to GetCharsPerRow-1 do
    begin
      if (Y * CharSize.H) + (PaintTop * 2) > Height then
        Break;
      RCount:=FStream.Read(B, 1);
      if RCount = 0 then
        Exit;

      CharPos := fpgRect(HexLeft+(x*(CharSize.W+CharSpacing)),PaintTop+y*CharSize.H, CharSize.W, CharSize.H);
      TextPos:=  fpgRect(TextLeft+x*CharWidth,PaintTop+y*CharSize.H, CharWidth, CharSize.H);

      if FStream.Position-1 = FCursor then
      begin
        Canvas.Color:=clRed;
        //Draw box around hex and text
        Canvas.DrawRectangle(CharPos);
        Canvas.DrawRectangle(TextPos);
      end;
      // Draw HexChar
      Canvas.DrawText(CharPos.Left, CharPos.Top, HexStr(B, 2));
      // draw text char
      Canvas.DrawText(TextPos.Left, CharPos.Top, GetTextChar(B));
    end;
  end;
end;

function TfpgHexView.GetTextChar(B: Byte): Char;
begin
  if B in [$20..$7E] then
    Result := Char(B)
  else
    Result := '.';
end;

procedure TfpgHexView.UpdateScrollbar;
var
  Rows: Integer;
begin
  if Assigned(FStream) then
  begin
    Rows := FStream.Size div GetCharsPerRow + 1;
    FVScroll.Max:=Rows-GetRowsPerPage;
  end
  else
    FVScroll.Max:=0;

  FVScroll.SliderSize := FVScroll.Height / (FVScroll.Max + FVScroll.Height);
end;

procedure TfpgHexView.MakeCursorVisible;
var
  FirstVisible: Int64;
  LastVisible: Int64;
  Row: Integer;
begin
  FirstVisible := FVScroll.Position * GetCharsPerRow;
  LastVisible  := FirstVisible + GetRowsPerPage * GetCharsPerRow;
  if (Cursor >= FirstVisible) and (Cursor <= LastVisible) then
    Exit;

  Row := Cursor div GetCharsPerRow;
  if Row < FVScroll.Position then
    FVScroll.Position:=Row
  else
    FVScroll.Position:=Row+1-GetRowsPerPage;
  FVScroll.RepaintSlider;
end;

procedure TfpgHexView.DoCursorChange;
var
  I: IfpgHexEventListener;
  Data: QWord;
begin
  if Assigned(FStream) then
  begin
    FStream.Position:=FCursor;
    FStream.Read(Data, 8);
  end
  else
    Data := 0;

  if not (csDestroying in ComponentState) then
    for I in FEventListenerList do
      I.HexCursorChanged(Self, Data);
end;

procedure TfpgHexView.HandlePaint;
var
  StartRow: Integer;
begin
  UpdateScrollbar;
  fpgStyle.DrawBevel(Canvas,0,0,Width,Height, False);
  Canvas.SetClipRect(fpgRect(0,0,Width-fpgStyle.GetBevelWidth*2, Height-fpgStyle.GetBevelWidth*2));
  Canvas.Font := Font;

  StartRow:= FVScroll.Position;
  PaintAddressGutter(StartRow*GetCharsPerRow);
  PaintHexArea(StartRow*GetCharsPerRow);

end;

procedure TfpgHexView.HandleResize(AWidth, AHeight: TfpgCoord);
var
  BS: Integer;
begin
  inherited HandleResize(AWidth, AHeight);
  BS := fpgStyle.GetBevelWidth;
  FVScroll.SetPosition(Width-FVScroll.Width-BS, BS, FVScroll.Width, Height-BS*2);
end;

procedure TfpgHexView.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  Row: Integer;
  Col: Integer;
  NewPos: Int64;
  CharSize: TfpgSize;
  BS: Integer;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  BS := fpgStyle.GetBevelWidth;

  // see if we clicked in a valid spot
  if (X < GetAddressWidth + BS) or (X >= FVScroll.Left) or (Y <= BS) or (Y > Height- BS) then
    Exit;

  if X > GetTextLeft then
  begin
    // we clicked in the text area
    Row := (y - BS) div Font.Height + FVScroll.Position;
    Col := (x - GetTextLeft) div Font.TextWidth('X');
    NewPos := Row * GetCharsPerRow + Col;
    Cursor := NewPos;
  end
  else
  begin
    // we clicked in the hex area
    CharSize := GetHexCharSize;
    Row := (y - BS) div CharSize.H + FVScroll.Position;
    Col := (x - BS - GetAddressWidth) div (CharSize.W + CharSpacing);
    if Col > GetCharsPerRow-1 then
      Col := GetCharsPerRow-1;
    NewPos := Row * GetCharsPerRow + Col;

    Cursor := NewPos;
  end;
end;

procedure TfpgHexView.HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint);
begin
  // casting is safe since it's a virtual function. This is to access HandleMouseScroll.
  TfpgHexView(FVScroll).HandleMouseScroll(x, y, shiftstate, delta);
end;

procedure TfpgHexView.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);

  case keycode of
    keyUp: Cursor := Cursor - GetCharsPerRow;
    keyDown: Cursor := Cursor + GetCharsPerRow;
    keyLeft:  if ssCtrl in shiftstate then
                Cursor := Cursor - 4
              else
                Cursor := Cursor - 1;
    keyRight: if ssCtrl in shiftstate then
                Cursor := Cursor + 4
              else
                Cursor := Cursor + 1;
    keyPageDown: Cursor := Cursor + GetCharsPerRow * (GetRowsPerPage -1);
    keyPageUp: Cursor := Cursor - GetCharsPerRow * (GetRowsPerPage -1);
    keyHome: if ssCtrl in shiftstate then
               Cursor := 0
             else
               Cursor := (Cursor div GetCharsPerRow) * GetCharsPerRow;
    keyEnd:  if ssCtrl in shiftstate then
               Cursor := FStream.Size-1
             else
               Cursor := (Cursor div GetCharsPerRow) * GetCharsPerRow + GetCharsPerRow-1;
  end;
  Invalidate;
end;

constructor TfpgHexView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth := 400;
  FHeight := 300;
  FVScroll := TfpgScrollBar.Create(Self);
  FVScroll.Orientation:=orVertical;
  FVScroll.OnScroll:=@DoScroll;
  Focusable:=True;
  FEventListenerList:=TEventList.Create;
end;

destructor TfpgHexView.Destroy;
begin
  FEventListenerList.Clear;
  Stream := nil; // Frees the stream if OnwsStream is true
  FreeAndNil(FEventListenerList);
  if Assigned(FFont) then
    FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TfpgHexView.AddEventListener(AListener: IfpgHexEventListener);
begin
  if FEventListenerList.IndexOf(AListener) = -1 then
    FEventListenerList.Add(AListener);
end;

procedure TfpgHexView.RemoveEventListener(AListener: IfpgHexEventListener);
var
  Index: LongInt;
begin
   Index := FEventListenerList.IndexOf(AListener);
   if Index <> -1 then
      FEventListenerList.Delete(Index);
end;

procedure TfpgHexView.MoveCursor(APos: Int64);
begin
  Cursor:=APos;
end;


end.

