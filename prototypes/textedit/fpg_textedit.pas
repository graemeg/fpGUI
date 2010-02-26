{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      A new Memo component. It's actually more a TextEdit or MulitLineEdit
      component because it has a lot more features than simply a Memo. Features
      include: gutter, line numbers in gutter, right edge margin, syntax
      highlighting, much more optimised etc...
}

unit fpg_textedit;

{$mode objfpc}{$H+}
{$Define gDEBUG}

interface

uses
  Classes, SysUtils, fpg_base, fpg_main, fpg_widget,
  fpg_scrollbar;

type
  // forward declaration
  TfpgBaseTextEdit = class;

  TfpgGutter = class(TfpgWidget)
  private
    FOwner: TfpgBaseTextEdit; // convenience reference variable
    FDigits: Integer;
    FShowNum: Boolean;
    FSpace: Integer;
    FStartNum: Integer;
    FZeroStart: Boolean;
    procedure   SetDigits(const AValue: Integer);
    procedure   SetShowNum(const AValue: Boolean);
    procedure   SetSpace(const AValue: Integer);
    procedure   SetStartNum(const AValue: Integer);
    procedure   DrawLineNums;
    procedure   SetZeroStart(const AValue: Boolean);
  protected
    procedure   HandlePaint; override;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
  public
    constructor CreateGutter(AOwner: TfpgBaseTextEdit);
    function    GetClientRect: TfpgRect; override;
    property    LeadingDigits: Integer read FDigits write SetDigits default 0;
    property    ShowNum: Boolean read FShowNum write SetShowNum default True;
    property    Space: Integer read FSpace write SetSpace default 2;
    property    StartNum: Integer read FStartNum write SetStartNum default 1;
    property    Width default 35;
    property    ZeroStart: Boolean read FZeroStart write SetZeroStart default False;
  end;


  TfpgBaseTextEdit = class(TfpgWidget)
  private
    FFont: TfpgFont;
    FFullRedraw: Boolean;
    FLines: TStrings;
    CaretPos: TPoint;
    FScrollBarStyle: TfpgScrollStyle;
    MousePos: TPoint;
    FChrW: Integer;
    FChrH: Integer;
    FTopLine: Integer;
    FVisLines: Integer;
    FVisCols: Integer;
    StartNo, EndNo, StartOffs, EndOffs: Integer;
    // Selection start and end line number
    FSelStartNo, FSelEndNo: Integer;
    // Selection start and end column
    FSelStartOffs, FSelEndOffs: Integer;
    FTabWidth: Integer;
    HPos, VPos, XSize, YSize: Integer;
    FMaxScrollH: Integer;
    FVScrollBar: TfpgScrollBar;
    FHScrollBar: TfpgScrollBar;
    FTracking: Boolean;
    FSelDrag: Boolean;
    FSelected, FSelMouseDwn: Boolean;
    FGutterPan: TfpgGutter;
    function    GetFontDesc: string;
    function    GetGutterShowLineNumbers: Boolean;
    function    GetGutterVisible: Boolean;
    function    GetHScrollPos: Integer;
    function    GetVScrollPos: Integer;
    procedure   SetFontDesc(const AValue: string);
    procedure   SetGutterShowLineNumbers(const AValue: Boolean);
    procedure   SetGutterVisible(const AValue: Boolean);
    procedure   SetHScrollPos(const AValue: Integer);
    procedure   SetLines(const AValue: TStrings);
    procedure   SetScrollBarStyle(const AValue: TfpgScrollStyle);
    procedure   SetTabWidth(const AValue: Integer);
    procedure   SetVScrollPos(const AValue: Integer);
    procedure   UpdateCharBounds;
    procedure   GetSelBounds(var AStartNo, AEndNo, AStartOffs, AEndOffs: Integer);
    procedure   UpdateScrollBars;
    procedure   VScrollBarMove(Sender: TObject; position: integer);
    procedure   HScrollBarMove(Sender: TObject; position: integer);
    procedure   SetVPos(p: Integer);
    procedure   SetHPos(p: Integer);
    procedure   UpdateScrollBarCoords;
    procedure   UpdateGutterCoords;
    procedure   KeyboardCaretNav(const ShiftState: TShiftState; const AKeyCode: Word);
    procedure   InitMemoObjects;
  protected
    { -- internal events -- }
    procedure   HandlePaint; override;
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: boolean); override;
    { -- local widget functions -- }
    procedure   DrawVisible; virtual;
    procedure   DrawLine(const I, Y: Integer); virtual;
    procedure   FormatLine(const I, X, Y: Integer);
    procedure   DrawCaret(const X, Y: Integer); virtual;
    { -- to be published --}
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    FullRedraw: Boolean read FFullRedraw write FFullRedraw default False;
    property    GutterVisible: Boolean read GetGutterVisible write SetGutterVisible default False;
    property    GutterShowLineNumbers: Boolean read GetGutterShowLineNumbers write SetGutterShowLineNumbers default True;
    property    Lines: TStrings read FLines write SetLines;
    property    ScrollBarStyle: TfpgScrollStyle read FScrollBarStyle write SetScrollBarStyle default ssAutoBoth;
    property    TabWidth: Integer read FTabWidth write SetTabWidth default 8;
    property    Tracking: Boolean read FTracking write FTracking default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    GetClientRect: TfpgRect; override;
    function    GetWordAtPos(const X, Y: Integer; out XBegin: Integer): TfpgString;
    procedure   GetRowColAtPos(const X, Y: Integer; out Row, Col: Integer);
    procedure   Clear;
    procedure   ScrollTo(X, Y: Integer);
    procedure   DeleteSelection;
    procedure   SaveToFile(const AFileName: TfpgString);
    procedure   LoadFromFile(const AFileName: TfpgString);
    property    ScrollPos_H: Integer read GetHScrollPos write SetHScrollPos;
    property    ScrollPos_V: Integer read GetVScrollPos write SetVScrollPos;
    property    TopLine: Integer read FTopLine;
    property    VisibleLines: Integer read FVisLines;
  end;


  TfpgTextEdit = class(TfpgBaseTextEdit)
  published
    property    FontDesc;
    property    FullRedraw;
    property    GutterVisible;
    property    GutterShowLineNumbers;
    property    Lines;
    property    ScrollBarStyle;
    property    TabWidth;
    property    Tracking;
  end;


implementation

uses
  fpg_dialogs{, fpg_constants}, fpg_stringutils, fpg_utils;


function GetNextWord(SLine: TfpgString; var PosX: Integer): Boolean;
const
  ValidChars = ['a'..'z', 'A'..'Z', '0'..'9', '#'];
var
  I, RetX: Integer;
  FindNext: Boolean;
  c: TfpgChar;
begin
  Result := False;
  if PosX > UTF8Length(SLine) then Exit;
  FindNext := False;
  RetX := 0;
  for I := PosX to UTF8Length(SLine) do
  begin
    c := fpgCharAt(SLine, I);
    { TODO -cUnicode Error : We need to fix c[i] usage. Also improve ValidChars definition. }
    if not FindNext and not (c[1] in ValidChars) then
    begin
      FindNext := True;
      Continue;
    end;
    if FindNext and (c[1] in ValidChars) then
    begin
      RetX := I;
      Result := True;
      Break;
    end;
  end;
  if RetX < 1 then
    Result := False;
  PosX := RetX;
end;


{ TfpgGutter }

procedure TfpgGutter.SetDigits(const AValue: Integer);
begin
  if FDigits=AValue then exit;
  FDigits:=AValue;
end;

procedure TfpgGutter.SetShowNum(const AValue: Boolean);
begin
  if FShowNum=AValue then exit;
  FShowNum:=AValue;
  Invalidate;
end;

procedure TfpgGutter.SetSpace(const AValue: Integer);
begin
  if FSpace=AValue then exit;
  FSpace:=AValue;
end;

procedure TfpgGutter.SetStartNum(const AValue: Integer);
begin
  if FStartNum=AValue then exit;
  FStartNum:=AValue;
end;

procedure TfpgGutter.DrawLineNums;
var
  r: TfpgRect;
  I, MaxI, W, H, ZeroL: Integer;
  s: TfpgString;
  ltxtflags: TFTextFlags;
begin
  if not FShowNum then
    Exit; //==>
  w         := GetClientRect.Width - FSpace - 1;
  H         := FOwner.FChrH;
  MaxI      := FOwner.FVisLines;
  ltxtflags := [txtRight, txtVCenter];
  Canvas.SetFont(FOwner.FFont);
  r.SetRect(2, 0, W, H);

  for i := 0 to MaxI do
  begin
//    writeln('i=', i);
    if FZeroStart then
      S := IntToStr(FStartNum + i - 1)
    else
      S := IntToStr(FStartNum + i);
    for ZeroL := Length(S) to FDigits do
      S := '0' + S;
    r.Top := i * h;
    Canvas.DrawText(r, S, ltxtflags);
  end;
end;

procedure TfpgGutter.SetZeroStart(const AValue: Boolean);
begin
  if FZeroStart=AValue then exit;
  FZeroStart:=AValue;
end;

procedure TfpgGutter.HandlePaint;
begin
  inherited HandlePaint;
  Canvas.Clear(clWindowBackground);
  // Gutter right border
  Canvas.SetColor(clHilite2);
  Canvas.DrawLine(Width - 2, 0, Width - 2, Height - 1);
  Canvas.SetColor(clShadow1);
  Canvas.DrawLine(Width - 1, 0, Width - 1, Height - 1);
  DrawLineNums;
end;

procedure TfpgGutter.HandleMouseScroll(x, y: integer; shiftstate: TShiftState;
    delta: smallint);
var
  msg: TfpgMessageParams;
begin
  inherited HandleMouseScroll(x, y, shiftstate, delta);
  fillchar(msg, sizeof(msg), 0);  // zero out the record - initialize it
  msg.mouse.x := x;
  msg.mouse.y := y;
  msg.mouse.shiftstate := shiftstate;
  msg.mouse.delta := delta;
  fpgPostMessage(self, FOwner.FVScrollBar, FPGM_SCROLL, msg);
end;

constructor TfpgGutter.CreateGutter(AOwner: TfpgBaseTextEdit);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  FDigits := 0;
  FShowNum := True;
  FSpace := 2;
  FStartNum := 1;
  FZeroStart := False;
  Width := 35;
end;

function TfpgGutter.GetClientRect: TfpgRect;
begin
  Result := inherited GetClientRect;
  Result.Width := Result.Width - 2; // border right line takes up two pixels
end;

{ TfpgBaseTextEdit }

procedure TfpgBaseTextEdit.SetLines(const AValue: TStrings);
begin
  FLines.Assign(AValue);
  Invalidate;
end;

procedure TfpgBaseTextEdit.SetScrollBarStyle(const AValue: TfpgScrollStyle);
begin
  if FScrollBarStyle = AValue then
    Exit; //==>
  FScrollBarStyle := AValue;
  UpdateScrollBarCoords;
end;

function TfpgBaseTextEdit.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

function TfpgBaseTextEdit.GetGutterShowLineNumbers: Boolean;
begin
 Result := FGutterPan.ShowNum;
end;

function TfpgBaseTextEdit.GetGutterVisible: Boolean;
begin
  Result := FGutterPan.Visible;
end;

function TfpgBaseTextEdit.GetHScrollPos: Integer;
begin
  Result := HPos;
end;

function TfpgBaseTextEdit.GetVScrollPos: Integer;
begin
  Result := VPos;
end;

procedure TfpgBaseTextEdit.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  Invalidate;
end;

procedure TfpgBaseTextEdit.SetGutterShowLineNumbers(const AValue: Boolean);
begin
  FGutterPan.ShowNum := AValue;
end;

procedure TfpgBaseTextEdit.SetGutterVisible(const AValue: Boolean);
begin
  FGutterPan.Visible := AValue;
  Invalidate;
end;

procedure TfpgBaseTextEdit.SetHScrollPos(const AValue: Integer);
begin
  SetHPos(AValue);
end;

procedure TfpgBaseTextEdit.SetTabWidth(const AValue: Integer);
begin
  if AValue < 1 then
  begin
    { todo: add these to resourcestring section }
    if csDesigning in ComponentState then
      TfpgMessageDialog.Information(ClassName + ' Tip', 'Value for TabWidth must be greater than 0.');
    Exit; //==>
  end;
  FTabWidth := AValue;
end;

procedure TfpgBaseTextEdit.SetVScrollPos(const AValue: Integer);
begin
  SetVPos(AValue);
end;

procedure TfpgBaseTextEdit.UpdateCharBounds;
begin
  FChrW := FFont.TextWidth('W');
  FChrH := FFont.Height;
  FVisLines := (GetClientRect.Height div FChrH) + 1;
  if FGutterPan.Visible then
    FVisCols := (GetClientRect.Width - FGutterPan.Width) div FChrW
  else
    FVisCols := GetClientRect.Width div FChrW;
end;

procedure TfpgBaseTextEdit.GetSelBounds(var AStartNo, AEndNo, AStartOffs,
  AEndOffs: Integer);
begin
  if FSelStartNo <= FSelEndNo then
  begin
    AStartNo := FSelStartNo;
    AEndNo := FSelEndNo;
    if not ((AStartNo = AEndNo) and (FSelStartOffs > FSelEndOffs)) then
    begin
      AStartOffs := FSelStartOffs;
      AEndOffs := FSelEndOffs;
    end else
    begin
      AStartOffs := FSelEndOffs;
      AEndOffs := FSelStartOffs;
    end;
  end else
  begin
    AStartNo := FSelEndNo;
    AEndNo := FSelStartNo;
    AStartOffs := FSelEndOffs;
    AEndOffs := FSelStartOffs;
  end;
end;

procedure TfpgBaseTextEdit.UpdateScrollBars;
begin
  FVScrollBar.Min := 0;
  FVScrollBar.PageSize := FVisLines - 2;
  FVScrollBar.Max := FLines.Count +1 - FVisLines;  // +1 is so the last line is completely visible
  FVScrollBar.Position := VPos;
  FVScrollBar.Visible := FLines.Count > FVisLines;

  FHScrollBar.Min := 0;
  FHScrollBar.PageSize := FVisCols div 4; //FMaxScrollH div 4;
  FHScrollBar.Max := FMaxScrollH;// div 2;
  FHScrollBar.Position := HPos;
  FHScrollBar.Visible := FMaxScrollH > FVisCols;

  UpdateScrollBarCoords;
end;

procedure TfpgBaseTextEdit.VScrollBarMove(Sender: TObject; position: integer);
begin
  //if FDropList.Visible then
    //FDropList.Visible := False;
  //FDropTimeCount := 0;
  //FLastDropPos.x := -1;
  //FLastDropPos.y := -1;
  if FTracking then
    SetVPos(position);
    //case ScrollCode of
      //SB_LINEUP: SetVPos(VPos - 1);
      //SB_LINEDOWN: SetVPos(VPos + 1);
      //SB_PAGEUP: SetVPos(VPos - FVisLines);
      //SB_PAGEDOWN: SetVPos(VPos + FVisLines);
      //SB_THUMBPOSITION: SetVPos(Pos);
      //SB_THUMBTRACK: if FTracking then SetVPos(Pos);
      //SB_TOP: SetVPos(0);
      //SB_BOTTOM: SetVPos(YSize);
    //end;
end;

procedure TfpgBaseTextEdit.HScrollBarMove(Sender: TObject; position: integer);
begin
  //if FDropList.Visible then
    //FDropList.Visible := False;
  //FDropTimeCount := 0;
  //FLastDropPos.x := -1;
  //FLastDropPos.y := -1;

  if FTracking then
    SetHPos(position);

    //case ScrollCode of
      //SB_LINERIGHT: SetHPos(HPos + 1);
      //SB_LINELEFT: SetHPos(HPos - 1);
      //SB_PAGEUP: SetHPos(HPos - FVisLines);
      //SB_PAGEDOWN: SetHPos(HPos + FVisLines);
      //SB_THUMBPOSITION: SetHPos(Pos);
      //SB_THUMBTRACK: if FTracking then SetHPos(Pos);
      //SB_TOP: SetHPos(0);
      //SB_BOTTOM: SetHPos(XSize);
    //end;
end;

procedure TfpgBaseTextEdit.SetVPos(p: Integer);
var
  OldPos: Integer;
//  R: TfpgRect;
begin
  OldPos := VPos;
  VPos := p;
  UpdateScrollBars;
  {$IFDEF gDEBUG}
  writeln('OldPos:', OldPos, '  NewPos:', VPos, ' SB.Max:', FVScrollBar.Max);
  {$ENDIF}

//  FVScrollBar.Position := VPos;

//  R := GetClientRect;
  if OldPos - VPos <> 0 then
  begin
    { todo: implement scrolling children }
//    ScrollChildren(0, (OldPos - VPos) * FChrH);
    FTopLine := VPos;

    if FFullRedraw then
      Invalidate
    else
      if (FTopLine + (FVisLines-1)) <= FLines.Count then
        Invalidate;
    { TODO : Implement scrolling events }
    //if Assigned(FOnScrolled_V) then
      //FOnScrolled_V(Self);
    //if Assigned(FOnTextScrolled) then
        //FOnTextScrolled(Self, FTopLine, FTopLine + FVisLines + 1,HPos, FMaxScrollH);
  end;
end;

procedure TfpgBaseTextEdit.SetHPos(p: Integer);
var
  OldPos: Integer;
//  R: TfpgRect;
begin
  OldPos := HPos;
  HPos := p;
  UpdateScrollBars;
  {$IFDEF gDEBUG}
  writeln('OldPos:', OldPos, '  NewPos:', HPos, ' SB.Max:', FHScrollBar.Max);
  {$ENDIF}

//  R := GetClientRect;
  if OldPos - HPos <> 0 then
  begin
    { TODO : Implemente scrolling children }
//    ScrollChildren((OldPos - HPos), 0);
    //if FFullRedraw then
      Invalidate;
    //else
      //DrawVisible;
    { TODO : Implement scrolling events }
    //if Assigned(FOnScrolled_H) then
      //FOnScrolled_H(Self);
    //if Assigned(FOnTextScrolled) then
      //FOnTextScrolled(Self, FTopLine, FTopLine + FVisLines, HPos, FMaxScrollH);
  end;
end;

procedure TfpgBaseTextEdit.UpdateScrollBarCoords;
var
  HWidth: integer;
  VHeight: integer;
  r: TfpgRect;
begin
  r := GetClientRect;
  VHeight := r.Height;
  HWidth  := r.Width;

  //if FVScrollBar.Visible then
    //Dec(HWidth, FVScrollBar.Width);
  //if FHScrollBar.Visible then
    //Dec(VHeight, FHScrollBar.Height);

  FHScrollBar.Top     := Height - FHScrollBar.Height - r.Top;
  FHScrollBar.Left    := r.Top;
  FHScrollBar.Width   := HWidth;

  FVScrollBar.Top     := r.Top;
  FVScrollBar.Left    := Width - FVScrollBar.Width - r.Top;
  FVScrollBar.Height  := VHeight;

  FVScrollBar.UpdateWindowPosition;
  FHScrollBar.UpdateWindowPosition;
end;

procedure TfpgBaseTextEdit.UpdateGutterCoords;
var
  r: TfpgRect;
begin
  r := GetClientRect;
  if FGutterPan.Visible then
    FGutterPan.SetPosition(r.Left, r.Top, FGutterPan.Width, r.Height);
end;

{ This procedure is used to set caret position on keyboard navigation and
  to set selection if Shift key is pressed. }
procedure TfpgBaseTextEdit.KeyboardCaretNav(const ShiftState: TShiftState; const AKeyCode: Word);

  procedure CtrlKeyLeftKey;
  var
    S: TfpgString;
    XB: Integer;
  begin
    S := GetWordAtPos(CaretPos.X, CaretPos.Y, XB);
    if (S <> '') and (XB > -1) then
    begin
      if FSelected then
        FSelEndOffs := XB;
      CaretPos.X := XB;
    end
    else
    begin
      if FSelected then
        FSelEndOffs := 0;
      CaretPos.X := 0;
    end;
  end;

  procedure CtrlKeyRightKey;
  var
    S: TfpgString;
    I: Integer;
    NotFindIt: Boolean;
  begin
    if CaretPos.Y <= pred(FLines.Count) then
    begin
      NotFindIt := True;
      while NotFindIt do
      begin
        S := FLines[CaretPos.Y];
        I := CaretPos.X;
        if GetNextWord(S, I) then
        begin
          CaretPos.X := I - 1;
          NotFindIt := False;
        end
        else
        begin
          CaretPos.Y := CaretPos.Y + 1;
          CaretPos.X := 0;
          NotFindIt := False;
        end;
        if CaretPos.Y > pred(FLines.Count) then
        begin
          NotFindIt := False;
          CaretPos.X := 0;
        end;
      end;
    end
    else
      CaretPos.X := 0;
  end;

begin
  writeln('DEBUG:  TfpgBaseTextEdit.KeyboardCaretNav >>');
  case AKeyCode of
    keyLeft:
        begin
          CaretPos.X := CaretPos.X - 1;
          if CaretPos.X < 0 then
          begin
            if CaretPos.Y > 0 then
            begin
              if CaretPos.Y <= pred(FLines.Count) then
              begin
                if (ssCtrl in ShiftState) and (CaretPos.Y > 0) then
                begin
                  CaretPos.Y := CaretPos.Y - 1;
                  CaretPos.X := Length(FLines[CaretPos.Y]);
                  if FSelected then
                  begin
                    FSelEndNo := CaretPos.Y;
                    FSelEndOffs := CaretPos.X;
                  end;
                  Exit;
                end;
              end;
              CaretPos.Y := CaretPos.Y - 1;
              CaretPos.X := Length(FLines[CaretPos.Y]);
            end
            else
            begin
              CaretPos.X := 0;
            end;
          end;
          if ssShift in ShiftState then
          begin
            if not FSelected then
            begin
              if CaretPos.Y <= pred(FLines.Count) then
                if CaretPos.X > Length(FLines[CaretPos.Y]) then
                  CaretPos.Y := Length(FLines[CaretPos.Y]) - 1;
              FSelected := True;
              FSelStartNo := CaretPos.Y;
              FSelStartOffs := CaretPos.X + 1;
              FSelEndNo := CaretPos.Y;
              if ssCtrl in ShiftState then
                CtrlKeyLeftKey
              else
                FSelEndOffs := CaretPos.X;
            end
            else
            begin
              FSelEndNo := CaretPos.Y;
              if ssCtrl in ShiftState then
                CtrlKeyLeftKey
              else
                FSelEndOffs := CaretPos.X;
              if FSelEndNo <= pred(FLines.Count) then
              begin
                if FSelEndOffs > Length(FLines[FSelEndNo]) then
                begin
                  FSelEndOffs := Length(FLines[FSelEndNo]) - 1;
                  CaretPos.X := FSelEndOffs;
                end;
              end
              else
              begin
                FSelEndOffs := 0;
                CaretPos.X := 0;
              end;
            end;
            FSelected := (FSelStartNo <> FSelEndNo) or (FSelStartOffs <> FSelEndOffs);
            Exit;
          end;
          if FSelected then
          begin
            FSelected := False;
          end;
          if ssCtrl in ShiftState then
          begin
            CtrlKeyLeftKey;
          end;
          FSelStartNo := CaretPos.Y;
          FSelStartOffs := CaretPos.X;
        end;

    keyRight:
        begin
          CaretPos.X := CaretPos.X + 1;
          if CaretPos.X > FMaxScrollH then
          begin
            FMaxScrollH := FMaxScrollH + 2;
            UpdateScrollBars;
          end;
          if ssShift in ShiftState then
          begin
            if not FSelected then
            begin
              FSelected := True;
              FSelStartNo := CaretPos.Y;
              FSelStartOffs := CaretPos.X - 1;
              if ssCtrl in ShiftState then
                CtrlKeyRightKey;
              FSelEndNo := CaretPos.Y;
              FSelEndOffs := CaretPos.X;
            end
            else
            begin
              if ssCtrl in ShiftState then
                CtrlKeyRightKey;
              FSelEndNo := CaretPos.Y;
              FSelEndOffs := CaretPos.X;
            end;
            FSelected := (FSelStartNo <> FSelEndNo) or (FSelStartOffs <> FSelEndOffs);
            Exit;
          end;
          if FSelected then
          begin
            FSelected := False;
          end;
          if ssCtrl in ShiftState then
          begin
            CtrlKeyRightKey;
          end;
          FSelStartNo := CaretPos.Y;
          FSelStartOffs := CaretPos.X;
        end;

    keyUp:
        begin
          if CaretPos.Y = 0 then
            Exit;
          if not (ssShift in ShiftState) and not (ssCtrl in ShiftState) then
          begin
            CaretPos.Y := CaretPos.Y - 1;
            if FSelected then
            begin
              FSelected := False;
              Exit;
            end;
            FSelStartNo := CaretPos.Y;
            Exit;
          end
          else if (ssCtrl in ShiftState) and not (ssShift in ShiftState) then
          begin
            CaretPos.Y := CaretPos.Y - 1;
            {$Note This does not work. The view must scroll and caret pos stay the same. }
            VScrollBarMove(self, FVScrollBar.Position-1);
            FSelStartNo := CaretPos.Y;
            Exit;
          end
          else if not (ssCtrl in ShiftState) and (ssShift in ShiftState) then
          begin
            CaretPos.Y := CaretPos.Y - 1;
            if not FSelected then
            begin
              FSelStartNo := CaretPos.Y + 1;
              FSelStartOffs := CaretPos.X;
              FSelEndNo := CaretPos.X;
              FSelEndOffs := CaretPos.X;
              FSelected := True;
            end
            else
            begin
              FSelEndNo := CaretPos.Y;
              FSelEndOffs := CaretPos.X;
              FSelected := (FSelStartNo <> FSelEndNo) or (FSelStartOffs <> FSelEndOffs);
            end;
          end;
        end;

    keyDown:
        begin
          if CaretPos.Y >= FLines.Count then
            Exit;
          if not (ssShift in ShiftState) and not (ssCtrl in ShiftState) then
          begin
            CaretPos.Y := CaretPos.Y + 1;
            if FSelected then
            begin
              FSelected := False;
              Exit;
            end;
            FSelStartNo := CaretPos.Y;
            Exit;
          end
          else if (ssCtrl in ShiftState) and not (ssShift in ShiftState) then
          begin
            CaretPos.Y := CaretPos.Y + 1;
            {$Note This does not work. The view must scroll and caret pos stay the same. }
            VScrollBarMove(self, FVScrollBar.Position+1);
            FSelStartNo := CaretPos.Y;
            Exit;
          end
          else if not (ssCtrl in ShiftState) and (ssShift in ShiftState) then
          begin
            CaretPos.Y := CaretPos.Y + 1;
            if not FSelected then
            begin
              FSelStartNo   := CaretPos.Y - 1;
              FSelStartOffs := CaretPos.X;
              FSelEndNo     := CaretPos.Y;
              FSelEndOffs   := CaretPos.X;
              FSelected     := True;
            end
            else
            begin
              FSelEndNo     := CaretPos.Y;
              FSelEndOffs   := CaretPos.X;
              FSelected     := (FSelStartNo <> FSelEndNo) or (FSelStartOffs <> FSelEndOffs);
            end;
          end;
        end;

    keyHome:
        begin
        end;

    keyEnd:
        begin
        end;

    keyPrior, keyNext:
        begin
        end;
  end;
  writeln('DEBUG:  TfpgBaseTextEdit.KeyboardCaretNav <<');
end;

procedure TfpgBaseTextEdit.InitMemoObjects;
begin
  FGutterPan := TfpgGutter.CreateGutter(Self);
  with FGutterPan do
  begin
    Left    := -Width - 1;
    Visible := False;
  end;
end;

procedure TfpgBaseTextEdit.HandlePaint;
begin
//  inherited HandlePaint;
  UpdateCharBounds;
  // normal house keeping
  Canvas.ClearClipRect;
  Canvas.Clear(clBoxColor);
  fpgStyle.DrawControlFrame(Canvas, 0, 0, Width, Height);
  Canvas.Font := FFont;
  Canvas.SetClipRect(GetClientRect);

  // do the actual drawing
  UpdateScrollBarCoords;
  UpdateGutterCoords;
  DrawVisible;
  DrawCaret(CaretPos.X, CaretPos.Y);
  Canvas.ClearClipRect;

  // The little square in the bottom right corner
  if FHScrollBar.Visible and FVScrollBar.Visible then
  begin
    Canvas.SetColor(clButtonFace);
    Canvas.FillRectangle(FHScrollBar.Left+FHScrollBar.Width,
                         FVScrollBar.Top+FVScrollBar.Height,
                         FVScrollBar.Width,
                         FHScrollBar.Height);
  end;
end;

procedure TfpgBaseTextEdit.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  MouseCursor := mcIBeam;
end;

procedure TfpgBaseTextEdit.HandleMouseExit;
begin
  inherited HandleMouseExit;
  MouseCursor := mcDefault;
end;

procedure TfpgBaseTextEdit.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  RNo: Integer;
  CNo: Integer;
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  if FGutterPan.Visible and (X <= FGutterPan.Width) then Exit;  //==>

  GetRowColAtPos(X + HPos * FChrW, Y + VPos * FChrH, RNo, CNo);
  CaretPos.X := CNo;
  CaretPos.Y := RNo;
  FSelDrag := False;
  if (RNo in [FSelStartNo..FSelEndNo]) or (RNo in [FSelEndNo..FSelStartNo]) then
  begin
    if (FSelStartNo = FSelEndNo) and ((CNo in [FSelStartOffs..FSelEndOffs]) or
      (CNo in [FSelEndOffs..FSelStartOffs])) then FSelDrag := True;
    if FSelStartNo <> FSelEndNo then
    begin
      FSelDrag := True;
      if (RNo = FSelStartNo) and (FSelStartNo < FSelEndNo) and (CNo < FSelStartOffs) then
        FSelDrag := False;
      if (RNo = FSelStartNo) and (FSelStartNo > FSelEndNo) and (CNo > FSelStartOffs) then
        FSelDrag := False;
      if (RNo = FSelEndNo) and (FSelStartNo < FSelEndNo) and (CNo > FSelStartOffs) then
        FSelDrag := False;
      if (RNo = FSelEndNo) and (FSelStartNo > FSelEndNo) and (CNo < FSelStartOffs) then
        FSelDrag := False;
    end;
  end;
  if FSelDrag then
  begin
writeln('  SelDrag is True!!!!');
//    Exit; //==>
  end;
  if not (ssShift in ShiftState) then
  begin
writeln(' shiftstate not detected');
    if FSelected then
    begin
      { Erase old selection, if any... }
      FSelected := False;
    end;
    FSelStartNo := RNo;
    FSelEndNo := FSelStartNo;
    FSelStartOffs := CNo;
    FSelEndOffs := FSelStartOffs;
//    FSelected := True;
    FSelMouseDwn := True;
  end
  else
  begin
    FSelEndNo := RNo;
    FSelEndOffs := CNo;
    FSelected := True;
  end;
  Invalidate;
end;

procedure TfpgBaseTextEdit.HandleMouseScroll(x, y: integer; shiftstate: TShiftState;
    delta: smallint);
var
  msg: TfpgMessageParams;
begin
  inherited HandleMouseScroll(x, y, shiftstate, delta);
  fillchar(msg, sizeof(msg), 0);  // zero out the record - initialize it
  msg.mouse.x := x;
  msg.mouse.y := y;
  msg.mouse.shiftstate := shiftstate;
  msg.mouse.delta := delta;
  fpgPostMessage(self, FVScrollBar, FPGM_SCROLL, msg);
end;

procedure TfpgBaseTextEdit.HandleKeyPress(var keycode: word;
    var shiftstate: TShiftState; var consumed: boolean);
var
  SLine: TfpgString;
  AddS: TfpgString;
  Y: Integer;
  X: Integer;
  CaretScroll: Boolean;
begin
  writeln('>> TfpgBaseTextEdit.HandleKeyPress: keycode:', keycode);
  writeln('  CaretPos X:', CaretPos.X, '  Y:', CaretPos.Y);
//  inherited HandleKeyPress(keycode, shiftstate, consumed);

  { Add lines as we go, so we can cursor past EOF. }
  { todo: This behaviour should be optional }
  if CaretPos.Y > pred(FLines.Count) then
  begin
    FLines.Add('');
    Exit; //==>
  end;
//  if (keycode = keyEscape) or (ssCtrl in ShiftState) then
//    Exit; //==>

  if FSelected then
  begin
    DeleteSelection;
    if keycode = keyBackSpace then
      Exit; //==>
  end;
  SLine := FLines[CaretPos.Y];

  case keycode of
    keyBackspace:
        begin
          if UTF8Length(SLine) >= CaretPos.X then
            X := CaretPos.X
          else
          begin
            X := UTF8Length(SLine);
            CaretPos.X := X;
          end;
          UTF8Delete(SLine, X, 1);
          FLines[CaretPos.Y] := SLine;
          CaretPos.X := CaretPos.X - 1;
          if CaretPos.X < 0 then
          begin
            if CaretPos.Y > 0 then
            begin
              AddS := FLines[CaretPos.Y];  { store any text from current line }
              FLines.Delete(CaretPos.Y);
              CaretPos.Y := CaretPos.Y - 1;
              CaretPos.X := UTF8Length(FLines[CaretPos.Y]); { reposition cursor }
              if AddS <> '' then
                FLines[CaretPos.Y] := FLines[CaretPos.Y] + AddS; { add stored text to new current line }
            end
            else
            begin
              CaretPos.X := 0;
            end;
          end;
          FSelStartNo := CaretPos.Y;
          FSelStartOffs := CaretPos.X;
        end;

    keyTab:
        begin
        end;

    keyReturn:
        begin
          AddS := '';
          if UTF8Length(SLine) > CaretPos.X then
          begin
            AddS := Copy(SLine, CaretPos.X + 1, Length(SLine) - CaretPos.X + 1);
            Delete(SLine, CaretPos.X + 1, Length(SLine) - CaretPos.X);
            FLines[CaretPos.Y] := SLine;
          end;
          if CaretPos.Y = pred(FLines.Count) then
            FLines.Add(AddS)
          else
            if CaretPos.Y < pred(FLines.Count) then
              FLines.Insert(CaretPos.Y + 1, AddS)
            else
              if CaretPos.Y > FLines.Count then
                FLines.Add(''); { ??? }
          CaretPos.Y := CaretPos.Y + 1;
          CaretPos.X := 0;
          FSelStartNo := CaretPos.Y;
          FSelStartOffs := CaretPos.X;
        end;

    keyLeft, keyRight, keyUp, keyDown, keyHome, keyEnd, keyPrior, keyNext:
        begin
          KeyboardCaretNav(ShiftState, keycode);
          CaretScroll := True;
        end;
  end;

  if CaretPos.X > HPos + FVisCols then
    ScrollPos_H := CaretPos.X - FVisCols
  else if CaretPos.X < HPos then
    ScrollPos_H := CaretPos.X;

  if CaretPos.Y < FTopLine then
    ScrollPos_V := CaretPos.Y
  else if CaretPos.Y > FTopLine + FVisLines - 2 then
    ScrollPos_V := CaretPos.Y - FVisLines + 2;

  Invalidate;
  writeln('<< TfpgBaseTextEdit.HandleKeyPress');
end;

procedure TfpgBaseTextEdit.HandleKeyChar(var AText: TfpgChar;
  var shiftstate: TShiftState; var consumed: boolean);
var
  SLine: TfpgString;
  Fill: Integer;
begin
  writeln('>> TfpgBaseTextEdit.HandleKeyChar');
  if not consumed then
  begin
    // Handle only printable characters
    // UTF-8 characters beyond ANSI range are supposed to be printable
    if ((Ord(AText[1]) > 31) and (Ord(AText[1]) < 127)) or (Length(AText) > 1) then
    begin
      SLine := FLines[CaretPos.Y];

      { cursor was somewhere in whitespace, so we need to fill up the spaces }
      if UTF8Length(SLine) < CaretPos.y + 1 then
        for Fill := Length(SLine) to CaretPos.y + 1 do
          SLine := SLine + ' ';

      UTF8Insert(AText, SLine, CaretPos.X + 1);
      FLines[CaretPos.Y] := SLine;
      CaretPos.X := CaretPos.X + 1;
      FSelStartNo := CaretPos.Y;
      FSelStartOffs := CaretPos.X;
      consumed := True;
    end;
  end;

  if consumed then
    RePaint
  else
    inherited HandleKeyChar(AText, shiftstate, consumed);
  writeln('<< TfpgBaseTextEdit.HandleKeyChar');
end;

procedure TfpgBaseTextEdit.DrawVisible;
var
  I, Y, cntVis: Integer;
begin
  Y := 0;
  cntVis := 1;
  GetSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  for I := FTopLine to FTopLine + FVisLines do
  begin
    DrawLine(I, Y);
    Y := Y + FChrH;
    cntVis := cntVis + 1;
    if cntVis > FVisLines then
      Break;  //==>
  end;
  { todo: implement this }
{
  if FRightEdge then
  begin
    if not FGutterOpt.Visible then
    begin
      with Canvas do
      begin
        Pen.Color := FEnvironment.RightEdgeColor;
        MoveTo((FRightEdgeCol * FChrW) - (HPos * FChrW), 0);
        LineTo((FRightEdgeCol * FChrW) - (HPos * FChrW), ClientHeight);
      end;
    end else
      with Canvas do
      begin
        Pen.Color := FEnvironment.RightEdgeColor;
        MoveTo((FRightEdgeCol * FChrW) - (HPos * FChrW) + FGutterPan.Width, 0);
        LineTo((FRightEdgeCol * FChrW) - (HPos * FChrW) + FGutterPan.Width, ClientHeight);
      end;
  end;
}
end;

procedure TfpgBaseTextEdit.DrawLine(const I, Y: Integer);
var
  X: Integer;
  GSz: Integer;
begin
  if FGutterPan.Visible then
  begin
    GSz := FGutterPan.Width + GetClientRect.Left + 1;
    if FGutterPan.ShowNum and (FGutterPan.StartNum <> FTopLine + 1) then
    begin
      FGutterPan.StartNum := FTopLine + 1;
      FGutterPan.Invalidate;
    end;
  end
  else
    GSz := GetClientRect.Left + 1; // gutter size if no gutter panel

  if I < FLines.Count then
  begin
    X := -(HPos * FChrW) + GSz;
    FormatLine(I, X, Y);
  end;
end;

procedure TfpgBaseTextEdit.FormatLine(const I, X, Y: Integer);
var
  S, CorrectS, SS: TfpgString;
  TI, Si, Ei, T: Integer;
  R: TfpgRect;
  AllowDraw: Boolean;
begin
  if FLines.Count = 0 then
    Exit; //==>
  if (I < 0) or (I > Pred(FLines.Count)) then
    Exit; //==>
  S := FLines[I];
  if Pos(#9, S) > 0 then
  begin
    CorrectS := '';
    for TI := 1 to Length(S) do    // no need to use utf8 version here
    begin
      if S[TI] = #9 then
      begin
        for T := 1 to FTabWidth do
          CorrectS := CorrectS + ' ';
      end
      else
        CorrectS := CorrectS + S[TI];
    end;
    S := CorrectS;
  end; { if }

  { start drawing formatted text }
  R.SetRect(X, Y, UTF8Length(S) * FChrW, FChrH);
  AllowDraw := True;
//  if Assigned(FOnDrawLine) then FOnDrawLine(Self, S, I, LGliph, R, AllowDraw);
  { Draw simple text line... }
  if AllowDraw then
    Canvas.DrawText(R, S);
//    DrawText(LGliph.Canvas.Handle, PChar(S), Length(S), R, DT_DRAWLINE);

  { todo: Do other formatting here. }
  { todo: Do selection painting here. }
  if UTF8Length(S) > FMaxScrollH then
  begin
    FMaxScrollH := UTF8Length(S);
    UpdateScrollBars;
  end;
end;

procedure TfpgBaseTextEdit.DrawCaret(const X, Y: Integer);
var
  Xp, Yp: Integer;
begin
  if csDesigning in ComponentState then
    Exit; //==>

  {$IFDEF gDEBUG}
  writeln('X:', X, '  Y:', Y, '  FTopLine:', FTopLine, ' HPos:', HPos, '  VPos:', VPos);
  {$ENDIF}

  if (Y < FTopLine) or (Y > FTopLine + FVisLines) then
  begin
    fpgCaret.UnSetCaret(Canvas);
    Exit;  //==>
  end;
  Yp := ((Y - FTopLine) * FChrH) + 1;
  Xp := ((X - HPos) * FChrW) + GetClientRect.Left;

  if FGutterPan.Visible then
    Xp := Xp + FGutterPan.Width;
  if (Xp < 0) or (Xp > GetClientRect.Width) then
  begin
    fpgCaret.UnSetCaret(Canvas);
    Exit; //==>
  end;
  //with Canvas do
  //begin
    //if ShowCaret then
    //begin
      //Pen.Mode := pmNotMerge;
      //Pen.Color := Font.Color;
    //end else
    //begin
      //if not FSelected then
        //Pen.Color := Self.Color
      //else
        //Pen.Color := FEnvironment.SelectionBackground;
      //Pen.Mode := pmCopy;
    //end;
    //MoveTo(Xp, Yp);
    //LineTo(Xp, Yp + FChrH);
    //Pen.Mode := pmCopy;
  //end;
  if Focused then
    fpgCaret.SetCaret(Canvas, Xp, Yp, fpgCaret.Width, FFont.Height)
  else
    fpgCaret.UnSetCaret(Canvas);

  if not FSelected then
  begin
    FSelStartNo := CaretPos.x;
    FSelStartOffs := CaretPos.y;
  end;
end;

constructor TfpgBaseTextEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Focusable   := True;
  FFont       := fpgGetFont('#Edit1');
  Width       := 320;
  Height      := 240;
  FLines      := TStringList.Create;
  CaretPos.x  := 0;
  CaretPos.y  := 0;
  FTopLine    := 0;
  FTabWidth   := 8;
  FMaxScrollH := 0;
  VPos        := 0;
  HPos        := 0;
  FTracking   := True;
  FFullRedraw := False;
  FSelected   := False;

  FVScrollBar          := TfpgScrollBar.Create(self);
  FVScrollBar.Orientation := orVertical;
  FVScrollBar.OnScroll := @VScrollBarMove;
  FVScrollBar.Visible  := False;

  FHScrollBar          := TfpgScrollBar.Create(self);
  FHScrollBar.Orientation := orHorizontal;
  FHScrollBar.OnScroll := @HScrollBarMove;
//  FHScrollBar.ScrollStep := 5;
  FHScrollBar.Visible  := False;

  InitMemoObjects;
end;

destructor TfpgBaseTextEdit.Destroy;
begin
  FLines.Free;
  FFont.Free;
  inherited Destroy;
end;

function TfpgBaseTextEdit.GetClientRect: TfpgRect;
begin
  // widget has a 2 pixel 3D border
  Result.SetRect(2, 2, Width-4, Height-4);
  if FVScrollBar.Visible then
    Result.Width := Result.Width - FVScrollBar.Width;
  if FHScrollBar.Visible then
    Result.Height := Result.Height - FHScrollBar.Height;
end;

function TfpgBaseTextEdit.GetWordAtPos(const X, Y: Integer; out XBegin: Integer): TfpgString;
{ todo: This needs to be made UTF8 compliant! It currently is not. }
const
  ValidChars = ['a'..'z', 'A'..'Z', '0'..'9', '#'];
var
  S: TfpgString;
  C: Char;
  I, Si, Ei, CrX: Integer;
  lX: integer;
begin
  Result := '';
  XBegin := -1;
  Si := 0;
  Ei := 0;
  lX := X;
  if Y > pred(FLines.Count) then Exit;  //==>
  S := FLines[Y];
  if S = '' then Exit;  //==>
  if lX > UTF8Length(S) - 1 then
    lX := UTF8Length(S) - 1;
  if not (S[lX + 1] in ValidChars) then
  begin
    CrX := lX - 1;
    for I := CrX downto 1 do
    begin
      C := S[I + 1];
      if (C in ValidChars) then
      begin
        lX := I;
        Break;
      end;
    end;
    if lX = 0 then Exit;  //==>
  end;
  for I := (lX + 1) downto 1 do
    if S[I] in ValidChars then
      Si := I
    else
      Break;
  for I := (lX + 1) to Length(S) do
    if S[I] in ValidChars then
      Ei := I + 1
    else
      Break;
  if Ei >= Si then
  begin
    Result := UTF8Copy(S, Si, Ei - Si);
    XBegin := Si - 1;
  end;
end;

procedure TfpgBaseTextEdit.GetRowColAtPos(const X, Y: Integer; out Row, Col: Integer);
var
  Fine: Integer;
  lX: Integer;
begin
  Row := Y div FChrH;
  if Row > Flines.Count then
    Row := FLines.Count;

  lX := X - GetClientRect.Left;
  if FGutterPan.Visible then
  begin
    if lX < FGutterPan.Width then
      lX := FGutterPan.Width;
    Col   := (lX - FGutterPan.Width) div FChrW;
    Fine  := (lX - FGutterPan.Width) mod FChrW;
  end
  else
  begin
    if lX < 0 then
      lX := 0;
    Col   := lX div FChrW;
    Fine  := lX mod FChrW;
  end;
  if Fine > (FChrW div 2) - 1 then
    Col := Col + 1;
end;

procedure TfpgBaseTextEdit.Clear;
begin
  CaretPos.x := 0;
  CaretPos.y := 0;
  ScrollTo(0, 0);
  FSelStartNo := 0;
  FSelStartOffs := 0;
  FSelEndNo := 0;
  FSelEndOffs := 0;
  FLines.Clear;
  FSelected := False;
  Invalidate;
end;

procedure TfpgBaseTextEdit.ScrollTo(X, Y: Integer);
begin
  SetVPos(Y div FChrH);
  SetHPos(X div FChrW);
end;

procedure TfpgBaseTextEdit.DeleteSelection;
begin
  { TODO : Implement DeleteSelection }
end;

procedure TfpgBaseTextEdit.SaveToFile(const AFileName: TfpgString);
var
  BuffList: TStringList;
  SLine: TfpgString;
  I, P: Integer;
  Replace: Boolean;
begin
  BuffList := TStringList.Create;
  try
    BuffList.Assign(FLines);
    for I := 0 to pred(BuffList.Count) do
    begin
      SLine := BuffList[I];
      P := UTF8Length(SLine);
      Replace := (P > 0) and (SLine <> '');
      if Replace then
      begin
        while (fpgCharAt(SLine, P) = ' ') do
        begin
          UTF8Delete(SLine, P, 1);
          P := UTF8Length(SLine);
        end;
        BuffList[I] := SLine;
      end;
    end;
    BuffList.SaveToFile(AFileName);
  finally
    BuffList.Free;
  end;
end;

procedure TfpgBaseTextEdit.LoadFromFile(const AFileName: TfpgString);
begin
  if not fpgFileExists(AFileName) then
    Exit; //==>
  Clear;
  FLines.LoadFromFile(AFileName);
  Invalidate;
end;


end.

