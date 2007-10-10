{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a Memo control. Also known as a multi-line text edit control.
}

unit gui_memo;

{$mode objfpc}{$H+}

{
  TODO:
    * Started a implementation for Tab support. It is still very experimental
      and should not be used yet.
}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_widget,
  gui_scrollbar;

type

  { TfpgMemo }

  TfpgMemo = class(TfpgWidget)
  private
    FLines: TStringList;
    FMaxLength: integer;
    FCursorPos: integer;
    FCursorLine: integer;
    FOnChange: TNotifyEvent;
    FSideMargin: integer;
    FSelStartLine: integer;
    FSelEndLine: integer;
    FSelStartPos: integer;
    FSelEndPos: integer;
    FSelecting: boolean;
    FMouseDragging: boolean;
    FMouseDragPos: integer;
    FFont: TfpgFont;
    FBackgroundColor: TfpgColor;
    FDrawOffset: integer;
    FLineHeight: integer;
    FFirstLine: integer;
    FTabWidth: integer;
    FUseTabs: boolean;
    FVScrollBar: TfpgScrollBar;
    FHScrollBar: TfpgScrollBar;
    FWrapping: boolean;
    FLongestLineWidth: TfpgCoord;
    function    GetFontDesc: string;
    procedure   SetFontDesc(const AValue: string);
    procedure   RecalcLongestLine;
    procedure   DeleteSelection;
    procedure   DoCopy;
    procedure   DoPaste;
    procedure   AdjustCursor;
    function    LineCount: integer;
    function    GetLineText(linenum: integer): string;
    procedure   SetLineText(linenum: integer; Value: string);
    function    GetCursorX: integer;
    procedure   SetCPByX(x: integer);
    function    CurrentLine: string;
    function    VisibleLines: integer;
    function    VisibleWidth: integer;
    procedure   VScrollBarMove(Sender: TObject; position: integer);
    procedure   HScrollBarMove(Sender: TObject; position: integer);
    procedure   SetText(const AValue: string);
    function    GetText: string;
    procedure   SetCursorLine(aValue: integer);
    procedure   UpdateScrollBarCoords;
  protected
    procedure   HandleKeyChar(var AText: string; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleResize(dwidth, dheight: integer); override;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandlePaint; override;
    procedure   HandleShow; override;
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   UpdateScrollBars;
    function    SelectionText: string;
    property    LineHeight: integer read FLineHeight;
    property    CursorLine: integer read FCursorLine write SetCursorLine;
    property    Text: string read GetText write SetText;
    property    Font: TfpgFont read FFont;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    property    UseTabs: boolean read FUseTabs write FUseTabs;
    property    TabWidth: integer read FTabWidth write FTabWidth;
    property    MaxLength: integer read FMaxLength write FMaxLength;
  published
    property    Lines: TStringList read FLines;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
  end;


implementation

uses
  gfx_UTF8utils;

{ TfpgMemo }

procedure TfpgMemo.SetCursorLine(aValue: integer);
var
  i: integer;
  MaxLine: integer;
  yp: integer;
begin
  if (aValue < 1) or (aValue = FCursorLine) then
    Exit; // wrong value
  if aValue < FFirstLine then
  begin
    FFirstLine  := aValue; // moves the selected line to the top of the displayed rectangle
    FCursorLine := aValue;
    FCursorPos  := 0;
    RePaint;
    Exit;
  end;
  yp      := 2;
  MaxLine := 0;
  for i := FFirstLine to LineCount do
  begin
    yp := yp + LineHeight;
    if yp > Height then
    begin
      MaxLine := i - 1;
      break;
    end;
  end;
  if MaxLine < aValue then
  begin
    FFirstLine  := aValue;
    FCursorLine := aValue;
    FCursorPos  := 0;
    RePaint;
    Exit;
  end
  else
  begin
    FCursorLine := aValue;
    FCursorPos  := 0;
    RePaint;
    Exit;
  end;
end;

procedure TfpgMemo.UpdateScrollBarCoords;
var
  HWidth: integer;
  VHeight: integer;
begin
  VHeight := Height - 4;
  HWidth  := Width - 4;
  
  if FVScrollBar.Visible then
    Dec(HWidth, FVScrollBar.Width);
  if FHScrollBar.Visible then
    Dec(VHeight, FHScrollBar.Height);
  
  FHScrollBar.Top     := Height -FHScrollBar.Height - 2;
  FHScrollBar.Left    := 2;
  FHScrollBar.Width   := HWidth;

  FVScrollBar.Top     := 2;
  FVScrollBar.Left    := Width - FVScrollBar.Width - 2;
  FVScrollBar.Height  := VHeight;

  FVScrollBar.UpdateWindowPosition;
  FHScrollBar.UpdateWindowPosition;
end;

constructor TfpgMemo.Create(AOwner: TComponent);
begin
  inherited;
  Focusable   := True;
  FFont       := fpgGetFont('#Edit1');
  FHeight     := FFont.Height * 3 + 4;
  FWidth      := 120;
  FLineHeight := FFont.Height + 2;
  FSelecting  := False;
  FSideMargin := 3;
  FMaxLength  := 0;
  FWrapping   := False;
  FOnChange   := nil;
  FBackgroundColor := clBoxColor;
  FUseTabs    := False;
  FTabWidth   := 4;

  FLines      := TStringList.Create;
  FFirstLine  := 1;
  FCursorLine := 1;

  FCursorPos    := 0;
  FSelStartPos  := FCursorPos;
  FSelEndPos    := 0;
  FSelStartLine := 0;
  FSelEndLine   := 0;

  FDrawOffset    := 0;
  FMouseDragging := False;

  FVScrollBar          := TfpgScrollBar.Create(self);
  FVScrollBar.Orientation := orVertical;
  FVScrollBar.OnScroll := @VScrollBarMove;
  FVScrollBar.Visible  := False;

  FHScrollBar          := TfpgScrollBar.Create(self);
  FHScrollBar.Orientation := orHorizontal;
  FHScrollBar.OnScroll := @HScrollBarMove;
  FHScrollBar.ScrollStep := 5;
  FHScrollBar.Visible  := False;
end;

destructor TfpgMemo.Destroy;
begin
  FLines.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TfpgMemo.RecalcLongestLine;
var
  n: integer;
  lw: TfpgCoord;
begin
  FLongestLineWidth := 0;
  for n := 1 to LineCount do
  begin
    lw := FFont.TextWidth(getlinetext(n));
    if lw > FlongestLineWidth then
      FlongestLineWidth := lw;
  end;
end;

function TfpgMemo.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TfpgMemo.DeleteSelection;
var
  n: integer;
  selsl: integer;
  selsp: integer;
  selel: integer;
  selep: integer;
  ls: string;
  len: integer;
  st: integer;
begin
  if FSelEndLine < 1 then
    Exit;

  if (FSelStartLine shl 16) + FSelStartPos <= (FSelEndLine shl 16) + FSelEndPos then
  begin
    selsl := FSelStartLine;
    selsp := FSelStartPos;
    selel := FSelEndLine;
    selep := FSelEndPos;
  end
  else
  begin
    selel := FSelStartLine;
    selep := FSelStartPos;
    selsl := FSelEndLine;
    selsp := FSelEndPos;
  end;

  for n := selsl to selel do
  begin
    ls := GetLineText(n);

    if selsl < n then
      st  := 0
    else
      st  := selsp;
    if selel > n then
      len := UTF8Length(ls)
    else
      len := selep - st;

    UTF8Delete(ls, st + 1, len);
    SetLineText(n, ls);
  end;

  if selsl < selel then
  begin
    ls := GetlineText(selsl);
    ls := ls + GetLineText(selel);
    SetLineText(selsl, ls);
  end;

  for n := selsl + 1 to selel do
    FLines.Delete(selsl);

  FCursorPos  := selsp;
  FCursorLine := selsl;
  FSelEndLine := 0;
end;

procedure TfpgMemo.DoCopy;
var
  n: integer;
  selsl: integer;
  selsp: integer;
  selel: integer;
  selep: integer;
  ls: string;
  len: integer;
  st: integer;
  s: string;
begin
  if FSelEndLine < 1 then
    Exit;

  if (FSelStartLine shl 16) + FSelStartPos <= (FSelEndLine shl 16) + FSelEndPos then
  begin
    selsl := FSelStartLine;
    selsp := FSelStartPos;
    selel := FSelEndLine;
    selep := FSelEndPos;
  end
  else
  begin
    selel := FSelStartLine;
    selep := FSelStartPos;
    selsl := FSelEndLine;
    selsp := FSelEndPos;
  end;

  s := '';

  for n := selsl to selel do
  begin
    if n > selsl then
      s := s + #13#10;

    ls := GetLineText(n);

    if selsl < n then
      st := 0
    else
      st := selsp;

    if selel > n then
      len := UTF8Length(ls)
    else
      len := selep - st;

    s := s + UTF8Copy(ls, st + 1, len);
  end;

  //SetClipboardText(s);
end;

procedure TfpgMemo.DoPaste;
var
  s: string;
  si: string;
  si8: string;
  lineend: string;
  n: integer;
  l: integer;
  lcnt: integer;
begin
  Exit;
  (*
  DeleteSelection;
  s := GetClipboardText;

  si := UTF8Copy(CurrentLine,1,FCursorPos);
  lineend := UTF8Copy(CurrentLine,FCursorPos+1, UTF8Length(CurrentLine));
  l := FCursorLine;
  n := 1;
  lcnt := 0;
  si8 := '';
  while n <= length(s) do
  begin
    if (s[n] = #13) or (s[n] = #10) then
    begin
      if lcnt = 0 then SetLineText(l, si + si8)
                  else FLines.Insert(l-1, si + si8);

      si := '';
      si8 := '';
      inc(lcnt);
      inc(l);

      // skip multibyte line end:
      if (s[n]=#13) and (n < length(s)) and (s[n+1]=#10) then inc(n);
    end
    else
    begin
      si8 := si8 + s[n];
    end;
    inc(n);
  end;

  si := si + si8;

  FCursorPos := UTF8Length(si);
  si := si + lineend;

  if lcnt = 0 then
  begin
    SetLineText(l, si)
  end
  else
  begin
    FLines.Insert(l-1, si);
    FCursorLine := l;
  end;

  AdjustCursor;
  Repaint;
*)
end;

procedure TfpgMemo.AdjustCursor;
var
  tw: integer;
begin
  // horizontal adjust
  RecalcLongestLine;
  tw := FFont.TextWidth(UTF8Copy(CurrentLine, 1, FCursorPos));

  if tw - FDrawOffset > VisibleWidth - 2 then
    FDrawOffset := tw - VisibleWidth + 2
  else if tw - FDrawOffset < 0 then
  begin
    FDrawOffset := tw;
    if tw <> 0 then
      Dec(FDrawOffset, 2);
  end;

  // vertical adjust
  if FCursorLine < FFirstLine then
    FFirstLine := FCursorLine;
  if FCursorline - FFirstLine + 1 > VisibleLines then
    FFirstLine := FCursorline - VisibleLines + 1;

  if FFirstLine + VisibleLines > LineCount then
  begin
    FFirstLine := LineCount - VisibleLines + 1;
    if FFirstline < 1 then
      FFirstLine := 1;
  end;

  UpdateScrollbars;
end;

procedure TfpgMemo.UpdateScrollBars;
var
  vlines: integer;
  vsbw, x: integer;
  hsbwas: boolean;
  vsbwas: boolean;
  vsbvis: boolean;
begin
  hsbwas := FHScrollBar.Visible;
  vsbwas := FVScrollBar.Visible;
  vlines := (Height - (FSideMargin shl 1)) div Lineheight;
  vsbvis := (LineCount > vlines);

  if vsbvis then
    vsbw := FVScrollBar.Width
  else
    vsbw := 0;

  FHScrollBar.Visible := FLongestLineWidth > (Width - vsbw - FSideMargin * 2) - 1;

  if FHScrollBar.Visible and not vsbvis then
  begin
    // recheck vertical scrollbar
    vlines := (Height - (FSideMargin shl 1) - FHScrollBar.Height) div Lineheight;
    vsbvis := (LineCount > vlines);
  end;

  FVScrollBar.Visible := vsbvis;

  UpdateScrollBarCoords;

  if FHScrollBar.Visible then
  begin
    FHScrollBar.Min := 0;
    FHScrollBar.Max := FLongestLineWidth - VisibleWidth - 1;
    if (FLongestLineWidth <= 0) or (FLongestLineWidth <= VisibleWidth) then
      FHScrollBar.SliderSize := 1
    else
      FHScrollBar.SliderSize := VisibleWidth / FLongestLineWidth;
    FHScrollBar.Position := FDrawOffset;
    FHScrollBar.RepaintSlider;
  end;

  if FVScrollBar.Visible then
  begin
    FVScrollBar.Min        := 1;
    // TODO: Look at calculation of vlines value to improve this!
    if LineCount > 0 then
    begin
      FVScrollBar.SliderSize := VisibleLines / LineCount;
      FVScrollBar.Max        := LineCount - VisibleLines + 1;
    end
    else
    begin
      FVScrollBar.SliderSize := 0.5;
      FVScrollBar.Max        := 10;
    end;
    FVScrollBar.Position   := FFirstLine;
    FVScrollBar.RepaintSlider;
  end;

  if (hsbwas <> FHScrollBar.Visible) or (vsbwas <> FVScrollBar.Visible) then
    AdjustCursor;
end;

function TfpgMemo.LineCount: integer;
begin
  Result := FLines.Count;
end;

function TfpgMemo.GetLineText(linenum: integer): string;
begin
  if LineCount < 1 then
    FLines.Add('');
  if (linenum >= 1) and (linenum <= LineCount) then
    Result := FLines.Strings[linenum - 1]
  else
    Result := '';
end;

procedure TfpgMemo.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  RePaint;
end;

procedure TfpgMemo.SetLineText(linenum: integer; Value: string);
begin
  FLines.Strings[linenum - 1] := Value;
end;

function TfpgMemo.GetCursorX: integer;
begin
  Result := FFont.TextWidth(copy(CurrentLine, 1, FCursorPos));
end;

// Set cursor position by X
procedure TfpgMemo.SetCPByX(x: integer);
var
  s: string;
  n: integer;
  cpx: integer;
  cp: integer;
  cx: integer;
  ls: string;
begin
  // searching the appropriate character position
  ls  := CurrentLine;
  cpx := FFont.TextWidth(UTF8Copy(ls, 1, FCursorPos)); // + FDrawOffset + FSideMargin;
  cp  := FCursorPos;
  if cp > UTF8Length(ls) then
    cp := UTF8Length(ls);

  s := '';

  for n := 0 to UTF8Length(ls) do
  begin
    cx := FFont.TextWidth(UTF8Copy(ls, 1, n)); // + FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp  := n;
    end;
  end;

  FCursorPos := cp;
end;

function TfpgMemo.CurrentLine: string;
begin
  Result := GetLineText(FCursorLine);
end;

function TfpgMemo.VisibleLines: integer;
var
  sh: integer;
begin
  if FHScrollBar.Visible then
    sh := 18
  else
    sh := 0;
  Result := (Height - (FSideMargin shl 1) - sh) div Lineheight;
end;

function TfpgMemo.VisibleWidth: integer;
var
  sw: integer;
begin
  if FVScrollBar.Visible then
    sw := FVScrollBar.Width
  else
    sw := 0;
  Result := (Width - (FSideMargin shl 1) - sw);
end;

procedure TfpgMemo.HandleShow;
begin
  inherited HandleShow;
  if (csLoading in ComponentState) then
    Exit;
  RecalcLongestLine;
  UpdateScrollBars;
  UpdateScrollBarCoords;
end;

procedure TfpgMemo.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  MouseCursor := mcIBeam;
end;

procedure TfpgMemo.HandleMouseExit;
begin
  inherited HandleMouseExit;
  MouseCursor := mcDefault;
end;

procedure TfpgMemo.VScrollBarMove(Sender: TObject; position: integer);
begin
  if FFirstLine <> position then
  begin
    FFirstLine := position;
    repaint;
  end;
end;

procedure TfpgMemo.HScrollBarMove(Sender: TObject; position: integer);
begin
  if position <> FDrawOffset then
  begin
    FDrawOffset := position;
    Repaint;
  end;
end;

procedure TfpgMemo.HandlePaint;
var
  n: integer;
  tw, tw2, st, len: integer;
  yp, xp: integer;
  ls: string;
  r: TfpgRect;
  selsl, selsp, selel, selep: integer;
  c: integer;
  s: string;
begin
  Canvas.BeginDraw;
  Canvas.ClearClipRect;
  r.SetRect(0, 0, Width, Height);
  Canvas.DrawControlFrame(r);

  InflateRect(r, -2, -2);
  Canvas.SetClipRect(r);

  if Enabled then
    Canvas.SetColor(FBackgroundColor)
  else
    Canvas.SetColor(clWindowBackground);
  Canvas.FillRectAngle(r);

  Canvas.SetFont(FFont);

  if (FSelStartLine shl 16) + FSelStartPos <= (FSelEndLine shl 16) + FSelEndPos then
  begin
    selsl := FSelStartLine;
    selsp := FSelStartPos;
    selel := FSelEndLine;
    selep := FSelEndPos;
  end
  else
  begin
    selel := FSelStartLine;
    selep := FSelStartPos;
    selsl := FSelEndLine;
    selsp := FSelEndPos;
  end;

  yp := 3;
  for n := FFirstline to LineCount do
  begin
    ls := GetLineText(n);
    if FUseTabs then
    begin
      xp := 0;
      s := '';
      for c := 1 to Length(ls) do
      begin
        if ls[c] = #9 then
        begin
          if s <> '' then
            Canvas.DrawString(-FDrawOffset + FSideMargin + xp, yp, s);
          xp := xp + Canvas.Font.TextWidth(' ') * FTabWidth;
          s := '';
        end
        else
          s := s + ls[c];
      end;
      if s <> '' then
        Canvas.DrawString(-FDrawOffset + FSideMargin + xp, yp, s);
    end
    else
      Canvas.DrawString(-FDrawOffset + FSideMargin, yp, ls);

    if Focused then
    begin
      // drawing selection
      if (FSelEndLine > 0) and (selsl <= n) and (selel >= n) then
      begin
        if selsl < n then
          st  := 0
        else
          st  := selsp;
        if selel > n then
          len := UTF8Length(ls)
        else
          len := selep - st;

        tw  := FFont.TextWidth(UTF8Copy(ls, 1, st));
        tw2 := FFont.TextWidth(UTF8Copy(ls, 1, st + len));
        Canvas.XORFillRectangle(fpgColorToRGB(clSelection) xor $FFFFFF, -FDrawOffset +
          FSideMargin + tw, yp, tw2 - tw, LineHeight);
      end;

      //drawing cursor
      if FCursorLine = n then
      begin
        // drawing cursor
        tw := FFont.TextWidth(UTF8Copy(ls, 1, FCursorPos));
        fpgCaret.SetCaret(Canvas, -FDrawOffset + FSideMargin + tw, yp, fpgCaret.Width, FFont.Height);
      end;
    end;  { if }

    yp := yp + LineHeight;
    if yp > Height then
      Break;
  end;  { for }

  if not Focused then
    fpgCaret.UnSetCaret(Canvas);
    
  // The little square in the bottom right corner
  if FHScrollBar.Visible and FVScrollBar.Visible then
  begin
    Canvas.SetColor(clButtonFace);
    Canvas.FillRectangle(FHScrollBar.Left+FHScrollBar.Width,
                         FVScrollBar.Top+FVScrollBar.Height,
                         FVScrollBar.Width,
                         FHScrollBar.Height);
  end;

  Canvas.EndDraw;
end;

procedure TfpgMemo.HandleKeyChar(var AText: string; var shiftstate: TShiftState; var consumed: boolean);
var
  prevval: string;
  s: string;
  ls: string;
begin
  inherited;
  prevval  := Text;
  s        := AText;

  // Printable characters only
  // Note: This is not UTF-8 compliant!
  if (Ord(AText[1]) > 31) and (Ord(AText[1]) < 127) then
  begin
    // printeable
    //FText := FText + s;

    if (FMaxLength <= 0) or (UTF8Length(FLines.Text) < FMaxLength) then
    begin
      DeleteSelection;
      ls := GetLineText(FCursorLine);
      UTF8Insert(s, ls, FCursorPos + 1);
      SetLineText(FCursorLine, ls);
      Inc(FCursorPos);
      FSelStartPos  := FCursorPos;
      FSelStartLine := FCursorLine;
      FSelEndLine   := 0;
      AdjustCursor;
    end;

    consumed := True;
  end;

  if prevval <> Text then
    if Assigned(FOnChange) then
      FOnChange(self);

  if consumed then
    RePaint;
end;

procedure TfpgMemo.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  cx: integer;
  ls: string;
  ls2: string;
  hasChanged: boolean;
  
  procedure StopSelection;
  begin
    FSelStartLine := FCursorLine;
    FSelStartPos  := FCursorPos;
    FSelEndLine   := 0;
  end;
  
begin
  Consumed := True;
  hasChanged := False;
  case CheckClipBoardKey(keycode, shiftstate) of
    ckCopy:
        begin
          DoCopy;
        end;
    ckPaste:
        begin
          DoPaste;
          hasChanged := True;
        end;
    ckCut:
        begin
          DoCopy;
          DeleteSelection;
          hasChanged := True;
        end;
  else
    Consumed := False;
  end;

  if not Consumed then
  begin
    // checking for movement keys:
    consumed   := True;
    FSelecting := (ssShift in shiftstate);

    case keycode of
      keyLeft:
        if FCursorPos > 0 then
        begin
          Dec(FCursorPos);

          if (ssCtrl in shiftstate) then
            // word search...
            (*
                    while (FCursorPos > 0) and not pgfIsAlphaNum(copy(CurrentLine,FCursorPos,1))
                      do Dec(FCursorPos);

                    while (FCursorPos > 0) and pgfIsAlphaNum(copy(CurrentLine,FCursorPos,1))
                      do Dec(FCursorPos);
            *);

        end;// left

      keyRight:
        if FCursorPos < UTF8Length(CurrentLine) then
        begin
          Inc(FCursorPos);

          if (ssCtrl in shiftstate) then
            // word search...
            (*
                    while (FCursorPos < length(CurrentLine)) and pgfIsAlphaNum(copy(CurrentLine,FCursorPos+1,1))
                      do Inc(FCursorPos);

                    while (FCursorPos < length(CurrentLine)) and not pgfIsAlphaNum(copy(CurrentLine,FCursorPos+1,1))
                      do Inc(FCursorPos);
              *);

        end;// right

      keyUp:
      begin  // up
        cx := GetCursorX;
        if FCursorLine > 1 then
        begin
          Dec(FCursorline);
          SetCPByX(cx);
        end;
      end;

      keyDown:
      begin
        cx := GetCursorX;
        if FCursorLine < LineCount then
        begin
          Inc(FCursorline);
          SetCPByX(cx);
        end;
      end;

      keyHome:
      begin
        if (ssCtrl in shiftstate) then
          FCursorLine := 1;
        FCursorPos := 0;
      end;

      keyEnd:
      begin
        if (ssCtrl in shiftstate) then
          FCursorLine := LineCount;
        FCursorPos := UTF8Length(CurrentLine);
      end;

      keyPageUp:
        if FCursorLine > 1 then
        begin
          cx := GetCursorX;
          Dec(FCursorLine, VisibleLines);
          if FCursorLine < 1 then
            FCursorLine := 1;
          SetCPByX(cx);
        end;

      keyPageDown:
      begin
        cx := GetCursorX;
        if FCursorLine < LineCount then
        begin
          Inc(FCursorline, VisibleLines);
          if FCursorLine > LineCount then
            FCursorLine := LineCount;
          SetCPByX(cx);
        end;
      end;

      else
        Consumed := False;
    end;

    if Consumed then
    begin
      AdjustCursor;

      if FSelecting then
      begin
        FSelEndPos  := FCursorPos;
        FSelEndLine := FCursorLine;
      end
      else
        StopSelection;
    end;
  end;

  if not Consumed then
  begin
    consumed := True;

    case keycode of
      keyReturn:
          begin
            ls  := UTF8Copy(FLines[FCursorline - 1], 1, FCursorPos);
            ls2 := UTF8Copy(FLines[FCursorline - 1], FCursorPos + 1, UTF8Length(FLines[FCursorline - 1]));
            FLines.Insert(FCursorLine - 1, ls);
            Inc(FCursorLine);
            SetLineText(FCursorLine, ls2);
            FCursorPos := 0;
            hasChanged := True;
          end;
      keyBackSpace:
          begin
            if FCursorPos > 0 then
            begin
              ls := GetLineText(FCursorLine);
              UTF8Delete(ls, FCursorPos, 1);
              SetLineText(FCursorLine, ls);
              Dec(FCursorPos);
            end
            else if FCursorLine > 1 then
            begin
              ls         := CurrentLine;
              FLines.Delete(FCursorLine - 1);
              Dec(FCursorLine);
              FCursorPos := UTF8Length(FLines.Strings[FCursorLine - 1]);
              FLines.Strings[FCursorLine - 1] := FLines.Strings[FCursorLine - 1] + ls;
            end;
            hasChanged := True;
          end;

      keyDelete:
          begin
            ls := GetLineText(FCursorLine);
            if FSelEndLine > 0 then
              DeleteSelection
            else if FCursorPos < UTF8Length(ls) then
            begin
              UTF8Delete(ls, FCursorPos + 1, 1);
              SetLineText(FCursorLine, ls);
            end
            else if FCursorLine < LineCount then
            begin
              ls2 := FLines.Strings[FCursorLine];
              FLines.Delete(FCursorLine);
              FLines.Strings[FCursorLine - 1] := ls + ls2;
            end;
            hasChanged := True;
          end;
      keyTab:
          begin
            if FUseTabs then
            begin
              ls := GetLineText(FCursorLine);
{              if FSelEndLine > 0 then
                DeleteSelection
              else} if FCursorPos < UTF8Length(ls) then
              begin
                UTF8Insert(#9, ls, FCursorPos);
                SetLineText(FCursorLine, ls);
              end;
{
              else if FCursorLine < LineCount then
              begin
                ls2 := FLines.Strings[FCursorLine];
                FLines.Delete(FCursorLine);
                FLines.Strings[FCursorLine - 1] := ls + ls2;
              end;
}
              hasChanged := True;
            end;
          end;
      else
        consumed := False;
    end;

    if Consumed then
    begin
      StopSelection;
      AdjustCursor;
    end;
  end;

  if consumed then
    RePaint
  else
    inherited;
    
  if hasChanged then
    if Assigned(FOnChange) then
      FOnChange(self);
end;

procedure TfpgMemo.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  s: string;
  n: integer;
  cpx: integer;
  cp: integer;
  cx: integer;
  lnum: integer;
  ls: string;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  // searching the appropriate character position
  lnum := FFirstLine + (y - FSideMargin) div LineHeight;
  if lnum > LineCount then
    lnum := LineCount;

  ls  := GetLineText(lnum);
  cpx := FFont.TextWidth(UTF8Copy(ls, 1, FCursorPos)) - FDrawOffset + FSideMargin;
  cp  := FCursorPos;

  s := '';

  for n := 0 to UTF8Length(ls) do
  begin
    cx := FFont.TextWidth(UTF8Copy(ls, 1, n)) - FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp  := n;
    end;
  end;

  FMouseDragging := True;
  FMouseDragPos  := cp;
  FCursorPos     := cp;
  FCursorLine    := lnum;

  if (ssShift in shiftstate) then
  begin
    FSelEndLine := lnum;
    FSelEndpos  := cp;
  end
  else
  begin
    FSelStartLine := lnum;
    FSelStartPos  := cp;
    FSelEndLine   := 0;
  end;
  Repaint;
end;

procedure TfpgMemo.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  s: string;
  n: integer;
  cpx: integer;
  cp: integer;
  cx: integer;
  lnum: integer;
  ls: string;
begin
  if not FMouseDragging or ((btnstate and 1) = 0) then
  begin
    FMouseDragging := False;
    Exit;
  end;

  // searching the appropriate character position
  lnum := FFirstLine + (y - FSideMargin) div LineHeight;
  if lnum > LineCount then
    lnum := LineCount;

  ls  := GetLineText(lnum);
  cpx := FFont.TextWidth(UTF8Copy(ls, 1, FCursorPos)) - FDrawOffset + FSideMargin;
  cp  := FCursorPos;
  s   := '';

  for n := 0 to UTF8Length(ls) do
  begin
    cx := FFont.TextWidth(UTF8Copy(ls, 1, n)) - FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp  := n;
    end;
  end;

  if (cp <> FCursorPos) or (lnum <> FCursorLine) then
  begin
    FCursorLine := lnum;
    FSelEndLine := lnum;
    FSelEndPos  := cp;
    FCursorPos  := cp;
    Repaint;
  end;


  // searching the appropriate character position
  {
  cpx := FFont.TextWidth16(copy16(FText,1,FCursorPos)) + FDrawOffset + FSideMargin;
  cp := FCursorPos;

  s := '';

  for n := 0 to Length16(Text) do
  begin
    cx := FFont.TextWidth16(copy16(Text,1,n)) + FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp := n;
    end;
  end;

  //FMouseDragPos := cp;
  FSelOffset := cp-FSelStart;
  if FCursorPos <> cp then
  begin
    FCursorPos := cp;
    Repaint;
  end;
}
end;

(*
procedure TfpgMemo.HandleWindowScroll(direction, amount: integer);
var
  pfl, pdo : integer;
begin
  inherited HandleWindowScroll(direction, amount);

  pfl := FFirstLine;
  pdo := FDrawOffset;

  if direction = 0 then
  begin
    dec(FFirstLine, amount);
  end;
  if direction = 1 then
  begin
    inc(FFirstLine, amount);
  end;
  if FFirstLine > LineCount - VisibleLines + 1 then FFirstLine := LineCount - VisibleLines + 1;
  if FFirstLine < 1 then FFirstLine := 1;

  if FHScrollBar.Visible then
  begin
    if Direction = 2 then
    begin
      dec(FDrawOffset, amount*16);
    end;
    if Direction = 3 then
    begin
      inc(FDrawOffset, amount*16);
    end;

    if FDrawOffset > FHScrollBar.Max then FDrawOffset := FHScrollBar.Max;
    if FDrawOffset < 0 then FDrawOffset := 0;
  end;

  if (pfl <> FFirstLine) or (pdo <> FDrawOffset) then
  begin
    UpdateScrollBars;
    Repaint;
  end;

end;
*)

procedure TfpgMemo.HandleResize(dwidth, dheight: integer);
begin
  inherited HandleResize(dwidth, dheight);
  if (csLoading in ComponentState) then
    Exit;
  UpdateScrollBarCoords;
  UpdateScrollBars;
end;

procedure TfpgMemo.HandleMouseScroll(x, y: integer; shiftstate: TShiftState;
  delta: smallint);
var
  pfl, pdo : integer;
begin
  inherited HandleMouseScroll(x, y, shiftstate, delta);

  pfl := FFirstLine;
  pdo := FDrawOffset;

  if delta < 0 then
    dec(FFirstLine, abs(delta))   // scroll up
  else
    inc(FFirstLine, abs(delta));  // scroll down

  if FFirstLine > LineCount - VisibleLines + 1 then
    FFirstLine := LineCount - VisibleLines + 1;
  if FFirstLine < 1 then
    FFirstLine := 1;

  if FHScrollBar.Visible then
  begin
    if FDrawOffset > FHScrollBar.Max then
      FDrawOffset := FHScrollBar.Max;
    if FDrawOffset < 0 then
      FDrawOffset := 0;
  end;

  if (pfl <> FFirstLine) or (pdo <> FDrawOffset) then
  begin
    UpdateScrollBars;
    Repaint;
  end;
end;

function TfpgMemo.SelectionText: string;
begin
  {
  if FSelOffset <> 0 then
  begin
    if FSelOffset < 0 then
    begin
      Result := Copy(FText,1+FSelStart + FSelOffset,-FSelOffset);
    end
    else
    begin
      result := Copy(FText,1+FSelStart,FSelOffset);
    end;
  end
  else
    Result := '';
}
end;

function TfpgMemo.GetText: string;
var
  n: integer;
  s: string;
begin
  s := '';
  for n := 1 to LineCount do
  begin
    if n > 1 then
      s := s + #13#10;
    s := s + GetLineText(n);
  end;
  Result := s;
end;

procedure TfpgMemo.SetText(const AValue: string);
var
  n: integer;
  c: string[2];
  s: string;
begin
  FLines.Clear;
  s := '';
  n := 1;
  while n <= UTF8Length(AValue) do
  begin
    c := UTF8Copy(AValue, n, 1);
    if (c[1] = #13) or (c[1] = #10) then
    begin
      FLines.Add(s);
      s := '';
      c := UTF8Copy(AValue, n + 1, 1);
      if c[1] = #10 then
        Inc(n);
    end
    else
      s := s + c;
    Inc(n);
  end;

  if s <> '' then
    FLines.Add(s);

  FDrawOffset   := 0;
  FCursorPos    := 0;
  FCursorLine   := 1;
  FSelStartLine := FCursorLine;
  FSelStartPos  := FCursorPos;
  FSelEndLine   := 0;

  AdjustCursor;
  Repaint;
end;

end.

