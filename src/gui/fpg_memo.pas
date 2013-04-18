{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2012 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a Memo control. Also known as a multi-line text edit control.
}

unit fpg_memo;

{$mode objfpc}{$H+}

  { TODO : Started a implementation for Tab support. It is still very experimental and should not be used yet. }

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_scrollbar,
  fpg_menu;

type

  TfpgMemo = class(TfpgWidget)
  private
    FBorderStyle: TfpgEditBorderStyle;
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
    FDrawOffset: integer;
    FLineHeight: integer;
    FFirstLine: integer;
    FTabWidth: integer;
    FUseTabs: boolean;
    FVScrollBar: TfpgScrollBar;
    FHScrollBar: TfpgScrollBar;
    FWrapping: boolean;
    FLongestLineWidth: TfpgCoord;
    FPopupMenu: TfpgPopupMenu;
    FDefaultPopupMenu: TfpgPopupMenu;
    FReadOnly: Boolean;
    FUpdateCount: integer;
    function    GetFontDesc: string;
    procedure   SetBorderStyle(const AValue: TfpgEditBorderStyle);
    procedure   SetFontDesc(const AValue: string);
    procedure   RecalcLongestLine;
    procedure   DeleteSelection;
    procedure   DoCopy;
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
    procedure   SetText(const AValue: TfpgString);
    function    GetText: TfpgString;
    procedure   SetCursorLine(aValue: integer);
    procedure   UpdateScrollBarCoords;
    procedure   DefaultPopupCut(Sender: TObject);
    procedure   DefaultPopupCopy(Sender: TObject);
    procedure   DefaultPopupPaste(Sender: TObject);
    procedure   DefaultPopupClearAll(Sender: TObject);
    procedure   DefaultPopupInsertFromCharmap(Sender: TObject);
    procedure   SetDefaultPopupMenuItemsState;
    procedure   ShowDefaultPopupMenu(const x, y: integer; const shiftstate: TShiftState); virtual;
    procedure   SetReadOnly(const AValue: Boolean);
    procedure   ResetSelectionVariables;
    procedure   SetCursorPos(const AValue: integer);
    function    GetSelectionText: TfpgString;
    procedure   SetSelectionText(const AText: TfpgString);
  protected
    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleRMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleResize(dwidth, dheight: integer); override;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandlePaint; override;
    procedure   HandleShow; override;
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
    procedure   HandleHide; override;
    procedure   RePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   UpdateScrollBars;
    procedure   CopyToClipboard;
    procedure   CutToClipboard;
    procedure   PasteFromClipboard;
    procedure   Clear;
    procedure   BeginUpdate;
    procedure   EndUpdate;
    property    CursorPos: integer read FCursorPos write SetCursorPos;
    property    CursorLine: integer read FCursorLine write SetCursorLine;
    property    Font: TfpgFont read FFont;
    property    LineHeight: integer read FLineHeight;
    property    MaxLength: integer read FMaxLength write FMaxLength;
    property    TabWidth: integer read FTabWidth write FTabWidth;
    property    Text: TfpgString read GetText write SetText;
    property    UseTabs: boolean read FUseTabs write FUseTabs default False;
    property    PopupMenu: TfpgPopupMenu read FPopupMenu write FPopupMenu;
    Property    SelectionText : TfpgString Read GetSelectionText Write SetSelectionText;
  published
    property    Align;
    property    BackgroundColor default clBoxColor;
    property    BorderStyle: TfpgEditBorderStyle read FBorderStyle write SetBorderStyle default ebsDefault;
    property    Enabled;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    Hint;
    property    Lines: TStringList read FLines;
    property    ParentShowHint;
    property    ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property    ShowHint;
    property    TabOrder;
    property    TextColor;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    property    OnEnter;
    property    OnExit;
    property    OnKeyPress;
    property    OnShowHint;
  end;


function CreateMemo(AOwner: TComponent; x, y, w, h: TfpgCoord): TfpgMemo;


implementation

uses
  fpg_stringutils
  ,fpg_constants
  ,fpg_dialogs
  ;

const
  // internal popupmenu item names
  ipmCut        = 'miDefaultCut';
  ipmCopy       = 'miDefaultCopy';
  ipmPaste      = 'miDefaultPaste';
  ipmClearAll   = 'miDefaultClearAll';
  ipmCharmap    = 'miDefaultCharmap';


type
  // custom stringlist that will notify the memo of item changes
  TfpgMemoStrings = class(TStringList)
  protected
    Memo: TfpgMemo;  { this is just a reference }
  public
    constructor Create(AMemo: TfpgMemo); reintroduce;
    function    Add(const s: String): Integer; override;
    procedure   Clear; override;
    procedure   Delete(Index: Integer); override;
    procedure   Insert(Index: Integer; const S: string); override;
  end;

{ TfpgMemoStrings }

constructor TfpgMemoStrings.Create(AMemo: TfpgMemo);
begin
  inherited Create;
  Memo := AMemo;
end;

function TfpgMemoStrings.Add(const s: String): Integer;
begin
  Memo.BeginUpdate;
  Result := inherited Add(s);
  Memo.EndUpdate;
end;

procedure TfpgMemoStrings.Delete(Index: Integer);
begin
  Memo.BeginUpdate;
  inherited Delete(Index);
  Memo.EndUpdate;
end;

procedure TfpgMemoStrings.Insert(Index: Integer; const S: string);
begin
  Memo.BeginUpdate;
  inherited Insert(Index, S);
  Memo.EndUpdate;
end;

procedure TfpgMemoStrings.Clear;
begin
  if Assigned(Memo) then
    Memo.BeginUpdate;
  inherited Clear;
  if Assigned(Memo) then
    Memo.EndUpdate;
end;


{ TfpgMemo }


function CreateMemo(AOwner: TComponent; x, y, w, h: TfpgCoord): TfpgMemo;
begin
  Result       := TfpgMemo.Create(AOwner);
  Result.Left  := x;
  Result.Top   := y;
  Result.Width := w;
  if h > 0 then
    Result.Height := h;
end;


procedure TfpgMemo.SetCursorLine(aValue: integer);
var
  i: integer;
  MaxLine: integer;
  yp: integer;
begin
  if (aValue < 0) or (aValue = FCursorLine) or (AValue > FLines.Count-1) then
    Exit; // wrong value

  if aValue < FFirstLine then
  begin
    FFirstLine  := aValue; // moves the selected line to the top of the displayed rectangle
    FCursorLine := aValue;
    FCursorPos  := 0;
    FSelStartPos  := FCursorPos;
    FSelStartLine := FCursorLine;
    FSelEndLine   := -1;
    AdjustCursor;
    RePaint;
    Exit;
  end;
  yp      := 2;
  MaxLine := 0;
  for i := FFirstLine to LineCount-1 do
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
    FSelStartPos  := FCursorPos;
    FSelStartLine := FCursorLine;
    FSelEndLine   := -1;
    AdjustCursor;
    RePaint;
  end
  else
  begin
    FCursorLine := aValue;
    FCursorPos  := 0;
    FSelStartPos  := FCursorPos;
    FSelStartLine := FCursorLine;
    FSelEndLine   := -1;
    AdjustCursor;
    RePaint;
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

procedure TfpgMemo.DefaultPopupCut(Sender: TObject);
begin
  if ReadOnly then
    Exit;
  CutToClipboard;
end;

procedure TfpgMemo.DefaultPopupCopy(Sender: TObject);
begin
  if ReadOnly then
    Exit;
  CopyToClipboard;
end;

procedure TfpgMemo.DefaultPopupPaste(Sender: TObject);
begin
  if ReadOnly then
    Exit;
  PasteFromClipboard;
end;

procedure TfpgMemo.DefaultPopupClearAll(Sender: TObject);
begin
  if ReadOnly then
    Exit;
  Clear;
end;

procedure TfpgMemo.DefaultPopupInsertFromCharmap(Sender: TObject);
var
  s: TfpgString;
begin
  if ReadOnly then
    Exit;
  s := fpgShowCharMap;
  if s <> '' then
    SetSelectionText(s);
end;

procedure TfpgMemo.SetDefaultPopupMenuItemsState;
var
  i: integer;
  itm: TfpgMenuItem;
  b: boolean;

  function SomethingSelected: boolean;
  begin
    Result := SelectionText <> '';
  end;

begin
  b := SomethingSelected;
  for i := 0 to FDefaultPopupMenu.ComponentCount-1 do
  begin
    if FDefaultPopupMenu.Components[i] is TfpgMenuItem then
    begin
      itm := TfpgMenuItem(FDefaultPopupMenu.Components[i]);
      // enabled/disable menu items
      if itm.Name = ipmCut then
        itm.Enabled := (not ReadOnly) and b
      else if itm.Name = ipmCopy then
        itm.Enabled := b
      else if itm.Name = ipmPaste then
        itm.Enabled := (not ReadOnly) and (fpgClipboard.Text <> '')
      else if itm.Name = ipmClearAll then
        itm.Enabled := (not ReadOnly) and (Text <> '')
      else if itm.Name = ipmCharmap then
        itm.Enabled := (not ReadOnly);
    end;
  end;
end;

procedure TfpgMemo.ShowDefaultPopupMenu(const x, y: integer;
  const shiftstate: TShiftState);
var
  itm: TfpgMenuItem;
begin
  if not Assigned(FDefaultPopupMenu) then
  begin
    FDefaultPopupMenu := TfpgPopupMenu.Create(nil);
    itm := FDefaultPopupMenu.AddMenuItem(rsCut, '', @DefaultPopupCut);
    itm.Name := ipmCut;
    itm := FDefaultPopupMenu.AddMenuItem(rsCopy, '', @DefaultPopupCopy);
    itm.Name := ipmCopy;
    itm := FDefaultPopupMenu.AddMenuItem(rsPaste, '', @DefaultPopupPaste);
    itm.Name := ipmPaste;
    itm := FDefaultPopupMenu.AddMenuItem(rsDelete, '', @DefaultPopupClearAll);
    itm.Name := ipmClearAll;
    itm := FDefaultPopupMenu.AddMenuItem('-', '', nil);
    itm.Name := 'N1';
    itm := FDefaultPopupMenu.AddMenuItem(rsInsertFromCharacterMap, '', @DefaultPopupInsertFromCharmap);
    itm.Name := ipmCharmap;
  end;

  SetDefaultPopupMenuItemsState;
  FDefaultPopupMenu.ShowAt(self, x, y);
end;

procedure TfpgMemo.SetReadOnly(const AValue: Boolean);
begin
  if FReadOnly = AValue then exit;
  FReadOnly := AValue;
  RePaint;
end;

procedure TfpgMemo.ResetSelectionVariables;
begin
  FSelecting      := False;
  FSelStartPos    := FCursorPos;
  FSelEndPos      := FCursorPos;
  FSelStartLine   := FCursorLine;
  FSelEndLine     := FCursorLine;
  FMouseDragging  := False;
end;

procedure TfpgMemo.SetCursorPos(const AValue: integer);
var
  x: integer;
begin
  if FCursorPos = AValue then
    exit;

  if AValue = 0 then
    FCursorPos := AValue
  else
  begin
    x := UTF8Length(FLines[CursorLine]);
    if AValue > x then  { can't set Cursorpos greater than number of characters on that line }
      FCursorPos := x
    else
      FCursorPos := AValue;
  end;
  FSelStartPos  := FCursorPos;
  FSelEndPos    := FCursorPos;
  AdjustCursor;
  Repaint;
end;

constructor TfpgMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Focusable   := True;
  FFont       := fpgGetFont('#Edit1');
  FHeight     := FFont.Height * 3 + 4;
  FWidth      := 120;
  FLineHeight := FFont.Height + 2;
  FSideMargin := 3;
  FMaxLength  := 0;
  FWrapping   := False;
  FOnChange   := nil;
  FTextColor  := Parent.TextColor;
  FBackgroundColor := clBoxColor;
  FUseTabs    := False;
  FTabWidth   := 4;
  FMinWidth   := 20;
  FMinHeight  := 30;
  FPopupMenu  := nil;
  FDefaultPopupMenu := nil;
  FReadOnly   := False;
  FUpdateCount := 0;
  FBorderStyle := ebsDefault;

  FLines      := TfpgMemoStrings.Create(self);
  FFirstLine  := 0;
  FCursorLine := 0;

  ResetSelectionVariables;

  FDrawOffset    := 0;

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
  if Assigned(FDefaultPopupMenu) then
    FDefaultPopupMenu.Free;
  TfpgMemoStrings(FLines).Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TfpgMemo.RecalcLongestLine;
var
  n: integer;
  lw: TfpgCoord;
begin
  FLongestLineWidth := 0;
  for n := 0 to LineCount-1 do
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

procedure TfpgMemo.SetBorderStyle(const AValue: TfpgEditBorderStyle);
begin
  if FBorderStyle = AValue then
    exit;
  FBorderStyle := AValue;
  RePaint;
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
  if ReadOnly then
    Exit;
  if (FSelEndLine < 0) or (FSelStartLine<0) then
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

  //delete moves lines up, so delete same line number over and over.
  for n := (selsl+1) to selel do
    FLines.Delete(selsl+1);

  FCursorPos  := selsp;
  FCursorLine := selsl;
  FSelStartPos := FCursorPos;
  FSelEndPos := FCursorPos;
  FSelStartLine := selsl;
  FSelEndLine := -1;
end;

procedure TfpgMemo.DoCopy;
begin
  if FSelEndLine < 0 then
    Exit;

  fpgClipboard.Text := SelectionText;
end;

procedure TfpgMemo.SetSelectionText(const AText: TfpgString);
var
  s: TfpgString;
  si: TfpgString;       { beginning of line to cursor }
  si8: TfpgString;
  lineend: TfpgString;  { from cursor to end of line }
  n: integer;
  l: integer;
  lcnt: integer;
begin
  if ReadOnly then
    Exit;
  DeleteSelection;
  s := AText;

  si := UTF8Copy(CurrentLine,1,FCursorPos);
  lineend := UTF8Copy(CurrentLine,FCursorPos+1, UTF8Length(CurrentLine));
  if FCursorLine = -1 then  { first time in, FLines has no data yet }
    l := 0
  else
    l := FCursorLine;

  n := 1;
  lcnt := 0;
  si8 := '';
  while n <= length(s) do
  begin
    if (s[n] = #13) or (s[n] = #10) then
    begin
      if lcnt = 0 then
        SetLineText(l, si + si8)
      else
        FLines.Insert(l, si + si8);

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
    FLines.Insert(l, si);
    FCursorLine := l;
  end;

  AdjustCursor;
  ResetSelectionVariables;
  Repaint;
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

  if (FFirstLine + VisibleLines) > LineCount then
  begin
    FFirstLine := LineCount - VisibleLines + 1;
    if FFirstline < 0 then
      FFirstLine := 0;
  end;

  UpdateScrollbars;
end;

procedure TfpgMemo.UpdateScrollBars;
var
  vlines: integer;
  vsbw: integer;
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
    FVScrollBar.Min        := 0;
    // TODO: Look at calculation of vlines value to improve this!
    if LineCount > 0 then
    begin
      FVScrollBar.SliderSize := VisibleLines / LineCount;
      FVScrollBar.Max        := LineCount - VisibleLines;
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
  if LineCount = 0 then
    FLines.Add('');
  if (linenum >= 0) and (linenum < LineCount) then
    Result := FLines.Strings[linenum]
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
  FLines.Strings[linenum] := Value;
end;

function TfpgMemo.GetCursorX: integer;
begin
  Result := FFont.TextWidth(copy(CurrentLine, 1, FCursorPos));
end;

// Set cursor position by X
procedure TfpgMemo.SetCPByX(x: integer);
var
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

procedure TfpgMemo.HandleHide;
begin
  fpgCaret.UnSetCaret(Canvas);
  inherited;
end;

procedure TfpgMemo.RePaint;
begin
  if FUpdateCount <= 0 then
    inherited RePaint;
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
  Canvas.ClearClipRect;
  r.SetRect(0, 0, Width, Height);
  case BorderStyle of
    ebsNone:
        begin
          // do nothing
        end;
    ebsDefault:
        begin
          Canvas.DrawControlFrame(r);
          InflateRect(r, -2, -2);
        end;
    ebsSingle:
        begin
          Canvas.SetColor(clShadow2);
          Canvas.DrawRectangle(r);
          InflateRect(r, -1, -1);
        end;
  end;
  Canvas.SetClipRect(r);

  if Enabled and not ReadOnly then
    Canvas.SetColor(FBackgroundColor)
  else
    Canvas.SetColor(clWindowBackground);
  Canvas.FillRectAngle(r);

  Canvas.SetTextColor(FTextColor);
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
  for n := FFirstline to LineCount-1 do
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
      if (FSelEndLine > -1) and (selsl <= n) and (selel >= n) then
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

  // Special case because it never entered the for loop above
  if (LineCount = 0) and Focused then
    fpgCaret.SetCaret(Canvas, FSideMargin, 3, fpgCaret.Width, FFont.Height);

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
end;

procedure TfpgMemo.HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: boolean);
var
  prevval: string;
  s: string;
  ls: string;
begin
  inherited;
  prevval  := Text;
  s        := AText;

  if (not consumed) and (not ReadOnly) then
  begin
    // Printable characters only
    // Note: This is now UTF-8 compliant!
    if (Ord(AText[1]) > 31) and (Ord(AText[1]) < 127) or (Length(AText) > 1) then
    begin
      if (FMaxLength <= 0) or (UTF8Length(FLines.Text) < FMaxLength) then
      begin
        if FCursorLine < 0 then
          FCursorLine := 0;
        DeleteSelection;
        ls := GetLineText(FCursorLine);
        UTF8Insert(s, ls, FCursorPos + 1);
        SetLineText(FCursorLine, ls);
        Inc(FCursorPos);
        FSelStartPos  := FCursorPos;
        FSelStartLine := FCursorLine;
        FSelEndLine   := -1;
        AdjustCursor;
      end;

      consumed := True;
    end;

    if prevval <> Text then
      if Assigned(FOnChange) then
        FOnChange(self);
  end;


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
begin
  fpgApplication.HideHint;
  Consumed := True;
  hasChanged := False;
  case CheckClipBoardKey(keycode, shiftstate) of
    ckCopy:
        begin
          DoCopy;
        end;
    ckPaste:
        begin
          SetSelectionText(fpgClipboard.Text);
          if not ReadOnly then
            hasChanged := True;
        end;
    ckCut:
        begin
          DoCopy;
          DeleteSelection;
          if not ReadOnly then
          begin
            AdjustCursor;
            hasChanged := True;
          end;
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
        if FCursorLine > 0 then
        begin
          Dec(FCursorline);
          SetCPByX(cx);
        end;
      end;

      keyDown:
      begin
        cx := GetCursorX;
        if FCursorLine < (LineCount-1) then
        begin
          Inc(FCursorline);
          SetCPByX(cx);
        end;
      end;

      keyHome:
      begin
        if (ssCtrl in shiftstate) then
          FCursorLine := 0;
        FCursorPos := 0;
      end;

      keyEnd:
      begin
        if (ssCtrl in shiftstate) then
          FCursorLine := LineCount-1;
        FCursorPos := UTF8Length(CurrentLine);
      end;

      keyPageUp:
        if FCursorLine > 0 then
        begin
          cx := GetCursorX;
          Dec(FCursorLine, VisibleLines);
          if FCursorLine < 0 then
            FCursorLine := 0;
          SetCPByX(cx);
        end;

      keyPageDown:
      begin
        cx := GetCursorX;
        if FCursorLine < (LineCount-1) then
        begin
          Inc(FCursorline, VisibleLines);
          if FCursorLine > (LineCount-1) then
            FCursorLine := LineCount-1;
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
        ResetSelectionVariables;
    end;
  end;

  if (not Consumed) and (not ReadOnly) then
  begin
    consumed := True;

    case keycode of
      keyReturn,
      keyPEnter:
          begin
            ls  := UTF8Copy(FLines[FCursorline], 1, FCursorPos);
            ls2 := UTF8Copy(FLines[FCursorline], FCursorPos + 1, UTF8Length(FLines[FCursorline]));
            FLines.Insert(FCursorLine, ls);
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
            else if FCursorLine > 0 then
            begin
              ls := CurrentLine;
              FLines.Delete(FCursorLine);
              Dec(FCursorLine);
              FCursorPos := UTF8Length(FLines.Strings[FCursorLine]);
              FLines.Strings[FCursorLine] := FLines.Strings[FCursorLine] + ls;
            end;
            hasChanged := True;
          end;

      keyDelete:
          begin
            ls := GetLineText(FCursorLine);
            if SelectionText <> '' then
              DeleteSelection
            else if FCursorPos < UTF8Length(ls) then
            begin
              UTF8Delete(ls, FCursorPos + 1, 1);
              SetLineText(FCursorLine, ls);
            end
            else if FCursorLine < (LineCount-1) then
            begin
              ls2 := FLines.Strings[FCursorLine+1];
              FLines.Delete(FCursorLine);
              FLines.Strings[FCursorLine] := ls + ls2;
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
            end
            else
              Consumed := False;
          end;
      else
        Consumed := False;
    end;

    if Consumed then
    begin
      AdjustCursor;
      ResetSelectionVariables;
    end;
  end;

  if Consumed then
    RePaint
  else
    inherited;
    
  if hasChanged then
    if Assigned(FOnChange) then
      FOnChange(self);
end;

procedure TfpgMemo.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  n: integer;
  cpx: integer;
  cp: integer;
  cx: integer;
  lnum: integer;
  ls: string;
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  ResetSelectionVariables;

  // searching the appropriate character position
  lnum := FFirstLine + (y - FSideMargin) div LineHeight;
  if lnum > (LineCount-1) then
    lnum := LineCount-1;

  ls  := GetLineText(lnum);
  cpx := FFont.TextWidth(UTF8Copy(ls, 1, FCursorPos)) - FDrawOffset + FSideMargin;
  cp  := FCursorPos;

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
    FSelecting := True;
  end
  else
  begin
    FSelecting := False;
    FSelStartLine := lnum;
    FSelStartPos  := cp;
    FSelEndLine   := -1;
  end;
  Repaint;
end;

procedure TfpgMemo.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleRMouseUp(x, y, shiftstate);
  if Assigned(PopupMenu) then
    PopupMenu.ShowAt(self, x, y)
  else
    ShowDefaultPopupMenu(x, y, ShiftState);
end;

procedure TfpgMemo.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
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
  if lnum > LineCount-1 then
    lnum := LineCount-1;

  ls  := GetLineText(lnum);
  cpx := FFont.TextWidth(UTF8Copy(ls, 1, FCursorPos)) - FDrawOffset + FSideMargin;
  cp  := FCursorPos;

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
    FSelecting := True;
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

  if FFirstLine > LineCount - VisibleLines{ + 1} then
    FFirstLine := LineCount - VisibleLines {+ 1};
  if FFirstLine < 0 then
    FFirstLine := 0;

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

function TfpgMemo.GetSelectionText: TfpgString;
var
  n: integer;
  selsl: integer;
  selsp: integer;
  selel: integer;
  selep: integer;
  ls: string;
  len: integer;
  st: integer;
  s: TfpgString;
begin
  if FSelEndLine = -1 then { no text is selected }
  begin
    Result := '';
    Exit;
  end;

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
      s := s + LineEnding;

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

  Result := s;
end;

procedure TfpgMemo.CopyToClipboard;
begin
  DoCopy;
end;

procedure TfpgMemo.CutToClipboard;
begin
  DoCopy;
  DeleteSelection;
  AdjustCursor;
  ResetSelectionVariables;
  RePaint;
end;

procedure TfpgMemo.PasteFromClipboard;
begin
  SetSelectionText(fpgClipboard.Text);
end;

procedure TfpgMemo.Clear;
begin
  FLines.Clear;
  { not sure if all of these are required }
  FFirstLine    := 0;
  FCursorLine   := 0;
  FCursorPos    := 0;
  FSelStartPos  := FCursorPos;
  FSelEndPos    := 0;
  FSelStartLine := -1;
  FSelEndLine   := -1;
  FDrawOffset   := 0;

  Repaint;
end;

procedure TfpgMemo.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TfpgMemo.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
  begin
    Invalidate;
    UpdateScrollBars;
  end;
end;

function TfpgMemo.GetText: TfpgString;
var
  n: integer;
  s: TfpgString;
begin
  s := '';
  for n := 0 to LineCount-1 do
  begin
    if n > 0 then
      s := s + LineEnding;
    s := s + GetLineText(n);
  end;
  Result := s;
end;

procedure TfpgMemo.SetText(const AValue: TfpgString);
var
  n: integer;
  c: TfpgChar;
  s: TfpgString;
begin
  FLines.Clear;
  s := '';
  n := 1;

  FLines.Text := AValue;
  //while n <= UTF8Length(AValue) do
  //begin
  //  c := UTF8Copy(AValue, n, 1);
  //  if (c[1] = #13) or (c[1] = #10) then
  //  begin
  //    FLines.Add(s);
  //    s := '';
  //    c := UTF8Copy(AValue, n + 1, 1);
  //    if c[1] = #10 then
  //      Inc(n);
  //  end
  //  else
  //    s := s + c;
  //  Inc(n);
  //end;
  //
  //if s <> '' then
  //  FLines.Add(s);

  FDrawOffset   := 0;
  FCursorPos    := 0;
  FCursorLine   := 0;
  FSelStartLine := FCursorLine;
  FSelStartPos  := FCursorPos;
  FSelEndLine   := -1;

  AdjustCursor;
  Repaint;
end;

end.

