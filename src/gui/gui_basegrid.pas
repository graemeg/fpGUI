{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2008 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a Base Grid control. Usable as the base for any grid type of
      component.
}

unit gui_basegrid;

{$mode objfpc}{$H+}

{.$Define DEBUG}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_widget,
  gui_scrollbar;
  
type

  TfpgGridDrawState = set of (gdSelected, gdFocused, gdFixed);

  TfpgFocusChangeNotify = procedure(Sender: TObject; ARow, ACol: Integer) of object;
  TfpgRowChangeNotify = procedure(Sender: TObject; ARow: Integer) of object;
  TfpgCanSelectCellEvent = procedure(Sender: TObject; const ARow, ACol: Integer; var ACanSelect: boolean) of object;
  TfpgDrawCellEvent = procedure(Sender: TObject; const ARow, ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean) of object;

  // Column 2 is special just for testing purposes. Descendant classes will
  // override that special behavior anyway.

  { TfpgBaseGrid }

  TfpgBaseGrid = class(TfpgWidget)
  private
    FColResizing: boolean;
    FDragPos: integer;      // used for column resizing
    FOnDrawCell: TfpgDrawCellEvent;
    FResizedCol: integer;   // used for column resizing
    FDefaultColWidth: integer;
    FDefaultRowHeight: integer;
    FFocusCol: Integer;
    FFocusRow: Integer;
    FHeaderHeight: integer;
    FOnCanSelectCell: TfpgCanSelectCellEvent;
    FOnFocusChange: TfpgFocusChangeNotify;
    FOnRowChange: TfpgRowChangeNotify;
    FPrevCol: Integer;
    FPrevRow: Integer;
    FFirstRow: Integer;
    FFirstCol: Integer;
    FMargin: integer;
    FFont: TfpgFont;
    FHeaderFont: TfpgFont;
    FRowSelect: boolean;
    FScrollBarStyle: TfpgScrollStyle;
    FShowGrid: boolean;
    FShowHeader: boolean;
    FTemp: integer;
    FVScrollBar: TfpgScrollBar;
    FHScrollBar: TfpgScrollBar;
    FUpdateCount: integer;
    function    GetFontDesc: string;
    function    GetHeaderFontDesc: string;
    procedure   HScrollBarMove(Sender: TObject; position: integer);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetHeaderFontDesc(const AValue: string);
    procedure   SetRowSelect(const AValue: boolean);
    procedure   SetScrollBarStyle(const AValue: TfpgScrollStyle);
    procedure   VScrollBarMove(Sender: TObject; position: integer);
    procedure   SetDefaultColWidth(const AValue: integer);
    procedure   SetDefaultRowHeight(const AValue: integer);
    procedure   SetFocusCol(const AValue: Integer);
    procedure   SetFocusRow(const AValue: Integer);
    procedure   CheckFocusChange;
    procedure   SetShowGrid(const AValue: boolean);
    procedure   SetShowHeader(const AValue: boolean);
    function    VisibleLines: Integer;
    function    VisibleWidth: integer;
    function    VisibleHeight: integer;
    procedure   SetFirstRow(const AValue: Integer);
  protected
    property    UpdateCount: integer read FUpdateCount;
    procedure   UpdateScrollBars; virtual;
    function    GetHeaderText(ACol: Integer): string; virtual;
    function    GetColumnWidth(ACol: Integer): integer; virtual;
    procedure   SetColumnWidth(ACol: Integer; const AValue: integer); virtual;
    function    GetColumnBackgroundColor(ACol: Integer): TfpgColor; virtual;
    procedure   SetColumnBackgroundColor(ACol: Integer; const AValue: TfpgColor); virtual;
    function    GetColumnTextColor(ACol: Integer): TfpgColor; virtual;
    procedure   SetColumnTextColor(ACol: Integer; const AValue: TfpgColor); virtual;
    function    GetColumnCount: Integer; virtual;
    function    GetRowCount: Integer; virtual;
    function    CanSelectCell(const ARow, ACol: Integer): boolean;
    function    DoDrawCellEvent(ARow, ACol: Integer; ARect: TfpgRect; AFlags: TfpgGridDrawState): boolean; virtual;
    procedure   DoCanSelectCell(const ARow, ACol: Integer; var ACanSelect: boolean);
    procedure   DrawCell(ARow, ACol: Integer; ARect: TfpgRect; AFlags: TfpgGridDrawState); virtual;
    procedure   DrawHeader(ACol: Integer; ARect: TfpgRect; AFlags: integer); virtual;
    procedure   DrawGrid(ARow, ACol: Integer; ARect: TfpgRect; AFlags: integer); virtual;
    procedure   HandlePaint; override;
    procedure   HandleShow; override;
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   FollowFocus; virtual;
    property    DefaultColWidth: integer read FDefaultColWidth write SetDefaultColWidth default 64;
    property    DefaultRowHeight: integer read FDefaultRowHeight write SetDefaultRowHeight;
    property    Font: TfpgFont read FFont;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    HeaderFont: TfpgFont read FHeaderFont;
    property    HeaderFontDesc: string read GetHeaderFontDesc write SetHeaderFontDesc;
    property    FocusCol: Integer read FFocusCol write SetFocusCol;
    property    FocusRow: Integer read FFocusRow write SetFocusRow;
    property    RowSelect: boolean read FRowSelect write SetRowSelect;
    property    ColumnCount: Integer read GetColumnCount;
    property    RowCount: Integer read GetRowCount;
    property    ShowHeader: boolean read FShowHeader write SetShowHeader default True;
    property    ShowGrid: boolean read FShowGrid write SetShowGrid default True;
    property    ScrollBarStyle: TfpgScrollStyle read FScrollBarStyle write SetScrollBarStyle default ssAutoBoth;
    property    HeaderHeight: integer read FHeaderHeight;
//    property    ColResizing: boolean read FColResizing write FColResizing;
    property    ColumnWidth[ACol: Integer]: integer read GetColumnWidth write SetColumnWidth;
    property    ColumnBackgroundColor[ACol: Integer]: TfpgColor read GetColumnBackgroundColor write SetColumnBackgroundColor;
    property    ColumnTextColor[ACol: Integer]: TfpgColor read GetColumnTextColor write SetColumnTextColor;
    property    TopRow: Integer read FFirstRow write SetFirstRow;
    property    OnDrawCell: TfpgDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property    OnFocusChange: TfpgFocusChangeNotify read FOnFocusChange write FOnFocusChange;
    property    OnRowChange: TfpgRowChangeNotify read FOnRowChange write FOnRowChange;
    property    OnCanSelectCell: TfpgCanSelectCellEvent read FOnCanSelectCell write FOnCanSelectCell;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterConstruction; override;
    procedure   Update;
    procedure   BeginUpdate;
    procedure   EndUpdate;
    procedure   MouseToCell(X, Y: Integer; var ACol, ARow: Integer);
  end;

implementation

{ TfpgBaseGrid }

procedure TfpgBaseGrid.HScrollBarMove(Sender: TObject; position: integer);
begin
  if FFirstCol <> position then
  begin
    if Position < 0 then
      Position := 0;
    FFirstCol := position;
    RePaint;
  end;
end;

function TfpgBaseGrid.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

function TfpgBaseGrid.GetHeaderFontDesc: string;
begin
  Result := FHeaderFont.FontDesc;
end;

procedure TfpgBaseGrid.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  if DefaultRowHeight < FFont.Height + 2 then
    DefaultRowHeight := FFont.Height + 2;
  RePaint;
end;

procedure TfpgBaseGrid.SetHeaderFontDesc(const AValue: string);
begin
  FHeaderFont.Free;
  FHeaderFont := fpgGetFont(AValue);
  if FHeaderHeight < FHeaderFont.Height + 2 then
    FHeaderHeight := FHeaderFont.Height + 2;
  RePaint;
end;

procedure TfpgBaseGrid.SetRowSelect(const AValue: boolean);
begin
  if FRowSelect = AValue then
    Exit; //==>
  FRowSelect := AValue;
  RePaint;
end;

procedure TfpgBaseGrid.SetScrollBarStyle(const AValue: TfpgScrollStyle);
begin
  if FScrollBarStyle = AValue then
    Exit; //==>
  FScrollBarStyle := AValue;
end;

procedure TfpgBaseGrid.VScrollBarMove(Sender: TObject; position: integer);
begin
  if FFirstRow <> position then
  begin
    FFirstRow := position;
    RePaint;
  end;
end;

procedure TfpgBaseGrid.SetDefaultColWidth(const AValue: integer);
begin
  if FDefaultColWidth = AValue then
    Exit; //==>
  FDefaultColWidth := AValue;
  RePaint;
end;

procedure TfpgBaseGrid.SetDefaultRowHeight(const AValue: integer);
begin
  if FDefaultRowHeight = AValue then
    Exit; //==>
  FDefaultRowHeight := AValue;
  RePaint;
end;

function TfpgBaseGrid.GetColumnWidth(ACol: Integer): integer;
begin
  Result := 50;
end;

procedure TfpgBaseGrid.SetColumnWidth(ACol: Integer; const AValue: integer);
begin
  // GetColumnWidth and SetColumnWidth will be overriden in decendant!
  // Column 2 is special just for testing purposes
  if (ACol = 2) and (AValue <> FTemp) then
  begin
    FTemp := AValue;
    UpdateScrollBars;
    Repaint;
  end;
end;

function TfpgBaseGrid.GetColumnBackgroundColor(ACol: Integer): TfpgColor;
begin
  // implemented in descendant
end;

procedure TfpgBaseGrid.SetColumnBackgroundColor(ACol: Integer; const AValue: TfpgColor);
begin
  // implemented in descendant
end;

function TfpgBaseGrid.GetColumnTextColor(ACol: Integer): TfpgColor;
begin
  // implemented in descendant
end;

procedure TfpgBaseGrid.SetColumnTextColor(ACol: Integer; const AValue: TfpgColor);
begin
  // implemented in descendant
end;

function TfpgBaseGrid.GetColumnCount: Integer;
begin
  Result := 7;
end;

function TfpgBaseGrid.GetRowCount: Integer;
begin
  Result := 24;
end;

function TfpgBaseGrid.CanSelectCell(const ARow, ACol: Integer): boolean;
begin
  Result := (ARow >= 0) and (ACol >= 0) and (ARow < RowCount) and (ACol < ColumnCount);
  if Result then
    DoCanSelectCell(ARow, ACol, Result);
end;

function TfpgBaseGrid.DoDrawCellEvent(ARow, ACol: Integer; ARect: TfpgRect;
  AFlags: TfpgGridDrawState): boolean;
begin
  Result := True;
  if Assigned(OnDrawCell) then
    FOnDrawCell(self, ARow, ACol, ARect, AFlags, Result);
end;

procedure TfpgBaseGrid.DoCanSelectCell(const ARow, ACol: Integer; var
  ACanSelect: boolean);
begin
  if Assigned(OnCanSelectCell) then
    FOnCanSelectCell(self, ARow, ACol, ACanSelect);
end;

procedure TfpgBaseGrid.DrawCell(ARow, ACol: Integer; ARect: TfpgRect; AFlags: TfpgGridDrawState);
var
  s: string;
begin
  s := 'c(' + IntToStr(ARow) + ',' + IntToStr(ACol) + ')';
  if (ARow = 5) and (ACol = 2) then
    s := 'Here lives Graeme!';
  if not Enabled then
    Canvas.SetTextColor(clShadow1);
  Canvas.DrawText(ARect, s, [txtHCenter, txtVCenter]);
end;

procedure TfpgBaseGrid.DrawHeader(ACol: Integer; ARect: TfpgRect; AFlags: integer);
var
  s: string;
  r: TfpgRect;
  x: integer;
begin
  // Here we can implement a head style check
  Canvas.DrawButtonFace(ARect, [btfIsEmbedded]);
  r := ARect;
  InflateRect(r, -2, -2);
  Canvas.AddClipRect(r);  // text may not overshoot header border
(*
  // drawing grid lines
  Canvas.SetColor(clGridLines);
  Canvas.DrawLine(r.Left, r.Bottom+1, r.Right+1, r.Bottom+1);  // horizontal bottom
  Canvas.DrawLine(r.Right+1, r.Top, r.Right+1, r.Bottom+1);    // vertical right

  if (ACol mod 2) = 0 then
    Canvas.SetColor(clGridHeader)
  else
    Canvas.SetColor(clMagenta);
  Canvas.FillRectangle(ARect);
*)

  Canvas.SetTextColor(clText1);
  s := GetHeaderText(ACol);
  x := (ARect.Left + (ARect.Width div 2)) - (FHeaderFont.TextWidth(s) div 2);
  if x < 1 then
    x := 1;
  fpgStyle.DrawString(Canvas, x, ARect.Top+1, s, Enabled);
end;

procedure TfpgBaseGrid.DrawGrid(ARow, ACol: Integer; ARect: TfpgRect;
  AFlags: integer);
begin
  // default is inside bottom/right edge or cell
  Canvas.SetColor(clGridLines);
  Canvas.DrawLine(ARect.Left, ARect.Bottom, ARect.Right, ARect.Bottom); // cell bottom
  Canvas.DrawLine(ARect.Right, ARect.Bottom, ARect.Right, ARect.Top-1); // cell right
end;

procedure TfpgBaseGrid.SetFocusCol(const AValue: Integer);
begin
  if FFocusCol = AValue then
    Exit; //==>
  FFocusCol := AValue;

  // apply min/max limit
  if FFocusCol < 0 then
    FFocusCol := 0;
  if FFocusCol > ColumnCount-1 then
    FFocusCol := ColumnCount-1;

  FollowFocus;
  CheckFocusChange;
end;

procedure TfpgBaseGrid.SetFocusRow(const AValue: Integer);
begin
  if FFocusRow = AValue then
    Exit; //==>
  FFocusRow := AValue;

  // apply min/max limit
  if FFocusRow < 0 then
    FFocusRow := 0;
  if FFocusRow > RowCount-1 then
    FFocusRow := RowCount-1;
    
  FollowFocus;
  CheckFocusChange;
end;

procedure TfpgBaseGrid.CheckFocusChange;
begin
  if ((FPrevCol <> FFocusCol) and not RowSelect) or (FPrevRow <> FFocusRow) then
    if Assigned(FOnFocusChange) then
      FOnFocusChange(self, FFocusRow, FFocusCol);

  if (FPrevRow <> FFocusRow) then
    if Assigned(FOnRowChange) then
      FOnRowChange(self, FFocusRow);

  FPrevCol := FFocusCol;
  FPrevRow := FFocusRow;
end;

procedure TfpgBaseGrid.SetShowGrid(const AValue: boolean);
begin
  if FShowGrid = AValue then
    Exit; //==>
  FShowGrid := AValue;
  RePaint;
end;

procedure TfpgBaseGrid.SetShowHeader(const AValue: boolean);
begin
  if FShowHeader = AValue then
    Exit; //==>
  FShowHeader := AValue;
  UpdateScrollBars;
  RePaint;
end;

// Return the fully visible lines only. Partial lines not counted
function TfpgBaseGrid.VisibleLines: Integer;
var
  hh: integer;
begin
  if FHScrollBar.Visible then
    hh := FHScrollbar.Height
  else
    hh := 0;
  if ShowHeader then
    hh := hh + FHeaderHeight+1;
  Result := (Height - (2*FMargin) - hh) div FDefaultRowHeight;
end;

function TfpgBaseGrid.VisibleWidth: integer;
var
  sw: integer;
begin
  if FVScrollBar.Visible then
    sw := FVScrollBar.Width-1
  else
    sw := 0;
  Result := Width - (FMargin*2) - sw;
end;

function TfpgBaseGrid.VisibleHeight: integer;
var
  sw: integer;
begin
  if FHScrollBar.Visible then
    sw := FHScrollBar.Height-1
  else
    sw := 0;
  Result := Height - (FMargin*2) - sw;
end;

procedure TfpgBaseGrid.SetFirstRow(const AValue: Integer);
begin
  if FFirstRow = AValue then
    Exit; //==>
  if AValue < ((RowCount - VisibleLines)) then
    FFirstRow := AValue
  else
    FFirstRow := (RowCount - VisibleLines);
  UpdateScrollBars;
  RePaint;
end;

procedure TfpgBaseGrid.UpdateScrollBars;
var
  HWidth: integer;
  VHeight: integer;
  vw: integer;
  cw: integer;
  i: integer;
begin
  VHeight := Height - 4;
  HWidth  := Width - 4;
  
  vw := VisibleWidth;
  cw := 0;
  for i := 0 to ColumnCount-1 do
    cw := cw + ColumnWidth[i];

  // This needs improving while resizing
  if cw > vw then
    FHScrollBar.Visible := not (FScrollBarStyle in [ssNone, ssVertical])
  else
  begin
    FHScrollBar.Visible := False;
    FFirstCol := 0;
  end;
  
  // This needs improving while resizing
  if (RowCount > VisibleLines) then
    FVScrollBar.Visible := not (FScrollBarStyle in [ssNone, ssHorizontal])
  else
  begin
    FVScrollBar.Visible := False;
    FFirstRow := 0;
  end;

  if FVScrollBar.Visible then
  begin
    Dec(HWidth, FVScrollBar.Width);
    FVScrollBar.Min         := 0;
    if RowCount > 0 then
      FVScrollBar.SliderSize := VisibleLines / RowCount
    else
      FVScrollBar.SliderSize := 0;
    FVScrollBar.Max         := RowCount-VisibleLines;
    FVScrollBar.Position    := FFirstRow;
    FVScrollBar.RepaintSlider;
  end;
  
  if FHScrollBar.Visible then
  begin
    Dec(VHeight, FHScrollBar.Height);
    FHScrollBar.Min         := 0;
    FHScrollBar.SliderSize  := 0.2;
    FHScrollBar.Max         := ColumnCount-1;
    FHScrollBar.Position    := FFirstCol;
    FHScrollBar.RepaintSlider;
  end;

  FHScrollBar.Top     := Height -FHScrollBar.Height - 2;
  FHScrollBar.Left    := 2;
  FHScrollBar.Width   := HWidth;

  FVScrollBar.Top     := 2;
  FVScrollBar.Left    := Width - FVScrollBar.Width - 2;
  FVScrollBar.Height  := VHeight;

  FVScrollBar.UpdateWindowPosition;
  FHScrollBar.UpdateWindowPosition;
end;

function TfpgBaseGrid.GetHeaderText(ACol: Integer): string;
begin
  Result := 'Head ' + IntToStr(ACol);
end;

procedure TfpgBaseGrid.HandlePaint;
var
  r: TfpgRect;
  r2: TfpgRect;
  col: Integer;
  row: Integer;
  clipr: TfpgRect;   // clip rectangle
  drawstate: TfpgGridDrawState;
begin
  drawstate := [];
  Canvas.BeginDraw;
//  inherited HandlePaint;
  Canvas.ClearClipRect;

  r.SetRect(0, 0, Width, Height);
  Canvas.DrawControlFrame(r);

  InflateRect(r, -2, -2);
  Canvas.SetClipRect(r);
  Canvas.SetColor(FBackgroundColor);
  Canvas.FillRectangle(r);

  clipr.SetRect(FMargin, FMargin, VisibleWidth, VisibleHeight);
  r := clipr;

  if (ColumnCount > 0) and ShowHeader then
  begin
    // Drawing horizontal headers
    r.Height := FHeaderHeight;
    Canvas.SetFont(FHeaderFont);
    for col := FFirstCol to ColumnCount-1 do
    begin
      r.Width := ColumnWidth[col];
      Canvas.SetClipRect(clipr);
      Canvas.AddClipRect(r);
      DrawHeader(col, r, 0);
      inc(r.Left, r.Width);
      if r.Left >= clipr.Right then
        Break;  // small optimization. Don't draw what we can't see
    end;
    inc(r.Top, r.Height);
  end;

  if (RowCount > 0) and (ColumnCount > 0) then
  begin
    // Drawing cells
    r.Height := DefaultRowHeight;
    Canvas.SetFont(FFont);

    for row := FFirstRow to RowCount-1 do
    begin
      r.Left := FMargin;
      for col := FFirstCol to ColumnCount-1 do
      begin
        r.Width := ColumnWidth[col];
        Canvas.SetClipRect(clipr);

        if (row = FFocusRow) and (RowSelect or (col = FFocusCol)) then
        begin
          if FFocused then
          begin
            Canvas.SetColor(clSelection);
            Canvas.SetTextColor(clSelectionText);
          end
          else
          begin
            Canvas.SetColor(clInactiveSel);
            Canvas.SetTextColor(clInactiveSelText);
          end;
        end
        else
        begin
          Canvas.SetColor(ColumnBackgroundColor[col]);
          Canvas.SetTextColor(ColumnTextColor[col]);
        end;
        Canvas.AddClipRect(r);
        Canvas.FillRectangle(r);
        // setup drawstate
        if FFocused then
          Include(drawstate, gdFocused);
        if (row = FFocusRow) and (col = FFocusCol) then
          Include(drawstate, gdSelected);
          
        if DoDrawCellEvent(row, col, r, drawstate) then
          DrawCell(row, col, r, drawstate);

        // drawing grid lines
        if FShowGrid then
          DrawGrid(row, col, r, 0);

        inc(r.Left, r.Width);
        if r.Left >= clipr.Right then
          Break;  // small optimization. Don't draw what we can't see
      end;
//      Inc(r.Top, FDefaultRowHeight+1);
      inc(r.Top, r.Height);
      if r.Top >= clipr.Bottom then
        break;
    end;
  end; // item drawing

  Canvas.SetClipRect(clipr);
  Canvas.SetColor(FBackgroundColor);
  
  // clearing after the last column
  if r.Left <= clipr.Right then
  begin
    r2.Left   := r.Left;
    r2.Top    := clipr.Top;
    r2.SetRight(clipr.Right);
    r2.Height := clipr.Height;
    Canvas.FillRectangle(r2);
  end;

  // clearing after the last row
  if r.Top <= clipr.Bottom then
  begin
    r.Left    := clipr.Left;
    r.Width   := clipr.Width;
    r.SetBottom(clipr.Bottom);
    Canvas.FillRectangle(r);
  end;

  // The little square in the bottom right corner
  if FHScrollBar.Visible and FVScrollBar.Visible then
  begin
    Canvas.ClearClipRect;
    Canvas.SetColor(clButtonFace);
    Canvas.FillRectangle(FHScrollBar.Left+FHScrollBar.Width,
                         FVScrollBar.Top+FVScrollBar.Height,
                         FVScrollBar.Width,
                         FHScrollBar.Height);
  end;

  Canvas.EndDraw;
end;

procedure TfpgBaseGrid.HandleShow;
begin
  inherited HandleShow;
  if (csLoading in ComponentState) then
    Exit;
  UpdateScrollBars;
end;

procedure TfpgBaseGrid.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);
  if (csLoading in ComponentState) then
    Exit;
  UpdateScrollBars;
end;

procedure TfpgBaseGrid.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  w: integer;
  r: integer;
begin
  consumed := True;
  case keycode of
    keyRight:
        begin
          if RowSelect then
          begin
            w := 0;
            FFocusCol := FFirstCol;
            while FFocusCol < ColumnCount do
            begin
              inc(w, ColumnWidth[FFocusCol]+1);
              if w + ColumnWidth[FFocusCol+1]+1 > VisibleWidth then
                Break;
              inc(FFocusCol);
            end;
          end;

          if CanSelectCell(FFocusRow, FFocusCol+1) then
          begin
            inc(FFocusCol);
            FollowFocus;
            RePaint;
          end;
        end;

    keyLeft:
        begin
          if RowSelect then
            FFocusCol := FFirstCol;
          if CanSelectCell(FFocusRow, FFocusCol-1) then
          begin
            dec(FFocusCol);
            FollowFocus;
            RePaint;
          end;
        end;

    keyUp:
        begin
          if CanSelectCell(FFocusRow-1, FFocusCol) then
          begin
            dec(FFocusRow);
            FollowFocus;
            RePaint;
          end;
        end;

    keyDown:
        begin
          if CanSelectCell(FFocusRow+1, FFocusCol) then
          begin
            inc(FFocusRow);
            FollowFocus;
            RePaint;
          end;
        end;

    keyPageUp:
        begin
          r := FFocusRow-VisibleLines;
          if r < 0 then
            r := 0;

          if (FFocusRow <> 0) and CanSelectCell(r, FFocusCol) then
          begin
            FFocusRow := r;
            FollowFocus;
            RePaint;
          end;
        end;

    keyPageDown:
        begin
          r := FFocusRow+VisibleLines;
          if r > (RowCount-1) then
            r := RowCount-1;

          if (FFocusRow <> (RowCount-1)) and CanSelectCell(r, FFocusCol) then
          begin
            FFocusRow := r;
            FollowFocus;
            RePaint;
          end;
        end;
        
    keyHome:
        begin
          if FRowSelect then
          begin
            if (FFocusRow <> 0) and CanSelectCell(0, FFocusCol) then
            begin
              FFocusRow := 0;
              FollowFocus;
              RePaint;
            end;
          end
          else if (FFocusCol <> 0) and CanSelectCell(FFocusRow, 0) then
          begin
            FFocusCol := 0;
            FollowFocus;
            RePaint;
          end;
        end;
        
    keyEnd:
        begin
          if FRowSelect then
          begin
            if (FFocusRow <> (RowCount-1)) and CanSelectCell(RowCount-1, FFocusCol) then
            begin
              FFocusRow := RowCount-1;
              FollowFocus;
              RePaint;
            end;
          end
          else if (FFocusCol <> (ColumnCount-1)) and CanSelectCell(FFocusRow, ColumnCount-1) then
          begin
            FFocusCol := ColumnCount-1;
            FollowFocus;
            RePaint;
          end;
        end;

  else
    consumed := False;
  end;
  
  if consumed then
    CheckFocusChange;

  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfpgBaseGrid.HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint);
var
  lRow: Integer;
  lCol: Integer;
begin
  inherited HandleMouseScroll(x, y, shiftstate, delta);

  lRow := FFirstRow;
  lCol := FFirstCol;

  if delta > 0 then // scroll down
    inc(FFirstRow, abs(delta))
  else              // scroll up
    if FFirstRow > 0 then
      dec(FFirstRow, abs(delta));

  // apply limits
  if FFirstRow > RowCount - VisibleLines then
    FFirstRow := RowCount - VisibleLines;
  if FFirstRow < 0 then
    FFirstRow := 0;
    
  // scroll left/right
  // If vertical scrollbar is not visible, but
  // horizontal is. Mouse wheel will scroll horizontally.  :)
  if FHScrollBar.Visible and (not FVScrollBar.Visible) then
  begin
    if delta > 0 then // scroll right
    begin
      if FFirstCol < (ColumnCount-1) then
        inc(FFirstCol);
    end
    else
    begin
      if FFirstCol > 0 then
        dec(FFirstCol);
    end;
  end;

  if (lRow <> FFirstRow) or (lCol <> FFirstCol) then
  begin
    UpdateScrollBars;
    RePaint;
  end;
end;

procedure TfpgBaseGrid.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  hh: integer;
  cw: integer;
  n: integer;
  colresize: boolean;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);
  
  if (ColumnCount = 0) or (RowCount = 0) then
    Exit; //==>

  if FColResizing then
  begin
    if (btnstate and 1) = 0 then
      FColResizing := False
    else
    begin
      cw := ColumnWidth[FResizedCol]+x-FDragPos;
      if cw < 1 then
        cw := 1;
      SetColumnWidth(FResizedCol, cw);
      FDragPos := x;
    end;
  end
  else if ShowHeader then
  begin
    colresize := False;
    hh := FHeaderHeight;

    if (y <= FMargin + hh) then // we are over the Header row
    begin
      cw := 0;
      for n := FFirstCol to ColumnCount-1 do
      begin
        inc(cw, ColumnWidth[n]);
        // Resizing is enabled 4 pixel either way of the cell border
        if ((x >= (FMargin+cw - 4)) and (x <= (FMargin+cw+4))) or
           (cw > (FMargin + VisibleWidth)) and (x >= FMargin + VisibleWidth-4)   then
        begin
          colresize := True;
          Break;
        end;

        if cw > VisibleWidth then
          Break;
      end;  { if }
    end;  { if/else }

    if colresize then
      MouseCursor := mcSizeEW
    else
      MouseCursor := mcDefault;
  end;  { if/else }
end;

procedure TfpgBaseGrid.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);

  {$IFDEF DEBUG}
  if FColResizing then
    Writeln('Column ', FResizedCol,' width = ', ColumnWidth[FResizedCol]);
  {$ENDIF}

  FColResizing  := False;
  MouseCursor   := mcDefault;
end;

procedure TfpgBaseGrid.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  hh: integer;
  n: Integer;
  cw: integer;
  nw: integer;
  prow: Integer;
  pcol: Integer;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  if (ColumnCount = 0) or (RowCount = 0) then
    Exit; //==>

  pcol := FFocusCol;
  prow := FFocusRow;

  // searching for the appropriate character position
  if ShowHeader then
    hh := FHeaderHeight+1
  else
    hh := 0;

  if ShowHeader and (y <= FMargin+hh) then  // inside Header row
  begin
    {$IFDEF DEBUG} Writeln('header click...'); {$ENDIF}
    cw := 0;
    for n := FFirstCol to ColumnCount-1 do
    begin
      inc(cw, ColumnWidth[n]);
      if (x >= (FMargin+cw - 4)) and (x <= (FMargin+cw + 4)) then
      begin
        {$IFDEF DEBUG} Writeln('column resize...'); {$ENDIF}
        FColResizing  := True;
        FResizedCol   := n;
        FDragPos      := x;
        Break;
      end
      else if (cw > FMargin+VisibleWidth) and (x >= FMargin+VisibleWidth-4) then
      begin
        FColResizing  := True;
        FResizedCol   := n;
        FDragPos      := x;
        nw := ColumnWidth[FResizedCol] - (cw+FMargin-x);
        if nw > 0 then
          SetColumnWidth(FResizedCol, nw );
        Break;
      end;  { if/else }

      if cw > VisibleWidth then
        Break;
    end;  { for }
  end
  else
  begin   // Selecting a Cell via mouse
    MouseToCell(x, y, FFocusCol, FFocusRow);
  end;  { if/else }
  
  if not CanSelectCell(FFocusRow, FFocusCol) then
  begin
    // restore previous values
    FFocusRow := prow;
    FFocusCol := pcol;
  end;

  if (prow <> FFocusRow) or (pcol <> FFocusCol) then
  begin
    FollowFocus;
    Repaint;
  end;

  if FColResizing then
    MouseCursor := mcSizeEW;

  CheckFocusChange;
end;

procedure TfpgBaseGrid.FollowFocus;
var
  n: Integer;
  w: TfpgCoord;
begin
  if (RowCount > 0) and (FFocusRow < 0) then
    FFocusRow := 0;
  if FFocusRow > RowCount-1 then
    FFocusRow := RowCount-1;

  if (ColumnCount > 0) and (FFocusCol < 0) then
    FFocusCol := 0;
  if FFocusCol > ColumnCount-1 then
    FFocusCol := ColumnCount-1;

  if FFirstRow < 0 then
    FFirstRow := 0;
  if FFirstCol < 0 then
    FFirstCol := 0;

  if FFocusRow < FFirstRow then
    FFirstRow := FFocusRow
  else
  begin
    if (FFirstRow + VisibleLines) <= FFocusRow then
      FFirstRow := (FFocusRow - VisibleLines) + 1;  // scroll last partial row into view
  end;  { if/else }

  if FFocusCol < FFirstCol then
    FFirstCol := FFocusCol
  else
  begin
    w := 0;
    for n := FFocusCol downto FFirstCol do
    begin
      w := w + ColumnWidth[n]+1;
      if w > VisibleWidth then
      begin
        if n = FFocusCol then
          FFirstCol := n
        else
          FFirstCol := n+1;
        break;
      end;
    end;  { for }
  end;  { if/else }

  UpdateScrollBars;
end;

constructor TfpgBaseGrid.Create(AOwner: TComponent);
begin
  Updating;
  inherited Create(AOwner);
  Focusable   := True;
  Width       := 120;
  Height      := 80;
  FFocusCol   := 0;
  FPrevCol    := -1;
  FFocusRow   := 0;
  FPrevRow    := -1;
  FFirstRow   := 0;
  FFirstCol   := 0;
  FMargin     := 2;
  FShowHeader := True;
  FShowGrid   := True;
  FRowSelect  := False;
  FScrollBarStyle := ssAutoBoth;
  FUpdateCount    := 0;

  FFont       := fpgGetFont('#Grid');
  FHeaderFont := fpgGetFont('#GridHeader');
  
  FTemp             := 50;  // Just to prove that ColumnWidth does adjust.
  FDefaultColWidth  := 64;
  FDefaultRowHeight := FFont.Height + 2;
  FHeaderHeight     := FHeaderFont.Height + 2;
  FBackgroundColor  := clBoxColor;
  FColResizing      := False;

  MinHeight   := HeaderHeight + DefaultRowHeight + FMargin;
  MinWidth    := DefaultColWidth + FMargin;
  
  FVScrollBar := TfpgScrollBar.Create(self);
  FVScrollBar.Orientation := orVertical;
  FVScrollBar.Visible     := False;
  FVScrollBar.OnScroll    := @VScrollBarMove;

  FHScrollBar := TfpgScrollBar.Create(self);
  FHScrollBar.Orientation := orHorizontal;
  FHScrollBar.Visible     := False;
  FHScrollBar.OnScroll    := @HScrollBarMove;
  FHScrollBar.ScrollStep  := 5;
end;

destructor TfpgBaseGrid.Destroy;
begin
  FOnRowChange := nil;
  FOnFocusChange := nil;
  FFont.Free;
  FHeaderFont.Free;
  inherited Destroy;
end;

procedure TfpgBaseGrid.AfterConstruction;
begin
  inherited AfterConstruction;
  Updated;
end;

procedure TfpgBaseGrid.Update;
begin
  UpdateScrollBars;
  FollowFocus;
  RePaint;
end;

procedure TfpgBaseGrid.BeginUpdate;
begin
  Inc(FUpdateCount);
  Updating;
end;

procedure TfpgBaseGrid.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      Updated;
      RePaint;
    end;
  end;
end;

procedure TfpgBaseGrid.MouseToCell(X, Y: Integer; var ACol, ARow: Integer);
var
  hh: integer;
  cw: integer;
  n: Integer;
begin
  if ShowHeader then
    hh := FHeaderHeight+1
  else
    hh := 0;

  ARow := FFirstRow + ((y - FMargin - hh) div FDefaultRowHeight);
  if ARow > RowCount-1 then
    ARow := RowCount-1;

  cw := 0;
  for n := FFirstCol to ColumnCount-1 do
  begin
    inc(cw, ColumnWidth[n]);
    if FMargin+cw >= x then
    begin
      ACol := n;
      Break;
    end;
  end;
end;


end.

