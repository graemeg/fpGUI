unit gui_basegrid;

{$mode objfpc}{$H+}

{
  TODO:
    * Selecting the last fully visible row, scrolls the grid. Selection
      is corruct, but because of the scroll it is confusing.
}

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

  TfpgFocusChangeNotify = procedure(Sender: TObject; ARow, ACol: integer) of object;
  TfpgRowChangeNotify = procedure(Sender: TObject; ARow: integer) of object;


  // Column 2 is special just for testing purposes. Descendant classes will
  // override that special behavior anyway.
  TfpgBaseGrid = class(TfpgWidget)
  private
    FBackgroundColor: TfpgColor;
    FColResizing: boolean;
    FDragPos: integer;      // used for column resizing
    FResizedCol: integer;   // used for column resizing
    FDefaultColWidth: integer;
    FDefaultRowHeight: integer;
    FFocusCol: integer;
    FFocusRow: integer;
    FHeaderHeight: integer;
    FOnFocusChange: TfpgFocusChangeNotify;
    FOnRowChange: TfpgRowChangeNotify;
    FPrevCol: integer;
    FPrevRow: integer;
    FFirstRow: integer;
    FFirstCol: integer;
    FMargin: integer;
    FFont: TfpgFont;
    FHeaderFont: TfpgFont;
    FRowSelect: boolean;
    FShowGrid: boolean;
    FShowHeader: boolean;
    FTemp: integer;
    FVScrollBar: TfpgScrollBar;
    FHScrollBar: TfpgScrollBar;
    function    GetFontDesc: string;
    function    GetHeaderFontDesc: string;
    procedure   HScrollBarMove(Sender: TObject; position: integer);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetHeaderFontDesc(const AValue: string);
    procedure   SetRowSelect(const AValue: boolean);
    procedure   VScrollBarMove(Sender: TObject; position: integer);
    procedure   SetBackgroundColor(const AValue: TfpgColor);
    procedure   SetDefaultColWidth(const AValue: integer);
    procedure   SetDefaultRowHeight(const AValue: integer);
    procedure   SetFocusCol(const AValue: integer);
    procedure   SetFocusRow(const AValue: integer);
    procedure   CheckFocusChange;
    procedure   SetShowGrid(const AValue: boolean);
    procedure   SetShowHeader(const AValue: boolean);
    function    VisibleLines: integer;
    function    VisibleWidth: integer;
  protected
    procedure   UpdateScrollBars; virtual;
    function    GetHeaderText(ACol: integer): string; virtual;
    function    GetColumnWidth(ACol: integer): integer; virtual;
    procedure   SetColumnWidth(ACol: integer; const AValue: integer); virtual;
    function    GetColumnCount: integer; virtual;
    function    GetRowCount: integer; virtual;
    procedure   DrawCell(ARow, ACol: integer; ARect: TfpgRect; AFlags: integer); virtual;
    procedure   DrawHeader(ACol: integer; ARect: TfpgRect; AFlags: integer); virtual;
    procedure   DrawGrid(ARow, ACol: integer; ARect: TfpgRect; AFlags: integer); virtual;
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
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor;
    property    FocusCol: integer read FFocusCol write SetFocusCol;
    property    FocusRow: integer read FFocusRow write SetFocusRow;
    property    RowSelect: boolean read FRowSelect write SetRowSelect;
    property    ColumnCount: integer read GetColumnCount;
    property    RowCount: integer read GetRowCount;
    property    ShowHeader: boolean read FShowHeader write SetShowHeader;
    property    ShowGrid: boolean read FShowGrid write SetShowGrid;
    property    HeaderHeight: integer read FHeaderHeight;
    property    ColResizing: boolean read FColResizing write FColResizing;
    property    ColumnWidth[ACol: integer]: integer read GetColumnWidth write SetColumnWidth;
    property    OnFocusChange: TfpgFocusChangeNotify read FOnFocusChange write FOnFocusChange;
    property    OnRowChange: TfpgRowChangeNotify read FOnRowChange write FOnRowChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Update;
  end;

implementation

{ TfpgBaseGrid }

procedure TfpgBaseGrid.HScrollBarMove(Sender: TObject; position: integer);
begin
  if FFirstCol <> position then
  begin
    if Position < 1 then
      Position := 1;
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
  RePaint;
end;

procedure TfpgBaseGrid.SetHeaderFontDesc(const AValue: string);
begin
  FHeaderFont.Free;
  FHeaderFont := fpgGetFont(AValue);
  RePaint;
end;

procedure TfpgBaseGrid.SetRowSelect(const AValue: boolean);
begin
  if FRowSelect = AValue then
    Exit; //==>
  FRowSelect := AValue;
  RePaint;
end;

procedure TfpgBaseGrid.VScrollBarMove(Sender: TObject; position: integer);
begin
  if FFirstRow <> position then
  begin
    FFirstRow := position;
    RePaint;
  end;
end;

procedure TfpgBaseGrid.SetBackgroundColor(const AValue: TfpgColor);
begin
  if FBackgroundColor = AValue then
    Exit; //==>
  FBackgroundColor := AValue;
  RePaint;
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

function TfpgBaseGrid.GetColumnWidth(ACol: integer): integer;
begin
  // GetColumnWidth and SetColumnWidth will be overriden in decendant!
  // Column 2 is special just for testing purposes
  if ACol = 2 then
    Result := FTemp
  else
    Result := FDefaultColWidth+(ACol*16);
end;

procedure TfpgBaseGrid.SetColumnWidth(ACol: integer; const AValue: integer);
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

function TfpgBaseGrid.GetColumnCount: integer;
begin
  Result := 7;
end;

function TfpgBaseGrid.GetRowCount: integer;
begin
  Result := 24;
end;

procedure TfpgBaseGrid.DrawCell(ARow, ACol: integer; ARect: TfpgRect; AFlags: integer);
var
  s: string;
begin
  s := 'c(' + IntToStr(ARow) + ',' + IntToStr(ACol) + ')';
  if (ARow = 5) and (ACol = 2) then
    s := 'Here lives Graeme!';
  if not Enabled then
    Canvas.SetTextColor(clShadow1);
  Canvas.DrawString(ARect.Left+1, ARect.Top+1, s);
end;

procedure TfpgBaseGrid.DrawHeader(ACol: integer; ARect: TfpgRect; AFlags: integer);
var
  s: string;
  r: TfpgRect;
  x: integer;
begin
  // Here we can implement a head style check
  Canvas.DrawButtonFace(ARect, [btnIsEmbedded]);
  r := ARect;
  InflateRect(r, -2, -2);
  Canvas.SetClipRect(r);  // text cannot oversheet header border
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

procedure TfpgBaseGrid.DrawGrid(ARow, ACol: integer; ARect: TfpgRect;
  AFlags: integer);
begin
  // default is inside bottom/right edge or cell
  Canvas.SetColor(clGridLines);
  Canvas.DrawLine(ARect.Left, ARect.Bottom, ARect.Right, ARect.Bottom); // cell bottom
  Canvas.DrawLine(ARect.Right, ARect.Bottom, ARect.Right, ARect.Top-1); // cell right
end;

procedure TfpgBaseGrid.SetFocusCol(const AValue: integer);
begin
  if FFocusCol = AValue then
    Exit; //==>
  FFocusCol := AValue;

  // apply min/max limit
  if FFocusCol < 1 then
    FFocusCol := 1;
  if FFocusCol > ColumnCount then
    FFocusCol := ColumnCount;

  FollowFocus;
  CheckFocusChange;
  UpdateScrollBars;
end;

procedure TfpgBaseGrid.SetFocusRow(const AValue: integer);
begin
  if FFocusRow = AValue then
    Exit; //==>
  FFocusRow := AValue;

  // apply min/max limit
  if FFocusRow < 1 then
    FFocusRow := 1;
  if FFocusRow > RowCount then
    FFocusRow := RowCount;
    
  FollowFocus;
  CheckFocusChange;
  UpdateScrollBars;
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

function TfpgBaseGrid.VisibleLines: integer;
var
  hh: integer;
begin
  if FHScrollBar.Visible then
    hh := FHScrollbar.Height
  else
    hh := 0;
  if ShowHeader then
    hh := hh + FHeaderHeight+1;
  Result := (Height - 2*FMargin - hh) div (FDefaultRowHeight+1)
end;

function TfpgBaseGrid.VisibleWidth: integer;
var
  sw: integer;
begin
  if FVScrollBar.Visible then
    sw := FVScrollBar.Width-1
  else
    sw := 0;
  Result := Width - FMargin*2 - sw;
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
  for i := 1 to ColumnCount do
    cw := cw + ColumnWidth[i];

  // This needs improving while resizing
  if cw > vw then
    FHScrollBar.Visible := True
  else
  begin
    FHScrollBar.Visible := False;
    FFirstCol := 1;
  end;
  
  // This needs improving while resizing
  if (RowCount > VisibleLines) then
    FVScrollBar.Visible := True
  else
  begin
    FVScrollBar.Visible := False;
    FFirstRow := 1;
  end;

  if FVScrollBar.Visible then
  begin
    Dec(HWidth, FVScrollBar.Width);
    FVScrollBar.Min         := 1;
    if RowCount > 0 then
      FVScrollBar.SliderSize := VisibleLines / RowCount
    else
      FVScrollBar.SliderSize := 0;
    FVScrollBar.Max         := RowCount-VisibleLines+1;
    FVScrollBar.Position    := FFirstRow;
    FVScrollBar.RepaintSlider;
  end;
  
  if FHScrollBar.Visible then
  begin
    Dec(VHeight, FHScrollBar.Height);
    FHScrollBar.Min         := 1;
    FHScrollBar.SliderSize  := 0.2;
    FHScrollBar.Max         := ColumnCount;
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

function TfpgBaseGrid.GetHeaderText(ACol: integer): string;
begin
  Result := 'Head ' + IntToStr(ACol);
end;

procedure TfpgBaseGrid.HandlePaint;
var
  r: TfpgRect;
  r2: TfpgRect;
  col: integer;
  row: integer;
  clipr: TfpgRect;   // clip rectangle
begin
  Canvas.BeginDraw;
//  inherited HandlePaint;
  Canvas.ClearClipRect;

  r.SetRect(0, 0, Width, Height);
  Canvas.DrawControlFrame(r);

  InflateRect(r, -2, -2);
  Canvas.SetClipRect(r);
  Canvas.SetColor(FBackgroundColor);
  Canvas.FillRectangle(r);
  
  clipr.SetRect(FMargin, FMargin, VisibleWidth, Height-(2*FMargin));
  r := clipr;

  if (ColumnCount > 0) and ShowHeader then
  begin
    // Drawing horizontal headers
    r.Height := FHeaderHeight;
    Canvas.SetFont(FHeaderFont);
    for col := FFirstCol to ColumnCount do
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

    for row := FFirstRow to RowCount do
    begin
      r.Left := FMargin;
      for col := FFirstCol to ColumnCount do
      begin
        r.Width := ColumnWidth[col];
        Canvas.SetClipRect(clipr);
//        Canvas.SetClipRect(r);

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
          Canvas.SetColor(BackgroundColor);
          Canvas.SetTextColor(clText1);
        end;
        Canvas.AddClipRect(r);
        Canvas.FillRectangle(r);
        DrawCell(row, col, r, 0);

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
  UpdateScrollBars;
end;

procedure TfpgBaseGrid.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);
  UpdateScrollBars;
end;

procedure TfpgBaseGrid.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  w: integer;
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

          if FFocusCol < ColumnCount then
          begin
            inc(FFocusCol);
            FollowFocus;
            RePaint;
            //DoChange;
          end;
        end;

    keyLeft:
        begin
          if RowSelect then
            FFocusCol := FFirstCol;
          if FFocusCol > 1 then
          begin
            dec(FFocusCol);
            FollowFocus;
            RePaint;
            //DoChange;
          end;
        end;

    keyUp:
        begin
          if FFocusRow > 1 then
          begin
            dec(FFocusRow);
            FollowFocus;
            RePaint;
            //DoChange;
          end;
        end;

    keyDown:
        begin
          if FFocusRow < RowCount then
          begin
            inc(FFocusRow);
            FollowFocus;
            RePaint;
            //DoChange;
          end;
        end;

    keyPageUp:
        begin
          dec(FFocusRow,VisibleLines);
          if FFocusRow < 1 then
            FFocusRow := 1;
          FollowFocus;
          RePaint;
          //DoChange;
        end;

    keyPageDown:
        begin
          inc(FFocusRow,VisibleLines);
          if FFocusRow > RowCount then
            FFocusRow := RowCount;
          FollowFocus;
          RePaint;
          //DoChange;
        end;
        
    keyHome:
        begin
          FFocusCol := 1;
          FollowFocus;
          RePaint;
          //DoChange;
        end;
        
    keyEnd:
        begin
          FFocusCol := ColumnCount;
          FollowFocus;
          RePaint;
          //DoChange;
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
  lRow: integer;
  lCol: integer;
begin
  inherited HandleMouseScroll(x, y, shiftstate, delta);

  lRow := FFirstRow;
  lCol := FFirstCol;

  if delta > 0 then // scroll down
    inc(FFirstRow, abs(delta))
  else              // scroll up
    dec(FFirstRow, abs(delta));

  // apply limits
  if FFirstRow > RowCount - VisibleLines + 1 then
    FFirstRow := RowCount - VisibleLines + 1;
  if FFirstRow < 1 then
    FFirstRow := 1;
    
  // scroll left/right
  // If vertical scrollbar is not visible, but
  // horizontal is. Mouse wheel will scroll horizontally.  :)
  if FHScrollBar.Visible and  (not FVScrollBar.Visible) then
  begin
    if delta > 0 then // scroll right
    begin
      if FFirstCol < ColumnCount then
        inc(FFirstCol);
    end
    else
    begin
      if FFirstCol > 1 then
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
  
  if (ColumnCount < 0) or (RowCount < 1) then
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
      for n := FFirstCol to ColumnCount do
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
  n: integer;
  cw: integer;
  nw: integer;
  prow: integer;
  pcol: integer;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  if (ColumnCount < 0) or (RowCount < 1) then
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
    for n := FFirstCol to ColumnCount do
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
    FFocusRow := FFirstRow + ((y - FMargin - hh) div FDefaultRowHeight);
    if FFocusRow > RowCount then
      FFocusRow := RowCount;
    cw := 0;
    for n := FFirstCol to ColumnCount do
    begin
      inc(cw, ColumnWidth[n]);
      if FMargin+cw >= x then
      begin
        FFocusCol := n;
        Break;
      end;
    end;
  end;  { if/else }

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
  n: integer;
  w: TfpgCoord;
begin
  if (RowCount > 0) and (FFocusRow < 1) then
    FFocusRow := 1;
  if FFocusRow > RowCount then
    FFocusRow := RowCount;

  if (ColumnCount > 0) and (FFocusCol < 1) then
    FFocusCol := 1;
  if FFocusCol > ColumnCount then
    FFocusCol := ColumnCount;

  if FFirstRow < 1 then
    FFirstRow := 1;
  if FFirstCol < 1 then
    FFirstCol := 1;

  if FFocusRow < FFirstRow then
    FFirstRow := FFocusRow
  else
  begin
    if (FFirstRow + VisibleLines - 1) < FFocusRow then
      FFirstRow := FFocusRow - VisibleLines + 1;
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
  inherited Create(AOwner);
  Focusable   := True;
  Width       := 120;
  Height      := 80;
  FFocusCol   := 1;
  FPrevCol    := 0;
  FFocusRow   := 1;
  FPrevRow    := 0;
  FFirstRow   := 1;
  FFirstCol   := 1;
  FMargin     := 2;
  FShowHeader := True;
  FShowGrid   := True;
  FRowSelect  := False;
  
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

procedure TfpgBaseGrid.Update;
begin
  UpdateScrollBars;
  FollowFocus;
  RePaint;
end;

end.

