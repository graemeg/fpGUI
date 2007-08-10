unit gui_grid;

{$mode objfpc}{$H+}

{
  TODO:
    * Keyboard navigation
    * Decendant with TColumn class
    * Painting of bottom right little rectangle between scrollbars
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

  TfpgFocusChangeNotify = procedure(Sender: TObject; ARow, ACol: integer) of object;
  TfpgRowChangeNotify = procedure(Sender: TObject; ARow: integer) of object;


  TfpgBaseGrid = class(TfpgWidget)
  private
    FBackgroundColor: TfpgColor;
    FColResizing: boolean;
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
    procedure   HScrollBarMove(Sender: TObject; position: integer);
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
    procedure   UpdateScrollBars;
  protected
    function    GetColumnWidth(ACol: integer): integer; virtual;
    procedure   SetColumnWidth(ACol: integer; const AValue: integer); virtual;
    function    GetColumnCount: integer; virtual;
    function    GetRowCount: integer; virtual;
    procedure   DrawCell(ARow, ACol: integer; ARect: TfpgRect; AFlags: integer); virtual;
    procedure   DrawHeader(ACol: integer; ARect: TfpgRect; AFlags: integer); virtual;
    procedure   DrawGrid(ARow, ACol: integer; ARect: TfpgRect; AFlags: integer); virtual;
    procedure   HandlePaint; override;
    procedure   HandleShow; override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   FollowFocus; virtual;
    property    DefaultColWidth: integer read FDefaultColWidth write SetDefaultColWidth default 64;
    property    DefaultRowHeight: integer read FDefaultRowHeight write SetDefaultRowHeight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Font: TfpgFont read FFont;
    property    HeaderFont: TfpgFont read FHeaderFont;
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor;
    property    FocusCol: integer read FFocusCol write SetFocusCol;
    property    FocusRow: integer read FFocusRow write SetFocusRow;
    property    RowSelect: boolean read FRowSelect write FRowSelect;
    property    ColumnCount: integer read GetColumnCount;
    property    RowCount: integer read GetRowCount;
    property    ShowHeader: boolean read FShowHeader write SetShowHeader;
    property    ShowGrid: boolean read FShowGrid write SetShowGrid;
    property    HeaderHeight: integer read FHeaderHeight;
    property    ColResizing: boolean read FColResizing write FColResizing;
    property    ColumnWidth[ACol: integer]: integer read GetColumnWidth write SetColumnWidth;
    property    OnFocusChange: TfpgFocusChangeNotify read FOnFocusChange write FOnFocusChange;
    property    OnRowChange: TfpgRowChangeNotify read FOnRowChange write FOnRowChange;
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
  {$Note Later we need to take into account Fixed Columns }
  // GetColumnWidth and SetColumnWidth will be overriden in decendant!
  if ACol = 2 then
    Result := FTemp
  else
    Result := 60+(ACol*16);
end;

procedure TfpgBaseGrid.SetColumnWidth(ACol: integer; const AValue: integer);
begin
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
    s := 'This is Graeme!';
  fpgStyle.DrawString(Canvas, ARect.Left+1, ARect.Top+1, s, Enabled);
end;

procedure TfpgBaseGrid.DrawHeader(ACol: integer; ARect: TfpgRect; AFlags: integer);
var
  s: string;
begin
  // Here we can implement a head style check
  Canvas.DrawButtonFace(ARect, [btnIsEmbedded]);
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
  s := 'Head ' + IntToStr(ACol);
  fpgStyle.DrawString(Canvas, (ARect.Left + (ARect.Width div 2)) - (FHeaderFont.TextWidth(s) div 2),
      ARect.Top+1, s, Enabled);
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
  FHScrollBar.Visible := cw > vw;

  FVScrollBar.Visible := (RowCount > VisibleLines);

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
  end;
  
  if FHScrollBar.Visible then
  begin
    Dec(VHeight, FHScrollBar.Height);
    FHScrollBar.Min         := 1;
    FHScrollBar.SliderSize  := 0.2;
    FHScrollBar.Max         := ColumnCount;
    FHScrollBar.Position    := FFocusCol;
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
  FWidth      := 120;
  FHeight     := 80;
  FFocusCol   := 1;
  FPrevCol    := 0;
  FFocusRow   := 1;
  FPrevRow    := 0;
  FFirstRow   := 1;
  FFirstCol   := 1;
  FMargin     := 2;
  FShowHeader := True;
  FShowGrid   := True;
  
  FFont       := fpgGetFont('#Grid');
  FHeaderFont := fpgGetFont('#GridHeader');
  
  FTemp             := 50;  // Just to proof that ColumnWidth does adjust.
  FDefaultColWidth  := 64;
  FDefaultRowHeight := FFont.Height + 2;
  FHeaderHeight     := FHeaderFont.Height + 2;
  FBackgroundColor  := clBoxColor;
  FColResizing      := False;

  FVScrollBar := TfpgScrollBar.Create(self);
  FVScrollBar.Orientation := orVertical;
  FVScrollBar.Visible     := False;
  FVScrollBar.OnScroll    := @VScrollBarMove;
  FVScrollBar.Anchors     := [anTop, anRight, anBottom];

  FHScrollBar := TfpgScrollBar.Create(self);
  FHScrollBar.Orientation := orHorizontal;
  FHScrollBar.Visible     := False;
  FHScrollBar.OnScroll    := @HScrollBarMove;
  FHScrollBar.ScrollStep  := 5;
  FHScrollBar.Anchors     := [anLeft, anBottom, anRight];
end;

destructor TfpgBaseGrid.Destroy;
begin
  FFont.Free;
  FHeaderFont.Free;
  inherited Destroy;
end;

end.

