unit gui_grid;

{$mode objfpc}{$H+}

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
    procedure   UpdateScrollBar;
  protected
    function    GetColumnWidth(ACol: integer): integer; virtual;
    procedure   SetColumnWidth(ACol: integer; const AValue: integer); virtual;
    function    GetColumnCount: integer; virtual;
    function    GetRowCount: integer; virtual;
    procedure   DrawCell(ARow, ACol: integer; ARect: TfpgRect; AFlags: integer); virtual;
    procedure   DrawHeader(ACol: integer; ARect: TfpgRect; AFlags: integer); virtual;
    procedure   HandlePaint; override;
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
  if ACol = 2 then
    Result := 60+(ACol*16) //FTemp
  else
    Result := 60+(ACol*16);
end;

procedure TfpgBaseGrid.SetColumnWidth(ACol: integer; const AValue: integer);
begin
  if (ACol = 2) and (AValue <> FTemp) then
  begin
    FTemp := AValue;
    UpdateScrollBar;
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
  s := 'Cell(' + IntToStr(ARow) + ',' + IntToStr(ACol) + ')';
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

  if (col mod 2) = 0 then
    Canvas.SetColor(clGridHeader)
  else
    Canvas.SetColor(clMagenta);
  Canvas.FillRectangle(r);
*)

  Canvas.SetTextColor(clText1);
  s := 'Head ' + IntToStr(ACol);
  fpgStyle.DrawString(Canvas, (ARect.Width div 2) - (FHeaderFont.TextWidth(s) div 2),
      ARect.Top+1, s, Enabled);
end;

procedure TfpgBaseGrid.SetFocusCol(const AValue: integer);
begin
  if FFocusCol = AValue then
    Exit; //==>
  FFocusCol := AValue;
end;

procedure TfpgBaseGrid.SetFocusRow(const AValue: integer);
begin
  if FFocusRow = AValue then
    Exit; //==>
  FFocusRow := AValue;
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

procedure TfpgBaseGrid.UpdateScrollBar;
var
  HWidth: integer;
  VHeight: integer;
begin
  VHeight := Height - 4;
  HWidth  := Width - 4;

  if FVScrollBar.Visible then Dec(HWidth, FVScrollBar.Width);
  if FHScrollBar.Visible then Dec(VHeight, FHScrollBar.Height);

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

  if (ColumnCount > 0) and ShowHeader then
  begin
    // Drawing horizontal headers
    r.Height := FHeaderHeight;
    Canvas.SetFont(FHeaderFont);
    for col := FFirstCol to ColumnCount do
    begin
      r.Width := FDefaultColWidth;
      DrawHeader(col, r, 0);
      r.Left := r.Left + r.Width + 1;
      if r.Left >= clipr.Right then
        Break;  // small optimization. Don't draw what we can't see
    end;
    r.Top := r.Top + r.Height + 1;
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
//        r.Width := ColumnWidth[col];
        r.Width := FDefaultColWidth;
//        Canvas.SetClipRect(clipr);

        // drawing grid lines
        if FShowGrid then
        begin
          Canvas.SetColor(clGridLines);
          Canvas.DrawLine(r.Left, r.Bottom+1, r.Right+1, r.Bottom+1); // cell bottom
          Canvas.DrawLine(r.Right+1, r.Top, r.Right+1, r.Bottom+1); // cell right
        end;

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
        Canvas.FillRectangle(r);
        DrawCell(row, col, r, 0);
        r.Left := r.Left + r.Width + 1;

        if r.Left >= clipr.Right then
          Break;  // small optimization. Don't draw what we can't see
      end;
//      Inc(r.Top, FDefaultRowHeight+1);
      r.Top := r.Top + r.Height + 1;
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
  
  Canvas.EndDraw;
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
  
  FDefaultColWidth  := 64;
  FDefaultRowHeight := FFont.Height + 2;
  FHeaderHeight     := FHeaderFont.Height + 2;
  FBackgroundColor  := clBoxColor;
  FColResizing      := False;

  FVScrollBar := TfpgScrollBar.Create(self);
  FVScrollBar.Orientation := orVertical;
  FVScrollBar.Visible     := False;
//  FVScrollBar.OnScroll := @VScrollBarMove;

  FHScrollBar := TfpgScrollBar.Create(self);
  FHScrollBar.Orientation := orHorizontal;
  FHScrollBar.Visible     := False;
//  FHScrollBar.OnScroll := @HScrollBarMove;
//  FHScrollBar.ScrollStep := 5;

end;

destructor TfpgBaseGrid.Destroy;
begin
  FFont.Free;
  FHeaderFont.Free;
  inherited Destroy;
end;

end.

