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

  TFocusChangeNotify = procedure(Sender: TObject; ARow, ACol: integer) of object;
  TRowChangeNotify = procedure(Sender: TObject; ARow: integer) of object;


  TfpgBaseGrid = class(TfpgWidget)
  private
    FBackgroundColor: TfpgColor;
    FColResizing: boolean;
    FFocusCol: integer;
    FFocusRow: integer;
    FHeaderHeight: integer;
    FOnFocusChange: TFocusChangeNotify;
    FOnRowChange: TRowChangeNotify;
    FPrevCol: integer;
    FPrevRow: integer;
    FFirstRow: integer;
    FFirstCol: integer;
    FMargin: integer;
    FFont: TfpgFont;
    FHeaderFont: TfpgFont;
    FRowHeight: integer;
    FRowSelect: boolean;
    FShowGrid: boolean;
    FShowHeader: boolean;
    FTemp: integer;
    FVScrollBar: TfpgScrollBar;
    FHScrollBar: TfpgScrollBar;
    procedure   SetBackgroundColor(const AValue: TfpgColor);
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
    procedure   DrawCell(ARow, ACol: integer; ARect: TRect; AFlags: integer); virtual;
    procedure   DrawHeader(ACol: integer; ARect: TRect; AFlags: integer); virtual;
    procedure   HandlePaint; override;
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
    property    RowHeight: integer read FRowHeight;
    property    HeaderHeight: integer read FHeaderHeight;
    property    ColResizing: boolean read FColResizing write FColResizing;
    property    ColumnWidth[ACol: integer]: integer read GetColumnWidth write SetColumnWidth;
    property    OnFocusChange: TFocusChangeNotify read FOnFocusChange write FOnFocusChange;
    property    OnRowChange: TRowChangeNotify read FOnRowChange write FOnRowChange;
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

function TfpgBaseGrid.GetColumnWidth(ACol: integer): integer;
begin
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

procedure TfpgBaseGrid.DrawCell(ARow, ACol: integer; ARect: TRect; AFlags: integer);
var
  s: string;
begin
  s := 'Cellg(' + IntToStr(ARow) + ',' + IntToStr(ACol) + ')';
  Canvas.DrawString(ARect.left+1, ARect.top+1, s);
end;

procedure TfpgBaseGrid.DrawHeader(ACol: integer; ARect: TRect; AFlags: integer);
var
  s: string;
begin
  s := 'Head ' + IntToStr(ACol);
  Canvas.DrawString(ARect.left + (ARect.Right div 2) - (FHeaderFont.TextWidth(s) div 2),
    ARect.top+1, s);
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
  if FShowGrid=AValue then exit;
  FShowGrid:=AValue;
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
  result := (Height - 2*FMargin - hh) div (FRowHeight+1)
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
  r: TRect;
  r2: TRect;
  col: integer;
  row: integer;
  clr: TRect;
begin
  Canvas.BeginDraw;
//  inherited HandlePaint;
  Canvas.ClearClipRect;
  r := Rect(0, 0, Width-1, Height-1);

  Canvas.DrawControlFrame(0, 0, Width, Height);
  InflateRect(r, -2, -2);
  Canvas.SetClipRect(r);
  Canvas.SetColor(FBackgroundColor);
  Canvas.FillRectangle(r);
  
  clr := Rect(FMargin, FMargin, VisibleWidth, Height-2*FMargin);
  r := clr;
  
  if (ColumnCount > 0) and ShowHeader then
  begin
    // Drawing headers
    r.Bottom := FHeaderHeight;

    Canvas.SetFont(FHeaderFont);
    for col := FFirstCol to ColumnCount do
    begin
      r.Right := ColumnWidth[col];
      Canvas.SetClipRect(clr);

      // drawing grid lines
      Canvas.SetColor(clGridLines);
      Canvas.DrawLine(r.Left, r.Bottom+1, r.Right+1, r.Bottom+1);
      Canvas.DrawLine(r.Right+1, r.Top, r.Right+1, r.Bottom+1);

      Canvas.AddClipRect(r);
      Canvas.SetColor(clGridHeader);
      Canvas.FillRectangle(r);

      Canvas.SetTextColor(clText1);
      DrawHeader(col, r, 0);

      r.Left := r.Left + r.Right + 1;

      if r.Left >= clr.Right then
        Break;
    end;

    r.Top := r.Top + r.Bottom + 1;
  end;


  if (RowCount > 0) and (ColumnCount > 0) then
  begin
    // Drawing items
    Canvas.SetFont(FFont);

    r.Bottom := RowHeight;

    for row := FFirstRow to RowCount do
    begin
      r.Left := FMargin;
      for col := FFirstCol to ColumnCount do
      begin
        r.Right := ColumnWidth[col];

        canvas.SetClipRect(clr);

        // drawing grid lines
        if FShowGrid then
          Canvas.SetColor(clGridLines)
        else
          Canvas.SetColor(FBackgroundColor);

        canvas.DrawLine(r.Left, r.Bottom+1, r.Right+1, r.Bottom+1);
        canvas.DrawLine(r.Right+1, r.Top, r.Right+1, r.Bottom+1);

        canvas.AddClipRect(r);

        if (row = FFocusRow) and (RowSelect or (col = FFocusCol)) then
        begin
          if FFocused then
          begin
            canvas.SetColor(clSelection);
            canvas.SetTextColor(clSelectionText);
          end
          else
          begin
            canvas.SetColor(clInactiveSel);
            canvas.SetTextColor(clInactiveSelText);
          end;
        end
        else
        begin
          canvas.SetColor(BackgroundColor);
          canvas.SetTextColor(clText1);
        end;

        canvas.FillRectangle(r);

        DrawCell(row, col, r, 0);

        r.Left := r.Left + r.Right + 1;

        if r.Left >= clr.Right then
          Break;
      end;

      r.Top := r.Top + r.Bottom + 1;

      if r.Top >= clr.Bottom then break;

    end;
  end; // item drawing

  canvas.SetClipRect(clr);
  canvas.SetColor(FBackgroundColor);

  // clearing after the last column
  if r.Left <= clr.Right then
  begin
    r2.Left   := r.Left;
    r2.Top    := clr.Top;
    r2.Right  := clr.Right;
    r2.Bottom := clr.Bottom;
    Canvas.FillRectangle(r2);
  end;

  // clearing after the last row
  if r.Top <= clr.Bottom then
  begin
    r.Left    := clr.Left;
    r.Right   := clr.Right;
    r.Bottom  := clr.Bottom;
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
  
  FBackgroundColor  := clBoxColor;
  FColResizing      := False;

  FFont       := fpgGetFont('#Grid');
  FHeaderFont := fpgGetFont('#GridHeader');
  
  FRowHeight    := FFont.Height + 2;
  FHeaderHeight := FHeaderFont.Height + 2;

  FVScrollBar := TfpgScrollBar.Create(self);
  FVScrollBar.Orientation := orVertical;
  FVScrollBar.Visible     := False;
//  FVScrollBar.OnScroll := @VScrollBarMove;

  FHScrollBar := TfpgScrollBar.Create(self);
  FHScrollBar.Orientation := orHorizontal;
  FHScrollBar.Visible     := False;
//  FHScrollBar.OnScroll := @HScrollBarMove;
//  FHScrollBar.ScrollStep := 5;

  FTemp := 50;  // a bit of a hack for now (default column width)
end;

destructor TfpgBaseGrid.Destroy;
begin
  FFont.Free;
  FHeaderFont.Free;
  inherited Destroy;
end;

end.

