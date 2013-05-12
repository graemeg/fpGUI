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
      Defines a Base Grid control. Usable as the base for any grid type of
      component.
}

unit fpg_basegrid;

{$mode objfpc}{$H+}

{.$Define DEBUG}

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

  TfpgGridDrawState = set of (gdSelected, gdFocused, gdFixed);

  TfpgGridHeaderStyle = (ghsButton, ghsThin, ghsFlat);

  TfpgFocusChangeNotify = procedure(Sender: TObject; ARow, ACol: Integer) of object;
  TfpgHeaderClick = procedure(Sender: TObject; ACol: Integer) of object;
  TfpgRowChangeNotify = procedure(Sender: TObject; ARow: Integer) of object;
  TfpgCanSelectCellEvent = procedure(Sender: TObject; const ARow, ACol: Integer; var ACanSelect: boolean) of object;
  TfpgDrawCellEvent = procedure(Sender: TObject; const ARow, ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean) of object;

  // widget options
  TfpgGridOption = (go_HideFocusRect, go_AlternativeColor, go_SmoothScroll);
  TfpgGridOptions = set of TfpgGridOption;

  // Column 2 is special just for testing purposes. Descendant classes will
  // override that special behavior anyway.
  
  { TfpgBaseGrid }

  TfpgBaseGrid = class(TfpgWidget)
  private
    FColResizing: boolean;
    FDragPos: integer;      // used for column resizing
    FHeaderStyle: TfpgGridHeaderStyle;
    FOnDrawCell: TfpgDrawCellEvent;
    FOnHeaderClick: TfpgHeaderClick;
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
    FXOffset: integer;  // used for go_SmoothScroll
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
    FOptions: TfpgGridOptions;
    FPopupMenu: TfpgPopupMenu;
    FAlternativeBGColor: TfpgColor;
    FBorderStyle: TfpgEditBorderStyle;
    function    GetFontDesc: string;
    function    GetHeaderFontDesc: string;
    function    GetTotalColumnWidth: integer;
    procedure   HScrollBarMove(Sender: TObject; position: integer);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetHeaderFontDesc(const AValue: string);
    procedure   SetHeaderStyle(const AValue: TfpgGridHeaderStyle);
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
    procedure   SetFirstRow(const AValue: Integer);
    procedure   SetAlternativeBGColor(const AValue: TfpgColor);
    procedure   SetBorderStyle(AValue: TfpgEditBorderStyle);
  protected
    property    UpdateCount: integer read FUpdateCount;
    procedure   UpdateScrollBars; virtual;
    function    GetHeaderText(ACol: Integer): string; virtual;
    function    GetColumnWidth(ACol: Integer): integer; virtual;
    procedure   SetColumnWidth(ACol: Integer; const AValue: integer); virtual;
    function    GetBackgroundColor(ARow: integer; ACol: integer): TfpgColor; virtual;
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
    procedure   HandleRMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   FollowFocus; virtual;
    procedure   PrepareCells (firstrow, lastrow, firstcol, lastcol : integer); virtual;
    property    AlternateBGColor: TfpgColor read FAlternativeBGColor write SetAlternativeBGColor default clHilite1;
    property    BorderStyle: TfpgEditBorderStyle read FBorderStyle write SetBorderStyle default ebsDefault;
    property    DefaultColWidth: integer read FDefaultColWidth write SetDefaultColWidth default 64;
    property    DefaultRowHeight: integer read FDefaultRowHeight write SetDefaultRowHeight;
    property    Font: TfpgFont read FFont;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    HeaderFont: TfpgFont read FHeaderFont;
    property    HeaderFontDesc: string read GetHeaderFontDesc write SetHeaderFontDesc;
    property    FocusCol: Integer read FFocusCol write SetFocusCol default -1;
    property    FocusRow: Integer read FFocusRow write SetFocusRow default -1;
    property    HeaderStyle: TfpgGridHeaderStyle read FHeaderStyle write SetHeaderStyle default ghsButton;
    property    RowSelect: boolean read FRowSelect write SetRowSelect;
    property    ColumnCount: Integer read GetColumnCount;
    property    PopupMenu: TfpgPopupMenu read FPopupMenu write FPopupMenu;
    property    RowCount: Integer read GetRowCount;
    property    ShowHeader: boolean read FShowHeader write SetShowHeader default True;
    property    ShowGrid: boolean read FShowGrid write SetShowGrid default True;
    property    ScrollBarStyle: TfpgScrollStyle read FScrollBarStyle write SetScrollBarStyle default ssAutoBoth;
    property    HeaderHeight: integer read FHeaderHeight;
    property    TotalColumnWidth: integer read GetTotalColumnWidth;
//    property    ColResizing: boolean read FColResizing write FColResizing;
    property    ColumnWidth[ACol: Integer]: integer read GetColumnWidth write SetColumnWidth;
    property    ColumnBackgroundColor[ACol: Integer]: TfpgColor read GetColumnBackgroundColor write SetColumnBackgroundColor;
    property    ColumnTextColor[ACol: Integer]: TfpgColor read GetColumnTextColor write SetColumnTextColor;
    property    VisibleRows: Integer read VisibleLines;
    property    TopRow: Integer read FFirstRow write SetFirstRow;
    property    Options: TfpgGridOptions read FOptions write FOptions default [];
    property    OnDrawCell: TfpgDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property    OnFocusChange: TfpgFocusChangeNotify read FOnFocusChange write FOnFocusChange;
    property    OnHeaderClick: TfpgHeaderClick read FOnHeaderClick write FOnHeaderClick;
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
    function    VisibleWidth: integer;
    function    VisibleHeight: integer;
  end;


implementation

{ TfpgBaseGrid }

procedure TfpgBaseGrid.HScrollBarMove(Sender: TObject; position: integer);
begin
  if go_SmoothScroll in FOptions then
  begin
    if FXOffset <> position then
    begin
      if Position < 0 then
        Position := 0;
      FXOffset := position;
      Repaint;
    end;
  end
  else
  begin
    if FFirstCol <> position then
    begin
      if Position < 0 then
        Position := 0;
      FFirstCol := position;
      RePaint;
    end;
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

function TfpgBaseGrid.GetTotalColumnWidth: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to ColumnCount-1 do
    Result := Result + ColumnWidth[i];
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

procedure TfpgBaseGrid.SetHeaderStyle(const AValue: TfpgGridHeaderStyle);
begin
  if FHeaderStyle = AValue then
    exit;
  FHeaderStyle := AValue;
  Repaint;
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

function TfpgBaseGrid.GetBackgroundColor(ARow: integer; ACol: integer): TfpgColor;
begin
  if (ARow >= 0) and (ACol >= 0) and (ARow < RowCount) and (ACol < ColumnCount) then
  begin
    if go_AlternativeColor in Options then
    begin
      if (ARow mod 2) <> 0 then
        Result := AlternateBGColor
      else
        Result := ColumnBackgroundColor[ACol];
    end
    else
      Result := ColumnBackgroundColor[ACol];
  end
  else
    Result := BackgroundColor;
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
  r := ARect;
  // Here we can implement a head style check
  case FHeaderStyle of
    ghsButton:
        begin
          Canvas.DrawButtonFace(ARect, [btfIsEmbedded]);
          InflateRect(r, -2, -2);
        end;
    ghsThin:
        begin
          Canvas.DrawBevel(ARect);
        end;
    ghsFlat:
        begin
          Canvas.Color:= clGridHeader;
          Canvas.FillRectangle(r);
          Canvas.Color:= clShadow2;
          Canvas.DrawLine(r.Left, r.Bottom, r.Right, r.Bottom);  { bottom line }
          Canvas.DrawLine(r.Right, r.Bottom, r.Right, r.Top-1);  { right line }
        end;
  end;
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
  if not (go_SmoothScroll in FOptions) then
  begin
    if x < 1 then
      x := 1;
  end;
  fpgStyle.DrawString(Canvas, x, ARect.Top+1, s, Enabled);
end;

procedure TfpgBaseGrid.DrawGrid(ARow, ACol: Integer; ARect: TfpgRect;
  AFlags: integer);
begin
  // default is inside bottom/right edge or cell
  Canvas.SetColor(clGridLines);
  if (BorderStyle <> ebsDefault) and (ACol = 0) then
    Canvas.DrawLine(ARect.Left, ARect.Top, ARect.Left, ARect.Bottom); // cell left of first column only
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
  Update;
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
  Update;
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

procedure TfpgBaseGrid.SetAlternativeBGColor(const AValue: TfpgColor);
begin
  if FAlternativeBGColor = AValue then exit;
  FAlternativeBGColor := AValue;
end;

procedure TfpgBaseGrid.SetBorderStyle(AValue: TfpgEditBorderStyle);
begin
  if FBorderStyle = AValue then
    Exit;
  FBorderStyle := AValue;
  Repaint;
end;

procedure TfpgBaseGrid.UpdateScrollBars;
var
  HWidth: integer;
  VHeight: integer;
  vw: integer;
  cw: integer;
  vl: integer;
  i: integer;
  x: integer;
  Hfits, showH : boolean;
  Vfits, showV : boolean;

  procedure hideScrollbar (sb : TfpgScrollBar);
  begin
    with sb do
      if Visible then
      begin
        Visible := False;
        UpdatePosition;
      end;
  end;
  
  procedure getVisWidth;
  begin
    if showV then
      vw := HWidth - (FVScrollBar.Width-1)
    else
      vw := HWidth;
    Hfits := vw >= cw;
  end;

  procedure getVisLines;
  var
    hh : integer; // header height
  begin
    hh := 0;
    if ShowHeader then inc (hh, FHeaderHeight+1);
    if showH then inc (hh, FHScrollBar.Height);
    vl := (VHeight - hh) div FDefaultRowHeight;
    Vfits := vl >= RowCount;
  end;

begin
  // if we don't want any scrollbars, hide them and exit
  if FScrollBarStyle = ssNone then
  begin
    hideScrollbar (FHScrollBar);
    hideScrollbar (FVScrollBar);
    exit;
  end;
  
  // preliminary width/height calculations
  VHeight := Height - 4;
  HWidth  := Width - 4;
  cw := 0;
  for i := 0 to ColumnCount-1 do
    cw := cw + ColumnWidth[i];
  showV := False;
  showH := False;
  getVisWidth;
  getVisLines;
  
  // determine whether to show scrollbars for different configurations
  case FScrollBarStyle of
    ssHorizontal:
        begin
          hideScrollbar (FVScrollBar);
          if not Hfits then
          begin
            showH := true;
            getVisLines;
          end;
        end;
    ssVertical:
        begin
          hideScrollbar (FHScrollBar);
          if not Vfits then
          begin
            showV := true;
            getVisWidth;
          end;
        end;
    ssAutoBoth:
        if not Vfits then
        begin
          showV := true;
          getVisWidth;
          if not Hfits then
          begin
            showH := true;
            getVisLines;
            getVisWidth;
          end;
        end
        else if not Hfits then
        begin
          showH := true;
          getVisLines;
          if not Vfits then
          begin
            showV := true;
            getVisWidth;
            getVisLines;
          end;
        end;
  end;

  // set the scrollbar width/height space
  if showV then
    Dec(HWidth, FVScrollBar.Width);
  if showH then
    Dec(VHeight, FHScrollBar.Height);

  // show or hide the scrollbars

  if showV then
  begin
    FVScrollBar.Visible := true;
    FVScrollBar.Min         := 0;
    if RowCount > 0 then
      FVScrollBar.SliderSize := VisibleLines / RowCount
    else
      FVScrollBar.SliderSize := 0;
    FVScrollBar.Max         := RowCount-VisibleLines;
    FVScrollBar.Position    := FFirstRow;
    FVScrollBar.RepaintSlider;
    FVScrollBar.Top     := 2;
    FVScrollBar.Left    := Width - FVScrollBar.Width - 2;
    FVScrollBar.Height  := VHeight;
  end
  else
  begin
    FVScrollBar.Visible := false;
    if Vfits then
      FFirstRow := 0;
    // if vertical doesn't fit and no scrollbar, do not change firstrow
  end;

  if showH then
  begin
    FHScrollBar.Visible := true;
    FHScrollBar.Min         := 0;
    if go_SmoothScroll in FOptions then
    begin
      FHScrollBar.Max := cw - vw;
      FHScrollBar.Position := FXOffset;
      FHScrollBar.SliderSize := HWidth / TotalColumnWidth;
      FHScrollBar.PageSize := 5;
    end
    else
    begin
      FHScrollBar.Max := ColumnCount-1;
      FHScrollBar.Position := FFirstCol;
      FHScrollBar.SliderSize  := 1 / ColumnCount;
      FHScrollBar.PageSize := 1;
    end;
    FHScrollBar.RepaintSlider;
    FHScrollBar.Top     := Height -FHScrollBar.Height - 2;
    FHScrollBar.Left    := 2;
    FHScrollBar.Width   := HWidth;
  end
  else
  begin
    FHScrollBar.Visible := false;
    if Hfits then
    begin
      FFirstCol := 0;
      FXOffset := 0;
    end;
    // if horizontal doesn't fit and no scrollbar, do not change firstcol/xoffset
  end;

  FHScrollBar.Top     := Height -FHScrollBar.Height - 2;
  FHScrollBar.Left    := 2;
  FHScrollBar.Width   := HWidth;

  FVScrollBar.Top     := 2;
  FVScrollBar.Left    := Width - FVScrollBar.Width - 2;
  FVScrollBar.Height  := VHeight;

  FVScrollBar.UpdatePosition;
  FHScrollBar.UpdatePosition;
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
  cLeft: integer;
  rTop: integer;
  firstcol, lastcol, firstrow, lastrow : integer;
  cWidths: array of integer;
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

  Canvas.SetColor(FBackgroundColor);
  Canvas.FillRectangle(r);

  clipr.SetRect(FMargin, FMargin, VisibleWidth, VisibleHeight);
  r := clipr;

  cLeft := FMargin; // column starting point
  rTop := FMargin; // row starting point

  if go_SmoothScroll in FOptions then
  begin
    if FHScrollBar.Visible then
      Dec(cLeft, FHScrollBar.Position);
    firstcol := 0;
  end
  else
  begin
    firstcol := FFirstCol;
  end;

  // calculate column widths, and first/last columns
  if (ColumnCount <= 0) then
  begin
    firstcol := -1;
    lastcol := -2;
  end
  else
  begin
    setlength (cWidths, ColumnCount);
    r.Left := cLeft;
    for col := firstcol to ColumnCount-1 do
    begin
      cWidths[col] := ColumnWidth[col];
      r.Width := cWidths[col];
      if (go_SmoothScroll in FOptions) and (r.Left <= clipr.Left) then
      begin
        firstcol := col;
        if col>0 then inc (cLeft, cWidths[col-1]);
      end;
      lastcol := col;
      if r.Right >= clipr.Right then
        break;
      inc (r.Left, r.Width);
    end;
    // first/last rows...
    if (RowCount <= 0) then
    begin
      firstrow := -1;
      lastrow := -2;
    end
    else
    begin
      if ShowHeader then
        inc (r.Top, FHeaderHeight);
      if r.Top > clipr.Bottom then
      begin
        firstrow := -1;
        lastrow := -2;
      end
      else
      begin
        firstrow := FFirstRow;
        lastrow := firstrow + (clipr.Bottom - r.Top) div DefaultRowHeight;
        if lastrow >= RowCount then
          lastrow := RowCount-1;
      end;
    end;
  end;

  PrepareCells (firstrow, lastrow, firstcol, lastcol);

  r.Left := cLeft;
  r.Top := rTop;

  if (ColumnCount > 0) and ShowHeader then
  begin
    // Drawing horizontal headers
    r.Height := FHeaderHeight;
    Canvas.SetFont(FHeaderFont);
    for col := firstcol to lastcol do
    begin
      r.Width := cWidths[col];
      Canvas.SetClipRect(clipr);
      Canvas.AddClipRect(r);
      DrawHeader(col, r, 0);
      inc(r.Left, r.Width);
      //if r.Left >= clipr.Right then
      //  Break;  // optimization made obsolete by lastcol
    end;
    inc(r.Top, r.Height);
  end;

  if (RowCount > 0) and (ColumnCount > 0) then
  begin
    // Drawing cells
    r.Height := DefaultRowHeight;
    Canvas.SetFont(FFont);

    for row := firstrow to lastrow do
    begin
      r.Left := cLeft;
      for col := firstcol to lastcol do
      begin
        drawstate := [];
        r.Width := cWidths[col];
        Canvas.SetClipRect(clipr);

        if (row = FFocusRow) and (RowSelect or (col = FFocusCol)) and not (go_HideFocusRect in FOptions) then
        begin
          if FFocused then
          begin
            Canvas.SetColor(clGridSelection);
            Canvas.SetTextColor(clGridSelectionText);
          end
          else
          begin
            Canvas.SetColor(clGridInactiveSel);
            Canvas.SetTextColor(clGridInactiveSelText);
          end;
        end
        else
        begin
          Canvas.SetColor(GetBackgroundColor(row, col));
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
        //if r.Left >= clipr.Right then
        //  Break;  // optimization made obsolete by lastcol
      end;
//      Inc(r.Top, FDefaultRowHeight+1);
      inc(r.Top, r.Height);
      //if r.Top >= clipr.Bottom then
      //  break;  // optimization made obsolete by lastrow
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
    Exit; //==>
  if csUpdating in ComponentState then
    Exit; //==>
  if WindowAllocated then
    UpdateScrollBars;
end;

procedure TfpgBaseGrid.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  w: integer;
  r: integer;
  c: integer;
begin
  if consumed then
    exit;
  fpgApplication.HideHint;

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
          consumed := True;
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
          consumed := True;
        end;

    keyUp:
        begin
          if CanSelectCell(FFocusRow-1, FFocusCol) then
          begin
            dec(FFocusRow);
            FollowFocus;
            RePaint;
          end;
          consumed := True;
        end;

    keyDown:
        begin
          if CanSelectCell(FFocusRow+1, FFocusCol) then
          begin
            inc(FFocusRow);
            FollowFocus;
            RePaint;
          end;
          consumed := True;
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
          consumed := True;
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
          consumed := True;
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
          else if (FFocusCol <> 0) then
          begin
            { find first selectable column }
            for c := 0 to FFocusCol-1 do
              if CanSelectCell(FFocusRow, c) then
              begin
                FFocusCol := c;
                FollowFocus;
                RePaint;
                break;
              end;
          end;
          consumed := True;
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
          else if (FFocusCol <> (ColumnCount-1)) then
          begin
            for c := ColumnCount-1 downto FFocusCol+1 do
              if CanSelectCell(FFocusRow, c) then
              begin
                FFocusCol := c;
                FollowFocus;
                RePaint;
                break;
              end;
          end;
          consumed := True;
        end;
  end;  { case }
  
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
    inc(FFirstRow, abs(delta)*3)
  else              // scroll up
    if FFirstRow > 0 then
      dec(FFirstRow, abs(delta)*3);

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
  cLeft: integer;
  c: integer;
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
      cw := (ColumnWidth[FResizedCol]+x)-FDragPos;
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

    cLeft := FMargin; // column starting point
    if go_SmoothScroll in FOptions then
    begin
      if FHScrollBar.Visible then
        Dec(cLeft, FHScrollBar.Position);
      c := 0;
    end
    else
    begin
      c := FFirstCol;
    end;

    if (y <= FMargin + hh) then // we are over the Header row
    begin
      cw := 0;
      for n := c to ColumnCount-1 do
      begin
        inc(cw, ColumnWidth[n]);
        // Resizing is enabled 4 pixel either way of the cell border
        if ((x >= (cLeft+cw-4)) and (x <= (cLeft+cw+4))) then
        begin
          colresize := True;
          Break;
        end;
        { TODO: We could optimize this slightly by breaking as soon as x is
          greater than current column edge we are over. }
      end;  { if }
    end;  { if/else }

    if colresize then
      MouseCursor := mcSizeEW
    else
      MouseCursor := mcDefault;
  end;  { if/else }
end;

procedure TfpgBaseGrid.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  lColumn: integer;
  hh: integer; { header height }
  cLeft: integer; { column left }
  c: integer;
  n: integer;
  cw: integer;
begin
  inherited HandleLMouseUp(x, y, shiftstate);

  if not FColResizing then
  begin
    if not ShowHeader then
      Exit;
    if (ColumnCount = 0) then
      Exit; //==>
    // searching for the appropriate character position
    hh := FHeaderHeight;

    if (y < FMargin+hh) then  // inside Header row
    begin
      {$IFDEF DEBUG} Writeln('header click...'); {$ENDIF}

      cLeft := FMargin; // column starting point
      if go_SmoothScroll in FOptions then
      begin
        if FHScrollBar.Visible then
          Dec(cLeft, FHScrollBar.Position);
        c := 0;
      end
      else
      begin
        c := FFirstCol;
      end;

      cw := 0;
      for n := c to ColumnCount-1 do
      begin
        inc(cw, ColumnWidth[n]);
        if x < (cLeft+cw+4) then
        begin
          if Assigned(FOnHeaderClick) then
            FOnHeaderClick(self, n);
          Break;
        end;
      end;  { for }
    end;
  end;  {if not FColResizing }

  {$IFDEF DEBUG}
  if FColResizing then
  begin
    Writeln('Column ', FResizedCol,' width = ', ColumnWidth[FResizedCol]);
  end;
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
  c: integer;
  cLeft: integer;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  if (ColumnCount = 0) or (RowCount = 0) then
    Exit; //==>

  pcol := FFocusCol;
  prow := FFocusRow;

  // searching for the appropriate character position
  if ShowHeader then
    hh := FHeaderHeight
  else
    hh := 0;

  if ShowHeader and (y < FMargin+hh) then  // inside Header row
  begin
    {$IFDEF DEBUG} Writeln('header click...'); {$ENDIF}

    cLeft := FMargin; // column starting point
    if go_SmoothScroll in FOptions then
    begin
      if FHScrollBar.Visible then
        Dec(cLeft, FHScrollBar.Position);
      c := 0;
    end
    else
    begin
      c := FFirstCol;
    end;

    cw := 0;
    for n := c to ColumnCount-1 do
    begin
      inc(cw, ColumnWidth[n]);
      if (x >= (cLeft+cw-4)) and (x <= (cLeft+cw+4)) then
      begin
        {$IFDEF DEBUG} Writeln('column resize...'); {$ENDIF}
        FColResizing  := True;
        FResizedCol   := n;
        FDragPos      := x;
        Exit;
      end;  { if/else }
      { TODO: We could optimize this slightly by breaking as soon as x is
        greater than current column edge we are over. }
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

procedure TfpgBaseGrid.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
var
  hh: integer;
begin
  inherited HandleRMouseUp(x, y, shiftstate);
  if Assigned(PopupMenu) then
  begin
    // popup should not appear if you clicked in header - maybe this behaviour should be user-selectable?
    if ShowHeader then
      hh := FHeaderHeight+1
    else
      hh := 0;

    if ShowHeader and (y > FMargin+hh) then  // not in Header row
    begin
      PopupMenu.ShowAt(self, x, y);
    end;
  end;
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
  CheckFocusChange;
  UpdateScrollBars;
end;

procedure TfpgBaseGrid.PrepareCells(firstrow, lastrow, firstcol, lastcol: integer);
begin
  // for descendents
end;

constructor TfpgBaseGrid.Create(AOwner: TComponent);
begin
  Updating;
  inherited Create(AOwner);
  Focusable   := True;
  Width       := 120;
  Height      := 80;
  FFocusCol   := -1;
  FPrevCol    := -1;
  FFocusRow   := -1;
  FPrevRow    := -1;
  FFirstRow   := 0;
  FFirstCol   := 0;
  FMargin     := 2;
  FShowHeader := True;
  FShowGrid   := True;
  FRowSelect  := False;
  FScrollBarStyle := ssAutoBoth;
  FUpdateCount    := 0;
  FOptions    := [];
  FHeaderStyle := ghsButton;
  FBorderStyle := ebsDefault;

  FFont       := fpgGetFont('#Grid');
  FHeaderFont := fpgGetFont('#GridHeader');
  
  FTemp             := 50;  // Just to prove that ColumnWidth does adjust.
  FDefaultColWidth  := 64;
  FDefaultRowHeight := FFont.Height + 2;
  FHeaderHeight     := FHeaderFont.Height + 2;
  FBackgroundColor  := clBoxColor;
  FAlternativeBGColor := clHilite1;
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
  if csUpdating in ComponentState then
    Exit;
  UpdateScrollBars;
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
//      RePaint;
      Update;
    end;
  end;
end;

procedure TfpgBaseGrid.MouseToCell(X, Y: Integer; var ACol, ARow: Integer);
var
  hh: integer;
  cw: integer;
  n: Integer;
  cLeft: integer;
  c: integer;
begin
  if ShowHeader then
    hh := FHeaderHeight+1
  else
    hh := 0;

  ARow := FFirstRow + ((y - FMargin - hh) div FDefaultRowHeight);
  if ARow > RowCount-1 then
    ARow := RowCount-1;

  cLeft := FMargin; // column starting point
  if go_SmoothScroll in FOptions then
  begin
    if FHScrollBar.Visible then
      Dec(cLeft, FHScrollBar.Position);
    c := 0;
  end
  else
  begin
    c := FFirstCol;
  end;

  cw := 0;
  for n := c to ColumnCount-1 do
  begin
    inc(cw, ColumnWidth[n]);
    if cLeft+cw >= x then
    begin
      ACol := n;
      Break;
    end;
  end;
end;


end.

