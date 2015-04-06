{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2014 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a scrollable frame widget.

      This unit was originally written by David Emerson <dle3ab@angelbase.com>
}
unit fpg_scrollframe;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_panel,
  fpg_scrollbar;

type
  
  TfpgScrollFrame = class;


  TfpgEmbeddingFrame = class (TfpgFrame)
  // The purpose of the EmbeddingFrame is to pass scroll events to the ParentScrollFrame
  private
    FParentScrollFrame : TfpgScrollFrame;
  protected
    procedure HandleMouseScroll(x, y: integer; shiftstate: TShiftState;
        delta: smallint); override;
    procedure HandleMouseHorizScroll(x, y: integer; shiftstate: TShiftState; 
        delta: smallint); override;
  public
    property ParentScrollFrame : TfpgScrollFrame read FParentScrollFrame write FParentScrollFrame;
  end;


  TfpgAutoSizingFrame = class (TfpgEmbeddingFrame)
  private
    FMarginBR : integer;
    procedure SetMarginBR (AValue: integer);
  public
    procedure AfterConstruction; override;
    procedure AdjustDimsFor (w : TfpgWidget; updatewp: boolean = true);
    procedure AdjustDimsWithout (w : TfpgWidget);
    procedure RecalcFrameSize;
    property MarginBR : integer read FMarginBR write SetMarginBR; // bottom-right margin
  end;

  TfpgASFrameClass = class of TfpgAutoSizingFrame;


  TfpgScrollFrame = class(TfpgFrame)
  private
    FContentFrame: TfpgAutoSizingFrame;
    FVisibleArea: TfpgEmbeddingFrame;
    FHScrollBar: TfpgScrollBar;
    FVScrollBar: TfpgScrollBar;
    FScrollBarStyle: TfpgScrollStyle;
    function    GetXOffset: integer;
    function    GetYOffset: integer;
    procedure   SetXOffset(x: integer);
    procedure   SetYOffset(y: integer);
  protected
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandleMouseHorizScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
    procedure   HandleShow; override;
    procedure   HandlePaint; override;
    procedure   HScrollBarMove(Sender: TObject; position: integer);
    procedure   VScrollBarMove(Sender: TObject; position: integer);
    procedure   UpdateScrollbars; virtual;
    property    XOffset: integer read GetXOffset write SetXOffset; // these do not...
    property    YOffset: integer read GetYOffset write SetYOffset; // ...updatewindowposition
  public
    constructor Create (AOwner: TComponent); override;
    constructor Create (AOwner: TComponent; ContentFrameType: TfpgASFrameClass); virtual;
    procedure   AfterCreate; override;
    procedure   SetContentFrameType(AContentFrameType: TfpgASFrameClass);
    property    ContentFrame: TfpgAutoSizingFrame read FContentFrame write FContentFrame;
  end;


implementation


{ TfpgEmbeddingFrame }

procedure TfpgEmbeddingFrame.HandleMouseScroll(x, y: integer; 
  shiftstate: TShiftState; delta: smallint);
begin
  ParentScrollFrame.HandleMouseScroll(x, y, shiftstate, delta);
end;

procedure TfpgEmbeddingFrame.HandleMouseHorizScroll(x, y: integer; 
  shiftstate: TShiftState; delta: smallint);
begin
  ParentScrollFrame.HandleMouseHorizScroll(x, y, shiftstate, delta);
end;


{ TfpgAutoSizingFrame }

procedure TfpgAutoSizingFrame.SetMarginBR(AValue: integer);
begin
  if FMarginBR=AValue then Exit;
  FMarginBR:=AValue;
  RecalcFrameSize;
end;

procedure TfpgAutoSizingFrame.AfterConstruction;
begin
  inherited AfterConstruction;
  RecalcFrameSize;
end;

procedure TfpgAutoSizingFrame.AdjustDimsFor (w: TfpgWidget; updatewp: boolean = true);
var
  new_w, new_h: integer;
begin
  if not w.Visible then
    Exit;
  new_w := w.Right+MarginBR+1;
  new_h := w.Bottom+MarginBR+1;
  if (Width < new_w) or (Height < new_h) then
  begin
    HandleResize(new_w, new_h);
    if updatewp then
      if ParentScrollFrame is TfpgScrollFrame then
        ParentScrollFrame.UpdateScrollbars
      else
        UpdateWindowPosition;
  end;
end;

procedure TfpgAutoSizingFrame.AdjustDimsWithout (w: TfpgWidget);
begin
  if (Width = w.Right+MarginBR+1)
  or (Height = w.Bottom+MarginBR+1) then
    RecalcFrameSize;
end;

procedure TfpgAutoSizingFrame.RecalcFrameSize;
var
  i : integer;
  c : TComponent;
  max_w, max_h : integer;
  this_need : integer;
begin
  if ComponentCount=0 then
    Exit;
  max_w := 1;
  max_h := 1;
  for i := 0 to ComponentCount-1 do begin
    c := Components[i];
    if c is TfpgWidget then
    begin
      if not TfpgWidget(c).Visible then
        continue;
      this_need := TfpgWidget(c).right+MarginBR+1;
      if (this_need>max_w) then
        max_w := this_need;
      this_need := TfpgWidget(c).bottom+MarginBR+1;
      if (this_need>max_h) then
        max_h := this_need;
    end;
  end;
  HandleResize(max_w, max_h);
  if ParentScrollFrame is TfpgScrollFrame then
    ParentScrollFrame.UpdateScrollbars
  else
    UpdateWindowPosition;
end;


{ TfpgScrollFrame }

function TfpgScrollFrame.GetXOffset: integer;
begin
  result := -FContentFrame.Left;
end;

function TfpgScrollFrame.GetYOffset: integer;
begin
  result := -FContentFrame.Top;
end;

procedure TfpgScrollFrame.SetXOffset (x: integer);
begin
  if ContentFrame.Left = -x then
    Exit;
  FContentFrame.Left := -x;
end;

procedure TfpgScrollFrame.SetYOffset (y: integer);
begin
  if ContentFrame.Top = -y then
    Exit;
  FContentFrame.Top := -y;
end;

procedure TfpgScrollFrame.HandleMouseScroll(x, y: integer; 
  shiftstate: TShiftState; delta: smallint);
begin
  inherited HandleMouseScroll(x, y, shiftstate, delta);
  with FVScrollBar do
  begin
    if not Visible then
      Exit;
    Position:=Position+delta*ScrollStep;
    if YOffset=Position then
      Exit;
    YOffset:=Position;
  end;
  UpdateScrollbars;
end;

procedure TfpgScrollFrame.HandleMouseHorizScroll(x, y: integer; 
  shiftstate: TShiftState; delta: smallint);
begin
  inherited HandleMouseHorizScroll(x, y, shiftstate, delta);
  with FHScrollBar do
  begin
    if not Visible then
      Exit;
    Position:=Position+delta*ScrollStep;
    if XOffset=Position then
      Exit;
    XOffset:=Position;
  end;
  UpdateScrollbars;
end;

procedure TfpgScrollFrame.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);
  if (csLoading in ComponentState) or (csUpdating in ComponentState) then
    Exit; //==>
  if HasHandle then
    UpdateScrollBars;
end;

procedure TfpgScrollFrame.HandleShow;
begin
  inherited HandleShow;
  if (csLoading in ComponentState) then
    Exit;
  UpdateScrollBars;
end;

procedure TfpgScrollFrame.HandlePaint;
begin
  if csDesigning in ComponentState then
  begin
    // clear background rectangle
    Canvas.Clear(clDarkGray);
    // When designing, don't draw colors
    // but draw an outline
    Canvas.SetLineStyle(1, lsDash);
    Canvas.DrawRectangle(GetClientRect);
    Canvas.SetLineStyle(1, lsSolid);
    Canvas.Color := clUIDesignerGreen;
    Canvas.DrawLine(0, 0, Width, Height);
    Canvas.DrawLine(Width, 0, 0, Height);
    Canvas.TextColor := clShadow1;
    Canvas.DrawText(5, 5, Name + ': ' + ClassName);
    Exit;  //==>
  end;

  inherited HandlePaint;
end;

procedure TfpgScrollFrame.HScrollBarMove (Sender: TObject; position: integer);
begin
  if position = XOffset then
    Exit;
  XOffset := position;
  FContentFrame.UpdateWindowPosition;
end;

procedure TfpgScrollFrame.VScrollBarMove (Sender: TObject; position: integer);
begin
  if position = YOffset then
    Exit;
  YOffset := position;
  FContentFrame.UpdateWindowPosition;
end;

procedure TfpgScrollFrame.UpdateScrollbars;
var
  contentWidth, contentHeight: integer;
  visWidth, visHeight: integer;
  Hfits, Vfits : boolean;
  showHsb, showVsb : boolean;
  prevHideHsb, prevHideVsb : boolean;

  procedure hideScrollbar (sb : TfpgScrollBar);
  begin
    with sb do
      if Visible then
      begin
        Visible := False;
        UpdateWindowPosition;
      end;
  end;

  procedure getVisWidth;
  begin
    if showVsb then
      visWidth := Width - (FVScrollBar.Width-1)
    else
      visWidth := Width;
    Hfits := visWidth >= contentWidth
  end;

  procedure getVisHeight;
  begin
    if showHsb then
      visHeight := Height - (FHScrollBar.Height-1)
    else
      visHeight := Height;
    Vfits := visHeight >= contentHeight;
  end;

begin
  if (csLoading in ComponentState) or (csUpdating in ComponentState) then
    Exit; //==>

  // if we don't want any scrollbars, hide them and exit
  if FScrollBarStyle = ssNone then
  begin
    hideScrollbar (FHScrollBar);
    hideScrollbar (FVScrollBar);
    exit;
  end;

  // preliminary width/height calculations
  prevHideHsb := not FHScrollBar.Visible;
  prevHideVsb := not FVScrollBar.Visible;
  showVsb := (FScrollBarStyle = ssBothVisible);
  showHsb := showVsb;
  contentWidth := ContentFrame.Width;
  contentHeight := ContentFrame.Height;
  getVisWidth;
  getVisHeight;

  // determine whether to show scrollbars for different configurations
  case FScrollBarStyle of
    ssHorizontal:
        begin
          hideScrollbar (FVScrollBar);
          if not Hfits then
          begin
            showHsb := true;
            getVisHeight;
          end;
        end;
    ssVertical:
        begin
          hideScrollbar (FHScrollBar);
          if not Vfits then
          begin
            showVsb := true;
            getVisWidth;
          end;
        end;
    ssAutoBoth:
        if not Vfits then
        begin
          showVsb := true;
          getVisWidth;
          if not Hfits then
          begin
            showHsb := true;
            getVisHeight;
            getVisWidth;
          end;
        end
        else if not Hfits then
        begin
          showHsb := true;
          getVisHeight;
          if not Vfits then
          begin
            showVsb := true;
            getVisWidth;
            getVisHeight;
          end;
        end;
  end;

  // show or hide the scrollbars

  if showVsb then with FVScrollBar do
  begin
    if prevHideVsb then
      Position := 0;
    Visible := true;
    Min := 0;
    Max := contentHeight - visHeight;  // may set position!
    YOffset := Position;
    if contentHeight > 0 then
      SliderSize := visHeight / contentHeight
    else
      SliderSize := 0;
    RepaintSlider;
    Top     := 0;
    Left    := visWidth;
    Height  := visHeight;
    PageSize:= visHeight;
  end
  else
  begin
    FVScrollBar.Visible := false;
    if Vfits then // if vertical doesn't fit and no scrollbar, do not change offset
      YOffset := 0;
  end;

  if showHsb then with FHScrollBar do
  begin
    if prevHideHsb then
      Position := 0;
    Visible := true;
    Min := 0;
    Max := contentWidth - visWidth;  // may set position!
    XOffset := Position;
    if contentWidth > 0 then
      SliderSize := visWidth / contentWidth
    else
      SliderSize := 0;
    RepaintSlider;
    Top     := visHeight;
    Left    := 0;
    Width   := visWidth;
    PageSize:= visWidth;
  end
  else
  begin
    FHScrollBar.Visible := false;
    if Hfits then // if horizontal doesn't fit and no scrollbar, do not change offset
      XOffset := 0;
  end;

  FVScrollBar.UpdateWindowPosition;
  FHScrollBar.UpdateWindowPosition;

  FVisibleArea.SetPosition(0, 0, visWidth, visHeight);
  FVisibleArea.UpdateWindowPosition;

  FContentFrame.UpdateWindowPosition;
end;

constructor TfpgScrollFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FVisibleArea := TfpgEmbeddingFrame.Create(self);
  FVisibleArea.HandleMove(0, 0);
  FVisibleArea.ParentScrollFrame := self;

  FContentFrame := TfpgAutoSizingFrame.Create(FVisibleArea);
  FContentFrame.HandleMove(0, 0);
  FContentFrame.ParentScrollFrame := self;
end;

constructor TfpgScrollFrame.Create(AOwner: TComponent; ContentFrameType: TfpgASFrameClass);
begin
  inherited Create(AOwner);

  FVisibleArea := TfpgEmbeddingFrame.Create(self);
  FVisibleArea.HandleMove(0, 0);
  FVisibleArea.ParentScrollFrame := self;

  FContentFrame := ContentFrameType.Create(FVisibleArea);
  FContentFrame.HandleMove(0, 0);
  FContentFrame.ParentScrollFrame := self;
end;

procedure TfpgScrollFrame.AfterCreate;
begin
  inherited AfterCreate;

  FVScrollBar := TfpgScrollBar.Create(self);
  with FVScrollBar do begin
    Orientation := orVertical;
    OnScroll    := @VScrollBarMove;
    Position    := 0;
    ScrollStep  := 10;
    end;

  FHScrollBar := TfpgScrollBar.Create(self);
  with FHScrollBar do begin
    Orientation := orHorizontal;
    OnScroll    := @HScrollBarMove;
    Position    := 0;
    ScrollStep  := 10;
    end;

  FScrollBarStyle := ssAutoBoth;
end;

procedure TfpgScrollFrame.SetContentFrameType(AContentFrameType: TfpgASFrameClass);
begin
  if Assigned(FContentFrame) then
    FContentFrame.Free;
  FContentFrame := AContentFrameType.Create(FVisibleArea);
  FContentFrame.HandleMove(0, 0);
  FContentFrame.ParentScrollFrame := self;
end;


end.
