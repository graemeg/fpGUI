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

  { TfpgAutoSizingFrame }

  TfpgAutoSizingFrame = class (TfpgFrame)
  private
    FMarginBR : integer;
    FParentScrollFrame : TfpgScrollFrame; // it's actually the grandparent
    procedure SetMarginBR (AValue: integer);
    procedure UpdatePos;
  public
    procedure AdjustDimsFor (w : TfpgWindow; updatewp: boolean = true);
    procedure AdjustDimsWithout (w : TfpgWindow);
    procedure RecalcFrameSize;
    property MarginBR : integer read FMarginBR write SetMarginBR; // bottom-right margin
    property ParentScrollFrame : TfpgScrollFrame read FParentScrollFrame write FParentScrollFrame;
  end;

  TfpgASFrameClass = class of TfpgAutoSizingFrame;

  { TfpgScrollFrame }

  TfpgScrollFrame = class (TfpgFrame)
  private
    FContentFrame : TfpgAutoSizingFrame;
    FScrollFrame : TfpgFrame;
    FHScrollBar : TfpgScrollBar;
    FVScrollBar : TfpgScrollBar;
    FScrollBarStyle : TfpgScrollStyle;
    function GetXOffset: integer;
    function GetYOffset: integer;
    procedure SetXOffset (x: integer);
    procedure SetYOffset (y: integer);
  protected
    procedure HandleResize(awidth, aheight: TfpgCoord); override;
    procedure HandleShow; override;
    procedure HScrollBarMove(Sender: TObject; position: integer);
    procedure VScrollBarMove(Sender: TObject; position: integer);
    procedure UpdateScrollbars; virtual;
    property XOffset : integer read GetXOffset write SetXOffset; // these do not...
    property YOffset : integer read GetYOffset write SetYOffset; // ...updatewindowposition
  public
    constructor Create (AOwner: TComponent); override;
    constructor Create (AOwner: TComponent; ContentFrameType: TfpgASFrameClass); virtual;
    procedure AfterCreate; override;
    property ContentFrame : TfpgAutoSizingFrame read FContentFrame write FContentFrame;
  end;

implementation

{ TfpgAutoSizingFrame }

procedure TfpgAutoSizingFrame.SetMarginBR(AValue: integer);
begin
  if FMarginBR=AValue then Exit;
  FMarginBR:=AValue;
  RecalcFrameSize;
end;

procedure TfpgAutoSizingFrame.UpdatePos;
begin
  UpdateWindowPosition;
  if ParentScrollFrame is TfpgScrollFrame then
    ParentScrollFrame.UpdateScrollbars;
end;

procedure TfpgAutoSizingFrame.AdjustDimsFor (w: TfpgWindow; updatewp: boolean = true);
var
  new_w, new_h: integer;
begin
  new_w := w.Right+MarginBR+1;
  new_h := w.Bottom+MarginBR+1;
  if (Width < new_w) or (Height < new_h) then
  begin
    HandleResize(new_w, new_h);
    if updatewp then
      UpdatePos;
  end;
end;

procedure TfpgAutoSizingFrame.AdjustDimsWithout (w: TfpgWindow);
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
  par : TfpgWidget;
begin
  if ComponentCount=0 then
    Exit;
  max_w := 1;
  max_h := 1;
  for i := 0 to ComponentCount-1 do begin
    c := Components[i];
    if c is TfpgWindow then
    begin
      this_need := TfpgWindow(c).right+MarginBR+1;
      if (this_need>max_w) then
        max_w := this_need;
      this_need := TfpgWindow(c).bottom+MarginBR+1;
      if (this_need>max_h) then
        max_h := this_need;
    end;
  end;
  HandleResize(max_w, max_h);
  UpdatePos;
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

  FScrollFrame.SetPosition(0, 0, visWidth, visHeight);
  FScrollFrame.UpdateWindowPosition;

  FContentFrame.UpdateWindowPosition;
end;

constructor TfpgScrollFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FScrollFrame := TfpgFrame.Create(self);
  FScrollFrame.SetPosition(0, 0, 1, 1);

  FContentFrame := TfpgAutoSizingFrame.Create(FScrollFrame);
  FContentFrame.SetPosition(0, 0, 1, 1);
  FContentFrame.ParentScrollFrame := self;
end;

constructor TfpgScrollFrame.Create(AOwner: TComponent; ContentFrameType: TfpgASFrameClass);
begin
  inherited Create(AOwner);

  FScrollFrame := TfpgFrame.Create(self);
  FScrollFrame.Left := 0;
  FScrollFrame.Top := 0;

  FContentFrame := ContentFrameType.Create(FScrollFrame);
  FContentFrame.Left := 0;
  FContentFrame.Top := 0;
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
    end;

  FHScrollBar := TfpgScrollBar.Create(self);
  with FHScrollBar do begin
    Orientation := orHorizontal;
    OnScroll    := @HScrollBarMove;
    Position    := 0;
    end;

  FScrollBarStyle := ssAutoBoth;
end;


end.
