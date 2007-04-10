{
    fpGUI  -  Free Pascal GUI Library
    
    Layout Managers class declarations
    
    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pas}

{$IFDEF read_interface}

// ===================================================================
//   Layouts
// ===================================================================

type

  ELayoutError = class(Exception);

  THorzAlign = (horzLeft, horzCenter, horzRight, horzFill);
  TVertAlign = (vertTop, vertCenter, vertBottom, vertFill);

  TFLayoutItem = class(TCollectionItem)
  private
    FWidget: TFWidget;
  published
    property Widget: TFWidget read FWidget write FWidget;
  end;

  TFWidgetArrayInfo = record
    min: Integer;
    def: Integer;
    max: Integer;
    MinFlag: Boolean;
    MaxFlag: Boolean;
  end;

  TFWidgetArrayInfoArray = array[0..(1 shl 30) div SizeOf(TFWidgetArrayInfo) - 1] of TFWidgetArrayInfo;
  PWidgetArrayInfoArray = ^TFWidgetArrayInfoArray;


  TFLayout = class(TFContainerWidget)
  protected
    FWidgets: TCollection;
    FBorderSpacing: Integer;
    IsRecalcingLayout: Boolean;
    function    GetChildCount: Integer; override;
    function    GetChild(Index: Integer): TFWidget; override;
    property    BorderSpacing: Integer read FBorderSpacing write FBorderSpacing;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    ContainsChild(AChild: TFWidget): Boolean; override;
  end;


// -------------------------------------------------------------------
//   Fixed Layout
// -------------------------------------------------------------------

  TFFixedItem = class(TFLayoutItem)
  public
    Left: Integer;
    Top: Integer;
  end;


  TFFixedLayout = class(TFLayout)
  protected
    procedure   CalcSizes; override;
    procedure   Resized; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AddWidget(AWidget: TFWidget; ALeft, ATop: Integer);
    procedure   MoveWidget(AWidget: TFWidget; ALeft, ATop: Integer);
  published
    property    Enabled;
  end;


// -------------------------------------------------------------------
//   Docking Layout
// -------------------------------------------------------------------

  TDockingMode = (dmTop, dmBottom, dmLeft, dmRight, dmClient, dmUndocked);

  TFDockingItem = class(TFLayoutItem)
  public
    Left: Integer;
    Top: Integer;
    DockingMode: TDockingMode;
  end;
  

  TFDockingLayout = class(TFLayout)
  private
    procedure   InternalLayoutChildren;
  protected
    procedure   CalcSizes; override;
    procedure   Resized; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AddWidget(AWidget: TFWidget; ADockingMode: TDockingMode);
    procedure   AddWidget(AWidget: TFWidget; ALeft, ATop: Integer);
  published
    property    Enabled;
  end;


// -------------------------------------------------------------------
//   Box Layout
// -------------------------------------------------------------------

  TFCustomBoxLayout = class(TFLayout)
  private
    FHorzAlign: THorzAlign;
    FVertAlign: TVertAlign;
    FOrientation: TOrientation;
    FSpacing: Integer;
    procedure   SetOrientation(AOrientation: TOrientation);
  protected
    procedure   CalcSizes; override;
    procedure   Resized; override;
    property    HorzAlign: THorzAlign read FHorzAlign write FHorzAlign default horzFill;
    property    VertAlign: TVertAlign read FVertAlign write FVertAlign default vertFill;
    property    Orientation: TOrientation read FOrientation write SetOrientation default Horizontal;
    property    Spacing: Integer read FSpacing write FSpacing default 4;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   InsertChild(AChild: TFWidget); override;
    procedure   RemoveChild(AChild: TFWidget); override;
  end;


  TFBoxLayout = class(TFCustomBoxLayout)
  published
    property CanExpandWidth;
    property CanExpandHeight;
    property Enabled;
    property BorderSpacing;
    property HorzAlign;
    property Orientation;
    property Spacing;
    property VertAlign;
  end;


// -------------------------------------------------------------------
//   Grid Layout
// -------------------------------------------------------------------

  TFGridItem = class(TFLayoutItem)
  private
    FX, FY, FWidth, FHeight: Integer;
  public
    constructor Create(ACollection: TCollection); override;
  published
    property x: Integer read FX write FX default 1;
    property y: Integer read FY write FY default 1;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
  end;


  { TFCustomGridLayout }

  TFCustomGridLayout = class(TFLayout)
  private
    FColCount: Integer;
    FRowCount: Integer;
    FColSpacing: Integer;
    FRowSpacing: Integer;
//    FWidgets: TCollection;
    procedure   SetColCount(AColCount: Integer);
    procedure   SetRowCount(ARowCount: Integer);
    procedure   SetColSpacing(AColSpacing: Integer);
    procedure   SetRowSpacing(ARowSpacing: Integer);
  protected
    procedure   InitSizeInfos(var ColInfos, RowInfos: PWidgetArrayInfoArray);
    procedure   CalcSizes; override;
    procedure   Resized; override;
    property    GridPositions: TCollection read FWidgets write FWidgets;
    property    ColCount: Integer read FColCount write SetColCount default 2;
    property    RowCount: Integer read FRowCount write SetRowCount default 2;
    property    ColSpacing: Integer read FColSpacing write SetColSpacing default 4;
    property    RowSpacing: Integer read FRowSpacing write SetRowSpacing default 4;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AddWidget(AWidget: TFWidget; x, y, w, h: Integer);
    procedure   MoveWidget(AWidget: TFWidget; x, y, w, h: Integer);
  end;


  TFGridLayout = class(TFCustomGridLayout)
  published
    property Enabled;
    property ColCount;
    property RowCount;
    property ColSpacing;
    property RowSpacing;
    property GridPositions;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}

// ===================================================================
//   Common layout widgets implementation
// ===================================================================

resourcestring
  SLayoutWidgetNotFound = 'Layout child widget not found';


procedure AddToSizes(infos: PWidgetArrayInfoArray; count: Integer; TooMuch: Integer);
var
  i, add, FoundElements: Integer;
begin
  while TooMuch > 0 do
  begin
    add := TooMuch;
    FoundElements := 0;
    for i := 0 to count - 1 do
    begin
      if not infos^[i].MaxFlag then
        continue;
      Inc(FoundElements);
      if infos^[i].def + add > infos^[i].max then
        add := infos^[i].max - infos^[i].def;
    end;
    if FoundElements > 0 then
    begin
      add := add div FoundElements;
      if add <= 0 then
        add := 1;
    end else
      break;

    for i := 0 to count - 1 do
    begin
      if not infos^[i].MaxFlag then
        continue;
      Inc(infos^[i].def, add);
      Dec(TooMuch, add);
      if TooMuch = 0 then
        exit;
      if infos^[i].def = infos^[i].max then
        infos^[i].MaxFlag := False;
    end;
  end;
end;

procedure SubFromSizes(infos: PWidgetArrayInfoArray; count: Integer; TooMuch: Integer);
var
  i, sub, FoundElements: Integer;
begin
  while TooMuch > 0 do
  begin
    sub := TooMuch;
    FoundElements := 0;
    for i := 0 to count - 1 do
    begin
      if not infos^[i].MinFlag then
        continue;
      Inc(FoundElements);
      if infos^[i].def - sub < infos^[i].min then
        sub := infos^[i].def - infos^[i].min;
    end;
    if FoundElements > 0 then
    begin
      sub := sub div FoundElements;
      if sub <= 0 then
        sub := 1;
    end else
      break;

    for i := 0 to count - 1 do
    begin
      if not infos^[i].MinFlag then
        continue;
      Dec(infos^[i].def, sub);
      Dec(TooMuch, sub);
      if TooMuch = 0 then
        exit;
      if infos^[i].def = infos^[i].min then
        infos^[i].MinFlag := False;
    end;
  end;
end;

procedure CorrectSizes(infos: PWidgetArrayInfoArray; count: Integer; SizeDiff: Integer);
var
  TooMuch: Integer;
begin
  TooMuch := SizeDiff;
  if TooMuch > 0 then
    AddToSizes(infos, count, TooMuch)
  else if TooMuch < 0 then
    SubFromSizes(infos, count, -TooMuch);
end;


// -------------------------------------------------------------------
//   TFLayout
// -------------------------------------------------------------------

constructor TFLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanExpandWidth := True;
  FCanExpandHeight := True;
end;

destructor TFLayout.Destroy;
begin
  FWidgets.Free;
  inherited Destroy;
end;

function TFLayout.ContainsChild(AChild: TFWidget): Boolean;
var
  i: Integer;
begin
  for i := 0 to FWidgets.Count - 1 do
    if TFLayoutItem(FWidgets.Items[i]).Widget = AChild then
    begin
      Result := True;
      Exit; //==>
    end;
  Result := False;
end;

function TFLayout.GetChildCount: Integer;
begin
  Result := FWidgets.Count;
end;

function TFLayout.GetChild(Index: Integer): TFWidget;
begin
  Result := TFLayoutItem(FWidgets.Items[Index]).Widget;
end;


// -------------------------------------------------------------------
//   TFFixedLayout
// -------------------------------------------------------------------

{ Find the biggest X and Y coordinates that will cover all Widgets, by looking
  at each Widget in the Layout. }
procedure TFFixedLayout.CalcSizes;
var
  i: Integer;
  item: TFFixedItem;
begin
  if FWidgets.Count = 0 then
  begin
    FDefSize := gfxbase.Size(50, 50)
  end
  else
  begin
    for i := 0 to FWidgets.Count - 1 do
    begin
      item := TFFixedItem(FWidgets.Items[i]);
      FDefSize.cx := Max(DefSize.cx, item.Left + item.Widget.DefSize.cx);
      FDefSize.cy := Max(DefSize.cy, item.Top + item.Widget.DefSize.cy);
    end;
  end;  { if..else }
end;

procedure TFFixedLayout.Resized;
var
  i: Integer;
  item: TFFixedItem;
begin
  writeln('==> ' + Classname + '.Resized');

  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TFFixedItem(FWidgets.Items[i]);
    item.Widget.SetBounds(item.Widget.Left, item.Widget.Top, item.Widget.Width, item.Widget.Height);
  end;
end;

constructor TFFixedLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TCollection.Create(TFFixedItem);
end;

procedure TFFixedLayout.AddWidget(AWidget: TFWidget; ALeft, ATop: Integer);
var
  item: TFFixedItem;
begin
  if not ContainsChild(AWidget) then
  begin
    item            := TFFixedItem(FWidgets.Add);
    item.Left       := ALeft;
    item.Top        := ATop;
    item.Widget     := AWidget;
    AWidget.Parent  := self;
    AWidget.SetBounds(Point(item.Left, item.Top), item.Widget.DefSize);
  end;
end;

procedure TFFixedLayout.MoveWidget(AWidget: TFWidget; ALeft, ATop: Integer);
var
  i: integer;
  item: TFFixedItem;
begin
  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TFFixedItem(FWidgets.Items[i]);
    if item.Widget = AWidget then
    begin
      item.Left := ALeft;
      item.Top  := ATop;
      AWidget.SetBounds(Point(item.Left, item.Top), item.Widget.DefSize);
      Exit; //==>
    end;
  end;
  raise ELayoutError.Create(SLayoutWidgetNotFound);
end;

{
procedure TFixedLayout.EvLayoutChildren(Canvas: TGfxCanvas);
var
  i: Integer;
  item: TFixedItem;
begin
  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TFixedItem(FWidgets.Items[i]);
    item.Widget.SetBounds(item.Left, item.Top, item.Widget.DefSize.cx, item.Widget.DefSize.cy);
  end;
end;
}

// -------------------------------------------------------------------
//   TFDockingLayout
// -------------------------------------------------------------------

constructor TFDockingLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TCollection.Create(TFDockingItem);
end;

procedure TFDockingLayout.InternalLayoutChildren;
var
  clx, cly, clw, clh: Integer;   // Client rectangle
  ClientWidget: TFWidget;
  i, WidgetW, WidgetH: Integer;
  item: TFDockingItem;
begin
  clx := 0;
  cly := 0;
  clw := BoundsSize.cx;
  clh := BoundsSize.cy;
//WriteLn('=> DockingLayout.EvLayoutChildren ', BoundsSize.cx, ' x ', BoundsSize.cy);
  if (clw = 0) or (clh = 0) then
    exit;

  // Process all attached widgets
  ClientWidget := nil;
  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TFDockingItem(FWidgets.Items[i]);
    case item.DockingMode of
      dmLeft:
        begin
      	  WidgetW := item.Widget.DefSize.cx;
      	  WidgetH := clh;
      	  item.Left := clx;
      	  item.Top := cly;
      	  Inc(clx, WidgetW);
      	  Dec(clw, WidgetW);
        end;
      dmTop:
        begin
      	  WidgetW := clw;
      	  WidgetH := item.Widget.DefSize.cy;
      	  item.Left := clx;
      	  item.Top := cly;
      	  Inc(cly, WidgetH);
      	  Dec(clh, WidgetH);
        end;
      dmRight:
        begin
      	  WidgetW := item.Widget.DefSize.cx;
      	  WidgetH := clh;
      	  item.Left := clx + clw - WidgetW;
                item.Top := cly;
      	  Dec(clw, WidgetW);
        end;
      dmBottom:
        begin
      	  WidgetH := item.Widget.DefSize.cy;
      	  WidgetW := clw;
      	  item.Left := clx;
      	  item.Top := cly + clh - WidgetH;
      	  Dec(clh, WidgetH);
        end;
      dmClient:
	      ClientWidget := item.Widget;
    end;  { case }
    if item.DockingMode <> dmClient then
      item.Widget.SetBounds(item.Left, item.Top, WidgetW, WidgetH);
  end;  { for }
  if Assigned(ClientWidget) then
    ClientWidget.SetBounds(clx, cly, clw, clh);
end;

procedure TFDockingLayout.CalcSizes;
var
  i: Integer;
  item: TFDockingItem;
  w: TFWidget;
  cw: TFWidget;
begin
  if FWidgets.Count = 0 then
  begin
    FDefSize := gfxbase.Size(200, 200);
    Exit; //==>
  end;

  // Find the client widget (widget with DockingMode "dmClient")
  cw := nil;
  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TFDockingItem(FWidgets.Items[i]);
    if item.DockingMode = dmClient then
    begin
      cw := item.Widget;
      Break;  //==>
    end;
  end;

  if Assigned(cw) then
  begin
    FMinSize := cw.MinSize;
    FMaxSize := cw.MaxSize;
    FDefSize := cw.DefSize;
  end
  else
    FDefSize := gfxbase.Size(200, 200);

  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TFDockingItem(FWidgets.Items[i]);
    w := item.Widget;
    case item.DockingMode of
      dmTop, dmBottom:
        begin
          if MinSize.cx < w.MinSize.cx then
	          FMinSize.cx := w.MinSize.cx;
      	  Inc(FMinSize.cy, w.MinSize.cy);
      	  if MaxSize.cx < w.MaxSize.cx then
      	    FMaxSize.cx := w.MaxSize.cx;
      	  if MaxSize.cy < InfiniteSize then
      	    Inc(FMaxSize.cy, w.MaxSize.cy);
      	  if DefSize.cx < w.DefSize.cx then
      	    FDefSize.cx := w.DefSize.cx;
      	  Inc(FDefSize.cy, w.DefSize.cy);
        end;
      dmLeft, dmRight:
        begin
	        Inc(FMinSize.cx, w.MinSize.cx);
          if MinSize.cy < w.MinSize.cy then
      	    FMinSize.cy := w.MinSize.cy;
      	  if MaxSize.cx < InfiniteSize then
      	    Inc(FMaxSize.cx, w.MaxSize.cx);
      	  if MaxSize.cy < w.MaxSize.cy then
      	    FMaxSize.cy := w.MaxSize.cy;
      	  Inc(FDefSize.cx, w.DefSize.cx);
      	  if DefSize.cy < w.DefSize.cy then
      	    FDefSize.cy := w.DefSize.cy;
        end;
    end;  { case }
  end;  { for }
end;

procedure TFDockingLayout.Resized;
begin
  inherited Resized;
  InternalLayoutChildren;
end;

procedure TFDockingLayout.AddWidget(AWidget: TFWidget; ADockingMode: TDockingMode);
var
  item: TFDockingItem;
begin
  if not ContainsChild(AWidget) then
  begin
    item              := TFDockingItem(FWidgets.Add);
    item.Widget       := AWidget;
    item.DockingMode  := ADockingMode;
    AWidget.Parent    := Self;
  end;
end;

procedure TFDockingLayout.AddWidget(AWidget: TFWidget; ALeft, ATop: Integer);
var
  item: TFDockingItem;
begin
  if not ContainsChild(AWidget) then
  begin
    item              := TFDockingItem(FWidgets.Add);
    item.Widget       := AWidget;
    item.DockingMode  := dmUndocked;
    item.Left         := ALeft;
    item.Top          := ATop;
    AWidget.Parent    := Self;
  end;
end;


// -------------------------------------------------------------------
//   TFCustomBoxLayout
// -------------------------------------------------------------------

procedure TFCustomBoxLayout.SetOrientation(AOrientation: TOrientation);
begin
  if AOrientation <> FOrientation then
  begin
    FOrientation := AOrientation;
    Update;
  end;
end;


constructor TFCustomBoxLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TCollection.Create(TFLayoutItem);
  FOrientation := Horizontal;
  FHorzAlign := horzFill;
  FVertAlign := vertFill;
  FSpacing := 4;
end;


procedure TFCustomBoxLayout.InsertChild(AChild: TFWidget);
var
  item: TFLayoutItem;
begin
  if not ContainsChild(AChild) then
  begin
    item := TFLayoutItem(FWidgets.Add);
    item.Widget := AChild;
    AChild.Parent := Self;
  end;
end;


procedure TFCustomBoxLayout.RemoveChild(AChild: TFWidget);
begin
  {$Warning Not implemented yet.}
  raise Exception.Create('TCustomBoxLayout.RemoveChild - Not implemented yet');
end;


procedure TFCustomBoxLayout.CalcSizes;
var
  i: Integer;
  item: TFLayoutItem;
begin
  i := (FWidgets.Count - 1) * FSpacing;
  if Orientation = Horizontal then
  begin
    FMinSize := Size(i, 0);
    FDefSize := MinSize;
    if HorzAlign = horzFill then
      FMaxSize.cx := i
    else
      FMaxSize.cx := InfiniteSize;
    FMaxSize.cy := InfiniteSize;
  end
  else
  begin
    FMinSize := Size(0, i);
    FDefSize := MinSize;
    FMaxSize.cx := InfiniteSize;
    if VertAlign = vertFill then
      FMaxSize.cy := i
    else
      FMaxSize.cy := InfiniteSize;
  end;  { if..else }

  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TFLayoutItem(FWidgets.Items[i]);
    if Orientation = Horizontal then
    begin
      Inc(FMinSize.cx, item.Widget.MinSize.cx);
      Inc(FDefSize.cx, item.Widget.DefSize.cx);
      FMaxSize.cx := Min(InfiniteSize, MaxSize.cx + item.Widget.MaxSize.cx);
      FMaxSize.cy := Min(MaxSize.cy, item.Widget.MaxSize.cy);
      if MinSize.cy < item.Widget.MinSize.cy then
        FMinSize.cy := item.Widget.MinSize.cy;
      if DefSize.cy < item.Widget.DefSize.cy then
        FDefSize.cy := item.Widget.DefSize.cy;
      if MaxSize.cy > item.Widget.MaxSize.cy then
        FMaxSize.cy := item.Widget.MaxSize.cy;
    end
    else
    begin { Vertical }
      Inc(FMinSize.cy, item.Widget.MinSize.cy);
      Inc(FDefSize.cy, item.Widget.DefSize.cy);
      FMaxSize.cx := Min(MaxSize.cx, item.Widget.MaxSize.cx);
      FMaxSize.cy := Min(InfiniteSize, MaxSize.cy + item.Widget.MaxSize.cy);
      if MinSize.cx < item.Widget.MinSize.cx then
        FMinSize.cx := item.Widget.MinSize.cx;
      if DefSize.cx < item.Widget.DefSize.cx then
        FDefSize.cx := item.Widget.DefSize.cx;
      if MaxSize.cx > item.Widget.MaxSize.cx then
        FMaxSize.cx := item.Widget.MaxSize.cx;
    end;  { if..else }
  end;

  Inc(FMinSize.cx, 2 * FBorderSpacing);
  Inc(FMinSize.cy, 2 * FBorderSpacing);
  Inc(FDefSize.cx, 2 * FBorderSpacing);
  Inc(FDefSize.cy, 2 * FBorderSpacing);
  FMaxSize.cx := Min(InfiniteSize, MaxSize.cx + 2 * FBorderSpacing);
  FMaxSize.cy := Min(InfiniteSize, MaxSize.cy + 2 * FBorderSpacing);
end;

procedure TFCustomBoxLayout.Resized;
var
  sizes: PWidgetArrayInfoArray;
  i, x, y, xpos, ypos, w, h, sum: Integer;
  item: TFLayoutItem;
begin
  GetMem(sizes, FWidgets.Count * SizeOf(TFWidgetArrayInfo));

  for i := 0 to FWidgets.Count - 1 do
  begin
    sizes^[i].min := 0;
    sizes^[i].def := 0;
    sizes^[i].max := InfiniteSize;
    sizes^[i].MinFlag := True;
    sizes^[i].MaxFlag := True;
  end;

  if Orientation = Horizontal then
    for i := 0 to FWidgets.Count - 1 do
    begin
      item := TFLayoutItem(FWidgets.Items[i]);
      sizes^[i].min := Max(sizes^[i].min, item.Widget.MinSize.cx);
      sizes^[i].def := Max(sizes^[i].def, item.Widget.DefSize.cx);
      sizes^[i].max := Min(sizes^[i].max, item.Widget.MaxSize.cx);
    end
  else
    for i := 0 to FWidgets.Count - 1 do
    begin
      item := TFLayoutItem(FWidgets.Items[i]);
      sizes^[i].min := Max(sizes^[i].min, item.Widget.MinSize.cy);
      sizes^[i].def := Max(sizes^[i].def, item.Widget.DefSize.cy);
      sizes^[i].max := Min(sizes^[i].max, item.Widget.MaxSize.cy);
    end;

  for i := 0 to FWidgets.Count - 1 do
  begin
    if sizes^[i].def = 0 then sizes^[i].def := 20;
    if sizes^[i].min >= sizes^[i].def then
      sizes^[i].MinFlag := False;
    if sizes^[i].max <= sizes^[i].def then
      sizes^[i].MaxFlag := False;
  end;

  if Orientation = Horizontal then
  begin
    if FHorzAlign = horzFill then
      CorrectSizes(sizes, FWidgets.Count, BoundsSize.cx - DefSize.cx)
  end else
    if FVertAlign = vertFill then
      CorrectSizes(sizes, FWidgets.Count, BoundsSize.cy - DefSize.cy);

  sum := (FWidgets.Count - 1) * FSpacing;
  for i := 0 to FWidgets.Count - 1 do
    Inc(sum, sizes^[i].def);

  if Orientation = Horizontal then
    case FHorzAlign of
      horzCenter: x := (BoundsSize.cx - sum) div 2;
      horzRight:  x := BoundsSize.cx - FBorderSpacing - sum;
      else        x := FBorderSpacing;
    end
  else
    case FVertAlign of
      vertCenter: y := (BoundsSize.cy - sum) div 2;
      vertBottom: y := BoundsSize.cy - FBorderSpacing - sum;
      else        y := FBorderSpacing;
    end;

  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TFLayoutItem(FWidgets.Items[i]);
    if Orientation = Horizontal then
    begin
      xpos := x;
      w := sizes^[i].def;
      h := Min(BoundsSize.cy, item.Widget.DefSize.cy);
      case FVertAlign of
        vertCenter: ypos := (BoundsSize.cy - h) div 2;
	      vertBottom: ypos := BoundsSize.cy - FBorderSpacing - h;
	      else        ypos := FBorderSpacing;
      end;
      Inc(x, sizes^[i].def + FSpacing);
      if FVertAlign = vertFill then
        h := Min(BoundsSize.cy, item.Widget.MaxSize.cy);
    end
    else
    begin
      ypos := y;
      w := Min(BoundsSize.cx, item.Widget.DefSize.cx);
      h := sizes^[i].def;
      case FHorzAlign of
        horzCenter: xpos := (BoundsSize.cx - w) div 2;
	      horzRight : xpos := BoundsSize.cx - FBorderSpacing - w;
	      else        xpos := FBorderSpacing;
      end;
      Inc(y, sizes^[i].def + FSpacing);
      if FHorzAlign = horzFill then
        w := Min(BoundsSize.cx, item.Widget.MaxSize.cx);
    end;
    item.Widget.SetBounds(Point(xpos, ypos), gfxbase.Size(w, h));
  end;

  FreeMem(sizes);
end;


// -------------------------------------------------------------------
//   TFCustomGridLayout
// -------------------------------------------------------------------

constructor TFGridItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Width := 1;
  Height := 1;
end;


procedure TFCustomGridLayout.SetColCount(AColCount: Integer);
begin
  if AColCount <> FColCount then
  begin
    FColCount := AColCount;
    Update;
  end;
end;


procedure TFCustomGridLayout.SetRowCount(ARowCount: Integer);
begin
  if ARowCount <> FRowCount then
  begin
    FRowCount := ARowCount;
    Update;
  end;
end;


procedure TFCustomGridLayout.SetColSpacing(AColSpacing: Integer);
begin
  if AColSpacing <> FColSpacing then
  begin
    FColSpacing := AColSpacing;
    Update;
  end;
end;


procedure TFCustomGridLayout.SetRowSpacing(ARowSpacing: Integer);
begin
  if ARowSpacing <> FRowSpacing then
  begin
    FRowSpacing := ARowSpacing;
    Update;
  end;
end;


procedure TFCustomGridLayout.InitSizeInfos(var ColInfos, RowInfos: PWidgetArrayInfoArray);
var
  i: Integer;
  item: TFGridItem;
begin
  GetMem(ColInfos, FColCount * SizeOf(TFWidgetArrayInfo));
  GetMem(RowInfos, FRowCount * SizeOf(TFWidgetArrayInfo));

  for i := 0 to FColCount - 1 do
  begin
    ColInfos^[i].min := 0;
    ColInfos^[i].def := 0;
    ColInfos^[i].max := InfiniteSize;
    ColInfos^[i].MinFlag := True;
    ColInfos^[i].MaxFlag := True;
  end;

  for i := 0 to FRowCount - 1 do
  begin
    RowInfos^[i].min := 0;
    RowInfos^[i].def := 0;
    RowInfos^[i].max := InfiniteSize;
    RowInfos^[i].MinFlag := True;
    RowInfos^[i].MaxFlag := True;
  end;

  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TFGridItem(FWidgets.Items[i]);
    ColInfos^[item.x].min := Max(ColInfos^[item.x].min, item.Widget.MinSize.cx);
    ColInfos^[item.x].def := Max(ColInfos^[item.x].def, item.Widget.DefSize.cx);
    ColInfos^[item.x].max := Min(ColInfos^[item.x].max, item.Widget.MaxSize.cx);
    RowInfos^[item.y].min := Max(RowInfos^[item.y].min, item.Widget.MinSize.cy);
    RowInfos^[item.y].def := Max(RowInfos^[item.y].def, item.Widget.DefSize.cy);
    RowInfos^[item.y].max := Min(RowInfos^[item.y].max, item.Widget.MaxSize.cy);
  end;

  for i := 0 to FColCount - 1 do
  begin
    if ColInfos^[i].def = 0 then
      ColInfos^[i].def := 20;
    if ColInfos^[i].min >= ColInfos^[i].def then
      ColInfos^[i].MinFlag := False;
    if ColInfos^[i].max <= ColInfos^[i].def then
      ColInfos^[i].MaxFlag := False;
  end;

  for i := 0 to FRowCount - 1 do
  begin
    if RowInfos^[i].def = 0 then
      RowInfos^[i].def := 20;
    if RowInfos^[i].min >= RowInfos^[i].def then
      RowInfos^[i].MinFlag := False;
    if RowInfos^[i].max <= RowInfos^[i].def then
      RowInfos^[i].MaxFlag := False;
  end;
end;


procedure TFCustomGridLayout.CalcSizes;
var
  ColInfos, RowInfos: PWidgetArrayInfoArray;
  i: Integer;
begin
  MinSize.cx := (FColCount - 1) * FColSpacing;
  MinSize.cy := (FRowCount - 1) * FRowSpacing;
  DefSize.cx := (FColCount - 1) * FColSpacing;
  DefSize.cy := (FRowCount - 1) * FRowSpacing;
  MaxSize.cx := (FColCount - 1) * FColSpacing;
  MaxSize.cy := (FRowCount - 1) * FRowSpacing;

  InitSizeInfos(ColInfos, RowInfos);

  for i := 0 to FColCount - 1 do
  begin
    Inc(FMinSize.cx, ColInfos^[i].min);
    Inc(FDefSize.cx, ColInfos^[i].def);
    FMaxSize.cx := Min(InfiniteSize, MaxSize.cx + ColInfos^[i].max);
  end;

  for i := 0 to FRowCount - 1 do
  begin
    Inc(FMinSize.cy, RowInfos^[i].min);
    Inc(FDefSize.cy, RowInfos^[i].def);
    FMaxSize.cy := Min(InfiniteSize, MaxSize.cy + RowInfos^[i].max);
  end;

  FreeMem(RowInfos);
  FreeMem(ColInfos);
end;


procedure TFCustomGridLayout.Resized;
var
  ColInfos, RowInfos: PWidgetArrayInfoArray;
  i, j, x, y, w, h: Integer;
  item: TFGridItem;
begin
  InitSizeInfos(ColInfos, RowInfos);

  CorrectSizes(ColInfos, FColCount, BoundsSize.cx - DefSize.cx);
  CorrectSizes(RowInfos, FRowCount, BoundsSize.cy - DefSize.cy);

  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TFGridItem(FWidgets.Items[i]);
    x := 0;
    for j := 0 to item.x - 1 do
      Inc(x, ColInfos^[j].def);
    y := 0;
    for j := 0 to item.y - 1 do
      Inc(y, RowInfos^[j].def);
    w := 0;
    for j := 0 to item.Width - 1 do
      Inc(w, ColInfos^[item.x + j].def);
    h := 0;
    for j := 0 to item.Height - 1 do
      Inc(h, RowInfos^[item.y + j].def);
    Inc(w, (item.Width - 1) * FColSpacing);
    Inc(h, (item.Height - 1) * FRowSpacing);
    item.Widget.SetBounds(Point(x + item.x * FColSpacing,
      y + item.y * FRowSpacing), gfxbase.Size(w, h));
  end;

  FreeMem(ColInfos);
  FreeMem(RowInfos);
end;

constructor TFCustomGridLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TCollection.Create(TFGridItem);
  FColCount := 2;
  FRowCount := 2;
  FColSpacing := 4;
  FRowSpacing := 4;
end;

procedure TFCustomGridLayout.AddWidget(AWidget: TFWidget; x, y, w, h: Integer);
var
  item: TFGridItem;
begin
  if not ContainsChild(AWidget) then
  begin
    item := TFGridItem(FWidgets.Add);
    item.Widget := AWidget;
    item.x := x;
    item.y := y;
    item.Width := w;
    item.Height := h;
    AWidget.Parent := Self;
  end;
end;

procedure TFCustomGridLayout.MoveWidget(AWidget: TFWidget; x, y, w, h: Integer);
var
  i: integer;
  item: TFGridItem;
begin
  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TFGridItem(FWidgets.Items[i]);
    if item.Widget = AWidget then
    begin
      item.x := x;
      item.y := y;
      item.Width := w;
      item.Height := h;
      Update;
      exit;
    end;
  end;
  raise ELayoutError.Create(SLayoutWidgetNotFound);
end;


{$ENDIF read_implementation}

