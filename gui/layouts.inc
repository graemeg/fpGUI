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

{%mainunit fpgui.pp}

{$IFDEF read_interface}

// ===================================================================
//   Layouts
// ===================================================================

type

  ELayoutError = class(Exception);

  THorzAlign = (horzLeft, horzCenter, horzRight, horzFill);
  TVertAlign = (vertTop, vertCenter, vertBottom, vertFill);

  TLayoutItem = class(TCollectionItem)
  private
    FWidget: TWidget;
  published
    property Widget: TWidget read FWidget write FWidget;
  end;

  TWidgetArrayInfo = record
    min, def, max: Integer;
    MinFlag, MaxFlag: Boolean;
  end;

  TWidgetArrayInfoArray = array[0..(1 shl 30) div SizeOf(TWidgetArrayInfo) - 1] of TWidgetArrayInfo;
  PWidgetArrayInfoArray = ^TWidgetArrayInfoArray;


  TLayout = class(TContainerWidget)
  protected
    FWidgets: TCollection;
    FBorderSpacing: Integer;
    IsRecalcingLayout: Boolean;
    function    GetChildCount: Integer; override;
    function    GetChild(Index: Integer): TWidget; override;
    property    BorderSpacing: Integer read FBorderSpacing write FBorderSpacing;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    ContainsChild(AChild: TWidget): Boolean; override;
  end;


// -------------------------------------------------------------------
//   FixedLayout
// -------------------------------------------------------------------

  TFixedItem = class(TLayoutItem)
  public
    Left: Integer;
    Top: Integer;
  end;


  { TFixedLayout }

  TFixedLayout = class(TLayout)
  private
    procedure   AddFixedChild(AChild: TWidget);
  protected
    procedure   CalcSizes; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AddWidget(AWidget: TWidget; ALeft, ATop: Integer);
    procedure   MoveWidget(AWidget: TWidget; ALeft, ATop: Integer);
  published
    property    Enabled;
  end;


// -------------------------------------------------------------------
//   DockingLayout
// -------------------------------------------------------------------

  TDockingMode = (dmTop, dmBottom, dmLeft, dmRight, dmClient, dmUndocked);

  TDockingItem = class(TLayoutItem)
  public
    Left, Top: Integer;
    DockingMode: TDockingMode;
  end;
  

  TDockingLayout = class(TLayout)
  protected
    procedure   CalcSizes; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AddWidget(AWidget: TWidget; ADockingMode: TDockingMode);
    procedure   AddWidget(AWidget: TWidget; ALeft, ATop: Integer);
  published
    property    Enabled;
  end;


// -------------------------------------------------------------------
//   BoxLayout
// -------------------------------------------------------------------

  TCustomBoxLayout = class(TLayout)
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
    procedure   InsertChild(AChild: TWidget); override;
    procedure   RemoveChild(AChild: TWidget); override;
  end;


  TBoxLayout = class(TCustomBoxLayout)
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
//   GridLayout
// -------------------------------------------------------------------

  TGridItem = class(TLayoutItem)
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


  { TCustomGridLayout }

  TCustomGridLayout = class(TLayout)
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
    procedure   AddWidget(AWidget: TWidget; x, y, w, h: Integer);
    procedure   MoveWidget(AWidget: TWidget; x, y, w, h: Integer);
  end;


  TGridLayout = class(TCustomGridLayout)
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
//   TLayout
// -------------------------------------------------------------------

// public methods

constructor TLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanExpandWidth := True;
  FCanExpandHeight := True;
end;


destructor TLayout.Destroy;
begin
  FWidgets.Free;
  inherited Destroy;
end;


function TLayout.ContainsChild(AChild: TWidget): Boolean;
var
  i: Integer;
begin
  for i := 0 to FWidgets.Count - 1 do
    if TLayoutItem(FWidgets.Items[i]).Widget = AChild then
    begin
      Result := True;
      exit;
    end;
  Result := False;
end;


// protected methods

function TLayout.GetChildCount: Integer;
begin
  Result := FWidgets.Count;
end;

function TLayout.GetChild(Index: Integer): TWidget;
begin
  Result := TLayoutItem(FWidgets.Items[Index]).Widget;
end;


// -------------------------------------------------------------------
//   TFixedLayout
// -------------------------------------------------------------------

constructor TFixedLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TCollection.Create(TFixedItem);
end;

procedure TFixedLayout.AddWidget(AWidget: TWidget; ALeft, ATop: Integer);
var
  item: TFixedItem;
begin
  AWidget.Parent := Self;
  item := TFixedItem(FWidgets.Add);
  item.Left := ALeft;
  item.Top := ATop;
  item.Widget := AWidget;
  AWidget.SetBounds(Point(item.Left, item.Top), item.Widget.DefSize);
  AddFixedChild(AWidget);
end;

procedure TFixedLayout.MoveWidget(AWidget: TWidget; ALeft, ATop: Integer);
var
  i: integer;
  item: TFixedItem;
begin
  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TFixedItem(FWidgets.Items[i]);
    if item.Widget = AWidget then
    begin
      item.Left := ALeft;
      item.Top := ATop;
      AWidget.SetBounds(Point(item.Left, item.Top), item.Widget.DefSize);
      exit;
    end;
  end;
  raise ELayoutError.Create(SLayoutWidgetNotFound);
end;

procedure TFixedLayout.AddFixedChild(AChild: TWidget);
begin
  // todo
end;

procedure TFixedLayout.CalcSizes;
var
  i: Integer;
  item: TFixedItem;
begin
  if FWidgets.Count = 0 then
    FDefSize := gfxbase.Size(50, 50)
  else
    for i := 0 to FWidgets.Count - 1 do
    begin
      item := TFixedItem(FWidgets.Items[i]);
      FDefSize.cx := Max(DefSize.cx, item.Left + item.Widget.DefSize.cx);
      FDefSize.cy := Max(DefSize.cx, item.Top + item.Widget.DefSize.cy);
    end;
end;

{procedure TFixedLayout.EvLayoutChildren(Canvas: TGfxCanvas);
var
  i: Integer;
  item: TFixedItem;
begin
  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TFixedItem(FWidgets.Items[i]);
    item.Widget.SetBounds(item.Left, item.Top, item.Widget.DefSize.cx, item.Widget.DefSize.cy);
  end;
end;}


// -------------------------------------------------------------------
//   TDockingLayout
// -------------------------------------------------------------------

constructor TDockingLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TCollection.Create(TDockingItem);
end;

procedure TDockingLayout.CalcSizes;
var
  i: Integer;
  item: TDockingItem;
  w, cw: TWidget;
begin
  if FWidgets.Count = 0 then
  begin
    FDefSize := gfxbase.Size(200, 200);
    exit;
  end;

  // Find the client widget (widget with DockingMode "dmClient")
  cw := nil;
  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TDockingItem(FWidgets.Items[i]);
    if item.DockingMode = dmClient then
    begin
      cw := item.Widget;
      break;
    end;
  end;

  if Assigned(cw) then
  begin
    FMinSize := cw.MinSize;
    FMaxSize := cw.MaxSize;
    FDefSize := cw.DefSize;
  end else
    FDefSize := gfxbase.Size(200, 200);

  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TDockingItem(FWidgets.Items[i]);
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
    end;
  end;
end;


{procedure TDockingLayout.EvLayoutChildren(Canvas: TGfxCanvas);
var
  clx, cly, clw, clh: Integer;   // Client rectangle
  ClientWidget: TWidget;
  i, WidgetW, WidgetH: Integer;
  item: TDockingItem;
begin
  clx := 0;
  cly := 0;
  clw := BoundsSize.cx;
  clh := BoundsSize.cy;
  // WriteLn('=> DockingLayout.EvLayoutChildren ', BoundsSize.cx, ' x ', BoundsSize.cy);
  if (clw = 0) or (clh = 0) then
    exit;

  // Process all attached widgets
  ClientWidget := nil;
  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TDockingItem(FWidgets.Items[i]);
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
    end;
    if item.DockingMode <> dmClient then
      item.Widget.SetBounds(item.Left, item.Top, WidgetW, WidgetH);
  end;
  if Assigned(ClientWidget) then
    ClientWidget.SetBounds(clx, cly, clw, clh);
end;}

procedure TDockingLayout.AddWidget(AWidget: TWidget; ADockingMode: TDockingMode);
var
  item: TDockingItem;
begin
  AWidget.Parent := Self;
  item := TDockingItem(FWidgets.Add);
  item.Widget := AWidget;
  item.DockingMode := ADockingMode;
//  AddFixedChild(AWidget);
end;

procedure TDockingLayout.AddWidget(AWidget: TWidget; ALeft, ATop: Integer);
var
  item: TDockingItem;
begin
  AWidget.Parent := Self;
  item := TDockingItem(FWidgets.Add);
  item.Widget := AWidget;
  item.DockingMode := dmUndocked;
  item.Left := ALeft;
  item.Top := ATop;
//  AddFixedChild(AWidget);
end;


// -------------------------------------------------------------------
//   TCustomBoxLayout
// -------------------------------------------------------------------

procedure TCustomBoxLayout.SetOrientation(AOrientation: TOrientation);
begin
  if AOrientation <> FOrientation then
  begin
    FOrientation := AOrientation;
    Update;
  end;
end;


constructor TCustomBoxLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TCollection.Create(TLayoutItem);
  FOrientation := Horizontal;
  FHorzAlign := horzFill;
  FVertAlign := vertFill;
  FSpacing := 4;
end;


procedure TCustomBoxLayout.InsertChild(AChild: TWidget);
var
  item: TLayoutItem;
begin
  if not ContainsChild(AChild) then
  begin
    item := TLayoutItem(FWidgets.Add);
    item.Widget := AChild;
    AChild.Parent := Self;
  end;
end;


procedure TCustomBoxLayout.RemoveChild(AChild: TWidget);
begin
  {$Warning Not implemented yet.}
  raise Exception.Create('TCustomBoxLayout.RemoveChild - Not implemented yet');
end;


procedure TCustomBoxLayout.CalcSizes;
var
  i: Integer;
  item: TLayoutItem;
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
  end else
  begin
    FMinSize := Size(0, i);
    FDefSize := MinSize;
    FMaxSize.cx := InfiniteSize;
    if VertAlign = vertFill then
      FMaxSize.cy := i
    else
      FMaxSize.cy := InfiniteSize;
  end;

  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TLayoutItem(FWidgets.Items[i]);
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
    end else
    begin
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
    end;
  end;

  Inc(FMinSize.cx, 2 * FBorderSpacing);
  Inc(FMinSize.cy, 2 * FBorderSpacing);
  Inc(FDefSize.cx, 2 * FBorderSpacing);
  Inc(FDefSize.cy, 2 * FBorderSpacing);
  FMaxSize.cx := Min(InfiniteSize, MaxSize.cx + 2 * FBorderSpacing);
  FMaxSize.cy := Min(InfiniteSize, MaxSize.cy + 2 * FBorderSpacing);
end;

procedure TCustomBoxLayout.Resized;
var
  sizes: PWidgetArrayInfoArray;
  i, x, y, xpos, ypos, w, h, sum: Integer;
  item: TLayoutItem;
begin
  GetMem(sizes, FWidgets.Count * SizeOf(TWidgetArrayInfo));

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
      item := TLayoutItem(FWidgets.Items[i]);
      sizes^[i].min := Max(sizes^[i].min, item.Widget.MinSize.cx);
      sizes^[i].def := Max(sizes^[i].def, item.Widget.DefSize.cx);
      sizes^[i].max := Min(sizes^[i].max, item.Widget.MaxSize.cx);
    end
  else
    for i := 0 to FWidgets.Count - 1 do
    begin
      item := TLayoutItem(FWidgets.Items[i]);
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
      else	  x := FBorderSpacing;
    end
  else
    case FVertAlign of
      vertCenter: y := (BoundsSize.cy - sum) div 2;
      vertBottom: y := BoundsSize.cy - FBorderSpacing - sum;
      else        y := FBorderSpacing;
    end;

  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TLayoutItem(FWidgets.Items[i]);
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
//   TCustomGridLayout
// -------------------------------------------------------------------

constructor TGridItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Width := 1;
  Height := 1;
end;


procedure TCustomGridLayout.SetColCount(AColCount: Integer);
begin
  if AColCount <> FColCount then
  begin
    FColCount := AColCount;
    Update;
  end;
end;


procedure TCustomGridLayout.SetRowCount(ARowCount: Integer);
begin
  if ARowCount <> FRowCount then
  begin
    FRowCount := ARowCount;
    Update;
  end;
end;


procedure TCustomGridLayout.SetColSpacing(AColSpacing: Integer);
begin
  if AColSpacing <> FColSpacing then
  begin
    FColSpacing := AColSpacing;
    Update;
  end;
end;


procedure TCustomGridLayout.SetRowSpacing(ARowSpacing: Integer);
begin
  if ARowSpacing <> FRowSpacing then
  begin
    FRowSpacing := ARowSpacing;
    Update;
  end;
end;


procedure TCustomGridLayout.InitSizeInfos(var ColInfos, RowInfos: PWidgetArrayInfoArray);
var
  i: Integer;
  item: TGridItem;
begin
  GetMem(ColInfos, FColCount * SizeOf(TWidgetArrayInfo));
  GetMem(RowInfos, FRowCount * SizeOf(TWidgetArrayInfo));

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
    item := TGridItem(FWidgets.Items[i]);
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


procedure TCustomGridLayout.CalcSizes;
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


procedure TCustomGridLayout.Resized;
var
  ColInfos, RowInfos: PWidgetArrayInfoArray;
  i, j, x, y, w, h: Integer;
  item: TGridItem;
begin
  InitSizeInfos(ColInfos, RowInfos);

  CorrectSizes(ColInfos, FColCount, BoundsSize.cx - DefSize.cx);
  CorrectSizes(RowInfos, FRowCount, BoundsSize.cy - DefSize.cy);

  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TGridItem(FWidgets.Items[i]);
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

constructor TCustomGridLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TCollection.Create(TGridItem);
  FColCount := 2;
  FRowCount := 2;
  FColSpacing := 4;
  FRowSpacing := 4;
end;

procedure TCustomGridLayout.AddWidget(AWidget: TWidget; x, y, w, h: Integer);
var
  item: TGridItem;
begin
  AWidget.Parent := Self;
  item := TGridItem(FWidgets.Add);
  item.Widget := AWidget;
  item.x := x;
  item.y := y;
  item.Width := w;
  item.Height := h;
end;

procedure TCustomGridLayout.MoveWidget(AWidget: TWidget; x, y, w, h: Integer);
var
  i: integer;
  item: TGridItem;
begin
  for i := 0 to FWidgets.Count - 1 do
  begin
    item := TGridItem(FWidgets.Items[i]);
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

