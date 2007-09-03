unit gui_tree;

{$mode objfpc}{$H+}

{
  TODO:
    * Lots!!
    
  WARNING:   This is still under heavy development! Do NOT use.
}

interface

uses
  Classes,
  SysUtils,
  gfx_widget,
  gfxbase,
  fpgfx,
  gui_scrollbar;
  
type

  PfpgTreeColumnWidth = ^TfpgTreeColumnWidth;
  TfpgTreeColumnWidth = record
    next: PfpgTreeColumnWidth;
    width: word;
  end;


  TfpgTreeNode = class(TObject)
  private
    FFirstSubNode: TfpgTreeNode; // the subnodes - for list implementation
    FLastSubNode: TfpgTreeNode;

    FText: string;
    FParent: TfpgTreeNode;
    FNext: TfpgTreeNode;
    FPrev: TfpgTreeNode;
    FCollapsed: boolean;

    FSelColor: TfpgColor;
    FTextColor: TfpgColor;
    FSelTextColor: TfpgColor;
    FInactSelColor: TfpgColor;
    FInactSelTextColor: TfpgColor;
    FImageIndex: integer;
    procedure   SetCollapsed(const AValue: boolean);
    procedure   SetInactSelColor(const AValue: TfpgColor);
    procedure   SetInactSelTextColor(const AValue: TfpgColor);
    procedure   SetParent(const AValue: TfpgTreeNode);
    procedure   SetSelColor(const AValue: TfpgColor);
    procedure   SetSelTextColor(const AValue: TfpgColor);
    procedure   SetText(const AValue: string);
    procedure   SetTextColor(const AValue: TfpgColor);
    procedure   DoRePaint;
  public
    constructor Create;
    destructor  Destroy; override;
    // node related
    procedure   UnregisterSubNode(aNode: TfpgTreeNode);
    procedure   Append(aValue: TfpgTreeNode);
    function    FindSubNode(AText: string; ARecursive: Boolean): TfpgTreeNode;
    function    AppendText(AText: string): TfpgTreeNode;
    function    GetMaxDepth: integer;
    function    GetMaxVisibleDepth: integer;
    procedure   Collapse;
    procedure   Expand;
    function    Count: integer;
    function    CountRecursive: integer;
    procedure   Remove(aNode: TfpgTreeNode);
    procedure   Clear;  // remove all nodes recursively
    // parent color settings
    function    ParentTextColor: TfpgColor;
    function    ParentSelTextColor: TfpgColor;
    function    ParentSelColor: TfpgColor;
    function    ParentInactSelTextColor: TfpgColor;
    function    ParentInactSelColor: TfpgColor;
    // general properties
    property    Collapsed: boolean read FCollapsed write SetCollapsed;
    property    Next: TfpgTreeNode read FNext write FNext;
    property    Prev: TfpgTreeNode read FPrev write FPrev;
    property    Text: string read FText write SetText;
    property    Parent: TfpgTreeNode read FParent write SetParent;
    property    FirstSubNode: TfpgTreeNode read FFirstSubNode;
    property    LastSubNode: TfpgTreeNode read FLastSubNode;
    property    ImageIndex : integer read FImageIndex write FImageIndex;
    // color settings
    property    TextColor: TfpgColor read FTextColor write SetTextColor;
    property    SelColor: TfpgColor read FSelColor write SetSelColor;
    property    SelTextColor: TfpgColor read FSelTextColor write SetSelTextColor;
    property    InactSelColor: TfpgColor read FInactSelColor write SetInactSelColor;
    property    InactSelTextColor: TfpgColor read FInactSelTextColor write SetInactSelTextColor;
  end;
  
  
  TfpgTreeExpandEvent = procedure(Sender: TObject; ANode: TfpgTreeNode) of object;
  
  
  TfpgTreeview = class(TfpgWidget)
  private
    FRootNode: TfpgTreeNode;
    FSelection: TfpgTreeNode; // currently selected node
    FDefaultColumnWidth: word;
    FFirstColumn: PfpgTreeColumnWidth; // the list for column widths
    FFont: TfpgFont;
    FShowColumns: boolean;
    FHScrollbar: TfpgScrollbar;
    FVScrollbar: TfpgScrollbar;
//    FImageList : TfpgImageList;   // imagelist to be implemented in the future
    FShowImages : boolean;
    FXOffset: integer; // for repaint and scrollbar-calculation
    FYOffset: integer;
    FColumnHeight: integer; // height of the column header
    FMoving: boolean;
    FMovingPos: integer;
    FMovingCol: integer;
    FBackgroundColor: TfpgColor;
    FOnChange: TNotifyEvent;
    FOnExpand: TfpgTreeExpandEvent;
    function    GetFontDesc: string;
    function    GetRootNode: TfpgTreeNode;
    procedure   SetDefaultColumnWidth(const AValue: word);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetSelection(const AValue: TfpgTreeNode);
    procedure   SetShowColumns(const AValue: boolean);
    procedure   SetShowImages(const AValue: boolean);
    function    VisibleWidth: integer;
    function    VisibleHeight: integer;
    function    GetNodeHeightSum: integer;
    function    MaxNodeWidth: integer;
    function    GetNodeHeight: integer;
    function    GetNodeWidth(ANode: TfpgTreeNode): integer; // width of a node inclusive image
    function    NodeIsVisible(ANode: TfpgTreeNode): boolean;
    function    GetAbsoluteNodeTop(ANode: TfpgTreeNode): integer; // returns the node-top in pixels
    function    GetColumnLeft(AIndex: integer): integer;
    procedure   PreCalcColumnLeft;
    procedure   VScrollbarScroll(Sender: TObject; position: integer);
    procedure   HScrollbarMove(Sender: TObject; position: integer);
    procedure   UpdateScrollbars;
    procedure   ResetScrollbar;
  protected
    FColumnLeft: TList;
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleDoubleClick(x, y: integer; button: word; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleShow; override;
    procedure   HandlePaint; override;
    procedure   DoChange; virtual;
    procedure   DoExpand(ANode: TfpgTreeNode); virtual;
    function    NextVisualNode(ANode: TfpgTreeNode): TfpgTreeNode;
    function    PrevVisualNode(ANode: TfpgTreeNode): TfpgTreeNode;
    function    SpaceToVisibleNext(aNode: TfpgTreeNode): integer; // the nodes between the given node and the direct next node
    function    StepToRoot(aNode: TfpgTreeNode): integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   SetColumnWidth(AIndex, AWidth: word);
    function    GetColumnWidth(AIndex: word): word; // the width of a column - aIndex of the rootnode = 0
    property    Font: TfpgFont read FFont;
    property    RootNode: TfpgTreeNode read GetRootNode;
    property    Selection: TfpgTreeNode read FSelection write SetSelection;
  published
    property    ShowImages: boolean read FShowImages write SetShowImages;
    property    ShowColumns: boolean read FShowColumns write SetShowColumns;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    DefaultColumnWidth: word read FDefaultColumnWidth write SetDefaultColumnWidth;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    property    OnExpand: TfpgTreeExpandEvent read FOnExpand write FOnExpand;
  end;
  

implementation

type
  PColumnLeft = ^integer;


{ TfpgTreeNode }

procedure TfpgTreeNode.SetInactSelColor(const AValue: TfpgColor);
begin
  if AValue <> FInactSelColor then
  begin
    FInactSelColor := AValue;
    DoRePaint;
  end;
end;

procedure TfpgTreeNode.SetCollapsed(const AValue: boolean);
begin
  if aValue <> FCollapsed then
  begin
    FCollapsed := AValue;
    DoRePaint;
  end;
end;

procedure TfpgTreeNode.SetInactSelTextColor(const AValue: TfpgColor);
begin
  if AValue <> FInactSelTextColor then
  begin
    FInactSelTextColor := AValue;
    DoRePaint;
  end;
end;

procedure TfpgTreeNode.SetParent(const AValue: TfpgTreeNode);
begin
  if aValue <> FParent then
  begin
    if FParent <> nil then
      FParent.UnRegisterSubNode(self); // unregisteres
    FParent := aValue;
    if FParent <> nil then
    begin
      DoRePaint;
    end;
  end;
end;

procedure TfpgTreeNode.SetSelColor(const AValue: TfpgColor);
begin
  if FSelColor <> aValue then
  begin
    FSelColor := aValue;
    DoRePaint;
  end;
end;

procedure TfpgTreeNode.SetSelTextColor(const AValue: TfpgColor);
begin
  if FTextColor <> aValue then
  begin
    FSelTextColor := aValue;
    DoRePaint;
  end;
end;

procedure TfpgTreeNode.SetText(const AValue: string);
begin
  if aValue <> FText then
  begin
    FText := aValue;
    DoRePaint;
  end;
end;

procedure TfpgTreeNode.SetTextColor(const AValue: TfpgColor);
begin
  if FTextColor <> aValue then
  begin
    FTextColor := aValue;
    DoRePaint;
  end;
end;

procedure TfpgTreeNode.DoRePaint;
begin
  // todo
end;

constructor TfpgTreeNode.Create;
begin
  FFirstSubNode   := nil;
  FLastSubNode    := nil;
  FText           := '';
  FImageIndex     := -1;
  
  FParent     := nil;
  FNext       := nil;
  FPrev       := nil;
  
  FSelColor           := clUnset;
  FSelTextColor       := clUnset;
  FTextColor          := clUnset;
  FInactSelColor      := clUnset;
  FInactSelTextColor  := clUnset;
end;

destructor TfpgTreeNode.Destroy;
begin
  if FParent <> nil then
    FParent.UnregisterSubNode(self);
  inherited Destroy;
end;

procedure TfpgTreeNode.UnregisterSubNode(aNode: TfpgTreeNode);
var
  h: TfpgTreeNode;
begin
  h := FFirstSubNode;
  while h <> nil do
  begin
    if h = aNode then
    begin
      if h = FFirstSubNode then
        FFirstSubNode := FFirstSubNode.Next;
      if h = FLastSubNode then
        FLastSubNode := FLastSubNode.Prev;
      if h.prev <> nil then
        h.prev.next := h.next;
      if h.next <> nil then
        h.next.prev := h.prev;
      exit;
    end;
    h := h.next;
  end;
end;

procedure TfpgTreeNode.Append(aValue: TfpgTreeNode);
begin
  aValue.Parent := self;
  aValue.next := nil;

  if FFirstSubNode = nil then
    FFirstSubNode := aValue;

  aValue.prev := FLastSubNode;

  if FLastSubNode <> nil then
    FLastSubNode.Next := aValue;

  FLastSubNode := aValue;
end;

function TfpgTreeNode.FindSubNode(AText: string; ARecursive: Boolean): TfpgTreeNode;
var
  h: TfpgTreeNode;
begin
  result := nil;
  if ARecursive then
  begin
    h := FirstSubNode;
    while h <> nil do
    begin
      if h.Text = AText then
      begin
        result := h;
        exit;
      end;
      if h.count > 0 then
      begin
        result := h.FirstSubNode.FindSubNode(AText, ARecursive);
        if result <> nil then
          exit;
      end;
      h := h.next;
    end;
  end
  else
  begin
    h := FirstSubNode;
    while h <> nil do
    begin
      if h.Text = AText then
      begin
        result := h;
        break;
      end;
      h := h.next;
    end;
  end;  { if/else }
end;

function TfpgTreeNode.AppendText(AText: string): TfpgTreeNode;
var
  h: TfpgTreeNode;
begin
  {$IFDEF DEBUG}
  writeln('TfpgTreeNode.AppendText');
  {$ENDIF}
  h := TfpgTreeNode.Create;
  h.Text := AText;
  Append(h);
  result := h;
end;

function TfpgTreeNode.GetMaxDepth: integer;
var
  h: TfpgTreeNode;
  a: integer;
  t: integer;
begin
  {$IFDEF DEBUG}
  writeln('TfpgTreeNode.GetMaxDepth');
  {$ENDIF}
  h := FirstSubNode;
  result := 1;
  a := 0;
  while h <> nil do
  begin
    t := h.GetMaxDepth;
    if t > a then
      a := t;
    h := h.next;
  end;
  result := result + a;
end;

function TfpgTreeNode.GetMaxVisibleDepth: integer;
var
  h: TfpgTreeNode;
  a: integer;
  t: integer;
begin
  {$IFDEF DEBUG}
  writeln('TfpgTreeNode.GetMaxVisibleDepth');
  {$ENDIF}
  result := 1;
  h := FirstSubNode;
  if h.Collapsed then
    exit;
  a := 0;
  while h <> nil do
  begin
    t := h.GetMaxDepth;
    if t > a then
      a := t;
    h := h.next;
  end;
  result := result + a;
end;

procedure TfpgTreeNode.Collapse;
begin
  Collapsed := True;
end;

procedure TfpgTreeNode.Expand;
begin
  Collapsed := False;
end;

function TfpgTreeNode.Count: integer;
var
  h: TfpgTreeNode;
  i: integer;
begin
  h := FirstSubNode;
  i := 0;
  while h <> nil do
  begin
    h := h.next;
    inc(i);
  end;
  result := i;
end;

function TfpgTreeNode.CountRecursive: integer;
var
  h: TfpgTreeNode;
  i: integer;
begin
  h := FFirstSubNode;
  i := 0;
  while h <> nil do
  begin
    i := i + h.CountRecursive; // increases i by the count of the subnodes of the subnode
    h := h.next;
    inc(i); // and the subnode...
  end;
  result := i;
end;

procedure TfpgTreeNode.Remove(aNode: TfpgTreeNode);
begin
  if FirstSubNode = aNode then
  begin
    FFirstSubNode := aNode.next;
    if FFirstSubNode <> nil then
      FFirstSubNode.Prev := nil;
  end
  else
    if aNode.prev <> nil then
      aNode.Prev.next := aNode.next;
  if LastSubNode = aNode then
  begin
    FLastSubNode := aNode.prev;
    if FLastSubNode <> nil then
      FLastSubNode.next := nil;
  end
  else
    if aNode.next <> nil then
      aNode.next.prev := aNode.prev;
  aNode.prev := nil;
  aNode.next := nil;
  aNode.parent := nil;
end;

procedure TfpgTreeNode.Clear;
begin
  while FirstSubNode <> nil do
  begin
    if FirstSubNode.Count > 0 then
      FirstSubNode.Clear;
    Remove(FirstSubNode);
  end;
end;

function TfpgTreeNode.ParentTextColor: TfpgColor;
begin
  if TextColor <> clUnset then
    result := TextColor
  else
  begin
    if parent <> nil then
      result := parent.ParentTextColor
    else
      result := clText1;
  end;
end;

function TfpgTreeNode.ParentSelTextColor: TfpgColor;
begin
  if SelTextColor <> clUnset then
    result := SelTextColor
  else
  begin
    if parent <> nil then
      result := parent.ParentSelTextColor
    else
      result := clSelectionText;
  end;
end;

function TfpgTreeNode.ParentSelColor: TfpgColor;
begin
  if SelColor <> clUnset then
    result := SelColor
  else
  begin
    if parent <> nil then
      result := parent.ParentSelColor
    else
      result := clSelection;
  end;
end;

function TfpgTreeNode.ParentInactSelTextColor: TfpgColor;
begin
  if InactSelTextColor <> clUnset then
    result := InactSelTextColor
  else
  begin
    if Parent <> nil then
      Result := Parent.ParentInactSelTextColor
    else
      Result := clInactiveSelText;
  end;
end;

function TfpgTreeNode.ParentInactSelColor: TfpgColor;
begin
  if InactSelColor <> clUnset then
    result := InactSelColor
  else
  begin
    if Parent <> nil then
      result := parent.ParentInactSelColor
    else
      result := clInactiveSel;
  end;
end;

{ TfpgTreeview }

procedure TfpgTreeview.VScrollbarScroll(Sender: TObject; position: integer);
begin
  {$IFDEF DEBUG}
  writeln(Classname, '.VScrollbarMove');
  {$ENDIF}
  FYOffset := Position;
  RePaint;
end;

function TfpgTreeview.GetFontDesc: string;
begin

end;

function TfpgTreeview.GetRootNode: TfpgTreeNode;
begin
{$IFDEF DEBUG}
  writeln('TfpgTreeview.GetRootNode');
{$ENDIF}
  if FRootNode = nil then
    FRootNode := TfpgTreeNode.Create;
  FRootNode.TextColor     := clText1;
  FRootnode.SelTextColor  := clSelectionText;
  FRootnode.SelColor      := clSelection;
  Result := FRootNode;
end;

procedure TfpgTreeview.SetDefaultColumnWidth(const AValue: word);
begin
  if (aValue <> FDefaultColumnWidth) and (aValue > 3) then
  begin
    FDefaultColumnWidth := AValue;
    RePaint;
  end;
end;

procedure TfpgTreeview.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  RePaint;
end;

procedure TfpgTreeview.SetSelection(const AValue: TfpgTreeNode);
var
  n: TfpgTreeNode;
begin
  if aValue <> FSelection then
  begin
    FSelection := aValue;
    if aValue <> nil then
    begin
      n := aValue.parent;
      while n <> nil do
      begin
        n.Expand;
        DoExpand(n);
        n := n.parent;
      end;
    end;
    
    if GetAbsoluteNodeTop(Selection) + GetNodeHeight - FVScrollbar.Position > VisibleHeight then
    begin
      FVScrollbar.Position := GetAbsoluteNodeTop(Selection) + GetNodeHeight - VisibleHeight;
      FYOffset := FVScrollbar.Position;
      UpdateScrollBars;
    end;
    
    if GetAbsoluteNodeTop(Selection) - FVScrollbar.Position < 0 then
    begin
      FVScrollbar.Position := GetAbsoluteNodeTop(Selection);
      FYOffset := FVScrollbar.Position;
      UpdateScrollbars;
    end;
  end;
end;

procedure TfpgTreeview.SetShowColumns(const AValue: boolean);
begin
  if FShowColumns <> aValue then
  begin
    FShowColumns := aValue;
    RePaint;
  end;
end;

procedure TfpgTreeview.SetShowImages(const AValue: boolean);
begin
  if AValue <> FShowImages then
  begin
    FShowImages := AValue;
    UpdateScrollbars;
    RePaint;
  end;
end;

function TfpgTreeview.VisibleWidth: integer;
begin
  if GetNodeHeightSum * (GetNodeHeight) > Height - 2 then
    VisibleWidth := Width - 2 - FVScrollbar.Width
  else
    result := Width - 2;
end;

function TfpgTreeview.VisibleHeight: integer;
begin
  if FShowColumns then
  begin
    if MaxNodeWidth > Width - 2 then
      result := Height - 2 - FHScrollbar.Height - FColumnHeight
    else
      result := Height - 2 - FColumnHeight;
  end
  else
    if MaxNodeWidth > width - 2 then
      result := Height - 2 - FHScrollbar.Height
    else
      result := Height - 2;
end;

function TfpgTreeview.GetNodeHeightSum: integer;
var
  h: TfpgTreeNode;
  i: integer;
begin
  h := RootNode;
  i := -1;
  while h <> nil do
  begin
    inc(i);
    if (not h.Collapsed) and (h.Count > 0) then
    begin
      h := h.FirstSubNode;
    end
    else
    begin
      if h.next <> nil then
        h := h.next
      else
      begin
        while h.next = nil do
        begin
          h := h.parent;
          if h = nil then
          begin
            result := i;
            exit;
          end;
        end;
        h := h.next;
      end;
    end;
  end;
  result := i;
end;

function TfpgTreeview.MaxNodeWidth: integer;
var
  h: TfpgTreeNode;
  w: integer;
  r: integer;
begin
  {$IFDEF DEBUG}
  writeln('TfpgTreeView.MaxNodeWidth');
  {$ENDIF}
  result := 0;
  h := RootNode.FirstSubNode;
  r := 0;
  while h <> nil do
  begin
    w := GetColumnLeft(StepToRoot(h));
    if r < w + GetNodeWidth(h) then
      r := w + GetNodeWidth(h);
    if (not h.collapsed) and (h.count > 0) then
      h := h.FirstSubNode
    else
    begin
      if h.next <> nil then
        h := h.next
      else
      begin
        while h.next = nil do
        begin
          h := h.parent;
          if h = nil then
          begin
            result := r + 4;
            exit;
          end;
        end;  { while }
        h := h.next;
      end;
    end;  { if/else }
  end;  { while }
end;

function TfpgTreeview.GetNodeHeight: integer;
begin
  Result := FFont.Height + 5;
end;

function TfpgTreeview.GetNodeWidth(ANode: TfpgTreeNode): integer;
//var
//   AImage: TfpgImageItem;
begin
  {$IFDEF DEBUG}
  writeln('TfpgTreeView.GetNodeWidth');
  {$ENDIF}
  if ANode = nil then
    Result := 0
  else
  begin
    Result := FFont.TextWidth(ANode.Text) + 2;
    //if ShowImages and (ImageList <> nil) then
    //begin
      //if ANode.ImageIndex > -1 then
      //begin
        //AImage := ImageList.Item[ANode.ImageIndex];
        //if AImage <> nil then
          //result := result + AImage.Image.Width + 2;
      //end;
    //end;
  end;  { if/else }
end;

function TfpgTreeview.NodeIsVisible(ANode: TfpgTreeNode): boolean;
begin
  Result := True;
  if ANode = nil then
  begin
    Result := False;
    exit;
  end;
  ANode := ANode.Parent;
  while ANode <> nil do
  begin
    if ANode.Collapsed and (ANode.Parent <> nil) then
      Result := False;
    ANode := ANode.Parent;
  end;
end;

function TfpgTreeview.GetAbsoluteNodeTop(ANode: TfpgTreeNode): integer;
var
  i: integer;
begin
  i := 0;
  while (ANode <> nil) and (ANode <> RootNode) do
  begin
    ANode := PrevVisualNode(ANode);
    inc(i);
  end;
  result := (i - 1) * GetNodeHeight;
end;

function TfpgTreeview.GetColumnLeft(AIndex: integer): integer;
var
   AColumnLeft: PColumnLeft;
begin
  if FColumnLeft = nil then
  begin
    PreCalcColumnLeft;
    if AIndex < 0 then
      Result := 0
    else
    begin
      if AIndex > FColumnLeft.Count - 1 then
      begin
        AColumnLeft := FColumnLeft[FColumnLeft.Count - 1];
        result := AColumnLeft^;
      end
      else
      begin
        AColumnLeft := FColumnLeft[AIndex];
        result := AColumnLeft^;
      end;
    end;
  end;
end;

function TfpgTreeview.GetColumnWidth(AIndex: word): word;
var
  h: PfpgTreeColumnWidth;
  i: integer;
begin
{$IFDEF DEBUG}
  writeln('TfpgTreeView.GetColumnWidth');
{$ENDIF}
  h := FFirstColumn;
  i := 0;
  if h = nil then // not found
  begin
    result := DefaultColumnWidth;
    exit;
  end;
  while i < aIndex do
  begin
    if h = nil then // not found - returns the default
    begin
      result := DefaultColumnWidth;
      exit;
    end;
    h := h^.next;
    inc(i);
  end;
  if h <> nil then
    result := h^.width
  else // not found -> returns the default
    result := DefaultColumnWidth;
end;

procedure TfpgTreeview.PreCalcColumnLeft;
var
  Aleft: TfpgCoord;
  ACounter: integer;
  AColumnLeft: PColumnLeft;
begin
  {$IFDEF DEBUG}
  writeln('TfpgTreeView.PreCalcColumnWidth');
  {$ENDIf}
  if FColumnLeft = nil then
	  FColumnLeft := TList.Create
  else
	  FColumnLeft.Clear;
  for ACounter := 0 to FColumnLeft.Count - 1 do  // Freeing Memory
  begin
    AColumnLeft := FColumnLeft[ACounter];
    Dispose(AColumnLeft);
  end;
  Aleft := 0;
  for ACounter := 1 to RootNode.GetMaxDepth do
  begin
  	AColumnLeft := new(PColumnLeft);
  	AColumnLeft^ := Aleft;
  	FColumnLeft.Add(AColumnLeft);
  	Aleft := ALeft + GetColumnWidth(ACounter);
  end;
end;

procedure TfpgTreeview.HScrollbarMove(Sender: TObject; position: integer);
begin
  FXOffset := Position;
  RePaint;
end;

procedure TfpgTreeview.UpdateScrollbars;
begin
  {$IFDEF DEBUG}
  writeln(Classname, '.UpdateScrollbars');
  {$ENDIF}
  FVScrollbar.Visible := VisibleHeight < GetNodeHeightSum * GetNodeHeight;
  FVScrollbar.Min := 0;
  FVScrollbar.Max := (GetNodeHeightSum - 1) * GetNodeHeight;
  FHScrollbar.Min := 0;
  FHScrollbar.Max := MaxNodeWidth - VisibleWidth + FVScrollbar.Width;
  FHScrollbar.Visible := MaxNodeWidth > Width - 2;
  if not FVScrollbar.Visible then
  begin
    FVScrollbar.Position := 0;
    FVScrollBar.RepaintSlider;
    FYOffset := 0;
  end;
  if not FHScrollbar.Visible then
  begin
    FHScrollbar.Position := 0;
    FHScrollBar.RepaintSlider;
    FXOffset := 0;
  end;
end;

procedure TfpgTreeview.ResetScrollbar;
begin
  {$IFDEF DEBUG}
  writeln(Classname, '.ResetScrollbar');
  {$ENDIF}
  UpdateScrollBars;
  if FHScrollbar.Visible then
    FVScrollbar.SetPosition(Width - 19, 1, 18, Height - 2 - 18)
  else
    FVScrollbar.SetPosition(Width - 19, 1, 18, Height - 2);
  FHScrollbar.SetPosition(1, Height - 19, Width - 2, 18);
end;

procedure TfpgTreeview.HandleResize(awidth, aheight: TfpgCoord);
begin
  {$IFDEF DEBUG}
  writeln(Classname, '.HandleResize');
  {$ENDIF}
  inherited HandleResize(awidth, aheight);
  ResetScrollbar;
  RePaint;
end;

procedure TfpgTreeview.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
end;

procedure TfpgTreeview.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
end;

procedure TfpgTreeview.HandleDoubleClick(x, y: integer; button: word;
  shiftstate: TShiftState);
begin
  inherited HandleDoubleClick(x, y, button, shiftstate);
end;

procedure TfpgTreeview.HandleShow;
begin
  ResetScrollbar;
  inherited HandleShow;
end;

procedure TfpgTreeview.HandlePaint;
var
  r: TfpgRect;
  h: TfpgTreeNode;
  i: integer;
  i1: integer;
  w: integer;
  YPos: integer;
  col: integer;
  ACenterPos: integer;
//  AImageItem: TfpgImageItem;
  AVisibleHeight: integer;
label
  label_next;
begin
  {$IFDEF DEBUG}
  writeln('TfpgTreeview.HandlePaint');
  {$ENDIF}
//  inherited HandlePaint;
  if not HasHandle then
    Exit; //==>
  i1 := 0;
  PreCalcColumnLeft;
  UpdateScrollbars;
  AVisibleHeight := VisibleHeight;

  Canvas.BeginDraw;  // start double buffering
  Canvas.ClearClipRect;
  Canvas.Clear(FBackgroundColor);
  if FFocused then
    Canvas.SetColor(clWidgetFrame)
  else
    Canvas.SetColor(clInactiveWgFrame);
  r.SetRect(0, 0, Width, Height);
  Canvas.DrawRectangle(r); // border

  if ShowColumns then // draw the column header?
  begin
    r.SetRect(1, 1, VisibleWidth, FColumnHeight);
    Canvas.SetClipRect(r);
    Canvas.SetColor(clHilite1);
    Canvas.DrawLine(1, 1, VisibleWidth, 1);
    Canvas.DrawLine(1, 1, 1, FColumnHeight - 1);
    Canvas.SetColor(clHilite2);
    Canvas.DrawLine(1, 2, VisibleWidth - 1, 2);
    Canvas.DrawLine(2, 2, FColumnHeight - 1, 2);
    Canvas.SetColor(clShadow2);
    Canvas.DrawLine(1, FColumnHeight, VisibleWidth, FColumnHeight);
    Canvas.DrawLine(VisibleWidth, 1, VisibleWidth, FColumnHeight);
    Canvas.SetColor(clShadow1);
    Canvas.DrawLine(2, FColumnHeight - 1, VisibleWidth - 1, FColumnHeight - 1);
    Canvas.DrawLine(VisibleWidth - 1, 2, VisibleWidth - 1, FColumnHeight - 1);
    Canvas.SetColor(clGridHeader);
    Canvas.FillRectangle(3, 3, VisibleWidth - 4, FColumnHeight - 4);
    Canvas.SetColor(clWidgetFrame);

    w := 0;
    r.SetRect(3, 2, VisibleWidth - 4, FColumnHeight - 2);
    Canvas.SetClipRect(r);
    for i := 1 to rootnode.getMaxDepth - 1 do
    begin
      w := w + GetColumnWidth(i);
      Canvas.DrawLine(w - FXOffset, 2, w - FXOffset, FColumnHeight - 1);
    end;
    Canvas.SetColor(clShadow1);
    w := 0;
    for i := 1 to rootnode.getMaxDepth - 1 do
    begin
      w := w + GetColumnWidth(i);
      Canvas.DrawLine(w - 1 - FXOffset, 3, w - 1 - FXOffset, FColumnHeight - 2);
    end;
    Canvas.SetColor(clHilite2);
    w := 0;
    for i := 1 to rootnode.getMaxDepth - 1 do
    begin
      w := w + GetColumnWidth(i);
      Canvas.DrawLine(w + 1 - FXOffset, 3, w + 1 - FXOffset, FColumnHeight - 2);
    end;
  end;
  
  if ShowColumns then
  begin
    r.SetRect(1, 1 + FColumnHeight, VisibleWidth, VisibleHeight); // drawing rectangle for nodes and lines
    col := FColumnHeight;
  end
  else
  begin
    r.SetRect(1, 1, VisibleWidth, VisibleHeight);
    col := 0;
  end;
  Canvas.ClearClipRect;
  Canvas.SetClipRect(r);

  // draw the nodes with lines
  h := RootNode.FirstSubNode;
  Canvas.SetTextColor(RootNode.ParentTextColor);
  YPos := 0;
  while h <> nil do
  begin
    Canvas.SetTextColor(h.ParentTextColor);
    // lines with + or -
    w := GetColumnLeft(StepToRoot(h));
    YPos := YPos + GetNodeHeight;
    ACenterPos := YPos - FYOffset + col - GetNodeHeight + (GetNodeHeight div 2);
    if ACenterPos > FHScrollbar.Position - GetNodeHeight then
    begin
      if h = Selection then // draw the selection rectangle and text
      begin
        if Focused then
        begin
          Canvas.SetColor(h.ParentSelColor);
          Canvas.SetTextColor(h.ParentSelTextColor);
        end
        else
        begin
          Canvas.SetColor(h.ParentInactSelColor);
          Canvas.SetTextColor(h.ParentInActSelTextColor);
        end;
        Canvas.FillRectangle(w - FXOffset, YPos - FYOffset + col - GetNodeHeight + FFont.Ascent div 2 - 2, GetNodeWidth(h), GetNodeHeight);
{
        if (ImageList <> nil) and ShowImages then
        begin
          AImageItem := ImageList.Item[h.ImageIndex];
          if AImageItem <> nil then
          begin
            Canvas.DrawImagePart(w - FXOffset + 1, ACenterPos - 4, AImageItem.Image,0,0,16,16);
            Canvas.DrawString(w - FXOffset + 1 + AImageItem.Image.Width + 2, ACenterPos - FFont.Ascent div 2, h.text);
          end
          else
            Canvas.DrawString(w - FXOffset + 1, ACenterPos - FFont.Ascent div 2, h.text);
        end
        else
}
          Canvas.DrawString(w - FXOffset + 1, ACenterPos - FFont.Ascent div 2, h.text);
        Canvas.SetTextColor(h.ParentTextColor);
      end
      else
      begin
{
        if (ImageList <> nil) and  ShowImages then
        begin
          AImageItem := ImageList.Item[h.ImageIndex];
          if AImageItem <> nil then
          begin
            Canvas.DrawImagePart(w - FXOffset + 1, ACenterPos - 4, AImageItem.Image,0,0,16,16);
            Canvas.DrawString(w - FXOffset + 1 + AImageItem.Image.Width + 2, ACenterPos - FFont.Ascent div 2, h.text);
          end
          else
            Canvas.DrawString(w - FXOffset + 1, ACenterPos - FFont.Ascent div 2, h.text);
        end
        else
}
          Canvas.DrawString(w - FXOffset + 1, ACenterPos - FFont.Ascent div 2, h.text);
      end;  { if/else }
      Canvas.SetColor(clText1);
      if h.Count > 0 then // subnodes?
      begin
        Canvas.DrawRectangle(w - FXOffset - GetColumnWidth(i1) div 2 - 3, ACenterPos - 3, 9, 9);
        if h.Collapsed then // draw a "+"
        begin
          Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 - 1, ACenterPos + 1, w - FXOffset - GetColumnWidth(i1) div 2 + 3, ACenterPos + 1);
          Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 1, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos + 3);
        end
        else
        begin // draw a "-"
          Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 - 1, ACenterPos + 1, w - FXOffset - GetColumnWidth(i1) div 2 + 3, ACenterPos + 1);
        end;
      end
      else
      begin
        Canvas.SetColor(clText1);
        Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1,  ACenterPos + 1, w - FXOffset - 3,  ACenterPos + 1);
      end;

      if h.prev <> nil then
      begin
        // line up to the previous node
        if h.prev.count > 0 then
        begin
          if h.count > 0 then
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 4, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - SpaceToVisibleNext(h.prev) * GetNodeHeight + 6)
          else
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - SpaceToVisibleNext(h.prev) * GetNodeHeight + 6)
        end
        else
        begin
          if h.count > 0 then
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 3, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - SpaceToVisibleNext(h.prev) * GetNodeHeight + 2)
          else
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - SpaceToVisibleNext(h.prev) * GetNodeHeight + 2);
        end;
      end
      else
      begin
        if h.count > 0 then
          Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1,ACenterPos - 3, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - GetNodeHeight div 2 + 3)
        else
          Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - GetNodeHeight div 2 + 3);
      end;
    end;
    if AVisibleHeight > ACenterPos + GetNodeHeight then
    begin
      if h.count > 0 then
      begin
        if not h.collapsed then
          h := h.FirstSubNode
        else
          goto label_next;
      end
      else
      begin
        label_next:
      if h.next <> nil then
        h := h.next // next node
      else
      begin
        while h.next = nil do // or recurse next node per parent
        begin
          h := h.parent;
          if (h = nil) or (h = rootnode) then
          begin
            Canvas.EndDraw;
            Exit; //==>
          end;
        end;  { while }
        h := h.next;
      end;  { if/else }
    end;
    end
    else
    begin
      // Draw Lines up to the parent nodes
      ACenterPos := ACenterPos + GetNodeHeight;
      while h <> RootNode do
      begin
        w := GetColumnLeft(StepToRoot(h));
        if h.next <> nil then
        begin
          h := h.next;
          if h.prev.count > 0 then
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, GetAbsoluteNodeTop(h.prev) - FYOffset + 6 + GetNodeHeight div 2)
          else
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, GetAbsoluteNodeTop(h.prev) - FYOffset + 2 + GetNodeHeight div 2);
         end;
         h := h.parent;
      end;
      Canvas.EndDraw;
      Exit; //==>
    end;
  end;
  Canvas.EndDraw;
end;

procedure TfpgTreeview.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfpgTreeview.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TfpgTreeview.DoExpand(ANode: TfpgTreeNode);
begin
  if Assigned(FOnExpand) then
	  FOnExpand(self, ANode);
end;

function TfpgTreeview.NextVisualNode(ANode: TfpgTreeNode): TfpgTreeNode;
label
  nextnode;
begin
  {$Note Remove the use of Label and Goto! Quick!!! }
  result := nil;
  if aNode.Collapsed then
  begin
    nextnode:
    if aNode.Next <> nil then
    begin
      result := aNode.Next;
      exit;
    end
    else
    begin
      while aNode.next = nil do
      begin
        aNode := aNode.Parent;
        if aNode = nil then
          exit;
      end;
      result := aNode.Next;
      exit;
    end;
  end
  else
  begin
    if aNode.Count > 0 then
    begin
      result := aNode.FirstSubNode;
      exit;
    end
    else
      goto nextnode;
  end;
end;

function TfpgTreeview.PrevVisualNode(ANode: TfpgTreeNode): TfpgTreeNode;
var
  n: TfpgTreeNode;
begin
  n := ANode;
  if ANode.Prev <> nil then
  begin
    result := ANode.Prev;
    ANode := ANode.Prev;
    while (not ANode.Collapsed) and (ANode.Count > 0) do
    begin
      result := ANode.LastSubNode;
      aNode := ANode.LastSubNode;
    end;
  end
  else
  begin
    if ANode.Parent <> nil then
      result := ANode.Parent
    else
      result := n;
  end;
end;

function TfpgTreeview.SpaceToVisibleNext(aNode: TfpgTreeNode): integer;
var
  h: TfpgTreeNode;
  i: integer;
begin
  result := 0;
  i := 0;
  if aNode.next = nil then
    exit;
  h := aNode;
  while h <> aNode.next do
  begin
    inc(i);
    if (h.count > 0) and (not h.collapsed) then
    begin
      h := h.FirstSubNode;
    end
    else
    begin
      while h.next = nil do
        h := h.parent;
      h := h.next;
    end;
  end;
  result := i;
end;

function TfpgTreeview.StepToRoot(aNode: TfpgTreeNode): integer;
var
  i: integer;
begin
  i := -1;
  while aNode <> nil do
  begin
    aNode := aNode.parent;
    inc(i);
  end;
  result := i;
end;

constructor TfpgTreeview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRootNode := nil;
  FSelection := nil;
  FDefaultColumnWidth := 15;
  FFirstColumn := nil;
  FFont := fpgGetFont('#Label1');
  
  FHScrollbar := TfpgScrollbar.Create(self);
  FHScrollbar.Orientation := orHorizontal;
  FHScrollbar.OnScroll    := @HScrollbarMove;
  FHScrollbar.Visible     := False;
  FHScrollbar.Position    := 0;
  FHScrollbar.SliderSize  := 0.2;
  FVScrollbar := TfpgScrollbar.Create(self);
  FVScrollbar.Orientation := orVertical;
  FVScrollbar.OnScroll    := @VScrollbarScroll;
  FVScrollbar.Visible     := False;
  FVScrollbar.Position    := 0;
  FVScrollbar.SliderSize  := 0.2;
  
  FBackgroundColor := clListBox;
  FFocusable    := True;
  FMoving       := False;
  FXOffset      := 0;
  FYOffset      := 0;
  FColumnHeight := FFont.Height + 2;
end;

procedure TfpgTreeview.SetColumnWidth(AIndex, AWidth: word);
var
  h: PfpgTreeColumnWidth;
  n: PfpgTreeColumnWidth;
  i: word;
begin
  {$IFDEF DEBUG}
  writeln('TfpgTreeView.SetColumnWidth');
  {$ENDIF}
  h := FFirstColumn;
  if h = nil then
  begin
    new(h);
    h^.width := FDefaultColumnWidth;
    h^.next := nil;
    FFirstColumn := h;
  end;
  i := 0;
  while i < AIndex do
  begin
    if h^.next = nil then
    begin
      new(n);
      h^.next := n;
      n^.width := DefaultColumnWidth;
      n^.next := nil;
    end;
    h := h^.next;
    inc(i);
  end;
  if h^.width <> AWidth then
  begin
    h^.width := AWidth;
    RePaint;
  end;
end;


end.

