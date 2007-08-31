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
    procedure   SetInActSelColor(const AValue: TfpgColor);
    procedure   SetInactSelTextColor(const AValue: TfpgColor);
    procedure   SetParent(const AValue: TfpgTreeNode);
    procedure   SetSelColor(const AValue: TfpgColor);
    procedure   SetSelTextColor(const AValue: TfpgColor);
    procedure   SetText(const AValue: string);
    procedure   SetTextColor(const AValue: TfpgColor);
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
    property    InactSelColor: TfpgColor read FInactSelColor write SetInActSelColor;
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
    function    NodeIsVisible(ANode: TfpgTreeNode): boolean;
    function    GetAbsoluteNodeTop(ANode: TfpgTreeNode): integer; // returns the node-top in pixels
    procedure   VScrollbarScroll(Sender: TObject; position: integer);
    procedure   HScrollbarMove(Sender: TObject; position: integer);
    procedure   UpdateScrollbars;
    procedure   ResetScrollbar;
  protected
    FColumnLeft: TList;
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
    procedure   DoChange; virtual;
    procedure   DoExpand(ANode: TfpgTreeNode); virtual;
    function    NextVisualNode(ANode: TfpgTreeNode): TfpgTreeNode;
    function    PrevVisualNode(ANode: TfpgTreeNode): TfpgTreeNode;
    function    SpaceToVisibleNext(aNode: TfpgTreeNode): integer; // the nodes between the given node and the direct next node
    function    StepToRoot(aNode: TfpgTreeNode): integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   SetColumnWidth(AIndex, AWidth: word);
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

{ TfpgTreeNode }

procedure TfpgTreeNode.SetInActSelColor(const AValue: TfpgColor);
begin

end;

procedure TfpgTreeNode.SetCollapsed(const AValue: boolean);
begin

end;

procedure TfpgTreeNode.SetInactSelTextColor(const AValue: TfpgColor);
begin

end;

procedure TfpgTreeNode.SetParent(const AValue: TfpgTreeNode);
begin

end;

procedure TfpgTreeNode.SetSelColor(const AValue: TfpgColor);
begin

end;

procedure TfpgTreeNode.SetSelTextColor(const AValue: TfpgColor);
begin

end;

procedure TfpgTreeNode.SetText(const AValue: string);
begin

end;

procedure TfpgTreeNode.SetTextColor(const AValue: TfpgColor);
begin

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

end;

procedure TfpgTreeview.SetSelection(const AValue: TfpgTreeNode);
var
  n: TfpgTreeNode;
begin
  {$IFDEF DEBUG}
  writeln('TfpgTreeView.SetSelection');
  {$ENDIF}
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

end;

procedure TfpgTreeview.SetShowImages(const AValue: boolean);
begin

end;

function TfpgTreeview.VisibleWidth: integer;
begin

end;

function TfpgTreeview.VisibleHeight: integer;
begin

end;

function TfpgTreeview.GetNodeHeightSum: integer;
var
  h: TfpgTreeNode;
  i: integer;
begin
{$IFDEF DEBUG}
  writeln('TfpgTreeView.GetNodeHeightSum');
{$ENDIF}
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
begin

end;

function TfpgTreeview.GetNodeHeight: integer;
begin
  Result := FFont.Height + 5;
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
  FFont := fpgGetFont('#Label');
  
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

