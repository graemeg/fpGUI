{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2009 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a basic Treeview control and Node classes. The treeview
      keeps track of the nodes in a double-linked list structure.
      Each Node as .prev and .next property pointing to it's neighbours.
}

unit fpg_tree;

{$mode objfpc}{$H+}

{
  TODO:
    * Lots!!
    * Columns need to be reworked. We don't want coluns per node levels. Instead
      we want a main column covering the tree. Then extra columns for user
      text and data.
    * Implement event handlers the user can hook into and do custom drawing.
    
  WARNING:   This is still under heavy development. Use at own risk!
}

{.$Define Debug}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_imagelist,
  fpg_scrollbar,
  fpg_menu;
  
type

  PfpgTreeColumnWidth = ^TfpgTreeColumnWidth;
  TfpgTreeColumnWidth = record
    next: PfpgTreeColumnWidth;
    width: word;
  end;
  
  // forward declaration
  TfpgTreeNode = class;
  
  TfpgTreeNodeFindMethod = procedure(ANode: TfpgTreeNode; var AFound: boolean) of object;


  TfpgTreeNode = class(TObject)
  private
    FCollapsed: boolean;
    FData: Pointer;
    FFirstSubNode: TfpgTreeNode; // the subnodes - for list implementation
    FImageIndex: integer;
    FInactSelColor: TfpgColor;
    FInactSelTextColor: TfpgColor;
    FLastSubNode: TfpgTreeNode;
    FNext: TfpgTreeNode;
    FParent: TfpgTreeNode;
    FPrev: TfpgTreeNode;
    FSelColor: TfpgColor;
    FSelTextColor: TfpgColor;
    FText: TfpgString;
    FTextColor: TfpgColor;
    FHasChildren: Boolean;
    procedure   SetCollapsed(const AValue: boolean);
    procedure   SetInactSelColor(const AValue: TfpgColor);
    procedure   SetInactSelTextColor(const AValue: TfpgColor);
    procedure   SetParent(const AValue: TfpgTreeNode);
    procedure   SetSelColor(const AValue: TfpgColor);
    procedure   SetSelTextColor(const AValue: TfpgColor);
    procedure   SetText(const AValue: TfpgString);
    procedure   SetTextColor(const AValue: TfpgColor);
    procedure   DoRePaint;
    procedure   SetHasChildren(const AValue: Boolean);
  public
    constructor Create;
    destructor  Destroy; override;
    // node related
    function    AppendText(AText: TfpgString): TfpgTreeNode;
    function    Count: integer;
    function    CountRecursive: integer;
    function    FindSubNode(AText: string; ARecursive: Boolean): TfpgTreeNode; overload;
    function    FindSubNode(ATreeNodeFindMethod: TfpgTreeNodeFindMethod): TfpgTreeNode; overload;
    function    FindSubNode(AData: TObject; ARecursive: Boolean): TfpgTreeNode; overload;
    function    GetMaxDepth: integer;
    function    GetMaxVisibleDepth: integer;
    procedure   Append(var aValue: TfpgTreeNode);
    procedure   Clear;  // remove all nodes recursively
    procedure   Collapse;
    procedure   Expand;
    procedure   Remove(var aNode: TfpgTreeNode);
    procedure   UnregisterSubNode(aNode: TfpgTreeNode);
    // parent color settings
    function    ParentInactSelColor: TfpgColor;
    function    ParentInactSelTextColor: TfpgColor;
    function    ParentSelColor: TfpgColor;
    function    ParentSelTextColor: TfpgColor;
    function    ParentTextColor: TfpgColor;
    // general properties
    property    Collapsed: boolean read FCollapsed write SetCollapsed;
    property    Data: Pointer read FData write FData;
    property    FirstSubNode: TfpgTreeNode read FFirstSubNode;
    property    ImageIndex: integer read FImageIndex write FImageIndex;
    property    LastSubNode: TfpgTreeNode read FLastSubNode;
    property    Next: TfpgTreeNode read FNext write FNext;
    property    Parent: TfpgTreeNode read FParent write SetParent;
    property    Prev: TfpgTreeNode read FPrev write FPrev;
    property    Text: TfpgString read FText write SetText;
          { determines the + or - image in the treeview }
    property    HasChildren: Boolean read FHasChildren write SetHasChildren;
    // color settings
    property    InactSelColor: TfpgColor read FInactSelColor write SetInactSelColor;
    property    InactSelTextColor: TfpgColor read FInactSelTextColor write SetInactSelTextColor;
    property    SelColor: TfpgColor read FSelColor write SetSelColor;
    property    SelTextColor: TfpgColor read FSelTextColor write SetSelTextColor;
    property    TextColor: TfpgColor read FTextColor write SetTextColor;
  end;
  
  
  TfpgTreeExpandEvent = procedure(Sender: TObject; ANode: TfpgTreeNode) of object;
  
  
  TfpgTreeView = class(TfpgWidget)
  private
    FImageList: TfpgImageList;
    FColumnHeight: integer; // height of the column header
    FDefaultColumnWidth: word;
    FIndentNodeWithNoImage: boolean;
    FFirstColumn: PfpgTreeColumnWidth; // the list for column widths
    FFont: TfpgFont;
    FHScrollbar: TfpgScrollbar;
    FMoving: boolean;
    FMovingCol: integer;
    FMovingPos: integer;
    FNoImageIndent: integer;
    FOnChange: TNotifyEvent;
    FOnExpand: TfpgTreeExpandEvent;
    FRootNode: TfpgTreeNode;
    FScrollWheelDelta: integer;
    FSelection: TfpgTreeNode; // currently selected node
    FShowColumns: boolean;
    FShowImages : boolean;
    FTreeLineColor: TfpgColor;
    FTreeLineStyle: TfpgLineStyle;
    FVScrollbar: TfpgScrollbar;
    FXOffset: integer; // for repaint and scrollbar-calculation
    FYOffset: integer;
    function    GetFontDesc: string;
    function    GetRootNode: TfpgTreeNode;
    procedure   SetDefaultColumnWidth(const AValue: word);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetSelection(const AValue: TfpgTreeNode);
    procedure   SetShowColumns(const AValue: boolean);
    procedure   SetShowImages(const AValue: boolean);
    procedure   SetTreeLineColor(const AValue: TfpgColor);
    procedure   SetTreeLineStyle(const AValue: TfpgLineStyle);
    procedure   SetIndentNodeWithNoImage(const AValue: boolean);
    function    VisibleWidth: integer;
    function    VisibleHeight: integer;
    function    GetNodeHeightSum: integer;
    function    MaxNodeWidth: integer;
    function    GetNodeHeight: integer;
    // width of a node inclusive image
    function    GetNodeWidth(ANode: TfpgTreeNode): integer;
    function    NodeIsVisible(ANode: TfpgTreeNode): boolean;
    // returns the node-top in pixels
    function    GetAbsoluteNodeTop(ANode: TfpgTreeNode): integer;
    function    GetColumnLeft(AIndex: integer): integer;
    procedure   PreCalcColumnLeft;
    procedure   VScrollbarScroll(Sender: TObject; position: integer);
    procedure   HScrollbarScroll(Sender: TObject; position: integer);
    procedure   UpdateScrollbars;
    procedure   ResetScrollbar;
    procedure   ClearColumnLeft;
    procedure   FreeAllTreeNodes;
  protected
    FColumnLeft: TList;
    FPopupMenu: TfpgPopupMenu;
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleRMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleDoubleClick(x, y: integer; button: word; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandleShow; override;
    procedure   HandlePaint; override;
    procedure   DrawHeader(ACol: integer; ARect: TfpgRect; AFlags: integer); virtual;
    procedure   DoChange; virtual;
    procedure   DoExpand(ANode: TfpgTreeNode); virtual;
    function    NextVisualNode(ANode: TfpgTreeNode): TfpgTreeNode;
    function    PrevVisualNode(ANode: TfpgTreeNode): TfpgTreeNode;
    // the nodes between the given node and the direct next node
    function    SpaceToVisibleNext(aNode: TfpgTreeNode): integer;
    function    StepToRoot(aNode: TfpgTreeNode): integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SetColumnWidth(AIndex, AWidth: word);
    // the width of a column - aIndex of the rootnode = 0
    function    GetColumnWidth(AIndex: word): word;
    property    Font: TfpgFont read FFont;
    // Invisible node that starts the tree
    property    RootNode: TfpgTreeNode read GetRootNode;
    property    Selection: TfpgTreeNode read FSelection write SetSelection;
    property    ImageList: TfpgImageList read FImageList write FImageList;
    property    PopupMenu: TfpgPopupMenu read FPopupMenu write FPopupMenu;
  published
    property    DefaultColumnWidth: word read FDefaultColumnWidth write SetDefaultColumnWidth default 15;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    IndentNodeWithNoImage: boolean read FIndentNodeWithNoImage write SetIndentNodeWithNoImage default True;
    property    NoImageIndent: integer read FNoImageIndent write FNoImageIndent default 16;
    property    ParentShowHint;
    property    ScrollWheelDelta: integer read FScrollWheelDelta write FScrollWheelDelta default 15;
    property    ShowColumns: boolean read FShowColumns write SetShowColumns default False;
    property    ShowHint;
    property    ShowImages: boolean read FShowImages write SetShowImages default False;
    property    TabOrder;
    property    TreeLineColor: TfpgColor read FTreeLineColor write SetTreeLineColor default clShadow1;
    property    TreeLineStyle: TfpgLineStyle read FTreeLineStyle write SetTreeLineStyle default lsDot;
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

procedure TfpgTreeNode.SetText(const AValue: TfpgString);
begin
  if AValue <> FText then
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

procedure TfpgTreeNode.SetHasChildren(const AValue: Boolean);
begin
  if FHasChildren <> AValue then
  begin
    FHasChildren := AValue;
    DoRePaint;
  end;
end;

constructor TfpgTreeNode.Create;
begin
  FData           := nil;
  FFirstSubNode   := nil;
  FLastSubNode    := nil;
  FText           := '';
  FImageIndex     := -1;
  FCollapsed      := True;
  FHasChildren    := False;

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
  Clear;
  FData       := nil;
  FParent     := nil;
  FNext       := nil;
  FPrev       := nil;
  FFirstSubNode   := nil;
  FLastSubNode    := nil;
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

procedure TfpgTreeNode.Append(var aValue: TfpgTreeNode);
begin
  aValue.Parent := self;
  aValue.Next   := nil;

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
//      writeln('h.Text = ', h.Text);
      if h.Text = AText then
      begin
        result := h;
        Exit; //==>
      end;
      if h.count > 0 then
      begin
        result := h.FindSubNode(AText, ARecursive);
        if result <> nil then
          Exit; //==>
      end;
      h := h.next;
    end;  { while }
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

function TfpgTreeNode.FindSubNode(ATreeNodeFindMethod: TfpgTreeNodeFindMethod): TfpgTreeNode;
var
  lFound: Boolean;
  h: TfpgTreeNode;
begin
  result := nil;
  lFound := False;
  if not Assigned(ATreeNodeFindMethod) then
    Exit; //==>

  h := FirstSubNode;
  while h <> nil do
  begin
    ATreeNodeFindMethod(h, lFound);
    if lFound then
    begin
      result := h;
      Exit; //==>
    end;
    if h.Count > 0 then
    begin
      result := h.FindSubNode(ATreeNodeFindMethod);
      if result <> nil then
        Exit; //==>
    end;
    h := h.next;
  end;
end;

function TfpgTreeNode.FindSubNode(AData: TObject; ARecursive: Boolean): TfpgTreeNode;
var
  h: TfpgTreeNode;
begin
  result := nil;
  if ARecursive then
  begin
    h := FirstSubNode;
    while h <> nil do
    begin
      if TObject(h.Data) = AData then
      begin
        result := h;
        Exit; //==>
      end;
      if h.count > 0 then
      begin
        result := h.FindSubNode(AData, ARecursive);
        if result <> nil then
          Exit; //==>
      end;
      h := h.next;
    end;  { while }
  end
  else
  begin
    h := FirstSubNode;
    while h <> nil do
    begin
      if TObject(h.Data) = AData then
      begin
        result := h;
        break;
      end;
      h := h.next;
    end;
  end;  { if/else }
end;

function TfpgTreeNode.AppendText(AText: TfpgString): TfpgTreeNode;
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
    inc(i);
    h := h.next;
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
    inc(i); // current node
    i := i + h.CountRecursive; // increases i by the count of the subnodes of the subnode
    h := h.next;
  end;
  result := i;
end;

procedure TfpgTreeNode.Remove(var aNode: TfpgTreeNode);
begin
  if FirstSubNode = aNode then
  begin
    FFirstSubNode := aNode.next;
    if FFirstSubNode <> nil then
      FFirstSubNode.Prev := nil;
  end
  else
  begin
    if aNode.prev <> nil then
      aNode.Prev.next := aNode.next;
  end;

  if LastSubNode = aNode then
  begin
    FLastSubNode := aNode.prev;
    if FLastSubNode <> nil then
      FLastSubNode.next := nil;
  end
  else
  begin
    if aNode.next <> nil then
      aNode.next.prev := aNode.prev;
  end;

  aNode.prev := nil;
  aNode.next := nil;
  aNode.parent := nil;
end;

procedure TfpgTreeNode.Clear;
var
  n: TfpgTreeNode;
  tn: TfpgTreeNode;
begin
  n := LastSubNode;
  while n <> nil do
  begin
    tn := n;
    n := n.prev;
    Remove(tn);
    tn.Free;
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
  Result := FFont.FontDesc;
end;

function TfpgTreeview.GetRootNode: TfpgTreeNode;
begin
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

procedure TfpgTreeview.SetTreeLineColor(const AValue: TfpgColor);
begin
  if FTreeLineColor = AValue then
    Exit; //==>
  FTreeLineColor := AValue;
  RePaint;
end;

procedure TfpgTreeview.SetTreeLineStyle(const AValue: TfpgLineStyle);
begin
  if FTreeLineStyle = AValue then
    Exit; //==>
  FTreeLineStyle := AValue;
  RePaint;
end;

procedure TfpgTreeView.SetIndentNodeWithNoImage(const AValue: boolean);
begin
  if AValue <> FIndentNodeWithNoImage then
  begin
    FIndentNodeWithNoImage := AValue;
    UpdateScrollbars;
    RePaint;
  end;
end;

function TfpgTreeview.VisibleWidth: integer;
begin
  Result := Width - 2; // border width = 2 pixels
  if FVScrollbar.Visible then
     dec(Result, FVScrollbar.Width);
end;

function TfpgTreeview.VisibleHeight: integer;
begin
  Result := Height - 2; // border width = 2 pixels
  if FShowColumns then
    dec(Result, FColumnHeight);
  if FHScrollbar.Visible then
    dec(Result, FHScrollbar.Height);
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
  Result := FFont.Height + 2;
end;

function TfpgTreeview.GetNodeWidth(ANode: TfpgTreeNode): integer;
var
  AImage: TfpgImageItem;
begin
  {$IFDEF DEBUG}
  writeln('TfpgTreeView.GetNodeWidth');
  {$ENDIF}
  if ANode = nil then
    Result := 0
  else
  begin
    Result := FFont.TextWidth(ANode.Text) + 2;
    if ShowImages and (ImageList <> nil) then
    begin
      if ANode.ImageIndex > -1 then
      begin
        AImage := ImageList.Item[ANode.ImageIndex];
        if AImage <> nil then
          result := result + AImage.Image.Width + 2;
      end
      else if IndentNodeWithNoImage then
        result := result + NoImageIndent + 2;
    end;
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
begin
  if FColumnLeft = nil then
    PreCalcColumnLeft;

  if AIndex < 0 then
    Result := 0
  else
  begin
    if AIndex > FColumnLeft.Count - 1 then
      result := PColumnLeft(FColumnLeft[FColumnLeft.Count - 1])^
    else
      result := PColumnLeft(FColumnLeft[AIndex])^;
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
	  FColumnLeft := TList.Create;

  ClearColumnLeft;  // Freeing memory

  Aleft := 0;
  for ACounter := 1 to RootNode.GetMaxDepth do
  begin
  	AColumnLeft := new(PColumnLeft);
  	AColumnLeft^ := Aleft;
  	FColumnLeft.Add(AColumnLeft);
  	Aleft := Aleft + GetColumnWidth(ACounter);
  end;
end;

procedure TfpgTreeview.HScrollbarScroll(Sender: TObject; position: integer);
begin
  FXOffset := Position;
  RePaint;
end;

procedure TfpgTreeview.UpdateScrollbars;
begin
  {$IFDEF DEBUG}
  writeln(Classname, '.UpdateScrollbars');
  {$ENDIF}
  FVScrollbar.Visible := VisibleHeight < (GetNodeHeightSum * GetNodeHeight);
  FVScrollbar.Min := 0;
  FVScrollbar.Max := (GetNodeHeightSum * GetNodeHeight) - VisibleHeight + FHScrollbar.Height;
  FHScrollbar.Min := 0;
  FHScrollbar.Max := MaxNodeWidth - VisibleWidth + FVScrollbar.Width;
  FHScrollbar.Visible := MaxNodeWidth > Width - 2;
  if not FVScrollbar.Visible then
  begin
    FVScrollbar.Position := 0;
    FVScrollBar.RepaintSlider;
    FYOffset := 0;
  end
  else
    FVScrollBar.RepaintSlider;

  if not FHScrollbar.Visible then
  begin
    FHScrollbar.Position := 0;
    FHScrollBar.RepaintSlider;
    FXOffset := 0;
  end
  else
    FHScrollBar.RepaintSlider;
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

procedure TfpgTreeView.ClearColumnLeft;
var
  i: integer;
  AColumnLeft: PColumnLeft;
begin
  for i := 0 to FColumnLeft.Count - 1 do  // Freeing Memory
  begin
    AColumnLeft := FColumnLeft[i];
    Dispose(AColumnLeft);
  end;
  FColumnLeft.Clear;
end;

procedure TfpgTreeView.FreeAllTreeNodes;
begin
  RootNode.Clear;
end;

procedure TfpgTreeview.HandleResize(awidth, aheight: TfpgCoord);
begin
  {$IFDEF DEBUG}
  writeln(Classname, '.HandleResize');
  {$ENDIF}
  inherited HandleResize(awidth, aheight);
  if (csLoading in ComponentState) then
    exit;
  ResetScrollbar;
  RePaint;
end;

procedure TfpgTreeview.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  col: integer;
  i: integer;
  w: integer;
  i1: integer;
  last: TfpgTreeNode;
  node: TfpgTreeNode;
  cancel: boolean;
  OldSel: TfpgTreeNode;
begin
  inherited HandleLMouseUp(x, y, shiftstate);

  node := nil;
  OldSel := Selection;
  if FMoving then // column resize
  begin
    FMoving := false;
    x := x + FXOffset;
    SetColumnWidth(FMovingCol, GetColumnWidth(FMovingCol) + x - FMovingPos);
    FMoving := false;
  end
  else
  begin
    if ShowColumns then
      col := FColumnHeight
    else
      col := 0;
    y := y - col - 1 + FYOffset;
    i := 0;
    x := x + FXOffset;
    cancel := False;
    last := RootNode;
    while not (((i - 1) * GetNodeHeight - 2 <= y) and ((i) * GetNodeHeight + 2 >= y)) do
    begin
      node := NextVisualNode(last);
      if node = nil then
        exit; //==>
      if node = last then
      begin
        cancel := True;
        break;  //==>
      end;
      inc(i);
      last := node;
    end;
    
    if (not cancel) or (node <> nil) then
    begin
      // +/- or node-selection?
      i1  := StepToRoot(node);
      w   := GetColumnLeft(i1);
      if (x >= w - GetColumnWidth(i1) div 2 - 3) and (x <= w - GetColumnWidth(i1) div 2 + 6) then
      // collapse or expand?
      begin // yes
        if (node.Count > 0) or node.HasChildren then
        begin
          if node.Collapsed then
          begin
            node.expand;
            DoExpand(node);
          end
          else
            node.Collapse;
          ResetScrollBar;
          RePaint;
        end;
      end
      else
      begin
        if x > w - GetColumnWidth(i1) div 2 + 6 then
          Selection := node;
      end;
    end;
  end;
  if OldSel <> Selection then
  begin
    RePaint;
    DoChange;
  end;
end;

procedure TfpgTreeview.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  xpos: integer;
  i: integer;
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  if ShowColumns then
  begin
    x := x + FXOffset;
    xpos := 0;
    i := 0;
    while xpos + 2 < x do
    begin
      inc(i);
      xpos := xpos + GetColumnWidth(i);
    end;
    if (x > xpos - 2) and (x < xpos + 2) then
    begin
      FMoving := True;
      FMovingPos := xpos;
      FMovingCol := i;
      SetColumnWidth(i, GetColumnWidth(i));
    end;
  end;
  RePaint;
end;

procedure TfpgTreeView.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleRMouseUp(x, y, shiftstate);
  if Assigned(PopupMenu) then
    PopupMenu.ShowAt(self, x, y);
end;

procedure TfpgTreeview.HandleDoubleClick(x, y: integer; button: word;
  shiftstate: TShiftState);
begin
  // to setup cursor co-ordinates and handle selection
  HandleLMouseUp(x, y, shiftstate);
  inherited HandleDoubleClick(x, y, button, shiftstate);
  if Selection <> nil then
  begin
    if Selection.Collapsed then
    begin
      Selection.Expand;
      DoExpand(Selection);
    end
    else
      Selection.Collapse;
    RePaint;
  end;
end;

procedure TfpgTreeview.HandleShow;
begin
  if (csLoading in ComponentState) then
    Exit;
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
  x: integer;
  y: integer;
  AImageItem: TfpgImageItem;
  AVisibleHeight: integer;
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

  { TODO : Columns need to be redesigned completely }
  if ShowColumns then
  begin
    // Drawing column headers
    r.SetRect(1, 1, 0, FColumnHeight);
    for col := 1 to rootnode.getMaxDepth - 1 do
    begin
      r.Width := GetColumnWidth(col);
      DrawHeader(col, r, 0);
      inc(r.Left, r.Width);
      if r.Left >= VisibleWidth then
        Break;  // small optimization. Don't draw what we can't see
    end;
    // Fill remainder of the client area with one big header
    r.width := VisibleWidth - r.Left + 1;
    DrawHeader(col+1, r, 0);
  end;

  // Calculate the client area used for nodes and lines
  if ShowColumns then
  begin
    r.SetRect(1, 1 + FColumnHeight, VisibleWidth, VisibleHeight);
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
  YPos := 0;
  while h <> nil do
  begin
    Canvas.SetTextColor(h.ParentTextColor);
    // lines with + or -
    w := GetColumnLeft(StepToRoot(h));
    ACenterPos := YPos - FYOffset + col + (GetNodeHeight div 2);
    YPos := YPos + GetNodeHeight;
    i := GetColumnLeft(StepToRoot(h)) + GetNodeWidth(h);

    // only paint the node if it is fully visible
    if i > FXOffset then
    begin
//      writeln('painting node: ', h.Text);
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
        if (ImageList <> nil) and ShowImages then
        begin
          AImageItem := ImageList.Item[h.ImageIndex];
          if AImageItem <> nil then
          begin
            Canvas.DrawImagePart(w - FXOffset + 1, ACenterPos - 4, AImageItem.Image, 0, 0, 16, 16);
            Canvas.DrawString(w - FXOffset + 1 + AImageItem.Image.Width + 2, ACenterPos - FFont.Ascent div 2, h.text);
          end
          else
          begin
            if FIndentNodeWithNoImage then
              Canvas.DrawString(w - FXOffset + 1 + FNoImageIndent + 2 {spacer}, ACenterPos - FFont.Ascent div 2, h.text)
            else
              Canvas.DrawString(w - FXOffset + 1, ACenterPos - FFont.Ascent div 2, h.text);
          end;
        end
        else
          Canvas.DrawString(w - FXOffset + 1, ACenterPos - FFont.Ascent div 2, h.text);
        Canvas.SetTextColor(h.ParentTextColor);
      end
      else
      begin
        if (ImageList <> nil) and ShowImages then
        begin
          AImageItem := ImageList.Item[h.ImageIndex];
          if AImageItem <> nil then
          begin
            Canvas.DrawImagePart(w - FXOffset + 1, ACenterPos - 4, AImageItem.Image, 0, 0, 16, 16);
            Canvas.DrawString(w - FXOffset + 1 + AImageItem.Image.Width + 2, ACenterPos - FFont.Ascent div 2, h.text);
          end
          else
          begin
            if FIndentNodeWithNoImage then
              Canvas.DrawString(w - FXOffset + 1 + FNoImageIndent + 2 {spacer}, ACenterPos - FFont.Ascent div 2, h.text)
            else
              Canvas.DrawString(w - FXOffset + 1, ACenterPos - FFont.Ascent div 2, h.text);
          end
        end
        else
          Canvas.DrawString(w - FXOffset + 1, ACenterPos - FFont.Ascent div 2, h.text);
      end;  { if/else }

      Canvas.SetLineStyle(1, FTreeLineStyle);
      if (h.Count > 0) or h.HasChildren then   // do we have subnodes?
      begin
        // small horizontal line above rectangle for first subnode (with children) only
        if (h <> RootNode.FirstSubNode) then
        begin
          if (h.Parent.FirstSubNode = h)  then
          begin
            Canvas.SetLineStyle(1, FTreeLineStyle);
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 7, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 3);
          end;
        end;

        // subnode rectangle around the "+" or "-"
        Canvas.SetColor(FTreeLineColor);
        Canvas.SetLineStyle(1, lsSolid);  // rectangle is always solid line style
        Canvas.DrawRectangle(w - FXOffset - GetColumnWidth(i1) div 2 - 3, ACenterPos - 3, 9, 9);
        
        Canvas.SetColor(clText1);

        if h.Collapsed {or h.HasChildren} then
        begin
          // draw a "+"
          Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 - 1, ACenterPos + 1, w - FXOffset - GetColumnWidth(i1) div 2 + 4, ACenterPos + 1);
          Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 1, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos + 4);
        end
        else
        begin
          // draw a "-"
          Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 - 1, ACenterPos + 1, w - FXOffset - GetColumnWidth(i1) div 2 + 4, ACenterPos + 1);
        end;
        
        Canvas.SetLineStyle(1, FTreeLineStyle);
      end
      else
      begin
        // short horizontal line for each node
        Canvas.SetColor(FTreeLineColor);
        Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1,  ACenterPos + 1, w - FXOffset - 1,  ACenterPos + 1);
      end;

      Canvas.SetColor(FTreeLineColor);
      if h.prev <> nil then
      begin
        // line up to the previous node
        if (h.prev.count > 0) {or h.prev.HasChildren} then
        begin
          // take the previous subnode rectangle in account
          if (h.count > 0) or h.HasChildren then
            // we have a subnode rectangle
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 4, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - (SpaceToVisibleNext(h.prev) * GetNodeHeight) + 5)
          else
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - (SpaceToVisibleNext(h.prev) * GetNodeHeight) + 5);
        end
        else
        begin
          // previous node has no subnodes
          if (h.count > 0) or h.HasChildren then
            // we have a subnode rectangle
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 3, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - SpaceToVisibleNext(h.prev) * GetNodeHeight + 1)
          else
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - SpaceToVisibleNext(h.prev) * GetNodeHeight + 1);
        end;
      end
      else
      begin
        if (h.count > 0) or h.HasChildren then
          // take the subnode rectangle in account
          Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1,ACenterPos - 3, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - GetNodeHeight div 2 + 3)
        else
          Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - GetNodeHeight div 2 + 3);
      end;
    end;
    
    if ShowColumns then
      i := ACenterPos
    else
      i := ACenterPos + GetNodeHeight;
    
    if AVisibleHeight > i then
    begin
      if (h.count > 0) and (not h.Collapsed) then
      begin
        h := h.FirstSubNode;
        continue;
      end;

      if h.next <> nil then
        h := h.next // next node
      else
      begin
        while h.next = nil do // or recurse next node per parent
        begin
          h := h.parent;
          if (h = nil) or (h = rootnode) then
          begin
            break;  //==>
          end;
        end;  { while }
        h := h.next;
      end;  { if/else }
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
          if (h.prev.count > 0) {or h.prev.HasChildren} then
          begin
            x := w - FXOffset - GetColumnWidth(i1) div 2 + 1;
            y := GetAbsoluteNodeTop(h.prev) - FYOffset + 5 + (GetNodeHeight div 2);
            if ShowColumns then
              inc(y, FColumnHeight);
            Canvas.DrawLine(x, ACenterPos, x, y);
          end
          else
          begin
            x := w - FXOffset - GetColumnWidth(i1) div 2 + 1;
            y := GetAbsoluteNodeTop(h.prev) - FYOffset + 1 + (GetNodeHeight div 2);
            if ShowColumns then
              inc(y, FColumnHeight);
            Canvas.DrawLine(x, ACenterPos, x, y);
          end;
        end;
        h := h.parent;
      end;
      break;  //==>
    end;
  end; { while h <> nil }
  Canvas.EndDraw;
end;

procedure TfpgTreeview.DrawHeader(ACol: integer; ARect: TfpgRect;
  AFlags: integer);
begin
  // Here we can implement a head style check
  Canvas.DrawButtonFace(ARect, [btfIsEmbedded]);
end;

procedure TfpgTreeview.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  h: TfpgTreeNode;
  OldSelection: TfpgTreeNode;
begin
  OldSelection := Selection;
  case KeyCode of
    keyRight:
      begin
        Consumed := True;
        Selection.Collapsed := false;
        DoExpand(Selection);
        ResetScrollbar;
        RePaint;
      end;
      
    keyLeft:
      begin
        Consumed := True;
        Selection.Collapsed := true;
        ResetScrollbar;
        RePaint;
      end;
      
    keyUp:
      begin
        if Selection = nil then
          Selection := RootNode.FirstSubNode
        else
          if Selection <> RootNode then
          begin
            if NodeIsVisible(selection) then
            begin
              h := PrevVisualNode(Selection);
              if (h <> RootNode) and (h <> nil) then
                Selection := h;
            end
            else
            begin
              Selection := RootNode.FirstSubNode;
            end;
          end;
          Consumed := True;
      end;
      
    keyDown:
      begin
        Consumed := True;
        if Selection = nil then
          Selection := RootNode.FirstSubNode
        else
        begin
          if NodeIsVisible(selection) then
          begin
            h := NextVisualNode(Selection);
            if (h <> nil) then
              Selection := h;
          end
          else
            Selection := RootNode.FirstSubNode;
        end;
      end;
      
    else
      Consumed := False;
  end;

  if Selection <> OldSelection then
  begin
    RePaint;
    DoChange;
  end;

  if not Consumed then
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfpgTreeview.HandleMouseScroll(x, y: integer;
  shiftstate: TShiftState; delta: smallint);
var
  i: integer;
begin
  inherited HandleMouseScroll(x, y, shiftstate, delta);
  if delta > 0 then
  begin
    inc(FYOffset, FScrollWheelDelta);
    i := (GetNodeHeightSum * GetNodeHeight) - VisibleHeight + FHScrollbar.Height;
    if FYOffset > i then
      FYOffset := i;
    i := FVScrollbar.Position + FScrollWheelDelta;
    FVScrollbar.Position := i;
  end
  else
  begin
    dec(FYOffset, FScrollWheelDelta);
    if FYOffset < 0 then
      FYOffset := 0;
    i := FVScrollbar.Position - FScrollWheelDelta;
    FVScrollbar.Position := i;
  end;
  UpdateScrollbars;
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
  //----------------
  procedure _FindNextNode;
  begin
    if ANode.Next <> nil then
    begin
      result := ANode.Next;
    end
    else
    begin
      while ANode.Next = nil do
      begin
        ANode := ANode.Parent;
        if ANode = nil then
          exit;  //==>
      end;
      result := ANode.Next;
    end;
  end;

begin
  result := nil;
  if ANode.Collapsed then
  begin
    _FindNextNode;
  end
  else
  begin
    if ANode.Count > 0 then
    begin
      result := ANode.FirstSubNode;
    end
    else
      _FindNextNode;
  end;
end;

function TfpgTreeview.PrevVisualNode(ANode: TfpgTreeNode): TfpgTreeNode;
var
  n: TfpgTreeNode;
begin
  n := ANode;
  if ANode.Prev <> nil then
  begin
    result  := ANode.Prev;
    ANode   := ANode.Prev;
    while (not ANode.Collapsed) and (ANode.Count > 0) do
    begin
      result  := ANode.LastSubNode;
      ANode   := ANode.LastSubNode;
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
  FRootNode     := nil;
  FSelection    := nil;
  FShowImages   := False;
  FShowColumns  := False;
  FDefaultColumnWidth := 15;
  FFirstColumn  := nil;
  FFont := fpgGetFont('#Label1');
  FWidth := 150;
  FHeight := 100;

  FHScrollbar := TfpgScrollbar.Create(self);
  FHScrollbar.Orientation := orHorizontal;
  FHScrollbar.OnScroll    := @HScrollbarScroll;
  FHScrollbar.Visible     := False;
  FHScrollbar.Position    := 0;
  FHScrollbar.SliderSize  := 0.5;
  
  FVScrollbar := TfpgScrollbar.Create(self);
  FVScrollbar.Orientation := orVertical;
  FVScrollbar.OnScroll    := @VScrollbarScroll;
  FVScrollbar.Visible     := False;
  FVScrollbar.Position    := 0;
  FVScrollbar.SliderSize  := 0.2;
  
  FBackgroundColor  := clListBox;
  FTreeLineColor    := clShadow1; //clText1;
  FTreeLineStyle    := lsDot;
  FFocusable        := True;
  FMoving           := False;
  FXOffset          := 0;
  FYOffset          := 0;
  FColumnHeight     := FFont.Height + 2;
  FScrollWheelDelta := 15;
  FNoImageIndent    := 16;
  FIndentNodeWithNoImage := True;
end;

destructor TfpgTreeView.Destroy;
begin
  if Assigned(FColumnLeft) then
  begin
    ClearColumnLeft;
    FColumnLeft.Free;
  end;
  FFont.Free;
  FreeAllTreeNodes;
  FRootNode.Free;
  inherited Destroy;
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

