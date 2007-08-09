unit gui_listview;

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
  TfpgListView = class;
  TfpgLVItem = class;
  TfpgLVColumns = class;
  TfpgLVColumn = class;
  
  TfpgLVColumnClickEvent = procedure(Listview: TfpgListView; Column: TfpgLVColumn; Button: Integer) of object;
  
  { TfpgLVColumn }

  TfpgLVColumn = class(TComponent)
  private
    FDown: Boolean;
    FAutoSize: Boolean;
    FCaption: String;
    FClickable: Boolean;
    FColumnIndex: Integer;
    FHeight: Integer;
    FResizable: Boolean;
    FVisible: Boolean;
    FWidth: Integer;
    procedure   SetAutoSize(const AValue: Boolean);
    procedure   SetCaption(const AValue: String);
    procedure   SetColumnIndex(const AValue: Integer);
    procedure   SetHeight(const AValue: Integer);
    procedure   SetResizable(const AValue: Boolean);
    procedure   SetVisible(const AValue: Boolean);
    procedure   SetWidth(const AValue: Integer);
  public
    constructor Create(AColumns: TfpgLVColumns);
    destructor  Destroy; override;
    property    Caption: String read FCaption write SetCaption;
    property    AutoSize: Boolean read FAutoSize write SetAutoSize;
    property    Width: Integer read FWidth write SetWidth;
    property    Height: Integer read FHeight write SetHeight;
    property    Visible: Boolean read FVisible write SetVisible;
    property    ColumnIndex: Integer read FColumnIndex write SetColumnIndex;
    property    Clickable: Boolean read FClickable write FClickable;
    property    Resizable: Boolean read FResizable write SetResizable;
  end;
  
  { TfpgLVColumns }

  TfpgLVColumns = class(TPersistent)
  private
    FListView: TfpgListView;
    FColumns: TList;
    function    GetColumn(AIndex: Integer): TfpgLVColumn;
    procedure   SetColumn(AIndex: Integer; const AValue: TfpgLVColumn);
  public
    constructor Create(AListView: TfpgListView);
    destructor  Destroy; override;
    function    Add(AColumn: TfpgLVColumn): Integer;
    procedure   Delete(AIndex: Integer);
    procedure   Insert(AColumn: TfpgLVColumn; AIndex: Integer);
    function    Count: Integer;
    property    Column[AIndex: Integer]: TfpgLVColumn read GetColumn write SetColumn;
  end;
  
  TfpgLVItemState = set of (lisFocused, lisSelected, lisHotTrack);
  
  TfpgLVItemPaintPart = set of (lvppBackground, lvppIcon, lvppText, lvppFocused);
  
  TfpgLVPaintColumnEvent = procedure(ListView: TfpgListView; Canvas: TfpgCanvas; Column: TfpgLVColumn;
                                     ColumnIndex: Integer; Area: TfpgRect; var PaintPart: TfpgLVItemPaintPart) of object;
  TfpgLVPaintItemEvent = procedure(ListView: TfpgListView; Canvas: TfpgCanvas; Item: TfpgLVItem;
                                   ItemIndex: Integer; Area:TfpgRect; var PaintPart: TfpgLVItemPaintPart) of object;
  
  
  IfpgLVItemViewer = interface
    procedure ItemDeleted(AIndex: Integer);
    procedure ItemAdded(AIndex: Integer);
    procedure ItemChanged(AIndex: Integer);
    procedure ItemsUpdated;
  end;
  
  { TfpgLVItems }

  TfpgLVItems = class(TObject)
  private
    FUpdateCount: Integer;
    FColumns: TfpgLVColumns;
    FViewers: TList;
    FItems: TList;
    function    GetCapacity: Integer;
    function    GetItem(AIndex: Integer): TfpgLVItem;
    procedure   SetCapacity(const AValue: Integer);
    procedure   SetItem(AIndex: Integer; const AValue: TfpgLVItem);
    procedure   AddViewer(AValue: IfpgLVItemViewer);
    procedure   DeleteViewer(AValue: IfpgLVItemViewer);
    // interface method triggers
    procedure   DoChange(AItem: TfpgLVItem);
    procedure   DoAdd(AItem: TfpgLVItem);
    procedure   DoDelete(AItem: TfpgLVItem);
    procedure   DoEndUpdate;
    
  protected
  public
    constructor Create(AViewer: IfpgLVItemViewer);
    destructor  Destroy; override;
    function    Add(AItem: TfpgLVItem): Integer;
    function    Count: Integer;
    procedure   Delete(AIndex: Integer);
    function    IndexOf(AItem: TfpgLVItem): Integer;
    procedure   InsertItem(AItem: TfpgLVItem; AIndex: Integer);
    procedure   BeginUpdate;
    procedure   EndUpdate;

    property    Capacity: Integer read GetCapacity write SetCapacity;
    property    Columns: TfpgLVColumns read FColumns;
    property    Item[AIndex: Integer]: TfpgLVItem read GetItem write SetItem;
  end;
  
  TfpgLVItem = class(TObject)
  private
    FCaption: String;
    FItems: TfpgLVItems;
    FSubItems: TStrings;
    function    GetSelected(ListView: TfpgListView): Boolean;
    procedure   SetCaption(const AValue: String);
    procedure   SetSelected(ListView: TfpgListView; const AValue: Boolean);
    procedure   SubItemsChanged(Sender: TObject);
  public
    constructor Create(Items: TfpgLVItems); virtual;
    destructor  Destroy; override;
    property    Caption: String read FCaption write SetCaption;
    property    SubItems: TStrings read FSubItems;
    property    Selected[ListView: TfpgListView]: Boolean read GetSelected write SetSelected;
  end;
  
  
  { TfpgListView }

  TfpgListView = class(TfpgWidget, IfpgLVItemViewer)
  private
    FItemIndex: Integer;
    FMultiSelect: Boolean;
    FOnPaintColumn: TfpgLVPaintColumnEvent;
    FShiftCount: Integer;
    FSelectionFollowsFocus: Boolean;
    FSelectionShiftStart: Integer;
    FOnColumnClick: TfpgLVColumnClickEvent;
    FSelected: TList;
    FOldSelected: TList;
    FUpdateCount: Integer;
    FVScrollBar,
    FHScrollBar: TfpgScrollBar;
    FColumns: TfpgLVColumns;
    FItems: TfpgLVItems;
    FOnPaintItem: TfpgLVPaintItemEvent;
    FShowHeaders: Boolean;
    function    GetItemHeight: Integer;
    procedure   SetItemIndex(const AValue: Integer);
    procedure   SetItems(const AValue: TfpgLVItems);
    procedure   SetMultiSelect(const AValue: Boolean);
    procedure   SetOnColumnClick(const AValue: TfpgLVColumnClickEvent);
    procedure   SetShowHeaders(const AValue: Boolean);
    procedure   VScrollChange(Sender: TObject; Position: Integer);
    procedure   HScrollChange(Sender: TObject; Position: Integer);
    // interface methods
    procedure   ItemDeleted(AIndex: Integer);
    procedure   ItemAdded(AIndex: Integer);
    procedure   ItemChanged(AIndex: Integer);
    procedure   ItemsUpdated;
    
    function    GetClientRect: TfpgRect;
    procedure   StartShiftSelection;
    procedure   EndShiftSelection;
    procedure   SelectionSetRangeEnabled(AStart, AEnd: Integer; AValue: Boolean);
    procedure   SelectionToggleRange(AStart, AEnd: Integer; const ShiftState: TShiftState; IgnoreStartIndex: Boolean);
    function    ItemGetSelected(const AItem: TfpgLVItem): Boolean;
    procedure   ItemSetSelected(const AItem: TfpgLVItem; const AValue: Boolean);
    function    ItemGetFromPoint(const X, Y: Integer): TfpgLVItem;
    function    ItemGetRect(AIndex: Integer): TfpgRect;
    function    ItemIndexFromY(Y: Integer): Integer;
    function    HeaderHeight: Integer;
    procedure   DoRepaint;
    procedure   DoColumnClick(Column: TfpgLVColumn; Button: Integer);
    procedure   HandleHeaderMouseMove(x, y: Integer; btnstate: word; Shiftstate: TShiftState);
  protected
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleRMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleRMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandlePaint; override;
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
    procedure   PaintHeaders; virtual;
    procedure   PaintItems; virtual;
    procedure   UpdateScrollBarPositions; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   BeginUpdate;
    procedure   EndUpdate;
    procedure   MakeItemVisible(AIndex: Integer; PartialOK: Boolean = False);
    function    ItemAdd: TfpgLVItem;
    property    Columns: TfpgLVColumns read FColumns;
    property    Items: TfpgLVItems read FItems write SetItems;
    property    SelectionFollowsFocus: Boolean read FSelectionFollowsFocus write FSelectionFollowsFocus;
    property    ShowHeaders: Boolean read FShowHeaders write SetShowHeaders;
    property    MultiSelect: Boolean read FMultiSelect write SetMultiSelect;
    property    VScrollBar: TfpgScrollBar read FVScrollBar;
    property    HScrollBar: TfpgScrollBar read FHScrollBar;
    property    ItemHeight: Integer read GetItemHeight;
    property    ItemIndex: Integer read FItemIndex write SetItemIndex;
    property    OnColumnClick: TfpgLVColumnClickEvent read FOnColumnClick write SetOnColumnClick;
    property    OnPaintColumn: TfpgLVPaintColumnEvent read FOnPaintColumn write FOnPaintColumn;
    property    OnPaintItem: TfpgLVPaintItemEvent read FOnPaintItem write FOnPaintItem;
  end;
implementation

{ TfpgLVItems }

function TfpgLVItems.GetItem(AIndex: Integer): TfpgLVItem;
begin
  Result := TfpgLVItem(FItems.Items[AIndex]);
end;

function TfpgLVItems.GetCapacity: Integer;
begin
  Result := FItems.Capacity;
end;

procedure TfpgLVItems.SetCapacity(const AValue: Integer);
begin
  FItems.Capacity := AValue;
end;

procedure TfpgLVItems.SetItem(AIndex: Integer; const AValue: TfpgLVItem);
begin
  FItems.Items[AIndex] := AValue;
end;

procedure TfpgLVItems.AddViewer(AValue: IfpgLVItemViewer);
begin
  if AValue <> nil then
    FViewers.Add(AValue);
end;

procedure TfpgLVItems.DeleteViewer(AValue: IfpgLVItemViewer);
var
  AIndex: Integer;
begin
  AIndex := FViewers.IndexOf(AValue);
  if AIndex > -1 then
  begin
    FViewers.Delete(AIndex);
  end;
  if FViewers.Count = 0 then
    Free;
end;

procedure TfpgLVItems.DoChange(AItem: TfpgLVItem);
var
  I: Integer;
  AIndex: Integer;
begin
  if FUpdateCount > 0 then
    Exit;
  AIndex := IndexOf(AItem);
  for I := 0 to FViewers.Count -1 do
  begin
    IfpgLVItemViewer(FViewers.Items[I]).ItemChanged(AIndex);
  end;
end;

procedure TfpgLVItems.DoAdd(AItem: TfpgLVItem);
var
  I: Integer;
  AIndex: Integer;
begin
  if FUpdateCount > 0 then
    Exit;
  AIndex := IndexOf(AItem);
  for I := 0 to FViewers.Count -1 do
  begin
    IfpgLVItemViewer(FViewers.Items[I]).ItemAdded(AIndex);
  end;
end;

procedure TfpgLVItems.DoDelete(AItem: TfpgLVItem);
var
  I: Integer;
  AIndex: Integer;
begin
  if FUpdateCount > 0 then
    Exit;
  AIndex := IndexOf(AItem);
  for I := 0 to FViewers.Count -1 do
  begin
    IfpgLVItemViewer(FViewers.Items[I]).ItemDeleted(AIndex);
  end;
end;

procedure TfpgLVItems.DoEndUpdate;
var
  I: Integer;
begin
  if FUpdateCount > 0 then
    Exit;
  for I := 0 to FViewers.Count -1 do
  begin
    IfpgLVItemViewer(FViewers.Items[I]).ItemsUpdated;
  end;
end;

constructor TfpgLVItems.Create(AViewer: IfpgLVItemViewer);
begin
  FItems := TList.Create;
  FViewers := TList.Create;
  AddViewer(AViewer);
end;

destructor TfpgLVItems.Destroy;
begin
  FItems.Free;
  FViewers.Free;
  inherited Destroy;
end;

function TfpgLVItems.Add(AItem: TfpgLVItem): Integer;
begin
  Result := Count;
  InsertItem(AItem, Count);
  DoAdd(AItem);
end;

function TfpgLVItems.Count: Integer;
begin
  Result := FItems.Count;
end;

procedure TfpgLVItems.Delete(AIndex: Integer);
begin
  DoDelete(GetItem(AIndex));
  FItems.Delete(AIndex);
end;

function TfpgLVItems.IndexOf(AItem: TfpgLVItem): Integer;
begin
  Result := FItems.IndexOf(AItem);
end;

procedure TfpgLVItems.InsertItem(AItem: TfpgLVItem; AIndex: Integer);
begin
  if AItem.InheritsFrom(TfpgLVItem) then
    FItems.Insert(AIndex, AItem)
  else
    raise Exception.Create('Item is not of TfpgLVItem type!');
end;

procedure TfpgLVItems.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TfpgLVItems.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then
    FUpdateCount := 0;
  if FUpdateCount = 0 then
    DoEndUpdate;
end;

{ TfpgLVItem }

procedure TfpgLVItem.SetCaption(const AValue: String);
begin
  if FCaption=AValue then
    Exit;
  FCaption:=AValue;
  if Assigned(FItems) then
    FItems.DoChange(Self);
end;

function TfpgLVItem.GetSelected(ListView: TfpgListView): Boolean;
begin
  Result := ListView.ItemGetSelected(Self);
end;

procedure TfpgLVItem.SetSelected(ListView: TfpgListView; const AValue: Boolean);
begin
  ListView.ItemSetSelected(Self, AValue);
end;

procedure TfpgLVItem.SubItemsChanged(Sender: TObject);
begin
  if Assigned(FItems) then
    FItems.DoChange(Self);
end;

constructor TfpgLVItem.Create(Items: TfpgLVItems);
begin
  FItems := Items;
  FSubItems := TStringList.Create;
  TStringList(FSubItems).OnChange := @SubItemsChanged;
end;

destructor TfpgLVItem.Destroy;
begin
  FSubItems.Free;
  inherited Destroy;
end;

{ TfpgListView }


procedure TfpgListView.SetShowHeaders(const AValue: Boolean);
begin
  if FShowHeaders=AValue then
    Exit;
  FShowHeaders:=AValue;
  DoRePaint;
end;
  

procedure TfpgListView.VScrollChange(Sender: TObject; Position: Integer);
begin
  DoRepaint;
end;

procedure TfpgListView.HScrollChange(Sender: TObject; Position: Integer);
begin
  DoRepaint;
end;

procedure TfpgListView.SetItems(const AValue: TfpgLVItems);
begin
  if AValue = FItems then
    Exit;
  AValue.AddViewer(Self);
  FItems.DeleteViewer(Self);
  Fitems := AValue;
end;

procedure TfpgListView.SetMultiSelect(const AValue: Boolean);
begin
  if FMultiSelect=AValue then
    Exit;
  FMultiSelect:=AValue;
end;

procedure TfpgListView.SetOnColumnClick(const AValue: TfpgLVColumnClickEvent);
begin
  if FOnColumnClick=AValue then
    Exit;
  FOnColumnClick:=AValue;
end;

function TfpgListView.GetItemHeight: Integer;
begin
  Result := Canvas.Font.Height + 4;
end;

procedure TfpgListView.SetItemIndex(const AValue: Integer);
begin
  if FItemIndex=AValue then
    Exit;
  if (AValue >= -1) and (AValue < FItems.Count) then
    FItemIndex:=AValue;
end;

procedure TfpgListView.ItemDeleted(AIndex: Integer);
begin
  if FUpdateCount = 0 then
    DoRePaint;
end;

procedure TfpgListView.ItemAdded(AIndex: Integer);
begin
  if FUpdateCount = 0 then
    DoRePaint;
end;

procedure TfpgListView.ItemChanged(AIndex: Integer);
begin
  if FUpdateCount = 0 then
    DoRePaint;
  // TODO
end;

procedure TfpgListView.ItemsUpdated;
begin
  DoRepaint;
end;

function TfpgListView.GetClientRect: TfpgRect;
begin
  Result.Top := 2;
  Result.Left := 2;
  Result.SetRight(Width - 2);
  Result.SetBottom(Height - 2);
end;

procedure TfpgListView.StartShiftSelection;
var
  I: Integer;
begin
  Inc(FShiftCount);
  if FItems.Count = 0 then
    Exit;
  if FShiftCount> 1 then
    Exit;
  FSelectionShiftStart := FItemIndex;
  if FSelectionShiftStart = -1 then
    Inc(FSelectionShiftStart);
  FOldSelected.Clear;
  FOldSelected.Capacity := FSelected.Capacity;
  for I := 0 to FSelected.Count-1 do
  begin
    FOldSelected.Add(FSelected.Items[I]);
  end;
end;

procedure TfpgListView.EndShiftSelection;
begin
  Dec(FShiftCount);
  if FShiftCount > 0 then
    Exit;
  FSelectionShiftStart := -1;
  FOldSelected.Clear;
end;

procedure TfpgListView.SelectionSetRangeEnabled(AStart, AEnd: Integer; AValue: Boolean);
var
  TmpI: LongInt;
  I: LongInt;
  ShouldShow: Boolean;
begin
  if AStart > AEnd then
  begin
    TmpI := AStart;
    AStart := AEnd;
    AEnd := TmpI;
  end;
  FSelected.Clear;
  FSelected.Capacity := FOldSelected.Capacity;
  for I := 0 to FOldSelected.Count-1 do
  begin
    FSelected.Add(FOldSelected.Items[I]);
  end;
  if (AStart < 0) or (AEnd > FItems.Count-1) then
    Exit;
  for I := AStart to AEnd do
  begin
    ShouldShow := AValue;
    if FOldSelected.IndexOf(FItems.Item[I]) > -1 then
      ShouldShow := not AValue;

    if I <> FSelectionShiftStart then
      ItemSetSelected(FItems.Item[I], ShouldShow);
  end;
end;

procedure TfpgListView.SelectionToggleRange(AStart, AEnd: Integer;
  const ShiftState: TShiftState; IgnoreStartIndex: Boolean);
var
 TmpI: Integer;
 I: LongInt;
begin
  TmpI := AStart;
  if AStart > AEnd then
  begin
    AStart := AEnd;
    AEnd := TmpI;
  end;
  if not FMultiSelect then
  begin
    FSelected.Clear;
    ItemSetSelected(FItems.Item[TmpI], True);
    Exit;
  end;
  if ssShift in ShiftState then
    for I := AStart to AEnd do
    begin
      if not(IgnoreStartIndex and (I = TmpI))
      then ItemSetSelected(FItems.Item[I], not ItemGetSelected(FItems.Item[I]));
    end;
end;


function TfpgListView.ItemGetSelected(const AItem: TfpgLVItem): Boolean;
begin
  Result := FSelected.IndexOf(AItem) > -1;
end;

procedure TfpgListView.ItemSetSelected(const AItem: TfpgLVItem; const AValue: Boolean);
var
  Index: Integer;
begin
  Index := FSelected.IndexOf(AItem);
  
  if AValue and (Index = -1) then
    FSelected.Add(AItem);
  if (AValue = False) and (Index <> -1) then
    FSelected.Delete(Index);
end;

function TfpgListView.ItemGetFromPoint(const X, Y: Integer): TfpgLVItem;
var
  Index: Integer;
  ItemTop: Integer;
begin
  Result := nil;
  ItemTop := (FVScrollBar.Position + Y) -2;
  if ShowHeaders then
    Dec(ItemTop, HeaderHeight);
  Index := ItemTop div ItemHeight;
  if Index < 0 then
    Exit;
  if Index >= FItems.Count then
    Exit;

  Result := FItems.Item[Index];
end;

function TfpgListView.ItemGetRect(AIndex: Integer): TfpgRect;
begin
  Result.Top := 2 + (AIndex * ItemHeight) - FVScrollBar.Position;
  if ShowHeaders then
    Inc(Result.Top, HeaderHeight);
  Result.SetBottom(Result.Top + ItemHeight-1);
  Result.Left := 2 - FHScrollBar.Position;
  Result.SetRight(FHScrollBar.Max+(Width-4) - FHScrollBar.Position);
  if FVScrollBar.Visible then
    Dec(Result.Width, FVScrollBar.Width);
end;

function TfpgListView.ItemIndexFromY(Y: Integer): Integer;
var
  TopPos: Integer;
begin
  if ShowHeaders and (Y < HeaderHeight) then
    Exit(-1);

  TopPos := (FVScrollBar.Position + Y) - 2;
  if ShowHeaders then
    Dec(TopPos, HeaderHeight);
  Result := TopPos div ItemHeight;
  if Result > Fitems.Count-1 then
    Result := -1;
end;

function TfpgListView.HeaderHeight: Integer;
begin
  Result := Canvas.Font.Height + 10;
end;

procedure TfpgListView.DoRepaint;
begin
  if FUpdateCount = 0 then
    RePaint;
end;

procedure TfpgListView.DoColumnClick(Column: TfpgLVColumn; Button: Integer);
begin
  if not Column.Clickable then
    Exit;
  if Assigned(FOnColumnClick) then
    FOnColumnClick(Self, Column, Button);

  Column.FDown := True;
  
  if FUpdateCount = 0 then
  begin
    Canvas.BeginDraw(False);//(2,2, width-4, Height-4);
       PaintHeaders;
    Canvas.EndDraw;//(2,2, width-4, Height-4);
  end;
end;

procedure TfpgListView.HandleHeaderMouseMove(x, y: Integer; btnstate: word;
  Shiftstate: TShiftState);
var
  I: Integer;
  cLeft: Integer;
  Column: TfpgLVColumn;
begin
  cLeft := 2;
  for I := 0 to FColumns.Count-1 do
  begin
    Column := FColumns.Column[I];
    if not Column.Visible then
      Continue;
    if Column.Resizable then
    begin
      if X - (cLeft + Column.Width) > 2 then
      begin
        // Mouse.Cursor := mcIBeam;?
        //xc
      end;
    end;
  end;
end;

procedure TfpgListView.HandleMouseScroll(x, y: integer;
  shiftstate: TShiftState; delta: smallint);
var
  cRect: TfpgRect;
begin
  // Yes this is a dirty dirty hack
  cRect := GetClientRect;
  if FShowHeaders then
    Inc(cRect.Top, HeaderHeight);
  if FHScrollBar.Visible then
    Dec(cRect.Height, FHScrollBar.Height);
  if FVScrollBar.Visible then
    Dec(cRect.Width,  FVScrollBar.Width);


  if not PtInRect(cRect, Point(X,Y)) then
    Exit;

  TfpgListView(FVScrollBar).HandleMouseScroll(x, y, shiftstate, delta);
end;

procedure TfpgListView.HandleLMouseDown(x, y: integer; shiftstate: TShiftState
  );
var
  Item: TfpgLVItem;
  IndexOfItem: Integer;
  cRect: TfpgRect;
  cLeft, cRight: Integer;
  I: Integer;
  Column: TfpgLVColumn;
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  
  cRect := GetClientRect;
  
  if not PtInRect(cRect, Point(X,Y)) then
    Exit;
  
  if FShowHeaders then
  begin
    if (Y < HeaderHeight + cRect.Top)  then
    begin
      cLeft := cRect.Left - FHScrollBar.Position;
      for I := 0 to FColumns.Count-1 do
      begin
        Column := FColumns.Column[I];
        if Column.Visible then
        begin
          cRight := cLeft + Column.Width-1;
          if (X <= cRight) and (X >= cLeft) then
            DoColumnClick(Column, 1);
          Inc(cLeft, Column.Width);
        end;
      end;
    end;
    Inc(cRect.Top, HeaderHeight);
  end;
  
  if FHScrollBar.Visible then
    Dec(cRect.Height, FHScrollBar.Height);
  if FVScrollBar.Visible then
    Dec(cRect.Width,  FVScrollBar.Width);
  
  if not PtInRect(cRect, Point(X,Y)) then
    Exit;

  Item := ItemGetFromPoint(X, Y);
  IndexOfItem := ItemIndexFromY(Y);
  if not FMultiSelect then
    FSelected.Clear;
  if Item <> nil then
  begin
    FItemIndex := ItemIndexFromY(Y);
    MakeItemVisible(FItemIndex);
    if FMultiSelect then
    begin
      if not ((ssCtrl in shiftstate) or (ssShift in shiftstate)) then
      begin
        FSelected.Clear;
        ItemSetSelected(Item, True);
      end
      else begin
        if ssCtrl in shiftstate then
          ItemSetSelected(Item,  not ItemGetSelected(Item));
        if ssShift in shiftstate then
          SelectionSetRangeEnabled(FSelectionShiftStart, FItemIndex, True);
      end
    end
    else ItemSetSelected(Item, True);
  end;
  DoRepaint;
end;

procedure TfpgListView.HandleRMouseDown(x, y: integer; shiftstate: TShiftState);
var
  I: Integer;
  cLeft, cRight: Integer;
  cRect: TfpgRect;
  Column: TfpgLVColumn;
begin
  inherited HandleRMouseDown(x, y, shiftstate);

  cRect := GetClientRect;

  if not PtInRect(cRect, Point(X,Y)) then
    Exit;

  if FShowHeaders then
  begin
    if (Y < HeaderHeight + cRect.Top) then
    begin
      cLeft := cRect.Left - FHScrollBar.Position;
      for I := 0 to FColumns.Count-1 do
      begin
        Column := FColumns.Column[I];
        if Column.Visible then
        begin
          cRight := cLeft + Column.Width-1;
          if (X <= cRight) and (X >= cLeft) then
            DoColumnClick(Column, 3);
          Inc(cLeft, Column.Width);
        end;
      end;
    end;
    Inc(cRect.Top, HeaderHeight);
  end;
  
  if FVScrollBar.Visible then
    Dec(cRect.Width, FVScrollBar.Width);
  if FHScrollBar.Visible then
    Dec(cRect.Height, FHScrollBar.Height);
end;

procedure TfpgListView.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  I: Integer;
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  for I := 0 to FColumns.Count-1 do
  begin
    FColumns.Column[I].FDown := False;
  end;
  DoRepaint;
end;

procedure TfpgListView.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
var
  I: Integer;
begin
  inherited HandleRMouseUp(x, y, shiftstate);
  for I := 0 to FColumns.Count-1 do
  begin
    FColumns.Column[I].FDown := False;
  end;
  DoRepaint;
end;

procedure TfpgListView.HandleMouseMove(x, y: integer; btnstate: word;
  shiftstate: TShiftState);
var
  cRect: TfpgRect;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  cRect := GetClientRect;

  if not PtInRect(cRect, Point(X,Y)) then
    Exit;

  if Y < (cRect.Top + HeaderHeight) then
  begin
    HandleHeaderMouseMove(x, y, btnstate, shiftstate);
  end;
  
  //if FVScrollBar.Visible then Dec(cRect.Width, FVScrollBar.Width);
  //if FHScrollBar.Visible then Dec(cRect.Height, FHScrollBar.Height);
end;

procedure TfpgListView.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  iIndex: Integer;
  AStart, AEnd: Integer;
  OldIndex: Integer;
  procedure CheckMultiSelect;
  begin
    if FMultiSelect then begin
      if (ssShift in shiftstate) or (FSelectionShiftStart > -1) then
      begin
        SelectionSetRangeEnabled(FSelectionShiftStart, FItemIndex, True);
      end
      else if ssCtrl in shiftstate then
      begin
        SelectionToggleRange(FItemIndex, FItemIndex, shiftstate, False);
      end;
    end;
  end;
  procedure CheckSelectionFocus;
  begin
    if ((ssShift in shiftstate) or (ssCtrl in shiftstate)) then
      Exit;
    FSelected.Clear;
    if FSelectionFollowsFocus and (FItemIndex > -1) then
      ItemSetSelected(FItems.Item[FItemIndex], True);
  end;
begin
  consumed := True;
  OldIndex := FItemIndex;
  //WriteLn('Got key: ',IntToHex(keycode, 4));
  case keycode of
    keyShift, keyShiftR:
    begin
      if FMultiSelect then
        StartShiftSelection;
    end;
    keyUp:
    begin
      if ItemIndex > 0 then
        ItemIndex := ItemIndex-1;
      MakeItemVisible(ItemIndex);
      CheckSelectionFocus;
      CheckMultiSelect;
    end;
    keyDown:
    begin
      ItemIndex := ItemIndex+1;
      MakeItemVisible(ItemIndex);
      CheckSelectionFocus;
      CheckMultiSelect;
    end;
    keyLeft:
    begin
      FHScrollBar.Position := FHScrollBar.Position - FHScrollBar.ScrollStep;
    end;
    keyRight:
    begin
      FHScrollBar.Position := FHScrollBar.Position + FHScrollBar.ScrollStep;
    end;
    keyHome:
    begin
      ItemIndex := 0;
      MakeItemVisible(ItemIndex);
      CheckSelectionFocus;
      CheckMultiSelect;
    end;
    keyEnd:
    begin
      ItemIndex := FItems.Count-1;
      MakeItemVisible(ItemIndex);
      CheckSelectionFocus;
      CheckMultiSelect;
    end;
    keyPageUp:
    begin
      iIndex := ItemIndex - (FVScrollBar.Height div ItemHeight);
      if iIndex < 0 then
        iIndex := 0;
      ItemIndex := iIndex;
      MakeItemVisible(ItemIndex);
      CheckSelectionFocus;
      CheckMultiSelect;
    end;
    keyPageDown:
    begin
      iIndex := ItemIndex + (FVScrollBar.Height div ItemHeight);
      if iIndex > FItems.Count-1 then
        iIndex := FItems.Count-1;
      ItemIndex := iIndex;
      MakeItemVisible(ItemIndex);
      CheckSelectionFocus;
      CheckMultiSelect
    end;
  else
    consumed := False;
    inherited HandleKeyPress(keycode, shiftstate, consumed);
    Exit;
  end;
  DoRepaint;

end;

procedure TfpgListView.HandleKeyRelease(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  consumed := True;
  case keycode of
    keyShift, keyShiftR:
    begin
      EndShiftSelection;
    end;
  else
    consumed := False;
    inherited HandleKeyRelease(keycode, shiftstate, consumed);
  end;
  
end;


procedure TfpgListView.HandlePaint;
var
 ClipRect: TfpgRect;
begin
  if FUpdateCount > 0 then
    Exit;
  
  // we disable bouble buffering
  Canvas.BeginDraw;//(False);

  UpdateScrollBarPositions;

  //Canvas.Clear(clListBox);
  
  fpgStyle.DrawControlFrame(Canvas, 0,0,Width,Height);
  
  ClipRect.Top := 2;
  ClipRect.Left := 2;
  ClipRect.Width := Width -4;
  ClipRect.Height := Height -4;
  
  if ShowHeaders then
  begin
    PaintHeaders;
    Inc(ClipRect.Top, HeaderHeight);
    Dec(ClipRect.Height, HeaderHeight);
  end;
  
  Canvas.SetClipRect(ClipRect);

  // this paints the small square remaining below the vscrollbar and to the right of the hscrollbar
  if FVScrollBar.Visible and FHScrollBar.Visible then
  begin
    Canvas.Color := clButtonFace;
    Canvas.FillRectangle(Width - 2 - FVScrollBar.Width,
                         Height - 2 - FHScrollBar.Height,
                         Width - 2,
                         Height - 2);
  end;
  
  if FVScrollBar.Visible then
    Dec(ClipRect.Width, FVScrollBar.Width);
  if FHScrollBar.Visible then
    Dec(ClipRect.Height, FhScrollBar.Height);
    
  Canvas.SetClipRect(ClipRect);
  
  PaintItems;
  
  Canvas.EndDraw;
end;

procedure TfpgListView.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);

  UpdateScrollBarPositions;
end;

procedure TfpgListView.PaintHeaders;
var
  I: Integer;
  cLeft,
  cTop: Integer;
  Column: TfpgLVColumn;
  Flags: TFButtonFlags;
  ClipRect: TfpgRect;
  cRect: TfpgRect;
  PaintPart: TfpgLVItemPaintPart;
begin
  cLeft := 2;
  ClipRect.Top := 2;
  ClipRect.Left := 2;
  ClipRect.Height := HeaderHeight;
  ClipRect.Width := Width -4;
  Canvas.SetClipRect(ClipRect);
  
  if FHScrollBar.Visible then Dec(cLeft, FHScrollBar.Position);
  cTop := 2;
  for I := 0 to Columns.Count-1 do
  begin
    Column := Columns.Column[I];
    if Column.Visible then
    begin
      Flags := [btnIsEmbedded];
      if Column.FDown then Flags := Flags + [btnIsPressed];
      cRect.Top := cTop;
      cRect.Left := cLeft;
      cRect.Width := Column.Width;
      cRect.Height := HeaderHeight;
      fpgStyle.DrawButtonFace(Canvas,cLeft, cRect.Top, cRect.Width, cRect.Height, Flags);
      PaintPart := [lvppText];
      
      if Assigned(FOnPaintColumn) then
        FOnPaintColumn(Self, Canvas, Column, I, cRect, PaintPart);
        
      if lvppText in PaintPart then
        fpgStyle.DrawString(Canvas, cLeft+5, cTop+5, Column.Caption, Enabled);
      Inc(cLeft, Column.Width);
    end;
  end;
  if cLeft < FWidth-2 then
  begin
    Canvas.SetColor(clButtonFace);
    Canvas.FillRectangle(cLeft, cTop, cLeft+(Width-3-cLeft), Canvas.Font.Height+10);

  end;
end;

procedure TfpgListView.PaintItems;
var
  VisibleItem: TfpgLVItem;
  FirstIndex,
  LastIndex: Integer;
  I, J : Integer;
  PaintPart: TfpgLVItemPaintPart;
  ItemRect: TfpgRect;
  ItemState: TfpgLVItemState;
  Item: TfpgLVItem;
  TheText: String;
  TextColor: TfpgColor;
  ColumnIndex: Integer;
  cBottom: Integer;
  vBottom: Integer;
  cRight: Integer;
begin
  FirstIndex := (FVScrollBar.Position) div ItemHeight;
  LastIndex := (FVScrollBar.Position+(Height-4)) div ItemHeight;

  if LastIndex > Fitems.Count-1 then
    LastIndex := FItems.Count-1;
    
  cBottom := 2 + ((LastIndex+1 - FirstIndex) * ItemHeight);
  
  if ShowHeaders then
    Inc(cBottom, HeaderHeight);

  
  for I := FirstIndex to LastIndex do
  begin
    ItemState := [];
    PaintPart := [lvppBackground, lvppIcon, lvppText];
    ItemRect := ItemGetRect(I);
    
    if  (I = FirstIndex)
    and (ItemRect.Top < 2 + HeaderHeight) then
      Dec(cBottom, (2 + HeaderHeight) - ItemRect.Top);
    
    Item := FItems.Item[I];
    if Item.Selected[Self] then
      Include(ItemState, lisSelected);
    if FItemIndex = I then
    begin
      Include(ItemState, lisFocused);
      Include(PaintPart, lvppFocused);
    end;

    if lisSelected in (ItemState) then
    begin
      if Focused then
        Canvas.Color := clSelection
      else
        Canvas.Color := clInactiveSel;
    end
    else Canvas.Color := clListBox;
    

    
    Canvas.FillRectangle(ItemRect);
    Exclude(PaintPart, lvppBackground);
    TextColor := Canvas.TextColor;
    if Assigned(FOnPaintItem) then
      FOnPaintItem(Self, Canvas, Item, I, ItemRect, PaintPart);

    if lvppIcon in PaintPart then
    begin
      // TODO paint icon
    end;
    
    if lvppFocused in PaintPart then
    begin
      Canvas.Color := clBlack;
      Canvas.SetLineStyle(1, lsDot);
      Canvas.DrawRectangle(ItemRect);
    end;
    
    if lvppText in PaintPart then
    begin
      if lisSelected in ItemState then
        Canvas.TextColor := clSelectionText;
      for J := 0 to FColumns.Count -1 do
      begin
        if FColumns.Column[J].Visible then
        begin
          if FColumns.Column[J].ColumnIndex <> -1 then
            ColumnIndex := FColumns.Column[J].ColumnIndex
          else ColumnIndex := J;
          if ColumnIndex = 0 then
            TheText := Item.Caption
          else if item.SubItems.Count > ColumnIndex then
            TheText := Item.SubItems.Strings[ColumnIndex-1]
          else
            TheText := '';

          fpgStyle.DrawString(Canvas, ItemRect.Left+5, ItemRect.Top+2, TheText, Enabled);
          Inc(ItemRect.Left, FColumns.Column[J].Width);
          //WriteLn(ItemRect.Left,' ', ItemRect.Top, ' ', ItemRect.Right, ' ', ItemRect.Bottom);
        end;
      end;
    end;
    
    //Inc(cBottom, ItemRect.Height);
    
    Canvas.TextColor := TextColor;
  end;
  
  vBottom := Height - 2;
  if FHScrollBar.Visible then
    Dec(vBottom, FHScrollBar.Height);
    
  // the painted items haven't fully covered the visible area
  if vBottom > cBottom then begin
    ItemRect.Left := 2;
    ItemRect.Top := cBottom;
    ItemRect.SetBottom(vBottom);
    ItemRect.Width := Width - 4;
    Canvas.SetColor(clListBox);
    Canvas.FillRectangle(ItemRect);
  end;
end;

procedure TfpgListView.UpdateScrollBarPositions;
var
  BevelSize: Integer;
  I: Integer;
  MaxH,
  MaxV: Integer;
begin
  MaxH := 0;
  MaxV := 0;
  BevelSize := 2;
  
  for I := 0 to Columns.Count -1 do
  begin
    if Columns.Column[I].Visible then
      Inc(MaxH, Columns.Column[I].Width);
  end;
  
  MaxV := (FItems.Count+2) * ItemHeight - (Height);
  if ShowHeaders then
    Inc(MaxV, HeaderHeight);
  
  FHScrollBar.Top := Height - FHScrollBar.Height - (BevelSize );
  FHScrollBar.Left := BevelSize;
  FHScrollBar.Width := Width - (BevelSize * 2);

  
  FVScrollBar.Top := BevelSize;
  if ShowHeaders then
    FVScrollBar.Top := FVScrollBar.Top + HeaderHeight;
  FVScrollBar.Left := Width - FVScrollBar.Width - (BevelSize );
  FVScrollBar.Height := Height - FVScrollBar.Top - BevelSize;

  
  if FVScrollBar.Visible and FHScrollBar.Visible then
  begin
    FHScrollBar.Width := FHScrollBar.Width - FVScrollBar.Width;
    FVScrollBar.Height := FVScrollBar.Height - FHScrollBar.Height;
  end;

  FHScrollBar.Max := MaxH-(Width-(BevelSize * 2));
  FVScrollBar.Max := MaxV;
  
  if FVScrollBar.Max = 0 then
    FVScrollBar.SliderSize := 1
  else
    FVScrollBar.SliderSize := FVScrollBar.Height / (FVScrollBar.Max + FVScrollBar.Height);
  FVScrollBar.RepaintSlider;

  if FHScrollBar.Max = 0 then
    FHScrollBar.SliderSize := 1
  else
    FHScrollBar.SliderSize := FHScrollBar.Width / (FHScrollBar.Max + FHScrollBar.Width);
  FHScrollBar.RepaintSlider;


  if FHScrollBar.Visible then
    FHScrollBar.UpdateWindowPosition;
  if FVScrollBar.Visible then
    FVScrollBar.UpdateWindowPosition;
end;

constructor TfpgListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Focusable := True;
  ShowHeaders := True;

  FVScrollBar := TfpgScrollBar.Create(Self);
  FVScrollBar.Orientation := orVertical;
  FVScrollBar.OnScroll := @VScrollChange;
  FVScrollBar.ScrollStep := 10;
  FVScrollBar.Position := 0;
  
  FHScrollBar := TfpgScrollBar.Create(Self);
  FHScrollBar.Orientation := orHorizontal;
  FHScrollBar.OnScroll := @HScrollChange;
  FHScrollBar.ScrollStep := 10;
  FHScrollBar.Position := 0;
  
  FColumns := TfpgLVColumns.Create(Self);

  FItems := TfpgLVItems.Create(Self);
  FSelected := TList.Create;
  FOldSelected := TList.Create;;
  FSelectionShiftStart := -1;
  FSelectionFollowsFocus := True;
  FItemIndex := -1;
end;

destructor TfpgListView.Destroy;
begin
  FItems.DeleteViewer(Self);
  FSelected.Free;
  FOldSelected.Free;
  inherited Destroy;
end;

procedure TfpgListView.BeginUpdate;
begin
  Inc(FUpdateCount);
  FItems.BeginUpdate;
end;

procedure TfpgListView.EndUpdate;
begin
  FItems.EndUpdate;
  Dec(FUpdateCount);
  if FUpdateCount < 0 then
    FUpdateCount := 0;
  if FUpdateCount = 0 then
    DoRePaint;
end;

procedure TfpgListView.MakeItemVisible(AIndex: Integer; PartialOK: Boolean);
var
  Index: Integer;
  iTop,
  iBottom,
  vTop,
  vBottom: Integer;
  tVisible, bVisible: Boolean;
begin
  if AIndex = -1 then
    Exit;
  iTop := AIndex * ItemHeight;
  iBottom := iTop + ItemHeight;

  tVisible := (iTop >= FVScrollBar.Position) and (iTop < FVScrollBar.Position + FVScrollBar.Height);
  bVisible := (iBottom >= FVScrollBar.Position) and (iBottom < FVScrollBar.Position + FVScrollBar.Height);

  if PartialOK and (bVisible or tVisible) then
    Exit;
  
  if bVisible and tVisible then
    Exit;
  
  if  (iBottom >= FVScrollBar.Position + FVScrollBar.Height) then
    FVScrollBar.Position := iBottom - FVScrollBar.Height
  else
    FVScrollBar.Position := iTop;
end;

function TfpgListView.ItemAdd: TfpgLVItem;
begin
  Result := TfpgLVItem.Create(FItems);
  FItems.Add(Result);
end;

{ TfpgLVColumns }

function TfpgLVColumns.GetColumn(AIndex: Integer): TfpgLVColumn;
begin
  Result := TfpgLVColumn(FColumns.Items[AIndex]);
end;

procedure TfpgLVColumns.SetColumn(AIndex: Integer; const AValue: TfpgLVColumn);
begin
  FColumns.Items[AIndex] := AValue;
end;

constructor TfpgLVColumns.Create(AListView: TfpgListView);
begin
  FListView := AListView;
  FColumns := TList.Create;
end;

destructor TfpgLVColumns.Destroy;
begin
  FColumns.Free;
  inherited Destroy;
end;

function TfpgLVColumns.Add(AColumn: TfpgLVColumn): Integer;
begin
  Result := Count;
  Insert(AColumn, Count);
end;

procedure TfpgLVColumns.Delete(AIndex: Integer);
begin
  FColumns.Delete(AIndex);
end;

procedure TfpgLVColumns.Insert(AColumn: TfpgLVColumn; AIndex: Integer);
begin
  FColumns.Insert(AIndex, AColumn);
end;

function TfpgLVColumns.Count: Integer;
begin
  Result := FColumns.Count;
end;

{ TfpgLVColumn }

procedure TfpgLVColumn.SetCaption(const AValue: String);
begin
  if FCaption=AValue then
    Exit;
  FCaption:=AValue;
end;

procedure TfpgLVColumn.SetColumnIndex(const AValue: Integer);
begin
  if FColumnIndex=AValue then
    Exit;
  FColumnIndex:=AValue;
end;

procedure TfpgLVColumn.SetHeight(const AValue: Integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
end;

procedure TfpgLVColumn.SetResizable(const AValue: Boolean);
begin
  if FResizable=AValue then exit;
  FResizable:=AValue;
end;

procedure TfpgLVColumn.SetVisible(const AValue: Boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
end;

procedure TfpgLVColumn.SetAutoSize(const AValue: Boolean);
begin
  if FAutoSize=AValue then exit;
  FAutoSize:=AValue;
end;

procedure TfpgLVColumn.SetWidth(const AValue: Integer);
begin
  if FWidth=AValue then exit;
  FWidth:=AValue;
end;

constructor TfpgLVColumn.Create(AColumns: TfpgLVColumns);
begin
  FVisible := True;
  FColumnIndex := -1;
  FClickable := True;
end;

destructor TfpgLVColumn.Destroy;
begin
  inherited Destroy;
end;

end.
