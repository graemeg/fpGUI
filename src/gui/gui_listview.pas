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
  
  { TfpgLVColumn }

  TfpgLVColumn = class(TComponent)
  private
    FAutoSize: Boolean;
    FCaption: String;
    FColumnIndex: Integer;
    FHeight: Integer;
    FVisible: Boolean;
    FWidth: Integer;
    procedure SetAutoSize(const AValue: Boolean);
    procedure SetCaption(const AValue: String);
    procedure SetColumnIndex(const AValue: Integer);
    procedure SetHeight(const AValue: Integer);
    procedure SetVisible(const AValue: Boolean);
    procedure SetWidth(const AValue: Integer);
  public
    constructor Create(AColumns: TfpgLVColumns);
    destructor  Destroy; override;
    property Caption: String read FCaption write SetCaption;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Visible: Boolean read FVisible write SetVisible;
    property ColumnIndex: Integer read FColumnIndex write SetColumnIndex;
  end;
  
  { TfpgLVColumns }

  TfpgLVColumns = class(TPersistent)
  private
    FListView: TfpgListView;
    FColumns: TList;
    function GetColumn(AIndex: Integer): TfpgLVColumn;
    procedure SetColumn(AIndex: Integer; const AValue: TfpgLVColumn);
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
  
  TfpgLVPaintItemEvent = procedure(ListView: TfpgListView; Canvas: TfpgCanvas; Item: TfpgLVItem;
                                   Area:TRect; var PaintPart: TfpgLVItemPaintPart) of object;
  
  
  IfpgLVItemViewer = interface
    procedure ItemDeleted(AIndex: Integer);
    procedure ItemAdded(AIndex: Integer);
    procedure ItemChanged(AIndex: Integer);
  end;
  
  { TfpgLVItems }

  TfpgLVItems = class(TObject)
  private
    FColumns: TfpgLVColumns;
    FViewers: TList;
    FItems: TList;
    function    GetItem(AIndex: Integer): TfpgLVItem;
    procedure   SetItem(AIndex: Integer; const AValue: TfpgLVItem);
    procedure   AddViewer(AValue: IfpgLVItemViewer);
    procedure   DeleteViewer(AValue: IfpgLVItemViewer);
    
    procedure   DoChange(AItem: TfpgLVItem);
    procedure   DoAdd(AItem: TfpgLVItem);
    procedure   DoDelete(AItem: TfpgLVItem);
  protected
  public
    constructor Create(AViewer: IfpgLVItemViewer);
    destructor  Destroy; override;
    function    Add(AItem: TfpgLVItem): Integer;
    function    Count: Integer;
    procedure   Delete(AIndex: Integer);
    function    IndexOf(AItem: TfpgLVItem): Integer;
    procedure   InsertItem(AItem: TfpgLVItem; AIndex: Integer);

    property    Columns: TfpgLVColumns read FColumns;
    property    Item[AIndex: Integer]: TfpgLVItem read GetItem write SetItem;
  end;
  
  TfpgLVItem = class(TObject)
  private
    FCaption: String;
    FItems: TfpgLVItems;
    FSubItems: TStrings;
    function GetSelected(ListView: TfpgListView): Boolean;
    procedure   SetCaption(const AValue: String);
    procedure SetSelected(ListView: TfpgListView; const AValue: Boolean);
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
    FSelected: TList;
    FUpdateCount: Integer;
    FVScrollBar,
    FHScrollBar: TfpgScrollBar;
    FColumns: TfpgLVColumns;
    FItems: TfpgLVItems;
    FOnPaintItem: TfpgLVPaintItemEvent;
    FShowHeaders: Boolean;
    function    GetItemHeight: Integer;
    procedure
    SetItemIndex(const AValue: Integer);
    procedure   SetItems(const AValue: TfpgLVItems);
    procedure   SetMultiSelect(const AValue: Boolean);
    procedure   SetShowHeaders(const AValue: Boolean);
    procedure   VScrollChange(Sender: TObject; Position: Integer);
    procedure   HScrollChange(Sender: TObject; Position: Integer);
    // interface methods
    procedure   ItemDeleted(AIndex: Integer);
    procedure   ItemAdded(AIndex: Integer);
    procedure   ItemChanged(AIndex: Integer);
    
    function    ItemGetSelected(const AItem: TfpgLVItem): Boolean;
    procedure   ItemSetSelected(const AItem: TfpgLVItem; const AValue: Boolean);
    function    ItemGetFromPoint(const X, Y: Integer): TfpgLVItem;
    function    ItemGetRect(AIndex: Integer): TRect;
    function    HeaderHeight: Integer;
    procedure   DoRepaint;
  protected
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
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
    function    ItemAdd: TfpgLVItem;
    property    Columns: TfpgLVColumns read FColumns;
    property    Items: TfpgLVItems read FItems write SetItems;
    property    OnPaintItem: TfpgLVPaintItemEvent read FOnPaintItem write FOnPaintItem;
    property    ShowHeaders: Boolean read FShowHeaders write SetShowHeaders;
    property    MultiSelect: Boolean read FMultiSelect write SetMultiSelect;
    property    VScrollBar: TfpgScrollBar read FVScrollBar;
    property    HScrollBar: TfpgScrollBar read FHScrollBar;
    property    ItemHeight: Integer read GetItemHeight;
    property    ItemIndex: Integer read FItemIndex write SetItemIndex;
  end;
implementation

{ TfpgLVItems }

function TfpgLVItems.GetItem(AIndex: Integer): TfpgLVItem;
begin
  Result := TfpgLVItem(FItems.Items[AIndex]);
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
  if AIndex > -1 then begin
    FViewers.Delete(AIndex);
  end;
  if FViewers.Count = 0 then Free;
end;

procedure TfpgLVItems.DoChange(AItem: TfpgLVItem);
var
  I: Integer;
  AIndex: Integer;
begin
  AIndex := IndexOf(AItem);
  for I := 0 to FViewers.Count -1 do begin
    IfpgLVItemViewer(FViewers.Items[I]).ItemChanged(AIndex);
  end;
end;

procedure TfpgLVItems.DoAdd(AItem: TfpgLVItem);
var
  I: Integer;
  AIndex: Integer;
begin
  AIndex := IndexOf(AItem);
  for I := 0 to FViewers.Count -1 do begin
    IfpgLVItemViewer(FViewers.Items[I]).ItemAdded(AIndex);
  end;
end;

procedure TfpgLVItems.DoDelete(AItem: TfpgLVItem);
var
  I: Integer;
  AIndex: Integer;
begin
  AIndex := IndexOf(AItem);
  for I := 0 to FViewers.Count -1 do begin
    IfpgLVItemViewer(FViewers.Items[I]).ItemDeleted(AIndex);
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
{ TfpgLVItem }

procedure TfpgLVItem.SetCaption(const AValue: String);
begin
  if FCaption=AValue then exit;
  FCaption:=AValue;
  if Assigned(FItems) then FItems.DoChange(Self);
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
  if FShowHeaders=AValue then exit;
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
  if AValue = FItems then Exit;
  AValue.AddViewer(Self);
  FItems.DeleteViewer(Self);
  Fitems := AValue;
end;

procedure TfpgListView.SetMultiSelect(const AValue: Boolean);
begin
  if FMultiSelect=AValue then exit;
  FMultiSelect:=AValue;
end;

function TfpgListView.GetItemHeight: Integer;
begin
  Result := Canvas.Font.Height + 4;
end;

procedure TfpgListView.SetItemIndex(const AValue: Integer);
begin
  if FItemIndex=AValue then exit;
  FItemIndex:=AValue;
end;

procedure TfpgListView.ItemDeleted(AIndex: Integer);
begin
  if FUpdateCount = 0 then DoRePaint;
end;

procedure TfpgListView.ItemAdded(AIndex: Integer);
begin
  if FUpdateCount = 0 then DoRePaint;
end;

procedure TfpgListView.ItemChanged(AIndex: Integer);
begin
  if FUpdateCount = 0 then DoRePaint;
  // TODO
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
  if ShowHeaders then Dec(ItemTop, HeaderHeight);
  Index := ItemTop div ItemHeight;
  if Index < 0 then Exit;
  if Index >= FItems.Count then Exit;

  Result := FItems.Item[Index];
end;

function TfpgListView.ItemGetRect(AIndex: Integer): TRect;
begin
  Result.Top := 2 + (AIndex * ItemHeight) - FVScrollBar.Position;
  if ShowHeaders then Inc(Result.Top, HeaderHeight);
  Result.Bottom := Result.Top + ItemHeight-1;
  Result.Left := 2 - FHScrollBar.Position;
  Result.Right := Width - 4;
  if FVScrollBar.Visible then Dec(Result.Right, FVScrollBar.Width);
end;

function TfpgListView.HeaderHeight: Integer;
begin
  Result := Canvas.Font.Height + 10;
end;

procedure TfpgListView.DoRepaint;
begin
  if FUpdateCount = 0 then RePaint;
end;

procedure TfpgListView.HandleMouseScroll(x, y: integer;
  shiftstate: TShiftState; delta: smallint);
begin
  // Yes this is a dirty dirty hack
  TfpgListView(FVScrollBar).HandleMouseScroll(x, y, shiftstate, delta);
end;

procedure TfpgListView.HandleLMouseDown(x, y: integer; shiftstate: TShiftState
  );
var
  Item: TfpgLVItem;
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  Item := ItemGetFromPoint(X, Y);
  if not FMultiSelect then FSelected.Clear;
  if Item <> nil then begin
    //WriteLn('Got ITem: ', Item.Caption);
    FItemIndex := FItems.IndexOf(Item);
    if FMultiSelect then begin
      if not ((ssCtrl in shiftstate) or (ssShift in shiftstate)) then begin
        FSelected.Clear;
        ItemSetSelected(Item, True);
      end
      else begin
        if ssCtrl in shiftstate then
          ItemSetSelected(Item,  not ItemGetSelected(Item));
        if ssShift in shiftstate then ;
      end
    end
    else ItemSetSelected(Item, True);
  end;
  DoRepaint;
end;


procedure TfpgListView.HandlePaint;
begin
  if FUpdateCount > 0 then Exit;
  
  Canvas.BeginDraw;

  UpdateScrollBarPositions;

  Canvas.Clear(clListBox);

  PaintItems;
  if ShowHeaders then
    PaintHeaders;

  
  fpgStyle.DrawControlFrame(Canvas, 0,0,Width,Height);
  
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
begin
  cLeft := 2;
  if FHScrollBar.Visible then Dec(cLeft, FHScrollBar.Position);
  cTop := 2;
  for I := 0 to Columns.Count-1 do begin
    Column := Columns.Column[I];
    if Column.Visible then begin
      fpgStyle.DrawButtonFace(Canvas,cLeft, cTop, cLeft+Column.Width, Canvas.Font.Height+10, [btnIsEmbedded]);
      fpgStyle.DrawString(Canvas, cLeft+5, cTop+5, Column.Caption, Enabled);
      Inc(cLeft, Column.Width);
    end;
  end;
  if cLeft < FWidth-2 then begin
    fpgStyle.DrawButtonFace(Canvas,cLeft, cTop, cLeft+(Width-2-cLeft), Canvas.Font.Height+10, [btnIsEmbedded, btnIsPressed]);
  end;
end;

procedure TfpgListView.PaintItems;
var
  //ItemRect: TfpgRect;
  VisibleItem: TfpgLVItem;
  FirstIndex,
  LastIndex: Integer;
  I, J : Integer;
  PaintPart: TfpgLVItemPaintPart;
  ItemRect: TRect;
  ItemState: TfpgLVItemState;
  Item: TfpgLVItem;
  TheText: String;
  TextColor: TfpgColor;
  ColumnIndex: Integer;
begin
  FirstIndex := (FVScrollBar.Position) div ItemHeight;
  LastIndex := (FVScrollBar.Position+(Height-4)) div ItemHeight;

  if LastIndex > Fitems.Count-1 then LastIndex := FItems.Count-1;
  
  //WriteLn('FirstIndex = ', FirstIndex, ' LastIndex = ', LastIndex);

  for I := FirstIndex to LastIndex do begin
    ItemState := [];
    PaintPart := [lvppBackground, lvppIcon, lvppText];
    ItemRect := ItemGetRect(I);
    Item := FItems.Item[I];
    if Item.Selected[Self] then Include(ItemState, lisSelected);
    if FItemIndex = I then begin
      Include(ItemState, lisFocused);
      Include(PaintPart, lvppFocused);
    end;

    if lisSelected in (ItemState) then begin
      Canvas.Color := clBlue;
    end
    else Canvas.Color := clListBox;
    

    
    Canvas.FillRectangle(ItemRect);
    Exclude(PaintPart, lvppBackground);
    TextColor := Canvas.TextColor;
    if Assigned(FOnPaintItem) then FOnPaintItem(Self, Canvas, Item, ItemRect, PaintPart);

    if lvppIcon in PaintPart then begin
      // TODO paint icon
    end;
    
    if lvppFocused in PaintPart then begin
      Canvas.Color := clBlack;
      canvas.DrawRectangle(ItemRect);
    end;
    
    if lvppText in PaintPart then begin
      if lisSelected in ItemState then Canvas.TextColor := clwhite;//Canvas.Color xor Canvas.Color;
      for J := 0 to FColumns.Count -1 do begin;
        if FColumns.Column[J].Visible then begin
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
    Canvas.TextColor := TextColor;
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
  
  for I := 0 to Columns.Count -1 do begin
    if Columns.Column[I].Visible then
      Inc(MaxH, Columns.Column[I].Width);
  end;
  
  MaxV := (FItems.Count+1) * ItemHeight - (Height);
  if ShowHeaders then Inc(MaxV, HeaderHeight);


  
  if FHScrollBar.Visible then begin
    FHScrollBar.Top := Height - FHScrollBar.Height - (BevelSize );
    FHScrollBar.Left := BevelSize;
    FHScrollBar.Width := Width - (BevelSize * 2);
    Inc(MaxV, FHScrollBar.Height);
  end;
  
  if FVScrollBar.Visible then begin
    FVScrollBar.Top := BevelSize;
    if ShowHeaders then FVScrollBar.Top := FVScrollBar.Top + HeaderHeight;
    FVScrollBar.Left := Width - FVScrollBar.Width - (BevelSize );
    FVScrollBar.Height := Height - FVScrollBar.Top - BevelSize;
  end;
  
  if FVScrollBar.Visible and FHScrollBar.Visible then begin
    FHScrollBar.Width := FHScrollBar.Width - FVScrollBar.Width;
    FVScrollBar.Height := FVScrollBar.Height - FHScrollBar.Height;
  end;
  

  FHScrollBar.Max := MaxH-(Width-(BevelSize * 2));
  FVScrollBar.Max := MaxV;
  
  FHScrollBar.UpdateWindowPosition;
  FVScrollBar.UpdateWindowPosition;
end;

constructor TfpgListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  ShowHeaders := True;

  FVScrollBar := TfpgScrollBar.Create(Self);
  FVScrollBar.Orientation := orVertical;
  FVScrollBar.OnScroll := @VScrollChange;
  FVScrollBar.ScrollStep := 5;
  FVScrollBar.Position := 0;
  
  FHScrollBar := TfpgScrollBar.Create(Self);
  FHScrollBar.Orientation := orHorizontal;
  FHScrollBar.OnScroll := @HScrollChange;
  FHScrollBar.ScrollStep := 5;
  FHScrollBar.Position := 0;
  
  FColumns := TfpgLVColumns.Create(Self);

  FItems := TfpgLVItems.Create(Self);
  FSelected := TList.Create;
end;

destructor TfpgListView.Destroy;
begin
  FItems.DeleteViewer(Self);
  FSelected.Free;
  inherited Destroy;
end;

procedure TfpgListView.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TfpgListView.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then FUpdateCount := 0;
  if FUpdateCount = 0 then DoRePaint;
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
  if FCaption=AValue then exit;
  FCaption:=AValue;
end;

procedure TfpgLVColumn.SetColumnIndex(const AValue: Integer);
begin
  if FColumnIndex=AValue then exit;
  FColumnIndex:=AValue;
end;

procedure TfpgLVColumn.SetHeight(const AValue: Integer);
begin
  if FHeight=AValue then exit;
  FHeight:=AValue;
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
end;

destructor TfpgLVColumn.Destroy;
begin
  inherited Destroy;
end;

end.
