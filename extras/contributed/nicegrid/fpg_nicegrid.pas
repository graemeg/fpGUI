unit fpg_nicegrid;
{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, fpg_base, fpg_main, fpg_panel, fpg_scrollbar, fpg_edit;

type
  PHeaderInfo = ^THeaderInfo;
  THeaderInfo = record
    Str: string;
    Rc: TfpgRect;
  end;

  THorzAlign = (haLeft, haCenter, haRight);
  TVertAlign = (vaTop, vaCenter, vaBottom);
  TGutterKind = (gkNone, gkBlank, gkPointer, gkNumber, gkString);
  TGridHittest = (gtNone, gtLeftTop, gtLeft, gtTop, gtCell, gtColSizing, gtSmallBox);

  TfpgNiceGrid = class;

  TfpgNiceColumn = class(TCollectionItem)
  private
    FTitle: string;
    FFooter: string;
    FWidth: Integer;
    FFont: string;
    FFontColor: TfpgColor;
    FColor: TfpgColor;
    FHorzAlign: THorzAlign;
    FVertAlign: TVertAlign;
    FVisible: Boolean;
    FStrings: TStrings;
    FTag: Integer;
    FTag2: Integer;
    FCanResize: Boolean;
    FHint: string;
    FReadOnly: Boolean;
    function GetGrid: TfpgNiceGrid;
    function IsFontStored: Boolean;
    procedure SetTitle(Value: string);
    procedure SetWidth(Value: Integer);
    procedure SetFont(Value: string);
    procedure SetColor(Value: TfpgColor);
    procedure SetHorzAlign(Value: THorzAlign);
    procedure SetVertAlign(Value: TVertAlign);
    procedure SetVisible(Value: Boolean);
    procedure SetStrings(Value: TStrings);
    procedure SetFooter(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collec: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Grid: TfpgNiceGrid read GetGrid;
    property Title: string read FTitle write SetTitle;
    property Footer: string read FFooter write SetFooter;
    property Width: Integer read FWidth write SetWidth;
    property Font: string read FFont write SetFont stored IsFontStored;
    property FontColor: TfpgColor read FFontColor write FFontColor;
    property Color: TfpgColor read FColor write SetColor default clGray; //clWindow;
    property HorzAlign: THorzAlign read FHorzAlign write SetHorzAlign default haLeft;
    property VertAlign: TVertAlign read FVertAlign write SetVertAlign default vaCenter;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Tag: Integer read FTag write FTag default 0;
    property Tag2: Integer read FTag2 write FTag2 default 0;
    property Hint: string read FHint write FHint;
    property Strings: TStrings read FStrings write SetStrings;
    property CanResize: Boolean read FCanResize write FCanResize default True;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
  end;


  TfpgNiceColumns = class(TOwnedCollection) 
  private
    FGrid: TfpgNiceGrid;
    function GetItem(Index: Integer): TfpgNiceColumn;
    procedure SetItem(Index: Integer; Value: TfpgNiceColumn);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    property Grid: TfpgNiceGrid read FGrid;
    property Items[Index: Integer]: TfpgNiceColumn read GetItem write SetItem; default;
    function Add: TfpgNiceColumn;
    function AddItem(Item: TfpgNiceColumn; Index: Integer): TfpgNiceColumn;
    function Insert(Index: Integer): TfpgNiceColumn;
  end;


   TfpgNiceInplace = class(TfpgEdit)
  private
    FGrid: TfpgNiceGrid;
    FAlignment: THorzAlign;
    CellX, CellY: Integer;
    BuffTmp: string;
    procedure SetAlignment(Value: THorzAlign);
  protected
   procedure Change(Sender: TObject);
   procedure HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);override;
  public
    constructor Create(AGrid: TfpgNiceGrid);reintroduce;
    procedure ShowEdit(X, Y: Integer);
    procedure HideEdit;
  end;

  TfpgMergeCell = class(TObject)
  public
    Text: string;
    Rc: TfpgRect;
    Color: TfpgColor;
    Font: string;
    HorzAlign: THorzAlign;
    VertAlign: TVertAlign;
    constructor Create;
    destructor Destroy; override;
  end;

  TOnDrawCellEvent = procedure (Sender: TObject; ACanvas: TfpgCanvas; X, Y: Integer;
    Rc: TfpgRect; var Handled: Boolean) of object;

  TOnDrawHeaderEvent = procedure (Sender: TObject; ACanvas: TfpgCanvas; Rc: TfpgRect;
    Str: string; var Handled: Boolean) of object;

  TOnHeaderClick = procedure (Sender: TObject; ACol: Integer;
    Button: TMouseButton; Shift: TShiftState) of object;

  TOnGutterClick = procedure (Sender: TObject; ARow: Integer;
    Button: TMouseButton; Shift: TShiftState) of object;

  TOnCellAssignment = procedure (Sender: TObject; ACol, ARow: Integer;
    var Str: string) of object;

  TOnCellChange = procedure (Sender: TObject; ACol, ARow: Integer; var Str: string)
    of object;

  TOnCellChanging = procedure (Sender: TObject; ACol, ARow: Integer;
    var CanChange: Boolean) of object;

  TOnRowEvent = procedure (Sender: TObject; ARow: Integer) of object;

  TOnColRowChanged = procedure (Sender: TObject; ACol, ARow: Integer) of object;

  TfpgNiceGridSync = class;

  TfpgNiceGrid = class(TfpgPanel)
  private
    ForcedColumn: Integer;
    FixedWidth, FixedHeight: Integer;
    BodyWidth, BodyHeight: Integer;
    AllWidth, AllHeight: Integer;
    FooterTop: Integer;
    CellBox: TfpgRect;

    FHorzOffset: Integer;
    FVertOffset: Integer;
    FMaxHScroll: Integer;
    FMaxVScroll: Integer;
    FSmallChange: Integer;
    FLargeChange: Integer;

    FAutoAddRow: Boolean;
    FRowCount: Integer;
    FDefRowHeight: Integer;
    FDefColWidth: Integer;
    FFlat: Boolean;

    FHeaderLine: Integer;
    FHeaderInfos: TList;
    FUpdating: Boolean;
    FColor: TfpgColor;
    FAlternateColor: TfpgColor;
    FGridColor: TfpgColor;
    FShowGrid: Boolean;
    FHeaderColor: TfpgColor;
    FHeaderLightColor: TfpgColor;
    FHeaderDarkColor: TfpgColor;
    FSelectionColor: TfpgColor;
    FHeaderFont: string;
    FHeaderFontColor: TfpgColor;
    FGutterFont: string;
    FGutterFontColor: TfpgColor;
    FFooterFont: string;
    FFooterFontColor: TfpgColor;
    
    FGutterKind: TGutterKind;
    FGutterWidth: Integer;

    FFitToWidth: Boolean;
    FAutoColWidth: Boolean;
    FReadOnly: Boolean;
    FColumns: TfpgNiceColumns;

    FEdit: TfpgNiceInplace;
    FCol: Integer;
    FRow: Integer;
    FCol2, FRow2: Integer; // Selection
    FSelectArea: TfpgRect;

    SmallBox: TfpgRect;
    SmallBoxArea: TfpgRect;
    SmallBoxPos: Byte;

    BuffString: string;
    IsEditing: Boolean;
    SizingCol: Integer;
    SizingColX: Integer;
    LastHover: Integer;
    Sync: TfpgNiceGridSync;
    Mergeds: TList;

    FOnDrawCell: TOnDrawCellEvent;
    FOnDrawHeader: TOnDrawHeaderEvent;
    FOnDrawGutter: TOnDrawHeaderEvent;
    FOnDrawFooter: TOnDrawHeaderEvent;
    FOnHeaderClick: TOnHeaderClick;
    FOnGutterClick: TOnGutterClick;
    FOnCellChange: TOnCellChange;
    FOnCellChanging: TOnCellChanging;
    FOnColRowChanged: TOnColRowChanged;
    FOnInsertRow: TOnRowEvent;
    FOnDeleteRow: TOnRowEvent;
    FOnCellAssignment: TOnCellAssignment;
    FGutterStrings: TStrings;
    FShowFooter: Boolean;

 //*************************   
    FVScrollBar: TfpgScrollBar;
    FHScrollBar: TfpgScrollBar;
    procedure   UpdateScrollBars; virtual;
    procedure   HScrollBarMove(Sender: TObject; position: integer);
    procedure   VScrollBarMove(Sender: TObject; position: integer);
  //*****************************  
  
    function TotalWidth: Integer;
    procedure ClearHeaderInfos;

    procedure ClearUnused;
    procedure RenderGutter;
    procedure RenderHeader;
    procedure DrawSelection;

    procedure SetHorzOffset(Value: Integer);
    procedure SetVertOffset(Value: Integer);
    function    GetColCount: Integer;
    procedure SetColCount(Value: Integer);
    procedure SetRowCount(Value: Integer);
    procedure SetDefColWidth(Value: Integer);
    procedure SetDefRowHeight(Value: Integer);
    procedure SetFlat(Value: Boolean);
    procedure SetColor(Value: TfpgColor);
    procedure SetAlternateColor(Value: TfpgColor);
    procedure SetGridColor(Value: TfpgColor);
    procedure SetShowGrid(Value: Boolean);
    procedure SetHeaderLine(Value: Integer);
    procedure SetHeaderColor(Value: TfpgColor);
    procedure SetHeaderLightColor(Value: TfpgColor);
    procedure SetHeaderDarkColor(Value: TfpgColor);
    procedure SetHeaderFont(Value: string);
    procedure SetHeaderFontColor(Value: TfpgColor);
    procedure SetSelectionColor(Value: TfpgColor);
    procedure SetFitToWidth(Value: Boolean);
    procedure SetAutoColWidth(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure InternalSetCell(X, Y: Integer; Value: string; FireOnChange: Boolean);
    procedure SetCell(X, Y: Integer; Value: string);
    function    GetColWidths(Idx: Integer): Integer;
    procedure SetColWidths(Idx: Integer; Value: Integer);
    procedure SetColumns(Value: TfpgNiceColumns);
    procedure SetCol(Value: Integer);
    procedure SetRow(Value: Integer);
    procedure AdjustSelection(Value: TfpgRect; Force: Boolean);
    procedure SetSelectArea(Value: TfpgRect);
    procedure SetGutterKind(Value: TGutterKind);
    procedure SetGutterWidth(Value: Integer);
    procedure SetGutterFont(const Value: string); 
    procedure SetGutterFontColor(Value: TfpgColor);
    procedure SetFooterFont(const Value: string);
    procedure SetFooterFontColor(Value: TfpgColor);
    function    CreateColumn: TfpgNiceColumn;
    procedure UpdateColumn(Index: Integer);
    procedure UpdateColumns;
    procedure UpdateHeader;

    function GetCellRect(x, y: Integer): TfpgRect;
    function CellRectToClient(R: TfpgRect): TfpgRect;
    function GetCellAtPos(X, Y: Integer): TPoint;
    function GetColFromX(X: Integer): Integer;
    function GetRowFromY(Y: Integer): Integer;
    function GetColCoord(I: Integer): Integer;
    function GetCell(X, Y: Integer): string;
    function SafeGetCell(X, Y: Integer): string;
    function GetCellColor(X, Y: Integer): TfpgColor;
    procedure DrawCell(X, Y: Integer);
    function FastDrawCell(X, Y: Integer): TPoint;
    procedure NormalizeVertOffset;

    function GetFirstVisible: Integer;
    function GetLastVisible: Integer;
    function GetNextVisible(Index: Integer): Integer;
    function GetPrevVisible(Index: Integer): Integer;
    procedure ColRowChanged;
    procedure SetGutterStrings(const Value: TStrings);
    function GetObject(X, Y: Integer): TObject;
    procedure SetObject(X, Y: Integer; const Value: TObject);
    procedure BuildMergeData;
    procedure DrawMergedCell(Index: Integer);
    procedure SetShowFooter(const Value: Boolean);
    procedure RenderFooter;
    procedure DrawFixCell(Rc: TfpgRect; Str: string; AFont: string;AFontColor: TfpgColor; AEvent: TOnDrawHeaderEvent);
    procedure SetEnabled(const Value: Boolean); reintroduce;

  protected
// *******************************************	  
    property VScrollBar: TfpgScrollBar read FVScrollBar write FVScrollBar;
    property HScrollBar: TfpgScrollBar read FHSCrollBar write FHScrollBar;	  
    procedure   HandleShow; override;
//*******************************************
  
    function GetMergedCellsData: TList;
    function GetHeaderInfo: TList;
    procedure SetScrollBar(AKind: TfpgScrollBar; AMax, APos, AStep: Integer); virtual;
    procedure ShowHideScrollBar(HorzVisible, VertVisible: Boolean); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Recalculate; virtual;
    procedure HandlePaint; override;
 //********************************* 
   procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
   procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);override;
   procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState);override;
   procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
   procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);override;
   procedure   HandleResize(awidth, aheight: TfpgCoord); override;
// ****************

  public
    ClientWidth: integer;
    ClientHeight: integer;  
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear;
    property Cells[X, Y: Integer]: string read GetCell write SetCell; default;
    property Objects[X, Y: Integer]: TObject read GetObject write SetObject;
    property ColWidths[Index: Integer]: Integer read GetColWidths write SetColWidths;
    procedure EnsureVisible(X, Y: Integer); overload;
    procedure CutToClipboard;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    function GetHitTestInfo(X, Y: Integer): TGridHitTest;
    function HeaderCellsCount: Integer;
    function HeaderCells(I: Integer): THeaderInfo;
    property Col: Integer read FCol write SetCol;
    property Row: Integer read FRow write SetRow;
    property SelectArea: TfpgRect read FSelectArea write SetSelectArea;
    procedure DeleteRow(ARow: Integer);
    procedure InsertRow(ARow: Integer);
    function AddRow: Integer;
    property HorzOffset: Integer read FHorzOffset write SetHorzOffset;
    property VertOffset: Integer read FVertOffset write SetVertOffset;
    function MergeCells(const X1, Y1, X2, Y2: Integer; ACaption: string): TfpgMergeCell;
    procedure ClearMergeCells;

  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property ColCount: Integer read GetColCount write SetColCount;
    property RowCount: Integer read FRowCount write SetRowCount default 5;
    property AutoAddRow: Boolean read FAutoAddRow write FAutoAddRow default False;
    property DefRowHeight: Integer read FDefRowHeight write SetDefRowHeight default 18;
    property DefColWidth: Integer read FDefColWidth write SetDefColWidth default 80;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Color: TfpgColor read FColor write SetColor default clGray;
    property AlternateColor: TfpgColor read FAlternateColor write SetAlternateColor default clGray;
    property GridColor: TfpgColor read FGridColor write SetGridColor default clButtonFace;
    property ShowGrid: Boolean read FShowGrid write SetShowGrid default True;
    property HeaderLine: Integer read FHeaderLine write SetHeaderLine default 1;
    property HeaderColor: TfpgColor read FHeaderColor write SetHeaderColor default clButtonFace;
    property HeaderLightColor: TfpgColor read FHeaderLightColor write SetHeaderLightColor default clHilite1; 
    property HeaderDarkColor: TfpgColor read FHeaderDarkColor write SetHeaderDarkColor default clHilite2; 
    property HeaderFont: string read FHeaderFont write SetHeaderFont;
    property HeaderFontColor: TfpgColor read FHeaderFontColor write SetHeaderFontColor;
    property FooterFont: string read FFooterFont write SetFooterFont;
    property FooterFontColor: TfpgColor read FFooterFontColor write SetFooterFontColor;
    property SelectionColor: TfpgColor read FSelectionColor write SetSelectionColor default $FFCAFFFF;
    property FitToWidth: Boolean read FFitToWidth write SetFitToWidth default False;
    property AutoColWidth: Boolean read FAutoColWidth write SetAutoColWidth default False;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Columns: TfpgNiceColumns read FColumns write SetColumns;
    property GutterKind: TGutterKind read FGutterKind write SetGutterKind default gkBlank;
    property GutterWidth: Integer read FGutterWidth write SetGutterWidth default 20;
    property GutterFont: string read FGutterFont write SetGutterFont;
    property GutterFontColor: TfpgColor read FGutterFontColor write SetGutterFontColor;
    property GutterStrings: TStrings read FGutterStrings write SetGutterStrings;
    property ShowFooter: Boolean read FShowFooter write SetShowFooter;
    property OnDrawCell: TOnDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnDrawHeader: TOnDrawHeaderEvent read FOnDrawHeader write FOnDrawHeader;
    property OnDrawGutter: TOnDrawHeaderEvent read FOnDrawGutter write FOnDrawGutter;
    property OnDrawFooter: TOnDrawHeaderEvent read FOnDrawFooter write FOnDrawFooter;
    property OnHeaderClick: TOnHeaderClick read FOnHeaderClick write FOnHeaderClick;
    property OnGutterClick: TOnGutterClick read FOnGutterClick write FOnGutterClick;
    property OnCellChange: TOnCellChange read FOnCellChange write FOnCellChange;
    property OnCellChanging: TOnCellChanging read FOnCellChanging write FOnCellChanging;
    property OnColRowChanged: TOnColRowChanged read FOnColRowChanged write FOnColRowChanged;
    property OnInsertRow: TOnRowEvent read FOnInsertRow write FOnInsertRow;
    property OnDeleteRow: TOnRowEvent read FOnDeleteRow write FOnDeleteRow;
    property OnCellAssignment: TOnCellAssignment read FOnCellAssignment write FOnCellAssignment;
    property Font;
    property Anchors;
    property Align;
    property BorderStyle default bsSingle;
    property TabOrder;
    property Tag;
    property OnClick;
    property OnDoubleClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyPress;
  end;

  TfpgNiceGridSync = class(TfpgNiceGrid)
  private
    FMasterGrid: TfpgNiceGrid;
    procedure SetMasterGrid(const Value: TfpgNiceGrid);
    procedure SyncDeleteRow(Sender: TObject; ARow: Integer);
    procedure SyncInsertRow(Sender: TObject; ARow: Integer);
    procedure SyncColRow(Sender: TObject; ACol, ARow: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetScrollBar(AKind: TfpgScrollBar; AMax, APos,AStep: Integer); override;
    procedure ShowHideScrollBar(HorzVisible, VertVisible: Boolean); override;
    property OnDeleteRow;
    property OnInsertRow;
    property OnColRowChanged;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MasterGrid: TfpgNiceGrid read FMasterGrid write SetMasterGrid;
  end;


  function DrawStringUni(Canvas: TfpgCanvas; Str: string; Rc: TfpgRect;
    HorzAlign: THorzAlign; VertAlign: TVertAlign): TPoint;

  procedure DrawStringMulti(Canvas: TfpgCanvas; Str: string; Rc: TfpgRect;
    HorzAlign: THorzAlign; VertAlign: TVertAlign);


implementation

uses
  Math;

const
 { crPlus = 101;
  crSmallCross = 102;
  crRight = 103;
  crDown = 104;
  crLeftTop = 105;}

  CursorArray: array [TGridHitTest] of TMouseCursor =
  //(gtNone, gtLeftTop, gtLeft, gtTop, gtCell, gtColSizing, gtSmallBox);
 //   (crDefault, crLeftTop, crRight, crDown, crPlus, crHSplit, crSmallCross);
    (mcDefault,mcSizeNWSE ,mcSizeEW,mcSizeNS, mcCross,mcMove,mcHand);
  
  MergeID = -2;  
 
  
{ TfpgNiceGrid }

constructor TfpgNiceGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FisContainer:=true;
  Width := 200;
  Height := 200;
  BorderStyle := bsSingle;
  Text:='';
  FFlat := True;
  FEnabled := True;
  FColor := clWindowBackground;
  FAlternateColor := clWindowBackground;
  FGridColor := clButtonFace;
  FShowGrid := True;
  FHeaderColor := clButtonface;
  FHeaderLightColor := clHilite1; 
  FHeaderDarkColor := clShadow1;
  FHeaderFont:='Arial-8';
  FSelectionColor := $FFCAFFFF;
  FFooterFont := 'Arial-8';
  FooterFontColor := clRed;
  FDefRowHeight := 18;
  FDefColWidth := 60;
  FRowCount := 5;
  FAutoAddRow := False;
  FGutterKind := gkBlank;
  FGutterWidth := 20;
  FGutterFont:='Arial-8';
  FGutterFontColor:=clBlack; 

  FHorzOffset  := 0;
  FVertOffset  := 0;
  FMaxHScroll  := 0;
  FMaxVScroll  := 0;
  FSmallChange := FDefRowHeight;
  FLargeChange := FDefRowHeight * 5;
  ForcedColumn := -1;
  AllWidth := 200;
  AllHeight := 200;
  ClientWidth:=Width -2;
  ClientHeight:=Height -2;
  
  FHeaderLine := 1;
  FHeaderInfos := TList.Create;

  CellBox:=fpgRect(0, 0, 0, 0);
  FCol := 0;
  FRow := 0;
  FCol2 := 0;
  FRow2 := 0;
  FSelectArea:=fpgRect(0, 0, 0, 0);
  IsEditing := False;
  BuffString := '';
  SmallBox := fpgRect(-1, -1,0, 0);
  SmallBoxArea:=fpgRect(-1, -1, 0,0);
  SmallBoxPos := 0;
  SizingCol := -1;
  SizingColX := -1;
  MouseCursor := mcCross;
  FColumns := TfpgNiceColumns.Create(Self,TfpgNiceColumn);  
  FEdit := TfpgNiceInplace.Create(Self);
  
  FGutterStrings := TStringList.Create;
  Mergeds := TList.Create;
  Sync:=nil;  
// ************************
  FVScrollBar := TfpgScrollBar.Create(self);
  FVScrollBar.Orientation := orVertical;
  FVScrollBar.Visible     := false;
  FVScrollBar.ScrollStep:= FDefRowHeight;
  FVScrollBar.OnScroll:=@VScrollBarMove;
  
  FHScrollBar := TfpgScrollBar.Create(self);
  FHScrollBar.Orientation := orHorizontal;
  FHScrollBar.Visible     := false;
  FHScrollBar.ScrollStep  := 5;
  FHScrollBar.OnScroll:=@HScrollBarMove;
//*******************************

end;

destructor TfpgNiceGrid.Destroy;
begin
  ClearMergeCells;
  Mergeds.Free;
  FGutterStrings.Free;
  FEdit.Free;
  FColumns.Free;
  ClearHeaderInfos;
  FHeaderInfos.Free;
  inherited Destroy;
end;

procedure TfpgNiceGrid.SetScrollBar(AKind: TfpgScrollbar; AMax, APos, AStep: Integer);
begin
  with AKind do
  begin	  
    Min:=0;
    Max:=AMax;
    ScrollStep:=AStep;
    Position:=APos;
    RepaintSlider;  
  end;
  if (AKind = FVScrollBar) and Assigned(Sync) then
  begin
    Sync.FMaxVScroll := AMax;
    Sync.VertOffset := APos;
  end;
end;

procedure TfpgNiceGrid.ShowHideScrollBar(HorzVisible, VertVisible: Boolean);
begin
  FVScrollBar.Visible:= VertVisible;
  FHScrollBar.Visible:=  HorzVisible;
end;

procedure TfpgNiceGrid.HScrollBarMove(Sender: TObject; position: integer);
begin
  if FHorzOffset <> position then
  begin
    if Position < 0 then
       Position := 0;
    FHorzOffset:= position;
    Invalidate;
  end;
end;

procedure TfpgNiceGrid.VScrollBarMove(Sender: TObject; position: integer);
begin
  FVertOffset :=  position; 
  FVertOffset := Max(0, Min(FMaxVScroll, FVertOffset));
  NormalizeVertOffset;
  Invalidate;
end;

procedure TfpgNiceGrid.HandleShow;
begin
  inherited HandleShow;
  if (csLoading in ComponentState) then
    Exit;
  UpdateScrollBars;
end;
  
procedure TfpgNiceGrid.SetColCount(Value: Integer);
begin
  if (ColCount <> Value) then
  begin
    FColumns.BeginUpdate;
    while (ColCount > Value)
      do FColumns.Delete(FColumns.Count-1);
    while (ColCount < Value)
      do FColumns.Add;
    FHorzOffset := 0;
    FVertOffset := 0;
    FCol := Max(0, Min(FCol, ColCount-1));
    FRow := Max(0, Min(FRow, FRowCount-1));
    if (FRowCount = 0) or (ColCount = 0) then
    begin
      FCol := -1;
      FRow := -1;
    end;
    FSelectArea:=fpgRect(FCol, FRow, FCol, FRow);
    FColumns.EndUpdate;
    ColRowChanged;
  end;
end;

procedure TfpgNiceGrid.SetRowCount(Value: Integer);
begin
  if (FRowCount <> Value) then
  begin
    FRowCount := Value;
    FCol := Max(0, Min(FCol, ColCount-1));
    FRow := Max(0, Min(FRow, FRowCount-1));
    if (FRowCount = 0) or (ColCount = 0) then
    begin
      FCol := -1;
      FRow := -1;
    end;
    FSelectArea:=fpgRect(FCol, FRow, FCol, FRow);
    Recalculate;
    Invalidate;
    UpdateScrollBars;
    ColRowChanged;
  end;
end;

procedure TfpgNiceGrid.ClearHeaderInfos;
var
  x: Integer;
  P: PHeaderInfo;
begin
  for x := 0 to FHeaderInfos.Count-1 do
  begin
    P := PHeaderInfo(FHeaderInfos[x]);
    Dispose(P);
  end;
  FHeaderInfos.Clear;
end;

procedure TfpgNiceGrid.Recalculate;
var
  x: Integer;
  HVisible, VVisible: Boolean;
  VisCount: Integer;
  WidthAvail, HeightAvail: Integer;
  v: Integer;
  LastBodyWidth: Integer;
  
  function GetColAutoWidth(i: Integer): Integer;
  var
    n: Integer;
    t: TStrings;
  begin
    Result := 0;
    t := Columns[i].FStrings;
    for n := 0 to t.Count-1
      do Result := Max(Result, Canvas.Font.TextWidth(t[n]) + 7);
    Result := Max(Result, 20);
  end;

begin

  BuildMergeData;

  VisCount := 0;
  for x := 0 to FColumns.Count-1 do
  begin
    if FColumns[x].FVisible
      then Inc(VisCount);
  end;

  if (VisCount = 0) then
  begin
    FixedHeight := 0;
    FixedWidth := 0;
    BodyWidth := 0;
    BodyHeight := 0;
    ShowHideScrollBar(False, False);
    Exit;
  end;

  ClientHeight:= Height-2;
  ClientWidth:= Width-2;
  
  if FAutoColWidth then
  begin
    for x := 0 to FColumns.Count-1
      do FColumns[x].FWidth := Max(FDefColWidth, GetColAutoWidth(x));
  end;

  FixedWidth := 0;
  if (FGutterKind <> gkNone)
    then FixedWidth := FGutterWidth;

  FixedHeight := FHeaderLine * FDefRowHeight;
  BodyHeight  := FRowCount * FDefRowHeight;

  WidthAvail := ClientWidth - FixedWidth;
  HeightAvail := ClientHeight - FixedHeight;
  if FShowFooter then
    HeightAvail := HeightAvail - FDefRowHeight;   
  
  BodyWidth := 0;
  for x := 0 to FColumns.Count-1 do
  begin
    if FColumns[x].FVisible
      then BodyWidth := BodyWidth + FColumns[x].FWidth;
  end;

  if FFitToWidth then
  begin
    if (BodyWidth < WidthAvail) then
    begin
      LastBodyWidth := BodyWidth;
      x := 0;
      while (BodyWidth < WidthAvail) do
      begin
        if (x > ColCount-1) then
        begin
          if (BodyWidth = LastBodyWidth)
            then Break
            else x := 0;
        end;
        if FColumns[x].FVisible and FColumns[x].FCanResize then
        begin
          FColumns[x].FWidth := FColumns[x].FWidth + 1;
          Inc(BodyWidth);
        end;
        Inc(x);
      end;
    end;
    if (BodyWidth > WidthAvail) then
    begin
      LastBodyWidth := BodyWidth;
      x := 0;
      while (BodyWidth > WidthAvail) do
      begin
        if (x > ColCount-1) then
        begin
          if (BodyWidth = LastBodyWidth)
            then Break
            else x := 0;
        end;  
        if FColumns[x].FVisible and (x <> ForcedColumn) and FColumns[x].FCanResize then 
        begin
          FColumns[x].FWidth := FColumns[x].FWidth - 1;
          Dec(BodyWidth);
        end;
        Inc(x);
      end;
    end;
    ForcedColumn := -1;
  end;

  if (BodyWidth < WidthAvail)
    then FHorzOffset := 0;

  if (BodyHeight < HeightAvail)
    then FVertOffset := 0;

  HVisible := BodyWidth > WidthAvail;
  VVisible := BodyHeight > HeightAvail;

  ShowHideScrollBar(HVisible, VVisible);
    
  if FHScrollBar.Visible then
	dec(ClientHeight,FHScrollBar.Height);  
  if FVScrollBar.Visible then
	dec(ClientWidth,FVScrollBar.Width); 
      
  FMaxHScroll := Max(0, BodyWidth - ClientWidth + FixedWidth);
    
  if FShowFooter
    then FMaxVScroll := Max(0, BodyHeight - ClientHeight + FixedHeight + FDefRowHeight)
    else FMaxVScroll := Max(0, BodyHeight - ClientHeight + FixedHeight);

  // Align to FDefRowHeight
  v := FMaxVScroll div FDefRowHeight;
  if (FMaxVScroll mod FDefRowHeight) > 0
    then Inc(v);
  FMaxVScroll := v * FDefRowHeight;
 
  if FShowFooter then
  begin
    if VVisible then 
      FooterTop := (((ClientHeight div FDefRowHeight) - 1) * FDefRowHeight) - 1
    else FooterTop := (FDefRowHeight * (FHeaderLine + FRowCount)) - 1;
  end;

  FHorzOffset := Max(0, Min(FHorzOffset, FMaxHScroll));
  FVertOffset := Max(0, Min(FVertOffset, FMaxVScroll));
  
  SetScrollBar(FHScrollBar, FMaxHScroll, FHorzOffset, 1);
  SetScrollBar(FVScrollBar, FMaxVScroll, FVertOffset, FDefRowHeight);

  AllWidth := Min(ClientWidth, BodyWidth + FixedWidth);
  if FShowFooter then
  begin
    AllHeight := Min(ClientHeight, BodyHeight + FixedHeight + FDefRowHeight);
    CellBox:=fpgRect(FixedWidth, FixedHeight, ClientWidth, FooterTop);
  end else
  begin
    AllHeight := Min(ClientHeight, BodyHeight + FixedHeight);
    CellBox:=fpgRect(FixedWidth, FixedHeight, ClientWidth, ClientHeight);
  end;
end;

function DrawStringUni(Canvas: TfpgCanvas; Str: string; Rc: TfpgRect;
  HorzAlign: THorzAlign; VertAlign: TVertAlign): TPoint;
var
  w, h, x, y: Integer;
  rw: Integer;
begin
  w := Canvas.Font.TextWidth(Str);
  h := Canvas.Font.Height;
  x := 0;
  y := 0;
  rw := Rc.Right - rc.Left;
  case HorzAlign of
    haLeft: x := Rc.Left;
    haCenter: x := Rc.Left + ((rw - w) div 2);
    haRight:  x := Rc.Right - w;
  end;
  case VertAlign of
    vaTop:    y := Rc.Top;
    vaCenter: y := Rc.Top + (((Rc.Bottom - Rc.Top) - h) div 2);
    vaBottom: y := Rc.Bottom - h;
  end;
  Canvas.DrawString(x, y, Str);
  // Return next cursor position
  Result := Point(Min(x + w + 1, Rc.Right), Rc.Top - 1);
end;

procedure DrawStringMulti(Canvas: TfpgCanvas; Str: string; Rc: TfpgRect;
  HorzAlign: THorzAlign; VertAlign: TVertAlign);
var
  w, h, x, y: Integer;
  t: TStringList;
  i: Integer;
  dh: Integer;

begin
  if Pos(';', Str) = 0 then
  begin
    DrawStringUni(Canvas, Str, Rc, HorzAlign, VertAlign);
    Exit;
  end;

  t := TStringList.Create;
  t.Text := StringReplace(Str, ';', #13, [rfReplaceAll]);
  h := Canvas.Font.Height;
  dh := Rc.Top + (((Rc.Bottom - Rc.Top) - (h * t.Count)) div 2);
  for i := 0 to t.Count-1 do
  begin
    w := Canvas.Font.TextWidth(t[i]);
    x := 0;
    y := 0;
    case HorzAlign of
      haLeft:   x := Rc.Left;
      haCenter: x := Rc.Left + (((Rc.Right - Rc.Left) - w) div 2);
      haRight:  x := Rc.Right - w;
    end;
    case VertAlign of
      vaTop:    y := Rc.Top + (i * h);
      vaCenter: y := dh + (i * h);
      vaBottom: y := Rc.Bottom - (h * (t.Count-i));
    end;
     Canvas.DrawString(x, y, t[i]); 
  end;
  t.Free;
end;

Function PtInSelectArea(ARect: TfpgRect; P: TPoint): boolean;
begin
  Result:=(p.y >= ARect.Top) and
             (p.y  <= ARect.Height) and
             (p.x  >= ARect.Left) and
             (p.x  <= ARect.Width);   
end;
   
function TfpgNiceGrid.GetCellColor(X, Y: Integer): TfpgColor;
var
  cl: TfpgColor;
  R: TfpgRect;
begin
  cl := FColumns[x].Color;
  if FEnabled then
  begin
    with FSelectArea
      do R:=fpgRect(Left, Top, Width, Height);
	      
    if PtInSelectArea(R, Point(X, Y)) then
    begin
      if not ((X = FCol) and (y = FRow))
        then cl := FSelectionColor;
    end;
  end;
  Result := cl;
end;

procedure TfpgNiceGrid.DrawFixCell(Rc: TfpgRect; Str: string; AFont: string;AFontColor: TfpgColor; AEvent: TOnDrawHeaderEvent);
var
  Rt: TfpgRect;
  Handled: Boolean;
begin
  Handled := False;
  with Canvas do
  begin
    Font := fpgGetFont(AFont);
    if not FEnabled then
      SetTextColor(FHeaderDarkColor)
    else SetTextColor(AFontColor);	      
	      
    if Assigned(AEvent)
      then AEvent(Self, Canvas, Rc, Str, Handled);
    if Handled
      then Exit;
	      
    SetColor(FHeaderColor);	      
    FillRectangle(Rc); 
    SetColor(FHeaderDarkColor);  
    DrawRectangle(Rc);
      
    // Draw text immediately
    Rt := fpgRect(Rc.Left + 2, Rc.Top + 2, Rc.Width - 4, Rc.Height - 4);  
    DrawStringMulti(Canvas, Str, Rt, haCenter, vaCenter);
      
    // cosmetics
    SetColor(FHeaderLightColor);
    DrawLine(Rc.Left + 1, Rc.Bottom - 2, Rc.Left + 1, Rc.Top + 1);
    DrawLine(Rc.Left + 1, Rc.Top + 1, Rc.Right - 1, Rc.Top + 1);     
      
    if not FFlat then
    begin
      SetColor(clBlack);
      DrawLine(Rc.Right - 2, Rc.Top + 1,Rc.Right - 2, Rc.Bottom - 2);
      DrawLine(Rc.Right - 2, Rc.Bottom - 2,Rc.Left, Rc.Bottom - 2);
    end;
  end;
end;
    
procedure TfpgNiceGrid.RenderGutter;
const
  ArrowWidth = 8;
var
  x: Integer;
  R, Dummy: TfpgRect;
  Str: string;
  l, t, m: Integer;
  GutterBox: TfpgRect;
begin
  if (FGutterKind = gkNone)
    then Exit;
  CopyRect(GutterBox, CellBox);
  GutterBox.Left := 0;
  for x := 0 to FRowCount-1 do
  begin
    R := fpgRect(-1, (x * FDefRowHeight) -1, FGutterWidth, FDefRowHeight +1);
    OffsetRect(R, 2, -FVertOffset + FixedHeight);
    if IntersectRect(Dummy, R, GutterBox) then
    begin
      case FGutterKind of
        gkBlank, gkPointer:
          Str := '';
        gkNumber:
          Str := IntToStr(x + 1);
        gkString:
          if (x > FGutterStrings.Count-1)
            then Str := ''
            else Str := FGutterStrings[x];
      end;
      DrawFixCell(R, Str, FGutterFont, FGutterFontColor, FOnDrawGutter);
      // Draw pointer triangle
      if (FGutterKind = gkpointer) and (x = FRow) then
      begin
        with Canvas do
        begin
          l := (FGutterWidth - ArrowWidth) div 2;
          t := (FDefRowHeight - ArrowWidth) div 2;
          m := R.Top + (FDefRowHeight div 2);
          SetColor(FHeaderDarkColor);
	  DrawLine(l, R.Bottom - t,l, R.Top + t);
	  DrawLine(l, R.Top + t,l + ArrowWidth, m);
          SetColor(FHeaderLightColor);
	  DrawLine(l + ArrowWidth, m,l, R.Bottom - t);
        end;
      end;
    end;
  end;
end;

procedure TfpgNiceGrid.RenderHeader;
var
  x: Integer;
  R, Dummy: TfpgRect;
  P: PHeaderInfo;
begin
  for x := 0 to FHeaderInfos.Count-1 do
  begin
    P := PHeaderInfo(FHeaderInfos[x]);
    R := fpgRect(
           GetColCoord(P^.Rc.Left)-1 ,
	   FDefRowHeight * P^.Rc.Top, 
           GetColCoord(P^.Rc.Width+1) - GetColCoord(P^.Rc.Left)+1,
           FDefRowHeight * (P^.Rc.Height+1)+1
                     );
    OffsetRect(R, -FHorzOffset + FixedWidth, 0);
    if IntersectRect(Dummy, R, fpgRect(1,1,ClientWidth,ClientHeight))
      then DrawFixCell(R, P^.Str, FHeaderFont, FHeaderFontColor, FOnDrawHeader);
  end;
  R := fpgRect(1,1, FixedWidth, FixedHeight );
  DrawFixCell(R, '', FHeaderFont, FHeaderFontColor, FOnDrawHeader);
end;

procedure TfpgNiceGrid.RenderFooter;
var
  x: Integer;
  R, Dummy: TfpgRect;
  FooterBottom: Integer;
  ARight: Integer;
begin
  FooterBottom := FooterTop + FDefRowHeight+1;
  for x := 0 to FColumns.Count-1 do
  begin
    R := fpgRect(GetColCoord(x)-1,
                      FooterTop,
                      GetColCoord(x+1)-GetColCoord(x)+1,
                      FooterBottom-FooterTop+1
                      ); 
    OffsetRect(R, -FHorzOffset + FixedWidth, 0);
  
    if IntersectRect(Dummy, R, fpgRect(1,1,ClientWidth,ClientHeight))
      then DrawFixCell(R, FColumns[x].FFooter, FFooterFont, FFooterFontColor, FOnDrawFooter);
  end;
  R := fpgRect(1, FooterTop, FixedWidth, FooterBottom-FooterTop);
  DrawFixCell(R, '', FFooterFont, FFooterFontColor, FOnDrawFooter);
  ARight := Min(AllWidth, ClientWidth);
  
  R := fpgRect(1, FooterBottom, ARight-1, ClientHeight-FooterBottom+1);
  DrawFixCell(R, '', FFooterFont, FFooterFontColor, FOnDrawFooter);
end;

procedure TfpgNiceGrid.DrawCell(X, Y: Integer);
var
  Rc, Dummy: TfpgRect;
  Column: TfpgNiceColumn;
  Handled: Boolean;
begin
  Handled := False;
  Rc := GetCellRect(x, y);
  OffsetRect(Rc, -FHorzOffset + FixedWidth, -FVertOffset + FixedHeight);
  if IntersectRect(Dummy, Rc, CellBox) then
  begin
    Column := FColumns[x];
    with Canvas do
    begin
      Font:=fpgGetFont(Column.Font);
      SetTextColor(Column.FontColor);
    
      if not FEnabled then
        SetTextColor(FGridColor);
		
      SetColor(GetCellColor(X, Y)); 
      if Assigned(FOnDrawCell)
        then FOnDrawCell(Self, Canvas, X, Y, Rc, Handled);

      if not Handled then
      begin
        FillRectangle(Rc);
        if FShowGrid then
        begin
          SetColor(FGridColor);
          inc(Rc.Width,1);
          inc(Rc.Height,1);
          DrawRectangle(Rc);
	      end;
        InflateRect(Rc, -4, -2);
        DrawStringUni(Canvas, SafeGetCell(x, y), Rc, Column.HorzAlign, Column.VertAlign);
      end;
    end;
  end;
end;

function TfpgNiceGrid.FastDrawCell(X, Y: Integer): TPoint;
var
  R, Dummy: TfpgRect;
  Handled: Boolean;
  Column: TfpgNiceColumn;
begin
  Handled := False;          
  Result := Point(-1, -1);
  R := GetCellRect(x, y);
  OffsetRect(R, -FHorzOffset + FixedWidth, -FVertOffset + FixedHeight);
  if IntersectRect(Dummy, R, CellBox) then
  begin
    Column := FColumns[x];
    with Canvas do
    begin
      Font:=fpgGetFont(Column.Font);
    end;
    if Assigned(FOnDrawCell)
      then FOnDrawCell(Self, Canvas, X, Y, R, Handled);
    if not Handled then
    begin
      with Canvas do
      begin
        InflateRect(R, -4, -2);
        FillRectangle(R);
      end;
      Result := DrawStringUni(Canvas, SafeGetCell(x, y), R, Column.HorzAlign,
        Column.VertAlign);
    end;
  end;
end;

procedure TfpgNiceGrid.DrawSelection;
var
  R, R1, R2: TfpgRect;
  HOffset, VOffset: Integer;

begin

  if (FCol = -1) or (FRow = -1)
    then Exit;
    
  HOffset := - FHorzOffset + FixedWidth;
  VOffset := - FVertOffset + FixedHeight;
  R1 := GetCellRect(FSelectArea.Left, FSelectArea.Top);
  R2 := GetCellRect(FSelectArea.Width, FSelectArea.Height);
  R := fpgRect(R1.Left+2, R1.Top+2, R2.Right - R1.Left, R2.Bottom - R1.Top); 
  OffsetRect(R, HOffset, VOffset);

  with Canvas do
  begin
    if Focused then 
      SetColor(clBlack)
    else
      SetColor(FGridColor);	    
    SetLineStyle(2, lsDash);
    DrawRectangle(R);
    SetLineStyle(1, lsSolid);
    SetColor(clRed); 
    case SmallBoxPos of
      0: SmallBox := fpgRect(R.Right - 3, R.Bottom - 3, 6,6);
      1: SmallBox := fpgRect(R.Right - 3, R.Top + 2, 6,8);
      2: SmallBox := fpgRect(R.Left - 3 + 5, R.Bottom - 3,8,3); 
    end;

    FillRectangle(SmallBox);
    SmallBoxPos := 0;  // Reset to Right Bottom
  end;
end;

procedure TfpgNiceGrid.ClearUnused;
var
  t: Integer;
begin
  if (AllWidth < ClientWidth) then
  begin
    with Canvas do
    begin
      SetColor(FColor);
      FillRectangle(fpgRect(AllWidth, 0, ClientWidth, ClientHeight));
    end;
  end;
  if FShowFooter
    then Exit;
  if (AllHeight < ClientHeight) then
  begin
    with Canvas do
    begin
      SetColor(FColor);
      FillRectangle(fpgRect(0, AllHeight, ClientWidth, ClientHeight));
    end;
  end;
  if ((FMaxVScroll - FVertOffset) < FDefRowHeight) then
  begin
    with Canvas do
    begin
      SetColor(FColor);
      t := FixedHeight + (((ClientHeight - FixedHeight) div FDefRowHeight) * FDefRowHeight);
      FillRectangle(fpgRect(0, t, ClientWidth, ClientHeight));
    end;
  end;
end;

procedure TfpgNiceGrid.HandlePaint;
var
  x, y: Integer;
  R1: TfpgRect;
begin
  if FUpdating then Exit;
  if not (HasHandle) then Exit;
  Canvas.Setcolor(FColor);
  Canvas.FillRectangle(fpgRect(0, 0, Width, Height));

  if (FRowCount > 0) then
  begin
    for x := 0 to ColCount-1 do
    begin
      if FColumns[x].FVisible then
      begin
        for y := 0 to FRowCount-1 do
        begin
          if (GetObject(x, y) <> TObject(MergeID)) then
	          DrawCell(X, Y);
        end;
      end;           
   end;
   for x := 0 to Mergeds.Count-1 do
      DrawMergedCell(x);
    if FEnabled then
      DrawSelection;
  end
  else
    ClearUnused;
  RenderGutter;
  RenderHeader;
  if FShowFooter then RenderFooter;
  // The little square in the bottom right corner
  if FHScrollBar.Visible and FVScrollBar.Visible then
  begin
    Canvas.ClearClipRect;
    Canvas.SetColor(clButtonFace);
    R1:=fpgRect(HScrollBar.Left+FHScrollBar.Width,
                      FVScrollBar.Top+FVScrollBar.Height,
                      FVScrollBar.Width,
                      FHScrollBar.Height);
    Canvas.FillRectangle(R1);
    SetColor(FHeaderLightColor);
    Canvas.DrawLine(R1.Left + 1, R1.Bottom+1, R1.Left + 1, R1.Top + 1);
    Canvas.DrawLine(R1.Left + 1, R1.Top + 1, R1.Right - 1, R1.Top + 1); 
  end;
  Canvas.Setcolor(clBlack);
  Canvas.DrawRectangle(fpgRect(0, 0, Width, Height));  
end;

procedure TfpgNiceGrid.UpdateHeader;
var
  P: PHeaderInfo;
  x, y: Integer;
  t: TStringList;
  s: string;
  LastX: TList;
  LastY: PHeaderInfo;
  Blank: PHeaderInfo;

begin
  ClearHeaderInfos;

  LastX := TList.Create;
  t := TStringList.Create;

  Blank := New(PHeaderInfo);
  Blank^.Str := '^%%%%%^******^';
  Blank^.Rc:=fpgRect(0,0,0,0);
  
  while (LastX.Count < FHeaderLine)
    do LastX.Add(Blank);

  P := nil;
  for x := 0 to FColumns.Count-1 do
  begin
    if not FColumns[x].FVisible then
    begin
      for y := 0 to FHeaderLine-1
        do LastX[y] := Blank;
      Continue;
    end;
    t.Text := StringReplace(FColumns[x].Title, '|', #13, [rfReplaceAll]);
    while (t.Count < FHeaderLine) do
    begin
      if (t.Count = 0)
        then t.Add('')
        else t.Add( t[t.Count-1]);
    end;
    LastY := Blank;
    for y := 0 to FHeaderLine-1 do
    begin
      s := t[y];
      if (s = LastY^.Str) then
      begin
        LastY^.Rc.Height := Min(FHeaderLine-1, Max(LastY^.Rc.Height, y)); 
      end
      else
      begin
        if (s = PHeaderInfo(LastX[y])^.Str) then
        begin
          P := PHeaderInfo(LastX[y]);
          P^.Rc.Width := P^.Rc.Width + 1; 
        end
	else
        begin
          P := New(PHeaderInfo);
          P^.Rc := fpgRect(x, y, x,0);
          P^.Str := s;
          FHeaderInfos.Add(P);
        end;
        LastX[y] := P;
      end;
      LastY := P;
    end;
  end;

  LastX.Free;
  t.Free;
  Dispose(Blank);
  Recalculate;	
end;

function TfpgNiceGrid.GetColCoord(I: Integer): Integer;
var
  x: Integer;
  Column: TfpgNiceColumn;
begin
  Result := 0;
  for x := 0 to I-1 do  
  begin
    Column := FColumns[x];
    if Column.FVisible
      then Result := Result + Column.FWidth;
  end;  
end;

function TfpgNiceGrid.GetCellRect(x, y: Integer): TfpgRect;
var
  l, t, w, h: Integer;
begin
  if (x = -1) or (y = -1) then 
  begin
    Result := fpgRect(0, 0, 0, 0);
    Exit;
  end;
  l := GetColCoord(x);
  t := FDefRowheight * y;
  w := 0;
  if (FColumns[x].FVisible)
    then w := FColumns[x].FWidth;
  h := FDefRowHeight;
  Result := fpgRect(l-1, t-1, w, h);
end;

function TfpgNiceGrid.CellRectToClient(R: TfpgRect): TfpgRect;
begin
  Result := R;
  OffsetRect(Result, - FHorzOffset + FixedWidth, - FVertOffset + FixedHeight);
end;

function TfpgNiceGrid.GetCellAtPos(X, Y: Integer): TPoint;
var
  ax, ay: Integer;
begin
  ax := (FHorzOffset + X) - FixedWidth;
  ay := (FVertOffset + Y) - FixedHeight;
  Result.X := 0;
  while (GetColCoord(Result.X) < ax) do
  begin
    Result.X := Result.X + 1;
    if (Result.X > FColumns.Count-1)
      then Break;
  end;
  Result.X := Max(0, Result.X - 1);
  Result.Y := Max(0, Min(ay div FDefRowHeight, FRowCount-1));
end;

function TfpgNiceGrid.GetColFromX(X: Integer): Integer;
var
  ax: Integer;
begin
  if (X < FixedWidth) then
  begin
    Result := -1;
    Exit;
  end;
  Result := 0;
  ax := (FHorzOffset + X) - FixedWidth;
  while (GetColCoord(Result) < ax) do
  begin
    Result := Result + 1;
    if (Result > FColumns.Count-1)
      then Break;
  end;
  Result := Result - 1;
  if (Result > FColumns.Count-1) or (Result < 0)
    then Result := -1;
end;

function TfpgNiceGrid.GetRowFromY(Y: Integer): Integer;
var
  ay: Integer;
begin
  if (Y < FixedHeight) then
  begin
    Result := -1;
    Exit;
  end;
  ay := (FVertOffset + Y) - FixedHeight;
  Result := ay div FDefRowHeight;
  if (Result > FRowCount-1)
    then Result := -1;
end;

function TfpgNiceGrid.SafeGetCell(X, Y: Integer): string;
var
  t: TStringList;
begin
  Result := '';
  t := TStringList(Columns[X].FStrings);
  if (Y < t.Count)
    then Result := t[Y];
end;

function TfpgNiceGrid.GetCell(X, Y: Integer): string;
var
  t: TStrings;
begin
  Result := '';
  if (X > ColCount-1) or (Y > FRowCount-1)
    then raise Exception.Create('Cell Index out of bound.');
  t := Columns[X].FStrings;
  if (Y < t.Count)
    then Result := t[Y];
end;

procedure TfpgNiceGrid.InternalSetCell(X, Y: Integer; Value: string;
  FireOnChange: Boolean);
var
  t: TStringList;
  s: string;
  CanChange: Boolean;
begin
  if (ColCount = 0) or (FRowCount = 0)
    then Exit;
  if FireOnChange and FColumns[X].FReadOnly
    then Exit;
  if (X > ColCount-1) or (Y > FRowCount-1)
    then raise Exception.Create('Cell Index out of bound.');
  t := TStringList(FColumns[X].FStrings);
  while (Y > t.Count-1)
    do t.Add('');
  if (t[Y] = Value)
    then Exit;  
  if FireOnChange then
  begin
    s := Value;
    CanChange := True;
    if Assigned(FOnCellChanging)
      then FOnCellChanging(Self, X, Y, CanChange);
    if not CanChange
      then Exit;
    if Assigned(FOnCellChange)
      then FOnCellChange(Self, X, Y, s);
    t[Y] := s;
  end else
    t[Y] := Value;
  if (not FUpdating) and (not IsEditing)  then
    FastDrawCell(X, Y);
end;

procedure TfpgNiceGrid.SetCell(X, Y: Integer; Value: string);
begin
  InternalSetCell(X, Y, Value, False);
end;

procedure TfpgNiceGrid.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TfpgNiceGrid.EndUpdate;
begin
  FUpdating := False;
  UpdateHeader;
  Invalidate;
end;

procedure TfpgNiceGrid.SetFlat(Value: Boolean);
begin
  if (FFlat <> Value) then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TfpgNiceGrid.SetColor(Value: TfpgColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TfpgNiceGrid.SetAlternateColor(Value: TfpgColor);
begin
  if (FAlternateColor <> Value) then
  begin
    FAlternateColor := Value;
    Invalidate; 	    
  end;
end;

procedure TfpgNiceGrid.SetGridColor(Value: TfpgColor);
begin
  if (FGridColor <> Value) then
  begin
    FGridColor := Value;
    Invalidate;
  end;
end;

function TfpgNiceGrid.GetColWidths(Idx: Integer): Integer;
begin
  Result := FColumns[Idx].FWidth;
end;

procedure TfpgNiceGrid.SetColWidths(Idx, Value: Integer);
begin
  if not FAutoColWidth then
  begin
    if (ColWidths[Idx] <> Value)
      then FColumns[Idx].Width := Value;
  end;
end;

procedure TfpgNiceGrid.SetAutoColWidth(Value: Boolean);
begin
  if (FAutoColWidth <> Value) then
  begin
    FAutoColWidth := Value;
    Recalculate;
    Invalidate;
    UpdateScrollbars;
  end;
end;

procedure TfpgNiceGrid.SetDefColWidth(Value: Integer);
begin
  if (FDefColWidth <> Value) then
  begin
    FDefColWidth := Value;
    if not FAutoColWidth then
    begin
      Recalculate;
      Invalidate;
    end;
  end;
end;

procedure TfpgNiceGrid.SetDefRowHeight(Value: Integer);
begin
  if (FDefRowHeight <> Value) then
  begin
    FDefRowHeight := Value;
    FSmallChange := Value;
    FLargeChange := Value * 5;
    Recalculate;
    Invalidate;
  end;
end;

procedure TfpgNiceGrid.SetFitToWidth(Value: Boolean);
begin
  if (FFitToWidth <> Value) then
  begin
    FFitToWidth := Value;
    FHorzOffset := 0;
    Recalculate;
    Invalidate;
  end;
end;

procedure TfpgNiceGrid.SetHeaderColor(Value: TfpgColor);
begin
  if (FHeaderColor <> Value) then
  begin
    FHeaderColor := Value;
    if not FUpdating then
      Invalidate;
  end;
end;

procedure TfpgNiceGrid.SetHeaderDarkColor(Value: TfpgColor);
begin
  if (FHeaderDarkColor <> Value) then
  begin
    FHeaderDarkColor := Value;
    if not FUpdating then 
      Invalidate;
  end;
end;

procedure TfpgNiceGrid.SetHeaderLightColor(Value: TfpgColor);
begin
  if (FHeaderLightColor <> Value) then
  begin
    FHeaderLightColor := Value;
    if not FUpdating then 
      Invalidate;
  end;
end;

procedure TfpgNiceGrid.SetHeaderLine(Value: Integer);
begin
  if (FHeaderLine <> Value) then
  begin
    FHeaderLine := Value;
    UpdateHeader;
    Invalidate;
   end;
end;

procedure TfpgNiceGrid.SetSelectionColor(Value: TfpgColor);
begin
  if (FSelectionColor <> Value) then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TfpgNiceGrid.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); 
var
  l, t, r, b: Integer;
  x, y: Integer;
  Empty: Boolean;
  Str: string;
  Old: Integer;
  OldS: string;

  procedure UpdateColRow;
  begin
    FUpdating := True;
    BuffString := '';
    FCol2 := FCol;
    FRow2 := FRow;
    EnsureVisible(FCol, FRow);
    FUpdating := False;
    SetSelectArea(fpgRect(FCol, FRow, FCol, FRow));
    ColRowChanged;
    SetScrollBar(FVScrollBar,FMaxVScroll, FVertOffset,FDefRowHeight);
    SetScrollBar(FHScrollBar, FMaxHScroll, FHorzOffset, 1); 
  end;

  procedure UpdateSelectArea;
  begin
    l := Min(FCol2, FCol);
    t := Min(FRow2, FRow);
    r := Max(FCol2, FCol);
    b := Max(FRow2, FRow);
    SetSelectArea(fpgRect(l, t, r, b)); 
    EnsureVisible(FCol2, FRow2);
  end;

begin
  if not FEnabled
    then Exit;

  if (ColCount = 0) or (FRowCount = 0)
    then Exit;

  inherited  HandleKeyPress(keycode, shiftstate, consumed);
    
  Consumed := true;    
  if (ssCtrl in shiftstate) then
  begin
    case KeyCode of

      Ord('X'), Ord('x'):
        if not FReadOnly then CutToClipboard;

      Ord('C'), Ord('c'):
        CopyToClipboard;

      Ord('V'), Ord('v'):
        if not FReadOnly
          then PasteFromClipboard;
		  
      Ord('E'), Ord('e'):
	begin
          if (not FReadOnly) and (not FColumns[FCol].FReadOnly) then
          begin
            IsEditing := True;
            FEdit.ShowEdit(FCol, FRow);
          end;
        end;	 
	  
      keyHome :
        begin
          FCol := GetFirstVisible;
          FRow := 0;
          UpdateColRow;
	end;

      keyEnd :
        begin
          FCol := GetLastVisible;
          FRow := FRowCount-1;
          UpdateColRow;
        end;

      keyDelete :
        begin
          if not FReadOnly and (FRowCount > 1) then
          begin
	    Old := FRow;
            DeleteRow(FRow);
            if Assigned(FOnDeleteRow)
              then FOnDeleteRow(Self, Old);
            UpdateColRow;
	  end;
        end;
	      
     keyInsert: 
        begin
          if not FReadOnly then
          begin
            InsertRow(Max(0, FRow));
            if Assigned(FOnInsertRow)
              then FOnInsertRow(Self, FRow);
            UpdateColRow;
          end;
        end;	      
      else Consumed:=false;
    end; {case}
  end  
  else	      
  if (ssShift in ShiftState) then
  begin
    case KeyCode of
      keyLeft:
        begin
          FCol2 := Max(GetPrevVisible(FCol2), GetFirstVisible);
          UpdateSelectArea;
        end;

      keyRight:
        begin
          FCol2 := Min(GetNextVisible(FCol2), GetLastVisible);
          UpdateSelectArea;
        end;

      keyUp:
        begin
          FRow2 := Max(FRow2 - 1, 0);
          UpdateSelectArea;
        end;

      keyDown:
        begin
          FRow2 := Min(FRow2 + 1, FRowCount-1);
          UpdateSelectArea;
        end;
      else Consumed:=false;
    end;  {case}

  end else
  begin
    case KeyCode of
      keyHome:
        begin
          FCol := GetFirstVisible;
          UpdateColRow;
        end;

      keyEnd:
        begin
          FCol := GetLastVisible;
          UpdateColRow;
        end;

      keyPrior:
        begin
          FRow := 0;
          UpdateColRow;
        end;

      keyNext:
        begin
          FRow := FRowCount-1;
          UpdateColRow;
        end;

      keyLeft:
        begin
          FCol := Max(GetPrevVisible(FCol), GetFirstVisible);
          UpdateColRow;
        end;

      keyRight:
        begin
          FCol := Min(GetNextVisible(FCol), GetLastVisible);
          UpdateColRow;
        end;

      keyUp:
        begin
          if FAutoAddRow and (FRow = (FRowCount-1)) and (FRow > 0) and not FReadOnly then
          begin
            Empty := True;
            for x := 0 to ColCount-1 do
            begin
              if (SafeGetCell(x, FRowCount-1) <> '') then
              begin
                Empty := False;
                Break;
              end;
            end;
            if Empty then
            begin
              RowCount := RowCount - 1;
              FRow := FRowCount - 1;
              if Assigned(FOnDeleteRow)
                then FOnDeleteRow(Self, FRowCount);
            end else
              FRow := Max(0, FRow - 1);
          end else
            FRow := Max(0, FRow - 1);
          UpdateColRow;
        end;

      keyDown:
        begin
          if FAutoAddRow and (FRow = (FRowCount-1)) and not FReadOnly then
          begin
            Inc(FRow);
            RowCount := RowCount + 1;
            if Assigned(FOnInsertRow)
              then FOnInsertRow(Self, FRow);
          end 
          else
            FRow := Min(FRowCount - 1, FRow + 1);
          UpdateColRow;
        end;

      keyReturn, keyPEnter:
        begin
          OldS := GetCell(Col, Row);
          Str := OldS;
          if Assigned(FOnCellAssignment)
            then FOnCellAssignment(Self, Col, Row, Str);
          if (Str <> Olds)
            then InternalSetCell(Col, Row, Str, True);
          if (FSelectArea.Left = FSelectArea.Width) and
             (FSelectArea.Top = FSelectArea.Height) then
          begin
              FRow := Min(FRowCount - 1, FRow + 1);
              UpdateColRow;
          end
          else
          begin
              if (FCol = FSelectArea.Width) and (FRow = FSelectArea.Height) then
              begin
                FCol := FSelectArea.Left;
                FRow := FSelectArea.Top;
              end
	      else if (FRow = FSelectArea.Height) then
              begin
                FCol := FCol + 1;
                FRow := FSelectArea.Top;
              end 
	      else
              begin
                FRow := Row + 1;
              end;
              BuffString := '';
              EnsureVisible(FCol, FRow);
              ColRowChanged;
          end;
        end;

      keyDelete:
        begin
          if (BuffString = '') then
          begin
            if not FReadOnly then
            begin
              FUpdating := True;
              for x := SelectArea.Left to SelectArea.Width do
              begin
                for y := SelectArea.Top to SelectArea.Height
                  do InternalSetCell(X, Y, '', True);
              end;
              FUpdating := False;
            end;
          end;
        end;
      else Consumed:=false;
    end; {case}

  end;
   if consumed then  Invalidate; 
end;


function TfpgNiceGrid.GetHitTestInfo(X, Y: Integer): TGridHitTest;
var
  a, i1, i2: Integer;
  ax, ay: Integer;
  IsSizing: Boolean;

begin
  Result := gtNone;
  IsSizing := False;

  ax := (FHorzOffset + X) - FixedWidth;
  ay := (FVertOffset + Y) - FixedHeight;

  if not FAutoColWidth then
  begin
    for a := 1 to ColCount do
    begin
      i1 := GetColCoord(a);
      i2 := X + FHorzOffset - FixedWidth;
      if (i2 > (i1-2)) and (i2 < (i1+2)) then
      begin
        SizingCol := a - 1;
        IsSizing := FColumns[SizingCol].FCanResize;
        Break;
      end;
    end;
  end;

  if PtInRect(SmallBox, Point(X, Y))
    then Result := gtSmallBox else
  if IsSizing
    then Result := gtColSizing else
  if ((X < FixedWidth) and (Y < FixedHeight))
    then Result := gtLeftTop else
  if ((X < FixedWidth) and (Y > FixedHeight) and (ay < BodyHeight))
    then Result := gtLeft else
  if ((Y < FixedHeight) and (X > FixedWidth) and (ax < BodyWidth))
    then Result := gtTop else
  if ((X > FixedWidth) and (Y > FixedHeight) and (ax < BodyWidth) and (ay < BodyHeight))
    then Result := gtCell;

end;

procedure TfpgNiceGrid.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  Pt: TPoint;
begin
  if not FEnabled then
  begin
    inherited;
    Exit;
  end;
  if (MouseCursor = mcMove) then
  begin
    SizingColX := GetColCoord(SizingCol);
  end else
  if (MouseCursor = mcHand) then
  begin
    SmallBoxArea := FSelectArea;
  end
  else
  if (MouseCursor = mcSizeNWSE) then
  begin
    FRow := 0;
    FCol := 0;
    BuffString := '';
    EnsureVisible(0, 0);
    FCol2 := ColCount-1;
    FRow2 := FRowCount-1;
    SetSelectArea(fpgRect(0, 0, ColCount-1, FRowCount-1));
    ColRowChanged;
  end
  else
  if (MouseCursor = mcSizeEW) then
  begin
    FRow := GetRowFromY(Y);
    FCol := 0;
    LastHover := FRow;
    BuffString := '';
    EnsureVisible(FCol, FRow);
    FCol2 := ColCount-1;
    FRow2 := FRow;
    SmallBoxPos := 2;
    AdjustSelection(fpgRect(0, FRow, ColCount-1, FRow), True);
    ColRowChanged;
    if Assigned(OnGutterClick)
      then FOnGutterClick(Self, FRow, mbLeft, ShiftState);
  end
  else
  if (MouseCursor = mcSizeNS) then
  begin
    FCol := GetColFromX(X);
    FRow := 0;
    LastHover := FCol;
    BuffString := '';
    EnsureVisible(FCol, FRow);
    FCol2 := FCol;
    FRow2 := FRowCount-1;
    SmallBoxPos := 1;
    AdjustSelection(fpgRect(FCol, 0, FCol, FRowCount-1), True);
    ColRowChanged;
    if Assigned(FOnHeaderClick)
      then FOnHeaderClick(Self, FCol, mbLeft, ShiftState);
  end
  else
  if (MouseCursor = mcCross) then
  begin
    BuffString := '';
    Pt := GetCellAtPos(X, Y);
    if (Pt.X = FCol) and (Pt.Y = FRow) then
    begin
      EnsureVisible(FCol, FRow);
      if (not FReadOnly) and (not FColumns[FCol].FReadOnly) then
      begin
        IsEditing := True;
        FEdit.ShowEdit(FCol, FRow);
      end;
    end
    else
    if (Pt.X <> -1) and (pt.Y <> -1) then
    begin
      FEdit.HideEdit;
      IsEditing := False;
    
      EnsureVisible(Pt.X, Pt.Y);
      FCol := Pt.X;
      FRow := Pt.Y;
      BuffString := '';
      FCol2 := FCol;
      FRow2 := FRow;
      SetSelectArea(fpgRect(FCol, FRow, FCol, FRow));
    end;
    ColRowChanged;
  end;

  CaptureMouse;
  Invalidate;
  inherited;
end;

procedure TfpgNiceGrid.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  Total2Col: Integer;
  Suggested: Integer;
  Pt: TPoint;
  l, t, r, b: Integer;
  i: Integer;

begin
 if not FEnabled then
  begin
    MouseCursor := mcDefault;
    inherited;
    Exit;
  end;

  if (ssLeft in ShiftState) then
  begin

    if (MouseCursor = mcCross) then
    begin
      Pt := GetCellAtPos(X, Y);
      if (Pt.X <> -1) and (Pt.Y <> -1) then
      begin
        l := Min(Pt.X, FCol);
        t := Min(Pt.Y, FRow);
        r := Max(Pt.X, FCol);
        b := Max(Pt.Y, FRow);
        FCol2 := Pt.X;
        FRow2 := Pt.Y;
        SetSelectArea(fpgRect(l, t, r, b)); 
        EnsureVisible(FCol2, FRow2);
        Invalidate;
      end;
    end else

    if (MouseCursor = mcHand) then
    begin
      Pt := GetCellAtPos(X, Y);
      if (Pt.X <> -1) and (Pt.Y <> -1) then
      begin
        l := Min(Pt.X, SmallBoxArea.Left);
        t := Min(Pt.Y, SmallBoxArea.Top);
        r := Max(Pt.X, SmallBoxArea.Width); 
        b := Max(Pt.Y, SmallBoxArea.Height);
        FCol2 := Pt.X;
        FRow2 := Pt.Y;
        SetSelectArea(fpgRect(l, t, r, b));
        EnsureVisible(FCol2, FRow2);
        Invalidate;
      end;
    end else

    if (MouseCursor = mcSizeEW) then
    begin
      i := GetRowFromY(Y);
      if (i <> -1) and (i <> LastHover) then
      begin
        LastHover := i;
        t := Min(i, FRow);
        b := Max(i, FRow);
        FRow2 := i;
        SmallBoxPos := 2;
        AdjustSelection(fpgRect(0, t, ColCount-1, b), True); 
        Invalidate; 
      end;
    end else

    if (MouseCursor = mcSizeNS) then
    begin
      i := GetColFromX(X);
      if (i <> -1) and (i <> LastHover) then
      begin
        LastHover := i;
        l := Min(i, FCol);
        r := Max(i, FCol);
        FCol2 := i;
        SmallBoxPos := 1;
        AdjustSelection(fpgRect(l, 0, r, FRowCount-1), True);
        Invalidate; 
      end;
    end else

    if (MouseCursor = mcMove) then
    begin
      Suggested := Max(5, X + FHorzOffset - SizingColX - FixedWidth);
      if FFitToWidth then
      begin
        if (SizingCol = ColCount-1) or (SizingCol = -1) then
        begin
          inherited;
          Exit;
        end;
        Total2Col := (ClientWidth - FixedWidth) - (TotalWidth - Columns[SizingCol].FWidth - Columns[SizingCol+1].FWidth);
        if (Total2Col > 10) then
        begin
          Columns[SizingCol].FWidth := Suggested;
          Columns[SizingCol+1].FWidth := Total2Col - Suggested;
        end;
        if (Columns[SizingCol+1].FWidth < 5) then
        begin
          Columns[SizingCol].FWidth := Total2Col - 5;
          Columns[SizingCol+1].FWidth := 5;
        end;
      end else
      begin
        Columns[SizingCol].FWidth := Suggested;
      end;
      Recalculate;
      Invalidate;
    end;
  end
  else
     MouseCursor := CursorArray[GetHitTestInfo(X, Y)];
  inherited;
end;

procedure TfpgNiceGrid.HandleLMouseUp(x, y: integer; shiftstate: TShiftState); 
var
  Ls: TList;
  ax, ay: Integer;
  l, t, w, h: Integer;

  function GetCopy(nx, ny: Integer): string;
  var
    ix, iy: Integer;
  begin
    ix := nx;
    iy := ny;
    while (ix < l)
      do ix := ix + w;
    while (iy < t)
      do iy := iy + h;
    ix := ((ix - l) mod w) + l;
    iy := ((iy - t) mod h) + t;
    Result := SafeGetCell(TfpgNiceColumn(Ls[ix]).Index, iy);
  end;

begin 
  if (MouseCursor = mcHand) then
  begin
    if FReadOnly then
    begin
      SmallBoxArea := fpgRect(-1, -1, -1,-1);
      Invalidate; 
    end
    else
    begin
      FUpdating := True;
      Ls := TList.Create;
      for ax := FSelectArea.Left to FSelectArea.Width do
        if FColumns[ax].FVisible
          then Ls.Add(FColumns[ax]);
      l := 0;
      for ax := 0 to Ls.Count-1 do
      begin
        if (TfpgNiceColumn(Ls[ax]).Index = SmallBoxArea.Left) then
        begin
          l := ax;
          Break;
        end;
      end;
      t := SmallBoxArea.Top;
      w := (SmallBoxArea.Width - SmallBoxArea.Left) + 1;
      h := (SmallBoxArea.Height- SmallBoxArea.Top) + 1; 
      for ax := 0 to Ls.Count-1 do
        for ay := FSelectArea.Top to FSelectArea.Height
          do InternalSetCell(TfpgNiceColumn(Ls[ax]).Index, ay, GetCopy(ax, ay), True);
      Ls.Free;
      SmallBoxArea := fpgRect(-1, -1, -1,-1);
      BuffString := '';
      FUpdating := False;
      Invalidate; 	  
    end;
  end;

  MouseCursor := CursorArray[GetHitTestInfo(X, Y)];
  ReleaseMouse;
  LastHover := -1;

  inherited;
end;

procedure   TfpgNiceGrid.HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint);
begin
  inherited HandleMouseScroll(x, y, shiftstate, delta);
  FVertOffset :=  FVertOffset + (delta*FDefRowHeight); 
  FVertOffset := Max(0, Min(FMaxVScroll, FVertOffset));
  NormalizeVertOffset;
  SetScrollBar(FVScrollBar, FMaxVScroll, FVertOffset, FDefRowHeight);
  Invalidate;  
end;	  
	  
procedure TfpgNiceGrid.SetColumns(Value: TfpgNiceColumns);
begin
  FColumns.Assign(Value);
end;

function TfpgNiceGrid.CreateColumn: TfpgNiceColumn;
begin
  Result := TfpgNiceColumn.Create(Columns);
end;

procedure TfpgNiceGrid.UpdateColumn(Index: Integer);
var
  i: Integer;
begin
  for i := 0 to FRowCount-1 do
    if (GetObject(Index, i) <> TObject(MergeID)) then
      DrawCell(Index, i);
end;

procedure TfpgNiceGrid.UpdateColumns;
begin
  UpdateHeader;
  Invalidate;
end;

function TfpgNiceGrid.GetColCount: Integer;
begin
  Result := FColumns.Count;
end;

function TfpgNiceGrid.TotalWidth: Integer;
var
  x: Integer;
begin
  Result := 0;
  for x := 0 to FColumns.Count-1 do
  begin
    if FColumns[x].FVisible
      then Result := Result + FColumns[x].FWidth;
  end;
end;

procedure TfpgNiceGrid.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);
  if (csLoading in ComponentState) then
    Exit; //==>
  if csUpdating in ComponentState then
    Exit; //==>
  Recalculate;
  if (FColumns.Count > 0)
    then EnsureVisible(FCol, FRow);
  if HasHandle then
     UpdateScrollBars;
end;

procedure TfpgNiceGrid.UpdateScrollBars;
var
  HWidth: integer;
  VHeight: integer;
  HeightAvail: integer;
  vw: integer;
  cw: integer;
  i: integer;
begin
  VHeight :=  Height -2;
  HWidth  := Width - 2;
  
  if FVScrollBar.Visible then
    vw := Width - FVScrollBar.Width
  else
    vw := Width;
 
  cw := 0;
  for i := 0 to FColumns.Count-1 do
    cw := cw + FColumns[i].Width;

  // This needs improving while resizing
  if cw > vw then
    FHScrollBar.Visible := true 
  else
  begin
    FHScrollBar.Visible := False;
    FHorzOffset:=0;
  end;

  HeightAvail := Height - FixedHeight;
  if FShowFooter
    then HeightAvail := HeightAvail - FDefRowHeight;  

  if FVScrollBar.Visible then
  begin
    Dec(HWidth, FVScrollBar.Width);
     if FRowCount > 0 then
      FVScrollBar.SliderSize := HeightAvail / BodyHeight
    else
      FVScrollBar.SliderSize := 0;
    FVScrollBar.Max:= FMaxVScroll; 
    FVScrollBar.Position:=FVertOffset;
    FVScrollBar.RepaintSlider;
  end;
  
  if FHScrollBar.Visible then
  begin
    Dec(VHeight, FHScrollBar.Height);
    FHScrollBar.SliderSize  := 0.2;
    FHScrollBar.RepaintSlider;
  end;

  FHScrollBar.Top     := Height -FHScrollBar.Height-1;
  FHScrollBar.Left    := 1;
  FHScrollBar.Width   := HWidth;

  FVScrollBar.Top     := 1;
  FVScrollBar.Left    := Width - FVScrollBar.Width-1;
  FVScrollBar.Height  := VHeight;

  FVScrollBar.UpdateWindowPosition;
  FHScrollBar.UpdateWindowPosition;
end;     
    
procedure TfpgNiceGrid.SetShowGrid(Value: Boolean);
begin
  if (FShowGrid <> Value) then
  begin
    FShowGrid := Value;
    Invalidate;  
  end;
end;

procedure TfpgNiceGrid.SetShowFooter(const Value: Boolean);
begin
  if (FShowFooter <> Value) then
  begin
    FShowFooter := Value;
    Recalculate;
    Invalidate;
    UpdateScrollBars;
  end;
end;

procedure TfpgNiceGrid.Clear;
var
  x: Integer;
begin
  for x := 0 to ColCount-1
    do FColumns[x].FStrings.Clear;
  Invalidate; 
end;

procedure TfpgNiceGrid.SetHorzOffset(Value: Integer);
begin
  if (FHorzOffset <> Value) then
  begin
    FHorzOffset := Max(0, Min(FMaxHScroll, Value));
    SetScrollBar(FHScrollBar, 0, FHorzOffset, 1);
  end;
end;

procedure TfpgNiceGrid.SetVertOffset(Value: Integer);
begin
  if (FVertOffset <> Value) then
  begin
    FVertOffset := Max(0, Min(FMaxVScroll, Value));
    NormalizeVertOffset;
    SetScrollBar(FVScrollBar, 0, FVertOffset, FDefRowHeight);
  end;
end;

procedure TfpgNiceGrid.EnsureVisible(X, Y: Integer);
var
  t, b, h: Integer;
  l, r: Integer;
  Horz, Vert: Boolean;
  SuggestedHorz, SuggestedVert: Integer;

begin

  if (X = -1) or (Y = -1)
    then Exit;

  if (AllWidth < ClientWidth) and (AllHeight < ClientHeight)
    then Exit;

  SuggestedVert := FVertOffset;
  t := FVertOffset div FDefRowHeight;
  h := ((ClientHeight - FixedHeight) div FDefRowHeight) - 1;
  if FShowFooter
    then h := h-1;
  b := t + h;
  Vert := (Y < t) or (Y > b);
  if (Y < t)
    then SuggestedVert := Y * FDefRowHeight;
  if (Y > b)
    then SuggestedVert := (Y - h) * FDefRowHeight;

  SuggestedHorz := FHorzOffset;
  l := GetColCoord(X) - FHorzOffset + FixedWidth;
  r := l + FColumns[x].FWidth;
  Horz := (l < FixedWidth) or (r > ClientWidth);
  if (l < FixedWidth)
    then SuggestedHorz := Max(0, SuggestedHorz + (l - FixedWidth));
  if (r > ClientWidth)
    then SuggestedHorz := Min(FMaxHScroll, SuggestedHorz - (ClientWidth - r) + 1);

  if Vert and not Horz
    then SetVertOffset(SuggestedVert) else

  if Horz and not Vert
    then SetHorzOffset(SuggestedHorz) else

  if Horz and Vert
    then
    begin
      FHorzOffset := SuggestedHorz;
      FVertOffset := SuggestedVert;
      SetScrollBar(FHScrollBar, 0, FHorzOffset, 1);
      SetScrollBar(FVScrollBar, 0, FVertOffset, FDefRowHeight);
      Invalidate;
   end;
end;

function TfpgNiceGrid.HeaderCells(I: Integer): THeaderInfo;
begin
  Result := PHeaderInfo(FHeaderInfos[I])^;
end;

function TfpgNiceGrid.HeaderCellsCount: Integer;
begin
  Result := FHeaderInfos.Count;
end;

procedure TfpgNiceGrid.SetReadOnly(Value: Boolean);
begin
  if (FReadOnly <> Value) then
  begin
    FReadOnly := Value;
  end;
end;

procedure TfpgNiceGrid.SetCol(Value: Integer);
begin
  if (FCol <> Value) then
  begin
    FCol := Value;
    FCol2 := Value;
    FRow2 := FRow;
    BuffString := '';
    SetSelectArea(fpgRect(FCol, FRow, FCol, FRow));
    ColRowChanged;
    Invalidate; 
  end;
end;

procedure TfpgNiceGrid.SetRow(Value: Integer);
begin
  if (FRow <> Value) then
  begin
    FRow := Value;
    FRow2 := Value;
    FCol2 := FCol;
    BuffString := '';
    SetSelectArea(fpgRect(FCol, FRow, FCol, FRow));
    ColRowChanged;
    Invalidate;
  end;
end;

procedure TfpgNiceGrid.AdjustSelection(Value: TfpgRect; Force: Boolean);
begin
  if (FSelectArea = Value) and not Force then
    Exit;  //==>
  FSelectArea := Value;
end;

procedure TfpgNiceGrid.SetSelectArea(Value: TfpgRect);
begin
  AdjustSelection(Value, False);
end;

procedure TfpgNiceGrid.SetGutterKind(Value: TGutterKind);
var
  Old: TGutterKind;
begin
  Old := FGutterKind;
  if (FGutterKind <> Value) then
  begin
    FGutterKind := Value;
    Recalculate;
    if (Old = gkNone) or (Value = gkNone) then
      Invalidate;
  end;
end;

procedure TfpgNiceGrid.SetGutterWidth(Value: Integer);
begin
  if (FGutterWidth <> Value) then
  begin
    FGutterWidth := Value;
    Recalculate;
    Invalidate;
    UpdateScrollBars;
  end;
end;

procedure TfpgNiceGrid.CopyToClipboard;
var
  s: string;
  t: TStringList;
  x, y: Integer;
begin
  t := TStringList.Create;
  with Fselectarea do   
  for y := FSelectArea.Top to FSelectArea.Height do
  begin
    s := '';
    for x := FSelectArea.Left to FSelectArea.Width do 
    begin
      if FColumns[x].FVisible then
      begin
        if (x = FSelectArea.Left)
          then s := SafeGetCell(X, Y)
          else s := s + #9 + SafeGetCell(X, Y);
      end;
    end;
    t.Add(s);
  end;
  fpgClipboard.Text := t.Text;
  t.Free;
end;

procedure TfpgNiceGrid.CutToClipboard;
var
  s: string;
  t: TStringList;
  x, y: Integer;
begin
  FUpdating := True;
  t := TStringList.Create;
  for y := FSelectArea.Top to FSelectArea.Height do
  begin
    s := '';
    for x := FSelectArea.Left to FSelectArea.Width do
    begin
      if FColumns[x].FVisible then
      begin
        if (x = FSelectArea.Left)
          then s := SafeGetCell(X, Y)
          else s := s + #9 + SafeGetCell(X, Y);
        InternalSetCell(X, Y, '', True);
      end;
    end;
    t.Add(s);
  end;
  fpgClipboard.Text := t.Text;
  t.Free;
  FUpdating := False;
  Invalidate; 
end;

procedure TfpgNiceGrid.PasteFromClipboard;
var
  tr, tc: TStringList;
  x, y: Integer;
  s: string;
  n: Integer;
  TabCnt: Integer;
  ax, ay: Integer;
  ColCnt: Integer;

begin
  FUpdating := True;  
  tr := TStringList.Create;
  tc := TStringList.Create;
  tr.Text := fpgClipboard.Text;
  TabCnt := 1;

  for y := 0 to tr.Count-1 do
  begin
    n := 1;
    s := tr[y];
    for x := 1 to Length(s) do
      if (s[x] = #9)
        then Inc(n);
    TabCnt := Max(TabCnt, n);
  end;

  ColCnt := ColCount; // Just to make it fast

  if (FSelectArea.Left = FSelectArea.Width) and (FSelectArea.Top = FSelectArea.Height) then
  begin

    for y := 0 to tr.Count-1 do
    begin
      tc.Text := StringReplace(tr[y], #9, #13#10, [rfReplaceAll]);
      while (tc.Count < TabCnt)
        do tc.Add('');
      x := 0;
      ax := FCol;
      while (x < tc.Count) do
      begin
        ay := FRow + y;
        if FColumns[ax].FVisible then
        begin
          if (ax < ColCnt) and (ay < FRowCount)
            then InternalSetCell(ax, ay, tc[x], True);
          Inc(x);
        end;
        Inc(ax);
      end;
    end;

  end else
  begin

    ay := FSelectArea.Top;
    while (ay <= FSelectArea.Height) do
    begin
      tc.Text := StringReplace(tr[(ay - FSelectArea.Top) mod tr.Count], #9, #13#10, [rfReplaceAll]);
      while (tc.Count < TabCnt)
        do tc.Add('');
      ax := FSelectArea.Left;
      x := 0;
      while (ax <= FSelectArea.Width) do
      begin
        if FColumns[ax].FVisible then
        begin
          InternalSetCell(ax, ay, tc[x], True);
          Inc(x);
          if (x = tc.Count)
            then x := 0;
        end;
        Inc(ax);
      end;
      Inc(ay);
    end;

  end;

  tr.Free;
  tc.Free;

  FUpdating := False;
  Invalidate;
end;

procedure TfpgNiceGrid.NormalizeVertOffset;
begin
  FVertOffset := (FVertOffset div FDefRowHeight) * FDefRowHeight;
end;

procedure TfpgNiceGrid.SetGutterFont(const Value: string);
begin
  if FGutterFont <> Value then
  begin
    FGutterFont:= Value;
    Invalidate; 
  end;	    
end;

procedure TfpgNiceGrid.SetGutterFontColor(Value: TfpgColor);
begin
  if FGutterFontColor <> Value then
  begin	  
    FGutterFontColor:= Value;
    Invalidate; 
  end;	    
end;

procedure TfpgNiceGrid.SetHeaderFont(Value: string);
begin
  FHeaderFont:=Value;
  Invalidate;
end;

procedure TfpgNiceGrid.SetHeaderFontColor(Value: TfpgColor);
begin
  if FHeaderFontColor <> Value then
  begin	  
    FHeaderFontColor:= Value;
    Invalidate;
  end;	    
end;    
    
procedure TfpgNiceGrid.SetFooterFont(const Value: string);
begin
  FFooterFont:= Value;
  Invalidate;
end;

procedure TfpgNiceGrid.SetFooterFontColor(Value: TfpgColor);
begin
  if FFooterFontColor <> Value then
  begin	  
    FFooterFontColor:= Value;
    Invalidate;
  end;	    
end;    

function TfpgNiceGrid.GetFirstVisible: Integer;
var
  x: Integer;
begin
  Result := -1;
  if (ColCount > 0) then
  begin
    for x := 0 to ColCount-1 do
    begin
      if Columns[x].Visible then
      begin
        Result := x;
        Break;
      end;
    end;
  end;
end;

function TfpgNiceGrid.GetLastVisible: Integer;
var
  x: Integer;
begin
  Result := -1;
  if (ColCount > 0) then
  begin
    for x := ColCount-1 downto 0 do
    begin
      if Columns[x].Visible then
      begin
        Result := x;
        Break;
      end;
    end;
  end;
end;

function TfpgNiceGrid.GetNextVisible(Index: Integer): Integer;
var
  x: Integer;
begin
  Result := Index;
  if (ColCount > 0) and (Index < ColCount) then
  begin
    for x := (Index + 1) to (ColCount - 1) do
    begin
      if Columns[x].Visible then
      begin
        Result := x;
        Break;
      end;
    end;
  end;
end;

function TfpgNiceGrid.GetPrevVisible(Index: Integer): Integer;
var
  x: Integer;
begin
  Result := Index;
  if (ColCount > 0) and (Index > 0) then
  begin
    for x := (Index - 1) downto 0 do
    begin
      if Columns[x].Visible then
      begin
        Result := x;
        Break;
      end;
    end;
  end;
end;

procedure TfpgNiceGrid.DeleteRow(ARow: Integer);
var
  x, y: Integer;
begin
  if (ARow >= 0) and (ARow < FRowCount) then
  begin
    for x := 0 to ColCount-1 do
    begin
      with FColumns[x].Strings do
      begin
        if (Count > ARow) then
        begin
          for y := ARow to Count-2
            do Strings[y] := Strings[y + 1];
          Strings[Count-1] := '';
        end;
      end;
    end;
    if (FRow = FRowCount-1)
      then Dec(FRow);
    RowCount := RowCount - 1;
    UpdateScrollBars;    
  end;
end;

procedure TfpgNiceGrid.InsertRow(ARow: Integer);
var
  x: Integer;
begin
  if (ARow >= 0) and (ARow < FRowCount) then
  begin
    for x := 0 to ColCount-1 do
    begin
      with FColumns[x].Strings do
      begin
        while (Count < ARow)
          do Add('');
        Insert(ARow, '');
      end;
    end;
    RowCount := RowCount + 1;
  end;
  UpdateScrollBars;  
end;

function TfpgNiceGrid.AddRow: Integer;
var
  x: Integer;
  n: Integer;
begin
  n := FRowCount + 1;
  for x := 0 to ColCount-1 do
  begin
    with FColumns[x].Strings do
    begin
      while (Count < n)
        do Add('');
      Strings[FRowCount] := '';
    end;
  end;
  RowCount := RowCount + 1;
  Result := FRowCount-1;
end;

procedure TfpgNiceGrid.ColRowChanged;
begin
  if Assigned(Sync)
    then Sync.Row := FRow;
  if Assigned(FOnColRowChanged)
    then FOnColRowChanged(Self, FCol, FRow);
end;

procedure TfpgNiceGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = Sync) and (Operation = opRemove) 
    then Sync := nil;
  inherited;
end;

procedure TfpgNiceGrid.SetGutterStrings(const Value: TStrings);
begin
  FGutterStrings.Assign(Value);
  if (FGutterKind = gkString) then
    Invalidate;
end;

function TfpgNiceGrid.GetObject(X, Y: Integer): TObject;
var
  t: TStrings;
begin
  Result := nil;
  if (X > ColCount-1) or (Y > FRowCount-1)
    then raise Exception.Create('Cell Index out of bound.');
  t := Columns[X].FStrings;
  if (Y < t.Count)
    then Result := t.Objects[Y];
end;

procedure TfpgNiceGrid.SetObject(X, Y: Integer; const Value: TObject);
var
  t: TStrings;
begin
  if (X > ColCount-1) or (Y > FRowCount-1)
    then raise Exception.Create('Cell Index out of bound.');
  t := Columns[X].FStrings;
  while (Y > t.Count-1)
    do t.Add('');
  t.Objects[Y] := Value;
end;

procedure TfpgNiceGrid.ClearMergeCells;
var
  x, y: Integer;
  List: TStrings;
begin
  for x := 0 to FColumns.Count-1 do
  begin
    List := FColumns[x].FStrings;
    for y := 0 to List.Count-1
      do List.Objects[y] := nil;
  end;
  for x := 0 to Mergeds.Count-1
    do TfpgMergeCell(Mergeds[x]).Free;
  Mergeds.Clear;  
end;

function TfpgNiceGrid.MergeCells(const X1, Y1, X2, Y2: Integer;
  ACaption: string): TfpgMergeCell;
begin
  Result := TfpgMergeCell.Create;
  Result.Font:= Font.FontDesc;
  Result.Color := Color;
  Result.Text := ACaption;
  Result.HorzAlign := haCenter;
  Result.VertAlign := vaCenter;
  Result.Rc := fpgRect(Min(X1, X2), Min(Y1, Y2), Max(X1, X2)-Min(X1, X2), Max(Y1, Y2)-Min(Y1, Y2));
  Mergeds.Add(Result);
  if not FUpdating then
  begin
    Recalculate;
  writeln('MergeCells');
    Invalidate;
  end;
end;

procedure TfpgNiceGrid.BuildMergeData;
var
  Rc: TfpgRect;
  x, y, z: Integer;
begin
  for x := 0 to Mergeds.Count-1 do
  begin
    CopyRect(Rc, TfpgMergeCell(Mergeds[x]).Rc);
    for y := Rc.Left to Rc.Right do
    begin
      if (y >= FColumns.Count)
        then Continue;
      for z := Rc.Top to Rc.Bottom do
      begin
        InternalSetCell(y, z, '', False);
        SetObject(y, z, TObject(MergeID));
      end;
    end;
  end;
end;

procedure TfpgNiceGrid.DrawMergedCell(Index: Integer);
var
  Data: TfpgMergeCell;
  Rc, Dummy: TfpgRect;
  l1, l2, t, h: Integer;
begin
  Data := TfpgMergeCell(Mergeds[Index]);
  l1 := GetColCoord(Data.Rc.Left);
  l2 := GetColCoord(Data.Rc.Right + 1);
  t := FDefRowHeight * Data.Rc.Top;
  h := FDefRowHeight * (Data.Rc.Bottom - Data.Rc.Top + 1);
  Rc := fpgRect(l1-1, t-1, l2-l1, h);
  OffsetRect(Rc, -FHorzOffset + FixedWidth, -FVertOffset + FixedHeight);
  if IntersectRect(Dummy, Rc, CellBox) then
  begin
    with Canvas do
    begin
      Font:= fpgGetFont(Data.Font);
      if not FEnabled
        then SetTextColor(FGridColor);
      if FShowGrid then
      begin
        SetColor(FGridColor);
        DrawRectangle(Rc);
      end
      else
      begin
        SetColor(Data.Color);
        FillRectangle(Rc);
      end;
      InflateRect(Rc, -4, -2);
      DrawStringUni(Canvas, Data.Text, Rc, Data.HorzAlign, Data.VertAlign);
    end;
  end;
end;

function TfpgNiceGrid.GetHeaderInfo: TList;
begin
  Result := FHeaderInfos;
end;

function TfpgNiceGrid.GetMergedCellsData: TList;
begin
  Result := Mergeds;
end;

procedure TfpgNiceGrid.SetEnabled(const Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    Invalidate;
  end;  
end;

{ TfpgNiceColumn }

constructor TfpgNiceColumn.Create(Collec: TCollection);
begin
  FStrings := TStringList.Create;
  FFont:= 'Arial-8';  
  FHorzAlign := haLeft;
  FVertAlign := vaCenter;
  FVisible := True;
  FCanResize := True;
  FReadOnly := False;
  FTag := 0;
  FTag2 := 0;
  with TfpgNiceColumns(Collec).Grid do
  begin
    Self.FFont:=Font.FontDesc;
    Self.FWidth := DefColWidth;
    Self.FColor := Color;
  end;
  inherited Create(Collec);
end;

destructor TfpgNiceColumn.Destroy;
begin
  inherited Destroy;
  FStrings.Free;
end;

procedure TfpgNiceColumn.Assign(Source: TPersistent);
begin
  if (Source is TfpgNiceColumn) then
  begin
    Title     := TfpgNiceColumn(Source).Title;
    Footer  := TfpgNiceColumn(Source).Footer;
    Width   := TfpgNiceColumn(Source).Width;
    Font     := TfpgNiceColumn(Source).Font;
    Color    := TfpgNiceColumn(Source).Color;
    HorzAlign := TfpgNiceColumn(Source).HorzAlign;
    VertAlign := TfpgNiceColumn(Source).VertAlign;
    Visible  := TfpgNiceColumn(Source).Visible;
    Tag     := TfpgNiceColumn(Source).Tag;
    Tag2   := TfpgNiceColumn(Source).Tag2;
    Hint     := TfpgNiceColumn(Source).Hint;
    CanResize := TfpgNiceColumn(Source).CanResize;
    ReadOnly  := TfpgNiceColumn(Source).ReadOnly;
    Strings.Assign(TfpgNiceColumn(Source).Strings);
    Changed(False);
  end;
end;

procedure TfpgNiceColumn.SetColor(Value: TfpgColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

procedure TfpgNiceColumn.SetFont(Value: string);
begin
  FFont:=Value;
  Changed(False);
end;

procedure TfpgNiceColumn.SetHorzAlign(Value: THorzAlign);
begin
  if (FHorzAlign <> Value) then
  begin
    FHorzAlign := Value;
    Changed(False);
  end;
end;

procedure TfpgNiceColumn.SetTitle(Value: string);
begin
  if (FTitle <> Value) then
  begin
    FTitle := Value;
    Changed(True);
  end;
end;

procedure TfpgNiceColumn.SetFooter(const Value: string);
begin
  if (FFooter <> Value) then
  begin
    FFooter := Value;
    Changed(False);
  end;
end;

procedure TfpgNiceColumn.SetVertAlign(Value: TVertAlign);
begin
  if (FVertAlign <> Value) then
  begin
    FVertAlign := Value;
    Changed(False);
  end;
end;

procedure TfpgNiceColumn.SetWidth(Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    Changed(True);
  end;
end;

procedure TfpgNiceColumn.SetVisible(Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    TfpgNiceColumns(Collection).FGrid.ForcedColumn := Index;
    Changed(True);
  end;
end;

procedure TfpgNiceColumn.SetStrings(Value: TStrings);
begin
  FStrings.Assign(Value);
  Changed(False);
end;

function TfpgNiceColumn.IsFontStored: Boolean;
begin
  Result := True;
  if (TfpgNiceColumns(Collection).FGrid.Font.FontDesc = FFont) then
	  Result:= false;
end;

function TfpgNiceColumn.GetGrid: TfpgNiceGrid;
begin
  Result := TfpgNiceColumns(Collection).FGrid;
end;

function TfpgNiceColumn.GetDisplayName: string;
begin
  if (FTitle <> '')
    then Result := FTitle
    else Result := 'Column ' + IntToStr(Index);
end;

{ TfpgNiceColumns }
    
constructor TfpgNiceColumns.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass); 
begin
  FGrid := TfpgNiceGrid(AOwner);    
  inherited Create(FGrid,TfpgNiceColumn);
end;    

function TfpgNiceColumns.Add: TfpgNiceColumn;
begin
  Result := TfpgNiceColumn(inherited Add);
end;

function TfpgNiceColumns.GetItem(Index: Integer): TfpgNiceColumn;
begin
  Result := TfpgNiceColumn(inherited GetItem(Index));
end;

procedure TfpgNiceColumns.SetItem(Index: Integer; Value: TfpgNiceColumn);
begin
  inherited SetItem(Index, Value);
end;

function TfpgNiceColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

function TfpgNiceColumns.Insert(Index: Integer): TfpgNiceColumn;
begin
  Result := AddItem(nil, Index);
end;

function TfpgNiceColumns.AddItem(Item: TfpgNiceColumn;
  Index: Integer): TfpgNiceColumn;
begin
  if (Item = nil)
    then Result := FGrid.CreateColumn
    else
    begin
      Result := Item;
      if Assigned(Item) then
      begin
        Result.Collection := Self;
        if (Index < 0)
          then Index := Count - 1;
        Result.Index := Index;
      end;
    end;
end;

procedure TfpgNiceColumns.Update(Item: TCollectionItem);
begin
  if not (Grid.HasHandle) then
    Exit; // ==> 	  
  if (Item <> nil)
    then FGrid.UpdateColumn(Item.Index)
    else FGrid.UpdateColumns;
end;


{ TInplaceEdit }

constructor TfpgNiceInplace.Create(AGrid: TfpgNiceGrid);
begin
  inherited Create(AGrid);
  FGrid := AGrid;
  Name:='EditGrid';
  BorderStyle:=ebsSingle;
  Left := 0;
  Top := 0;
  BuffTmp:=''; 
  Visible := False;
  OnChange:=@Change;  
end;

procedure TfpgNiceInplace.SetAlignment(Value: THorzAlign);
begin
  if (FAlignment <> Value) then
    FAlignment := Value;
end;

procedure TfpgNiceInplace.ShowEdit(X, Y: Integer);
var
  Rc: TfpgRect;
  Column: TfpgNiceColumn;
begin
  CellX := X;
  CellY := Y;
  Column := FGrid.FColumns[x];

  SetAlignment(Column.FHorzAlign);
  BackgroundColor := FGrid.GetCellColor(X, Y);
  
  FontDesc:=Column.Font;
  Text := Trim(FGrid.SafeGetCell(X, Y));
  BuffTmp:= Text;

  Rc := FGrid.GetCellRect(X, Y);
  Rc := FGrid.CellRectToClient(Rc);
  InflateRect(Rc, -2, -2);
  SetPosition(Rc.Left, Rc.Top,Rc.Width,Rc.Height);
  Visible:=true;
  SetFocus;
end;

procedure TfpgNiceInplace.HideEdit;
begin
  KillFocus;
  if Visible
    then Visible:=false;
  FGrid.IsEditing := False;
end;

procedure TfpgNiceInplace.Change(Sender: TObject);
begin
  if Visible then   //  Because when tfpgWidget is disabled it still receive keyboard msg
    FGrid.InternalSetCell(CellX, CellY, Text, True);
end;

procedure TfpgNiceInplace.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if not Visible then
Exit; //==>	  
  if not Visible then
    exit;
  case KeyCode of
    KeyEscape :
      begin
        Text:= BuffTmp;
        HideEdit;
      end;
    KeyReturn, KeyPEnter, KeyUp, keyDown:
      begin
        HideEdit;
      end;
  else
  begin	
    inherited HandleKeyPress(keycode, shiftstate, consumed);
    if (keycode= KeyLeft) and (FCursorPos=0) then 
      consumed:=true;
  end;  
  end;
end;


{ TfpgNiceGridSync }

constructor TfpgNiceGridSync.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnDeleteRow := @SyncDeleteRow;
  FOnInsertRow := @SyncInsertRow;
  FOnColRowChanged := @SyncColRow;
  FMasterGrid:=nil;
end;

procedure TfpgNiceGridSync.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FMasterGrid) and (Operation = opRemove)
    then FMasterGrid := nil;
  inherited;
end;

procedure TfpgNiceGridSync.SetMasterGrid(const Value: TfpgNiceGrid);
begin
  if (FMasterGrid <> Value) then
  begin
    FMasterGrid := Value;
    FMasterGrid.Sync := Self;
    FMasterGrid.RowCount := RowCount;
  end;
end;

procedure TfpgNiceGridSync.SetScrollBar(AKind: TfpgScrollbar; AMax, APos, AStep: Integer);
begin
  if (AKind = FVScrollBar) and Assigned(FMasterGrid) then
    FMasterGrid.VertOffset := APos;
end;

procedure TfpgNiceGridSync.ShowHideScrollBar(HorzVisible,
  VertVisible: Boolean);
begin
  FVScrollBar.Visible:= VertVisible;
  FHScrollBar.Visible:=  HorzVisible;      
  FHScrollBar.Enabled:=false; 
end;

procedure TfpgNiceGridSync.SyncColRow(Sender: TObject; ACol, ARow: Integer);
begin
  if Assigned(FMasterGrid)
    then FMasterGrid.Row := ARow;
end;

procedure TfpgNiceGridSync.SyncDeleteRow(Sender: TObject; ARow: Integer);
begin
  if Assigned(FMasterGrid)
    then FMasterGrid.DeleteRow(ARow);
end;

procedure TfpgNiceGridSync.SyncInsertRow(Sender: TObject; ARow: Integer);
begin
  if Assigned(FMasterGrid) then
  begin
    if (ARow = FMasterGrid.RowCount)
      then FMasterGrid.AddRow
      else FMasterGrid.InsertRow(ARow);
  end;
end;

{ TfpgMergeCell }

constructor TfpgMergeCell.Create;
begin
  inherited Create;
  Font := fpgStyle.DefaultFont.FontDesc;
end;

destructor TfpgMergeCell.Destroy;
begin
  inherited Destroy;
end;

end.
