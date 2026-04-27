unit fpg_miglayout;

{$I fpg_defines.inc}

{
  MigLayout v11 Port for fpGUI

  This is a port of MigLayout v11.4.2 from Java to Object Pascal.
  Unlike the Java version which supports multiple toolkits (Swing, SWT, JavaFX),
  this implementation works directly with TfpgWidgetBase - no wrapper abstraction needed.

  Based on: prototypes/miglayout/mig-java-latest/core/src/main/java/net/miginfocom/layout/Grid.java
}

interface

uses
  Classes, SysUtils, Math, Generics.Collections, DateUtils,
  fpg_base,
  fpg_widget,
  fpg_layouttypes,
  fpg_layoutmanager,
  fpg_migconstraint,
  fpg_mig_lc,
  fpg_mig_ac,
  fpg_mig_cc,
  fpg_mig_dimconstraint,
  fpg_mig_boundsize,
  fpg_mig_unitvalue,
  fpg_mig_resizeconstraint,
  fpg_mig_layoututil,
  fpg_mig_platformdefaults;

type
  { Forward declarations for Grid inner classes }
  TfpgMigCompWrap = class;
  TfpgMigCell = class;
  TfpgMigLinkedDimGroup = class;
  TfpgMigGrid = class;

  { Helper record for baseline calculations }
  TfpgMigAboveBelow = record
    MaxAbove: Integer;
    MaxBelow: Integer;
  end;

  { Specialized lists and arrays }
  TfpgMigCompWrapList = specialize TObjectList<TfpgMigCompWrap>;
  TfpgMigCellList = specialize TObjectList<TfpgMigCell>;
  TfpgMigLinkedDimGroupList = specialize TObjectList<TfpgMigLinkedDimGroup>;
  TfpgMigCellMap = specialize TDictionary<Integer, TfpgMigCell>;
  TfpgMigOccupiedCellsSet = specialize TDictionary<Integer, Boolean>;  // Set of occupied cell keys for O(1) lookup
  TfpgMigIntegerList = specialize TList<Integer>;
  TfpgMigIntArray = array of Integer;
  TfpgMigCCMap = specialize TDictionary<TfpgWidgetBase, TfpgMigCC>;
  TfpgMigSizeArray = array[0..2] of Integer;  // [min,pref,max]
  TfpgMigBooleanArray = array of Boolean;
  TfpgMigLinkedDimGroupListArray = array of TfpgMigLinkedDimGroupList;
  TfpgWidgetBaseList = specialize TList<TfpgWidgetBase>;


  { CompWrap - wraps a TfpgWidgetBase with its CC constraint
    Caches min/pref/max sizes and gap information }
  TfpgMigCompWrap = class
  private
    FComp: TfpgWidgetBase;
    FCC: TfpgMigCC;
    FEHideMode: Integer;  // Effective hide mode (<=0 means visible)
    FUseVisualPadding: Boolean;
    FSizesOk: Boolean;
    FIsAbsolute: Boolean;

    FGaps: array[0..3] of array[0..2] of Integer;  // [top,left,bottom,right][min,pref,max]
    FHasGaps: Boolean;

    FHorSizes: array[0..2] of Integer;  // [min,pref,max]
    FVerSizes: array[0..2] of Integer;

    FX, FY, FW, FH: Integer;  // Bounds

    FForcedPushGaps: Integer;  // 1=before, 2=after (bitwise)

    procedure ValidateSize;
    function GetSize(ABoundSize: TfpgMigBoundSize; ASizeType: Integer; AIsHor: Boolean;
                     AUseVP: Boolean; ASizeHint: Integer): Integer;
    procedure MergeGapSizes(const ASizes: array of Integer; AIsHor, AIsTL: Boolean);
    procedure CalcGaps(ABefore: TfpgWidgetBase; ABeforeCC: TfpgMigCC;
                       AAfter: TfpgWidgetBase; AAfterCC: TfpgMigCC;
                       const ATag: string; AFlowX, AIsLTR: Boolean);
  public
    constructor Create(AComp: TfpgWidgetBase; ACC: TfpgMigCC; AEHideMode: Integer; AUseVisualPadding: Boolean);
    destructor Destroy; override;

    { Public for testing }
    procedure CorrectMinMax(var ASizes: array of Integer);
    function ConstrainSize(ASize: Integer): Integer;
    function GetGapIx(AIsHor, AIsTL: Boolean): Integer;
    function Filter(ASizeType, ASize: Integer): Integer;
    function GetSizes(AIsHor: Boolean): PInteger;  // Returns pointer to [min,pref,max] array
    procedure InvalidateSizes;
    function GetSize(ASizeType: Integer; AIsHor: Boolean): Integer;  // Size without gaps
    function GetSizeInclGaps(ASizeType: Integer; AIsHor: Boolean): Integer;
    function GetGapBefore(ASizeType: Integer; AIsHor: Boolean): Integer;
    function GetGapAfter(ASizeType: Integer; AIsHor: Boolean): Integer;
    function GetGaps(AIsHor, AIsBefore: Boolean): PInteger;
    function GetBaseline(ASizeType: Integer): Integer;
    function HasBaseline: Boolean;
    function IsPushGap(AIsHor, AIsBefore: Boolean): Boolean;
    procedure SetDimBounds(AStart, ASize: Integer; AIsHor: Boolean);
    procedure TransferBounds(AAddVisualPadding: Boolean);

    property Comp: TfpgWidgetBase read FComp;
    property CC: TfpgMigCC read FCC;
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
    property Width: Integer read FW write FW;
    property Height: Integer read FH write FH;
  end;

  { Cell - holds component wraps in a grid cell }
  TfpgMigCell = class
  private
    FSpanX, FSpanY: Integer;
    FFlowX: Boolean;
    FCompWraps: TfpgMigCompWrapList;
    FHasTagged: Boolean;
  public
    constructor Create(ACompWrap: TfpgMigCompWrap); overload;
    constructor Create(ASpanX, ASpanY: Integer; AFlowX: Boolean); overload;
    constructor Create(ACompWrap: TfpgMigCompWrap; ASpanX, ASpanY: Integer; AFlowX: Boolean); overload;
    destructor Destroy; override;

    property SpanX: Integer read FSpanX;
    property SpanY: Integer read FSpanY;
    property FlowX: Boolean read FFlowX;
    property CompWraps: TfpgMigCompWrapList read FCompWraps;
    property HasTagged: Boolean read FHasTagged write FHasTagged;
  end;

  { LinkedDimGroup - components sharing layout properties }
  TfpgMigLinkedDimGroup = class
  private
    const
      TYPE_SERIAL = 0;
      TYPE_PARALLEL = 1;
      TYPE_BASELINE = 2;
  private
    FLinkCtx: string;
    FSpan: Integer;
    FLinkType: Integer;
    FIsHor, FFromEnd: Boolean;
    FCompWraps: TfpgMigCompWrapList;
    FLStart, FLSize: Integer;
  public
    constructor Create(const ALinkCtx: string; ASpan, ALinkType: Integer; AIsHor, AFromEnd: Boolean);
    destructor Destroy; override;

    procedure AddCompWrap(ACompWrap: TfpgMigCompWrap);
    function GetMinPrefMax: TfpgMigIntArray;
    procedure Layout(ADC: TfpgMigDimConstraint; AStart, ASize, ASpanCount: Integer);

    property Span: Integer read FSpan;
    property CompWraps: TfpgMigCompWrapList read FCompWraps;
    property LStart: Integer read FLStart write FLStart;  // Layout start position
    property LSize: Integer read FLSize write FLSize;     // Layout size
    property FromEnd: Boolean read FFromEnd;              // Layout direction
  end;



  { Grid - the main layout engine }
  TfpgMigGrid = class
  private
    const
      { Port of Grid.java GROW_100 constant - line 45
        Default grow weight array for when fill is enabled }
      GROW_100: array[0..0] of Single = (100.0);
  private
    FLC: TfpgMigLC;
    FRowConstr, FColConstr: TfpgMigAC;
    FContainer: TfpgWidgetBase;
    FGrid: TfpgMigCellMap;
    FOccupiedCells: TfpgMigOccupiedCellsSet;  // Cache of occupied cell keys for O(1) lookup
    FRowIndexes, FColIndexes: TfpgMigIntegerList;
    FColGroupLists, FRowGroupLists: array of TfpgMigLinkedDimGroupList;
    FWidth, FHeight: array[0..2] of Integer;
    FColFlowSpecs, FRowFlowSpecs: TfpgMigFlowSizeSpec;

    { Port of Grid.java growXs, growYs fields - line 115
      Default grow weights for columns and rows (from push or fill) }
    FGrowXs, FGrowYs: TfpgMigFloatArray;

    { Port of Grid.java dockOffX, dockOffY fields - line 118
      Number of docking rows/columns at the edges of the grid }
    FDockOffX, FDockOffY: Integer;

    { Port of Grid.java debugRects field - line 108
      If debug is on, contains bounds for cells to paint }
    FDebugRects: specialize TList<TfpgRect>;

    procedure checkSizeCalcs(refWidth, refHeight: Integer);
    procedure calcGridSizes(refWidth, refHeight: Integer);

    { Build row and column index lists from grid }
    procedure BuildIndexes;

    { Check if a cell position is occupied by a spanning component }
    function IsCellOccupied(ACellX, ACellY: Integer): Boolean;

    { Mark cells as occupied by a spanning component - O(spanX * spanY) }
    procedure MarkCellsOccupied(ACellX, ACellY, ASpanX, ASpanY: Integer);

    { Port of Grid.java addDockingCell() - line 1592
      Adds a docking cell outside the normal cell indexes
      @param ADockInsets Current dock insets [top, left, bottom, right] - will be updated
      @param ASide Dock side: 0=north/top, 1=west/left, 2=south/bottom, 3=east/right
      @param ACW Component wrap to add to docking cell }
    procedure AddDockingCell(var ADockInsets: array of Integer; ASide: Integer; ACW: TfpgMigCompWrap);

    { Port of Grid.java getDockInsets() - line 693
      Counts how many docking rows/columns are before the normal grid
      @param AIndexes Row or column index list
      @returns Count of indexes less than -MAX_GRID }
    class function GetDockInsets(AIndexes: TfpgMigIntegerList): Integer;

    { Port of Grid.java hasDocks() - line 1409
      Checks if grid has any docking components
      @returns True if there are docking components }
    function HasDocks: Boolean;

    { Build dimension groups from component constraints }
    procedure BuildDimensionGroups;

    { Sort components in cells based on platform-specific button order }
    procedure SortCellsByPlatform;

    procedure LayoutInOneDim(ARefSize: Integer; AAlign: TfpgMigUnitValue; AIsRows: Boolean; ADefGrowW: TfpgMigFloatArray);

    { Grid position encoding/decoding helpers }
    class function EncodeCellKey(AX, AY: Integer): Integer;
    class procedure DecodeCellKey(AKey: Integer; out AX, AY: Integer);

    { Helper methods for size calculations }
    class function GetTotalSizeParallel(const ACompWraps: TfpgMigCompWrapList;
                                       ASizeType: Integer; AIsHor: Boolean): Integer;
    class function GetTotalSizeSerial(const ACompWraps: TfpgMigCompWrapList;
                                     ASizeType: Integer; AIsHor: Boolean): Integer;
    class function GetBaselineAboveBelow(const ACompWraps: TfpgMigCompWrapList;
                                        ASizeType: Integer; ACenterBaseline: Boolean): TfpgMigAboveBelow;

    class procedure LayoutSerial(AParent: TfpgWidgetBase; ACompWraps: TfpgMigCompWrapList; ADC: TfpgMigDimConstraint; AStart, ASize: Integer; AIsHor: Boolean; ASpanCount: Integer; AFromEnd: Boolean);
    class procedure LayoutParallel(AParent: TfpgWidgetBase; ACompWraps: TfpgMigCompWrapList; ADC: TfpgMigDimConstraint; AStart, ASize: Integer; AIsHor: Boolean; ASpanCount: Integer; AFromEnd: Boolean);
    class procedure LayoutBaseline(AParent: TfpgWidgetBase; ACompWraps: TfpgMigCompWrapList; ADC: TfpgMigDimConstraint; AStart, ASize, ASizeType, ASpanCount: Integer);
    class procedure SetCompWrapBounds(AParent: TfpgWidgetBase; const AAllSizes: TfpgMigIntegerArray; ACompWraps: TfpgMigCompWrapList; ARowAlign: TfpgMigUnitValue;  AStart, ASize: Integer; AIsHor, AFromEnd: Boolean);
    class procedure SetCompWrapBoundsFromSizes(AParent: TfpgWidgetBase; const ASizes: TfpgMigSizeArrayArray; ACompWraps: TfpgMigCompWrapList; ARowAlign: TfpgMigUnitValue;  AStart, ASize: Integer; AIsHor, AFromEnd: Boolean);
    class function CorrectAlign(ACC: TfpgMigCC; ARowAlign: TfpgMigUnitValue; AIsHor, AFromEnd: Boolean): TfpgMigUnitValue;
    class function GetGaps(ACompWraps: TfpgMigCompWrapList; AIsHor: Boolean): TfpgMigSizeArrayArray;
    class function GetComponentSizes(ACompWraps: TfpgMigCompWrapList; AIsHor: Boolean): TfpgMigSizeArrayArray;
    class function GetComponentGapPush(ACompWraps: TfpgMigCompWrapList; AIsHor: Boolean): TfpgMigBooleanArray;
    class function GetComponentResizeConstraints(ACompWraps: TfpgMigCompWrapList; AIsHor: Boolean): TfpgMigResizeConstraintArray;

    { Calculate final dimension sizes using LayoutUtil.CalculateSerial }
    function CalculateDimensionSizes(const AIndexes: TfpgMigIntegerList;
                                     const APrefSizes: array of Integer;
                                     AAC: TfpgMigAC; ABounds: Integer;
                                     AIsHor: Boolean): TfpgMigIntegerArray;

    { Aggregate component grow/shrink into dimension constraints }
    procedure AggregateComponentConstraints(AResConstr: TfpgMigResizeConstraint;
                                           ADimIndex: Integer; AIsHor: Boolean);

    { Port of Grid.java calcRowsOrColsSizes() - line 1059
      Calculates Min, Preferred and Max size for the columns OR rows.
      @param AGroupsLists Array of LinkedDimGroup lists for each row/col
      @param ADefGrow Default grow weight if specs don't have grow (from push in CC)
      @param ARefSize Reference size for pixel calculations
      @param AIsHor True for columns, False for rows
      @returns FlowSizeSpec with alternating gap/component sizes and resize constraints }
    function CalcRowsOrColsSizes(AGroupsLists: array of TfpgMigLinkedDimGroupList;
                                 ADefGrow: TfpgMigFloatArray; ARefSize: Integer;
                                 AIsHor: Boolean): TfpgMigFlowSizeSpec;

    { Port of Grid.java adjustMinPrefForSpanningComps() - line 1420
      Adjusts min/pref sizes for components spanning multiple cells.
      @param ASpecs The row or column dimension constraints
      @param ADefPush Default push weights if specs don't have grow
      @param AFss The FlowSizeSpec to adjust
      @param AGroupsLists Array of LinkedDimGroup lists for each row/col }
    procedure AdjustMinPrefForSpanningComps(ASpecs: TfpgMigDimConstraintArray;
                                           ADefPush: TfpgMigFloatArray;
                                           AFss: TfpgMigFlowSizeSpec;
                                           AGroupsLists: array of TfpgMigLinkedDimGroupList);

    { Port of Grid.java mergeSizesGapsAndResConstrs() - line 2074
      Merges row/col sizes, gaps, and resize constraints into FlowSizeSpec }
    class function MergeSizesGapsAndResConstrs(AResConstr: array of TfpgMigResizeConstraint;
                                         AGapPush: array of Boolean;
                                         AMinPrefMaxSizes: array of TfpgMigSizeArray;
                                         AGapSizes: array of TfpgMigSizeArray): TfpgMigFlowSizeSpec;

    { Port of Grid.java getRowGaps() - line 1224
      Returns the row gaps in pixel sizes. One more than there are specs sent in. }
    function GetRowGaps(ASpecs: array of TfpgMigDimConstraint; ARefSize: Integer;
                       AIsHor: Boolean; var AFillInPushGaps: TfpgMigBooleanArray): TfpgMigSizeArrayArray;

    { Helper to get gaps between rows/columns from component constraints }
    function GetComponentGapsBetweenDims(AGapIndex: Integer; AIsHor: Boolean;
                                         ARefSize: Integer; const ADefGap: TfpgMigSizeArray): TfpgMigSizeArray;

    { Port of Grid.java correctMinMax() - line 2135
      Corrects a size array so min <= pref <= max }
    class procedure CorrectMinMax(var ASizes: TfpgMigSizeArray);

    { Port of Grid.java mergeSizes() - line 2104
      Merges two size arrays (takes max for each element) }
    class function MergeSizes(AOldValues, ANewValues: TfpgMigSizeArray): TfpgMigSizeArray;

    { Port of Grid.java getTotalGroupsSizeParallel() - line 2033
      Gets the combined size of all groups in parallel (max for min/pref, min for max) }
    class function GetTotalGroupsSizeParallel(AGroups: TfpgMigLinkedDimGroupList;
                                              ASizeType: Integer; ACountSpanning: Boolean): Integer;

    { Port of Grid.java constrainSize() - line 2130 }
    class function ConstrainSize(ASize: Integer): Integer;

    { Port of Grid.java getDefaultGrowWeights() - line 772
      Gets default grow weights based on push or fill flags and component grow weights
      @param AHasPush If any push gap is set
      @param AIsRows True for row dimension, False for column dimension
      @returns Grow weights array or nil if no growth }
    function GetDefaultGrowWeights(AHasPush: Boolean; AIsRows: Boolean): TfpgMigFloatArray;

    { Port of Grid.java divideIntoLinkedGroups() - line 1458
      Creates LinkedDimGroup objects for each row/column
      @param AIsRows True for rows, False for columns
      @returns Array of group lists, one for each row/column }
    function DivideIntoLinkedGroups(AIsRows: Boolean): TfpgMigLinkedDimGroupListArray;

    { Port of Grid.java convertSpanToSparseGrid() - line 1539
      Converts a span from dense grid coordinates to sparse grid coordinates
      @param ACurIx Current index in sparse grid
      @param ASpan Span in dense grid coordinates
      @param AIndexes Sorted list of actual sparse grid indexes
      @returns Converted span in sparse grid coordinates }
    class function ConvertSpanToSparseGrid(ACurIx, ASpan: Integer;
                                           const AIndexes: TfpgMigIntegerList): Integer;

    { Helper method for finding the dimension group containing a component
      Used for debug painting }
    function GetGroupContaining(const AGroupLists: array of TfpgMigLinkedDimGroupList;
                                ACompWrap: TfpgMigCompWrap): TfpgMigLinkedDimGroup;
  public
    constructor Create(AContainer: TfpgWidgetBase; ALC: TfpgMigLC;
                       ARowConstr, AColConstr: TfpgMigAC;
                       const ACCMap: TfpgMigCCMap;
                       AComponents: TfpgWidgetBaseList);
    destructor Destroy; override;

    { Main layout method - positions and sizes all components }
    function Layout(const ABounds: array of Integer; AAlignX, AAlignY: TfpgMigUnitValue;
                    ADebug: Boolean): Boolean;

    { Port of Grid.java paintDebug() - line 567
      Paints debug visualization (cell bounds and component outlines) }
    procedure PaintDebug(ACanvas: TfpgCanvasBase);

    { Get calculated dimensions }
    function GetWidth: TfpgMigIntArray;
    function GetHeight: TfpgMigIntArray;
  end;

  TfpgMigLayoutManager = class(TfpgBaseLayoutManager)
  private
    FLC: TfpgMigLC;
    FRowConstr: TfpgMigAC;
    FColConstr: TfpgMigAC;
    FIsLayingOut: Boolean;  // Prevents recursive layout calls
    FGrid: TfpgMigGrid;     // Cached grid instance for debug painting
    FDirty: Boolean;        // True when grid needs to be recreated
    FContainer: TfpgWidgetBase;  // Last container we laid out
    FLastComponentCount: Integer;  // Track component count to detect structural changes
    FComponentOrder: TfpgWidgetBaseList;  // Tracks order of AddLayoutComponent calls

    procedure SetLC(AValue: TfpgMigLC);
    procedure SetRowConstr(AValue: TfpgMigAC);
    procedure SetColConstr(AValue: TfpgMigAC);
  protected
    // TfpgBaseLayoutManager overrides
    function CreateDefaultConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint; override;
    procedure DoLayout(AContainer: TfpgWidgetBase); override;
    function DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize; override;
    function DoGetMinimumSize(AContainer: TfpgWidgetBase): TfpgSize; override;

    // Helper to calculate sizes from grid
    function CalculateGridSize(AContainer: TfpgWidgetBase; ASizeType: Integer): TfpgSize;
  public
    constructor Create; override;
    destructor Destroy; override;

    // TfpgBaseLayoutManager overrides
    procedure InvalidateLayout(AContainer: TfpgWidgetBase); override;
    procedure PaintDebug(AWidget: TfpgWidgetBase; ACanvas: TfpgCanvasBase); override;
    procedure AddLayoutComponent(AWidget: TfpgWidgetBase; AConstraint: TfpgLayoutConstraint); override;
    procedure RemoveLayoutComponent(AWidget: TfpgWidgetBase); override;

    { MigLayout v11 constraint properties }
    property LC: TfpgMigLC read FLC write SetLC;
    property RowConstraints: TfpgMigAC read FRowConstr write SetRowConstr;
    property ColumnConstraints: TfpgMigAC read FColConstr write SetColConstr;
  end;

implementation

const
  { Port of Grid.java MAX_GRID constant - line 56
    Maximum grid position for normal components }
  MAX_GRID = 30000;

  { Port of Grid.java MAX_DOCK_GRID constant - line 60
    Docking components use grid coordinates -MAX_DOCK_GRID -> 0 and MAX_GRID -> MAX_DOCK_GRID }
  MAX_DOCK_GRID = 32767;

var
  { Port of Grid.java DOCK_DIM_CONSTRAINT - line 48
    DimConstraint used for docked components }
  DOCK_DIM_CONSTRAINT: TfpgMigDimConstraint = nil;

{ Implementation of inner classes }

{ TfpgMigCell }

constructor TfpgMigCell.Create(ACompWrap: TfpgMigCompWrap);
begin
  Create(ACompWrap, 1, 1, True);
end;

constructor TfpgMigCell.Create(ASpanX, ASpanY: Integer; AFlowX: Boolean);
begin
  Create(nil, ASpanX, ASpanY, AFlowX);
end;

constructor TfpgMigCell.Create(ACompWrap: TfpgMigCompWrap; ASpanX, ASpanY: Integer; AFlowX: Boolean);
begin
  inherited Create;
  FSpanX := ASpanX;
  FSpanY := ASpanY;
  FFlowX := AFlowX;
  FCompWraps := TfpgMigCompWrapList.Create(True);  // Own CompWraps so they're freed
  if ACompWrap <> nil then
    FCompWraps.Add(ACompWrap);
  FHasTagged := False;
end;

destructor TfpgMigCell.Destroy;
begin
  FCompWraps.Free;
  inherited Destroy;
end;

{ TfpgMigCompWrap }

constructor TfpgMigCompWrap.Create(AComp: TfpgWidgetBase; ACC: TfpgMigCC;
  AEHideMode: Integer; AUseVisualPadding: Boolean);
var
  hBS, vBS: TfpgMigBoundSize;
  i: Integer;
begin
  inherited Create;
  FComp := AComp;
  FCC := ACC;
  FEHideMode := AEHideMode;
  FUseVisualPadding := AUseVisualPadding;
  FSizesOk := False;

  // Check if component has absolute (non-relative) sizes
  // Percentage-based and other relative constraints are not absolute
  if ACC <> nil then
    FIsAbsolute := ACC.Horizontal.GetSize.IsAbsolute and ACC.Vertical.GetSize.IsAbsolute
  else
    FIsAbsolute := False;

  FHasGaps := False;

  FX := NOT_SET;
  FY := NOT_SET;
  FW := NOT_SET;
  FH := NOT_SET;

  FForcedPushGaps := 0;

  // Don't calculate sizes eagerly in constructor - they will be calculated
  // lazily in ValidateSize() when needed during layout. This ensures
  // percentage-based constraints use correct reference values after
  // insets are accounted for.

  // Initialize gaps if hide mode > 1
  if AEHideMode > 1 then
  begin
    for i := 0 to 3 do
    begin
      FGaps[i][SIZE_MIN] := 0;
      FGaps[i][SIZE_PREF] := 0;
      FGaps[i][SIZE_MAX] := INF;
    end;
  end;
end;

destructor TfpgMigCompWrap.Destroy;
begin
  // FComp and FCC are not owned - don't free
  inherited Destroy;
end;

procedure TfpgMigCompWrap.ValidateSize;
var
  hBS, vBS: TfpgMigBoundSize;
  i: Integer;
begin
  // If sizes are already calculated and valid, skip
  if FSizesOk then
    Exit;

  // Calculate sizes now (lazy evaluation)
  // This happens during layout when we have proper context
  if (FEHideMode <= 0) and (FCC <> nil) then
  begin
    hBS := FCC.Horizontal.GetSize;
    vBS := FCC.Vertical.GetSize;

    for i := SIZE_MIN to SIZE_MAX do
    begin
      FHorSizes[i] := GetSize(hBS, i, True, FUseVisualPadding, -1);
      FVerSizes[i] := GetSize(vBS, i, False, FUseVisualPadding, -1);
    end;

    CorrectMinMax(FHorSizes);
    CorrectMinMax(FVerSizes);
  end
  else
  begin
    // Hidden component - zero sizes
    for i := SIZE_MIN to SIZE_MAX do
    begin
      FHorSizes[i] := 0;
      FVerSizes[i] := 0;
    end;
  end;

  FSizesOk := True;
end;

function TfpgMigCompWrap.GetSize(ABoundSize: TfpgMigBoundSize; ASizeType: Integer;
  AIsHor: Boolean; AUseVP: Boolean; ASizeHint: Integer): Integer;
var
  uv: TfpgMigUnitValue;
  sz: TfpgSize;
  refSize: Single;
begin
  // Get the UnitValue for this size type
  uv := nil;
  if ABoundSize <> nil then
  begin
    case ASizeType of
      SIZE_MIN: uv := ABoundSize.Min;
      SIZE_PREF: uv := ABoundSize.Preferred;
      SIZE_MAX: uv := ABoundSize.Max;
    end;
  end;

  // If no UnitValue, use widget's intrinsic size
  if uv = nil then
  begin
    case ASizeType of
      SIZE_MIN:
        if AIsHor then
          Result := IfThen(FComp.MinWidth > 0, FComp.MinWidth, 0)
        else
          Result := IfThen(FComp.MinHeight > 0, FComp.MinHeight, 0);

      SIZE_PREF:
        begin
          sz := FComp.PreferredSize;
          if AIsHor then
            Result := sz.W
          else
            Result := sz.H;
        end;

      SIZE_MAX:
        if AIsHor then
          Result := IfThen(FComp.MaxWidth > 0, FComp.MaxWidth, INF)
        else
          Result := IfThen(FComp.MaxHeight > 0, FComp.MaxHeight, INF);
    else
      Result := 0;
    end;
  end
  else
  begin
    if (FComp <> nil) and (FComp.Parent <> nil) then
    begin
      if AIsHor then
        refSize := FComp.Parent.ActualWidth
      else
        refSize := FComp.Parent.ActualHeight;
    end
    else
      refSize := ASizeHint; // Fallback, though likely -1

    Result := Round(uv.GetPixels(refSize, FComp.Parent, FComp));
  end;
end;

procedure TfpgMigCompWrap.CorrectMinMax(var ASizes: array of Integer);
begin
  // Ensure min <= pref <= max
  // Since MAX is almost always explicitly set, use that as the limit
  if ASizes[SIZE_MIN] > ASizes[SIZE_MAX] then
    ASizes[SIZE_MIN] := ASizes[SIZE_MAX];

  if ASizes[SIZE_PREF] < ASizes[SIZE_MIN] then
    ASizes[SIZE_PREF] := ASizes[SIZE_MIN];

  if ASizes[SIZE_PREF] > ASizes[SIZE_MAX] then
    ASizes[SIZE_PREF] := ASizes[SIZE_MAX];
end;

function TfpgMigCompWrap.GetSizes(AIsHor: Boolean): PInteger;
begin
  ValidateSize;
  if AIsHor then
    Result := @FHorSizes[0]
  else
    Result := @FVerSizes[0];
end;

procedure TfpgMigCompWrap.InvalidateSizes;
begin
  FSizesOk := False;
end;

function TfpgMigCompWrap.GetGapIx(AIsHor, AIsTL: Boolean): Integer;
begin
  if AIsHor then
    Result := IfThen(AIsTL, 1, 3)
  else
    Result := IfThen(AIsTL, 0, 2);
end;

procedure TfpgMigCompWrap.MergeGapSizes(const ASizes: array of Integer; AIsHor, AIsTL: Boolean);
var
  gapIx: Integer;
begin
  if Length(ASizes) = 0 then
    Exit;

  gapIx := GetGapIx(AIsHor, AIsTL);
  FHasGaps := True;

  // Initialize if not set
  if FGaps[gapIx][SIZE_MIN] = 0 then
  begin
    FGaps[gapIx][SIZE_MIN] := 0;
    FGaps[gapIx][SIZE_PREF] := 0;
    FGaps[gapIx][SIZE_MAX] := INF;
  end;

  // Merge: max of min/pref, min of max
  if Length(ASizes) > SIZE_MIN then
    FGaps[gapIx][SIZE_MIN] := Max(ASizes[SIZE_MIN], FGaps[gapIx][SIZE_MIN]);
  if Length(ASizes) > SIZE_PREF then
    FGaps[gapIx][SIZE_PREF] := Max(ASizes[SIZE_PREF], FGaps[gapIx][SIZE_PREF]);
  if Length(ASizes) > SIZE_MAX then
    FGaps[gapIx][SIZE_MAX] := Min(ASizes[SIZE_MAX], FGaps[gapIx][SIZE_MAX]);
end;

procedure TfpgMigCompWrap.CalcGaps(ABefore: TfpgWidgetBase; ABeforeCC: TfpgMigCC;
  AAfter: TfpgWidgetBase; AAfterCC: TfpgMigCC; const ATag: string; AFlowX,
  AIsLTR: Boolean);
var
  par: TfpgWidgetBase;
  parW, parH: Integer;
  befGap, aftGap: TfpgMigBoundSize;
  gaps: TfpgMigGapArray;
begin
  // Can't calculate gaps without CC
  if FCC = nil then
    Exit;

  // Get parent dimensions
  par := FComp.Parent;
  if par = nil then
    Exit;

  parW := par.ActualWidth;
  parH := par.ActualHeight;

  // Get gap constraints from adjacent components
  if (ABefore <> nil) and (ABeforeCC <> nil) then
  begin
    if AFlowX then
      befGap := ABeforeCC.Horizontal.GetGapAfter
    else
      befGap := ABeforeCC.Vertical.GetGapAfter;
  end
  else
    befGap := nil;

  if (AAfter <> nil) and (AAfterCC <> nil) then
  begin
    if AFlowX then
      aftGap := AAfterCC.Horizontal.GetGapBefore
    else
      aftGap := AAfterCC.Vertical.GetGapBefore;
  end
  else
    aftGap := nil;

  // Calculate and merge gaps for all four sides
  // Top gap (vertical, before)
  if AFlowX then
    gaps := FCC.Vertical.GetComponentGaps(par, FComp, befGap, nil, ATag, parH, 0, AIsLTR)
  else
    gaps := FCC.Vertical.GetComponentGaps(par, FComp, befGap, ABefore, ATag, parH, 0, AIsLTR);
  if Length(gaps) > 0 then
    MergeGapSizes(gaps, False, True);

  // Left gap (horizontal, before)
  if AFlowX then
    gaps := FCC.Horizontal.GetComponentGaps(par, FComp, befGap, ABefore, ATag, parW, 1, AIsLTR)
  else
    gaps := FCC.Horizontal.GetComponentGaps(par, FComp, befGap, nil, ATag, parW, 1, AIsLTR);
  if Length(gaps) > 0 then
    MergeGapSizes(gaps, True, True);

  // Bottom gap (vertical, after)
  if AFlowX then
    gaps := FCC.Vertical.GetComponentGaps(par, FComp, aftGap, nil, ATag, parH, 2, AIsLTR)
  else
    gaps := FCC.Vertical.GetComponentGaps(par, FComp, aftGap, AAfter, ATag, parH, 2, AIsLTR);
  if Length(gaps) > 0 then
    MergeGapSizes(gaps, False, False);

  // Right gap (horizontal, after)
  if AFlowX then
    gaps := FCC.Horizontal.GetComponentGaps(par, FComp, aftGap, AAfter, ATag, parW, 3, AIsLTR)
  else
    gaps := FCC.Horizontal.GetComponentGaps(par, FComp, aftGap, nil, ATag, parW, 3, AIsLTR);
  if Length(gaps) > 0 then
    MergeGapSizes(gaps, True, False);
end;

function TfpgMigCompWrap.Filter(ASizeType, ASize: Integer): Integer;
begin
  if ASize = NOT_SET then
  begin
    if ASizeType <> SIZE_MAX then
      Result := 0
    else
      Result := INF;
  end
  else
    Result := ConstrainSize(ASize);
end;

function TfpgMigCompWrap.ConstrainSize(ASize: Integer): Integer;
begin
  // Constrain size to valid range: 0 to INF
  if ASize > 0 then
  begin
    if ASize < INF then
      Result := ASize
    else
      Result := INF;
  end
  else
    Result := 0;
end;

function TfpgMigCompWrap.GetSize(ASizeType: Integer; AIsHor: Boolean): Integer;
begin
  Result := Filter(ASizeType, GetSizes(AIsHor)[ASizeType]);
end;

function TfpgMigCompWrap.GetSizeInclGaps(ASizeType: Integer; AIsHor: Boolean): Integer;
begin
  Result := Filter(ASizeType, GetGapBefore(ASizeType, AIsHor) +
                   GetSizes(AIsHor)[ASizeType] + GetGapAfter(ASizeType, AIsHor));
end;

function TfpgMigCompWrap.GetGapBefore(ASizeType: Integer; AIsHor: Boolean): Integer;
var
  gapIx: Integer;
begin
  if not FHasGaps then
    Exit(0);
  gapIx := GetGapIx(AIsHor, True);
  Result := Filter(ASizeType, FGaps[gapIx][ASizeType]);
end;

function TfpgMigCompWrap.GetGapAfter(ASizeType: Integer; AIsHor: Boolean): Integer;
var
  gapIx: Integer;
begin
  if not FHasGaps then
    Exit(0);
  gapIx := GetGapIx(AIsHor, False);
  Result := Filter(ASizeType, FGaps[gapIx][ASizeType]);
end;

function TfpgMigCompWrap.GetGaps(AIsHor, AIsBefore: Boolean): PInteger;
var
  gapIx: Integer;
begin
  if not FHasGaps then
    Exit(nil);
  if AIsBefore then
    gapIx := GetGapIx(AIsHor, True)
  else
    gapIx := GetGapIx(AIsHor, False);
  Result := @FGaps[gapIx][0];
end;

function TfpgMigCompWrap.GetBaseline(ASizeType: Integer): Integer;
begin
  // TODO: Baseline support disabled for initial release
  // Proper baseline calculation needs to account for:
  // - Component borders/insets (buttons, edits)
  // - Padding/margins
  // - Text positioning within component (tlTop, tlCenter, tlBottom for labels)
  // - Component-specific rendering logic
  //
  // Future implementation should make GetBaseline() a virtual method in
  // TfpgWidgetBase so each widget can calculate its actual text baseline position.
  Result := 0;
end;

function TfpgMigCompWrap.HasBaseline: Boolean;
begin
  // TODO: Baseline support disabled for initial release
  // When HasBaseline returns False, MigLayout falls back to CENTER alignment
  // for rows (or whatever alignment is explicitly specified).
  //
  // To enable baseline support in the future:
  // 1. Make TfpgWidgetBase.GetBaseline(width, height) virtual
  // 2. Override in TfpgLabel, TfpgButton, TfpgEdit, etc.
  // 3. Calculate actual baseline position considering insets/padding/layout
  // 4. Return True here for components that implement GetBaseline properly
  Result := False;
end;

function TfpgMigCompWrap.IsPushGap(AIsHor, AIsBefore: Boolean): Boolean;
var
  dc: TfpgMigDimConstraint;
  bs: TfpgMigBoundSize;
  mask: Integer;
begin
  // Check for forced push gaps (bitwise: 1=before, 2=after)
  if AIsHor then
  begin
    if AIsBefore then
      mask := 1
    else
      mask := 2;

    if (mask and FForcedPushGaps) <> 0 then
      Exit(True);
  end;

  // If no CC, can't check gap push
  if FCC = nil then
    Exit(False);

  // Get dimension constraint and gap bound size
  if AIsHor then
    dc := FCC.Horizontal
  else
    dc := FCC.Vertical;

  if AIsBefore then
    bs := dc.GetGapBefore
  else
    bs := dc.GetGapAfter;

  Result := (bs <> nil) and bs.GapPush;
end;

procedure TfpgMigCompWrap.SetDimBounds(AStart, ASize: Integer; AIsHor: Boolean);
begin
  if AIsHor then
  begin
    if (AStart <> FX) or (ASize <> FW) then
    begin
      FX := AStart;
      FW := ASize;
      // TODO: Invalidate sizes if component has horizontal content bias
    end;
  end
  else
  begin
    if (AStart <> FY) or (ASize <> FH) then
    begin
      FY := AStart;
      FH := ASize;
      // TODO: Invalidate sizes if component has vertical content bias
    end;
  end;
end;

procedure TfpgMigCompWrap.TransferBounds(AAddVisualPadding: Boolean);
var
  compX, compY, compW, compH: Integer;
begin
  // Can't transfer bounds without component or constraint
  if (FComp = nil) or (FCC = nil) then
    Exit;

  // Don't transfer bounds for external components
  if FCC.IsExternal then
    Exit;

  // Ensure all bounds are set before transferring
  if (FX = NOT_SET) or (FY = NOT_SET) or (FW = NOT_SET) or (FH = NOT_SET) then
    Exit;

  compX := FX;
  compY := FY;
  compW := FW;
  compH := FH;

  // TODO: Implement visual padding support when needed
  // Visual padding allows components to have extra space for shadows, borders, etc.
  // that shouldn't be counted in layout calculations. fpGUI doesn't currently
  // expose visual padding, but this could be added through a custom interface.
  //
  // Java MigLayout implementation:
  //   int[] visualPadding = comp.getVisualPadding();
  //   if (visualPadding != null) {  // [top, left, bottom, right]
  //     compX -= visualPadding[1];     // Adjust for left padding
  //     compY -= visualPadding[0];     // Adjust for top padding
  //     compW += visualPadding[1] + visualPadding[3];  // left + right
  //     compH += visualPadding[0] + visualPadding[2];  // top + bottom
  //   }
  if AAddVisualPadding then
  begin
    // Visual padding not yet implemented in fpGUI
    // When implemented, adjust compX, compY, compW, compH here
  end;

  // Sanity check on bounds before transferring - prevent negative or zero sizes
  if (compW < 1) or (compH < 1) then
    Exit;

  // Transfer calculated bounds to the widget via ILayoutTarget interface
  (FComp as ILayoutTarget).MoveAndResize(compX, compY, compW, compH);
end;

{ TfpgMigLinkedDimGroup }

constructor TfpgMigLinkedDimGroup.Create(const ALinkCtx: string; ASpan, ALinkType: Integer;
  AIsHor, AFromEnd: Boolean);
begin
  inherited Create;
  FLinkCtx := ALinkCtx;
  FSpan := ASpan;
  FLinkType := ALinkType;
  FIsHor := AIsHor;
  FFromEnd := AFromEnd;
  FCompWraps := TfpgMigCompWrapList.Create(False);  // Don't own
  FLStart := 0;
  FLSize := 0;
end;

destructor TfpgMigLinkedDimGroup.Destroy;
begin
  FCompWraps.Free;
  inherited Destroy;
end;

procedure TfpgMigLinkedDimGroup.AddCompWrap(ACompWrap: TfpgMigCompWrap);
begin
  FCompWraps.Add(ACompWrap);
end;

function TfpgMigLinkedDimGroup.GetMinPrefMax: TfpgMigIntArray;
var
  sType: Integer;
  aboveBelow: TfpgMigAboveBelow;
begin
  SetLength(Result, 3);

  // If no components, return zeros with INF max
  if FCompWraps.Count = 0 then
  begin
    Result[SIZE_MIN] := 0;
    Result[SIZE_PREF] := 0;
    Result[SIZE_MAX] := INF;
    Exit;
  end;

  // Calculate min and pref sizes based on link type
  for sType := SIZE_MIN to SIZE_PREF do
  begin
    case FLinkType of
      TYPE_PARALLEL:
        // Parallel: components side-by-side, use maximum size
        Result[sType] := TfpgMigGrid.GetTotalSizeParallel(FCompWraps, sType, FIsHor);

      TYPE_BASELINE:
        begin
          // Baseline: align text on baseline, sum above + below
          aboveBelow := TfpgMigGrid.GetBaselineAboveBelow(FCompWraps, sType, False);
          Result[sType] := aboveBelow.MaxAbove + aboveBelow.MaxBelow;
        end;

      else  // TYPE_SERIAL
        // Serial: components in sequence, sum sizes
        Result[sType] := TfpgMigGrid.GetTotalSizeSerial(FCompWraps, sType, FIsHor);
    end;
  end;

  // Max is always INF
  Result[SIZE_MAX] := INF;
end;

procedure TfpgMigLinkedDimGroup.Layout(ADC: TfpgMigDimConstraint; AStart, ASize, ASpanCount: Integer);
var
  Parent: TfpgWidgetBase;
{$IFDEF MIGDEBUG}
  i: Integer;
{$ENDIF}
begin
  FLStart := AStart;
  FLSize := ASize;

  if FCompWraps.Count = 0 then
    Exit;

  Parent := FCompWraps[0].Comp.Parent;

  case FLinkType of
    TYPE_PARALLEL:
    begin
      TfpgMigGrid.LayoutParallel(Parent, FCompWraps, ADC, AStart, ASize, FIsHor, ASpanCount, FFromEnd);
    end;
    TYPE_BASELINE:
    begin
      TfpgMigGrid.LayoutBaseline(Parent, FCompWraps, ADC, AStart, ASize, SIZE_PREF, ASpanCount);
    end;
  else // TYPE_SERIAL
    TfpgMigGrid.LayoutSerial(Parent, FCompWraps, ADC, AStart, ASize, FIsHor, ASpanCount, FFromEnd);
  end;
end;


{ TfpgMigGrid }

constructor TfpgMigGrid.Create(AContainer: TfpgWidgetBase; ALC: TfpgMigLC;
  ARowConstr, AColConstr: TfpgMigAC; const ACCMap: TfpgMigCCMap;
  AComponents: TfpgWidgetBaseList);
var
  child: TfpgWidgetBase;
  cc: TfpgMigCC;
  cw: TfpgMigCompWrap;
  flowX, cellFlowX: Boolean;
  wrap: Integer;
  cellX, cellY: Integer;
  spanX, spanY: Integer;
  cellKey: Integer;
  cell: TfpgMigCell;
  ltr: Boolean;
  gridPair: TfpgMigCellMap.TDictionaryPair;
  cws: TfpgMigCompWrapList;
  cwBef, cwAft: TfpgWidgetBase;
  tag: string;
  ccBef, ccAft: TfpgMigCC;
  i: Integer;
  splitLeft: Integer;  // Remaining components to add to current cell due to Split
  dockInsets: array[0..3] of Integer;  // [top, left, bottom, right] for docking
  dockSide: Integer;
  hasPushX, hasPushY: Boolean;  // Port of Grid.java lines 147, 285-286
begin
  inherited Create;
  FContainer := AContainer;
  FLC := ALC;
  FRowConstr := ARowConstr;
  FColConstr := AColConstr;

  FGrid := TfpgMigCellMap.Create;
  FOccupiedCells := TfpgMigOccupiedCellsSet.Create;
  FRowIndexes := TfpgMigIntegerList.Create;
  FColIndexes := TfpgMigIntegerList.Create;
  FDebugRects := nil;  // Created on demand when debug is enabled
  FDockOffX := 0;
  FDockOffY := 0;

  // Determine flow direction and wrap setting
  if ALC <> nil then
  begin
    flowX := ALC.IsFlowX;
    wrap := ALC.GetWrapAfter;
  end
  else
  begin
    flowX := True;
    wrap := -1;  // No wrap
  end;

  { Port of Grid.java lines 155-156: Initialize dockInsets as nil
    Will be created on first docking component encounter }
  dockInsets[0] := 0;  // Will be set to -MAX_DOCK_GRID when first dock found
  dockInsets[1] := 0;
  dockInsets[2] := 0;
  dockInsets[3] := 0;

  // First pass: Create CompWraps for all managed widgets
  // Iterate in component order to preserve widget order, but only process those in ACCMap
  cellX := 0;
  cellY := 0;
  splitLeft := 0;  // Track remaining components to add to current cell due to Split
  hasPushX := False;
  hasPushY := False;

  if (ACCMap <> nil) and (AComponents <> nil) then
  begin
    // Iterate through the provided component list
    for child in AComponents do
    begin
      // Only process widgets that are visible and were added to ACCMap
      if not child.Visible or not ACCMap.ContainsKey(child) then
        Continue;

      // Get the constraint for this widget (may be nil)
      cc := ACCMap[child];

      { Port of Grid.java lines 285-286: Detect push weights from CC
        hasPushX/Y become True if any visible component has push weight set }
      if cc <> nil then
      begin
        if not IsNan(cc.PushWeightX) then
          hasPushX := True;
        if not IsNan(cc.PushWeightY) then
          hasPushY := True;
      end;

      { Port of Grid.java lines 196-203: Handle docking components
        Docking components bypass normal grid flow }
      if (cc <> nil) and (cc.Dock <> -1) then
      begin
        // Initialize dockInsets array on first docking component
        if dockInsets[0] = 0 then
        begin
          dockInsets[0] := -MAX_DOCK_GRID;
          dockInsets[1] := -MAX_DOCK_GRID;
          dockInsets[2] := MAX_DOCK_GRID;
          dockInsets[3] := MAX_DOCK_GRID;
        end;

        dockSide := cc.Dock;
        cw := TfpgMigCompWrap.Create(child, cc, 0, False);
        AddDockingCell(dockInsets, dockSide, cw);
        Continue;  // Skip normal grid placement
      end;

      // Create CompWrap for this widget
      cw := TfpgMigCompWrap.Create(child, cc, 0, False);

      // Determine grid position
      // TODO: Handle explicit grid coordinates from CC
      // For now, use simple flow placement

      // Get span from CC if available
      // Port of Grid.java lines 252-253: Clamp spans to prevent going beyond MAX_GRID
      spanX := 1;
      spanY := 1;
      if cc <> nil then
      begin
        // Clamp spanX to remaining grid width
        spanX := Min(cc.CellSpanX, MAX_GRID - cellX);
        // Clamp spanY to remaining grid height
        spanY := Min(cc.CellSpanY, MAX_GRID - cellY);
      end;

      // Encode grid position as integer key
      cellKey := EncodeCellKey(cellX, cellY);

      // Get or create cell at this position
      if not FGrid.TryGetValue(cellKey, cell) then
      begin
        // Check if CC has flowX override (Java: cellFlowX != null ? cellFlowX : lc.isFlowX())
        // CC.IsFlowX: 0=nil, 1=false, 2=true
        cellFlowX := flowX;  // Default to LC's flowX
        if (cc <> nil) and (cc.IsFlowX <> 0) then
          cellFlowX := (cc.IsFlowX = 2);  // Use CC's flowX if set

        cell := TfpgMigCell.Create(spanX, spanY, cellFlowX);
        FGrid.Add(cellKey, cell);

        // Mark all cells covered by this span as occupied
        MarkCellsOccupied(cellX, cellY, spanX, spanY);
      end;

      // Add CompWrap to cell
      cell.CompWraps.Add(cw);

      // Track if cell has tagged components
      if (cc <> nil) and (cc.GetTag <> '') then
        cell.HasTagged := True;

      // Handle Split: if this component has Split > 1, the next (Split-1) components
      // should be added to the SAME cell instead of advancing
      if (cc <> nil) and (cc.SplitParts > 1) and (splitLeft = 0) then
        splitLeft := cc.SplitParts - 1;  // Reserve space for additional components in this cell

      // Decrement splitLeft if we're in a split cell
      if splitLeft > 0 then
      begin
        Dec(splitLeft);
        // Don't advance to next cell - next component goes in same cell
        Continue;  // Skip cell advancement
      end;

      // Advance to next cell based on flow direction
      if flowX then
      begin
        // Horizontal flow
        cellX := cellX + spanX;

        // Check for component-level wrap constraint (manual wrap)
        // Port of Grid.java line 312-314: if (cc.isWrap())
        if (cc <> nil) and (cc.WrapGap <> nil) then
        begin
          cellX := 0;
          cellY := cellY + 1;
        end
        // Check for layout-level auto-wrap
        else if (wrap > 0) and (cellX >= wrap) then
        begin
          cellX := 0;
          cellY := cellY + 1;
        end;

        // Skip cells occupied by spanning components
        while IsCellOccupied(cellX, cellY) do
        begin
          cellX := cellX + 1;
          if (wrap > 0) and (cellX >= wrap) then
          begin
            cellX := 0;
            cellY := cellY + 1;
          end;
        end;
      end
      else
      begin
        // Vertical flow
        cellY := cellY + spanY;

        // Check for component-level wrap constraint (manual wrap)
        // Port of Grid.java line 312-314: if (cc.isWrap())
        if (cc <> nil) and (cc.WrapGap <> nil) then
        begin
          cellY := 0;
          cellX := cellX + 1;
        end
        // Check for layout-level auto-wrap
        else if (wrap > 0) and (cellY >= wrap) then
        begin
          cellY := 0;
          cellX := cellX + 1;
        end;

        // Skip cells occupied by spanning components
        while IsCellOccupied(cellX, cellY) do
        begin
          cellY := cellY + 1;
          if (wrap > 0) and (cellY >= wrap) then
          begin
            cellY := 0;
            cellX := cellX + 1;
          end;
        end;
      end;
    end;  // end for child in AComponents
  end;  // end if ACCMap <> nil

  // Second pass: Build row and column indexes
  BuildIndexes;

  { Port of Grid.java lines 385-386: Calculate docking offsets
    This determines how many rows/cols are used for docking }
  FDockOffX := GetDockInsets(FColIndexes);
  FDockOffY := GetDockInsets(FRowIndexes);

  // Sort cells by platform-specific button order (if any cells have tagged components)
  SortCellsByPlatform;

  // Third pass: Divide components into linked dimension groups (Port of Grid.java line 391-392)
  FColGroupLists := DivideIntoLinkedGroups(False);  // Columns
  FRowGroupLists := DivideIntoLinkedGroups(True);   // Rows

  // Fourth pass: Calculate default push weights (Port of Grid.java lines 394-395)
  // hasPushX/Y detected during first pass from CC.PushWeightX/Y
  FGrowXs := GetDefaultGrowWeights(hasPushX, False);  // For columns
  FGrowYs := GetDefaultGrowWeights(hasPushY, True);   // For rows

  // Calculate gaps now that the cells are filled
  ltr := TfpgMigLayoutUtil.IsLeftToRight(FLC, FContainer);
  for gridPair in FGrid do
  begin
    cell := gridPair.Value;
    cws := cell.CompWraps;
    for i := 0 to cws.Count - 1 do
    begin
      cw := cws[i];
      if i > 0 then
        cwBef := cws[i - 1].Comp
      else
        cwBef := nil;
      if i < cws.Count - 1 then
        cwAft := cws[i + 1].Comp
      else
        cwAft := nil;

      // Safely get CC from map - component might not have constraints
      if (ACCMap <> nil) and ACCMap.ContainsKey(cw.Comp) then
      begin
        cc := ACCMap[cw.Comp];
        if cc <> nil then
          tag := cc.GetTag
        else
          tag := '';
      end
      else
      begin
        cc := nil;
        tag := '';
      end;

      if (cwBef <> nil) and (ACCMap <> nil) and ACCMap.ContainsKey(cwBef) then
        ccBef := ACCMap[cwBef]
      else
        ccBef := nil;
      if (cwAft <> nil) and (ACCMap <> nil) and ACCMap.ContainsKey(cwAft) then
        ccAft := ACCMap[cwAft]
      else
        ccAft := nil;

      cw.CalcGaps(cwBef, ccBef, cwAft, ccAft, tag, cell.FlowX, ltr);
    end;
  end;

  // TODO: Create flow specs
end;

{ Grid position encoding/decoding }

class function TfpgMigGrid.EncodeCellKey(AX, AY: Integer): Integer;
begin
  // Encode (x, y) as single integer: key = (y << 16) | x
  // Supports grid positions up to 65535 x 65535
  Result := (AY shl 16) or (AX and $FFFF);
end;

class procedure TfpgMigGrid.DecodeCellKey(AKey: Integer; out AX, AY: Integer);
begin
  // Decode integer key back to (x, y)
  AX := AKey and $FFFF;
  AY := AKey shr 16;
end;

procedure TfpgMigGrid.BuildDimensionGroups;
var
  pair: TfpgMigCellMap.TDictionaryPair;
  cell: TfpgMigCell;
  i, j: Integer;
  cw: TfpgMigCompWrap;
  cc: TfpgMigCC;
  dc: TfpgMigDimConstraint;
  groupName: string;
  groupList: TfpgMigLinkedDimGroupList;
  group: TfpgMigLinkedDimGroup;
  found: Boolean;
begin
  // Initialize group arrays - one list per row/column
  SetLength(FRowGroupLists, FRowIndexes.Count);
  SetLength(FColGroupLists, FColIndexes.Count);

  for i := 0 to High(FRowGroupLists) do
    FRowGroupLists[i] := TfpgMigLinkedDimGroupList.Create(True);
  for i := 0 to High(FColGroupLists) do
    FColGroupLists[i] := TfpgMigLinkedDimGroupList.Create(True);

  // Scan all cells and build groups
  for pair in FGrid do
  begin
    cell := pair.Value;
    if cell = nil then
      Continue;

    for i := 0 to cell.CompWraps.Count - 1 do
    begin
      cw := cell.CompWraps[i];
      cc := cw.CC;

      if cc = nil then
        Continue;

      // Process horizontal dimension groups (columns)
      dc := cc.Horizontal;
      if dc <> nil then
      begin
        // Size group
        groupName := dc.GetSizeGroup;
        if groupName <> '' then
        begin
          // Find or create group across all columns
          group := nil;
          for j := 0 to High(FColGroupLists) do
          begin
            groupList := FColGroupLists[j];
            found := False;

            // Search for existing group with this name
            if groupList <> nil then
            begin
              for group in groupList do
              begin
                if group.FLinkCtx = groupName then
                begin
                  found := True;
                  Break;
                end;
              end;
            end;

            if found then
              Break;
          end;

          // Create group if not found
          if not found then
          begin
            group := TfpgMigLinkedDimGroup.Create(groupName, 1, TfpgMigLinkedDimGroup.TYPE_PARALLEL, True, False);
            // Add to first column's group list
            if FColGroupLists[0] <> nil then
              FColGroupLists[0].Add(group);
          end;

          // Add CompWrap to group
          if group <> nil then
            group.AddCompWrap(cw);
        end;

        // End group
        groupName := dc.GetEndGroup;
        if groupName <> '' then
        begin
          // Similar logic for end groups
          group := nil;
          for j := 0 to High(FColGroupLists) do
          begin
            groupList := FColGroupLists[j];
            found := False;

            if groupList <> nil then
            begin
              for group in groupList do
              begin
                if group.FLinkCtx = groupName then
                begin
                  found := True;
                  Break;
                end;
              end;
            end;

            if found then
              Break;
          end;

          if not found then
          begin
            group := TfpgMigLinkedDimGroup.Create(groupName, 1, TfpgMigLinkedDimGroup.TYPE_PARALLEL, True, True);
            if FColGroupLists[0] <> nil then
              FColGroupLists[0].Add(group);
          end;

          if group <> nil then
            group.AddCompWrap(cw);
        end;
      end;

      // Process vertical dimension groups (rows)
      dc := cc.Vertical;
      if dc <> nil then
      begin
        // Size group
        groupName := dc.GetSizeGroup;
        if groupName <> '' then
        begin
          group := nil;
          for j := 0 to High(FRowGroupLists) do
          begin
            groupList := FRowGroupLists[j];
            found := False;

            if groupList <> nil then
            begin
              for group in groupList do
              begin
                if group.FLinkCtx = groupName then
                begin
                  found := True;
                  Break;
                end;
              end;
            end;

            if found then
              Break;
          end;

          if not found then
          begin
            group := TfpgMigLinkedDimGroup.Create(groupName, 1, TfpgMigLinkedDimGroup.TYPE_PARALLEL, False, False);
            if FRowGroupLists[0] <> nil then
              FRowGroupLists[0].Add(group);
          end;

          if group <> nil then
            group.AddCompWrap(cw);
        end;

        // End group
        groupName := dc.GetEndGroup;
        if groupName <> '' then
        begin
          group := nil;
          for j := 0 to High(FRowGroupLists) do
          begin
            groupList := FRowGroupLists[j];
            found := False;

            if groupList <> nil then
            begin
              for group in groupList do
              begin
                if group.FLinkCtx = groupName then
                begin
                  found := True;
                  Break;
                end;
              end;
            end;

            if found then
              Break;
          end;

          if not found then
          begin
            group := TfpgMigLinkedDimGroup.Create(groupName, 1, TfpgMigLinkedDimGroup.TYPE_PARALLEL, False, True);
            if FRowGroupLists[0] <> nil then
              FRowGroupLists[0].Add(group);
          end;

          if group <> nil then
            group.AddCompWrap(cw);
        end;
      end;
    end;
  end;
end;

{ Port of Grid.java sortCellsByPlatform() - lines 780-864
  Sorts components in cells based on platform-specific button order }
procedure TfpgMigGrid.SortCellsByPlatform;
var
  order, orderLo: string;
  unrelSize: Integer;
  gapUnrel, flGap: array[0..2] of Integer;
  pair: TfpgMigCellMap.TDictionaryPair;
  cell: TfpgMigCell;
  sortedList: TfpgMigCompWrapList;
  prevCW, cw: TfpgMigCompWrap;
  nextUnrel, nextPush: Boolean;
  i, j, iSz, jSz: Integer;
  c: Char;
  tag: string;
begin
  // Get platform-specific button order string
  order := TfpgMigPlatformDefaults.GetButtonOrder;
  if order = '' then
    Exit;  // No button order defined

  orderLo := LowerCase(order);

  // Get unrelated gap size (use hardcoded value for now - typical unrelated gap is 11px)
  // TODO: Properly evaluate unrelated gap from PlatformDefaults
  unrelSize := 11;

  gapUnrel[0] := unrelSize;
  gapUnrel[1] := unrelSize;
  gapUnrel[2] := NOT_SET;

  flGap[0] := 0;
  flGap[1] := 0;
  flGap[2] := NOT_SET;

  // Process each cell
  for pair in FGrid do
  begin
    cell := pair.Value;
    if (cell = nil) or (not cell.HasTagged) then
      Continue;

    prevCW := nil;
    nextUnrel := False;
    nextPush := False;
    sortedList := TfpgMigCompWrapList.Create(False);  // Don't own objects
    try
      // Iterate through button order string
      iSz := Length(orderLo);
      for i := 1 to iSz do  // Pascal strings are 1-based
      begin
        // Early exit if all components are sorted
        if sortedList.Count >= cell.CompWraps.Count then
          Break;

        c := orderLo[i];
        if (c = '+') or (c = '_') then
        begin
          nextUnrel := True;
          if c = '+' then
            nextPush := True;
        end
        else
        begin
          tag := TfpgMigPlatformDefaults.GetTagForChar(c);
          if tag <> '' then
          begin
            // Find components with this tag
            jSz := cell.CompWraps.Count;
            for j := 0 to jSz - 1 do
            begin
              cw := cell.CompWraps[j];
              if (cw.CC <> nil) and (tag = cw.CC.GetTag) then
              begin
                // TODO: Implement adjustMinHorSizeUp for uppercase chars
                // if (order[i] >= 'A') and (order[i] <= 'Z') then
                //   cw.adjustMinHorSizeUp(...);

                sortedList.Add(cw);

                if nextUnrel then
                begin
                  if prevCW <> nil then
                    prevCW.MergeGapSizes(gapUnrel, cell.FlowX, False)
                  else
                    cw.MergeGapSizes(gapUnrel, cell.FlowX, True);

                  if nextPush then
                  begin
                    cw.FForcedPushGaps := 1;
                    nextUnrel := False;
                    nextPush := False;
                  end;
                end;

                // "unknown" components always get an Unrelated gap
                if c = 'u' then
                  nextUnrel := True;

                prevCW := cw;
              end;
            end;
          end;
        end;
      end;

      // Handle trailing push gap
      if sortedList.Count > 0 then
      begin
        cw := sortedList[sortedList.Count - 1];
        if nextUnrel then
        begin
          cw.MergeGapSizes(gapUnrel, cell.FlowX, False);
          if nextPush then
            cw.FForcedPushGaps := cw.FForcedPushGaps or 2;
        end;

        // Remove first and last gap if not set explicitly
        if (cw.CC <> nil) and (cw.CC.Horizontal.GetGapAfter = nil) then
          cw.MergeGapSizes(flGap, cell.FlowX, False);

        cw := sortedList[0];
        if (cw.CC <> nil) and (cw.CC.Horizontal.GetGapBefore = nil) then
          cw.MergeGapSizes(flGap, cell.FlowX, True);
      end;

      // Replace cell's CompWraps with sorted list
      if cell.CompWraps.Count = sortedList.Count then
      begin
        // All components were sorted - clear without freeing
        cell.CompWraps.OwnsObjects := False;
        try
          cell.CompWraps.Clear;
        finally
          cell.CompWraps.OwnsObjects := True;
        end;
      end
      else
      begin
        // Some components weren't tagged - remove only sorted ones without freeing
        cell.CompWraps.OwnsObjects := False;
        try
          for i := 0 to sortedList.Count - 1 do
            cell.CompWraps.Remove(sortedList[i]);
        finally
          cell.CompWraps.OwnsObjects := True;
        end;
      end;

      // Add sorted components back
      for i := 0 to sortedList.Count - 1 do
        cell.CompWraps.Add(sortedList[i]);
    finally
      sortedList.Free;
    end;
  end;
end;

procedure TfpgMigGrid.BuildIndexes;
var
  pair: TfpgMigCellMap.TDictionaryPair;
  cell: TfpgMigCell;
  cellKey: Integer;
  cellX, cellY: Integer;
  i, j, writePos: Integer;
  found: Boolean;
  tempRow, tempCol: Integer;
  spanIdx: Integer;
begin
  { Port of Grid.java: DON'T clear indexes - AddDockingCell() has already added docking indexes
    We need to merge the normal grid indexes with existing docking indexes }
  // FRowIndexes.Clear;  // REMOVED - would delete docking indexes added by AddDockingCell
  // FColIndexes.Clear;  // REMOVED - would delete docking indexes added by AddDockingCell

  // Collect unique row and column indexes from normal grid cells only.
  // IMPORTANT: Skip docking cells here - their indexes were already added by AddDockingCell.
  // DecodeCellKey doesn't handle negative coordinates correctly (shr is logical, not
  // arithmetic), so docking cell keys (which use negative row/col values) would decode
  // to incorrect large positive values, creating phantom rows/columns.
  for pair in FGrid do
  begin
    cellKey := pair.Key;
    cell := pair.Value;
    if cell = nil then
      Continue;

    DecodeCellKey(cellKey, cellX, cellY);

    // Skip docking cells - they have coordinates outside the normal grid range.
    // Normal cells always have coordinates in [0, MAX_GRID).
    // Docking cells decode to values >= MAX_GRID due to the encoding of negative values.
    if (cellX >= MAX_GRID) or (cellY >= MAX_GRID) then
      Continue;

    // Add row indexes for this cell and its span
    // For very large spans (INF_SIZE), only add the starting row
    if cell.SpanY >= MAX_GRID then
    begin
      found := False;
      for i := 0 to FRowIndexes.Count - 1 do
      begin
        if FRowIndexes[i] = cellY then
        begin
          found := True;
          Break;
        end;
      end;
      if not found then
        FRowIndexes.Add(cellY);
    end
    else
    begin
      // Normal span: add all spanned row indexes
      for spanIdx := 0 to cell.SpanY - 1 do
      begin
        found := False;
        for i := 0 to FRowIndexes.Count - 1 do
        begin
          if FRowIndexes[i] = (cellY + spanIdx) then
          begin
            found := True;
            Break;
          end;
        end;
        if not found then
          FRowIndexes.Add(cellY + spanIdx);
      end;
    end;

    // Add column indexes for this cell and its span

    // For very large spans (e.g., INF_SIZE/30000 from SpanX()), only add the starting column
    // The actual span will be calculated later based on how many real columns exist
    // This prevents creating thousands of unnecessary column indexes
    if cell.SpanX >= MAX_GRID then
    begin
      // Just add the starting column for infinite/very large spans
      found := False;
      for i := 0 to FColIndexes.Count - 1 do
      begin
        if FColIndexes[i] = cellX then
        begin
          found := True;
          Break;
        end;
      end;
      if not found then
      begin
        FColIndexes.Add(cellX);
      end;
    end
    else
    begin
      // Normal span: add all spanned column indexes
      for spanIdx := 0 to cell.SpanX - 1 do
      begin
        found := False;
        for i := 0 to FColIndexes.Count - 1 do
        begin
          if FColIndexes[i] = (cellX + spanIdx) then
          begin
            found := True;
            Break;
          end;
        end;
        if not found then
        begin
          FColIndexes.Add(cellX + spanIdx);
        end;
      end;
    end;
  end;

  // Sort row indexes using bubble sort (simple for small lists)
  for i := 0 to FRowIndexes.Count - 2 do
  begin
    for j := i + 1 to FRowIndexes.Count - 1 do
    begin
      if FRowIndexes[i] > FRowIndexes[j] then
      begin
        tempRow := FRowIndexes[i];
        FRowIndexes[i] := FRowIndexes[j];
        FRowIndexes[j] := tempRow;
      end;
    end;
  end;

  // Sort column indexes using bubble sort
  for i := 0 to FColIndexes.Count - 2 do
  begin
    for j := i + 1 to FColIndexes.Count - 1 do
    begin
      if FColIndexes[i] > FColIndexes[j] then
      begin
        tempCol := FColIndexes[i];
        FColIndexes[i] := FColIndexes[j];
        FColIndexes[j] := tempCol;
      end;
    end;
  end;

  // Remove consecutive duplicates from sorted row indexes.
  // AddDockingCell doesn't deduplicate (Java uses TreeSet which auto-deduplicates).
  if FRowIndexes.Count > 1 then
  begin
    writePos := 1;
    for i := 1 to FRowIndexes.Count - 1 do
    begin
      if FRowIndexes[i] <> FRowIndexes[writePos - 1] then
      begin
        FRowIndexes[writePos] := FRowIndexes[i];
        Inc(writePos);
      end;
    end;
    while FRowIndexes.Count > writePos do
      FRowIndexes.Delete(FRowIndexes.Count - 1);
  end;

  // Remove consecutive duplicates from sorted column indexes
  if FColIndexes.Count > 1 then
  begin
    writePos := 1;
    for i := 1 to FColIndexes.Count - 1 do
    begin
      if FColIndexes[i] <> FColIndexes[writePos - 1] then
      begin
        FColIndexes[writePos] := FColIndexes[i];
        Inc(writePos);
      end;
    end;
    while FColIndexes.Count > writePos do
      FColIndexes.Delete(FColIndexes.Count - 1);
  end;
end;

function TfpgMigGrid.IsCellOccupied(ACellX, ACellY: Integer): Boolean;
var
  cellKey: Integer;
begin
  // O(1) lookup using cached occupied cells dictionary
  cellKey := EncodeCellKey(ACellX, ACellY);
  Result := FOccupiedCells.ContainsKey(cellKey);
end;

procedure TfpgMigGrid.MarkCellsOccupied(ACellX, ACellY, ASpanX, ASpanY: Integer);
var
  x, y: Integer;
  cellKey: Integer;
begin
  // Mark all cells within the span as occupied - O(spanX * spanY) using dictionary
  for x := ACellX to ACellX + ASpanX - 1 do
    for y := ACellY to ACellY + ASpanY - 1 do
    begin
      cellKey := EncodeCellKey(x, y);
      // AddOrSetValue is safe - won't throw if key exists
      FOccupiedCells.AddOrSetValue(cellKey, True);
    end;
end;

{ Port of Grid.java addDockingCell() - line 1592 }
procedure TfpgMigGrid.AddDockingCell(var ADockInsets: array of Integer; ASide: Integer; ACW: TfpgMigCompWrap);
var
  r, c, spanX, spanY: Integer;
  cellKey: Integer;
  cell: TfpgMigCell;
begin
  spanX := 1;
  spanY := 1;

  case ASide of
    0, 2:  // North (top) or South (bottom)
      begin
        if ASide = 0 then
        begin
          r := ADockInsets[0];
          Inc(ADockInsets[0]);
        end
        else
        begin
          r := ADockInsets[2];
          Dec(ADockInsets[2]);
        end;
        c := ADockInsets[1];
        spanX := ADockInsets[3] - ADockInsets[1] + 1;  // The +1 is for cell 0
        FColIndexes.Add(ADockInsets[3]);  // Make sure there is a receiving cell
      end;

    1, 3:  // West (left) or East (right)
      begin
        if ASide = 1 then
        begin
          c := ADockInsets[1];
          Inc(ADockInsets[1]);
        end
        else
        begin
          c := ADockInsets[3];
          Dec(ADockInsets[3]);
        end;
        r := ADockInsets[0];
        spanY := ADockInsets[2] - ADockInsets[0] + 1;  // The +1 is for cell 0
        FRowIndexes.Add(ADockInsets[2]);  // Make sure there is a receiving cell
      end;
  else
    raise Exception.Create('Internal error: Invalid dock side');
  end;

  FRowIndexes.Add(r);
  FColIndexes.Add(c);

  cellKey := EncodeCellKey(c, r);
  cell := TfpgMigCell.Create(ACW, spanX, spanY, spanX > 1);
  FGrid.Add(cellKey, cell);
end;

{ Port of Grid.java getDockInsets() - line 693 }
class function TfpgMigGrid.GetDockInsets(AIndexes: TfpgMigIntegerList): Integer;
var
  i, idx: Integer;
begin
  Result := 0;
  for i := 0 to AIndexes.Count - 1 do
  begin
    idx := AIndexes[i];
    if idx < -MAX_GRID then
      Inc(Result)
    else
      Break;  // Since they are sorted we can break
  end;
end;

{ Port of Grid.java hasDocks() - line 1409 }
function TfpgMigGrid.HasDocks: Boolean;
begin
  Result := (FDockOffX > 0) or (FDockOffY > 0) or
            (FRowIndexes.Count > 0) and (FRowIndexes[FRowIndexes.Count - 1] > MAX_GRID) or
            (FColIndexes.Count > 0) and (FColIndexes[FColIndexes.Count - 1] > MAX_GRID);
end;

destructor TfpgMigGrid.Destroy;
var
  i: Integer;
  cell: TfpgMigCell;
begin
  // Free all cells in the grid (TDictionary doesn't own its values)
  for cell in FGrid.Values do
    cell.Free;
  FGrid.Free;
  FOccupiedCells.Free;
  FRowIndexes.Free;
  FColIndexes.Free;

  // Free group lists
  for i := 0 to High(FRowGroupLists) do
    FRowGroupLists[i].Free;
  SetLength(FRowGroupLists, 0);

  for i := 0 to High(FColGroupLists) do
    FColGroupLists[i].Free;
  SetLength(FColGroupLists, 0);

  FColFlowSpecs.Free;
  FRowFlowSpecs.Free;

  // Free debug rects if allocated (TfpgRect is a record, no element cleanup needed)
  FreeAndNil(FDebugRects);

  inherited Destroy;
end;

function TfpgMigGrid.Layout(const ABounds: array of Integer; AAlignX, AAlignY: TfpgMigUnitValue;
                    ADebug: Boolean): Boolean;
var
  pair: TfpgMigCellMap.TDictionaryPair;
  cell: TfpgMigCell;
  cw: TfpgMigCompWrap;
  addVisualPadding: Boolean;
  hGrp, vGrp: TfpgMigLinkedDimGroup;
  debugRect: TfpgRect;
begin
  Result := False;

  // Port of Grid.java line 479-480: Create debug rects list if debug enabled
  if ADebug then
  begin
    if FDebugRects = nil then
      FDebugRects := specialize TList<TfpgRect>.Create
    else
      FDebugRects.Clear;
  end;

  if FColFlowSpecs = nil then
    checkSizeCalcs(ABounds[2], ABounds[3]);

  // TODO: Port resetLinkValues

  LayoutInOneDim(ABounds[2], AAlignX, False, FGrowXs);
  LayoutInOneDim(ABounds[3], AAlignY, True, FGrowYs);

  // Final loop to adjust for container bounds and transfer to components
  addVisualPadding := FLC.IsVisualPadding;

  for pair in FGrid do
  begin
    cell := pair.Value;
    if cell = nil then Continue;

    for cw in cell.CompWraps do
    begin
      // TODO: Port end group logic and absolute position logic

      cw.X := cw.X + ABounds[0];
      cw.Y := cw.Y + ABounds[1];

      cw.transferBounds(addVisualPadding);

      // Port of Grid.java lines 553-564: Populate debugRects
      if ADebug then
      begin
        hGrp := GetGroupContaining(FColGroupLists, cw);
        vGrp := GetGroupContaining(FRowGroupLists, cw);

        if (hGrp <> nil) and (vGrp <> nil) then
        begin
          // Calculate cell rectangle bounds
          if hGrp.FromEnd then
            debugRect.Left := hGrp.LStart + ABounds[0] - hGrp.LSize
          else
            debugRect.Left := hGrp.LStart + ABounds[0];

          if vGrp.FromEnd then
            debugRect.Top := vGrp.LStart + ABounds[1] - vGrp.LSize
          else
            debugRect.Top := vGrp.LStart + ABounds[1];

          debugRect.Width := hGrp.LSize;
          debugRect.Height := vGrp.LSize;

          FDebugRects.Add(debugRect);
        end;
      end;

      // TODO: Port callback logic
    end;
  end;

  // TODO: Port the rest of the layout logic (absolute positioning, end groups, etc.)
end;

procedure TfpgMigGrid.checkSizeCalcs(refWidth, refHeight: Integer);
begin
  // This is a simplified port for now. The full Java version also handles
  // recalculating the grid if the container's size has changed significantly.
  if FColFlowSpecs = nil then
    calcGridSizes(refWidth, refHeight);
end;

procedure TfpgMigGrid.calcGridSizes(refWidth, refHeight: Integer);
var
  colSpecs, rowSpecs: TfpgMigFlowSizeSpec;
begin
  colSpecs := CalcRowsOrColsSizes(FColGroupLists, FGrowXs, refWidth, True);
  rowSpecs := CalcRowsOrColsSizes(FRowGroupLists, FGrowYs, refHeight, False);

  FreeAndNil(FColFlowSpecs);
  FreeAndNil(FRowFlowSpecs);

  FColFlowSpecs := colSpecs;
  FRowFlowSpecs := rowSpecs;

  // TODO: Port the rest of this method which calculates overall width/height
  // and adjusts for absolute positioned components.
end;

procedure TfpgMigGrid.AggregateComponentConstraints(AResConstr: TfpgMigResizeConstraint;
  ADimIndex: Integer; AIsHor: Boolean);
var
  pair: TfpgMigCellMap.TDictionaryPair;
  cell: TfpgMigCell;
  cellKey: Integer;
  cellX, cellY: Integer;
  i, dimPos: Integer;
  cw: TfpgMigCompWrap;
  compDim: TfpgMigDimConstraint;
  growPrio, shrinkPrio: Integer;
  growWeight, shrinkWeight: Single;
begin
  // Bounds check to prevent access violations when form is resized very small
  if AIsHor then
  begin
    if (ADimIndex < 0) or (ADimIndex >= FColIndexes.Count) then
      Exit;  // Index out of bounds, skip this dimension
    dimPos := FColIndexes[ADimIndex];
  end
  else
  begin
    if (ADimIndex < 0) or (ADimIndex >= FRowIndexes.Count) then
      Exit;  // Index out of bounds, skip this dimension
    dimPos := FRowIndexes[ADimIndex];
  end;

  // Iterate through all cells to find components in this dimension
  for pair in FGrid do
  begin
    cellKey := pair.Key;
    cell := pair.Value;
    if cell = nil then
      Continue;

    DecodeCellKey(cellKey, cellX, cellY);

    // Check if this cell is in the dimension we're aggregating
    if AIsHor and (cellX <> dimPos) then
      Continue;
    if not AIsHor and (cellY <> dimPos) then
      Continue;

    // Check all components in this cell
    for i := 0 to cell.CompWraps.Count - 1 do
    begin
      cw := cell.CompWraps[i];
      if (cw = nil) or (cw.FCC = nil) then
        Continue;

      // Get component's dimension constraint
      if AIsHor then
        compDim := cw.FCC.Horizontal
      else
        compDim := cw.FCC.Vertical;

      if compDim = nil then
        Continue;

      // Aggregate grow priority and weight (use max)
      growPrio := compDim.GetGrowPriority;
      if growPrio > AResConstr.GrowPrio then
        AResConstr.GrowPrio := growPrio;

      if compDim.HasGrowWeight then
      begin
        growWeight := compDim.GetGrowWeight;
        if IsNaN(AResConstr.Grow) or (growWeight > AResConstr.Grow) then
          AResConstr.Grow := growWeight;
      end;

      // Aggregate shrink priority and weight (use max)
      shrinkPrio := compDim.GetShrinkPriority;
      if shrinkPrio > AResConstr.ShrinkPrio then
        AResConstr.ShrinkPrio := shrinkPrio;

      if compDim.HasShrinkWeight then
      begin
        shrinkWeight := compDim.GetShrinkWeight;
        if IsNaN(AResConstr.Shrink) or (shrinkWeight > AResConstr.Shrink) then
          AResConstr.Shrink := shrinkWeight;
      end;
    end;
  end;
end;

{ Port of Grid.java correctMinMax() - line 2135 }
class procedure TfpgMigGrid.CorrectMinMax(var ASizes: TfpgMigSizeArray);
begin
  if ASizes[SIZE_MIN] > ASizes[SIZE_MAX] then
    ASizes[SIZE_MIN] := ASizes[SIZE_MAX];  // Since MAX is almost always explicitly set use that

  if ASizes[SIZE_PREF] < ASizes[SIZE_MIN] then
    ASizes[SIZE_PREF] := ASizes[SIZE_MIN];

  if ASizes[SIZE_PREF] > ASizes[SIZE_MAX] then
    ASizes[SIZE_PREF] := ASizes[SIZE_MAX];
end;

{ Port of Grid.java mergeSizes(int oldValue, int newValue, boolean toMax) - line 2119 }
class function TfpgMigGrid.MergeSizes(AOldValues, ANewValues: TfpgMigSizeArray): TfpgMigSizeArray;
var
  i: Integer;
  oldVal, newVal: Integer;
begin
  // TfpgMigSizeArray is a fixed-size array[0..2], no SetLength needed
  for i := 0 to 2 do
  begin
    oldVal := AOldValues[i];
    newVal := ANewValues[i];

    if (oldVal = NOT_SET) or (oldVal = newVal) then
      Result[i] := newVal
    else if newVal = NOT_SET then
      Result[i] := oldVal
    else
      Result[i] := Max(oldVal, newVal);  // toMax = true for merging gaps
  end;
end;

{ Port of Grid.java mergeSizesGapsAndResConstrs() - line 2074 }
class function TfpgMigGrid.MergeSizesGapsAndResConstrs(
    AResConstr: array of TfpgMigResizeConstraint;
    AGapPush: array of Boolean;
    AMinPrefMaxSizes: array of TfpgMigSizeArray;
    AGapSizes: array of TfpgMigSizeArray): TfpgMigFlowSizeSpec;
var
  sizes: TfpgMigSizeArrayArray;
  resConstsInclGaps: TfpgMigResizeConstraintArray;
  i, crIx: Integer;
begin
  // Make room for gaps around: [gap, comp, gap, comp, ..., gap]
  SetLength(sizes, (Length(AMinPrefMaxSizes) * 2) + 1);
  SetLength(resConstsInclGaps, Length(sizes));

  // First gap
  sizes[0] := AGapSizes[0];
  if (0 < Length(AGapPush)) and AGapPush[0] then
    resConstsInclGaps[0] := TfpgMigResizeConstraint.Create(200, 100.0, 50, 100.0) // GAP_RC_CONST_PUSH
  else
    resConstsInclGaps[0] := TfpgMigResizeConstraint.Create(200, 100.0, 50, NaN); // GAP_RC_CONST

  for i := 0 to High(AMinPrefMaxSizes) do
  begin
    crIx := (i * 2) + 1;

    // Component bounds and constraints
    resConstsInclGaps[crIx] := AResConstr[i];
    sizes[crIx] := AMinPrefMaxSizes[i];

    // Gap after component
    sizes[crIx + 1] := AGapSizes[i + 1];

    if ((i + 1) < Length(AGapPush)) and AGapPush[i + 1] then
      resConstsInclGaps[crIx + 1] := TfpgMigResizeConstraint.Create(200, 100.0, 50, 100.0) // GAP_RC_CONST_PUSH
    else
      resConstsInclGaps[crIx + 1] := TfpgMigResizeConstraint.Create(200, 100.0, 50, NaN); // GAP_RC_CONST
  end;

  Result := TfpgMigFlowSizeSpec.Create(sizes, resConstsInclGaps);
end;

{ Port of Grid.java getRowGaps() - line 1224 }
function TfpgMigGrid.GetRowGaps(ASpecs: array of TfpgMigDimConstraint; ARefSize: Integer;
  AIsHor: Boolean; var AFillInPushGaps: TfpgMigBooleanArray): TfpgMigSizeArrayArray;
var
  defGap: TfpgMigBoundSize;
  defGapArr: TfpgMigSizeArray;
  i, val: Integer;
  defIns: Boolean;
  firstGap, lastGap: TfpgMigUnitValue;
  specBefore, specAfter: TfpgMigDimConstraint;
  edgeBefore, edgeAfter: Boolean;
begin
  // Get default grid gap from LC or PlatformDefaults
  if AIsHor then
    defGap := FLC.GetGridGapX
  else
    defGap := FLC.GetGridGapY;

  if defGap = nil then
  begin
    if AIsHor then
      defGap := TfpgMigPlatformDefaults.GetDefaultHGap
    else
      defGap := TfpgMigPlatformDefaults.GetDefaultVGap;
  end;

  // Safely convert default gap to pixel sizes
  if defGap <> nil then
  begin
    if defGap.Min <> nil then
      defGapArr[SIZE_MIN] := Round(defGap.Min.GetPixels(ARefSize, FContainer, nil))
    else
      defGapArr[SIZE_MIN] := 0;

    if defGap.Preferred <> nil then
      defGapArr[SIZE_PREF] := Round(defGap.Preferred.GetPixels(ARefSize, FContainer, nil))
    else
      defGapArr[SIZE_PREF] := 0;

    if defGap.Max <> nil then
    begin
      val := Round(defGap.Max.GetPixels(ARefSize, FContainer, nil));
      defGapArr[SIZE_MAX] := IfThen(val = 0, INF, val);
    end
    else
      defGapArr[SIZE_MAX] := INF;
  end else
  begin
    defGapArr[0] := 0; defGapArr[1] := 0; defGapArr[2] := INF;
  end;

  SetLength(Result, Length(ASpecs) + 1);

  { Port of Grid.java line 1340: Get container insets for edge gaps
    defIns is True when there are no docking components }
  defIns := not HasDocks;
  firstGap := TfpgMigLayoutUtil.GetInsets(FLC, IfThen(AIsHor, 1, 0), defIns);
  lastGap  := TfpgMigLayoutUtil.GetInsets(FLC, IfThen(AIsHor, 3, 2), defIns);

  for i := 0 to High(Result) do
  begin
    { Port of Grid.java lines 1348-1355: No gap if between docking components }
    if i > 0 then
      specBefore := ASpecs[i - 1]
    else
      specBefore := nil;

    if i < Length(ASpecs) then
      specAfter := ASpecs[i]
    else
      specAfter := nil;

    edgeBefore := (specBefore = DOCK_DIM_CONSTRAINT) or (specBefore = nil);
    edgeAfter := (specAfter = DOCK_DIM_CONSTRAINT) or (specAfter = nil);

    if edgeBefore and edgeAfter then
    begin
      // No gap between docking components - set to zero
      Result[i][SIZE_MIN] := 0;
      Result[i][SIZE_PREF] := 0;
      Result[i][SIZE_MAX] := 0;
      Continue;
    end;

    if (i = 0) and (firstGap <> nil) then
    begin
      val := Round(firstGap.GetPixels(ARefSize, FContainer, nil));
      Result[i][0] := val; Result[i][1] := val; Result[i][2] := val;
    end
    else if (i = High(Result)) and (lastGap <> nil) then
    begin
      val := Round(lastGap.GetPixels(ARefSize, FContainer, nil));
      Result[i][0] := val; Result[i][1] := val; Result[i][2] := val;
    end
    else
    begin
      // Handle gaps between columns/rows from component constraints
      Result[i] := GetComponentGapsBetweenDims(i, AIsHor, ARefSize, defGapArr);
    end;

    // TODO: Port full gap push logic
    if (i > 0) and (i < Length(ASpecs)) then
    begin
      if (ASpecs[i-1] <> nil) and ASpecs[i-1].IsGapAfterPush then
        AFillInPushGaps[i] := True;
      if (ASpecs[i] <> nil) and ASpecs[i].IsGapBeforePush then
        AFillInPushGaps[i] := True;
    end;
  end;
end;

{ Helper to get gaps between rows/columns from component constraints }
function TfpgMigGrid.GetComponentGapsBetweenDims(AGapIndex: Integer; AIsHor: Boolean;
  ARefSize: Integer; const ADefGap: TfpgMigSizeArray): TfpgMigSizeArray;
var
  pair: TfpgMigCellMap.TDictionaryPair;
  cell: TfpgMigCell;
  cellKey, cellX, cellY: Integer;
  dimBefore, dimAfter: Integer;
  i: Integer;
  cw: TfpgMigCompWrap;
  compDim: TfpgMigDimConstraint;
  gapBoundSize: TfpgMigBoundSize;
  gapVal: Integer;
  maxGap: TfpgMigSizeArray;
begin
  // Start with default gap
  Result := ADefGap;
  maxGap[SIZE_MIN] := ADefGap[SIZE_MIN];
  maxGap[SIZE_PREF] := ADefGap[SIZE_PREF];
  maxGap[SIZE_MAX] := ADefGap[SIZE_MAX];

  // AGapIndex represents the gap between dimension (AGapIndex-1) and dimension (AGapIndex)
  if AGapIndex <= 0 then
    Exit; // First gap (before first row/col) - use default or insets

  if AIsHor then
  begin
    // Horizontal: gap between columns
    if (AGapIndex - 1 >= FColIndexes.Count) or (AGapIndex > FColIndexes.Count) then
      Exit; // Out of bounds

    if AGapIndex - 1 < FColIndexes.Count then
      dimBefore := FColIndexes[AGapIndex - 1]
    else
      dimBefore := -1;

    if AGapIndex < FColIndexes.Count then
      dimAfter := FColIndexes[AGapIndex]
    else
      dimAfter := -1;
  end
  else
  begin
    // Vertical: gap between rows
    if (AGapIndex - 1 >= FRowIndexes.Count) or (AGapIndex > FRowIndexes.Count) then
      Exit; // Out of bounds

    if AGapIndex - 1 < FRowIndexes.Count then
      dimBefore := FRowIndexes[AGapIndex - 1]
    else
      dimBefore := -1;

    if AGapIndex < FRowIndexes.Count then
      dimAfter := FRowIndexes[AGapIndex]
    else
      dimAfter := -1;
  end;

  // Iterate through all cells to find components and get their gaps
  for pair in FGrid do
  begin
    cellKey := pair.Key;
    cell := pair.Value;
    if cell = nil then
      Continue;

    DecodeCellKey(cellKey, cellX, cellY);

    // Check all components in this cell
    for i := 0 to cell.CompWraps.Count - 1 do
    begin
      cw := cell.CompWraps[i];
      if (cw = nil) or (cw.FCC = nil) then
        Continue;

      // Get the appropriate dimension constraint
      if AIsHor then
        compDim := cw.FCC.Horizontal
      else
        compDim := cw.FCC.Vertical;

      if compDim = nil then
        Continue;

      // Check if this component is in dimBefore (check GapAfter)
      if (dimBefore >= 0) and ((AIsHor and (cellX = dimBefore)) or (not AIsHor and (cellY = dimBefore))) then
      begin
        gapBoundSize := compDim.GetGapAfter;
        if (gapBoundSize <> nil) and (not gapBoundSize.IsUnset) then
        begin
          if gapBoundSize.Min <> nil then
          begin
            gapVal := Round(gapBoundSize.Min.GetPixels(ARefSize, FContainer, nil));
            if gapVal > maxGap[SIZE_MIN] then
              maxGap[SIZE_MIN] := gapVal;
          end;
          if gapBoundSize.Preferred <> nil then
          begin
            gapVal := Round(gapBoundSize.Preferred.GetPixels(ARefSize, FContainer, nil));
            if gapVal > maxGap[SIZE_PREF] then
              maxGap[SIZE_PREF] := gapVal;
          end;
          if gapBoundSize.Max <> nil then
          begin
            gapVal := Round(gapBoundSize.Max.GetPixels(ARefSize, FContainer, nil));
            if (gapVal > 0) and (gapVal < maxGap[SIZE_MAX]) then
              maxGap[SIZE_MAX] := gapVal;
          end;
        end;
      end;

      // Check if this component is in dimAfter (check GapBefore)
      if (dimAfter >= 0) and ((AIsHor and (cellX = dimAfter)) or (not AIsHor and (cellY = dimAfter))) then
      begin
        gapBoundSize := compDim.GetGapBefore;
        if (gapBoundSize <> nil) and (not gapBoundSize.IsUnset) then
        begin
          if gapBoundSize.Min <> nil then
          begin
            gapVal := Round(gapBoundSize.Min.GetPixels(ARefSize, FContainer, nil));
            if gapVal > maxGap[SIZE_MIN] then
              maxGap[SIZE_MIN] := gapVal;
          end;
          if gapBoundSize.Preferred <> nil then
          begin
            gapVal := Round(gapBoundSize.Preferred.GetPixels(ARefSize, FContainer, nil));
            if gapVal > maxGap[SIZE_PREF] then
              maxGap[SIZE_PREF] := gapVal;
          end;
          if gapBoundSize.Max <> nil then
          begin
            gapVal := Round(gapBoundSize.Max.GetPixels(ARefSize, FContainer, nil));
            if (gapVal > 0) and (gapVal < maxGap[SIZE_MAX]) then
              maxGap[SIZE_MAX] := gapVal;
          end;
        end;
      end;
    end;
  end;

  Result := maxGap;
end;

{ Port of Grid.java constrainSize() - line 2130 }
class function TfpgMigGrid.ConstrainSize(ASize: Integer): Integer;
begin
  if ASize > 0 then
  begin
    if ASize < INF then
      Result := ASize
    else
      Result := INF;
  end
  else
    Result := 0;
end;

{ Port of Grid.java getDefaultGrowWeights() - line 772 }
function TfpgMigGrid.GetDefaultGrowWeights(AHasPush: Boolean; AIsRows: Boolean): TfpgMigFloatArray;
var
  groupLists: array of TfpgMigLinkedDimGroupList;
  gwArr: TfpgMigFloatArray;
  i, j, c, ix: Integer;
  grps: TfpgMigLinkedDimGroupList;
  rowPushWeight: Single;
  grp: TfpgMigLinkedDimGroup;
  cw: TfpgMigCompWrap;
  pushWeight: Single;
begin
  // Port of Grid.java getDefaultPushWeights() - line 866
  // If no push and no fill, return empty array (no growth) - line 774
  if not AHasPush then
  begin
    if AIsRows then
    begin
      if not FLC.IsFillY then
      begin
        SetLength(Result, 0);
        Exit;
      end;
    end
    else
    begin
      if not FLC.IsFillX then
      begin
        SetLength(Result, 0);
        Exit;
      end;
    end;
  end;

  // Get the appropriate group lists
  if AIsRows then
    groupLists := FRowGroupLists
  else
    groupLists := FColGroupLists;

  // Start with GROW_100 (single element array with value 100.0)
  SetLength(gwArr, 1);
  gwArr[0] := GROW_100[0];

  // Loop through each row/column
  ix := 1;  // Index for alternating array (gaps at even, sizes at odd)
  for i := 0 to Length(groupLists) - 1 do
  begin
    grps := groupLists[i];
    rowPushWeight := NaN;  // null in Java

    // Find maximum push weight for this row/column from all components - line 879
    for j := 0 to grps.Count - 1 do
    begin
      grp := grps[j];
      for c := 0 to grp.CompWraps.Count - 1 do
      begin
        cw := grp.CompWraps[c];

        // Skip components without constraints
        if cw.FCC = nil then
          Continue;

        // Get push weight from CC (PushX or PushY) - line 879
        if AIsRows then
          pushWeight := cw.FCC.PushWeightY
        else
          pushWeight := cw.FCC.PushWeightX;

        // Keep max push weight for this row/column - line 880
        if IsNan(rowPushWeight) or (not IsNan(pushWeight) and (pushWeight > rowPushWeight)) then
          rowPushWeight := pushWeight;
      end;
    end;

    // If this row/column has a push weight, expand array and set it - line 885
    if not IsNan(rowPushWeight) then
    begin
      // First time we find a push weight, expand from GROW_100 to full array - line 886
      if Length(gwArr) = 1 then
      begin
        SetLength(gwArr, (Length(groupLists) * 2) + 1);
        // Fill with NaN
        for j := 0 to High(gwArr) do
          gwArr[j] := NaN;
      end;
      gwArr[ix] := rowPushWeight;  // Put push weight at odd index
    end;

    ix := ix + 2;  // Move to next odd index (skip gap at even index)
  end;

  Result := gwArr;
end;

{ Port of Grid.java divideIntoLinkedGroups() - line 1458 }
function TfpgMigGrid.DivideIntoLinkedGroups(AIsRows: Boolean): TfpgMigLinkedDimGroupListArray;
var
  fromEnd: Boolean;
  primIndexes, secIndexes: TfpgMigIntegerList;
  primDCs: TfpgMigDimConstraintArray;
  gIx, i, ix, cellKey: Integer;
  cellX, cellY, span, linkType: Integer;
  dc: TfpgMigDimConstraint;
  groupList: TfpgMigLinkedDimGroupList;
  cell: TfpgMigCell;
  isPar: Boolean;
  lg: TfpgMigLinkedDimGroup;
  cwIx, glIx: Integer;
  cw: TfpgMigCompWrap;
  linkCtx: string;
  foundList: Boolean;
  group: TfpgMigLinkedDimGroup;
begin
  if AIsRows then
    fromEnd := not FLC.IsTopToBottom
  else
    fromEnd := not TfpgMigLayoutUtil.IsLeftToRight(FLC, FContainer);

  if AIsRows then
  begin
    primIndexes := FRowIndexes;
    secIndexes := FColIndexes;
    primDCs := FRowConstr.GetConstraints;
  end
  else
  begin
    primIndexes := FColIndexes;
    secIndexes := FRowIndexes;
    primDCs := FColConstr.GetConstraints;
  end;

  SetLength(Result, primIndexes.Count);

  gIx := 0;
  for i := 0 to primIndexes.Count - 1 do
  begin
    { Port of Grid.java: Get dimension constraint for this row/column
      Use DOCK_DIM_CONSTRAINT for docking cells (outside -MAX_GRID to MAX_GRID range) }
    if (primIndexes[i] >= -MAX_GRID) and (primIndexes[i] <= MAX_GRID) then
    begin
      // Normal grid cell
      if primIndexes[i] < Length(primDCs) then
        dc := primDCs[primIndexes[i]]
      else if Length(primDCs) > 0 then
        dc := primDCs[High(primDCs)]
      else
        dc := nil;
    end
    else
    begin
      // Docking cell
      dc := DOCK_DIM_CONSTRAINT;
    end;

    groupList := TfpgMigLinkedDimGroupList.Create(True);
    Result[gIx] := groupList;
    Inc(gIx);

    for ix := 0 to secIndexes.Count - 1 do
    begin
      // Get cell at this position
      if AIsRows then
      begin
        cellX := secIndexes[ix];
        cellY := primIndexes[i];
      end
      else
      begin
        cellX := primIndexes[i];
        cellY := secIndexes[ix];
      end;

      cellKey := EncodeCellKey(cellX, cellY);
      if not FGrid.TryGetValue(cellKey, cell) then
        Continue;

      if (cell = nil) or (cell.CompWraps.Count = 0) then
        Continue;

      // Get span for this cell
      if AIsRows then
        span := cell.SpanY
      else
        span := cell.SpanX;

      // Convert span to sparse grid coordinates (for components spanning multiple actual columns/rows)
      // Port of Grid.java line 1488: span = convertSpanToSparseGrid(i, span, primIndexes);
      // NOTE: Java's 'i' is the VALUE from primIndexes (e.g., row index 0, 1, 2...), not a loop counter
      // In our Pascal code, primIndexes[i] gives us that value
      if AIsRows then
      begin
        span := ConvertSpanToSparseGrid(primIndexes[i], span, FRowIndexes);
      end
      else
        span := ConvertSpanToSparseGrid(primIndexes[i], span, FColIndexes);

      isPar := (cell.FlowX = AIsRows);

      // If serial flow with multiple components or spanning cell, create group for whole cell
      if ((not isPar) and (cell.CompWraps.Count > 1)) or (span > 1) then
      begin
        if isPar then
          linkType := TfpgMigLinkedDimGroup.TYPE_PARALLEL
        else
          linkType := TfpgMigLinkedDimGroup.TYPE_SERIAL;

        lg := TfpgMigLinkedDimGroup.Create('p,' + IntToStr(ix), span, linkType, not AIsRows, fromEnd);
        for cwIx := 0 to cell.CompWraps.Count - 1 do
          lg.AddCompWrap(cell.CompWraps[cwIx]);
        groupList.Add(lg);
      end
      else
      begin
        // Create individual groups for each component
        for cwIx := 0 to cell.CompWraps.Count - 1 do
        begin
          cw := cell.CompWraps[cwIx];

          // TODO: Handle baseline alignment
          linkCtx := '';  // For now, no special link context

          // Find existing group with same link context
          foundList := False;
          for glIx := 0 to groupList.Count - 1 do
          begin
            group := groupList[glIx];
            if group.FLinkCtx = linkCtx then
            begin
              group.AddCompWrap(cw);
              foundList := True;
              Break;
            end;
          end;

          // Create new group if none found
          if not foundList then
          begin
            linkType := TfpgMigLinkedDimGroup.TYPE_PARALLEL;
            lg := TfpgMigLinkedDimGroup.Create(linkCtx, 1, linkType, not AIsRows, fromEnd);
            lg.AddCompWrap(cw);
            groupList.Add(lg);
          end;
        end;
      end;
    end;
  end;

end;

procedure TfpgMigGrid.LayoutInOneDim(ARefSize: Integer; AAlign: TfpgMigUnitValue; AIsRows: Boolean; ADefGrowW: TfpgMigFloatArray);
var
  fromEnd: Boolean;
  primDCs: TfpgMigDimConstraintArray;
  fss: TfpgMigFlowSizeSpec;
  rowCols: array of TfpgMigLinkedDimGroupList;
  rowColSizes: TfpgMigIntegerArray;
  curPos: Integer;
  i, j, scIx, bIx, bIx2: Integer;
  linkedGroups: TfpgMigLinkedDimGroupList;
  primDC: TfpgMigDimConstraint;
  rowSize, groupSize: Integer;
  group: TfpgMigLinkedDimGroup;
begin
  if AIsRows then
    fromEnd := not FLC.IsTopToBottom
  else
    fromEnd := not TfpgMigLayoutUtil.IsLeftToRight(FLC, FContainer);

  if AIsRows then
    primDCs := FRowConstr.GetConstraints
  else
    primDCs := FColConstr.GetConstraints;

  if AIsRows then
    fss := FRowFlowSpecs
  else
    fss := FColFlowSpecs;

  if AIsRows then
    rowCols := FRowGroupLists
  else
    rowCols := FColGroupLists;

  // Safety check - if fss is nil, we can't proceed with layout
  if fss = nil then
    Exit;

  rowColSizes := TfpgMigLayoutUtil.CalculateSerial(fss.GetSizes, fss.ResConstsInclGaps, ADefGrowW, SIZE_PREF, ARefSize);

  // TODO: Port isDesignTime logic if needed

  if AAlign <> nil then
    curPos := Round(AAlign.GetPixels(ARefSize - TfpgMigLayoutUtil.Sum(rowColSizes), FContainer, nil))
  else
    curPos := 0;

  if fromEnd then
    curPos := ARefSize - curPos;

  for i := 0 to High(rowCols) do
  begin
    linkedGroups := rowCols[i];
    { Port of Grid.java line 1091: Adjust spec index by dock offset }
    if AIsRows then
      scIx := i - FDockOffY
    else
      scIx := i - FDockOffX;

    bIx := i shl 1;
    bIx2 := bIx + 1;

    // Safety check - ensure array indices are within bounds
    if bIx2 >= Length(rowColSizes) then
      Continue;

    if fromEnd then
      curPos := curPos - rowColSizes[bIx]
    else
      curPos := curPos + rowColSizes[bIx];

    { Port of Grid.java line 1098: Get dimension constraint, using DOCK_DIM_CONSTRAINT for dock cells }
    if (scIx >= 0) and (scIx < Length(primDCs)) then
      primDC := primDCs[scIx]
    else if Length(primDCs) > 0 then
      primDC := primDCs[High(primDCs)]
    else
      primDC := DOCK_DIM_CONSTRAINT;  // Use docking dimension constraint for dock cells

    rowSize := rowColSizes[bIx2];

    // Safety check - linkedGroups could be nil
    if linkedGroups <> nil then
    begin
      for j := 0 to linkedGroups.Count - 1 do
      begin
        group := linkedGroups[j];
        groupSize := rowSize;
        if group.Span > 1 then
          groupSize := TfpgMigLayoutUtil.Sum(rowColSizes, bIx2, Min((group.Span shl 1) - 1, Length(rowColSizes) - bIx2 - 1));

        group.Layout(primDC, curPos, groupSize, group.Span);
      end;
    end;

    if fromEnd then
      curPos := curPos - rowSize
    else
      curPos := curPos + rowSize;
  end;
end;

{ Port of Grid.java getTotalGroupsSizeParallel() - line 2033 }
class function TfpgMigGrid.GetTotalGroupsSizeParallel(AGroups: TfpgMigLinkedDimGroupList;
  ASizeType: Integer; ACountSpanning: Boolean): Integer;
var
  i: Integer;
  group: TfpgMigLinkedDimGroup;
  grpSize: Integer;
  groupSizes: TfpgMigIntArray;
begin
  if ASizeType = SIZE_MAX then
    Result := INF
  else
    Result := 0;

  for i := 0 to AGroups.Count - 1 do
  begin
    group := AGroups[i];
    if ACountSpanning or (group.Span = 1) then
    begin
      groupSizes := group.GetMinPrefMax;
      if Length(groupSizes) > ASizeType then
        grpSize := groupSizes[ASizeType]
      else
        grpSize := 0;

      if grpSize >= INF then
        Exit(INF);

      // For MAX: take minimum, for MIN/PREF: take maximum
      if ASizeType = SIZE_MAX then
      begin
        if grpSize < Result then
          Result := grpSize;
      end
      else
      begin
        if grpSize > Result then
          Result := grpSize;
      end;
    end;
  end;

  Result := ConstrainSize(Result);
end;

{ Port of Grid.java calcRowsOrColsSizes() - line 1059 }
function TfpgMigGrid.CalcRowsOrColsSizes(AGroupsLists: array of TfpgMigLinkedDimGroupList;
  ADefGrow: TfpgMigFloatArray; ARefSize: Integer; AIsHor: Boolean): TfpgMigFlowSizeSpec;
var
  primDCs: array of TfpgMigDimConstraint;
  rowColBoundSizes: array of TfpgMigSizeArray;
  allDCs: array of TfpgMigDimConstraint;
  resConstrs: array of TfpgMigResizeConstraint;
  fillInPushGaps: TfpgMigBooleanArray;
  gapSizes: TfpgMigSizeArrayArray;
  primIndexes: TfpgMigIntegerList;
  i, r, cellIx: Integer;
  groups: TfpgMigLinkedDimGroupList;
  rowColSizes: TfpgMigSizeArray;
begin
  // Get dimension constraints (lines 1061-1062 of Grid.java)
  if AIsHor then
  begin
    primDCs := FColConstr.GetConstraints;
    primIndexes := FColIndexes;
  end
  else
  begin
    primDCs := FRowConstr.GetConstraints;
    primIndexes := FRowIndexes;
  end;

  // Allocate arrays (lines 1064-1066)
  SetLength(rowColBoundSizes, primIndexes.Count);
  SetLength(allDCs, primIndexes.Count);

  // Calculate sizes for each row/column (lines 1068-1117)
  for r := 0 to primIndexes.Count - 1 do
  begin
    cellIx := primIndexes[r];

    { Port of Grid.java lines 1168-1172: Get dimension constraint for this row/column
      Use DOCK_DIM_CONSTRAINT for docking cells (outside -MAX_GRID to MAX_GRID range) }
    if (cellIx >= -MAX_GRID) and (cellIx <= MAX_GRID) then
    begin
      // Normal grid cell
      if cellIx < Length(primDCs) then
        allDCs[r] := primDCs[cellIx]
      else if Length(primDCs) > 0 then
        allDCs[r] := primDCs[High(primDCs)]
      else
        allDCs[r] := nil;
    end
    else
    begin
      // Docking cell
      allDCs[r] := DOCK_DIM_CONSTRAINT;
    end;

    // Get groups for this row/column (line 1079)
    if r < Length(AGroupsLists) then
      groups := AGroupsLists[r]
    else
      groups := nil;

    // Calculate group sizes using getTotalGroupsSizeParallel (lines 1081-1084)
    if (groups <> nil) and (groups.Count > 0) then
    begin
      rowColSizes[SIZE_MIN] := GetTotalGroupsSizeParallel(groups, SIZE_MIN, False);
      rowColSizes[SIZE_PREF] := GetTotalGroupsSizeParallel(groups, SIZE_PREF, False);
      rowColSizes[SIZE_MAX] := INF;
    end
    else
    begin
      rowColSizes[SIZE_MIN] := 0;
      rowColSizes[SIZE_PREF] := 0;
      rowColSizes[SIZE_MAX] := INF;
    end;

    // Correct min/max (line 1086)
    CorrectMinMax(rowColSizes);

    // TODO: Apply DimConstraint size overrides (lines 1087-1111)
    // For now, just use the calculated sizes
    rowColBoundSizes[r] := rowColSizes;

    // TODO: Handle size groups (line 1114)
  end;

  // TODO: Equalize size groups (lines 1119-1125)

  // Build resize constraints from dimension constraints (line 1128)
  SetLength(resConstrs, Length(allDCs));
  for i := 0 to High(allDCs) do
  begin
    if allDCs[i] <> nil then
    begin
      resConstrs[i] := TfpgMigResizeConstraint.Create(
        allDCs[i].GetShrinkPriority,
        allDCs[i].GetShrinkWeight,
        allDCs[i].GetGrowPriority,
        allDCs[i].GetGrowWeight
      );
    end
    else
    begin
      resConstrs[i] := TfpgMigResizeConstraint.Create;
    end;
  end;

  // Get gaps (lines 1130-1131)
  SetLength(fillInPushGaps, Length(allDCs) + 1);
  for i := 0 to High(fillInPushGaps) do
    fillInPushGaps[i] := False;
  gapSizes := GetRowGaps(allDCs, ARefSize, AIsHor, fillInPushGaps);

  // Merge sizes, gaps, and resize constraints (line 1134)
  Result := MergeSizesGapsAndResConstrs(resConstrs, fillInPushGaps, rowColBoundSizes, gapSizes);

  // Spanning components are not handled yet. Check and adjust the multi-row min/pref they enforce (line 1137)
  AdjustMinPrefForSpanningComps(allDCs, ADefGrow, Result, AGroupsLists);
end;

procedure TfpgMigGrid.AdjustMinPrefForSpanningComps(ASpecs: TfpgMigDimConstraintArray;
  ADefPush: TfpgMigFloatArray; AFss: TfpgMigFlowSizeSpec; AGroupsLists: array of TfpgMigLinkedDimGroupList);
var
  r, s, sIx, len, j, cSize, rowSize, newRowSize, eagerness: Integer;
  groups: TfpgMigLinkedDimGroupList;
  group: TfpgMigLinkedDimGroup;
  sizes: TfpgMigIntegerArray;
  sz: Integer;
begin
  // Port of Grid.java adjustMinPrefForSpanningComps() - lines 1420-1451

  // Since 3.7.3: Iterate from end to start. Will solve some multiple spanning components hard to solve problems
  for r := High(AGroupsLists) downto 0 do
  begin
    groups := AGroupsLists[r];
    if groups = nil then
      Continue;

    for group in groups do
    begin
      if group.Span = 1 then
        Continue;

      sizes := group.GetMinPrefMax;
      for s := SIZE_MIN to SIZE_PREF do
      begin
        cSize := sizes[s];
        if cSize = NOT_SET then
          Continue;

        rowSize := 0;
        sIx := (r shl 1) + 1;
        len := Min((group.Span shl 1), Length(AFss.GetSizes) - sIx) - 1;
        if len < 1 then
          Continue;

        for j := sIx to sIx + len - 1 do
        begin
          sz := AFss.GetSizes[j][s];
          if sz <> NOT_SET then
            rowSize := rowSize + sz;
        end;

        if (rowSize < cSize) and (len > 0) then
        begin
          eagerness := 0;
          newRowSize := 0;
          while (eagerness < 4) and (newRowSize < cSize) do
          begin
            newRowSize := AFss.ExpandSizes(ASpecs, ADefPush, cSize, sIx, len, s, eagerness);
            Inc(eagerness);
          end;
        end;
      end;
    end;
  end;
end;

class procedure TfpgMigGrid.LayoutParallel(AParent: TfpgWidgetBase; ACompWraps: TfpgMigCompWrapList; ADC: TfpgMigDimConstraint; AStart, ASize: Integer; AIsHor: Boolean; ASpanCount: Integer; AFromEnd: Boolean);
var
  sizes: TfpgMigSizeArrayArray;
  i, j: Integer;
  cw: TfpgMigCompWrap;
  cDc: TfpgMigDimConstraint;
  resConstr: TfpgMigResizeConstraintArray;
  sz: TfpgMigSizeArrayArray;
  growW: TfpgMigFloatArray;
  rowAlign: TfpgMigUnitValue;
  p: PInteger;
  calculatedSizes: TfpgMigIntegerArray;
begin
  SetLength(sizes, ACompWraps.Count);

  for i := 0 to ACompWraps.Count - 1 do
  begin
    cw := ACompWraps[i];

    // Skip components without constraints
    if cw.CC = nil then
      Continue;

    cDc := cw.CC.GetDimConstraint(AIsHor);

    SetLength(resConstr, 3);
    if cw.IsPushGap(AIsHor, True) then
      resConstr[0] := TfpgMigResizeConstraint.Create(200, 100.0, 50, 100.0) // GAP_RC_CONST_PUSH
    else
      resConstr[0] := TfpgMigResizeConstraint.Create(200, 100.0, 50, NaN); // GAP_RC_CONST;
    resConstr[1] := cDc.GetResize;
    if cw.IsPushGap(AIsHor, False) then
      resConstr[2] := TfpgMigResizeConstraint.Create(200, 100.0, 50, 100.0) // GAP_RC_CONST_PUSH
    else
      resConstr[2] := TfpgMigResizeConstraint.Create(200, 100.0, 50, NaN); // GAP_RC_CONST;

    SetLength(sz, 3);
    // For parallel layout, gaps are handled at row/column level, not component level
    // Component gaps would double-count with container insets
    sz[0][SIZE_MIN] := 0;
    sz[0][SIZE_PREF] := 0;
    sz[0][SIZE_MAX] := 0;

    p := cw.GetSizes(AIsHor);
    if p <> nil then
    begin
      sz[1][SIZE_MIN] := p[SIZE_MIN];
      sz[1][SIZE_PREF] := p[SIZE_PREF];
      sz[1][SIZE_MAX] := p[SIZE_MAX];
    end;

    sz[2][SIZE_MIN] := 0;
    sz[2][SIZE_PREF] := 0;
    sz[2][SIZE_MAX] := 0;


    if (ADC <> nil) and ADC.IsFill then
    begin
      SetLength(growW, 1);
      growW[0] := 100.0;
    end
    else
      SetLength(growW, 0);

    calculatedSizes := TfpgMigLayoutUtil.CalculateSerial(sz, resConstr, growW, SIZE_PREF, ASize);
    if Length(calculatedSizes) > 0 then sizes[i][0] := calculatedSizes[0];
    if Length(calculatedSizes) > 1 then sizes[i][1] := calculatedSizes[1];
    if Length(calculatedSizes) > 2 then sizes[i][2] := calculatedSizes[2];

    // Free temporary ResizeConstraint objects (all are newly created)
    resConstr[0].Free;
    resConstr[1].Free;
    resConstr[2].Free;
  end;

  rowAlign := ADC.GetAlignOrDefault(AIsHor);
  SetCompWrapBoundsFromSizes(AParent, sizes, ACompWraps, rowAlign, AStart, ASize, AIsHor, AFromEnd);
end;

class procedure TfpgMigGrid.LayoutBaseline(AParent: TfpgWidgetBase; ACompWraps: TfpgMigCompWrapList; ADC: TfpgMigDimConstraint; AStart, ASize, ASizeType, ASpanCount: Integer);
var
  aboveBelow: TfpgMigAboveBelow;
  i: Integer;
  cw: TfpgMigCompWrap;
  h: Integer;
  baseline: Integer;
begin
  aboveBelow := TfpgMigGrid.GetBaselineAboveBelow(ACompWraps, ASizeType, True);

  // layout with respect to baseline
  for i := 0 to ACompWraps.Count - 1 do
  begin
    cw := ACompWraps[i];
    h := cw.GetSize(ASizeType, False);
    baseline := cw.GetBaseline(ASizeType);
    cw.SetDimBounds(AStart + aboveBelow.MaxAbove - baseline, h, False);
  end;
end;

class procedure TfpgMigGrid.SetCompWrapBoundsFromSizes(AParent: TfpgWidgetBase; const ASizes: TfpgMigSizeArrayArray; ACompWraps: TfpgMigCompWrapList; ARowAlign: TfpgMigUnitValue;  AStart, ASize: Integer; AIsHor, AFromEnd: Boolean);
var
  i: Integer;
  cw: TfpgMigCompWrap;
  align: TfpgMigUnitValue;
  cSizes: TfpgMigSizeArray;
  gapBef, cSize, gapAft: Integer;
  cSt, slack, al: Integer;
begin
  // Port of Grid.java setCompWrapBounds() line 2126 (parallel layout version)
  // Each component is positioned INDEPENDENTLY from the base start position,
  // not serially accumulated like in serial layout
  for i := 0 to ACompWraps.Count - 1 do
  begin
    cw := ACompWraps[i];
    if cw.CC <> nil then
      align := CorrectAlign(cw.CC, ARowAlign, AIsHor, AFromEnd)
    else
      align := ARowAlign;

    cSizes := ASizes[i];  // Get [gapBef, size, gapAft] for this component
    gapBef := cSizes[0];
    cSize := cSizes[1];
    gapAft := cSizes[2];

    // Position from base start, not accumulated
    if AFromEnd then
      cSt := AStart - gapBef
    else
      cSt := AStart + gapBef;

    // Apply alignment within available slack
    slack := ASize - cSize - gapBef - gapAft;
    if (slack > 0) and (align <> nil) then
    begin
      al := Min(slack, Max(0, Round(align.GetPixels(slack, AParent, nil))));
      if AFromEnd then
        cSt := cSt - al
      else
        cSt := cSt + al;
    end;

    // Set bounds - each component at same base position (parallel layout)
    if AFromEnd then
      cw.SetDimBounds(cSt - cSize, cSize, AIsHor)
    else
      cw.SetDimBounds(cSt, cSize, AIsHor);
  end;
end;

class function TfpgMigGrid.GetGaps(ACompWraps: TfpgMigCompWrapList; AIsHor: Boolean): TfpgMigSizeArrayArray;
var
  i: Integer;
  gap1, gap2: PInteger;
  gap1arr, gap2arr: TfpgMigSizeArray;
begin
  SetLength(Result, ACompWraps.Count + 1);

  gap1 := ACompWraps[0].GetGaps(AIsHor, True);
  if gap1 <> nil then
  begin
    Result[0][SIZE_MIN] := gap1[SIZE_MIN];
    Result[0][SIZE_PREF] := gap1[SIZE_PREF];
    Result[0][SIZE_MAX] := gap1[SIZE_MAX];
  end;

  for i := 0 to ACompWraps.Count - 1 do
  begin
    gap1 := ACompWraps[i].GetGaps(AIsHor, False);

    if i < ACompWraps.Count - 1 then
      gap2 := ACompWraps[i + 1].GetGaps(AIsHor, True)
    else
      gap2 := nil;

    if gap1 <> nil then
    begin
      gap1arr[SIZE_MIN] := gap1[SIZE_MIN];
      gap1arr[SIZE_PREF] := gap1[SIZE_PREF];
      gap1arr[SIZE_MAX] := gap1[SIZE_MAX];
    end;

    if gap2 <> nil then
    begin
      gap2arr[SIZE_MIN] := gap2[SIZE_MIN];
      gap2arr[SIZE_PREF] := gap2[SIZE_PREF];
      gap2arr[SIZE_MAX] := gap2[SIZE_MAX];
    end;

    if (gap1 = nil) and (gap2 <> nil) then
      Result[i + 1] := gap2arr
    else if (gap1 <> nil) and (gap2 = nil) then
      Result[i + 1] := gap1arr
    else if (gap1 <> nil) and (gap2 <> nil) then
      Result[i + 1] := MergeSizes(gap1arr, gap2arr)
    else
    begin
      // no gaps
    end;
  end;
end;

class function TfpgMigGrid.GetComponentSizes(ACompWraps: TfpgMigCompWrapList; AIsHor: Boolean): TfpgMigSizeArrayArray;
var
  i: Integer;
  p: PInteger;
begin
  SetLength(Result, ACompWraps.Count);
  for i := 0 to ACompWraps.Count - 1 do
  begin
    p := ACompWraps[i].GetSizes(AIsHor);
    Result[i][SIZE_MIN] := p[SIZE_MIN];
    Result[i][SIZE_PREF] := p[SIZE_PREF];
    Result[i][SIZE_MAX] := p[SIZE_MAX];
  end;
end;

class function TfpgMigGrid.GetComponentGapPush(ACompWraps: TfpgMigCompWrapList; AIsHor: Boolean): TfpgMigBooleanArray;
var
  i: Integer;
  push: Boolean;
begin
  SetLength(Result, ACompWraps.Count + 1);
  for i := 0 to ACompWraps.Count do
  begin
    if i > 0 then
      push := ACompWraps[i - 1].IsPushGap(AIsHor, False)
    else
      push := False;

    if not push and (i < ACompWraps.Count) then
      push := ACompWraps[i].IsPushGap(AIsHor, True);

    Result[i] := push;
  end;
end;

class function TfpgMigGrid.GetComponentResizeConstraints(ACompWraps: TfpgMigCompWrapList; AIsHor: Boolean): TfpgMigResizeConstraintArray;
var
  i: Integer;
  cc: TfpgMigCC;
  dc: TfpgMigDimConstraint;
  dock: Integer;
begin
  SetLength(Result, ACompWraps.Count);
  for i := 0 to ACompWraps.Count - 1 do
  begin
    cc := ACompWraps[i].CC;
    if cc = nil then
      Continue;
    dc := cc.GetDimConstraint(AIsHor);
    Result[i] := dc.Resize;

    { Port of Grid.java lines 1300-1305: Always grow docking components in the correct dimension
      - North/South (dock 0/2) grow horizontally (AIsHor = True)
      - West/East (dock 1/3) grow vertically (AIsHor = False) }
    dock := cc.Dock;
    if (AIsHor and ((dock = 0) or (dock = 2))) or
       (not AIsHor and ((dock = 1) or (dock = 3))) then
    begin
      // Create new resize constraint with 100% grow weight in docking dimension
      Result[i] := TfpgMigResizeConstraint.Create(
        Result[i].ShrinkPrio,
        Result[i].Shrink,
        Result[i].GrowPrio,
        100.0  // WEIGHT_100
      );
    end;
  end;
end;

function TfpgMigGrid.CalculateDimensionSizes(const AIndexes: TfpgMigIntegerList;
  const APrefSizes: array of Integer; AAC: TfpgMigAC; ABounds: Integer;
  AIsHor: Boolean): TfpgMigIntegerArray;
var
  count, i, gapSize, totalGaps: Integer;
  sizeMatrix: TfpgMigSizeArrayArray;
  resConstr: TfpgMigResizeConstraintArray;
  pushWeights: TfpgMigFloatArray;
  dimConstr: TfpgMigDimConstraint;
  dimConstraints: TfpgMigDimConstraintArray;
begin
  count := AIndexes.Count;
  if count = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  // Build size matrix [min, pref, max] for each dimension
  SetLength(sizeMatrix, count);
  for i := 0 to count - 1 do
  begin
    sizeMatrix[i][SIZE_MIN] := APrefSizes[i];     // For now, min = pref
    sizeMatrix[i][SIZE_PREF] := APrefSizes[i];
    sizeMatrix[i][SIZE_MAX] := INF;          // Max is unlimited
  end;

  // Get constraints from AC
  if AAC <> nil then
    dimConstraints := AAC.GetConstraints
  else
    SetLength(dimConstraints, 0);

  // Build ResizeConstraint array from AC + component constraints
  SetLength(resConstr, count);
  for i := 0 to count - 1 do
  begin
    // Start with AC constraints (or defaults)
    if (i < Length(dimConstraints)) and (dimConstraints[i] <> nil) then
    begin
      dimConstr := dimConstraints[i];
      resConstr[i] := TfpgMigResizeConstraint.Create(
        dimConstr.GetShrinkPriority,
        dimConstr.GetShrinkWeight,
        dimConstr.GetGrowPriority,
        dimConstr.GetGrowWeight
      );
    end
    else
    begin
      // Default: no grow, default shrink (Java v11 behavior)
      resConstr[i] := TfpgMigResizeConstraint.Create;
    end;

    // Now aggregate grow/shrink from components in this row/column
    // This is done in Java MigLayout - components affect their column/row
    AggregateComponentConstraints(resConstr[i], i, AIsHor);
  end;

  // Default push weights (nil for now)
  SetLength(pushWeights, 0);

  // Calculate gap size (TODO: get from LC)
  gapSize := 6;
  totalGaps := (count - 1) * gapSize;

  // Call CalculateSerial to distribute space
  Result := TfpgMigLayoutUtil.CalculateSerial(
    sizeMatrix,
    resConstr,
    pushWeights,
    SIZE_PREF,
    ABounds - totalGaps  // Subtract gaps from available space
  );

  // Cleanup
  for i := 0 to count - 1 do
  begin
    resConstr[i].Free;
  end;
end;

function TfpgMigGrid.GetWidth: TfpgMigIntArray;
begin
  SetLength(Result, 0);
  SetLength(Result, 3);
  Result[SIZE_MIN] := FWidth[SIZE_MIN];
  Result[SIZE_PREF] := FWidth[SIZE_PREF];
  Result[SIZE_MAX] := FWidth[SIZE_MAX];
end;

function TfpgMigGrid.GetHeight: TfpgMigIntArray;
begin
  SetLength(Result, 3);
  Result[SIZE_MIN] := FHeight[SIZE_MIN];
  Result[SIZE_PREF] := FHeight[SIZE_PREF];
  Result[SIZE_MAX] := FHeight[SIZE_MAX];
end;

{ TfpgMigGrid - Static helper methods for layout }

class procedure TfpgMigGrid.LayoutSerial(AParent: TfpgWidgetBase; ACompWraps: TfpgMigCompWrapList; ADC: TfpgMigDimConstraint; AStart, ASize: Integer; AIsHor: Boolean; ASpanCount: Integer; AFromEnd: Boolean);
var
  fss: TfpgMigFlowSizeSpec;
  growW: TfpgMigFloatArray;
  sizes: TfpgMigIntegerArray;
  gaps: TfpgMigSizeArrayArray;
begin
  // For single-component spanning layouts, use zero gaps so the component fills the entire span
  if ACompWraps.Count = 1 then
  begin
    SetLength(gaps, 2);  // gap before and gap after
    gaps[0][SIZE_MIN] := 0;
    gaps[0][SIZE_PREF] := 0;
    gaps[0][SIZE_MAX] := INF;
    gaps[1][SIZE_MIN] := 0;
    gaps[1][SIZE_PREF] := 0;
    gaps[1][SIZE_MAX] := INF;
  end
  else
    gaps := GetGaps(ACompWraps, AIsHor);

  fss := MergeSizesGapsAndResConstrs(
            GetComponentResizeConstraints(ACompWraps, AIsHor),
            GetComponentGapPush(ACompWraps, AIsHor),
            GetComponentSizes(ACompWraps, AIsHor),
            gaps
         );

  // Port of Grid.java line 2066: Enable growth only if fill is set
  // Spanning does NOT automatically enable growth - component keeps preferred size
  // and is aligned within the spanned space
  if (ADC <> nil) and ADC.IsFill then
  begin
    SetLength(growW, 1);
    growW[0] := 100.0;
  end
  else
  begin
    SetLength(growW, 0);
  end;

  sizes := TfpgMigLayoutUtil.CalculateSerial(fss.GetSizes, fss.ResConstsInclGaps, growW, SIZE_PREF, ASize);
  SetCompWrapBounds(AParent, sizes, ACompWraps, ADC.GetAlignOrDefault(AIsHor), AStart, ASize, AIsHor, AFromEnd);

  // Free the FlowSizeSpec object created by MergeSizesGapsAndResConstrs
  fss.Free;
end;

class procedure TfpgMigGrid.SetCompWrapBounds(AParent: TfpgWidgetBase; const AAllSizes: TfpgMigIntegerArray; ACompWraps: TfpgMigCompWrapList; ARowAlign: TfpgMigUnitValue;  AStart, ASize: Integer; AIsHor, AFromEnd: Boolean);
var
  totSize, i, bIx: Integer;
  cw: TfpgMigCompWrap;
  align: TfpgMigUnitValue;
  cSt, slack, al: Integer;
begin
  totSize := TfpgMigLayoutUtil.Sum(AAllSizes);
  if (ACompWraps.Count > 0) and (ACompWraps[0].CC <> nil) then
    align := CorrectAlign(ACompWraps[0].CC, ARowAlign, AIsHor, AFromEnd)
  else
    align := ARowAlign;

  cSt := AStart;
  slack := ASize - totSize;

  if (slack > 0) and (align <> nil) then
  begin
    al := Min(slack, Max(0, Round(align.GetPixels(slack, AParent, nil))));
    if AFromEnd then
      cSt := cSt - al
    else
      cSt := cSt + al;
  end;

  bIx := 0;
  for i := 0 to ACompWraps.Count - 1 do
  begin
    cw := ACompWraps[i];
    if AFromEnd then
    begin
      cSt := cSt - AAllSizes[bIx]; // gap
      Inc(bIx);
      cw.SetDimBounds(cSt - AAllSizes[bIx], AAllSizes[bIx], AIsHor);
      cSt := cSt - AAllSizes[bIx];
      Inc(bIx);
    end
    else
    begin
      cSt := cSt + AAllSizes[bIx]; // gap
      Inc(bIx);
      cw.SetDimBounds(cSt, AAllSizes[bIx], AIsHor);
      cSt := cSt + AAllSizes[bIx];
      Inc(bIx);
    end;
  end;
end;

class function TfpgMigGrid.CorrectAlign(ACC: TfpgMigCC; ARowAlign: TfpgMigUnitValue; AIsHor, AFromEnd: Boolean): TfpgMigUnitValue;
var
  align: TfpgMigUnitValue;
  dc: TfpgMigDimConstraint;
begin
  // Safety check - return row align if no component constraints
  if ACC = nil then
  begin
    Result := ARowAlign;
    Exit;
  end;

  if AIsHor then
    align := ACC.Horizontal.GetAlign
  else
    align := ACC.Vertical.GetAlign;

  if align = nil then
    align := ARowAlign;

    dc := ACC.GetDimConstraint(False);
    if (dc <> nil) and (dc.GetAlignOrDefault(False) = UnitValueBaselineIdentity) then
    begin
      // TODO: Handle baseline alignment
    end;

  if AFromEnd then
  begin
    if align = UnitValueLeft then
      align := UnitValueRight
    else if align = UnitValueRight then
      align := UnitValueLeft;
  end;
  Result := align;
end;


{ TfpgMigGrid - Helper methods }

class function TfpgMigGrid.GetTotalSizeParallel(const ACompWraps: TfpgMigCompWrapList;
  ASizeType: Integer; AIsHor: Boolean): Integer;
var
  i: Integer;
  cw: TfpgMigCompWrap;
  cwSize: Integer;
begin
  // For parallel layout (components side-by-side):
  // - For MIN/PREF: return maximum size
  // - For MAX: return minimum size (or INF if any component is INF)
  if ASizeType = SIZE_MAX then
    Result := INF
  else
    Result := 0;

  for i := 0 to ACompWraps.Count - 1 do
  begin
    cw := ACompWraps[i];
    // For parallel layout (row/column sizing), use size WITHOUT gaps.
    // Gaps are handled separately by GetRowGaps to avoid double-counting.
    // This matches the fix we made in LayoutParallel for positioning.
    cwSize := cw.GetSizes(AIsHor)[ASizeType];

    if cwSize >= INF then
      Exit(INF);

    if ASizeType = SIZE_MAX then
    begin
      if cwSize < Result then
        Result := cwSize;
    end
    else
    begin
      if cwSize > Result then
        Result := cwSize;
    end;
  end;

  Result := TfpgMigLayoutUtil.Clamp(Result, 0, INF);
end;

class function TfpgMigGrid.GetTotalSizeSerial(const ACompWraps: TfpgMigCompWrapList;
  ASizeType: Integer; AIsHor: Boolean): Integer;
var
  i: Integer;
  cw: TfpgMigCompWrap;
  gapBef, lastGapAfter: Integer;
  totSize: Integer;
begin
  // For serial layout (components in sequence):
  // Sum all component sizes plus gaps between them
  totSize := 0;
  lastGapAfter := 0;

  for i := 0 to ACompWraps.Count - 1 do
  begin
    cw := ACompWraps[i];

    // Get gap before this component
    gapBef := cw.GetGapBefore(ASizeType, AIsHor);
    // Only add the gap if it's larger than the previous component's gap after
    if gapBef > lastGapAfter then
      totSize := totSize + (gapBef - lastGapAfter);

    // Add component size
    totSize := totSize + cw.GetSize(ASizeType, AIsHor);

    // Add gap after this component and remember it
    lastGapAfter := cw.GetGapAfter(ASizeType, AIsHor);
    totSize := totSize + lastGapAfter;

    // Check for overflow
    if totSize >= INF then
      Exit(INF);
  end;

  Result := TfpgMigLayoutUtil.Clamp(totSize, 0, INF);
end;

class function TfpgMigGrid.GetBaselineAboveBelow(const ACompWraps: TfpgMigCompWrapList;
  ASizeType: Integer; ACenterBaseline: Boolean): TfpgMigAboveBelow;
var
  i: Integer;
  cw: TfpgMigCompWrap;
  height, baseline, above: Integer;
  maxAbove, maxBelow: Integer;
begin
  // For baseline alignment (text components aligned on text baseline):
  // Calculate maximum distance above and below the baseline
  maxAbove := Low(Integer);
  maxBelow := Low(Integer);

  for i := 0 to ACompWraps.Count - 1 do
  begin
    cw := ACompWraps[i];
    height := cw.GetSize(ASizeType, False);  // Vertical size

    if height >= INF then
    begin
      Result.MaxAbove := INF div 2;
      Result.MaxBelow := INF div 2;
      Exit;
    end;

    baseline := cw.GetBaseline(ASizeType);
    above := baseline + cw.GetGapBefore(ASizeType, False);

    if above > maxAbove then
      maxAbove := above;

    if (height - baseline + cw.GetGapAfter(ASizeType, False)) > maxBelow then
      maxBelow := height - baseline + cw.GetGapAfter(ASizeType, False);

    // If centering baseline, set component bounds to align baselines
    if ACenterBaseline then
      cw.SetDimBounds(-baseline, height, False);
  end;

  Result.MaxAbove := maxAbove;
  Result.MaxBelow := maxBelow;
end;

{ TfpgMigLayoutManager - Public API }

constructor TfpgMigLayoutManager.Create;
begin
  inherited Create;
  // Create default constraints - user can replace or modify
  FLC := TfpgMigLC.Create;
  // Default is horizontal flow with no wrap (Java MigLayout behavior)

  FRowConstr := TfpgMigAC.Create;
  FColConstr := TfpgMigAC.Create;
  FIsLayingOut := False;
  FGrid := nil;
  FDirty := True;
  FContainer := nil;
  FLastComponentCount := -1;  // -1 indicates never laid out
  FComponentOrder := TfpgWidgetBaseList.Create;
end;

destructor TfpgMigLayoutManager.Destroy;
begin
  FreeAndNil(FLC);
  FreeAndNil(FRowConstr);
  FreeAndNil(FColConstr);
  FreeAndNil(FConstraints);
  FreeAndNil(FGrid);
  FreeAndNil(FComponentOrder);
  inherited Destroy;
end;

procedure TfpgMigLayoutManager.AddLayoutComponent(AWidget: TfpgWidgetBase; AConstraint: TfpgLayoutConstraint);
begin
  inherited AddLayoutComponent(AWidget, AConstraint);
  FComponentOrder.Add(AWidget);
  InvalidateLayout(AWidget.Parent);
end;

procedure TfpgMigLayoutManager.RemoveLayoutComponent(AWidget: TfpgWidgetBase);
begin
  FComponentOrder.Remove(AWidget);
  inherited RemoveLayoutComponent(AWidget);
  InvalidateLayout(AWidget.Parent);
end;

{ Setters that mark grid as dirty }

procedure TfpgMigLayoutManager.SetLC(AValue: TfpgMigLC);
begin
  if FLC <> AValue then
  begin
    FLC.Free;
    FLC := AValue;
    FDirty := True;
  end;
end;

procedure TfpgMigLayoutManager.SetRowConstr(AValue: TfpgMigAC);
begin
  if FRowConstr <> AValue then
  begin
    FRowConstr.Free;
    FRowConstr := AValue;
    FDirty := True;
  end;
end;

procedure TfpgMigLayoutManager.SetColConstr(AValue: TfpgMigAC);
begin
  if FColConstr <> AValue then
  begin
    FColConstr.Free;
    FColConstr := AValue;
    FDirty := True;
  end;
end;

function TfpgMigLayoutManager.CreateDefaultConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint;
begin
  Result := TfpgMigCC.Create;  // Use new v11 CC class
end;

procedure TfpgMigLayoutManager.InvalidateLayout(AContainer: TfpgWidgetBase);
begin
  inherited InvalidateLayout(AContainer);
  // Mark grid as dirty so it will be recreated on next layout
  // This is important when widgets are added/removed or constraints change
  FDirty := True;
end;

procedure TfpgMigLayoutManager.DoLayout(AContainer: TfpgWidgetBase);
var
  ccMap: TfpgMigCCMap;
  bounds: array[0..3] of Integer;
  Iterator: ILayoutIterator;
  child: TfpgWidget;
  constraint: TfpgLayoutConstraint;
  cc: TfpgMigCC;
  //insTop, insLeft, insBottom, insRight: Integer;
  //insUV: TfpgMigUnitValue;
  needsRecreate: Boolean;
  isDebug: Boolean;
begin
  if AContainer = nil then
    Exit;

  // Prevent recursive layout calls
  if FIsLayingOut then
    Exit;

  FIsLayingOut := True;
  try

  // Get iterator for proper widget traversal (skips native window, hidden widgets, etc.)
  Iterator := GetIterator(AContainer);
  if not Assigned(Iterator) then
    Exit;

  // 1. Build CC map from widgets and their constraints
  // Add ALL valid widgets (filtered by iterator), even those without constraints
  ccMap := TfpgMigCCMap.Create;
  try
    while Iterator.HasNext do
    begin
      child := Iterator.Next as TfpgWidget;
      if child = nil then
        Continue;

      // Get layout constraint if it exists (can be nil for default behavior)
      constraint := GetConstraint(child);
      if (constraint <> nil) and (constraint is TfpgMigCC) then
        cc := TfpgMigCC(constraint)
      else
        cc := nil;  // Widget will use default layout behavior

      // Add to map - Grid will only process widgets in this map
      ccMap.Add(child, cc);
    end;

    // 2. Check if grid needs to be recreated
    // Only recreate if:
    // - FDirty is true (explicit invalidation via AddLayoutComponent)
    // - Grid doesn't exist yet
    // - Container changed
    // - Component count changed (structural change - widget added/removed)
    needsRecreate := FDirty or (FGrid = nil) or (FContainer <> AContainer) or
                     (ccMap.Count <> FLastComponentCount);

    // 3. Create or reuse Grid instance
    if needsRecreate then
    begin
      FreeAndNil(FGrid);
      FGrid := TfpgMigGrid.Create(AContainer, FLC, FRowConstr, FColConstr, ccMap, FComponentOrder);
      FDirty := False;
      FContainer := AContainer;
      FLastComponentCount := ccMap.Count;
    end;

    // 4. Setup bounds for layout
    // In Java MigLayout, bounds subtracts the container's NATIVE insets (Swing
    // border padding) from the size, while LC insets are handled separately as
    // edge gaps inside Grid.getRowGaps(). fpGUI has no native container insets,
    // so bounds should be the full container size. The LC insets (panel padding)
    // are applied internally by the grid as the first/last gaps.
    bounds[0] := 0;   // no native container inset offset
    bounds[1] := 0;
    bounds[2] := Max(2, AContainer.ActualWidth);    // full container width
    bounds[3] := Max(2, AContainer.ActualHeight);   // full container height

    // 6. Perform layout with debug flag
    isDebug := FLC.GetDebug;
    FGrid.Layout(bounds, nil, nil, isDebug);

    // Grid is kept alive for PaintDebug to use
  finally
    ccMap.Free;
  end;

  finally
    FIsLayingOut := False;
  end;
end;

function TfpgMigLayoutManager.CalculateGridSize(AContainer: TfpgWidgetBase; ASizeType: Integer): TfpgSize;
var
  ccMap: TfpgMigCCMap;
  grid: TfpgMigGrid;
  Iterator: ILayoutIterator;
  child: TfpgWidget;
  constraint: TfpgLayoutConstraint;
  cc: TfpgMigCC;
  widthArray, heightArray: TfpgMigIntArray;
  refWidth, refHeight: Integer;
begin
  Result.SetSize(0, 0);

  if AContainer = nil then
    Exit;

  // Build CC map from widgets and their constraints (same as DoLayout)
  Iterator := GetIterator(AContainer);
  if not Assigned(Iterator) then
    Exit;

  ccMap := TfpgMigCCMap.Create;
  try
    while Iterator.HasNext do
    begin
      child := Iterator.Next as TfpgWidget;
      if child = nil then
        Continue;

      constraint := GetConstraint(child);
      if (constraint <> nil) and (constraint is TfpgMigCC) then
        cc := TfpgMigCC(constraint)
      else
        cc := nil;

      ccMap.Add(child, cc);
    end;

    // Create Grid instance to calculate sizes
    grid := TfpgMigGrid.Create(AContainer, FLC, FRowConstr, FColConstr, ccMap, FComponentOrder);
    try
      // Use large reference size for calculation (Grid will shrink to minimum/preferred)
      // For minimum size, Grid should calculate the smallest dimensions needed
      // For preferred size, Grid should calculate the ideal dimensions
      refWidth := 10000;
      refHeight := 10000;

      // Force grid to calculate its dimensions
      grid.checkSizeCalcs(refWidth, refHeight);

      // Get calculated dimensions from grid
      widthArray := grid.GetWidth;
      heightArray := grid.GetHeight;

      // Extract the requested size type (SIZE_MIN=0, SIZE_PREF=1, SIZE_MAX=2)
      if (Length(widthArray) > ASizeType) and (Length(heightArray) > ASizeType) then
      begin
        Result.W := widthArray[ASizeType];
        Result.H := heightArray[ASizeType];

        // NOTE: Do NOT add LC insets here. The grid's internal size calculation
        // (via GetRowGaps/GetColGaps) already includes LC insets as edge gaps.
        // Adding them again would double-count. The grid's GetWidth/GetHeight
        // already includes all gaps (including edge insets).
      end;
    finally
      grid.Free;
    end;
  finally
    ccMap.Free;
  end;
end;

function TfpgMigLayoutManager.DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize;
begin
  Result := CalculateGridSize(AContainer, SIZE_PREF);
end;

function TfpgMigLayoutManager.DoGetMinimumSize(AContainer: TfpgWidgetBase): TfpgSize;
begin
  Result := CalculateGridSize(AContainer, SIZE_MIN);
end;

procedure TfpgMigLayoutManager.PaintDebug(AWidget: TfpgWidgetBase; ACanvas: TfpgCanvasBase);
begin
  // Check if debug is enabled via LC
  if (FLC = nil) or (not FLC.GetDebug) then
    Exit;

  // Delegate to Grid's PaintDebug if grid exists
  if (FGrid <> nil) and Assigned(ACanvas) then
    FGrid.PaintDebug(ACanvas);
end;

{ TfpgMigGrid.ConvertSpanToSparseGrid - Port of Grid.java:1539 }
class function TfpgMigGrid.ConvertSpanToSparseGrid(ACurIx, ASpan: Integer;
  const AIndexes: TfpgMigIntegerList): Integer;
var
  lastIx, retSpan, i, ix: Integer;
begin
  // Port of Grid.java convertSpanToSparseGrid() - lines 1539-1554
  lastIx := ACurIx + ASpan;
  retSpan := 1;

  for i := 0 to AIndexes.Count - 1 do
  begin
    ix := AIndexes[i];

    if ix <= ACurIx then
      Continue;  // Haven't arrived at the current index yet

    if ix >= lastIx then
      Break;  // Past the end of the span

    Inc(retSpan);
  end;

  Result := retSpan;
end;

{ Port of Grid.java getGroupContaining() - line 932
  Searches for the LinkedDimGroup that contains the specified CompWrap }
function TfpgMigGrid.GetGroupContaining(const AGroupLists: array of TfpgMigLinkedDimGroupList;
                                        ACompWrap: TfpgMigCompWrap): TfpgMigLinkedDimGroup;
var
  i, j, k: Integer;
  groupList: TfpgMigLinkedDimGroupList;
  group: TfpgMigLinkedDimGroup;
  cw: TfpgMigCompWrap;
begin
  Result := nil;
  for i := 0 to High(AGroupLists) do
  begin
    groupList := AGroupLists[i];
    if groupList = nil then
      Continue;

    for j := 0 to groupList.Count - 1 do
    begin
      group := groupList[j];
      if group = nil then
        Continue;

      for k := 0 to group.CompWraps.Count - 1 do
      begin
        cw := group.CompWraps[k];
        if cw = ACompWrap then
        begin
          Result := group;
          Exit;
        end;
      end;
    end;
  end;
end;

{ Port of Grid.java paintDebug() - line 567
  Paints debug visualization: cell bounds (red) and component outlines (blue) }
procedure TfpgMigGrid.PaintDebug(ACanvas: TfpgCanvasBase);
var
  i: Integer;
  rect: TfpgRect;
  painted: specialize TList<TfpgRect>;
  pair: TfpgMigCellMap.TDictionaryPair;
  cell: TfpgMigCell;
  cw: TfpgMigCompWrap;
  widget: TfpgWidget;
  compRect: TfpgRect;
  savedClipRect: TfpgRect;
  savedColor: TfpgColor;
  savedLineWidth: Integer;
  savedLineStyle: TfpgLineStyle;
begin
  if (FDebugRects = nil) or (ACanvas = nil) then
    Exit;

  // Save canvas state manually
  savedClipRect := ACanvas.GetClipRect;
  savedColor := ACanvas.Color;
  savedLineWidth := ACanvas.GetLineWidth;
  savedLineStyle := ACanvas.LineStyle;

  ACanvas.ClearClipRect;
  try
    // Paint cell outlines (red dashed rectangles)
    // Port of Grid.java lines 572-578
    painted := specialize TList<TfpgRect>.Create;
    try
      ACanvas.Color := fpgColor($FF,0,0);  // Red
      ACanvas.SetLineStyle(1, lsDash);

      for i := 0 to FDebugRects.Count - 1 do
      begin
        rect := FDebugRects[i];

        // Avoid painting duplicate rects (Java uses ArrayList.contains())
        if painted.IndexOf(rect) = -1 then
        begin
          ACanvas.DrawRectangle(rect);
          painted.Add(rect);
        end;
      end;
    finally
      painted.Free;
    end;

    // Paint component outlines (blue dashed rectangles)
    // Port of Grid.java lines 580-584
    ACanvas.Color := fpgColor($00, $00, $C8);  // Blue (0, 0, 200)
    ACanvas.SetLineStyle(1, lsDash);

    for pair in FGrid do
    begin
      cell := pair.Value;
      if cell = nil then
        Continue;

      for cw in cell.CompWraps do
      begin
        // Draw outline around each component
        if cw.Comp is TfpgWidget then
        begin
          widget := TfpgWidget(cw.Comp);
          compRect.SetRect(widget.Left, widget.Top, widget.ActualWidth, widget.ActualHeight);
          ACanvas.DrawRectangle(compRect);
        end;
      end;
    end;

  finally
    // Restore canvas state
    ACanvas.Color := savedColor;
    ACanvas.SetLineStyle(savedLineWidth, savedLineStyle);
    ACanvas.SetClipRect(savedClipRect);
  end;
end;

initialization
  // Port of Grid.java static initializer - line 50
  // Create DOCK_DIM_CONSTRAINT with grow priority of 0
  DOCK_DIM_CONSTRAINT := TfpgMigDimConstraint.Create;
  DOCK_DIM_CONSTRAINT.SetGrowWeight(0);
  DOCK_DIM_CONSTRAINT.SetGrowPriority(0);
  DOCK_DIM_CONSTRAINT.SetShrinkWeight(0);
  DOCK_DIM_CONSTRAINT.SetShrinkPriority(0);

finalization
  FreeAndNil(DOCK_DIM_CONSTRAINT);

end.
