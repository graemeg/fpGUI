unit fpg_miglayout;

{$mode objfpc}{$H+}

{
  MigLayout v11 Port for fpGUI

  This is a port of MigLayout v11.4.2 from Java to Object Pascal.
  Unlike the Java version which supports multiple toolkits (Swing, SWT, JavaFX),
  this implementation works directly with TfpgWidgetBase - no wrapper abstraction needed.

  Based on: prototypes/miglayout/mig-java-latest/core/src/main/java/net/miginfocom/layout/Grid.java
}

interface

uses
  Classes, SysUtils, Math, Generics.Collections,
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
  TfpgMigFlowSizeSpec = class;
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
  TfpgMigIntegerList = specialize TList<Integer>;
  TfpgMigIntArray = array of Integer;
  TfpgMigCCMap = specialize TDictionary<TfpgWidgetBase, TfpgMigCC>;
  TfpgMigSizeArray = array[0..2] of Integer;  // [min,pref,max]
  TfpgMigBooleanArray = array of Boolean;
  TfpgMigSizeArrayArray = array of TfpgMigSizeArray;  // For return values

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

    property Span: Integer read FSpan;
    property CompWraps: TfpgMigCompWrapList read FCompWraps;
  end;

  { FlowSizeSpec - size specifications for flow layout
    Holds alternating [gap, component, gap, component...] sizes and resize constraints
    Matches: Grid.FlowSizeSpec in Grid.java }
  TfpgMigFlowSizeSpec = class
  private
    FSizes: array of TfpgMigSizeArray;  // Alternating gap and component sizes [min,pref,max]
    FResConstsInclGaps: array of TfpgMigResizeConstraint;  // Alternating gap and component constraints
  public
    constructor Create(ASizes: array of TfpgMigSizeArray; AResConstr: array of TfpgMigResizeConstraint);
    destructor Destroy; override;

    function GetSizes: Pointer;  // Returns pointer to FSizes array for CalculateSerial
    function GetResizeConstraints: Pointer;  // Returns pointer to FResConstsInclGaps array
    function GetCount: Integer;  // Returns length of arrays
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
    FRowIndexes, FColIndexes: TfpgMigIntegerList;
    FColGroupLists, FRowGroupLists: array of TfpgMigLinkedDimGroupList;
    FWidth, FHeight: array[0..2] of Integer;
    FColFlowSpecs, FRowFlowSpecs: TfpgMigFlowSizeSpec;

    { Port of Grid.java growXs, growYs fields - line 115
      Default grow weights for columns and rows (from push or fill) }
    FGrowXs, FGrowYs: TfpgMigFloatArray;

    { Build row and column index lists from grid }
    procedure BuildIndexes;

    { Build dimension groups from component constraints }
    procedure BuildDimensionGroups;

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

    { Port of Grid.java mergeSizesGapsAndResConstrs() - line 2074
      Merges row/col sizes, gaps, and resize constraints into FlowSizeSpec }
    function MergeSizesGapsAndResConstrs(AResConstr: array of TfpgMigResizeConstraint;
                                         AGapPush: array of Boolean;
                                         AMinPrefMaxSizes: array of TfpgMigSizeArray;
                                         AGapSizes: array of TfpgMigSizeArray;
                                         ARefSize: Integer): TfpgMigFlowSizeSpec;

    { Port of Grid.java getRowGaps() - line 1224
      Returns the row gaps in pixel sizes. One more than there are specs sent in. }
    function GetRowGaps(ASpecs: array of TfpgMigDimConstraint; ARefSize: Integer;
                       AIsHor: Boolean; var AFillInPushGaps: TfpgMigBooleanArray): TfpgMigSizeArrayArray;

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
  public
    constructor Create(AContainer: TfpgWidgetBase; ALC: TfpgMigLC;
                       ARowConstr, AColConstr: TfpgMigAC;
                       const ACCMap: TfpgMigCCMap);
    destructor Destroy; override;

    { Main layout method - positions and sizes all components }
    function Layout(const ABounds: array of Integer; AAlignX, AAlignY: TfpgMigUnitValue;
                    ADebug: Boolean): Boolean;

    { Get calculated dimensions }
    function GetWidth: TfpgMigIntArray;
    function GetHeight: TfpgMigIntArray;
  end;

  TfpgMigLayoutManager = class(TfpgBaseLayoutManager)
  private
    FLC: TfpgMigLC;
    FRowConstr: TfpgMigAC;
    FColConstr: TfpgMigAC;
  protected
    // TfpgBaseLayoutManager overrides
    function CreateDefaultConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint; override;
    procedure DoLayout(AContainer: TfpgWidgetBase); override;
    function DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    { MigLayout v11 constraint properties }
    property LC: TfpgMigLC read FLC write FLC;
    property RowConstraints: TfpgMigAC read FRowConstr write FRowConstr;
    property ColumnConstraints: TfpgMigAC read FColConstr write FColConstr;
  end;

implementation

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
  FCompWraps := TfpgMigCompWrapList.Create(False);  // Don't own CompWraps
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
  FIsAbsolute := False;  // TODO: Check if horizontal and vertical sizes are absolute
  FHasGaps := False;

  FX := NOT_SET;
  FY := NOT_SET;
  FW := NOT_SET;
  FH := NOT_SET;

  FForcedPushGaps := 0;

  // Calculate sizes if visible and CC is provided
  if (AEHideMode <= 0) and (ACC <> nil) then
  begin
    hBS := ACC.Horizontal.GetSize;
    vBS := ACC.Vertical.GetSize;

    for i := SIZE_MIN to SIZE_MAX do
    begin
      FHorSizes[i] := GetSize(hBS, i, True, AUseVisualPadding, -1);
      FVerSizes[i] := GetSize(vBS, i, False, AUseVisualPadding, -1);
    end;

    CorrectMinMax(FHorSizes);
    CorrectMinMax(FVerSizes);
  end;

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
begin
  // TODO: Implement size validation and calculation
  // This is complex - involves content bias, callbacks, visual padding
  FSizesOk := True;
end;

function TfpgMigCompWrap.GetSize(ABoundSize: TfpgMigBoundSize; ASizeType: Integer;
  AIsHor: Boolean; AUseVP: Boolean; ASizeHint: Integer): Integer;
var
  uv: TfpgMigUnitValue;
  sz: TfpgSize;
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
          sz.W := 0;
          sz.H := 0;
          FComp.GetPreferredSize(sz);
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
    // TODO: Implement full GetPixels with parent size, relative units, etc.
    // For now, just use the value directly (assumes pixels)
    Result := Round(uv.Value);
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

  parW := par.Width;
  parH := par.Height;

  // Get gap constraints from adjacent components
  if ABefore <> nil then
  begin
    if AFlowX then
      befGap := ABeforeCC.Horizontal.GetGapAfter
    else
      befGap := ABeforeCC.Vertical.GetGapAfter;
  end
  else
    befGap := nil;

  if AAfter <> nil then
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

function TfpgMigCompWrap.GetBaseline(ASizeType: Integer): Integer;
begin
  // TODO: fpGUI doesn't currently support baseline positioning
  // Baseline is used for aligning text components on their text baseline
  // For now, return 0 (top of component)
  // When implemented: return comp.getBaseline(width, height)
  Result := 0;
end;

function TfpgMigCompWrap.HasBaseline: Boolean;
begin
  // TODO: fpGUI doesn't currently support baseline queries
  // When implemented, check if component supports baseline alignment
  // (typically true for text components like labels, buttons with text)
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

  // Transfer calculated bounds to the widget
  FComp.SetPosition(compX, compY, compW, compH);
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

{ TfpgMigFlowSizeSpec }

constructor TfpgMigFlowSizeSpec.Create(ASizes: array of TfpgMigSizeArray;
  AResConstr: array of TfpgMigResizeConstraint);
var
  i: Integer;
begin
  inherited Create;

  // Copy size arrays
  SetLength(FSizes, Length(ASizes));
  for i := 0 to High(ASizes) do
    FSizes[i] := ASizes[i];

  // Copy resize constraints
  SetLength(FResConstsInclGaps, Length(AResConstr));
  for i := 0 to High(AResConstr) do
    FResConstsInclGaps[i] := AResConstr[i];
end;

destructor TfpgMigFlowSizeSpec.Destroy;
var
  i: Integer;
begin
  // Free resize constraints
  for i := 0 to High(FResConstsInclGaps) do
    if FResConstsInclGaps[i] <> nil then
      FResConstsInclGaps[i].Free;

  inherited Destroy;
end;

function TfpgMigFlowSizeSpec.GetSizes: Pointer;
begin
  if Length(FSizes) > 0 then
    Result := @FSizes[0]
  else
    Result := nil;
end;

function TfpgMigFlowSizeSpec.GetResizeConstraints: Pointer;
begin
  if Length(FResConstsInclGaps) > 0 then
    Result := @FResConstsInclGaps[0]
  else
    Result := nil;
end;

function TfpgMigFlowSizeSpec.GetCount: Integer;
begin
  Result := Length(FSizes);
end;

{ TfpgMigGrid }

constructor TfpgMigGrid.Create(AContainer: TfpgWidgetBase; ALC: TfpgMigLC;
  ARowConstr, AColConstr: TfpgMigAC; const ACCMap: TfpgMigCCMap);
var
  i, childCount: Integer;
  child: TfpgWidgetBase;
  cc: TfpgMigCC;
  cw: TfpgMigCompWrap;
  flowX: Boolean;
  wrap: Integer;
  cellX, cellY: Integer;
  spanX, spanY: Integer;
  cellKey: Integer;
  cell: TfpgMigCell;
begin
  inherited Create;
  FContainer := AContainer;
  FLC := ALC;
  FRowConstr := ARowConstr;
  FColConstr := AColConstr;

  FGrid := TfpgMigCellMap.Create;
  FRowIndexes := TfpgMigIntegerList.Create;
  FColIndexes := TfpgMigIntegerList.Create;

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

  // First pass: Create CompWraps for all child widgets
  cellX := 0;
  cellY := 0;

  childCount := AContainer.ComponentCount;
  for i := 0 to childCount - 1 do
  begin
    child := TfpgWidgetBase(AContainer.Components[i]);

    // Skip if not a widget
    if child = nil then
      Continue;

    // TODO: Handle visibility check when we determine correct property

    // Get component constraints from map
    cc := nil;
    if (ACCMap <> nil) and ACCMap.ContainsKey(child) then
      cc := ACCMap[child];

    // Create CompWrap for this widget
    cw := TfpgMigCompWrap.Create(child, cc, 0, False);

    // Determine grid position
    // TODO: Handle explicit grid coordinates from CC
    // For now, use simple flow placement

    // Get span from CC if available
    spanX := 1;
    spanY := 1;
    if cc <> nil then
    begin
      // TODO: Get span from CC.GridCell or CC.Span properties
      // For now, default to 1x1
    end;

    // Encode grid position as integer key
    cellKey := EncodeCellKey(cellX, cellY);

    // Get or create cell at this position
    if not FGrid.TryGetValue(cellKey, cell) then
    begin
      cell := TfpgMigCell.Create(spanX, spanY, flowX);
      FGrid.Add(cellKey, cell);
    end;

    // Add CompWrap to cell
    cell.CompWraps.Add(cw);

    // Advance to next cell based on flow direction
    if flowX then
    begin
      // Horizontal flow
      cellX := cellX + spanX;
      // Check for wrap
      if (wrap > 0) and (cellX >= wrap) then
      begin
        cellX := 0;
        cellY := cellY + 1;
      end;
    end
    else
    begin
      // Vertical flow
      cellY := cellY + spanY;
      // Check for wrap
      if (wrap > 0) and (cellY >= wrap) then
      begin
        cellY := 0;
        cellX := cellX + 1;
      end;
    end;
  end;

  // Second pass: Build row and column indexes
  BuildIndexes;

  // Third pass: Create dimension groups for size groups and end groups
  BuildDimensionGroups;

  // Fourth pass: Calculate default grow weights (Port of Grid.java lines 385-386)
  // Note: hasPush parameters not yet implemented, passing False for now
  FGrowXs := GetDefaultGrowWeights(False, False);  // For columns
  FGrowYs := GetDefaultGrowWeights(False, True);   // For rows

  // TODO: Calculate gaps between components
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

procedure TfpgMigGrid.BuildIndexes;
var
  pair: TfpgMigCellMap.TDictionaryPair;
  cellKey: Integer;
  cellX, cellY: Integer;
  i, j: Integer;
  found: Boolean;
  tempRow, tempCol: Integer;
begin
  // Clear existing indexes
  FRowIndexes.Clear;
  FColIndexes.Clear;

  // Collect unique row and column indexes from grid cells
  for pair in FGrid do
  begin
    cellKey := pair.Key;
    DecodeCellKey(cellKey, cellX, cellY);

    // Add row index if not already present
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

    // Add column index if not already present
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
      FColIndexes.Add(cellX);
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
end;

destructor TfpgMigGrid.Destroy;
var
  i: Integer;
begin
  FGrid.Free;
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
  inherited Destroy;
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
  // Get actual grid position from index
  if AIsHor then
    dimPos := FColIndexes[ADimIndex]
  else
    dimPos := FRowIndexes[ADimIndex];

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
function TfpgMigGrid.MergeSizesGapsAndResConstrs(AResConstr: array of TfpgMigResizeConstraint;
  AGapPush: array of Boolean; AMinPrefMaxSizes: array of TfpgMigSizeArray;
  AGapSizes: array of TfpgMigSizeArray; ARefSize: Integer): TfpgMigFlowSizeSpec;
var
  sizes: array of TfpgMigSizeArray;
  resConstsInclGaps: array of TfpgMigResizeConstraint;
  i, crIx: Integer;
  GAP_RC_CONST, GAP_RC_CONST_PUSH: TfpgMigResizeConstraint;
begin
  // Make room for gaps around: [gap, comp, gap, comp, ..., gap]
  SetLength(sizes, (Length(AMinPrefMaxSizes) * 2) + 1);
  SetLength(resConstsInclGaps, Length(sizes));

  // Gap resize constraints (no grow/shrink for gaps)
  GAP_RC_CONST := TfpgMigResizeConstraint.Create;
  GAP_RC_CONST_PUSH := TfpgMigResizeConstraint.Create;  // TODO: Set push flag

  // First gap
  sizes[0] := AGapSizes[0];

  for i := 0 to High(AMinPrefMaxSizes) do
  begin
    crIx := (i * 2) + 1;

    // Component bounds and constraints
    resConstsInclGaps[crIx] := AResConstr[i];
    sizes[crIx] := AMinPrefMaxSizes[i];

    // Gap after component
    sizes[crIx + 1] := AGapSizes[i + 1];

    // Set gap constraints (sizes array is always initialized with SetLength, no nil check needed)
    if (i < Length(AGapPush)) and AGapPush[i] then
      resConstsInclGaps[crIx - 1] := GAP_RC_CONST_PUSH
    else
      resConstsInclGaps[crIx - 1] := GAP_RC_CONST;

    if i = High(AMinPrefMaxSizes) then
    begin
      if ((i + 1) < Length(AGapPush)) and AGapPush[i + 1] then
        resConstsInclGaps[crIx + 1] := GAP_RC_CONST_PUSH
      else
        resConstsInclGaps[crIx + 1] := GAP_RC_CONST;
    end;
  end;

  Result := TfpgMigFlowSizeSpec.Create(sizes, resConstsInclGaps);
end;

{ Port of Grid.java getRowGaps() - line 1224 }
function TfpgMigGrid.GetRowGaps(ASpecs: array of TfpgMigDimConstraint; ARefSize: Integer;
  AIsHor: Boolean; var AFillInPushGaps: TfpgMigBooleanArray): TfpgMigSizeArrayArray;
var
  defGap: TfpgMigBoundSize;
  defGapArr: TfpgMigSizeArray;
  retValues: array of TfpgMigSizeArray;
  i: Integer;
  specBefore, specAfter: TfpgMigDimConstraint;
  gapBefore, gapAfter: TfpgMigSizeArray;
begin
  // Get default gap from LC
  if AIsHor then
    defGap := FLC.GetGridGapX
  else
    defGap := FLC.GetGridGapY;

  if defGap = nil then
  begin
    if AIsHor then
      defGap := TfpgMigPlatformDefaults.GetRelatedGapX  // Grid gap is same as related gap
    else
      defGap := TfpgMigPlatformDefaults.GetRelatedGapY;
  end;

  // Convert default gap to pixel sizes (defGapArr is a static array[0..2], no SetLength needed)
  if defGap <> nil then
  begin
    if defGap.Min <> nil then
      defGapArr[SIZE_MIN] := Round(defGap.Min.Value)
    else
      defGapArr[SIZE_MIN] := 6;  // Default
    if defGap.Preferred <> nil then
      defGapArr[SIZE_PREF] := Round(defGap.Preferred.Value)
    else
      defGapArr[SIZE_PREF] := 6;
    if defGap.Max <> nil then
      defGapArr[SIZE_MAX] := Round(defGap.Max.Value)
    else
      defGapArr[SIZE_MAX] := INF;
  end
  else
  begin
    defGapArr[SIZE_MIN] := 6;
    defGapArr[SIZE_PREF] := 6;
    defGapArr[SIZE_MAX] := INF;
  end;

  SetLength(retValues, Length(ASpecs) + 1);

  for i := 0 to High(retValues) do
  begin
    if i > 0 then
      specBefore := ASpecs[i - 1]
    else
      specBefore := nil;

    if i < Length(ASpecs) then
      specAfter := ASpecs[i]
    else
      specAfter := nil;

    // TODO: For now, use default gaps everywhere
    // Full implementation would check gap before/after on specs
    // retValues[i] is a static array[0..2], just assign directly
    retValues[i] := defGapArr;

    // Check for push gaps
    if ((specBefore <> nil) and specBefore.IsGapAfterPush) or
       ((specAfter <> nil) and specAfter.IsGapBeforePush) then
      AFillInPushGaps[i] := True;
  end;

  Result := retValues;
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
  rowGw: Single;
  grp: TfpgMigLinkedDimGroup;
  cw: TfpgMigCompWrap;
  gw: Single;
  hasGrowWeight: Boolean;
begin
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

  // Get the appropriate group lists - line 777
  if AIsRows then
    groupLists := FRowGroupLists
  else
    groupLists := FColGroupLists;

  // Start with GROW_100 (single element array with value 100.0) - line 779
  SetLength(gwArr, 1);
  gwArr[0] := GROW_100[0];

  // Loop through each row/column - line 780
  ix := 1;  // Index for alternating array (gaps at even, sizes at odd)
  for i := 0 to Length(groupLists) - 1 do
  begin
    grps := groupLists[i];
    rowGw := -1.0;  // Use -1 to indicate "not set"
    hasGrowWeight := False;

    // Find maximum grow weight for this row/column from all components - line 783
    for j := 0 to grps.Count - 1 do
    begin
      grp := grps[j];
      for c := 0 to grp.CompWraps.Count - 1 do
      begin
        cw := grp.CompWraps[c];

        // Get grow weight from component CC - line 788
        // Note: hasPush parameter not yet implemented, so we use component grow weight
        if AIsRows then
          hasGrowWeight := cw.FCC.Vertical.HasGrowWeight
        else
          hasGrowWeight := cw.FCC.Horizontal.HasGrowWeight;

        if hasGrowWeight then
        begin
          if AIsRows then
            gw := cw.FCC.Vertical.GetGrowWeight
          else
            gw := cw.FCC.Horizontal.GetGrowWeight;

          if (rowGw < 0) or (gw > rowGw) then
            rowGw := gw;
        end;
      end;
    end;

    // If this row/column has a specific grow weight, expand array and set it - line 794
    if rowGw >= 0 then
    begin
      // First time we find a specific weight, expand from GROW_100 to full array - line 795
      if Length(gwArr) = 1 then
      begin
        // Array size: (groupLists.length << 1) + 1 = groupLists.length * 2 + 1 - line 796
        SetLength(gwArr, (Length(groupLists) * 2) + 1);
        // Fill with zeros except first element (already has GROW_100[0])
        for j := 1 to High(gwArr) do
          gwArr[j] := 0.0;
      end;
      gwArr[ix] := rowGw;  // Put grow weight at odd index for this row/column
    end;

    ix := ix + 2;  // Move to next odd index (skip gap at even index)
  end;

  Result := gwArr;
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

    // Get dimension constraint for this row/column (lines 1073-1077)
    if cellIx < Length(primDCs) then
      allDCs[r] := primDCs[cellIx]
    else if Length(primDCs) > 0 then
      allDCs[r] := primDCs[High(primDCs)]
    else
      allDCs[r] := nil;

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
  Result := MergeSizesGapsAndResConstrs(resConstrs, fillInPushGaps, rowColBoundSizes, gapSizes, ARefSize);

  // TODO: Adjust for spanning components (line 1137)
end;

function TfpgMigGrid.CalculateDimensionSizes(const AIndexes: TfpgMigIntegerList;
  const APrefSizes: array of Integer; AAC: TfpgMigAC; ABounds: Integer;
  AIsHor: Boolean): TfpgMigIntegerArray;
var
  count, i, gapSize, totalGaps: Integer;
  sizeMatrix: TfpgMigSizeMatrix;
  resConstr: TfpgMigResizeConstraintArray;
  pushWeights: TfpgMigFloatArray;
  dimConstr: TfpgMigDimConstraint;
  dimConstraints: TfpgMigDimConstraintArray;
  sizeArray: PfpgMigSizeArray;
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
    New(sizeArray);
    sizeArray^[SIZE_MIN] := APrefSizes[i];     // For now, min = pref
    sizeArray^[SIZE_PREF] := APrefSizes[i];
    sizeArray^[SIZE_MAX] := INF_SIZE;          // Max is unlimited
    sizeMatrix[i] := sizeArray;
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
    Dispose(sizeMatrix[i]);
    resConstr[i].Free;
  end;
end;

function TfpgMigGrid.Layout(const ABounds: array of Integer; AAlignX, AAlignY: TfpgMigUnitValue;
  ADebug: Boolean): Boolean;
var
  containerX, containerY, containerW, containerH: Integer;
  lc: TfpgMigLC;
  pair: TfpgMigCellMap.TDictionaryPair;
  cell: TfpgMigCell;
  i, j: Integer;
  cw: TfpgMigCompWrap;
  cellKey: Integer;
  cellX, cellY: Integer;
  compX, compY: Integer;
  insets: array[0..3] of Integer;  // top, left, bottom, right
  colWidths, rowHeights: array of Integer;
  colPositions, rowPositions: array of Integer;
  maxWidth, maxHeight: Integer;
  alignX, alignY: TfpgMigUnitValue;
  offsetX, offsetY: Integer;
begin
  Result := False;

  // Extract bounds
  if Length(ABounds) < 4 then
    Exit;

  containerX := ABounds[0];
  containerY := ABounds[1];
  containerW := ABounds[2];
  containerH := ABounds[3];

  // Get layout constraints
  lc := FLC;

  // Apply insets from LC if present
  insets[0] := 6;  // top - default inset
  insets[1] := 6;  // left
  insets[2] := 6;  // bottom
  insets[3] := 6;  // right

  // TODO: Extract actual insets from LC.GetInsets

  // Adjust container bounds for insets
  containerX := containerX + insets[1];
  containerY := containerY + insets[0];
  containerW := containerW - insets[1] - insets[3];
  containerH := containerH - insets[0] - insets[2];

  // Store container dimensions for layout calculations
  FWidth[SIZE_PREF] := containerW;
  FHeight[SIZE_PREF] := containerH;

  // Calculate preferred sizes for each column and row
  if (FColIndexes.Count > 0) and (FRowIndexes.Count > 0) then
  begin
    SetLength(colWidths, FColIndexes.Count);
    SetLength(rowHeights, FRowIndexes.Count);
    SetLength(colPositions, FColIndexes.Count);
    SetLength(rowPositions, FRowIndexes.Count);

    // Initialize with zeros
    for i := 0 to FColIndexes.Count - 1 do
      colWidths[i] := 0;
    for i := 0 to FRowIndexes.Count - 1 do
      rowHeights[i] := 0;

    // Calculate preferred size for each column and row based on components
    for pair in FGrid do
    begin
      cellKey := pair.Key;
      cell := pair.Value;

      if cell = nil then
        Continue;

      DecodeCellKey(cellKey, cellX, cellY);

      // Find column and row indexes
      j := -1;
      for i := 0 to FColIndexes.Count - 1 do
      begin
        if FColIndexes[i] = cellX then
        begin
          j := i;
          Break;
        end;
      end;
      if j < 0 then
        Continue;
      cellX := j;  // Now cellX is the index, not the grid position

      j := -1;
      for i := 0 to FRowIndexes.Count - 1 do
      begin
        if FRowIndexes[i] = cellY then
        begin
          j := i;
          Break;
        end;
      end;
      if j < 0 then
        Continue;
      cellY := j;  // Now cellY is the index

      // Get maximum preferred size for this cell
      maxWidth := 0;
      maxHeight := 0;
      for i := 0 to cell.CompWraps.Count - 1 do
      begin
        cw := cell.CompWraps[i];
        if cw = nil then
          Continue;

        // Get component's preferred size
        if cw.Comp <> nil then
        begin
          if cw.Comp.Width > maxWidth then
            maxWidth := cw.Comp.Width;
          if cw.Comp.Height > maxHeight then
            maxHeight := cw.Comp.Height;
        end;
      end;

      // Update column and row sizes
      if maxWidth > colWidths[cellX] then
        colWidths[cellX] := maxWidth;
      if maxHeight > rowHeights[cellY] then
        rowHeights[cellY] := maxHeight;
    end;

    // Use CalcRowsOrColsSizes + LayoutUtil.CalculateSerial (Java MigLayout v11 approach)
    // This returns FlowSizeSpec with alternating [gap, size, gap, size, ...] and resize constraints
    // Pass FGrowXs/FGrowYs to CalcRowsOrColsSizes for spanning components (Java line 527-528)
    FColFlowSpecs := CalcRowsOrColsSizes(FColGroupLists, FGrowXs, containerW, True);
    FRowFlowSpecs := CalcRowsOrColsSizes(FRowGroupLists, FGrowYs, containerH, False);

    // Calculate actual column widths and row heights (Java line 984)
    // Pass FGrowXs/FGrowYs to CalculateSerial for grow weight distribution
    colWidths := TfpgMigLayoutUtil.CalculateSerial(
      FColFlowSpecs.GetSizes,
      FColFlowSpecs.GetResizeConstraints,
      FGrowXs,
      SIZE_PREF,
      containerW
    );

    rowHeights := TfpgMigLayoutUtil.CalculateSerial(
      FRowFlowSpecs.GetSizes,
      FRowFlowSpecs.GetResizeConstraints,
      FGrowYs,
      SIZE_PREF,
      containerH
    );

    // Calculate positions from sizes (sizes array includes gaps: [gap, col0, gap, col1, gap, ...])
    // Positions are calculated cumulatively from the alternating gap/size array
    compX := containerX;
    SetLength(colPositions, FColIndexes.Count);
    for i := 0 to FColIndexes.Count - 1 do
    begin
      // Index in sizes array: gap at (i*2), column at (i*2+1)
      if Length(colWidths) > (i * 2) then
        compX := compX + colWidths[i * 2];  // Add gap before
      colPositions[i] := compX;
      if Length(colWidths) > (i * 2 + 1) then
        compX := compX + colWidths[i * 2 + 1];  // Add column width
    end;

    compY := containerY;
    SetLength(rowPositions, FRowIndexes.Count);
    for i := 0 to FRowIndexes.Count - 1 do
    begin
      // Index in sizes array: gap at (i*2), row at (i*2+1)
      if Length(rowHeights) > (i * 2) then
        compY := compY + rowHeights[i * 2];  // Add gap before
      rowPositions[i] := compY;
      if Length(rowHeights) > (i * 2 + 1) then
        compY := compY + rowHeights[i * 2 + 1];  // Add row height
    end;

    // Position all components using calculated sizes
    for pair in FGrid do
    begin
      cellKey := pair.Key;
      cell := pair.Value;

      if cell = nil then
        Continue;

      DecodeCellKey(cellKey, cellX, cellY);

      // Find column and row indexes
      j := -1;
      for i := 0 to FColIndexes.Count - 1 do
      begin
        if FColIndexes[i] = cellX then
        begin
          j := i;
          Break;
        end;
      end;
      if j < 0 then
        Continue;
      cellX := j;

      j := -1;
      for i := 0 to FRowIndexes.Count - 1 do
      begin
        if FRowIndexes[i] = cellY then
        begin
          j := i;
          Break;
        end;
      end;
      if j < 0 then
        Continue;
      cellY := j;

      // Position all CompWraps in this cell
      for i := 0 to cell.CompWraps.Count - 1 do
      begin
        cw := cell.CompWraps[i];
        if (cw = nil) or (cw.Comp = nil) then
          Continue;

        // Get component's preferred size
        maxWidth := cw.Comp.Width;
        maxHeight := cw.Comp.Height;

        // Get actual cell width and height from alternating sizes array
        // Array format: [gap, col0, gap, col1, gap, ...]
        // So column cellX width is at index (cellX * 2 + 1)
        if Length(colWidths) > (cellX * 2 + 1) then
          maxWidth := colWidths[cellX * 2 + 1]  // Use cell width
        else
          maxWidth := cw.Comp.Width;  // Fallback to component width

        if Length(rowHeights) > (cellY * 2 + 1) then
          maxHeight := rowHeights[cellY * 2 + 1]  // Use cell height
        else
          maxHeight := cw.Comp.Height;  // Fallback to component height

        // Calculate position within cell based on alignment
        compX := colPositions[cellX];
        compY := rowPositions[cellY];

        // Apply horizontal alignment (if no grow, component keeps preferred width)
        if (cw.FCC <> nil) and (cw.FCC.Horizontal <> nil) then
        begin
          alignX := cw.FCC.Horizontal.GetAlign;
          // Check if component has grow weight - if so, it fills the cell
          if cw.FCC.Horizontal.HasGrowWeight and (cw.FCC.Horizontal.GetGrowWeight > 0) then
          begin
            // Component grows to fill cell - maxWidth already set to cell width above
          end
          else if alignX <> nil then
          begin
            // No grow - calculate offset based on alignment
            if (alignX.UnitType = utPercent) then
            begin
              // Percent-based alignment (0% = left, 50% = center, 100% = right)
              // Component keeps its preferred size, just positioned within cell
              offsetX := Round((maxWidth - cw.Comp.Width) * alignX.Value / 100);
              compX := compX + offsetX;
              maxWidth := cw.Comp.Width;  // Use component's preferred width
            end;
            // utAlign with value 0 (leading) means no offset needed
          end
          else
          begin
            // No explicit alignment - use component's preferred width
            maxWidth := cw.Comp.Width;
          end;
        end
        else
        begin
          // No horizontal constraint - use component's preferred width
          maxWidth := cw.Comp.Width;
        end;

        // Apply vertical alignment (if no grow, component keeps preferred height)
        if (cw.FCC <> nil) and (cw.FCC.Vertical <> nil) then
        begin
          alignY := cw.FCC.Vertical.GetAlign;
          // Check if component has grow weight - if so, it fills the cell
          if cw.FCC.Vertical.HasGrowWeight and (cw.FCC.Vertical.GetGrowWeight > 0) then
          begin
            // Component grows to fill cell - maxHeight already set to cell height above
          end
          else if alignY <> nil then
          begin
            // No grow - calculate offset based on alignment
            if (alignY.UnitType = utPercent) then
            begin
              // Percent-based alignment (0% = top, 50% = center, 100% = bottom)
              // Component keeps its preferred size, just positioned within cell
              offsetY := Round((maxHeight - cw.Comp.Height) * alignY.Value / 100);
              compY := compY + offsetY;
              maxHeight := cw.Comp.Height;  // Use component's preferred height
            end;
            // utAlign with value 0 (leading) means no offset needed
          end
          else
          begin
            // No explicit alignment - use component's preferred height
            maxHeight := cw.Comp.Height;
          end;
        end
        else
        begin
          // No vertical constraint - use component's preferred height
          maxHeight := cw.Comp.Height;
        end;

        // Set bounds for this component
        cw.SetDimBounds(compX, maxWidth, True);   // horizontal
        cw.SetDimBounds(compY, maxHeight, False);  // vertical
        cw.TransferBounds(False);
      end;
    end;
  end;

  Result := True;
end;

function TfpgMigGrid.GetWidth: TfpgMigIntArray;
begin
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
    cwSize := cw.GetSizeInclGaps(ASizeType, AIsHor);

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
end;

destructor TfpgMigLayoutManager.Destroy;
begin
  FLC.Free;
  FRowConstr.Free;
  FColConstr.Free;
  inherited Destroy;
end;

function TfpgMigLayoutManager.CreateDefaultConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint;
begin
  Result := TfpgMigCC.Create;  // Use new v11 CC class
end;

procedure TfpgMigLayoutManager.DoLayout(AContainer: TfpgWidgetBase);
var
  ccMap: TfpgMigCCMap;
  grid: TfpgMigGrid;
  bounds: array[0..3] of Integer;
  i: Integer;
  child: TfpgWidgetBase;
  constraint: TfpgLayoutConstraint;
  cc: TfpgMigCC;
begin
  if AContainer = nil then
    Exit;

  // 1. Build CC map from widgets and their constraints
  ccMap := TfpgMigCCMap.Create;
  try
    for i := 0 to AContainer.ComponentCount - 1 do
    begin
      child := TfpgWidgetBase(AContainer.Components[i]);
      if child = nil then
        Continue;

      // Get layout constraint if it exists
      constraint := GetConstraint(child);
      if (constraint <> nil) and (constraint is TfpgMigCC) then
      begin
        cc := TfpgMigCC(constraint);
        ccMap.Add(child, cc);
      end;
    end;

    // 2. Create Grid instance using stored constraints
    grid := TfpgMigGrid.Create(AContainer, FLC, FRowConstr, FColConstr, ccMap);
    try
      // 3. Setup bounds for layout
      bounds[0] := 0;  // x
      bounds[1] := 0;  // y
      bounds[2] := AContainer.Width;   // width
      bounds[3] := AContainer.Height;  // height

      // 4. Perform layout
      grid.Layout(bounds, nil, nil, False);

      // Bounds are transferred to widgets inside Layout method
    finally
      grid.Free;
    end;
  finally
    ccMap.Free;
  end;
end;

function TfpgMigLayoutManager.DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize;
begin
  // TODO: Use Grid.getWidth/getHeight for preferred size
  Result.SetSize(0, 0);
end;

end.
