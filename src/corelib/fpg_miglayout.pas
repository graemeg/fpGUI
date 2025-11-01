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
  TfpgMigLinkedDimGroupListArray = array of TfpgMigLinkedDimGroupList;


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

    procedure checkSizeCalcs(refWidth, refHeight: Integer);
    procedure calcGridSizes(refWidth, refHeight: Integer);

    { Build row and column index lists from grid }
    procedure BuildIndexes;

    { Build dimension groups from component constraints }
    procedure BuildDimensionGroups;

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

procedure TfpgMigLinkedDimGroup.Layout(ADC: TfpgMigDimConstraint; AStart, ASize, ASpanCount: Integer);
var
  Parent: TfpgWidgetBase;
  i: Integer;
begin
  FLStart := AStart;
  FLSize := ASize;

  if FCompWraps.Count = 0 then
    Exit;

  Parent := FCompWraps[0].Comp.Parent;

  {$IFDEF MIGDEBUG}
  // DEBUG: Show layout parameters BEFORE calling layout to avoid output corruption
  if FIsHor then
    Write('DEBUG: BEFORE Layout group (H) with ')
  else
    Write('DEBUG: BEFORE Layout group (V) with ');
  Write(FCompWraps.Count, ' components at start=', AStart, ', size=', ASize);
  Write(', linkType=', FLinkType, ', components: [');
  for i := 0 to FCompWraps.Count - 1 do
  begin
    Write(FCompWraps[i].Comp.Name);
    if i < FCompWraps.Count - 1 then Write(', ');
  end;
  WriteLn(']');
  {$ENDIF}

  case FLinkType of
    TYPE_PARALLEL:
      TfpgMigGrid.LayoutParallel(Parent, FCompWraps, ADC, AStart, ASize, FIsHor, ASpanCount, FFromEnd);
    TYPE_BASELINE:
      TfpgMigGrid.LayoutBaseline(Parent, FCompWraps, ADC, AStart, ASize, SIZE_PREF, ASpanCount);
  else // TYPE_SERIAL
    TfpgMigGrid.LayoutSerial(Parent, FCompWraps, ADC, AStart, ASize, FIsHor, ASpanCount, FFromEnd);
  end;
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
  ltr: Boolean;
  pair: TfpgMigCellMap.TDictionaryPair;
  cws: TfpgMigCompWrapList;
  cwBef, cwAft: TfpgWidgetBase;
  tag: string;
  ccBef, ccAft: TfpgMigCC;
begin
  {$IFDEF MIGDEBUG}
  WriteLn('DEBUG: ===== Creating TfpgMigGrid =====');
  WriteLn('DEBUG: Container has ', AContainer.ComponentCount, ' children');
  {$ENDIF}
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

    {$IFDEF MIGDEBUG}
    WriteLn('DEBUG: Placing widget "', child.Name, '" at cell (', cellX, ', ', cellY, ')');
    {$ENDIF}

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
        {$IFDEF MIGDEBUG}
        WriteLn('DEBUG: Wrapping after ', wrap, ' components, moving to next row');
        {$ENDIF}
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
        {$IFDEF MIGDEBUG}
        WriteLn('DEBUG: Wrapping after ', wrap, ' components, moving to next column');
        {$ENDIF}
        cellY := 0;
        cellX := cellX + 1;
      end;
    end;
    {$IFDEF MIGDEBUG}
    WriteLn('DEBUG: Next cell position will be (', cellX, ', ', cellY, ')');
    {$ENDIF}
  end;

  // Second pass: Build row and column indexes
  BuildIndexes;

  // Third pass: Divide components into linked dimension groups (Port of Grid.java line 391-392)
  FColGroupLists := DivideIntoLinkedGroups(False);  // Columns
  FRowGroupLists := DivideIntoLinkedGroups(True);   // Rows

  // Fourth pass: Calculate default grow weights (Port of Grid.java lines 394-395)
  // Note: hasPush parameters not yet implemented, passing False for now
  FGrowXs := GetDefaultGrowWeights(False, False);  // For columns
  FGrowYs := GetDefaultGrowWeights(False, True);   // For rows

  // Calculate gaps now that the cells are filled
  ltr := TfpgMigLayoutUtil.IsLeftToRight(FLC, FContainer);
  for pair in FGrid do
  begin
    cell := pair.Value;
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

      cc := ACCMap[cw.Comp];
      if cc <> nil then
        tag := cc.GetTag
      else
        tag := '';

      if cwBef <> nil then
        ccBef := ACCMap[cwBef]
      else
        ccBef := nil;
      if cwAft <> nil then
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

  {$IFDEF MIGDEBUG}
  Write('DEBUG: Row indexes: [');
  for i := 0 to FRowIndexes.Count - 1 do
  begin
    Write(FRowIndexes[i]);
    if i < FRowIndexes.Count - 1 then Write(', ');
  end;
  WriteLn(']');
  {$ENDIF}

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

  {$IFDEF MIGDEBUG}
  Write('DEBUG: Column indexes: [');
  for i := 0 to FColIndexes.Count - 1 do
  begin
    Write(FColIndexes[i]);
    if i < FColIndexes.Count - 1 then Write(', ');
  end;
  WriteLn(']');
  {$ENDIF}
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

function TfpgMigGrid.Layout(const ABounds: array of Integer; AAlignX, AAlignY: TfpgMigUnitValue;
                    ADebug: Boolean): Boolean;
var
  pair: TfpgMigCellMap.TDictionaryPair;
  cell: TfpgMigCell;
  cw: TfpgMigCompWrap;
  addVisualPadding: Boolean;
begin
  Result := False;
  if ADebug then
  begin
    // TODO: debugRects logic
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

  if FColFlowSpecs <> nil then FColFlowSpecs.Free;
  if FRowFlowSpecs <> nil then FRowFlowSpecs.Free;

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

  // Get container insets for edge gaps
  defIns := True; // Simplified: !hasDocks()
  firstGap := TfpgMigLayoutUtil.GetInsets(FLC, IfThen(AIsHor, 1, 0), defIns);
  lastGap  := TfpgMigLayoutUtil.GetInsets(FLC, IfThen(AIsHor, 3, 2), defIns);

  for i := 0 to High(Result) do
  begin
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
      // TODO: Handle gaps between columns/rows from DimConstraints
      Result[i] := defGapArr;
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
    rowGw := NaN;  // Use NaN to indicate "not set"

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

          if IsNan(rowGw) or (gw > rowGw) then
            rowGw := gw;
        end;
      end;
    end;

    // If this row/column has a specific grow weight, expand array and set it - line 794
    if not IsNan(rowGw) then
    begin
      // First time we find a specific weight, expand from GROW_100 to full array - line 795
      if Length(gwArr) = 1 then
      begin
        // Array size: (groupLists.length << 1) + 1 = groupLists.length * 2 + 1 - line 796
        SetLength(gwArr, (Length(groupLists) * 2) + 1);
        // Fill with NaN
        for j := 0 to High(gwArr) do
          gwArr[j] := NaN;
      end;
      gwArr[ix] := rowGw;  // Put grow weight at odd index for this row/column
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
    // Get dimension constraint for this row/column
    if primIndexes[i] < Length(primDCs) then
      dc := primDCs[primIndexes[i]]
    else if Length(primDCs) > 0 then
      dc := primDCs[High(primDCs)]
    else
      dc := nil;

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

      // TODO: Convert span if needed for sparse grid (convertSpanToSparseGrid)

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

  {$IFDEF MIGDEBUG}
  // DEBUG: Show created dimension groups
  if AIsRows then
    WriteLn('DEBUG: Created ', Length(Result), ' row group lists')
  else
  begin
    WriteLn('DEBUG: Created ', Length(Result), ' column group lists');
    for i := 0 to High(Result) do
    begin
      WriteLn('DEBUG:   Column ', i, ' has ', Result[i].Count, ' groups:');
      for ix := 0 to Result[i].Count - 1 do
      begin
        Write('DEBUG:     Group ', ix, ' has ', Result[i][ix].CompWraps.Count, ' components: [');
        for cellKey := 0 to Result[i][ix].CompWraps.Count - 1 do
        begin
          Write(Result[i][ix].CompWraps[cellKey].Comp.Name);
          if cellKey < Result[i][ix].CompWraps.Count - 1 then Write(', ');
        end;
        WriteLn(']');
      end;
    end;
  end;
  {$ENDIF}
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
    // scIx := i - (isRows ? dockOffY : dockOffX); // TODO: Port dockOff
    scIx := i;

    bIx := i shl 1;
    bIx2 := bIx + 1;

    if fromEnd then
      curPos := curPos - rowColSizes[bIx]
    else
      curPos := curPos + rowColSizes[bIx];

    if (scIx >= 0) and (scIx < Length(primDCs)) then
      primDC := primDCs[scIx]
    else if Length(primDCs) > 0 then
      primDC := primDCs[High(primDCs)]
    else
      primDC := nil; // TODO: Port DOCK_DIM_CONSTRAINT

    rowSize := rowColSizes[bIx2];

    for j := 0 to linkedGroups.Count - 1 do
    begin
      group := linkedGroups[j];
      groupSize := rowSize;
      if group.Span > 1 then
        groupSize := TfpgMigLayoutUtil.Sum(rowColSizes, bIx2, Min((group.Span shl 1) - 1, Length(rowColSizes) - bIx2 - 1));

      group.Layout(primDC, curPos, groupSize, group.Span);
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
  Result := MergeSizesGapsAndResConstrs(resConstrs, fillInPushGaps, rowColBoundSizes, gapSizes);

  // TODO: Adjust for spanning components (line 1137)
end;

class procedure TfpgMigGrid.LayoutParallel(AParent: TfpgWidgetBase; ACompWraps: TfpgMigCompWrapList; ADC: TfpgMigDimConstraint; AStart, ASize: Integer; AIsHor: Boolean; ASpanCount: Integer; AFromEnd: Boolean);
var
  sizes: TfpgMigSizeArrayArray;
  i: Integer;
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
  allSizes: TfpgMigIntegerArray;
  i: Integer;
begin
  SetLength(allSizes, Length(ASizes) * 3);
  for i := 0 to High(ASizes) do
  begin
    allSizes[(i * 3) + 0] := ASizes[i][0];
    allSizes[(i * 3) + 1] := ASizes[i][1];
    allSizes[(i * 3) + 2] := ASizes[i][2];
  end;
  SetCompWrapBounds(AParent, allSizes, ACompWraps, ARowAlign, AStart, ASize, AIsHor, AFromEnd);
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
  // dock: Integer; // TODO: port docking
begin
  SetLength(Result, ACompWraps.Count);
  for i := 0 to ACompWraps.Count - 1 do
  begin
    cc := ACompWraps[i].CC;
    dc := cc.GetDimConstraint(AIsHor);
    Result[i] := dc.Resize;

    // TODO: Port docking grow
    // dock := cc.GetDockSide;
    // if (AIsHor and ((dock = 0) or (dock = 2))) or (not AIsHor and ((dock = 1) or (dock = 3))) then
    // begin
    //   // Result[i] := ...
    // end;
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
begin
  fss := MergeSizesGapsAndResConstrs(
            GetComponentResizeConstraints(ACompWraps, AIsHor),
            GetComponentGapPush(ACompWraps, AIsHor),
            GetComponentSizes(ACompWraps, AIsHor),
            GetGaps(ACompWraps, AIsHor)
         );

  if (ADC <> nil) and ADC.IsFill then
  begin
    SetLength(growW, 1);
    growW[0] := 100.0;
  end
  else
    SetLength(growW, 0);

  sizes := TfpgMigLayoutUtil.CalculateSerial(fss.GetSizes, fss.ResConstsInclGaps, growW, SIZE_PREF, ASize);
  SetCompWrapBounds(AParent, sizes, ACompWraps, ADC.GetAlignOrDefault(AIsHor), AStart, ASize, AIsHor, AFromEnd);
end;

class procedure TfpgMigGrid.SetCompWrapBounds(AParent: TfpgWidgetBase; const AAllSizes: TfpgMigIntegerArray; ACompWraps: TfpgMigCompWrapList; ARowAlign: TfpgMigUnitValue;  AStart, ASize: Integer; AIsHor, AFromEnd: Boolean);
var
  totSize, i, bIx: Integer;
  cw: TfpgMigCompWrap;
  align: TfpgMigUnitValue;
  cSt, slack, al: Integer;
begin
  {$IFDEF MIGDEBUG}
  if AIsHor then
  begin
    Write('DEBUG: SetCompWrapBounds (H) for ', ACompWraps.Count, ' components, AStart=', AStart, ', ASize=', ASize, ', AAllSizes=[');
    for i := 0 to Min(High(AAllSizes), 9) do
    begin
      Write(AAllSizes[i]);
      if i < Min(High(AAllSizes), 9) then Write(', ');
    end;
    WriteLn(']');
  end;
  {$ENDIF}

  totSize := TfpgMigLayoutUtil.Sum(AAllSizes);
  align := CorrectAlign(ACompWraps[0].CC, ARowAlign, AIsHor, AFromEnd);

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
      {$IFDEF MIGDEBUG}
      if AIsHor then
        WriteLn('DEBUG:   Setting ', cw.Comp.Name, ' at position ', cSt, ', size=', AAllSizes[bIx]);
      {$ENDIF}
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
