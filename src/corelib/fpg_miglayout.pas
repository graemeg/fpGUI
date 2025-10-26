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
  fpg_mig_layoututil;

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
  end;

  { FlowSizeSpec - size specifications for flow layout }
  TfpgMigFlowSizeSpec = class
  private
    FSizes: array of TfpgMigSizeArray;
    FResConstsInclGaps: array of TfpgMigResizeConstraint;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { Grid - the main layout engine }
  TfpgMigGrid = class
  private
    FLC: TfpgMigLC;
    FRowConstr, FColConstr: TfpgMigAC;
    FContainer: TfpgWidgetBase;
    FGrid: TfpgMigCellMap;
    FRowIndexes, FColIndexes: TfpgMigIntegerList;
    FColGroupLists, FRowGroupLists: array of TfpgMigLinkedDimGroupList;
    FWidth, FHeight: array[0..2] of Integer;
    FColFlowSpecs, FRowFlowSpecs: TfpgMigFlowSizeSpec;

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

constructor TfpgMigFlowSizeSpec.Create;
begin
  inherited Create;
  // TODO: Initialize with sizes and constraints when needed
end;

destructor TfpgMigFlowSizeSpec.Destroy;
begin
  inherited Destroy;
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

    // Calculate positions for each column and row
    compX := containerX;
    for i := 0 to FColIndexes.Count - 1 do
    begin
      colPositions[i] := compX;
      compX := compX + colWidths[i];
    end;

    compY := containerY;
    for i := 0 to FRowIndexes.Count - 1 do
    begin
      rowPositions[i] := compY;
      compY := compY + rowHeights[i];
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
        if cw = nil then
          Continue;

        // Set bounds for this component using its preferred size
        cw.SetDimBounds(colPositions[cellX], colWidths[cellX], True);   // horizontal
        cw.SetDimBounds(rowPositions[cellY], rowHeights[cellY], False);  // vertical
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
  FLC.SetFlowX(True);  // Default to horizontal flow with wrap
  FLC.SetWrapAfter(1);  // Wrap after each component (vertical stacking)

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
