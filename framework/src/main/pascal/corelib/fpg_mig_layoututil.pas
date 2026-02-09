unit fpg_mig_layoututil;

{
  MigLayout for fpGUI - Layout Utility Functions

  Port of LayoutUtil.java from MigLayout v11.4.2

  Core layout algorithms including calculateSerial() which distributes
  space across components according to size constraints and grow/shrink weights.

  License (BSD):
  ==============

  Copyright (c) 2004, Mikael Grev, MiG InfoCom AB. (miglayout@miginfocomo.com)
  Pascal port Copyright (c) 2025, Graeme Geldenhuys
  All rights reserved.
}

{$I fpg_defines.inc}

interface

uses
  Classes, SysUtils, Math, Generics.Collections,
  fpg_base,
  fpg_mig_unitvalue,
  fpg_mig_boundsize,
  fpg_mig_resizeconstraint,
  fpg_mig_dimconstraint,
  fpg_mig_ac,
  fpg_mig_lc;

const
  { A substitute value for a really large value. Integer.MAX_VALUE is not used
    since that means a lot of defensive code for potential overflow must exist
    in many places. This value is large enough for being unreasonable yet it is
    hard to overflow. }
  INF = (High(Integer) shr 10) - 100;  // 2097051

  { Tag int for a value that is considered "not set". Used as "null" element
    in int arrays. }
  NOT_SET = Low(Integer) + 12346;  // -2147471302

  { Index constants for size arrays [MIN, PREF, MAX] }
  SIZE_MIN = 0;
  SIZE_PREF = 1;
  SIZE_MAX = 2;

  { Orientation constants }
  HORIZONTAL = 0;
  VERTICAL = 1;

type
  { Array types for size calculations }
  TfpgMigSizeArray = array[SIZE_MIN..SIZE_MAX] of Integer;  // [min, pref, max]
  TfpgMigSizeArrayArray = array of TfpgMigSizeArray;
  TfpgMigResizeConstraintArray = array of TfpgMigResizeConstraint;
  TfpgMigFloatArray = array of Single;
  TfpgMigIntegerArray = array of Integer;  // Specialized integer array
  TfpgMigIntegerList = specialize TList<Integer>;  // Specialized integer list

  { FlowSizeSpec - size specifications for flow layout
    Holds alternating [gap, component, gap, component...] sizes and resize constraints
    Matches: Grid.FlowSizeSpec in Grid.java }
  TfpgMigFlowSizeSpec = class(TObject)
  private
    FSizes: TfpgMigSizeArrayArray;
    FResConstsInclGaps: TfpgMigResizeConstraintArray;
  public
    constructor Create(ASizes: TfpgMigSizeArrayArray; AResConstr: TfpgMigResizeConstraintArray);
    destructor Destroy; override;
    function CalculateSerial(AStartSizeType, ABounds: Integer): TfpgMigIntegerArray;
    function GetSizes: TfpgMigSizeArrayArray;

    { Port of Grid.java FlowSizeSpec.expandSizes() - line 2369
      Expands sizes to meet a target size for spanning components.
      @param ASpecs The specs for the columns or rows
      @param ADefGrow The default grow weight if the specs don't have anyone that will grow
      @param ATargetSize The size to try to meet
      @param AFromIx Starting index in sizes array
      @param ALen Number of elements to expand
      @param ASizeType SIZE_MIN or SIZE_PREF
      @param AEagerness How eager to expand (0-3)
      @returns The new total size }
    function ExpandSizes(ASpecs: TfpgMigDimConstraintArray; ADefGrow: TfpgMigFloatArray;
                        ATargetSize, AFromIx, ALen, ASizeType, AEagerness: Integer): Integer;

    property ResConstsInclGaps: TfpgMigResizeConstraintArray read FResConstsInclGaps;
  end;

  { Static utility class with layout algorithms }
  TfpgMigLayoutUtil = class
  public
    { Takes a number on min/preferred/max sizes and resize constraints and returns
      the calculated sizes which sum should add up to bounds. Whether the sum
      will actually equal bounds is dependent on the pref/max sizes and resize
      constraints.

      @param ASizes [ix][MIN][PREF][MAX]. NOT_SET will be treated as N/A or 0.
             A [MIN][PREF][MAX] array with NOT_SET elements will be interpreted
             as very flexible (no bounds) but if the array itself is nil it will
             not get any size.
      @param AResConstr Elements can be nil and the whole array can be nil. Nil
             means that the size will not be flexible at all. Can have length less
             than ASizes in which case the last element should be used for the
             elements missing.
      @param ADefPushWeights If there is no grow weight for a resConstr the
             corresponding value of this array is used. These forced resConstr will
             be grown last though and only if needed to fill to the bounds.
      @param AStartSizeType The initial size to use. E.g. SIZE_PREF.
      @param ABounds To use for relative sizes.
      @returns The sizes. Array length will match ASizes. }
    class function CalculateSerial(const ASizes: TfpgMigSizeArrayArray;
                                   const AResConstr: TfpgMigResizeConstraintArray;
                                   const ADefPushWeights: TfpgMigFloatArray;
                                   AStartSizeType, ABounds: Integer): TfpgMigIntegerArray;

    { Safe array index accessor - returns nil if index out of bounds }
    class function GetIndexSafe(const AArr: TfpgMigResizeConstraintArray;
                               AIndex: Integer): TfpgMigResizeConstraint; overload;
    class function GetIndexSafe(const AArr: TfpgMigDimConstraintArray;
                               AIndex: Integer): TfpgMigDimConstraint; overload;

    { Returns size value from size array safely, handling NOT_SET }
    class function GetSizeSafe(const ASizes: TfpgMigSizeArray;
                              ASizeType: Integer): Integer;

    { Sums array elements from start for len items }
    class function Sum(const ATerms: TfpgMigIntegerArray; AStart, ALen: Integer): Integer; overload;
    { Sums all array elements }
    class function Sum(const ATerms: TfpgMigIntegerArray): Integer; overload;

    { Clamps a value between min and max }
    class function Clamp(AValue, AMin, AMax: Single): Single; overload;
    class function Clamp(AValue, AMin, AMax: Integer): Integer; overload;

    { Rounds float sizes to integer sizes }
    class function RoundSizes(const ASizes: TfpgMigFloatArray): TfpgMigIntegerArray;

    { Derives a new BoundSize from an existing one with new min/pref/max values }
    class function Derive(ABase: TfpgMigBoundSize;
                         AMin, APref, AMax: TfpgMigUnitValue): TfpgMigBoundSize;

    { Returns if layout direction is left-to-right }
    class function IsLeftToRight(ALC: TfpgMigLC;
                                 AContainer: TfpgWidgetBase): Boolean;

    { Gets insets for a container }
    class function GetInsets(ALC: TfpgMigLC; ASide: Integer;
                            AGetDefault: Boolean): TfpgMigUnitValue;

    { Equality check supporting nil values }
    class function ObjectEquals(AObj1, AObj2: TObject): Boolean;

    { Port of Grid.java extractSubArray() - line 2403
      Extracts a subarray or creates default grow weights for spanning }
    class function ExtractSubArray(ASpecs: TfpgMigDimConstraintArray;
                                   AArr: TfpgMigFloatArray;
                                   AIx, ALen: Integer): TfpgMigFloatArray;
  end;

implementation

uses
  fpg_mig_platformdefaults;

{ Helper function - returns the bounded value if sz is outside lower/upper bounds }
function GetBrokenBoundary(ASz, ALower, AUpper: Single): Integer; forward;

{ TfpgMigFlowSizeSpec }

constructor TfpgMigFlowSizeSpec.Create(ASizes: TfpgMigSizeArrayArray;
  AResConstr: TfpgMigResizeConstraintArray);
var
  i: Integer;
begin
  inherited Create;
  SetLength(FSizes, Length(ASizes));
  for i := 0 to High(ASizes) do
    FSizes[i] := ASizes[i];
  FResConstsInclGaps := AResConstr;
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

function TfpgMigFlowSizeSpec.GetSizes: TfpgMigSizeArrayArray;
begin
  Result := FSizes;
end;

function TfpgMigFlowSizeSpec.CalculateSerial(AStartSizeType, ABounds: Integer): TfpgMigIntegerArray;
begin
  Result := TfpgMigLayoutUtil.CalculateSerial(FSizes, FResConstsInclGaps, nil, AStartSizeType, ABounds);
end;

function TfpgMigFlowSizeSpec.ExpandSizes(ASpecs: TfpgMigDimConstraintArray;
  ADefGrow: TfpgMigFloatArray; ATargetSize, AFromIx, ALen, ASizeType, AEagerness: Integer): Integer;
var
  resConstr: TfpgMigResizeConstraintArray;
  sizesToExpand: TfpgMigSizeArrayArray;
  i, cIx: Integer;
  minPrefMax: TfpgMigSizeArray;
  spec: TfpgMigDimConstraint;
  sz: TfpgMigBoundSize;
  growW: TfpgMigFloatArray;
  newSizes: TfpgMigIntegerArray;
  s, newSize: Integer;
begin
  // Port of Grid.java FlowSizeSpec.expandSizes() - lines 2369-2400
  if ALen < 1 then
    Exit(0);
  SetLength(resConstr, ALen);
  SetLength(sizesToExpand, ALen);

  for i := 0 to ALen - 1 do
  begin
    minPrefMax := FSizes[i + AFromIx];
    // Initialize the size array for this element
    sizesToExpand[i][SIZE_MIN] := minPrefMax[ASizeType];
    sizesToExpand[i][SIZE_PREF] := minPrefMax[SIZE_PREF];
    sizesToExpand[i][SIZE_MAX] := minPrefMax[SIZE_MAX];

    // Line 2377: Check if we should skip this element based on eagerness
    // (i % 2 == 0) means only odd indexes, which is only rows/col indexes and not gaps
    if (AEagerness <= 1) and ((i mod 2) = 0) then
    begin
      cIx := (i + AFromIx - 1) shr 1;
      spec := TfpgMigLayoutUtil.GetIndexSafe(ASpecs, cIx) as TfpgMigDimConstraint;

      if spec <> nil then
      begin
        sz := spec.GetSize;
        if sz <> nil then
        begin
          // Skip if we're expanding min and it has an explicit min (not MIN_SIZE)
          if (ASizeType = SIZE_MIN) and (sz.Min <> nil) and (sz.Min.UnitType <> utMinSize) then
            Continue;
          // Skip if we're expanding pref and it has an explicit pref (not PREF_SIZE)
          if (ASizeType = SIZE_PREF) and (sz.Preferred <> nil) and (sz.Preferred.UnitType <> utPrefSize) then
            Continue;
        end;
      end;
    end;

    resConstr[i] := TfpgMigLayoutUtil.GetIndexSafe(FResConstsInclGaps, i + AFromIx);
  end;

  // Line 2390: Determine if we should use default grow weights
  if (AEagerness = 1) or (AEagerness = 3) then
    growW := TfpgMigLayoutUtil.ExtractSubArray(ASpecs, ADefGrow, AFromIx, ALen)
  else
    growW := nil;

  // Line 2391: Calculate new sizes
  newSizes := TfpgMigLayoutUtil.CalculateSerial(sizesToExpand, resConstr, growW, SIZE_PREF, ATargetSize);
  newSize := 0;

  // Line 2394-2398: Apply new sizes and calculate total
  for i := 0 to ALen - 1 do
  begin
    s := newSizes[i];
    FSizes[i + AFromIx][ASizeType] := s;
    newSize := newSize + s;
  end;

  Result := newSize;
end;

{ TfpgMigLayoutUtil }

class function TfpgMigLayoutUtil.CalculateSerial(const ASizes: TfpgMigSizeArrayArray;
  const AResConstr: TfpgMigResizeConstraintArray;
  const ADefPushWeights: TfpgMigFloatArray; AStartSizeType, ABounds: Integer): TfpgMigIntegerArray;
var
  lengths: TfpgMigFloatArray;
  usedLength: Single;
  i, useLengthI, newSizeBounded: Integer;
  len: Single;
  isGrow: Boolean;
  prioList: TfpgMigIntegerList;
  prioIntegers: TfpgMigIntegerArray;
  force, pr, curPrio: Integer;
  resC: TfpgMigResizeConstraint;
  totWeight: Single;
  resizeWeight: TfpgMigFloatArray;
  prio: Integer;
  hit: Boolean;
  toChange, changedWeight, sizeDelta, newSize: Single;
  weight: Single;
begin
  SetLength(lengths, Length(ASizes));
  usedLength := 0.0;

  // Give all preferred size to start with
  for i := 0 to High(ASizes) do
  begin
    if ASizes[i][AStartSizeType] <> NOT_SET then
      len := ASizes[i][AStartSizeType]
    else
      len := 0;

    newSizeBounded := GetBrokenBoundary(len, ASizes[i][SIZE_MIN], ASizes[i][SIZE_MAX]);
    if newSizeBounded <> NOT_SET then
      len := newSizeBounded;

    usedLength := usedLength + len;
    lengths[i] := len;
  end;

  useLengthI := Round(usedLength);
  if (useLengthI <> ABounds) and (Length(AResConstr) > 0) then
  begin
    isGrow := useLengthI < ABounds;

    // Create a list with the available priorities
    prioList := TfpgMigIntegerList.Create;
    try
      for i := 0 to High(ASizes) do
      begin
        resC := GetIndexSafe(AResConstr, i);
        if resC <> nil then
        begin
          if isGrow then
            prio := resC.GrowPrio
          else
            prio := resC.ShrinkPrio;

          if prioList.IndexOf(prio) < 0 then
            prioList.Add(prio);
        end;
      end;

      prioList.Sort;
      SetLength(prioIntegers, prioList.Count);
      for i := 0 to prioList.Count - 1 do
        prioIntegers[i] := prioList[i];
    finally
      prioList.Free;
    end;

    // Run twice if defPushWeights and need for growing
    for force := 0 to IfThen(isGrow and (Length(ADefPushWeights) > 0), 1, 0) do
    begin
      // Process priorities from highest to lowest
      for pr := High(prioIntegers) downto 0 do
      begin
        curPrio := prioIntegers[pr];

        totWeight := 0.0;
        SetLength(resizeWeight, Length(ASizes));
        for i := 0 to High(resizeWeight) do
          resizeWeight[i] := NaN;

        for i := 0 to High(ASizes) do
        begin
          resC := GetIndexSafe(AResConstr, i);
          if resC <> nil then
          begin
            if isGrow then
              prio := resC.GrowPrio
            else
              prio := resC.ShrinkPrio;

            if curPrio = prio then
            begin
              if isGrow then
              begin
                if (force = 0) or not IsNaN(resC.Grow) then
                  resizeWeight[i] := resC.Grow
                else if Length(ADefPushWeights) > 0 then
                  resizeWeight[i] := ADefPushWeights[IfThen(i < Length(ADefPushWeights), i, Length(ADefPushWeights) - 1)]
                else
                  resizeWeight[i] := NaN;
              end
              else
                resizeWeight[i] := resC.Shrink;

              if not IsNaN(resizeWeight[i]) then
                totWeight := totWeight + resizeWeight[i];
            end;
          end;
        end;

        if totWeight > 0.0 then
        begin
          repeat
            toChange := ABounds - usedLength;
            hit := False;
            changedWeight := 0.0;

            for i := 0 to High(ASizes) do
            begin
              if totWeight <= 0.0001 then
                Break;

              weight := resizeWeight[i];
              if not IsNaN(weight) and (weight > 0) then
              begin
                sizeDelta := toChange * weight / totWeight;
                newSize := lengths[i] + sizeDelta;

                newSizeBounded := GetBrokenBoundary(newSize, ASizes[i][SIZE_MIN], ASizes[i][SIZE_MAX]);
                if newSizeBounded <> NOT_SET then
                begin
                  sizeDelta := newSizeBounded - lengths[i];
                  resizeWeight[i] := NaN;  // Don't use this one anymore
                  hit := True;
                  changedWeight := changedWeight + weight;
                end;

                lengths[i] := lengths[i] + sizeDelta;
                usedLength := usedLength + sizeDelta;
              end;
            end;

            totWeight := totWeight - changedWeight;
          until not hit;
        end;
      end;
    end;
  end;

  // Convert float lengths to integer results
  Result := RoundSizes(lengths);
end;

function GetBrokenBoundary(ASz, ALower, AUpper: Single): Integer;
begin
  Result := NOT_SET;

  if (ALower <> NOT_SET) and (ASz < ALower) then
    Exit(Round(ALower));

  if (AUpper <> NOT_SET) and (ASz > AUpper) then
    Exit(Round(AUpper));

  Result := NOT_SET;
end;

class function TfpgMigLayoutUtil.GetIndexSafe(const AArr: TfpgMigResizeConstraintArray;
  AIndex: Integer): TfpgMigResizeConstraint;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < Length(AArr)) then
    Result := AArr[AIndex]
  else if Length(AArr) > 0 then
    Result := AArr[High(AArr)]  // Use last element for indices beyond array
  else
    Result := nil;
end;

class function TfpgMigLayoutUtil.GetIndexSafe(const AArr: TfpgMigDimConstraintArray;
  AIndex: Integer): TfpgMigDimConstraint;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < Length(AArr)) then
    Result := AArr[AIndex]
  else if Length(AArr) > 0 then
    Result := AArr[High(AArr)]  // Use last element for indices beyond array
  else
    Result := nil;
end;

class function TfpgMigLayoutUtil.ExtractSubArray(ASpecs: TfpgMigDimConstraintArray;
  AArr: TfpgMigFloatArray; AIx, ALen: Integer): TfpgMigFloatArray;
var
  i, specIx: Integer;
  spec: TfpgMigDimConstraint;
begin
  // Port of Grid.java extractSubArray() - lines 2403-2422

  if (Length(AArr) = 0) or (Length(AArr) < AIx + ALen) then
  begin
    SetLength(Result, ALen);

    // Handle a group where some rows (first one/few and/or last one/few) are docks
    // Line 2409-2416
    // In Java, this checks for DOCK_DIM_CONSTRAINT which has grow priority = 0
    // We check for non-nil spec with grow priority > 0
    // Java uses "i -= 2" to iterate only over column/row indices (odd), not gaps (even)
    //
    // IMPORTANT: Original Java code sets only ONE element and returns early.
    // This works for normal cases, but for SPANNING components, we need ALL
    // spanned columns to grow. So we set WEIGHT_100 for all non-dock column
    // indices in the range, not just the first one found.
    i := AIx + ALen - 1;
    while i >= AIx do
    begin
      specIx := i shr 1;
      if specIx < Length(ASpecs) then
      begin
        spec := ASpecs[specIx];
        // If spec is not nil and not a dock (grow priority > 0), use it
        if (spec <> nil) and (spec.GetGrowPriority > 0) then
        begin
          Result[i - AIx] := WEIGHT_100;
          // Don't exit early - continue to set weights for all non-dock columns
        end;
      end;
      Dec(i, 2);  // Step by 2 to only visit column/row indices, not gaps
    end;

    Exit;
  end;

  // Line 2419-2421: Copy subarray
  SetLength(Result, ALen);
  Move(AArr[AIx], Result[0], ALen * SizeOf(Single));
end;

class function TfpgMigLayoutUtil.GetSizeSafe(const ASizes: TfpgMigSizeArray;
  ASizeType: Integer): Integer;
begin
  Result := ASizes[ASizeType];
  if Result = NOT_SET then
    Result := 0;
end;

class function TfpgMigLayoutUtil.Sum(const ATerms: TfpgMigIntegerArray; AStart,
  ALen: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := AStart to Min(AStart + ALen - 1, High(ATerms)) do
    Result := Result + ATerms[i];
end;

class function TfpgMigLayoutUtil.Sum(const ATerms: TfpgMigIntegerArray): Integer;
begin
  Result := Sum(ATerms, 0, Length(ATerms));
end;

class function TfpgMigLayoutUtil.Clamp(AValue, AMin, AMax: Single): Single;
begin
  Result := Math.Max(AMin, Math.Min(AValue, AMax));
end;

class function TfpgMigLayoutUtil.Clamp(AValue, AMin, AMax: Integer): Integer;
begin
  Result := Math.Max(AMin, Math.Min(AValue, AMax));
end;

class function TfpgMigLayoutUtil.RoundSizes(const ASizes: TfpgMigFloatArray): TfpgMigIntegerArray;
var
  i: Integer;
begin
  SetLength(Result, Length(ASizes));
  for i := 0 to High(ASizes) do
    Result[i] := Round(ASizes[i]);
end;

class function TfpgMigLayoutUtil.Derive(ABase: TfpgMigBoundSize; AMin, APref,
  AMax: TfpgMigUnitValue): TfpgMigBoundSize;
var
  min, pref, max: TfpgMigUnitValue;
begin
  if ABase <> nil then
  begin
    if AMin = nil then min := ABase.Min else min := AMin;
    if APref = nil then pref := ABase.Preferred else pref := APref;
    if AMax = nil then max := ABase.Max else max := AMax;
  end
  else
  begin
    min := AMin;
    pref := APref;
    max := AMax;
  end;

  Result := TfpgMigBoundSize.Create(min, pref, max);
end;

class function TfpgMigLayoutUtil.IsLeftToRight(ALC: TfpgMigLC;
  AContainer: TfpgWidgetBase): Boolean;
begin
  // For now, always return True (LTR)
  // TODO: Implement proper BiDi support when needed
  Result := True;
end;

class function TfpgMigLayoutUtil.GetInsets(ALC: TfpgMigLC; ASide: Integer;
  AGetDefault: Boolean): TfpgMigUnitValue;
var
  i: TfpgMigUnitValueArray;
begin
  if ALC <> nil then
    i := ALC.GetInsets
  else
    i := nil;

  if (i <> nil) and (Length(i) > ASide) and (i[ASide] <> nil) then
    Result := i[ASide]
  else if AGetDefault then
    Result := TfpgMigPlatformDefaults.GetPanelInsets(ASide)
  else
    Result := UnitValueZero();
end;

class function TfpgMigLayoutUtil.ObjectEquals(AObj1, AObj2: TObject): Boolean;
begin
  if (AObj1 = nil) and (AObj2 = nil) then
    Result := True
  else if (AObj1 = nil) or (AObj2 = nil) then
    Result := False
  else
    Result := AObj1 = AObj2;
end;

end.
