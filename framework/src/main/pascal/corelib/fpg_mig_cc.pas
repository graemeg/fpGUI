unit fpg_mig_cc;

{
  MigLayout v11 CC (Component Constraints) for fpGUI

  Ported from: net.miginfocom.layout.CC.java (v11.4.2)

  Widget-level constraints. This is the most complex constraint class
  with 79 fluent API methods for configuring component placement, sizing,
  alignment, gaps, spanning, docking, and more.
}

{$I fpg_defines.inc}

interface

uses
  Classes, SysUtils,
  fpg_layouttypes,  // For TfpgLayoutConstraint
  fpg_mig_boundsize, fpg_mig_unitvalue, fpg_mig_dimconstraint,
  fpg_mig_constraintparser;

const
  INF_SIZE = 2147471302;

type
  { Array type for position and padding }
  TfpgMigUnitValueArray = array of TfpgMigUnitValue;

  { Dock sides constants }
  TfpgMigDockSide = (dsNone = -1, dsNorth = 0, dsWest = 1, dsSouth = 2, dsEast = 3);

  { Component Constraints - widget-level layout constraints }
  TfpgMigCC = class(TfpgLayoutConstraint)
  private
    // Docking
    FDock: Integer;  // -1 = no dock, 0=north, 1=west, 2=south, 3=east

    // Absolute positioning [x1, y1, x2, y2]
    FPos: array[0..3] of TfpgMigUnitValue;

    // Padding [top, left, bottom, right]
    FPadding: array[0..3] of TfpgMigUnitValue;

    // Visual padding [top, left, bottom, right]
    FVisualPadding: array[0..3] of TfpgMigUnitValue;

    // Flow direction override
    FFlowX: Integer;  // 0=nil, 1=false, 2=true (nullable boolean)

    // Skip n cells
    FSkip: Integer;

    // Split into n columns/rows
    FSplit: Integer;

    // Span cells
    FSpanX, FSpanY: Integer;

    // Cell position
    FCellX, FCellY: Integer;  // If CellX is -1, CellY is also considered -1

    // Tag for logical grouping
    FTag: string;

    // ID for linking components
    FId: string;

    // Hide mode
    FHideMode: Integer;  // -1 = not set

    // Dimension constraints (horizontal and vertical)
    FHor: TfpgMigDimConstraint;
    FVer: TfpgMigDimConstraint;

    // Wrap/newline gaps
    FNewline: TfpgMigBoundSize;
    FWrap: TfpgMigBoundSize;

    // Bounds in grid
    FBoundsInGrid: Boolean;

    // External (component is outside container)
    FExternal: Boolean;

    // Push weights
    FPushX, FPushY: Single;  // Using Single for nullable Float (NaN = null)

    // Link targets cache
    FLinkTargets: array of string;

    // Getters/Setters
    function GetDock: Integer;
    procedure SetDock(AValue: Integer);

    function GetFlowX: Integer;
    procedure SetFlowX(AValue: Integer);

    function GetSkip: Integer;
    procedure SetSkip(AValue: Integer);

    function GetSplit: Integer;
    procedure SetSplit(AValue: Integer);

    function GetSpanX: Integer;
    procedure SetSpanX(AValue: Integer);

    function GetSpanY: Integer;
    procedure SetSpanY(AValue: Integer);

    function GetCellX: Integer;
    procedure SetCellX(AValue: Integer);

    function GetCellY: Integer;
    procedure SetCellY(AValue: Integer);

    procedure SetTag(const AValue: string);

    function GetId: string;
    procedure SetId(const AValue: string);

    function GetHideMode: Integer;
    procedure SetHideMode(AValue: Integer);

    function GetHorizontal: TfpgMigDimConstraint;
    function GetVertical: TfpgMigDimConstraint;

    function GetNewline: TfpgMigBoundSize;
    procedure SetNewline(AValue: TfpgMigBoundSize);

    function GetWrapGap: TfpgMigBoundSize;
    procedure SetWrapGap(AValue: TfpgMigBoundSize);

    function GetBoundsInGrid: Boolean;
    procedure SetBoundsInGrid(AValue: Boolean);

    function GetExternal: Boolean;
    procedure SetExternal(AValue: Boolean);

    function GetPushX: Single;
    procedure SetPushX(AValue: Single);

    function GetPushY: Single;
    procedure SetPushY(AValue: Single);

  public
    constructor Create;
    destructor Destroy; override;

    function GetDimConstraint(AIsHor: Boolean): TfpgMigDimConstraint;
    function GetTag: string;

    { === Dimension Constraints (Width) === }

    { Sets size group for width }
    function SizeGroupX(const AGroup: string): TfpgMigCC;

    { Sets end group for width }
    function EndGroupX(const AGroup: string): TfpgMigCC;

    { Sets minimum width }
    function MinWidth(const ASize: string): TfpgMigCC;

    { Sets width as min:pref:max }
    function Width(const ASize: string): TfpgMigCC;

    { Sets maximum width }
    function MaxWidth(const ASize: string): TfpgMigCC;

    { Sets horizontal gap before and after }
    function GapX(const ABefore, AAfter: string): TfpgMigCC;

    { Sets horizontal alignment }
    function AlignX(const AAlign: string): TfpgMigCC;

    { Sets horizontal grow priority }
    function GrowPrioX(APriority: Integer): TfpgMigCC;

    { Sets horizontal grow with priority 100 }
    function GrowX: TfpgMigCC; overload;

    { Sets horizontal grow weight }
    function GrowX(AWeight: Single): TfpgMigCC; overload;

    { Sets grow for both dimensions }
    function Grow(AWeightX, AWeightY: Single): TfpgMigCC; overload;
    function Grow: TfpgMigCC; overload;

    { Sets grow priority for both dimensions }
    function GrowPrio(APrioX, APrioY: Integer): TfpgMigCC;

    { Sets horizontal shrink priority }
    function ShrinkPrioX(APriority: Integer): TfpgMigCC;

    { Sets horizontal shrink weight }
    function ShrinkX(AWeight: Single): TfpgMigCC;

    { Sets shrink for both dimensions }
    function Shrink(AWeightX, AWeightY: Single): TfpgMigCC; overload;
    function Shrink: TfpgMigCC; overload;

    { Sets shrink priority for both dimensions }
    function ShrinkPrio(APrioX, APrioY: Integer): TfpgMigCC;

    { === Dimension Constraints (Height) === }

    { Sets size group for height }
    function SizeGroupY(const AGroup: string): TfpgMigCC;

    { Sets size group for both dimensions }
    function SizeGroup(const AGroupX, AGroupY: string): TfpgMigCC;

    { Sets end group for height }
    function EndGroupY(const AGroup: string): TfpgMigCC;

    { Sets end group for both dimensions }
    function EndGroup(const AGroupX, AGroupY: string): TfpgMigCC;

    { Sets minimum height }
    function MinHeight(const ASize: string): TfpgMigCC;

    { Sets height as min:pref:max }
    function Height(const ASize: string): TfpgMigCC;

    { Sets maximum height }
    function MaxHeight(const ASize: string): TfpgMigCC;

    { Sets vertical gap before and after }
    function GapY(const ABefore, AAfter: string): TfpgMigCC;

    { Sets vertical alignment }
    function AlignY(const AAlign: string): TfpgMigCC;

    { Sets vertical grow priority }
    function GrowPrioY(APriority: Integer): TfpgMigCC;

    { Sets vertical grow with priority 100 }
    function GrowY: TfpgMigCC; overload;

    { Sets vertical grow weight }
    function GrowY(AWeight: Single): TfpgMigCC; overload;

    { Sets vertical shrink priority }
    function ShrinkPrioY(APriority: Integer): TfpgMigCC;

    { Sets vertical shrink weight }
    function ShrinkY(AWeight: Single): TfpgMigCC;

    { === Gap Methods === }

    { Sets all gaps (before, after, top, bottom) }
    function Gap(const AGaps: string): TfpgMigCC; overload;
    function Gap(const ABefore, AAfter, ATop, ABottom: string): TfpgMigCC; overload;

    { Sets gap before component }
    function GapBefore(const AGap: string): TfpgMigCC;

    { Sets gap after component }
    function GapAfter(const AGap: string): TfpgMigCC;

    { Sets gap at top }
    function GapTop(const AGap: string): TfpgMigCC;

    { Sets gap at bottom }
    function GapBottom(const AGap: string): TfpgMigCC;

    { Sets gap at left }
    function GapLeft(const AGap: string): TfpgMigCC;

    { Sets gap at right }
    function GapRight(const AGap: string): TfpgMigCC;

    { === Cell and Spanning === }

    { Sets cell position }
    function Cell(ACol, ARow: Integer): TfpgMigCC; overload;
    function Cell(ACol, ARow, AWidth, AHeight: Integer): TfpgMigCC; overload;

    { Sets span }
    function Span(ACellsX, ACellsY: Integer): TfpgMigCC; overload;
    function Span: TfpgMigCC; overload;

    { Sets horizontal span }
    function SpanX(ACells: Integer): TfpgMigCC; overload;
    function SpanX: TfpgMigCC; overload;

    { Sets vertical span }
    function SpanY(ACells: Integer): TfpgMigCC; overload;
    function SpanY: TfpgMigCC; overload;

    { === Flow and Wrapping === }

    { Sets flow direction to horizontal }
    function FlowX: TfpgMigCC;

    { Sets flow direction to vertical }
    function FlowY: TfpgMigCC;

    { Skip cells }
    function Skip(ACells: Integer): TfpgMigCC; overload;
    function Skip: TfpgMigCC; overload;

    { Split into columns/rows }
    function Split(AParts: Integer): TfpgMigCC; overload;
    function Split: TfpgMigCC; overload;

    { Wrap to new line after this component }
    function Wrap: TfpgMigCC; overload;
    function Wrap(const AGap: string): TfpgMigCC; overload;

    { New line before this component }
    function Newline: TfpgMigCC; overload;
    function Newline(const AGap: string): TfpgMigCC; overload;

    { === Docking === }

    { Dock component to north }
    function DockNorth: TfpgMigCC;

    { Dock component to west }
    function DockWest: TfpgMigCC;

    { Dock component to south }
    function DockSouth: TfpgMigCC;

    { Dock component to east }
    function DockEast: TfpgMigCC;

    { === Positioning === }

    { Sets absolute position }
    function Pos(const AX1, AY1: string): TfpgMigCC; overload;
    function Pos(const AX1, AY1, AX2, AY2: string): TfpgMigCC; overload;

    { Sets X position }
    function X(const AX: string): TfpgMigCC;

    { Sets X2 position }
    function X2(const AX2: string): TfpgMigCC;

    { Sets Y position }
    function Y(const AY: string): TfpgMigCC;

    { Sets Y2 position }
    function Y2(const AY2: string): TfpgMigCC;

    { === Padding === }

    { Sets padding }
    function Pad(const APad: string): TfpgMigCC; overload;
    function Pad(const ATop, ALeft, ABottom, ARight: string): TfpgMigCC; overload;

    { === Miscellaneous === }

    { Sets hide mode }
    function HideMode(AMode: Integer): TfpgMigCC;

    { Sets component ID for linking }
    function Id(const AId: string): TfpgMigCC;

    { Sets tag for logical grouping }
    function Tag(const ATag: string): TfpgMigCC;

    { Sets external flag }
    function External: TfpgMigCC; overload;
    function External(AValue: Boolean): TfpgMigCC; overload;

    { Sets push weight }
    function Push: TfpgMigCC; overload;
    function Push(AWeightX, AWeightY: Single): TfpgMigCC; overload;

    { Sets horizontal push weight }
    function PushX: TfpgMigCC; overload;
    function PushX(AWeight: Single): TfpgMigCC; overload;

    { Sets vertical push weight }
    function PushY: TfpgMigCC; overload;
    function PushY(AWeight: Single): TfpgMigCC; overload;

    { === Property Access === }

    property Dock: Integer read GetDock write SetDock;
    property IsFlowX: Integer read GetFlowX write SetFlowX;
    property SkipCells: Integer read GetSkip write SetSkip;
    property SplitParts: Integer read GetSplit write SetSplit;
    property CellSpanX: Integer read GetSpanX write SetSpanX;
    property CellSpanY: Integer read GetSpanY write SetSpanY;
    property CellX: Integer read GetCellX write SetCellX;
    property CellY: Integer read GetCellY write SetCellY;
    property TagValue: string read GetTag write SetTag;
    property IdValue: string read GetId write SetId;
    property HideModeValue: Integer read GetHideMode write SetHideMode;
    property Horizontal: TfpgMigDimConstraint read GetHorizontal;
    property Vertical: TfpgMigDimConstraint read GetVertical;
    property NewlineGap: TfpgMigBoundSize read GetNewline write SetNewline;
    property WrapGap: TfpgMigBoundSize read GetWrapGap write SetWrapGap;
    property IsBoundsInGrid: Boolean read GetBoundsInGrid write SetBoundsInGrid;
    property IsExternal: Boolean read GetExternal write SetExternal;
    property PushWeightX: Single read GetPushX write SetPushX;
    property PushWeightY: Single read GetPushY write SetPushY;
  end;

{ Default null gap constant }
function CCDefGap: TfpgMigBoundSize;

implementation

uses
  Math;

var
  GCCDefGap: TfpgMigBoundSize = nil;

function CCDefGap: TfpgMigBoundSize;
begin
  if GCCDefGap = nil then
    GCCDefGap := BoundSizeNullSize;
  Result := GCCDefGap;
end;

{ TfpgMigCC }

constructor TfpgMigCC.Create;
var
  i: Integer;
begin
  inherited Create;

  // Initialize to defaults
  FDock := -1;

  for i := 0 to 3 do
  begin
    FPos[i] := nil;
    FPadding[i] := nil;
    FVisualPadding[i] := nil;
  end;

  FFlowX := 0;  // nil
  FSkip := 0;
  FSplit := 1;
  FSpanX := 1;
  FSpanY := 1;
  FCellX := -1;
  FCellY := 0;
  FTag := '';
  FId := '';
  FHideMode := -1;

  FHor := TfpgMigDimConstraint.Create;
  FVer := TfpgMigDimConstraint.Create;

  FNewline := nil;
  FWrap := nil;

  FBoundsInGrid := True;
  FExternal := False;

  FPushX := NaN;  // null
  FPushY := NaN;  // null

  SetLength(FLinkTargets, 0);
end;

destructor TfpgMigCC.Destroy;
var
  i: Integer;
begin
  FHor.Free;
  FVer.Free;

  // Free BoundSize objects if not constants
  if (FNewline <> nil) and (FNewline <> CCDefGap) then
    FNewline.Free;
  if (FWrap <> nil) and (FWrap <> CCDefGap) then
    FWrap.Free;

  // Free UnitValue arrays
  for i := 0 to 3 do
  begin
    FreeAndNil(FPos[i]);
    FreeAndNil(FPadding[i]);
    FreeAndNil(FVisualPadding[i]);
  end;

  inherited Destroy;
end;

{ Getters and Setters }

function TfpgMigCC.GetDock: Integer;
begin
  Result := FDock;
end;

procedure TfpgMigCC.SetDock(AValue: Integer);
begin
  FDock := AValue;
end;

function TfpgMigCC.GetFlowX: Integer;
begin
  Result := FFlowX;
end;

procedure TfpgMigCC.SetFlowX(AValue: Integer);
begin
  FFlowX := AValue;
end;

function TfpgMigCC.GetSkip: Integer;
begin
  Result := FSkip;
end;

procedure TfpgMigCC.SetSkip(AValue: Integer);
begin
  FSkip := Max(0, AValue);
end;

function TfpgMigCC.GetSplit: Integer;
begin
  Result := FSplit;
end;

procedure TfpgMigCC.SetSplit(AValue: Integer);
begin
  FSplit := Max(1, AValue);
end;

function TfpgMigCC.GetSpanX: Integer;
begin
  Result := FSpanX;
end;

procedure TfpgMigCC.SetSpanX(AValue: Integer);
begin
  FSpanX := Max(1, AValue);
end;

function TfpgMigCC.GetSpanY: Integer;
begin
  Result := FSpanY;
end;

procedure TfpgMigCC.SetSpanY(AValue: Integer);
begin
  FSpanY := Max(1, AValue);
end;

function TfpgMigCC.GetCellX: Integer;
begin
  Result := FCellX;
end;

procedure TfpgMigCC.SetCellX(AValue: Integer);
begin
  FCellX := AValue;
end;

function TfpgMigCC.GetCellY: Integer;
begin
  Result := FCellY;
end;

procedure TfpgMigCC.SetCellY(AValue: Integer);
begin
  FCellY := Max(0, AValue);
end;

function TfpgMigCC.GetTag: string;
begin
  Result := FTag;
end;

procedure TfpgMigCC.SetTag(const AValue: string);
begin
  FTag := AValue;
end;

function TfpgMigCC.GetId: string;
begin
  Result := FId;
end;

procedure TfpgMigCC.SetId(const AValue: string);
begin
  FId := AValue;
end;

function TfpgMigCC.GetHideMode: Integer;
begin
  Result := FHideMode;
end;

procedure TfpgMigCC.SetHideMode(AValue: Integer);
begin
  FHideMode := AValue;
end;

function TfpgMigCC.GetHorizontal: TfpgMigDimConstraint;
begin
  Result := FHor;
end;

function TfpgMigCC.GetVertical: TfpgMigDimConstraint;
begin
  Result := FVer;
end;

function TfpgMigCC.GetDimConstraint(AIsHor: Boolean): TfpgMigDimConstraint;
begin
  if AIsHor then
    Result := FHor
  else
    Result := FVer;
end;

function TfpgMigCC.GetNewline: TfpgMigBoundSize;
begin
  Result := FNewline;
end;

procedure TfpgMigCC.SetNewline(AValue: TfpgMigBoundSize);
begin
  // Free old BoundSize if it's not a constant
  if (FNewline <> nil) and (FNewline <> CCDefGap) then
    FNewline.Free;
  FNewline := AValue;
end;

function TfpgMigCC.GetWrapGap: TfpgMigBoundSize;
begin
  Result := FWrap;
end;

procedure TfpgMigCC.SetWrapGap(AValue: TfpgMigBoundSize);
begin
  // Free old BoundSize if it's not a constant
  if (FWrap <> nil) and (FWrap <> CCDefGap) then
    FWrap.Free;
  FWrap := AValue;
end;

function TfpgMigCC.GetBoundsInGrid: Boolean;
begin
  Result := FBoundsInGrid;
end;

procedure TfpgMigCC.SetBoundsInGrid(AValue: Boolean);
begin
  FBoundsInGrid := AValue;
end;

function TfpgMigCC.GetExternal: Boolean;
begin
  Result := FExternal;
end;

procedure TfpgMigCC.SetExternal(AValue: Boolean);
begin
  FExternal := AValue;
end;

function TfpgMigCC.GetPushX: Single;
begin
  Result := FPushX;
end;

procedure TfpgMigCC.SetPushX(AValue: Single);
begin
  FPushX := AValue;
end;

function TfpgMigCC.GetPushY: Single;
begin
  Result := FPushY;
end;

procedure TfpgMigCC.SetPushY(AValue: Single);
begin
  FPushY := AValue;
end;

{ Fluent API Methods - Dimension Constraints (Width) }

function TfpgMigCC.SizeGroupX(const AGroup: string): TfpgMigCC;
begin
  FHor.SetSizeGroup(AGroup);
  Result := Self;
end;

function TfpgMigCC.EndGroupX(const AGroup: string): TfpgMigCC;
begin
  FHor.SetEndGroup(AGroup);
  Result := Self;
end;

function TfpgMigCC.MinWidth(const ASize: string): TfpgMigCC;
var
  minVal: TfpgMigUnitValue;
  newSize: TfpgMigBoundSize;
begin
  minVal := ParseUnitValue(ASize, True);
  try
    newSize := TfpgMigBoundSize.Create(minVal, FHor.GetSize.Preferred, FHor.GetSize.Max, FHor.GetSize.GapPush);
    FHor.SetSize(newSize);
  finally
    minVal.Free;
  end;
  Result := Self;
end;

function TfpgMigCC.Width(const ASize: string): TfpgMigCC;
begin
  FHor.SetSize(ParseBoundSize(ASize, False, True));
  Result := Self;
end;

function TfpgMigCC.MaxWidth(const ASize: string): TfpgMigCC;
var
  maxVal: TfpgMigUnitValue;
  newSize: TfpgMigBoundSize;
begin
  maxVal := ParseUnitValue(ASize, True);
  try
    newSize := TfpgMigBoundSize.Create(FHor.GetSize.Min, FHor.GetSize.Preferred, maxVal, FHor.GetSize.GapPush);
    FHor.SetSize(newSize);
  finally
    maxVal.Free;
  end;
  Result := Self;
end;

function TfpgMigCC.GapX(const ABefore, AAfter: string): TfpgMigCC;
begin
  if ABefore <> '' then
    FHor.SetGapBefore(ParseBoundSize(ABefore, True, True));
  if AAfter <> '' then
    FHor.SetGapAfter(ParseBoundSize(AAfter, True, True));
  Result := Self;
end;

function TfpgMigCC.AlignX(const AAlign: string): TfpgMigCC;
begin
  FHor.SetAlign(ParseUnitValueOrAlign(AAlign, True));
  Result := Self;
end;

function TfpgMigCC.GrowPrioX(APriority: Integer): TfpgMigCC;
begin
  FHor.SetGrowPriority(APriority);
  Result := Self;
end;

function TfpgMigCC.GrowX: TfpgMigCC;
begin
  FHor.SetGrowPriority(100);
  FHor.SetGrowWeight(100.0);  // Set default grow weight
  Result := Self;
end;

function TfpgMigCC.GrowX(AWeight: Single): TfpgMigCC;
begin
  FHor.SetGrowWeight(AWeight);
  Result := Self;
end;

function TfpgMigCC.Grow(AWeightX, AWeightY: Single): TfpgMigCC;
begin
  FHor.SetGrowWeight(AWeightX);
  FVer.SetGrowWeight(AWeightY);
  Result := Self;
end;

function TfpgMigCC.Grow: TfpgMigCC;
begin
  FHor.SetGrowPriority(100);
  FHor.SetGrowWeight(100.0);
  FVer.SetGrowPriority(100);
  FVer.SetGrowWeight(100.0);
  Result := Self;
end;

function TfpgMigCC.GrowPrio(APrioX, APrioY: Integer): TfpgMigCC;
begin
  FHor.SetGrowPriority(APrioX);
  FVer.SetGrowPriority(APrioY);
  Result := Self;
end;

function TfpgMigCC.ShrinkPrioX(APriority: Integer): TfpgMigCC;
begin
  FHor.SetShrinkPriority(APriority);
  Result := Self;
end;

function TfpgMigCC.ShrinkX(AWeight: Single): TfpgMigCC;
begin
  FHor.SetShrinkWeight(AWeight);
  Result := Self;
end;

function TfpgMigCC.Shrink(AWeightX, AWeightY: Single): TfpgMigCC;
begin
  FHor.SetShrinkWeight(AWeightX);
  FVer.SetShrinkWeight(AWeightY);
  Result := Self;
end;

function TfpgMigCC.Shrink: TfpgMigCC;
begin
  FHor.SetShrinkPriority(100);
  FVer.SetShrinkPriority(100);
  Result := Self;
end;

function TfpgMigCC.ShrinkPrio(APrioX, APrioY: Integer): TfpgMigCC;
begin
  FHor.SetShrinkPriority(APrioX);
  FVer.SetShrinkPriority(APrioY);
  Result := Self;
end;

{ Fluent API Methods - Dimension Constraints (Height) }

function TfpgMigCC.SizeGroupY(const AGroup: string): TfpgMigCC;
begin
  FVer.SetSizeGroup(AGroup);
  Result := Self;
end;

function TfpgMigCC.SizeGroup(const AGroupX, AGroupY: string): TfpgMigCC;
begin
  if AGroupX <> '' then
    FHor.SetSizeGroup(AGroupX);
  if AGroupY <> '' then
    FVer.SetSizeGroup(AGroupY);
  Result := Self;
end;

function TfpgMigCC.EndGroupY(const AGroup: string): TfpgMigCC;
begin
  FVer.SetEndGroup(AGroup);
  Result := Self;
end;

function TfpgMigCC.EndGroup(const AGroupX, AGroupY: string): TfpgMigCC;
begin
  if AGroupX <> '' then
    FHor.SetEndGroup(AGroupX);
  if AGroupY <> '' then
    FVer.SetEndGroup(AGroupY);
  Result := Self;
end;

function TfpgMigCC.MinHeight(const ASize: string): TfpgMigCC;
var
  minVal: TfpgMigUnitValue;
  newSize: TfpgMigBoundSize;
begin
  minVal := ParseUnitValue(ASize, False);
  try
    newSize := TfpgMigBoundSize.Create(minVal, FVer.GetSize.Preferred, FVer.GetSize.Max, FVer.GetSize.GapPush);
    FVer.SetSize(newSize);
  finally
    minVal.Free;
  end;
  Result := Self;
end;

function TfpgMigCC.Height(const ASize: string): TfpgMigCC;
begin
  FVer.SetSize(ParseBoundSize(ASize, False, True));
  Result := Self;
end;

function TfpgMigCC.MaxHeight(const ASize: string): TfpgMigCC;
var
  maxVal: TfpgMigUnitValue;
  newSize: TfpgMigBoundSize;
begin
  maxVal := ParseUnitValue(ASize, False);
  try
    newSize := TfpgMigBoundSize.Create(FVer.GetSize.Min, FVer.GetSize.Preferred, maxVal, FVer.GetSize.GapPush);
    FVer.SetSize(newSize);
  finally
    maxVal.Free;
  end;
  Result := Self;
end;

function TfpgMigCC.GapY(const ABefore, AAfter: string): TfpgMigCC;
begin
  if ABefore <> '' then
    FVer.SetGapBefore(ParseBoundSize(ABefore, True, True));
  if AAfter <> '' then
    FVer.SetGapAfter(ParseBoundSize(AAfter, True, True));
  Result := Self;
end;

function TfpgMigCC.AlignY(const AAlign: string): TfpgMigCC;
begin
  FVer.SetAlign(ParseUnitValueOrAlign(AAlign, False));
  Result := Self;
end;

function TfpgMigCC.GrowPrioY(APriority: Integer): TfpgMigCC;
begin
  FVer.SetGrowPriority(APriority);
  Result := Self;
end;

function TfpgMigCC.GrowY: TfpgMigCC;
begin
  FVer.SetGrowPriority(100);
  FVer.SetGrowWeight(100.0);  // Set default grow weight
  Result := Self;
end;

function TfpgMigCC.GrowY(AWeight: Single): TfpgMigCC;
begin
  FVer.SetGrowWeight(AWeight);
  Result := Self;
end;

function TfpgMigCC.ShrinkPrioY(APriority: Integer): TfpgMigCC;
begin
  FVer.SetShrinkPriority(APriority);
  Result := Self;
end;

function TfpgMigCC.ShrinkY(AWeight: Single): TfpgMigCC;
begin
  FVer.SetShrinkWeight(AWeight);
  Result := Self;
end;

{ Fluent API Methods - Gap }

function TfpgMigCC.Gap(const AGaps: string): TfpgMigCC;
begin
  // Parse and set all gaps from a single string
  // TODO: Implement full parsing in later phase
  raise Exception.Create('Gap(single string) not yet implemented - needs parser');
end;

function TfpgMigCC.Gap(const ABefore, AAfter, ATop, ABottom: string): TfpgMigCC;
begin
  if ABefore <> '' then
    FHor.SetGapBefore(ParseBoundSize(ABefore, True, True));
  if AAfter <> '' then
    FHor.SetGapAfter(ParseBoundSize(AAfter, True, True));
  if ATop <> '' then
    FVer.SetGapBefore(ParseBoundSize(ATop, True, False));
  if ABottom <> '' then
    FVer.SetGapAfter(ParseBoundSize(ABottom, True, False));
  Result := Self;
end;

function TfpgMigCC.GapBefore(const AGap: string): TfpgMigCC;
begin
  FHor.SetGapBefore(ParseBoundSize(AGap, True, True));
  Result := Self;
end;

function TfpgMigCC.GapAfter(const AGap: string): TfpgMigCC;
begin
  FHor.SetGapAfter(ParseBoundSize(AGap, True, True));
  Result := Self;
end;

function TfpgMigCC.GapTop(const AGap: string): TfpgMigCC;
begin
  FVer.SetGapBefore(ParseBoundSize(AGap, True, False));
  Result := Self;
end;

function TfpgMigCC.GapBottom(const AGap: string): TfpgMigCC;
begin
  FVer.SetGapAfter(ParseBoundSize(AGap, True, False));
  Result := Self;
end;

function TfpgMigCC.GapLeft(const AGap: string): TfpgMigCC;
begin
  FHor.SetGapBefore(ParseBoundSize(AGap, True, True));
  Result := Self;
end;

function TfpgMigCC.GapRight(const AGap: string): TfpgMigCC;
begin
  FHor.SetGapAfter(ParseBoundSize(AGap, True, True));
  Result := Self;
end;

{ Fluent API Methods - Cell and Spanning }

function TfpgMigCC.Cell(ACol, ARow: Integer): TfpgMigCC;
begin
  FCellX := ACol;
  FCellY := ARow;
  Result := Self;
end;

function TfpgMigCC.Cell(ACol, ARow, AWidth, AHeight: Integer): TfpgMigCC;
begin
  FCellX := ACol;
  FCellY := ARow;
  FSpanX := AWidth;
  FSpanY := AHeight;
  Result := Self;
end;

function TfpgMigCC.Span(ACellsX, ACellsY: Integer): TfpgMigCC;
begin
  FSpanX := Max(1, ACellsX);
  FSpanY := Max(1, ACellsY);
  Result := Self;
end;

function TfpgMigCC.Span: TfpgMigCC;
begin
  FSpanX := INF_SIZE;
  FSpanY := 1;
  Result := Self;
end;

function TfpgMigCC.SpanX(ACells: Integer): TfpgMigCC;
begin
  FSpanX := Max(1, ACells);
  Result := Self;
end;

function TfpgMigCC.SpanX: TfpgMigCC;
begin
  FSpanX := INF_SIZE;
  Result := Self;
end;

function TfpgMigCC.SpanY(ACells: Integer): TfpgMigCC;
begin
  FSpanY := Max(1, ACells);
  Result := Self;
end;

function TfpgMigCC.SpanY: TfpgMigCC;
begin
  FSpanY := INF_SIZE;
  Result := Self;
end;

{ Fluent API Methods - Flow and Wrapping }

function TfpgMigCC.FlowX: TfpgMigCC;
begin
  FFlowX := 2;  // true
  Result := Self;
end;

function TfpgMigCC.FlowY: TfpgMigCC;
begin
  FFlowX := 1;  // false
  Result := Self;
end;

function TfpgMigCC.Skip(ACells: Integer): TfpgMigCC;
begin
  FSkip := Max(0, ACells);
  Result := Self;
end;

function TfpgMigCC.Skip: TfpgMigCC;
begin
  FSkip := 1;
  Result := Self;
end;

function TfpgMigCC.Split(AParts: Integer): TfpgMigCC;
begin
  FSplit := Max(1, AParts);
  Result := Self;
end;

function TfpgMigCC.Split: TfpgMigCC;
begin
  FSplit := INF_SIZE;
  Result := Self;
end;

function TfpgMigCC.Wrap: TfpgMigCC;
begin
  FWrap := CCDefGap;
  Result := Self;
end;

function TfpgMigCC.Wrap(const AGap: string): TfpgMigCC;
begin
  FWrap := ParseBoundSize(AGap, True, True);
  Result := Self;
end;

function TfpgMigCC.Newline: TfpgMigCC;
begin
  FNewline := CCDefGap;
  Result := Self;
end;

function TfpgMigCC.Newline(const AGap: string): TfpgMigCC;
begin
  FNewline := ParseBoundSize(AGap, True, False);
  Result := Self;
end;

{ Fluent API Methods - Docking }

function TfpgMigCC.DockNorth: TfpgMigCC;
begin
  FDock := Ord(TfpgMigDockSide.dsNorth);
  Result := Self;
end;

function TfpgMigCC.DockWest: TfpgMigCC;
begin
  FDock := Ord(TfpgMigDockSide.dsWest);
  Result := Self;
end;

function TfpgMigCC.DockSouth: TfpgMigCC;
begin
  FDock := Ord(TfpgMigDockSide.dsSouth);
  Result := Self;
end;

function TfpgMigCC.DockEast: TfpgMigCC;
begin
  FDock := Ord(TfpgMigDockSide.dsEast);
  Result := Self;
end;

{ Fluent API Methods - Positioning }

function TfpgMigCC.Pos(const AX1, AY1: string): TfpgMigCC;
begin
  FPos[0] := ParseUnitValue(AX1, True);
  FPos[1] := ParseUnitValue(AY1, False);
  Result := Self;
end;

function TfpgMigCC.Pos(const AX1, AY1, AX2, AY2: string): TfpgMigCC;
begin
  FPos[0] := ParseUnitValue(AX1, True);
  FPos[1] := ParseUnitValue(AY1, False);
  FPos[2] := ParseUnitValue(AX2, True);
  FPos[3] := ParseUnitValue(AY2, False);
  Result := Self;
end;

function TfpgMigCC.X(const AX: string): TfpgMigCC;
begin
  FPos[0] := ParseUnitValue(AX, True);
  Result := Self;
end;

function TfpgMigCC.X2(const AX2: string): TfpgMigCC;
begin
  FPos[2] := ParseUnitValue(AX2, True);
  Result := Self;
end;

function TfpgMigCC.Y(const AY: string): TfpgMigCC;
begin
  FPos[1] := ParseUnitValue(AY, False);
  Result := Self;
end;

function TfpgMigCC.Y2(const AY2: string): TfpgMigCC;
begin
  FPos[3] := ParseUnitValue(AY2, False);
  Result := Self;
end;

{ Fluent API Methods - Padding }

function TfpgMigCC.Pad(const APad: string): TfpgMigCC;
var
  insets: TfpgMigUnitValueArray;
  i: Integer;
begin
  insets := ParseInsets(APad, False);
  for i := 0 to Min(3, High(insets)) do
    FPadding[i] := insets[i];
  Result := Self;
end;

function TfpgMigCC.Pad(const ATop, ALeft, ABottom, ARight: string): TfpgMigCC;
begin
  FPadding[0] := ParseUnitValue(ATop, False);
  FPadding[1] := ParseUnitValue(ALeft, True);
  FPadding[2] := ParseUnitValue(ABottom, False);
  FPadding[3] := ParseUnitValue(ARight, True);
  Result := Self;
end;

{ Fluent API Methods - Miscellaneous }

function TfpgMigCC.HideMode(AMode: Integer): TfpgMigCC;
begin
  FHideMode := AMode;
  Result := Self;
end;

function TfpgMigCC.Id(const AId: string): TfpgMigCC;
begin
  FId := AId;
  Result := Self;
end;

function TfpgMigCC.Tag(const ATag: string): TfpgMigCC;
begin
  FTag := ATag;
  Result := Self;
end;

function TfpgMigCC.External: TfpgMigCC;
begin
  FExternal := True;
  Result := Self;
end;

function TfpgMigCC.External(AValue: Boolean): TfpgMigCC;
begin
  FExternal := AValue;
  Result := Self;
end;

function TfpgMigCC.Push: TfpgMigCC;
begin
  FPushX := 100.0;
  FPushY := 100.0;
  Result := Self;
end;

function TfpgMigCC.Push(AWeightX, AWeightY: Single): TfpgMigCC;
begin
  FPushX := AWeightX;
  FPushY := AWeightY;
  Result := Self;
end;

function TfpgMigCC.PushX: TfpgMigCC;
begin
  FPushX := 100.0;
  Result := Self;
end;

function TfpgMigCC.PushX(AWeight: Single): TfpgMigCC;
begin
  FPushX := AWeight;
  Result := Self;
end;

function TfpgMigCC.PushY: TfpgMigCC;
begin
  FPushY := 100.0;
  Result := Self;
end;

function TfpgMigCC.PushY(AWeight: Single): TfpgMigCC;
begin
  FPushY := AWeight;
  Result := Self;
end;

end.
