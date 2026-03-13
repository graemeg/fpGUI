unit fpg_mig_lc;

{
  MigLayout v11 LC (Layout Constraints) for fpGUI

  Ported from: net.miginfocom.layout.LC.java (v11.4.2)

  Container-level layout constraints - controls overall layout behavior:
  - Flow direction (horizontal/vertical)
  - Grid gaps
  - Insets (padding around container)
  - Fill behavior
  - Wrap behavior
  - Alignment
  - Debug mode
  - Hide mode for invisible components

  Supports fluent API for building constraints:
    TfpgMigLC.Create.FlowX.Fill.Wrap.GridGap('10px', '10px')
}

{$I fpg_defines.inc}

interface

uses
  Classes, SysUtils, fpg_mig_unitvalue, fpg_mig_boundsize, fpg_mig_hidemode;

const
  INF_SIZE = 2000000000;  // Effectively infinite for layout calculations

type
  { Array type for insets }
  TfpgMigUnitValueArray = array of TfpgMigUnitValue;

  { TfpgMigLC - Layout Constraints (container-level)

    Holds constraints for the entire layout container.
    Uses fluent API pattern - methods return Self for chaining.
  }
  TfpgMigLC = class
  private
    FWrapAfter: Integer;
    FLeftToRight: Integer;  // 0=nil, 1=false, 2=true (nullable boolean)
    FInsets: array[0..3] of TfpgMigUnitValue;  // top, left, bottom, right
    FAlignX: TfpgMigUnitValue;
    FAlignY: TfpgMigUnitValue;
    FGridGapX: TfpgMigBoundSize;
    FGridGapY: TfpgMigBoundSize;
    FWidth: TfpgMigBoundSize;
    FHeight: TfpgMigBoundSize;
    FPackW: TfpgMigBoundSize;
    FPackH: TfpgMigBoundSize;
    FPwAlign: Single;  // Pack width alignment 0.0-1.0
    FPhAlign: Single;  // Pack height alignment 0.0-1.0
    FDebug: Boolean;
    FHideMode: Integer;
    FNoCache: Boolean;
    FFlowX: Boolean;
    FFillX: Boolean;
    FFillY: Boolean;
    FTopToBottom: Boolean;
    FNoGrid: Boolean;
    FVisualPadding: Boolean;

    { Setters - private; use the fluent API methods instead }
    procedure SetWrapAfter(ACount: Integer);
    procedure SetLeftToRight(AValue: Integer);
    procedure SetInsets(const AInsets: TfpgMigUnitValueArray);
    procedure SetAlignX(AValue: TfpgMigUnitValue);
    procedure SetAlignY(AValue: TfpgMigUnitValue);
    procedure SetGridGapX(AValue: TfpgMigBoundSize);
    procedure SetGridGapY(AValue: TfpgMigBoundSize);
    procedure SetWidth(AValue: TfpgMigBoundSize);
    procedure SetHeight(AValue: TfpgMigBoundSize);
    procedure SetPackWidth(AValue: TfpgMigBoundSize);
    procedure SetPackHeight(AValue: TfpgMigBoundSize);
    procedure SetPackWidthAlign(AAlign: Single);
    procedure SetPackHeightAlign(AAlign: Single);
    procedure SetDebug(AValue: Boolean);
    procedure SetHideMode(AMode: Integer);
    procedure SetNoCache(AValue: Boolean);
    procedure SetFlowX(AValue: Boolean);
    procedure SetFillX(AValue: Boolean);
    procedure SetFillY(AValue: Boolean);
    procedure SetTopToBottom(AValue: Boolean);
    procedure SetNoGrid(AValue: Boolean);
    procedure SetVisualPadding(AValue: Boolean);

  public
    constructor Create;
    destructor Destroy; override;

    { Getters - used by the layout engine to read constraint state }
    function GetWrapAfter: Integer;
    function GetLeftToRight: Integer;  // Returns 0=nil, 1=false, 2=true
    function GetInsets: TfpgMigUnitValueArray;  // Returns copy
    function GetAlignX: TfpgMigUnitValue;
    function GetAlignY: TfpgMigUnitValue;
    function GetGridGapX: TfpgMigBoundSize;
    function GetGridGapY: TfpgMigBoundSize;
    function GetWidth: TfpgMigBoundSize;
    function GetHeight: TfpgMigBoundSize;
    function GetPackWidth: TfpgMigBoundSize;
    function GetPackHeight: TfpgMigBoundSize;
    function GetPackWidthAlign: Single;
    function GetPackHeightAlign: Single;
    function GetDebug: Boolean;
    function GetHideMode: Integer;
    function IsNoCache: Boolean;
    function IsFlowX: Boolean;
    function IsFillX: Boolean;
    function IsFillY: Boolean;
    function IsTopToBottom: Boolean;
    function IsNoGrid: Boolean;
    function IsVisualPadding: Boolean;

    { Fluent API - Flow direction }
    function FlowX: TfpgMigLC;
    function FlowY: TfpgMigLC;

    { Fluent API - Wrapping }
    function Wrap: TfpgMigLC; overload;
    function WrapAfter(ACount: Integer): TfpgMigLC; overload;

    { Fluent API - Fill }
    function Fill: TfpgMigLC; overload;
    function FillX: TfpgMigLC; overload;
    function FillY: TfpgMigLC; overload;

    { Fluent API - Insets }
    function InsetsAll(const AAllSides: string): TfpgMigLC;
    function Insets(const AInsets: string): TfpgMigLC; overload;
    function Insets(const ATop, ALeft, ABottom, ARight: string): TfpgMigLC; overload;

    { Fluent API - Alignment }
    function AlignX(const AAlign: string): TfpgMigLC; overload;
    function AlignY(const AAlign: string): TfpgMigLC; overload;
    function Align(const AX, AY: string): TfpgMigLC; overload;

    { Fluent API - Grid gaps }
    function GridGapX(const ABoundSize: string): TfpgMigLC; overload;
    function GridGapY(const ABoundSize: string): TfpgMigLC; overload;
    function GridGap(const AGapX, AGapY: string): TfpgMigLC; overload;

    { Fluent API - Container size }
    function Width(const AWidth: string): TfpgMigLC; overload;
    function MinWidth(const AWidth: string): TfpgMigLC; overload;
    function MaxWidth(const AWidth: string): TfpgMigLC; overload;
    function Height(const AHeight: string): TfpgMigLC; overload;
    function MinHeight(const AHeight: string): TfpgMigLC; overload;
    function MaxHeight(const AHeight: string): TfpgMigLC; overload;

    { Fluent API - Packing }
    function Pack: TfpgMigLC; overload;
    function Pack(const AWidth, AHeight: string): TfpgMigLC; overload;
    function PackAlign(AAlignX, AAlignY: Single): TfpgMigLC; overload;

    { Fluent API - Direction }
    function LeftToRight(AValue: Boolean): TfpgMigLC; overload;
    function RightToLeft: TfpgMigLC;
    function TopToBottom: TfpgMigLC; overload;
    function BottomToTop: TfpgMigLC;

    { Fluent API - Behavior flags }
    function NoGrid: TfpgMigLC; overload;
    function NoCache: TfpgMigLC; overload;
    function NoVisualPadding: TfpgMigLC;

    { Fluent API - Debug and hide mode }
    function Debug: TfpgMigLC;
    function HideMode(AMode: Integer): TfpgMigLC; overload;
    function HideMode(AMode: TfpgMigHideMode): TfpgMigLC; overload;
  end;

implementation

uses
  fpg_mig_constraintparser, Math;

{ TfpgMigLC }

constructor TfpgMigLC.Create;
var
  i: Integer;
begin
  inherited Create;

  // Initialize to defaults
  FWrapAfter := INF_SIZE;
  FLeftToRight := 0;  // nil/undefined
  for i := 0 to 3 do
    FInsets[i] := nil;
  FAlignX := nil;
  FAlignY := nil;
  FGridGapX := nil;
  FGridGapY := nil;
  FWidth := BoundSizeNullSize;
  FHeight := BoundSizeNullSize;
  FPackW := BoundSizeNullSize;
  FPackH := BoundSizeNullSize;
  FPwAlign := 0.5;
  FPhAlign := 1.0;
  FDebug := False;
  FHideMode := 0;
  FNoCache := False;
  FFlowX := True;
  FFillX := False;
  FFillY := False;
  FTopToBottom := True;
  FNoGrid := False;
  FVisualPadding := True;
end;

destructor TfpgMigLC.Destroy;
begin
  // Free BoundSize objects if they're not constants
  if (FWidth <> nil) and (FWidth <> BoundSizeNullSize) then
    FWidth.Free;
  if (FHeight <> nil) and (FHeight <> BoundSizeNullSize) then
    FHeight.Free;
  if (FPackW <> nil) and (FPackW <> BoundSizeNullSize) then
    FPackW.Free;
  if (FPackH <> nil) and (FPackH <> BoundSizeNullSize) then
    FPackH.Free;
  if FGridGapX <> nil then
    FGridGapX.Free;
  if FGridGapY <> nil then
    FGridGapY.Free;

  // Note: FInsets and FAlign values are NOT freed here because they can be
  // singleton constants (UnitValueZero, UnitValueCenter, GetDialogInsets, etc.)
  // They are managed by the constant pool / platform defaults

  inherited Destroy;
end;

function TfpgMigLC.GetWrapAfter: Integer;
begin
  Result := FWrapAfter;
end;

procedure TfpgMigLC.SetWrapAfter(ACount: Integer);
begin
  FWrapAfter := ACount;
end;

function TfpgMigLC.GetLeftToRight: Integer;
begin
  Result := FLeftToRight;
end;

procedure TfpgMigLC.SetLeftToRight(AValue: Integer);
begin
  FLeftToRight := AValue;
end;

function TfpgMigLC.GetInsets: TfpgMigUnitValueArray;
var
  i: Integer;
begin
  SetLength(Result, 4);
  for i := 0 to 3 do
    Result[i] := FInsets[i];
end;

procedure TfpgMigLC.SetInsets(const AInsets: TfpgMigUnitValueArray);
var
  i: Integer;
begin
  if Length(AInsets) <> 4 then
    raise Exception.Create('Insets array must have exactly 4 elements');
  for i := 0 to 3 do
    FInsets[i] := AInsets[i];
end;

function TfpgMigLC.GetAlignX: TfpgMigUnitValue;
begin
  Result := FAlignX;
end;

procedure TfpgMigLC.SetAlignX(AValue: TfpgMigUnitValue);
begin
  FAlignX := AValue;
end;

function TfpgMigLC.GetAlignY: TfpgMigUnitValue;
begin
  Result := FAlignY;
end;

procedure TfpgMigLC.SetAlignY(AValue: TfpgMigUnitValue);
begin
  FAlignY := AValue;
end;

function TfpgMigLC.GetGridGapX: TfpgMigBoundSize;
begin
  Result := FGridGapX;
end;

procedure TfpgMigLC.SetGridGapX(AValue: TfpgMigBoundSize);
begin
  // Free old BoundSize if it exists
  if FGridGapX <> nil then
    FGridGapX.Free;
  FGridGapX := AValue;
end;

function TfpgMigLC.GetGridGapY: TfpgMigBoundSize;
begin
  Result := FGridGapY;
end;

procedure TfpgMigLC.SetGridGapY(AValue: TfpgMigBoundSize);
begin
  // Free old BoundSize if it exists
  if FGridGapY <> nil then
    FGridGapY.Free;
  FGridGapY := AValue;
end;

function TfpgMigLC.GetWidth: TfpgMigBoundSize;
begin
  Result := FWidth;
end;

procedure TfpgMigLC.SetWidth(AValue: TfpgMigBoundSize);
begin
  if AValue <> nil then
  begin
    // Free old BoundSize if it's not a constant
    if (FWidth <> nil) and (FWidth <> BoundSizeNullSize) then
      FWidth.Free;
    FWidth := AValue;
  end
  else
    FWidth := BoundSizeNullSize;
end;

function TfpgMigLC.GetHeight: TfpgMigBoundSize;
begin
  Result := FHeight;
end;

procedure TfpgMigLC.SetHeight(AValue: TfpgMigBoundSize);
begin
  if AValue <> nil then
  begin
    // Free old BoundSize if it's not a constant
    if (FHeight <> nil) and (FHeight <> BoundSizeNullSize) then
      FHeight.Free;
    FHeight := AValue;
  end
  else
    FHeight := BoundSizeNullSize;
end;

function TfpgMigLC.GetPackWidth: TfpgMigBoundSize;
begin
  Result := FPackW;
end;

procedure TfpgMigLC.SetPackWidth(AValue: TfpgMigBoundSize);
begin
  if AValue <> nil then
  begin
    // Free old BoundSize if it's not a constant
    if (FPackW <> nil) and (FPackW <> BoundSizeNullSize) then
      FPackW.Free;
    FPackW := AValue;
  end
  else
    FPackW := BoundSizeNullSize;
end;

function TfpgMigLC.GetPackHeight: TfpgMigBoundSize;
begin
  Result := FPackH;
end;

procedure TfpgMigLC.SetPackHeight(AValue: TfpgMigBoundSize);
begin
  if AValue <> nil then
  begin
    // Free old BoundSize if it's not a constant
    if (FPackH <> nil) and (FPackH <> BoundSizeNullSize) then
      FPackH.Free;
    FPackH := AValue;
  end
  else
    FPackH := BoundSizeNullSize;
end;

function TfpgMigLC.GetPackWidthAlign: Single;
begin
  Result := FPwAlign;
end;

procedure TfpgMigLC.SetPackWidthAlign(AAlign: Single);
begin
  FPwAlign := Max(0.0, Min(1.0, AAlign));
end;

function TfpgMigLC.GetPackHeightAlign: Single;
begin
  Result := FPhAlign;
end;

procedure TfpgMigLC.SetPackHeightAlign(AAlign: Single);
begin
  FPhAlign := Max(0.0, Min(1.0, AAlign));
end;

function TfpgMigLC.GetDebug: Boolean;
begin
  Result := FDebug;
end;

procedure TfpgMigLC.SetDebug(AValue: Boolean);
begin
  FDebug := AValue;
end;

function TfpgMigLC.GetHideMode: Integer;
begin
  Result := FHideMode;
end;

procedure TfpgMigLC.SetHideMode(AMode: Integer);
begin
  if (AMode < 0) or (AMode > 3) then
    raise Exception.CreateFmt('Invalid hide mode: %d', [AMode]);
  FHideMode := AMode;
end;

function TfpgMigLC.IsNoCache: Boolean;
begin
  Result := FNoCache;
end;

procedure TfpgMigLC.SetNoCache(AValue: Boolean);
begin
  FNoCache := AValue;
end;

function TfpgMigLC.IsFlowX: Boolean;
begin
  Result := FFlowX;
end;

procedure TfpgMigLC.SetFlowX(AValue: Boolean);
begin
  FFlowX := AValue;
end;

function TfpgMigLC.IsFillX: Boolean;
begin
  Result := FFillX;
end;

procedure TfpgMigLC.SetFillX(AValue: Boolean);
begin
  FFillX := AValue;
end;

function TfpgMigLC.IsFillY: Boolean;
begin
  Result := FFillY;
end;

procedure TfpgMigLC.SetFillY(AValue: Boolean);
begin
  FFillY := AValue;
end;

function TfpgMigLC.IsTopToBottom: Boolean;
begin
  Result := FTopToBottom;
end;

procedure TfpgMigLC.SetTopToBottom(AValue: Boolean);
begin
  FTopToBottom := AValue;
end;

function TfpgMigLC.IsNoGrid: Boolean;
begin
  Result := FNoGrid;
end;

procedure TfpgMigLC.SetNoGrid(AValue: Boolean);
begin
  FNoGrid := AValue;
end;

function TfpgMigLC.IsVisualPadding: Boolean;
begin
  Result := FVisualPadding;
end;

procedure TfpgMigLC.SetVisualPadding(AValue: Boolean);
begin
  FVisualPadding := AValue;
end;

{ Fluent API implementations }

function TfpgMigLC.FlowX: TfpgMigLC;
begin
  SetFlowX(True);
  Result := Self;
end;

function TfpgMigLC.FlowY: TfpgMigLC;
begin
  SetFlowX(False);
  Result := Self;
end;

function TfpgMigLC.Wrap: TfpgMigLC;
begin
  SetWrapAfter(0);
  Result := Self;
end;

function TfpgMigLC.WrapAfter(ACount: Integer): TfpgMigLC;
begin
  SetWrapAfter(ACount);
  Result := Self;
end;

function TfpgMigLC.Fill: TfpgMigLC;
begin
  SetFillX(True);
  SetFillY(True);
  Result := Self;
end;

function TfpgMigLC.FillX: TfpgMigLC;
begin
  SetFillX(True);
  Result := Self;
end;

function TfpgMigLC.FillY: TfpgMigLC;
begin
  SetFillY(True);
  Result := Self;
end;

function TfpgMigLC.InsetsAll(const AAllSides: string): TfpgMigLC;
var
  insH, insV: TfpgMigUnitValue;
begin
  insH := ParseUnitValue(AAllSides, True);
  insV := ParseUnitValue(AAllSides, False);
  FInsets[0] := insV;  // top
  FInsets[1] := insH;  // left
  FInsets[2] := insV;  // bottom (shared reference)
  FInsets[3] := insH;  // right (shared reference)
  Result := Self;
end;

function TfpgMigLC.Insets(const AInsets: string): TfpgMigLC;
var
  parsed: array of TfpgMigUnitValue;
  i: Integer;
begin
  parsed := ParseInsets(AInsets, True);
  if Length(parsed) = 4 then
    for i := 0 to 3 do
      FInsets[i] := parsed[i];
  Result := Self;
end;

function TfpgMigLC.Insets(const ATop, ALeft, ABottom, ARight: string): TfpgMigLC;
begin
  FInsets[0] := ParseUnitValue(ATop, False);
  FInsets[1] := ParseUnitValue(ALeft, True);
  FInsets[2] := ParseUnitValue(ABottom, False);
  FInsets[3] := ParseUnitValue(ARight, True);
  Result := Self;
end;

function TfpgMigLC.AlignX(const AAlign: string): TfpgMigLC;
begin
  SetAlignX(ParseUnitValueOrAlign(AAlign, True));
  Result := Self;
end;

function TfpgMigLC.AlignY(const AAlign: string): TfpgMigLC;
begin
  SetAlignY(ParseUnitValueOrAlign(AAlign, False));
  Result := Self;
end;

function TfpgMigLC.Align(const AX, AY: string): TfpgMigLC;
begin
  if AX <> '' then
    AlignX(AX);
  if AY <> '' then
    AlignY(AY);
  Result := Self;
end;

function TfpgMigLC.GridGapX(const ABoundSize: string): TfpgMigLC;
begin
  SetGridGapX(ParseBoundSize(ABoundSize, True, True));
  Result := Self;
end;

function TfpgMigLC.GridGapY(const ABoundSize: string): TfpgMigLC;
begin
  SetGridGapY(ParseBoundSize(ABoundSize, True, False));
  Result := Self;
end;

function TfpgMigLC.GridGap(const AGapX, AGapY: string): TfpgMigLC;
begin
  if AGapX <> '' then
    GridGapX(AGapX);
  if AGapY <> '' then
    GridGapY(AGapY);
  Result := Self;
end;

function TfpgMigLC.Width(const AWidth: string): TfpgMigLC;
begin
  SetWidth(ParseBoundSize(AWidth, False, True));
  Result := Self;
end;

function TfpgMigLC.MinWidth(const AWidth: string): TfpgMigLC;
var
  minVal: TfpgMigUnitValue;
  newSize: TfpgMigBoundSize;
begin
  minVal := ParseUnitValue(AWidth, True);
  try
    newSize := TfpgMigBoundSize.Create(minVal, FWidth.Preferred, FWidth.Max, FWidth.GapPush);
    SetWidth(newSize);
  finally
    minVal.Free;
  end;
  Result := Self;
end;

function TfpgMigLC.MaxWidth(const AWidth: string): TfpgMigLC;
var
  maxVal: TfpgMigUnitValue;
  newSize: TfpgMigBoundSize;
begin
  maxVal := ParseUnitValue(AWidth, True);
  try
    newSize := TfpgMigBoundSize.Create(FWidth.Min, FWidth.Preferred, maxVal, FWidth.GapPush);
    SetWidth(newSize);
  finally
    maxVal.Free;
  end;
  Result := Self;
end;

function TfpgMigLC.Height(const AHeight: string): TfpgMigLC;
begin
  SetHeight(ParseBoundSize(AHeight, False, False));
  Result := Self;
end;

function TfpgMigLC.MinHeight(const AHeight: string): TfpgMigLC;
var
  minVal: TfpgMigUnitValue;
  newSize: TfpgMigBoundSize;
begin
  minVal := ParseUnitValue(AHeight, False);
  try
    newSize := TfpgMigBoundSize.Create(minVal, FHeight.Preferred, FHeight.Max, FHeight.GapPush);
    SetHeight(newSize);
  finally
    minVal.Free;
  end;
  Result := Self;
end;

function TfpgMigLC.MaxHeight(const AHeight: string): TfpgMigLC;
var
  maxVal: TfpgMigUnitValue;
  newSize: TfpgMigBoundSize;
begin
  maxVal := ParseUnitValue(AHeight, False);
  try
    newSize := TfpgMigBoundSize.Create(FHeight.Min, FHeight.Preferred, maxVal, FHeight.GapPush);
    SetHeight(newSize);
  finally
    maxVal.Free;
  end;
  Result := Self;
end;

function TfpgMigLC.Pack: TfpgMigLC;
begin
  Result := Pack('pref', 'pref');
end;

function TfpgMigLC.Pack(const AWidth, AHeight: string): TfpgMigLC;
begin
  if AWidth <> '' then
    SetPackWidth(ParseBoundSize(AWidth, False, True))
  else
    SetPackWidth(nil);

  if AHeight <> '' then
    SetPackHeight(ParseBoundSize(AHeight, False, False))
  else
    SetPackHeight(nil);

  Result := Self;
end;

function TfpgMigLC.PackAlign(AAlignX, AAlignY: Single): TfpgMigLC;
begin
  SetPackWidthAlign(AAlignX);
  SetPackHeightAlign(AAlignY);
  Result := Self;
end;

function TfpgMigLC.LeftToRight(AValue: Boolean): TfpgMigLC;
begin
  if AValue then
    SetLeftToRight(2)  // true
  else
    SetLeftToRight(1);  // false
  Result := Self;
end;

function TfpgMigLC.RightToLeft: TfpgMigLC;
begin
  SetLeftToRight(1);  // false
  Result := Self;
end;

function TfpgMigLC.TopToBottom: TfpgMigLC;
begin
  SetTopToBottom(True);
  Result := Self;
end;

function TfpgMigLC.BottomToTop: TfpgMigLC;
begin
  SetTopToBottom(False);
  Result := Self;
end;

function TfpgMigLC.NoGrid: TfpgMigLC;
begin
  SetNoGrid(True);
  Result := Self;
end;

function TfpgMigLC.NoCache: TfpgMigLC;
begin
  SetNoCache(True);
  Result := Self;
end;

function TfpgMigLC.NoVisualPadding: TfpgMigLC;
begin
  SetVisualPadding(False);
  Result := Self;
end;

function TfpgMigLC.Debug: TfpgMigLC;
begin
  SetDebug(True);
  Result := Self;
end;

function TfpgMigLC.HideMode(AMode: Integer): TfpgMigLC;
begin
  SetHideMode(AMode);
  Result := Self;
end;

function TfpgMigLC.HideMode(AMode: TfpgMigHideMode): TfpgMigLC;
begin
  case AMode of
    hmNormal: SetHideMode(0);
    hmSize0RetainGaps: SetHideMode(1);
    hmSize0Gaps0: SetHideMode(2);
    hmDisregard: SetHideMode(3);
  end;
  Result := Self;
end;

end.
