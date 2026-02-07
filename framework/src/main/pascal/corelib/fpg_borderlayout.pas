{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2025 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      A layout manager that arranges components in all four cardinal
      directions and in the center.
}
unit fpg_borderlayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  fpg_base, fpg_main, fpg_widget, fpg_layouttypes, fpg_layoutmanager;

type

  { TfpgBorderLayoutRegion }

  TfpgBorderLayoutRegion = (
    blrNorth,
    blrSouth,
    blrWest,
    blrEast,
    blrCenter
    );

  { TfpgBorderLayoutConstraint }

  TfpgBorderLayoutConstraint = class(TfpgLayoutConstraint)
  private
    FRegion: TfpgBorderLayoutRegion;
  public
    constructor Create; virtual;
  published
    property Region: TfpgBorderLayoutRegion read FRegion write FRegion default blrCenter;
  end;

  { TfpgBorderLayoutManager }

  TfpgBorderLayoutManager = class(TfpgBaseLayoutManager)
  private
    FHGap: integer;
    FVGap: integer;
    procedure SetHGap(AValue: integer);
    procedure SetVGap(AValue: integer);
  protected
    // TfpgBaseLayoutManager overrides
    function CreateDefaultConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint; override;
    procedure DoLayout(AContainer: TfpgWidgetBase); override;
    function DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize; override;
    function DoGetMinimumSize(AContainer: TfpgWidgetBase): TfpgSize; override;
  public
    constructor Create; override; overload;
    constructor Create(const hgap: integer; const vgap: integer); overload;
  published
    property HGap: integer read FHGap write SetHGap default 0;
    property VGap: integer read FVGap write SetVGap default 0;
  end;


implementation

{ TfpgBorderLayoutConstraint }

constructor TfpgBorderLayoutConstraint.Create;
begin
  inherited Create;
  FRegion := blrCenter;
end;

{ TfpgBorderLayoutManager }

constructor TfpgBorderLayoutManager.Create;
begin
  inherited Create;
  FHGap := 0;
  FVGap := 0;
end;

constructor TfpgBorderLayoutManager.Create(const hgap: integer; const vgap: integer);
begin
  Create;
  FHGap := hgap;
  FVGap := vgap;
end;

procedure TfpgBorderLayoutManager.SetHGap(AValue: integer);
begin
  if FHGap <> AValue then
  begin
    FHGap := AValue;
  end;
end;

procedure TfpgBorderLayoutManager.SetVGap(AValue: integer);
begin
  if FVGap <> AValue then
  begin
    FVGap := AValue;
  end;
end;

function TfpgBorderLayoutManager.CreateDefaultConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint;
begin
  Result := TfpgBorderLayoutConstraint.Create;
end;

procedure TfpgBorderLayoutManager.DoLayout(AContainer: TfpgWidgetBase);
var
  w: TfpgWidget;
  constraint: TfpgBorderLayoutConstraint;
  north, south, east, west, center: TfpgWidget;
  bounds: TfpgRect;
  north_h, south_h, east_w, west_w: TfpgCoord;
  tmpSize: TfpgSize;
  top, left, right, bottom: TfpgCoord;
  iterator: ILayoutIterator;
begin
  north := nil;
  south := nil;
  east := nil;
  west := nil;
  center := nil;

  iterator := GetIterator(AContainer);
  if iterator = nil then Exit;

  while iterator.HasNext do
  begin
    w := iterator.Next as TfpgWidget;
    constraint := GetConstraintOrDefault(w) as TfpgBorderLayoutConstraint;
    case constraint.Region of
      blrNorth: north := w;
      blrSouth: south := w;
      blrWest:  west  := w;
      blrEast:  east  := w;
      blrCenter: center := w;
    end;
  end;

  bounds := (AContainer as TfpgWidget).GetClientRect;

  if Assigned(north) then begin tmpSize := north.PreferredSize; north_h := tmpSize.H; end else north_h := 0;
  if Assigned(south) then begin tmpSize := south.PreferredSize; south_h := tmpSize.H; end else south_h := 0;
  if Assigned(east) then begin tmpSize := east.PreferredSize; east_w := tmpSize.W; end else east_w := 0;
  if Assigned(west) then begin tmpSize := west.PreferredSize; west_w := tmpSize.W; end else west_w := 0;

  top := bounds.Top + FVGap;
  left := bounds.Left + FHGap;
  right := bounds.Left + bounds.Width - FHGap;
  bottom := bounds.Top + bounds.Height - FVGap;

  if Assigned(north) then
  begin
    (north as ILayoutTarget).MoveAndResize(left, top, right - left, north_h);
    Inc(top, north_h + FVGap);
  end;
  if Assigned(south) then
  begin
    (south as ILayoutTarget).MoveAndResize(left, bottom - south_h, right - left, south_h);
    Dec(bottom, south_h + FVGap);
  end;
  if Assigned(east) then
  begin
    (east as ILayoutTarget).MoveAndResize(right - east_w, top, east_w, bottom - top);
    Dec(right, east_w + FHGap);
  end;
  if Assigned(west) then
  begin
    (west as ILayoutTarget).MoveAndResize(left, top, west_w, bottom - top);
    Inc(left, west_w + FHGap);
  end;
  if Assigned(center) then
  begin
    (center as ILayoutTarget).MoveAndResize(left, top, right - left, bottom - top);
  end;
end;

function TfpgBorderLayoutManager.DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize;
var
  w: TfpgWidget;
  constraint: TfpgBorderLayoutConstraint;
  north, south, east, west, center: TfpgWidget;
  north_ps, south_ps, east_ps, west_ps, center_ps: TfpgSize;
  center_h: TfpgCoord;
  iterator: ILayoutIterator;
begin
  north := nil;
  south := nil;
  east := nil;
  west := nil;
  center := nil;
  north_ps.SetSize(0,0);
  south_ps.SetSize(0,0);
  east_ps.SetSize(0,0);
  west_ps.SetSize(0,0);
  center_ps.SetSize(0,0);

  iterator := GetIterator(AContainer);
  if iterator <> nil then
  begin
    while iterator.HasNext do
    begin
      w := iterator.Next as TfpgWidget;
      constraint := GetConstraintOrDefault(w) as TfpgBorderLayoutConstraint;
      case constraint.Region of
        blrNorth: begin north := w; north_ps := w.PreferredSize; end;
        blrSouth: begin south := w; south_ps := w.PreferredSize; end;
        blrWest:  begin west := w; west_ps := w.PreferredSize; end;
        blrEast:  begin east := w; east_ps := w.PreferredSize; end;
        blrCenter: begin center := w; center_ps := w.PreferredSize; end;
      end;
    end;
  end;

  // Height calculation
  center_h := max(west_ps.H, max(center_ps.H, east_ps.H));
  Result.H := north_ps.H + south_ps.H + center_h;
  if Assigned(north) and (north_ps.H > 0) then Result.H := Result.H + FVGap;
  if Assigned(south) and (south_ps.H > 0) then Result.H := Result.H + FVGap;

  // Width calculation
  Result.W := west_ps.W + east_ps.W + center_ps.W;
  if Assigned(west) and (west_ps.W > 0) then Result.W := Result.W + FHGap;
  if Assigned(east) and (east_ps.W > 0) then Result.W := Result.W + FHGap;

  Result.W := max(Result.W, north_ps.W);
  Result.W := max(Result.W, south_ps.W);
end;

function TfpgBorderLayoutManager.DoGetMinimumSize(AContainer: TfpgWidgetBase): TfpgSize;
var
  w: TfpgWidget;
  constraint: TfpgBorderLayoutConstraint;
  north, south, east, west, center: TfpgWidget;
  north_ms, south_ms, east_ms, west_ms, center_ms: TfpgSize;
  center_h: TfpgCoord;
  iterator: ILayoutIterator;
begin
  north := nil;
  south := nil;
  east := nil;
  west := nil;
  center := nil;
  north_ms.SetSize(0,0);
  south_ms.SetSize(0,0);
  east_ms.SetSize(0,0);
  west_ms.SetSize(0,0);
  center_ms.SetSize(0,0);

  iterator := GetIterator(AContainer);
  if iterator <> nil then
  begin
    while iterator.HasNext do
    begin
      w := iterator.Next as TfpgWidget;
      constraint := GetConstraintOrDefault(w) as TfpgBorderLayoutConstraint;
      case constraint.Region of
        blrNorth: begin north := w; north_ms.SetSize(w.MinWidth, w.MinHeight); end;
        blrSouth: begin south := w; south_ms.SetSize(w.MinWidth, w.MinHeight); end;
        blrWest:  begin west := w; west_ms.SetSize(w.MinWidth, w.MinHeight); end;
        blrEast:  begin east := w; east_ms.SetSize(w.MinWidth, w.MinHeight); end;
        blrCenter: begin center := w; center_ms.SetSize(w.MinWidth, w.MinHeight); end;
      end;
    end;
  end;

  // Height calculation
  center_h := max(west_ms.H, max(center_ms.H, east_ms.H));
  Result.H := north_ms.H + south_ms.H + center_h;
  if Assigned(north) and (north_ms.H > 0) then Result.H := Result.H + FVGap;
  if Assigned(south) and (south_ms.H > 0) then Result.H := Result.H + FVGap;

  // Width calculation
  Result.W := west_ms.W + east_ms.W + center_ms.W;
  if Assigned(west) and (west_ms.W > 0) then Result.W := Result.W + FHGap;
  if Assigned(east) and (east_ms.W > 0) then Result.W := Result.W + FHGap;

  Result.W := max(Result.W, north_ms.W);
  Result.W := max(Result.W, south_ms.W);
end;

end.
