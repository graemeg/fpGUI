{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2025 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Helper class that implements ILayoutManager
}
unit fpg_layoutmanager;

{$I fpg_defines.inc}

interface

uses
  Classes, SysUtils, Generics.Collections,
  fpg_base,
  fpg_layouttypes;

type
  TLayoutConstraints = specialize TObjectDictionary<TfpgWidgetBase, TfpgLayoutConstraint>;

  TfpgBaseLayoutManager = class(TInterfacedObject, ILayoutManager)
  private
    FLayoutDirty: Boolean;
    FCachedPreferredSize: TfpgSize;
    FCachedMinimumSize: TfpgSize;
  protected
    FConstraints: TLayoutConstraints;
  protected
    // Override these in subclasses
    procedure DoLayout(AContainer: TfpgWidgetBase); virtual; abstract;
    function DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize; virtual; abstract;
    function DoGetMinimumSize(AContainer: TfpgWidgetBase): TfpgSize; virtual;

    // Helper methods for subclasses
    function GetConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint;
    function GetConstraintOrDefault(AWidget: TfpgWidgetBase): TfpgLayoutConstraint;
    function CreateDefaultConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // ILayoutManager implementation
    procedure LayoutContainer(AContainer: TfpgWidgetBase); virtual;
    function GetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize; virtual;
    function GetMinimumSize(AContainer: TfpgWidgetBase): TfpgSize; virtual;
    procedure AddLayoutComponent(AWidget: TfpgWidgetBase; AConstraint: TfpgLayoutConstraint); virtual;
    procedure RemoveLayoutComponent(AWidget: TfpgWidgetBase); virtual;
    procedure InvalidateLayout(AContainer: TfpgWidgetBase); virtual;
    procedure PaintDebug(AWidget: TfpgWidgetBase; ACanvas: TfpgCanvasBase); virtual;

    function GetIterator(AContainer: TfpgWidgetBase): ILayoutIterator; virtual;
  end;

implementation

uses
  fpg_widget;

type
  TLayoutIterator = class(TInterfacedObject, ILayoutIterator)
  private
    FContainer: TfpgWidget;
    FConstraints: TLayoutConstraints;
    FCurrentIndex: Integer;
    FNextWidget: TfpgWidgetBase;
    procedure FindNext;
  public
    constructor Create(AContainer: TfpgWidget; AConstraints: TLayoutConstraints);
    function HasNext: Boolean;
    function Next: TfpgWidgetBase;
  end;

{ TLayoutIterator }

constructor TLayoutIterator.Create(AContainer: TfpgWidget; AConstraints: TLayoutConstraints);
begin
  inherited Create;
  FContainer := AContainer;
  FConstraints := AConstraints;
  FCurrentIndex := 0;
  FindNext; // Find the first valid widget
end;

procedure TLayoutIterator.FindNext;
var
  w: TfpgWidget;
begin
  FNextWidget := nil;
  while (FCurrentIndex < FContainer.ComponentCount) and (FNextWidget = nil) do
  begin
    if FContainer.Components[FCurrentIndex] is TfpgWidget then
    begin
      w := FContainer.Components[FCurrentIndex] as TfpgWidget;
      // Filter by managed AND visible
      if FConstraints.ContainsKey(w) and w.Visible then
        FNextWidget := w;
    end;
    Inc(FCurrentIndex);
  end;
end;

function TLayoutIterator.HasNext: Boolean;
begin
  Result := Assigned(FNextWidget);
end;

function TLayoutIterator.Next: TfpgWidgetBase;
begin
  Result := FNextWidget;
  FindNext; // Prepare for the next call
end;

{ TfpgBaseLayoutManager }

constructor TfpgBaseLayoutManager.Create;
begin
  inherited Create;
  FConstraints := TLayoutConstraints.Create([doOwnsValues]);
  FLayoutDirty := True;
end;

destructor TfpgBaseLayoutManager.Destroy;
begin
  FConstraints.Free;
  inherited Destroy;
end;

procedure TfpgBaseLayoutManager.LayoutContainer(AContainer: TfpgWidgetBase);
var
  minSize: TfpgSize;
begin
  if FLayoutDirty then
  begin
    DoLayout(AContainer);
    FLayoutDirty := False;

    // Update container MinWidth/MinHeight so the window manager can enforce them.
    // This must happen after DoLayout because the grid needs to be built first
    // before minimum sizes can be calculated accurately.
    // Only increase MinWidth/MinHeight - never reduce below the developer's explicit constraints.
    minSize := GetMinimumSize(AContainer);
    if (minSize.W > 0) and (minSize.H > 0) then
    begin
      if (minSize.W > AContainer.MinWidth) or (minSize.H > AContainer.MinHeight) then
      begin
        if minSize.W > AContainer.MinWidth then
          AContainer.MinWidth := minSize.W;
        if minSize.H > AContainer.MinHeight then
          AContainer.MinHeight := minSize.H;
        AContainer.UpdatePosition;
      end;
    end;
  end;
end;

function TfpgBaseLayoutManager.GetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize;
begin
  if FLayoutDirty or (FCachedPreferredSize.W = 0) and (FCachedPreferredSize.H = 0) then
  begin
    FCachedPreferredSize := DoGetPreferredSize(AContainer);
  end;
  Result := FCachedPreferredSize;
end;

function TfpgBaseLayoutManager.GetMinimumSize(AContainer: TfpgWidgetBase): TfpgSize;
begin
  if FLayoutDirty or (FCachedMinimumSize.W = 0) and (FCachedMinimumSize.H = 0) then
  begin
    FCachedMinimumSize := DoGetMinimumSize(AContainer);
  end;
  Result := FCachedMinimumSize;
end;

procedure TfpgBaseLayoutManager.AddLayoutComponent(AWidget: TfpgWidgetBase; AConstraint: TfpgLayoutConstraint);
begin
  FConstraints.Add(AWidget, AConstraint);
  if AWidget is TfpgWidget then
    InvalidateLayout(TfpgWidget(AWidget).Parent);
end;

procedure TfpgBaseLayoutManager.RemoveLayoutComponent(AWidget: TfpgWidgetBase);
begin
  FConstraints.Remove(AWidget);
  if AWidget is TfpgWidget then
    InvalidateLayout(TfpgWidget(AWidget).Parent);
end;

procedure TfpgBaseLayoutManager.InvalidateLayout(AContainer: TfpgWidgetBase);
var
  minSize: TfpgSize;
begin
  FLayoutDirty := True;
  FCachedPreferredSize.SetSize(0, 0);
  FCachedMinimumSize.SetSize(0, 0);

  // Update container's MinWidth/MinHeight when layout changes
  // This ensures the minimum size constraints are updated as components are added/removed
  // Only do this if container has reasonable dimensions to avoid issues during extreme resizing.
  // Only increase MinWidth/MinHeight - never reduce below the developer's explicit constraints.
  if Assigned(AContainer) and (AContainer.Width > 10) and (AContainer.Height > 10) then
  begin
    try
      minSize := GetMinimumSize(AContainer);
      if (minSize.W > 0) and (minSize.H > 0) then
      begin
        if minSize.W > AContainer.MinWidth then
          AContainer.MinWidth := minSize.W;
        if minSize.H > AContainer.MinHeight then
          AContainer.MinHeight := minSize.H;
      end;
    except
      // Silently ignore errors during minimum size calculation
      // This can happen during extreme resize operations
    end;
  end;
end;

function TfpgBaseLayoutManager.GetConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint;
begin
  if not FConstraints.TryGetValue(AWidget, Result) then
    Result := nil;
end;

function TfpgBaseLayoutManager.GetConstraintOrDefault(AWidget: TfpgWidgetBase): TfpgLayoutConstraint;
begin
  Result := GetConstraint(AWidget);
  if not Assigned(Result) then
  begin
    Result := CreateDefaultConstraint(AWidget);
    FConstraints.Add(AWidget, Result);
  end;
end;

function TfpgBaseLayoutManager.CreateDefaultConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint;
begin
  Result := TfpgLayoutConstraint.Create;
end;

function TfpgBaseLayoutManager.DoGetMinimumSize(AContainer: TfpgWidgetBase): TfpgSize;
begin
  Result := DoGetPreferredSize(AContainer);
end;

function TfpgBaseLayoutManager.GetIterator(AContainer: TfpgWidgetBase): ILayoutIterator;
begin
  if AContainer is TfpgWidget then
    Result := TLayoutIterator.Create(AContainer as TfpgWidget, FConstraints)
  else
    Result := nil;
end;

procedure TfpgBaseLayoutManager.PaintDebug(AWidget: TfpgWidgetBase; ACanvas: TfpgCanvasBase);
begin
  // Do nothing in base class
end;

end.
