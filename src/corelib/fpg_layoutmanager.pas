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
    FConstraints: TLayoutConstraints;
    FLayoutDirty: Boolean;
    FCachedPreferredSize: TfpgSize;
    FCachedMinimumSize: TfpgSize;
  protected
    // Override these in subclasses
    procedure DoLayout(AContainer: TfpgWidgetBase); virtual; abstract;
    function DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize; virtual; abstract;
    function DoGetMinimumSize(AContainer: TfpgWidgetBase): TfpgSize; virtual;

    // Helper methods for subclasses
    function GetConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint;
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
  end;

implementation

uses
  fpg_widget;

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
begin
  if FLayoutDirty then
  begin
    DoLayout(AContainer);
    FLayoutDirty := False;
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
begin
  FLayoutDirty := True;
  FCachedPreferredSize.SetSize(0, 0);
  FCachedMinimumSize.SetSize(0, 0);
end;

function TfpgBaseLayoutManager.GetConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint;
begin
  Result := FConstraints.Items[AWidget];
end;

function TfpgBaseLayoutManager.DoGetMinimumSize(AContainer: TfpgWidgetBase): TfpgSize;
begin
  Result := DoGetPreferredSize(AContainer);
end;

end.
