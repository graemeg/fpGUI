{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2025 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Basic proof of concept layout manager.
}
unit fpg_flowlayout;

{$I fpg_defines.inc}

interface

uses
  Classes, SysUtils,
  Generics.Collections,
  fpg_base,
  fpg_layoutmanager, fpg_layouttypes;

type
  TLayoutPair = specialize TPair<TfpgWidgetBase, TfpgLayoutConstraint>;

  TfpgFlowLayoutManager = class(TfpgBaseLayoutManager)
  protected
    procedure DoLayout(AContainer: TfpgWidgetBase); override;
    function DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize; override;
    function CreateDefaultConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint; override;
  end;

implementation

uses
  fpg_widget;

{ TfpgFlowLayoutManager }

procedure TfpgFlowLayoutManager.DoLayout(AContainer: TfpgWidgetBase);
var
  w: TfpgWidgetBase;
  x, y: TfpgCoord;
  Pair: TLayoutPair;
begin
  writeln('TfpgFlowLayoutManager.DoLayout');
  if not (AContainer is TfpgWidget) then Exit;

  x := 0;
  y := 0;

  for Pair in FConstraints do
  begin
    w := Pair.Key;
    if (w is TfpgWidget) and TfpgWidget(w).Visible then
    begin
      writeln('  - Placing widget ', w.Name, ' at ', x, ',', y);
      TfpgWidget(w).SetPosition(x, y, w.Width, w.Height);
      x := x + w.Width;
    end;
  end;
end;

function TfpgFlowLayoutManager.CreateDefaultConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint;
begin
  Result := TfpgLayoutConstraint.Create;
end;

function TfpgFlowLayoutManager.DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize;
var
  w: TfpgWidgetBase;
  totalWidth: TfpgCoord;
  maxHeight: TfpgCoord;
  Pair: TLayoutPair;
begin
  if not (AContainer is TfpgWidget) then
  begin
    Result.SetSize(0, 0);
    Exit;
  end;

  totalWidth := 0;
  maxHeight := 0;

  for Pair in FConstraints do
  begin
    w := Pair.Key;
    if (w is TfpgWidget) and TfpgWidget(w).Visible then
    begin
      totalWidth := totalWidth + w.Width;
      if w.Height > maxHeight then
        maxHeight := w.Height;
    end;
  end;

  Result.SetSize(totalWidth, maxHeight);
end;

end.
