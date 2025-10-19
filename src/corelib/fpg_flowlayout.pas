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
  fpg_base,
  fpg_layoutmanager, fpg_layouttypes;

type

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
  Iterator: ILayoutIterator;
  w: TfpgWidget;
  x, y: TfpgCoord;
  rowMaxHeight: TfpgCoord;
  ContainerWidth: TfpgCoord;
begin
  if not (AContainer is TfpgWidget) then Exit;

  writeln('TfpgFlowLayoutManager.DoLayout');

  Iterator := GetIterator(AContainer);
  if not Assigned(Iterator) then Exit;

  x := 0;
  y := 0;
  rowMaxHeight := 0;
  ContainerWidth := (AContainer as TfpgWidget).Width;

  while Iterator.HasNext do
  begin
    w := Iterator.Next as TfpgWidget;

    // Wrap to next row if needed
    if (x > 0) and (x + w.Width > ContainerWidth) then
    begin
      y := y + rowMaxHeight;
      x := 0;
      rowMaxHeight := 0;
    end;

    writeln('  - Placing widget ', w.Name, ' at ', x, ',', y);
    w.SetPosition(x, y, w.Width, w.Height);

    x := x + w.Width;
    if w.Height > rowMaxHeight then
      rowMaxHeight := w.Height;
  end;
end;

function TfpgFlowLayoutManager.CreateDefaultConstraint(AWidget: TfpgWidgetBase): TfpgLayoutConstraint;
begin
  Result := TfpgLayoutConstraint.Create;
end;

function TfpgFlowLayoutManager.DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize;
var
  Iterator: ILayoutIterator;
  w: TfpgWidget;
  totalWidth: TfpgCoord;
  maxHeight: TfpgCoord;
begin
  if not (AContainer is TfpgWidget) then
  begin
    Result.SetSize(0, 0);
    Exit;
  end;

  Iterator := GetIterator(AContainer);
  totalWidth := 0;
  maxHeight := 0;
  while Iterator.HasNext do
  begin
    w := Iterator.Next as TfpgWidget;
    totalWidth := totalWidth + w.Width;
    if w.Height > maxHeight then
      maxHeight := w.Height;
  end;

  Result.SetSize(totalWidth, maxHeight);
end;

end.
