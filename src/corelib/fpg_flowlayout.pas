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
  fpg_layoutmanager;

type
  TfpgFlowLayoutManager = class(TfpgBaseLayoutManager)
  protected
    procedure DoLayout(AContainer: TfpgWidgetBase); override;
    function DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize; override;
  end;

implementation

uses
  fpg_widget;

{ TfpgFlowLayoutManager }

procedure TfpgFlowLayoutManager.DoLayout(AContainer: TfpgWidgetBase);
var
  i: Integer;
  w: TfpgWidget;
  x, y: TfpgCoord;
begin
  writeln('TfpgFlowLayoutManager.DoLayout');
  if not (AContainer is TfpgWidget) then Exit;

  x := 0;
  y := 0;

  for i := 0 to TfpgWidget(AContainer).ComponentCount - 1 do
  begin
    if TfpgWidget(AContainer).Components[i] is TfpgWidget then
    begin
      w := TfpgWidget(TfpgWidget(AContainer).Components[i]);
      if w.Visible then
      begin
        writeln('  - Placing widget ', w.Name, ' at ', x, ',', y);
        w.SetPosition(x, y, w.Width, w.Height);
        x := x + w.Width;
      end;
    end;
  end;
end;

function TfpgFlowLayoutManager.DoGetPreferredSize(AContainer: TfpgWidgetBase): TfpgSize;
var
  i: Integer;
  w: TfpgWidget;
  totalWidth: TfpgCoord;
  maxHeight: TfpgCoord;
begin
  if not (AContainer is TfpgWidget) then
  begin
    Result.SetSize(0, 0);
    Exit;
  end;

  totalWidth := 0;
  maxHeight := 0;

  for i := 0 to TfpgWidget(AContainer).ComponentCount - 1 do
  begin
    if TfpgWidget(AContainer).Components[i] is TfpgWidget then
    begin
      w := TfpgWidget(TfpgWidget(AContainer).Components[i]);
      if w.Visible then
      begin
        totalWidth := totalWidth + w.Width;
        if w.Height > maxHeight then
          maxHeight := w.Height;
      end;
    end;
  end;

  Result.SetSize(totalWidth, maxHeight);
end;

end.
