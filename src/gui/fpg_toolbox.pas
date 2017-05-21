{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2017 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit has helper function to help you create high dpi applications,
      scalling the UI as needed.
}
unit fpg_toolbox;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_main;


procedure ScaleDPI(Control: TComponent; FromDPI: Integer);
function  ScaleX(const SizeX, FromDPI: Integer): Integer;
function  ScaleY(const SizeY, FromDPI: Integer): Integer;
function  MathRound(AValue: ValReal): Int64; inline;
function  MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;


implementation

uses
  fpg_widget,
  fpg_basegrid;

type
  // to access protected methods
  TfpgWidgetFriend = class(TfpgWidget);
  TfpgGridFriend = class(TfpgBaseGrid);

procedure ScaleDPI(Control: TComponent; FromDPI: Integer);
var
  n: integer;
  c: integer;
  widget: TfpgWidget;
begin
  if Control is TfpgWidget then
  begin
    widget := TfpgWidget(Control);
    TfpgWidgetFriend(widget).Loading;
    with widget do
    begin      Left := ScaleX(Left,FromDPI);
      Top := ScaleY(Top,FromDPI);
      Width := ScaleX(Width,FromDPI);
      Height := ScaleY(Height,FromDPI);

      MinWidth := ScaleX(MinWidth, FromDPI);
      MinHeight := ScaleY(MinHeight, FromDPI);
      MaxWidth := ScaleX(MaxWidth, FromDPI);
      MaxHeight := ScaleY(MaxHeight, FromDPI);

      // grids get special treatment
      if (widget is TfpgBaseGrid) then
      begin
        for c := 0 to TfpgGridFriend(widget).ColumnCount-1 do
          TfpgGridFriend(widget).ColumnWidth[c] := ScaleX(TfpgGridFriend(widget).ColumnWidth[c], FromDPI);
      end;
    end;
    TfpgWidgetFriend(widget).Loaded;
    // process children
    if widget.ComponentCount > 0 then
    begin
      for n := 0 to widget.ComponentCount-1 do
      begin
        if widget.Components[n] is TfpgWidget then
        begin
          ScaleDPI(widget.Components[n], FromDPI);
        end;
      end;
    end;
    widget.UpdatePosition;
  end; { if Control }
end;

function ScaleX(const SizeX, FromDPI: Integer): Integer;
begin
  Result := MulDiv(SizeX, fpgApplication.Screen_dpi_x, FromDPI);
end;

function ScaleY(const SizeY, FromDPI: Integer): Integer;
begin
  Result := MulDiv(SizeY, fpgApplication.Screen_dpi_y, FromDPI);
end;

function MathRound(AValue: ValReal): Int64; inline;
begin
  if AValue >= 0 then
    Result := Trunc(AValue + 0.5)
  else
    Result := Trunc(AValue - 0.5);
end;

function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
begin
  if nDenominator = 0 then
    Result := -1
  else
    Result := MathRound(int64(nNumber) * int64(nNumerator) / nDenominator);
end;


end.

