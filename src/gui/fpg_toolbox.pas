{
    Description:
      This unit has helper function to help you create high dpi applications,
      scalling the UI as needed.
}
unit fpg_toolbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_main, fpg_widget;


procedure ScaleDPI(Control: TComponent; FromDPI: Integer);
function  ScaleX(const SizeX, FromDPI: Integer): Integer;
function  ScaleY(const SizeY, FromDPI: Integer): Integer;
function  MathRound(AValue: ValReal): Int64; inline;
function  MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;


implementation


procedure ScaleDPI(Control: TComponent; FromDPI: Integer);
var
  n: Integer;
  widget: TfpgWidget;
begin
  if Control is TfpgWidget then
  begin
    widget := TfpgWidget(Control);
    with widget do
    begin
      Left := ScaleX(Left,FromDPI);
      Top := ScaleY(Top,FromDPI);
      Width := ScaleX(Width,FromDPI);
      Height := ScaleY(Height,FromDPI);
      UpdateWindowPosition;
//      Font.Size := ScaleY(Font.Size,FromDPI);
    end;
  end;
  if Control is TfpgWidget then
  begin
    widget := TfpgWidget(Control);
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
  end;
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

