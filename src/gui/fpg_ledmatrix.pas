{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2015 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit defines a classic 5x7 LED Matrix display widget.
}
unit fpg_ledmatrix;

{$mode objfpc}{$h+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget;

type

  TfpgLEDMatrix = class(TfpgWidget)
  private
    FLEDOnColor: TfpgColor;
    FLEDOffColor: TfpgColor;
    FLEDSize: integer;
    FLEDGap: integer;
    FText: TfpgString;
    procedure   SetLEDOnColor(const avalue: TfpgColor);
    procedure   SetLEDOffColor(const avalue: TfpgColor);
    procedure    PaintBackgroundLEDs;
    procedure   SetLEDSize(const avalue: integer);
    procedure   SetLEDGap(const avalue: integer);
    procedure   SetText(const avalue: TfpgString);
  protected
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    BackgroundColor default clBlack;
//    property    BorderStyle: TfpgEditBorderStyle read FBorderStyle write SetBorderStyle default ebsDefault;
    property    Height default 30;
    property    LEDGap: integer read FLEDGap write SetLEDGap default 1;
    property    LEDSize: integer read FLEDSize write SetLEDSize default 2;
    property    LEDOnColor: TfpgColor read FLEDOnColor write SetLEDOnColor default TfpgColor($FFFFB539);
    property    LEDOffColor: TfpgColor read FLEDOffColor write SetLEDOffColor default TfpgColor($FF634210);
    property    Text: TfpgString read FText write SetText;
    property    Width default 150;
  end;


implementation

type

  TLEDCharMask = record
    Col1: byte;
    Col2: byte;
    Col3: byte;
    Col4: byte;
    Col5: byte;
  end;

const
  cLEDFont: array[0..0] of TLEDCharMask = (
      (Col1: $00; Col2: $00; Col3: $0D; Col4: $0E; Col5: $00)
    );


{ TfpgLEDMatrix }

procedure TfpgLEDMatrix.SetLEDOnColor(const avalue: TfpgColor);
begin
  if FLEDOnColor = AValue then
    Exit;
  FLEDOnColor := AValue;
  Repaint;
end;

procedure TfpgLEDMatrix.SetLEDOffColor(const avalue: TfpgColor);
begin
  if FLEDOffColor = AValue then
    Exit;
  FLEDOffColor := AValue;
  Repaint;
end;

procedure TfpgLEDMatrix.PaintBackgroundLEDs;
var
  c, r: integer;
  lStartY: TfpgCoord;
  rect: TfpgRect;
  dx, dy: TfpgCoord;
begin
  rect := GetClientRect;
  lStartY := (rect.Height - (LEDSize * 7) - (LEDGap * 6)) div 2;
  Canvas.Color := LEDOffColor;
  dx := LEDGap;
  while dx < Width do
  begin
    for c := 1 to 5 do
    begin
      dy := lStartY;
      for r := 1 to 7 do
      begin
        Canvas.FillRectangle(dx, dy, LEDSize, LEDSize);
        inc(dy, LEDSize + LEDGap);
      end;
      inc(dx, LEDSize + LEDGap);
    end;
  end;  { while }
end;

constructor TfpgLEDMatrix.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth := 30;
  FHeight := 150;
  FBackgroundColor := clBlack;
  FLEDOnColor := TfpgColor($FFFFB539);
  FLEDOffColor := TfpgColor($FF634210);
  FLEDGap := 1;
  FLEDSize := 2;
end;

procedure TfpgLEDMatrix.SetLEDSize(const avalue: integer);
begin
  if FLEDSize = AValue then
    Exit;
  FLEDSize := AValue;
  Repaint;
end;

procedure TfpgLEDMatrix.SetLEDGap(const avalue: integer);
begin
  if FLEDGap = AValue then
    Exit;
  FLEDGap := AValue;
  Repaint;
end;

procedure TfpgLEDMatrix.HandlePaint;
begin
  inherited HandlePaint;
  PaintBackgroundLEDs;
end;

procedure TfpgLEDMatrix.SetText(const avalue: TfpgString);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  RePaint;
end;

end.
