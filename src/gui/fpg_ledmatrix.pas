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
      This unit defines a classic 5x7 LED Matrix display widget. The
      layout of the matrix is as follows. Each column is represented by a Byte
      data type. In the figure belowe we are representing a '?' symbol. The o
      denotes the bit is not set. The x denotes the bit is set.

            |  COLUMN
            | 0 1 2 3 4
       -----+----------
          0 | o x x x o
          1 | x o o o x
       R  2 | o o o o x
       O  3 | o o o x o
       W  4 | o o x o o
          5 | o o o o o
          6 | o o x o o
          7 | -unused-


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
    procedure   SetLEDSize(const avalue: integer);
    procedure   SetLEDGap(const avalue: integer);
    procedure   SetText(const avalue: TfpgString);
  protected
    procedure   HandlePaint; override;
    procedure   PaintBackgroundLEDs(const AX, AY: TfpgCoord); virtual;
    procedure   DrawLEDChar(const AX, AY: TfpgCoord; const AChar: TfpgChar); virtual;
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

uses
  fpg_stringutils;

type

  TLEDCharMask = object
    Col0: byte;
    Col1: byte;
    Col2: byte;
    Col3: byte;
    Col4: byte;
    function GetMaskForColumn(const ACol: integer): Byte;
    function IsBitSet(const ACol, ARow: integer): boolean;
  end;

const
  cLEDFont: array[0..2] of TLEDCharMask = (
      (Col0: $00; Col1: $00; Col2: $1A; Col3: $1C; Col4: $00),    //  ,
      (Col0: $FE; Col1: $90; Col2: $90; Col3: $90; Col4: $6E),    //  R
      (Col0: $40; Col1: $80; Col2: $9A; Col3: $90; Col4: $60)     //  ?
    );


{ TLEDCharMask }

function TLEDCharMask.GetMaskForColumn(const ACol: integer): Byte;
begin
  case ACol of
    0: Result := Col0;
    1: Result := Col1;
    2: Result := Col2;
    3: Result := Col3;
    4: Result := Col4;
  end;
end;

function TLEDCharMask.IsBitSet(const ACol, ARow: integer): boolean;
begin
  Result := (GetMaskForColumn(ACol) and ($80 shr ARow)) <> 0;
end;


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

procedure TfpgLEDMatrix.PaintBackgroundLEDs(const AX, AY: TfpgCoord);
var
  c, r: integer;
  dx, dy: TfpgCoord;
begin
  Canvas.Color := LEDOffColor;
  dx := AX;
  while dx < Width do
  begin
    for c := 0 to 4 do  // 5 columns
    begin
      dy := AY;
      for r := 0 to 6 do  // 7 rows
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
var
  i: integer;
  c: TfpgChar;
  dx, dy: TfpgCoord;
  lStartY: TfpgCoord;
begin
  inherited HandlePaint;
  dx := LEDGap;
  dy := (GetClientRect.Height - (LEDSize * 7) - (LEDGap * 6)) div 2;
  Canvas.Color := LEDOffColor;
  PaintBackgroundLEDs(dx, dy);
  for i := 0 to UTF8Length(Text)-1 do
    DrawLEDChar(dx, dy, UTF8Copy(Text, i, 1));
end;

procedure TfpgLEDMatrix.SetText(const avalue: TfpgString);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  RePaint;
end;

procedure TfpgLEDMatrix.DrawLEDChar(const AX, AY: TfpgCoord; const AChar: TfpgChar);
begin
end;

end.
