unit frm_basic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_widget, fpg_form, fpg_label, fpg_button;

type
  TBasicAlignments = class(TfpgForm)
  private
    lblTop: array[1..3] of TfpgLabel;
    lblBottom: array[1..3] of TfpgLabel;
    lblLeft: array[1..3] of TfpgLabel;
    lblRight: array[1..3] of TfpgLabel;
    lblClient: TfpgLabel;
    lblNone: TfpgLabel;
    btnNameNE: TfpgButton;
    btnNameNW: TfpgButton;
    btnNameSW: TfpgButton;
    btnNameSE: TfpgButton;
  public
    procedure AfterCreate; override;
  end;

implementation


{ TBasicAlignments }

procedure TBasicAlignments.AfterCreate;
var
  x: integer;
  y: integer;
  n: integer;
  ColorArray: array[1..3] of TfpgColor;
begin
  Width := 300;
  Height := 300;
  WindowTitle := 'Basic Alignment';
  WindowPosition := wpOneThirdDown;
  MinWidth := 50;
  MinHeight := 50;

  x := 10;
  y := 10;
  ColorArray[1] := clDodgerBlue;
  ColorArray[2] := clDeepSkyBlue;
  ColorArray[3] := clSkyBlue;

  for n := low(lblTop) to high(lblTop) do
  begin
    lblTop[n] := CreateLabel(self, x, y, 'alTop '+IntToStr(n));
    lblTop[n].BackgroundColor := ColorArray[n];
    lblTop[n].Align := alTop;
    lblTop[n].Width := 100;
    inc(y,20);
  end;

  y := 280;
  for n:=low(lblBottom) to high(lblBottom) do
  begin
    lblBottom[n] := CreateLabel(self, x, y, 'alBottom '+IntToStr(n));
    lblBottom[n].BackgroundColor := ColorArray[n];
    lblBottom[n].Align := alBottom;
    dec(y,20);
  end;

  y := 100;
  x := 10;
  for n:=low(lblLeft) to high(lblLeft) do
  begin
    lblLeft[n] := CreateLabel(self, x, y, 'L'+IntToStr(n));
    lblLeft[n].BackgroundColor := ColorArray[n];
    lblLeft[n].Align := alLeft;
    inc(x,30);
  end;

  x := 200;
  for n:=low(lblRight) to high(lblRight) do
  begin
    lblRight[n] := CreateLabel(self, x, y, 'R'+IntToStr(n));
    lblRight[n].BackgroundColor := ColorArray[n];
    lblRight[n].Align := alRight;
    dec(x,30);
  end;

  lblClient := CreateLabel(self, 150, 150, 'alClient');
  lblClient.BackgroundColor := clWhite;
  lblClient.Align := alClient;

  lblNone := CreateLabel(self, 15, 120, 'Resize the form to see Align in action');
  lblNone.TextColor := clWhite;
  lblNone.BackgroundColor := clBlack;



  btnNameNW := TfpgButton.Create(self);
  with btnNameNW do
  begin
    Name := 'btnNameNW';
    Left := 8;
    Top := 8;
    Width := 30;
    Height := 24;
    Text := 'NW';
    Anchors := [anLeft,anTop];
  end;

  btnNameNE := TfpgButton.Create(self);
  with btnNameNE do
  begin
    Name := 'btnNameNE';
    Left := 280-8-30;
    Top := 8;
    Width := 30;
    Height := 24;
    Text := 'NE';
    Anchors := [anRight,anTop];
  end;

  btnNameSE := TfpgButton.Create(self);
  with btnNameSE do
  begin
    Name := 'btnNameSE';
    Left := 280-8-30;
    Top := 280-8-24;
    Width := 30;
    Height := 24;
    Text := 'SE';
    Anchors := [anRight,anBottom];
  end;

  btnNameSW := TfpgButton.Create(self);
  with btnNameSW do
  begin
    Name := 'btnNameSW';
    Left := 8;
    Top := 280-8-24;
    Width := 30;
    Height := 24;
    Text := 'SW';
    Anchors := [anLeft,anBottom];
  end;

end;

end.
