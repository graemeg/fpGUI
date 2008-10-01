program aligntest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_widget, fpg_form, fpg_label;

type
  TMainForm = class(TfpgForm)
  private
    lblTop: array[1..3] of TfpgLabel;
    lblBottom: array[1..3] of TfpgLabel;
    lblLeft: array[1..3] of TfpgLabel;
    lblRight: array[1..3] of TfpgLabel;
    lblClient: TfpgLabel;
    lblNone: TfpgLabel;
  public
    procedure AfterCreate; override;
  end;
  

{ TMainForm }

procedure TMainForm.AfterCreate;
var
  x: integer;
  y: integer;
  n: integer;
  ColorArray: array[1..3] of TfpgColor;
begin
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
end;


procedure MainProc;
var
  frm : TMainForm;
begin
  fpgApplication.Initialize;

  frm := TMainForm.Create(nil);
  frm.WindowPosition := wpScreenCenter;
  frm.Width       := 300;
  frm.Height      := 300;
  frm.MinWidth    := 250;
  frm.MinHeight   := 150;
  frm.WindowTitle := 'fpGUI Align Example';
  frm.Show;

  fpgApplication.Run;
  frm.Free;
end;

begin
  MainProc;
end.

