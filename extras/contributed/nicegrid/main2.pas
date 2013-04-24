unit main2;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,  fpg_base, fpg_main, fpg_form, fpg_nicegrid;

type
  TfrmMain = class(TfpgForm)
  private
    Grid1: TfpgNiceGrid;	  
    procedure Grid1DrawCell(Sender: TObject; ACanvas: TfpgCanvas; X, Y: Integer; Rc: TfpgRect; var Handled: Boolean);
  public
    procedure AfterCreate; override;
  end;


implementation


procedure TfrmMain.AfterCreate;
var
  x: Integer;
begin
  Name := 'frmMain';
  SetPosition(471, 120, 472, 419);
  WindowTitle := 'Customizing Cells Based on Conditions';
  Hint := '';
  Grid1 := TfpgNiceGrid.Create(self);
  with Grid1 do
  begin
    BeginUpdate; //JP
    Name := 'Grid1';
    SetPosition(16, 16, 441, 385);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Grid';
    Color:=$ECE9D8; 
    GridColor := clSilver;
    ColCount:= 5;
    RowCount:= 20;
    FooterFontColor:= clBlack;
    FitToWidth:= True;
    
    with Columns.Items[0]do
    begin
      Title:= 'Column 1';
      Width:= 84;
    end;
    with Columns.Items[1]do
    begin
      Title:= 'Column 2';
      Width:= 84;
    end;
    with Columns.Items[2]do
    begin
      Title:= 'Column 3';
      Width:= 83;
    end;
    with Columns.Items[3]do
    begin
      Title:= 'Column 4';
      Width:= 83;
    end;
    with Columns.Items[4]do
    begin
      Title:= 'Column 5';
      Width:= 83;
    end;
    GutterFont:='Arial-8';
    GutterFontColor:=clBlack;
    ShowFooter:= False;
    OnDrawCell:= @Grid1DrawCell;
    TabOrder:= 0;
    EndUpdate; //JP
  end;
  
  Grid1.BeginUpdate; //JP
  for x := 0 to 19 do
  begin
    Grid1.Cells[0, x] := IntToStr(Random(100));
    Grid1.Cells[1, x] := IntToStr(Random(100));
    Grid1.Cells[2, x] := IntToStr(Random(100));
    Grid1.Cells[3, x] := IntToStr(Random(100));
    Grid1.Cells[4, x] := IntToStr(Random(100));
  end;
  Grid1.EndUpdate; //JP
end;

procedure TfrmMain.Grid1DrawCell(Sender: TObject; ACanvas: TfpgCanvas; X,
  Y: Integer; Rc: TfpgRect; var Handled: Boolean);
var
  i: Integer;
begin
  i := StrToIntDef(Grid1.Cells[X, Y], 0);
  if Odd(i)
    then ACanvas.SetTextColor (clRed);
  if ((i mod 10) = 0)
    then ACanvas.SetColor(clYellow);
end;


end.
