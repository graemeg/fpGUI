unit main3;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,  fpg_base, fpg_main, fpg_form, fpg_panel, fpg_splitter,
  fpg_nicegrid;

type
  TfrmMain = class(TfpgForm)
  private
    Panel1: TfpgPanel;
    Splitter1: TfpgSplitter;
    Grid1: TfpgNiceGrid;
    GridSync1: TfpgNiceGridSync;
  public
    procedure AfterCreate; override;
  end;



implementation

procedure TfrmMain.AfterCreate;
begin
  Name := 'frmMain';
  SetPosition(260, 99, 719, 570);
  WindowTitle := 'Tabel Budget';
  Hint := ''; 
  WindowAttributes := [waSizeable, waScreenCenterPos];
  
  Panel1:= TfpgPanel.Create(self);
  with Panel1 do
  begin	  
    Left:= 16;
    Top:= 16;
    Width:= 682;
    Height:= 504;
    Anchors:= [anLeft, anTop, anRight, anBottom];
    TabOrder:= 0;
  end;

  GridSync1:= TfpgNiceGridSync.Create(Panel1);
  with GridSync1 do
  begin
    BeginUpdate;
    Name := 'GridSync1';
    Left:= 1;
    Top:= 1;
    Width:= 329;
    Height:= 502;
    ColCount:= 3;
    RowCount:= 20;
    AutoAddRow:= True;
    GridColor:= clSilver;
    HeaderLine:= 2;
    FooterFontColor:= clBlack;
    FitToWidth:= True;

    with Columns.Items[0]do
    begin
      Title:= 'Unit Name';
      Width:= 135;
    end;
    with Columns.Items[1]do
    begin
      Title:= 'Unit Cost|Capital';
      Width:= 80;
      Color:= 15790335;
      CanResize:= False;
    end;
    with Columns.Items[2]do
    begin
      Title:= 'Unit Cost|Non Capital';
      Width:= 80;
      Color:= 14671871;
      CanResize:= False;
    end;
    GutterKind:= gkNumber;
    GutterWidth:= 30;
    ShowFooter:= False;
    GutterFont:='Arial-8';
    GutterFontColor:=clBlack;
    Align:= alLeft;
    TabOrder:= 1;
    EndUpdate;
  end;
  
  Splitter1:= TfpgSplitter.Create(Panel1);
  with Splitter1 do
  begin
    Name:='Splitter1';
    SetPosition(330,1,8,502);  
    Align := alLeft;
  end;
  
  Grid1:= TfpgNiceGrid.Create(Panel1);
  with Grid1 do
  begin
    BeginUpdate;
    Name := 'Grid1';
    Left:= 338;
    Top:= 1;
    Width:= 344;
    Height:= 502;
    ColCount:= 12;
    RowCount:= 20;
    GridColor:= clSilver;
    HeaderLine:= 2;
    HeaderColor := clButtonFace;
    HeaderLightColor := clHilite1;
    HeaderDarkColor := clShadow1;
    FooterFontColor:= clBlack;

    with Columns.Items[0]do
    begin
      Title:= '0|Capital';
      Width:= 80;
      Color:= 16775924;
    end;
    with Columns.Items[1]do
    begin
      Title:= '0|Non Capital';
      Width:= 80;
      Color:= 16773601;
    end;
    with Columns.Items[2]do
    begin
      Title:= '2000|Capital';
      Width:= 80;
      Color:= 16775924;
    end;
    with Columns.Items[3]do
    begin
      Title:= '2000|Non Capital';
      Width:= 80;
      Color:= 16773601;
    end;
    with Columns.Items[4]do
    begin
      Title:= '2001|Capital';
      Width:= 80;
      Color:= 16775924;
    end;
    with Columns.Items[5]do
    begin
      Title:= '2001|Non Capital';
      Width:= 80;
      Color:= 16773601;
    end;
    with Columns.Items[6]do
    begin
      Title:= '2002|Capital';
      Width:= 80;
      Color:= 16775924;
    end;
    with Columns.Items[7]do
    begin
      Title:= '2002|Non Capital';
      Width:= 80;
      Color:= 16773601;
    end;
    with Columns.Items[8]do
    begin
      Title:= '2003|Capital';
      Width:= 80;
      Color:= 16775924;
    end;
    with Columns.Items[9]do
    begin
      Title:= '2003|Non Capital';
      Width:= 80;
      Color:= 16773601;
    end;
    with Columns.Items[10]do
    begin
      Title:= '2004|Capital';
      Width:= 80;
      Color:= 16775924;
    end;
    with Columns.Items[11]do
    begin
      Title:= '2004|Non Capital';
      Width:= 80;
      Color:= 16773601;
    end;
    GutterKind:= gkNone;
    GutterWidth:= 40;
    Align:= alClient;

    GutterFont:='Arial-8';
    GutterFontColor:=clBlack;
    ShowFooter:=True;

    TabOrder:= 0;
    EndUpdate;
  end;
  
  GridSync1.MasterGrid:= Grid1;
end;

end.
