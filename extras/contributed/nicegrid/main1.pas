unit main1;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_menu,
  fpg_nicegrid, fpg_button, fpg_checkbox, fpg_label;

type

  TfrmMain = class(TfpgForm)
  private
    FFileSubMenu: TfpgPopupMenu;
    FMenuBar : TfpgMenuBar;
    Grid1: TfpgNiceGrid;
    CheckBox1: TfpgCheckBox;
    CheckBox2: TfpgCheckBox;
    CheckBox3: TfpgCheckBox;
    CheckBox4: TfpgCheckBox;
    CheckBox5: TfpgCheckBox;
    CheckBox6: TfpgCheckBox;
    Label1: TfpgLabel;
    Button1: TfpgButton;
    Button2: TfpgButton;
    Button3: TfpgButton;
    Button4: TfpgButton;
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Grid1DrawHeader(Sender: TObject; ACanvas: TfpgCanvas;
      Rc: TfpgRect; Str: string; var Handled: Boolean);
    procedure Grid1InsertRow(Sender: TObject; ARow: Integer);
    
    procedure miExitClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

  
implementation
  

procedure TfrmMain.AfterCreate;
var x: integer;  
begin
  Name := 'frmMain';
  SetPosition(252, 121, 638, 575);
  WindowTitle := 'NiceGrid - Demo 1';
  Hint := '';
  FFileSubMenu := TfpgPopupMenu.Create(self);
  with FFileSubMenu do
  begin
    Name := 'FFileSubMenu';
    SetPosition(0, 0, 120, 32);
  end;
  FFileSubMenu.AddMenuItem('&Quit', 'Ctrl-Q', @miExitClicked);
  FMenuBar := CreateMenuBar(self);
  FMenuBar.AddMenuItem('&File', nil).SubMenu := FFileSubMenu;
  
 Grid1 := TfpgNiceGrid.Create(self);
  with Grid1 do
  begin
    Name := 'Grid1';
    SetPosition(16, 88, 597, 370);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Grid';
    Hint := '';
 //   Cursor = 1
    RowCount := 20;
    AutoAddRow := True;
    DefColWidth := 100;
    Color := $ffece9d8;
    GridColor := clSilver;
    HeaderLine := 2;
    HeaderColor := clButtonFace;
    HeaderLightColor := clHilite1;
    HeaderDarkColor := clShadow1;
    HeaderFontColor := clWhite;
    HeaderFont := 'MS Sans Serif';
    FooterFontColor := clRed;
    SelectionColor := $ffD2D2FF;
    BeginUpdate;  // JP
    with Columns.Add do
    begin
      Title := 'Merged;Multilined|Merged;Multilined';
      Footer := 'Footer 0';
      Font:='Arial-8';
      FontColor:=clBlack;
      Width := 100;
      CanResize := False;
    end;
    with Columns.Add do
    begin
      Title:='First Group|One';
      Footer:='Footer 1';
      Width:=100;
      Font:='Arial-8';
      FontColor:=clRed;
      Color:=$FFFFFACD;//14024703;
      HorzAlign:=haCenter;
    end;
    with Columns.Add do
    begin
      Title:='First Group|Two';
      Footer:='Footer 2';
      Font:='Arial-8';
      FontColor:=clBlack;
      Width:=100;
    end;
    with Columns.Add do
    begin
      Title:='Second Group|One';
      Footer:='Footer 3';
      Width:=100;
      Font:='Arial-8';
      FontColor:=clBlack;
      Color:=clWhite;
      HorzAlign:=haRight;
    end;
    with Columns.Add do
    begin
      Title:='Second Group|Two';
      Footer:='Footer 4';
      Font:='Arial-8';
      FontColor:=clBlack;
      Width:=100;
      HorzAlign:=haCenter;
    end;
    GutterKind:=gkNumber;
    GutterWidth:=40;
    GutterFont:='Arial-8';
    GutterFontColor:=clBlack;
    ShowFooter:=True;
    OnDrawHeader:=@Grid1DrawHeader;
    OnInsertRow:=@Grid1InsertRow;
    TabOrder:=0;
    EndUpdate;
  end;  {Grid1}
  
  Label1:= TfpgLabel.Create(self);
  with Label1 do
  begin	  
    SetPosition(16, 42, 300, 18);
    Text:= '- Try to copy paste a cell with Ctrl+c and  Ctrl+v' 
  end;  
  CheckBox1:= TfpgCheckBox.Create(self);
  with CheckBox1 do
  begin
    Name:='CheckBox1';
    SetPosition(16,470,49,17);  
    Anchors := [anLeft, anBottom];
    Text := 'Flat';
    Checked := True;
    TabOrder := 1;
    OnChange := @CheckBox1Click;
  end;
  CheckBox2:= TfpgCheckBox.Create(self);
  with CheckBox2 do
  begin
    Name:='CheckBox2';
    SetPosition(78,470,110,17);  
    Anchors := [anLeft, anBottom];
    Text:= 'System Colors';
    Checked := True;
    TabOrder := 2;
    OnChange := @CheckBox2Click;
  end;
  CheckBox3:= TfpgCheckBox.Create(self);
  with CheckBox3 do
  begin
    Name:='CheckBox3';
    SetPosition(192,470,90,17);  
    Anchors := [anLeft, anBottom];
    Text := 'Fit to Width';
    TabOrder := 3;
    OnChange := @CheckBox3Click;
  end;
  CheckBox4:= TfpgCheckBox.Create(self);
  with CheckBox4 do
  begin
    Name:='CheckBox4';
    SetPosition(288,470,135,17);  
    Anchors := [anLeft, anBottom];
    Text := 'Auto Column Width';
    TabOrder := 4;
    OnChange := @CheckBox4Click;
  end;
  CheckBox5:= TfpgCheckBox.Create(self);
  with CheckBox5 do
  begin
    Name:='CheckBox5';
    SetPosition(424,470,90,17);  
    Anchors := [anLeft, anBottom];
    Text := 'Show Grids';
    Checked := True;
    TabOrder := 5;
    OnChange := @CheckBox5Click;
  end;
  CheckBox6:= TfpgCheckBox.Create(self);
  with CheckBox6 do
  begin
    Name:='CheckBox6';
    SetPosition(528,470,95,17);
    Anchors := [anLeft, anBottom];
    Text := 'Show Footer';
    Checked := True;
    TabOrder := 10;
    OnChange:= @CheckBox6Click;
  end;
  
  Button1:= TfpgButton.Create(self);
  with Button1 do
  begin
    Name:='Button1';
    SetPosition(272,505,129,25);
    Anchors:= [anLeft, anBottom];
    Text:= 'Hide 3rd Column';
    TabOrder:= 8;
    OnClick:= @Button1Click;
  end;
  Button2:= TfpgButton.Create(self);
  with Button2 do
  begin
    Name:='Button2';
    SetPosition(16,505,121,25);
    Anchors:= [anLeft, anBottom];
    Text:= 'Insert New Row';
    TabOrder:= 6;
    OnClick:= @Button2Click;
  end;
  Button3:= TfpgButton.Create(self);
  with Button3 do
  begin
    Name:='Button3';
    SetPosition(144,505,121,25);
    Anchors:= [anLeft, anBottom];
    Text:= 'Delete Current Row';
    TabOrder:= 7;
    OnClick:= @Button3Click;
  end;
  Button4:= TfpgButton.Create(self);
  with Button4 do
  begin
    Name:='Button4';
    SetPosition(416,505,180,25);
    Anchors:= [anLeft, anBottom];
    Text:= 'Toggle ReadOnly 3rd Column';
    TabOrder:= 9;
    OnClick:=@Button4Click;
  end;

  Grid1.BeginUpdate;
  for x := 0 to 9 do
  begin
    Grid1[0, x] := 'Sample Text';
    Grid1[1, x] := 'Centered Text';
    Grid1[2, x] := 'Left Alignment';
    Grid1[3, x] := FormatFloat('### ### ##0.##', Random(20000000));
    Grid1[4, x] := IntToStr(Random(2000));
  end;
  Grid1.EndUpdate;
end;
  
procedure TfrmMain.miExitClicked(Sender: TObject);
begin
  Close;
end;
  
procedure TfrmMain.CheckBox1Click(Sender: TObject);
begin
  Grid1.Flat := CheckBox1.Checked;
end;

procedure TfrmMain.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
  begin
    with Grid1 do
    begin
      BeginUpdate;
      GridColor := clSilver;  
      HeaderColor := clButtonFace;
      HeaderDarkColor := clShadow1;
      HeaderLightColor := clHilite1;
      HeaderFontColor := clBlack;
      GutterFontColor:=clBlack;
      EndUpdate; 
    end;
  end
  else
  begin
    with Grid1 do
    begin
      BeginUpdate; 
      GridColor := clGray;
      HeaderColor := $FF0000DF;
      HeaderDarkColor := clBlack;
      HeaderLightColor := $FF0080FF;
      HeaderFontColor := clWhite;
      GutterFontColor:=clWhite;
      EndUpdate;
    end;
  end;
  Grid1.Invalidate; 
end;

procedure TfrmMain.CheckBox3Click(Sender: TObject);
begin
  Grid1.FitToWidth := CheckBox3.Checked;
end;

procedure TfrmMain.CheckBox4Click(Sender: TObject);
begin
  Grid1.AutoColWidth := CheckBox4.Checked;
end;

procedure TfrmMain.CheckBox5Click(Sender: TObject);
begin
  Grid1.ShowGrid := CheckBox5.Checked;
end;

procedure TfrmMain.CheckBox6Click(Sender: TObject);
begin
  Grid1.ShowFooter := CheckBox6.Checked;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  Grid1.Columns[2].Visible := not Grid1.Columns[2].Visible;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  Grid1.InsertRow(Grid1.Row);
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  Grid1.DeleteRow(Grid1.Row);
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  Grid1.Columns[2].ReadOnly := not Grid1.Columns[2].ReadOnly;
end;

procedure TfrmMain.Grid1DrawHeader(Sender: TObject; ACanvas: TfpgCanvas;
  Rc: TfpgRect; Str: String; var Handled: Boolean);
begin
  if (Str = 'One')
    then ACanvas.SetTextColor(clRed);
end;

procedure TfrmMain.Grid1InsertRow(Sender: TObject; ARow: Integer);
begin
  Grid1.Cells[0, ARow] := 'New Row';
end;
  
end.
