unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_button,
  fpg_label,
  fpg_panel,
  fpg_radiobutton,
  fpg_combobox,
  fpg_checkbox;

type
  TfrmMain = class(TfpgForm)
  private
    L_Bevel: TfpgLabel;
    B_Bevel: TfpgBevel;
    L_BLabel: TfpgLabel;
    L_Panel: TfpgLabel;
    P_Panel: TfpgPanel;
    L_PLabel: TfpgLabel;
    L_GroupBox: TfpgLabel;
    G_GroupBox: TfpgGroupBox;
    L_GLabel: TfpgLabel;
    B_Align: TfpgBevel;
    RB_Left: TfpgRadioButton;
    RB_Center: TfpgRadioButton;
    RB_Right: TfpgRadioButton;
    B_Layout: TfpgBevel;
    RB_Top: TfpgRadioButton;
    RB_LCenter: TfpgRadioButton;
    RB_Bottom: TfpgRadioButton;
    B_Margin: TfpgBevel;
    RB_M0: TfpgRadioButton;
    RB_M2: TfpgRadioButton;
    RB_M20: TfpgRadioButton;
    B_LigneSpace: TfpgBevel;
    RB_LS0: TfpgRadioButton;
    RB_LS2: TfpgRadioButton;
    RB_LS5: TfpgRadioButton;
    RB_LS10: TfpgRadioButton;
    B_Color: TfpgBevel;
    RB_Silver: TfpgRadioButton;
    RB_Yellow: TfpgRadioButton;
    RB_Cream: TfpgRadioButton;
    B_TextColor: TfpgBevel;
    RB_Black: TfpgRadioButton;
    RB_Blue: TfpgRadioButton;
    RB_Red: TfpgRadioButton;
    RB_Green: TfpgRadioButton;
    B_Style: TfpgBevel;
    RB_Raised: TfpgRadioButton;
    RB_Lowered: TfpgRadioButton;
    Bt_Quit: TfpgButton;
    cbxShape: TfpgComboBox;
    cbxBorder: TfpgComboBox;
    chkEnabled: TfpgCheckBox;
    procedure chkEnabledChanged(Sender: TObject);
    procedure RB_LeftChange(Sender: TObject);
    procedure RB_CenterChange(Sender: TObject);
    procedure RB_RightChange(Sender: TObject);
    procedure RB_TopChange(Sender: TObject);
    procedure RB_LCenterChange(Sender: TObject);
    procedure RB_BottomChange(Sender: TObject);
    procedure RB_M0Change(Sender: TObject);
    procedure RB_M2Change(Sender: TObject);
    procedure RB_M20Change(Sender: TObject);
    procedure RB_LS0Change(Sender: TObject);
    procedure RB_LS2Change(Sender: TObject);
    procedure RB_LS5Change(Sender: TObject);
    procedure RB_LS10Change(Sender: TObject);
    procedure RB_SilverChange(Sender: TObject);
    procedure RB_YellowChange(Sender: TObject);
    procedure RB_CreamChange(Sender: TObject);
    procedure RB_BlackChange(Sender: TObject);
    procedure RB_BlueChange(Sender: TObject);
    procedure RB_RedChange(Sender: TObject);
    procedure RB_GreenChange(Sender: TObject);
    procedure RB_RaisedChange(Sender: TObject);
    procedure RB_LoweredChange(Sender: TObject);
    procedure Bt_QuitClick(Sender: TObject);
    procedure cbxShapeChange(Sender: TObject);
    procedure cbxBorderChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

procedure TfrmMain.chkEnabledChanged(Sender: TObject);
begin
  P_Panel.Enabled := chkEnabled.Checked;
  B_Bevel.Enabled := chkEnabled.Checked;
  G_GroupBox.Enabled := chkEnabled.Checked;
end;

procedure TfrmMain.RB_LeftChange(Sender: TObject);
begin
  P_Panel.Alignment    := taLeftJustify;
  G_GroupBox.Alignment := taLeftJustify;
end;

procedure TfrmMain.RB_CenterChange(Sender: TObject);
begin
  P_Panel.Alignment    := taCenter;
  G_GroupBox.Alignment := taCenter;
end;

procedure TfrmMain.RB_RightChange(Sender: TObject);
begin
  P_Panel.Alignment    := taRightJustify;
  G_GroupBox.Alignment := taRightJustify;
end;

procedure TfrmMain.RB_TopChange(Sender: TObject);
begin
  P_Panel.Layout := tlTop;
end;

procedure TfrmMain.RB_LCenterChange(Sender: TObject);
begin
  P_Panel.Layout := tlCenter;
end;

procedure TfrmMain.RB_BottomChange(Sender: TObject);
begin
  P_Panel.Layout := tlBottom;
end;

procedure TfrmMain.RB_M0Change(Sender: TObject);
begin
  P_Panel.Margin    := 0;
  G_GroupBox.Margin := 0;
end;

procedure TfrmMain.RB_M2Change(Sender: TObject);
begin
  P_Panel.Margin    := 2;
  G_GroupBox.Margin := 2;
end;

procedure TfrmMain.RB_M20Change(Sender: TObject);
begin
  P_Panel.Margin    := 20;
  G_GroupBox.Margin := 20;
end;

procedure TfrmMain.RB_LS0Change(Sender: TObject);
begin
  P_Panel.LineSpace := 0;
end;

procedure TfrmMain.RB_LS2Change(Sender: TObject);
begin
  P_Panel.LineSpace := 2;
end;

procedure TfrmMain.RB_LS5Change(Sender: TObject);
begin
  P_Panel.LineSpace := 5;
end;

procedure TfrmMain.RB_LS10Change(Sender: TObject);
begin
  P_Panel.LineSpace := 10;
end;

procedure TfrmMain.RB_SilverChange(Sender: TObject);
begin
  B_Bevel.BackgroundColor    := clSilver;
  P_Panel.BackgroundColor    := clSilver;
  G_GroupBox.BackgroundColor := clSilver;
end;

procedure TfrmMain.RB_YellowChange(Sender: TObject);
begin
  B_Bevel.BackgroundColor    := clYellow;
  P_Panel.BackgroundColor    := clYellow;
  G_GroupBox.BackgroundColor := clYellow;
end;

procedure TfrmMain.RB_CreamChange(Sender: TObject);
begin
  B_Bevel.BackgroundColor    := clCream;
  P_Panel.BackgroundColor    := clCream;
  G_GroupBox.BackgroundColor := clCream;
end;

procedure TfrmMain.RB_BlackChange(Sender: TObject);
begin
  P_Panel.TextColor    := clBlack;
  G_GroupBox.TextColor := clBlack;
end;

procedure TfrmMain.RB_BlueChange(Sender: TObject);
begin
  P_Panel.TextColor    := clBlue;
  G_GroupBox.TextColor := clBlue;
end;

procedure TfrmMain.RB_RedChange(Sender: TObject);
begin
  P_Panel.TextColor    := clRed;
  G_GroupBox.TextColor := clRed;
end;

procedure TfrmMain.RB_GreenChange(Sender: TObject);
begin
  P_Panel.TextColor    := clGreen;
  G_GroupBox.TextColor := clGreen;
end;

procedure TfrmMain.RB_RaisedChange(Sender: TObject);
begin
  B_Bevel.Style    := bsRaised;
  P_Panel.Style    := bsRaised;
  G_GroupBox.Style := bsRaised;
end;

procedure TfrmMain.RB_LoweredChange(Sender: TObject);
begin
  B_Bevel.Style    := bsLowered;
  P_Panel.Style    := bsLowered;
  G_GroupBox.Style := bsLowered;
end;

procedure TfrmMain.Bt_QuitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.cbxShapeChange(Sender: TObject);
begin
  if Sender is TfpgComboBox then
    B_Bevel.Shape := TPanelShape((Sender as TfpgComboBox).FocusItem);
end;

procedure TfrmMain.cbxBorderChange(Sender: TObject);
begin
  if cbxBorder.FocusItem = 0 then
  begin
    B_Bevel.BorderStyle     := bsSingle;
    P_Panel.BorderStyle     := bsSingle;
    G_GroupBox.BorderStyle  := bsSingle;
  end
  else
  begin
    B_Bevel.BorderStyle     := bsDouble;
    P_Panel.BorderStyle     := bsDouble;
    G_GroupBox.BorderStyle  := bsDouble;
  end;
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name           := 'Main';
  SetPosition(0, 0, 800, 600);
  WindowPosition := wpScreencenter;
  WindowTitle := 'Bevel, Panel and GroupBox demo';
  
  L_Bevel        := CreateLabel(Self, 50, 30, 'Here is a TfpgBevel');
  B_Bevel        := CreateBevel(Self, 50, 50, 300, 200, bsBox, bsRaised);
  B_Bevel.BackgroundColor := clYellow;
  L_BLabel       := CreateLabel(B_Bevel, 10, 10, 'A simple label on a bevel');
  L_Panel        := CreateLabel(Self, 450, 30, 'Here is a TfpgPanel');
  P_Panel        := CreatePanel(Self, 450, 50, 300, 200, 'Panel', bsRaised);
  P_Panel.BackgroundColor := clYellow;
  P_Panel.Alignment := taCenter;
  P_Panel.Layout := tlCenter;
  P_Panel.TextColor := clRed;
  P_Panel.Text   := 'This is to try a long text on a panel' + #13 + 'including a line feed';
  P_Panel.FontDesc := 'bitstream vera sans-12:bold:italic';
  P_Panel.LineSpace := 2;
  P_Panel.Margin := 2;
  P_Panel.WrapText := True;
  L_PLabel       := CreateLabel(P_Panel, 10, 10, 'A simple label on a panel');
  L_GroupBox     := CreateLabel(Self, 50, 260, 'Here is a TfpgGroupBox');
  G_GroupBox     := CreateGroupBox(Self, 50, 280, 300, 200, 'Group box', bsRaised);
  G_GroupBox.BackgroundColor := clYellow;
  G_GroupBox.TextColor := clGreen;
  G_GroupBox.Text := 'This is a groupbox';
  G_GroupBox.Margin := 2;
  G_GroupBox.FontDesc := 'bitstream vera sans-10:italic';
  L_GLabel       := CreateLabel(G_GroupBox, 10, 20, 'A simple label on a groupbox');
  B_Align        := CreateBevel(Self, 400, 270, 180, 100, bsBox, bsRaised);
  RB_Left        := CreateRadioButton(B_Align, 20, 20, 'Align left');
  RB_Left.OnChange := @RB_LeftChange;
  RB_Center      := CreateRadioButton(B_Align, 20, 40, 'Align center');
  RB_Center.OnChange := @RB_CenterChange;
  RB_Center.Checked := True;
  RB_Right       := CreateRadioButton(B_Align, 20, 60, 'Align right');
  RB_Right.OnChange := @RB_RightChange;
  B_Layout       := CreateBevel(Self, 600, 270, 180, 100, bsBox, bsRaised);
  RB_Top         := CreateRadioButton(B_Layout, 20, 20, 'Layout top');
  RB_Top.OnChange := @RB_TopChange;
  RB_LCenter     := CreateRadioButton(B_Layout, 20, 40, 'Layout center');
  RB_LCenter.OnChange := @RB_LCenterChange;
  RB_LCenter.Checked := True;
  RB_Bottom      := CreateRadioButton(B_Layout, 20, 60, 'Layout bottom');
  RB_Bottom.OnChange := @RB_BottomChange;
  B_Margin       := CreateBevel(Self, 400, 380, 180, 100, bsBox, bsRaised);
  RB_M0          := CreateRadioButton(B_Margin, 20, 20, 'Margin 0');
  RB_M0.OnChange := @RB_M0Change;
  RB_M2          := CreateRadioButton(B_Margin, 20, 40, 'Margin 2');
  RB_M2.OnChange := @RB_M2Change;
  RB_M2.Checked  := True;
  RB_M20         := CreateRadioButton(B_Margin, 20, 60, 'Margin 20');
  RB_M20.OnChange := @RB_M20Change;
  B_LigneSpace   := CreateBevel(Self, 600, 380, 180, 100, bsBox, bsRaised);
  RB_LS0         := CreateRadioButton(B_LigneSpace, 20, 10, 'Line space 0');
  RB_LS0.OnChange := @RB_LS0Change;
  RB_LS2         := CreateRadioButton(B_LigneSpace, 20, 30, 'Line space 2');
  RB_LS2.OnChange := @RB_LS2Change;
  RB_LS2.Checked := True;
  RB_LS5         := CreateRadioButton(B_LigneSpace, 20, 50, 'Line space 5');
  RB_LS5.OnChange := @RB_LS5Change;
  RB_LS10        := CreateRadioButton(B_LigneSpace, 20, 70, 'Line space 10');
  RB_LS10.OnChange := @RB_LS10Change;
  B_Color        := CreateBevel(Self, 400, 490, 180, 100, bsBox, bsRaised);
  RB_Silver      := CreateRadioButton(B_Color, 20, 20, 'Background silver');
  RB_Silver.OnChange := @RB_SilverChange;
  RB_Yellow      := CreateRadioButton(B_Color, 20, 40, 'Background yellow');
  RB_Yellow.OnChange := @RB_YellowChange;
  RB_Yellow.Checked := True;
  RB_Cream       := CreateRadioButton(B_Color, 20, 60, 'Background cream');
  RB_Cream.OnChange := @RB_CreamChange;
  B_TextColor    := CreateBevel(Self, 600, 490, 180, 100, bsBox, bsRaised);
  RB_Black       := CreateRadioButton(B_TextColor, 20, 10, 'Text black');
  RB_Black.OnChange := @RB_BlackChange;
  RB_Blue        := CreateRadioButton(B_TextColor, 20, 30, 'Text blue');
  RB_Blue.OnChange := @RB_BlueChange;
  RB_Red         := CreateRadioButton(B_TextColor, 20, 50, 'Text red');
  RB_Red.OnChange := @RB_RedChange;
  RB_Red.Checked := True;
  RB_Green       := CreateRadioButton(B_TextColor, 20, 70, 'Text green');
  RB_Green.OnChange := @RB_GreenChange;
  B_Style        := CreateBevel(Self, 50, 500, 180, 80, bsBox, bsRaised);
  RB_Raised      := CreateRadioButton(B_Style, 20, 10, 'Raised');
  RB_Raised.OnChange := @RB_RaisedChange;
  RB_Raised.Checked := True;
  RB_Lowered     := CreateRadioButton(B_Style, 20, 30, 'Lowered');
  RB_Lowered.OnChange := @RB_LoweredChange;
  chkEnabled := CreateCheckBox(B_Style, 20, 50, 'Enabled');
  chkEnabled.Checked := True;
  chkEnabled.OnChange := @chkEnabledChanged;
  
  Bt_Quit        := CreateButton(Self, 250, 555, 100, 'Quit', @Bt_QuitClick);
  
  cbxShape := CreateComboBox(self, 250, 500, 100, nil);
  cbxShape.Items.Add('bsBox');
  cbxShape.Items.Add('bsFrame');
  cbxShape.Items.Add('bsTopLine');
  cbxShape.Items.Add('bsBottomLine');
  cbxShape.Items.Add('bsLeftLine');
  cbxShape.Items.Add('bsRightLine');
  cbxShape.Items.Add('bsSpacer');
  cbxShape.FocusItem := 0;
  cbxShape.OnChange := @cbxShapeChange;
  
  cbxBorder := CreateComboBox(self, 250, 528, 100, nil);
  cbxBorder.Items.Add('bsSingle');
  cbxBorder.Items.Add('bsDouble');
  cbxBorder.FocusItem := 0;
  cbxBorder.OnChange := @cbxBorderChange;
end;

end.

