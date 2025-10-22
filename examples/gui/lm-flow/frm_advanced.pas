unit frm_advanced;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_panel, fpg_splitter,
  fpg_label, fpg_radiobutton, fpg_spinedit,
  fpg_flowlayout,
  fpg_layouttypes;

type

  TAdvancedFlowForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: AdvancedFlowForm}
    pnlControls: TfpgPanel;
    splVertical: TfpgSplitter;
    pnlFlow: TfpgPanel;
    GroupBox1: TfpgGroupBox;
    rbAlignmentL: TfpgRadioButton;
    rbAlignmentC: TfpgRadioButton;
    rbAlignmentR: TfpgRadioButton;
    rgVAlignment: TfpgGroupBox;
    rbVAlignmentT: TfpgRadioButton;
    rbVAlignmentC: TfpgRadioButton;
    rbVAlignmentB: TfpgRadioButton;
    lblHGap: TfpgLabel;
    seHGap: TfpgSpinEdit;
    lblVGap: TfpgLabel;
    seVGap: TfpgSpinEdit;
    {@VFD_HEAD_END: AdvancedFlowForm}
    FlowLayout: TfpgFlowLayoutManager;
    procedure HGapChanged(Sender: TObject);
    procedure VGapChanged(Sender: TObject);
    procedure AlignmentChanged(Sender: TObject);
    procedure VAlignmentChanged(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TAdvancedFlowForm.AfterCreate;
var
  i: integer;
  Btn: TfpgButton;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: AdvancedFlowForm}
  Name := 'AdvancedFlowForm';
  SetPosition(460, 355, 550, 350);
  WindowTitle := 'Advanced Flow Demo';
  Hint := '';
  IconName := 'stdimg.windowicon';

  pnlControls := TfpgPanel.Create(self);
  with pnlControls do
  begin
    Name := 'pnlControls';
    SetPosition(0, 0, 180, 350);
    Align := alLeft;
    Text := '';
  end;

  splVertical := TfpgSplitter.Create(self);
  with splVertical do
  begin
    Name := 'splVertical';
    SetPosition(180, 0, 8, 350);
    Align := alLeft;
  end;

  pnlFlow := TfpgPanel.Create(self);
  with pnlFlow do
  begin
    Name := 'pnlFlow';
    SetPosition(188, 0, 100, 100);
    Align := alClient;
    Style := bsLowered;
    Text := 'Client Panel';
    TextColor := TfpgColor($FF808080);
  end;

  GroupBox1 := TfpgGroupBox.Create(pnlControls);
  with GroupBox1 do
  begin
    Name := 'GroupBox1';
    SetPosition(8, 8, 160, 110);
    Text := 'Alignments';
  end;

  rbAlignmentL := TfpgRadioButton.Create(GroupBox1);
  with rbAlignmentL do
  begin
    Name := 'rbAlignmentL';
    SetPosition(12, 20, 120, 25);
    Checked := True;
    GroupIndex := 1;
    TabOrder := 1;
    Text := 'Left';
    Tag := 0;
    OnChange := @AlignmentChanged;
  end;

  rbAlignmentC := TfpgRadioButton.Create(GroupBox1);
  with rbAlignmentC do
  begin
    Name := 'rbAlignmentC';
    SetPosition(12, 44, 120, 25);
    GroupIndex := 1;
    TabOrder := 2;
    Text := 'Center';
    Tag := 1;
    OnChange := @AlignmentChanged;
  end;

  rbAlignmentR := TfpgRadioButton.Create(GroupBox1);
  with rbAlignmentR do
  begin
    Name := 'rbAlignmentR';
    SetPosition(12, 68, 120, 25);
    GroupIndex := 1;
    TabOrder := 3;
    Text := 'Right';
    Tag := 2;
    OnChange := @AlignmentChanged;
  end;

  rgVAlignment := TfpgGroupBox.Create(pnlControls);
  with rgVAlignment do
  begin
    Name := 'rgVAlignment';
    SetPosition(8, 132, 160, 110);
    Text := 'Vertical Alignment';
  end;

  rbVAlignmentT := TfpgRadioButton.Create(rgVAlignment);
  with rbVAlignmentT do
  begin
    Name := 'rbVAlignmentT';
    SetPosition(12, 20, 120, 25);
    Checked := True;
    GroupIndex := 2;
    TabOrder := 1;
    Text := 'Top';
    Tag := 0;
    OnChange := @VAlignmentChanged;
  end;

  rbVAlignmentC := TfpgRadioButton.Create(rgVAlignment);
  with rbVAlignmentC do
  begin
    Name := 'rbVAlignmentC';
    SetPosition(12, 44, 120, 25);
    GroupIndex := 2;
    TabOrder := 2;
    Text := 'Center';
    Tag := 1;
    OnChange := @VAlignmentChanged;
  end;

  rbVAlignmentB := TfpgRadioButton.Create(rgVAlignment);
  with rbVAlignmentB do
  begin
    Name := 'rbVAlignmentB';
    SetPosition(12, 68, 120, 25);
    GroupIndex := 2;
    TabOrder := 3;
    Text := 'Bottom';
    Tag := 2;
    OnChange := @VAlignmentChanged;
  end;

  lblHGap := TfpgLabel.Create(pnlControls);
  with lblHGap do
  begin
    Name := 'lblHGap';
    SetPosition(8, 264, 50, 25);
    BackgroundColor := TfpgColor($FFD5D2CD);
    Text := 'HGap';
    TextColor := TfpgColor($FF808080);
  end;

  seHGap := TfpgSpinEdit.Create(pnlControls);
  with seHGap do
  begin
    Name := 'seHGap';
    SetPosition(72, 260, 92, 24);
    TabOrder := 4;
    Value := 5;
    OnChange := @HGapChanged;
  end;

  lblVGap := TfpgLabel.Create(pnlControls);
  with lblVGap do
  begin
    Name := 'lblVGap';
    SetPosition(8, 296, 50, 25);
    Text := 'VGap';
    TextColor := TfpgColor($FF808080);
  end;

  seVGap := TfpgSpinEdit.Create(pnlControls);
  with seVGap do
  begin
    Name := 'seVGap';
    SetPosition(72, 288, 92, 24);
    TabOrder := 6;
    Value := 5;
    OnChange := @VGapChanged;
  end;

  {@VFD_BODY_END: AdvancedFlowForm}
  {%endregion}

  // Create the FlowLayoutManager and assign it to the panel
  FlowLayout := TfpgFlowLayoutManager.Create;
  pnlFlow.LayoutManager := FlowLayout;

  // Add some buttons to the flow panel to demonstrate wrapping
  for i := 1 to 10 do
  begin
    Btn := TfpgButton.Create(pnlFlow); // Parent is now pnlFlow
    with Btn do
    begin
      Name := 'Btn' + IntToStr(i);
      Text := Format('Button %d', [i]);
      TextColor := clBlack;
      Width := 60 + (i * 5); // Vary width to force wrapping
      Height := 30;
    end;
    pnlFlow.LayoutManager.AddLayoutComponent(Btn, TfpgLayoutConstraint.Create);
  end;

//  pnlFlow.Realign;
end;

procedure TAdvancedFlowForm.HGapChanged(Sender: TObject);
begin
  FlowLayout.HGap := seHGap.Value;
  pnlFlow.Realign;
end;

procedure TAdvancedFlowForm.VGapChanged(Sender: TObject);
begin
  FlowLayout.VGap := seVGap.Value;
  pnlFlow.Realign;
end;

procedure TAdvancedFlowForm.AlignmentChanged(Sender: TObject);
begin
  case TfpgRadioButton(Sender).Tag of
    0: FlowLayout.Alignment := flaLeft;
    1: FlowLayout.Alignment := flaCenter;
    2: FlowLayout.Alignment := flaRight;
  end;
  pnlFlow.Realign;
end;

procedure TAdvancedFlowForm.VAlignmentChanged(Sender: TObject);
begin
  case TfpgRadioButton(Sender).Tag of
    0: FlowLayout.VAlignment := flvaTop;
    1: FlowLayout.VAlignment := flvaCenter;
    2: FlowLayout.VAlignment := flvaBottom;
  end;
  pnlFlow.Realign;
end;

end.
