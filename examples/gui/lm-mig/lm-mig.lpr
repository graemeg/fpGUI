program lm_mig;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_checkbox,
  fpg_radiobutton,
  fpg_form,
  fpg_button,
  fpg_label,
  frm_basic,
  frm_alignment,
  frm_spanning,
  frm_growth,
  frm_complex,
  frm_parser,
  frm_buttonorder;


type
  TMainForm = class(TfpgForm)
  private
    lblTitle: TfpgLabel;
    btnBasic: TfpgButton;
    btnAlignment: TfpgButton;
    btnSpanning: TfpgButton;
    btnGrowth: TfpgButton;
    btnComplex: TfpgButton;
    btnParser: TfpgButton;
    btnButtonOrder: TfpgButton;
    btnQuit: TfpgButton;
    chkDebug: TfpgCheckBox;
    procedure btnBasicClicked(Sender: TObject);
    procedure btnAlignmentClicked(Sender: TObject);
    procedure btnSpanningClicked(Sender: TObject);
    procedure btnGrowthClicked(Sender: TObject);
    procedure btnComplexClicked(Sender: TObject);
    procedure btnParserClicked(Sender: TObject);
    procedure btnButtonOrderClicked(Sender: TObject);
    procedure btnQuitClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

procedure TMainForm.btnBasicClicked(Sender: TObject);
var
  frm: TBasicMigForm;
begin
  frm := TBasicMigForm.Create(chkDebug.Checked);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TMainForm.btnAlignmentClicked(Sender: TObject);
var
  frm: TAlignmentMigForm;
begin
  frm := TAlignmentMigForm.Create(chkDebug.Checked);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TMainForm.btnSpanningClicked(Sender: TObject);
var
  frm: TSpanningMigForm;
begin
  frm := TSpanningMigForm.Create(chkDebug.Checked);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TMainForm.btnGrowthClicked(Sender: TObject);
var
  frm: TGrowthMigForm;
begin
  frm := TGrowthMigForm.Create(chkDebug.Checked);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TMainForm.btnComplexClicked(Sender: TObject);
var
  frm: TComplexMigForm;
begin
  frm := TComplexMigForm.Create(chkDebug.Checked);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TMainForm.btnParserClicked(Sender: TObject);
var
  frm: TParserMigForm;
begin
  frm := TParserMigForm.Create(chkDebug.Checked);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TMainForm.btnButtonOrderClicked(Sender: TObject);
var
  frm: TButtonOrderForm;
begin
  frm := TButtonOrderForm.Create(chkDebug.Checked);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AfterCreate;
begin
  inherited AfterCreate;
  Name := 'MainForm';
  Left := 400;
  Top := 300;
  Width := 280;
  Height := 460;
  WindowTitle := 'MigLayout Examples';

  lblTitle := TfpgLabel.Create(self);
  with lblTitle do
  begin
    Name := 'lblTitle';
    Left := 20;
    Top := 12;
    Width := 240;
    Height := 30;
    Text := 'MigLayout Demo Application';
    FontDesc := '#Label2';
    Alignment := taCenter;
  end;

  btnBasic := TfpgButton.Create(self);
  with btnBasic do
  begin
    Name := 'btnBasic';
    Left := 20;
    Top := 55;
    Width := 240;
    Height := 35;
    Text := 'Basic Layouts';
    FontDesc := '#Label1';
    OnClick := @btnBasicClicked;
  end;

  btnAlignment := TfpgButton.Create(self);
  with btnAlignment do
  begin
    Name := 'btnAlignment';
    // Using SetPosition() to test deprecated method still works
    SetPosition(20, 95, 240, 35);
    Text := 'Cell Alignment';
    FontDesc := '#Label1';
    OnClick := @btnAlignmentClicked;
  end;

  btnSpanning := TfpgButton.Create(self);
  with btnSpanning do
  begin
    Name := 'btnSpanning';
    Left := 20;
    Top := 135;
    Width := 240;
    Height := 35;
    Text := 'Column & Row Spanning';
    FontDesc := '#Label1';
    OnClick := @btnSpanningClicked;
  end;

  btnGrowth := TfpgButton.Create(self);
  with btnGrowth do
  begin
    Name := 'btnGrowth';
    Left := 20;
    Top := 175;
    Width := 240;
    Height := 35;
    Text := 'Growth Behavior';
    FontDesc := '#Label1';
    OnClick := @btnGrowthClicked;
  end;

  btnComplex := TfpgButton.Create(self);
  with btnComplex do
  begin
    Name := 'btnComplex';
    Left := 20;
    Top := 215;
    Width := 240;
    Height := 35;
    Text := 'Complex Form Example';
    FontDesc := '#Label1';
    OnClick := @btnComplexClicked;
  end;

  btnParser := TfpgButton.Create(self);
  with btnParser do
  begin
    Name := 'btnParser';
    Left := 20;
    Top := 255;
    Width := 240;
    Height := 35;
    Text := 'String Parser Examples';
    FontDesc := '#Label1';
    OnClick := @btnParserClicked;
  end;

  btnButtonOrder := TfpgButton.Create(self);
  with btnButtonOrder do
  begin
    Name := 'btnButtonOrder';
    Left := 20;
    Top := 295;
    Width := 240;
    Height := 35;
    Text := 'Platform Button Ordering';
    FontDesc := '#Label1';
    OnClick := @btnButtonOrderClicked;
  end;

  chkDebug := TfpgCheckBox.Create(Self);
  chkDebug.Left := 20;
  chkDebug.Top := 370;
  chkDebug.Text := 'Enable layout debugging';
  chkDebug.Width := 220;
  chkDebug.Height := 20;


  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    Left := 20;
    Top := 400;
    Width := 240;
    Height := 35;
    Text := 'Quit';
    FontDesc := '#Label1';
    OnClick := @btnQuitClicked;
  end;
end;

begin
  MainProc;
end.
