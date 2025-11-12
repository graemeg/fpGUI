program lm_mig;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_button,
  fpg_label,
  frm_basic,
  frm_alignment,
  frm_spanning,
  frm_growth,
  frm_complex;


type
  TMainForm = class(TfpgForm)
  private
    lblTitle: TfpgLabel;
    btnBasic: TfpgButton;
    btnAlignment: TfpgButton;
    btnSpanning: TfpgButton;
    btnGrowth: TfpgButton;
    btnComplex: TfpgButton;
    btnQuit: TfpgButton;
    procedure btnBasicClicked(Sender: TObject);
    procedure btnAlignmentClicked(Sender: TObject);
    procedure btnSpanningClicked(Sender: TObject);
    procedure btnGrowthClicked(Sender: TObject);
    procedure btnComplexClicked(Sender: TObject);
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
  frm := TBasicMigForm.Create(nil);
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
  frm := TAlignmentMigForm.Create(nil);
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
  frm := TSpanningMigForm.Create(nil);
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
  frm := TGrowthMigForm.Create(nil);
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
  frm := TComplexMigForm.Create(nil);
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
  SetPosition(400, 300, 280, 380);
  WindowTitle := 'MigLayout Examples';

  lblTitle := TfpgLabel.Create(self);
  with lblTitle do
  begin
    Name := 'lblTitle';
    SetPosition(20, 12, 240, 30);
    Text := 'MigLayout Demo Application';
    FontDesc := '#Label1:bold';
    Alignment := taCenter;
  end;

  btnBasic := TfpgButton.Create(self);
  with btnBasic do
  begin
    Name := 'btnBasic';
    SetPosition(20, 55, 240, 35);
    Text := 'Basic Layouts';
    FontDesc := '#Button';
    OnClick := @btnBasicClicked;
  end;

  btnAlignment := TfpgButton.Create(self);
  with btnAlignment do
  begin
    Name := 'btnAlignment';
    // Using SetPosition() to test deprecated method still works
    SetPosition(20, 95, 240, 35);
    Text := 'Cell Alignment';
    FontDesc := '#Button';
    OnClick := @btnAlignmentClicked;
  end;

  btnSpanning := TfpgButton.Create(self);
  with btnSpanning do
  begin
    Name := 'btnSpanning';
    SetPosition(20, 135, 240, 35);
    Text := 'Column & Row Spanning';
    FontDesc := '#Button';
    OnClick := @btnSpanningClicked;
  end;

  btnGrowth := TfpgButton.Create(self);
  with btnGrowth do
  begin
    Name := 'btnGrowth';
    SetPosition(20, 175, 240, 35);
    Text := 'Growth Behavior';
    FontDesc := '#Button';
    OnClick := @btnGrowthClicked;
  end;

  btnComplex := TfpgButton.Create(self);
  with btnComplex do
  begin
    Name := 'btnComplex';
    SetPosition(20, 215, 240, 35);
    Text := 'Complex Form Example';
    FontDesc := '#Button';
    OnClick := @btnComplexClicked;
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(20, 320, 240, 35);
    Text := 'Quit';
    FontDesc := '#Button';
    OnClick := @btnQuitClicked;
  end;
end;

begin
  MainProc;
end.
