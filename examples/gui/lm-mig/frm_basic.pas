unit frm_basic;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_label, fpg_edit,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc;

type

  TBasicMigForm = class(TfpgForm)
  private
    FDebug: boolean;
  public
    lbl1, lbl2, lbl3: TfpgLabel;
    edt1, edt2, edt3: TfpgEdit;
    btn1, btn2: TfpgButton;
    constructor Create(AEnableDebug: Boolean); reintroduce;
    procedure AfterCreate; override;
  end;

implementation

constructor TBasicMigForm.Create(AEnableDebug: Boolean);
begin
  inherited Create(nil);
  FDebug := AEnableDebug;
end;

procedure TBasicMigForm.AfterCreate;
var
  mig: TfpgMigLayoutManager;
begin
  inherited AfterCreate;
  Name := 'BasicMigForm';
  Left := 300;
  Top := 200;
  Width := 450;
  Height := 250;
  WindowTitle := 'MigLayout - Basic Layouts';

  // Create MigLayout with 2-column grid
  mig := TfpgMigLayoutManager.Create;
  mig.LC.SetWrapAfter(2);  // Wrap after 2 components (2 columns)
  if FDebug then
    mig.LC.Debug(500);       // Enable debug visualization
  LayoutManager := mig;

  // Row 1: Label + Edit
  lbl1 := TfpgLabel.Create(Self);
  lbl1.Name := 'lbl1';
  lbl1.Text := 'Name:';
//  lbl1.PreferredSize := fpgSize(80, 20);
  mig.AddLayoutComponent(lbl1, TfpgMigCC.Create());

  edt1 := TfpgEdit.Create(Self);
  edt1.Name := 'edt1';
  edt1.Text := '';
  edt1.PreferredSize := fpgSize(200, 24);
  mig.AddLayoutComponent(edt1, TfpgMigCC.Create().GrowX());

  lbl2 := TfpgLabel.Create(Self);
  lbl2.Name := 'lbl2';
  lbl2.Text := 'Email:';
//  lbl2.PreferredSize := fpgSize(80, 20);
  mig.AddLayoutComponent(lbl2, TfpgMigCC.Create());

  edt2 := TfpgEdit.Create(Self);
  edt2.Name := 'edt2';
  edt2.Text := '';
  edt2.PreferredSize := fpgSize(200, 24);
  mig.AddLayoutComponent(edt2, TfpgMigCC.Create().GrowX());

  lbl3 := TfpgLabel.Create(Self);
  lbl3.Name := 'lbl3';
  lbl3.Text := 'Phone:';
//  lbl3.PreferredSize := fpgSize(80, 20);
  mig.AddLayoutComponent(lbl3, TfpgMigCC.Create());

  edt3 := TfpgEdit.Create(Self);
  edt3.Name := 'edt3';
  edt3.Text := '';
  edt3.PreferredSize := fpgSize(200, 24);
  mig.AddLayoutComponent(edt3, TfpgMigCC.Create().GrowX());

  // Row 4: Buttons (spanning 2 columns, right-aligned)
  btn1 := TfpgButton.Create(Self);
  btn1.Name := 'btn1';
  btn1.Text := 'OK';
  btn1.PreferredSize := fpgSize(80, 24);
  mig.AddLayoutComponent(btn1, TfpgMigCC.Create().SpanX.Split(2).Tag('ok'));

  btn2 := TfpgButton.Create(Self);
  btn2.Name := 'btn2';
  btn2.Text := 'Cancel';
  btn2.PreferredSize := fpgSize(80, 24);
  mig.AddLayoutComponent(btn2, TfpgMigCC.Create().Tag('cancel'));
end;

end.
