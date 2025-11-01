unit frm_basic;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_label, fpg_edit,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc;

type

  TBasicMigForm = class(TfpgForm)
  public
    procedure AfterCreate; override;
  end;

implementation

procedure TBasicMigForm.AfterCreate;
var
  mig: TfpgMigLayoutManager;
  lbl1, lbl2, lbl3: TfpgLabel;
  edt1, edt2, edt3: TfpgEdit;
  btn1, btn2: TfpgButton;
begin
  inherited AfterCreate;
  Name := 'BasicMigForm';
  SetPosition(300, 200, 450, 250);
  WindowTitle := 'MigLayout - Basic Layouts';

  // Create MigLayout with 2-column grid
  mig := TfpgMigLayoutManager.Create;
  mig.LC.SetWrapAfter(2);  // Wrap after 2 components (2 columns)
  mig.LC.Debug(500);       // Enable debug visualization
  LayoutManager := mig;

  // Row 1: Label + Edit
  lbl1 := TfpgLabel.Create(Self);
  lbl1.Name := 'lbl1';
  lbl1.Text := 'Name:';
  lbl1.Width := 80;
  lbl1.Height := 20;
  mig.AddLayoutComponent(lbl1, TfpgMigCC.Create());

  edt1 := TfpgEdit.Create(Self);
  edt1.Name := 'edt1';
  edt1.Text := '';
  edt1.Width := 200;
  edt1.Height := 24;
  mig.AddLayoutComponent(edt1, TfpgMigCC.Create().GrowX().AlignX('fill'));

  // Row 2: Label + Edit
  lbl2 := TfpgLabel.Create(Self);
  lbl2.Name := 'lbl2';
  lbl2.Text := 'Email:';
  lbl2.Width := 80;
  lbl2.Height := 20;
  mig.AddLayoutComponent(lbl2, TfpgMigCC.Create());

  edt2 := TfpgEdit.Create(Self);
  edt2.Name := 'edt2';
  edt2.Text := '';
  edt2.Width := 200;
  edt2.Height := 24;
  mig.AddLayoutComponent(edt2, TfpgMigCC.Create().GrowX().AlignX('fill'));

  // Row 3: Label + Edit
  lbl3 := TfpgLabel.Create(Self);
  lbl3.Name := 'lbl3';
  lbl3.Text := 'Phone:';
  lbl3.Width := 80;
  lbl3.Height := 20;
  mig.AddLayoutComponent(lbl3, TfpgMigCC.Create());

  edt3 := TfpgEdit.Create(Self);
  edt3.Name := 'edt3';
  edt3.Text := '';
  edt3.Width := 200;
  edt3.Height := 24;
  mig.AddLayoutComponent(edt3, TfpgMigCC.Create().GrowX().AlignX('fill'));

  // Row 4: Buttons (spanning 2 columns, right-aligned)
  btn1 := TfpgButton.Create(Self);
  btn1.Name := 'btn1';
  btn1.Text := 'OK';
  btn1.Width := 80;
  btn1.Height := 30;
  mig.AddLayoutComponent(btn1, TfpgMigCC.Create().Tag('buttons'));

  btn2 := TfpgButton.Create(Self);
  btn2.Name := 'btn2';
  btn2.Text := 'Cancel';
  btn2.Width := 80;
  btn2.Height := 30;
  mig.AddLayoutComponent(btn2, TfpgMigCC.Create().Tag('buttons'));
end;

end.
