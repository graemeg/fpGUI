unit frm_complex;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_label, fpg_edit,
  fpg_combobox, fpg_checkbox, fpg_memo,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc;

type

  TComplexMigForm = class(TfpgForm)
  private
    FDebug: boolean;
    procedure btnCancelClicked(Sender: TObject);
  public
    constructor Create(AEnableDebug: Boolean); reintroduce;
    procedure AfterCreate; override;
  end;

implementation

constructor TComplexMigForm.Create(AEnableDebug: Boolean);
begin
  inherited Create(nil);
  FDebug := AEnableDebug;
end;

procedure TComplexMigForm.btnCancelClicked(Sender: TObject);
begin
  Close;
end;

procedure TComplexMigForm.AfterCreate;
var
  mig: TfpgMigLayoutManager;
  lblTitle, lblName, lblEmail, lblCountry, lblComments: TfpgLabel;
  edtName, edtEmail: TfpgEdit;
  cmbCountry: TfpgComboBox;
  chkNewsletter, chkTerms: TfpgCheckBox;
  memoComments: TfpgMemo;
  btnOK, btnCancel: TfpgButton;
begin
  inherited AfterCreate;
  Name := 'ComplexMigForm';
  Left := 200;
  Top := 100;
  Width := 550;
  Height := 500;
  WindowTitle := 'MigLayout - Complex Form Example';

  // Create MigLayout with 4 columns for flexible layout
  mig := TfpgMigLayoutManager.Create;
  mig.LC.SetWrapAfter(4);  // 4 columns
  mig.LC.Fill;             // Fill both horizontal and vertical space
  if FDebug then
    mig.LC.Debug;
  LayoutManager := mig;

  // Title spanning all columns
  lblTitle := TfpgLabel.Create(Self);
  lblTitle.Text := 'User Registration Form';
  lblTitle.Width := 200;
  lblTitle.Height := 24;
  lblTitle.FontDesc := '#Label1:bold';
  mig.AddLayoutComponent(lblTitle, TfpgMigCC.Create().SpanX(4).AlignX('center'));

  // Name field (label + edit spanning 3 columns)
  lblName := TfpgLabel.Create(Self);
  lblName.Text := 'Full Name:';
  lblName.Width := 80;
  lblName.Height := 20;
  mig.AddLayoutComponent(lblName, TfpgMigCC.Create());

  edtName := TfpgEdit.Create(Self);
  edtName.Width := 250;
  edtName.Height := 24;
  mig.AddLayoutComponent(edtName, TfpgMigCC.Create().SpanX(3).GrowX());

  // Email field (label + edit spanning 3 columns)
  lblEmail := TfpgLabel.Create(Self);
  lblEmail.Text := 'Email:';
  lblEmail.Width := 80;
  lblEmail.Height := 20;
  mig.AddLayoutComponent(lblEmail, TfpgMigCC.Create());

  edtEmail := TfpgEdit.Create(Self);
  edtEmail.Width := 250;
  edtEmail.Height := 24;
  mig.AddLayoutComponent(edtEmail, TfpgMigCC.Create().SpanX(3).GrowX());

  // Country combobox (label + combo + space)
  lblCountry := TfpgLabel.Create(Self);
  lblCountry.Text := 'Country:';
  lblCountry.Width := 80;
  lblCountry.Height := 20;
  mig.AddLayoutComponent(lblCountry, TfpgMigCC.Create());

  cmbCountry := TfpgComboBox.Create(Self);
  cmbCountry.Width := 180;
  cmbCountry.Height := 24;
  cmbCountry.Items.Add('United Kingdom');
  cmbCountry.Items.Add('United States');
  cmbCountry.Items.Add('Canada');
  cmbCountry.Items.Add('Australia');
  cmbCountry.Items.Add('Germany');
  cmbCountry.Items.Add('France');
  cmbCountry.FocusItem := 0;
  mig.AddLayoutComponent(cmbCountry, TfpgMigCC.Create().SpanX(2));

  // Empty cell for spacing
  lblComments := TfpgLabel.Create(Self);
  lblComments.Text := '';
  lblComments.Width := 1;
  lblComments.Height := 1;
  mig.AddLayoutComponent(lblComments, TfpgMigCC.Create());

  // Checkboxes spanning columns
  chkNewsletter := TfpgCheckBox.Create(Self);
  chkNewsletter.Text := 'Subscribe to newsletter';
  chkNewsletter.Width := 180;
  chkNewsletter.Height := 20;
  mig.AddLayoutComponent(chkNewsletter, TfpgMigCC.Create().SpanX(4));

  chkTerms := TfpgCheckBox.Create(Self);
  chkTerms.Text := 'I agree to the terms and conditions';
  chkTerms.Width := 220;
  chkTerms.Height := 20;
  mig.AddLayoutComponent(chkTerms, TfpgMigCC.Create().SpanX(4));

  // Comments section (label above memo)
  lblComments := TfpgLabel.Create(Self);
  lblComments.Text := 'Comments:';
  lblComments.Width := 80;
  lblComments.Height := 20;
  mig.AddLayoutComponent(lblComments, TfpgMigCC.Create().SpanX(4));

  // Memo spanning all columns with grow
  memoComments := TfpgMemo.Create(Self);
  memoComments.Width := 400;
  memoComments.Height := 100;
  memoComments.Lines.Add('Enter any additional comments here...');
  mig.AddLayoutComponent(memoComments, TfpgMigCC.Create().SpanX(4).GrowX().GrowY());

  // Button row at bottom right
  // Use SpanX to span all columns, then align right with gap
  btnOK := TfpgButton.Create(Self);
  btnOK.Text := 'Submit';
  btnOK.Width := 90;
  btnOK.Height := 30;
  mig.AddLayoutComponent(btnOK, TfpgMigCC.Create().SpanX(3).AlignX('right').Tag('buttons'));

  btnCancel := TfpgButton.Create(Self);
  btnCancel.Text := 'Cancel';
  btnCancel.Width := 90;
  btnCancel.Height := 30;
  btnCancel.OnClick := @btnCancelClicked;
  mig.AddLayoutComponent(btnCancel, TfpgMigCC.Create().AlignX('right').Tag('buttons'));
end;

end.
