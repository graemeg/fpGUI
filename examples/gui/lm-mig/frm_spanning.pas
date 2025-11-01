unit frm_spanning;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_label, fpg_memo,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc;

type

  TSpanningMigForm = class(TfpgForm)
  public
    procedure AfterCreate; override;
  end;

implementation

procedure TSpanningMigForm.AfterCreate;
var
  mig: TfpgMigLayoutManager;
  lbl: TfpgLabel;
  btnSpan2Cols, btn1, btn2, btn3: TfpgButton;
  btnSpan2Rows, btn4, btn5: TfpgButton;
  memo: TfpgMemo;
begin
  inherited AfterCreate;
  Name := 'SpanningMigForm';
  SetPosition(250, 150, 550, 450);
  WindowTitle := 'MigLayout - Column and Row Spanning';

  // Create MigLayout with 3 columns
  mig := TfpgMigLayoutManager.Create;
  mig.LC.SetWrapAfter(3);  // 3 columns
  mig.LC.Debug(500);
  LayoutManager := mig;

  // Title
  lbl := TfpgLabel.Create(Self);
  lbl.Text := 'Column Spanning Demo';
  lbl.Width := 200;
  lbl.Height := 20;
  mig.AddLayoutComponent(lbl, TfpgMigCC.Create().SpanX(3).AlignX('center'));

  // Row 1: Button spanning 2 columns
  btnSpan2Cols := TfpgButton.Create(Self);
  btnSpan2Cols.Text := 'This button spans 2 columns';
  btnSpan2Cols.Width := 220;
  btnSpan2Cols.Height := 30;
  mig.AddLayoutComponent(btnSpan2Cols, TfpgMigCC.Create().SpanX(2));

  btn1 := TfpgButton.Create(Self);
  btn1.Text := 'Button 1';
  btn1.Width := 80;
  btn1.Height := 30;
  mig.AddLayoutComponent(btn1, TfpgMigCC.Create());

  // Row 2: Three normal buttons
  btn2 := TfpgButton.Create(Self);
  btn2.Text := 'Button 2';
  btn2.Width := 80;
  btn2.Height := 30;
  mig.AddLayoutComponent(btn2, TfpgMigCC.Create());

  btn3 := TfpgButton.Create(Self);
  btn3.Text := 'Button 3';
  btn3.Width := 80;
  btn3.Height := 30;
  mig.AddLayoutComponent(btn3, TfpgMigCC.Create());

  btn4 := TfpgButton.Create(Self);
  btn4.Text := 'Button 4';
  btn4.Width := 80;
  btn4.Height := 30;
  mig.AddLayoutComponent(btn4, TfpgMigCC.Create());

  // Section separator
  lbl := TfpgLabel.Create(Self);
  lbl.Text := 'Row Spanning Demo';
  lbl.Width := 200;
  lbl.Height := 20;
  mig.AddLayoutComponent(lbl, TfpgMigCC.Create().SpanX(3).AlignX('center'));

  // Row spanning: Button spanning 2 rows in first column
  btnSpan2Rows := TfpgButton.Create(Self);
  btnSpan2Rows.Text := 'Spans 2 rows';
  btnSpan2Rows.Width := 100;
  btnSpan2Rows.Height := 80;
  mig.AddLayoutComponent(btnSpan2Rows, TfpgMigCC.Create().SpanY(2));

  // Two buttons next to the spanning button
  btn5 := TfpgButton.Create(Self);
  btn5.Text := 'Button 5';
  btn5.Width := 80;
  btn5.Height := 30;
  mig.AddLayoutComponent(btn5, TfpgMigCC.Create());

  memo := TfpgMemo.Create(Self);
  memo.Width := 120;
  memo.Height := 30;
  memo.Lines.Add('Memo widget');
  mig.AddLayoutComponent(memo, TfpgMigCC.Create());

  // Next row - should skip occupied cell
  lbl := TfpgLabel.Create(Self);
  lbl.Text := 'Next row auto-positioned';
  lbl.Width := 150;
  lbl.Height := 20;
  mig.AddLayoutComponent(lbl, TfpgMigCC.Create().SpanX(2));
end;

end.
