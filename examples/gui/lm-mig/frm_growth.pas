unit frm_growth;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_label, fpg_edit, fpg_memo,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc;

type

  TGrowthMigForm = class(TfpgForm)
  public
    procedure AfterCreate; override;
  end;

implementation

procedure TGrowthMigForm.AfterCreate;
var
  mig: TfpgMigLayoutManager;
  lbl1, lbl2, lbl3, lbl4: TfpgLabel;
  edt1, edt2: TfpgEdit;
  btn1, btn2, btn3: TfpgButton;
  memo: TfpgMemo;
begin
  inherited AfterCreate;
  Name := 'GrowthMigForm';
  SetPosition(200, 150, 550, 450);
  WindowTitle := 'MigLayout - Growth Behavior';

  // Create MigLayout with Fill to distribute extra space
  mig := TfpgMigLayoutManager.Create;
  mig.LC.SetWrapAfter(2);  // 2 columns
  mig.LC.Fill;             // Fill both horizontal and vertical space
  mig.LC.Debug(500);
  LayoutManager := mig;

  // Title
  lbl1 := TfpgLabel.Create(Self);
  lbl1.Text := 'Growth Demo - Resize window to see growth behavior';
  lbl1.Width := 350;
  lbl1.Height := 20;
  mig.AddLayoutComponent(lbl1, TfpgMigCC.Create().SpanX(2).AlignX('center'));

  // Row 1: Label (no grow) + Edit (grows)
  lbl2 := TfpgLabel.Create(Self);
  lbl2.Text := 'No Grow:';
  lbl2.Width := 80;
  lbl2.Height := 20;
  mig.AddLayoutComponent(lbl2, TfpgMigCC.Create());

  btn1 := TfpgButton.Create(Self);
  btn1.Text := 'Fixed Width Button';
  btn1.Width := 150;
  btn1.Height := 30;
  mig.AddLayoutComponent(btn1, TfpgMigCC.Create());

  // Row 2: Label + Edit with GrowX
  lbl3 := TfpgLabel.Create(Self);
  lbl3.Text := 'Grows:';
  lbl3.Width := 80;
  lbl3.Height := 20;
  mig.AddLayoutComponent(lbl3, TfpgMigCC.Create());

  edt1 := TfpgEdit.Create(Self);
  edt1.Text := 'This edit grows horizontally';
  edt1.Width := 200;
  edt1.Height := 24;
  mig.AddLayoutComponent(edt1, TfpgMigCC.Create().GrowX().AlignX('fill'));

  // Row 3: Three buttons - left fixed, middle grows, right fixed
  lbl4 := TfpgLabel.Create(Self);
  lbl4.Text := 'Mixed:';
  lbl4.Width := 80;
  lbl4.Height := 20;
  mig.AddLayoutComponent(lbl4, TfpgMigCC.Create());

  btn2 := TfpgButton.Create(Self);
  btn2.Text := 'Grows with fill';
  btn2.Width := 100;
  btn2.Height := 30;
  mig.AddLayoutComponent(btn2, TfpgMigCC.Create().GrowX().AlignX('fill'));

  // Row 4: Memo spanning 2 columns with both horizontal and vertical growth
  memo := TfpgMemo.Create(Self);
  memo.Width := 300;
  memo.Height := 120;
  memo.Lines.Add('This memo grows both horizontally and vertically.');
  memo.Lines.Add('');
  memo.Lines.Add('Try resizing the window to see the growth behavior.');
  memo.Lines.Add('');
  memo.Lines.Add('The memo will expand to fill available space.');
  mig.AddLayoutComponent(memo, TfpgMigCC.Create().SpanX(2).GrowX().GrowY().AlignX('fill').AlignY('fill'));

  // Bottom buttons
  btn3 := TfpgButton.Create(Self);
  btn3.Text := 'Close';
  btn3.Width := 100;
  btn3.Height := 30;
//  btn3.OnClick := @Close;
  mig.AddLayoutComponent(btn3, TfpgMigCC.Create().SpanX(2).AlignX('right'));
end;

end.
