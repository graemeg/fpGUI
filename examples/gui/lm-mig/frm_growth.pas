unit frm_growth;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_label, fpg_edit, fpg_memo,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc;

type

  TGrowthMigForm = class(TfpgForm)
  private
    FDebug: boolean;
    procedure CloseClicked(Sender: TObject);
  public
    constructor Create(AEnableDebug: Boolean); reintroduce;
    procedure AfterCreate; override;
  end;

implementation

constructor TGrowthMigForm.Create(AEnableDebug: Boolean);
begin
  inherited Create(nil);
  FDebug := AEnableDebug;
end;

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
  Left := 200;
  Top := 150;
  Width := 550;
  Height := 450;
  WindowTitle := 'MigLayout - Growth Behavior';

  // Create MigLayout with Fill to distribute extra space
  mig := TfpgMigLayoutManager.Create;
  mig.LC.WrapAfter(2);  // 2 columns
  mig.LC.Fill;             // Fill both horizontal and vertical space
  if FDebug then
    mig.LC.Debug;
  LayoutManager := mig;

  // Title
  lbl1 := TfpgLabel.Create(Self);
  lbl1.Text := 'Growth Demo - Resize window to see growth behavior';
  mig.AddLayoutComponent(lbl1, TfpgMigCC.Create().SpanX(2).AlignX('center'));

  // Row 1: Label (no grow) + Edit (grows)
  lbl2 := TfpgLabel.Create(Self);
  lbl2.Text := 'No Grow:';
  mig.AddLayoutComponent(lbl2, TfpgMigCC.Create().MinWidth('70lp'));

  btn1 := TfpgButton.Create(Self);
  btn1.Text := 'Fixed Width Button';
  btn1.Width := 150;
  btn1.Height := 30;
  mig.AddLayoutComponent(btn1, TfpgMigCC.Create());

  // Row 2: Label + Edit with GrowX
  lbl3 := TfpgLabel.Create(Self);
  lbl3.Text := 'Grows:';
  mig.AddLayoutComponent(lbl3, TfpgMigCC.Create());

  edt1 := TfpgEdit.Create(Self);
  edt1.Text := 'This edit grows horizontally';
  edt1.Width := 200;
  edt1.Height := 24;
  mig.AddLayoutComponent(edt1, TfpgMigCC.Create().GrowX());

  // Row 3: Three buttons - left fixed, middle grows, right fixed
  lbl4 := TfpgLabel.Create(Self);
  lbl4.Text := 'Mixed:';
  mig.AddLayoutComponent(lbl4, TfpgMigCC.Create());

  btn2 := TfpgButton.Create(Self);
  btn2.Text := 'Grows horizontally';
  btn2.Width := 100;
  btn2.Height := 30;
  mig.AddLayoutComponent(btn2, TfpgMigCC.Create().GrowX());

  // Row 4: Memo spanning 2 columns with both horizontal and vertical growth
  memo := TfpgMemo.Create(Self);
  memo.Width := 300;
  memo.Height := 120;
  memo.Lines.Add('This memo grows both horizontally and vertically.');
  memo.Lines.Add('');
  memo.Lines.Add('Try resizing the window to see the growth behavior.');
  memo.Lines.Add('');
  memo.Lines.Add('The memo will expand to fill available space.');
  mig.AddLayoutComponent(memo, TfpgMigCC.Create().SpanX(2).GrowX().GrowY());

  // Bottom buttons
  btn3 := TfpgButton.Create(Self);
  btn3.Text := 'Close (maxWidth=550)';
  btn3.Width := 100;
  btn3.Height := 30;
  btn3.MaxWidth := 550;
  btn3.OnClick := @CloseClicked;
  mig.AddLayoutComponent(btn3, TfpgMigCC.Create().SpanX(2).AlignX('center'));
end;

procedure TGrowthMigForm.CloseClicked(Sender: TObject);
begin
  Close;
end;

end.
