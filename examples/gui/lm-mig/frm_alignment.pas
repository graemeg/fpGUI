unit frm_alignment;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_label,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc;

type

  TAlignmentMigForm = class(TfpgForm)
  private
    FDebug: boolean;
  public
    constructor Create(AEnableDebug: Boolean); reintroduce;
    procedure AfterCreate; override;
  end;

implementation

constructor TAlignmentMigForm.Create(AEnableDebug: Boolean);
begin
  inherited Create(nil);
  FDebug := AEnableDebug;
end;

procedure TAlignmentMigForm.AfterCreate;
var
  mig: TfpgMigLayoutManager;
  btnTopLeft, btnTopCenter, btnTopRight: TfpgButton;
  btnMiddleLeft, btnMiddleCenter, btnMiddleRight: TfpgButton;
  btnBottomLeft, btnBottomCenter, btnBottomRight: TfpgButton;
  lbl: TfpgLabel;
begin
  inherited AfterCreate;
  Name := 'AlignmentMigForm';
  SetPosition(250, 150, 500, 400);
  WindowTitle := 'MigLayout - Cell Alignment';

  // Create MigLayout with Fill to make cells fill container
  mig := TfpgMigLayoutManager.Create;
  mig.LC.WrapAfter(3);  // 3 columns
  mig.LC.Fill;             // Cells fill available space
  if FDebug then
    mig.LC.Debug;
  LayoutManager := mig;

  // Title
  lbl := TfpgLabel.Create(Self);
  lbl.Text := 'Alignment Demo - 9 buttons in 3x3 grid';
  lbl.Width := 300;
  lbl.Height := 20;
  mig.AddLayoutComponent(lbl, TfpgMigCC.Create().SpanX(3).AlignX('center'));

  // Top row
  btnTopLeft := TfpgButton.Create(Self);
  btnTopLeft.Text := 'Top Left';
  btnTopLeft.Width := 80;
  btnTopLeft.Height := 30;
  mig.AddLayoutComponent(btnTopLeft, TfpgMigCC.Create().AlignX('left').AlignY('top'));

  btnTopCenter := TfpgButton.Create(Self);
  btnTopCenter.Text := 'Top Center';
  btnTopCenter.Width := 80;
  btnTopCenter.Height := 30;
  mig.AddLayoutComponent(btnTopCenter, TfpgMigCC.Create().AlignX('center').AlignY('top'));

  btnTopRight := TfpgButton.Create(Self);
  btnTopRight.Text := 'Top Right';
  btnTopRight.Width := 80;
  btnTopRight.Height := 30;
  mig.AddLayoutComponent(btnTopRight, TfpgMigCC.Create().AlignX('right').AlignY('top'));

  // Middle row
  btnMiddleLeft := TfpgButton.Create(Self);
  btnMiddleLeft.Text := 'Middle Left';
  btnMiddleLeft.Width := 90;
  btnMiddleLeft.Height := 30;
  mig.AddLayoutComponent(btnMiddleLeft, TfpgMigCC.Create().AlignX('left').AlignY('center'));

  btnMiddleCenter := TfpgButton.Create(Self);
  btnMiddleCenter.Text := 'Center';
  btnMiddleCenter.Width := 80;
  btnMiddleCenter.Height := 30;
  mig.AddLayoutComponent(btnMiddleCenter, TfpgMigCC.Create().AlignX('center').AlignY('center'));

  btnMiddleRight := TfpgButton.Create(Self);
  btnMiddleRight.Text := 'Middle Right';
  btnMiddleRight.Width := 90;
  btnMiddleRight.Height := 30;
  mig.AddLayoutComponent(btnMiddleRight, TfpgMigCC.Create().AlignX('right').AlignY('center'));

  // Bottom row
  btnBottomLeft := TfpgButton.Create(Self);
  btnBottomLeft.Text := 'Bottom Left';
  btnBottomLeft.Width := 90;
  btnBottomLeft.Height := 30;
  mig.AddLayoutComponent(btnBottomLeft, TfpgMigCC.Create().AlignX('left').AlignY('bottom'));

  btnBottomCenter := TfpgButton.Create(Self);
  btnBottomCenter.Text := 'Bottom Center';
  btnBottomCenter.Width := 100;
  btnBottomCenter.Height := 30;
  mig.AddLayoutComponent(btnBottomCenter, TfpgMigCC.Create().AlignX('center').AlignY('bottom'));

  btnBottomRight := TfpgButton.Create(Self);
  btnBottomRight.Text := 'Bottom Right';
  btnBottomRight.Width := 100;
  btnBottomRight.Height := 30;
  mig.AddLayoutComponent(btnBottomRight, TfpgMigCC.Create().AlignX('right').AlignY('bottom'));
end;

end.
