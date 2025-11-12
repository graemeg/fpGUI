program aligntest;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_button,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc,
  frm_basic,
  frm_nested;


type
  TMainForm = class(TfpgForm)
  private
    btnBasic: TfpgButton;
    btnNested: TfpgButton;
    btnQuit: TfpgButton;
    procedure btnBasicClicked(Sender: TObject);
    procedure btnNestedClicked(Sender: TObject);
    procedure btnQuitClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

procedure TMainForm.btnBasicClicked(Sender: TObject);
var
  frm: TBasicAlignments;
begin
  frm := TBasicAlignments.Create(nil);
  frm.ShowModal;
  frm.Free;
end;

procedure TMainForm.btnNestedClicked(Sender: TObject);
var
  frm: TNestedAlignment;
begin
  frm := TNestedAlignment.Create(nil);
  frm.ShowModal;
  frm.Free;
end;

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AfterCreate;
var
  mig: TfpgMigLayoutManager;
begin
  inherited AfterCreate;
  Name := 'MainForm';
  Width := 175;
  Height := 234;
  WindowPosition := wpOneThirdDown;
  WindowTitle := 'Alignment Example';

  mig := TfpgMigLayoutManager.Create;
  mig.LC.SetWrapAfter(1);
  LayoutManager := mig;

  btnBasic := TfpgButton.Create(self);
  with btnBasic do
  begin
    Name := 'btnBasic';
    Text := 'Basic Alignment';
    PreferredSize := fpgSize(150, 35);
    OnClick := @btnBasicClicked;
  end;
  mig.AddLayoutComponent(btnBasic, TfpgMigCC.Create().GrowX().AlignX('fill'));

  btnNested := TfpgButton.Create(self);
  with btnNested do
  begin
    Name := 'btnNested';
    Text := 'Nested Alignments';
    PreferredSize := fpgSize(150, 35);
    OnClick := @btnNestedClicked;
  end;
  mig.AddLayoutComponent(btnNested, TfpgMigCC.Create().GrowX().AlignX('fill'));

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    Text := 'Quit';
    PreferredSize := fpgSize(150, 35);
    FontDesc := 'Liberation Sans-10:antialias=true';
    OnClick := @btnQuitClicked;
  end;
  mig.AddLayoutComponent(btnQuit, TfpgMigCC.Create().GrowX().AlignX('fill'));

end;


procedure MainProc;
var
  frm : TMainForm;
begin
  fpgApplication.Initialize;

  frm := TMainForm.Create(nil);
  frm.Show;

  fpgApplication.Run;
  frm.Free;
end;

begin
  MainProc;
end.

