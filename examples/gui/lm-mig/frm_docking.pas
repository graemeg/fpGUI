unit frm_docking;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_button,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc;

type

  { TDockingForm }

  TDockingForm = class(TfpgForm)
  private
    FDebug: boolean;
  public
    constructor Create(AEnableDebug: Boolean); reintroduce;
    procedure AfterCreate; override;
  end;

implementation

{ TDockingForm }

constructor TDockingForm.Create(AEnableDebug: Boolean);
begin
  inherited Create(nil);
  FDebug := AEnableDebug;
end;

procedure TDockingForm.AfterCreate;
var
  mig: TfpgMigLayoutManager;
  btn1, btn2, btn3, btn4: TfpgButton;
  btn1N, btn2W, btn3S, btn4E: TfpgButton;
begin
  inherited AfterCreate;
  Name := 'AlignmentMigForm';
  PreferredSize := fpgSize(500, 400);
  WindowPosition := wpOneThirdDown;
  WindowTitle := 'MigLayout - Docking Components';

  // Create MigLayout with Fill to make cells fill container
  mig := TfpgMigLayoutManager.Create;
  mig.LC.Fill;             // Cells fill available space
  if FDebug then
    mig.LC.Debug;
  LayoutManager := mig;


  btn1 := TfpgButton.Create(Self);
  btn1.Name := 'btn1';
  btn1.Text := 'btn1';
  btn1.PreferredSize := fpgSize(80, 24);

  btn2 := TfpgButton.Create(Self);
  btn2.Name := 'btn2';
  btn2.Text := 'btn2';
  btn2.PreferredSize := fpgSize(80, 24);

  btn3 := TfpgButton.Create(Self);
  btn3.Name := 'btn3';
  btn3.Text := 'btn3';
  btn3.PreferredSize := fpgSize(80, 24);

  btn4 := TfpgButton.Create(Self);
  btn4.Name := 'btn4';
  btn4.Text := 'btn4';
  btn4.PreferredSize := fpgSize(80, 24);

  btn1N := TfpgButton.Create(Self);
  btn1N.Name := 'btn1N';
  btn1N.Text := 'btn1N';
  btn1N.PreferredSize := fpgSize(80, 24);

  btn2W := TfpgButton.Create(Self);
  btn2W.Name := 'btn2W';
  btn2W.Text := 'btn2W';
  btn2W.PreferredSize := fpgSize(80, 24);

  btn3S := TfpgButton.Create(Self);
  btn3S.Name := 'btn3S';
  btn3S.Text := 'btn3S';
  btn3S.PreferredSize := fpgSize(80, 24);

  btn4E := TfpgButton.Create(Self);
  btn4E.Name := 'btn4E';
  btn4E.Text := 'btn4E';
  btn4E.PreferredSize := fpgSize(80, 24);


  mig.AddLayoutComponent(btn1, TfpgMigCC.Create());
  mig.AddLayoutComponent(btn2, TfpgMigCC.Create());
  mig.AddLayoutComponent(btn3, TfpgMigCC.Create().Wrap);
  mig.AddLayoutComponent(btn4, TfpgMigCC.Create());

  mig.AddLayoutComponent(btn1N, TfpgMigCC.Create().DockNorth);
  mig.AddLayoutComponent(btn2W, TfpgMigCC.Create().DockWest);
  mig.AddLayoutComponent(btn3S, TfpgMigCC.Create().DockSouth);
  mig.AddLayoutComponent(btn4E, TfpgMigCC.Create().DockEast);

end;

end.

