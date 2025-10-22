program lm_border;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_panel,
  fpg_layoutmanager, fpg_borderlayout;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
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

procedure TMainForm.AfterCreate;
var
  pnl: TfpgPanel;
  btn: TfpgButton;
  lm: TfpgBorderLayoutManager;
  constraint: TfpgBorderLayoutConstraint;
begin
  WindowTitle := 'BorderLayout Example';
  SetPosition(100, 100, 400, 300);
  BackgroundColor := clMoneyGreen;

  lm := TfpgBorderLayoutManager.Create(10, 5);
  Self.LayoutManager := lm;

  // North
  btn := TfpgButton.Create(Self);
  btn.Text := 'North';
  constraint := TfpgBorderLayoutConstraint.Create;
  constraint.Region := blrNorth;
  lm.AddLayoutComponent(btn, constraint);

  // South
  btn := TfpgButton.Create(Self);
  btn.Text := 'South';
  constraint := TfpgBorderLayoutConstraint.Create;
  constraint.Region := blrSouth;
  lm.AddLayoutComponent(btn, constraint);

  // East
  btn := TfpgButton.Create(Self);
  btn.Text := 'East';
  constraint := TfpgBorderLayoutConstraint.Create;
  constraint.Region := blrEast;
  lm.AddLayoutComponent(btn, constraint);

  // West
  btn := TfpgButton.Create(Self);
  btn.Text := 'West';
  constraint := TfpgBorderLayoutConstraint.Create;
  constraint.Region := blrWest;
  lm.AddLayoutComponent(btn, constraint);

  // Center
  pnl := TfpgPanel.Create(Self);
  pnl.BackgroundColor := clLightBlue;
  constraint := TfpgBorderLayoutConstraint.Create;
  constraint.Region := blrCenter;
  lm.AddLayoutComponent(pnl, constraint);

end;

begin
  MainProc;
end.
