program lm_flow;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_button,
  fpg_flowlayout,
  fpg_layouttypes;

type
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
  FlowLayout: ILayoutManager;
  Btn: TfpgButton;
  i: Integer;
begin
  WindowTitle := 'Flow Layout Example';
  Name := 'TMainForm';
  SetPosition(100, 100, 300, 250);

  // Create the FlowLayoutManager and assign it to the form
  FlowLayout := TfpgFlowLayoutManager.Create as ILayoutManager;
  LayoutManager := FlowLayout;

  // Add some buttons to demonstrate wrapping
  for i := 1 to 10 do
  begin
    Btn := TfpgButton.Create(Self);
    Btn.Name := 'Btn' + IntToStr(i);
    Btn.Text := Format('Button %d', [i]);
    Btn.Width := 60 + (i * 5); // Vary width to force wrapping
    Btn.Height := 30;
    FlowLayout.AddLayoutComponent(Btn, TfpgLayoutConstraint.Create());
  end;
end;

begin
  MainProc;
end.
