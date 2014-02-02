program test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpg_main, fpg_form, fpg_label,
  gui_miglayout,
  gui_mig_lc,
  gui_mig_constraintparser,
  gui_mig_boundsize,
  gui_mig_unitvalue,
  gui_mig_exceptions;

type
  TMainForm = class(TfpgForm)
  private
    FPanel: TfpgLayoutPanel;
    FLabel1: TfpgLabel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
  end;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'MiG Layout Test';
  WindowPosition := wpUser;

  FPanel := TfpgLayoutPanel.Create(self);

  SetPosition(100, 100, 300, 200);
end;

procedure TMainForm.AfterCreate;
begin
//  FLabel1 := TfpgLabel.Create(FPanel);
//  FPanel.Add(FLabel1, '');
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

begin
  MainProc;
end.



