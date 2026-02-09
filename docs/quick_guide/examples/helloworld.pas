program helloworld;

{$mode objfpc}{$H+}

uses
  Classes, fpg_main, fpg_form, fpg_label;

type
  TMainForm = class(TfpgForm)
  public
    procedure AfterCreate; override;
  end;

procedure TMainForm.AfterCreate;
begin
  Name := 'MainForm';
  SetPosition(316, 186, 170, 30);
  WindowTitle := 'MainForm';
  CreateLabel(self, 40, 4, 'Hello World!');
end;

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  fpgApplication.CreateForm(TMainForm, frm);
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

