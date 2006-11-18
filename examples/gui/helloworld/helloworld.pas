program HelloWorld;

{$mode objfpc}{$h+}

uses
  fpGUI, fpGUI_laz;

type
  TMainForm = class(TForm)
    TextLabel: TLabel;
    lblClose: TLabel;
  end;

var
  MainForm: TMainForm;

begin
  Application.CreateForm(TMainForm, MainForm);
  
  Application.Run;
  MainForm.Free;
end.
