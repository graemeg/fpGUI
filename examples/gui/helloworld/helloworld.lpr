program helloworld;

{$mode objfpc}{$H+}

uses
  Classes,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_button;

type

  { THelloWorldForm }

  THelloWorldForm = class(TfpgForm)
    procedure ButtonClick(Sender: TObject);
  private
    Button: TfpgButton;
  protected
    procedure AfterCreate; override;
  end;

procedure MainProc;
var
  frm: THelloWorldForm;
begin
  fpgApplication.Initialize;
  frm := THelloWorldForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

{ THelloWorldForm }

procedure THelloWorldForm.ButtonClick(Sender: TObject);
begin
  Close;
end;

procedure THelloWorldForm.AfterCreate;
begin
  inherited AfterCreate;
  Width := 300;
  Height := 200;
  WindowTitle:='Hello World!';
  Button := TfpgButton.Create(Self);
  Button.Text:='Hello World!';
  Button.ShowHint:=True;
  Button.Hint:='Click to Quit';
  Button.Align:=alClient;
  Button.OnClick:=@ButtonClick;
end;

begin
  MainProc;
end.

