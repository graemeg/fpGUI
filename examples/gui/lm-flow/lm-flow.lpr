program lm_flow;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_button,
  frm_simple,
  frm_containers;


{@VFD_NEWFORM_DECL}

{@VFD_NEWFORM_IMPL}


type
  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    btnSimple: TfpgButton;
    btnContainers: TfpgButton;
    btnQuit: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    procedure btnSimpleClicked(Sender: TObject);
    procedure btnContainersClicked(Sender: TObject);
    procedure btnQuitClicked(Sender: TObject);
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

procedure TMainForm.btnSimpleClicked(Sender: TObject);
var
  frm: TSimpleFlowForm;
begin
  frm := TSimpleFlowForm.Create(nil);
  frm.ShowModal;
end;

procedure TMainForm.btnContainersClicked(Sender: TObject);
var
  frm: TFlowWithContainers;
begin
  frm := TFlowWithContainers.Create(nil);
  frm.ShowModal;
end;

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AfterCreate;
begin
  inherited AfterCreate;
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(474, 349, 175, 250);
  WindowTitle := 'Flow Layout Example';
  Hint := '';
  IconName := 'stdimg.windowicon';

  btnSimple := TfpgButton.Create(self);
  with btnSimple do
  begin
    Name := 'btnSimple';
    SetPosition(12, 12, 150, 35);
    Text := 'Simple Flow';
    FontDesc := 'Liberation Sans-10:antialias=true';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
    OnClick := @btnSimpleClicked;
  end;

  btnContainers := TfpgButton.Create(self);
  with btnContainers do
  begin
    Name := 'btnContainers';
    SetPosition(12, 52, 150, 35);
    Text := 'Flow with Containers';
    FontDesc := 'Liberation Sans-10:antialias=true';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnContainersClicked;
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(12, 124, 150, 35);
    Text := 'Quit';
    FontDesc := 'Liberation Sans-10:antialias=true';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
    OnClick := @btnQuitClicked;
  end;

  {@VFD_BODY_END: MainForm}
end;

begin
  MainProc;
end.
