program filedialog;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpgfx,
  gui_form,
  gui_dialogs,
  gui_button,
  gui_edit,
  gui_label;


type
  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    btnOpenFile: TfpgButton;
    btnSaveFile: TfpgButton;
    edFilename: TfpgEdit;
    btnQuit: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    procedure   btnQuitClick(Sender: TObject);
    procedure   btnOpenFileClick(Sender: TObject);
    procedure   btnSaveFileClick(Sender: TObject);
  public
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

{ TMainForm }

procedure TMainForm.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnOpenFileClick(Sender: TObject);
var
  dlg: TfpgFileDialog;
begin
  dlg := TfpgFileDialog.Create(nil);
  try
    // defines 3 filters (All Files, Object Pascal and Lazarus Project)
    dlg.Filter := 'All Files (*)|*|Object Pascal (*.pas;*.lpr;*.pp)|*.pas;*.lpr;*.pp|Lazarus Project (*.lpi)|*.lpi';
    if dlg.RunOpenFile then
      edFilename.Text := dlg.FileName;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.btnSaveFileClick(Sender: TObject);
var
  dlg: TfpgFileDialog;
begin
  dlg := TfpgFileDialog.Create(nil);
  try
    if dlg.RunSaveFile then
      edFilename.Text := dlg.FileName;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.AfterCreate;
begin
  inherited AfterCreate;
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(100, 100, 419, 138);
  WindowTitle := 'File dialog test';
  MinWidth := 300;
  MinHeight := 135;

  btnOpenFile := TfpgButton.Create(self);
  with btnOpenFile do
  begin
    Name := 'btnOpenFile';
    SetPosition(8, 8, 80, 23);
    Text := 'Open File...';
    AllowAllUp := False;
    Embedded := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    ImageMargin := 3;
    ImageName := '';
    ImageSpacing := -1;
    ModalResult := 0;
    ShowImage := True;
    OnClick := @btnOpenFileClick;
  end;

  btnSaveFile := TfpgButton.Create(self);
  with btnSaveFile do
  begin
    Name := 'btnSaveFile';
    SetPosition(8, 34, 80, 23);
    Text := 'Save File...';
    AllowAllUp := False;
    Embedded := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    ImageMargin := 3;
    ImageName := '';
    ImageSpacing := -1;
    ModalResult := 0;
    ShowImage := True;
    OnClick := @btnSaveFileClick;
  end;

  edFilename := TfpgEdit.Create(self);
  with edFilename do
  begin
    Name := 'edFilename';
    SetPosition(8, 70, 400, 24);
    Anchors := [anLeft,anRight,anTop];
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(329, 107, 80, 23);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    AllowAllUp := False;
    Embedded := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    ImageMargin := 3;
    ImageName := 'stdimg.Quit';
    ImageSpacing := -1;
    ModalResult := 0;
    ShowImage := True;
    OnClick := @btnQuitClick;
  end;

  {@VFD_BODY_END: MainForm}
end;

{@VFD_NEWFORM_IMPL}

procedure MainProc;
var
  frm: TMainForm;
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

