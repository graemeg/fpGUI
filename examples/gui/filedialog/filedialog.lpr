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

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    btnOpenFile: TfpgButton;
    btnSaveFile: TfpgButton;
    edFilename: TfpgEdit;
    btnQuit: TfpgButton;
    btnName1: TfpgButton;
    btnName2: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    procedure   btnQuitClick(Sender: TObject);
    procedure   btnOpenFileClick(Sender: TObject);
    procedure   btnSaveFileClick(Sender: TObject);
    procedure   btnMessageBoxClick(Sender: TObject);
    procedure   btnMessageDlgClick(Sender: TObject);
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

procedure TMainForm.btnMessageBoxClick(Sender: TObject);
begin
  ShowMessage('This is some pretty cool shit');
end;

procedure TMainForm.btnMessageDlgClick(Sender: TObject);
begin
  TfpgMessageDialog.AboutFPGui('My title here');
  TfpgMessageDialog.Critical('Something Critical...', 'And this is where the text goes.', mbAbortRetryIgnore, mbAbort);
  TfpgMessageDialog.Warning('Some Warning...', 'And this is where the text goes.', mbYesNoCancel, mbNo);
  TfpgMessageDialog.Information('Some Information...', 'And this is where the text goes.', [mbOK], mbNoButton);
  TfpgMessageDialog.Question('Some Question...', 'Did everything work okay?', mbYesNo, mbNoButton);
end;

procedure TMainForm.AfterCreate;
begin
  inherited AfterCreate;
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(197, 147, 419, 138);
  WindowTitle := 'File dialog test';
  MinWidth := 300;
  MinHeight := 135;

  btnOpenFile := TfpgButton.Create(self);
  with btnOpenFile do
  begin
    Name := 'btnOpenFile';
    SetPosition(8, 8, 80, 23);
    Text := 'Open File...';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnOpenFileClick;
  end;

  btnSaveFile := TfpgButton.Create(self);
  with btnSaveFile do
  begin
    Name := 'btnSaveFile';
    SetPosition(8, 34, 80, 23);
    Text := 'Save File...';
    FontDesc := '#Label1';
    ImageName := '';
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
    FontDesc := '#Label1';
    ImageName := 'stdimg.Quit';
    OnClick := @btnQuitClick;
  end;

  btnName1 := TfpgButton.Create(self);
  with btnName1 do
  begin
    Name := 'btnName1';
    SetPosition(148, 8, 119, 27);
    Text := 'Message Box';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnMessageBoxClick;
  end;

  btnName2 := TfpgButton.Create(self);
  with btnName2 do
  begin
    Name := 'btnName2';
    SetPosition(272, 8, 131, 27);
    Text := 'Message Dialog';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnMessageDlgClick;
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

