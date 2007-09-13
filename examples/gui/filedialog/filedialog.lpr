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
    btnQuit: TfpgButton;
    btnOpenFile: TfpgButton;
    btnSaveFile: TfpgButton;
    edFilename: TfpgEdit;
    procedure   btnQuitClick(Sender: TObject);
    procedure   btnOpenFileClick(Sender: TObject);
    procedure   btnSaveFileClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

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

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'File dialog test';
  SetPosition(100, 100, 500, 400);
  
  btnOpenFile := CreateButton(self, 10, 10, 110, 'Open File...', @btnOpenFileClick);
  btnSaveFile := CreateButton(self, 10, btnOpenFile.Bottom + 8, 110, 'Save File...', @btnSaveFileClick);

  edFilename := CreateEdit(self, 10, btnSaveFile.Bottom + 15, Width - 20, 24);
  edFilename.Text := '';

  btnQuit := CreateButton(self, 415, 370, 80, 'Quit', @btnQuitClick);
  btnQuit.ImageName := 'stdimg.Quit';
  btnQuit.ShowImage := True;
  btnQuit.Anchors := [anRight, anBottom];

  btnOpenFile.TabOrder := 0;
end;

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

