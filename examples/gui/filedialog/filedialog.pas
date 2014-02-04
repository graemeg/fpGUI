program filedialog;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_dialogs,
  fpg_button,
  fpg_edit,
  fpg_label;


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
    btnUserPrompt: TfpgButton;
    btnUserInput: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    procedure   btnQuitClick(Sender: TObject);
    procedure   btnOpenFileClick(Sender: TObject);
    procedure   btnSaveFileClick(Sender: TObject);
    procedure   btnMessageBoxClick(Sender: TObject);
    procedure   btnMessageDlgClick(Sender: TObject);
    procedure   btnUserPromptClick(Sender: TObject);
    procedure   btnUserInputClicked(Sender: TObject);
  public
    procedure   AfterCreate; override;
  end;
  

  TMyDBLoginDlg = class(TfpgPromptUserDbDialog)
  private
    function    GetDatabase: TfpgString;
  protected
    procedure   PopulateComboDb; override;
  public
    property    Database: TfpgString read GetDatabase;
  end;

{ TMyDBLoginDlg }

function TMyDBLoginDlg.GetDatabase: TfpgString;
begin
  if cbDatabases.FocusItem = -1 then
    Result := '<nothing selected>'
  else
    Result := aStringList.ValueFromIndex[cbDatabases.FocusItem];
end;

procedure TMyDBLoginDlg.PopulateComboDb;
var
  i: integer;
begin
  aStringList.Clear;
  aStringList.Add('Database1=192.168.0.1:/data/db1.gdb');
  aStringList.Add('Database2=192.168.0.10:/data/db2.gdb');
  aStringList.Add('Database3=192.168.0.150:/data/db3.gdb');
  aStringList.Add('Database4=192.168.0.200:c:\MyData\db4.gdb');
  cbDatabases.Items.Clear;
  for i := 0 to aStringList.Count-1 do
    cbDatabases.Items.Add(aStringList.Names[i]);
end;

{@VFD_NEWFORM_DECL}

{ TMainForm }

procedure TMainForm.btnUserPromptClick(Sender: TObject);
var
  dlg: TMyDBLoginDlg;
begin
  dlg := TMyDBLoginDlg.Create(nil);
  try
    dlg.WindowTitle := 'Sample Login';
    if dlg.ShowModal = mrOK then
    begin
      TfpgMessageDialog.Information('User Results',
          'User=' + dlg.UserID + #13 +
          'Pass=' + dlg.Password +  #13 +
          'Database=' + dlg.Database, [mbOK]);
//      fpgApplication.ProcessMessages;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.btnUserInputClicked(Sender: TObject);
var
  lAnswer: TfpgString;
begin
  if fpgInputQuery('Caption here', 'And the prompt goes here', lAnswer) then
    ShowMessage(Format('User entered <%s>', [lAnswer]));
end;

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
    if edFilename.Text <> '' then
      dlg.Filename := edFilename.Text;
    if dlg.RunSaveFile then
      edFilename.Text := dlg.FileName;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.btnMessageBoxClick(Sender: TObject);
begin
  ShowMessage('This is some pretty cool stuff');
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
  SetPosition(330, 199, 419, 138);
  WindowTitle := 'File dialog test';
  Hint := '';
  MinWidth := 300;
  MinHeight := 135;

  btnOpenFile := TfpgButton.Create(self);
  with btnOpenFile do
  begin
    Name := 'btnOpenFile';
    SetPosition(8, 8, 80, 23);
    Text := 'Open File...';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick := @btnOpenFileClick;
  end;

  btnSaveFile := TfpgButton.Create(self);
  with btnSaveFile do
  begin
    Name := 'btnSaveFile';
    SetPosition(8, 34, 80, 23);
    Text := 'Save File...';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
    OnClick := @btnSaveFileClick;
  end;

  edFilename := TfpgEdit.Create(self);
  with edFilename do
  begin
    Name := 'edFilename';
    SetPosition(8, 70, 400, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    Hint := '';
    TabOrder := 2;
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
    Hint := '';
    ImageName := 'stdimg.Quit';
    TabOrder := 6;
    OnClick := @btnQuitClick;
  end;

  btnName1 := TfpgButton.Create(self);
  with btnName1 do
  begin
    Name := 'btnName1';
    SetPosition(148, 8, 116, 27);
    Text := 'Message Box';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnMessageBoxClick;
  end;

  btnName2 := TfpgButton.Create(self);
  with btnName2 do
  begin
    Name := 'btnName2';
    SetPosition(272, 8, 131, 27);
    Text := 'Message Dialog';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
    OnClick := @btnMessageDlgClick;
  end;

  btnUserPrompt := TfpgButton.Create(self);
  with btnUserPrompt do
  begin
    Name := 'btnUserPrompt';
    SetPosition(272, 40, 131, 24);
    Text := 'User Prompt';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 5;
    OnClick := @btnUserPromptClick;
  end;

  btnUserInput := TfpgButton.Create(self);
  with btnUserInput do
  begin
    Name := 'btnUserInput';
    SetPosition(148, 40, 116, 24);
    Text := 'User Input';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 8;
    OnClick :=@btnUserInputClicked;
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

