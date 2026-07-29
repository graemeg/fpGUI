{
  Demonstrates &-prefixed accelerator (mnemonic) keys.

  A '&' in a widget's Text marks the next character as its accelerator: it is
  painted underlined, and Alt+<char> activates the widget. Use '&&' to paint a
  literal ampersand.

  Note that Alt must be the only modifier - Ctrl+Alt+S or Shift+Alt+S will
  deliberately NOT fire the Alt+S accelerator.
}
program acceltest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_button,
  fpg_label,
  fpg_edit,
  fpg_checkbox,
  fpg_radiobutton;

type
  TMainForm = class(TfpgForm)
  private
    lblIntro: TfpgLabel;
    lblStatus: TfpgLabel;
    lblName: TfpgLabel;
    edName: TfpgEdit;
    btnScan: TfpgButton;
    btnClear: TfpgButton;
    btnAmp: TfpgButton;
    chkVerbose: TfpgCheckBox;
    rbFast: TfpgRadioButton;
    rbThorough: TfpgRadioButton;
    btnQuit: TfpgButton;
    procedure   btnScanClick(Sender: TObject);
    procedure   btnClearClick(Sender: TObject);
    procedure   btnAmpClick(Sender: TObject);
    procedure   chkVerboseChanged(Sender: TObject);
    procedure   rbChanged(Sender: TObject);
    procedure   btnQuitClick(Sender: TObject);
  public
    procedure   AfterCreate; override;
  end;


{ TMainForm }

procedure TMainForm.btnScanClick(Sender: TObject);
begin
  lblStatus.Text := 'Status: Scan activated (Alt+S)';
end;

procedure TMainForm.btnClearClick(Sender: TObject);
begin
  lblStatus.Text := 'Status: Clear activated (Alt+C)';
end;

procedure TMainForm.btnAmpClick(Sender: TObject);
begin
  lblStatus.Text := 'Status: Black & White activated (Alt+W)';
end;

procedure TMainForm.chkVerboseChanged(Sender: TObject);
begin
  if chkVerbose.Checked then
    lblStatus.Text := 'Status: Verbose enabled (Alt+V)'
  else
    lblStatus.Text := 'Status: Verbose disabled (Alt+V)';
end;

procedure TMainForm.rbChanged(Sender: TObject);
begin
  if rbFast.Checked then
    lblStatus.Text := 'Status: Fast mode selected (Alt+F)'
  else
    lblStatus.Text := 'Status: Thorough mode selected (Alt+T)';
end;

procedure TMainForm.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AfterCreate;
begin
  WindowTitle := 'Accelerator (Alt+key) demo';
  SetPosition(100, 100, 400, 340);

  lblIntro := TfpgLabel.Create(self);
  lblIntro.SetPosition(12, 10, 380, 32);
  lblIntro.Text := 'Press the underlined letter with Alt. Alt+N focuses the edit.'
      + LineEnding + 'Ctrl+Alt or Shift+Alt must NOT trigger them.';

  btnScan := TfpgButton.Create(self);
  btnScan.SetPosition(12, 55, 110, 26);
  btnScan.Text := '&Scan';
  btnScan.OnClick := @btnScanClick;

  btnClear := TfpgButton.Create(self);
  btnClear.SetPosition(132, 55, 110, 26);
  btnClear.Text := '&Clear';
  btnClear.OnClick := @btnClearClick;

  { '&&' paints a literal '&', and '&W' is the actual accelerator }
  btnAmp := TfpgButton.Create(self);
  btnAmp.SetPosition(252, 55, 130, 26);
  btnAmp.Text := 'Black && &White';
  btnAmp.OnClick := @btnAmpClick;

  { A label's accelerator moves focus to its FocusWidget - it never activates
    it. Without a FocusWidget assigned a label has no accelerator at all, and
    the '&' is painted literally. }
  lblName := TfpgLabel.Create(self);
  lblName.SetPosition(12, 98, 60, 22);
  lblName.Text := '&Name:';

  edName := TfpgEdit.Create(self);
  edName.SetPosition(78, 95, 160, 24);
  lblName.FocusWidget := edName;

  chkVerbose := TfpgCheckBox.Create(self);
  chkVerbose.SetPosition(12, 140, 200, 22);
  chkVerbose.Text := '&Verbose output';
  chkVerbose.OnChange := @chkVerboseChanged;

  rbFast := TfpgRadioButton.Create(self);
  rbFast.SetPosition(12, 170, 200, 22);
  rbFast.Text := '&Fast scan';
  rbFast.GroupIndex := 1;
  rbFast.OnChange := @rbChanged;

  rbThorough := TfpgRadioButton.Create(self);
  rbThorough.SetPosition(12, 195, 200, 22);
  rbThorough.Text := '&Thorough scan';
  rbThorough.GroupIndex := 1;
  rbThorough.OnChange := @rbChanged;

  lblStatus := TfpgLabel.Create(self);
  lblStatus.SetPosition(12, 235, 380, 22);
  lblStatus.Text := 'Status: waiting...';

  btnQuit := TfpgButton.Create(self);
  btnQuit.SetPosition(282, 280, 100, 26);
  btnQuit.Text := '&Quit';
  btnQuit.OnClick := @btnQuitClick;
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
