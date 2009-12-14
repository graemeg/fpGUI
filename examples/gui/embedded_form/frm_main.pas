unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_panel, fpg_button,
  fpg_checkbox, fra_test, fpg_menu;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    BorderBevel: TfpgBevel;
    Container: TfpgBevel;
    btnExternal: TfpgButton;
    btnEmbedded: TfpgButton;
    CheckBox1: TfpgCheckBox;
    MainMenu: TfpgMenubar;
    mnuFile: TfpgPopupMenu;
    mnuHelp: TfpgPopupMenu;
    {@VFD_HEAD_END: MainForm}
    frame: TMyFrame;
    procedure btnExternalClicked(Sender: TObject);
    procedure btnEmbeddedClicked(Sender: TObject);
    procedure CheckBoxChanged(Sender: TObject);
    procedure miQuitClicked(Sender: TObject);
    procedure miHelpAboutClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  fpg_widget, fpg_dialogs;


{@VFD_NEWFORM_IMPL}

procedure TMainForm.CheckBoxChanged(Sender: TObject);
begin
  frame.Visible := Checkbox1.Checked;
end;

procedure TMainForm.miQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miHelpAboutClicked(Sender: TObject);
begin
  TfpgMessageDialog.Information('Embedded Frame Demo', 'A simple demo showing how to embed frames')
end;

procedure TMainForm.btnExternalClicked(Sender: TObject);
var
  frm: TfpgForm;
  fra: TMyFrame;
begin
  { create a form at runtime that will hold our frame. }
  frm := TfpgForm.Create(nil);
  try
    frm.WindowPosition := wpOneThirdDown;
    frm.Width := 284;
    frm.Height := 257;

    // embed the frame in the form
    fra := TMyFrame.Create(frm);
    fra.Align := alClient;

    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TMainForm.btnEmbeddedClicked(Sender: TObject);
begin
  if Assigned(frame) then
    exit;

  frame := TMyFrame.Create(Container);
  with frame do
  begin
    Name := 'frame';
    SetPosition(0, 0, 280, 196);
    Shape := bsSpacer;
    Align := alClient;
    Visible := True;
  end;

  CheckBox1.Enabled := True;
  CheckBox1.Checked := True;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(387, 207, 300, 340);
  WindowTitle := 'MainForm';

  BorderBevel := TfpgBevel.Create(self);
  with BorderBevel do
  begin
    Name := 'BorderBevel';
    SetPosition(0, 68, 300, 272);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Shape := bsSpacer;
  end;

  Container := TfpgBevel.Create(BorderBevel);
  with Container do
  begin
    Name := 'Container';
    SetPosition(8, 8, 284, 257);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Style := bsLowered;
    Shape := bsSpacer;
  end;

  btnExternal := TfpgButton.Create(self);
  with btnExternal do
  begin
    Name := 'btnExternal';
    SetPosition(12, 40, 80, 24);
    Text := 'External';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick := @btnExternalClicked;
  end;

  btnEmbedded := TfpgButton.Create(self);
  with btnEmbedded do
  begin
    Name := 'btnEmbedded';
    SetPosition(100, 40, 80, 24);
    Text := 'Embedded';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
    OnClick := @btnEmbeddedClicked;
  end;

  CheckBox1 := TfpgCheckBox.Create(self);
  with CheckBox1 do
  begin
    Name := 'CheckBox1';
    SetPosition(192, 40, 93, 20);
    FontDesc := '#Label1';
    TabOrder := 4;
    Text := 'Visible';
    Enabled := false;
    OnChange  := @CheckBoxChanged;
  end;

  MainMenu := TfpgMenubar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 300, 28);
    Anchors := [anLeft,anRight,anTop];
  end;

  mnuFile := TfpgPopupMenu.Create(self);
  with mnuFile do
  begin
    Name := 'mnuFile';
    SetPosition(156, 88, 120, 20);
    AddMenuItem('Quit', '', @miQuitClicked);
  end;

  mnuHelp := TfpgPopupMenu.Create(self);
  with mnuHelp do
  begin
    Name := 'mnuHelp';
    SetPosition(156, 112, 120, 20);
    AddMenuItem('About...', '', @miHelpAboutClicked);
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
  MainMenu.AddMenuItem('File', nil).SubMenu := mnuFile;
  MainMenu.AddMenuItem('Help', nil).SubMenu := mnuHelp;
end;


end.
