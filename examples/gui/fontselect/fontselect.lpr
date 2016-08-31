program fontselect;

{$mode objfpc}{$H+}

uses
  Classes,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_dialogs,
  fpg_button,
  fpg_listbox,
  fpg_edit,
  fpg_label,
  fpg_constants,
  fpg_trackbar;
  
  
type
  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    btnSelectFont: TfpgButton;
    edFontDesc: TfpgEdit;
    lblFontList: TfpgLabel;
    lbFontList: TfpgListBox;
    btnQuit: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    procedure   btnQuitClick(Sender: TObject);
    procedure   btnSelectFontClick(Sender: TObject);
    procedure   CreateFontList;
  public
    procedure   AfterCreate; override;
  end;


{ TMainForm }

procedure TMainForm.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnSelectFontClick(Sender: TObject);
var
  fontdesc: string;
begin
  fontdesc := edFontDesc.Text;
  if SelectFontDialog(fontdesc) then
    edFontDesc.Text := fontdesc;
end;

procedure TMainForm.CreateFontList;
var
  fl: TStringList;
  i: integer;
begin
  lbFontList.Items.Clear;
  fl := fpgApplication.GetFontFaceList;
  for i := 0 to fl.Count-1 do
    lbFontList.Items.Add(fl.Strings[i]);
  fl.Free;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(430, 283, 500, 400);
  WindowTitle := 'Font selection test';
  Hint := '';
  IconName := '';
  WindowPosition:= wpOneThirdDown;

  btnSelectFont := TfpgButton.Create(self);
  with btnSelectFont do
  begin
    Name := 'btnSelectFont';
    SetPosition(10, 10, 110, 24);
    Text := 'Select a font...';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick := @btnSelectFontClick;
  end;

  edFontDesc := TfpgEdit.Create(self);
  with edFontDesc do
  begin
    Name := 'edFontDesc';
    SetPosition(10, 45, 480, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 2;
    Text := 'Bitstream Vera Sans-9';
  end;

  lblFontList := TfpgLabel.Create(self);
  with lblFontList do
  begin
    Name := 'lblFontList';
    SetPosition(10, 84, 160, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Font List:';
  end;

  lbFontList := TfpgListBox.Create(self);
  with lbFontList do
  begin
    Name := 'lbFontList';
    SetPosition(10, 100, 232, 236);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 4;
    Items.Clear;
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(415, 370, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.Quit';
    TabOrder := 5;
    OnClick := @btnQuitClick;
  end;


  {@VFD_BODY_END: MainForm}

  CreateFontList;
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

