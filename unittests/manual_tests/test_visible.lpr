{
  Project to test the .Visible property of components.
}
program test_visible;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, fpg_main, fpg_form, fpg_edit, fpg_button, fpg_label,
  fpg_memo, fpg_checkbox, fpg_radiobutton, fpg_widget, fpg_panel, fpg_menu;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    Edit1: TfpgEdit;
    Label1: TfpgLabel;
    Button1: TfpgButton;
    Memo1: TfpgMemo;
    CheckBox1: TfpgCheckBox;
    RadioButton1: TfpgRadioButton;
    Panel1: TfpgPanel;
    Edit2: TfpgEdit;
    Label2: TfpgLabel;
    CheckBox2: TfpgCheckBox;
    Memo2: TfpgMemo;
    MainMenu: TfpgMenuBar;
    pmFile: TfpgPopupMenu;
    {@VFD_HEAD_END: MainForm}
    procedure FormShow(Sender: TObject);
    procedure CheckboxChanged(Sender: TObject);
    procedure miFileQuitClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}



{@VFD_NEWFORM_IMPL}

var
  { manage indentation level }
  indent: integer = 0;

{ Output component hierarchy and status of each component's Visible property }
procedure PrintVisibleState(AComponent: TfpgWidget);
var
  i: integer;
  { Create string equal to indentation level }
  function Spaces: string;
  var
    j: integer;
  begin
    if indent = 0 then
      exit;
    for j := 1 to indent do
      Result := Result + ' ';
  end;

begin
  writeln(Spaces + AComponent.ClassName + ' - [parent]: ' + BoolToStr(AComponent.Visible, True));
  Inc(indent, 2);
  for i := 0 to AComponent.ComponentCount-1 do
  begin
    if AComponent.Components[i].ComponentCount > 0 then
      PrintVisibleState(TfpgWidget(AComponent.Components[i]))
    else
      writeln(Spaces + AComponent.Components[i].ClassName + ': ' + BoolToStr(TfpgWidget(AComponent.Components[i]).Visible, True));
  end;
  dec(indent, 2);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  writeln('DEBUG:  TMainForm.FormShow >>');
  PrintVisibleState(self);
  writeln('DEBUG:  TMainForm.FormShow <<');
end;

procedure TMainForm.CheckboxChanged(Sender: TObject);
begin
  writeln('Checkbox clicked...');
  Edit1.Visible := CheckBox1.Checked;
  Panel1.Visible := CheckBox1.Checked;
  PrintVisibleState(self);
end;

procedure TMainForm.miFileQuitClicked(Sender: TObject);
begin
  Close;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  writeln('DEBUG:  TMainForm.Create >>');
  inherited Create(AOwner);
  OnShow  := @FormShow;
  writeln('DEBUG:  TMainForm.Create <<');
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(316, 186, 591, 250);
  WindowTitle := 'Test Visible property';
  Hint := '';

  Edit1 := TfpgEdit.Create(self);
  with Edit1 do
  begin
    Name := 'Edit1';
    SetPosition(8, 28, 120, 24);
    Hint := '';
    TabOrder := 0;
    Text := '';
    FontDesc := '#Edit1';
    Visible := False;
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(140, 152, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(140, 28, 80, 24);
    Text := 'Button';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
  end;

  Memo1 := TfpgMemo.Create(self);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(8, 60, 120, 52);
    Hint := '';
    Lines.Add('');
    FontDesc := '#Edit1';
    TabOrder := 3;
  end;

  CheckBox1 := TfpgCheckBox.Create(self);
  with CheckBox1 do
  begin
    Name := 'CheckBox1';
    SetPosition(144, 64, 84, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 4;
    Text := 'Is Visible';
    OnChange  := @CheckboxChanged;
  end;

  RadioButton1 := TfpgRadioButton.Create(self);
  with RadioButton1 do
  begin
    Name := 'RadioButton1';
    SetPosition(8, 120, 120, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 5;
    Text := 'RadioButton';
  end;

  Panel1 := TfpgPanel.Create(self);
  with Panel1 do
  begin
    Name := 'Panel1';
    SetPosition(8, 148, 116, 72);
    FontDesc := '#Label1';
    Hint := '';
    Text := '';
  end;

  Edit2 := TfpgEdit.Create(Panel1);
  with Edit2 do
  begin
    Name := 'Edit2';
    SetPosition(8, 8, 96, 24);
    Hint := '';
    TabOrder := 0;
    Text := '';
    FontDesc := '#Edit1';
    Visible := False;
  end;

  Label2 := TfpgLabel.Create(Panel1);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(8, 36, 64, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  CheckBox2 := TfpgCheckBox.Create(Panel1);
  with CheckBox2 do
  begin
    Name := 'CheckBox2';
    SetPosition(8, 50, 101, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 2;
    Text := 'CheckBox';
  end;

  Memo2 := TfpgMemo.Create(self);
  with Memo2 do
  begin
    Name := 'Memo2';
    SetPosition(248, 4, 320, 232);
    Hint := '';
    Lines.Add('This apps tests the .Visible property of components.');
    Lines.Add('At startup, all TEdit componts should be invisible.');
    Lines.Add('When you click the "Is Visible" checkbox for the first');
    Lines.Add('time, then the Edit1 (top left) should become visible.');
    Lines.Add('');
    Lines.Add('All sub-sequent clicks of the "Is Visble" checkbox will');
    Lines.Add('toggle the Edit1 and Panel1 visibility. Not other');
    Lines.Add('components like Lable or Checbox inside Panel1');
    Lines.Add('should always stay Visible = True.');
    Lines.Add('');
    Lines.Add('Console output at runtime will show .Visible status of');
    Lines.Add('each component.');
    FontDesc := '#Edit1';
    TabOrder := 7;
  end;

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 144, 24);
  end;

  pmFile := TfpgPopupMenu.Create(self);
  with pmFile do
  begin
    Name := 'pmFile';
    SetPosition(108, 88, 120, 20);
    AddMenuItem('Quit', '', @miFileQuitClicked);
  end;

  {@VFD_BODY_END: MainForm}

  // hook up any sub-menus
  MainMenu.AddMenuItem('File', nil).SubMenu := pmFile;
  {%endregion}
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
