unit frm_test;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_widget,
  fpg_edit, fpg_form, fpg_label, fpg_button,
  fpg_listbox, fpg_memo, fpg_combobox, fpg_basegrid, fpg_grid, 
  fpg_dialogs, fpg_checkbox, fpg_tree, fpg_trackbar, 
  fpg_progressbar, fpg_radiobutton, fpg_tab, fpg_menu,
  fpg_panel, fpg_popupcalendar, fpg_gauge;

type

  TTestForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: TestForm}
    btnName1: TfpgButton;
    lblName1: TfpgLabel;
    lblName2: TfpgLabel;
    btnName2: TfpgButton;
    lblName3: TfpgLabel;
    btnName3: TfpgButton;
    lblName4: TfpgLabel;
    btnName4: TfpgButton;
    edtName1: TfpgEdit;
    btnClose: TfpgButton;
    {@VFD_HEAD_END: TestForm}
    procedure CloseClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TTestForm.CloseClicked(Sender: TObject);
begin
  Close;
end;

procedure TTestForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: TestForm}
  Name := 'TestForm';
  SetPosition(335, 206, 300, 250);
  WindowTitle := 'Testing Custom Styles';
  WindowPosition := wpScreenCenter;

  btnName1 := TfpgButton.Create(self);
  with btnName1 do
  begin
    Name := 'btnName1';
    SetPosition(24, 48, 80, 24);
    Text := 'Button1';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(20, 24, 116, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Standard Button';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(164, 24, 124, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Embedded Button';
  end;

  btnName2 := TfpgButton.Create(self);
  with btnName2 do
  begin
    Name := 'btnName2';
    SetPosition(176, 48, 80, 24);
    Text := 'Button2';
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(20, 92, 100, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Default Button';
  end;

  btnName3 := TfpgButton.Create(self);
  with btnName3 do
  begin
    Name := 'btnName3';
    SetPosition(24, 116, 80, 24);
    Text := 'Button3';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    Default := True;
  end;

  lblName4 := TfpgLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(164, 92, 116, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Flat Button';
  end;

  btnName4 := TfpgButton.Create(self);
  with btnName4 do
  begin
    Name := 'btnName4';
    SetPosition(176, 116, 80, 24);
    Text := 'Button4';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
  end;

  edtName1 := TfpgEdit.Create(self);
  with edtName1 do
  begin
    Name := 'edtName1';
    SetPosition(24, 168, 120, 22);
    TabOrder := 5;
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnClose := TfpgButton.Create(self);
  with btnClose do
  begin
    Name := 'btnClose';
    SetPosition(212, 216, 80, 24);
    Text := 'Close';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.close';
    TabOrder := 6;
    OnClick := @CloseClicked;
  end;

  {@VFD_BODY_END: TestForm}
end;



end.
