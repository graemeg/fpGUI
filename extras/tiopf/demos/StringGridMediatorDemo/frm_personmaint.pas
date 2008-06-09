unit frm_personmaint;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, gfxbase, fpgfx, gui_edit, 
  gfx_widget, gui_form, gui_label, gui_button,
  gui_listbox, gui_memo, gui_combobox, gui_basegrid, gui_grid, 
  gui_dialogs, gui_checkbox, gui_tree, gui_trackbar, 
  gui_progressbar, gui_radiobutton, gui_tab, gui_menu,
  gui_panel, gui_popupcalendar, gui_gauge, model, model_view;

type

  TPersonMaintForm = class(TfpgForm)
  private
    FData: TPerson;
    FMemento: TPersonMemento;   // This form is the Caretaker
    FmedName: TPerson_Name_TextEdit_View;
    FmedAge: TPerson_Age_TextEdit_View;
    procedure   FormShow(Sender: TObject);
    procedure   SetData(const AValue: TPerson);
    procedure   SetupMediators;
  public
    {@VFD_HEAD_BEGIN: PersonMaintForm}
    lblName1: TfpgLabel;
    edtName: TfpgEdit;
    lblName2: TfpgLabel;
    edtAge: TfpgEdit;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    lblName3: TfpgLabel;
    {@VFD_HEAD_END: PersonMaintForm}
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
    property    Data: TPerson read FData write SetData;
  end;


procedure EditPerson(const AData: TPerson);

{@VFD_NEWFORM_DECL}

implementation

procedure EditPerson(const AData: TPerson);
var
  frm: TPersonMaintForm;
begin
  frm := TPersonMaintForm.Create(nil);
  try
    frm.Data := AData;
    if frm.ShowModal = 2 then // Cancel clicked
    begin
      // undo changes
      AData.BeginUpdate;
      AData.Memento := frm.FMemento;
      AData.EndUpdate;
    end;
  finally
    frm.Free;
  end;
end;

{@VFD_NEWFORM_IMPL}

procedure TPersonMaintForm.FormShow(Sender: TObject);
begin
  SetupMediators;
end;

procedure TPersonMaintForm.SetData(const AValue: TPerson);
begin
  if FData = AValue then
    exit; //==>>
  FData := AValue;
  FreeAndNil(FMemento);
  FMemento := FData.Memento;
end;

procedure TPersonMaintForm.SetupMediators;
begin
  FmedName := TPerson_Name_TextEdit_View.CreateCustom(edtName, FData, 'Name', 'Text');
  FmedAge := TPerson_Age_TextEdit_View.CreateCustom(edtAge, FData, 'Age', 'Text');
//  edtName.Text := FData.Name;
//  edtAge.Text := IntToStr(FData.Age);
  
  // Notify all observers to update themselves.
  FData.NotifyObservers;
end;

constructor TPersonMaintForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnShow := @FormShow;
end;

destructor TPersonMaintForm.Destroy;
begin
  FmedName.Free;
  FmedAge.Free;
  inherited Destroy;
end;

procedure TPersonMaintForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: PersonMaintForm}
  Name := 'PersonMaintForm';
  SetPosition(418, 244, 232, 190);
  WindowTitle := 'Edit Person...';

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(8, 8, 212, 15);
    FontDesc := '#Label1';
    Text := 'Name:';
  end;

  edtName := TfpgEdit.Create(self);
  with edtName do
  begin
    Name := 'edtName';
    SetPosition(8, 24, 212, 21);
    TabOrder := 1;
    Text := '';
    FontDesc := '#Edit1';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(8, 56, 212, 15);
    FontDesc := '#Label1';
    Text := 'Age:';
  end;

  edtAge := TfpgEdit.Create(self);
  with edtAge do
  begin
    Name := 'edtAge';
    SetPosition(8, 72, 64, 21);
    TabOrder := 3;
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    Name := 'btnOK';
    SetPosition(56, 159, 80, 23);
    Anchors := [anRight,anBottom];
    Text := 'OK';
    FontDesc := '#Label1';
    ImageName := '';
    ModalResult := 1;
    TabOrder := 4;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(140, 159, 80, 23);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    ImageName := '';
    ModalResult := 2;
    TabOrder := 5;
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(8, 108, 212, 39);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Label1';
    Text := 'Notice as you change the values they are updated in the MainForm''s Grid.';
    WrapText := True;
  end;

  {@VFD_BODY_END: PersonMaintForm}
end;


end.
