unit frmAddressMaint;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_edit,
  fpg_widget, fpg_form, fpg_label, fpg_button, fpg_combobox,
  fpg_dialogs, fpg_menu,
  model, tiFormMediator;

type
  TAddressEditForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: AddressEditForm}
    cbType: TfpgComboBox;
    edNo: TfpgEdit;
    edStreet: TfpgEdit;
    cbCity: TfpgComboBox;
    edPhone1: TfpgEdit;
    edPhone2: TfpgEdit;
    edFax: TfpgEdit;
    lblName1: TfpgLabel;
    lblName2: TfpgLabel;
    lblName3: TfpgLabel;
    lblName4: TfpgLabel;
    lblName5: TfpgLabel;
    lblName6: TfpgLabel;
    lblName7: TfpgLabel;
    btnSave: TfpgButton;
    btnCancel: TfpgButton;
    {@VFD_HEAD_END: AddressEditForm}
    FData: TAddress;
    FMediator: TFormMediator;
    procedure SetData(const AValue: TAddress);
    procedure SetupMediators;
  public
    procedure AfterCreate; override;
    property  Data: TAddress read FData write SetData;
  end;

{@VFD_NEWFORM_DECL}

function EditAddress(AData: TAddress): Boolean;

implementation

uses
  contactmanager;


function EditAddress(AData: TAddress): Boolean;
var
  frm: TAddressEditForm;
begin
  frm:= TAddressEditForm.Create(nil);
  try
    frm.SetData(AData);
    result:= frm.ShowModal = mrOK;
  finally
    frm.Free;
  end;
end;

{@VFD_NEWFORM_IMPL}

procedure TAddressEditForm.SetData(const AValue: TAddress);
begin
  if FData=AValue then exit;
  FData:=AValue;
  SetupMediators;
end;

procedure TAddressEditForm.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TFormMediator.Create(self);
    FMediator.AddProperty('AddressType', cbType).ValueList := gContactManager.AddressTypeList;
    FMediator.AddProperty('Nr', edNo);
    FMediator.AddProperty('Street', edStreet);
    FMediator.AddProperty('City', cbCity).ValueList := gContactManager.CityList;
    FMediator.AddProperty('Telephone1', edPhone1);
    FMediator.AddProperty('Telephone2', edPhone2);
    FMediator.AddProperty('Fax', edFax);
  end;
  FMediator.Subject := FData;
  FMediator.Active := True;
end;

procedure TAddressEditForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: AddressEditForm}
  Name := 'AddressEditForm';
  SetPosition(300, 231, 296, 282);
  WindowTitle := 'Address Edit Form';

  cbType := TfpgComboBox.Create(self);
  with cbType do
  begin
    Name := 'cbType';
    SetPosition(8, 24, 272, 22);
    FontDesc := '#List';
  end;

  edNo := TfpgEdit.Create(self);
  with edNo do
  begin
    Name := 'edNo';
    SetPosition(8, 68, 56, 22);
    TabOrder := 1;
    Text := '';
    FontDesc := '#Edit1';
  end;

  edStreet := TfpgEdit.Create(self);
  with edStreet do
  begin
    Name := 'edStreet';
    SetPosition(76, 68, 204, 22);
    TabOrder := 2;
    Text := '';
    FontDesc := '#Edit1';
  end;

  cbCity := TfpgComboBox.Create(self);
  with cbCity do
  begin
    Name := 'cbCity';
    SetPosition(8, 112, 272, 22);
    FontDesc := '#List';
    TabOrder := 3;
  end;

  edPhone1 := TfpgEdit.Create(self);
  with edPhone1 do
  begin
    Name := 'edPhone1';
    SetPosition(8, 160, 120, 22);
    TabOrder := 4;
    Text := '';
    FontDesc := '#Edit1';
  end;

  edPhone2 := TfpgEdit.Create(self);
  with edPhone2 do
  begin
    Name := 'edPhone2';
    SetPosition(160, 160, 120, 22);
    TabOrder := 5;
    Text := '';
    FontDesc := '#Edit1';
  end;

  edFax := TfpgEdit.Create(self);
  with edFax do
  begin
    Name := 'edFax';
    SetPosition(8, 204, 120, 22);
    TabOrder := 6;
    Text := '';
    FontDesc := '#Edit1';
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(76, 52, 120, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Street:';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(8, 52, 60, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Number:';
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(8, 144, 120, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Telephone #1:';
  end;

  lblName4 := TfpgLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(160, 144, 112, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Telephone #2:';
  end;

  lblName5 := TfpgLabel.Create(self);
  with lblName5 do
  begin
    Name := 'lblName5';
    SetPosition(8, 188, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Fax:';
  end;

  lblName6 := TfpgLabel.Create(self);
  with lblName6 do
  begin
    Name := 'lblName6';
    SetPosition(8, 8, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Type:';
  end;

  lblName7 := TfpgLabel.Create(self);
  with lblName7 do
  begin
    Name := 'lblName7';
    SetPosition(8, 96, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'City:';
  end;

  btnSave := TfpgButton.Create(self);
  with btnSave do
  begin
    Name := 'btnSave';
    SetPosition(124, 248, 80, 24);
    Text := 'Save';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 7;
    ModalResult := mrOK;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(208, 248, 80, 24);
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 8;
    ModalResult := mrCancel;
  end;

  {@VFD_BODY_END: AddressEditForm}
end;


end.
