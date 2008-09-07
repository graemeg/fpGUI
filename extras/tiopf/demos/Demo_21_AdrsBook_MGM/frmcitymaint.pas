unit frmCityMaint;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, gfxbase, fpgfx, gui_edit, 
  gfx_widget, gui_form, gui_label, gui_button,
  gui_dialogs, gui_combobox,
  model, tiFormMediator;

type

  TCityEditForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: CityEditForm}
    lblName1: TfpgLabel;
    edName: TfpgEdit;
    lblName2: TfpgLabel;
    edZIP: TfpgEdit;
    lblName3: TfpgLabel;
    cbCountry: TfpgComboBox;
    btnSave: TfpgButton;
    btnCancel: TfpgButton;
    {@VFD_HEAD_END: CityEditForm}
    FMediator: TFormMediator;
    FData: TCity;
    procedure SetData(const AValue: TCity);
    procedure SetupMediators;
  public
    procedure AfterCreate; override;
    property  Data: TCity read FData write SetData;
  end;

{@VFD_NEWFORM_DECL}

function EditCity(AData: TCity): boolean;


implementation

uses
  tiBaseMediator, tiMediators, contactmanager, typinfo;


function EditCity(AData: TCity): boolean;
var
  frm: TCityEditForm;
begin
  frm:= TCityEditForm.Create(nil);
  try
    frm.SetData(AData);
    result:= frm.ShowModal = mrOK;
  finally
    frm.Free;
  end;
end;

{@VFD_NEWFORM_IMPL}

procedure TCityEditForm.SetData(const AValue: TCity);
begin
  if FData=AValue then exit;
  FData:=AValue;
  SetupMediators;
end;

procedure TCityEditForm.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    RegisterFallBackMediators;
    gMediatorManager.RegisterMediator(TMediatorDynamicComboBoxView, TCity, 'Country');

    FMediator := TFormMediator.Create(self);
    FMediator.AddProperty('Name', edName);
    FMediator.AddProperty('ZIP', edZIP);
    FMediator.AddProperty('Country', cbCountry).ValueList := gContactManager.CountryList;
  end;
  FMediator.Subject := FData;
  FMediator.Active := True;
end;

procedure TCityEditForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: CityEditForm}
  Name := 'CityEditForm';
  SetPosition(673, 204, 260, 186);
  WindowTitle := 'City Maintenance';

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(8, 8, 80, 16);
    FontDesc := '#Label1';
    Text := 'City name:';
  end;

  edName := TfpgEdit.Create(self);
  with edName do
  begin
    Name := 'edName';
    SetPosition(8, 24, 200, 22);
    TabOrder := 1;
    Text := '';
    FontDesc := '#Edit1';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(8, 52, 80, 16);
    FontDesc := '#Label1';
    Text := 'ZIP code:';
  end;

  edZIP := TfpgEdit.Create(self);
  with edZIP do
  begin
    Name := 'edZIP';
    SetPosition(8, 68, 120, 22);
    TabOrder := 3;
    Text := '';
    FontDesc := '#Edit1';
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(8, 96, 80, 16);
    FontDesc := '#Label1';
    Text := 'Country:';
  end;

  cbCountry := TfpgComboBox.Create(self);
  with cbCountry do
  begin
    Name := 'cbCountry';
    SetPosition(8, 112, 200, 22);
    FontDesc := '#List';
    TabOrder := 5;
  end;

  btnSave := TfpgButton.Create(self);
  with btnSave do
  begin
    Name := 'btnSave';
    SetPosition(92, 156, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Save';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 6;
    ModalResult := mrOK;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(176, 156, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 7;
    ModalResult := mrCancel;
  end;

  {@VFD_BODY_END: CityEditForm}
end;


end.
