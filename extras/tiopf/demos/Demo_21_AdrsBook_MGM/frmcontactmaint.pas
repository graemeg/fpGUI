unit frmContactMaint;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_edit,
  fpg_widget, fpg_form, fpg_label, fpg_button,
  fpg_listview, fpg_memo,
  model, tiFormMediator;

type

  TContactEditForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: ContactEditForm}
    lblName1: TfpgLabel;
    edFName: TfpgEdit;
    lblName2: TfpgLabel;
    edLName: TfpgEdit;
    lblName3: TfpgLabel;
    edEmail: TfpgEdit;
    lblName4: TfpgLabel;
    edMobile: TfpgEdit;
    lblName5: TfpgLabel;
    meComments: TfpgMemo;
    lblName6: TfpgLabel;
    lvAddresses: TfpgListView;
    btnSave: TfpgButton;
    btnCancel: TfpgButton;
    btnAdd: TfpgButton;
    btnEdit: TfpgButton;
    btnDelete: TfpgButton;
    btnDebug: TfpgButton;
    {@VFD_HEAD_END: ContactEditForm}
    FData: TContact;
    FMediator: TFormMediator;
    FAdrsMediator: TFormMediator;
    procedure SetData(const AValue: TContact);
    procedure SetupMediators;
    procedure btnDebugClicked(Sender: TObject);
    procedure btnEditClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
    property  Data: TContact read FData write SetData;
  end;

{@VFD_NEWFORM_DECL}

function EditContact(AData: TContact): Boolean;


implementation

uses
  contactmanager, tiDialogs, frmAddressMaint, tiListMediators;


function EditContact(AData: TContact): Boolean;
var
  frm: TContactEditForm;
begin
  frm:= TContactEditForm.Create(nil);
  try
    frm.SetData(AData);
    result:= frm.ShowModal = mrOK;
  finally
    frm.Free;
  end;
end;


{@VFD_NEWFORM_IMPL}

procedure TContactEditForm.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TFormMediator.Create(self);
    FMediator.AddProperty('FirstName', edFName);
    FMediator.AddProperty('LastName', edLName);
    FMediator.AddProperty('EMail', edEmail);
    FMediator.AddProperty('Mobile', edMobile);
    FMediator.AddProperty('Comments', meComments);
  end;
  FMediator.Subject := FData;
  FMediator.Active := True;

  if not Assigned(FAdrsMediator) then
  begin
    FAdrsMediator := TFormMediator.Create(self);
    FAdrsMediator.AddComposite({'AddressType.Name;}'AddressType4GUI(50,"Type");Nr;Street;Telephone1', lvAddresses);
  end;
  FAdrsMediator.Subject := FData.AddressList;
  FAdrsMediator.Active := True;
end;

procedure TContactEditForm.btnDebugClicked(Sender: TObject);
begin
  tiShowString(FData.AsDebugString);
end;

procedure TContactEditForm.btnEditClicked(Sender: TObject);
var
  obj: TAddress;
begin
  obj := TAddress(TListViewMediator(FAdrsMediator.FindByComponent(lvAddresses).Mediator).SelectedObject);
//  tiShowString(obj.AsDebugString);

  if not Assigned(obj) then
    Exit; //==>

  if EditAddress(obj) then
  begin
    // do nothing
  end;
end;

procedure TContactEditForm.SetData(const AValue: TContact);
begin
  if FData=AValue then exit;
  FData:=AValue;
  SetupMediators;
end;

procedure TContactEditForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: ContactEditForm}
  Name := 'ContactEditForm';
  SetPosition(513, 423, 537, 331);
  WindowTitle := 'Contact Edit Form';

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(8, 8, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Firstname:';
  end;

  edFName := TfpgEdit.Create(self);
  with edFName do
  begin
    Name := 'edFName';
    SetPosition(8, 24, 216, 22);
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
    Hint := '';
    Text := 'Lastname:';
  end;

  edLName := TfpgEdit.Create(self);
  with edLName do
  begin
    Name := 'edLName';
    SetPosition(8, 68, 216, 22);
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
    Hint := '';
    Text := 'EMail:';
  end;

  edEmail := TfpgEdit.Create(self);
  with edEmail do
  begin
    Name := 'edEmail';
    SetPosition(8, 112, 216, 22);
    TabOrder := 5;
    Text := '';
    FontDesc := '#Edit1';
  end;

  lblName4 := TfpgLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(8, 140, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Mobile:';
  end;

  edMobile := TfpgEdit.Create(self);
  with edMobile do
  begin
    Name := 'edMobile';
    SetPosition(8, 156, 216, 22);
    TabOrder := 7;
    Text := '';
    FontDesc := '#Edit1';
  end;

  lblName5 := TfpgLabel.Create(self);
  with lblName5 do
  begin
    Name := 'lblName5';
    SetPosition(8, 184, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Comments:';
  end;

  meComments := TfpgMemo.Create(self);
  with meComments do
  begin
    Name := 'meComments';
    SetPosition(8, 200, 216, 80);
    FontDesc := '#Edit1';
    TabOrder := 9;
    Lines.text := 'Hello';
  end;

  lblName6 := TfpgLabel.Create(self);
  with lblName6 do
  begin
    Name := 'lblName6';
    SetPosition(264, 8, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Addresses:';
  end;

  lvAddresses := TfpgListView.Create(self);
  with lvAddresses do
  begin
    Name := 'lvAddresses';
    SetPosition(264, 24, 260, 124);
    ShowHeaders := True;
    TabOrder := 11;
  end;

  btnSave := TfpgButton.Create(self);
  with btnSave do
  begin
    Name := 'btnSave';
    SetPosition(364, 300, 80, 24);
    Text := 'Save';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 12;
    ModalResult := mrOK;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(448, 300, 80, 24);
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 13;
    ModalResult := mrCancel;
  end;

  btnAdd := TfpgButton.Create(self);
  with btnAdd do
  begin
    Name := 'btnAdd';
    SetPosition(264, 152, 52, 24);
    Text := 'Add';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 14;
    Enabled := False;
  end;

  btnEdit := TfpgButton.Create(self);
  with btnEdit do
  begin
    Name := 'btnEdit';
    SetPosition(320, 152, 52, 24);
    Text := 'Edit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 15;
    OnClick := @btnEditClicked;
  end;

  btnDelete := TfpgButton.Create(self);
  with btnDelete do
  begin
    Name := 'btnDelete';
    SetPosition(376, 152, 52, 24);
    Text := 'Delete';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 16;
    Enabled := False;
  end;

  btnDebug := TfpgButton.Create(self);
  with btnDebug do
  begin
    Name := 'btnDebug';
    SetPosition(8, 300, 100, 24);
    Text := 'Debug (Show)';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 17;
    OnClick := @btnDebugClicked;
  end;

  {@VFD_BODY_END: ContactEditForm}
end;


end.
