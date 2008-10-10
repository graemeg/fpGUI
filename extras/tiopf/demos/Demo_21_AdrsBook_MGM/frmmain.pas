unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_edit,
  fpg_widget, fpg_form, fpg_label, fpg_button,
  fpg_basegrid, fpg_grid, fpg_dialogs, fpg_menu,
  fpg_panel, fpg_popupcalendar, fpg_gauge, tiFormMediator;

type
  { The main application window }
  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    grdContacts: TfpgStringGrid;
    btnAdd: TfpgButton;
    btnEdit: TfpgButton;
    btnDelete: TfpgButton;
    MainMenu: TfpgMenuBar;
    miFile: TfpgPopupMenu;
    miEdit: TfpgPopupMenu;
    miSystem: TfpgPopupMenu;
    {@VFD_HEAD_END: MainForm}
    FMediator: TFormMediator;
    procedure FormShow(Sender: TObject);
    procedure GridDoubleClicked(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure SetupMediators;
    procedure miEditAddClick(Sender: TObject);
    procedure miEditEditClick(Sender: TObject);
    procedure miEditDeleteClick(Sender: TObject);
    procedure miSystemCityList(Sender: TObject);
    procedure miSystemCountryList(Sender: TObject);
    procedure miSystemAddressTypeList(Sender: TObject);
    procedure miFileExit(Sender: TObject);
  public
    procedure AfterCreate; override;
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  model, contactmanager, tiListMediators, tiBaseMediator, tiMediators,
  frmcontactmaint, frmcitylist, frmcountrylist;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.FormShow(Sender: TObject);
begin
// do nothing yet
end;

procedure TMainForm.GridDoubleClicked(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  miEditEditClick(nil);
end;

procedure TMainForm.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TFormMediator.Create(self);
    FMediator.AddComposite('FirstName;LastName(130);EMail(180);Mobile(130);Comments(200)', grdContacts);
  end;
  FMediator.Subject := gContactManager.ContactList;
  FMediator.Active := True;
end;

procedure TMainForm.miEditAddClick(Sender: TObject);
var
  c: TContact;
begin
  c := TContact.CreateNew;
  if EditContact(c) then
    gContactManager.ContactList.Add(c)
  else
    c.Free;
end;

procedure TMainForm.miEditEditClick(Sender: TObject);
var
  c: TContact;
begin
  c := TContact(TStringGridMediator(FMediator.FindByComponent(grdContacts).Mediator).SelectedObject);
//  tiShowString(c.AsDebugString);

  if not Assigned(c) then
    Exit; //==>

  if EditContact(c) then
  begin
    // we can save contact here
  end;
end;

procedure TMainForm.miEditDeleteClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.miSystemCityList(Sender: TObject);
begin
  ShowCities(gContactManager.CityList);
end;

procedure TMainForm.miSystemCountryList(Sender: TObject);
begin
  ShowCountries(gContactManager.CountryList);
end;

procedure TMainForm.miSystemAddressTypeList(Sender: TObject);
begin
  // ShowAddressTypeList(gContactManager.AddressTypeList);
end;

procedure TMainForm.miFileExit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(373, 273, 540, 404);
  WindowTitle := 'Demo 21: Address Book Demo using MGM';

  grdContacts := TfpgStringGrid.Create(self);
  with grdContacts do
  begin
    Name := 'grdContacts';
    SetPosition(12, 56, 516, 336);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    OnDoubleClick := @GridDoubleClicked;
  end;

  btnAdd := TfpgButton.Create(self);
  with btnAdd do
  begin
    Name := 'btnAdd';
    SetPosition(12, 28, 52, 24);
    Text := 'Add';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick := @miEditAddClick;
  end;

  btnEdit := TfpgButton.Create(self);
  with btnEdit do
  begin
    Name := 'btnEdit';
    SetPosition(68, 28, 52, 24);
    Text := 'Edit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
    OnClick := @miEditEditClick;
  end;

  btnDelete := TfpgButton.Create(self);
  with btnDelete do
  begin
    Name := 'btnDelete';
    SetPosition(124, 28, 52, 24);
    Text := 'Delete';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @miEditDeleteClick;
    Enabled := False;
  end;

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 540, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  miFile := TfpgPopupMenu.Create(self);
  with miFile do
  begin
    Name := 'miFile';
    SetPosition(344, 136, 120, 20);
    AddMenuItem('E&xit', 'Alt+F4', @miFileExit);
  end;

  miEdit := TfpgPopupMenu.Create(self);
  with miEdit do
  begin
    Name := 'miEdit';
    SetPosition(344, 156, 120, 20);
    AddMenuItem('Add Contact', '', @miEditAddClick).Enabled := False;
    AddMenuItem('Edit Contact', '', @miEditEditClick);
    AddMenuItem('Delete Contact', '', @miEditDeleteClick).Enabled := False;
  end;

  miSystem := TfpgPopupMenu.Create(self);
  with miSystem do
  begin
    Name := 'miSystem';
    SetPosition(344, 176, 120, 20);
    AddMenuItem('City List', '', @miSystemCityList);
    AddMenuItem('Country List', '', @miSystemCountryList);
    AddMenuItem('Address Type List', '', @miSystemAddressTypeList).Enabled := False;
  end;

  {@VFD_BODY_END: MainForm}

  // setup main menu
  MainMenu.AddMenuItem('&File', nil).SubMenu := miFile;
  MainMenu.AddMenuItem('&Edit', nil).SubMenu := miEdit;
  MainMenu.AddMenuItem('&System', nil).SubMenu := miSystem;
end;

procedure TMainForm.AfterConstruction;
begin
  inherited AfterConstruction;
  SetupMediators;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  gContactManager.PopulateContacts;
  OnShow := @FormShow;
end;


initialization
  RegisterFallBackMediators;

  gMediatorManager.RegisterMediator(TStringGridMediator, TContactList);
  gMediatorManager.RegisterMediator(TListViewMediator, TAddressList);
  gMediatorManager.RegisterMediator(TStringGridMediator, TCityList);
  gMediatorManager.RegisterMediator(TStringGridMediator, TCountryList);
  gMediatorManager.RegisterMediator(TMediatorDynamicComboBoxView, TCity, 'Country');
  gMediatorManager.RegisterMediator(TMediatorDynamicComboBoxView, TAddressType, 'AddressType');

end.
