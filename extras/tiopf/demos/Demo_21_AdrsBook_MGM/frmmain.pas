unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, gfxbase, fpgfx, gui_edit, 
  gfx_widget, gui_form, gui_label, gui_button,
  gui_listbox, gui_memo, gui_combobox, gui_basegrid, gui_grid, 
  gui_dialogs, gui_checkbox, gui_tree, gui_trackbar, 
  gui_progressbar, gui_radiobutton, gui_tab, gui_menu,
  gui_panel, gui_popupcalendar, gui_gauge, tiFormMediator;

type

  TMainForm = class(TfpgForm)
  private
    FMediator: TFormMediator;

    {@VFD_HEAD_BEGIN: MainForm}
    grdContacts: TfpgStringGrid;
    btnAdd: TfpgButton;
    btnEdit: TfpgButton;
    btnDelete: TfpgButton;
    btnCityList: TfpgButton;
    btnName5: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    procedure FormShow(Sender: TObject);
    procedure SetupMediators;
    procedure miEditEditClick(Sender: TObject);
    procedure btnShowCityList(Sender: TObject);
  public
    procedure AfterCreate; override;
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  model, contactmanager, tiListMediators, tiBaseMediator, frmcontactmaint,
  frmcitylist, tiDialogs;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.FormShow(Sender: TObject);
begin
// do nothing yet
end;

procedure TMainForm.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TFormMediator.Create(self);
    FMediator.AddComposite('FirstName;LastName(130);EMail(180);Mobile(130);Comments', grdContacts);
  end;
  FMediator.Subject := gContactManager.ContactList;
  FMediator.Active := True;
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

procedure TMainForm.btnShowCityList(Sender: TObject);
begin
  ShowCities(gContactManager.CityList);
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(373, 273, 708, 456);
  WindowTitle := 'Demo 21: Address Book Demo using MGM';

  grdContacts := TfpgStringGrid.Create(self);
  with grdContacts do
  begin
    Name := 'grdContacts';
    SetPosition(20, 36, 516, 336);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
  end;

  btnAdd := TfpgButton.Create(self);
  with btnAdd do
  begin
    Name := 'btnAdd';
    SetPosition(572, 48, 80, 24);
    Text := 'Add';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 1;
  end;

  btnEdit := TfpgButton.Create(self);
  with btnEdit do
  begin
    Name := 'btnEdit';
    SetPosition(572, 76, 80, 24);
    Text := 'Edit';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 2;
    OnClick := @miEditEditClick;
  end;

  btnDelete := TfpgButton.Create(self);
  with btnDelete do
  begin
    Name := 'btnDelete';
    SetPosition(572, 104, 80, 24);
    Text := 'Delete';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 3;
  end;

  btnCityList := TfpgButton.Create(self);
  with btnCityList do
  begin
    Name := 'btnCityList';
    SetPosition(576, 164, 80, 24);
    Text := 'City List';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 4;
    OnClick := @btnShowCityList;
  end;

  btnName5 := TfpgButton.Create(self);
  with btnName5 do
  begin
    Name := 'btnName5';
    SetPosition(576, 192, 80, 24);
    Text := 'Button';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 5;
  end;

  {@VFD_BODY_END: MainForm}
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
  gMediatorManager.RegisterMediator(TStringGridMediator, TContactList);
  gMediatorManager.RegisterMediator(TListViewMediator, TAddressList);
  gMediatorManager.RegisterMediator(TStringGridMediator, TCityList);

end.
