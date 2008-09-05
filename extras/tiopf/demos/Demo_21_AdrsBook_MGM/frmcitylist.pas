unit frmCityList;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, gfxbase, fpgfx, gui_edit, 
  gfx_widget, gui_form, gui_label, gui_button,
  gui_listbox, gui_memo, gui_combobox, gui_basegrid, gui_grid, 
  gui_dialogs, gui_checkbox, gui_tree, gui_trackbar, 
  gui_progressbar, gui_radiobutton, gui_tab, gui_menu,
  gui_panel, gui_popupcalendar, gui_gauge,
  tiFormMediator, model;

type

  TCityListForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: CityListForm}
    bvlName1: TfpgBevel;
    btnAdd: TfpgButton;
    btnEdit: TfpgButton;
    btnDelete: TfpgButton;
    grdName1: TfpgStringGrid;
    btnCancel: TfpgButton;
    {@VFD_HEAD_END: CityListForm}
    FData: TCityList;
    FMediator: TFormMediator;
    procedure SetData(const AValue: TCityList);
    procedure SetupMediators;
  public
    procedure AfterCreate; override;
    procedure AfterConstruction; override;
    property Data: TCityList read FData write SetData;
  end;

{@VFD_NEWFORM_DECL}

procedure ShowCities(const AList: TCityList);


implementation

uses
  tiListMediators;


procedure ShowCities(const AList: TCityList);
var
  frm: TCityListForm;
begin
  frm := TCityListForm.Create(nil);
  try
    frm.SetData(AList);
    frm.ShowModal;
  finally;
    frm.Free;
  end;
end;

{@VFD_NEWFORM_IMPL}

procedure TCityListForm.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TFormMediator.Create(self);
    FMediator.AddComposite('Name(110);Zip(80);CountryAsString(150)', grdName1);
  end;
  FMediator.Subject := FData;
  FMediator.Active := True;
end;

procedure TCityListForm.SetData(const AValue: TCityList);
begin
  if FData=AValue then exit;
  FData:=AValue;
  SetupMediators;
end;

procedure TCityListForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: CityListForm}
  Name := 'CityListForm';
  SetPosition(412, 278, 421, 315);
  WindowTitle := 'City Listing';

  bvlName1 := TfpgBevel.Create(self);
  with bvlName1 do
  begin
    Name := 'bvlName1';
    SetPosition(0, 0, 420, 36);
    Anchors := [anLeft,anRight,anTop];
    Shape := bsSpacer;
  end;

  btnAdd := TfpgButton.Create(bvlName1);
  with btnAdd do
  begin
    Name := 'btnAdd';
    SetPosition(12, 4, 52, 24);
    Text := 'Add';
    FontDesc := '#Label1';
    ImageName := '';
  end;

  btnEdit := TfpgButton.Create(bvlName1);
  with btnEdit do
  begin
    Name := 'btnEdit';
    SetPosition(68, 4, 52, 24);
    Text := 'Edit';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 1;
  end;

  btnDelete := TfpgButton.Create(bvlName1);
  with btnDelete do
  begin
    Name := 'btnDelete';
    SetPosition(124, 4, 52, 24);
    Text := 'Delete';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 2;
  end;

  grdName1 := TfpgStringGrid.Create(self);
  with grdName1 do
  begin
    Name := 'grdName1';
    SetPosition(8, 40, 404, 220);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    TabOrder := 1;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(332, 276, 80, 24);
    Text := 'Cancel';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 2;
    ModalResult := mrOK;
  end;

  {@VFD_BODY_END: CityListForm}
end;

procedure TCityListForm.AfterConstruction;
begin
  inherited AfterConstruction;
//  SetupMediators;
end;


end.
