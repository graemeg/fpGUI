unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_widget, fpg_form, fpg_button,
  fpg_grid, fpg_checkbox, fpg_panel, fpg_listview, fpg_listbox, fpg_combobox,
  Model, tiModelMediator, tiMediators;

type

  TMainForm = class(TfpgForm)
  private
    { The object we will be working with. }
    FPersonList: TPersonList;
    FMediator: TtiModelMediator;
    medCombo: TtiComboBoxMediatorView;
    procedure btnViaCodeChangeClick(Sender: TObject);
    procedure btnQuitClicked(Sender: TObject);
    procedure btnViaCodeAddClick(Sender: TObject);
    procedure btnShowModelClick(Sender: TObject);
    procedure btnShowDeletedClick(Sender: TObject);
    procedure SetupMediators;
  public
    {@VFD_HEAD_BEGIN: MainForm}
    lvName1: TfpgListView;
    grdName1: TfpgStringGrid;
    pnlName1: TfpgPanel;
    btnName1: TfpgButton;
    btnName2: TfpgButton;
    btnName3: TfpgButton;
    btnName4: TfpgButton;
    btnQuit: TfpgButton;
    cbName1: TfpgCheckBox;
    lstName1: TfpgListBox;
    cbName2: TfpgComboBox;
    {@VFD_HEAD_END: MainForm}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  tiBaseMediator, tiListMediators, tiObject, tiDialogs;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.btnViaCodeChangeClick(Sender: TObject);
begin
  { The BeginUpdate/EndUpdate will let the Item notify its observers
    only once, even though two change where made.
    Note:
    This is for observers to the Item, not the List that the Item belongs to! }
  FPersonList.Items[1].BeginUpdate;
  FPersonList.Items[1].Name := 'I have changed via code';
  FPersonList.Items[1].Age  := 99;
  FPersonList.Items[1].EndUpdate;
  { This notifies observers of the List, that something has changed. }
  FPersonList.NotifyObservers;
end;

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnViaCodeAddClick(Sender: TObject);
var
  lData: TPerson;
begin
  lData := TPerson.Create;
  lData.Name := 'I am new';
  lData.Age := 44;
  FPersonList.Add(lData);
end;

procedure TMainForm.btnShowModelClick(Sender: TObject);
begin
  tiShowString(FPersonList.AsDebugString);
end;

procedure TMainForm.btnShowDeletedClick(Sender: TObject);
var
  med: TtiMediatorView;
begin
  med := FMediator.FindByComponent(grdName1).Mediator;
  tiShowString(TtiStringGridMediatorView(med).SelectedObject.AsDebugString);
end;

procedure TMainForm.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(self);
    FMediator.Name := 'DemoFormMediator';
    FMediator.AddComposite('Name(150,"Name",<);Age(50,"Age",>);GenderGUI(80,"Gender",|)', grdName1);
//    FMediator.AddComposite('Name(150,"Name",<);Age(75,"Age",>);GenderGUI(50,"Gender",|)', lvName1);
    FMediator.AddProperty('Name', lstName1);
  end;
  FMediator.Subject := FPersonList;
  FMediator.Active := True;

//  medCombo := TMediatorComboBoxView.CreateCustom(cbName1, FPerson);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPersonList := GeneratePersonList;
end;

destructor TMainForm.Destroy;
begin
  FMediator.Active := False;
  FPersonList.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterConstruction;
begin
  inherited AfterConstruction;
  SetupMediators;
  FPersonList.NotifyObservers;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(298, 184, 617, 358);
  WindowTitle := 'Demo 20: ListMediators';
  Sizeable := False;

  lvName1 := TfpgListView.Create(self);
  with lvName1 do
  begin
    Name := 'lvName1';
    SetPosition(8, 52, 292, 112);
    ShowHeaders := True;
  end;

  grdName1 := TfpgStringGrid.Create(self);
  with grdName1 do
  begin
    Name := 'grdName1';
    SetPosition(316, 52, 292, 220);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    TabOrder := 1;
    Tag := 99;
  end;

  pnlName1 := TfpgPanel.Create(self);
  with pnlName1 do
  begin
    Name := 'pnlName1';
    SetPosition(8, 12, 600, 28);
    Anchors := [anLeft,anRight,anTop];
    Alignment := taLeftJustify;
    Margin := 8;
    Text := 'ListMediator Demo';
    FontDesc := '#Label2';
    TextColor := clWhite;
    BackGroundColor := clDarkBlue;
  end;

  btnName1 := TfpgButton.Create(self);
  with btnName1 do
  begin
    Name := 'btnName1';
    SetPosition(8, 296, 120, 24);
    Text := 'Add via Code';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnViaCodeAddClick;
  end;

  btnName2 := TfpgButton.Create(self);
  with btnName2 do
  begin
    Name := 'btnName2';
    SetPosition(8, 324, 120, 24);
    Text := 'Change via Code';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
    OnClick := @btnViaCodeChangeClick;
  end;

  btnName3 := TfpgButton.Create(self);
  with btnName3 do
  begin
    Name := 'btnName3';
    SetPosition(132, 324, 96, 24);
    Text := 'Show Model';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 5;
    OnClick := @btnShowModelClick;
  end;

  btnName4 := TfpgButton.Create(self);
  with btnName4 do
  begin
    Name := 'btnName4';
    SetPosition(232, 324, 80, 24);
    Text := 'Delete';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 6;
    OnClick := @btnShowDeletedClick;
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(528, 324, 80, 24);
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 7;
    OnClick := @btnQuitClicked;
  end;

  cbName1 := TfpgCheckBox.Create(self);
  with cbName1 do
  begin
    Name := 'cbName1';
    SetPosition(148, 296, 120, 20);
    FontDesc := '#Label1';
    TabOrder := 8;
    Text := 'Show Deleted';
    OnClick := @btnShowDeletedClick;
  end;

  lstName1 := TfpgListBox.Create(self);
  with lstName1 do
  begin
    Name := 'lstName1';
    SetPosition(8, 168, 228, 120);
    FontDesc := '#List';
    TabOrder := 9;
  end;

  cbName2 := TfpgComboBox.Create(self);
  with cbName2 do
  begin
    Name := 'cbName2';
    SetPosition(320, 276, 204, 22);
    FontDesc := '#List';
    TabOrder := 10;
  end;

  {@VFD_BODY_END: MainForm}
end;


initialization
  gMediatorManager.RegisterMediator(TtiStringGridMediatorView, TtiObjectList);
//  gMediatorManager.RegisterMediator(TListBoxMediator, TPersonList);
//  gMediatorManager.RegisterMediator(TListViewMediator, TtiObjectList);

end.
