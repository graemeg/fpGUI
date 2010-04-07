unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_edit,
  fpg_widget, fpg_form, fpg_label, fpg_button,
  fpg_basegrid, fpg_grid, fpg_menu,
  fpg_panel, fpg_popupcalendar, fpg_gauge, Model, tiListMediators;

type

  TMainForm = class(TfpgForm)
  private
    FList: TPersonList;
    medGrid: TtiStringGridMediatorView;
    procedure   SetupMediators;
    procedure   ValidateData;
    procedure   btnAddClicked(Sender: TObject);
    procedure   btnEditClicked(Sender: TObject);
    procedure   btnDeleteClicked(Sender: TObject);
    procedure   btnUpdateClicked(Sender: TObject);
    procedure   btnQuitClicked(Sender: TObject);
    procedure   btnRetrieveClicked(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    grdName1: TfpgStringGrid;
    lblName1: TfpgLabel;
    edtName: TfpgEdit;
    lblName2: TfpgLabel;
    edtAge: TfpgEdit;
    btnQuit: TfpgButton;
    btnUpdate: TfpgButton;
    btnAdd: TfpgButton;
    btnEdit: TfpgButton;
    btnDelete: TfpgButton;
    btnRetrieve: TfpgButton;
    lblName3: TfpgLabel;
    {@VFD_HEAD_END: MainForm}
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  fpg_dialogs, frm_personmaint;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.SetupMediators;
begin
  medGrid := TtiStringGridMediatorView.CreateCustom(FList, grdName1, 'Name(200);Age');
end;

procedure TMainForm.ValidateData;
var
  i: integer;
begin
  try
    i := StrToInt(edtAge.Text);
    if (i < 1) or (i > 100) then
    begin
      TfpgMessageDialog.Warning('Age out of range', 'Age must be between 1 and 100');
      Abort;
    end;
  except
    on E: Exception do
      TfpgMessageDialog.Critical('Age must be a numeric value', E.Message);
  end;
end;

procedure TMainForm.btnAddClicked(Sender: TObject);
var
  lData: TPerson;
begin
  lData := TPerson.CreateNew;
  lData.Name := 'New Name';
  lData.Age := 12;
  FList.Add(lData);
end;

procedure TMainForm.btnEditClicked(Sender: TObject);
begin
  if medGrid.SelectedObject = nil then
    Exit;
  EditPerson(TPerson(medGrid.SelectedObject));
end;

procedure TMainForm.btnDeleteClicked(Sender: TObject);
begin
  medGrid.SelectedObject.Deleted := True;
  FList.NotifyObservers;
end;

procedure TMainForm.btnUpdateClicked(Sender: TObject);
var
  lData: TPerson;
begin
  ValidateData;
  lData := medGrid.SelectedObject as TPerson;
  lData.BeginUpdate;
  lData.Name  := edtName.Text;
  lData.Age   := StrToInt(edtAge.Text);
  lData.EndUpdate;
end;

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnRetrieveClicked(Sender: TObject);
var
  lData: TPerson;
begin
  lData := medGrid.SelectedObject as TPerson;
  if lData = nil then
    Exit;
  edtName.Text  := lData.Name;
  edtAge.Text   := IntToStr(lData.Age);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := GeneratePersonList;
end;

destructor TMainForm.Destroy;
begin
  medGrid.Free;
  FList.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(308, 203, 463, 265);
  WindowTitle := 'StringGrid Mediator Demo';

  grdName1 := TfpgStringGrid.Create(self);
  with grdName1 do
  begin
    Name := 'grdName1';
    SetPosition(8, 28, 272, 172);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(332, 80, 80, 15);
    FontDesc := '#Label1';
    Text := 'Name:';
  end;

  edtName := TfpgEdit.Create(self);
  with edtName do
  begin
    Name := 'edtName';
    SetPosition(332, 96, 120, 21);
    TabOrder := 2;
    Text := '';
    FontDesc := '#Edit1';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(332, 120, 80, 15);
    FontDesc := '#Label1';
    Text := 'Age:';
  end;

  edtAge := TfpgEdit.Create(self);
  with edtAge do
  begin
    Name := 'edtAge';
    SetPosition(332, 136, 52, 21);
    TabOrder := 4;
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(376, 236, 80, 23);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    ImageName := 'stdimg.quit';
    TabOrder := 5;
    OnClick := @btnQuitClicked;
  end;

  btnUpdate := TfpgButton.Create(self);
  with btnUpdate do
  begin
    Name := 'btnUpdate';
    SetPosition(356, 168, 80, 23);
    Text := 'Update';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 6;
    OnClick := @btnUpdateClicked;
  end;

  btnAdd := TfpgButton.Create(self);
  with btnAdd do
  begin
    Name := 'btnAdd';
    SetPosition(8, 4, 48, 20);
    Text := 'add';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 7;
    OnClick := @btnAddClicked;
//    Enabled := False;
  end;

  btnEdit := TfpgButton.Create(self);
  with btnEdit do
  begin
    Name := 'btnEdit';
    SetPosition(60, 4, 48, 20);
    Text := 'edit';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 8;
    OnClick := @btnEditClicked;
  end;

  btnDelete := TfpgButton.Create(self);
  with btnDelete do
  begin
    Name := 'btnDelete';
    SetPosition(112, 4, 48, 20);
    Text := 'delete';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 9;
    OnClick := @btnDeleteClicked;
//    Enabled := False;
  end;

  btnRetrieve := TfpgButton.Create(self);
  with btnRetrieve do
  begin
    Name := 'btnRetrieve';
    SetPosition(288, 84, 28, 23);
    Text := '>>';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 10;
    OnClick := @btnRetrieveClicked;
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(288, 0, 167, 70);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    Text := 'The controls below allow you to manually update the selected object. No mediators are used.';
    WrapText := True;
  end;

  {@VFD_BODY_END: MainForm}
  
  SetupMediators;
end;


end.
