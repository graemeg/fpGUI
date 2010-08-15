unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_form, fpg_button, fpg_label, fpg_edit, fpg_trackbar,
  fpg_combobox, fpg_memo, Model, tiModelMediator, Model_View;

type
  TMainForm = class(TfpgForm)
  private
    btnClose: TfpgButton;
    btnViaCode: TfpgButton;
    btnShowModel: TfpgButton;
    Label1: TfpgLabel;
    Label2: TfpgLabel;
    edtName: TfpgEdit;
    AgeTrackBar: TfpgTrackBar;
    memMemo: TfpgMemo;
    cbGender: TfpgComboBox;
    Label3: TfpgLabel;
    
    { The object we will be working with. }
    FPerson: TPerson;
    { Form Mediator }
    FMediator: TtiModelMediator;

    procedure   btnCloseClick(Sender: TObject);
    procedure   btnShowModelClick(Sender: TObject);
    procedure   btnViaCodeClick(Sender: TObject);
    procedure   InitializeComponents;
    procedure   SetupMediators;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterConstruction; override;
  end;
  
implementation

uses
  fpg_dialogs
  ;

{ TMainForm }

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnShowModelClick(Sender: TObject);
begin
  ShowMessage(FPerson.AsDebugString);
end;

{ The controls will automatically update as well! }
procedure TMainForm.btnViaCodeClick(Sender: TObject);
begin
  FPerson.Name    := 'John Doe';
  FPerson.Age     := 23;
  FPerson.Gender  := genFemale;
end;

procedure TMainForm.InitializeComponents;
begin
  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Left := 8;
    Height := 17;
    Top := 16;
    Width := 49;
    Text := 'Name:';
  end;
  
  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Left := 8;
    Height := 17;
    Top := 48;
    Width := 34;
    Text := 'Age:';
  end;
  
  Label3 := TfpgLabel.Create(self);
  with Label3 do
  begin
    Left := 264;
    Height := 17;
    Top := 16;
    Width := 90;
    Text := '(Read-Only)';
  end;
  
  btnClose := TfpgButton.Create(self);
  with btnClose do
  begin
    Left := 416;
    Height := 25;
    Top := 160;
    Width := 75;
    Text := 'Close';
    OnClick := @btnCloseClick;
    ImageName := 'stdimg.Close';
    ShowImage := True;
    TabOrder := 0;
  end;

  btnViaCode := TfpgButton.Create(self);
  with btnViaCode do
  begin
    Left := 7;
    Height := 25;
    Top := 160;
    Width := 150;
    Text := 'Change via Code';
    OnClick := @btnViaCodeClick;
    TabOrder := 1;
  end;
  
  btnShowModel := TfpgButton.Create(self);
  with btnShowModel do
  begin
    Left := 164;
    Height := 25;
    Top := 160;
    Width := 100;
    Text := 'Show Model';
    OnClick := @btnShowModelClick;
    TabOrder := 2;
  end;
  
  edtName := TfpgEdit.Create(self);
  with edtName do
  begin
    Left := 64;
    Height := 21;
    Top := 16;
    Width := 150;
    TabOrder := 3;
  end;
  
  AgeTrackBar := TfpgTrackBar.Create(self);
  with AgeTrackBar do
  begin
    Left := 64;
    Height := 41;
    Top := 72;
    Width := 150;
    Max := 100;
    ShowPosition := True;
    TabOrder := 5;
  end;
  
  memMemo := TfpgMemo.Create(self);
  with memMemo do
  begin
    Left := 264;
    Height := 57;
    Top := 32;
    Width := 185;
    TabOrder := 6;
  end;
  
  cbGender := TfpgComboBox.Create(self);
  with cbGender do
  begin
    Left := 264;
    Height := 21;
    Top := 96;
    TabOrder := 7;
  end;
end;

procedure TMainForm.SetupMediators;
begin
{
  FComboBoxGenderMediator := TPerson_Gender_ComboBox_Mediator.CreateCustom(cbGender, FPerson, 'GenderGUI', 'Text');
}
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(self);
    FMediator.Name := 'DemoFormMediator';
    FMediator.AddProperty('Name', edtName);
    FMediator.AddProperty('Age', AgeTrackBar);
    FMediator.AddProperty('Name', memMemo);
    FMediator.AddProperty('GenderGUI', cbGender);
  end;
  FMediator.Subject := FPerson;
  FMediator.Active := True;
end;

constructor TMainForm.Create(AOwner: TComponent);
var
  i: TGender;
begin
  inherited Create(AOwner);
  WindowTitle := 'Edit Mediators Demo';
  WindowPosition := wpUser;
  SetPosition(100, 100, 500, 200);

  { The Data Object being observed }
  FPerson       := TPerson.Create;
  FPerson.Name  := 'Graeme Geldenhuys';
  FPerson.Age   := 32;

  InitializeComponents;

  for i := Low(TGender) to High(TGender) do
    cbGender.Items.Add(cGender[i]);
end;

destructor TMainForm.Destroy;
begin
  // TFormMediator (FMediator) get free'd automatically because it's a TComponent
  FPerson.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterConstruction;
begin
  inherited AfterConstruction;
  { The only trick here is to not let the OnChange events fire
    before the mediators are not set up!! }
  SetupMediators;

  // This will cause all components to update at once
  FPerson.NotifyObservers;
end;


end.

