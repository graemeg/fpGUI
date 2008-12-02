unit frm_pickdatabase;

{$mode objfpc}{$H+}

interface

uses
  SysUtils
  ,Classes
  ,contnrs
  // fpGUI
  ,fpg_base
  ,fpg_main
  ,fpg_form
  ,fpg_button
  ,fpg_edit
  ,fpg_combobox
  ,fpg_tab
  ,fpg_label
  // tiOPF
  ,tiPersistenceLayers
  ;


const
  cINIIdentLastPerLayer = 'LastPerLayer';

type

  TPickDatabaseForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: PickDatabaseForm}
    PageControl1: TfpgPageControl;
    tsDatabase: TfpgTabSheet;
    btnReset: TfpgButton;
    Label1: TfpgLabel;
    cbPersistenceLayer: TfpgComboBox;
    Label2: TfpgLabel;
    edtDatabaseName: TfpgEdit;
    Label3: TfpgLabel;
    edtUsername: TfpgEdit;
    Label4: TfpgLabel;
    edtPassword: TfpgEdit;
    btnDone: TfpgButton;
    {@VFD_HEAD_END: PickDatabaseForm}
    FSingleUserPersistenceLayers: TObjectList;
    function GetDatabaseName: string;
    function GetPassword: string;
    function GetPersistenceLayerName: string;
    function GetUserName: string;
    procedure SetPersistenceLayer(const APersistenceLayerName: string);
    procedure RegisterPersistenceLayersAsTests;
    procedure RegisterPersistenceLayerAsTest(const APersistenceLayer: TtiPersistenceLayer);
    procedure FormShow(Sender: TObject);
    procedure PersistenceLayerChanged(Sender: TObject);
    procedure DefaultToPresetValuesClick(Sender: TObject);
  protected
    function  GetDataDir: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
    property  SingleUserPersistenceLayers: TObjectList read FSingleUserPersistenceLayers;
    property  PersistenceLayerName: string  read GetPersistenceLayerName;
    property  DatabaseName        : string  read GetDatabaseName;
    property  UserName            : string  read GetUserName;
    property  Password            : string  read GetPassword;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  tiConstants
  ,tiUtils
  ,tiOPFManager
  ,tiINI
  ;

{@VFD_NEWFORM_IMPL}

function TPickDatabaseForm.GetDatabaseName: string;
begin
  Result := edtDatabaseName.Text;
end;

function TPickDatabaseForm.GetPassword: string;
begin
  Result := edtPassword.Text;
end;

function TPickDatabaseForm.GetPersistenceLayerName: string;
begin
  Result := cbPersistenceLayer.Text;
end;

function TPickDatabaseForm.GetUserName: string;
begin
  Result := edtUsername.Text;
end;

procedure TPickDatabaseForm.SetPersistenceLayer(const APersistenceLayerName: string);
var
  LPL: TtiPersistenceLayer;
  LDefaults: TtiPersistenceLayerDefaults;
begin
  LPL:= GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(APersistenceLayerName);
  if LPL<>nil then
  begin
    LDefaults:= TtiPersistenceLayerDefaults.Create;
    try
      LPL.AssignPersistenceLayerDefaults(LDefaults);
      cbPersistenceLayer.Text := LDefaults.PersistenceLayerName;
      edtDatabaseName.Text    := ExpandFileName(GetDataDir + LDefaults.DatabaseName);
      edtUserName.Text        := LDefaults.UserName;
      edtPassword.Text        := LDefaults.Password;
      gINI.WriteString(Name, cINIIdentLastPerLayer, LDefaults.PersistenceLayerName);
    finally
      LDefaults.Free;
    end;
  end else
  begin
    cbPersistenceLayer.FocusItem  := -1;
    edtDatabaseName.Text          := '';
    edtUserName.Text              := '';
    edtPassword.Text              := '';
  end;
end;

procedure TPickDatabaseForm.RegisterPersistenceLayersAsTests;
var
  i: integer;
begin
  for i := 0 to GTIOPFManager.PersistenceLayers.Count - 1 do
    RegisterPersistenceLayerAsTest(GTIOPFManager.PersistenceLayers.Items[i]);
end;

procedure TPickDatabaseForm.RegisterPersistenceLayerAsTest(const APersistenceLayer: TtiPersistenceLayer);
var
  LDefaults: TtiPersistenceLayerDefaults;
begin
  Assert(APersistenceLayer.TestValid, CTIErrorInvalidObject);
  LDefaults:= TtiPersistenceLayerDefaults.Create;
  try
    APersistenceLayer.AssignPersistenceLayerDefaults(LDefaults);
    cbPersistenceLayer.Items.Add(LDefaults.PersistenceLayerName);
  finally
    LDefaults.Free;
  end;
end;

procedure TPickDatabaseForm.FormShow(Sender: TObject);
var
  lLastPerLayer: string;
begin
  PageControl1.ActivePageIndex := 0;
  RegisterPersistenceLayersAsTests;
  lLastPerLayer := gINI.ReadString(Name, 'LastPerLayer', '');
  SetPersistenceLayer(lLastPerLayer);
end;

procedure TPickDatabaseForm.PersistenceLayerChanged(Sender: TObject);
begin
  SetPersistenceLayer(cbPersistenceLayer.Text);
end;

procedure TPickDatabaseForm.DefaultToPresetValuesClick(Sender: TObject);
begin
  SetPersistenceLayer(cbPersistenceLayer.Text);
end;

function TPickDatabaseForm.GetDataDir: string;
var
  dir: string;
begin
  dir := tiAddTrailingSlash(tiGetAppDataDirPrivate) + '_Data\';
  result := ExpandFileName(tiFixPathDelim(dir));
end;

constructor TPickDatabaseForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnShow := @FormShow;
  FSingleUserPersistenceLayers := TObjectList.Create(False);
end;

procedure TPickDatabaseForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: PickDatabaseForm}
  Name := 'PickDatabaseForm';
  SetPosition(301, 186, 464, 248);
  WindowTitle := 'Pick Database';

  PageControl1 := TfpgPageControl.Create(self);
  with PageControl1 do
  begin
    Name := 'PageControl1';
    SetPosition(8, 12, 446, 224);
    Anchors := [anLeft,anRight,anTop,anBottom];
    ActivePageIndex := 0;
    ParentShowHint := True;
    TabOrder := 0;
  end;

  tsDatabase := TfpgTabSheet.Create(PageControl1);
  with tsDatabase do
  begin
    Name := 'tsDatabase';
    SetPosition(3, 24, 440, 197);
    Text := 'Database';
  end;

  btnReset := TfpgButton.Create(tsDatabase);
  with btnReset do
  begin
    Name := 'btnReset';
    SetPosition(296, 24, 132, 24);
    Text := 'Reset to Defaults';
    AllowAllUp := False;
    Embedded := False;
    Flat := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    ImageLayout := ilImageLeft;
    ImageMargin := 3;
    ImageName := '';
    ImageSpacing := -1;
    ModalResult := 0;
    ParentShowHint := True;
    ShowImage := True;
    TabOrder := 0;
    OnClick := @DefaultToPresetValuesClick;
  end;

  Label1 := TfpgLabel.Create(tsDatabase);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(8, 4, 220, 18);
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Layout := tlTop;
    ParentShowHint := True;
    Text := 'Persistence Layer';
    WrapText := False;
  end;

  cbPersistenceLayer := TfpgComboBox.Create(tsDatabase);
  with cbPersistenceLayer do
  begin
    Name := 'cbPersistenceLayer';
    SetPosition(8, 24, 268, 24);
    FontDesc := '#List';
    ParentShowHint := True;
    TabOrder := 2;
    OnChange :=@PersistenceLayerChanged;
  end;

  Label2 := TfpgLabel.Create(tsDatabase);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(8, 50, 232, 18);
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Layout := tlTop;
    ParentShowHint := True;
    Text := 'Database Name';
    WrapText := False;
  end;

  edtDatabaseName := TfpgEdit.Create(tsDatabase);
  with edtDatabaseName do
  begin
    Name := 'edtDatabaseName';
    SetPosition(8, 68, 268, 24);
    TabOrder := 4;
    Text := '';
    FontDesc := '#Edit1';
    ParentShowHint := True;
  end;

  Label3 := TfpgLabel.Create(tsDatabase);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(8, 94, 232, 18);
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Layout := tlTop;
    ParentShowHint := True;
    Text := 'Username';
    WrapText := False;
  end;

  edtUsername := TfpgEdit.Create(tsDatabase);
  with edtUsername do
  begin
    Name := 'edtUsername';
    SetPosition(8, 112, 268, 24);
    TabOrder := 6;
    Text := '';
    FontDesc := '#Edit1';
    ParentShowHint := True;
  end;

  Label4 := TfpgLabel.Create(tsDatabase);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(8, 138, 232, 18);
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Layout := tlTop;
    ParentShowHint := True;
    Text := 'Password';
    WrapText := False;
  end;

  edtPassword := TfpgEdit.Create(tsDatabase);
  with edtPassword do
  begin
    Name := 'edtPassword';
    SetPosition(8, 156, 268, 24);
    TabOrder := 8;
    Text := '';
    FontDesc := '#Edit1';
    ParentShowHint := True;
  end;

  btnDone := TfpgButton.Create(tsDatabase);
  with btnDone do
  begin
    Name := 'btnDone';
    SetPosition(296, 156, 132, 24);
    Text := 'DONE';
    AllowAllUp := False;
    Embedded := False;
    Flat := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    ImageLayout := ilImageLeft;
    ImageMargin := 3;
    ImageName := '';
    ImageSpacing := -1;
    ModalResult := 0;
    ParentShowHint := True;
    ShowImage := True;
    TabOrder := 11;
    ModalResult := mrOK;
  end;

  {@VFD_BODY_END: PickDatabaseForm}
end;


end.
