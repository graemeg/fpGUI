unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_button, fpg_panel;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    btnPickDB: TfpgButton;
    btnCreateDB: TfpgButton;
    btnDBExists: TfpgButton;
    pnlDBName: TfpgPanel;
    {@VFD_HEAD_END: MainForm}
    FPersistenceLayerName: string;
    FDatabaseName: string;
    FUsername: string;
    FPassword: string;
    procedure PickDBClicked(Sender: TObject);
    procedure DatabaseExistsClicked(Sender: TObject);
    procedure CreateDatabaseClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  frm_pickdatabase
  ,fpg_dialogs
  ,tiOPFManager
  ,tiPersistenceLayers
  ,tiDialogs
  ;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.PickDBClicked(Sender: TObject);
var
  frm: TPickDatabaseForm;
begin
  frm := TPickDatabaseForm.Create(nil);
  try
    if frm.ShowModal = mrOK then
    begin
      FPersistenceLayerName := frm.PersistenceLayerName;
      FDatabaseName := frm.DatabaseName;
      FUsername := frm.UserName;
      FPassword := frm.Password;
      pnlDBName.Text := frm.DatabaseName;
    end;
  finally
    frm.Free;
  end;
end;

procedure TMainForm.DatabaseExistsClicked(Sender: TObject);
var
  LPerLayer: TtiPersistenceLayer;
begin
  LPerLayer:= GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(FPersistenceLayerName);
  Assert(LPerLayer<>nil, '"' + FPersistenceLayerName + '" not registered');
  if LPerLayer.DatabaseExists(FDatabaseName, FUserName, FPassword)
  then
    tiAppMessage('Database <' + FDatabaseName + '> exists.')
  else
    tiAppWarning('Database <' + FDatabaseName + '> does not exist.');
end;

procedure TMainForm.CreateDatabaseClicked(Sender: TObject);
var
  LPerLayer: TtiPersistenceLayer;
begin
  LPerLayer:= GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(FPersistenceLayerName);
  Assert(LPerLayer<>nil, '"' + FPersistenceLayerName + '" not registered');
  LPerLayer.CreateDatabase(FDatabaseName, FUserName, FPassword);
  tiAppMessage('Database "' + FDatabaseName + '" has been created.');
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(312, 189, 359, 182);
  WindowTitle := 'Demo 04 - Create Database';
  WindowPosition := wpScreenCenter;

  btnPickDB := TfpgButton.Create(self);
  with btnPickDB do
  begin
    Name := 'btnPickDB';
    SetPosition(108, 20, 144, 24);
    Text := 'Pick Database';
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
    OnClick := @PickDBClicked;
  end;

  btnCreateDB := TfpgButton.Create(self);
  with btnCreateDB do
  begin
    Name := 'btnCreateDB';
    SetPosition(108, 96, 136, 24);
    Text := 'Create Database';
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
    TabOrder := 1;
    OnClick := @CreateDatabaseClicked;
  end;

  btnDBExists := TfpgButton.Create(self);
  with btnDBExists do
  begin
    Name := 'btnDBExists';
    SetPosition(108, 128, 136, 24);
    Text := 'Database Exists';
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
    TabOrder := 2;
    OnClick := @DatabaseExistsClicked;
  end;

  pnlDBName := TfpgPanel.Create(self);
  with pnlDBName do
  begin
    Name := 'pnlDBName';
    SetPosition(12, 56, 336, 32);
    Anchors := [anLeft,anRight,anTop];
    ParentShowHint := True;
    Style := bsLowered;
    Text := '---';
  end;

  {@VFD_BODY_END: MainForm}
end;


end.
