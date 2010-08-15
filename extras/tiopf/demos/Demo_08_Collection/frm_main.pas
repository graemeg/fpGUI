unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_edit,
  fpg_widget, fpg_form, fpg_label, fpg_button,
  fpg_listbox, fpg_memo, fpg_combobox, fpg_grid,
  fpg_dialogs, fpg_checkbox, fpg_tree, fpg_trackbar,
  fpg_progressbar, fpg_radiobutton, fpg_tab, fpg_menu,
  fpg_panel, Client_BOM, tiModelMediator;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    lblName1: TfpgLabel;
    lblName2: TfpgLabel;
    lblName3: TfpgLabel;
    edtOID: TfpgEdit;
    edtClientName: TfpgEdit;
    edtClientID: TfpgEdit;
    btnInsertRow: TfpgButton;
    btnDeleteRow: TfpgButton;
    btnShow: TfpgButton;
    btnSave: TfpgButton;
    btnRead: TfpgButton;
    grdCollection: TfpgStringGrid;
    {@VFD_HEAD_END: MainForm}
    FClients: TClients;
    FMediator: TtiModelMediator;
    procedure   MainFormShow(Sender: TObject);
    procedure   CreateTable;
    procedure   DropTable;
    procedure   CreateMediators;
    function    TableExists: boolean;
    procedure   btnInsertRowClick(Sender: TObject);
    procedure   btnDeleteRowClick(Sender: TObject);
    procedure   btnSaveClick(Sender: TObject);
    procedure   btnReadClick(Sender: TObject);
    procedure   btnShowClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
   tiQuery
  ,tiOIDGUID
  ,tiOPFManager
  ,tiDBConnectionPool
  ,tiOID
  ,tiDialogs
  ,tiConstants
  ,tiBaseMediator
  ,tiListMediators
  ,tiLog
  ;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.MainFormShow(Sender: TObject);
begin
  Log('Creating mediators');
  CreateMediators;

  // Drop and re-create to be sure we start with the correct structure
  Log('Testing if table exists');
  if TableExists then
  begin
    Log('  Should we drop and recreate the tables');
    if TfpgMessageDialog.Question(ApplicationName, 'Must we delete existing data?', mbYesNo) = mbYes then
    begin
      DropTable;
      CreateTable;
    end;
  end
  else
    CreateTable;
end;

procedure TMainForm.CreateTable;
var
  LTableMetaData: TtiDBMetaDataTable;
begin
  LTableMetaData := TtiDBMetaDataTable.Create;
  try
    LTableMetaData.Name:= 'Client';
    LTableMetaData.AddField('OID',               qfkString,  36); // Using GUID OIDs
    LTableMetaData.AddField('Client_Name',       qfkString, 200);
    LTableMetaData.AddField('Client_ID',         qfkString,   9);
    gTIOPFManager.CreateTable(LTableMetaData);
  finally
    LTableMetaData.Free;
  end;
end;

procedure TMainForm.DropTable;
begin
  gTIOPFManager.DropTable('Client');
end;

procedure TMainForm.CreateMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(self);
    FMediator.AddComposite('ClientName(200,"Client name");ClientID(80,"Client ID")', grdCollection);
  end;
  FMediator.Subject := FClients;
  FMediator.Active := True;

  //FmedClients := TClient_StringGrid_Mediator.CreateCustom(FClients, grdCollection, 'ClientName(200,"Client name");ClientID(80,"Client ID")');
  //FClients.NotifyObservers;

  //LV.AddColumn(LVDeriveOID, 'OID', 270);
  //LV.AddColumn('ClientName', vttkString, 'Client name', 200);
  //LV.AddColumn('ClientID',   vttkString, 'Client ID', 80);
  //LV.Data:= FClients;
end;

function TMainForm.TableExists: boolean;
var
  LDBMetaData: TtiDBMetaData;
  LDatabase: TtiDatabase;
begin
  LDBMetaData := TtiDBMetaData.Create;
  try
    LDatabase := gTIOPFManager.DefaultDBConnectionPool.Lock;
    try
//      Log('  Reading meta data of tables');
      LDatabase.ReadMetaDataTables(LDBMetaData);
//      log('  Finding the table <Client>');
      result := LDBMetaData.FindByTableName('Client') <> nil;
    finally
      gTIOPFManager.DefaultDBConnectionPool.UnLock(LDatabase);
    end;
  finally
    LDBMetaData.Free;
  end;
end;

procedure TMainForm.btnInsertRowClick(Sender: TObject);
var
  LClient: TClient;
begin
  LClient:= TClient.CreateNew;
  FClients.Add(LClient);
end;

procedure TMainForm.btnDeleteRowClick(Sender: TObject);
begin
  //if LV.SelectedData <> nil then
    //LV.SelectedData.Deleted:= true;
  //LV.Refresh;
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  FClients.Save;
end;

procedure TMainForm.btnReadClick(Sender: TObject);
begin
  FMediator.Active := False;
  FClients.Clear;
  FClients.Read;
  FMediator.Active := True;
end;

procedure TMainForm.btnShowClick(Sender: TObject);
begin
  tiShowString(FClients.AsDebugString);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Connected to ' + gTIOPFManager.DefaultDBConnectionName;
  FClients := TClients.Create;
  OnShow := @MainFormShow;
end;

destructor TMainForm.Destroy;
begin
  FMediator.Active := False;
  FClients.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(304, 254, 565, 250);
  WindowTitle := 'Collection demo';
  WindowPosition := wpScreenCenter;
  Sizeable := False;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(8, 12, 80, 16);
    Text := 'OID:';
    FontDesc := '#Label1';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(8, 36, 80, 16);
    Text := 'Client Name:';
    FontDesc := '#Label1';
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(8, 60, 80, 16);
    Text := 'Client ID:';
    FontDesc := '#Label1';
  end;

  edtOID := TfpgEdit.Create(self);
  with edtOID do
  begin
    Name := 'edtOID';
    SetPosition(88, 8, 172, 22);
    Text := '';
    FontDesc := '#Edit1';
  end;

  edtClientName := TfpgEdit.Create(self);
  with edtClientName do
  begin
    Name := 'edtClientName';
    SetPosition(88, 32, 172, 22);
    Text := '';
    FontDesc := '#Edit1';
  end;

  edtClientID := TfpgEdit.Create(self);
  with edtClientID do
  begin
    Name := 'edtClientID';
    SetPosition(88, 56, 172, 22);
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnInsertRow := TfpgButton.Create(self);
  with btnInsertRow do
  begin
    Name := 'btnInsertRow';
    SetPosition(268, 8, 143, 24);
    Text := 'Insert object into list';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnInsertRowClick;
  end;

  btnDeleteRow := TfpgButton.Create(self);
  with btnDeleteRow do
  begin
    Name := 'btnDeleteRow';
    SetPosition(268, 36, 143, 24);
    Text := 'Delete object in list';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnDeleteRowClick;
  end;

  btnShow := TfpgButton.Create(self);
  with btnShow do
  begin
    Name := 'btnShow';
    SetPosition(416, 8, 143, 24);
    Text := 'Show Objects in list';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnShowClick;
  end;

  btnSave := TfpgButton.Create(self);
  with btnSave do
  begin
    Name := 'btnSave';
    SetPosition(416, 36, 143, 24);
    Text := 'Save';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnSaveClick;
  end;

  btnRead := TfpgButton.Create(self);
  with btnRead do
  begin
    Name := 'btnRead';
    SetPosition(416, 64, 143, 24);
    Text := 'Read list from DB';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnReadClick;
  end;

  grdCollection := TfpgStringGrid.Create(self);
  with grdCollection do
  begin
    Name := 'grdCollection';
    SetPosition(8, 96, 552, 148);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
  end;

  {@VFD_BODY_END: MainForm}
end;

initialization
  gMediatorManager.RegisterMediator(TtiStringGridMediatorView, TClients);

end.
