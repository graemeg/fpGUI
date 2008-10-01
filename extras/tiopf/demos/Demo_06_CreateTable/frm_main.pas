unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_main, fpg_widget, fpg_form, fpg_label, fpg_button,
  fpg_memo, fpg_dialogs;

type

  TMainForm = class(TfpgForm)
  private
    procedure btnCreateTableClick(Sender: TObject);
    procedure btnDropTableClick(Sender: TObject);
    procedure btnShowMetaDataClick(Sender: TObject);
    procedure btnTableExistsClick(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    lblConnectedTo: TfpgLabel;
    btnCreateTable: TfpgButton;
    btnDropTable: TfpgButton;
    btnShowMetaData: TfpgButton;
    btnTableExists: TfpgButton;
    memName1: TfpgMemo;
    {@VFD_HEAD_END: MainForm}
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
   tiQuery
  ,tiOPFManager
  ,tiOIDGUID
  ,tiDialogs
  ;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.btnCreateTableClick(Sender: TObject);
var
  LTableMetaData: TtiDBMetaDataTable;
begin
  LTableMetaData:= TtiDBMetaDataTable.Create;
  try
    LTableMetaData.Name:= 'Client';
    LTableMetaData.AddField('OID',               qfkString,  36); // Using GUID OIDs
    LTableMetaData.AddField('Client_Name',       qfkString, 200);
    LTableMetaData.AddField('Client_ID',         qfkString,   9);
    gTIOPFManager.CreateTable(LTableMetaData);
  finally
    LTableMetaData.Free;
  end;
  ShowMessage('Table ''Client'' created');
end;

procedure TMainForm.btnDropTableClick(Sender: TObject);
begin
  gTIOPFManager.DropTable('Client');
  ShowMessage('Table ''Client'' dropped');
end;

procedure TMainForm.btnShowMetaDataClick(Sender: TObject);
var
  LTableMetaData: TtiDBMetaDataTable;
  LDatabase     : TtiDatabase;
begin
  LTableMetaData:= TtiDBMetaDataTable.Create;
  try
    LTableMetaData.Name:= 'Client';
    LDatabase:= gTIOPFManager.DefaultDBConnectionPool.Lock;
    try
      LDatabase.ReadMetaDataFields(LTableMetaData);
      tiShowMessage(LTableMetaData.AsDebugString);
    finally
      gTIOPFManager.DefaultDBConnectionPool.UnLock(LDatabase);
    end;
  finally
    LTableMetaData.Free;
  end;
end;

procedure TMainForm.btnTableExistsClick(Sender: TObject);
var
  LDBMetaData: TtiDBMetaData;
  LDatabase  : TtiDatabase;
begin
  LDBMetaData:= TtiDBMetaData.Create;
  try
    LDatabase:= gTIOPFManager.DefaultDBConnectionPool.Lock;
    try
      LDatabase.ReadMetaDataTables(LDBMetaData);
      if LDBMetaData.FindByTableName('Client') <> nil then
        ShowMessage('Table <Client> exists')
      else
        ShowMessage('Table <Client> does not exist');
    finally
      gTIOPFManager.DefaultDBConnectionPool.UnLock(LDatabase);
    end;
  finally
    LDBMetaData.Free;
  end;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  lblConnectedTo.Text := 'Connected to: ' + gTIOPFManager.DefaultDBConnectionName;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(738, 153, 389, 250);
  WindowTitle := 'Create table demo';
  WindowPosition := wpScreenCenter;
  Sizeable := False;

  lblConnectedTo := TfpgLabel.Create(self);
  with lblConnectedTo do
  begin
    Name := 'lblConnectedTo';
    SetPosition(4, 4, 380, 16);
    Text := 'Connected To:';
    FontDesc := '#Label1';
  end;

  btnCreateTable := TfpgButton.Create(self);
  with btnCreateTable do
  begin
    Name := 'btnCreateTable';
    SetPosition(280, 24, 103, 24);
    Text := 'Create Table';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnCreateTableClick;
  end;

  btnDropTable := TfpgButton.Create(self);
  with btnDropTable do
  begin
    Name := 'btnDropTable';
    SetPosition(280, 52, 103, 24);
    Text := 'Drop Table';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnDropTableClick;
  end;

  btnShowMetaData := TfpgButton.Create(self);
  with btnShowMetaData do
  begin
    Name := 'btnShowMetaData';
    SetPosition(280, 80, 103, 24);
    Text := 'Show metadata';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnShowMetaDataClick;
  end;

  btnTableExists := TfpgButton.Create(self);
  with btnTableExists do
  begin
    Name := 'btnTableExists';
    SetPosition(280, 108, 103, 24);
    Text := 'Table exists?';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnTableExistsClick;
  end;

  memName1 := TfpgMemo.Create(self);
  with memName1 do
  begin
    Name := 'memName1';
    SetPosition(4, 24, 264, 220);
    Lines.Add('This demo will:');
    Lines.Add('');
    Lines.Add('a) Create a table called Client with the');
    Lines.Add('    following structure:');
    Lines.Add('       OID String(36)');
    Lines.Add('       Client_Name String(200)');
    Lines.Add('       Client_ID String(9)');
    Lines.Add('');
    Lines.Add('b) Test if a table called Client exists');
    Lines.Add('');
    Lines.Add('c) Show metadata for the Client table');
    Lines.Add('');
    Lines.Add('d) Drop the client table');
    FontDesc := '#Edit1';
    Enabled := False;
  end;

  {@VFD_BODY_END: MainForm}
end;


end.
