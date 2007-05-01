{
    fpGUI  -  Free Pascal GUI Library
    
    Database support classes
    
    Copyright (C) 2000 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit fpGUI_DB;

{$IFDEF Debug}
  {$ASSERTIONS On}
{$ENDIF}

interface

uses
  Classes
  ,fpGUI
  ,DB
  ;

type

  { TFieldDataLink }

  TFieldDataLink = class(TDataLink)
  private
    FWidget: TFWidget;
    FField: TField;
    FFieldName: string;
    FOnDataChange: TNotifyEvent;
    function    GetCanModify: Boolean;
    procedure   SetFieldName(const AFieldName: string);
    procedure   UpdateField;
  protected
    procedure   ActiveChanged; override;
    procedure   RecordChanged(AField: TField); override;
  public
    constructor Create(AWidget: TFWidget);
    property    CanModify: Boolean read GetCanModify;
    property    Field: TField read FField;
    property    FieldName: string read FFieldName write SetFieldName;
    property    Widget: TFWidget read FWidget write FWidget;
    property    OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
  end;


  { TDBText }

  TDBText = class(TFCustomLabel)
  private
    FDataLink: TFieldDataLink;
    function    GetDataField: String;
    function    GetField: TField;
    procedure   SetDataField(const ADataField: String);
    function    GetDataSource: TDataSource;
    procedure   SetDataSource(ADataSource: TDataSource);
    procedure   DataChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Field: TField read GetField;
  published
    property    Alignment default taLeftJustify;
    property    CanExpandWidth;
    property    DataField: string read GetDataField write SetDataField;
    property    DataSource: TDataSource read GetDataSource write SetDataSource;
    property    Enabled;
    property    FontColor;
    property    Text;
  end;


  { TDBEdit }

  TDBEdit = class(TFCustomEdit)
  private
    FDataLink: TFieldDataLink;
    function    GetDataField: string;
    function    GetDataSource: TDataSource;
    function    GetField: TField;
    function    GetReadOnly: Boolean;
    procedure   SetDataField(const ADataField: string);
    procedure   SetDataSource(const ADataSource: TDataSource);
    procedure   DataChange(Sender: TObject);
    procedure   SetReadOnly(const AValue: Boolean);
  protected
    procedure   EvKeyPressed(Key: Word; Shift: TShiftState); override;
    procedure   EvKeyChar(KeyChar: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Field: TField read GetField;
  published
    property    BorderStyle;
    property    CanExpandWidth;
    property    DataField: string read GetDataField write SetDataField;
    property    DataSource: TDataSource read GetDataSource write SetDataSource;
    property    Enabled;
    property    FontColor;
    property    Text;
    property    ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;


implementation


{ TFieldDataLink }

constructor TFieldDataLink.Create(AWidget: TFWidget);
begin
  inherited Create;
  FWidget := AWidget;
end;

procedure TFieldDataLink.ActiveChanged;
begin
  UpdateField;
end;

procedure TFieldDataLink.RecordChanged(AField: TField);
begin
  if Assigned(OnDataChange) then
    OnDataChange(Self);
end;

procedure TFieldDataLink.SetFieldName(const AFieldName: string);
begin
  if AFieldName <> FieldName then
  begin
    FFieldName := AFieldName;
    UpdateField;
  end;
end;

function TFieldDataLink.GetCanModify: Boolean;
begin
  Result := not ReadOnly and (Field <> nil) and Field.CanModify;
end;

procedure TFieldDataLink.UpdateField;
begin
  {$IFDEF DEBUG} WriteLn('## UpdateField. DataSet: ', DataSource.DataSet.ClassName); {$ENDIF}
  FField := DataSource.DataSet.FindField(FieldName);
  if Assigned(OnDataChange) then
    OnDataChange(Self);
end;


{ TDBText }

constructor TDBText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create(Self);
  FDataLink.OnDataChange := @DataChange;
end;

destructor TDBText.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

function TDBText.GetDataField: String;
begin
  Result := FDataLink.FieldName;
end;

function TDBText.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDBText.SetDataField(const ADataField: String);
begin
  FDataLink.FieldName := ADataField;
end;

function TDBText.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBText.SetDataSource(ADataSource: TDataSource);
begin
  FDataLink.DataSource := ADataSource;
end;

procedure TDBText.DataChange(Sender: TObject);
begin
  {$IFDEF DEBUG} Write(Classname + '.DataChange'); {$ENDIF}
  if Assigned(FDataLink.Field) then
  begin
    Text := FDataLink.Field.DisplayText;
    {$IFDEF DEBUG} WriteLn(' new text: "', Text, '"'); {$ENDIF}
  end
  else
  begin
    Text := '';
    {$IFDEF DEBUG} WriteLn('DataLink has no data'); {$ENDIF}
  end;
end;


{ TDBEdit }

function TDBEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TDBEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TDBEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TDBEdit.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
//  Result := FDataLink.ReadOnly;     { will add this in later }
end;

procedure TDBEdit.SetDataField(const ADataField: string);
begin
  FDataLink.FieldName := ADataField;
end;

procedure TDBEdit.SetDataSource(const ADataSource: TDataSource);
begin
  FDataLink.DataSource := ADataSource;
end;

procedure TDBEdit.DataChange(Sender: TObject);
begin
  {$IFDEF DEBUG} Write(Classname + '.DataChange'); {$ENDIF}
  if Assigned(FDataLink.Field) then
  begin
    Text := FDataLink.Field.DisplayText;
    {$IFDEF DEBUG} WriteLn(' new text: "', Text, '"'); {$ENDIF}
  end
  else
  begin
    Text := '';
    {$IFDEF DEBUG} WriteLn('DataLink has no data'); {$ENDIF}
  end;
end;

procedure TDBEdit.SetReadOnly(const AValue: Boolean);
begin
  inherited ReadOnly := AValue;
//  FDataLink.ReadOnly := AValue;         { will add this in later }
end;

procedure TDBEdit.EvKeyPressed(Key: Word; Shift: TShiftState);
begin
//  if ReadOnly then
//    Exit; //==>
  inherited EvKeyPressed(Key, Shift);
end;

procedure TDBEdit.EvKeyChar(KeyChar: Char);
begin
  if ReadOnly then
    Exit; //==>
  inherited EvKeyChar(KeyChar);
end;

constructor TDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  FDataLink := TFieldDataLink.Create(Self);
  FDataLink.OnDataChange := @DataChange;
end;

destructor TDBEdit.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

end.

