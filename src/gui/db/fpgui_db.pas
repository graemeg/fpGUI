{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Database support classes. This serves as an example of how someone
      could implement DB aware components for fpGUI.
}

unit fpgui_db;

{$mode objfpc}{$H+}

// If enabled it outputs debug information for this unit
{.$Define DEBUG}

interface

uses
  Classes,
  db,
  fpg_widget,
  fpg_label{, fpg_edit};
  
type

  TfpgFieldDataLink = class(TDataLink)
  private
    FWidget: TfpgWidget;
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
    constructor Create(AWidget: TfpgWidget);
    property    CanModify: Boolean read GetCanModify;
    property    Field: TField read FField;
    property    FieldName: string read FFieldName write SetFieldName;
    property    Widget: TfpgWidget read FWidget write FWidget;
    property    OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
  end;
  
  
  TfpgDBLabel = class(TfpgCustomLabel)
  private
    FDataLink: TfpgFieldDataLink;
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
    property    AutoSize;
    property    BackgroundColor;
    property    DataField: string read GetDataField write SetDataField;
    property    DataSource: TDataSource read GetDataSource write SetDataSource;
    property    Enabled;
    property    FontDesc;
    property    Text;
    property    TextColor;
  end;

{
  // TfpgEdit needs to be refactor some more first!!
  TfpgDBEdit = class(TfpgCustomEdit)
  private
    FDataLink: TfpgFieldDataLink;
    function    GetDataField: string;
    function    GetDataSource: TDataSource;
    function    GetField: TField;
    function    GetReadOnly: Boolean;
    procedure   SetDataField(const ADataField: string);
    procedure   SetDataSource(const ADataSource: TDataSource);
    procedure   DataChange(Sender: TObject);
    procedure   SetReadOnly(const AValue: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Field: TField read GetField;
  published
    property    DataField: string read GetDataField write SetDataField;
    property    DataSource: TDataSource read GetDataSource write SetDataSource;
    property    Enabled;
    property    BackgroundColor;
    property    Color;
    property    FontDesc;
    property    Text;
    property    ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;
}


implementation


{ TfpgFieldDataLink }

function TfpgFieldDataLink.GetCanModify: Boolean;
begin
  Result := not ReadOnly and (Field <> nil) and Field.CanModify;
end;

procedure TfpgFieldDataLink.SetFieldName(const AFieldName: string);
begin
  if AFieldName <> FieldName then
  begin
    FFieldName := AFieldName;
    UpdateField;
  end;
end;

procedure TfpgFieldDataLink.UpdateField;
begin
  {$IFDEF DEBUG} WriteLn('## UpdateField. DataSet: ', DataSource.DataSet.ClassName); {$ENDIF}
  FField := DataSource.DataSet.FindField(FieldName);
  if Assigned(OnDataChange) then
    OnDataChange(Self);
end;

procedure TfpgFieldDataLink.ActiveChanged;
begin
  inherited ActiveChanged;
  UpdateField;
end;

procedure TfpgFieldDataLink.RecordChanged(AField: TField);
begin
  inherited RecordChanged(AField);
  if Assigned(OnDataChange) then
    OnDataChange(Self);
end;

constructor TfpgFieldDataLink.Create(AWidget: TfpgWidget);
begin
  inherited Create;
  FWidget := AWidget;
end;


{ TfpgDBLabel }

function TfpgDBLabel.GetDataField: String;
begin
  Result := FDataLink.FieldName;
end;

function TfpgDBLabel.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TfpgDBLabel.SetDataField(const ADataField: String);
begin
  FDataLink.FieldName := ADataField;
end;

function TfpgDBLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TfpgDBLabel.SetDataSource(ADataSource: TDataSource);
begin
  FDataLink.DataSource := ADataSource;
end;

procedure TfpgDBLabel.DataChange(Sender: TObject);
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

constructor TfpgDBLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TfpgFieldDataLink.Create(Self);
  FDataLink.OnDataChange := @DataChange;
end;

destructor TfpgDBLabel.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

end.

