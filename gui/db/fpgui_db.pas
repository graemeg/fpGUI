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

  TFieldDataLink = class(TDataLink)
  private
    FWidget: TFWidget;
    FField: TField;
    FFieldName: String;
    FOnDataChange: TNotifyEvent;
    procedure   SetFieldName(const AFieldName: String);
    procedure   UpdateField;
  protected
    procedure   ActiveChanged; override;
    procedure   RecordChanged(AField: TField); override;
  public
    constructor Create(AWidget: TFWidget);
    property    Field: TField read FField;
    property    FieldName: String read FFieldName write SetFieldName;
    property    OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
  end;


  TDBText = class(TFCustomLabel)
  private
    FDataLink: TFieldDataLink;
    function    GetDataField: String;
    procedure   SetDataField(const ADataField: String);
    function    GetDataSource: TDataSource;
    procedure   SetDataSource(ADataSource: TDataSource);
    procedure   DataChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property    Alignment default taLeftJustify;
    property    CanExpandWidth;
    property    DataField: string read GetDataField write SetDataField;
    property    DataSource: TDataSource read GetDataSource write SetDataSource;
    property    Enabled;
    property    FontColor;
    property    Text;
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

procedure TFieldDataLink.SetFieldName(const AFieldName: String);
begin
  if AFieldName <> FieldName then
  begin
    FFieldName := AFieldName;
    UpdateField;
  end;
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
  {$IFDEF DEBUG} Write('TDBText.DataChange'); {$ENDIF}
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

end.

