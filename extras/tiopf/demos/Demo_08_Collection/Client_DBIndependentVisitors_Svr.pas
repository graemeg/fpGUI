unit Client_DBIndependentVisitors_Svr;

{$mode objfpc}{$H+}

interface
uses
  tiVisitorDBAutoGen
 ;

type

  TVisClient_Read = class(TVisDBAutoGenRead)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init          ; override;
    procedure SetupParams   ; override;
    procedure MapRowToObject; override;
  end;

  TVisClient_Create = class(TVisDBAutoGenUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;

  TVisClient_Update = class(TVisDBAutoGenUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;

  TVisClient_Delete = class(TVisDBAutoGenDelete)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;

procedure RegisterVisitors;

implementation
uses
  Client_BOM
  ,tiOPFManager
  ,tiObject
  ,tiLog
  ,tiQuery
 ;

procedure RegisterVisitors;
begin
  gTIOPFManager.RegReadVisitor(TVisClient_Read);
  gTIOPFManager.RegSaveVisitor(TVisClient_Create);
  gTIOPFManager.RegSaveVisitor(TVisClient_Update);
  gTIOPFManager.RegSaveVisitor(TVisClient_Delete);
end;

{ TVisClient_Read }

function TVisClient_Read.AcceptVisitor: boolean;
begin
  result:= (Visited is TClients) and
            (Visited.ObjectState = posEmpty);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result ]);
end;

procedure TVisClient_Read.Init;
begin
  TableName:= 'Client';
end;

procedure TVisClient_Read.MapRowToObject;
var
  LClient: TClient;
begin
  LClient:= TClient.Create;
  LClient.OID.AssignFromTIQuery('OID',Query);
  LClient.ClientName:= Query.FieldAsString['Client_Name'];
  LClient.ClientID:= Query.FieldAsString['Client_ID'];
  LClient.ObjectState:= posClean;
  TClients(Visited).Add(LClient);
end;

procedure TVisClient_Read.SetupParams;
begin
  // Do nothing
end;

{ TVisClient_Create }

function TVisClient_Create.AcceptVisitor: boolean;
begin
  result:= (Visited is TClient) and
            (Visited.ObjectState = posCreate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result ]);
end;

procedure TVisClient_Create.SetupParams;
var
  LData: TClient;
begin
  LData:= Visited as TClient;
  TableName:= 'Client';
  QueryType:= qtInsert;
  QueryParams.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('Client_Name', LData.ClientName);
  QueryParams.SetValueAsString('Client_ID', LData.ClientID);
end;

{ TVisClient_Update }

function TVisClient_Update.AcceptVisitor: boolean;
begin
  result:= (Visited is TClient) and
            (Visited.ObjectState = posUpdate);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result ]);
end;

procedure TVisClient_Update.SetupParams;
var
  LData: TClient;
begin
  LData:= Visited as TClient;
  TableName:= 'Client';
  QueryType:= qtUpdate;
  QueryWhere.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('Client_Name', LData.ClientName);
  QueryParams.SetValueAsString('Client_ID', LData.ClientID);
end;

{ TVisClient_Delete }

function TVisClient_Delete.AcceptVisitor: boolean;
begin
  result:= (Visited is TClient) and
            (Visited.ObjectState = posDelete);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result ]);
end;

procedure TVisClient_Delete.SetupParams;
begin
  inherited;
  TableName:= 'Client';
end;

end.

