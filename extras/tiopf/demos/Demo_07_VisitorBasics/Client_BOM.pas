unit Client_BOM;

interface
uses
  tiObject
  ,tiOID
  ,tiOIDGUID
  ,tiVisitor
  ;

type

  TClient = class;
  TClientList = class;

  TClientName = String[200];
  TClientID   = String[9];

  TClientList = class(TtiObjectList);


  TClient = class(TtiObject)
  private
    FClientID: TClientID;
    FClientName: TClientName;
  published
    property    ClientName: TClientName read FClientName write FClientName;
    property    ClientID  : TClientID read FClientID write FClientID;
  end;


  TClientVisitor = class(TtiVisitor)
  protected
    function    AcceptVisitor: boolean; override;
  public
    procedure   Execute(const AVisited: TtiVisited); override;
  end;


procedure RegisterMappings;


implementation
uses
   tiOPFManager
  ,tiAutoMap
  ,tiConstants
  ,tiDialogs
 ;

procedure RegisterMappings;
begin
  //                                          Class,   Table,    Property,     Column,       Special Info
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'OID',        'OID',        [pktDB]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientName', 'Client_Name'        );
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientID',   'Client_ID'          );
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClientList, TClient);
end;

{ TClientVisitor }

function TClientVisitor.AcceptVisitor: boolean;
begin
  // Put the code to check if this visitor should act on this object in here.
  Result:= Visited is TClient;
  // Remove this line and the visitor will touch the TClientList object
  // as well as it's owned TClient objects.
end;

procedure TClientVisitor.Execute(const AVisited: TtiVisited);
begin
  inherited;
  if not AcceptVisitor then
    Exit;
  tiShowMessage((Visited as TtiObject).AsDebugString);
end;

end.



