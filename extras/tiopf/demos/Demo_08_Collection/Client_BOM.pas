unit Client_BOM;

{$mode objfpc}{$H+}

interface
uses
  tiObject
  ;

type

  TClient = class;
  TClients = class;

  TClientName = String[200];
  TClientID   = String[9];


  TClients = class(TtiObjectList)
  public
    procedure Read; override;
    procedure Save; override;
  end;


  TClient = class(TtiObject)
  private
    FClientID: TClientID;
    FClientName: TClientName;
  public
    constructor CreateNew(const ADatabaseName: string=''; const APersistenceLayerName: string=''); override;
  published
    property    ClientName: TClientName read FClientName write FClientName;
    property    ClientID: TClientID read FClientID write FClientID;
  end;


implementation
uses
   tiOPFManager
  ,SysUtils
  ,tiUtils
  ;

{ TClient }

constructor TClient.CreateNew(const ADatabaseName: string = ''; const APersistenceLayerName: string = '');
begin
  inherited;
  // Set some default values for the demo
  ClientName := 'TEST ' + DateTimeToStr(Now);
  ClientID := IntToStr(tiGetTickCount);
end;

{ TClients }

procedure TClients.Read;
begin
  inherited Read;
  NotifyObservers;
end;

procedure TClients.Save;
begin
  inherited Save;
  NotifyObservers;
end;

end.

