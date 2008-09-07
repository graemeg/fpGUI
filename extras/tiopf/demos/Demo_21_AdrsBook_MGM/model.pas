{
  The business object model
}
unit model;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, tiObject;
  
type

  { TMarkObject }

  TMarkObject = class(TtiObject)
  protected
    procedure Mark;
  end;
  
  
  { TMarkObjectList }

  TMarkObjectList = class(TtiObjectList)
  protected
    procedure Mark;
  end;


  { TCountry }

  TCountry = class(TMarkObject)
  private
    FISO: string;
    FName: string;
    procedure SetISO(const AValue: string);
    procedure SetName(const AValue: string);
  protected
    function GetCaption: string; override;
  public
    constructor CreateNew(const AISO: string; const AName: string); overload; reintroduce;
  published
    property ISO: string read FISO write SetISO;
    property Name: string read FName write SetName;
  end;
  
  
  { TCountryList }

  TCountryList = class(TMarkObjectList)
  protected
    function GetItems(i: integer): TCountry; reintroduce;
    procedure SetItems(i: integer; const AValue: TCountry); reintroduce;
  public
    function Add(const AObject: TCountry): integer; reintroduce;
    property Items[i: integer]: TCountry read GetItems write SetItems; default;
  end;

  
  { TCity }

  TCity = class(TMarkObject)
  private
    FZIP: string;
    FName: string;
    FCountry: TCountry;
    function GetCountryAsString: string;
    procedure SetCountry(const AValue: TCountry);
    procedure SetName(const AValue: string);
    procedure SetZIP(const AValue: string);
  public
    procedure AssignClassProps(ASource: TtiObject); override;
  published
    property Country: TCountry read FCountry write SetCountry;
    property Name: string read FName write SetName;
    property ZIP: string read FZIP write SetZIP;
    property CountryAsString: string read GetCountryAsString;
  end;
  
  
  { TCityList }

  TCityList = class(TMarkObjectList)
  protected
    function GetItems(i: integer): TCity; reintroduce;
    procedure SetItems(i: integer; const AValue: TCity); reintroduce;
  public
    function Add(const AObject: TCity): integer; reintroduce;
    property Items[i: integer]: TCity read GetItems write SetItems; default;
  end;
  
  
  { TAddressType }

  TAddressType = class(TMarkObject)
  private
    FName: string;
    procedure SetName(const AValue: string);
  published
    property Name: string read FName write SetName;
  end;


  TAddress = class(TMarkObject)
  private
    FAddressType: TAddressType;
    FCity: TCity;
    FFax: string;
    FNr: integer;
    FStreet: string;
    FTelephone1: string;
    FTelephone2: string;
    function GetAddressType4GUI: string;
    procedure SetAddressType(const AValue: TAddressType);
    procedure SetCity(const AValue: TCity);
    procedure SetFax(const AValue: string);
    procedure SetNr(const AValue: integer);
    procedure SetStreet(const AValue: string);
    procedure SetTelephone1(const AValue: string);
    procedure SetTelephone2(const AValue: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AssignClassProps(ASource: TtiObject); override;
  published
    property Street: string read FStreet write SetStreet;
    property Nr: integer read FNr write SetNr;
    property Telephone1: string read FTelephone1 write SetTelephone1;
    property Telephone2: string read FTelephone2 write SetTelephone2;
    property Fax: string read FFax write SetFax;
    property AddressType: TAddressType read FAddressType write SetAddressType;
    property AddressType4GUI: string read GetAddressType4GUI;
    property City: TCity read FCity write SetCity;
  end;
  
  
  { TAddressList }

  TAddressList = class(TMarkObjectList)
  protected
    function GetItems(i: integer): TAddress; reintroduce;
    procedure SetItems(i: integer; const AValue: TAddress); reintroduce;
  public
    function Add(const AObject: TAddress): integer; reintroduce;
    property Items[i: integer]: TAddress read GetItems write SetItems; default;
  end;

  
  { TContact }

  TContact = class(TMarkObject)
  private
    FAddressList: TAddressList;
    FComments: string;
    FEmail: string;
    FFirstName: string;
    FLastName: string;
    FMobile: string;
    procedure SetComments(const AValue: string);
    procedure SetEmail(const AValue: string);
    procedure SetFirstName(const AValue: string);
    procedure SetLastName(const AValue: string);
    procedure SetMobile(const AValue: string);
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property EMail: string read FEmail write SetEmail;
    property Mobile: string read FMobile write SetMobile;
    property Comments: string read FComments write SetComments;
    property AddressList: TAddressList read FAddressList;
  end;
  
  
  { TContactList }

  TContactList = class(TMarkObjectList)
  protected
    function GetItems(i: integer): TContact; reintroduce;
    procedure SetItems(i: integer; const AValue: TContact); reintroduce;
  public
    function Add(const AObject: TContact): integer; reintroduce;
    property Items[i: integer]: TContact read GetItems write SetItems; default;
  end;
  
  
  { TContactAddressList }

  TContactAddressList = class(TAddressList)
  protected
    function GetOwner: TContact; reintroduce;
    procedure SetOwner(const AValue: TContact); reintroduce;
  public
    property Owner: TContact read GetOwner write SetOwner;
  end;




implementation


{ TMarkObject }

procedure TMarkObject.Mark;
begin
  if (ObjectState <> posEmpty) then
    Dirty:= True;
end;

{ TMarkObjectList }

procedure TMarkObjectList.Mark;
begin
  if (ObjectState <> posEmpty) then
    Dirty:= True;
end;

{ TCountry }

procedure TCountry.SetISO(const AValue: string);
begin
  if FISO = AValue then
    Exit; //==>
    
  BeginUpdate;
  FISO:= AValue;
  Mark;
  EndUpdate;
end;

procedure TCountry.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit; //==>

  BeginUpdate;
  FName:= AValue;
  Mark;
  EndUpdate;
end;

function TCountry.GetCaption: string;
begin
  Result:= Name;
end;

constructor TCountry.CreateNew(const AISO: string; const AName: string);
begin
  inherited CreateNew;
  FISO:= AISO;
  FName:= AName;
end;

{ TCity }

procedure TCity.SetCountry(const AValue: TCountry);
begin
  if FCountry = AValue then
    Exit; //==>

  BeginUpdate;
  FCountry:= AValue;
  Mark;
  EndUpdate;
end;

function TCity.GetCountryAsString: string;
begin
  result:= Country.Name + ' (' + Country.ISO + ')';
end;

procedure TCity.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit; //==>

  BeginUpdate;
  FName:= AValue;
  Mark;
  EndUpdate;
end;

procedure TCity.SetZIP(const AValue: string);
begin
  if FZip = AValue then
    Exit; //==>

  BeginUpdate;
  FZip:= AValue;
  Mark;
  EndUpdate;
end;

procedure TCity.AssignClassProps(ASource: TtiObject);
begin
  FCountry:= TCity(ASource).Country; // reference only
end;

{ TAddressType }

procedure TAddressType.SetName(const AValue: string);
begin
  if FName = AValue then
    Exit; //==>
    
  BeginUpdate;
  FName:= AValue;
  Mark;
  EndUpdate;
end;

{ TAddress }

procedure TAddress.SetStreet(const AValue: string);
begin
  if FStreet=AValue then exit;
  
  BeginUpdate;
  FStreet:=AValue;
  Mark;
  EndUpdate;
end;

procedure TAddress.SetTelephone1(const AValue: string);
begin
  if FTelephone1=AValue then exit;
  
  BeginUpdate;
  FTelephone1:=AValue;
  Mark;
  EndUpdate;
end;

procedure TAddress.SetTelephone2(const AValue: string);
begin
  if FTelephone2=AValue then exit;
  
  BeginUpdate;
  FTelephone2:=AValue;
  Mark;
  EndUpdate;
end;

constructor TAddress.Create;
begin
  inherited Create;
  FAddressType:= TAddressType.Create;
  FAddressType.Owner:= self;
end;

destructor TAddress.Destroy;
begin
  FreeAndNil(FAddressType);
  inherited Destroy;
end;

procedure TAddress.AssignClassProps(ASource: TtiObject);
begin
  FAddressType.Assign(ASource);
  FCity:= TAddress(ASource).City;  // reference only
end;

procedure TAddress.SetNr(const AValue: integer);
begin
  if FNr=AValue then exit;
  
  BeginUpdate;
  FNr:=AValue;
  Mark;
  EndUpdate;
end;

procedure TAddress.SetAddressType(const AValue: TAddressType);
begin
  if FAddressType.Name = AValue.Name then
    Exit; //==>
  
  BeginUpdate;
  FAddressType.Assign(AValue);
  Mark;
  EndUpdate;
end;

function TAddress.GetAddressType4GUI: string;
begin
  Result := FAddressType.Name;
end;

procedure TAddress.SetCity(const AValue: TCity);
begin
  if FCity=AValue then exit;
  
  BeginUpdate;
  FCity:=AValue;
  Mark;
  EndUpdate;
end;

procedure TAddress.SetFax(const AValue: string);
begin
  if FFax=AValue then exit;
  
  BeginUpdate;
  FFax:=AValue;
  Mark;
  EndUpdate;
end;

{ TContact }

procedure TContact.SetFirstName(const AValue: string);
begin
  if FFirstName=AValue then exit;
  
  BeginUpdate;
  FFirstName:=AValue;
  Mark;
  EndUpdate;
end;

procedure TContact.SetEmail(const AValue: string);
begin
  if FEmail=AValue then exit;
  
  BeginUpdate;
  FEmail:=AValue;
  Mark;
  EndUpdate;
end;

procedure TContact.SetComments(const AValue: string);
begin
  if FComments=AValue then exit;
  
  BeginUpdate;
  FComments:=AValue;
  Mark;
  EndUpdate;
end;

procedure TContact.SetLastName(const AValue: string);
begin
  if FLastName=AValue then exit;
  
  BeginUpdate;
  FLastName:=AValue;
  Mark;
  EndUpdate;
end;

procedure TContact.SetMobile(const AValue: string);
begin
  if FMobile=AValue then exit;
  
  BeginUpdate;
  FMobile:=AValue;
  Mark;
  EndUpdate;
end;

constructor TContact.Create;
begin
  inherited Create;
  FAddressList:= TAddressList.Create;
  FAddressList.Owner:= self;
  // ToDo: Refactor to remove need for ItemOwner. Use Parent instead
  FAddressList.ItemOwner:= self;
end;

destructor TContact.Destroy;
begin
  FAddressList.Free;
  inherited Destroy;
end;

{ TCountryList }

function TCountryList.GetItems(i: integer): TCountry;
begin
  Result:= TCountry(inherited GetItems(i));
end;

procedure TCountryList.SetItems(i: integer; const AValue: TCountry);
begin
  inherited SetItems(i, AValue);
end;

function TCountryList.Add(const AObject: TCountry): integer;
begin
  Result:= inherited Add(AObject);
end;

{ TCityList }

function TCityList.GetItems(i: integer): TCity;
begin
  result:= TCity(inherited GetItems(i));
end;

procedure TCityList.SetItems(i: integer; const AValue: TCity);
begin
  inherited SetItems(i, AValue);
end;

function TCityList.Add(const AObject: TCity): integer;
begin
  result:= inherited Add(AObject);
end;

{ TAddressList }

function TAddressList.GetItems(i: integer): TAddress;
begin
  result:= TAddress(inherited GetItems(i));
end;

procedure TAddressList.SetItems(i: integer; const AValue: TAddress);
begin
  inherited SetItems(i, AValue);
end;

function TAddressList.Add(const AObject: TAddress): integer;
begin
  result:= inherited Add(AObject);
end;

{ TContactList }

function TContactList.GetItems(i: integer): TContact;
begin
  result:= TContact(inherited GetItems(i));
end;

procedure TContactList.SetItems(i: integer; const AValue: TContact);
begin
  inherited SetItems(i, AValue);
end;

function TContactList.Add(const AObject: TContact): integer;
begin
  result:= inherited Add(AObject);
end;

{ TContactAddressList }

function TContactAddressList.GetOwner: TContact;
begin
  result:= TContact(inherited GetOwner);
end;

procedure TContactAddressList.SetOwner(const AValue: TContact);
begin
  inherited SetOwner(AValue);
end;


end.

