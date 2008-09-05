unit contactmanager;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  model;
  
type

  { TContactManager }

  TContactManager = class(TMarkObject)
  private
    FCityList: TCityList;
    FContactList: TContactList;
    FCountryList: TCountryList;
    procedure PopulateCountries;
    procedure PopulateCities;
    function GenPhone: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure PopulateContacts;
  published
    property CountryList: TCountryList read FCountryList;
    property CityList: TCityList read FCityList;
    property ContactList: TContactList read FContactList;
  end;


// Global singleton
function gContactManager: TContactManager;


implementation

uses
  SysUtils;

var
  uContactManager: TContactManager;
  
const
  LastNames: array[1..10] of string = ('Geldenhuys', 'Botha', 'Johnson', 'Fourie',
      'Louw', 'Bougas', 'van der Mescht', 'Waskanski', 'Welgens', 'Viljoen');

  FirstNames: array[1..10] of string = ('Graeme', 'Johan', 'Debbie', 'Freda',
      'Jack', 'Ryno', 'Dirkus', 'Angela', 'Denise', 'Daniel');
      
  StreetNames: array[1..11] of string = ('Stellenberg Rd', 'Stellendal Rd',
      'Abelia', 'Main Rd', 'Links Drive', 'Short Street',
      'Long Street', 'Loop Street', 'Hillside Rd', 'Mountain Rd', 'Beach Drive');

function gContactManager: TContactManager;
begin
  if not Assigned(uContactManager) then
    uContactManager:= TContactManager.Create;
  result:= uContactManager;
end;

{ TContactManager }

procedure TContactManager.PopulateCountries;
begin
  FCountryList.Add(TCountry.CreateNew('za', 'South Africa'));
  FCountryList.Add(TCountry.CreateNew('gb', 'Great Britain'));
  FCountryList.Add(TCountry.CreateNew('uk', 'Ukrain'));
  FCountryList.Add(TCountry.CreateNew('fr', 'France'));
  FCountryList.Add(TCountry.CreateNew('us', 'United States'));
  FCountryList.Add(TCountry.CreateNew('gr', 'Germany'));
end;

procedure TContactManager.PopulateCities;
var
  c: TCity;
begin
  c:= TCity.CreateNew;
  c.Name:= 'Somerset West';
  c.Country:= TCountry(FCountryList.FindByProps(['ISO'], ['za'], True));
  FCityList.Add(c);
  c:= TCity.CreateNew;
  c.Name:= 'Cape Town';
  c.Country:= TCountry(FCountryList.FindByProps(['ISO'], ['za'], True));
  FCityList.Add(c);
  c:= TCity.CreateNew;
  c.Name:= 'Pretoria';
  c.Country:= TCountry(FCountryList.FindByProps(['ISO'], ['za'], True));
  FCityList.Add(c);
  c:= TCity.CreateNew;
  c.Name:= 'Durban';
  c.Country:= TCountry(FCountryList.FindByProps(['ISO'], ['za'], True));
  FCityList.Add(c);
  c:= TCity.CreateNew;
  c.Name:= 'London';
  c.Country:= TCountry(FCountryList.FindByProps(['ISO'], ['gb'], True));
  FCityList.Add(c);
  c:= TCity.CreateNew;
  c.Name:= 'Watford';
  c.Country:= TCountry(FCountryList.FindByProps(['ISO'], ['gb'], True));
  FCityList.Add(c);
  c:= TCity.CreateNew;
  c.Name:= 'Frankfurt';
  c.Country:= TCountry(FCountryList.FindByProps(['ISO'], ['gr'], True));
  FCityList.Add(c);
  c:= TCity.CreateNew;
  c.Name:= 'New York';
  c.Country:= TCountry(FCountryList.FindByProps(['ISO'], ['us'], True));
  FCityList.Add(c);
  c:= TCity.CreateNew;
  c.Name:= 'San Fransisco';
  c.Country:= TCountry(FCountryList.FindByProps(['ISO'], ['us'], True));
  FCityList.Add(c);
  c:= TCity.CreateNew;
  c.Name:= 'Paris';
  c.Country:= TCountry(FCountryList.FindByProps(['ISO'], ['fr'], True));
  FCityList.Add(c);
  c:= TCity.CreateNew;
  c.Name:= 'Big City';
  c.Country:= TCountry(FCountryList.FindByProps(['ISO'], ['uk'], True));
  FCityList.Add(c);
end;

function TContactManager.GenPhone: string;
begin
  result:= '+27 ' + IntToStr(Random(9)) + IntToStr(Random(9)) + ' '
    + IntToStr(Random(9)) + IntToStr(Random(9)) + IntToStr(Random(9)) + '-'
    + IntToStr(Random(9)) + IntToStr(Random(9)) + IntToStr(Random(9)) + IntToStr(Random(9));
end;

constructor TContactManager.Create;
begin
  inherited Create;
  FCountryList:= TCountryList.Create;
  FCountryList.Owner:= self;
  FCityList:= TCityList.Create;
  FCityList.Owner:= self;
  FContactList:= TContactList.Create;
  FContactList.Owner:= self;
end;

destructor TContactManager.Destroy;
begin
  FContactList.Free;
  FCityList.Free;
  FCountryList.Free;
  inherited Destroy;
end;

procedure TContactManager.PopulateContacts;
var
  C: TContact;
  I,J: Integer;
  A: TAddress;
begin
  PopulateCountries;
  PopulateCities;
  for I:= 1 to 10 do
  begin
    C:= TContact.CreateNew;
    C.FirstName:= FirstNames[I];
    C.LastName := LastNames[I];
    C.Mobile   := GenPhone;
    C.Email    := LowerCase(FirstNames[i])+ '@freepascal.org';
    for J:= 1 to 1+Random(2) do
    begin
      A:= TAddress.CreateNew;
      A.Street := StreetNames[1+Random(10)];
      A.Nr     := Random(100)+1;
      A.City   := FCityList[Random(10)];
      A.Fax    := GenPhone;
      A.Telephone1:= GenPhone;
      If Random(2)>0 then
         A.Telephone2:= GenPhone;
      C.AddressList.Add(A);
    end;
    FContactList.Add(C);
  end;
end;


initialization
  uContactManager:= nil;
  
finalization
  uContactManager.Free;

end.

