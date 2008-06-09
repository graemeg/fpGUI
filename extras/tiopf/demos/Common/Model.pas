unit Model;

{$mode objfpc}{$H+}

interface
uses
  tiObject
  ;

type
  TGender = (genMale, genFemale);

const
  cGender: array[TGender] of string = ('Male', 'Female');

type
  TPerson = class;
  TPersonList = class;


  { Undo feature for TPerson }
  TPersonMemento = class(TObject)
  private
    FOID: string;
    FObjectState: TPerObjectState;
    FName: string;
    FAge: integer;
    FGender: TGender;
  end;


  { TPerson - The subject being observed }
  TPerson = class(TtiObject)
  private
    FGender: TGender;
    FName: string;
    FAge: integer;
    function    GetGenderGUI: string;
    function    GetMemento: TPersonMemento;
    procedure   SetGender(const AValue: TGender);
    procedure   SetGenderGUI(const AValue: string);
    procedure   SetName(const Value: string);
    procedure   SetAge(const Value: integer);
    procedure   SetMemento(const AValue: TPersonMemento);
  protected
    function    GetCaption: string; override;
  public
    constructor Create; override;
    function    IsValid(const pErrors: TtiObjectErrors): Boolean; override;
    procedure   NotifyObservers; override; 
    property    Memento: TPersonMemento read GetMemento write SetMemento;
    property    Gender: TGender read FGender write SetGender;
  published
    property    Name: string read FName write SetName;
    property    Age: integer read FAge write SetAge;
    property    GenderGUI: string read GetGenderGUI write SetGenderGUI;
  end;


  { TPersonList }
  TPersonList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TPerson; reintroduce;
    procedure   SetItems(i: integer; const Value: TPerson); reintroduce;
  public
    property    Items[i: integer]: TPerson read GetItems write SetItems;
    procedure   Add(const pObject: TPerson); reintroduce;
  end;
  

function GeneratePersonList: TPersonList;


implementation
uses
  Constants
  ;


function GeneratePersonList: TPersonList;
var
  lList: TPersonList;
  lData: TPerson;
begin
  lList := TPersonList.Create;

  lData := TPerson.Create;
  lData.Name    := 'Graeme Geldenhuys';
  lData.Age     := 23;
  lList.Add(lData);

  lData := TPerson.Create;
  lData.Name    := 'Peter Hinrichsen';
  lData.Age     := 34;
  lList.Add(lData);

  lData := TPerson.Create;
  lData.Name    := 'Ian Krigsman';
  lData.Age     := 45;
  lData.Deleted := True;
  lList.Add(lData);

  lData := TPerson.Create;
  lData.Name    := 'John Guthrie';
  lData.Age     := 56;
  lList.Add(lData);

  Result := lList;
end;


{ TPerson }

function TPerson.IsValid(const pErrors: TtiObjectErrors): Boolean;
begin
  inherited IsValid(pErrors);

  if Name = '' then
    pErrors.AddError('Name', cNameMissing);

  if (Age < 1) or (Age > 100) then
    pErrors.AddError('Age', cAgeOutofRange);

  Result := pErrors.Count = 0;
end;

{ This was used for debugging, so you can see when NotifiObservers get called } 
procedure TPerson.NotifyObservers; 
begin
//  writeln('NotifyObservers');
  inherited NotifyObservers;
end;

procedure TPerson.SetAge(const Value: integer);
begin
  { BeginUpdate and EndUpdate are optional. They allow the observers to only
    get updated once, and not continuous for small updates. It doesn't really
    make a difference for this simple example though. }
  BeginUpdate;
  FAge := Value;
  EndUpdate;
  { If you don't use BeginUpdate and EndUpdate, you need to call NotifyObserver
    to they can be updated. }
//  NotifyObservers;
end;

procedure TPerson.SetMemento(const AValue: TPersonMemento);
begin
  // Update the Person state from the memento. Only if their OID's match.
  if (OID.AsString = AValue.FOID) then
  begin
    FName        := AValue.FName;
    FAge         := AValue.FAge;
    FGender      := AValue.FGender;
    ObjectState  := AValue.FObjectState;
  end;
end;

function TPerson.GetCaption: string;
begin
  Result := Name;
  if Deleted then
    Result := Result + ' (deleted)';
end;

constructor TPerson.Create;
begin
  inherited Create;
  FGender := genMale;
end;

procedure TPerson.SetName(const Value: string);
begin
  BeginUpdate;
  FName := Value;
  EndUpdate;
end;

procedure TPerson.SetGender(const AValue: TGender);
begin
  if FGender = AValue then exit;
  BeginUpdate;
  FGender := AValue;
  EndUpdate;
end;

function TPerson.GetGenderGUI: string;
begin
  result := cGender[FGender];
end;

function TPerson.GetMemento: TPersonMemento;
begin
  // Create a new memento, store the Centre state and return it.
  Result := TPersonMemento.Create;
  Result.FOID         := OID.AsString;
  Result.FObjectState := ObjectState;
  Result.FName        := FName;
  Result.FAge         := FAge;
  Result.FGender      := FGender;
end;

procedure TPerson.SetGenderGUI(const AValue: string);
var
  i: TGender;
begin
  for i := Low(TGender) to High(TGender) do
  begin
    if cGender[i] = AValue then
    begin
      Gender := i;
      Exit; //==>
    end;
  end;
  Gender := genMale;
end;


{ TPersonList }

function TPersonList.GetItems(i: integer): TPerson;
begin
  result := TPerson(inherited GetItems(i));
end;

procedure TPersonList.SetItems(i: integer; const Value: TPerson);
begin
  inherited SetItems(i, Value);
end;

procedure TPersonList.Add(const pObject: TPerson);
begin
  BeginUpdate;
  inherited Add(pObject);
  EndUpdate;
end;


end.

