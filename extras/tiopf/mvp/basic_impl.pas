unit basic_impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, basic_intf;
  
type

  TSubject = class(TInterfacedObject, ISubject)
  private
    fController: Pointer;
    fObservers: IInterfaceList;
    fUpdateCount: integer;
    function GetController: IInterface;
    procedure Attach(Observer: IObserver);
    procedure Detach(Observer: IObserver);
    procedure Notify;
    procedure BeginUpdate;
    procedure EndUpdate;
  public
    constructor Create(const Controller: IInterface);
  end;


  TListModel = class(TInterfacedObject, IListModel, ISubject)
  private
    fItems: IInterfaceList;
    fSubject: ISubject;
  protected
    property Items: IInterfaceList read fItems;
    // IListModel
    function GetCount: Integer;
    function GetItem(Idx: Integer): IInterface;
    procedure Add(Item: IInterface);
    procedure Clear;
    procedure Insert(Item, Before: IInterface);
    procedure Move(Item, Before: IInterface);
    procedure Remove(Item: IInterface);
    // ISubject
    property Subject: ISubject read FSubject implements ISubject;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;
  
  
  TString = class(TInterfacedObject, IString, IVisited)
  private
    fString: string;
    // IString
    function GetAsString: string;
    procedure SetAsString(const AValue: string);
    // IVisited
    procedure Accept(const Visitor: IVisitor);
  end;
  
  
  TStringListModel = class(TListModel, IStringListModel)
  private
    // IStringListModel
    function GetItem(Idx: Integer): IString;
  end;
  
  
  TStringSelection = class(TInterfacedObject, ISelection, IVisited)
  private
    fModel: IStringListModel;
    fItems: IInterfaceList;
    // ISelection
    procedure AddItem(const Item: IInterface);
    procedure Clear;
    procedure RemoveItem(const Item: IInterface);
    procedure SelectModel;
    // IVisited
    procedure Accept(const Visitor: IVisitor);
  public
    constructor Create(const Model: IStringListModel); virtual;
  end;
  
  

implementation

{ TSubject }

function TSubject.GetController: IInterface;
begin
  Result := IInterface(fController);
end;

procedure TSubject.Attach(Observer: IObserver);
begin
  if fObservers = nil then
    fObservers := TInterfaceList.Create;
  if fObservers.IndexOf(Observer) < 0 then
    fObservers.Add(Observer);
end;

procedure TSubject.Detach(Observer: IObserver);
begin
  if fObservers <> nil then
  begin
    if fObservers.IndexOf(Observer) >= 0 then
      fObservers.Remove(Observer);
    if fObservers.Count = 0 then
      fObservers := nil;
  end;
end;

procedure TSubject.Notify;
var
  i: integer;
begin
  if fObservers <> nil then
    for i := 0 to fObservers.Count-1 do
      (fObservers[i] as IObserver).Update(GetController);
end;

procedure TSubject.BeginUpdate;
begin
  Inc(fUpdateCount);
end;

procedure TSubject.EndUpdate;
begin
  Dec(fUpdateCount);
  if fUpdateCount = 0 then
    Notify;
end;

constructor TSubject.Create(const Controller: IInterface);
begin
  inherited Create;
  fController := Pointer(Controller);
end;

{ TListModel }

function TListModel.GetCount: Integer;
begin
  Result := fItems.Count;
end;

function TListModel.GetItem(Idx: Integer): IInterface;
begin
  Result := fItems[Idx];
end;

procedure TListModel.Add(Item: IInterface);
begin
  fSubject.BeginUpdate;
  fItems.Add(Item);
  fSubject.EndUpdate;
end;

procedure TListModel.Clear;
begin
  fSubject.BeginUpdate;
  fItems.Clear;
  fSubject.EndUpdate;
end;

procedure TListModel.Insert(Item, Before: IInterface);
begin
  fSubject.BeginUpdate;
  fItems.Insert(fItems.IndexOf(Before), Item);
  fSubject.EndUpdate;
end;

procedure TListModel.Move(Item, Before: IInterface);
var
  IndexOfBefore: integer;
begin
  fSubject.BeginUpdate;
  IndexOfBefore := fItems.IndexOf(Before);
  if IndexOfBefore < 0 then
    IndexOfBefore := 0;
  fItems.Delete(fItems.IndexOf(Item));
  fItems.Insert(IndexOfBefore, Item);
  fSubject.EndUpdate;
end;

procedure TListModel.Remove(Item: IInterface);
begin
  fSubject.BeginUpdate;
  fItems.Delete(fItems.IndexOf(Item));
  fSubject.EndUpdate;
end;

constructor TListModel.Create;
begin
  inherited Create;
end;

destructor TListModel.Destroy;
begin
  inherited Destroy;
end;

{ TString }

function TString.GetAsString: string;
begin

end;

procedure TString.SetAsString(const AValue: string);
begin

end;

procedure TString.Accept(const Visitor: IVisitor);
begin

end;

{ TStringListModel }

function TStringListModel.GetItem(Idx: Integer): IString;
begin
  Result := Items[Idx] as IString;
end;

{ TStringSelection }

procedure TStringSelection.AddItem(const Item: IInterface);
begin
  if fItems = nil then
    fItems := TInterfaceList.Create;
  if fItems.IndexOf(Item) < 0 then
    fItems.Add(Item);
end;

procedure TStringSelection.Clear;
begin

end;

procedure TStringSelection.RemoveItem(const Item: IInterface);
begin
  if fItems <> nil then
  begin
    if fItems.IndexOf(Item) >= 0 then
      fItems.Remove(Item);
    if fItems.Count = 0 then
      fItems := nil;
  end;
end;

procedure TStringSelection.SelectModel;
var
  i: integer;
begin
  for i := 0 to (fModel as IListModel).Count-1 do
    fItems.Add(fModel.Item[i]);
end;

procedure TStringSelection.Accept(const Visitor: IVisitor);
var
  i: integer;
begin
  for i := 0 to fItems.Count-1 do
    (fItems[i] as IVisited).Accept(Visitor);
end;

constructor TStringSelection.Create(const Model: IStringListModel);
begin
  inherited Create;
  fModel := Model;
end;

end.

