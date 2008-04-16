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


  TString = class(TInterfacedObject, IString, IVisited)
  private
    fString: string;
    // IString
    function GetAsString: string;
    procedure SetAsString(const AValue: string);
    // IVisited
    procedure Accept(const Visitor: IVisitor);
  end;
  
  
  TStringSelection = class(TInterfacedObject, ISelection, IVisited)
  private
    fModel: IStringListModel;
    fItems: IInterfaceList;
    // ISelection
    procedure AddItem(const Item: IInterface);
    procedure Clear;
    function GetCount: integer;
    procedure RemoveItem(const Item: IInterface);
    procedure SelectModel;
    // IVisited
    procedure Accept(const Visitor: IVisitor);
  public
    constructor Create(const Model: IStringListModel); virtual;
  end;
  
  
  TCommand = class(TInterfacedObject, ICommand, IVisited)
  private
    fSelection: ISelection;
    procedure BindSelection(const Selection: ISelection);
  protected
    // IVisited
    procedure Accept(const Visitor: IVisitor); virtual;
    // ICommand
    function Execute: Boolean; virtual; abstract;
    function GetEnabled: Boolean; virtual; abstract;
    function GetText: string; virtual; abstract;
  end;
  
  
  TCommandSet = class(TInterfacedObject, ICommandSet, IObserver, ISubject, IVisited)
  private
    fItems: IInterfaceList;
    fSubject: ISubject;
    function GetCount: integer;
    // IVisited
    procedure Accept(const Visitor: IVisitor);
  protected
    property Count: integer read GetCount;
    property Items: IInterfaceList read fItems;
    // IObserver
    procedure Update(Subject: IInterface);
    // ISubject
    property Subject: ISubject read fSubject implements ISubject;
  public
    constructor Create; virtual;
  end;
  
  
  TMVPModel = class(TInterfacedObject, IMVPModel, ISubject)
  private
    fCommandSet: ICommandSet;
    fCurrentSelection: ISelection;
    fSubject: ISubject;
    // IMVPModel
    function GetCommandSet: ICommandSet;
    function GetCurrentSelection: ISelection;
  protected
    property CommandSet: ICommandSet read GetCommandSet;
    property CurrentSelection: ISelection read GetCurrentSelection;
    // 3 methods to be called by the constructor
    procedure BindSelection; virtual;
    procedure CreateCommandSet(var ACommandSet: ICommandSet); virtual; abstract;
    procedure CreateSelection(var ASelection: ISelection); virtual; abstract;
    // ISubject
    property Subject: ISubject read fSubject implements ISubject;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;
  

  TListModel = class(TMVPModel, IListModel)
  private
    fItems: IInterfaceList;
  protected
    property Items: IInterfaceList read fItems;
    // IListModel
    function GetCount: Integer; virtual;
    function GetItem(Idx: Integer): IInterface; virtual;
    procedure Add(const Item: IInterface); virtual;
    procedure Clear; virtual;
    function IndexOf(const Item: IInterface): Integer; virtual;
    procedure Insert(const Item, Before: IInterface); virtual;
    procedure Move(const Item, Before: IInterface); virtual;
    procedure Remove(const Item: IInterface); virtual;
  public
    destructor Destroy; override;
  end;


  TStringListModel = class(TListModel, IStringListModel, IVisited)
  private
    // IStringListModel
    function IStringListModel.GetItem = StringListModelGetItem;
    function StringListModelGetItem(Idx: Integer): IString;
    // IVisited
    procedure Accept(const Visitor: IVisitor); virtual;
  protected
    // IMVPModel
    procedure CreateCommandSet(var ACommandSet: ICommandSet); override;
    procedure CreateSelection(var ASelection: ISelection); override;
  public
    destructor Destroy; override;
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

procedure TListModel.Add(const Item: IInterface);
begin
  Subject.BeginUpdate;
  if fItems = nil then
    fItems := TInterfaceList.Create;
//  fItems.Add(Item as IInterface);
  fItems.Add(Item);
  Subject.EndUpdate;
end;

procedure TListModel.Clear;
begin
  Subject.BeginUpdate;
  fItems.Clear;
  Subject.EndUpdate;
end;

function TListModel.IndexOf(const Item: IInterface): Integer;
begin
  if fItems <> nil then
//    Result := fItems.IndexOf(Item as IInterface)
    Result := fItems.IndexOf(Item)
  else
    Result := -1;
end;

procedure TListModel.Insert(const Item, Before: IInterface);
var
  InsertIdx: integer;
begin
  if fItems = nil then
    fItems := TInterfaceList.Create;
  if fItems.IndexOf(Item) < 0 then
  begin
    Subject.BeginUpdate;
    InsertIdx := fItems.IndexOf(Before);
    if InsertIdx < 0 then
      InsertIdx := 0;
    fItems.Insert(InsertIdx, Item);
    Subject.EndUpdate;
  end;
end;

procedure TListModel.Move(const Item, Before: IInterface);
var
  IdxItem: integer;
  IdxBefore: integer;
  MoveItem: IInterface;
begin
  if fItems <> nil then
  begin
    IdxItem := fItems.IndexOf(Item);
    if IdxItem >= 0 then
    begin
      Subject.BeginUpdate;
      MoveItem := fItems[IdxItem];
      fItems.Delete(IdxItem);
      IdxBefore := fItems.IndexOf(Before);
      if IdxBefore >0 then
        fItems.Insert(IdxBefore, MoveItem);
      Subject.EndUpdate;
    end;
  end;  { if }
end;

procedure TListModel.Remove(const Item: IInterface);
begin
  if fItems <> nil then
  begin
    Subject.BeginUpdate;
    fItems.Remove(Item);
    Subject.EndUpdate;
  end;
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

function TStringListModel.StringListModelGetItem(Idx: Integer): IString;
begin
  Result := Items[Idx] as IString;
end;

procedure TStringListModel.Accept(const Visitor: IVisitor);
begin

end;

procedure TStringListModel.CreateCommandSet(var ACommandSet: ICommandSet);
begin

end;

procedure TStringListModel.CreateSelection(var ASelection: ISelection);
begin

end;

destructor TStringListModel.Destroy;
begin
  inherited Destroy;
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

function TStringSelection.GetCount: integer;
begin
  Result := fItems.Count;
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

{ TCommand }

procedure TCommand.BindSelection(const Selection: ISelection);
begin
  fSelection := Selection;
end;

procedure TCommand.Accept(const Visitor: IVisitor);
begin
  (Visitor as ICommandVisitor).VisitComand(self);
end;

{ TCommandSet }

function TCommandSet.GetCount: integer;
begin
  if fItems <> nil then
    Result := fItems.Count
  else
    Result := 0;
end;

procedure TCommandSet.Accept(const Visitor: IVisitor);
var
  i: integer;
begin
  for i := 0 to fItems.Count-1 do
    (fItems[i] as IVisited).Accept(Visitor);
end;

procedure TCommandSet.Update(Subject: IInterface);
begin
  // do nothing yet
end;

constructor TCommandSet.Create;
begin
  inherited Create;
  fItems := TInterfaceList.Create;
end;

{ TMVPModel }

function TMVPModel.GetCommandSet: ICommandSet;
begin
  Result := fCommandSet;
end;

function TMVPModel.GetCurrentSelection: ISelection;
begin
  Result := fCurrentSelection;
end;

procedure TMVPModel.BindSelection;
begin
  (fCurrentSelection as ISubject).Attach(fCommandSet as IObserver);
end;

constructor TMVPModel.Create;
begin
  inherited Create;
  fSubject := TSubject.Create(self);
  CreateSelection(fCurrentSelection);
  CreateCommandSet(fCommandSet);
  BindSelection;
end;

destructor TMVPModel.Destroy;
begin
  inherited Destroy;
end;

end.

