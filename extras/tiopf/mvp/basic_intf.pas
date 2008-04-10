unit basic_intf;

{$mode objfpc}{$H+}

interface

type
  // forward declarations
  ISubject = interface;

  
  IObserver = interface(IInterface)
  ['{16CD208B-5F37-41FC-82A4-BFDD16DB3203}']
    procedure Update(Subject: IInterface);
  end;
  
  
  ISubject = interface(IInterface)
  ['{004B3299-C221-4A44-87A7-7657D90B6493}']
    procedure Attach(Observer: IObserver);
    procedure Detach(Observer: IObserver);
    procedure Notify;
    procedure BeginUpdate;
    procedure EndUpdate;
  end;
  
  
  IVisitor = interface(IInterface)
  ['{35E154D2-6573-42DA-9854-156F3B19C95F}']
    // empty interface
  end;

  IVisited = interface(IInterface)
  ['{7CF62F51-9412-445C-9E8C-DE94F2B1E280}']
    procedure Accept(const Visitor: IVisitor);
  end;
  

  
  IListModel = interface(IInterface)
  ['{1A772375-1263-4790-8827-F7BEA358674A}']
    function GetCount: Integer;
    function GetItem(Idx: Integer): IInterface;
    procedure Add(Item: IInterface);
    procedure Clear;
    procedure Insert(Item, Before: IInterface);
    procedure Move(Item, Before: IInterface);
    procedure Remove(Item: IInterface);
    property Count: Integer read GetCount;
    property Item[Idx: Integer]: IInterface read GetItem;
  end;
{
  
  IController = interface(IInterface)
  ['{4A99C01A-D025-4562-8E94-3A0C873CE894}']
    function GetModel: IModel;
    function GetView: IView;
    procedure SetModel(const AValue: IModel);
    procedure SetView(const AValue: IView);
    property Model: IModel read GetModel write SetModel;
    property View: IView read GetView write SetView;
  end;
}

  IString = interface(IInterface)
  ['{E76984A4-1287-4353-8370-A7332B9FB1CB}']
    function GetAsString: string;
    procedure SetAsString(const AValue: string);
    property AsString: string read GetAsString write SetAsString;
  end;
  
  
  IStringListModel = interface(IInterface)
  ['{769804CD-89E4-43C7-B8EF-783BFE27214E}']
    function GetItem(Idx: Integer): IString;
    property Item[Idx: Integer]: IString read GetItem;
  end;
  
  
  ISelection = interface(IInterface)
  ['{F4DDA0EA-E982-4785-8602-5B32E8DD6DA2}']
    procedure AddItem(const Item: IInterface);
    procedure Clear;
    procedure RemoveItem(const Item: IInterface);
    procedure SelectModel;
  end;
  
  
  ICommand = interface(IInterface)
  ['{B333C7E1-B124-4D08-A640-DC02F36264C7}']
    procedure BindSelection(const Selection: ISelection);
    function Execute: Boolean;
    function GetText: string;
    property Text: string read GetText;
  end;
  
  
  ICommandSet = interface(IInterface)
  ['{1622FF69-3104-47EA-8741-9C1B05ADA30B}']
    // empty interface
  end;
  

implementation




end.

