unit basic_intf;

{$mode objfpc}{$H+}

interface

type
  // forward declarations
  ISubject = interface;
  IMVPView = interface;
  ICommandMenuItem = interface;
  IString = interface;


  // event types
  TSelectStringEvent = procedure(const AString: IString) of object;

  
  IObserver = interface(IInterface)
  ['{16CD208B-5F37-41FC-82A4-BFDD16DB3203}']
    procedure Update(const ASubject: IInterface);
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
    procedure Add(const Item: IInterface);
    procedure Clear;
    function IndexOf(const Item: IInterface): Integer;
    procedure Insert(const Item, Before: IInterface);
    procedure Move(const Item, Before: IInterface);
    procedure Remove(const Item: IInterface);
    property Count: Integer read GetCount;
    property Item[Idx: Integer]: IInterface read GetItem;
  end;

(*
  IController = interface(IInterface)
  ['{4A99C01A-D025-4562-8E94-3A0C873CE894}']
    function GetModel: IModel;
    function GetView: IView;
    procedure SetModel(const AValue: IModel);
    procedure SetView(const AValue: IView);
    property Model: IModel read GetModel write SetModel;
    property View: IView read GetView write SetView;
  end;
*)

  IString = interface(IInterface)
  ['{E76984A4-1287-4353-8370-A7332B9FB1CB}']
    function GetAsString: string;
    procedure SetAsString(const AValue: string);
    property AsString: string read GetAsString write SetAsString;
  end;
  
  
  IStringListModel = interface(IListModel)
  ['{769804CD-89E4-43C7-B8EF-783BFE27214E}']
    function GetItem(Idx: Integer): IString; overload;
    property Item[Idx: Integer]: IString read GetItem;
  end;
  
  
  ISelection = interface(IInterface)
  ['{F4DDA0EA-E982-4785-8602-5B32E8DD6DA2}']
    procedure AddItem(const Item: IInterface);
    procedure Clear;
    function GetCount: integer;
    procedure RemoveItem(const Item: IInterface);
    property Count: integer read GetCount;
  end;
  
  
  ICommand = interface(IInterface)
  ['{B333C7E1-B124-4D08-A640-DC02F36264C7}']
    procedure BindSelection(const Selection: ISelection);
    function Execute: Boolean;
    function GetEnabled: Boolean;
    function GetText: string;
    property Enabled: Boolean read GetEnabled;
    property Text: string read GetText;
  end;
  
  
  ICommandSet = interface(IInterface)
  ['{1622FF69-3104-47EA-8741-9C1B05ADA30B}']
    // empty interface
  end;
  
  
  ICommandVisitor = interface(IVisitor)
  ['{628B3A4A-30D1-48D3-8B46-090F08AD2AC8}']
    procedure VisitComand(const Command: ICommand);
  end;
  
  
  ICommandMenu = interface(IInterface)
  ['{3C666D8F-6BED-454B-8BFE-28422943B300}']
    function AddItem(const Caption: string; Enabled: Boolean): ICommandMenuItem;
  end;
  
  
  ICommandMenuItem = interface(IInterface)
  ['{7DFCF2BD-70DA-4DAC-B8D5-C6FB882267CF}']
    function GetCaption: string;
    function GetChecked: Boolean;
    function GetCommand: ICommand;
    procedure SetCaption(const AValue: string);
    procedure SetChecked(const AValue: Boolean);
    procedure SetCommand(const AValue: ICommand);
    property Caption: string read GetCaption write SetCaption;
    property Checked: Boolean read GetChecked write SetChecked;
    property Command: ICommand read GetCommand write SetCommand;
  end;
  

  IStringVisitor = interface(IVisitor)
  ['{DA12355F-0727-41B3-9080-DDAF20797FC5}']
    function GetTheString: IString;
    procedure VisitString(const Str: IString);
    property TheString: IString
             read GetTheString;
  end;
  
  
  IMVPModel = interface(IInterface)
  ['{85223140-B263-4413-89E3-BFA37E9D3112}']
    function GetCommandSet: ICommandSet;
    function GetCurrentSelection: ISelection;
    property CommandSet: ICommandSet read GetCommandSet;
    property CurrentSelection: ISelection read GetCurrentSelection;
  end;
  

  IMVPPresenter = interface(IInterface)
  ['{5B8477DA-A006-4DE1-B304-9512BFAD7507}']
    function GetCommandMenu: ICommandMenu;
    function GetModel: IMVPModel;
    function GetView: IMVPView;
    procedure SetCommandMenu(const AValue: ICommandMenu);
    procedure SetModel(const AValue: IMVPModel);
    procedure SetView(const AValue: IMVPView);
    property CommandMenu: ICommandMenu read GetCommandMenu write SetCommandMenu;
    property Model: IMVPModel read GetModel write SetModel;
    property View: IMVPView read GetView write SetView;
  end;
  
  
  IMVPView = interface(IInterface)
  ['{2C575FE7-BACD-46EC-9D72-AEDA44836B20}']
    procedure AdoptCommandMenu(const Value: ICommandMenu);
    procedure OrphanCommandMenu(const Value: ICommandMenu);
  end;
  
  
  IStringListView = interface(IMVPView)
  ['{D834710A-9C1A-42D1-A29B-7F9F8FB46426}']
    function GetOnSelectString: TSelectStringEvent;
    procedure SetOnSelectString(const AValue: TSelectStringEvent);
    property OnSelectString: TSelectStringEvent read GetOnSelectString write SetOnSelectString;
  end;


  IStringMoveVisitor = interface(IStringVisitor)
  ['{DB89C96F-DA90-43ED-A621-51B70E6C600E}']
    function GetCanDemote: Boolean;
    function GetCanPromote: Boolean;
    property CanDemote: Boolean read GetCanDemote;
    property CanPromote: Boolean read GetCanPromote;
  end;
  
  
  
  

implementation




end.

