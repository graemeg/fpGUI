(*

Revision history:

  2005-09-01: First release by Graeme Geldenhuys (graemeg@gmail.com)
  2007-08-27: Ported the code to the fpGUI toolkit.  [Graeme]

Purpose:
  Abstract mediating views for GUI list controls. This allows you to use
  standard list components and make them object-aware.  See the demo
  application for usage.

ToDo:
  * Unit tests
  * More refactoring
  * Implement a View Manager class, so we can remove the View Lists
    created in each Form using mediating views.
  * TfpgListView mediator implementation - I first need to double check how
    far the TfpgListView control itself has been implemented.

*)

unit tiGenericListMediators;

{$mode objfpc}{$H+}

interface
uses
  tiObject
  ,tiGenericEditMediators
  ,gfx_widget
  ,gui_listbox
  ,gui_combobox
  ,gui_listview
  ,gui_menu
  ,Classes
  ;


type
  { Used so we know what needs updating, the Internal List, or just the
    Selected Object. }
  TUpdateMode = (umSelectedObject, umObjectList);

  { Abstract class that observes a list object }

  { TListMediator }

  TListMediator = class(TtiObject)
  private
    FObjectList: TtiObjectList;
    FControl: TfpgWidget;
    FSelectedObject: TtiObject;
    FShowDeleted: Boolean;
    procedure   SetShowDeleted(const Value: Boolean);
  protected
    FObserversInTransit: TList;
    FUpdateMode: TUpdateMode;
    FPopupMenu: TfpgPopupMenu;
    procedure   SetSelectedObject(const Value: TtiObject); virtual;
    function    GetModel: TtiObjectList; virtual;
    procedure   SetModel(const Value: TtiObjectList); virtual;
    function    GetView: TfpgWidget; virtual;
    procedure   SetView(const Value: TfpgWidget); virtual;
    procedure   RebuildList; virtual; abstract;
    { Used to setup things like the MaxLength of a edit box, etc. }
    procedure   SetupGUIandObject; virtual;
    procedure   BuildPopupMenu; virtual;
  public
    constructor Create; override;
    constructor CreateCustom(pObjectList: TtiObjectList; pView: TfpgWidget); virtual;
    destructor  Destroy; override;
    procedure   Update(pSubject: TtiObject); override;

    { Called from GUI to trigger events }
    procedure   HandleDeleteItem; virtual;
    procedure   HandleListChanged; virtual;
    procedure   HandleSelectionChanged; virtual; abstract;
    procedure   MenuItemAddClick(Sender: TObject); virtual;
    procedure   MenuItemEditClick(Sender: TObject); virtual;
    procedure   MenuItemDeleteClick(Sender: TObject); virtual;

    property    SelectedObject: TtiObject read FSelectedObject write SetSelectedObject;
    property    ShowDeleted: Boolean read FShowDeleted write SetShowDeleted;
    property    Model: TtiObjectList read GetModel write SetModel;
    property    View: TfpgWidget read GetView;
  end;


  { Observes a list object - TfpgListBox }

  TListBoxMediator = class(TListMediator)
  private
    OldPos: Integer;
    NewPos: Integer;
  protected
    procedure   SetSelectedObject(const Value: TtiObject); override;
    function    GetView: TfpgListBox; reintroduce;
    procedure   RebuildList; override;
    procedure   SaveBookmark;
    procedure   RestoreBookmark;
  public
    procedure   HandleSelectionChanged; override;
  published
    property    View: TfpgListBox read GetView;
  end;


  { Observes a list object - TfpgComboBox }

  TComboBoxMediator = class(TListMediator)
  protected
    procedure   SetSelectedObject(const Value: TtiObject); override;
    function    GetView: TfpgComboBox; reintroduce;
    procedure   RebuildList; override;
  public
    procedure   HandleSelectionChanged; override;
  published
    property    View: TfpgComboBox read GetView;
  end;


  { Observes a list object - TListView }
(*
  TListViewMediator = class(TListMediator)
  protected
    function    GetView: TfpgListView; reintroduce;
    procedure   RebuildList; override;
  public
    procedure   HandleSelectionChanged; override;
  published
    property    View: TfpgListView read GetView;
  end;
*)

implementation
uses
  SysUtils
  ;


{ TListBoxMediator }

procedure TListBoxMediator.SetSelectedObject(const Value: TtiObject);
var
  i: integer;
begin
  inherited SetSelectedObject(Value);
  
  if Value = nil then
  begin
    View.FocusItem := 0;
    exit; //==>
  end;

  for i := 0 to Pred(Model.Count) do
  begin
    if Value.OID.AsString = Model.Items[i].OID.AsString then
    begin
      View.FocusItem := i+1;  // fpGUI is 1-based
      exit; //==>
    end;
  end;
end;

function TListBoxMediator.GetView: TfpgListBox;
begin
  result := TfpgListBox(inherited GetView);
end;

procedure TListBoxMediator.HandleSelectionChanged;
var
  i: integer;
begin
  if View.FocusItem = 0 then
    FSelectedObject := nil
  else
  begin
    { If an item is already selected, assign the item's List of observers to a
      temporary container. This is done so that the same observers can be
      assigned to the new item. }
    if Assigned(FSelectedObject) then
      FObserversInTransit.Assign(FSelectedObject.ObserverList);

    // Assign Newly selected item to SelectedObject Obj.
    FSelectedObject := TtiObject(View.Items.Objects[View.FocusItem-1]);

    { If an object was selected, copy the old item's observer List
      to the new item's observer List. }
    if FObserversInTransit.Count > 0 then
      FSelectedObject.ObserverList.Assign(FObserversInTransit);

    { set the observers's Subject property to the selected object }
    for i := 0 to FSelectedObject.ObserverList.Count - 1 do
    begin
      TMediatorView(FSelectedObject.ObserverList.Items[i]).Subject :=
          FSelectedObject;
    end;

    // execute the NotifyObservers event to update the observers.
    FSelectedObject.NotifyObservers;
  end;
end;


procedure TListBoxMediator.RebuildList;
var
  i: Integer;
  ptr: TNotifyEvent;
  selected: integer;
begin
  selected := 0;
  if (Model.CountNotDeleted) >= View.FocusItem then
  begin
    selected := View.FocusItem;
  end;

  ptr := View.OnChange;
  View.OnChange := nil;
  View.Items.BeginUpdate;
  try
    View.Items.Clear;
    for i := 0 to Pred(Model.Count) do
    begin
      if (not Model.Items[i].Deleted) or
         (ShowDeleted and Model.Items[i].Deleted) then
      begin
        View.Items.AddObject(Model.Items[i].Caption, Model.Items[i]);
      end;
    end;
    if Model.CountNotDeleted > 0 then
    begin
      if selected = 0 then
        selected := 1;
      View.FocusItem := selected;
    end;
  finally
    View.Items.EndUpdate;
    view.Update;
    View.OnChange := ptr;
    HandleSelectionChanged;
  end;
end;


procedure TListBoxMediator.RestoreBookmark;
begin
  if OldPos > View.Items.Count then
    NewPos := View.Items.Count
  else if OldPos = 0 then
    NewPos := 0
  else
    NewPos := OldPos;
  View.FocusItem := NewPos;
  HandleSelectionChanged;
end;

procedure TListBoxMediator.SaveBookmark;
begin
  OldPos := View.FocusItem;
end;


{ TComboBoxMediator }

procedure TComboBoxMediator.SetSelectedObject(const Value: TtiObject);
var
  i: integer;
begin
  inherited SetSelectedObject(Value);
  
  if Value = nil then
  begin
    View.FocusItem := 0;
    exit; //==>
  end;
  
  for i := 0 to Pred(Model.Count) do
  begin
    if Value = Model.Items[i] then
    begin
      View.FocusItem := i+1;  // fpGUI is 1-based
      exit; //==>
    end;
  end;
end;

function TComboBoxMediator.GetView: TfpgComboBox;
begin
  result := TfpgComboBox(inherited GetView);
end;


procedure TComboBoxMediator.HandleSelectionChanged;
var
  i: integer;
begin
  if View.FocusItem = 0 then
    SelectedObject := nil
  else
  begin
    if Assigned(SelectedObject) then
      FObserversInTransit.Assign(SelectedObject.ObserverList);

    SelectedObject := TtiObject(View.Items.Objects[View.FocusItem-1]);

    if FObserversInTransit.Count > 0 then
      SelectedObject.ObserverList.Assign(FObserversInTransit);

    for i := 0 to SelectedObject.ObserverList.Count - 1 do
    begin
      TMediatorView(SelectedObject.ObserverList.Items[i]).Subject :=
          SelectedObject;
    end;

    SelectedObject.NotifyObservers;
  end;
end;


procedure TComboBoxMediator.RebuildList;
var
  i: Integer;
  ptr: TNotifyEvent;
  selected: integer;
begin
  selected := 0;
  if (Model.CountNotDeleted-1) >= View.FocusItem then
    selected := View.FocusItem;

  ptr := View.OnChange;
  View.OnChange := nil;
  View.Items.BeginUpdate;
  try
    View.Items.Clear;
    for i := 0 to Pred(Model.Count) do
    begin
      if (not Model.Items[i].Deleted) or
         (ShowDeleted and Model.Items[i].Deleted) then
      begin
        View.Items.AddObject( Model.Items[i].Caption, Model.Items[i] );
      end;
    end;
    if Model.CountNotDeleted > 0 then
    begin
      if selected = 0 then
        selected := 1;
      View.FocusItem := selected;
    end;
  finally
    View.Items.EndUpdate;
    View.FocusItem := 1;
    View.OnChange := ptr;
    HandleSelectionChanged;
  end;
end;


{ TListViewMediator }

(*
function TListViewMediator.GetView: TfpgListView;
begin
  result := TfpgListView(inherited GetView);
end;


procedure TListViewMediator.HandleSelectionChanged;
var
  i: integer;
begin
  if not Assigned(View.Selected) then
    SelectedObject := nil
  else
  begin
    if Assigned(SelectedObject) then  // and Assigned(SelectedObject.ObserverList)
      FObserversInTransit.Assign( SelectedObject.ObserverList);

    SelectedObject := TtiObject(View.Selected.Data);

    if FObserversInTransit.Count > 0 then
      SelectedObject.ObserverList.Assign(FObserversInTransit);

    for i := 0 to SelectedObject.ObserverList.Count - 1 do
    begin
      TMediatorView(SelectedObject.ObserverList.Items[i]).Subject :=
          SelectedObject;
    end;

    SelectedObject.NotifyObservers;
  end;
end;


procedure TListViewMediator.RebuildList;
var
  i: Integer;
  lItem: TListItem;
  ptr: TLVChangeEvent;
begin
  ptr := View.OnChange;
  View.OnChange := nil;
  {$IFDEF FPC}
    View.BeginUpdate;
  {$ELSE}
    View.Items.BeginUpdate;
  {$ENDIF}
  try
    View.Items.Clear;
    for i := 0 to Pred(Model.Count) do
    begin
      if (not Model.Items[i].Deleted) or
         (ShowDeleted and Model.Items[i].Deleted) then
      begin
        lItem := View.Items.Add;
        lItem.Caption := Model.Items[i].Caption;
        lItem.Data := Model.Items[i];
      end;
    end;
    if Model.CountNotDeleted > 0 then
    begin
      SelectedObject := Model.Items[0];
      View.Selected := View.Items[0];
    end;
  finally
    {$IFDEF FPC}
      View.EndUpdate;
    {$ELSE}
      View.Items.EndUpdate;
    {$ENDIF}
    View.OnChange := ptr;
    HandleSelectionChanged;
  end;
end;
*)

{ TListMediator }

procedure TListMediator.BuildPopupMenu;
begin
  FPopupMenu := TfpgPopupMenu.Create(View);
  FPopupMenu.AddMenuItem('Add', '', @MenuItemAddClick);
  FPopupMenu.AddMenuItem('Edit', '', @MenuItemEditClick);
  FPopupMenu.AddMenuItem('Delete', '', @MenuItemDeleteClick);
end;


constructor TListMediator.Create;
begin
  inherited;
  FObserversInTransit := TList.Create;
  FShowDeleted  := False;
  { This is under construction. }
  FUpdateMode   := umObjectList;
end;


constructor TListMediator.CreateCustom(pObjectList: TtiObjectList; pView: TfpgWidget);
begin
  Create;
  Model := pObjectList;
  FControl := pView;
  BuildPopupMenu;
  Model.AttachObserver(self);
  SetupGUIandObject;
  
  // I prefer to do this once in the form after all mediator are created.
  Model.NotifyObservers;
end;


destructor TListMediator.Destroy;
begin
  FObserversInTransit.Free;
  Model.DetachObserver(self);
  inherited;
end;


function TListMediator.GetModel: TtiObjectList;
begin
  Result := FObjectList;
end;


function TListMediator.GetView: TfpgWidget;
begin
  Result := FControl;
end;


procedure TListMediator.HandleDeleteItem;
begin
  if not Assigned(SelectedObject) then
    Exit; //==>

  BeginUpdate;
  try
    SelectedObject.Deleted := True;
    RebuildList;
  finally
    EndUpdate;
  end;
end;


procedure TListMediator.HandleListChanged;
begin
  BeginUpdate;
  try
    RebuildList;
  finally
    EndUpdate;
  end;
end;


procedure TListMediator.MenuItemAddClick(Sender: TObject);
begin
  { do nothing here }
end;


procedure TListMediator.MenuItemDeleteClick(Sender: TObject);
begin
  { do nothing here }
end;


procedure TListMediator.MenuItemEditClick(Sender: TObject);
begin
  { do nothing here }
end;


procedure TListMediator.SetModel(const Value: TtiObjectList);
begin
  FObjectList := Value;
//  if FObjectList.Count > 0 then
//    FSelectedObject := FObjectList.Items[0];
end;


procedure TListMediator.SetSelectedObject(const Value: TtiObject);
begin
  FSelectedObject := Value;
end;


procedure TListMediator.SetShowDeleted(const Value: Boolean);
begin
  BeginUpdate;
  try
    FShowDeleted := Value;
    RebuildList;
  finally
    EndUpdate;
  end;
end;


procedure TListMediator.SetView(const Value: TfpgWidget);
begin
  FControl := Value;
end;


procedure TListMediator.SetupGUIandObject;
begin
  { Do nothing. Can be implemented in decendant classes. }
end;


procedure TListMediator.Update(pSubject: TtiObject);
begin
  BeginUpdate;
  try
//    inherited Update(pSubject);
    RebuildList;
  finally
    EndUpdate
  end;
end;


end.
