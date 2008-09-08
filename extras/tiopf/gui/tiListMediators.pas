{
  Abstract mediating views for GUI list controls. This allows you to use
  standard list components and make them object-aware.  See the demo
  application for usage.
}
unit tiListMediators;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,SysUtils
  ,tiBaseMediator
  ,gui_listview
  ,gui_grid
  ,gui_listbox
  ,tiObject
  ;

  
type

  { Composite mediator for TfpgListView }
  TListViewMediator = class(TCustomListMediator)
  private
    FObserversInTransit: TList;
    FView: TfpgListView;
    procedure SetView(const AValue: TfpgListView);
  protected
    function  GetSelectedObject: TtiObject; override;
    procedure SetSelectedObject(const AValue: TtiObject);override;
    procedure CreateColumns; override;
    procedure DoCreateItemMediator(AData: TtiObject; ARowIdx : Integer); override;
    function  GetGuiControl : TComponent; override;
    procedure SetGuiControl (Const AValue : TComponent); override;
    procedure SetupGUIandObject; override;
    procedure ClearList; override;
    procedure RebuildList; override;
  public
    constructor CreateCustom(AModel: TtiObjectList; AView: TfpgListView; ADisplayNames: string; AIsObserving: Boolean = True); overload;
    constructor CreateCustom(AModel: TtiObjectList; AView: TfpgListView; AOnBeforeSetupField: TOnBeforeSetupField; ADisplayNames: string; AIsObserving: Boolean = True); overload;
    class function ComponentClass: TClass; override;
    Constructor Create; override;
    Destructor Destroy; override;
    procedure HandleSelectionChanged; override;
  published
    property    View: TfpgListView read FView Write SetView;
  end;


  // for backwards compatibility
  TCompositeListViewMediator = TListViewMediator;


  { Composite mediator for TfpgStringGrid }
  TStringGridMediator = class(TCustomListMediator)
  private
    FView: TfpgStringGrid;
    procedure DoCreateItemMediator(AData: TtiObject; ARowIdx : Integer); override;
    procedure SetView(const AValue: TfpgStringGrid);
  protected
    function  GetSelectedObject: TtiObject; override;
    procedure SetSelectedObject(const AValue: TtiObject);override;
    procedure CreateColumns; override;
    Function  GetGuiControl : TComponent; override;
    Procedure SetGuiControl (Const AValue : TComponent); override;
    procedure SetupGUIandObject; override;
    procedure RebuildList;override;
  public
    constructor CreateCustom(AModel: TtiObjectList; AGrid: TfpgStringGrid; ADisplayNames: string; AIsObserving: Boolean = True);
    class function ComponentClass: TClass; override;
  published
    property    View: TfpgStringGrid read FView Write SetView;
    property    SelectedObject: TtiObject read GetSelectedObject write SetSelectedObject;
  end;


  // for backwards compatibility
  TCompositeStringGridMediator = TStringGridMediator;


  { Used internally for sub-mediators in ListView mediator. Moved to interface
    section so it can be overridden. }
  TListViewListItemMediator = class(TListItemMediator)
  private
    FView: TfpgLVItem;
    procedure   SetupFields; virtual;
  public
    constructor CreateCustom(AModel: TtiObject; AView: TfpgLVItem; const AFieldsInfo : TtiMediatorFieldInfoList; IsObserving: Boolean = True);
    constructor CreateCustom(AModel: TtiObject; AView: TfpgLVItem; AOnBeforeSetupField: TOnBeforeSetupField; const AFieldsInfo : TtiMediatorFieldInfoList; IsObserving: Boolean = True); overload;
    procedure   BeforeDestruction; override;
    procedure   Update(ASubject: TtiObject); override;
  published
    property    View: TfpgLVItem read FView;
  end;


  { Used internally for sub-mediators in StringGrid mediator. Moved to interface
    section so it can be overridden. }
  TStringGridRowMediator = class(TListItemMediator)
  private
    FView: TfpgStringGrid;
    FRowIndex: Integer;
  public
    constructor CreateCustom(AModel: TtiObject; AGrid: TfpgStringGrid; Const AFieldsInfo : TtiMediatorFieldInfoList; ARowIndex: integer; IsObserving: Boolean = True);
    procedure   Update(ASubject: TtiObject); override;
  published
    property View: TfpgStringGrid read FView;
    Property RowIndex : Integer Read FRowIndex;
  end;


  { Used for presenting a list of objects in a TfpgListBox component. }
  TListBoxMediator = class(TCustomListMediator)
  private
    FView: TfpgListBox;
    FSelectedObject: TtiObject;
    FObserversInTransit: TList;
  protected
    function GetGUIControl: TComponent; override;
    function GetSelectedObject: TtiObject; override;
    procedure SetGUIControl(const AValue: TComponent);override;
    procedure SetSelectedObject(const AValue: TtiObject); override;
    procedure CreateColumns; override;
    procedure ClearList; override;
    procedure DoCreateItemMediator(AData: TtiObject; ARowIdx: integer); override;
    procedure RebuildList; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function CompositeMediator: Boolean; override;
    class function ComponentClass: TClass; override;
    procedure HandleSelectionChanged; override;
    property View: TfpgListBox read FView;
  end;


implementation


{ TListViewMediator }

procedure TListViewMediator.SetView(const AValue: TfpgListView);
begin
  FView:=AValue;
  SetGUIControl(AValue);
end;

function TListViewMediator.GetGuiControl: TComponent;
begin
  Result:=FView;
end;

procedure TListViewMediator.SetGuiControl(const AValue: TComponent);
begin
  FView:=AValue as TfpgListView;
  inherited SetGuiControl(AValue);
end;

procedure TListViewMediator.SetSelectedObject(const AValue: TtiObject);
var
  i: integer;
begin
  for i := 0 to FView.Items.Count-1 do
  begin
    if TtiObject(FView.Items.Item[i].UserData) = AValue then
    begin
//      FView.Selected := FView.Items.Item[i];
      FView.ItemIndex := i;
      HandleSelectionChanged;
      Exit; //==>
    end;
  end;
end;

function TListViewMediator.GetSelectedObject: TtiObject;
begin
//  if FView.SelCount = 0 then
  if FView.ItemIndex = -1 then
    Result := nil
  else
//    FSelectedObject := TtiObject(FView.Selected.Data);
    Result := TtiObject(FView.Items.Item[FView.ItemIndex].UserData);
end;


procedure TListViewMediator.DoCreateItemMediator(AData: TtiObject; ARowIdx : Integer);
var
  li: TfpgLVItem;
  m: TListViewListItemMediator;
begin
  DataAndPropertyValid(AData);
  
  { Create ListItem and Mediator }
  li := TfpgLVItem.Create(FView.Items);
  li.UserData := AData;
  FView.Items.Add(li);
  m := TListViewListItemMediator.CreateCustom(AData, li, OnBeforeSetupField, FieldsInfo, Active);
  MediatorList.Add(m);
end;

procedure TListViewMediator.CreateColumns;
var
  c: integer;
  lc: TfpgLVColumn;
  lInfo : TtiMediatorFieldInfo;
begin
  if View.Columns.Count = 0 then
  begin
    { Create column headers }
    for c := 0 to Pred(FieldsInfo.Count) do
    begin
      lInfo := FieldsInfo[c];
      lc := TfpgLVColumn.Create(View.Columns);
      lc.AutoSize   := False;
      lc.Caption    := lInfo.Caption;
      lc.Width      := lInfo.FieldWidth;
      lc.Alignment  := lInfo.Alignment;
      View.Columns.Add(lc);
    end;
  end;
end;

procedure TListViewMediator.SetupGUIandObject;
begin
  { Setup TfpgListView defaults }
  FView.Columns.Clear;
  FView.Items.Clear;
//  FView.ViewStyle         := vsReport;
  FView.ShowHeaders := True;
//  FView.RowSelect         := True;
//  FView.AutoSize          := False;
//  FView.ScrollBars        := ssAutoBoth;
end;

procedure TListViewMediator.ClearList;
begin
  View.Items.Clear;
end;

procedure TListViewMediator.RebuildList;
begin
  MediatorList.Clear;
//  ClearList;

  { This rebuilds the whole list. Not very efficient. You can always override
    this in your mediators to create a more optimised rebuild. }
  View.BeginUpdate;
  try
    View.Columns.Clear;
    View.Items.Clear;
    CreateSubMediators;
  finally
    View.EndUpdate;
  end;
end;

constructor TListViewMediator.CreateCustom(AModel: TtiObjectList;
  AView: TfpgListView; AOnBeforeSetupField: TOnBeforeSetupField;
  ADisplayNames: string; AIsObserving: Boolean);
begin
  Create; // don't forget this
  OnBeforeSetupField := AOnBeforeSetupField;
  DisplayNames  := ADisplayNames;      // Will call ParseDisplaynames.
  Subject      := AModel;
  GUIControl   := AView;               // Will call SetupGUIandObject;
  CreateSubMediators;
  Active  := AIsObserving;         // Will attach/Detach
end;

class function TListViewMediator.ComponentClass: TClass;
begin
  Result:=TfpgListView;
end;

constructor TListViewMediator.Create;
begin
  inherited Create;
  FObserversInTransit := TList.Create;
end;

constructor TListViewMediator.CreateCustom(AModel: TtiObjectList;
  AView: TfpgListView; ADisplayNames: string; AIsObserving: Boolean);
begin
  CreateCustom(AModel,AView,Nil,ADisplayNames,AIsObserving);
end;

Destructor TListViewMediator.Destroy;
begin
  IsObserving:=False;
  FView   := nil;
  inherited;
end;

procedure TListViewMediator.HandleSelectionChanged;
var
  i: integer;
begin
  if View.ItemIndex = -1 then
    SelectedObject := nil
  else
  begin
    FObserversInTransit.Clear;
    { If an item is already selected, assign the item's List of observers to a
      temporary container. This is done so that the same observers can be
      assigned to the new item. }
    if Assigned(SelectedObject) then
      FObserversInTransit.Assign(SelectedObject.ObserverList);

    // Assign Newly selected item to SelectedObject Obj.
    SelectedObject := TtiObject(View.Items.Item[View.ItemIndex].UserData);

    { If an object was selected, copy the old item's observer List
      to the new item's observer List. }
    if FObserversInTransit.Count > 0 then
      SelectedObject.ObserverList.Assign(FObserversInTransit);

    { Set the Observers Subject property to the selected object }
    for i := 0 to SelectedObject.ObserverList.Count-1 do
    begin
      TMediatorView(SelectedObject.ObserverList.Items[i]).Subject :=
          SelectedObject;
    end;

    // execute the NotifyObservers event to update the observers.
    SelectedObject.NotifyObservers;
  end;
end;


{ TListViewListItemMediator }

procedure TListViewListItemMediator.SetupFields;
var
  c: integer;
  lMemberName: string;
  lValue: string;
begin
  lMemberName :=FFieldsInfo[0].PropName;
  lValue:=FModel.PropValue[lMemberName];
  if Assigned(OnBeforeSetupField) then
    OnBeforeSetupField(FModel, lMemberName, lValue);
  FView.Caption := lValue;
  for c := 1 to FFieldsInfo.Count-1 do
    begin
    lMemberName := FFieldsInfo[c].PropName;
    lValue := FModel.PropValue[lMemberName];
    if Assigned(OnBeforeSetupField) then
      OnBeforeSetupField(FModel, lMemberName, lValue);
    FView.SubItems.Add(lValue);
    end;
end;

constructor TListViewListItemMediator.CreateCustom(AModel: TtiObject;
  AView: TfpgLVItem; const AFieldsInfo: TtiMediatorFieldInfoList;
  IsObserving: Boolean);
begin
  CreateCustom(AModel,AView,Nil,AFieldsInfo,IsObserving);
end;

constructor TListViewListItemMediator.CreateCustom(AModel: TtiObject;
  AView: TfpgLVItem; AOnBeforeSetupField: TOnBeforeSetupField;
  const AFieldsInfo: TtiMediatorFieldInfoList; IsObserving: Boolean);
begin
  inherited Create;
  FModel              := AModel;
  FView               := AView;
  FFieldsInfo         := AFieldsInfo;
  OnBeforeSetupField := AOnBeforeSetupField;
  SetupFields;
  Active:=IsObserving; // Will attach
end;

procedure TListViewListItemMediator.BeforeDestruction;
begin
  FModel.DetachObserver(self);
  FModel  := nil;
  FView   := nil;
  inherited BeforeDestruction;
end;

procedure TListViewListItemMediator.Update(ASubject: TtiObject);
var
  c: integer;
  lMemberName: string;
  lValue: string;
begin
  Assert(FModel = ASubject);

  lMemberName := FFieldsInfo[0].PropName;
  lValue := FModel.PropValue[lMemberName];
  if Assigned(OnBeforeSetupField) then
    OnBeforeSetupField(FModel, lMemberName, lValue);

  FView.Caption := lValue;

  for c := 1 to FFieldsInfo.Count-1 do
    begin
    lMemberName := FFieldsInfo[c].PropName;
    lValue := FModel.PropValue[lMemberName];
    if Assigned(OnBeforeSetupField) then
      OnBeforeSetupField(FModel, lMemberName, lValue);
    FView.SubItems[c-1] := lValue;
  end;
end;


{ TStringGridMediator }

function TStringGridMediator.GetSelectedObject: TtiObject;
begin
  if FView.FocusRow = -1 then
//  if FView.Selection.Top = 0 then
    Result := nil
  else
//    Result := TtiObject(FView.Objects[1, FView.Selection.Top]);
    Result := TtiObject(FView.Objects[0, FView.FocusRow]);
end;

procedure TStringGridMediator.SetSelectedObject(const AValue: TtiObject);
var
  i: integer;
begin
  for i := 0 to FView.RowCount-1 do
  begin
    if TtiObject(FView.Objects[0, i]) = AValue then
    begin
      FView.FocusRow := i;
      Exit; //==>
    end;
  end;
end;

procedure TStringGridMediator.SetView(const AValue: TfpgStringGrid);
begin
  SetGUIControl(AValue);
end;

function TStringGridMediator.GetGuiControl: TComponent;
begin
  Result:=fView;
end;

procedure TStringGridMediator.SetGuiControl(const AValue: TComponent);
begin
  FView:=AValue as TfpgStringGrid;
end;

procedure TStringGridMediator.DoCreateItemMediator(AData: TtiObject; ARowIdx: Integer);
var
  i: Integer;
  lFieldName: string;
  lMediatorView: TStringGridRowMediator;
begin
  FView.Objects[0, ARowIdx] := AData;   // set Object reference inside grid
  for i := 0 to FieldsInfo.Count-1 do
    begin
    lFieldName:=FieldsInfo[i].PropName;
    FView.Cells[i, ARowIdx]:=AData.PropValue[lFieldName];  // set Cell text
    end;
  lMediatorView := TStringGridRowMediator.CreateCustom(AData, FView, FieldsInfo, ARowIdx, Active);
  MediatorList.Add(lMediatorView);
end;

procedure TStringGridMediator.CreateColumns;
var
  i: integer;
  lColumnTotalWidth: integer;
begin
  lColumnTotalWidth := 0;
  for i := 0 to FieldsInfo.Count-1 do
    begin
    FView.ColumnWidth[i]  := FieldsInfo[i].FieldWidth;
    FView.ColumnTitle[i]  := FieldsInfo[i].Caption;
    FView.Columns[i].Alignment:=FieldsInfo[i].Alignment;
    //resize the last column to fill the grid.
    if i = FieldsInfo.Count-1 then
      begin
      If FView.Width > (lColumnTotalWidth + 10) then
        FView.ColumnWidth[i] := FView.Width - (lColumnTotalWidth + 10)
      end
    else
      lColumnTotalWidth := lColumnTotalWidth + FView.ColumnWidth[i] + 20;
  end;
end;

procedure TStringGridMediator.SetupGUIandObject;
begin
  //Setup default properties for the StringGrid
  FView.RowSelect     := True;
  FView.ColumnCount   := FieldsInfo.Count;
  if ShowDeleted then
    FView.RowCount := Model.Count
  else
    FView.RowCount := Model.CountNotDeleted;
end;

procedure TStringGridMediator.RebuildList;
begin
  { This rebuilds the whole list. Not very efficient. }
  View.BeginUpdate;
  try
    SetupGUIandObject;
    MediatorList.Clear;
//    for i := View.ColumnCount-1 downto 0 do
//      View.DeleteColumn(i);
    CreateSubMediators;
  finally
    View.EndUpdate;
  end;
end;

constructor TStringGridMediator.CreateCustom(AModel: TtiObjectList;
  AGrid: TfpgStringGrid; ADisplayNames: string; AIsObserving: Boolean);
begin
  inherited Create;
  DisplayNames := ADisplayNames;
  Subject      := AModel;
  GUIControl   := AGrid;
  CreateSubMediators;
  IsObserving  := AIsObserving;
end;

class function TStringGridMediator.ComponentClass: TClass;
begin
  Result:=TfpgStringGrid
end;


{ TStringGridRowMediator }

constructor TStringGridRowMediator.CreateCustom(AModel: TtiObject;
    AGrid: TfpgStringGrid; Const AFieldsInfo : TtiMediatorFieldInfoList; ARowIndex: integer;
    IsObserving: Boolean);
begin
  inherited Create;
  FModel        := AModel;
  FView         := AGrid;
  FFieldsInfo   := AFieldsInfo;
  FRowIndex     := ARowIndex;
  Active        :=IsObserving; // Will attach
end;

procedure TStringGridRowMediator.Update(ASubject: TtiObject);
var
  i: Integer;
  lvalue,
  lFieldName: string;
begin
  Assert(FModel = ASubject);
  for i := 0 to FFieldsInfo.Count-1 do
    begin
    lFieldName := FFieldsInfo[I].PropName;
    lValue := FModel.PropValue[lFieldName];
    if Assigned(OnBeforeSetupField) then
      OnBeforeSetupField(FModel, lFieldName, lValue);
    FView.Cells[i, FRowIndex] := lValue;
    end;
end;


{ TListBoxMediator }

function TListBoxMediator.GetGUIControl: TComponent;
begin
  Result := FView;
end;

function TListBoxMediator.GetSelectedObject: TtiObject;
begin
  Result := FSelectedObject;
end;

procedure TListBoxMediator.SetGUIControl(const AValue: TComponent);
begin
  FView := AValue as TfpgListBox;
  inherited SetGUIControl(AValue);
end;

procedure TListBoxMediator.SetSelectedObject(const AValue: TtiObject);
begin
  FSelectedObject := AValue;
end;

procedure TListBoxMediator.CreateColumns;
begin
  // do nothing
end;

procedure TListBoxMediator.ClearList;
begin

end;

procedure TListBoxMediator.DoCreateItemMediator(AData: TtiObject;
  ARowIdx: integer);
begin

end;

procedure TListBoxMediator.RebuildList;
var
  i: Integer;
  ptr: TNotifyEvent;
  selected: integer;
begin
  selected := -1;
  if (Model.CountNotDeleted) >= View.FocusItem+1 then
  begin
    selected := View.FocusItem;
  end;

  ptr := View.OnChange;
  View.OnChange := nil;
  View.BeginUpdate;
  try
    View.Items.Clear;
    for i := 0 to Model.Count-1 do
    begin
      if (not Model.Items[i].Deleted) or
         (ShowDeleted and Model.Items[i].Deleted) then
      begin
        View.Items.AddObject(Model.Items[i].Caption, Model.Items[i]);
      end;
    end;
    if Model.CountNotDeleted > 0 then
    begin
      if selected = -1 then
        selected := 0;
      View.FocusItem := selected;
    end;
  finally
    View.EndUpdate;
    View.Update;
    View.OnChange := ptr;
    HandleSelectionChanged;
  end;
end;

constructor TListBoxMediator.Create;
begin
  inherited Create;
  FSelectedObject := nil;
  FObserversInTransit := TList.Create;
end;

destructor TListBoxMediator.Destroy;
begin
  FObserversInTransit.Free;
  inherited Destroy;
end;

class function TListBoxMediator.CompositeMediator: Boolean;
begin
  Result := False;
end;

class function TListBoxMediator.ComponentClass: TClass;
begin
  Result := TfpgListBox;
end;

{$Note Refactor this as it's the same as TListView.HandleSelectionChanged, except
  for one single line. }
procedure TListBoxMediator.HandleSelectionChanged;
var
  i: integer;
begin
  if View.FocusItem = -1 then
    SelectedObject := nil
  else
  begin
    { If an item is already selected, assign the item's List of observers to a
      temporary container. This is done so that the same observers can be
      assigned to the new item. }
    if Assigned(FSelectedObject) then
      FObserversInTransit.Assign(FSelectedObject.ObserverList);

    // Assign Newly selected item to SelectedObject Obj.
    FSelectedObject := TtiObject(View.Items.Objects[View.FocusItem]);

    { If an object was selected, copy the old item's observer List
      to the new item's observer List. }
    if FObserversInTransit.Count > 0 then
      FSelectedObject.ObserverList.Assign(FObserversInTransit);

    { set the observers's Subject property to the selected object }
    for i := 0 to FSelectedObject.ObserverList.Count-1 do
    begin
      TMediatorView(FSelectedObject.ObserverList.Items[i]).Subject :=
          FSelectedObject;
    end;

    // execute the NotifyObservers event to update the observers.
    FSelectedObject.NotifyObservers;
  end;
end;

end.

