{
  Abstract mediating views for GUI list controls. This allows you to use
  standard list components and make them object-aware.  See the demo
  application for usage.
}
unit tiListMediators;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  tiBaseMediator,
  fpg_listview,
  fpg_grid,
  tiObject;

type
  { Composite mediator for TfpgListView }
  TListViewMediator = class(TCustomListMediator)
  private
    FObserversInTransit: TList;
    FView: TfpgListView;
    procedure SetView(const AValue: TfpgListView);
  protected
    function GetSelectedObject: TtiObject; override;
    procedure SetSelectedObject(const AValue: TtiObject); override;
    procedure CreateColumns; override;
    procedure DoCreateItemMediator(AData: TtiObject; ARowIdx: integer); override;
    procedure DoDeleteItemMediator(AIndex: Integer; AMediator: TListItemMediator); override;
    function GetGuiControl: TComponent; override;
    procedure SetGuiControl(const AValue: TComponent); override;
    procedure SetupGUIandObject; override;
    procedure ClearList; override;
    procedure RebuildList; override;
  public
    constructor CreateCustom(AModel: TtiObjectList; AView: TfpgListView; ADisplayNames: string; AIsObserving: Boolean = True); overload;
    constructor CreateCustom(AModel: TtiObjectList; AView: TfpgListView; AOnBeforeSetupField: TOnBeforeSetupField; ADisplayNames: string; AIsObserving: Boolean = True); overload;
    class function ComponentClass: TClass; override;
    constructor Create; override;
    destructor Destroy; override;
    procedure HandleSelectionChanged; override;
    function GetObjectFromItem(AItem: TfpgLVItem): TtiObject;
  published
    property View: TfpgListView read FView write SetView;
  end;


  // for backwards compatibility
  TCompositeListViewMediator = TListViewMediator;


  { Composite mediator for TfpgStringGrid }
  TStringGridMediator = class(TCustomListMediator)
  private
    FView: TfpgStringGrid;
    procedure DoCreateItemMediator(AData: TtiObject; ARowIdx: integer); override;
    procedure DoDeleteItemMediator(AIndex: Integer; AMediator: TListItemMediator); override;
    procedure SetView(const AValue: TfpgStringGrid);
  protected
    function GetSelectedObject: TtiObject; override;
    procedure SetSelectedObject(const AValue: TtiObject); override;
    procedure CreateColumns; override;
    function GetGuiControl: TComponent; override;
    procedure SetGuiControl(const AValue: TComponent); override;
    procedure SetupGUIandObject; override;
    procedure ClearList; override;
    procedure RebuildList; override;
  public
    constructor CreateCustom(AModel: TtiObjectList; AGrid: TfpgStringGrid; ADisplayNames: string; AIsObserving: Boolean = True);
    destructor Destroy; override;
    class function ComponentClass: TClass; override;
    function GetObjectFromRow(ARow: Integer): TtiObject;
  published
    property View: TfpgStringGrid read FView write SetView;
  end;


  // for backwards compatibility
  TCompositeStringGridMediator = TStringGridMediator;


  { Used internally for sub-mediators in ListView mediator. Moved to interface
    section so it can be overridden. }
  TListViewListItemMediator = class(TListItemMediator)
  private
    FView: TfpgLVItem;
    procedure SetupFields; virtual;
  public
    constructor CreateCustom(AModel: TtiObject; AView: TfpgLVItem; const AFieldsInfo: TtiMediatorFieldInfoList; IsObserving: Boolean = True);
    constructor CreateCustom(AModel: TtiObject; AView: TfpgLVItem; AOnBeforeSetupField: TOnBeforeSetupField; const AFieldsInfo: TtiMediatorFieldInfoList; IsObserving: Boolean = True); overload;
    procedure BeforeDestruction; override;
    procedure Update(ASubject: TtiObject); override;
  published
    property View: TfpgLVItem read FView;
  end;


  { Used internally for sub-mediators in StringGrid mediator. Moved to interface
    section so it can be overridden. }
  TStringGridRowMediator = class(TListItemMediator)
  private
    FView: TfpgStringGrid;
    FRowIndex: integer;
  public
    constructor CreateCustom(AModel: TtiObject; AGrid: TfpgStringGrid; const AFieldsInfo: TtiMediatorFieldInfoList; ARowIndex: integer; IsObserving: Boolean = True);
    procedure Update(ASubject: TtiObject); override;
  published
    property View: TfpgStringGrid read FView;
    property RowIndex: integer read FRowIndex;
  end;


implementation


{ TListViewMediator }

procedure TListViewMediator.SetView(const AValue: TfpgListView);
begin
  FView := AValue;
  SetGUIControl(AValue);
end;

function TListViewMediator.GetGuiControl: TComponent;
begin
  Result := FView;
end;

procedure TListViewMediator.SetGuiControl(const AValue: TComponent);
begin
  FView := AValue as TfpgListView;
  inherited SetGuiControl(AValue);
end;

procedure TListViewMediator.SetSelectedObject(const AValue: TtiObject);
var
  i: integer;
begin
  for i := 0 to FView.Items.Count - 1 do
    if TtiObject(FView.Items.Item[i].UserData) = AValue then
    begin
      FView.ItemIndex := i;
      HandleSelectionChanged;   {$Note Is this line required?}
      Exit; //==>
    end;
end;

function TListViewMediator.GetSelectedObject: TtiObject;
begin
  Result := GetObjectFromItem(FView.Items.Item[FView.ItemIndex]);
end;

procedure TListViewMediator.DoCreateItemMediator(AData: TtiObject; ARowIdx: integer);
var
  li: TfpgLVItem;
  m: TListViewListItemMediator;
begin
  DataAndPropertyValid(AData);

  { Create ListItem and Mediator }
  FView.BeginUpdate;
  try
    li          := TfpgLVItem.Create(FView.Items);
    FView.Items.Add(li);
    m           := TListViewListItemMediator.CreateCustom(AData, li, OnBeforeSetupField, FieldsInfo, Active);
    li.UserData := m;
    MediatorList.Add(m);
  finally
    FView.EndUpdate;
  end;
end;

procedure TListViewMediator.DoDeleteItemMediator(AIndex: Integer; AMediator: TListItemMediator);
begin
  FView.Items.Delete(FView.Items.IndexOf(TListViewListItemMediator(AMediator).FView));
  inherited DoDeleteItemMediator(AIndex, AMediator);
end;

procedure TListViewMediator.CreateColumns;
var
  c: integer;
  lc: TfpgLVColumn;
  lInfo: TtiMediatorFieldInfo;
begin
  if (View.Columns.Count<>FieldsInfo.Count) then
    View.Columns.Clear;
  if View.Columns.Count = 0 then
  begin
    for c := 0 to FieldsInfo.Count-1 do
    begin
      lInfo        := FieldsInfo[c];
      lc           := TfpgLVColumn.Create(View.Columns);
      lc.AutoSize  := False;
      lc.Caption   := lInfo.Caption;
      lc.Width     := lInfo.FieldWidth;
//      lc.CaptionAlignment:= lInfo.Alignment;
      lc.Alignment := lInfo.Alignment;
      lc.Resizable := True;
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
  View.BeginUpdate;
  try
    CreateColumns;
    CreateSubMediators;
  finally
    View.EndUpdate;
  end;
end;

constructor TListViewMediator.CreateCustom(AModel: TtiObjectList; AView: TfpgListView; AOnBeforeSetupField: TOnBeforeSetupField; ADisplayNames: string; AIsObserving: Boolean);
begin
  Create; // don't forget this
  OnBeforeSetupField := AOnBeforeSetupField;
  DisplayNames := ADisplayNames;      // Will call ParseDisplaynames.
  Subject    := AModel;
  GUIControl := AView;               // Will call SetupGUIandObject;
  CreateSubMediators;
  Active     := AIsObserving;         // Will attach/Detach
end;

class function TListViewMediator.ComponentClass: TClass;
begin
  Result := TfpgListView;
end;

constructor TListViewMediator.Create;
begin
  inherited Create;
  FObserversInTransit := TList.Create;
end;

constructor TListViewMediator.CreateCustom(AModel: TtiObjectList; AView: TfpgListView; ADisplayNames: string; AIsObserving: Boolean);
begin
  CreateCustom(AModel, AView, nil, ADisplayNames, AIsObserving);
end;

destructor TListViewMediator.Destroy;
begin
  IsObserving := False;
  FView       := nil;
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
    for i := 0 to SelectedObject.ObserverList.Count - 1 do
      TMediatorView(SelectedObject.ObserverList.Items[i]).Subject :=
        SelectedObject;

    // execute the NotifyObservers event to update the observers.
    SelectedObject.NotifyObservers;
  end;
end;

function TListViewMediator.GetObjectFromItem(AItem: TfpgLVItem): TtiObject;
begin
  if (AItem = nil) or (AItem.UserData = nil) then
    Result := nil
  else
    Result := TListItemMediator(AItem.UserData).Model;
end;


{ TListViewListItemMediator }

procedure TListViewListItemMediator.SetupFields;
var
  c: integer;
  lMemberName: string;
  lValue: string;
begin
  lMemberName := FFieldsInfo[0].PropName;
  lValue      := Model.PropValue[lMemberName];
  if Assigned(OnBeforeSetupField) then
    OnBeforeSetupField(Model, lMemberName, lValue);
  FView.Caption := lValue;
  for c := 1 to FFieldsInfo.Count - 1 do
  begin
    lMemberName := FFieldsInfo[c].PropName;
    lValue      := Model.PropValue[lMemberName];
    if Assigned(OnBeforeSetupField) then
      OnBeforeSetupField(Model, lMemberName, lValue);
    FView.SubItems.Add(lValue);
  end;
end;

constructor TListViewListItemMediator.CreateCustom(AModel: TtiObject; AView: TfpgLVItem; const AFieldsInfo: TtiMediatorFieldInfoList; IsObserving: Boolean);
begin
  CreateCustom(AModel, AView, nil, AFieldsInfo, IsObserving);
end;

constructor TListViewListItemMediator.CreateCustom(AModel: TtiObject; AView: TfpgLVItem; AOnBeforeSetupField: TOnBeforeSetupField; const AFieldsInfo: TtiMediatorFieldInfoList; IsObserving: Boolean);
begin
  inherited Create;
  Model      := AModel;
  FView       := AView;
  FFieldsInfo := AFieldsInfo;
  OnBeforeSetupField := AOnBeforeSetupField;
  SetupFields;
  Active      := IsObserving; // Will attach
end;

procedure TListViewListItemMediator.BeforeDestruction;
begin
  Model.DetachObserver(self);
  Model := nil;
  FView  := nil;
  inherited BeforeDestruction;
end;

procedure TListViewListItemMediator.Update(ASubject: TtiObject);
var
  c: integer;
  lMemberName: string;
  lValue: string;
begin
  Assert(Model = ASubject);

  lMemberName := FFieldsInfo[0].PropName;
  lValue      := Model.PropValue[lMemberName];
  if Assigned(OnBeforeSetupField) then
    OnBeforeSetupField(Model, lMemberName, lValue);

  FView.Caption := lValue;

  for c := 1 to FFieldsInfo.Count - 1 do
  begin
    lMemberName := FFieldsInfo[c].PropName;
    lValue      := Model.PropValue[lMemberName];
    if Assigned(OnBeforeSetupField) then
      OnBeforeSetupField(Model, lMemberName, lValue);
    FView.SubItems[c - 1] := lValue;
  end;
end;


{ TStringGridMediator }

function TStringGridMediator.GetSelectedObject: TtiObject;
begin
  Result := GetObjectFromRow(FView.FocusRow);
end;

procedure TStringGridMediator.SetSelectedObject(const AValue: TtiObject);
var
  i: integer;
  o: TObject;
begin
  for i := 0 to FView.RowCount - 1 do
  begin
    o := FView.Objects[0, i];
    if Assigned(o) and (TtiObject(o) = AValue) then
    begin
      FView.FocusRow := i;
      Exit; //==>
    end;
  end;  { for }
end;

procedure TStringGridMediator.SetView(const AValue: TfpgStringGrid);
begin
  SetGUIControl(AValue);
end;

function TStringGridMediator.GetGuiControl: TComponent;
begin
  Result := fView;
end;

procedure TStringGridMediator.SetGuiControl(const AValue: TComponent);
begin
  FView := AValue as TfpgStringGrid;
end;

procedure TStringGridMediator.DoCreateItemMediator(AData: TtiObject; ARowIdx: integer);
var
  i: integer;
  lFieldName: string;
  lMediatorView: TStringGridRowMediator;
begin
  FView.BeginUpdate;
  try
    if ARowIdx = FView.RowCount then // In case of add notification
      FView.RowCount := FView.RowCount+1;
    for i := 0 to FieldsInfo.Count - 1 do
    begin
      lFieldName := FieldsInfo[i].PropName;
      FView.Cells[i, ARowIdx] := AData.PropValue[lFieldName];  // set Cell text
    end;
    lMediatorView := TStringGridRowMediator.CreateCustom(AData, FView, FieldsInfo, ARowIdx, Active);
    FView.Objects[0, ARowIdx] := lMediatorView;   // set Object reference inside grid. It used to be AData.
    MediatorList.Add(lMediatorView);
  finally
    FView.EndUpdate;
  end;
end;

procedure TStringGridMediator.DoDeleteItemMediator(AIndex: Integer; AMediator: TListItemMediator);
begin
  FView.DeleteRow(AIndex);
  inherited DoDeleteItemMediator(AIndex, AMediator);
end;

procedure TStringGridMediator.CreateColumns;
var
  i: integer;
  lColumnTotalWidth: integer;
begin
  lColumnTotalWidth := 0;
  for i := 0 to FieldsInfo.Count - 1 do
  begin
    FView.ColumnWidth[i]       := FieldsInfo[i].FieldWidth;
    FView.ColumnTitle[i]       := FieldsInfo[i].Caption;
    FView.Columns[i].Alignment := FieldsInfo[i].Alignment;
    //resize the last column to fill the grid.
    if i = FieldsInfo.Count - 1 then
    begin
      if FView.Width > (lColumnTotalWidth + 10) then
        FView.ColumnWidth[i] := FView.Width - (lColumnTotalWidth + 10);
    end
    else
      lColumnTotalWidth := lColumnTotalWidth + FView.ColumnWidth[i] + 20;
  end;
end;

procedure TStringGridMediator.SetupGUIandObject;
begin
  //Setup default properties for the StringGrid
  FView.RowSelect   := True;
  FView.ColumnCount := FieldsInfo.Count;
  if ShowDeleted then
    FView.RowCount := Model.Count
  else
    FView.RowCount := Model.CountNotDeleted;
end;

procedure TStringGridMediator.ClearList;
begin
  MediatorList.Clear;
  if Assigned(View) then
    View.RowCount := 1; {$Note Double check if this is desired. Shouldn't it be 0 instead. }
end;

procedure TStringGridMediator.RebuildList;
begin
//  writeln('--- TStringGridMediator.RebuildList');
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

constructor TStringGridMediator.CreateCustom(AModel: TtiObjectList; AGrid: TfpgStringGrid; ADisplayNames: string; AIsObserving: Boolean);
begin
  inherited Create;
  DisplayNames := ADisplayNames;
  Subject      := AModel;
  GUIControl   := AGrid;
  CreateSubMediators;
  IsObserving  := AIsObserving;
end;

destructor TStringGridMediator.Destroy;
begin
  IsObserving := False;
  FView       := nil;
  inherited Destroy;
end;

class function TStringGridMediator.ComponentClass: TClass;
begin
  Result := TfpgStringGrid;
end;

function TStringGridMediator.GetObjectFromRow(ARow: Integer): TtiObject;
var
  O: TObject;
begin
  if FView.RowCount = 0 then
  begin
    Result := nil;
    Exit;
  end;

  if FView.FocusRow = -1 then
    Result := nil
  else
  begin
    O := FView.Objects[0, ARow];
    if O <> nil then
      Result := TListItemMediator(O).Model
    else
      Result := nil;
  end;
end;


{ TStringGridRowMediator }

constructor TStringGridRowMediator.CreateCustom(AModel: TtiObject; AGrid: TfpgStringGrid; const AFieldsInfo: TtiMediatorFieldInfoList; ARowIndex: integer; IsObserving: Boolean);
begin
  inherited Create;
  Model      := AModel;
  FView       := AGrid;
  FFieldsInfo := AFieldsInfo;
  FRowIndex   := ARowIndex;
  Active      := IsObserving; // Will attach
end;

procedure TStringGridRowMediator.Update(ASubject: TtiObject);
var
  i: integer;
  lvalue, lFieldName: string;
begin
  Assert(Model = ASubject);
  for i := 0 to FFieldsInfo.Count - 1 do
  begin
    lFieldName := FFieldsInfo[I].PropName;
    lValue     := Model.PropValue[lFieldName];
    if Assigned(OnBeforeSetupField) then
      OnBeforeSetupField(Model, lFieldName, lValue);
    FView.Cells[i, FRowIndex] := lValue;
  end;
end;


end.

