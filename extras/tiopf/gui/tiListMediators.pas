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
  TtiListViewMediatorView = class(TtiCustomListMediatorView)
  private
    FObserversInTransit: TList;
  protected
    function    GetSelectedObject: TtiObject; override;
    procedure   SetSelectedObject(const AValue: TtiObject); override;
    procedure   CreateColumns; override;
    function    DoCreateItemMediator(AData: TtiObject; ARowIdx: integer): TtiListItemMediator; override;
    procedure   DoDeleteItemMediator(AIndex: Integer; AMediator: TtiListItemMediator); override;
    procedure   SetupGUIandObject; override;
    procedure   ClearList; override;
    procedure   RebuildList; override;
    procedure   SetActive(const AValue: Boolean); override;
  public
    constructor CreateCustom(AModel: TtiObjectList; AView: TfpgListView; ADisplayNames: string; AIsObserving: Boolean = True); overload;
    constructor CreateCustom(AModel: TtiObjectList; AView: TfpgListView; AOnBeforeSetupField: TtiOnBeforeSetupField; ADisplayNames: string; AIsObserving: Boolean = True); overload;
    class function ComponentClass: TClass; override;
    constructor Create; override;
    destructor  Destroy; override;
    function    View: TfpgListView; reintroduce;
    procedure   HandleSelectionChanged; override;
    function    GetObjectFromItem(AItem: TfpgLVItem): TtiObject;
  end;


  { Composite mediator for TfpgStringGrid }
  TtiStringGridMediatorView = class(TtiCustomListMediatorView)
  private
    function    DoCreateItemMediator(AData: TtiObject; ARowIdx: integer): TtiListItemMediator; override;
    procedure   DoDeleteItemMediator(AIndex: Integer; AMediator: TtiListItemMediator); override;
  protected
    function    GetSelectedObject: TtiObject; override;
    procedure   SetSelectedObject(const AValue: TtiObject); override;
    procedure   CreateColumns; override;
    procedure   SetupGUIandObject; override;
    procedure   ClearList; override;
    procedure   RebuildList; override;
  public
    constructor CreateCustom(AModel: TtiObjectList; AGrid: TfpgStringGrid; ADisplayNames: string; AIsObserving: Boolean = True); reintroduce; overload;
    destructor  Destroy; override;
    class function ComponentClass: TClass; override;
    function    GetObjectFromRow(ARow: Integer): TtiObject;
    function    View: TfpgStringGrid; reintroduce;
  end;


  { Used internally for sub-mediators in ListView mediator. Moved to interface
    section so it can be overridden. }
  TtiListViewListItemMediator = class(TtiListItemMediator)
  private
    FView: TfpgLVItem;
    procedure SetupFields; virtual;
  public
    constructor CreateCustom(AModel: TtiObject; AView: TfpgLVItem; const AFieldsInfo: TtiMediatorFieldInfoList; IsObserving: Boolean = True); reintroduce; overload;
    constructor CreateCustom(AModel: TtiObject; AView: TfpgLVItem; AOnBeforeSetupField: TtiOnBeforeSetupField; const AFieldsInfo: TtiMediatorFieldInfoList; IsObserving: Boolean = True); reintroduce; overload;
    procedure BeforeDestruction; override;
    procedure Update(ASubject: TtiObject); override;
  published
    property View: TfpgLVItem read FView;
  end;


  { Used internally for sub-mediators in StringGrid mediator. Moved to interface
    section so it can be overridden. }
  TtiStringGridRowMediator = class(TtiListItemMediator)
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


procedure RegisterFallBackListMediators;


implementation

uses
  tiRTTI;


procedure RegisterFallBackListMediators;
begin
  gMediatorManager.RegisterMediator(TtiListViewMediatorView, TtiObjectList);
  gMediatorManager.RegisterMediator(TtiStringGridMediatorView, TtiObjectList);
end;

{ TtiListViewMediatorView }

procedure TtiListViewMediatorView.SetSelectedObject(const AValue: TtiObject);
var
  i: integer;
begin
  for i := 0 to View.Items.Count - 1 do
    if TtiObject(View.Items.Item[i].UserData) = AValue then
    begin
      View.ItemIndex := i;
      HandleSelectionChanged;   {$Note Is this line required?}
      Exit; //==>
    end;
end;

function TtiListViewMediatorView.GetSelectedObject: TtiObject;
begin
  Result := GetObjectFromItem(View.Items.Item[View.ItemIndex]);
end;

function TtiListViewMediatorView.DoCreateItemMediator(AData: TtiObject; ARowIdx: integer): TtiListItemMediator;
var
  li: TfpgLVItem;
begin
  DataAndPropertyValid(AData);
  { Create ListItem and Mediator }
  View.BeginUpdate;
  try
    li := TfpgLVItem.Create(View.Items);
    View.Items.Add(li);
    Result := TtiListViewListItemMediator.CreateCustom(AData, li, OnBeforeSetupField, FieldsInfo, Active);
    li.UserData := Result;
    MediatorList.Add(Result);
  finally
    View.EndUpdate;
  end;
end;

procedure TtiListViewMediatorView.DoDeleteItemMediator(AIndex: Integer; AMediator: TtiListItemMediator);
begin
  View.Items.Delete(View.Items.IndexOf(TtiListViewListItemMediator(AMediator).View));
  inherited DoDeleteItemMediator(AIndex, AMediator);
end;

procedure TtiListViewMediatorView.CreateColumns;
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

procedure TtiListViewMediatorView.SetupGUIandObject;
begin
  { Setup TfpgListView defaults }
  View.Columns.Clear;
  View.Items.Clear;
  //  FView.ViewStyle         := vsReport;
  View.ShowHeaders := True;
  //  FView.RowSelect         := True;
  //  FView.AutoSize          := False;
  //  FView.ScrollBars        := ssAutoBoth;
end;

procedure TtiListViewMediatorView.ClearList;
begin
  MediatorList.Clear;
  if View <> nil then
    View.Items.Clear;
end;

procedure TtiListViewMediatorView.RebuildList;
begin
  View.BeginUpdate;
  try
    CreateColumns;
    CreateSubMediators;
  finally
    View.EndUpdate;
  end;
end;

procedure TtiListViewMediatorView.SetActive(const AValue: Boolean);
begin
  if not AValue then
    ClearList;
  inherited SetActive(AValue);
end;

constructor TtiListViewMediatorView.CreateCustom(AModel: TtiObjectList; AView: TfpgListView; AOnBeforeSetupField: TtiOnBeforeSetupField; ADisplayNames: string; AIsObserving: Boolean);
begin
  Create; // don't forget this
  OnBeforeSetupField := AOnBeforeSetupField;
  DisplayNames := ADisplayNames;  // Will call ParseDisplaynames.
  Subject := AModel;
  SetView(AView);                 // Will call SetupGUIandObject;
  CreateSubMediators;
  Active := AIsObserving;         // Will attach/Detach
end;

class function TtiListViewMediatorView.ComponentClass: TClass;
begin
  Result := TfpgListView;
end;

constructor TtiListViewMediatorView.Create;
begin
  inherited Create;
  FObserversInTransit := TList.Create;
end;

constructor TtiListViewMediatorView.CreateCustom(AModel: TtiObjectList; AView: TfpgListView; ADisplayNames: string; AIsObserving: Boolean);
begin
  CreateCustom(AModel, AView, nil, ADisplayNames, AIsObserving);
end;

destructor TtiListViewMediatorView.Destroy;
begin
  IsObserving := False;
  FObserversInTransit.Free;
  inherited Destroy;
end;

function TtiListViewMediatorView.View: TfpgListView;
begin
  Result := TfpgListView(inherited View);
end;

procedure TtiListViewMediatorView.HandleSelectionChanged;
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
      TtiMediatorView(SelectedObject.ObserverList.Items[i]).Subject :=
        SelectedObject;

    // execute the NotifyObservers event to update the observers.
    SelectedObject.NotifyObservers;
  end;
end;

function TtiListViewMediatorView.GetObjectFromItem(AItem: TfpgLVItem): TtiObject;
begin
  if (AItem = nil) or (AItem.UserData = nil) then
    Result := nil
  else
    Result := TtiListItemMediator(AItem.UserData).Model;
end;


{ TtiListViewListItemMediator }

procedure TtiListViewListItemMediator.SetupFields;
var
  c: integer;
  lMemberName: string;
  lValue: string;
begin
  lMemberName := FFieldsInfo[0].PropName;
  lValue      := tiGetProperty(Model, lMemberName);
  if Assigned(OnBeforeSetupField) then
    OnBeforeSetupField(Model, lMemberName, lValue);
  FView.Caption := lValue;
  for c := 1 to FFieldsInfo.Count - 1 do
  begin
    lMemberName := FFieldsInfo[c].PropName;
    lValue      := tiGetProperty(Model, lMemberName);
    if Assigned(OnBeforeSetupField) then
      OnBeforeSetupField(Model, lMemberName, lValue);
    FView.SubItems.Add(lValue);
  end;
end;

constructor TtiListViewListItemMediator.CreateCustom(AModel: TtiObject; AView: TfpgLVItem; const AFieldsInfo: TtiMediatorFieldInfoList; IsObserving: Boolean);
begin
  CreateCustom(AModel, AView, nil, AFieldsInfo, IsObserving);
end;

constructor TtiListViewListItemMediator.CreateCustom(AModel: TtiObject; AView: TfpgLVItem; AOnBeforeSetupField: TtiOnBeforeSetupField; const AFieldsInfo: TtiMediatorFieldInfoList; IsObserving: Boolean);
begin
  inherited Create;
  Model      := AModel;
  FView       := AView;
  FFieldsInfo := AFieldsInfo;
  OnBeforeSetupField := AOnBeforeSetupField;
  SetupFields;
  Active      := IsObserving; // Will attach
end;

procedure TtiListViewListItemMediator.BeforeDestruction;
begin
  Model.DetachObserver(self);
  Model := nil;
  FView  := nil;
  inherited BeforeDestruction;
end;

procedure TtiListViewListItemMediator.Update(ASubject: TtiObject);
var
  c: integer;
  lMemberName: string;
  lValue: string;
begin
  Assert(Model = ASubject);

  lMemberName := FFieldsInfo[0].PropName;
  lValue      := tiGetProperty(Model, lMemberName);
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


{ TtiStringGridMediatorView }

function TtiStringGridMediatorView.GetSelectedObject: TtiObject;
begin
  Result := GetObjectFromRow(View.FocusRow);
end;

procedure TtiStringGridMediatorView.SetSelectedObject(const AValue: TtiObject);
var
  i: integer;
  o: TObject;
begin
  for i := 0 to View.RowCount - 1 do
  begin
    o := View.Objects[0, i];
    if Assigned(o) and (TtiListItemMediator(o).Model = AValue) then
    begin
      View.FocusRow := i;
      Exit; //==>
    end;
  end;  { for }
end;

function TtiStringGridMediatorView.DoCreateItemMediator(AData: TtiObject; ARowIdx: integer): TtiListItemMediator;
var
  i: integer;
  lFieldName: string;
begin
  View.BeginUpdate;
  try
    if ARowIdx = View.RowCount then // In case of add notification
      View.RowCount := View.RowCount+1;
    for i := 0 to FieldsInfo.Count - 1 do
    begin
      lFieldName := FieldsInfo[i].PropName;
      View.Cells[i, ARowIdx] := tiGetProperty(AData, lFieldName);  // set Cell text
    end;
    Result := TtiStringGridRowMediator.CreateCustom(AData, View, FieldsInfo, ARowIdx, Active);
    View.Objects[0, ARowIdx] := Result;   // set Object reference inside grid. It used to be AData.
    MediatorList.Add(Result);
  finally
    View.EndUpdate;
  end;
end;

procedure TtiStringGridMediatorView.DoDeleteItemMediator(AIndex: Integer; AMediator: TtiListItemMediator);
begin
  View.DeleteRow(AIndex);
  inherited DoDeleteItemMediator(AIndex, AMediator);
end;

procedure TtiStringGridMediatorView.CreateColumns;
var
  i: integer;
  lColumnTotalWidth: integer;
  lGridNonContentWidth: integer;
  lLastColumnWidth: integer;
begin
  lColumnTotalWidth := 0;
  // Grid is 2px border left + right, 1px col gridline separator, 15px vertical scrollbar width
  lGridNonContentWidth := 2 + 2 + (FieldsInfo.Count - 1) + 15;
  for i := 0 to FieldsInfo.Count - 1 do
  begin
    View.ColumnWidth[i]       := FieldsInfo[i].FieldWidth;
    View.ColumnTitle[i]       := FieldsInfo[i].Caption;
    View.Columns[i].Alignment := FieldsInfo[i].Alignment;
    //resize the last column to fill the grid.
    if i = FieldsInfo.Count - 1 then
    begin
      if View.Width > (lColumnTotalWidth + lGridNonContentWidth) then
      begin
        lLastColumnWidth := View.Width - (lColumnTotalWidth + lGridNonContentWidth);
        if lLastColumnWidth > 10 then
          View.ColumnWidth[i] := lLastColumnWidth;
      end;
    end
    else
      lColumnTotalWidth := lColumnTotalWidth + View.ColumnWidth[i];
  end;
end;

procedure TtiStringGridMediatorView.SetupGUIandObject;
begin
  //Setup default properties for the StringGrid
  View.RowSelect   := True;
  View.ColumnCount := FieldsInfo.Count;
  if ShowDeleted then
    View.RowCount := Model.Count
  else
    View.RowCount := Model.CountNotDeleted;
end;

procedure TtiStringGridMediatorView.ClearList;
begin
  MediatorList.Clear;
  if View <> nil then
    View.RowCount := 1; {$Note Double check if this is desired. Shouldn't it be 0 instead. }
end;

procedure TtiStringGridMediatorView.RebuildList;
begin
//  writeln('--- TStringGridMediator.RebuildList');
  { This rebuilds the whole list. Not very efficient. }
  View.BeginUpdate;
  try
    SetupGUIandObject;
    MediatorList.Clear;
    CreateSubMediators;
  finally
    View.EndUpdate;
  end;
end;

constructor TtiStringGridMediatorView.CreateCustom(AModel: TtiObjectList; AGrid: TfpgStringGrid; ADisplayNames: string; AIsObserving: Boolean);
begin
  inherited Create;
  DisplayNames := ADisplayNames;
  Subject := AModel;
  SetView(AGrid);
  CreateSubMediators;
  IsObserving := AIsObserving;
end;

destructor TtiStringGridMediatorView.Destroy;
begin
  IsObserving := False;
  inherited Destroy;
end;

class function TtiStringGridMediatorView.ComponentClass: TClass;
begin
  Result := TfpgStringGrid;
end;

function TtiStringGridMediatorView.GetObjectFromRow(ARow: Integer): TtiObject;
var
  O: TObject;
begin
  if View.RowCount = 0 then
  begin
    Result := nil;
    Exit;
  end;

  if ARow = -1 then
    Result := nil
  else
  begin
    O := View.Objects[0, ARow];
    if O <> nil then
      Result := TtiListItemMediator(O).Model
    else
      Result := nil;
  end;
end;

function TtiStringGridMediatorView.View: TfpgStringGrid;
begin
  Result := TfpgStringGrid(inherited View);
end;


{ TtiStringGridRowMediator }

constructor TtiStringGridRowMediator.CreateCustom(AModel: TtiObject; AGrid: TfpgStringGrid; const AFieldsInfo: TtiMediatorFieldInfoList; ARowIndex: integer; IsObserving: Boolean);
begin
  inherited Create;
  Model      := AModel;
  FView       := AGrid;
  FFieldsInfo := AFieldsInfo;
  FRowIndex   := ARowIndex;
  Active      := IsObserving; // Will attach
end;

procedure TtiStringGridRowMediator.Update(ASubject: TtiObject);
var
  i: integer;
  lFieldName: string;
  lValue: string;
begin
  Assert(Model = ASubject);
  for i := 0 to FFieldsInfo.Count - 1 do
  begin
    lFieldName := FFieldsInfo[I].PropName;
    lValue     := tiGetProperty(Model, lFieldName);
    if Assigned(OnBeforeSetupField) then
      OnBeforeSetupField(Model, lFieldName, lValue);
    FView.Cells[i, FRowIndex] := lValue;
  end;
end;


end.

