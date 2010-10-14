{
  Purpose:
    Abstract mediating view and Mediator Factory. This allows you to use
    standard edit components and make them object-aware.  See the demo
    application for usage.
}

unit tiMediators;

{$mode objfpc}{$H+}

interface
uses
  tiObject
  ,Classes
  ,tiBaseMediator
  ,fpg_base         // for predefined colors
  ,fpg_main
  ,fpg_widget
  ,fpg_edit
  ,fpg_checkbox
  ,fpg_label
  ,fpg_trackbar
  ,fpg_combobox
  ,fpg_memo
  ,fpg_popupcalendar
  ,fpg_spinedit
  ,fpg_listbox
  ;

type
  { Base class to handle TfpgWidget controls }
  TtiControlMediatorView = class(TtiMediatorView)
  private
    FViewColor: TfpgColor;
    FViewHint: TfpgString;
    FViewErrorColor: TfpgColor;
    FEditControl: TfpgEdit;
    procedure   SetViewErrorColor(const AValue: TfpgColor);
  protected
    function    GetCurrentControlColor: TfpgColor; virtual;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
  public
    constructor Create; override;
    property    ViewErrorColor: TfpgColor read FViewErrorColor write SetViewErrorColor;
    procedure   SetView(const AValue: TComponent); override;
    function    View: TfpgWidget; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgBaseEdit controls (TfpgEdit, TfpgNumericEdit, ...) }
  TtiBaseEditMediatorView = class(TtiControlMediatorView)
  private
    FControlReadOnlyColor: TfpgColor;
    procedure   SetControlReadOnlyColor(const AValue: TfpgColor);
  protected
    function    GetCurrentControlColor: TfpgColor; override;
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    ControlReadOnlyColor: TfpgColor read FControlReadOnlyColor write SetControlReadOnlyColor;
    function    View: TfpgBaseEdit; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgEdit controls }
  TtiEditMediatorView = class(TtiBaseEditMediatorView)
  public
    function    View: TfpgEdit; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  TtiEditIntegerMediatorView = class(TtiBaseEditMediatorView)
  public
    constructor Create; override;
    function    View: TfpgEditInteger; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  TtiEditFloatMediatorView = class(TtiBaseEditMediatorView)
  public
    constructor Create; override;
    function    View: TfpgEditFloat; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  TtiEditCurrencyMediatorView = class(TtiBaseEditMediatorView)
  public
    constructor Create; override;
    function    View: TfpgEditCurrency; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgCheckBox controls }
  TtiCheckBoxMediatorView = class(TtiControlMediatorView)
  protected
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    function    View: TfpgCheckBox; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgLabel controls }
  TtiStaticTextMediatorView = class(TtiControlMediatorView)
  protected
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;
    function    View: TfpgLabel; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgSpinEdit controls }
  TtiSpinEditMediatorView = class(TtiControlMediatorView)
  protected
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    function    View: TfpgSpinEdit; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgSpinEditFloat controls }
  TtiSpinEditFloatMediatorView = class(TtiControlMediatorView)
  protected
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    function    View: TfpgSpinEditFloat; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgTrackBar controls }
  TtiTrackBarMediatorView = class(TtiControlMediatorView)
  private
    procedure   DoTrackBarChanged(Sender: TObject; APosition: integer);
  protected
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    function    View: TfpgTrackBar; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgComboBox controls }
  TtiComboBoxMediatorView = class(TtiControlMediatorView)
  protected
    procedure   DoObjectToGui; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    function    View: TfpgComboBox; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Sets ItemIndex based on integer property }
  TtiItemComboBoxMediatorView = class(TtiComboBoxMediatorView)
  protected
    procedure   DoGUIToObject; override;
    procedure   DoObjectToGUI; override;
  public
    constructor Create; override;
  end;


  { TfpgComboBox observing a list and setting a Object property }
  TtiDynamicComboBoxMediatorView = class(TtiComboBoxMediatorView)
  private
    FDisplayFieldName: string;
    FExternalOnChange: TNotifyEvent;
    function    GetDisplayFieldName: string;
    procedure   InternalListRefresh;
  protected
    procedure   SetListObject(const AValue: TtiObjectList); override;
    procedure   SetOnChangeActive(AValue: Boolean); virtual;
    procedure   SetupGUIandObject; override;
    procedure   DoGuiToObject; override;
    procedure   DoObjectToGui; override;
  public
    procedure   RefreshList; virtual;
    property    DisplayFieldName: string read GetDisplayFieldName write FDisplayFieldName;
  end;
  

  { Base class to handle TfpgMemo controls }
  TtiMemoMediatorView = class(TtiControlMediatorView)
  protected
    procedure   SetupGUIandObject; override;
    procedure   DoObjectToGui; override;
    procedure   DoGuiToObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    function    View: TfpgMemo; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgCalendarCombo controls }
  TtiCalendarComboMediatorView = class(TtiControlMediatorView)
  protected
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    function    View: TfpgCalendarCombo; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgListBox controls }
  TtiListBoxMediatorView = class(TtiControlMediatorView)
  protected
    procedure   DoObjectToGui; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    function    View: TfpgListBox; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Sets ItemIndex based on integer property }
  TtiItemListBoxMediatorView = class(TtiListBoxMediatorView)
  protected
    procedure   DoGUIToObject; override;
    procedure   DoObjectToGUI; override;
  public
    constructor Create; override;
  end;


  { TfpgListBox observing a list and setting a Object property }
  TtiDynamicListBoxMediatorView = class(TtiListBoxMediatorView)
  private
    FDisplayFieldName: string;
    FExternalOnChange: TNotifyEvent;
    function    GetDisplayFieldName: string;
    procedure   InternalListRefresh;
  protected
    procedure   SetListObject(const AValue: TtiObjectList); override;
    procedure   SetOnChangeActive(AValue: Boolean); virtual;
    procedure   SetupGUIandObject; override;
    procedure   DoGuiToObject; override;
    procedure   DoObjectToGui; override;
  public
    procedure   RefreshList; virtual;
    property    DisplayFieldName: string read GetDisplayFieldName write FDisplayFieldName;
  end;


// Registering generic mediators which can handle most cases by default.
procedure RegisterFallBackMediators;


implementation
uses
  SysUtils
  ,TypInfo
  ,fpg_dialogs      // for TfpgMessageDialog
  ,tiGUIConstants   // for error color
  ;

type
  // Friend class to get access to protected methods
  THackWidget = class(TfpgWidget);
  THackBaseEdit = class(TfpgBaseEdit);


const
  cErrorListHasNotBeenAssigned   = 'List has not been assigned';
  cErrorPropertyNotClass         = 'Property is not a class type!';
  cErrorAddingItemToCombobox     = 'Error adding list items to combobox ' +
                                   'Message: %s, Item Property Name: %s';


procedure RegisterFallBackMediators;
begin
  gMediatorManager.RegisterMediator(TtiEditMediatorView, TtiObject, [tkSString,tkAString,tkLString,tkWString,tkUString,tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TtiEditIntegerMediatorView, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TtiEditFloatMediatorView, TtiObject, [tkFloat]);
  gMediatorManager.RegisterMediator(TtiEditCurrencyMediatorView, TtiObject, [tkFloat]);
  gMediatorManager.RegisterMediator(TtiCheckBoxMediatorView, TtiObject, [tkBool]);
  gMediatorManager.RegisterMediator(TtiComboBoxMediatorView, TtiObject, [tkSString,tkAString]);
  gMediatorManager.RegisterMediator(TtiItemComboBoxMediatorView, TtiObject, [tkInteger, tkEnumeration]);
  gMediatorManager.RegisterMediator(TtiDynamicComboBoxMediatorView, TtiObject, [tkClass]);
  gMediatorManager.RegisterMediator(TtiStaticTextMediatorView, TtiObject);
  gMediatorManager.RegisterMediator(TtiTrackBarMediatorView, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TtiMemoMediatorView, TtiObject, [tkSString,tkAString,tkLString,tkWString,tkUString]);
  gMediatorManager.RegisterMediator(TtiCalendarComboMediatorView, TtiObject, [tkFloat]);
  gMediatorManager.RegisterMediator(TtiSpinEditMediatorView, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TtiSpinEditFloatMediatorView, TtiObject, [tkFloat]);
  gMediatorManager.RegisterMediator(TtiListBoxMediatorView, TtiObject, [tkSString,tkAString]);
  gMediatorManager.RegisterMediator(TtiItemListBoxMediatorView, TtiObject, [tkInteger, tkEnumeration]);
  gMediatorManager.RegisterMediator(TtiDynamicListBoxMediatorView, TtiObject, [tkClass]);
end;

{ TtiControlMediatorView }

procedure TtiControlMediatorView.SetViewErrorColor(const AValue: TfpgColor);
begin
  if AValue <> FViewErrorColor then
  begin
    FViewErrorColor := AValue;
    TestIfValid; // Update view
  end;
end;

function TtiControlMediatorView.GetCurrentControlColor: TfpgColor;
begin
  Result := fpgColorToRGB(FViewColor);
end;

procedure TtiControlMediatorView.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGuiValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(RootFieldName);
  if oError <> nil then
  begin
    View.BackgroundColor  := ViewErrorColor;
    View.Hint   := oError.ErrorMessage;
  end
  else
  begin
    View.BackgroundColor  := GetCurrentControlColor;
    View.Hint   := FViewHint;
  end;
end;

constructor TtiControlMediatorView.Create;
begin
  inherited Create;
  FViewErrorColor := clError;
end;

procedure TtiControlMediatorView.SetView(const AValue: TComponent);
var
  LValue: TfpgWidget;
begin
  Assert((AValue = nil) or (AValue is TfpgWidget), 'Expected TfpgWidget');
  LValue := AValue as TfpgWidget;

  if LValue <> View then
  begin
    // Restore state of previous view
    if View <> nil then
    begin
      View.Hint := FViewHint;
      View.BackgroundColor := FViewColor;
    end;

    // Preserve state of new view
    if Assigned(LValue) then
    begin
      FViewHint := LValue.Hint;
      FViewColor := LValue.BackgroundColor;
    end;
  end;

  inherited SetView(AValue);
end;

function TtiControlMediatorView.View: TfpgWidget;
begin
  Result := TfpgWidget(inherited View);
end;

class function TtiControlMediatorView.ComponentClass: TClass;
begin
  Result := TfpgWidget;
end;

{ TtiBaseEditMediatorView }

procedure TtiBaseEditMediatorView.SetControlReadOnlyColor(const AValue: TfpgColor);
begin
  if AValue <> FControlReadOnlyColor then
  begin
    FControlReadOnlyColor := AValue;
    TestIfValid; // Update view
  end;
end;

function TtiBaseEditMediatorView.GetCurrentControlColor: TfpgColor;
begin
  if THackBaseEdit(View).ReadOnly then
    Result := fpgColorToRGB(ControlReadOnlyColor)
  else
    Result := inherited GetCurrentControlColor;
end;

procedure TtiBaseEditMediatorView.SetupGUIandObject;
var
  Mi, Ma: Integer;
begin
  inherited SetupGUIandObject;
  if Subject.GetFieldBounds(FieldName,Mi,Ma) and (Ma>0) then
    THackBaseEdit(View).MaxLength := Ma;
end;

procedure TtiBaseEditMediatorView.SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if View <> nil then
  begin
    if ObjectUpdateMoment in [ouOnchange,ouCustom,ouDefault] then
      THackBaseEdit(View).OnChange := @DoOnChange
    else
      THackBaseEdit(View).OnExit := @DoOnChange;
    if ObjectUpdateMoment in [ouNone] then
    begin
      THackbaseEdit(View).OnChange := nil;
      THackbaseEdit(View).OnExit := nil;
    end;
  end;
end;

constructor TtiBaseEditMediatorView.Create;
begin
  inherited Create;
  FControlReadOnlyColor := clWindowBackground;
  GUIFieldName := 'Text';
end;

destructor TtiBaseEditMediatorView.Destroy;
begin
  if View <> nil then
  begin
    if Assigned(THackBaseEdit(View).OnChange) then
      THackBaseEdit(View).OnChange := nil;
  end;
  inherited Destroy;
end;

function TtiBaseEditMediatorView.View: TfpgBaseEdit;
begin
  Result := TfpgBaseEdit(inherited View);
end;

class function TtiBaseEditMediatorView.ComponentClass: TClass;
begin
  Result := TfpgBaseEdit;
end;

{ TtiEditMediatorView }

function TtiEditMediatorView.View: TfpgEdit;
begin
  Result := TfpgEdit(inherited View);
end;

class function TtiEditMediatorView.ComponentClass: TClass;
begin
  Result := TfpgEdit;
end;

{ TtiSpinEditMediatorView}

class function TtiSpinEditMediatorView.ComponentClass: TClass;
begin
  Result := TfpgSpinEdit;
end;

procedure TtiSpinEditMediatorView.SetupGUIandObject;
var
  Mi, Ma: Integer;
begin
  inherited SetupGUIandObject;
  if Subject.GetFieldBounds(FieldName, Mi, Ma) then
  begin
    View.MinValue := Mi;
    View.MaxValue := Ma;
  end;
end;

procedure TtiSpinEditMediatorView.SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if View <> nil then
  begin
    if ObjectUpdateMoment in [ouOnChange,ouCustom,ouDefault] then
      View.OnChange := @DoOnChange
    else
      View.OnExit := @DoOnChange;
    if ObjectUpdateMoment in [ouNone] then
    begin
      View.OnChange := nil;
      View.OnExit := nil;
    end;
  end;
end;

constructor TtiSpinEditMediatorView.Create;
begin
  inherited Create;
  GuiFieldName := 'Value';
end;

function TtiSpinEditMediatorView.View: TfpgSpinEdit;
begin
  Result := TfpgSpinEdit(inherited View);
end;


{ TtiTrackBarMediatorView}

procedure TtiTrackBarMediatorView.DoTrackBarChanged(Sender: TObject; APosition: integer);
begin
  GUIChanged;
end;

procedure TtiTrackBarMediatorView.SetupGUIandObject;
var
  Mi, Ma: Integer;
begin
  inherited SetupGUIandObject;
  if Subject.GetFieldBounds(FieldName, Mi, Ma) then
  begin
    View.Min := Mi;
    View.Max := Ma;
  end;
end;

procedure TtiTrackBarMediatorView.SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if View <> nil then
  begin
    if ObjectUpdateMoment in [ouOnChange,ouCustom,ouDefault] then
      View.OnChange := @DoTrackBarChanged   // TfpgTrackBar has a different event signature
    else
      View.OnExit := @DoOnChange;
    if ObjectUpdateMoment in [ouNone] then
    begin
      View.OnChange := nil;
      View.OnExit := nil;
    end;
  end;
end;

constructor TtiTrackBarMediatorView.Create;
begin
  inherited;
  GuiFieldName := 'Position';
end;

function TtiTrackBarMediatorView.View: TfpgTrackBar;
begin
  Result := TfpgTrackBar(inherited View);
end;

class function TtiTrackBarMediatorView.ComponentClass: TClass;
begin
  Result := TfpgTrackBar;
end;


{ TtiComboBoxMediatorView }

class function TtiComboBoxMediatorView.ComponentClass: TClass;
begin
  Result := TfpgComboBox;
end;

constructor TtiComboBoxMediatorView.Create;
begin
  inherited Create;
  GuiFieldName := 'Text';
end;

function TtiComboBoxMediatorView.View: TfpgComboBox;
begin
  Result := TfpgComboBox(inherited View);
end;

procedure TtiComboBoxMediatorView.DoObjectToGui;
begin
  View.FocusItem := View.Items.IndexOf(Subject.PropValue[FieldName]);
end;

procedure TtiComboBoxMediatorView.SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if View <> nil then
  begin
    if ObjectUpdateMoment in [ouOnChange,ouCustom,ouDefault] then
      View.OnChange := @DoOnChange
    else
      View.OnExit := @DoOnChange;
    if ObjectUpdateMoment in [ouNone] then
    begin
      View.OnChange := nil;
      View.OnExit := nil;
    end;  
  end;
end;


{ TtiMemoMediatorView }

class function TtiMemoMediatorView.ComponentClass: TClass;
begin
  Result := TfpgMemo;
end;

procedure TtiMemoMediatorView.DoGuiToObject;
begin
  Subject.PropValue[FieldName] := Trim(View.Lines.Text);
end;

procedure TtiMemoMediatorView.SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if View <> nil then
  begin
    if ObjectUpdateMoment in [ouOnChange,ouCustom,ouDefault] then
      View.OnChange := @DoOnChange
    else
      View.OnExit := @DoOnChange;
    if ObjectUpdateMoment in [ouNone] then
    begin
      View.OnChange := nil;
      View.OnExit := nil;
    end;
  end;
end;

constructor TtiMemoMediatorView.Create;
begin
  inherited Create;
//  FControlReadOnlyColor := clWindowBackground;
  GUIFieldName := 'Lines';
end;

function TtiMemoMediatorView.View: TfpgMemo;
begin
  Result := TfpgMemo(inherited View);
end;

procedure TtiMemoMediatorView.DoObjectToGui;
begin
  View.Lines.Text := Subject.PropValue[FieldName];
end;

procedure TtiMemoMediatorView.SetupGUIandObject;
var
  Mi, Ma: integer;
begin
  inherited SetupGUIandObject;
  if Subject.GetFieldBounds(FieldName,Mi,Ma) and (Ma>0) then
    View.MaxLength := Ma;
  View.Lines.Text := '';
end;


{ TtiDynamicComboBoxMediatorView }

procedure TtiDynamicComboBoxMediatorView.SetListObject(const AValue: TtiObjectList);
begin
  inherited;
  InternalListRefresh;
  if Assigned(ValueList) then
    View.Enabled := ValueList.Count > 0;
end;

procedure TtiDynamicComboBoxMediatorView.InternalListRefresh;
var
  lItems: TStrings;
  i: Integer;
begin
  lItems := View.Items;
  lItems.Clear;
  View.Text := '';

  if (ValueList = nil) or
     (ValueList.Count < 1) or
     (SameText(FieldName, EmptyStr)) then
    Exit; //==>

  try
    for i := 0 to ValueList.Count - 1 do
      lItems.AddObject(GetStrProp(ValueList.Items[i], DisplayFieldName), ValueList.Items[i]);
  except
    on E: Exception do
      RaiseMediatorError(cErrorAddingItemToCombobox,[E.message, FieldName]);
  end;
  ObjectToGui;
end;

function TtiDynamicComboBoxMediatorView.GetDisplayFieldName: string;
begin
  Result := FDisplayFieldName;
  if (Result = '') then
    Result := 'Caption'; // Do not localize.
end;

procedure TtiDynamicComboBoxMediatorView.SetOnChangeActive(AValue: Boolean);
begin
  if AValue then
  begin
    if not UseInternalOnChange then
      View.OnChange := FExternalOnChange
    else
      View.OnChange := @DoOnChange;
  end
  else
  begin
    if not UseInternalOnChange then
      FExternalOnChange := View.OnChange;
    View.OnChange := nil;
  end;
end;

procedure TtiDynamicComboBoxMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;

  if UseInternalOnChange then
    View.OnChange := @DoOnChange; // default OnChange event handler

  {$Note As far as I can see, ValueList is always going to be nil here! - Graeme }
  if ValueList <> nil then
    View.Enabled := (ValueList.Count > 0);
end;

procedure TtiDynamicComboBoxMediatorView.DoGuiToObject;
var
  lValue: TtiObject;
  lPropType: TTypeKind;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  if View.FocusItem < 0 then
    Exit; //==>

  lValue := TtiObject(ValueList.Items[View.FocusItem]);

  lPropType := typinfo.PropType(Subject, FieldName);
  if lPropType = tkClass then
    typinfo.SetObjectProp(Subject, FieldName, lValue)
  else
    RaiseMediatorError(cErrorPropertyNotClass);
end;

procedure TtiDynamicComboBoxMediatorView.DoObjectToGui;
var
  i: Integer;
  lValue: TtiObject;
  lPropType: TTypeKind;
begin
  SetOnChangeActive(false);

  //  Set the index only (We're assuming the item is present in the list)
  View.FocusItem := -1;
  if Subject = nil then
    Exit; //==>

  if not Assigned(ValueList) then
    RaiseMediatorError(cErrorListHasNotBeenAssigned);

  lValue := nil;
  lPropType := typinfo.PropType(Subject, FieldName);
  if lPropType = tkClass then
    lValue := TtiObject(typinfo.GetObjectProp(Subject, FieldName))
  else
    RaiseMediatorError(cErrorPropertyNotClass);

  for i := 0 to ValueList.Count - 1 do
    if ValueList.Items[i] = lValue then
    begin
      View.FocusItem := i;
      Break; //==>
    end;

  SetOnChangeActive(true);
end;

procedure TtiDynamicComboBoxMediatorView.RefreshList;
begin
  InternalListRefresh;
end;


{ TtiCheckBoxMediatorView }

procedure TtiCheckBoxMediatorView.SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if View <> nil then
  begin
    if ObjectUpdateMoment in [ouOnChange,ouCustom,ouDefault] then
      View.OnChange := @DoOnChange
    else
      View.OnExit := @DoOnChange;
    if ObjectUpdateMoment in [ouNone] then
    begin
      View.OnChange := nil;
      View.OnExit := nil;
    end;
  end;
end;

constructor TtiCheckBoxMediatorView.Create;
begin
  inherited Create;
  GuiFieldName := 'Checked';
end;

function TtiCheckBoxMediatorView.View: TfpgCheckBox;
begin
  Result := TfpgCheckBox(inherited View);
end;

class function TtiCheckBoxMediatorView.ComponentClass: TClass;
begin
  Result := TfpgCheckBox;
end;


{ TtiStaticTextMediatorView }

procedure TtiStaticTextMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  View.Text := '';
end;

constructor TtiStaticTextMediatorView.Create;
begin
  inherited Create;
  GuiFieldName := 'Text';
end;

function TtiStaticTextMediatorView.View: TfpgLabel;
begin
  Result := TfpgLabel(inherited View);
end;

class function TtiStaticTextMediatorView.ComponentClass: TClass;
begin
  Result := TfpgLabel;
end;


{ TtiCalendarComboMediatorView }

procedure TtiCalendarComboMediatorView.SetupGUIandObject;
var
  Mi, Ma: TDateTime;
begin
//  inherited SetupGUIandObject;
  if Subject.GetFieldBounds(FieldName, Mi, Ma) then
  begin
    View.MinDate := Mi;
    View.MaxDate := Ma;
  end;
end;

procedure TtiCalendarComboMediatorView.SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if View <> nil then
  begin
    if ObjectUpdateMoment in [ouOnChange,ouCustom,ouDefault] then
      View.OnChange := @DoOnChange
    else
      View.OnExit := @DoOnChange;
    if ObjectUpdateMoment in [ouNone] then
    begin
      View.OnChange := nil;
      View.OnExit := nil;
    end;
  end;
end;

constructor TtiCalendarComboMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'DateValue';
end;

function TtiCalendarComboMediatorView.View: TfpgCalendarCombo;
begin
  Result := TfpgCalendarCombo(inherited View);
end;

class function TtiCalendarComboMediatorView.ComponentClass: TClass;
begin
  Result := TfpgCalendarCombo;
end;


{ TtiItemComboBoxMediatorView }

procedure TtiItemComboBoxMediatorView.DoGUIToObject;
begin
  SetOrdProp(Subject, FieldName, View.FocusItem);
end;

procedure TtiItemComboBoxMediatorView.DoObjectToGUI;
begin
  View.FocusItem := GetOrdProp(Subject, FieldName);
end;

constructor TtiItemComboBoxMediatorView.Create;
begin
  inherited Create;
  GuiFieldName := 'FocusItem';
end;

{ TtiSpinEditFloatMediatorView }

procedure TtiSpinEditFloatMediatorView.SetupGUIandObject;
var
  Mi, Ma: Integer;
begin
  inherited SetupGUIandObject;
  if Subject.GetFieldBounds(FieldName, Mi, Ma) then
  begin
    View.MinValue := Mi;
    View.MaxValue := Ma;
  end;
end;

procedure TtiSpinEditFloatMediatorView.SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if View <> nil then
  begin
    if ObjectUpdateMoment in [ouOnChange,ouCustom,ouDefault] then
      View.OnChange := @DoOnChange
    else
      View.OnExit := @DoOnChange;
    if ObjectUpdateMoment in [ouNone] then
    begin
      View.OnChange := nil;
      View.OnExit := nil;
    end;
  end;
end;

constructor TtiSpinEditFloatMediatorView.Create;
begin
  inherited Create;
  GuiFieldName := 'Value';
end;

function TtiSpinEditFloatMediatorView.View: TfpgSpinEditFloat;
begin
  Result := TfpgSpinEditFloat(inherited View);
end;

class function TtiSpinEditFloatMediatorView.ComponentClass: TClass;
begin
  Result := TfpgSpinEditFloat;
end;

{ TtiEditIntegerMediatorView }

constructor TtiEditIntegerMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Value';
end;

function TtiEditIntegerMediatorView.View: TfpgEditInteger;
begin
  Result := TfpgEditInteger(inherited View);
end;

class function TtiEditIntegerMediatorView.ComponentClass: TClass;
begin
  Result := TfpgEditInteger;
end;

{ TtiEditFloatMediatorView }

constructor TtiEditFloatMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Value';
end;

function TtiEditFloatMediatorView.View: TfpgEditFloat;
begin
  Result := TfpgEditFloat(inherited View);
end;

class function TtiEditFloatMediatorView.ComponentClass: TClass;
begin
  Result := TfpgEditFloat;
end;

{ TtiEditCurrencyMediatorView }

constructor TtiEditCurrencyMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Value';
end;

function TtiEditCurrencyMediatorView.View: TfpgEditCurrency;
begin
  Result := TfpgEditCurrency(inherited View);
end;

class function TtiEditCurrencyMediatorView.ComponentClass: TClass;
begin
  Result := TfpgEditCurrency;
end;


{ TtiDynamicListBoxMediatorView }

function TtiDynamicListBoxMediatorView.GetDisplayFieldName: string;
begin
  Result := FDisplayFieldName;
  if (Result = '') then
    Result := 'Caption'; // Do not localize.
end;

procedure TtiDynamicListBoxMediatorView.InternalListRefresh;
var
  lItems: TStrings;
  i: Integer;
begin
  lItems := View.Items;
  lItems.Clear;
  View.Text := '';

  if (ValueList = nil) or
     (ValueList.Count < 1) or
     (SameText(FieldName, EmptyStr)) then
    Exit; //==>

  try
    for i := 0 to ValueList.Count - 1 do
      lItems.AddObject(GetStrProp(ValueList.Items[i], DisplayFieldName), ValueList.Items[i]);
  except
    on E: Exception do
      RaiseMediatorError(cErrorAddingItemToCombobox,[E.message, FieldName]);
  end;
  ObjectToGui;
end;

procedure TtiDynamicListBoxMediatorView.SetListObject(const AValue: TtiObjectList);
begin
  inherited SetListObject(AValue);
  InternalListRefresh;
  if Assigned(ValueList) then
    View.Enabled := ValueList.Count > 0;
end;

procedure TtiDynamicListBoxMediatorView.SetOnChangeActive(AValue: Boolean);
begin
  if AValue then
  begin
    if not UseInternalOnChange then
      View.OnChange := FExternalOnChange
    else
      View.OnChange := @DoOnChange;
  end
  else
  begin
    if not UseInternalOnChange then
      FExternalOnChange := View.OnChange;
    View.OnChange := nil;
  end;
end;

procedure TtiDynamicListBoxMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;

  if UseInternalOnChange then
    View.OnChange := @DoOnChange; // default OnChange event handler

  {$Note As far as I can see, ValueList is always going to be nil here! - Graeme }
  if ValueList <> nil then
    View.Enabled := (ValueList.Count > 0);
end;

procedure TtiDynamicListBoxMediatorView.DoGuiToObject;
var
  lValue: TtiObject;
  lPropType: TTypeKind;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  if View.FocusItem < 0 then
    Exit; //==>

  lValue := TtiObject(ValueList.Items[View.FocusItem]);

  lPropType := typinfo.PropType(Subject, FieldName);
  if lPropType = tkClass then
    typinfo.SetObjectProp(Subject, FieldName, lValue)
  else
    RaiseMediatorError(cErrorPropertyNotClass);
end;

procedure TtiDynamicListBoxMediatorView.DoObjectToGui;
var
  i: Integer;
  lValue: TtiObject;
  lPropType: TTypeKind;
begin
  SetOnChangeActive(false);

  //  Set the index only (We're assuming the item is present in the list)
  View.FocusItem := -1;
  if Subject = nil then
    Exit; //==>

  if not Assigned(ValueList) then
    RaiseMediatorError(cErrorListHasNotBeenAssigned);

  lValue := nil;
  lPropType := typinfo.PropType(Subject, FieldName);
  if lPropType = tkClass then
    lValue := TtiObject(typinfo.GetObjectProp(Subject, FieldName))
  else
    RaiseMediatorError(cErrorPropertyNotClass);

  for i := 0 to ValueList.Count - 1 do
    if ValueList.Items[i] = lValue then
    begin
      View.FocusItem := i;
      Break; //==>
    end;

  SetOnChangeActive(true);
end;

procedure TtiDynamicListBoxMediatorView.RefreshList;
begin
  InternalListRefresh;
end;

{ TtiItemListBoxMediatorView }

procedure TtiItemListBoxMediatorView.DoGUIToObject;
begin
  SetOrdProp(Subject, FieldName, View.FocusItem);
end;

procedure TtiItemListBoxMediatorView.DoObjectToGUI;
begin
  View.FocusItem := GetOrdProp(Subject, FieldName);
end;

constructor TtiItemListBoxMediatorView.Create;
begin
  inherited Create;
  GuiFieldName := 'FocusItem';
end;

{ TtiListBoxMediatorView }

procedure TtiListBoxMediatorView.DoObjectToGui;
begin
  View.FocusItem := View.Items.IndexOf(Subject.PropValue[FieldName]);
end;

procedure TtiListBoxMediatorView.SetObjectUpdateMoment(
  const AValue: TtiObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if View <> nil then
  begin
    if ObjectUpdateMoment in [ouOnChange,ouCustom,ouDefault] then
      View.OnChange := @DoOnChange
    else
      View.OnExit := @DoOnChange;
    if ObjectUpdateMoment in [ouNone] then
    begin
      View.OnChange := nil;
      View.OnExit := nil;
    end;
  end;
end;

constructor TtiListBoxMediatorView.Create;
begin
  inherited Create;
  GuiFieldName := 'Text';
end;

function TtiListBoxMediatorView.View: TfpgListBox;
begin
  Result := TfpgListBox(inherited View);
end;

class function TtiListBoxMediatorView.ComponentClass: TClass;
begin
  Result := TfpgListBox;
end;

end.

