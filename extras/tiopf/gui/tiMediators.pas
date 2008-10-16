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
  ;

type

  { Base class to handle TfpgEdit controls }
  TMediatorEditView = class(TMediatorView)
  private
    FEditControl: TfpgEdit;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent); override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment (Const AValue : TObjectUpdateMoment); override;
  public
    constructor Create; override;
    constructor CreateCustom(pEditControl: TfpgWidget; pSubject: TtiObject; pFieldName: string; pGuiFieldName: string = 'Text'); reintroduce;
    destructor  Destroy; override;
    property    EditControl: TfpgEdit read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgCheckBox controls }
  TMediatorCheckBoxView = class(TMediatorView)
  private
    FEditControl: TfpgCheckBox;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
  public
    constructor Create; override;
    property    EditControl: TfpgCheckBox read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgLabel controls }
  TMediatorStaticTextView = class(TMediatorView)
  private
    FEditControl: TfpgLabel;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;
    property    EditControl: TfpgLabel read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgSpinEdit controls }
  TMediatorSpinEditView = class(TMediatorView)
  private
    FEditControl: TfpgSpinEdit;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
  public
    constructor Create; override;
    property    EditControl: TfpgSpinEdit read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgSpinEditFloat controls }
  TMediatorSpinEditFloatView = class(TMediatorView)
  private
    FEditControl: TfpgSpinEditFloat;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
  public
    constructor Create; override;
    property    EditControl: TfpgSpinEditFloat read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgTrackBar controls }
  TMediatorTrackBarView = class(TMediatorView)
  private
    FEditControl: TfpgTrackBar;
    procedure   DoTrackBarChanged(Sender: TObject; APosition: integer);
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   SetupGUIandObject; override;
  public
    Constructor Create; override;
    property    EditControl: TfpgTrackBar read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgComboBox controls }
  TMediatorComboBoxView = class(TMediatorView)
  private
    FEditControl: TfpgComboBox;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent); override;
    procedure   SetupGUIandObject; override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
    procedure   DoObjectToGui; override;
  public
    constructor Create; override;
    property    EditControl: TfpgComboBox read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Sets ItemIndex based on integer property }
  TMediatorItemComboBoxView = class(TMediatorComboBoxView)
  protected
    Procedure   DoGUIToObject; override;
    Procedure   DoObjectToGUI; override;
  public
    constructor Create; override;
  end;


  { TComboBox observing a list and setting a Object property }
  TMediatorDynamicComboBoxView = class(TMediatorComboBoxView)
  private
    FExternalOnChange: TNotifyEvent;
    procedure   InternalListRefresh;
  protected
    procedure   SetListObject(const AValue: TtiObjectList); override;
    procedure   SetOnChangeActive(AValue: Boolean); virtual;
    procedure   SetupGUIandObject; override;
    procedure   DoGuiToObject; override;
    procedure   DoObjectToGui; override;
  public
    procedure   RefreshList; virtual;
  end;
  

  { Base class to handle TfpgMemo controls }
  TMediatorMemoView = class(TMediatorView)
  private
    FEditControl: TfpgMemo;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   SetupGUIandObject; override;
    procedure   DoObjectToGui; override;
    procedure   DoGuiToObject; override;
  public
    property    EditControl: TfpgMemo read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgCalendarCombo controls }
  TMediatorCalendarComboView = class(TMediatorView)
  private
    FEditControl: TfpgCalendarCombo;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
  public
    constructor Create; override;
    property    EditControl: TfpgCalendarCombo read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


// Registering generic mediators which can handle most cases by default.
procedure RegisterFallBackMediators;


implementation
uses
  SysUtils
  ,TypInfo
  ,tiExcept
  ,fpg_dialogs      // for TfpgMessageDialog
  ,fpg_base         // for predefined colors
  ,tiGUIConstants   // for error color
  ;

const
  cErrorListHasNotBeenAssigned   = 'List has not been assigned';


procedure RegisterFallBackMediators;
begin
  gMediatorManager.RegisterMediator(TMediatorEditView, TtiObject, [tkSString,tkAString,tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TMediatorCheckBoxView, TtiObject, [tkBool]);
  gMediatorManager.RegisterMediator(TMediatorComboboxView, TtiObject, [tkSString,tkAString]);
  gMediatorManager.RegisterMediator(TMediatorStaticTextView, TtiObject);
  gMediatorManager.RegisterMediator(TMediatorTrackBarView, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TMediatorDynamicComboBoxView, TtiObject, [tkClass]);
  gMediatorManager.RegisterMediator(TMediatorMemoView, TtiObject, [tkSString,tkAString]);
  gMediatorManager.RegisterMediator(TMediatorCalendarComboView, TtiObject, [tkFloat]);
  gMediatorManager.RegisterMediator(TMediatorSpinEditView, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TMediatorSpinEditFloatView, TtiObject, [tkFloat]);
end;

{ TMediatorEditView }

function TMediatorEditView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorEditView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TfpgEdit;
  inherited SetGUIControl(AValue);
end;

procedure TMediatorEditView.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGuiValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(FieldName);
  if oError <> nil then
  begin
    EditControl.BackgroundColor  := clError;
    EditControl.Hint   := oError.ErrorMessage;
  end
  else
  begin
    EditControl.BackgroundColor  := clBoxColor;
    EditControl.Hint   := '';
  end;
end;

procedure TMediatorEditView.SetupGUIandObject;
var
  Mi, Ma: Integer;
begin
  inherited;
  if Subject.GetFieldBounds(FieldName,Mi,Ma) and (Ma>0) then
    FEditControl.MaxLength := Ma;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.OnChange := @DoOnChange
  else
    FEditControl.OnExit := @DoOnChange;
end;

procedure TMediatorEditView.SetObjectUpdateMoment(const AValue: TObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if Assigned(FEditControl) then
    If ObjectUpdateMoment in [ouOnchange,ouCustom] then
      FeditControl.OnChange := @DoOnChange
    else
      FeditControl.OnExit := @DoOnChange;
end;

constructor TMediatorEditView.Create;
begin
  inherited Create;
  GuiFieldName:='Text';
end;

constructor TMediatorEditView.CreateCustom(pEditControl: TfpgWidget;
  pSubject: TtiObject; pFieldName: string; pGuiFieldName: string);
begin
  inherited;
end;

destructor TMediatorEditView.Destroy;
begin
  if Assigned(EditControl) and Assigned(EditControl.OnChange) then
    EditControl.OnChange := nil;
  inherited Destroy;
end;

class function TMediatorEditView.ComponentClass: TClass;
begin
  Result := TfpgEdit;
end;


{ TMediatorSpinEditView}

class function TMediatorSpinEditView.ComponentClass: TClass;
begin
  Result := TfpgSpinEdit;
end;

function TMediatorSpinEditView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorSpinEditView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TfpgSpinEdit;
  inherited SetGUIControl(AValue);
end;

procedure TMediatorSpinEditView.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGuiValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(FieldName);
  if oError <> nil then
  begin
    EditControl.BackgroundColor  := clError;
    EditControl.Hint   := oError.ErrorMessage;
  end
  else
  begin
    EditControl.BackgroundColor  := clWindowBackground;
    EditControl.Hint   := '';
  end;
end;

constructor TMediatorSpinEditView.Create;
begin
  inherited Create;
  GuiFieldName := 'Value';
end;


{ TMediatorTrackBarView}

function TMediatorTrackBarView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorTrackBarView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TfpgTrackBar;
  inherited;
end;

procedure TMediatorTrackBarView.DoTrackBarChanged(Sender: TObject; APosition: integer);
begin
  GUIChanged;
end;

procedure TMediatorTrackBarView.SetupGUIandObject;
var
  Mi, Ma: Integer;
begin
  inherited;
  if Subject.GetFieldBounds(FieldName,Mi,Ma) and (Ma>0) then
  begin
    FEditControl.Min := Mi;
    FEditControl.Max := Ma;
  end;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.OnChange := @DoTrackBarChanged
  else
    FeditControl.OnExit := @DoOnChange;
end;

constructor TMediatorTrackBarView.Create;
begin
  inherited;
  GuiFieldName := 'Position';
end;

class function TMediatorTrackBarView.ComponentClass: TClass;
begin
  Result := TfpgTrackBar;
end;


{ TMediatorComboBoxView }

class function TMediatorComboBoxView.ComponentClass: TClass;
begin
  Result := TfpgComboBox;
end;

function TMediatorComboBoxView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorComboBoxView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TfpgComboBox;
  inherited SetGUIControl(AValue);
end;

procedure TMediatorComboBoxView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.OnChange := @DoOnChange
  else
    FEditControl.OnExit := @DoOnChange;
end;

procedure TMediatorComboBoxView.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGuiValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(FieldName);
  if oError <> nil then
  begin
    EditControl.BackgroundColor  := clError;
    EditControl.Hint   := oError.ErrorMessage;
  end
  else
  begin
    EditControl.BackgroundColor  := clBoxColor;
    EditControl.Hint   := '';
  end;
end;

constructor TMediatorComboBoxView.Create;
begin
  inherited Create;
  GuiFieldName := 'Text'; //'FocusItem';
end;

procedure TMediatorComboBoxView.DoObjectToGui;
begin
  EditControl.FocusItem :=
      EditControl.Items.IndexOf(Subject.PropValue[FieldName]);
end;


{ TMediatorMemoView }

class function TMediatorMemoView.ComponentClass: TClass;
begin
  Result := TfpgMemo;
end;

procedure TMediatorMemoView.DoGuiToObject;
begin
  Subject.PropValue[FieldName] := EditControl.Lines.Text;
end;

procedure TMediatorMemoView.DoObjectToGui;
begin
  EditControl.Lines.Text := Subject.PropValue[FieldName];
end;

function TMediatorMemoView.GetGUIControl: TComponent;
begin
  Result:=FEditControl;
end;

procedure TMediatorMemoView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl:=AValue as TfpgMemo;
  inherited SetGUIControl(AValue);
end;

procedure TMediatorMemoView.SetupGUIandObject;
begin
  inherited;
  EditControl.Lines.Clear;

  //if UseInternalOnChange then
    //EditControl.OnChange := @DoOnChange; // default OnChange event handler
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.OnChange := @DoOnChange
  else
    FEditControl.OnExit := @DoOnChange;

//  EditControl.ScrollBars := ssVertical;
//  EditControl.WordWrap   := True;
end;


{ TMediatorDynamicComboBoxView }

procedure TMediatorDynamicComboBoxView.SetListObject(const AValue: TtiObjectList);
begin
  inherited;
  InternalListRefresh;
  if Assigned(ValueList) then
    EditControl.Enabled := ValueList.Count > 0;
end;

procedure TMediatorDynamicComboBoxView.InternalListRefresh;
var
  lItems: TStrings;
  i: Integer;
begin
  lItems := EditControl.Items;
  lItems.Clear;
  EditControl.Text := '';

  if (ValueList = nil) or
     (ValueList.Count < 1) or
     (SameText(FieldName, EmptyStr)) then
    Exit; //==>

  try
    for i := 0 to ValueList.Count - 1 do
    begin
      lItems.Add(ValueList.Items[i].Caption);
    end;
  except
    on E: Exception do
      raise Exception.CreateFmt('Error adding list items to combobox ' +
                                 'Message: %s, Item Property Name: %s',
                                 [E.message, FieldName]);
  end;

  ObjectToGui;
end;

procedure TMediatorDynamicComboBoxView.SetOnChangeActive(AValue: Boolean);
begin
  if AValue then
  begin
    if not UseInternalOnChange then
      EditControl.OnChange := FExternalOnChange
    else
      EditControl.OnChange := @DoOnChange;
  end
  else
  begin
    if not UseInternalOnChange then
      FExternalOnChange := EditControl.OnChange;
    EditControl.OnChange := nil;
  end;
end;

procedure TMediatorDynamicComboBoxView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;

  if UseInternalOnChange then
    EditControl.OnChange := @DoOnChange; // default OnChange event handler

  {$Note As far as I can see, ValueList is always going to be nil here! - Graeme }
  if ValueList <> nil then
    EditControl.Enabled := (ValueList.Count > 0);
end;

procedure TMediatorDynamicComboBoxView.DoGuiToObject;
var
  lValue: TtiObject;
  lPropType: TTypeKind;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  if EditControl.FocusItem < 0 then
    Exit; //==>

  lValue := TtiObject(ValueList.Items[EditControl.FocusItem]);

  lPropType := typinfo.PropType(Subject, FieldName);
  if lPropType = tkClass then
    typinfo.SetObjectProp(Subject, FieldName, lValue)
  else
    raise EtiOPFProgrammerException.Create('Error property type not a Class');
end;

procedure TMediatorDynamicComboBoxView.DoObjectToGui;
var
  i: Integer;
  lValue: TtiObject;
  lPropType: TTypeKind;
begin
  SetOnChangeActive(false);

  //  Set the index only (We're assuming the item is present in the list)
  EditControl.FocusItem := -1;
  if Subject = nil then
    Exit; //==>

  if not Assigned(ValueList) then
    raise EtiOPFProgrammerException.Create(cErrorListHasNotBeenAssigned);

  lPropType := typinfo.PropType(Subject, FieldName);
  if lPropType = tkClass then
    lValue := TtiObject(typinfo.GetObjectProp(Subject, FieldName))
  else
    raise Exception.Create('Property is not a class type!');

  for i := 0 to ValueList.Count - 1 do
    if ValueList.Items[i] = lValue then
    begin
      EditControl.FocusItem := i;
      Break; //==>
    end;

  SetOnChangeActive(true);
end;

procedure TMediatorDynamicComboBoxView.RefreshList;
begin
  InternalListRefresh;
end;


{ TMediatorCheckBoxView }

function TMediatorCheckBoxView.GetGUIControl: TComponent;
begin
  Result:=FEditControl;
end;

procedure TMediatorCheckBoxView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl:=AValue as TfpgCheckBox;
  inherited SetGUIControl(AValue);
end;

procedure TMediatorCheckBoxView.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGuiValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(FieldName);
  if oError <> nil then
  begin
    EditControl.BackgroundColor  := clError;
    EditControl.Hint   := oError.ErrorMessage;
  end
  else
  begin
    EditControl.BackgroundColor  := clWindowBackground;
    EditControl.Hint   := '';
  end;
end;

constructor TMediatorCheckBoxView.Create;
begin
  inherited Create;
  GuiFieldName:='Checked';
end;

class function TMediatorCheckBoxView.ComponentClass: TClass;
begin
  Result := TfpgCheckBox;
end;


{ TMediatorStaticTextView }

procedure TMediatorStaticTextView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  EditControl.Text := '';
end;

constructor TMediatorStaticTextView.Create;
begin
  inherited Create;
  GuiFieldName := 'Text';
end;

function TMediatorStaticTextView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorStaticTextView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TfpgLabel;
  inherited SetGUIControl(AValue);
end;

class function TMediatorStaticTextView.ComponentClass: TClass;
begin
  Result := TfpgLabel;
end;


{ TMediatorCalendarComboView }

function TMediatorCalendarComboView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorCalendarComboView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TfpgCalendarCombo;
  inherited SetGUIControl(AValue);
end;

constructor TMediatorCalendarComboView.Create;
begin
  inherited Create;
  GUIFieldName := 'DateValue';
end;

class function TMediatorCalendarComboView.ComponentClass: TClass;
begin
  Result := TfpgCalendarCombo;
end;


{ TMediatorItemComboBoxView }

procedure TMediatorItemComboBoxView.DoGUIToObject;
begin
  SetOrdProp(Subject,FieldName,EditControl.FocusItem);
end;

procedure TMediatorItemComboBoxView.DoObjectToGUI;
begin
  EditCOntrol.FocusItem := GetOrdProp(Subject,FieldName);
end;

constructor TMediatorItemComboBoxView.Create;
begin
  inherited Create;
  GuiFieldName := 'FocusItem';
end;

{ TMediatorSpinEditFloatView }

function TMediatorSpinEditFloatView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorSpinEditFloatView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TfpgSpinEditFloat;
  inherited SetGUIControl(AValue);
end;

procedure TMediatorSpinEditFloatView.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGuiValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(FieldName);
  if oError <> nil then
  begin
    EditControl.BackgroundColor  := clError;
    EditControl.Hint   := oError.ErrorMessage;
  end
  else
  begin
    EditControl.BackgroundColor  := clWindowBackground;
    EditControl.Hint   := '';
  end;
end;

constructor TMediatorSpinEditFloatView.Create;
begin
  inherited Create;
  GuiFieldName := 'Value';
end;

class function TMediatorSpinEditFloatView.ComponentClass: TClass;
begin
  Result := TfpgSpinEditFloat;
end;

end.

