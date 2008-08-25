{
  Purpose:
    Abstract mediating view and Mediator Factory. This allows you to use
    standard edit components and make them object-aware.  See the demo
    application for usage.

  ToDo:
    * As soon as TfpgSpinEdit has been implemented, port the SpinEdit mediator
}

unit tiMediators;

{$mode objfpc}{$H+}

interface
uses
  tiObject
  ,Classes
  ,tiBaseMediator
  ,fpgfx
  ,gfx_widget
  ,gui_edit
  ,gui_checkbox
  ,gui_label
  ,gui_trackbar
  ,gui_combobox
  ,gui_memo
  ,gui_popupcalendar
  ;

type

  { Base class to handle TfpgEdit controls }
  TMediatorEditView = class(TMediatorView)
  private
    FEditControl: TfpgEdit;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
  protected
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment (Const AValue : TObjectUpdateMoment); override;
  public
    Constructor Create; override;
    constructor CreateCustom(pEditControl: TfpgWidget; pSubject: TtiObject; pFieldName: string; pGuiFieldName: string = 'Text'); reintroduce;
    destructor  Destroy; override;
    property    EditControl: TfpgEdit read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgCheckBox controls }
  TMediatorCheckBoxView = class(TMediatorView)
  private
    FEditControl: TfpgCheckBox;
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
  protected
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
  public
    Constructor Create; override;
    property    EditControl: TfpgCheckBox read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgLabel controls }
  TMediatorStaticTextView = class(TMediatorView)
  private
    FEditControl: TfpgLabel;
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
  protected
    procedure   SetupGUIandObject; override;
  public
    Constructor Create; override;
    property    EditControl: TfpgLabel read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TSpinEdit controls }
{
  TMediatorSpinEditView = class(TMediatorView)
  private
    function    GetEditControl: TSpinEdit;
    procedure   OnLostFocus(Sender: TObject);
    procedure   SetEditControl(const AValue: TSpinEdit);
  protected
    procedure   SetupGUIandObject; override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
  public
    property    EditControl: TSpinEdit read GetEditControl write SetEditControl;
    procedure   GuiToObject; override;
    class function ComponentClass: TClass; override;
  end;
}

  { Base class to handle TfpgTrackBar controls }
  TMediatorTrackBarView = class(TMediatorView)
  private
    FEditControl: TfpgTrackBar;
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   DoTrackBarChanged(Sender: TObject; APosition: integer);
  protected
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
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
  protected
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
    procedure   DoObjectToGui; override;
  public
    Constructor Create; override;
    property    EditControl: TfpgComboBox read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  // Sets ItemIndex based on integer property
  TMediatorItemComboBoxView = class(TMediatorComboBoxView)
  Protected
    Procedure DoGUIToObject; override;
    Procedure DoObjectToGUI; override;
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
  public
    procedure   DoGuiToObject; override;
    procedure   DoObjectToGui; override;
    procedure   RefreshList; virtual;
  end;
  

  { Base class to handle TfpgMemo controls }
  TMediatorMemoView = class(TMediatorView)
  private
    FEditControl: TfpgMemo;
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
  protected
    procedure   SetupGUIandObject; override;
  public
    property    EditControl: TfpgMemo read FEditControl write FEditControl;
    procedure   DoObjectToGui; override;
    procedure   DoGuiToObject; override;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TfpgCalendarCombo controls }
  TMediatorCalendarComboView = class(TMediatorView)
  private
    FEditControl: TfpgCalendarCombo;
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
  public
    Constructor Create; override;
    property    EditControl: TfpgCalendarCombo read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


implementation
uses
  SysUtils
  ,TypInfo
  ,tiExcept
  ,gui_dialogs      // for TfpgMessageDialog
  ,tiGUIConstants   // for error color
  ,gfxbase          // for predefined colors
  ;

const
  cErrorListHasNotBeenAssigned   = 'List has not been assigned';


{ TMediatorEditView }

function TMediatorEditView.GetGUIControl: TComponent;
begin
  Result:=FeditControl;
end;

procedure TMediatorEditView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl:=AValue as TfpgEdit;
  Inherited;
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
    FeditControl.OnChange := @DoOnChange
  else
    FeditControl.OnExit := @DoOnChange;
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
(*
class function TMediatorSpinEditView.ComponentClass: TClass;
begin
  Result := TSpinEdit;
end;


procedure TMediatorSpinEditView.GuiToObject;
begin
  { Control is busy clearing the value before replacing it with what the user
    typed. }
  if (TSpinEdit(EditControl).Text = '') then
    Exit; //==>

  { continue as normal }
  inherited;
end;

function TMediatorSpinEditView.GetEditControl: TSpinEdit;
begin
  Result := TSpinEdit(FEditControl);
end;

procedure TMediatorSpinEditView.OnLostFocus(Sender: TObject);
begin
  if (TSpinEdit(EditControl).Text = '') then
  begin
    { Default the EditControl to a valid value }
    TSpinEdit(EditControl).Value := 0;
    GUIChanged;
  end;
end;

procedure TMediatorSpinEditView.SetEditControl(const AValue: TSpinEdit);
begin
  FEditControl := AValue;
end;


procedure TMediatorSpinEditView.SetupGUIandObject;
begin
  inherited;
  TSpinEdit(EditControl).Text := '';
  TSpinEdit(EditControl).OnExit := OnLostFocus;
end;

procedure TMediatorSpinEditView.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGuiValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(FieldName);
  if oError <> nil then
  begin
    EditControl.Color  := clError;
    EditControl.Hint   := oError.ErrorMessage;
  end
  else
  begin
    EditControl.Color  := ColorToRGB(clWindow);
    EditControl.Hint   := '';
  end;
end;
*)

{ TMediatorTrackBarView}

function TMediatorTrackBarView.GetGUIControl: TComponent;
begin
  Result:=FEditControl;
end;

procedure TMediatorTrackBarView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl:=AValue as TfpgTrackBar;
  Inherited;
end;

procedure TMediatorTrackBarView.DoTrackBarChanged(Sender: TObject; APosition: integer);
begin
//  writeln(' executing - DoTrackBarChanged');
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
  begin
//    writeln('  Trackbar: setting OnChange event handler');
    FEditControl.OnChange := @DoTrackBarChanged;
  end;
//  else
//    FeditControl.OnExit := @DoOnChange;
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
  inherited;
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
  GuiFieldName:='FocusItem';
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
  Inherited;
end;

procedure TMediatorMemoView.SetupGUIandObject;
begin
  inherited;
  EditControl.Lines.Clear;
//  EditControl.ScrollBars := ssVertical;
//  EditControl.WordWrap   := True;
end;


{ TMediatorDynamicComboBoxView }

procedure TMediatorDynamicComboBoxView.SetListObject(const AValue: TtiObjectList);
begin
  Inherited;
  InternalListRefresh;
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

  EditControl.Enabled   := (ValueList.Count > 0);
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
  Inherited;
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
  inherited;
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
  inherited;
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
  EditCOntrol.FocusItem:=GetOrdProp(Subject,FieldName);
end;

end.

