program LayoutTest;

uses
  Classes
  ,fpGUI
  ,fpguipackage
  ,fpGFX
  ,SysUtils
  ;

type
  // Forward declarations
  TDockingForm = class;
  TGridForm = class;
  TBoxForm = class;
  TSimpleForm = class;
  TFixedForm = class;
  
  { TMainForm }

  TMainForm = class(TFForm)
  private
    DockingForm: TDockingForm;
    GridForm: TGridForm;
    BoxForm: TBoxForm;
    SimpleForm: TSimpleForm;
    FixedForm: TFixedForm;
    procedure   BuildGUI;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    Box: TFBoxLayout;
    Title: TFLabel;
    SimpleBtn, FixedBtn, BoxBtn, GridBtn, DockingBtn, ExitBtn: TFButton;
    Separator: TSeparator;
    procedure   SimpleBtnClicked(Sender: TObject);
    procedure   FixedBtnClicked(Sender: TObject);
    procedure   DockingBtnClicked(Sender: TObject);
    procedure   GridBtnClicked(Sender: TObject);
    procedure   BoxBtnClicked(Sender: TObject);
    procedure   ExitBtnClicked(Sender: TObject);
  end;


  TSimpleForm = class(TFForm)
    Button: TFButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TFixedForm = class(TFForm)
    Layout: TFFixedLayout;
    Button1, Button2: TFButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
  
  TDockingForm = Class(TFForm)
    Layout: TFDockingLayout;
    Button1, Button2, Button3, Button4, Button5: TFButton;
    ListBox: TFListBox;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  

  TGridForm = Class(TFForm)
    Layout: TFGridLayout;
    Button1, Button2, Button3, Button4, Button5, Button6: TFButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  

  TBoxForm = Class(TFForm)
    Layout, BoxLayout: TFBoxLayout;
    Button1, Button2, Button3, FlipButton: TFButton;
    procedure   FlipOrientation(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;


// -------------------------------------------------------------------
//   TMainForm
// -------------------------------------------------------------------

procedure TMainForm.BuildGUI;
begin
  Name := 'MainForm';
  BorderWidth := 8;
  Text := 'Layout Demo';

  Box := TFBoxLayout.Create(self);
  Box.Spacing := 8;
  Box.Orientation := Vertical;
  InsertChild(Box);

  Title := TFLabel.Create('Choose a test Window:', self);
  Box.InsertChild(Title);
  
  SimpleBtn := TFButton.Create('Simple layout', self);
  SimpleBtn.CanExpandWidth := True;
  SimpleBtn.OnClick := @SimpleBtnClicked;
  Box.InsertChild(SimpleBtn);
  
  FixedBtn := TFButton.Create('Fixed layout', self);
  FixedBtn.CanExpandWidth := True;
  FixedBtn.OnClick := @FixedBtnClicked;
  Box.InsertChild(FixedBtn);

  BoxBtn := TFButton.Create('Boxed layout', self);
  BoxBtn.CanExpandWidth := True;
  BoxBtn.OnClick := @BoxBtnClicked;
  Box.InsertChild(BoxBtn);
  
  GridBtn := TFButton.Create('Grid layout', self);
  GridBtn.CanExpandWidth := True;
  GridBtn.OnClick := @GridBtnClicked;
  Box.InsertChild(GridBtn);
  
  DockingBtn := TFButton.Create('Docking layout', self);
  DockingBtn.CanExpandWidth := True;
  DockingBtn.OnClick := @DockingBtnClicked;
  Box.InsertChild(DockingBtn);
  
  Separator := TSeparator.Create(self);
  Box.InsertChild(Separator);
  
  ExitBtn := TFButton.Create('Exit', self);
  ExitBtn.CanExpandWidth := True;
  ExitBtn.OnClick := @ExitBtnClicked;
  Box.InsertChild(ExitBtn);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BuildGUI;
end;

destructor TMainForm.Destroy;
begin
  FixedForm.Free;
  GridForm.Free;
  BoxForm.Free;
  DockingForm.Free;
  SimpleForm.Free;
  inherited Destroy;
end;


procedure TMainForm.SimpleBtnClicked(Sender: TObject);
begin
  if not Assigned(SimpleForm) then
    SimpleForm := TSimpleForm.Create(self);
  SimpleForm.Show;
end;


type
  TFriend = class(TFFixedLayout)
  end;
  
procedure TMainForm.FixedBtnClicked(Sender: TObject);
begin
  if not Assigned(FixedForm) then
    FixedForm := TFixedForm.Create(self);
  FixedForm.Show;
end;


procedure TMainForm.DockingBtnClicked(Sender: TObject);
begin
  if not Assigned(DockingForm) then
    DockingForm := TDockingForm.Create(self);
  DockingForm.Show;
end;


procedure TMainForm.GridBtnClicked(Sender: TObject);
var
  f, f2: TStream;
begin
  if not Assigned(GridForm) then
    GridForm := TGridForm.Create(self);
  GridForm.Show;
  { Output the structure of GridForm.Layout }
  f := TMemoryStream.Create;
  f.WriteComponent(GridForm.Layout);
  f2 := THandleStream.Create(StdOutputHandle);
  f.Position := 0;
  ObjectBinaryToText(f, f2);
  f2.Free;
  f.Free;
end;


procedure TMainForm.BoxBtnClicked(Sender: TObject);
begin
  if not Assigned(BoxForm) then
    BoxForm := TBoxForm.Create(self);
  BoxForm.Show;
end;


procedure TMainForm.ExitBtnClicked(Sender: TObject);
begin
  Close;
end;


// -------------------------------------------------------------------
//   TSimpleForm
// -------------------------------------------------------------------

constructor TSimpleForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Text := 'Simple Layout';
  BorderWidth := 8;

  Button := TFButton.Create(Self);
    Button.Text := 'A button...';
  Child := Button;
end;


// -------------------------------------------------------------------
//   TFixedForm
// -------------------------------------------------------------------

constructor TFixedForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Text := 'Fixed Layout';
  BorderWidth := 8;

  Layout := TFFixedLayout.Create(Self);
  Layout.Name := 'Layout';
  InsertChild(Layout);

    Button1 := TFButton.Create('A button', self);
      Button1.Name := 'Button1';
    Layout.AddWidget(Button1, 20, 20);

    Button2 := TFButton.Create('Another button', self);
      Button2.Name := 'Button2';
    Layout.AddWidget(Button2, 50, 100);
    Button2.SetBounds(50, 100, 100, 25);  // resize second button to show it works
end;


// -------------------------------------------------------------------
//   TDockingForm
// -------------------------------------------------------------------


constructor TDockingForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Text := 'Docking Layout';
  BorderWidth := 8;

  Layout := TFDockingLayout.Create(Self);
  Layout.Name := 'Layout';
  InsertChild(Layout);

    Layout.AddWidget(TFButton.Create('Top Alignment', self), dmTop);
    Layout.AddWidget(TFButton.Create('Bottom Alignment', self), dmBottom);
    Layout.AddWidget(TFButton.Create('Left Alignment', self), dmLeft);
    Layout.AddWidget(TFButton.Create('Right Alignment', self), dmRight);

    Layout.AddWidget(TFButton.Create('aaa (bottom)', self), dmBottom);
    Layout.AddWidget(TFButton.Create('bbb (bottom)', self), dmBottom);
    Layout.AddWidget(TFButton.Create('ccc (bottom)', self), dmBottom);

    ListBox := TFListBox.Create(self);
    ListBox.Items.Add('Client Alignment');
    ListBox.Items.Add('');
    ListBox.Items.Add('procedure KeyPressed();');
    ListBox.Items.Add('procedure KeyReleased();');
    ListBox.Items.Add('procedure ButtonPressed();');
    ListBox.Items.Add('procedure ButtonReleased();');
    ListBox.Items.Add('procedure EnterWindow();');
    ListBox.Items.Add('procedure LeaveWindow();');
    ListBox.Items.Add('procedure PointerMoved();');
    ListBox.Items.Add('procedure Expose();');
    ListBox.Items.Add('procedure FocusIn();');
    ListBox.Items.Add('procedure FocusOut();');
    ListBox.Items.Add('procedure Map();');
    ListBox.Items.Add('procedure Unmap();');
    ListBox.Items.Add('procedure Reparent();');
    ListBox.Items.Add('procedure DestroyWindow();');
    ListBox.Items.Add('procedure Configure();');
    ListBox.Items.Add('procedure ClientMessage();');
    Layout.AddWidget(ListBox, dmClient);
end;


// -------------------------------------------------------------------
//   TGridForm
// -------------------------------------------------------------------

constructor TGridForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Text := 'Grid Layout';
  BorderWidth := 8;

  Layout := TFGridLayout.Create(Self);
  Layout.Name := 'Layout';
  Layout.RowCount := 4;
  Layout.ColCount := 3;
  InsertChild(Layout);

    Button1 := TFButton.Create('Top Left', Self);
      Button1.Name := 'TopLeft';
    Layout.AddWidget(Button1, 0, 0, 1, 1);

    Button2 := TFButton.Create('Top Right', Self);
      Button2.Name := 'TopRight';
    Layout.AddWidget(Button2, 2,0,1,1);

    Button3 := TFButton.Create(Self);
      Button3.Name := 'CenterCenter';
      Button3.Text := 'Center Center';
    Layout.AddWidget(Button3, 1,1,1,1);

    Button4 := TFButton.Create('Bottom Left', Self);
      Button4.Name := 'BottomLeft';
    Layout.AddWidget(Button4,0,2,1,1);

    Button5 := TFButton.Create('Bottom Right', Self);
      Button5.Name := 'BottomRight';
    Layout.AddWidget(Button5, 2,2,1,1);

    Button6 := TFButton.Create('Span Columns', Self);
      Button6.Name := 'BottomSpan';
    Layout.AddWidget(Button6, 0,3,3,1);
end;


// -------------------------------------------------------------------
//   TBoxForm
// -------------------------------------------------------------------

procedure TBoxForm.FlipOrientation(Sender: TObject);
begin
  with BoxLayout do
  begin
    if Orientation = Horizontal then
    begin
      Orientation := Vertical;
      FlipButton.Text:='Switch to horizontal';
    end
    else
    begin
      Orientation := Horizontal;
      FlipButton.text:='Switch to vertical';
    end;
  end;  { with }
  
  // A quick hack to get the Form to redraw correctly
  Close;
  Show;
end;

constructor TBoxForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text := 'Box Layout';
  BorderWidth := 8;
  
  Layout := TFBoxLayout.Create(self);
  Layout.Spacing := 8;
  Layout.Orientation := Vertical;
  InsertChild(Layout);
  
  BoxLayout := TFBoxLayout.Create(self);
  BoxLayout.Spacing := 4;
  Layout.InsertChild(BoxLayout);
  
    Button1 := TFButton.Create('Button 1', self);
    BoxLayout.InsertChild(Button1);

    Button2 := TFButton.Create('Button 2', self);
    BoxLayout.InsertChild(Button2);

    Button3 := TFButton.Create('Button 3', self);
    BoxLayout.InsertChild(Button3);

  FlipButton := TFButton.Create('Switch to vertical', self);
  FlipButton.OnClick := @FlipOrientation;
  Layout.InsertChild(FlipButton);
end;


var
  MainForm: TMainForm;
begin
  GFApplication.Initialize;
  MainForm := TMainForm.Create(GFApplication);
  try
    MainForm.Show;
    GFApplication.Run;
  finally
    MainForm.Free;
  end;
end.
