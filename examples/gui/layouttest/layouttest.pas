program LayoutTest;

uses
  Classes
  ,fpGUI
  ,fpguipackage
  ,fpGFX
  ;

type
  TDockingForm = class;
  TGridForm = class;
  TBoxForm = class;
  TSimpleForm = class;
  
  { TMainForm }

  TMainForm = class(TForm)
  private
    DockingForm: TDockingForm;
    GridForm: TGridForm;
    BoxForm: TBoxForm;
    SimpleForm: TSimpleForm;
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

  TSimpleForm = class(TForm)
    Button: TFButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{  TFixedForm = class(TForm)
    Layout: TFixedLayout;
    Button1, Button2: TFButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;        }

  TDockingForm = Class(TForm)
    Layout : TDockingLayout;
    Button1,Button2,Button3,Button4,Button5 : TFButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  

  TGridForm = Class(TForm)
    Layout : TFGridLayout;
    Button1,Button2,Button3,Button4,Button5, Button6 : TFButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TBoxForm = Class(TForm)
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
  Box.VertAlign := vertFill;
  InsertChild(Box);

  Title := TFLabel.Create('Choose a test Window:', self);
  Box.InsertChild(Title);
  
  SimpleBtn := TFButton.Create('Simple layout', self);
  SimpleBtn.OnClick := @SimpleBtnClicked;
  Box.InsertChild(SimpleBtn);
  
  FixedBtn := TFButton.Create('Fixed layout', self);
  FixedBtn.OnClick := @FixedBtnClicked;
  FixedBtn.Enabled := False;
  Box.InsertChild(FixedBtn);

  BoxBtn := TFButton.Create('Boxed layout', self);
  BoxBtn.OnClick := @BoxBtnClicked;
  Box.InsertChild(BoxBtn);
  
  GridBtn := TFButton.Create('Grid layout', self);
  GridBtn.OnClick := @GridBtnClicked;
  GridBtn.Enabled := False;
  Box.InsertChild(GridBtn);
  
  DockingBtn := TFButton.Create('Docking layout', self);
  DockingBtn.OnClick := @DockingBtnClicked;
  DockingBtn.Enabled := False;
  Box.InsertChild(DockingBtn);
  
  Separator := TSeparator.Create(self);
  Box.InsertChild(Separator);
  
  ExitBtn := TFButton.Create('Exit', self);
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
//  GFApplication.AddWindow(SimpleForm.Wnd);
  SimpleForm.Show;
end;


procedure TMainForm.FixedBtnClicked(Sender: TObject);
begin
//  Application.AddForm(TFixedForm.Create(Application));
end;


procedure TMainForm.DockingBtnClicked(Sender: TObject);
begin
  Exit; //==>
  
{  if not Assigned(DockingForm) then
    DockingForm := TDockingForm.Create(self);
  DockingForm.Show;
}
//  Application.AddForm(TDockingForm.Create(self));
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
{
constructor TFixedForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Text := 'Fixed Layout';
  BorderWidth := 8;

  Layout := TFixedLayout.Create(Self);
    Layout.Name := 'Layout';
    Button1 := TFButton.Create(Self);
      Button1.Name := 'Button1';
      Button1.Text := 'A button';
    Layout.AddControl(Button1, 20, 20);
    Button2 := TFButton.Create(Self);
      Button2.Name := 'Button2';
      Button2.Text := 'Another button';
    Layout.AddControl(Button2, 50, 100);
  Child := Layout;
end;

}
// -------------------------------------------------------------------
//   TDockingForm
// -------------------------------------------------------------------


constructor TDockingForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Text := 'Docking Layout';
  BorderWidth := 8;

  Layout := TDockingLayout.Create(Self);
    Layout.Name := 'Layout';
    Button1 := TFButton.Create(Self);
      Button1.Name := 'BTop';
      Button1.Text := 'Top Alignment';
    Layout.AddWidget(Button1, dmTop);
    Button2 := TFButton.Create(Self);
      Button2.Name := 'BBottom';
      Button2.Text := 'Bottom Alignment';
    Layout.AddWidget(Button2, dmBottom);
    Button3 := TFButton.Create(Self);
      Button3.Name := 'BLeft';
      Button3.Text := 'Left Alignment';
    Layout.AddWidget(Button3, dmLeft);
    Button4 := TFButton.Create(Self);
      Button4.Name := 'BRight';
      Button4.Text := 'Right Alignment';
    Layout.AddWidget(Button4, dmRight);
    Button5 := TFButton.Create(Self);
      Button5.Name := 'BCLient';
      Button5.Text := 'Client Alignment';
    Layout.AddWidget(Button5, dmClient);
  Child := Layout;
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

    Button1 := TFButton.Create(Self);
      Button1.Name := 'TopLeft';
      Button1.Text := 'Top Left';
    Layout.AddWidget(Button1, 0, 0, 1, 1);
    Button2 := TFButton.Create(Self);
      Button2.Name := 'TopRight';
      Button2.Text := 'Top Right';
    Layout.AddWidget(Button2, 2,0,1,1);
    Button3 := TFButton.Create(Self);
      Button3.Name := 'CenterCenter';
      Button3.Text := 'Center Center';
      // Button3.CanExpandWidth := False;
      // Button3.CanExpandHeight := False;
    Layout.AddWidget(Button3, 1,1,1,1);
    Button4 := TFButton.Create(Self);
      Button4.Name := 'BottomLeft';
      Button4.Text := 'Bottom Left';
    Layout.AddWidget(Button4,0,2,1,1);
    Button5 := TFButton.Create(Self);
      Button5.Name := 'BottomRight';
      Button5.Text := 'Bottom Right';
    Layout.AddWidget(Button5, 2,2,1,1);
    Button6 := TFButton.Create(Self);
      Button6.Name := 'BottomSpan';
      Button6.Text := 'Span Columns';
    Layout.AddWidget(Button6, 0,3,3,1);
  Child := Layout;
end;


// -------------------------------------------------------------------
//   TBoxForm
// -------------------------------------------------------------------

procedure TBoxForm.FlipOrientation (Sender : TObject);
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
