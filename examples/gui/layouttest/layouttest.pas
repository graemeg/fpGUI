program LayoutTest;

uses Classes, fpGUI;

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
  public
    destructor Destroy; override;
  published
    Box: TBoxLayout;
    Title: TLabel;
    SimpleBtn, FixedBtn, BoxBtn, GridBtn, DockingBtn, ExitBtn: TButton;
    //Separator: TSeparator;
    procedure SimpleBtnClicked(Sender: TObject);
    procedure FixedBtnClicked(Sender: TObject);
    procedure DockingBtnClicked(Sender: TObject);
    procedure GridBtnClicked(Sender: TObject);
    procedure BoxBtnClicked(Sender: TObject);
    procedure ExitBtnClicked(Sender: TObject);
  end;

  TSimpleForm = class(TForm)
    Button: TButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{  TFixedForm = class(TForm)
    Layout: TFixedLayout;
    Button1, Button2: TButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;        }

  TDockingForm = Class(TForm)
    Layout : TDockingLayout;
    Button1,Button2,Button3,Button4,Button5 : TButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  

  TGridForm = Class(TForm)
    Layout : TGridLayout;
    Button1,Button2,Button3,Button4,Button5, Button6 : TButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TBoxForm = Class(TForm)
    Layout, BoxLayout: TBoxLayout;
    Button1, Button2, Button3, FlipButton: TButton;
    procedure FlipOrientation(Sender: TObject);
  end;


// -------------------------------------------------------------------
//   TMainForm
// -------------------------------------------------------------------

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
  Application.AddForm(TDockingForm.Create(self));
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
    Application.CreateForm(TBoxForm, BoxForm);
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

  Button := TButton.Create(Self);
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
    Button1 := TButton.Create(Self);
      Button1.Name := 'Button1';
      Button1.Text := 'A button';
    Layout.AddControl(Button1, 20, 20);
    Button2 := TButton.Create(Self);
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
    Button1 := TButton.Create(Self);
      Button1.Name := 'BTop';
      Button1.Text := 'Top Alignment';
    Layout.AddWidget(Button1, dmTop);
    Button2 := TButton.Create(Self);
      Button2.Name := 'BBottom';
      Button2.Text := 'Bottom Alignment';
    Layout.AddWidget(Button2, dmBottom);
    Button3 := TButton.Create(Self);
      Button3.Name := 'BLeft';
      Button3.Text := 'Left Alignment';
    Layout.AddWidget(Button3, dmLeft);
    Button4 := TButton.Create(Self);
      Button4.Name := 'BRight';
      Button4.Text := 'Right Alignment';
    Layout.AddWidget(Button4, dmRight);
    Button5 := TButton.Create(Self);
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

  Layout := TGridLayout.Create(Self);
    Layout.Name := 'Layout';
    Layout.RowCount := 4;
    Layout.ColCount := 3;

    Button1 := TButton.Create(Self);
      Button1.Name := 'TopLeft';
      Button1.Text := 'Top Left';
    Layout.AddWidget(Button1, 0, 0, 1, 1);
    Button2 := TButton.Create(Self);
      Button2.Name := 'TopRight';
      Button2.Text := 'Top Right';
    Layout.AddWidget(Button2, 2,0,1,1);
    Button3 := TButton.Create(Self);
      Button3.Name := 'CenterCenter';
      Button3.Text := 'Center Center';
      // Button3.CanExpandWidth := False;
      // Button3.CanExpandHeight := False;
    Layout.AddWidget(Button3, 1,1,1,1);
    Button4 := TButton.Create(Self);
      Button4.Name := 'BottomLeft';
      Button4.Text := 'Bottom Left';
    Layout.AddWidget(Button4,0,2,1,1);
    Button5 := TButton.Create(Self);
      Button5.Name := 'BottomRight';
      Button5.Text := 'Bottom Right';
    Layout.AddWidget(Button5, 2,2,1,1);
    Button6 := TButton.Create(Self);
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
end;


var
  MainForm: TMainForm;
begin
  MainForm := nil;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
  MainForm.Free;
end.
