program hello;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpgui, gfxbase, frmCompilerOpt, OpenSoftStyle;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure Button3Clicked(Sender: TObject);
  private
    procedure btnCloseClick(Sender: TObject);
    procedure btnGridFormClick(Sender: TObject);
    procedure btnCompOptClick(Sender: TObject);
    procedure btnTopClick(Sender: TObject);
    procedure MainFormActivate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    HBox: TBoxLayout;
    VBox: TBoxLayout;
    TextLabel: TLabel;
    Button: TButton;
    btnGridForm: TButton;
    btnCompOpt: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
  end;
  
  
  { TGridForm }

  TGridForm = Class(TForm)
    Layout : TGridLayout;
    Button1,Button2,Button3,Button4,Button5 : TButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
  
  { TFindDialog }

  TFindDialog = class(TForm)
  private
    procedure btnCloseClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    lblFind: TLabel;
    btnFindNext, btnClose: TButton;
    edFind: TEdit;
    GroupBox1: TGroupBox;
    grpBox1Layout: TBoxLayout;
    Radio1, Radio2: TRadioButton;
    cbCase: TCheckBox;
    leftLayout, rightLayout: TBoxLayout;
    topLeftLayout: TBoxLayout;
    mainLayout: TBoxLayout;
  end;


{ TFindDialog }

procedure TFindDialog.btnCloseClick(Sender: TObject);
begin
  Close;
end;


constructor TFindDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text := 'Find Dialog';
  BorderWidth := 8;
  WindowType := wtWindow;

  topLeftLayout := TBoxLayout.Create(self);
  lblFind := TLabel.Create(self);
  lblFind.Text := 'Find what';
  edFind := TEdit.Create(self);
  topLeftLayout.InsertChild(lblFind);
  topLeftLayout.InsertChild(edFind);
  
  leftLayout := TBoxLayout.Create(self);
  leftLayout.Orientation := Vertical;
  leftLayout.CanExpandHeight := True;
  GroupBox1 := TGroupBox.Create(self);
  GroupBox1.Text := 'Direction';
  grpBox1Layout := TBoxLayout.Create(self);
//  grpBox1Layout.Orientation := Vertical;
  GroupBox1.InsertChild(grpBox1Layout);
  Radio1 := TRadioButton.Create(self);
  Radio1.Text := 'Up';
  Radio2 := TRadioButton.Create(self);
  Radio2.Text := 'Down';
  grpBox1Layout.InsertChild(Radio1);
  grpBox1Layout.InsertChild(Radio2);

  cbCase := TCheckBox.Create(self);
  cbCase.Text := 'Case sensitive';
  leftLayout.InsertChild(topLeftLayout);
  leftLayout.InsertChild(GroupBox1);
  leftLayout.InsertChild(cbCase);
  
  rightLayout := TBoxLayout.Create(self);
  rightLayout.Orientation := Vertical;
  rightLayout.VertAlign := vertTop;
  btnFindNext := TButton.Create(self);
  btnFindNext.Text := 'Find Next';
  btnClose := TButton.Create(self);
  btnClose.Text := 'Close';
  btnClose.CanExpandWidth := True;
  btnClose.OnClick := @btnCloseClick;
  rightLayout.InsertChild(btnFindNext);
  rightLayout.InsertChild(btnClose);
  
  mainLayout := TBoxLayout.Create(self);
  mainLayout.InsertChild(leftLayout);
  mainLayout.InsertChild(rightLayout);
  
  Child := mainLayout;
end;
  
{
QHBoxLayout *topLeftLayout = new QHBoxLayout;
topLeftLayout->addWidget(label);
topLeftLayout->addWidget(lineEdit);

QVBoxLayout *leftLayout = new QVBoxLayout;
leftLayout->addLayout(topLeftLayout);
leftLayout->addWidget(buttonGroup);
leftLayout->addWidget(caseCheckBox);

QVBoxLayout *rightLayout = new QVBoxLayout;
rightLayout->addWidget(findNextButton);
rightLayout->addWidget(closeButton);
rightLayout->addStretch(1);

QHBoxLayout *mainLayout = new QHBoxLayout(this);
mainLayout->setMargin(11);
mainLayout->setSpacing(6);
mainLayout->addLayout(leftLayout);
mainLayout->addLayout(rightLayout);
}


var
  GridForm: TGridForm;
  FindDialog: TFindDialog;

{ TGridForm }

constructor TGridForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Text := 'Grid Layout';
  BorderWidth := 8;
  self.BorderWidth := 11;

  Layout := TGridLayout.Create(Self);
    Layout.Name := 'Layout';
    Layout.RowCount := 3;
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
  Child := Layout;
end;


{ TMainForm }

procedure TMainForm.Button3Clicked(Sender: TObject);
begin
  Button.SetBounds(100, 50, 100, 45);
  Redraw;
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TMainForm.btnGridFormClick(Sender: TObject);
begin
  if not Assigned(GridForm) then
  begin
    GridForm := TGridForm.Create(self);
//    Application.AddForm(GridForm);
  end;

    GridForm.Show;
end;


procedure TMainForm.btnCompOptClick(Sender: TObject);
begin
  if not Assigned(CompOpt) then
    Application.CreateForm(TCompilerOptForm, CompOpt)
  else
    CompOpt.Show;
end;


procedure TMainForm.btnTopClick(Sender: TObject);
begin
  if not Assigned(FindDialog) then
    FindDialog := TFindDialog.Create(self);
  FindDialog.Show;
end;


procedure TMainForm.MainFormActivate(Sender: TObject);
var
  max: TSize;
begin
  max.cx := 600;
  max.cy := 400;
  self.Wnd.SetMinMaxClientSize(MinSize, max);
end;


constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'frmMain';
  BorderWidth := 8;
  WindowType := wtWindow;
  OnActivate := @MainFormActivate;

  HBox := TBoxLayout.Create(self);
  HBox.Name := 'HBox';
  HBox.Parent := self;
  HBox.Spacing := 3;
  HBox.Orientation := Horizontal;

  TextLabel := TLabel.Create(self);
  TextLabel.Name := 'TextLabel';
  TextLabel.Text := 'Hello';
  TextLabel.Parent := HBox;

  Button := TButton.Create(self);
  Button.Parent := HBox;
  Button.Text := 'Close';
  Button.OnClick := @btnCloseClick;

  btnGridForm := TButton.Create(self);
  btnGridForm.Parent := HBox;
  btnGridForm.Text := 'Grid Form';
  btnGridForm.OnClick := @btnGridFormClick;
  
  btnCompOpt := TButton.Create(Self);
    btnCompOpt.Parent := HBox;
    btnCompOpt.Name := 'btnCompOpt';
    btnCompOpt.Text := 'Compiler Options';
    btnCompOpt.OnClick := @btnCompOptClick;
//    btnCompOpt.FCanExpandWidth := True;  // if not used, button is smaller than others

  VBox := TBoxLayout.Create(self);
  VBox.Orientation := Vertical;
    Button1 := TButton.Create(Self);
      Button1.Name := 'Top';
      Button1.Text := 'Top';
      Button1.CanExpandWidth := True;  // if not used, button is smaller than others
      Button1.OnClick := @btnTopClick;
    VBox.InsertChild(Button1);
    Button2 := TButton.Create(Self);
      Button2.Name := 'Centre';
      Button2.Text := 'Centre';
      Button2.CanExpandWidth := True;
    VBox.InsertChild(Button2);
    Button3 := TButton.Create(Self);
      Button3.Name := 'Bottom';
      Button3.Text := 'Bottom (SetBounds)';
      Button3.OnClick :=@Button3Clicked;
    VBox.InsertChild(Button3);

  HBox.InsertChild(VBox);


  Child := HBox;
end;


var
  MainForm: TMainForm;

begin
  MainForm := nil;
  { set application wide style }

  Application.SetStyle(FOpenSoftStyle);

  MainForm := TMainForm.Create(Application);
//  MainForm.SetBounds(Point(100, 300), Size(300,200));
  try
    MainForm.Show;
    Application.Run;
  finally
    MainForm.Free;
  end;

end.

