program hello;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpGUI, fpGFX, gfxbase, frmCompilerOpt, StyleManager;

type

  { TMainForm }

  TMainForm = class(TFForm)
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
    HBox: TFBoxLayout;
    VBox: TFBoxLayout;
    TextLabel: TFLabel;
    Button: TFButton;
    btnGridForm: TFButton;
    btnCompOpt: TFButton;
    Button1: TFButton;
    Button2: TFButton;
    Button3: TFButton;
  end;
  
  
  { TGridForm }

  TGridForm = Class(TFForm)
    Layout: TFGridLayout;
    Button1,Button2,Button3,Button4,Button5: TFButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
  
  { TFindDialog }

  TFindDialog = class(TFForm)
  private
    procedure btnCloseClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    lblFind: TFLabel;
    btnFindNext, btnClose: TFButton;
    edFind: TFEdit;
    GroupBox1: TFGroupBox;
    grpBox1Layout: TFBoxLayout;
    Radio1, Radio2: TFRadioButton;
    cbCase: TFCheckBox;
    leftLayout, rightLayout: TFBoxLayout;
    topLeftLayout: TFBoxLayout;
    mainLayout: TFBoxLayout;
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

  topLeftLayout := TFBoxLayout.Create(self);
  topLeftLayout.CanExpandWidth := True;
    lblFind := TFLabel.Create(self);
    lblFind.Text := 'Find what:';
    edFind := TFEdit.Create(self);
  topLeftLayout.InsertChild(lblFind);
  topLeftLayout.InsertChild(edFind);
  
  leftLayout := TFBoxLayout.Create(self);
  leftLayout.Orientation := Vertical;
  leftLayout.CanExpandWidth := True;

  GroupBox1 := TFGroupBox.Create(self);
  GroupBox1.Text := 'Direction';
  GroupBox1.CanExpandWidth := True;
    grpBox1Layout := TFBoxLayout.Create(self);
      Radio1 := TFRadioButton.Create(self);
      Radio1.Text := 'Up';
      Radio1.CanExpandWidth := True;
      Radio2 := TFRadioButton.Create(self);
      Radio2.Text := 'Down';
      Radio2.CanExpandWidth := True;
  GroupBox1.InsertChild(grpBox1Layout);
  grpBox1Layout.InsertChild(Radio1);
  grpBox1Layout.InsertChild(Radio2);

  cbCase := TFCheckBox.Create(self);
  cbCase.Text := 'Case sensitive';
  cbCase.CanExpandWidth := True;

  leftLayout.InsertChild(topLeftLayout);
  leftLayout.InsertChild(GroupBox1);
  leftLayout.InsertChild(cbCase);
  
  rightLayout := TFBoxLayout.Create(self);
  rightLayout.Orientation := Vertical;
  rightLayout.VertAlign := vertTop;
    btnFindNext := TFButton.Create(self);
    btnFindNext.Text := 'Find Next';
    btnClose := TFButton.Create(self);
    btnClose.Text := 'Close';
    btnClose.CanExpandWidth := True;
    btnClose.OnClick := @btnCloseClick;
  rightLayout.InsertChild(btnFindNext);
  rightLayout.InsertChild(btnClose);
  
  mainLayout := TFBoxLayout.Create(self);
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

  Layout := TFGridLayout.Create(Self);
    Layout.Name := 'Layout';
    Layout.RowCount := 3;
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
  begin
    CompOpt := TCompilerOptForm.Create(GFApplication);
    LoadForm(CompOpt);
    CompOpt.Style := gStyleManager.CreateInstance('OpenSoft');
    CompOpt.Show;
  end
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
  OnActivate := @MainFormActivate;

  HBox := TFBoxLayout.Create(self);
  HBox.Name         := 'HBox';
  HBox.Parent       := self;
  HBox.Spacing      := 3;
  HBox.Orientation  := Horizontal;

  TextLabel := TFLabel.Create(self);
  TextLabel.Name    := 'TextLabel';
  TextLabel.Text    := 'Hello';
  TextLabel.Parent  := HBox;

  Button := TFButton.Create(self);
  Button.Parent     := HBox;
  Button.Text       := 'Close';
  Button.OnClick    := @btnCloseClick;

  btnGridForm := TFButton.Create(self);
  btnGridForm.Parent    := HBox;
  btnGridForm.Text      := 'Grid Form';
  btnGridForm.OnClick   := @btnGridFormClick;
  
  btnCompOpt := TFButton.Create(Self);
    btnCompOpt.Parent   := HBox;
    btnCompOpt.Name     := 'btnCompOpt';
    btnCompOpt.Text     := 'Compiler Options';
    btnCompOpt.OnClick  := @btnCompOptClick;

  VBox := TFBoxLayout.Create(self);
  VBox.Orientation            := Vertical;
    Button1 := TFButton.Create(Self);
      Button1.Name            := 'Top';
      Button1.Text            := 'Top';
      Button1.CanExpandWidth  := True;  // if not used, button is smaller than others
      Button1.OnClick         := @btnTopClick;
    VBox.InsertChild(Button1);
    Button2 := TFButton.Create(Self);
      Button2.Name            := 'Centre';
      Button2.Text            := 'Centre';
      Button2.CanExpandWidth  := True;
    VBox.InsertChild(Button2);
    Button3 := TFButton.Create(Self);
      Button3.Name            := 'Bottom';
      Button3.Text            := 'Bottom (SetBounds)';
      Button3.OnClick         := @Button3Clicked;
    VBox.InsertChild(Button3);

  HBox.InsertChild(VBox);

  Child := HBox;
end;


var
  MainForm: TMainForm;

begin
  MainForm := nil;

  { set application wide style }
  GFApplication.Initialize;
//  gStyleManager.CreateInstance('OpenSoft');

  MainForm := TMainForm.Create(GFApplication);
  try
    MainForm.Style := gStyleManager.CreateInstance('OpenSoft');
    MainForm.Show;
    GFApplication.Run;
  finally
    MainForm.Free;
  end;
end.


