program WidgetTest;

uses
  SysUtils
  ,Classes
  ,fpGFX
  ,fpGUI
  ;

type

  // forward declarations
  TCheckBoxForm       = class;
  TRadioButtonForm    = class;
  TGroupBoxForm       = class;
  TEditForm           = class;
  TScrollBarForm      = class;
  TScrollBoxForm      = class;
  TListBoxForm        = class;
  TComboBoxForm       = class;
  TGridForm           = class;
  TMenuForm           = class;
  TPanelForm          = class;
  TProgressBarForm    = class;

  { TMainForm }

  TMainForm = class(TForm)
  private
    _frmCheckBox: TCheckBoxForm;
    _frmRadioButton: TRadioButtonForm;
    _frmGroupBox: TGroupBoxForm;
    _frmEdit: TEditForm;
    _frmScrollBar: TScrollBarForm;
    _frmScrollBox: TScrollBoxForm;
    _frmListBox: TListBoxForm;
    _frmComboBox: TComboBoxForm;
    _frmGrid: TGridForm;
    _frmMenu: TMenuForm;
    _frmPanel: TPanelForm;
    _frmProgressBar: TProgressBarForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    Box: TBoxLayout;
    Title: TLabel;
    CheckboxBtn: TButton;
    RadioButtonBtn: TButton;
    GroupBoxBtn: TButton;
    EditBtn: TButton;
    ScrollBarBtn: TButton;
    ScrollBoxBtn: TButton;
    ListBoxBtn: TButton;
    ComboBoxBtn: TButton;
    GridBtn: TButton;
    MenuBtn: TButton;
    PanelBtn: TButton;
    ProgressBarBtn: TButton;
    Separator: TSeparator;
    ExitBtn: TButton;
    procedure CheckBoxBtnClick(Sender: TObject);
    procedure RadioButtonBtnClick(Sender: TObject);
    procedure GroupBoxBtnClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure ScrollBarBtnClick(Sender: TObject);
    procedure ScrollBoxBtnClick(Sender: TObject);
    procedure ListBoxBtnClick(Sender: TObject);
    procedure ComboBoxBtnClick(Sender: TObject);
    procedure GridBtnClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure MenuBtnClick(Sender: TObject);
    procedure PanelBtnClick(Sender: TObject);
    procedure ProgressBarBtnClick(Sender: TObject);
  end;


  TTestForm = class(TForm)
  end;


  TCheckBoxForm = class(TTestForm)
    Box: TBoxLayout;
    GrayCheckBox, CheckBox1, CheckBox2: TCheckBox;
    procedure GrayCheckBoxClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TRadioButtonForm = class(TTestForm)
    Box, HorzBox, ButtonBox1, ButtonBox2: TBoxLayout;
    GrayCheckBox: TCheckBox;
    Radio1a, Radio1b, Radio2a, Radio2b: TRadioButton;
    procedure GrayCheckBoxClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TGroupBoxForm = class(TTestForm)
    HorzBox, VertBox1, VertBox2: TBoxLayout;
    GroupBox1, GroupBox2: TGroupBox;
    GrayCheckBox: TCheckBox;
    Button: TButton;
    Radio1, Radio2, Radio3, Radio4, Radio5: TRadioButton;
    procedure GrayCheckBoxClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure RadioButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TEditForm = class(TTestForm)
    Grid: TGridLayout;
    VertBox, HorzBox1, HorzBox2: TBoxLayout;
    Label1, Label2, PasswordDisplay: TLabel;
    Edit1, Edit2: TEdit;
    GrayCheckBox1, GrayCheckBox2: TCheckBox;
    Separator: TSeparator;
    procedure GrayCheckBox1Click(Sender: TObject);
    procedure GrayCheckBox2Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TScrollBarForm = class(TTestForm)
    VertLayout: TBoxLayout;
    GrayCheckBox: TCheckBox;
    HorzBox: TBoxLayout;
    HorzGrid, VertGrid: TGridLayout;
    VertBar: TSeparator;
    VertLabel, Label1, Label2, Label3, Label4, Label5: TLabel;
    PosLabel1, PosLabel2, PosLabel3, PosLabel4, PosLabel5: TLabel;
    VertScrollBar, ScrollBar1, ScrollBar2, ScrollBar3,
      ScrollBar4, ScrollBar5: TScrollBar;
    procedure GrayCheckBoxClick(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure ScrollBar2Change(Sender: TObject);
    procedure ScrollBar3Change(Sender: TObject);
    procedure ScrollBar4Change(Sender: TObject);
    procedure ScrollBar5Change(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TScrollBoxForm = class(TTestForm)
    VertLayout: TBoxLayout;
    Label1: TLabel;
    ScrollBox: TScrollBox;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TListBoxForm = class(TTestForm)
    ListBox: TListBox;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TComboBoxForm = class(TTestForm)
  published
    VertLayout: TBoxLayout;
    GrayCheckBox: TCheckBox;
    BetaLabel: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    procedure GrayCheckBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TGridForm = class(TTestForm)
    StringGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;
  

  { TMenuForm }

  TMenuForm = class(TTestForm)
  private
    procedure   CloseMenuClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    BoxLayout: TBoxLayout;
    MainMenu: TMenuBar;
    Title: TLabel;
    p1, p2, p3, p4, p5, p6: TPanel;
    MenuBox: TBoxLayout;
  end;
  
  
  TPanelForm = class(TTestForm)
  private
    procedure   RadioButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    MainLayout: TBoxLayout;
    StyleGroup: TGroupBox;
    rbPlain, rbLowered, rbRaised: TRadioButton;
    VBox1: TBoxLayout;
    Panel: TPanel;
    Separator: TSeparator;
  end;
  

  TProgressBarForm = class(TTestForm)
  private
    procedure   cbShowPercentClick(Sender: TObject);
    procedure   RadioButtonClick(Sender: TObject);
    procedure   GeneratePercentage(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    MainLayout: TGridLayout;
    VBox: TBoxLayout;
    PB: TProgressBar;
    cbShowPercent: TCheckBox;
    gbColor: TGroupBox;
    rbBlue: TRadioButton;
    rbRed: TRadioButton;
    rbGreen: TRadioButton;
    Separator: TSeparator;
    btnRandom: TButton;
  end;

{ TListBoxForm }

constructor TListBoxForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoadForm(self);
end;

{ TScrollBoxForm }

constructor TScrollBoxForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoadForm(self);
end;


{ TMenuForm }

procedure TMenuForm.CloseMenuClicked(Sender: TObject);
begin
  writeln('...Close menu clicked');
  Close;
end;

constructor TMenuForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'MenuForm';
  Text := 'Menu Test';
  
  BoxLayout := TBoxLayout.Create(self);
  BoxLayout.Orientation := Vertical;
  
  MainMenu := TMenuBar.Create(self);
  BoxLayout.InsertChild(MainMenu);

  MainMenu.AddMenu('Close', 'C', @CloseMenuClicked);
//  MainMenu.AddMenu('File');
  MainMenu.AddMenu('Edit');
  MainMenu.AddMenu('Options');
  MainMenu.AddMenu('Windows');
  MainMenu.AddMenu('Help');

  Title := TLabel.Create(self);
  Title.CanExpandWidth := True;
  Title.Alignment := taCenter;
  Title.Text := 'This is work in progress...';
  Title.FontColor := clBlue;
  BoxLayout.InsertChild(Title);

  Child := BoxLayout;
end;


{ TPanelForm }

procedure TPanelForm.RadioButtonClick(Sender: TObject);
begin
  case TRadioButton(Sender).Tag of
    1: Panel.BevelStyle := bsPlain;
    2: Panel.BevelStyle := bsLowered;
    3: Panel.BevelStyle := bsRaised;
  end;
end;

constructor TPanelForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'PanelForm';
  Text := 'Panel Test';
  BorderWidth := 8;

  MainLayout := TBoxLayout.Create(self);
  MainLayout.Orientation := Vertical;
  
  StyleGroup := TGroupBox.Create('Bevel Style:', self);
  StyleGroup.CanExpandWidth := True;
  MainLayout.InsertChild(StyleGroup);
  
  VBox1 := TBoxLayout.Create(self);
  VBox1.Orientation := Vertical;
  StyleGroup.InsertChild(VBox1);
  
  rbPlain := TRadioButton.Create('Plain', self);
  rbPlain.Tag := 1;
  rbPlain.OnClick := @RadioButtonClick;
  rbLowered := TRadioButton.Create('Lowered', self);
  rbLowered.Tag := 2;
  rbLowered.OnClick := @RadioButtonClick;
  rbRaised := TRadioButton.Create('Raised', self);
  rbRaised.Tag := 3;
  rbRaised.OnClick := @RadioButtonClick;
  rbRaised.Checked := True;
  VBox1.InsertChild(rbPlain);
  VBox1.InsertChild(rbLowered);
  VBox1.InsertChild(rbRaised);
  
  Separator := TSeparator.Create(self);
  MainLayout.InsertChild(Separator);

  Panel := TPanel.Create('My Panel', self);
  MainLayout.InsertChild(Panel);
  
  Child := MainLayout;
end;

destructor TPanelForm.Destroy;
begin
  inherited Destroy;
end;


{ TProgressBarForm }

procedure TProgressBarForm.cbShowPercentClick(Sender: TObject);
begin
  PB.ShowPercentage := cbShowPercent.Checked;
end;

procedure TProgressBarForm.RadioButtonClick(Sender: TObject);
begin
  case TRadioButton(Sender).Tag of
    1: PB.FillColor := clRed;
    2: PB.FillColor := clGreen;
    3: PB.FillColor := clBlue;
  end;
end;

procedure TProgressBarForm.GeneratePercentage(Sender: TObject);
begin
  PB.Position := Random(100);
end;

constructor TProgressBarForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text := 'Progress Bar Demo';
  BorderWidth := 8;

  MainLayout := TGridLayout.Create(self);
  MainLayout.RowCount := 4;
  MainLayout.ColCount := 2;
  
  VBox := TBoxLayout.Create(self);
  VBox.Orientation := Vertical;
  
  gbColor   := TGroupBox.Create('Fill Color', self);
  rbRed           := TRadioButton.Create('Red', self);
  rbRed.Tag       := 1;
  rbRed.OnClick   := @RadioButtonClick;
  rbRed.Checked   := True;

  rbGreen         := TRadioButton.Create('Green', self);
  rbGreen.Tag     := 2;
  rbGreen.OnClick := @RadioButtonClick;

  rbBlue          := TRadioButton.Create('Blue', self);
  rbBlue.Tag      := 3;
  rbBlue.OnClick  := @RadioButtonClick;

  VBox.InsertChild(rbRed);
  VBox.InsertChild(rbGreen);
  VBox.InsertChild(rbBlue);
  gbColor.InsertChild(VBox);
  MainLayout.AddWidget(gbColor, 0, 0, 1, 2);

  cbShowPercent := TCheckbox.Create('Show Percentage', self);
  cbShowPercent.Checked := True;
  cbShowPercent.OnClick := @cbShowPercentClick;
  cbShowPercent.CanExpandWidth := True;
  MainLayout.AddWidget(cbShowPercent, 1, 0, 1, 1);
  
  btnRandom := TButton.Create('Randomize', self);
  btnRandom.OnClick := @GeneratePercentage;
  MainLayout.AddWidget(btnRandom, 1, 1, 1, 1);

  Separator := TSeparator.Create(self);
  MainLayout.AddWidget(Separator, 0, 2, 2, 1);
  
  PB := TProgressBar.Create('', self);
  PB.Position := 75;
  MainLayout.AddWidget(PB, 0, 3, 2, 1);
  
  Child := MainLayout;
end;

destructor TProgressBarForm.Destroy;
begin
  inherited Destroy;
end;


// -------------------------------------------------------------------
//   TMainForm
// -------------------------------------------------------------------

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoadForm(self);
end;

destructor TMainForm.Destroy;
begin
  _frmCheckBox.Free;
  _frmRadioButton.Free;
  _frmGroupBox.Free;
  _frmEdit.Free;
  _frmScrollBar.Free;
  _frmScrollBox.Free;
  _frmListBox.Free;
  _frmComboBox.Free;
  _frmGrid.Free;
  _frmMenu.Free;
  _frmPanel.Free;
  _frmProgressBar.Free;
  inherited Destroy;
end;


procedure TMainForm.CheckBoxBtnClick(Sender: TObject);
begin
  if not Assigned(_frmCheckBox) then
    _frmCheckBox := TCheckBoxForm.Create(self);
  _frmCheckBox.Show;
end;


procedure TMainForm.RadioButtonBtnClick(Sender: TObject);
begin
  if not Assigned(_frmRadioButton) then
    _frmRadioButton := TRadioButtonForm.Create(self);
  _frmRadioButton.Show;
end;


procedure TMainForm.GroupBoxBtnClick(Sender: TObject);
begin
  if not Assigned(_frmGroupBox) then
    _frmGroupBox := TGroupBoxForm.Create(self);
  _frmGroupBox.Show;
end;


procedure TMainForm.EditBtnClick(Sender: TObject);
begin
  if not Assigned(_frmEdit) then
    _frmEdit := TEditForm.Create(self);
  _frmEdit.Show;
end;


procedure TMainForm.ScrollBarBtnClick(Sender: TObject);
begin
  if not Assigned(_frmScrollBar) then
    _frmScrollBar := TScrollBarForm.Create(self);
  _frmScrollBar.Show;
end;


procedure TMainForm.ScrollBoxBtnClick(Sender: TObject);
begin
  if not Assigned(_frmScrollBox) then
    _frmScrollBox := TScrollBoxForm.Create(self);
  _frmScrollBox.Show;
end;


procedure TMainForm.ListBoxBtnClick(Sender: TObject);
begin
  if not Assigned(_frmListBox) then
    _frmListBox := TListBoxForm.Create(self);
  _frmListBox.Show;
end;


procedure TMainForm.ComboBoxBtnClick(Sender: TObject);
begin
  if not Assigned(_frmComboBox) then
    _frmComboBox := TComboBoxForm.Create(self);
  _frmComboBox.Show;
end;


procedure TMainForm.GridBtnClick(Sender: TObject);
begin
  if not Assigned(_frmGrid) then
    _frmGrid := TGridForm.Create(self);
  _frmGrid.Show;
end;


procedure TMainForm.ExitBtnClick(Sender: TObject);
begin
  Close;
end;


procedure TMainForm.MenuBtnClick(Sender: TObject);
begin
  if not Assigned(_frmMenu) then
    _frmMenu := TMenuForm.Create(self);
  _frmMenu.Show;
  _frmMenu.SetPosition(Point(Left + Width + 5, FindForm.Top));
end;

procedure TMainForm.PanelBtnClick(Sender: TObject);
begin
  if not Assigned(_frmPanel) then
    _frmPanel := TPanelForm.Create(self);
  _frmPanel.Show;
  _frmPanel.SetPosition(Point(Left + Width + 5, FindForm.Top));
end;

procedure TMainForm.ProgressBarBtnClick(Sender: TObject);
begin
  if not Assigned(_frmProgressBar) then
    _frmProgressBar := TProgressBarForm.Create(self);
  _frmProgressBar.Show;
  _frmProgressBar.SetPosition(Point(Left + Width + 5, FindForm.Top));
end;


// -------------------------------------------------------------------
//   TCheckBoxForm
// -------------------------------------------------------------------

procedure TCheckBoxForm.GrayCheckBoxClick(Sender: TObject);
begin
  CheckBox1.Enabled := not GrayCheckBox.Checked;
  CheckBox2.Enabled := not GrayCheckBox.Checked;
end;

constructor TCheckBoxForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoadForm(self);
end;


// -------------------------------------------------------------------
//   TRadioButtonForm
// -------------------------------------------------------------------

procedure TRadioButtonForm.GrayCheckBoxClick(Sender: TObject);
begin
  HorzBox.Enabled := not GrayCheckBox.Checked;
end;

constructor TRadioButtonForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoadForm(self);
end;


// -------------------------------------------------------------------
//   TGroupBoxForm
// -------------------------------------------------------------------

procedure TGroupBoxForm.GrayCheckBoxClick(Sender: TObject);
begin
  GroupBox2.Enabled := not GrayCheckBox.Checked;
end;


procedure TGroupBoxForm.ButtonClick(Sender: TObject);
begin
  Radio1.Checked := True;
  Button.Enabled := False;
end;


procedure TGroupBoxForm.RadioButtonClick(Sender: TObject);
begin
  Button.Enabled := not Radio1.Checked;
end;

constructor TGroupBoxForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoadForm(self);
end;


// -------------------------------------------------------------------
//   TEditForm
// -------------------------------------------------------------------

procedure TEditForm.GrayCheckBox1Click(Sender: TObject);
begin
  Edit1.Enabled := not GrayCheckBox1.Checked;
end;


procedure TEditForm.GrayCheckBox2Click(Sender: TObject);
begin
  Edit2.Enabled := not GrayCheckBox2.Checked;
end;


procedure TEditForm.Edit2Change(Sender: TObject);
begin
//  PasswordDisplay.Text := '(= ' + Edit2.Text + ')';
end;

constructor TEditForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoadForm(self);
end;


// -------------------------------------------------------------------
//   TScrollBarForm
// -------------------------------------------------------------------

procedure TScrollBarForm.GrayCheckBoxClick(Sender: TObject);
begin
  HorzBox.Enabled := not GrayCheckBox.Checked;
end;


procedure TScrollBarForm.ScrollBar1Change(Sender: TObject);
begin
  PosLabel1.Text := IntToStr(ScrollBar1.Position);
end;


procedure TScrollBarForm.ScrollBar2Change(Sender: TObject);
begin
  PosLabel2.Text := IntToStr(ScrollBar2.Position);
end;


procedure TScrollBarForm.ScrollBar3Change(Sender: TObject);
begin
  PosLabel3.Text := IntToStr(ScrollBar3.Position);
end;


procedure TScrollBarForm.ScrollBar4Change(Sender: TObject);
begin
  PosLabel4.Text := IntToStr(ScrollBar4.Position);
end;


procedure TScrollBarForm.ScrollBar5Change(Sender: TObject);
begin
  PosLabel5.Text := IntToStr(ScrollBar5.Position);
end;

constructor TScrollBarForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoadForm(self);
end;


// -------------------------------------------------------------------
//   TComboBoxForm
// -------------------------------------------------------------------

procedure TComboBoxForm.GrayCheckBoxClick(Sender: TObject);
begin
  ComboBox1.Enabled := not GrayCheckBox.Checked;
  ComboBox2.Enabled := not GrayCheckBox.Checked;
end;


procedure TComboBoxForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to 20 do
  begin
    ComboBox1.Items.Add(Format('Item 1.%d...', [i]));
    ComboBox2.Items.Add(Format('Item 2.%d...', [i]));
  end;
  ComboBox2.Items.Add('A long item that should cause a horizontal scrollbar to appear.');
  BetaLabel.FontColor := clBlue;
end;

constructor TComboBoxForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoadForm(self);
end;


// -------------------------------------------------------------------
//   TGridForm
// -------------------------------------------------------------------

procedure TGridForm.FormCreate(Sender: TObject);
var
  x, y: Integer;
begin
  for y := 0 to StringGrid.RowCount - 1 do
    for x := 0 to StringGrid.ColCount - 1 do
      StringGrid.Cells[x, y] := Format('%d, %d', [x, y]);
end;

constructor TGridForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoadForm(self);
end;


var
  MainForm: TMainForm;
begin
  WriteLn('Version: ' + {$I %date%} + ' ' + {$I %time%});
  GFApplication.Initialize;
  
  MainForm := TMainForm.Create(GFApplication);
  try
    MainForm.Show;
    GFApplication.Run;
  finally
    MainForm.Free;
  end;
end.
