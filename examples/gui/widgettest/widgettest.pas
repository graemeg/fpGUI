program WidgetTest;

uses
  SysUtils
  ,Classes
  ,fpGFX
  ,fpGUI
  ;

type

  // forward declarations
  TCheckboxForm       = class;
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

  TMainForm = class(TFForm)
  private
    _frmCheckBox: TCheckboxForm;
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
    Box: TFBoxLayout;
    Title: TFLabel;
    CheckboxBtn: TFButton;
    RadioButtonBtn: TFButton;
    GroupBoxBtn: TFButton;
    EditBtn: TFButton;
    ScrollBarBtn: TFButton;
    ScrollBoxBtn: TFButton;
    ListBoxBtn: TFButton;
    ComboBoxBtn: TFButton;
    GridBtn: TFButton;
    MenuBtn: TFButton;
    PanelBtn: TFButton;
    ProgressBarBtn: TFButton;
    ShowMessageBtn: TFButton;
    Separator: TSeparator;
    ExitBtn: TFButton;
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
    procedure ShowMessageBtnClick(Sender: TObject);
  end;


  TTestForm = class(TFForm)
  end;


  TCheckboxForm = class(TTestForm)
    Box: TFBoxLayout;
    GrayCheckBox, CheckBox1, CheckBox2: TFCheckbox;
    procedure GrayCheckBoxClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TRadioButtonForm = class(TTestForm)
    Box, HorzBox, ButtonBox1, ButtonBox2: TFBoxLayout;
    GrayCheckBox: TFCheckbox;
    Radio1a, Radio1b, Radio2a, Radio2b: TFRadioButton;
    procedure GrayCheckBoxClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TGroupBoxForm = class(TTestForm)
    HorzBox, VertBox1, VertBox2: TFBoxLayout;
    GroupBox1, GroupBox2: TFGroupBox;
    GrayCheckBox: TFCheckbox;
    Button: TFButton;
    Radio1, Radio2, Radio3, Radio4, Radio5: TFRadioButton;
    procedure GrayCheckBoxClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure RadioButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;


  { TEditForm }

  TEditForm = class(TTestForm)
    Grid: TFGridLayout;
    VertBox, HorzBox1, HorzBox2: TFBoxLayout;
    Label1, Label2, PasswordDisplay: TFLabel;
    Edit1, Edit2: TFEdit;
    GrayCheckBox1, GrayCheckBox2: TFCheckbox;
    Separator: TSeparator;
    cbBorderStyle: TFButton;
    procedure GrayCheckBox1Click(Sender: TObject);
    procedure GrayCheckBox2Click(Sender: TObject);
    procedure cbBorderStyleClick(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TScrollBarForm = class(TTestForm)
    VertLayout: TFBoxLayout;
    GrayCheckBox: TFCheckbox;
    HorzBox: TFBoxLayout;
    HorzGrid, VertGrid: TFGridLayout;
    VertBar: TSeparator;
    VerTFLabel, Label1, Label2, Label3, Label4, Label5: TFLabel;
    PosLabel1, PosLabel2, PosLabel3, PosLabel4, PosLabel5: TFLabel;
    VerTFScrollBar, ScrollBar1, ScrollBar2, ScrollBar3,
      ScrollBar4, ScrollBar5: TFScrollBar;
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
    VertLayout: TFBoxLayout;
    Label1: TFLabel;
    ScrollBox: TFScrollBox;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TListBoxForm = class(TTestForm)
    ListBox: TFListBox;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TComboBoxForm = class(TTestForm)
  published
    VertLayout: TFBoxLayout;
    GrayCheckBox: TFCheckbox;
    BetaLabel: TFLabel;
    ComboBox1: TFComboBox;
    ComboBox2: TFComboBox;
    procedure GrayCheckBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TGridForm = class(TTestForm)
    StringGrid: TFStringGrid;
    procedure FormCreate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;
  

  { TMenuForm }

  TMenuForm = class(TTestForm)
  private
    procedure   CloseMenuClicked(Sender: TObject);
    procedure   AboutMenuClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    BoxLayout: TFBoxLayout;
    MainMenu: TFMenuBar;
    Title: TFLabel;
    p1, p2, p3, p4, p5, p6: TFPanel;
    MenuBox: TFBoxLayout;
  end;
  
  
  TPanelForm = class(TTestForm)
  private
    procedure   RadioButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    MainLayout: TFBoxLayout;
    StyleGroup: TFGroupBox;
    rbPlain, rbLowered, rbRaised: TFRadioButton;
    VBox1: TFBoxLayout;
    Panel: TFPanel;
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
    MainLayout: TFGridLayout;
    VBox: TFBoxLayout;
    PB: TFProgressBar;
    cbShowPercent: TFCheckbox;
    gbColor: TFGroupBox;
    rbBlue: TFRadioButton;
    rbRed: TFRadioButton;
    rbGreen: TFRadioButton;
    Separator: TSeparator;
    btnRandom: TFButton;
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

procedure TMenuForm.AboutMenuClicked(Sender: TObject);
begin
  writeln('   About menu clicked from <' + Sender.ClassName + '>');
  if Sender is TFMenuItem then
    writeln('   from: ' + TFMenuItem(Sender).Text);
end;

constructor TMenuForm.Create(AOwner: TComponent);
var
  lMenuItem: TFMenuItem;
begin
  inherited Create(AOwner);
  Name := 'MenuForm';
  Text := 'Menu Test';
  
  BoxLayout := TFBoxLayout.Create(self);
  BoxLayout.Orientation := Vertical;
  
  MainMenu := TFMenuBar.Create(self);
  BoxLayout.InsertChild(MainMenu);

  MainMenu.AddMenu('Close', 'C', @CloseMenuClicked);
//  MainMenu.AddMenu('File');
  MainMenu.AddMenu('Edit');
  MainMenu.AddMenu('Options');
  MainMenu.AddMenu('Windows');
  lMenuItem := MainMenu.AddMenu('Help');
  lMenuItem.SubMenu.AddMenu('Online Help');
  lMenuItem.SubMenu.AddMenu('Tutorials');
  lMenuItem.SubMenu.AddMenu('About', '', @AboutMenuClicked);

  Title := TFLabel.Create(self);
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
  case TFRadioButton(Sender).Tag of
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

  MainLayout := TFBoxLayout.Create(self);
  MainLayout.Orientation := Vertical;
  
  StyleGroup := TFGroupBox.Create('Bevel Style:', self);
  StyleGroup.CanExpandWidth := True;
  MainLayout.InsertChild(StyleGroup);
  
  VBox1 := TFBoxLayout.Create(self);
  VBox1.Orientation := Vertical;
  StyleGroup.InsertChild(VBox1);
  
  rbPlain := TFRadioButton.Create('Plain', self);
  rbPlain.Tag := 1;
  rbPlain.OnClick := @RadioButtonClick;
  rbLowered := TFRadioButton.Create('Lowered', self);
  rbLowered.Tag := 2;
  rbLowered.OnClick := @RadioButtonClick;
  rbRaised := TFRadioButton.Create('Raised', self);
  rbRaised.Tag := 3;
  rbRaised.OnClick := @RadioButtonClick;
  rbRaised.Checked := True;
  VBox1.InsertChild(rbPlain);
  VBox1.InsertChild(rbLowered);
  VBox1.InsertChild(rbRaised);
  
  Separator := TSeparator.Create(self);
  MainLayout.InsertChild(Separator);

  Panel := TFPanel.Create('My Panel', self);
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
  case TFRadioButton(Sender).Tag of
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

  MainLayout := TFGridLayout.Create(self);
  MainLayout.RowCount := 4;
  MainLayout.ColCount := 2;
  
  VBox := TFBoxLayout.Create(self);
  VBox.Orientation := Vertical;
  
  gbColor   := TFGroupBox.Create('Fill Color', self);
  rbRed           := TFRadioButton.Create('Red', self);
  rbRed.Tag       := 1;
  rbRed.OnClick   := @RadioButtonClick;
  rbRed.Checked   := True;

  rbGreen         := TFRadioButton.Create('Green', self);
  rbGreen.Tag     := 2;
  rbGreen.OnClick := @RadioButtonClick;

  rbBlue          := TFRadioButton.Create('Blue', self);
  rbBlue.Tag      := 3;
  rbBlue.OnClick  := @RadioButtonClick;

  VBox.InsertChild(rbRed);
  VBox.InsertChild(rbGreen);
  VBox.InsertChild(rbBlue);
  gbColor.InsertChild(VBox);
  MainLayout.AddWidget(gbColor, 0, 0, 1, 2);

  cbShowPercent := TFCheckbox.Create('Show Percentage', self);
  cbShowPercent.Checked := True;
  cbShowPercent.OnClick := @cbShowPercentClick;
  cbShowPercent.CanExpandWidth := True;
  MainLayout.AddWidget(cbShowPercent, 1, 0, 1, 1);
  
  btnRandom := TFButton.Create('Randomize', self);
  btnRandom.OnClick := @GeneratePercentage;
  MainLayout.AddWidget(btnRandom, 1, 1, 1, 1);

  Separator := TSeparator.Create(self);
  MainLayout.AddWidget(Separator, 0, 2, 2, 1);
  
  PB := TFProgressBar.Create('', self);
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
    _frmCheckBox := TCheckboxForm.Create(self);
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
  _frmPanel.ShowModal;
//  _frmPanel.SetPosition(Point(Left + Width + 5, FindForm.Top));
end;

procedure TMainForm.ProgressBarBtnClick(Sender: TObject);
begin
  if not Assigned(_frmProgressBar) then
    _frmProgressBar := TProgressBarForm.Create(self);
  _frmProgressBar.Show;
  _frmProgressBar.SetPosition(Point(Left + Width + 5, FindForm.Top));
end;

procedure TMainForm.ShowMessageBtnClick(Sender: TObject);
begin
  ShowMessage('Hello World!');
end;


// -------------------------------------------------------------------
//   TCheckboxForm
// -------------------------------------------------------------------

procedure TCheckboxForm.GrayCheckBoxClick(Sender: TObject);
begin
  CheckBox1.Enabled := not GrayCheckBox.Checked;
  CheckBox2.Enabled := not GrayCheckBox.Checked;
end;

constructor TCheckboxForm.Create(AOwner: TComponent);
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

procedure TEditForm.cbBorderStyleClick(Sender: TObject);
begin
  if Edit1.BorderStyle = bsNone then
  begin
    Edit1.BorderStyle := bsSingle;
    Edit2.BorderStyle := bsSingle;
  end
  else
  begin
    Edit1.BorderStyle := bsNone;
    Edit2.BorderStyle := bsNone;
  end;
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
      
  StringGrid.Cells[3, 3] := 'This is one long piece of text';
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
