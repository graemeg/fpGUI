{
  A proof of concept demo to see if we could duplicate one of Qt4's demos.
}

program WidgetDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  ,SysUtils
  ,fpGFX
  ,fpGUI
  ,StyleManager
  ;
  
type

  { TWidgetDemoForm }

  TWidgetDemoForm = class(TFForm)
    topLayout: TFBoxLayout;
    mainLayout: TFGridLayout;
    MainMenu: TFMenuBar;

    topStyleComboLayout: TFBoxLayout;
    lblStyle: TFLabel;
    cbStyle: TFComboBox;
    
    topCheckboxLayout: TFBoxLayout;
    chkStdPalette: TFCheckBox;
    chkDisable: TFCheckBox;

    topLeftGroupBox: TFGroupBox;
    topLeftGroupBoxLayout: TFBoxLayout;
    Radio1: TFRadioButton;
    Radio2: TFRadioButton;
    Radio3: TFRadioButton;

    topRightGroupBox: TFGroupBox;
    topRightGroupBoxLayout: TFBoxLayout;
    Button1: TFButton;
    Button2: TFButton;

    StringGrid: TFStringGrid;

    bottomRightGroupBox: TFGroupBox;
    bottomRightGroupBoxLayout: TFBoxLayout;
    Edit1: TFEdit;
    Edit2: TFEdit;

    bottomButtonLayout: TFBoxLayout;
    btnExit: TFButton;
    btnHelp: TFButton;

    procedure   btnExitClick(Sender: TObject);
    procedure   Radio1Click(Sender: TObject);
    procedure   Radio2Click(Sender: TObject);
    procedure   cbStyleChanged(Sender: TObject);
    procedure   TranslateToAfrikaans;
    procedure   TranslateToEnglish;
    procedure   chkDisableClick(Sender: TObject);
  private
    procedure   CreateTopMenu;
    procedure   CreateStyleCombo;
    procedure   CreateTopCheckboxLine;
    procedure   CreateTopLeftGroupBox;
    procedure   CreateTopRightGroupBox;
    procedure   CreateBottomLeftStringGrid;
    procedure   CreateBottomRightGroupBox;
    procedure   CreateBottomButtonLayout;
  public
    constructor Create(AOwner: TComponent); override;
  end;


{ TWidgetDemoForm }

procedure TWidgetDemoForm.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TWidgetDemoForm.Radio1Click(Sender: TObject);
begin
  TranslateToEnglish;
end;

procedure TWidgetDemoForm.Radio2Click(Sender: TObject);
begin
  TranslateToAfrikaans;
end;

procedure TWidgetDemoForm.cbStyleChanged(Sender: TObject);
begin
  { I want to try something later with this }
//  gStyleManager.SetStyle(cbStyle.Text);
  if cbStyle.Text <> cDefaultStyle then
    Style.Free;
  Style := gStyleManager.CreateInstance(cbStyle.Text);
  Redraw;
end;

procedure TWidgetDemoForm.TranslateToAfrikaans;
begin
  Text                    := 'Widget Demo - Afrikaans';
  lblStyle.Text           := 'Venster Steil:';
  chkStdPalette.Text      := 'Standaard kleur tablette';
  chkDisable.Text         := 'Ge-deaktiveerde controles';
  topLeftGroupBox.Text    := 'Groep Boks Een';
  Radio1.Text             := 'Radio knoppie een';
  Radio2.Text             := 'Radio knoppie twee';
  Radio3.Text             := 'Radio knoppie drie';
  topLeftGroupBox.Text    := 'Groep Boks Twee';
  Button1.Text            := 'Normal Button';
  Button2.Text            := 'Embedded Button';
  bottomRightGroupBox.Text := 'Group Boks Drie';
  Edit1.Text              := 'Normale teks';
  Edit2.Text              := 'Wagwoord teks';
  btnExit.Text            := 'Verlaat Verlaat';
  btnHelp.Text            := 'Hulp';
  Update;
  Redraw;
end;

procedure TWidgetDemoForm.TranslateToEnglish;
begin
  Text                    := 'Widget Demo - English';
  lblStyle.Text           := 'Style:';
  chkStdPalette.Text      := 'Standard color palette';
  chkDisable.Text         := 'Disable widgets';
  topLeftGroupBox.Text    := 'Group Box 1';
  Radio1.Text             := 'Radio button 1';
  Radio2.Text             := 'Radio button 2';
  Radio3.Text             := 'Radio button 3';
  topRightGroupBox.Text   := 'Group Box 2';
  Button1.Text            := 'Normal Button';
  Button2.Text            := 'Embedded Button';
  bottomRightGroupBox.Text := 'Group Box 3';
  Edit1.Text              := 'Normal Edit';
  Edit2.Text              := 'Password Edit';
  btnExit.Text            := 'Exit';
  btnHelp.Text            := 'Help';
  Update;
  Redraw;
end;

procedure TWidgetDemoForm.chkDisableClick(Sender: TObject);
begin
  lblStyle.Enabled            := not chkDisable.Checked;
  cbStyle.Enabled             := not chkDisable.Checked;
  chkStdPalette.Enabled       := not chkDisable.Checked;
  topLeftGroupBox.Enabled     := not chkDisable.Checked;
  topRightGroupBox.Enabled    := not chkDisable.Checked;
  StringGrid.Enabled          := not chkDisable.Checked;
  bottomRightGroupBox.Enabled := not chkDisable.Checked;
end;

procedure TWidgetDemoForm.CreateTopMenu;
var
  mi: TFMenuItem;
begin
  MainMenu := TFMenuBar.Create(self);
  mi := MainMenu.AddMenu('File');
  mi.SubMenu.AddMenu('Exit', '', @btnExitClick);
  MainMenu.AddMenu('Edit');
  MainMenu.AddMenu('Options');
  MainMenu.AddMenu('Windows');
  MainMenu.AddMenu('Help');
//  MainMenu.CanExpandWidth := True;
end;

procedure TWidgetDemoForm.CreateStyleCombo;
begin
  topStyleComboLayout := TFBoxLayout.Create(self);

  lblStyle := TFLabel.Create('Style:', self);

  cbStyle := TFComboBox.Create(self);
  cbStyle.CanExpandWidth := True;
  gStyleManager.AssignStyleTypes(cbStyle.Items);
  cbStyle.OnChange := @cbStyleChanged;
  cbStyle.ItemIndex := 0;

  topStyleComboLayout.InsertChild(lblStyle);
  topStyleComboLayout.InsertChild(cbStyle);
end;

procedure TWidgetDemoForm.CreateTopCheckboxLine;
begin
  topCheckboxLayout := TFBoxLayout.Create(self);

  chkStdPalette := TFCheckBox.Create('Standard color palette', self);
  chkStdPalette.CanExpandWidth  := True;
  chkStdPalette.Checked         := True;

  chkDisable := TFCheckBox.Create('Disable widgets', self);
  chkDisable.OnClick := @chkDisableClick;

  topCheckboxLayout.InsertChild(chkStdPalette);
  topCheckboxLayout.InsertChild(chkDisable);
end;

procedure TWidgetDemoForm.CreateTopLeftGroupBox;
begin
  topLeftGroupBox := TFGroupBox.Create('Group Box 1', self);
  topLeftGroupBox.CanExpandWidth   := True;

  topLeftGroupBoxLayout := TFBoxLayout.Create(self);
  topLeftGroupBoxLayout.Orientation := Vertical;

  Radio1 := TFRadioButton.Create('Radio button 1', self);
  Radio1.Checked          := True;
  Radio1.CanExpandWidth   := True;
  Radio1.OnClick          := @Radio1Click;

  Radio2 := TFRadioButton.Create('Radio button 2', self);
  Radio2.CanExpandWidth   := True;
  Radio2.OnClick          := @Radio2Click;

  Radio3 := TFRadioButton.Create('Radio button 3', self);
  Radio3.CanExpandWidth   := True;

  topLeftGroupBox.InsertChild(topLeftGroupBoxLayout);
  topLeftGroupBoxLayout.InsertChild(Radio1);
  topLeftGroupBoxLayout.InsertChild(Radio2);
  topLeftGroupBoxLayout.InsertChild(Radio3);
end;

procedure TWidgetDemoForm.CreateTopRightGroupBox;
begin
  topRightGroupBox := TFGroupBox.Create('Group Box 2', self);
  topRightGroupBox.CanExpandWidth     := True;
  topRightGroupBox.CanExpandHeight    := True;

  topRightGroupBoxLayout := TFBoxLayout.Create(self);
  topRightGroupBoxLayout.Orientation  := Vertical;
  topRightGroupBoxLayout.VertAlign    := vertCenter;
  topRightGroupBoxLayout.Spacing      := 8;

  Button1 := TFButton.Create('Normal Button', self);
  Button1.CanExpandWidth := True;

  Button2 := TFButton.Create('Embedded Button', self);
  Button2.CanExpandWidth := True;
  Button2.Embedded := True;

  topRightGroupBox.InsertChild(topRightGroupBoxLayout);
  topRightGroupBoxLayout.InsertChild(Button1);
  topRightGroupBoxLayout.InsertChild(Button2);
end;

procedure TWidgetDemoForm.CreateBottomLeftStringGrid;
var
  x, y: integer;
begin
  StringGrid := TFStringGrid.Create(self);
  StringGrid.ColCount := 10;
  StringGrid.RowCount := 15;
  for y := 0 to StringGrid.RowCount - 1 do
    for x := 0 to StringGrid.ColCount - 1 do
      StringGrid.Cells[x, y] := Format('%d, %d', [x, y]);
end;

procedure TWidgetDemoForm.CreateBottomRightGroupBox;
begin
  bottomRightGroupBox := TFGroupBox.Create('Group Box 3', self);
  bottomRightGroupBox.CanExpandHeight := True;
  bottomRightGroupBox.CanExpandWidth := True;

  bottomRightGroupBoxLayout := TFBoxLayout.Create(self);
  bottomRightGroupBoxLayout.Orientation := Vertical;

  Edit1 := TFEdit.Create('Normal Edit', self);
  Edit2 := TFEdit.Create('Password Edit', self);
  Edit2.PasswordChar := '*';

  bottomRightGroupBox.InsertChild(bottomRightGroupBoxLayout);
  bottomRightGroupBoxLayout.InsertChild(Edit1);
  bottomRightGroupBoxLayout.InsertChild(Edit2);
end;

procedure TWidgetDemoForm.CreateBottomButtonLayout;
begin
  bottomButtonLayout := TFBoxLayout.Create(self);
  bottomButtonLayout.HorzAlign := horzRight;

  btnHelp := TFButton.Create('Help', self);

  btnExit := TFButton.Create('Exit', self);
  btnExit.OnClick := @btnExitClick;

  bottomButtonLayout.InsertChild(btnHelp);
  bottomButtonLayout.InsertChild(btnExit);
end;

constructor TWidgetDemoForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text          := 'Widget Demo';
  BorderWidth   := 8;

  topLayout := TFBoxLayout.Create(self);
  topLayout.Orientation     := Vertical;
  mainLayout := TFGridLayout.Create(self);
  mainLayout.RowCount       := 2;
//  mainlayout.RowSpacing := 0;
  
  CreateTopMenu;
  topLayout.InsertChild(MainMenu);

  CreateStyleCombo;
  topLayout.InsertChild(topStyleComboLayout);

  CreateTopCheckboxLine;
  topLayout.InsertChild(topCheckboxLayout);
  
  CreateTopLeftGroupBox;
  mainLayout.AddWidget(topLeftGroupBox, 0, 0, 1, 1);

  CreateTopRightGroupBox;
  mainLayout.AddWidget(topRightGroupBox, 1, 0, 1, 1);

  CreateBottomLeftStringGrid;
  mainLayout.AddWidget(StringGrid, 0, 1, 1, 1);

  CreateBottomRightGroupBox;
  mainLayout.AddWidget(bottomRightGroupBox, 1, 1, 1, 1);

  topLayout.InsertChild(mainLayout);

  CreateBottomButtonLayout;
  topLayout.InsertChild(bottomButtonLayout);

  Child := topLayout;
end;
  
  
var
  WidgetDemoForm: TWidgetDemoForm;
  
begin
  GFApplication.Initialize;

  WidgetDemoForm := TWidgetDemoForm.Create(nil);
  try
    WidgetDemoForm.Show;
    GFApplication.Run;
  finally
    WidgetDemoForm.Free;
  end;
end.

