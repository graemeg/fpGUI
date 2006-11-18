program WidgetDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  ,SysUtils
  ,fpgui
  ,OpenSoftStyle
  ,gfxbase
  ;
  
type

  { TWidgetDemoForm }

  TWidgetDemoForm = class(TForm)
    topLayout: TBoxLayout;
    mainLayout: TGridLayout;
    MainMenu: TMenuBar;

    topStyleComboLayout: TBoxLayout;
    lblStyle: TLabel;
    cbStyle: TComboBox;
    
    topCheckboxLayout: TBoxLayout;
    chkStdPalette: TCheckBox;
    chkDisable: TCheckBox;

    topLeftGroupBox: TGroupBox;
    topLeftGroupBoxLayout: TBoxLayout;
    Radio1: TRadioButton;
    Radio2: TRadioButton;
    Radio3: TRadioButton;

    topRightGroupBox: TGroupBox;
    topRightGroupBoxLayout: TBoxLayout;
    Button1: TButton;
    Button2: TButton;

    StringGrid: TStringGrid;

    bottomRightGroupBox: TGroupBox;
    bottomRightGroupBoxLayout: TBoxLayout;
    Edit1: TEdit;
    Edit2: TEdit;

    bottomButtonLayout: TBoxLayout;
    btnExit: TButton;
    btnHelp: TButton;

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
  if cbStyle.Text = 'OpenSoft' then
  begin
    if self.Style is TOpenSoftStyle then
      exit  //==>
    else
    begin
      Style := TOpenSoftStyle.Create(Application.Display);
      Redraw;
    end;
  end
  else
  begin
    if self.Style is TDefaultStyle then
      exit  //==>
    else
    begin
      Style := Application.DefaultStyle;
      Redraw;
    end;
  end;
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
begin
  MainMenu := TMenuBar.Create(self);
  MainMenu.AddMenu('File');
  MainMenu.AddMenu('Edit');
  MainMenu.AddMenu('Options');
  MainMenu.AddMenu('Windows');
  MainMenu.AddMenu('Help');
//  MainMenu.CanExpandWidth := True;
end;

procedure TWidgetDemoForm.CreateStyleCombo;
begin
  topStyleComboLayout := TBoxLayout.Create(self);

  lblStyle := TLabel.Create('Style:', self);

  cbStyle := TComboBox.Create(self);
  cbStyle.CanExpandWidth := True;
  cbStyle.Items.Add('Windows');
  cbStyle.Items.Add('WindowsXP');
  cbStyle.Items.Add('Motif');
  cbStyle.Items.Add('ClearLooks');
  cbStyle.Items.Add('OpenSoft');
  cbStyle.OnChange := @cbStyleChanged;
  cbStyle.ItemIndex := 0;

  topStyleComboLayout.InsertChild(lblStyle);
  topStyleComboLayout.InsertChild(cbStyle);
end;

procedure TWidgetDemoForm.CreateTopCheckboxLine;
begin
  topCheckboxLayout := TBoxLayout.Create(self);

  chkStdPalette := TCheckBox.Create('Standard color palette', self);
  chkStdPalette.CanExpandWidth  := True;
  chkStdPalette.Checked         := True;

  chkDisable := TCheckBox.Create('Disable widgets', self);
  chkDisable.OnClick :=@chkDisableClick;

  topCheckboxLayout.InsertChild(chkStdPalette);
  topCheckboxLayout.InsertChild(chkDisable);
end;

procedure TWidgetDemoForm.CreateTopLeftGroupBox;
begin
  topLeftGroupBox := TGroupBox.Create('Group Box 1', self);
  topLeftGroupBox.CanExpandWidth   := True;

  topLeftGroupBoxLayout := TBoxLayout.Create(self);
  topLeftGroupBoxLayout.Orientation := Vertical;

  Radio1 := TRadioButton.Create('Radio button 1', self);
  Radio1.Checked          := True;
  Radio1.CanExpandWidth   := True;
  Radio1.OnClick          := @Radio1Click;

  Radio2 := TRadioButton.Create('Radio button 2', self);
  Radio2.CanExpandWidth   := True;
  Radio2.OnClick          := @Radio2Click;

  Radio3 := TRadioButton.Create('Radio button 3', self);
  Radio3.CanExpandWidth   := True;

  topLeftGroupBox.InsertChild(topLeftGroupBoxLayout);
  topLeftGroupBoxLayout.InsertChild(Radio1);
  topLeftGroupBoxLayout.InsertChild(Radio2);
  topLeftGroupBoxLayout.InsertChild(Radio3);
end;

procedure TWidgetDemoForm.CreateTopRightGroupBox;
begin
  topRightGroupBox := TGroupBox.Create('Group Box 2', self);
  topRightGroupBox.CanExpandWidth     := True;
  topRightGroupBox.CanExpandHeight    := True;

  topRightGroupBoxLayout := TBoxLayout.Create(self);
  topRightGroupBoxLayout.Orientation  := Vertical;
  topRightGroupBoxLayout.VertAlign    := vertCenter;
  topRightGroupBoxLayout.Spacing      := 8;

  Button1 := TButton.Create('Normal Button', self);
  Button1.CanExpandWidth := True;

  Button2 := TButton.Create('Embedded Button', self);
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
  StringGrid := TStringGrid.Create(self);
  StringGrid.ColCount := 10;
  StringGrid.RowCount := 15;
  for y := 0 to StringGrid.RowCount - 1 do
    for x := 0 to StringGrid.ColCount - 1 do
      StringGrid.Cells[x, y] := Format('%d, %d', [x, y]);
end;

procedure TWidgetDemoForm.CreateBottomRightGroupBox;
begin
  bottomRightGroupBox := TGroupBox.Create('Group Box 3', self);
  bottomRightGroupBox.CanExpandHeight := True;
  bottomRightGroupBox.CanExpandWidth := True;

  bottomRightGroupBoxLayout := TBoxLayout.Create(self);
  bottomRightGroupBoxLayout.Orientation := Vertical;

  Edit1 := TEdit.Create('Normal Edit', self);
  Edit2 := TEdit.Create('Password Edit', self);
  Edit2.PasswordChar := '*';

  bottomRightGroupBox.InsertChild(bottomRightGroupBoxLayout);
  bottomRightGroupBoxLayout.InsertChild(Edit1);
  bottomRightGroupBoxLayout.InsertChild(Edit2);
end;

procedure TWidgetDemoForm.CreateBottomButtonLayout;
begin
  bottomButtonLayout := TBoxLayout.Create(self);
  bottomButtonLayout.HorzAlign := horzRight;

  btnHelp := TButton.Create('Help', self);

  btnExit := TButton.Create('Exit', self);
  btnExit.OnClick := @btnExitClick;

  bottomButtonLayout.InsertChild(btnHelp);
  bottomButtonLayout.InsertChild(btnExit);
end;

constructor TWidgetDemoForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text          := 'Widget Demo';
  BorderWidth   := 8;
//  WindowType    := wtWindow;
  
  topLayout := TBoxLayout.Create(self);
  topLayout.Orientation     := Vertical;
  mainLayout := TGridLayout.Create(self);
  mainLayout.RowCount       := 2;
  
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
  WidgetDemoForm := TWidgetDemoForm.Create(nil);
  Application.AddForm(WidgetDemoForm);
  Application.Run;
  WidgetDemoForm.Free;
end.

