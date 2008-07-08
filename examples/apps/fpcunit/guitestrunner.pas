unit GuiTestRunner;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  // fpGUI toolkit
  gfxbase, fpgfx, gui_edit, gfx_widget, gui_form, gui_label, gui_button,
  gui_listbox, gui_memo, gui_combobox, gui_basegrid, gui_grid, 
  gui_dialogs, gui_checkbox, gui_tree, gui_trackbar, gui_progressbar,
  gui_radiobutton, gui_tab, gui_menu, gui_panel, gui_popupcalendar,
  gui_gauge,
  // FPCUnit support
  fpcunit, testregistry, testdecorator;

type

  TGUITestRunnerForm = class(TfpgForm, ITestListener)
  private
    failureCounter: Integer;
    errorCounter: Integer;
    testsCounter: Integer;
    skipsCounter: Integer;
    testSuite: TTest;
    // ITestListener
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
    //
    procedure RunTest(ATest: TTest);
    procedure MemoLog(LogEntry: string);
    procedure BuildTree(ARootNode: TfpgTreeNode; ASuite: TTestSuite);
    procedure FormCreate(Sender: TObject);
    procedure btnQuitClicked(Sender: TObject);
    procedure btnClearClicked(Sender: TObject);
    procedure btnRunClicked(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: GUITestRunnerForm}
    pbName1: TfpgProgressBar;
    cbName1: TfpgComboBox;
    btnRun: TfpgButton;
    lblName1: TfpgLabel;
    lblName2: TfpgLabel;
    lblRuns: TfpgLabel;
    lblName4: TfpgLabel;
    lblErrors: TfpgLabel;
    lblName6: TfpgLabel;
    lblFailures: TfpgLabel;
    tvTests: TfpgTreeView;
    memName1: TfpgMemo;
    btnClear: TfpgButton;
    btnQuit: TfpgButton;
    {@VFD_HEAD_END: GUITestRunnerForm}
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TGUITestRunnerForm.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  MemoLog(AFailure.ExceptionMessage);
end;

procedure TGUITestRunnerForm.AddError(ATest: TTest; AError: TTestFailure);
begin
  MemoLog(AError.ExceptionMessage);
end;

procedure TGUITestRunnerForm.StartTest(ATest: TTest);
begin

end;

procedure TGUITestRunnerForm.EndTest(ATest: TTest);
begin

end;

procedure TGUITestRunnerForm.StartTestSuite(ATestSuite: TTestSuite);
begin

end;

procedure TGUITestRunnerForm.EndTestSuite(ATestSuite: TTestSuite);
begin

end;

procedure TGUITestRunnerForm.RunTest(ATest: TTest);
var
  lTestResult: TTestResult;
  FStopCrono: TDateTime;
  FStartCrono: TDateTime;
begin
  // Reset counters
  failureCounter  := 0;
  errorCounter    := 0;
  testsCounter    := 0;
  skipsCounter    := 0;

  lTestResult := TTestResult.Create;
  try
    lTestResult.AddListener(self);
    MemoLog('Running ' + tvTests.Selection.Text);
    FStartCrono := Now;
    ATest.Run(lTestResult);
    FStopCrono  := Now;

    MemoLog('Number of executed tests: '
      + IntToStr(lTestResult.RunTests)
      + '  Time elapsed: '
      + FormatDateTime('hh:nn:ss.zzz', FStopCrono - FStartCrono));

  finally
    lTestResult.Free;
  end;
end;

procedure TGUITestRunnerForm.MemoLog(LogEntry: string);
begin
  memName1.Lines.Add(TimeToStr(Now) + ' - ' + LogEntry);
end;

procedure TGUITestRunnerForm.BuildTree(ARootNode: TfpgTreeNode; ASuite: TTestSuite);
var
  node: TfpgTreeNode;
  i: integer;
begin
//  ARootNode.StateIndex := Ord(tsChecked);

  for i := 0 to ASuite.Tests.Count-1 do
  begin
    node := ARootNode.AppendText(ASuite.Test[i].TestName);
    node.Data := ASuite.Test[i];
    if ASuite.Test[i] is TTestSuite then
      BuildTree(node, TTestSuite(ASuite.Test[i]))
    else
      if TObject(ASuite.Test[i]).InheritsFrom(TTestDecorator) then
        BuildTree(node, TTestSuite(TTestDecorator(ASuite.Test[i]).Test));
//    node.ImageIndex := 12;
//    node.SelectedIndex := 12;
//    node.StateIndex := ord(tsChecked);
  end;
//  rootNode.Expand(False);
//  ResetNodeColors;

end;

procedure TGUITestRunnerForm.FormCreate(Sender: TObject);
var
  n: TfpgTreeNode;
begin
  n := tvTests.RootNode.AppendText('All Tests');
  BuildTree(n, GetTestRegistry);
end;

procedure TGUITestRunnerForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TGUITestRunnerForm.btnClearClicked(Sender: TObject);
begin
  memName1.Lines.Clear;
end;

procedure TGUITestRunnerForm.btnRunClicked(Sender: TObject);
begin
  if (tvTests.Selection <> nil) and (tvTests.Selection.Data <> nil) then
  begin
    testSuite := TTest(tvTests.Selection.Data);
//    tvTests.Selection.Collapse;
  end;
  RunTest(testSuite);
end;

constructor TGUITestRunnerForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnCreate := @FormCreate;
end;

procedure TGUITestRunnerForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: GUITestRunnerForm}
  Name := 'GUITestRunnerForm';
  SetPosition(372, 260, 359, 547);
  WindowTitle := 'GUI Test Runner';

  pbName1 := TfpgProgressBar.Create(self);
  with pbName1 do
  begin
    Name := 'pbName1';
    SetPosition(8, 64, 340, 22);
    Anchors := [anLeft,anRight,anTop];
  end;

  cbName1 := TfpgComboBox.Create(self);
  with cbName1 do
  begin
    Name := 'cbName1';
    SetPosition(16, 24, 244, 22);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#List';
    TabOrder := 1;
  end;

  btnRun := TfpgButton.Create(self);
  with btnRun do
  begin
    Name := 'btnRun';
    SetPosition(268, 24, 80, 24);
    Anchors := [anRight,anTop];
    Text := 'Run';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 2;
    OnClick := @btnRunClicked;
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(4, 4, 236, 16);
    FontDesc := '#Label2';
    Text := 'TestCase';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(8, 96, 40, 16);
    FontDesc := '#Label2';
    Text := 'Runs:';
  end;

  lblRuns := TfpgLabel.Create(self);
  with lblRuns do
  begin
    Name := 'lblRuns';
    SetPosition(48, 96, 55, 16);
    FontDesc := '#Label1';
    Text := '---';
  end;

  lblName4 := TfpgLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(120, 96, 44, 16);
    FontDesc := '#Label2';
    Text := 'Errors:';
  end;

  lblErrors := TfpgLabel.Create(self);
  with lblErrors do
  begin
    Name := 'lblErrors';
    SetPosition(166, 96, 55, 16);
    FontDesc := '#Label1';
    Text := '---';
  end;

  lblName6 := TfpgLabel.Create(self);
  with lblName6 do
  begin
    Name := 'lblName6';
    SetPosition(232, 96, 60, 16);
    FontDesc := '#Label2';
    Text := 'Failures:';
  end;

  lblFailures := TfpgLabel.Create(self);
  with lblFailures do
  begin
    Name := 'lblFailures';
    SetPosition(292, 96, 55, 16);
    FontDesc := '#Label1';
    Text := '---';
  end;

  tvTests := TfpgTreeView.Create(self);
  with tvTests do
  begin
    Name := 'tvTests';
    SetPosition(8, 120, 340, 268);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Label1';
    TabOrder := 3;
  end;

  memName1 := TfpgMemo.Create(self);
  with memName1 do
  begin
    Name := 'memName1';
    SetPosition(8, 400, 340, 113);
    Anchors := [anLeft,anRight,anBottom];
    FontDesc := '#Edit1';
    TabOrder := 4;
  end;

  btnClear := TfpgButton.Create(self);
  with btnClear do
  begin
    Name := 'btnClear';
    SetPosition(8, 517, 80, 24);
    Anchors := [anLeft,anBottom];
    Text := 'Clear';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 5;
    OnClick := @btnClearClicked;
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(268, 517, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 6;
    OnClick := @btnQuitClicked;
  end;

  {@VFD_BODY_END: GUITestRunnerForm}
end;


end.
