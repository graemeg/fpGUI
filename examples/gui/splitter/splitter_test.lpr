program splitter_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Classes, fpg_base, fpg_main,
  fpg_form, fpg_menu, fpg_memo, fpg_listbox,
  fpg_panel, fpg_progressbar, fpg_splitter, fpg_checkbox;

type
  { TfrmSplitterTest }

  TfrmSplitterTest = class(TfpgForm)
  private
    procedure CheckBoxChanged(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: frmSplitterTest}
    menu: TfpgMenuBar;
    pnlStatus: TfpgPanel;
    pnlLeft: TfpgPanel;
    pnlRight: TfpgPanel;
    lstChoice: TfpgListBox;
    spl1: TfpgSplitter;
    mmSource: TfpgMemo;
    spl2: TfpgSplitter;
    mmDest: TfpgMemo;
    pnlRigth: TfpgPanel;
    pnlName1: TfpgPanel;
    spl3: TfpgSplitter;
    pbName1: TfpgProgressBar;
    spl4: TfpgSplitter;
    cbShowGrabBar: TfpgCheckBox;
    {@VFD_HEAD_END: frmSplitterTest}
    bogus: TComponent; // for testing FindControl
    procedure AfterCreate; override;
    procedure MenuExitClick(sender: TObject);
  end;

{@VFD_NEWFORM_DECL}

{@VFD_NEWFORM_IMPL}

procedure TfrmSplitterTest.CheckBoxChanged(Sender: TObject);
begin
  //
end;

procedure TfrmSplitterTest.AfterCreate;
begin
  // put something for TfpgSplitter.FindControl to /trip/ over
  bogus := TComponent.create(self);
  bogus.name := 'bogus';

  {@VFD_BODY_BEGIN: frmSplitterTest}
  Name := 'frmSplitterTest';
  SetPosition(292, 184, 553, 290);
  WindowTitle := 'Splitter Demo';

  menu:=TfpgMenuBar.create(self);
  with menu do begin
    name:='menu';
    SetPosition(0, 0, 553, 24);
    align:=alTop;
    AddMenuItem('E&xit', @MenuExitClick);
  end;

  pnlStatus:=TfpgPanel.create(self);
  with pnlStatus do begin
    name:='pnlStatus';
    SetPosition(0, 266, 553, 24);
    align:=alBottom;
    text:='Status Bar';
  end;

  pnlLeft:=TfpgPanel.create(self);
  with pnlLeft do begin
    name:='pnlLeft';
    SetPosition(0, 24, 24, 242);
    align:=alLeft;
    text:='b1';
  end;

  pnlRight:=TfpgPanel.create(self);
  with pnlRight do begin
    name:='pnlRight';
    SetPosition(505, 24, 24, 242);
    align:=alRight;
    text:='b2';
  end;

  lstChoice := TfpgListBox.Create(self);
  with lstChoice do
  begin
    Name := 'lstChoice';
    SetPosition(24, 24, 136, 242);
    FontDesc := '#List';
    Items.Add('List item #1');
    Items.Add('List item #2');
    TabOrder := 3;
    Align := alLeft;
  end;

  spl1 := TfpgSplitter.Create(self);
  with spl1 do
  begin
    Name := 'spl1';
    SetPosition(160, 24, 8, 224);
    Align := alLeft;
  end;

  mmSource := TfpgMemo.Create(self);
  with mmSource do
  begin
    Name := 'mmSource';
    SetPosition(164, 24, 257, 90);
    Lines.Add('Memo has a MinHeight=30 so the splitter');
    Lines.Add('snap effect will not take affect - as expected.');
    FontDesc := '#Edit1';
    TabOrder := 2;
    Align := alTop;
  end;

  spl2 := TfpgSplitter.Create(self);
  with spl2 do
  begin
    Name := 'spl2';
    SetPosition(164, 90, 257, 8);
    Align := alTop;
  end;

  pnlName1 := TfpgPanel.Create(self);
  with pnlName1 do
  begin
    Name := 'pnlName1';
    SetPosition(425, 0, 128, 208);
    Text := 'Panel';
    Align := alRight;
  end;

  cbShowGrabBar := TfpgCheckBox.Create(pnlName1);
  with cbShowGrabBar do
  begin
    Name := 'cbShowGrabBar';
    SetPosition(4, 4, 120, 23);
    Text := 'Show GrabBar';
    Checked := True;
    OnChange :=@CheckBoxChanged;
  end;

  spl3 := TfpgSplitter.Create(self);
  with spl3 do
  begin
    Name := 'spl3';
    SetPosition(422, 0, 8, 208);
    Align := alRight;
  end;

  pbName1 := TfpgProgressBar.Create(self);
  with pbName1 do
  begin
    Name := 'pbName1';
    SetPosition(0, 213, 554, 78);
    Position := 100;
    Align := alBottom;
  end;

  spl4 := TfpgSplitter.Create(self);
  with spl4 do
  begin
    Name := 'spl4';
    SetPosition(0, 211, 554, 8);
    Align := alBottom;
  end;

  mmDest := TfpgMemo.Create(self);
  with mmDest do
  begin
    Name := 'mmDest';
    SetPosition(165, 94, 100, 100);
    Lines.Add('Memo2 Line #1');
    Lines.Add('Memo2 Line #2');
    FontDesc := '#Edit1';
    TabOrder := 1;
    Align := alClient;
  end;

  {@VFD_BODY_END: frmSplitterTest}
end;

procedure TfrmSplitterTest.MenuExitClick(sender: TObject);
begin
  close;
end;

procedure MainProc;
var
  frmSplitterTest: TfrmSplitterTest;
begin
  fpgApplication.Initialize;
  frmSplitterTest := TfrmSplitterTest.Create(nil);
  try
    frmSplitterTest.Show;
    fpgApplication.Run;
  finally
    frmSplitterTest.Free;
  end;
end;

begin
  MainProc;
end.
