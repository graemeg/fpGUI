program demo_textedit;

{$mode objfpc}{$H+}

uses
//  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
//  {$ENDIF}{$ENDIF}
  Classes,
  typinfo,
  Sysutils,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_button,
  fpg_label,
  fpg_memo, fpg_dialogs, fpg_utils, fpg_radiobutton,
  fpg_progressbar, fpg_textedit, fpg_checkbox;
  
type

  TMyThread = class(TThread)
  private
    bar: TfpgProgressBar;
    pos: integer;
    procedure UpdateGUI;
  protected
    procedure Execute; override;
  public
    constructor CreateCustom(pb: TfpgProgressBar);
  end;


  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    memo: TfpgMemo;
    btnQuit: TfpgButton;
    TextEdit: TfpgTextEdit;
    btnLoad: TfpgButton;
    rbLeft: TfpgRadioButton;
    rbRight: TfpgRadioButton;
    rbBoth: TfpgRadioButton;
    Label1: TfpgLabel;
    ProgressBar2: TfpgProgressBar;
    ProgressBar1: TfpgProgressBar;
    ProgressBar3: TfpgProgressBar;
    Button1: TfpgButton;
    Button2: TfpgButton;
    Button3: TfpgButton;
    chkShowGutter: TfpgCheckBox;
    chkLineNumbers: TfpgCheckBox;
    Label2: TfpgLabel;
    btnChangeFont: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    t1: TMyThread;
    t2: TMyThread;
    t3: TMyThread;
    procedure   ShowGutterChanged(Sender: TObject);
    procedure   ShowLineNumbers(Sender: TObject);
    procedure   AppExceptions(Sender: TObject; E: Exception);
    procedure   btnQuitClick(Sender: TObject);
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
    procedure   btnLoadClicked(Sender: TObject);
    procedure   btn1(Sender: TObject);
    procedure   btn2(Sender: TObject);
    procedure   btn3(Sender: TObject);
    procedure   ChangeFontClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  end;

{ TMyThread }

procedure TMyThread.UpdateGUI;
begin
  bar.Position := pos;
end;

procedure TMyThread.Execute;
begin
  pos := -1;
  while not Terminated do
  begin
    inc(pos);
    Synchronize(@UpdateGUI);
    sleep(200);
    if pos >= 100 then
      break;
  end;
end;

constructor TMyThread.CreateCustom(pb: TfpgProgressBar);
begin
  Create(True);
  bar := pb;
  FreeOnTerminate := True;
  Suspended := False;
end;

{ TMainForm }

procedure TMainForm.ShowGutterChanged(Sender: TObject);
begin
  TextEdit.GutterVisible := chkShowGutter.Checked;
end;

procedure TMainForm.ShowLineNumbers(Sender: TObject);
begin
  TextEdit.GutterShowLineNumbers := chkLineNumbers.Checked;
end;

procedure TMainForm.AppExceptions(Sender: TObject; E: Exception);
begin
  DumpStack;
end;

procedure TMainForm.btnQuitClick(Sender: TObject);
begin
{
  if Assigned(t1) then
  begin
    t1.Terminate;
    writeln('t1 waiting...');
    t1.WaitFor;
  end;
  writeln('t1 done');
  if Assigned(t2) then
  begin
    t2.Terminate;
    writeln('t2 waiting...');
    t2.WaitFor;
  end;
  writeln('t2 done');
  if Assigned(t3) then
  begin
    t3.Terminate;
    writeln('t3 waiting...');
    t3.WaitFor;
  end;
  writeln('t3 done');
}
  Close;
end;

procedure TMainForm.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);
  //if Assigned(Memo) then
  //begin
    //Memo.SetPosition(Memo.Left, Memo.Top, awidth-20, aheight- Memo.Top - 10);
    //btnQuit.Left := awidth - btnQuit.Width - 10;
    //btnQuit.UpdateWindowPosition;
  //end;
end;

procedure TMainForm.btnLoadClicked(Sender: TObject);
var
  s: string;
  t: Cardinal;
begin
  s := SelectFileDialog;
  if (s <> '') and fpgFileExists(s) then
  begin
    if rbLeft.Checked or rbBoth.Checked then
    begin
      t := fpgGetTickCount;
      memo.Lines.LoadFromFile(s);
      Label1.Text := Format('%d ticks', [fpgGetTickCount - t]);
    end;
    if rbRight.Checked or rbBoth.Checked then
    begin
      t := fpgGetTickCount;
      TextEdit.Lines.LoadFromFile(s);
      TextEdit.Invalidate;
      Label2.Text := Format('%d ticks', [fpgGetTickCount - t]);
    end;
  end;
  fpgApplication.ProcessMessages;
end;

procedure TMainForm.btn1(Sender: TObject);
begin
  t1 := TMyThread.CreateCustom(ProgressBar1);
end;

procedure TMainForm.btn2(Sender: TObject);
begin
  t2 := TMyThread.CreateCustom(ProgressBar2);
end;

procedure TMainForm.btn3(Sender: TObject);
begin
  t3 := TMyThread.CreateCustom(ProgressBar3);
end;

procedure TMainForm.ChangeFontClicked(Sender: TObject);
var
  fnt: string;
begin
  fnt := TextEdit.FontDesc;
  if SelectFontDialog(fnt) then
    TextEdit.FontDesc := fnt;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

//  HandleResize(Width, Height);

  fpgApplication.OnException :=@AppExceptions;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(319, 180, 594, 455);
  WindowTitle := 'Memo test';
  WindowPosition := wpScreenCenter;

  memo := TfpgMemo.Create(self);
  with memo do
  begin
    Name := 'memo';
    SetPosition(6, 172, 280, 235);
    Anchors := [anLeft,anTop,anBottom];
    Lines.Add('Memo Test0');
    Lines.Add('Memo Test1');
    Lines.Add('Memo Test2');
    Lines.Add('Memo Test3');
    Lines.Add('Memo Test4');
    FontDesc := '#Edit1';
    ParentShowHint := True;
    TabOrder := 0;
    //    FontDesc := 'Arial-15';
    //    Lines.Insert(1,'0 Before 1 after');
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(310, 10, 80, 23);
    Text := 'Button';
    AllowAllUp := False;
    Embedded := False;
    Flat := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    ImageLayout := ilImageLeft;
    ImageMargin := 3;
    ImageName := 'stdimg.quit';
    ImageSpacing := -1;
    ModalResult := 0;
    ParentShowHint := True;
    ShowImage := True;
    TabOrder := 1;
    OnClick := @btnQuitClick;
  end;

  TextEdit := TfpgTextEdit.Create(self);
  with TextEdit do
  begin
    Name := 'TextEdit';
    SetPosition(300, 172, 280, 235);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Lines.Add('Memo Test0');
    Lines.Add('Memo Test1');
    Lines.Add('Memo Test2');
    Lines.Add('Memo Test3');
    Lines.Add('Memo Test4');
    //    FontDesc := '#Edit1';
    FontDesc := 'Bitstream Vera Sans Mono-10';
    //    Lines.Insert(1,'0 Beforje 1 after');
    ParentShowHint := True;
  end;

  btnLoad := TfpgButton.Create(self);
  with btnLoad do
  begin
    Name := 'btnLoad';
    SetPosition(312, 44, 80, 24);
    Text := 'Load';
    AllowAllUp := False;
    Embedded := False;
    Flat := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    ImageLayout := ilImageLeft;
    ImageMargin := 3;
    ImageName := '';
    ImageSpacing := -1;
    ModalResult := 0;
    ParentShowHint := True;
    ShowImage := True;
    TabOrder := 3;
    OnClick := @btnLoadClicked;
  end;

  rbLeft := TfpgRadioButton.Create(self);
  with rbLeft do
  begin
    Name := 'rbLeft';
    SetPosition(416, 8, 120, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    ParentShowHint := True;
    TabOrder := 4;
    Text := 'Left';
  end;

  rbRight := TfpgRadioButton.Create(self);
  with rbRight do
  begin
    Name := 'rbRight';
    SetPosition(416, 32, 120, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    ParentShowHint := True;
    TabOrder := 5;
    Text := 'Right';
  end;

  rbBoth := TfpgRadioButton.Create(self);
  with rbBoth do
  begin
    Name := 'rbBoth';
    SetPosition(416, 56, 120, 20);
    Checked := True;
    FontDesc := '#Label1';
    GroupIndex := 0;
    ParentShowHint := True;
    TabOrder := 6;
    Text := 'Both';
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(52, 417, 172, 16);
    Anchors := [anLeft,anBottom];
    Alignment := taCenter;
    FontDesc := '#Label2';
    Hint := '';
    Layout := tlTop;
    ParentShowHint := True;
    Text := 'Label';
    WrapText := False;
  end;

  ProgressBar2 := TfpgProgressBar.Create(self);
  with ProgressBar2 do
  begin
    Name := 'ProgressBar2';
    SetPosition(12, 48, 254, 22);
    Min := 0;
    Max := 100;
    ParentShowHint := True;
    Position := 0;
    ShowCaption := False;
  end;

  ProgressBar1 := TfpgProgressBar.Create(self);
  with ProgressBar1 do
  begin
    Name := 'ProgressBar1';
    SetPosition(12, 20, 254, 22);
    Min := 0;
    Max := 100;
    ParentShowHint := True;
    Position := 0;
    ShowCaption := False;
  end;

  ProgressBar3 := TfpgProgressBar.Create(self);
  with ProgressBar3 do
  begin
    Name := 'ProgressBar3';
    SetPosition(12, 76, 254, 22);
    Min := 0;
    Max := 100;
    ParentShowHint := True;
    Position := 0;
    ShowCaption := False;
  end;

  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(276, 20, 20, 20);
    Text := '';
    AllowAllUp := False;
    Embedded := False;
    Flat := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    ImageLayout := ilImageLeft;
    ImageMargin := 3;
    ImageName := '';
    ImageSpacing := -1;
    ModalResult := 0;
    ParentShowHint := True;
    ShowImage := True;
    TabOrder := 11;
    OnClick := @btn1;
  end;

  Button2 := TfpgButton.Create(self);
  with Button2 do
  begin
    Name := 'Button2';
    SetPosition(276, 48, 20, 20);
    Text := '';
    AllowAllUp := False;
    Embedded := False;
    Flat := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    ImageLayout := ilImageLeft;
    ImageMargin := 3;
    ImageName := '';
    ImageSpacing := -1;
    ModalResult := 0;
    ParentShowHint := True;
    ShowImage := True;
    TabOrder := 12;
    OnClick := @btn2;
  end;

  Button3 := TfpgButton.Create(self);
  with Button3 do
  begin
    Name := 'Button3';
    SetPosition(276, 76, 20, 20);
    Text := '';
    AllowAllUp := False;
    Embedded := False;
    Flat := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    ImageLayout := ilImageLeft;
    ImageMargin := 3;
    ImageName := '';
    ImageSpacing := -1;
    ModalResult := 0;
    ParentShowHint := True;
    ShowImage := True;
    TabOrder := 13;
    OnClick := @btn3;
  end;

  chkShowGutter := TfpgCheckBox.Create(self);
  with chkShowGutter do
  begin
    Name := 'chkShowGutter';
    SetPosition(416, 84, 120, 20);
    FontDesc := '#Label1';
    ParentShowHint := True;
    TabOrder := 14;
    Text := 'Show Gutter';
    OnChange :=@ShowGutterChanged;
  end;

  chkLineNumbers := TfpgCheckBox.Create(self);
  with chkLineNumbers do
  begin
    Name := 'chkLineNumbers';
    SetPosition(416, 108, 120, 20);
    Checked := True;
    FontDesc := '#Label1';
    ParentShowHint := True;
    TabOrder := 15;
    Text := 'Show Line Numbers';
    OnChange := @ShowLineNumbers;
  end;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(348, 416, 176, 16);
    Alignment := taCenter;
    FontDesc := '#Label2';
    Hint := '';
    Layout := tlTop;
    ParentShowHint := True;
    Text := 'Label';
    WrapText := False;
  end;

  btnChangeFont := TfpgButton.Create(self);
  with btnChangeFont do
  begin
    Name := 'btnChangeFont';
    SetPosition(312, 76, 80, 24);
    Text := 'Font...';
    AllowAllUp := False;
    Embedded := False;
    Flat := False;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    ImageLayout := ilImageLeft;
    ImageMargin := 3;
    ImageName := '';
    ImageSpacing := -1;
    ModalResult := 0;
    ParentShowHint := True;
    ShowImage := True;
    TabOrder := 17;
    OnClick := @ChangeFontClicked;
  end;

  {@VFD_BODY_END: MainForm}
end;

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
  frm.Free;
end;

begin
  MainProc;
end.
