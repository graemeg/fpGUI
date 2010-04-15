program demo_textedit;

{$mode objfpc}{$H+}

uses
  Classes,
  typinfo,
  Sysutils,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_label,
  fpg_memo, fpg_dialogs, fpg_utils, fpg_radiobutton,
  fpg_textedit, fpg_checkbox, fpg_panel;
  
type

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
    chkShowGutter: TfpgCheckBox;
    chkLineNumbers: TfpgCheckBox;
    Label2: TfpgLabel;
    btnChangeFont: TfpgButton;
    Label3: TfpgLabel;
    Label4: TfpgLabel;
    Bevel2: TfpgBevel;
    chkRightEdge: TfpgCheckBox;
    {@VFD_HEAD_END: MainForm}
    procedure   ShowGutterChanged(Sender: TObject);
    procedure   ShowLineNumbers(Sender: TObject);
    procedure   ShowRightEdge(Sender: TObject);
    procedure   AppExceptions(Sender: TObject; E: Exception);
    procedure   btnQuitClick(Sender: TObject);
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
    procedure   btnLoadClicked(Sender: TObject);
    procedure   ChangeFontClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
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

procedure TMainForm.ShowRightEdge(Sender: TObject);
begin
  TextEdit.RightEdge := chkRightEdge.Checked;
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
      TextEdit.LoadFromFile(s);
      Label2.Text := Format('%d ticks', [fpgGetTickCount - t]);
    end;
  end;
  fpgApplication.ProcessMessages;
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
  fpgApplication.OnException := @AppExceptions;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(319, 180, 594, 455);
  WindowTitle := 'TextEdit (new Memo) test';
  Hint := '';
  WindowPosition := wpScreenCenter;

  memo := TfpgMemo.Create(self);
  with memo do
  begin
    Name := 'memo';
    SetPosition(6, 172, 280, 235);
    Anchors := [anLeft,anTop,anBottom];
    Hint := '';
    Lines.Add('Memo Test0');
    Lines.Add('Memo Test1');
    Lines.Add('Memo Test2');
    Lines.Add('Memo Test3');
    Lines.Add('Memo Test4');
    FontDesc := '#Edit1';
    TabOrder := 0;
    //    FontDesc := 'Arial-15';
    //    Lines.Insert(1,'0 Before 1 after');
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(492, 64, 92, 23);
    Anchors := [anRight,anTop];
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.quit';
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
    SetPosition(492, 8, 92, 24);
    Anchors := [anRight,anTop];
    Text := 'Load File...';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnLoadClicked;
  end;

  rbLeft := TfpgRadioButton.Create(self);
  with rbLeft do
  begin
    Name := 'rbLeft';
    SetPosition(20, 28, 120, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 4;
    Text := 'Left';
  end;

  rbRight := TfpgRadioButton.Create(self);
  with rbRight do
  begin
    Name := 'rbRight';
    SetPosition(20, 52, 120, 20);
    Checked := True;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 5;
    Text := 'Right';
  end;

  rbBoth := TfpgRadioButton.Create(self);
  with rbBoth do
  begin
    Name := 'rbBoth';
    SetPosition(20, 76, 120, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
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
    Text := 'Label';
  end;

  chkShowGutter := TfpgCheckBox.Create(self);
  with chkShowGutter do
  begin
    Name := 'chkShowGutter';
    SetPosition(168, 28, 172, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 14;
    Text := 'Show Gutter';
    OnChange := @ShowGutterChanged;
  end;

  chkLineNumbers := TfpgCheckBox.Create(self);
  with chkLineNumbers do
  begin
    Name := 'chkLineNumbers';
    SetPosition(168, 52, 172, 20);
    Checked := True;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 15;
    Text := 'Show Line Numbers';
    OnChange := @ShowLineNumbers;
  end;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(348, 416, 176, 16);
    Anchors := [anLeft,anRight,anBottom];
    Alignment := taCenter;
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Label';
  end;

  btnChangeFont := TfpgButton.Create(self);
  with btnChangeFont do
  begin
    Name := 'btnChangeFont';
    SetPosition(492, 36, 92, 24);
    Anchors := [anRight,anTop];
    Text := 'Font...';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 17;
    OnClick := @ChangeFontClicked;
  end;

  Label3 := TfpgLabel.Create(self);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(8, 8, 112, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Side to Load';
  end;

  Label4 := TfpgLabel.Create(self);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(156, 8, 124, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'General Options';
  end;

  Bevel2 := TfpgBevel.Create(self);
  with Bevel2 do
  begin
    Name := 'Bevel2';
    SetPosition(140, 8, 16, 148);
    Hint := '';
    Shape := bsLeftLine;
  end;

  chkRightEdge := TfpgCheckBox.Create(self);
  with chkRightEdge do
  begin
    Name := 'chkRightEdge';
    SetPosition(168, 76, 172, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 16;
    Text := 'Right Edge Line';
    OnChange := @ShowRightEdge;
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
