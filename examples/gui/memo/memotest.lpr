program memotest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  typinfo,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_button,
  fpg_label,
  fpg_memo;
  
type
  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    memo: TfpgMemo;
    btnQuit: TfpgButton;
    llineHeight: Tfpglabel;
    btnFive: Tfpgbutton;
    btnDefault: Tfpgbutton;
    btnTwenty: Tfpgbutton;
    {@VFD_HEAD_END: MainForm}
    procedure   btnQuitClicked(Sender: TObject);
    procedure   btnFiveClick(Sender: TObject);
    procedure   btnDefaultClick(Sender: TObject);
    procedure   btnTwentyClick(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{ TMainForm }

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnFiveClick(Sender: TObject);
begin
  memo.LineHeight:= 5;
  memo.Invalidate;
end;

procedure TMainForm.btnDefaultClick(Sender: TObject);
begin
  memo.LineHeight:= memo.Font.Height + 2;
  memo.Invalidate;
end;

procedure TMainForm.btnTwentyClick(Sender: TObject);
begin
  memo.LineHeight:= 20;
  memo.Invalidate;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(329, 251, 300, 230);
  WindowTitle := 'Memo Test';
  WindowPosition := wpOneThirdDown;

  memo := TfpgMemo.Create(self);
  with memo do
  begin
    Name := 'memo';
    SetPosition(10, 40, 280, 150);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Lines.Add('Memo Test0');
    Lines.Add('Memo Test1');
    Lines.Add('Memo Test2');
    Lines.Add('Memo Test3');
    Lines.Add('Memo Test4');
    FontDesc := '#Edit1';
    TabOrder := 0;
    Lines.Insert(1, '0 Before 1 after');
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(208, 8, 80, 24);
    Anchors := [anRight,anTop];
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.quit';
    TabOrder := 1;
    OnClick := @btnQuitClicked;
  end;

  llineHeight := TfpgLabel.Create(self);
  with llineHeight do
  begin
    Name := 'llineHeight';
    SetPosition(10,200,65,24);
    Text := 'Line height';
  end;

  btnFive := Tfpgbutton.Create(self);
  with btnFive do
  begin
    Name := 'btnFive';
    SetPosition(80,195,30,24);
    Text := '5';
    OnClick := @btnFiveClick;
  end;

  btnDefault := Tfpgbutton.Create(self);
  with btnDefault do
  begin
    Name := 'btnDefault';
    SetPosition(120,195,50,24);
    Text := 'Default';
    OnClick := @btnDefaultClick;
  end;

  btnTwenty := Tfpgbutton.Create(self);
  with btnTwenty do
  begin
    Name := 'btnTwenty';
    SetPosition(180,195,30,24);
    Text := '20';
    OnClick := @btnTwentyClick;
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
