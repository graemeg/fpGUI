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
  fpg_memo;
  
type
  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    memo: TfpgMemo;
    btnQuit: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    procedure   btnQuitClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{ TMainForm }

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(329, 251, 300, 201);
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
