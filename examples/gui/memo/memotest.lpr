program memotest;

{$mode objfpc}{$H+}

uses
  Classes,
  typinfo,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_button,
  fpg_memo,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc;

type
  TMainForm = class(TfpgForm)
  private
    memo: TfpgMemo;
    btnQuit: TfpgButton;
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
var
  mig: TfpgMigLayoutManager;
begin
  Name := 'MainForm';
  Left := 329;
  Top := 251;
  Width := 300;
  Height := 200;
  WindowTitle := 'Memo Test';
  WindowPosition := wpOneThirdDown;

  mig := TfpgMigLayoutManager.Create;
  mig.LC.SetWrapAfter(1);
  mig.LC.Fill;
  LayoutManager := mig;

  memo := TfpgMemo.Create(self);
  with memo do
  begin
    Name := 'memo';
    Left := 10;
    Top := 40;
    PreferredSize := fpgSize(280, 150);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Lines.Add(#9'Memo Test0');
    Lines.Add('Memo Test1');
    Lines.Add('Memo'#9'Test2');
    Lines.Add('Memo Test3');
    Lines.Add('Memo Test'#9'4');
    Lines.Insert(1, '0 Before 1 after');
    UseTabs := true;
  end;
  mig.AddLayoutComponent(memo, TfpgMigCC.Create().GrowX().GrowY());

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    PreferredSize := fpgSize(80, 24);
    Anchors := [anRight,anTop];
    Text := 'Quit';
    ImageName := 'stdimg.quit';
    OnClick := @btnQuitClicked;
  end;
  mig.AddLayoutComponent(btnQuit, TfpgMigCC.Create().AlignX('right').AlignY('bottom'));
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
