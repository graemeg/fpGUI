program memotest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  typinfo,
  fpgfx,
  gfxbase,
  gui_form,
  gui_button,
  gui_label,
  gui_memo;
  
type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    memo: TfpgMemo;
    btnQuit: TfpgButton;
    procedure   btnQuitClick(Sender: TObject);
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

procedure TMainForm.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);
  if Assigned(Memo) then
  begin
    Memo.SetPosition(Memo.Left, Memo.Top, awidth-20, aheight- Memo.Top - 10);
    btnQuit.Left := awidth - btnQuit.Width - 10;
    btnQuit.UpdateWindowPosition;
  end;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Memo test';
  SetPosition(100, 100, 300, 300);

  memo      :=  CreateMemo(self, 10, 40, 280, 150);

  memo.Lines.Add('Memo Test0');
  memo.Lines.Add('Memo Test1');
  //memo.Lines.Add('Memo Test2');
  //memo.Lines.Add('Memo Test3');
  //memo.Lines.Add('Memo Test4');
  memo.Lines.Insert(1,'0 Before 1 after');
  //memo.Lines.Delete(1);
  //memo.Lines.Text := 'Dude'+LineEnding+'What''s mine say?'+LineEnding;;
  //memo.Lines.Text := memo.Lines.Text + 'Sweet'+LineEnding;
  //memo.lines.LoadFromFile('/home/andrew/programming/groupprojects/fpgui/src/gui/gui_memo.pas');
  //memo.lines.LoadFromFile('/usr/share/dict/cracklib-small');
  {memo.lines.Add(memo.lines.text);
  memo.lines.Add(memo.lines.text);
  memo.lines.Add(memo.lines.text);
  memo.lines.Add(memo.lines.text);}
  {memo.lines.Add(memo.lines.text);
  memo.lines.Add(memo.lines.text);
  memo.lines.Add(memo.lines.text);
  memo.lines.Add(memo.lines.text);
  memo.lines.Add(memo.lines.text);
  memo.lines.Add(memo.lines.text);}
  {memo.lines.Add(memo.lines.text);
  memo.lines.Add(memo.lines.text);
  memo.lines.Add(memo.lines.text);}
 { memo.Lines.Text := memo.Lines.Text + memo.Lines.Text;
  memo.Lines.Text := memo.Lines.Text + memo.Lines.Text;
  memo.Lines.Text := memo.Lines.Text + memo.Lines.Text;
  memo.Lines.Text := memo.Lines.Text + memo.Lines.Text;
  memo.Lines.Text := memo.Lines.Text + memo.Lines.Text;
  memo.Lines.Text := memo.Lines.Text + memo.Lines.Text; }
  btnQuit   := CreateButton(self, 210, 10, 80, 'Quit', @btnQuitClick);
  btnQuit.ImageName := 'stdimg.quit';
  btnQuit.ShowImage := True;
  
  HandleResize(Width, Height);

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
