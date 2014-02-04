program timertest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_label;

type
  TMainForm = class(TfpgForm)
  private
    btnClose: TfpgButton;
    btnStopStart: TfpgButton;
    timer1: TfpgTimer;
    timer2: TfpgTimer;
    timer3: TfpgTimer;
    lblTimer1: TfpgLabel;
    lblTimer2: TfpgLabel;
    lblTimer3: TfpgLabel;
    cnt1: integer;
    cnt2: integer;
    procedure   MyTimer1(Sender: TObject);
    procedure   MyTimer2(Sender: TObject);
    procedure   MyTimer3(Sender: TObject);
    procedure   btnCloseClick(Sender: TObject);
    procedure   btnStopStartClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;


{ TMainForm }

procedure TMainForm.MyTimer1(Sender: TObject);
begin
  lblTimer1.Text := FormatDateTime('hh:nn:ss.zzz', now);
end;

procedure TMainForm.MyTimer2(Sender: TObject);
begin
  Inc(cnt1);
  lblTimer2.Text := IntToStr(cnt1);
end;

procedure TMainForm.MyTimer3(Sender: TObject);
begin
  Inc(cnt2);
  lblTimer3.Text := IntToStr(cnt2);
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnStopStartClick(Sender: TObject);
begin
  if btnStopStart.Text = 'Stop' then
    btnStopStart.Text := 'Start'
  else
    btnStopStart.Text := 'Stop';
    
  timer1.Enabled := not timer1.Enabled;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'fpGUI Timer test';
  WindowPosition := wpScreenCenter;
  Width := 400;
  Height := 250;

  cnt1    := 0;
  cnt2    := 0;

  btnClose := CreateButton(self, 320, 220, 75, 'Close', @btnCloseClick);
  btnStopStart := CreateButton(self, 200, 50, 75, 'Stop', @btnStopStartClick);
  
  lblTimer1 := CreateLabel(self, 50, 50, '---');
  lblTimer1.FontDesc  := 'Arial-14:bold';
  lblTimer1.Height    := lblTimer1.Font.Height;
  lblTimer1.Width     := 150;
  
  lblTimer2 := CreateLabel(self, 50, 80, '---');
  lblTimer2.FontDesc  := 'Arial-14:bold';
  lblTimer2.Height    := lblTimer2.Font.Height;
  lblTimer2.Width     := 150;

  lblTimer3 := CreateLabel(self, 50, 110, '---');
  lblTimer3.FontDesc  := 'Arial-14:bold';
  lblTimer3.Height    := lblTimer3.Font.Height;
  lblTimer3.Width     := 150;

  timer1          := TfpgTimer.Create(50);
  timer1.OnTimer  := @MyTimer1;
  timer1.Enabled  := True;

  timer2          := TfpgTimer.Create(200);
  timer2.OnTimer  := @MyTimer2;
  timer2.Enabled  := True;

  timer3          := TfpgTimer.Create(1000);
  timer3.OnTimer  := @MyTimer3;
  timer3.Enabled  := True;
end;

destructor TMainForm.Destroy;
begin
  timer3.Free;
  timer2.Free;
  timer1.Free;
  inherited Destroy;
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

