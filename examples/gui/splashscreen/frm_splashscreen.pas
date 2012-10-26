unit frm_splashscreen;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_panel,
  fpg_label;

type
  TSplashForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: SplashForm}
    pnlName1: TfpgBevel;
    lblName2: TfpgLabel;
    lblName1: TfpgLabel;
    {@VFD_HEAD_END: SplashForm}
    tmr: TfpgTimer;
    procedure   SplashFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure   SplashFormShow(Sender: TObject);
    procedure   TimerFired(Sender: TObject);
    procedure   SplashFormClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

var
  frmSplash: TSplashForm;
  
implementation

{@VFD_NEWFORM_IMPL}

procedure TSplashForm.SplashFormClick(Sender: TObject);
begin
  TimerFired(nil);
end;

procedure TSplashForm.SplashFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TSplashForm.SplashFormShow(Sender: TObject);
begin
  tmr.Enabled := True;
end;

procedure TSplashForm.TimerFired(Sender: TObject);
begin
  tmr.Enabled := False;
  tmr.Free;
  Close;
end;

constructor TSplashForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowType := wtPopup;  // borderless but doesn't steal focus
  WindowAttributes := WindowAttributes + [waStayOnTop]; // well, it lets the window stay on top. :)

  tmr := TfpgTimer.Create(3000);
  tmr.OnTimer := @TimerFired;
  
  OnShow := @SplashFormShow;
  OnClick := @SplashFormClick;
  OnClose := @SplashFormClose;
end;

procedure TSplashForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: SplashForm}
  Name := 'SplashForm';
  SetPosition(298, 261, 300, 64);
  WindowTitle := 'SplashForm';
  WindowPosition := wpScreenCenter;
  Sizeable := False;

  pnlName1 := TfpgBevel.Create(self);
  with pnlName1 do
  begin
    Name := 'pnlName1';
    SetPosition(0, 0, 300, 64);
    OnClick := @TimerFired;
  end;

  lblName2 := TfpgLabel.Create(pnlName1);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(24, 8, 272, 31);
    Text := 'Splash screen goes here!';
    FontDesc := 'Arial-18';
    OnClick := @TimerFired;
  end;

  lblName1 := TfpgLabel.Create(pnlName1);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(52, 42, 188, 15);
    Text := 'Click me to make me disappear.';
    FontDesc := '#Label1';
    OnClick := @TimerFired;
  end;

  {@VFD_BODY_END: SplashForm}
end;


end.
