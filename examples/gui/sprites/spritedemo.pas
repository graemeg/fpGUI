program spritedemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_imgfmt_bmp;

type

  TMainForm = class(TfpgForm)
  private
    Timer: TfpgTimer;
    Background: TfpgImage;
    SpriteImg: TfpgImage;
    FShowInterval: Boolean;
    procedure   TimerFired(Sender: TObject);
    procedure   FormDestroy(Sender: TObject);
    procedure   FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure   FormPaint(Sender: TObject);
  protected
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState;
       var consumed: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
  end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Timer.Free;
  SpriteImg.Free;
  Background.Free;
end;

procedure TMainForm.TimerFired(Sender: TObject);
begin
  if csDestroying in ComponentState then
    Exit;
  Repaint;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer.Enabled := False;
end;

procedure TMainForm.FormPaint(Sender: TObject);
var
  t: Double;
  x: Int64;
  y: Integer;
  CenterX: Integer;
  CenterY: Integer;
begin
  // paint background
  Canvas.DrawImage(0, 0, Background);

  // calculate sprite position
  CenterX:=Background.Width div 2;
  CenterY:=Background.Height div 2;
  t := Now*86400;
  x := CenterX+round(cos(t)*CenterX*2/3)-(SpriteImg.Width div 2);
  y := CenterY+round(sin(t*0.7)*CenterY*2/3)-(SpriteImg.Height div 2);
  
  // paint sprite
  Canvas.DrawImage(x, y, SpriteImg);
  
  // paint debug info
  if FShowInterval then
  begin
    Canvas.TextColor := clWhite;
    Canvas.DrawText(4, 4, 'Timer Interval: ' + IntToStr(Timer.Interval));
  end;
end;

procedure TMainForm.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  interval: integer;
begin
  case keycode of
    keyPageUp:  // increase timer interval
        begin
          interval := Timer.Interval;
          inc(interval, 10);
          Timer.Interval := interval;
          consumed := True;
        end;
    keyPageDown:  // decrease timer interval
        begin
          interval := Timer.Interval;
          dec(interval, 10);
          if interval < 10 then
            interval := 10;
          Timer.Interval := interval;
          consumed := True;
        end;
    keyEscape:  // exit application
        begin
          Close;
        end;
  end;

  if KeycodeToText(keycode, shiftstate) = 'D' then  // show debug info
  begin
    FShowInterval := True;
    consumed := True;
  end;

  if not consumed then
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowInterval := False;
  
  OnPaint   := @FormPaint;
  OnClose   := @FormClose;
  OnDestroy := @FormDestroy;
  
  Background := LoadImage_BMP('background.bmp');
  SpriteImg := LoadImage_BMP('ufo.bmp');
  SpriteImg.CreateMaskFromSample(0, 0);
  SpriteImg.UpdateImage;

  Timer := TfpgTimer.Create(50);
  Timer.OnTimer := @TimerFired;
  Timer.Enabled := True;
end;

procedure TMainForm.AfterCreate;
begin
  Name := 'MainForm';
  SetPosition(316, 186, 429, 341);
  WindowTitle := 'Sprite Demo';
  WindowPosition := wpScreenCenter;
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.


