unit frmvlcplayer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_panel, fpg_button, fpg_main, fpg_form,
  fpg_editbtn, fpg_memo, fpg_label, vlc, fpg_vlc;

type

  { TVLCPlayerDemoForm }

  TVLCPlayerDemoForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: VLCPlayerDemo}
    Panel1: TfpgPanel;
    FilenameEdit1: TfpgFileNameEdit;
    Label1: TfpgLabel;
    Button1: TfpgButton;
    Button2: TfpgButton;
    Button3: TfpgButton;
    Button4: TfpgButton;
    Memo1: TfpgMemo;
    lblTimeLapse: TfpgLabel;
    {@VFD_HEAD_END: VLCPlayerDemo}
    procedure Sync;
    procedure DoGUIUpdateTimeLapse;
  public
    P :  TFpgVLCPlayer;
    FMsg: String;
    procedure AfterCreate; override;
    Procedure InitPlayer;
    Procedure Log(Const Msg : String);
    Procedure UpdateTimeLapse(const Msg: String);
    Procedure DoPlay(sender : TObject);
    Procedure DoPause(sender : TObject);
    Procedure DoResume(sender : TObject);
    Procedure DoStop(sender : TObject);
    // Event callbacks
    procedure DoOnBackward(Sender: TObject);
    procedure DoOnMediaChanged(Sender: TObject);
    procedure DoOnNothingSpecial(Sender: TObject);
    procedure DoOnBuffering(Sender: TObject);
    procedure DoOnEOF(Sender: TObject);
    procedure DoOnError(Sender: TObject; const AError: string);
    procedure DoOnForward(Sender: TObject);
    procedure DoOnLengthChanged(Sender: TObject; const time: TDateTime);
    procedure DoOnOpening(Sender: TObject);
    procedure DoOnPause(Sender: TObject);
    procedure DoOnPlaying(Sender: TObject);
    procedure DoOnStop(Sender: TObject);
    procedure DoOnPausableChanged(Sender: TObject; const AValue: Boolean);
    procedure DoOnPositionChanged(Sender: TObject; const APos: Double);
    procedure DoOnSeekableChanged(Sender: TObject; const AValue: Boolean);
    procedure DoOnTimeChanged(Sender: TObject; const time: TDateTime);
    procedure DoOnSnapshot(Sender: TObject; const AfileName: string);
    procedure DoOnTitleChanged(Sender: TObject; const ATitle: Integer);
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TVLCPlayerDemoForm.DoOnBackward(Sender: TObject);
begin
  Log('Backward');
end;

procedure TVLCPlayerDemoForm.DoOnMediaChanged(Sender: TObject);
begin
  Log('Media changed');
end;

procedure TVLCPlayerDemoForm.DoOnNothingSpecial(Sender: TObject);
begin
  Log('Idle');
end;

procedure TVLCPlayerDemoForm.DoOnBuffering(Sender: TObject);
begin
  Log('Buffering');
end;

procedure TVLCPlayerDemoForm.DoOnEOF(Sender: TObject);
begin
  Log('EOF');
end;

procedure TVLCPlayerDemoForm.DoOnError(Sender: TObject; const AError: string);
begin
  Log('Error : '+AError);
end;

procedure TVLCPlayerDemoForm.DoOnForward(Sender: TObject);
begin
  Log('Forward');
end;

procedure TVLCPlayerDemoForm.DoOnLengthChanged(Sender: TObject;
  const time: TDateTime);
begin
  Log('Length changed : '+TimeToStr(Time));
end;

procedure TVLCPlayerDemoForm.DoOnOpening(Sender: TObject);
begin
  Log('Opening');
end;

procedure TVLCPlayerDemoForm.DoOnPause(Sender: TObject);
begin
  Log('Pause');
end;

procedure TVLCPlayerDemoForm.DoOnPlaying(Sender: TObject);
begin
  Log('Playing');
end;

procedure TVLCPlayerDemoForm.DoOnStop(Sender: TObject);
begin
  Log('Stop');
end;

procedure TVLCPlayerDemoForm.DoOnPausableChanged(Sender: TObject;
  const AValue: Boolean);
begin
  Log('Pausable changed : '+BoolToStr(AValue,True));
end;

procedure TVLCPlayerDemoForm.DoOnPositionChanged(Sender: TObject;
  const APos: Double);
begin
  Log('Position changed : '+FloatToStr(APos));
end;

procedure TVLCPlayerDemoForm.DoOnSeekableChanged(Sender: TObject;
  const AValue: Boolean);
begin
  Log('Seekable changed : '+BoolToStr(AValue,True));
end;

procedure TVLCPlayerDemoForm.DoOnTimeChanged(Sender: TObject;
  const time: TDateTime);
begin
  Log('Time changed : '+TimeToStr(Time));
  UpdateTimeLapse(TimeToStr(time));
end;

procedure TVLCPlayerDemoForm.DoOnSnapshot(Sender: TObject;
  const AfileName: string);
begin
  Log('Wrote snapshot to file : '+AFileName);
end;

procedure TVLCPlayerDemoForm.DoOnTitleChanged(Sender: TObject;
  const ATitle: Integer);
begin
  Log('Title changed : '+IntToStr(ATitle));
end;

procedure TVLCPlayerDemoForm.AfterCreate;
begin

  {@VFD_BODY_BEGIN: VLCPlayerDemo}
  Name := 'VLCPlayerDemo';
  SetPosition(424, 319, 813, 574);
  WindowTitle := 'VLCPlayerDemo';
  Hint := '';
  IconName := '';

  Panel1 := TfpgPanel.Create(self);
  with Panel1 do
  begin
    Name := 'Panel1';
    SetPosition(28, 40, 754, 422);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Select a video';
  end;

  FilenameEdit1 := TfpgFileNameEdit.Create(self);
  with FilenameEdit1 do
  begin
    Name := 'FilenameEdit1';
    SetPosition(108, 8, 566, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FileName := '';
    Filter := 'Video files|*.avi;*.flv;*.mp4;*.mkv|Audio files|*.mp3;*.acc;*.flac;*.ogg';
    InitialDir := '';
    TabOrder := 2;
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(32, 12, 72, 20);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Play file:';
  end;

  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(698, 8, 80, 24);
    Anchors := [anRight,anTop];
    Text := 'Play';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
    OnClick:=@DoPlay;
  end;

  Button2 := TfpgButton.Create(self);
  with Button2 do
  begin
    Name := 'Button2';
    SetPosition(28, 472, 80, 28);
    Anchors := [anLeft,anBottom];
    Text := 'Pause';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 5;
    OnClick:=@DoPause;
  end;

  Button3 := TfpgButton.Create(self);
  with Button3 do
  begin
    Name := 'Button3';
    SetPosition(116, 472, 80, 28);
    Anchors := [anLeft,anBottom];
    Text := 'Resume';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 6;
    OnClick:=@DoResume;
  end;

  Button4 := TfpgButton.Create(self);
  with Button4 do
  begin
    Name := 'Button4';
    SetPosition(204, 472, 80, 28);
    Anchors := [anLeft,anBottom];
    Text := 'Stop';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 7;
    OnClick:=@DoStop;
  end;

  Memo1 := TfpgMemo.Create(self);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(288, 469, 494, 100);
    Anchors := [anLeft,anRight,anBottom];
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 8;
  end;

  lblTimeLapse := TfpgLabel.Create(self);
  with lblTimeLapse do
  begin
    Name := 'lblTimeLapse';
    SetPosition(30, 510, 105, 15);
    FontDesc := 'Liberation Sans-12:bold:antialias=true';
    Hint := '';
    Text := 'Label';
  end;

  {@VFD_BODY_END: VLCPlayerDemo}
  {%endregion}
end;

procedure TVLCPlayerDemoForm.InitPlayer;
begin
  If P<>Nil then
    exit;
  P:=TFpgVLCPlayer.Create(Self);
  P.UseEvents:=True;
  P.ParentWindow:=Panel1;
  P.OnMediaChanged:=@DoOnMediaChanged;
  P.OnNothingSpecial:=@DoOnNothingSpecial;
  P.OnBackward:=@DoOnBackward;
  P.OnBuffering:=@DoOnBuffering;
  P.OnEOF:=@DoOnEOF;
  P.OnError:=@DoOnError;
  P.OnForward:=@DoOnForward;
  P.OnOpening:=@DoOnOpening;
  P.OnPause:=@DoOnPause;
  P.OnPlaying:=@DoOnPlaying;
  P.OnStop:=@DoOnStop;
  P.OnLengthChanged:=@DoOnLengthChanged;
  P.OnTimeChanged:=@DoOnTimeChanged;
  P.OnPausableChanged:=@DoOnPausableChanged;
  P.OnPositionChanged:=@DoOnPositionChanged;
  P.OnSeekableChanged:=@DoOnSeekableChanged;
  P.OnTitleChanged:=@DoOnTitleChanged;
  P.OnSnapshot:=@DoOnSnapshot;

end;

procedure TVLCPlayerDemoForm.Sync;
begin
  Memo1.Lines.Add(FMsg);
end;

procedure TVLCPlayerDemoForm.DoGUIUpdateTimeLapse;
begin
  lblTimeLapse.Text := FMsg;
end;

procedure TVLCPlayerDemoForm.Log(const Msg: String);
begin
  FMsg:=Msg;
  TThread.Synchronize(Nil,@Self.Sync);
end;

procedure TVLCPlayerDemoForm.UpdateTimeLapse(const Msg: String);
begin
  FMsg := Msg;
  TThread.Synchronize(nil, @self.DoGUIUpdateTimeLapse);
// This could also be used in FPC 3.0+
//  TThread.Queue(nil, @DoGUIUpdateTimeLapse);
end;

procedure TVLCPlayerDemoForm.DoPlay(sender: TObject);
begin
  InitPlayer;
  P.PlayFile(FileNameEdit1.FileName);
end;

procedure TVLCPlayerDemoForm.DoPause(sender: TObject);
begin
  If Assigned(P) then
    P.Pause;
end;

procedure TVLCPlayerDemoForm.DoResume(sender: TObject);
begin
  if Assigned(P) then
    P.Resume;
end;

procedure TVLCPlayerDemoForm.DoStop(sender: TObject);
begin
  If Assigned(P) then
    P.Stop;
end;


end.
