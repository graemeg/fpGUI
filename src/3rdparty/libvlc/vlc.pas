unit vlc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, libvlc, syncobjs;

Type

  { TVLCLibrary }

  TVLCLibrary = class(TComponent)
  private
    FInstance : plibvlc_instance_t;
    FLibraryArgs: TStrings;
    FLibraryPath : String;
    function GetI: Boolean;
    function GetLastError: String;
    Function GetVersion : String;
    Function GetCompiler : String;
    Function GetChangeSet : String;
    Procedure SetLibraryPath(Const AValue : String);
  Protected
    Function GetInstance : plibvlc_instance_t; virtual;
    property Instance : plibvlc_instance_t read GetInstance;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    Procedure Initialize;
    Procedure Release;
    property LastError : String read GetLastError;
    property Version : String read GetVersion;
    property Compiler : String read GetCompiler;
    property ChangeSer : String read GetChangeSet;
    property LibraryPath : String read FLibraryPath write SetLibraryPath;
    Property LibraryArgs : TStrings read FLibraryArgs Write FLibraryArgs;
    Property Initialized : Boolean Read GetI;
  end;

  TVLCLibraryClass = Class of TVLCLibrary;

  TCustomVLCMediaPlayer = Class;
  TVLCMediaItems = Class;

  { TVLCMediaItem }
  TSnapShotFormat = (ssfPNG,ssfJPG);
  TDeinterlaceMode   = (dmBlend, dmDiscard, dmBob, dmLinear, dmMean, dmX, dmYadif, dmYadif2x);

  TVLCMediaItem = Class(TCollectionItem)
  private
    FDIM: TDeinterlaceMode;
    FInstance : plibvlc_media_t;
    FOpts : Array [0..3] of Boolean; // Keep in sync with property indexes
    FPath: String;
    FSS: TSNapshotFormat;
    FFD: Integer;
    function GetInstance: plibvlc_media_t;
    function GetM(AIndex: Integer): Boolean;
    function GetMD(AMeta : libvlc_meta_t): String;
    function GetMRL: String;
    function GetParsed: Boolean;
    function GetUD: Pointer;
    procedure SetDIM(AValue: TDeinterlaceMode);
    procedure SetFSS(AValue: TSNapshotFormat);
    procedure SetM(AIndex: Integer; AValue: Boolean);
    Function GetBoolOpt(AIndex : Integer; AValue : Boolean) : String;
    procedure SetMD(AMeta : libvlc_meta_t; AValue: String);
    procedure SetUD(AValue: Pointer);
    function GetState : libvlc_state_t;
    function GetDuration : TDateTime;
  Protected
    Procedure RegisterInstance;
    Procedure UnRegisterInstance;
    procedure SetMRL(AValue: String); virtual;
    procedure SetPath(AValue: String); virtual;
    procedure SetFD(AValue: Integer); virtual;
    Function GetVLC : TVLCLibrary; virtual;
    function GetEventManager : plibvlc_event_manager_t;
    Procedure SetInstance( Avalue : plibvlc_media_t);
    Property Instance : plibvlc_media_t Read GetInstance;
  Public
    Destructor Destroy; override;
    Procedure AddOption(Const AValue : String);
    procedure Parse;
    procedure ParseAsync;
    Procedure SaveMetaData;
    Function GetStats(Var AStats : libvlc_media_stats_t) : Boolean;
    Function Duplicate : TVLCMediaItem;
    Property Parsed : Boolean Read GetParsed;
    Property ShowTitle : Boolean Index 0 Read GetM Write SetM;
    Property VideoOnTop : Boolean Index 1 Read GetM Write SetM;
    Property UseOverlay : Boolean Index 2 Read GetM Write SetM;
    Property FullScreen : Boolean Index 3 Read GetM Write SetM;
    Property DeinterlaceFilter : Boolean Index 4 Read GetM Write SetM;
    Property DeInterlaceMode : TDeinterlaceMode Read FDIM Write SetDIM;
    Property SnapShotFormat : TSNapshotFormat Read FSS Write SetFSS;
    Property UserData : Pointer Read GetUD Write SetUD;
    Property State :  libvlc_state_t Read GetState;
    Property Duration : TDateTime Read GetDuration;
    Property MetaData[AMeta : libvlc_meta_t] : String Read GetMD Write SetMD;
    // These must be set prior to using any of the above.
    Property MRL : String Read GetMRL Write SetMRL;
    Property Path : String Read FPath Write SetPath;
    property FileDescriptor : Integer Read FFD Write SetFD;
  end;

  TVLCMediaItemClass = Class of TVLCMediaItem;

  { TVLCMediaItems }
  TVLCPlayMode = (pmNormal,pmLoop,pmRepeat);

  TVLCMediaItems = Class(TCollection)
  Private
    FPlayer: TCustomVLCMediaPlayer;
    FPlayMode: TVLCPlayMode;
    FVLC : TVLCLibrary;
    FInstance : Plibvlc_media_list_t;
    function GetI(AIndex : Integer): TVLCMediaItem;
    function GetInstance: Plibvlc_media_list_t;
    function GetIsReadOnly: Boolean;
    procedure SetI(AIndex : Integer; AValue: TVLCMediaItem);
  Protected
    Function GetVLC : TVLCLibrary; virtual;
  Public
    Constructor Create(ALibrary : TVLCLibrary;AItemClass: TVLCMediaItemClass = Nil); overload;
    Constructor Create(AInstance : Plibvlc_media_list_t;AItemClass: TVLCMediaItemClass = Nil); overload;
    Procedure Lock;
    Procedure Unlock;
    Property Instance : Plibvlc_media_list_t read GetInstance;
    Property VLC : TVLCLibrary Read GetVLC Write FVLC;
    Property MediaItems[AIndex : Integer] : TVLCMediaItem Read GetI Write SetI; default;
    Property ReadOnly : Boolean Read GetIsReadOnly;
  end;

  { TCustomVLCMediaPlayer }
  TBooleanEvent = procedure(Sender : TObject; Const AValue : Boolean) of object;
  TTitleEvent = procedure(Sender : TObject; Const ATitle : Integer) of object;
  TSnapshotEvent = procedure(Sender : TObject; Const AfileName : string) of object;
  TErrorEvent = procedure(Sender : TObject; Const AError : string) of object;
  TTimeEvent = procedure(Sender : TObject; Const time : TDateTime) of object;
  TPositionEvent = procedure(Sender : TObject; Const APos : Double)  of object;

  TCustomVLCMediaPlayer = Class(TComponent)
  private
    FFitWindow: Boolean;
    FOnBackward: TNotifyEvent;
    FOnBuffering: TNotifyEvent;
    FOnEOF: TNotifyEvent;
    FOnError: TErrorEvent;
    FOnForward: TNotifyEvent;
    FOnLengthChanged: TTimeEvent;
    FOnMediaChanged: TNotifyEvent;
    FOnNothingSpecial: TNotifyEvent;
    FOnOpening: TNotifyEvent;
    FOnPausableChanged: TBooleanEvent;
    FOnPause: TNotifyEvent;
    FOnPlaying: TNotifyEvent;
    FOnPositionChanged: TPositionEvent;
    FOnSeekableChanged: TBooleanEvent;
    FOnSnapShot: TSnapShotEvent;
    FOnStop: TNotifyEvent;
    FOnTimeChanged: TTimeEvent;
    FOnTitleChanged: TTitleEvent;
    FUseEvents: Boolean;
    Finstance : Plibvlc_media_player_t;
    FVLC: TVLCLibrary;
    FECS : TCriticalSection;
    function GetAspectRatio: String;
    function GetAudioMuted: Boolean;
    function GetAudioTrack: Integer;
    function GetHaveInstance: Boolean;
    function GetState: libvlc_state_t;
    function GetVideoDuration: TDateTime;
    function GetVideoFPS: Double;
    function GetVideoFractional: Double;
    function GetVideoHeight: Cardinal;
    function GetVideoLength: Int64;
    function GetVideoPos: Int64;
    function GetVideoScale: Double;
    function GetVideoWidth: Cardinal;
    function GetVLC: TVLCLibrary;
    procedure SetAspectRatio(AValue: String);
    procedure SetAudioMuted(AValue: Boolean);
    procedure SetFitWindow(AValue: Boolean);
    procedure SetUseEVents(AValue: Boolean);
    function GetAudioTrackCount : Integer;
    procedure SetAudioTrack(AValue: Integer);
    function GetAudioTrackDescriptions(AIndex : Integer) : String;
    function GetChannel: Integer;
    procedure SetChannel(AValue : Integer);
    function GetAudioDelay : Int64;
    procedure SetAudioDelay (AValue: Int64);
    function GetPlaying : Boolean;
    function GetChapter : Integer;
    procedure SetChapter(AValue : Integer);
    function GetChapterCount: Integer;
    Function GetPlayable : Boolean;
    Function GetPausable : Boolean;
    Function GetSeekable : Boolean;
    function GetAudioVolume : Integer;
    function GetPlayRate: Integer;
    procedure SetAudioVolume(AValue : Integer);
    procedure SetPlayRate(AValue: Integer);
    procedure SetFullScreenMode(AValue: Boolean);
    function  GetFullScreenMode: Boolean;
    procedure SetVideoFractional(AValue: Double);
    procedure SetVideoPos(AValue: Int64);
    procedure SetVideoScale(AValue: Double);
  Protected
    function GetInstance: Plibvlc_media_player_t; virtual;
    // Called to set parent window. Descendents must override this.
    Procedure SetParentWindow; virtual;
    // Called when FitWindow is true.
    Procedure SetParentWindowSize(AWidth,AHeight : Cardinal); Virtual;
    procedure DoMediaChanged; virtual;
    procedure DoNothingSpecial; virtual;
    procedure DoOnBackward; virtual;
    procedure DoOnBuffering;virtual;
    procedure DoOnEOF;virtual;
    procedure DoOnError;virtual;
    procedure DoOnForward;virtual;
    procedure DoOnOpening;virtual;
    procedure DoOnPause;virtual;
    procedure DoOnPlaying;virtual;
    procedure DoOnStop;virtual;
    procedure DoOnLengthChanged(const ATime: libvlc_time_t); virtual;
    procedure DoOnPausableChanged(const APausable: Boolean); virtual;
    procedure DoOnPositionChanged(const Aposition: Double); virtual;
    procedure DoOnSeekableChanged(const ASeekable: Boolean); virtual;
    procedure DoOnTimeChanged(const ATime: libvlc_time_t); virtual;
    procedure DoOnTitleChanged(const ATitle: cint); virtual;
    procedure DoOnSnapshot(const AFileName: PCChar); virtual;
    procedure HookupEvents; virtual;
    procedure UnHookEvents; virtual;
    procedure HandleVLCEvent(e: Plibvlc_event_t); virtual;
    Property VLC : TVLCLibrary Read GetVLC Write FVLC;
    Property Instance : Plibvlc_media_player_t Read GetInstance;
    Property HaveInstance : Boolean Read GetHaveInstance;
  Public
    Destructor Destroy; override;
    procedure Play;
    procedure SetMedia(M: TVLCMediaItem);
    Procedure Play(M : TVLCMediaItem);
    Procedure PlayFile(Const AFileName : String);
    Procedure Stop;
    procedure Pause;
    procedure Resume;
    procedure NextFrame;
    function Snapshot(Const AFileName: String): Boolean;
    function Snapshot(Const AFileName: String; AWidth, AHeight: Cardinal): Boolean;
    function GetVideoSize(Var AWidth, AHeight: Cardinal): Boolean;
  // These can be made public/published in descendents
  Protected
    Property Playable : Boolean Read GetPlayable;
    Property Pausable : Boolean Read GetPausable;
    Property Seekable : Boolean Read GetSeekable;
    Property Playing : Boolean Read GetPlaying;
    Property State : libvlc_state_t Read GetState;
    Property AudioTrackDescriptions [AIndex : Integer] : String Read GetAudioTrackDescriptions;
    Property ChapterCount : Integer Read GetChapterCount;
    Property AudioTrackCount : Integer Read GetAudioTrackCount;
    Property AudioTrack : Integer Read GetAudioTrack Write SetAudioTrack;
    Property AudioDelay : Int64 Read GetAudioDelay Write SetAudioDelay;
    Property AudioVolume : Integer Read GetAudioVolume Write SetAudioVolume;
    Property AudioMuted : Boolean Read GetAudioMuted Write SetAudioMuted;
    Property FitWindow : Boolean Read FFitWindow Write SetFitWindow;
    Property VideoWidth : Cardinal Read GetVideoWidth;
    Property VideoHeight : Cardinal Read GetVideoHeight;
    // In MS.
    Property VideoLength : Int64 Read GetVideoLength;
    Property VideoDuration : TDateTime Read GetVideoDuration;
    // In MS
    Property VideoPosition : Int64 Read GetVideoPos Write SetVideoPos;
    Property VideoFractionalPosition : Double Read GetVideoFractional Write SetVideoFractional;
    Property VideoFramesPerSecond : Double Read GetVideoFPS;
    Property VideoScale : Double Read GetVideoScale Write SetVideoScale;
    Property AspectRatio : String Read GetAspectRatio Write SetAspectRatio;
    Property Channel : Integer Read GetChannel Write SetChannel;
    Property Chapter : Integer Read GetChapter Write SetChapter;
    Property FullScreenMode : Boolean Read GetFullScreenMode Write SetFullScreenMode;
    Property UseEvents : Boolean Read FUseEvents Write SetUseEVents;
    // Events from VLC player
    Property OnMediaChanged : TNotifyEvent Read FOnMediaChanged Write FOnMediaChanged;
    Property OnNothingSpecial : TNotifyEvent Read FOnNothingSpecial Write FOnNothingSpecial;
    Property OnBackward : TNotifyEvent Read FOnBackward Write FOnBackward;
    Property OnBuffering : TNotifyEvent Read FOnBuffering Write FOnBuffering;
    Property OnEOF : TNotifyEvent Read FOnEOF Write FOnEOF;
    Property OnError : TErrorEvent Read FOnError Write FOnError;
    Property OnForward : TNotifyEvent Read FOnForward Write FOnForward;
    Property OnOpening : TNotifyEvent Read FOnOpening Write FOnOpening;
    Property OnPause : TNotifyEvent Read FOnPause Write FOnPause;
    Property OnPlaying : TNotifyEvent Read FOnPlaying Write FOnPlaying;
    Property OnStop : TNotifyEvent Read FOnStop Write FOnStop;
    Property OnLengthChanged : TTimeEvent Read FOnLengthChanged Write FOnLengthChanged;
    Property OnTimeChanged : TTimeEvent Read FOnTimeChanged Write FOnTimeChanged;
    Property OnPausableChanged : TBooleanEvent Read FOnPausableChanged Write FOnPausableChanged;
    Property OnPositionChanged : TPositionEvent Read FOnPositionChanged Write FOnPositionChanged;
    Property OnSeekableChanged : TBooleanEvent Read FOnSeekableChanged Write FOnSeekableChanged;
    Property OnTitleChanged : TTitleEvent Read FOnTitleChanged Write FOnTitleChanged;
    Property OnSnapshot : TSnapShotEvent Read FOnSnapShot Write FOnSnapShot;
  end;

  EVLC = Class(Exception);

  TVLCMediaPlayer = Class(TCustomVLCMediaPlayer)
  Public
    Property Playable ;
    Property Pausable ;
    Property Seekable ;
    Property PLaying ;
    Property State ;
    Property AudioTrackDescriptions;
    Property ChapterCount ;
    Property AudioTrackCount ;
    Property AudioTrack ;
    Property VideoWidth ;
    Property VideoHeight;
    Property VideoLength;
    Property VideoDuration ;
    Property VideoPosition ;
    Property VideoFractionalPosition ;
    Property VideoFramesPerSecond;
    Property VideoScale;
    Property AspectRatio;
  Published
    Property AudioDelay ;
    Property AudioVolume ;
    Property AudioMuted ;
    Property Channel ;
    Property Chapter ;
    Property FitWindow;
    Property FullScreenMode ;
    Property UseEvents ;
    Property OnMediaChanged ;
    Property OnNothingSpecial ;
    Property OnBackward ;
    Property OnBuffering ;
    Property OnEOF ;
    Property OnError ;
    Property OnForward ;
    Property OnOpening ;
    Property OnPause ;
    Property OnPlaying ;
    Property OnStop ;
    Property OnLengthChanged ;
    Property OnTimeChanged ;
    Property OnPausableChanged ;
    Property OnPositionChanged ;
    Property OnSeekableChanged ;
    Property OnTitleChanged ;
    Property OnSnapshot ;
  end;

  { TCustomVLCMediaListPlayer }

  TCustomVLCMediaListPlayer = Class(TComponent)
  Private
    FMediaItems: TVLCMediaItems;
    FPlayer: TCustomVLCMediaPlayer;
    FPlayMode: TVLCPlayMode;
    FInstance : plibvlc_media_list_player_t;
    FVLC: TVLCLibrary;
    function GetInstance: plibvlc_media_list_player_t;
    function  GetPlaying : Boolean;
    function  GetState: libvlc_state_t;
    function GetVLC: TVLCLibrary;
    procedure SetMediaItems(AValue: TVLCMediaItems);
    procedure SetPlayer(AValue: TCustomVLCMediaPlayer); virtual;
    procedure SetPlayMode(AValue: TVLCPlayMode);
  Protected
    Function CreateMediaItems : TVLCMediaItems; virtual;
    Property Instance : plibvlc_media_list_player_t Read GetInstance;
    Property Player : TCustomVLCMediaPlayer Read FPlayer write SetPlayer;
    Property PlayMode : TVLCPlayMode read FPlayMode write SetPlayMode;
    Property Playing : Boolean Read GetPLaying;
    Property State : libvlc_state_t Read GetState;
    Property MediaItems : TVLCMediaItems Read FMediaItems Write SetMediaItems;
    Property VLC : TVLCLibrary Read GetVLC Write FVLC;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    procedure Play(Item : TVLCMediaItem);
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure Next;
    procedure Prev;
  end;

  TVLCMediaListPlayer = Class(TCustomVLCMediaListPlayer)
  Public
    Property VLC;
  Published
    Property Player;
    Property PlayMode;
    Property Playing;
    Property State;
    Property MediaItems;
  end;

Function VLCLibrary  : TVLCLibrary;

Var
  VLCLibraryClass : TVLCLibraryClass = TVLCLibrary;

Function VLCTimeToDateTime (T : libvlc_time_t) : TDateTime;

implementation

{ TVLCLibrary }
Var
  LVLC : TVLCLibrary;

Function VLCLibrary  : TVLCLibrary;

begin
  If LVLC=Nil then
    LVLC:=VLCLibraryClass.Create(Nil);
  Result:=LVLC;
end;

Procedure DoneVLC;

begin
  If Assigned(LVLC) then
    FreeAndNil(LVLC);
end;

Function VLCTimeToDateTime (T : libvlc_time_t) : TDateTime;

  Function MD(Var MS : libvlc_time_t; D : Integer) : Word;  inline;

  begin
    Result:=MS Mod D;
    MS:=MS div D;
  end;

var
  d,h,m,s,ms: word;

begin
  ms:=MD(T,1000);
  s:=MD(T,60);
  m:=MD(T,60);
  h:=MD(T,24);
  d:=T;
  Result:=D+EncodeTime(h,m,s,ms);
end;

procedure PlayerEventHelper(event: Plibvlc_event_t; data: Pointer); cdecl;

begin
  if Not Assigned(data) then
    exit;
  TCustomVLCMediaPlayer(data).HandleVLCEvent(event);
end;

{ TCustomVLCMediaListPlayer }

function TCustomVLCMediaListPlayer.GetPlaying: Boolean;
begin
  Result:=libvlc_media_list_player_is_playing(Instance)<>0;
end;

function TCustomVLCMediaListPlayer.GetInstance: plibvlc_media_list_player_t;
begin
  if (FInstance=Nil) then
    begin
    Finstance:=libvlc_media_list_player_new(VLC.Instance);
    if Assigned(MediaItems) then
      begin
      libvlc_media_list_player_set_media_list(FInstance,MediaItems.Instance);
      end;
    If Assigned(FPlayer) then
      begin
      libvlc_media_list_player_set_media_player(FInstance, FPlayer.Instance);
      end;
    end;
  Result:=FInstance;
end;

function TCustomVLCMediaListPlayer.GetState: libvlc_state_t;
begin
  Result:=libvlc_media_list_player_get_state(Instance)
end;

function TCustomVLCMediaListPlayer.GetVLC: TVLCLibrary;
begin
  Result:=FVLC;
  If Result=Nil then
    Result:=VLCLibrary;
end;

procedure TCustomVLCMediaListPlayer.Play(Item: TVLCMediaItem);
begin
  libvlc_media_list_player_play_item(Instance, item.Instance);
end;

procedure TCustomVLCMediaListPlayer.SetMediaItems(AValue: TVLCMediaItems);
begin
  if FMediaItems=AValue then Exit;
  FMediaItems.Assign(AValue);
end;

procedure TCustomVLCMediaListPlayer.SetPlayer(AValue: TCustomVLCMediaPlayer);
begin
  if FPlayer=AValue then Exit;
  FPlayer:=AValue;
  If Assigned(FInstance) then
    begin
    libvlc_media_list_player_set_media_player(FInstance, FPlayer.Instance);
    end;
end;

procedure TCustomVLCMediaListPlayer.SetPlayMode(AValue: TVLCPlayMode);
Const
  M : Array  [TVLCPlayMode] of libvlc_playback_mode_t
    = (libvlc_playback_mode_default,
       libvlc_playback_mode_loop,
       libvlc_playback_mode_repeat);

begin
  if FPlayMode=AValue then Exit;
  FPlayMode:=AValue;
  libvlc_media_list_player_set_playback_mode(FInstance, M[AValue]);
end;

function TCustomVLCMediaListPlayer.CreateMediaItems: TVLCMediaItems;
begin
  Result:=TVLCMediaItems.Create(TVLCMediaItem);
end;

constructor TCustomVLCMediaListPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMediaItems:=CreateMediaItems;
end;

destructor TCustomVLCMediaListPlayer.Destroy;
begin
  If Assigned(Finstance) then
    libvlc_media_list_player_release(FInstance);
  FreeAndNil(FMediaItems);
  inherited Destroy;
end;

procedure TCustomVLCMediaListPlayer.Play;
begin
  libvlc_media_list_player_play(Instance);
end;

procedure TCustomVLCMediaListPlayer.Pause;
begin
  libvlc_media_list_player_pause(Instance);
end;

procedure TCustomVLCMediaListPlayer.Stop;
begin
  libvlc_media_list_player_stop(Instance);
end;

procedure TCustomVLCMediaListPlayer.Next;
begin
  libvlc_media_list_player_next(Instance);
end;

procedure TCustomVLCMediaListPlayer.Prev;
begin
  libvlc_media_list_player_previous(Instance);
end;


{ TCustomVLCMediaPlayer }

function TCustomVLCMediaPlayer.GetVLC: TVLCLibrary;
begin
  Result:=FVLC;
  if Result=Nil then
    Result:=VLCLibrary;
end;

procedure TCustomVLCMediaPlayer.SetAspectRatio(AValue: String);

begin
  libvlc_video_set_aspect_ratio(Instance,Pcchar(PChar(AValue)));
end;

function TCustomVLCMediaPlayer.GetAudioMuted: Boolean;
begin
  if Assigned(Finstance) then
    Result:=libvlc_audio_get_mute(instance)<>0
  else
    Result:=False;
end;

function TCustomVLCMediaPlayer.GetAspectRatio: String;

Var
  P : Pcchar;

begin
  P:=libvlc_video_get_aspect_ratio(Instance);
  if (P<>Nil) then
    Result:=StrPas(PChar(P))
  else
    Result:='';
end;

function TCustomVLCMediaPlayer.GetAudioTrack: Integer;
begin
  if Assigned(FInstance) then
    Result := libvlc_audio_get_track(FINstance)
  else
    Result:=-1;
end;

function TCustomVLCMediaPlayer.GetHaveInstance: Boolean;
begin
  Result:=(FInstance<>Nil);
end;

function TCustomVLCMediaPlayer.GetState: libvlc_state_t;
begin
  If Assigned(FInstance) then
    Result:=libvlc_media_player_get_state(FInstance)
  else
    Result:=libvlc_NothingSpecial;
end;

function TCustomVLCMediaPlayer.GetVideoDuration: TDateTime;
begin
  Result:=VLCTimeToDateTime(GetVideoLength);
end;

function TCustomVLCMediaPlayer.GetVideoFPS: Double;
begin
  Result:=libvlc_media_player_get_fps(FInstance);
end;

function TCustomVLCMediaPlayer.GetVideoFractional: Double;
begin
  Result:=libvlc_media_player_get_Position(FInstance);
end;

function TCustomVLCMediaPlayer.GetVideoHeight: Cardinal;
begin
  Result:=libvlc_video_get_height(FInstance);
end;

function TCustomVLCMediaPlayer.GetVideoLength: Int64;
begin
  Result:=libvlc_media_player_get_length(Finstance);
end;

function TCustomVLCMediaPlayer.GetVideoPos: Int64;
begin
  Result:=libvlc_media_player_get_time(FInstance);
end;

function TCustomVLCMediaPlayer.GetVideoScale: Double;
begin
  Result:=libvlc_video_get_scale(Finstance);
end;

function TCustomVLCMediaPlayer.GetVideoWidth: Cardinal;
begin
  Result:=libvlc_video_get_width(FInstance);
end;


procedure TCustomVLCMediaPlayer.SetAudioMuted(AValue: Boolean);
begin
  libvlc_audio_set_mute(instance, ord(AValue));
end;

procedure TCustomVLCMediaPlayer.SetFitWindow(AValue: Boolean);

Var
  W,H : Cardinal;

begin
  if FFitWindow=AValue then Exit;
  FFitWindow:=AValue;
  If FFitWindow and Playing then
    begin
    if GetVideoSize(W,H) then
      SetParentWindowSize(W,H);
    end;
end;

procedure TCustomVLCMediaPlayer.SetUseEVents(AValue: Boolean);
begin
  if FUseEvents=AValue then Exit;
  FUseEvents:=AValue;
  If Assigned(Finstance) then
    If AValue then
      HookupEvents
    else
      UnhookEvents;
end;

function TCustomVLCMediaPlayer.GetAudioTrackCount: Integer;
begin
  if Assigned(FInstance) then
    Result := libvlc_audio_get_track_count(FINstance)
  else
    Result:=-1;
end;


procedure TCustomVLCMediaPlayer.SetAudioTrack(AValue: Integer);
begin
  if Assigned(FInstance) then
    begin
    if (AValue<0) then
      AValue:=0;
    libvlc_audio_set_track(FInstance,AValue);
    end;
end;

function TCustomVLCMediaPlayer.GetAudioTrackDescriptions(AIndex: Integer): String;

var
  t : plibvlc_track_description_t;

begin
  Result := '';
  If (AIndex>=0) And Assigned(FInstance) then
    begin
    T:=libvlc_audio_get_track_description(Finstance);
    while (AIndex>0) and Assigned(t) do
      begin
      Dec(Aindex);
      t:=t^.p_next;
      end;
    If Assigned(t) and Assigned(t^.psz_name) then
      Result:=StrPas(PChar(t^.psz_name));
    end;
end;

function TCustomVLCMediaPlayer.GetChannel: Integer;
begin
  If Assigned(Finstance) then
    Result:=libvlc_audio_get_channel(FInstance)
 else
   Result:=-1;
end;

procedure TCustomVLCMediaPlayer.SetChannel(AValue: Integer);
begin
  If Assigned(Finstance) then
    libvlc_audio_set_channel(Finstance,AValue)
end;

function TCustomVLCMediaPlayer.GetAudioDelay: Int64;
begin
  if Assigned(FInstance) then
    Result:=libvlc_audio_get_delay(FInstance)
  else
    Result:=-1;
end;

procedure TCustomVLCMediaPlayer.SetAudioDelay(AValue: Int64);
begin
  if Assigned(FInstance) then
    libvlc_audio_set_delay(FInstance,AValue)
end;

function TCustomVLCMediaPlayer.GetPlaying: Boolean;
begin
  Result:=(State=libvlc_Playing);
end;

function TCustomVLCMediaPlayer.GetChapter: Integer;
begin
  if Assigned(FInstance) then
    Result:=libvlc_media_player_get_chapter(FInstance)
  else
    Result:=-1;
end;

procedure TCustomVLCMediaPlayer.SetChapter(AValue: Integer);
begin
  if Assigned(FInstance) then
    libvlc_media_player_set_chapter(FInstance,AValue);
end;

function TCustomVLCMediaPlayer.GetChapterCount: Integer;
begin
  if Assigned(FInstance) then
    Result:=libvlc_media_player_get_chapter_count(FInstance)
  else
    Result:=-1;
end;

function TCustomVLCMediaPlayer.GetPlayable: Boolean;
begin
  if Assigned(FInstance) then
    Result:=(libvlc_media_player_will_play(FInstance)<>0)
  else
    Result:=False
end;

function TCustomVLCMediaPlayer.GetPausable: Boolean;
begin
  if Assigned(FInstance) then
    Result:=(libvlc_media_player_can_pause(FInstance)<>0)
  else
    Result:=False
end;

function TCustomVLCMediaPlayer.GetSeekable: Boolean;
begin
  if Assigned(FInstance) then
    Result:=(libvlc_media_player_is_seekable(FInstance)<>0)
  else
    Result:=False
end;

function TCustomVLCMediaPlayer.GetAudioVolume: Integer;
begin
  if Assigned(FInstance) then
    Result:=libvlc_audio_get_volume(FInstance)
  else
    Result:=-1
end;

procedure TCustomVLCMediaPlayer.SetAudioVolume(AValue: Integer);
begin
  if Assigned(FInstance) then
    begin
    if (AValue<0) then
      AValue:=0
    else if (AValue>200) then
      AValue:=200;
    libvlc_audio_set_volume(Finstance,AValue);
    end;
end;

procedure TCustomVLCMediaPlayer.SetPlayRate(Avalue : Integer);
begin
  if Assigned(FInstance) then
    begin
    if (Avalue< 1) then
       AValue:=1
    else if (AValue>1000) then
      AValue:=1000;
    libvlc_media_player_set_rate(FInstance,AValue/100);
    end;
end;

function TCustomVLCMediaPlayer.GetPlayRate: Integer;
begin
  if Assigned(FInstance) then
    Result:=Round(libvlc_media_player_get_rate(FInstance)*100)
  else
    Result:=-1;
end;

procedure TCustomVLCMediaPlayer.SetFullScreenMode(AValue: Boolean);
begin
  if Assigned(FInstance) then
    libvlc_set_fullscreen(Finstance,Ord(AValue));
end;

function TCustomVLCMediaPlayer.GetFullScreenMode: Boolean;
begin
  If Assigned(FInstance) then
    Result:=libvlc_get_fullscreen(Finstance)<>0
  else
    Result:=False;
end;

procedure TCustomVLCMediaPlayer.SetVideoFractional(AValue: Double);
begin
  libvlc_media_player_set_position(FInstance,AValue);
end;

procedure TCustomVLCMediaPlayer.SetVideoPos(AValue: Int64);
begin
  libvlc_media_player_set_time(FInstance,AVAlue);
end;

procedure TCustomVLCMediaPlayer.SetVideoScale(AValue: Double);
begin
  libvlc_video_set_scale(Finstance,AVAlue);
end;

function TCustomVLCMediaPlayer.GetInstance: Plibvlc_media_player_t;
begin
  Result:=FInstance;
  if (FInstance=Nil) then
    begin
    FInstance:=libvlc_media_player_new(VLC.Instance);
    libvlc_video_set_mouse_input(FInstance,1);
    libvlc_video_set_key_input(FInstance,1);
    if FUseEvents then
      HookupEvents;
    end;
  Result:=FInstance;
end;

procedure TCustomVLCMediaPlayer.SetParentWindow;
begin
  // Do nothing
end;

procedure TCustomVLCMediaPlayer.SetParentWindowSize(AWidth, AHeight: Cardinal);
begin
  // Do nothing
end;

Procedure TCustomVLCMediaPlayer.UnHookEvents;


  Procedure ClearEvent(M : plibvlc_event_manager_t;t : libvlc_event_e);

  begin
    libvlc_event_detach(M,ord(t),@PlayerEventHelper,Self);
  end;

Var
  M : plibvlc_event_manager_t;

begin
  M:=libvlc_media_player_event_manager(Instance);
  if (M<>Nil) then
    begin
    ClearEvent(M,libvlc_MediaPlayerMediaChanged);
    ClearEvent(M,libvlc_MediaPlayerNothingSpecial);
    ClearEvent(M,libvlc_MediaPlayerOpening);
    ClearEvent(M,libvlc_MediaPlayerBuffering);
    ClearEvent(M,libvlc_MediaPlayerPlaying);
    ClearEvent(M,libvlc_MediaPlayerPaused);
    ClearEvent(M,libvlc_MediaPlayerStopped);
    ClearEvent(M,libvlc_MediaPlayerForward);
    ClearEvent(M,libvlc_MediaPlayerBackward);
    ClearEvent(M,libvlc_MediaPlayerEndReached);
    ClearEvent(M,libvlc_MediaPlayerEncounteredError);
    ClearEvent(M,libvlc_MediaPlayerTimeChanged);
    ClearEvent(M,libvlc_MediaPlayerPositionChanged);
    ClearEvent(M,libvlc_MediaPlayerSeekableChanged);
    ClearEvent(M,libvlc_MediaPlayerPausableChanged);
    ClearEvent(M,libvlc_MediaPlayerTitleChanged);
    ClearEvent(M,libvlc_MediaPlayerSnapshotTaken);
    ClearEvent(M,libvlc_MediaPlayerLengthChanged);
    FreeAndNil(FECS);
    end;
end;

Procedure TCustomVLCMediaPlayer.HookupEvents;

  Procedure AttachEvent( M : plibvlc_event_manager_t;t : libvlc_event_e);

  begin
    libvlc_event_attach(M,ord(t),@PlayerEventHelper,Self);
  end;

Var
  M : plibvlc_event_manager_t;

begin
  M:=libvlc_media_player_event_manager(Instance);
  if (M<>Nil) then
    begin
    FECS:=TCriticalSection.Create;
    AttachEvent(M,libvlc_MediaPlayerMediaChanged);
    AttachEvent(M,libvlc_MediaPlayerNothingSpecial);
    AttachEvent(M,libvlc_MediaPlayerOpening);
    AttachEvent(M,libvlc_MediaPlayerBuffering);
    AttachEvent(M,libvlc_MediaPlayerPlaying);
    AttachEvent(M,libvlc_MediaPlayerPaused);
    AttachEvent(M,libvlc_MediaPlayerStopped);
    AttachEvent(M,libvlc_MediaPlayerForward);
    AttachEvent(M,libvlc_MediaPlayerBackward);
    AttachEvent(M,libvlc_MediaPlayerEndReached);
    AttachEvent(M,libvlc_MediaPlayerEncounteredError);
    AttachEvent(M,libvlc_MediaPlayerTimeChanged);
    AttachEvent(M,libvlc_MediaPlayerPositionChanged);
    AttachEvent(M,libvlc_MediaPlayerSeekableChanged);
    AttachEvent(M,libvlc_MediaPlayerPausableChanged);
    AttachEvent(M,libvlc_MediaPlayerTitleChanged);
    AttachEvent(M,libvlc_MediaPlayerSnapshotTaken);
    AttachEvent(M,libvlc_MediaPlayerLengthChanged);
    end;
end;

procedure TCustomVLCMediaPlayer.DoMediaChanged;

begin
  If Assigned(FOnMediaChanged) then
    FOnMediaChanged(Self);
end;

procedure TCustomVLCMediaPlayer.DoNothingSpecial;

begin
  If Assigned(FOnNothingSpecial) then
    FOnNothingSpecial(Self);
end;

procedure TCustomVLCMediaPlayer.DoOnOpening;

begin
  If Assigned(FOnOpening) then
    FOnOpening(Self);
end;

procedure TCustomVLCMediaPlayer.DoOnPlaying;

begin
  If Assigned(FOnPlaying) then
    FOnPlaying(Self);
end;

procedure TCustomVLCMediaPlayer.DoOnPause;

begin
  If Assigned(FOnPause) then
    FOnPause(Self);
end;


procedure TCustomVLCMediaPlayer.DoOnStop;

begin
  If Assigned(FOnStop) then
    FOnStop(Self);
end;


procedure TCustomVLCMediaPlayer.DoOnForward;

begin
  If Assigned(FOnForward) then
    FOnForward(Self);
end;


procedure TCustomVLCMediaPlayer.DoOnBackward;

begin
  If Assigned(FOnBackward) then
    FOnBackward(Self);
end;

procedure TCustomVLCMediaPlayer.DoOnEOF;

begin
  If Assigned(FOnEOF) then
    FOnEOF(Self);
end;

procedure TCustomVLCMediaPlayer.DoOnBuffering;

begin
  If Assigned(FOnBuffering) then
    FOnBuffering(Self);
end;

procedure TCustomVLCMediaPlayer.DoOnError;

Var
  P : pcchar;
  E : String;
begin
  p:=libvlc_errmsg();
  if p<>Nil then
    E:=StrPas(PChar(P))
  else
    E:='';
  If Assigned(FOnError) then
    FOnError(Self,E);
end;

procedure TCustomVLCMediaPlayer.DoOnTimeChanged(Const ATime: libvlc_time_t);

begin
  If Assigned(FOnTimeChanged) then
    FOnTimeChanged(Self,VLCTimeToDateTime(ATime));
end;

procedure TCustomVLCMediaPlayer.DoOnPositionChanged(Const Aposition: Double);

begin
  If Assigned(FOnPositionChanged) then
    FOnPositionChanged(Self,APosition);
end;


procedure TCustomVLCMediaPlayer.DoOnSeekableChanged(Const ASeekable : Boolean);

begin
  If Assigned(FOnSeekableChanged) then
    FOnSeekableChanged(Self,ASeekable);
end;

procedure TCustomVLCMediaPlayer.DoOnPausableChanged(Const APausable : Boolean);

begin
  If Assigned(FOnPausableChanged) then
    FOnPausableChanged(Self,APausable);
end;

procedure TCustomVLCMediaPlayer.DoOnTitleChanged(Const ATitle: cint);

begin
  If Assigned(FOnTitleChanged) then
    FOnTitleChanged(Self,ATitle);
end;

procedure TCustomVLCMediaPlayer.DoOnSnapshot(Const AFileName : PCChar);

Var
  S :String;

begin
  If Assigned(FOnSnapshot) then
    begin
    if Assigned(AFileName) then
      S:=StrPas(PChar(AFileName))
    else
      S:='';
    FOnSnapShot(Self,S);
    end;
end;

procedure TCustomVLCMediaPlayer.DoOnLengthChanged(Const ATime: libvlc_time_t);

begin
  If Assigned(FOnLengtHChanged) then
    FOnLengtHChanged(Self,VLCTimeToDateTime(ATime));
end;



procedure TCustomVLCMediaPlayer.HandleVLCEvent(e: Plibvlc_event_t);

begin
  FECS.Enter;
  try
    case libvlc_event_e(e^._type) of
      libvlc_MediaPlayerMediaChanged     : DoMediaChanged;
      libvlc_MediaPlayerNothingSpecial   : DoNothingSpecial;
      libvlc_MediaPlayerOpening          : DoOnOpening;
      libvlc_MediaPlayerBuffering        : DoOnBuffering;
      libvlc_MediaPlayerPlaying          : DoOnPlaying;
      libvlc_MediaPlayerPaused           : DoOnPause;
      libvlc_MediaPlayerStopped          : DoOnStop;
      libvlc_MediaPlayerForward          : DoOnForward;
      libvlc_MediaPlayerBackward         : DoOnBackward;
      libvlc_MediaPlayerEndReached       : DoOnEOF;
      libvlc_MediaPlayerEncounteredError : DoOnError;
      libvlc_MediaPlayerTimeChanged      : begin
                                           DoOnTimeChanged(e^.media_player_time_changed.new_time);
                                           end;
      libvlc_MediaPlayerPositionChanged  : begin
                                           DoOnPositionChanged(e^.media_player_position_changed.new_position);
      end;
      libvlc_MediaPlayerSeekableChanged  : begin
                                           DoOnSeekableChanged(e^.media_player_seekable_changed.new_seekable<>0);
      end;
      libvlc_MediaPlayerPausableChanged  : begin
                                           DoOnPausableChanged(e^.media_player_pausable_changed.new_pausable<>0) ;
      end;
      libvlc_MediaPlayerTitleChanged     : begin
                                           DoOnTitleChanged(e^.media_player_title_changed.new_title);
      end;
      libvlc_MediaPlayerSnapshotTaken    : begin
                                           DoOnSnapShot(e^.media_player_snapshot_taken.psz_filename);
      end;
      libvlc_MediaPlayerLengthChanged    : begin
                                           DoOnLengthChanged(e^.media_player_length_changed.new_length);
      end;
    else
      // Writeln('Unknown event type ',e^._type);
    end;
  finally
    FECS.Leave;
  end;
end;

destructor TCustomVLCMediaPlayer.Destroy;
begin
  If Assigned(FInstance) then
    begin
    libvlc_media_player_release(FInstance);
    FInstance:=Nil;
    end;
  FreeAndNil(FECS);
  inherited Destroy;
end;

procedure TCustomVLCMediaPlayer.SetMedia(M: TVLCMediaItem);

begin
  libvlc_media_player_set_media(Instance,M.Instance);
end;

procedure TCustomVLCMediaPlayer.Play;

Var
  W,H : Cardinal;

begin
  SetParentWindow;
  libvlc_media_player_play(Instance);
  If FitWindow then
    begin
    VideoScale:=1.0;
    if GetVideoSize(W,H) then
      SetParentWindowSize(W,H);
    end;
end;

procedure TCustomVLCMediaPlayer.Play(M: TVLCMediaItem);

begin
  if Playing then
    begin
    Stop;
    While Playing do
      Sleep(100);
    end;
  SetMedia(M);
  Play;
end;

procedure TCustomVLCMediaPlayer.PlayFile(const AFileName: String);

Var
  M : TVLCMediaItem;
begin
  M:=TVLCMediaItem.Create(Nil);
  try
    M.Path:=AFileName;
    Play(M);
  finally
    M.Free;
  end;
end;

procedure TCustomVLCMediaPlayer.Stop;
begin
  if Assigned(FInstance) then
    libvlc_media_player_stop(FInstance);
end;

procedure TCustomVLCMediaPlayer.Pause;
begin
  if Assigned(FInstance) then
    libvlc_media_player_pause(FInstance);
end;

procedure TCustomVLCMediaPlayer.Resume;
begin
  if (GetState()=libvlc_Paused)  then
   if Assigned(FInstance) then
     libvlc_media_player_play(FInstance);
end;

procedure TCustomVLCMediaPlayer.NextFrame;
begin
  if Assigned(FInstance) then
   libvlc_media_player_next_frame(Finstance);
end;

function TCustomVLCMediaPlayer.Snapshot(const AFileName: String): Boolean;

var
  w,h : Cardinal;
begin
  Result:=Assigned(FInstance);
  if Result then
    begin
    w:=0;
    h:=0;
    Result:=libvlc_video_get_size(FInstance,0,@W,@H)=0;
    if Result then
      Result:=SnapShot(AFileName,W,H);
    end;
end;

function TCustomVLCMediaPlayer.Snapshot(const AFileName: String; AWidth,
  AHeight: Cardinal): Boolean;
begin
  Result:=Assigned(FInstance);
  If Result then
    Result:=libvlc_video_take_snapshot(FInstance,0,PCChar(PChar(AFileName)),AWidth,AHeight)=0;
end;

function TCustomVLCMediaPlayer.GetVideoSize(var AWidth, AHeight: Cardinal
  ): Boolean;
begin
  Result:=libvlc_video_get_size(FInstance,0,@AWidth,@AHeight)=0;
end;

{ TVLCMediaItems }

constructor TVLCMediaItems.Create(ALibrary: TVLCLibrary;AItemClass: TVLCMediaItemClass = Nil);
begin
  Inherited Create(AItemClass);
  FVLC:=ALibrary;
end;

constructor TVLCMediaItems.Create(AInstance: Plibvlc_media_list_t;
  AItemClass: TVLCMediaItemClass);


Var
  I : Integer;
  P : plibvlc_media_t;

begin
  Inherited Create(AItemClass);
  FInstance:=AInstance;
  For I:=0 to libvlc_media_list_count(FInstance)-1 do
    begin
    P:=libvlc_media_list_item_at_index(FInstance,I);
    (Add as TVLCMediaItem).SetInstance(P);
    end;
end;

procedure TVLCMediaItems.Lock;
begin
  libvlc_media_list_lock(FInstance);
end;

procedure TVLCMediaItems.Unlock;
begin
  libvlc_media_list_lock(FInstance);
end;

function TVLCMediaItems.GetInstance: Plibvlc_media_list_t;
Var
  I :integer;
begin
  if FInstance=Nil then
    begin
    FInstance:=libvlc_media_list_new(VLC.Instance);
    For I:=0 to Count-1 do
      GetI(I).RegisterInstance;
    end;
  Result:=Finstance;
end;

function TVLCMediaItems.GetIsReadOnly: Boolean;
begin
  Result:=libvlc_media_list_is_readonly(FInstance)<>0;
end;

function TVLCMediaItems.GetI(AIndex : Integer): TVLCMediaItem;
begin
  Result:=Items[AIndex] as TVLCMediaItem;
end;

procedure TVLCMediaItems.SetI(AIndex : Integer; AValue: TVLCMediaItem);
begin
  Items[AIndex]:=AValue;
end;


function TVLCMediaItems.GetVLC: TVLCLibrary;
begin
  Result:=VLCLibrary;
end;

{ TVLCMediaItem }

function TVLCMediaItem.GetInstance: plibvlc_media_t;
begin
  Result:=Finstance;
  If (Result=Nil) then
    Raise EVLC.Create('No instance available at this time. Set MRL, Path or FileDescriptor first');
end;

function TVLCMediaItem.GetM(AIndex: Integer): Boolean;
begin
  Result:=FOpts[AIndex];
end;

function TVLCMediaItem.GetMD(AMeta : libvlc_meta_t): String;

Var
  P : PCChar;

begin
  P:=libvlc_media_get_meta(Instance,AMeta);
  if (P<>Nil) then
    Result:=StrPas(PChar(p))
  else
    Result:='';
end;

function TVLCMediaItem.GetMRL: String;
Var
  P : PCChar;

begin
  P:=libvlc_media_get_mrl(Instance);
  if (P<>Nil) then
    Result:=StrPas(PChar(p))
  else
    Result:='';
end;

function TVLCMediaItem.GetParsed: Boolean;
begin
  Result:=libvlc_media_is_parsed(Instance)<>0;
end;

function TVLCMediaItem.GetUD: Pointer;
begin
  Result:=libvlc_media_get_user_data(Instance);
end;

procedure TVLCMediaItem.SetDIM(AValue: TDeinterlaceMode);

Const
  DMS : Array[TDeinterlaceMode] of string
      = ('blend', 'discard', 'bob', 'linear', 'mean', 'x', 'yadif', 'yadif2x');
begin
  if (FDIM=AValue) then Exit;
  FDIM:=AValue;
  libvlc_media_add_option(Instance, PCChar(PChar('deinterlace-mode='+DMS[AValue])));
end;

procedure TVLCMediaItem.SetFD(AValue: Integer);
begin
  FFD:=AValue;
  Finstance:=libvlc_media_new_fd(GetVLC.Instance,AValue);
  If (FInstance=Nil) then
    Raise EVLC.CreateFmt('Failed to create media item from file descriptor "%d"',[AValue]);
  RegisterInstance;
end;

procedure TVLCMediaItem.SetFSS(AValue: TSNapshotFormat);

Const
  ssfs : Array[TSnapShotFormat] of string = ('png','jpg');

begin
  if FSS=AValue then Exit;
  FSS:=AValue;
  libvlc_media_add_option(Instance, PCChar(PChar('no-snapshot-preview')));
  libvlc_media_add_option(instance, PCChar(PChar('snapshot-format=' + SSFS[aValue])));
end;

procedure TVLCMediaItem.SetM(AIndex: Integer; AValue: Boolean);

begin
  FOpts[AIndex]:=AValue;
  libvlc_media_add_option(FInstance,PcChar(PChar(GetBoolOpt(AIndex,AValue))));
end;

function TVLCMediaItem.GetBoolOpt(AIndex: Integer; AValue: Boolean): String;
begin
  Case AINdex of
    0 : Result:='video-title-show';
    1 : Result:='video-on-top';
    2 : Result:='overlay';
    3 : Result:='fullscreen';
    4 : Result:='deinterlace='+IntToStr(Ord(AValue));
  end;
  if (AIndex < 4) and Not AValue then
    Result:='no-'+Result;
end;

procedure TVLCMediaItem.SetMD(AMeta : libvlc_meta_t; AValue: String);
begin
  libvlc_media_set_meta(Instance,AMeta,Pcchar(PChar(AValue)));
end;

procedure TVLCMediaItem.SetMRL(AValue: String);
begin
  Finstance:=libvlc_media_new_location(GetVLC.Instance,PCChar(AValue));
  If (FInstance=Nil) then
    Raise EVLC.CreateFmt('Failed to create media item from MRL : "%s"',[AValue]);
  RegisterInstance;
end;

procedure TVLCMediaItem.SetPath(AValue: String);
begin
  if FPath=AValue then Exit;
  FPath:=AValue;
  FInstance:=libvlc_media_new_path(GetVLC.Instance,PCChar(AValue));
  if (FInstance=Nil) then
    Raise EVLC.CreateFmt('Failed to create media item from path : "%s"',[AValue]);
  RegisterInstance;
end;

procedure TVLCMediaItem.SetUD(AValue: Pointer);
begin
  libvlc_media_set_user_data(Instance,AValue);
end;

function TVLCMediaItem.GetState: libvlc_state_t;
begin
  Result:=libvlc_media_get_state(instance);
end;

function TVLCMediaItem.GetDuration: TDateTime;

Var
  d :  libvlc_time_t;

begin
  d:=libvlc_media_get_duration(Instance);
  Result:=D
end;

procedure TVLCMediaItem.RegisterInstance;

Var
  L : Plibvlc_media_list_t;

begin
  If Assigned(Collection) and (Collection is TVLCMediaItems) then
    begin
    L:=TVLCMediaItems(Collection).FInstance;
    if (L<>Nil) then
      begin
      libvlc_media_list_lock(L);
      libvlc_media_list_add_media(L, FInstance);
      libvlc_media_list_unlock(L);
      end;
    end;
end;

procedure TVLCMediaItem.UnRegisterInstance;
Var
  L : Plibvlc_media_list_t;
  i : integer;

begin
  If Assigned(Collection) and (Collection is TVLCMediaItems) then
    begin
    L:=TVLCMediaItems(Collection).FInstance;
    if L<>Nil then
      begin
      libvlc_media_list_lock(L);
      I:=libvlc_media_list_index_of_item(L,Finstance);
      if (i>=0) then
        libvlc_media_list_remove_index(L,i);
      libvlc_media_list_unlock(L);
      end;
    end;
end;

function TVLCMediaItem.GetVLC: TVLCLibrary;
begin
  If Assigned(Collection) and (Collection is TVLCMediaItems) then
    Result:=TVLCMediaItems(Collection).GetVLC
  else
    Result:=VLCLibrary;
  if not Result.Initialized then
    Result.Initialize;
end;

function TVLCMediaItem.GetEventManager: plibvlc_event_manager_t;
begin
  Result:=libvlc_media_event_manager(Instance);
end;

procedure TVLCMediaItem.SetInstance(Avalue: plibvlc_media_t);
begin
  FInstance:=AValue;
end;

destructor TVLCMediaItem.Destroy;
begin
  inherited Destroy;
  if Assigned(FInstance) then
    begin
    UnregisterInstance;
    libvlc_media_release(FInstance);
    FInstance:=Nil;
    end;
end;

procedure TVLCMediaItem.AddOption(const AValue: String);
begin
  libvlc_media_add_option(Instance,PCChar(PChar(AValue)));
end;

procedure TVLCMediaItem.Parse;
begin
  libvlc_media_parse(Instance);
end;

procedure TVLCMediaItem.ParseAsync;
begin
  libvlc_media_parse_async(Instance);
end;

procedure TVLCMediaItem.SaveMetaData;
begin
  libvlc_media_save_meta(Instance);
end;

function TVLCMediaItem.GetStats(var AStats: libvlc_media_stats_t): Boolean;
begin
  Result:=libvlc_media_get_stats(Instance,@AStats)<>0;
end;

function TVLCMediaItem.Duplicate: TVLCMediaItem;
begin
  If Assigned(Collection) and (Collection is TVLCMediaItems) then
    Result:=TVLCMediaItems(Collection).Add as TVLCMediaItem
  else
    Result:=TVLCMediaItem.Create(Nil);
  Result.SetInstance(libvlc_media_duplicate(Instance));
end;


function TVLCLibrary.GetLastError: String;

Var
  P : PCChar;

begin
  P:=libvlc_errmsg();
  if Assigned(P) then
    Result := StrPas(PChar(P))
  else
    Result:='';
end;

function TVLCLibrary.GetI: Boolean;
begin
  Result:=FInstance<>Nil;
end;

function TVLCLibrary.GetVersion: String;
Var
  P : PCChar;

begin
  P:=libvlc_get_version();
  if Assigned(P) then
    Result := StrPas(PChar(P))
  else
    Result:='';
end;

function TVLCLibrary.GetCompiler: String;
Var
  P : PCChar;

begin
  P:=libvlc_get_compiler();
  if Assigned(P) then
    Result := StrPas(PChar(P))
  else
    Result:='';
end;

function TVLCLibrary.GetChangeSet: String;

Var
  P : PCChar;

begin
  P:=libvlc_get_changeset();
  if Assigned(P) then
    Result := StrPas(PChar(P))
  else
    Result:='';
end;

procedure TVLCLibrary.SetLibraryPath(const AValue: String);
begin
  If AValue=FLibraryPath then exit;
  If Assigned(FInstance) then
    Raise EVLC.Create('VLC already initialized, cannot set librarypath');
  FLibraryPath:=AVAlue;
end;

function TVLCLibrary.GetInstance: plibvlc_instance_t;

var
  args: Array of AnsiString;
  cargs : array of PAnsiChar;
  argc,
  I : integer;

begin
  If (FInstance=Nil) then
    begin
    LibraryArgs.add('--no-video-title-show');
    SetLength(cArgs,LibraryArgs.Count+2);
    SetLength(Args,LibraryArgs.Count+1);
    cargs[0] := PChar(FLibraryPath);
    For I:=0 to LibraryArgs.Count-1 do
      begin
      Args[i]:=LibraryArgs[i];
      CArgs[i+1]:=PChar(Args[i]);
      end;
    argc:=Length(CArgs);
    cargs[argc-1] := NIL;
    FInstance := libvlc_new(argc-1, PPcchar(cargs));
    if (FInstance=Nil) then
      Raise EVLC.Create('Could not create instance of libvlc');
    end;
  Result:=FInstance;
end;

constructor TVLCLibrary.Create(AOwner: TComponent);
begin
  Inherited;
  FInstance:=Nil;
  FLibraryPath:=LibName;
  FLibraryArgs:=TStringList.Create;
end;

destructor TVLCLibrary.Destroy;
begin
  FreeAndNil(FLibraryArgs);
  Release;
  inherited Destroy;
end;

procedure TVLCLibrary.Initialize;
begin
  LoadLibVLC(LibraryPath,False);
  GetInstance;
end;

procedure TVLCLibrary.Release;
begin
  If (FInstance<>Nil) then
    begin
    libvlc_release(FInstance);
    FreeLibVLC;
    end;
  FInstance:=Nil;
end;

Initialization

Finalization
  DoneVLC;
end.

