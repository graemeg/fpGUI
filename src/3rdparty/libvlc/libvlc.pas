
{$mode objfpc}
unit libvlc;
interface

uses
  ctypes;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

Const

{$ifdef unix}
  libname = 'libvlc.so';
{$else}
{$ifdef windows}
  DefaultlibPath = 'C:\Program files\Videolan\VLC\';
  corelibname    = 'libvlccore.dll';
  libname        = 'libvlc.dll';
{$endif}
{$endif}

  Type
  _bool = cint;
   Ppcchar = ^Pcchar;

  // Opaque types.  
  libvlc_event_manager_t = record end;
  Libvlc_instance_t = record end;
  Libvlc_log_iterator_t = record end;
  Libvlc_log_t = record end;
  Libvlc_media_discoverer_t = record end;
  Libvlc_media_library_t = record end;
  Libvlc_media_list_player_t = record end;
  Libvlc_media_list_t = record end;
  Libvlc_media_player_t = record end;
  Libvlc_media_t = record end;
  
  Plibvlc_audio_output_t  = ^libvlc_audio_output_t;
  Plibvlc_event_manager_t  = ^libvlc_event_manager_t;
  Plibvlc_event_t  = ^libvlc_event_t;
  Plibvlc_instance_t  = ^libvlc_instance_t;
  Plibvlc_log_iterator_t  = ^libvlc_log_iterator_t;
  Plibvlc_log_message_t  = ^libvlc_log_message_t;
  Plibvlc_log_t  = ^libvlc_log_t;
  Plibvlc_media_discoverer_t  = ^libvlc_media_discoverer_t;
  Plibvlc_media_library_t  = ^libvlc_media_library_t;
  Plibvlc_media_list_player_t  = ^libvlc_media_list_player_t;
  Plibvlc_media_list_t  = ^libvlc_media_list_t;
  Plibvlc_media_player_t  = ^libvlc_media_player_t;
  Plibvlc_media_stats_t  = ^libvlc_media_stats_t;
  Plibvlc_media_t  = ^libvlc_media_t;
  Plibvlc_media_track_info_t  = ^libvlc_media_track_info_t;
  Plibvlc_module_description_t  = ^libvlc_module_description_t;
  Plibvlc_track_description_t  = ^libvlc_track_description_t;

  int8_t = cschar;
  int16_t = csint;
  int32_t = cint;
  int64_t = clong;
  uint8_t = cuchar;
  uint16_t = csint;
  uint32_t = cuint;
  uint64_t = culong;
  int_least8_t = cschar;
  int_least16_t = csint;
  int_least32_t = cint;
  int_least64_t = clong;
  uint_least8_t = cuchar;
  uint_least16_t = csint;
  uint_least32_t = cuint;
  uint_least64_t = culong;
  int_fast8_t = cschar;
  int_fast16_t = clong;
  int_fast32_t = clong;
  int_fast64_t = clong;
  uint_fast8_t = cuchar;
  uint_fast16_t = culong;
  uint_fast32_t = culong;
  uint_fast64_t = culong;
  intptr_t = clong;
  uintptr_t = culong;
  intmax_t = clong;
  uintmax_t = culong;

  libvlc_time_t = int64_t;
  libvlc_log_message_t = record
    i_severity : cint;
    psz_type : ^cchar;
    psz_name : ^cchar;
    psz_header : ^cchar;
    psz_message : ^cchar;
  end;

  libvlc_event_type_t = cint;
  libvlc_callback_t = procedure (_para1:Plibvlc_event_t; _para2:pointer);cdecl;

  libvlc_module_description_t = record
    psz_name : ^cchar;
    psz_shortname : ^cchar;
    psz_longname : ^cchar;
    psz_help : ^cchar;
    p_next : ^libvlc_module_description_t;
  end;

{
static inline int64_t libvlc_delay(int64_t pts)

    return pts - libvlc_clock();

 }


  libvlc_meta_t = (libvlc_meta_Title,libvlc_meta_Artist,
    libvlc_meta_Genre,libvlc_meta_Copyright,
    libvlc_meta_Album,libvlc_meta_TrackNumber,
    libvlc_meta_Description,libvlc_meta_Rating,
    libvlc_meta_Date,libvlc_meta_Setting,
    libvlc_meta_URL,libvlc_meta_Language,
    libvlc_meta_NowPlaying,libvlc_meta_Publisher,
    libvlc_meta_EncodedBy,libvlc_meta_ArtworkURL,
    libvlc_meta_TrackID);

  libvlc_state_t = (libvlc_NothingSpecial := 0,libvlc_Opening,
    libvlc_Buffering,libvlc_Playing,libvlc_Paused,
    libvlc_Stopped,libvlc_Ended,libvlc_Error
    );

  libvlc_media_option_t = (libvlc_media_option_trusted := $2,libvlc_media_option_unique := $100
    );

  libvlc_track_type_t = (libvlc_track_unknown := -(1),libvlc_track_audio := 0,
    libvlc_track_video := 1,libvlc_track_text := 2
    );

  libvlc_media_stats_t = record
    i_read_bytes : cint;
    f_input_bitrate : cfloat;
    i_demux_read_bytes : cint;
    f_demux_bitrate : cfloat;
    i_demux_corrupted : cint;
    i_demux_discontinuity : cint;
    i_decoded_video : cint;
    i_decoded_audio : cint;
    i_displayed_pictures : cint;
    i_lost_pictures : cint;
    i_played_abuffers : cint;
    i_lost_abuffers : cint;
    i_sent_packets : cint;
    i_sent_bytes : cint;
    f_send_bitrate : cfloat;
  end;

  libvlc_media_track_info_t = record
    i_codec : uint32_t;
    i_id : cint;
    i_type : libvlc_track_type_t;
    i_profile : cint;
    i_level : cint;
    u : record
      case longint of
        0 : ( audio : record
            i_channels : cunsigned;
            i_rate : cunsigned;
          end );
        1 : ( video : record
            i_height : cunsigned;
            i_width : cunsigned;
          end );
      end;
  end;


  libvlc_track_description_t = record
    i_id : cint;
    psz_name : ^cchar;
    p_next : ^libvlc_track_description_t;
  end;

  libvlc_audio_output_t = record
    psz_name : ^cchar;
    psz_description : ^cchar;
    p_next : ^libvlc_audio_output_t;
  end;

  libvlc_rectangle_t = record
    top : cint;
    left : cint;
    bottom : cint;
    right : cint;
  end;

  libvlc_video_marquee_option_t = (libvlc_marquee_Enable := 0,libvlc_marquee_Text,
    libvlc_marquee_Color,libvlc_marquee_Opacity,
    libvlc_marquee_Position,libvlc_marquee_Refresh,
    libvlc_marquee_Size,libvlc_marquee_Timeout,
    libvlc_marquee_X,libvlc_marquee_Y);

  libvlc_navigate_mode_t = (libvlc_navigate_activate := 0,libvlc_navigate_up,
    libvlc_navigate_down,libvlc_navigate_left,
    libvlc_navigate_right);


  libvlc_video_lock_cb = function (opaque:pointer; planes:Ppointer):pointer;cdecl;
  libvlc_video_unlock_cb = procedure (opaque:pointer; picture:pointer; planes:Ppointer);cdecl;
  libvlc_video_display_cb = procedure (opaque:pointer; picture:pointer);cdecl;
  libvlc_video_format_cb = function (opaque:Ppointer; chroma:pcchar; width:pcunsigned; height:pcunsigned; pitches:pcunsigned;
               lines:pcunsigned):cunsigned;cdecl;
  libvlc_video_cleanup_cb = procedure (opaque:pointer);cdecl;
  libvlc_audio_play_cb = procedure (data:pointer; samples:pointer; count:cunsigned; pts:int64_t);cdecl;
  libvlc_audio_pause_cb = procedure (data:pointer; pts:int64_t);cdecl;
  libvlc_audio_resume_cb = procedure (data:pointer; pts:int64_t);cdecl;
  libvlc_audio_flush_cb = procedure (data:pointer; pts:int64_t);cdecl;
  libvlc_audio_drain_cb = procedure (data:pointer);cdecl;
  libvlc_audio_set_volume_cb = procedure (data:pointer; volume:cfloat; mute:_Bool);cdecl;
  libvlc_audio_setup_cb = function (data:Ppointer; format:pcchar; rate:pcunsigned; channels:pcunsigned):cint;cdecl;
  libvlc_audio_cleanup_cb = procedure (data:pointer);cdecl;
  libvlc_video_logo_option_t = (libvlc_logo_enable,libvlc_logo_file,libvlc_logo_x,
    libvlc_logo_y,libvlc_logo_delay,libvlc_logo_repeat,
    libvlc_logo_opacity,libvlc_logo_position
    );
  libvlc_video_adjust_option_t = (libvlc_adjust_Enable := 0,libvlc_adjust_Contrast,
    libvlc_adjust_Brightness,libvlc_adjust_Hue,
    libvlc_adjust_Saturation,libvlc_adjust_Gamma
    );
  libvlc_audio_output_device_types_t = (libvlc_AudioOutputDevice_Error := -(1),
    libvlc_AudioOutputDevice_Mono := 1,
    libvlc_AudioOutputDevice_Stereo := 2,
    libvlc_AudioOutputDevice_2F2R := 4,
    libvlc_AudioOutputDevice_3F2R := 5,
    libvlc_AudioOutputDevice_5_1 := 6,libvlc_AudioOutputDevice_6_1 := 7,
    libvlc_AudioOutputDevice_7_1 := 8,libvlc_AudioOutputDevice_SPDIF := 10
    );

  libvlc_audio_output_channel_t = (libvlc_AudioChannel_Error := -(1),libvlc_AudioChannel_Stereo := 1,
    libvlc_AudioChannel_RStereo := 2,libvlc_AudioChannel_Left := 3,
    libvlc_AudioChannel_Right := 4,libvlc_AudioChannel_Dolbys := 5
    );
  libvlc_playback_mode_t = (libvlc_playback_mode_default,libvlc_playback_mode_loop,
    libvlc_playback_mode_repeat);

  libvlc_event_e = (libvlc_MediaMetaChanged := 0,
    libvlc_MediaSubItemAdded,
    libvlc_MediaDurationChanged,libvlc_MediaParsedChanged,
    libvlc_MediaFreed,libvlc_MediaStateChanged,
    libvlc_MediaPlayerMediaChanged := $100,
    libvlc_MediaPlayerNothingSpecial,libvlc_MediaPlayerOpening,
    libvlc_MediaPlayerBuffering,libvlc_MediaPlayerPlaying,
    libvlc_MediaPlayerPaused,libvlc_MediaPlayerStopped,
    libvlc_MediaPlayerForward,libvlc_MediaPlayerBackward,
    libvlc_MediaPlayerEndReached,libvlc_MediaPlayerEncounteredError,
    libvlc_MediaPlayerTimeChanged,libvlc_MediaPlayerPositionChanged,
    libvlc_MediaPlayerSeekableChanged,libvlc_MediaPlayerPausableChanged,
    libvlc_MediaPlayerTitleChanged,libvlc_MediaPlayerSnapshotTaken,
    libvlc_MediaPlayerLengthChanged,libvlc_MediaPlayerVout,
    libvlc_MediaListItemAdded := $200,libvlc_MediaListWillAddItem,
    libvlc_MediaListItemDeleted,libvlc_MediaListWillDeleteItem,
    libvlc_MediaListViewItemAdded := $300,
    libvlc_MediaListViewWillAddItem,libvlc_MediaListViewItemDeleted,
    libvlc_MediaListViewWillDeleteItem,libvlc_MediaListPlayerPlayed := $400,
    libvlc_MediaListPlayerNextItemSet,libvlc_MediaListPlayerStopped,
    libvlc_MediaDiscovererStarted := $500,
    libvlc_MediaDiscovererEnded,libvlc_VlmMediaAdded := $600,
    libvlc_VlmMediaRemoved,libvlc_VlmMediaChanged,
    libvlc_VlmMediaInstanceStarted,libvlc_VlmMediaInstanceStopped,
    libvlc_VlmMediaInstanceStatusInit,libvlc_VlmMediaInstanceStatusOpening,
    libvlc_VlmMediaInstanceStatusPlaying,
    libvlc_VlmMediaInstanceStatusPause,libvlc_VlmMediaInstanceStatusEnd,
    libvlc_VlmMediaInstanceStatusError);


  libvlc_event_t = record
    _type : cint;
    p_obj : pointer;
      case longint of
        0 : ( media_meta_changed : record
            meta_type : libvlc_meta_t;
          end );
        1 : ( media_subitem_added : record
            new_child : ^libvlc_media_t;
          end );
        2 : ( media_duration_changed : record
            new_duration : int64_t;
          end );
        3 : ( media_parsed_changed : record
            new_status : cint;
          end );
        4 : ( media_freed : record
            md : ^libvlc_media_t;
          end );
        5 : ( media_state_changed : record
            new_state : libvlc_state_t;
          end );
        6 : ( media_player_buffering : record
            new_cache : cfloat;
          end );
        7 : ( media_player_position_changed : record
            new_position : cfloat;
          end );
        8 : ( media_player_time_changed : record
            new_time : libvlc_time_t;
          end );
        9 : ( media_player_title_changed : record
            new_title : cint;
          end );
        10 : ( media_player_seekable_changed : record
            new_seekable : cint;
          end );
        11 : ( media_player_pausable_changed : record
            new_pausable : cint;
          end );
        12 : ( media_player_vout : record
            new_count : cint;
          end );
        13 : ( media_list_item_added : record
            item : ^libvlc_media_t;
            index : cint;
          end );
        14 : ( media_list_will_add_item : record
            item : ^libvlc_media_t;
            index : cint;
          end );
        15 : ( media_list_item_deleted : record
            item : ^libvlc_media_t;
            index : cint;
          end );
        16 : ( media_list_will_delete_item : record
            item : ^libvlc_media_t;
            index : cint;
          end );
        17 : ( media_list_player_next_item_set : record
            item : ^libvlc_media_t;
          end );
        18 : ( media_player_snapshot_taken : record
            psz_filename : ^cchar;
          end );
        19 : ( media_player_length_changed : record
            new_length : libvlc_time_t;
          end );
        20 : ( vlm_media_event : record
            psz_media_name : ^cchar;
            psz_instance_name : ^cchar;
          end );
        21 : ( media_player_media_changed : record
            new_media : ^libvlc_media_t;
          end );
    end;

  PPlibvlc_media_track_info_t = ^Plibvlc_media_track_info_t;
  cbtype1 = procedure (_para1:pointer); cdecl;

Var
    libvlc_media_player_new : function(p_libvlc_instance:Plibvlc_instance_t):plibvlc_media_player_t; cdecl;
    libvlc_media_player_new_from_media : function(p_md:Plibvlc_media_t):plibvlc_media_player_t; cdecl;
    libvlc_media_player_release : procedure(p_mi:Plibvlc_media_player_t); cdecl;
    libvlc_media_player_retain : procedure(p_mi:Plibvlc_media_player_t); cdecl;
    libvlc_media_player_set_media : procedure(p_mi:Plibvlc_media_player_t; p_md:Plibvlc_media_t); cdecl;
    libvlc_media_player_get_media : function(p_mi:Plibvlc_media_player_t):plibvlc_media_t; cdecl;
    libvlc_media_player_event_manager : function(p_mi:Plibvlc_media_player_t):plibvlc_event_manager_t; cdecl;
    libvlc_media_player_is_playing : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_media_player_play : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_media_player_set_pause : procedure(mp:Plibvlc_media_player_t; do_pause:cint); cdecl;
    libvlc_media_player_pause : procedure(p_mi:Plibvlc_media_player_t); cdecl;
    libvlc_media_player_stop : procedure(p_mi:Plibvlc_media_player_t); cdecl;
    libvlc_media_new_location : function(p_instance:Plibvlc_instance_t; psz_mrl:pcchar):plibvlc_media_t; cdecl;
    libvlc_media_new_path : function(p_instance:Plibvlc_instance_t; path:pcchar):plibvlc_media_t; cdecl;
    libvlc_media_new_fd : function(p_instance:Plibvlc_instance_t; fd:cint):plibvlc_media_t; cdecl;
    libvlc_media_new_as_node : function(p_instance:Plibvlc_instance_t; psz_name:pcchar):plibvlc_media_t; cdecl;
    libvlc_media_add_option : procedure(p_md:Plibvlc_media_t; ppsz_options:pcchar); cdecl;
    libvlc_media_add_option_flag : procedure(p_md:Plibvlc_media_t; ppsz_options:pcchar; i_flags:cunsigned); cdecl;
    libvlc_media_retain : procedure(p_md:Plibvlc_media_t); cdecl;
    libvlc_media_release : procedure(p_md:Plibvlc_media_t); cdecl;
    libvlc_media_get_mrl : function(p_md:Plibvlc_media_t):pcchar; cdecl;
    libvlc_media_duplicate : function(p_md:Plibvlc_media_t):plibvlc_media_t; cdecl;
    libvlc_media_get_meta : function(p_md:Plibvlc_media_t; e_meta:libvlc_meta_t):pcchar; cdecl;
    libvlc_media_set_meta : procedure(p_md:Plibvlc_media_t; e_meta:libvlc_meta_t; psz_value:pcchar); cdecl;
    libvlc_media_save_meta : function(p_md:Plibvlc_media_t):cint; cdecl;
    libvlc_media_get_state : function(p_md:Plibvlc_media_t):libvlc_state_t; cdecl;
    libvlc_media_get_stats : function(p_md:Plibvlc_media_t; p_stats:Plibvlc_media_stats_t):cint; cdecl;
    libvlc_media_subitems : function(p_md:Plibvlc_media_t):plibvlc_media_list_t; cdecl;
    libvlc_media_event_manager : function(p_md:Plibvlc_media_t):plibvlc_event_manager_t; cdecl;
    libvlc_media_get_duration : function(p_md:Plibvlc_media_t):libvlc_time_t; cdecl;
    libvlc_media_parse : procedure(p_md:Plibvlc_media_t); cdecl;
    libvlc_media_parse_async : procedure(p_md:Plibvlc_media_t); cdecl;
    libvlc_media_is_parsed : function(p_md:Plibvlc_media_t):cint; cdecl;
    libvlc_media_set_user_data : procedure(p_md:Plibvlc_media_t; p_new_user_data:pointer); cdecl;
    libvlc_media_get_user_data : function(p_md:Plibvlc_media_t):pointer; cdecl;
    libvlc_media_get_tracks_info : function(p_md:Plibvlc_media_t; tracks:PPlibvlc_media_track_info_t):cint; cdecl;
    libvlc_module_description_list_release : procedure(p_list:Plibvlc_module_description_t); cdecl;
    libvlc_audio_filter_list_get : function(p_instance:Plibvlc_instance_t):plibvlc_module_description_t; cdecl;
    libvlc_video_filter_list_get : function(p_instance:Plibvlc_instance_t):plibvlc_module_description_t; cdecl;
    libvlc_clock : function:int64_t; cdecl;

    libvlc_errmsg : function:pcchar; cdecl;
    libvlc_clearerr : procedure; cdecl;
    libvlc_printerr : function(fmt:pcchar):pcchar;varargs; cdecl;

    libvlc_new : function(argc:cint; argv:Ppcchar):plibvlc_instance_t; cdecl;
    libvlc_release : procedure(p_instance:Plibvlc_instance_t); cdecl;
    libvlc_retain : procedure(p_instance:Plibvlc_instance_t); cdecl;
    libvlc_add_intf : function(p_instance:Plibvlc_instance_t; name:pcchar):cint; cdecl;
    libvlc_set_exit_handler : procedure(p_instance:Plibvlc_instance_t; cb:cbtype1; opaque:pointer); cdecl;
    libvlc_wait : procedure(p_instance:Plibvlc_instance_t); cdecl;
    libvlc_set_user_agent : procedure(p_instance:Plibvlc_instance_t; name:pcchar; http:pcchar); cdecl;
    libvlc_get_version : function:pcchar; cdecl;
    libvlc_get_compiler : function:pcchar; cdecl;
    libvlc_get_changeset : function:pcchar; cdecl;
    libvlc_free : procedure(ptr:pointer); cdecl;
    libvlc_event_attach : function(p_event_manager:Plibvlc_event_manager_t; i_event_type:libvlc_event_type_t; f_callback:libvlc_callback_t; user_data:pointer):cint; cdecl;
    libvlc_event_detach : procedure(p_event_manager:Plibvlc_event_manager_t; i_event_type:libvlc_event_type_t; f_callback:libvlc_callback_t; p_user_data:pointer); cdecl;
    libvlc_event_type_name : function(event_type:libvlc_event_type_t):pcchar; cdecl;
    libvlc_get_log_verbosity : function(p_instance:Plibvlc_instance_t):cunsigned; cdecl;
    libvlc_set_log_verbosity : procedure(p_instance:Plibvlc_instance_t; level:cunsigned); cdecl;
    libvlc_log_open : function(p_instance:Plibvlc_instance_t):plibvlc_log_t; cdecl;
    libvlc_log_close : procedure(p_log:Plibvlc_log_t); cdecl;
    libvlc_log_count : function(p_log:Plibvlc_log_t):cunsigned; cdecl;
    libvlc_log_clear : procedure(p_log:Plibvlc_log_t); cdecl;
    libvlc_log_get_iterator : function(p_log:Plibvlc_log_t):plibvlc_log_iterator_t; cdecl;
    libvlc_log_iterator_free : procedure(p_iter:Plibvlc_log_iterator_t); cdecl;
    libvlc_log_iterator_has_next : function(p_iter:Plibvlc_log_iterator_t):cint; cdecl;
    libvlc_log_iterator_next : function(p_iter:Plibvlc_log_iterator_t; p_buffer:Plibvlc_log_message_t):plibvlc_log_message_t; cdecl;
    libvlc_audio_output_list_get : function(p_instance:Plibvlc_instance_t):plibvlc_audio_output_t; cdecl;
    libvlc_audio_output_list_release : procedure(p_list:Plibvlc_audio_output_t); cdecl;
    libvlc_audio_output_set : function(p_mi:Plibvlc_media_player_t; psz_name:pcchar):cint; cdecl;
    libvlc_audio_output_device_count : function(p_instance:Plibvlc_instance_t; psz_audio_output:pcchar):cint; cdecl;
    libvlc_audio_output_device_longname : function(p_instance:Plibvlc_instance_t; psz_audio_output:pcchar; i_device:cint):pcchar; cdecl;
    libvlc_audio_output_device_id : function(p_instance:Plibvlc_instance_t; psz_audio_output:pcchar; i_device:cint):pcchar; cdecl;
    libvlc_audio_output_device_set : procedure(p_mi:Plibvlc_media_player_t; psz_audio_output:pcchar; psz_device_id:pcchar); cdecl;
    libvlc_audio_output_get_device_type : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_audio_output_set_device_type : procedure(p_mi:Plibvlc_media_player_t; device_type:cint); cdecl;
    libvlc_audio_toggle_mute : procedure(p_mi:Plibvlc_media_player_t); cdecl;
    libvlc_audio_get_mute : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_audio_set_mute : procedure(p_mi:Plibvlc_media_player_t; status:cint); cdecl;
    libvlc_audio_get_volume : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_audio_set_volume : function(p_mi:Plibvlc_media_player_t; i_volume:cint):cint; cdecl;
    libvlc_audio_get_track_count : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_audio_get_track_description : function(p_mi:Plibvlc_media_player_t):plibvlc_track_description_t; cdecl;
    libvlc_audio_get_track : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_audio_set_track : function(p_mi:Plibvlc_media_player_t; i_track:cint):cint; cdecl;
    libvlc_audio_get_channel : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_audio_set_channel : function(p_mi:Plibvlc_media_player_t; channel:cint):cint; cdecl;
    libvlc_audio_get_delay : function(p_mi:Plibvlc_media_player_t):int64_t; cdecl;
    libvlc_audio_set_delay : function(p_mi:Plibvlc_media_player_t; i_delay:int64_t):cint; cdecl;
    libvlc_media_list_new : function(p_instance:Plibvlc_instance_t):plibvlc_media_list_t; cdecl;
    libvlc_media_list_release : procedure(p_ml:Plibvlc_media_list_t); cdecl;
    libvlc_media_list_retain : procedure(p_ml:Plibvlc_media_list_t); cdecl;

    libvlc_media_list_add_file_content : function(p_ml:Plibvlc_media_list_t; psz_uri:pcchar):cint; cdecl;
    libvlc_media_list_set_media : procedure(p_ml:Plibvlc_media_list_t; p_md:Plibvlc_media_t); cdecl;
    libvlc_media_list_media : function(p_ml:Plibvlc_media_list_t):plibvlc_media_t; cdecl;
    libvlc_media_list_add_media : function(p_ml:Plibvlc_media_list_t; p_md:Plibvlc_media_t):cint; cdecl;
    libvlc_media_list_insert_media : function(p_ml:Plibvlc_media_list_t; p_md:Plibvlc_media_t; i_pos:cint):cint; cdecl;
    libvlc_media_list_remove_index : function(p_ml:Plibvlc_media_list_t; i_pos:cint):cint; cdecl;
    libvlc_media_list_count : function(p_ml:Plibvlc_media_list_t):cint; cdecl;
    libvlc_media_list_item_at_index : function(p_ml:Plibvlc_media_list_t; i_pos:cint):plibvlc_media_t; cdecl;
    libvlc_media_list_index_of_item : function(p_ml:Plibvlc_media_list_t; p_md:Plibvlc_media_t):cint; cdecl;
    libvlc_media_list_is_readonly : function(p_ml:Plibvlc_media_list_t):cint; cdecl;
    libvlc_media_list_lock : procedure(p_ml:Plibvlc_media_list_t); cdecl;
    libvlc_media_list_unlock : procedure(p_ml:Plibvlc_media_list_t); cdecl;
    libvlc_media_list_event_manager : function(p_ml:Plibvlc_media_list_t):plibvlc_event_manager_t; cdecl;
    libvlc_media_list_player_new : function(p_instance:Plibvlc_instance_t):plibvlc_media_list_player_t; cdecl;
    libvlc_media_list_player_release : procedure(p_mlp:Plibvlc_media_list_player_t); cdecl;
    libvlc_media_list_player_retain : procedure(p_mlp:Plibvlc_media_list_player_t); cdecl;
    libvlc_media_list_player_event_manager : function(p_mlp:Plibvlc_media_list_player_t):plibvlc_event_manager_t; cdecl;
    libvlc_media_list_player_set_media_player : procedure(p_mlp:Plibvlc_media_list_player_t; p_mi:Plibvlc_media_player_t); cdecl;
    libvlc_media_list_player_set_media_list : procedure(p_mlp:Plibvlc_media_list_player_t; p_mlist:Plibvlc_media_list_t); cdecl;
    libvlc_media_list_player_play : procedure(p_mlp:Plibvlc_media_list_player_t); cdecl;
    libvlc_media_list_player_pause : procedure(p_mlp:Plibvlc_media_list_player_t); cdecl;
    libvlc_media_list_player_is_playing : function(p_mlp:Plibvlc_media_list_player_t):cint; cdecl;
    libvlc_media_list_player_get_state : function(p_mlp:Plibvlc_media_list_player_t):libvlc_state_t; cdecl;
    libvlc_media_list_player_play_item_at_index : function(p_mlp:Plibvlc_media_list_player_t; i_index:cint):cint; cdecl;
    libvlc_media_list_player_play_item : function(p_mlp:Plibvlc_media_list_player_t; p_md:Plibvlc_media_t):cint; cdecl;
    libvlc_media_list_player_stop : procedure(p_mlp:Plibvlc_media_list_player_t); cdecl;
    libvlc_media_list_player_next : function(p_mlp:Plibvlc_media_list_player_t):cint; cdecl;
    libvlc_media_list_player_previous : function(p_mlp:Plibvlc_media_list_player_t):cint; cdecl;
    libvlc_media_list_player_set_playback_mode : procedure(p_mlp:Plibvlc_media_list_player_t; e_mode:libvlc_playback_mode_t); cdecl;
    libvlc_media_library_new : function(p_instance:Plibvlc_instance_t):plibvlc_media_library_t; cdecl;
    libvlc_media_library_release : procedure(p_mlib:Plibvlc_media_library_t); cdecl;
    libvlc_media_library_retain : procedure(p_mlib:Plibvlc_media_library_t); cdecl;
    libvlc_media_library_load : function(p_mlib:Plibvlc_media_library_t):cint; cdecl;
    libvlc_media_library_media_list : function(p_mlib:Plibvlc_media_library_t):plibvlc_media_list_t; cdecl;
    libvlc_video_get_adjust_int : function(p_mi:Plibvlc_media_player_t; option:cunsigned):cint; cdecl;
    libvlc_video_set_adjust_int : procedure(p_mi:Plibvlc_media_player_t; option:cunsigned; value:cint); cdecl;
    libvlc_video_get_adjust_float : function(p_mi:Plibvlc_media_player_t; option:cunsigned):cfloat; cdecl;
    libvlc_video_set_adjust_float : procedure(p_mi:Plibvlc_media_player_t; option:cunsigned; value:cfloat); cdecl;
    libvlc_video_get_logo_int : function(p_mi:Plibvlc_media_player_t; option:cunsigned):cint; cdecl;
    libvlc_video_set_logo_int : procedure(p_mi:Plibvlc_media_player_t; option:cunsigned; value:cint); cdecl;
    libvlc_video_set_logo_string : procedure(p_mi:Plibvlc_media_player_t; option:cunsigned; psz_value:pcchar); cdecl;
    libvlc_audio_set_format_callbacks : procedure(mp:Plibvlc_media_player_t; setup:libvlc_audio_setup_cb; cleanup:libvlc_audio_cleanup_cb); cdecl;
    libvlc_audio_set_format : procedure(mp:Plibvlc_media_player_t; format:pcchar; rate:cunsigned; channels:cunsigned); cdecl;
    libvlc_media_player_get_length : function(p_mi:Plibvlc_media_player_t):libvlc_time_t; cdecl;
    libvlc_media_player_get_time : function(p_mi:Plibvlc_media_player_t):libvlc_time_t; cdecl;
    libvlc_media_player_set_time : procedure(p_mi:Plibvlc_media_player_t; i_time:libvlc_time_t); cdecl;
    libvlc_media_player_get_position : function(p_mi:Plibvlc_media_player_t):cfloat; cdecl;
    libvlc_media_player_set_position : procedure(p_mi:Plibvlc_media_player_t; f_pos:cfloat); cdecl;
    libvlc_media_player_set_chapter : procedure(p_mi:Plibvlc_media_player_t; i_chapter:cint); cdecl;
    libvlc_media_player_get_chapter : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_media_player_get_chapter_count : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_media_player_will_play : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_media_player_get_chapter_count_for_title : function(p_mi:Plibvlc_media_player_t; i_title:cint):cint; cdecl;
    libvlc_media_player_set_title : procedure(p_mi:Plibvlc_media_player_t; i_title:cint); cdecl;
    libvlc_media_player_get_title : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_media_player_get_title_count : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_media_player_previous_chapter : procedure(p_mi:Plibvlc_media_player_t); cdecl;
    libvlc_media_player_next_chapter : procedure(p_mi:Plibvlc_media_player_t); cdecl;
    libvlc_media_player_get_rate : function(p_mi:Plibvlc_media_player_t):cfloat; cdecl;
    libvlc_media_player_set_rate : function(p_mi:Plibvlc_media_player_t; rate:cfloat):cint; cdecl;
    libvlc_media_player_get_state : function(p_mi:Plibvlc_media_player_t):libvlc_state_t; cdecl;
    libvlc_media_player_get_fps : function(p_mi:Plibvlc_media_player_t):cfloat; cdecl;
    libvlc_media_player_has_vout : function(p_mi:Plibvlc_media_player_t):cunsigned; cdecl;
    libvlc_media_player_is_seekable : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_media_player_can_pause : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_media_player_next_frame : procedure(p_mi:Plibvlc_media_player_t); cdecl;
    libvlc_media_player_navigate : procedure(p_mi:Plibvlc_media_player_t; navigate:cunsigned); cdecl;
    libvlc_track_description_list_release : procedure(p_track_description:Plibvlc_track_description_t); cdecl;
    libvlc_track_description_release : procedure(p_track_description:Plibvlc_track_description_t); cdecl;
    libvlc_toggle_fullscreen : procedure(p_mi:Plibvlc_media_player_t); cdecl;
    libvlc_set_fullscreen : procedure(p_mi:Plibvlc_media_player_t; b_fullscreen:cint); cdecl;
    libvlc_get_fullscreen : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_video_set_key_input : procedure(p_mi:Plibvlc_media_player_t; on:cunsigned); cdecl;
    libvlc_video_set_mouse_input : procedure(p_mi:Plibvlc_media_player_t; on:cunsigned); cdecl;
    libvlc_video_get_size : function(p_mi:Plibvlc_media_player_t; num:cunsigned; px:pcunsigned; py:pcunsigned):cint; cdecl;
    libvlc_video_get_height : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_video_get_width : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_video_get_cursor : function(p_mi:Plibvlc_media_player_t; num:cunsigned; px:pcint; py:pcint):cint; cdecl;
    libvlc_video_get_scale : function(p_mi:Plibvlc_media_player_t):cfloat; cdecl;
    libvlc_video_set_scale : procedure(p_mi:Plibvlc_media_player_t; f_factor:cfloat); cdecl;
    libvlc_video_get_aspect_ratio : function(p_mi:Plibvlc_media_player_t):pcchar; cdecl;
    libvlc_video_set_aspect_ratio : procedure(p_mi:Plibvlc_media_player_t; psz_aspect:pcchar); cdecl;
    libvlc_video_get_spu : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_video_get_spu_count : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_video_get_spu_description : function(p_mi:Plibvlc_media_player_t):plibvlc_track_description_t; cdecl;
    libvlc_video_set_spu : function(p_mi:Plibvlc_media_player_t; i_spu:cunsigned):cint; cdecl;
    libvlc_video_set_subtitle_file : function(p_mi:Plibvlc_media_player_t; psz_subtitle:pcchar):cint; cdecl;
    libvlc_video_get_spu_delay : function(p_mi:Plibvlc_media_player_t):int64_t; cdecl;
    libvlc_video_set_spu_delay : function(p_mi:Plibvlc_media_player_t; i_delay:int64_t):cint; cdecl;
    libvlc_video_get_title_description : function(p_mi:Plibvlc_media_player_t):plibvlc_track_description_t; cdecl;
    libvlc_video_get_chapter_description : function(p_mi:Plibvlc_media_player_t; i_title:cint):plibvlc_track_description_t; cdecl;
    libvlc_video_get_crop_geometry : function(p_mi:Plibvlc_media_player_t):pcchar; cdecl;
    libvlc_video_set_crop_geometry : procedure(p_mi:Plibvlc_media_player_t; psz_geometry:pcchar); cdecl;
    libvlc_video_get_teletext : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_video_set_teletext : procedure(p_mi:Plibvlc_media_player_t; i_page:cint); cdecl;
    libvlc_toggle_teletext : procedure(p_mi:Plibvlc_media_player_t); cdecl;
    libvlc_video_get_track_count : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_video_get_track_description : function(p_mi:Plibvlc_media_player_t):plibvlc_track_description_t; cdecl;
    libvlc_video_get_track : function(p_mi:Plibvlc_media_player_t):cint; cdecl;
    libvlc_video_set_track : function(p_mi:Plibvlc_media_player_t; i_track:cint):cint; cdecl;
    libvlc_video_take_snapshot : function(p_mi:Plibvlc_media_player_t; num:cunsigned; psz_filepath:pcchar; i_width:cuint; i_height:cuint):cint; cdecl;
    libvlc_video_set_deinterlace : procedure(p_mi:Plibvlc_media_player_t; psz_mode:pcchar); cdecl;
    libvlc_video_get_marquee_int : function(p_mi:Plibvlc_media_player_t; option:cunsigned):cint; cdecl;
    libvlc_video_get_marquee_string : function(p_mi:Plibvlc_media_player_t; option:cunsigned):pcchar; cdecl;
    libvlc_video_set_marquee_int : procedure(p_mi:Plibvlc_media_player_t; option:cunsigned; i_val:cint); cdecl;
    libvlc_video_set_marquee_string : procedure(p_mi:Plibvlc_media_player_t; option:cunsigned; psz_text:pcchar); cdecl;
    libvlc_audio_set_callbacks : procedure(mp:Plibvlc_media_player_t; play:libvlc_audio_play_cb; pause:libvlc_audio_pause_cb; resume:libvlc_audio_resume_cb; flush:libvlc_audio_flush_cb; 
      drain:libvlc_audio_drain_cb; opaque:pointer); cdecl;
    libvlc_audio_set_volume_callback : procedure(mp:Plibvlc_media_player_t; set_volume:libvlc_audio_set_volume_cb); cdecl;
    libvlc_video_set_callbacks : procedure(mp:Plibvlc_media_player_t; lock:libvlc_video_lock_cb; unlock:libvlc_video_unlock_cb; display:libvlc_video_display_cb; opaque:pointer); cdecl;
    libvlc_video_set_format : procedure(mp:Plibvlc_media_player_t; chroma:pcchar; width:cunsigned; height:cunsigned; pitch:cunsigned); cdecl;
    libvlc_video_set_format_callbacks : procedure(mp:Plibvlc_media_player_t; setup:libvlc_video_format_cb; cleanup:libvlc_video_cleanup_cb); cdecl;
    libvlc_media_player_set_nsobject : procedure(p_mi:Plibvlc_media_player_t; drawable:pointer); cdecl;
    libvlc_media_player_get_nsobject : function(p_mi:Plibvlc_media_player_t):pointer; cdecl;
    libvlc_media_player_set_agl : procedure(p_mi:Plibvlc_media_player_t; drawable:uint32_t); cdecl;
    libvlc_media_player_get_agl : function(p_mi:Plibvlc_media_player_t):uint32_t; cdecl;
    libvlc_media_player_set_xwindow : procedure(p_mi:Plibvlc_media_player_t; drawable:uint32_t); cdecl;
    libvlc_media_player_get_xwindow : function(p_mi:Plibvlc_media_player_t):uint32_t; cdecl;
    libvlc_media_player_set_hwnd : procedure(p_mi:Plibvlc_media_player_t; drawable:pointer); cdecl;
    libvlc_media_player_get_hwnd : function(p_mi:Plibvlc_media_player_t):pointer; cdecl;
    libvlc_media_discoverer_new_from_name : function(p_inst:Plibvlc_instance_t; psz_name:pcchar):plibvlc_media_discoverer_t; cdecl;
    libvlc_media_discoverer_release : procedure(p_mdis:Plibvlc_media_discoverer_t); cdecl;
    libvlc_media_discoverer_localized_name : function(p_mdis:Plibvlc_media_discoverer_t):pcchar; cdecl;
    libvlc_media_discoverer_media_list : function(p_mdis:Plibvlc_media_discoverer_t):plibvlc_media_list_t; cdecl;
    libvlc_media_discoverer_event_manager : function(p_mdis:Plibvlc_media_discoverer_t):plibvlc_event_manager_t; cdecl;
    libvlc_media_discoverer_is_running : function(p_mdis:Plibvlc_media_discoverer_t):cint; cdecl;
    libvlc_vlm_release : procedure(p_instance:Plibvlc_instance_t); cdecl;
    libvlc_vlm_add_broadcast : function(p_instance:Plibvlc_instance_t; psz_name:pcchar; psz_input:pcchar; psz_output:pcchar; i_options:cint; 
      ppsz_options:Ppcchar; b_enabled:cint; b_loop:cint):cint; cdecl;
    libvlc_vlm_add_vod : function(p_instance:Plibvlc_instance_t; psz_name:pcchar; psz_input:pcchar; i_options:cint; ppsz_options:Ppcchar; 
      b_enabled:cint; psz_mux:pcchar):cint; cdecl;
    libvlc_vlm_del_media : function(p_instance:Plibvlc_instance_t; psz_name:pcchar):cint; cdecl;
    libvlc_vlm_set_enabled : function(p_instance:Plibvlc_instance_t; psz_name:pcchar; b_enabled:cint):cint; cdecl;
    libvlc_vlm_set_output : function(p_instance:Plibvlc_instance_t; psz_name:pcchar; psz_output:pcchar):cint; cdecl;
    libvlc_vlm_set_input : function(p_instance:Plibvlc_instance_t; psz_name:pcchar; psz_input:pcchar):cint; cdecl;
    libvlc_vlm_add_input : function(p_instance:Plibvlc_instance_t; psz_name:pcchar; psz_input:pcchar):cint; cdecl;
    libvlc_vlm_set_loop : function(p_instance:Plibvlc_instance_t; psz_name:pcchar; b_loop:cint):cint; cdecl;
    libvlc_vlm_set_mux : function(p_instance:Plibvlc_instance_t; psz_name:pcchar; psz_mux:pcchar):cint; cdecl;
    libvlc_vlm_change_media : function(p_instance:Plibvlc_instance_t; psz_name:pcchar; psz_input:pcchar; psz_output:pcchar; i_options:cint; 
      ppsz_options:Ppcchar; b_enabled:cint; b_loop:cint):cint; cdecl;
    libvlc_vlm_play_media : function(p_instance:Plibvlc_instance_t; psz_name:pcchar):cint; cdecl;
    libvlc_vlm_stop_media : function(p_instance:Plibvlc_instance_t; psz_name:pcchar):cint; cdecl;
    libvlc_vlm_pause_media : function(p_instance:Plibvlc_instance_t; psz_name:pcchar):cint; cdecl;
    libvlc_vlm_seek_media : function(p_instance:Plibvlc_instance_t; psz_name:pcchar; f_percentage:cfloat):cint; cdecl;
    libvlc_vlm_show_media : function(p_instance:Plibvlc_instance_t; psz_name:pcchar):pcchar; cdecl;
    libvlc_vlm_get_media_instance_position : function(p_instance:Plibvlc_instance_t; psz_name:pcchar; i_instance:cint):cfloat; cdecl;
    libvlc_vlm_get_media_instance_time : function(p_instance:Plibvlc_instance_t; psz_name:pcchar; i_instance:cint):cint; cdecl;
    libvlc_vlm_get_media_instance_length : function(p_instance:Plibvlc_instance_t; psz_name:pcchar; i_instance:cint):cint; cdecl;
    libvlc_vlm_get_media_instance_rate : function(p_instance:Plibvlc_instance_t; psz_name:pcchar; i_instance:cint):cint; cdecl;
    libvlc_vlm_get_event_manager : function(p_instance:Plibvlc_instance_t):plibvlc_event_manager_t; cdecl;
    libvlc_playlist_play : procedure(p_instance:Plibvlc_instance_t; i_id:cint; i_options:cint; ppsz_options:Ppcchar); cdecl;

Procedure Freelibvlc;
Procedure Loadlibvlc(lib : AnsiString; CheckProcNames : Boolean = False);

implementation

uses
  SysUtils, dynlibs;

var
  hlib : tlibhandle;
  LibRefCount : Integer;

procedure Freelibvlc;

begin
  if (LibRefCount>0) then
    Dec(LibRefCount);
  if LibRefCount>0 then
    exit;
  FreeLibrary(hlib);
  libvlc_errmsg:=nil;
  libvlc_clearerr:=nil;
  libvlc_printerr:=nil;
  libvlc_new:=nil;
  libvlc_release:=nil;
  libvlc_retain:=nil;
  libvlc_add_intf:=nil;
  libvlc_set_exit_handler:=nil;
  libvlc_wait:=nil;
  libvlc_set_user_agent:=nil;
  libvlc_get_version:=nil;
  libvlc_get_compiler:=nil;
  libvlc_get_changeset:=nil;
  libvlc_free:=nil;
  libvlc_event_attach:=nil;
  libvlc_event_detach:=nil;
  libvlc_event_type_name:=nil;
  libvlc_get_log_verbosity:=nil;
  libvlc_set_log_verbosity:=nil;
  libvlc_log_open:=nil;
  libvlc_log_close:=nil;
  libvlc_log_count:=nil;
  libvlc_log_clear:=nil;
  libvlc_log_get_iterator:=nil;
  libvlc_log_iterator_free:=nil;
  libvlc_log_iterator_has_next:=nil;
  libvlc_log_iterator_next:=nil;
  libvlc_module_description_list_release:=nil;
  libvlc_audio_filter_list_get:=nil;
  libvlc_video_filter_list_get:=nil;
  libvlc_clock:=nil;
  libvlc_media_new_location:=nil;
  libvlc_media_new_path:=nil;
  libvlc_media_new_fd:=nil;
  libvlc_media_new_as_node:=nil;
  libvlc_media_add_option:=nil;
  libvlc_media_add_option_flag:=nil;
  libvlc_media_retain:=nil;
  libvlc_media_release:=nil;
  libvlc_media_get_mrl:=nil;
  libvlc_media_duplicate:=nil;
  libvlc_media_get_meta:=nil;
  libvlc_media_set_meta:=nil;
  libvlc_media_save_meta:=nil;
  libvlc_media_get_state:=nil;
  libvlc_media_get_stats:=nil;
  libvlc_media_subitems:=nil;
  libvlc_media_event_manager:=nil;
  libvlc_media_get_duration:=nil;
  libvlc_media_parse:=nil;
  libvlc_media_parse_async:=nil;
  libvlc_media_is_parsed:=nil;
  libvlc_media_set_user_data:=nil;
  libvlc_media_get_user_data:=nil;
  libvlc_media_get_tracks_info:=nil;
  libvlc_media_player_new:=nil;
  libvlc_media_player_new_from_media:=nil;
  libvlc_media_player_release:=nil;
  libvlc_media_player_retain:=nil;
  libvlc_media_player_set_media:=nil;
  libvlc_media_player_get_media:=nil;
  libvlc_media_player_event_manager:=nil;
  libvlc_media_player_is_playing:=nil;
  libvlc_media_player_play:=nil;
  libvlc_media_player_set_pause:=nil;
  libvlc_media_player_pause:=nil;
  libvlc_media_player_stop:=nil;
  libvlc_video_set_callbacks:=nil;
  libvlc_video_set_format:=nil;
  libvlc_video_set_format_callbacks:=nil;
  libvlc_media_player_set_nsobject:=nil;
  libvlc_media_player_get_nsobject:=nil;
  libvlc_media_player_set_agl:=nil;
  libvlc_media_player_get_agl:=nil;
  libvlc_media_player_set_xwindow:=nil;
  libvlc_media_player_get_xwindow:=nil;
  libvlc_media_player_set_hwnd:=nil;
  libvlc_media_player_get_hwnd:=nil;
  libvlc_audio_set_callbacks:=nil;
  libvlc_audio_set_volume_callback:=nil;
  libvlc_audio_set_format_callbacks:=nil;
  libvlc_audio_set_format:=nil;
  libvlc_media_player_get_length:=nil;
  libvlc_media_player_get_time:=nil;
  libvlc_media_player_set_time:=nil;
  libvlc_media_player_get_position:=nil;
  libvlc_media_player_set_position:=nil;
  libvlc_media_player_set_chapter:=nil;
  libvlc_media_player_get_chapter:=nil;
  libvlc_media_player_get_chapter_count:=nil;
  libvlc_media_player_will_play:=nil;
  libvlc_media_player_get_chapter_count_for_title:=nil;
  libvlc_media_player_set_title:=nil;
  libvlc_media_player_get_title:=nil;
  libvlc_media_player_get_title_count:=nil;
  libvlc_media_player_previous_chapter:=nil;
  libvlc_media_player_next_chapter:=nil;
  libvlc_media_player_get_rate:=nil;
  libvlc_media_player_set_rate:=nil;
  libvlc_media_player_get_state:=nil;
  libvlc_media_player_get_fps:=nil;
  libvlc_media_player_has_vout:=nil;
  libvlc_media_player_is_seekable:=nil;
  libvlc_media_player_can_pause:=nil;
  libvlc_media_player_next_frame:=nil;
  libvlc_media_player_navigate:=nil;
  libvlc_track_description_list_release:=nil;
  libvlc_track_description_release:=nil;
  libvlc_toggle_fullscreen:=nil;
  libvlc_set_fullscreen:=nil;
  libvlc_get_fullscreen:=nil;
  libvlc_video_set_key_input:=nil;
  libvlc_video_set_mouse_input:=nil;
  libvlc_video_get_size:=nil;
  libvlc_video_get_height:=nil;
  libvlc_video_get_width:=nil;
  libvlc_video_get_cursor:=nil;
  libvlc_video_get_scale:=nil;
  libvlc_video_set_scale:=nil;
  libvlc_video_get_aspect_ratio:=nil;
  libvlc_video_set_aspect_ratio:=nil;
  libvlc_video_get_spu:=nil;
  libvlc_video_get_spu_count:=nil;
  libvlc_video_get_spu_description:=nil;
  libvlc_video_set_spu:=nil;
  libvlc_video_set_subtitle_file:=nil;
  libvlc_video_get_spu_delay:=nil;
  libvlc_video_set_spu_delay:=nil;
  libvlc_video_get_title_description:=nil;
  libvlc_video_get_chapter_description:=nil;
  libvlc_video_get_crop_geometry:=nil;
  libvlc_video_set_crop_geometry:=nil;
  libvlc_video_get_teletext:=nil;
  libvlc_video_set_teletext:=nil;
  libvlc_toggle_teletext:=nil;
  libvlc_video_get_track_count:=nil;
  libvlc_video_get_track_description:=nil;
  libvlc_video_get_track:=nil;
  libvlc_video_set_track:=nil;
  libvlc_video_take_snapshot:=nil;
  libvlc_video_set_deinterlace:=nil;
  libvlc_video_get_marquee_int:=nil;
  libvlc_video_get_marquee_string:=nil;
  libvlc_video_set_marquee_int:=nil;
  libvlc_video_set_marquee_string:=nil;
  libvlc_video_get_logo_int:=nil;
  libvlc_video_set_logo_int:=nil;
  libvlc_video_set_logo_string:=nil;
  libvlc_video_get_adjust_int:=nil;
  libvlc_video_set_adjust_int:=nil;
  libvlc_video_get_adjust_float:=nil;
  libvlc_video_set_adjust_float:=nil;
  libvlc_audio_output_list_get:=nil;
  libvlc_audio_output_list_release:=nil;
  libvlc_audio_output_set:=nil;
  libvlc_audio_output_device_count:=nil;
  libvlc_audio_output_device_longname:=nil;
  libvlc_audio_output_device_id:=nil;
  libvlc_audio_output_device_set:=nil;
  libvlc_audio_output_get_device_type:=nil;
  libvlc_audio_output_set_device_type:=nil;
  libvlc_audio_toggle_mute:=nil;
  libvlc_audio_get_mute:=nil;
  libvlc_audio_set_mute:=nil;
  libvlc_audio_get_volume:=nil;
  libvlc_audio_set_volume:=nil;
  libvlc_audio_get_track_count:=nil;
  libvlc_audio_get_track_description:=nil;
  libvlc_audio_get_track:=nil;
  libvlc_audio_set_track:=nil;
  libvlc_audio_get_channel:=nil;
  libvlc_audio_set_channel:=nil;
  libvlc_audio_get_delay:=nil;
  libvlc_audio_set_delay:=nil;
  libvlc_media_list_new:=nil;
  libvlc_media_list_release:=nil;
  libvlc_media_list_retain:=nil;
  libvlc_media_list_add_file_content:=nil;
  libvlc_media_list_set_media:=nil;
  libvlc_media_list_media:=nil;
  libvlc_media_list_add_media:=nil;
  libvlc_media_list_insert_media:=nil;
  libvlc_media_list_remove_index:=nil;
  libvlc_media_list_count:=nil;
  libvlc_media_list_item_at_index:=nil;
  libvlc_media_list_index_of_item:=nil;
  libvlc_media_list_is_readonly:=nil;
  libvlc_media_list_lock:=nil;
  libvlc_media_list_unlock:=nil;
  libvlc_media_list_event_manager:=nil;
  libvlc_media_list_player_new:=nil;
  libvlc_media_list_player_release:=nil;
  libvlc_media_list_player_retain:=nil;
  libvlc_media_list_player_event_manager:=nil;
  libvlc_media_list_player_set_media_player:=nil;
  libvlc_media_list_player_set_media_list:=nil;
  libvlc_media_list_player_play:=nil;
  libvlc_media_list_player_pause:=nil;
  libvlc_media_list_player_is_playing:=nil;
  libvlc_media_list_player_get_state:=nil;
  libvlc_media_list_player_play_item_at_index:=nil;
  libvlc_media_list_player_play_item:=nil;
  libvlc_media_list_player_stop:=nil;
  libvlc_media_list_player_next:=nil;
  libvlc_media_list_player_previous:=nil;
  libvlc_media_list_player_set_playback_mode:=nil;
  libvlc_media_library_new:=nil;
  libvlc_media_library_release:=nil;
  libvlc_media_library_retain:=nil;
  libvlc_media_library_load:=nil;
  libvlc_media_library_media_list:=nil;
  libvlc_media_discoverer_new_from_name:=nil;
  libvlc_media_discoverer_release:=nil;
  libvlc_media_discoverer_localized_name:=nil;
  libvlc_media_discoverer_media_list:=nil;
  libvlc_media_discoverer_event_manager:=nil;
  libvlc_media_discoverer_is_running:=nil;
  libvlc_vlm_release:=nil;
  libvlc_vlm_add_broadcast:=nil;
  libvlc_vlm_add_vod:=nil;
  libvlc_vlm_del_media:=nil;
  libvlc_vlm_set_enabled:=nil;
  libvlc_vlm_set_output:=nil;
  libvlc_vlm_set_input:=nil;
  libvlc_vlm_add_input:=nil;
  libvlc_vlm_set_loop:=nil;
  libvlc_vlm_set_mux:=nil;
  libvlc_vlm_change_media:=nil;
  libvlc_vlm_play_media:=nil;
  libvlc_vlm_stop_media:=nil;
  libvlc_vlm_pause_media:=nil;
  libvlc_vlm_seek_media:=nil;
  libvlc_vlm_show_media:=nil;
  libvlc_vlm_get_media_instance_position:=nil;
  libvlc_vlm_get_media_instance_time:=nil;
  libvlc_vlm_get_media_instance_length:=nil;
  libvlc_vlm_get_media_instance_rate:=nil;
  libvlc_vlm_get_event_manager:=nil;
  libvlc_playlist_play:=nil;
end;


Procedure Loadlibvlc(lib : AnsiString; CheckProcNames : Boolean = False);

  Function GetProcAddress(h : TLibHandle; Name : AnsiString) : Pointer;
  
  begin
    Result:=dynlibs.GetProcAddress(h,Name);
    If (Result=Nil) and CheckProcNames then
      raise Exception.CreateFmt('Could not find procedure address: %s ',[Name]);
  end;
  
  Procedure EM(FN : String);
  
  begin
    {$ifndef VER2_6}
    Raise Exception.CreateFmt('Could not load library "%s": %s',[FN,GetLoadErrorStr]);
    {$else}
    raise Exception.CreateFmt('Could not load library "%s"',[FN]);
    {$endif}
  end;
  
  
  
Var
  D : String;
  
begin
  if (hLib<>NilHandle) then
    begin
    Inc(LibRefCount);
    Exit;
    end;
  D:=ExtractFilePath(lib);
  {$ifdef windows}
  if (LoadLibrary(d+corelibname)=NilHandle) then
    if (d='') and (LoadLibrary(DefaultlibPath+corelibname)=NilHandle) then
      EM(DefaultlibPath+corelibname);
  {$endif}
  hlib:=LoadLibrary(lib);
  if (hlib=NilHandle) then
{$ifndef windows}
    EM(Lib);
{$else}
    if (d='') then
      begin
      hlib:=LoadLibrary(DefaultlibPath+ExtractFileName(Lib));
      if (hlib=NilHandle) then
        EM(Lib);
      end;
{$endif}
  Inc(LibRefCount);
  pointer(libvlc_errmsg):=GetProcAddress(hlib,'libvlc_errmsg');
  pointer(libvlc_clearerr):=GetProcAddress(hlib,'libvlc_clearerr');
  pointer(libvlc_printerr):=GetProcAddress(hlib,'libvlc_printerr');
  pointer(libvlc_new):=GetProcAddress(hlib,'libvlc_new');
  pointer(libvlc_release):=GetProcAddress(hlib,'libvlc_release');
  pointer(libvlc_retain):=GetProcAddress(hlib,'libvlc_retain');
  pointer(libvlc_add_intf):=GetProcAddress(hlib,'libvlc_add_intf');
  pointer(libvlc_set_exit_handler):=GetProcAddress(hlib,'libvlc_set_exit_handler');
  pointer(libvlc_wait):=GetProcAddress(hlib,'libvlc_wait');
  pointer(libvlc_set_user_agent):=GetProcAddress(hlib,'libvlc_set_user_agent');
  pointer(libvlc_get_version):=GetProcAddress(hlib,'libvlc_get_version');
  pointer(libvlc_get_compiler):=GetProcAddress(hlib,'libvlc_get_compiler');
  pointer(libvlc_get_changeset):=GetProcAddress(hlib,'libvlc_get_changeset');
  pointer(libvlc_free):=GetProcAddress(hlib,'libvlc_free');
  pointer(libvlc_event_attach):=GetProcAddress(hlib,'libvlc_event_attach');
  pointer(libvlc_event_detach):=GetProcAddress(hlib,'libvlc_event_detach');
  pointer(libvlc_event_type_name):=GetProcAddress(hlib,'libvlc_event_type_name');
  pointer(libvlc_get_log_verbosity):=GetProcAddress(hlib,'libvlc_get_log_verbosity');
  pointer(libvlc_set_log_verbosity):=GetProcAddress(hlib,'libvlc_set_log_verbosity');
  pointer(libvlc_log_open):=GetProcAddress(hlib,'libvlc_log_open');
  pointer(libvlc_log_close):=GetProcAddress(hlib,'libvlc_log_close');
  pointer(libvlc_log_count):=GetProcAddress(hlib,'libvlc_log_count');
  pointer(libvlc_log_clear):=GetProcAddress(hlib,'libvlc_log_clear');
  pointer(libvlc_log_get_iterator):=GetProcAddress(hlib,'libvlc_log_get_iterator');
  pointer(libvlc_log_iterator_free):=GetProcAddress(hlib,'libvlc_log_iterator_free');
  pointer(libvlc_log_iterator_has_next):=GetProcAddress(hlib,'libvlc_log_iterator_has_next');
  pointer(libvlc_log_iterator_next):=GetProcAddress(hlib,'libvlc_log_iterator_next');
  pointer(libvlc_module_description_list_release):=GetProcAddress(hlib,'libvlc_module_description_list_release');
  pointer(libvlc_audio_filter_list_get):=GetProcAddress(hlib,'libvlc_audio_filter_list_get');
  pointer(libvlc_video_filter_list_get):=GetProcAddress(hlib,'libvlc_video_filter_list_get');
  pointer(libvlc_clock):=GetProcAddress(hlib,'libvlc_clock');
  pointer(libvlc_media_new_location):=GetProcAddress(hlib,'libvlc_media_new_location');
  pointer(libvlc_media_new_path):=GetProcAddress(hlib,'libvlc_media_new_path');
  pointer(libvlc_media_new_fd):=GetProcAddress(hlib,'libvlc_media_new_fd');
  pointer(libvlc_media_new_as_node):=GetProcAddress(hlib,'libvlc_media_new_as_node');
  pointer(libvlc_media_add_option):=GetProcAddress(hlib,'libvlc_media_add_option');
  pointer(libvlc_media_add_option_flag):=GetProcAddress(hlib,'libvlc_media_add_option_flag');
  pointer(libvlc_media_retain):=GetProcAddress(hlib,'libvlc_media_retain');
  pointer(libvlc_media_release):=GetProcAddress(hlib,'libvlc_media_release');
  pointer(libvlc_media_get_mrl):=GetProcAddress(hlib,'libvlc_media_get_mrl');
  pointer(libvlc_media_duplicate):=GetProcAddress(hlib,'libvlc_media_duplicate');
  pointer(libvlc_media_get_meta):=GetProcAddress(hlib,'libvlc_media_get_meta');
  pointer(libvlc_media_set_meta):=GetProcAddress(hlib,'libvlc_media_set_meta');
  pointer(libvlc_media_save_meta):=GetProcAddress(hlib,'libvlc_media_save_meta');
  pointer(libvlc_media_get_state):=GetProcAddress(hlib,'libvlc_media_get_state');
  pointer(libvlc_media_get_stats):=GetProcAddress(hlib,'libvlc_media_get_stats');
  pointer(libvlc_media_subitems):=GetProcAddress(hlib,'libvlc_media_subitems');
  pointer(libvlc_media_event_manager):=GetProcAddress(hlib,'libvlc_media_event_manager');
  pointer(libvlc_media_get_duration):=GetProcAddress(hlib,'libvlc_media_get_duration');
  pointer(libvlc_media_parse):=GetProcAddress(hlib,'libvlc_media_parse');
  pointer(libvlc_media_parse_async):=GetProcAddress(hlib,'libvlc_media_parse_async');
  pointer(libvlc_media_is_parsed):=GetProcAddress(hlib,'libvlc_media_is_parsed');
  pointer(libvlc_media_set_user_data):=GetProcAddress(hlib,'libvlc_media_set_user_data');
  pointer(libvlc_media_get_user_data):=GetProcAddress(hlib,'libvlc_media_get_user_data');
  pointer(libvlc_media_get_tracks_info):=GetProcAddress(hlib,'libvlc_media_get_tracks_info');
  pointer(libvlc_media_player_new):=GetProcAddress(hlib,'libvlc_media_player_new');
  pointer(libvlc_media_player_new_from_media):=GetProcAddress(hlib,'libvlc_media_player_new_from_media');
  pointer(libvlc_media_player_release):=GetProcAddress(hlib,'libvlc_media_player_release');
  pointer(libvlc_media_player_retain):=GetProcAddress(hlib,'libvlc_media_player_retain');
  pointer(libvlc_media_player_set_media):=GetProcAddress(hlib,'libvlc_media_player_set_media');
  pointer(libvlc_media_player_get_media):=GetProcAddress(hlib,'libvlc_media_player_get_media');
  pointer(libvlc_media_player_event_manager):=GetProcAddress(hlib,'libvlc_media_player_event_manager');
  pointer(libvlc_media_player_is_playing):=GetProcAddress(hlib,'libvlc_media_player_is_playing');
  pointer(libvlc_media_player_play):=GetProcAddress(hlib,'libvlc_media_player_play');
  pointer(libvlc_media_player_set_pause):=GetProcAddress(hlib,'libvlc_media_player_set_pause');
  pointer(libvlc_media_player_pause):=GetProcAddress(hlib,'libvlc_media_player_pause');
  pointer(libvlc_media_player_stop):=GetProcAddress(hlib,'libvlc_media_player_stop');
  pointer(libvlc_video_set_callbacks):=GetProcAddress(hlib,'libvlc_video_set_callbacks');
  pointer(libvlc_video_set_format):=GetProcAddress(hlib,'libvlc_video_set_format');
  pointer(libvlc_video_set_format_callbacks):=GetProcAddress(hlib,'libvlc_video_set_format_callbacks');
  pointer(libvlc_media_player_set_nsobject):=GetProcAddress(hlib,'libvlc_media_player_set_nsobject');
  pointer(libvlc_media_player_get_nsobject):=GetProcAddress(hlib,'libvlc_media_player_get_nsobject');
  pointer(libvlc_media_player_set_agl):=GetProcAddress(hlib,'libvlc_media_player_set_agl');
  pointer(libvlc_media_player_get_agl):=GetProcAddress(hlib,'libvlc_media_player_get_agl');
  pointer(libvlc_media_player_set_xwindow):=GetProcAddress(hlib,'libvlc_media_player_set_xwindow');
  pointer(libvlc_media_player_get_xwindow):=GetProcAddress(hlib,'libvlc_media_player_get_xwindow');
  pointer(libvlc_media_player_set_hwnd):=GetProcAddress(hlib,'libvlc_media_player_set_hwnd');
  pointer(libvlc_media_player_get_hwnd):=GetProcAddress(hlib,'libvlc_media_player_get_hwnd');
  pointer(libvlc_audio_set_callbacks):=GetProcAddress(hlib,'libvlc_audio_set_callbacks');
  pointer(libvlc_audio_set_volume_callback):=GetProcAddress(hlib,'libvlc_audio_set_volume_callback');
  pointer(libvlc_audio_set_format_callbacks):=GetProcAddress(hlib,'libvlc_audio_set_format_callbacks');
  pointer(libvlc_audio_set_format):=GetProcAddress(hlib,'libvlc_audio_set_format');
  pointer(libvlc_media_player_get_length):=GetProcAddress(hlib,'libvlc_media_player_get_length');
  pointer(libvlc_media_player_get_time):=GetProcAddress(hlib,'libvlc_media_player_get_time');
  pointer(libvlc_media_player_set_time):=GetProcAddress(hlib,'libvlc_media_player_set_time');
  pointer(libvlc_media_player_get_position):=GetProcAddress(hlib,'libvlc_media_player_get_position');
  pointer(libvlc_media_player_set_position):=GetProcAddress(hlib,'libvlc_media_player_set_position');
  pointer(libvlc_media_player_set_chapter):=GetProcAddress(hlib,'libvlc_media_player_set_chapter');
  pointer(libvlc_media_player_get_chapter):=GetProcAddress(hlib,'libvlc_media_player_get_chapter');
  pointer(libvlc_media_player_get_chapter_count):=GetProcAddress(hlib,'libvlc_media_player_get_chapter_count');
  pointer(libvlc_media_player_will_play):=GetProcAddress(hlib,'libvlc_media_player_will_play');
  pointer(libvlc_media_player_get_chapter_count_for_title):=GetProcAddress(hlib,'libvlc_media_player_get_chapter_count_for_title');
  pointer(libvlc_media_player_set_title):=GetProcAddress(hlib,'libvlc_media_player_set_title');
  pointer(libvlc_media_player_get_title):=GetProcAddress(hlib,'libvlc_media_player_get_title');
  pointer(libvlc_media_player_get_title_count):=GetProcAddress(hlib,'libvlc_media_player_get_title_count');
  pointer(libvlc_media_player_previous_chapter):=GetProcAddress(hlib,'libvlc_media_player_previous_chapter');
  pointer(libvlc_media_player_next_chapter):=GetProcAddress(hlib,'libvlc_media_player_next_chapter');
  pointer(libvlc_media_player_get_rate):=GetProcAddress(hlib,'libvlc_media_player_get_rate');
  pointer(libvlc_media_player_set_rate):=GetProcAddress(hlib,'libvlc_media_player_set_rate');
  pointer(libvlc_media_player_get_state):=GetProcAddress(hlib,'libvlc_media_player_get_state');
  pointer(libvlc_media_player_get_fps):=GetProcAddress(hlib,'libvlc_media_player_get_fps');
  pointer(libvlc_media_player_has_vout):=GetProcAddress(hlib,'libvlc_media_player_has_vout');
  pointer(libvlc_media_player_is_seekable):=GetProcAddress(hlib,'libvlc_media_player_is_seekable');
  pointer(libvlc_media_player_can_pause):=GetProcAddress(hlib,'libvlc_media_player_can_pause');
  pointer(libvlc_media_player_next_frame):=GetProcAddress(hlib,'libvlc_media_player_next_frame');
  pointer(libvlc_media_player_navigate):=GetProcAddress(hlib,'libvlc_media_player_navigate');
  pointer(libvlc_track_description_list_release):=GetProcAddress(hlib,'libvlc_track_description_list_release');
  pointer(libvlc_track_description_release):=GetProcAddress(hlib,'libvlc_track_description_release');
  pointer(libvlc_toggle_fullscreen):=GetProcAddress(hlib,'libvlc_toggle_fullscreen');
  pointer(libvlc_set_fullscreen):=GetProcAddress(hlib,'libvlc_set_fullscreen');
  pointer(libvlc_get_fullscreen):=GetProcAddress(hlib,'libvlc_get_fullscreen');
  pointer(libvlc_video_set_key_input):=GetProcAddress(hlib,'libvlc_video_set_key_input');
  pointer(libvlc_video_set_mouse_input):=GetProcAddress(hlib,'libvlc_video_set_mouse_input');
  pointer(libvlc_video_get_size):=GetProcAddress(hlib,'libvlc_video_get_size');
  pointer(libvlc_video_get_height):=GetProcAddress(hlib,'libvlc_video_get_height');
  pointer(libvlc_video_get_width):=GetProcAddress(hlib,'libvlc_video_get_width');
  pointer(libvlc_video_get_cursor):=GetProcAddress(hlib,'libvlc_video_get_cursor');
  pointer(libvlc_video_get_scale):=GetProcAddress(hlib,'libvlc_video_get_scale');
  pointer(libvlc_video_set_scale):=GetProcAddress(hlib,'libvlc_video_set_scale');
  pointer(libvlc_video_get_aspect_ratio):=GetProcAddress(hlib,'libvlc_video_get_aspect_ratio');
  pointer(libvlc_video_set_aspect_ratio):=GetProcAddress(hlib,'libvlc_video_set_aspect_ratio');
  pointer(libvlc_video_get_spu):=GetProcAddress(hlib,'libvlc_video_get_spu');
  pointer(libvlc_video_get_spu_count):=GetProcAddress(hlib,'libvlc_video_get_spu_count');
  pointer(libvlc_video_get_spu_description):=GetProcAddress(hlib,'libvlc_video_get_spu_description');
  pointer(libvlc_video_set_spu):=GetProcAddress(hlib,'libvlc_video_set_spu');
  pointer(libvlc_video_set_subtitle_file):=GetProcAddress(hlib,'libvlc_video_set_subtitle_file');
  pointer(libvlc_video_get_spu_delay):=GetProcAddress(hlib,'libvlc_video_get_spu_delay');
  pointer(libvlc_video_set_spu_delay):=GetProcAddress(hlib,'libvlc_video_set_spu_delay');
  pointer(libvlc_video_get_title_description):=GetProcAddress(hlib,'libvlc_video_get_title_description');
  pointer(libvlc_video_get_chapter_description):=GetProcAddress(hlib,'libvlc_video_get_chapter_description');
  pointer(libvlc_video_get_crop_geometry):=GetProcAddress(hlib,'libvlc_video_get_crop_geometry');
  pointer(libvlc_video_set_crop_geometry):=GetProcAddress(hlib,'libvlc_video_set_crop_geometry');
  pointer(libvlc_video_get_teletext):=GetProcAddress(hlib,'libvlc_video_get_teletext');
  pointer(libvlc_video_set_teletext):=GetProcAddress(hlib,'libvlc_video_set_teletext');
  pointer(libvlc_toggle_teletext):=GetProcAddress(hlib,'libvlc_toggle_teletext');
  pointer(libvlc_video_get_track_count):=GetProcAddress(hlib,'libvlc_video_get_track_count');
  pointer(libvlc_video_get_track_description):=GetProcAddress(hlib,'libvlc_video_get_track_description');
  pointer(libvlc_video_get_track):=GetProcAddress(hlib,'libvlc_video_get_track');
  pointer(libvlc_video_set_track):=GetProcAddress(hlib,'libvlc_video_set_track');
  pointer(libvlc_video_take_snapshot):=GetProcAddress(hlib,'libvlc_video_take_snapshot');
  pointer(libvlc_video_set_deinterlace):=GetProcAddress(hlib,'libvlc_video_set_deinterlace');
  pointer(libvlc_video_get_marquee_int):=GetProcAddress(hlib,'libvlc_video_get_marquee_int');
  pointer(libvlc_video_get_marquee_string):=GetProcAddress(hlib,'libvlc_video_get_marquee_string');
  pointer(libvlc_video_set_marquee_int):=GetProcAddress(hlib,'libvlc_video_set_marquee_int');
  pointer(libvlc_video_set_marquee_string):=GetProcAddress(hlib,'libvlc_video_set_marquee_string');
  pointer(libvlc_video_get_logo_int):=GetProcAddress(hlib,'libvlc_video_get_logo_int');
  pointer(libvlc_video_set_logo_int):=GetProcAddress(hlib,'libvlc_video_set_logo_int');
  pointer(libvlc_video_set_logo_string):=GetProcAddress(hlib,'libvlc_video_set_logo_string');
  pointer(libvlc_video_get_adjust_int):=GetProcAddress(hlib,'libvlc_video_get_adjust_int');
  pointer(libvlc_video_set_adjust_int):=GetProcAddress(hlib,'libvlc_video_set_adjust_int');
  pointer(libvlc_video_get_adjust_float):=GetProcAddress(hlib,'libvlc_video_get_adjust_float');
  pointer(libvlc_video_set_adjust_float):=GetProcAddress(hlib,'libvlc_video_set_adjust_float');
  pointer(libvlc_audio_output_list_get):=GetProcAddress(hlib,'libvlc_audio_output_list_get');
  pointer(libvlc_audio_output_list_release):=GetProcAddress(hlib,'libvlc_audio_output_list_release');
  pointer(libvlc_audio_output_set):=GetProcAddress(hlib,'libvlc_audio_output_set');
  pointer(libvlc_audio_output_device_count):=GetProcAddress(hlib,'libvlc_audio_output_device_count');
  pointer(libvlc_audio_output_device_longname):=GetProcAddress(hlib,'libvlc_audio_output_device_longname');
  pointer(libvlc_audio_output_device_id):=GetProcAddress(hlib,'libvlc_audio_output_device_id');
  pointer(libvlc_audio_output_device_set):=GetProcAddress(hlib,'libvlc_audio_output_device_set');
  pointer(libvlc_audio_output_get_device_type):=GetProcAddress(hlib,'libvlc_audio_output_get_device_type');
  pointer(libvlc_audio_output_set_device_type):=GetProcAddress(hlib,'libvlc_audio_output_set_device_type');
  pointer(libvlc_audio_toggle_mute):=GetProcAddress(hlib,'libvlc_audio_toggle_mute');
  pointer(libvlc_audio_get_mute):=GetProcAddress(hlib,'libvlc_audio_get_mute');
  pointer(libvlc_audio_set_mute):=GetProcAddress(hlib,'libvlc_audio_set_mute');
  pointer(libvlc_audio_get_volume):=GetProcAddress(hlib,'libvlc_audio_get_volume');
  pointer(libvlc_audio_set_volume):=GetProcAddress(hlib,'libvlc_audio_set_volume');
  pointer(libvlc_audio_get_track_count):=GetProcAddress(hlib,'libvlc_audio_get_track_count');
  pointer(libvlc_audio_get_track_description):=GetProcAddress(hlib,'libvlc_audio_get_track_description');
  pointer(libvlc_audio_get_track):=GetProcAddress(hlib,'libvlc_audio_get_track');
  pointer(libvlc_audio_set_track):=GetProcAddress(hlib,'libvlc_audio_set_track');
  pointer(libvlc_audio_get_channel):=GetProcAddress(hlib,'libvlc_audio_get_channel');
  pointer(libvlc_audio_set_channel):=GetProcAddress(hlib,'libvlc_audio_set_channel');
  pointer(libvlc_audio_get_delay):=GetProcAddress(hlib,'libvlc_audio_get_delay');
  pointer(libvlc_audio_set_delay):=GetProcAddress(hlib,'libvlc_audio_set_delay');
  pointer(libvlc_media_list_new):=GetProcAddress(hlib,'libvlc_media_list_new');
  pointer(libvlc_media_list_release):=GetProcAddress(hlib,'libvlc_media_list_release');
  pointer(libvlc_media_list_retain):=GetProcAddress(hlib,'libvlc_media_list_retain');
  pointer(libvlc_media_list_add_file_content):=GetProcAddress(hlib,'libvlc_media_list_add_file_content');
  pointer(libvlc_media_list_set_media):=GetProcAddress(hlib,'libvlc_media_list_set_media');
  pointer(libvlc_media_list_media):=GetProcAddress(hlib,'libvlc_media_list_media');
  pointer(libvlc_media_list_add_media):=GetProcAddress(hlib,'libvlc_media_list_add_media');
  pointer(libvlc_media_list_insert_media):=GetProcAddress(hlib,'libvlc_media_list_insert_media');
  pointer(libvlc_media_list_remove_index):=GetProcAddress(hlib,'libvlc_media_list_remove_index');
  pointer(libvlc_media_list_count):=GetProcAddress(hlib,'libvlc_media_list_count');
  pointer(libvlc_media_list_item_at_index):=GetProcAddress(hlib,'libvlc_media_list_item_at_index');
  pointer(libvlc_media_list_index_of_item):=GetProcAddress(hlib,'libvlc_media_list_index_of_item');
  pointer(libvlc_media_list_is_readonly):=GetProcAddress(hlib,'libvlc_media_list_is_readonly');
  pointer(libvlc_media_list_lock):=GetProcAddress(hlib,'libvlc_media_list_lock');
  pointer(libvlc_media_list_unlock):=GetProcAddress(hlib,'libvlc_media_list_unlock');
  pointer(libvlc_media_list_event_manager):=GetProcAddress(hlib,'libvlc_media_list_event_manager');
  pointer(libvlc_media_list_player_new):=GetProcAddress(hlib,'libvlc_media_list_player_new');
  pointer(libvlc_media_list_player_release):=GetProcAddress(hlib,'libvlc_media_list_player_release');
  pointer(libvlc_media_list_player_retain):=GetProcAddress(hlib,'libvlc_media_list_player_retain');
  pointer(libvlc_media_list_player_event_manager):=GetProcAddress(hlib,'libvlc_media_list_player_event_manager');
  pointer(libvlc_media_list_player_set_media_player):=GetProcAddress(hlib,'libvlc_media_list_player_set_media_player');
  pointer(libvlc_media_list_player_set_media_list):=GetProcAddress(hlib,'libvlc_media_list_player_set_media_list');
  pointer(libvlc_media_list_player_play):=GetProcAddress(hlib,'libvlc_media_list_player_play');
  pointer(libvlc_media_list_player_pause):=GetProcAddress(hlib,'libvlc_media_list_player_pause');
  pointer(libvlc_media_list_player_is_playing):=GetProcAddress(hlib,'libvlc_media_list_player_is_playing');
  pointer(libvlc_media_list_player_get_state):=GetProcAddress(hlib,'libvlc_media_list_player_get_state');
  pointer(libvlc_media_list_player_play_item_at_index):=GetProcAddress(hlib,'libvlc_media_list_player_play_item_at_index');
  pointer(libvlc_media_list_player_play_item):=GetProcAddress(hlib,'libvlc_media_list_player_play_item');
  pointer(libvlc_media_list_player_stop):=GetProcAddress(hlib,'libvlc_media_list_player_stop');
  pointer(libvlc_media_list_player_next):=GetProcAddress(hlib,'libvlc_media_list_player_next');
  pointer(libvlc_media_list_player_previous):=GetProcAddress(hlib,'libvlc_media_list_player_previous');
  pointer(libvlc_media_list_player_set_playback_mode):=GetProcAddress(hlib,'libvlc_media_list_player_set_playback_mode');
  pointer(libvlc_media_library_new):=GetProcAddress(hlib,'libvlc_media_library_new');
  pointer(libvlc_media_library_release):=GetProcAddress(hlib,'libvlc_media_library_release');
  pointer(libvlc_media_library_retain):=GetProcAddress(hlib,'libvlc_media_library_retain');
  pointer(libvlc_media_library_load):=GetProcAddress(hlib,'libvlc_media_library_load');
  pointer(libvlc_media_library_media_list):=GetProcAddress(hlib,'libvlc_media_library_media_list');
  pointer(libvlc_media_discoverer_new_from_name):=GetProcAddress(hlib,'libvlc_media_discoverer_new_from_name');
  pointer(libvlc_media_discoverer_release):=GetProcAddress(hlib,'libvlc_media_discoverer_release');
  pointer(libvlc_media_discoverer_localized_name):=GetProcAddress(hlib,'libvlc_media_discoverer_localized_name');
  pointer(libvlc_media_discoverer_media_list):=GetProcAddress(hlib,'libvlc_media_discoverer_media_list');
  pointer(libvlc_media_discoverer_event_manager):=GetProcAddress(hlib,'libvlc_media_discoverer_event_manager');
  pointer(libvlc_media_discoverer_is_running):=GetProcAddress(hlib,'libvlc_media_discoverer_is_running');
  pointer(libvlc_vlm_release):=GetProcAddress(hlib,'libvlc_vlm_release');
  pointer(libvlc_vlm_add_broadcast):=GetProcAddress(hlib,'libvlc_vlm_add_broadcast');
  pointer(libvlc_vlm_add_vod):=GetProcAddress(hlib,'libvlc_vlm_add_vod');
  pointer(libvlc_vlm_del_media):=GetProcAddress(hlib,'libvlc_vlm_del_media');
  pointer(libvlc_vlm_set_enabled):=GetProcAddress(hlib,'libvlc_vlm_set_enabled');
  pointer(libvlc_vlm_set_output):=GetProcAddress(hlib,'libvlc_vlm_set_output');
  pointer(libvlc_vlm_set_input):=GetProcAddress(hlib,'libvlc_vlm_set_input');
  pointer(libvlc_vlm_add_input):=GetProcAddress(hlib,'libvlc_vlm_add_input');
  pointer(libvlc_vlm_set_loop):=GetProcAddress(hlib,'libvlc_vlm_set_loop');
  pointer(libvlc_vlm_set_mux):=GetProcAddress(hlib,'libvlc_vlm_set_mux');
  pointer(libvlc_vlm_change_media):=GetProcAddress(hlib,'libvlc_vlm_change_media');
  pointer(libvlc_vlm_play_media):=GetProcAddress(hlib,'libvlc_vlm_play_media');
  pointer(libvlc_vlm_stop_media):=GetProcAddress(hlib,'libvlc_vlm_stop_media');
  pointer(libvlc_vlm_pause_media):=GetProcAddress(hlib,'libvlc_vlm_pause_media');
  pointer(libvlc_vlm_seek_media):=GetProcAddress(hlib,'libvlc_vlm_seek_media');
  pointer(libvlc_vlm_show_media):=GetProcAddress(hlib,'libvlc_vlm_show_media');
  pointer(libvlc_vlm_get_media_instance_position):=GetProcAddress(hlib,'libvlc_vlm_get_media_instance_position');
  pointer(libvlc_vlm_get_media_instance_time):=GetProcAddress(hlib,'libvlc_vlm_get_media_instance_time');
  pointer(libvlc_vlm_get_media_instance_length):=GetProcAddress(hlib,'libvlc_vlm_get_media_instance_length');
  pointer(libvlc_vlm_get_media_instance_rate):=GetProcAddress(hlib,'libvlc_vlm_get_media_instance_rate');
  pointer(libvlc_vlm_get_event_manager):=GetProcAddress(hlib,'libvlc_vlm_get_event_manager');
  pointer(libvlc_playlist_play):=GetProcAddress(hlib,'libvlc_playlist_play');
end;


end.
