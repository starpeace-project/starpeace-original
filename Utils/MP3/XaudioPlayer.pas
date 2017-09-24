unit XaudioPlayer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

const
{ Messages }
  XA_MSG_BASE                              = $9000;
  XA_MSG_UNKNOWN                           = 0;
  XA_MSG_COMMAND_EXIT                      = 1;
  XA_MSG_COMMAND_PING                      = 2;
  XA_MSG_COMMAND_PLAY                      = 3;
  XA_MSG_COMMAND_PAUSE                     = 4;
  XA_MSG_COMMAND_STOP                      = 5;
  XA_MSG_COMMAND_SEEK                      = 6;
  XA_MSG_COMMAND_INPUT_OPEN                = 7;
  XA_MSG_COMMAND_INPUT_CLOSE               = 8;
  XA_MSG_COMMAND_INPUT_SEND_MESSAGE        = 9;
  XA_MSG_COMMAND_INPUT_ADD_FILTER          = 10;
  XA_MSG_COMMAND_INPUT_REMOVE_FILTER       = 11;
  XA_MSG_COMMAND_INPUT_FILTERS_LIST        = 12;
  XA_MSG_COMMAND_INPUT_MODULE_REGISTER     = 13;
  XA_MSG_COMMAND_INPUT_MODULE_QUERY        = 14;
  XA_MSG_COMMAND_INPUT_MODULES_LIST        = 15;
  XA_MSG_COMMAND_OUTPUT_OPEN               = 16;
  XA_MSG_COMMAND_OUTPUT_CLOSE              = 17;
  XA_MSG_COMMAND_OUTPUT_SEND_MESSAGE       = 18;
  XA_MSG_COMMAND_OUTPUT_MUTE               = 19;
  XA_MSG_COMMAND_OUTPUT_UNMUTE             = 20;
  XA_MSG_COMMAND_OUTPUT_RESET              = 21;
  XA_MSG_COMMAND_OUTPUT_DRAIN              = 22;
  XA_MSG_COMMAND_OUTPUT_ADD_FILTER         = 23;
  XA_MSG_COMMAND_OUTPUT_REMOVE_FILTER      = 24;
  XA_MSG_COMMAND_OUTPUT_FILTERS_LIST       = 25;
  XA_MSG_COMMAND_OUTPUT_MODULE_REGISTER    = 26;
  XA_MSG_COMMAND_OUTPUT_MODULE_QUERY       = 27;
  XA_MSG_COMMAND_OUTPUT_MODULES_LIST       = 28;
  XA_MSG_SET_PLAYER_MODE                   = 29;
  XA_MSG_GET_PLAYER_MODE                   = 30;
  XA_MSG_SET_PLAYER_ENVIRONMENT_INTEGER    = 31;
  XA_MSG_GET_PLAYER_ENVIRONMENT_INTEGER    = 32;
  XA_MSG_SET_PLAYER_ENVIRONMENT_STRING     = 33;
  XA_MSG_GET_PLAYER_ENVIRONMENT_STRING     = 34;
  XA_MSG_UNSET_PLAYER_ENVIRONMENT          = 35;
  XA_MSG_SET_INPUT_NAME                    = 36;
  XA_MSG_GET_INPUT_NAME                    = 37;
  XA_MSG_SET_INPUT_MODULE                  = 38;
  XA_MSG_GET_INPUT_MODULE                  = 39;
  XA_MSG_SET_INPUT_POSITION_RANGE          = 40;
  XA_MSG_GET_INPUT_POSITION_RANGE          = 41;
  XA_MSG_SET_INPUT_TIMECODE_GRANULARITY    = 42;
  XA_MSG_GET_INPUT_TIMECODE_GRANULARITY    = 43;
  XA_MSG_SET_OUTPUT_NAME                   = 44;
  XA_MSG_GET_OUTPUT_NAME                   = 45;
  XA_MSG_SET_OUTPUT_MODULE                 = 46;
  XA_MSG_GET_OUTPUT_MODULE                 = 47;
  XA_MSG_SET_OUTPUT_VOLUME                 = 48;
  XA_MSG_GET_OUTPUT_VOLUME                 = 49;
  XA_MSG_SET_OUTPUT_CHANNELS               = 50;
  XA_MSG_GET_OUTPUT_CHANNELS               = 51;
  XA_MSG_SET_OUTPUT_PORTS                  = 52;
  XA_MSG_GET_OUTPUT_PORTS                  = 53;
  XA_MSG_SET_CODEC_QUALITY                 = 54;
  XA_MSG_GET_CODEC_QUALITY                 = 55;
  XA_MSG_SET_OUTPUT_FEEDBACK_RATE          = 56;
  XA_MSG_GET_OUTPUT_FEEDBACK_RATE          = 57;
  XA_MSG_SET_CODEC_EQUALIZER               = 58;
  XA_MSG_GET_CODEC_EQUALIZER               = 59;
  XA_MSG_SET_NOTIFICATION_MASK             = 60;
  XA_MSG_GET_NOTIFICATION_MASK             = 61;
  XA_MSG_SET_DEBUG_LEVEL                   = 62;
  XA_MSG_GET_DEBUG_LEVEL                   = 63;
  XA_MSG_NOTIFY_READY                      = 64;
  XA_MSG_NOTIFY_ACK                        = 65;
  XA_MSG_NOTIFY_NACK                       = 66;
  XA_MSG_NOTIFY_PONG                       = 67;
  XA_MSG_NOTIFY_EXITED                     = 68;
  XA_MSG_NOTIFY_PLAYER_STATE               = 69;
  XA_MSG_NOTIFY_PLAYER_MODE                = 70;
  XA_MSG_NOTIFY_PLAYER_ENVIRONMENT_INTEGER = 71;
  XA_MSG_NOTIFY_PLAYER_ENVIRONMENT_STRING  = 72;
  XA_MSG_NOTIFY_INPUT_STATE                = 73;
  XA_MSG_NOTIFY_INPUT_NAME                 = 74;
  XA_MSG_NOTIFY_INPUT_CAPS                 = 75;
  XA_MSG_NOTIFY_INPUT_POSITION             = 76;
  XA_MSG_NOTIFY_INPUT_POSITION_RANGE       = 77;
  XA_MSG_NOTIFY_INPUT_TIMECODE             = 78;
  XA_MSG_NOTIFY_INPUT_TIMECODE_GRANULARITY = 79;
  XA_MSG_NOTIFY_INPUT_DURATION             = 80;
  XA_MSG_NOTIFY_INPUT_STREAM_INFO          = 81;
  XA_MSG_NOTIFY_INPUT_MODULE               = 82;
  XA_MSG_NOTIFY_INPUT_MODULE_INFO          = 83;
  XA_MSG_NOTIFY_INPUT_DEVICE_INFO          = 84;
  XA_MSG_NOTIFY_INPUT_FILTER_INFO          = 85;
  XA_MSG_NOTIFY_OUTPUT_STATE               = 86;
  XA_MSG_NOTIFY_OUTPUT_NAME                = 87;
  XA_MSG_NOTIFY_OUTPUT_CAPS                = 88;
  XA_MSG_NOTIFY_OUTPUT_VOLUME              = 89;
  XA_MSG_NOTIFY_OUTPUT_BALANCE             = 90;
  XA_MSG_NOTIFY_OUTPUT_PCM_LEVEL           = 91;
  XA_MSG_NOTIFY_OUTPUT_MASTER_LEVEL        = 92;
  XA_MSG_NOTIFY_OUTPUT_CHANNELS            = 93;
  XA_MSG_NOTIFY_OUTPUT_PORTS               = 94;
  XA_MSG_NOTIFY_OUTPUT_FEEDBACK_RATE       = 95;
  XA_MSG_NOTIFY_OUTPUT_MODULE              = 96;
  XA_MSG_NOTIFY_OUTPUT_MODULE_INFO         = 97;
  XA_MSG_NOTIFY_OUTPUT_DEVICE_INFO         = 98;
  XA_MSG_NOTIFY_OUTPUT_FILTER_INFO         = 99;
  XA_MSG_NOTIFY_CODEC_QUALITY              = 100;
  XA_MSG_NOTIFY_CODEC_EQUALIZER            = 101;
  XA_MSG_NOTIFY_NOTIFICATION_MASK          = 102;
  XA_MSG_NOTIFY_DEBUG_LEVEL                = 103;
  XA_MSG_NOTIFY_PROGRESS                   = 104;
  XA_MSG_NOTIFY_DEBUG                      = 105;
  XA_MSG_NOTIFY_ERROR                      = 106;

{ Player State }
  XA_PLAYER_STATE_STOPPED                  = 0;
  XA_PLAYER_STATE_PLAYING                  = 1;
  XA_PLAYER_STATE_PAUSED                   = 2;
  XA_PLAYER_STATE_EOF                      = 3;

{ Input State }
  XA_INPUT_STATE_OPEN                      = 0;
  XA_INPUT_STATE_CLOSED                    = 1;

{ Output State }
  XA_OUTPUT_STATE_OPEN                     = 0;
  XA_OUTPUT_STATE_CLOSED                   = 1;

{ Channel Configurations }
  XA_OUTPUT_CHANNELS_STEREO                = 0;
  XA_OUTPUT_CHANNELS_MONO_LEFT             = 1;
  XA_OUTPUT_CHANNELS_MONO_RIGHT            = 2;
  XA_OUTPUT_CHANNELS_MONO_MIX              = 3;

{ Player Modes }
  XA_PLAYER_MODE_OUTPUT_AUTO_CLOSE_ON_STOP      = 1;
  XA_PLAYER_MODE_OUTPUT_AUTO_CLOSE_ON_PAUSE     = 2;
  XA_PLAYER_MODE_OUTPUT_AUTO_CLOSE_ON_EOF       = 4;

{ Notification Masks }
  XA_NOTIFY_MASK_ERROR                          = $0000001;
  XA_NOTIFY_MASK_DEBUG                          = $0000002;
  XA_NOTIFY_MASK_PROGRESS                       = $0000004;
  XA_NOTIFY_MASK_ACK                            = $0000008;
  XA_NOTIFY_MASK_NACK                           = $0000010;
  XA_NOTIFY_MASK_PLAYER_STATE                   = $0000020;
  XA_NOTIFY_MASK_INPUT_STATE                    = $0000040;
  XA_NOTIFY_MASK_INPUT_NAME                     = $0000080;
  XA_NOTIFY_MASK_INPUT_CAPS                     = $0000100;
  XA_NOTIFY_MASK_INPUT_DURATION                 = $0000200;
  XA_NOTIFY_MASK_INPUT_POSITION                 = $0000400;
  XA_NOTIFY_MASK_INPUT_POSITION_RANGE           = $0000800;
  XA_NOTIFY_MASK_INPUT_TIMECODE                 = $0001000;
  XA_NOTIFY_MASK_INPUT_TIMECODE_GRANULARITY     = $0002000;
  XA_NOTIFY_MASK_INPUT_STREAM_INFO              = $0004000;
  XA_NOTIFY_MASK_OUTPUT_STATE                   = $0008000;
  XA_NOTIFY_MASK_OUTPUT_NAME                    = $0010000;
  XA_NOTIFY_MASK_OUTPUT_CAPS                    = $0020000;
  XA_NOTIFY_MASK_OUTPUT_VOLUME                  = $0040000;
  XA_NOTIFY_MASK_OUTPUT_BALANCE                 = $0080000;
  XA_NOTIFY_MASK_OUTPUT_PCM_LEVEL               = $0100000;
  XA_NOTIFY_MASK_OUTPUT_MASTER_LEVEL            = $0200000;
  XA_NOTIFY_MASK_OUTPUT_PORTS                   = $0400000;
  XA_NOTIFY_MASK_CODEC_EQUALIZER                = $0800000;
  XA_NOTIFY_MASK_FEEDBACK_EVENT                 = $1000000;

{ Input Caps }
  XA_DECODER_INPUT_SEEKABLE                  = $0001;

{ Output Caps }
  XA_DECODER_DEVICE_HAS_MASTER_LEVEL_CONTROL = $0000001;
  XA_DECODER_DEVICE_HAS_PCM_LEVEL_CONTROL    = $0000002;
  XA_DECODER_DEVICE_HAS_BALANCE_CONTROL      = $0000004;
  XA_DECODER_DEVICE_HAS_LINE_OUT             = $0000008;
  XA_DECODER_DEVICE_HAS_SPEAKER_OUT          = $0000010;
  XA_DECODER_DEVICE_HAS_HEADPHONE_OUT        = $0000020;
  XA_DECODER_DEVICE_HAS_MONO_16_00_KHZ       = $0000080;
  XA_DECODER_DEVICE_HAS_MONO_22_05_KHZ       = $0000100;
  XA_DECODER_DEVICE_HAS_MONO_24_00_KHZ       = $0000200;
  XA_DECODER_DEVICE_HAS_MONO_32_00_KHZ       = $0000400;
  XA_DECODER_DEVICE_HAS_MONO_44_10_KHZ       = $0000800;
  XA_DECODER_DEVICE_HAS_MONO_48_00_KHZ       = $0001000;
  XA_DECODER_DEVICE_HAS_STEREO_16_00_KHZ     = $0002000;
  XA_DECODER_DEVICE_HAS_STEREO_22_05_KHZ     = $0004000;
  XA_DECODER_DEVICE_HAS_STEREO_24_00_KHZ     = $0008000;
  XA_DECODER_DEVICE_HAS_STEREO_32_00_KHZ     = $0010000;
  XA_DECODER_DEVICE_HAS_STEREO_44_10_KHZ     = $0020000;
  XA_DECODER_DEVICE_HAS_STEREO_48_00_KHZ     = $0040000;
  XA_DECODER_DEVICE_HAS_8_BITS               = $0080000;
  XA_DECODER_DEVICE_HAS_MU_LAW               = $0100000;
  XA_DECODER_DEVICE_HAS_16_BITS              = $0200000;
  XA_DECODER_DEVICE_HAS_MUTE                 = $0400000;
  XA_DECODER_DEVICE_HAS_RESET                = $0800000;
  XA_DECODER_DEVICE_HAS_PAUSE                = $1000000;
  XA_DECODER_DEVICE_HAS_DRAIN                = $2000000;
  XA_DECODER_DEVICE_HAS_BUFFER_STATUS        = $4000000;

{ Output Ports }
  XA_DECODER_CONTROL_OUTPUT_LINE             = $01;
  XA_DECODER_CONTROL_OUTPUT_SPEAKER          = $02;
  XA_DECODER_CONTROL_OUTPUT_HEADPHONE        = $04;

{ Environment Variables }
  XA_DECODER_ENVIRONMENT_MAX_NAME_LENGTH     = 256;
  XA_DECODER_ENVIRONMENT_MAX_STRING_LENGTH   = 1024;

{ Input Device Flags }
  XA_DECODER_INPUT_QUERY_NAME_IS_GENERIC     = $01;
  XA_DECODER_INPUT_QUERY_DEVICE_IS_DEFAULT   = $02;

{ Output Device Flags }
  XA_DECODER_OUTPUT_QUERY_NAME_IS_GENERIC     = $01;
  XA_DECODER_OUTPUT_QUERY_DEVICE_IS_DEFAULT   = $02;
  XA_DECODER_OUTPUT_QUERY_DEVICE_IS_SHAREABLE = $04;

{ Modules }
  XA_DECODER_INPUT_AUTOSELECT              = -1;
  XA_DECODER_OUTPUT_AUTOSELECT             = -1;

{ Filters }
  XA_DECODER_INPUT_FILTER_FIRST            = -1;
  XA_DECODER_INPUT_FILTER_LAST             =  0;
  XA_DECODER_INPUT_FILTER_BY_NAME          = -2;

  XA_DECODER_OUTPUT_FILTER_FIRST           = -1;
  XA_DECODER_OUTPUT_FILTER_LAST            =  0;
  XA_DECODER_OUTPUT_FILTER_BY_NAME         = -2;

{ Priorities }
  XA_CONTROL_PRIORITY_LOWEST               = 0;
  XA_CONTROL_PRIORITY_LOW                  = 1;
  XA_CONTROL_PRIORITY_NORMAL               = 2;
  XA_CONTROL_PRIORITY_HIGH                 = 3;
  XA_CONTROL_PRIORITY_HIGHEST              = 4;

  XA_DECODER_CODEC_QUALITY_HIGH            = 0;
  XA_DECODER_CODEC_QUALITY_MEDIUM          = 1;
  XA_DECODER_CODEC_QUALITY_LOW             = 2;

{ API Ids }
  XA_API_ID_SYNC  = 1;
  XA_API_ID_ASYNC = 2;

{ Misc Constants }
  XA_OUTPUT_VOLUME_IGNORE_FIELD            = 255;

  XA_DECODER_MAX_NAME_LENGTH               = 255;
  XA_DECODER_MAX_DESCRIPTION_LENGTH        = 255;

{ API Version }
  XA_ASYNC_API_MAJOR = 3;
  XA_ASYNC_API_MINOR = 2;

{ Types }
type
  XA_EnvironmentInfo = record
    name: PChar;
    case Integer of
      0: (integer_value: LongInt);
      1: (string_value:   PChar);
  end;

  XA_TimecodeInfo = record
    h: Byte;
    m: Byte;
    s: Byte;
    f: Byte;
  end;

  XA_NackInfo = record
    command: Byte;
    code:    SmallInt;
  end;

  XA_VolumeInfo = record
    master_level: Byte;
    pcm_level:    Byte;
    balance:      Byte;
  end;

  XA_PositionInfo = record
    offset: Cardinal;
    range:  Cardinal;
  end;

  XA_ModuleInfo = record
    id:          Byte;
    nb_devices:  Byte;
    name:        PChar;
    description: PChar;
  end;

  XA_FilterInfo = record
    name: PChar;
    id:   Word;
  end;

  XA_DeviceInfo = record
    module_id:   Byte;
    index:       Byte;
    flags:       Byte;
    name:        PChar;
    description: PChar;
  end;

  XA_StreamInfo = record
    level:     Byte;
    layer:     Byte;
    bitrate:   Word;
    frequency: Word;
    mode:      Byte;
  end;

  XA_ModuleMessage = record
    message_type: Word;
    size:         Word;
    data:         Pointer;
  end;

  XA_ProgressInfo = record
    source:  Byte;
    code:    Byte;
    value:   SmallInt;
    msg:     PChar;
  end;

  XA_DebugInfo = record
    source:  Byte;
    level:   Byte;
    msg:     PChar;
  end;

  XA_ErrorInfo = record
    source: Byte;
    code:   SmallInt;
    msg:    PChar;
  end;

  XA_EqualizerInfo = record
    left:  array[0..31] of ShortInt;
    right: array[0..31] of ShortInt;
  end;
  PXA_EqualizerInfo = ^XA_EqualizerInfo;

  XA_Message = record
    code: Integer;
    case Integer of
      0:  (buffer:           Pchar);
      1:  (name:             PChar);
      2:  (module_id:        SmallInt);
      3:  (state:            Byte);
      4:  (mode:             Cardinal);
      5:  (channels:         Byte);
      6:  (quality:          Byte);
      7:  (duration:         Cardinal);
      8:  (range:            Cardinal);
      9:  (granularity:      Cardinal);
      10: (caps:             Cardinal);
      11: (ports:            Byte);
      12: (ack:              Byte);
      13: (tag:              Cardinal);
      14: (debug_level:      Byte);
      15: (notification_mask:Cardinal);
      16: (rate:             Byte);
      17: (nack:             XA_NackInfo);
      18: (volume:           XA_VolumeInfo);
      19: (position:         XA_PositionInfo);
      20: (equalizer:       ^XA_EqualizerInfo);
      21: (module_info:      XA_ModuleInfo);
      22: (filter_info:      XA_FilterInfo);
      23: (device_info:      XA_DeviceInfo);
      24: (stream_info:      XA_StreamInfo);
      25: (environment_info: XA_EnvironmentInfo);
      26: (timecode:         XA_TimecodeInfo);
      27: (module_message:   XA_ModuleMessage);
      28: (progress:         XA_ProgressInfo);
      29: (debug:            XA_DebugInfo);
      30: (error:            XA_ErrorInfo);
  end;

{ Notification Functions }
  TXaudioPlayer = class;
  TNotifyMessageEvent = procedure(Sender: TXaudioPlayer; var Msg: XA_Message) of object;
  TNotifyAckEvent = procedure(Sender: TXaudioPlayer; Command: Byte) of object;
  TNotifyNackEvent = procedure(Sender: TXaudioPlayer; Command: Byte; Code: SmallInt) of object;
  TNotifyPongEvent = procedure(Sender: TXaudioPlayer; Tag: Cardinal) of object;
  TNotifyPlayerStateEvent = procedure(Sender: TXaudioPlayer; State: Integer) of object;
  TNotifyPlayerModeEvent = procedure(Sender: TXaudioPlayer; Mode: Integer) of object;
  TNotifyEnvironmentIntegerEvent = procedure(Sender: TXaudioPlayer; const Name: String; Value: Integer) of object;
  TNotifyEnvironmentStringEvent = procedure(Sender: TXaudioPlayer; const Name: String; const Value: String) of object;
  TNotifyInputStateEvent = procedure(Sender: TXaudioPlayer; State: Integer) of object;
  TNotifyInputNameEvent = procedure(Sender: TXaudioPlayer; const Name: String) of object;
  TNotifyInputCapsEvent = procedure(Sender: TXaudioPlayer; Caps: Cardinal) of object;
  TNotifyInputPositionEvent = procedure(Sender: TXaudioPlayer; Offset: Cardinal; Range: Cardinal) of object;
  TNotifyInputPositionRangeEvent = procedure(Sender: TXaudioPlayer; Range: Integer) of object;
  TNotifyInputTimecodeEvent = procedure(Sender: TXaudioPlayer; Hours: Byte; Minutes: Byte; Seconds: Byte; Fractions: Byte) of object;
  TNotifyInputTimecodeGranularityEvent = procedure(Sender: TXaudioPlayer; Granularity: Cardinal) of object;
  TNotifyInputDurationEvent = procedure(Sender: TXaudioPlayer; Duration: Cardinal) of object;
  TNotifyInputStreamInfoEvent = procedure(Sender: TXaudioPlayer; Level: Byte; Layer: Byte; Bitrate: Word; Frequency: Word; Mode: Byte) of object;
  TNotifyInputModuleEvent = procedure(Sender: TXaudioPlayer; ID: SmallInt) of object;
  TNotifyInputModuleInfoEvent = procedure(Sender: TXaudioPlayer; ID: Byte; NbDevices: Byte; const Name: String; const Description: String) of object;
  TNotifyInputDeviceInfoEvent = procedure(Sender: TXaudioPlayer; ModuleID: Byte; Index: Byte; Flags: Byte; const Name: String; const Description: String) of object;
  TNotifyInputFilterInfoEvent = procedure(Sender: TXaudioPlayer; ID: Byte; const Name: String) of object;
  TNotifyOutputStateEvent = procedure(Sender: TXaudioPlayer; State: Integer) of object;
  TNotifyOutputNameEvent = procedure(Sender: TXaudioPlayer; const Name: String) of object;
  TNotifyOutputCapsEvent = procedure(Sender: TXaudioPlayer; Caps: Cardinal) of object;
  TNotifyOutputVolumeEvent = procedure(Sender: TXaudioPlayer; MasterLevel: Byte; PcmLevel: Byte; Balance: Byte) of object;
  TNotifyOutputBalanceEvent = procedure(Sender: TXaudioPlayer; Balance: Byte) of object;
  TNotifyOutputPcmLevelEvent = procedure(Sender: TXaudioPlayer; PcmLevel: Byte) of object;
  TNotifyOutputMasterLevelEvent = procedure(Sender: TXaudioPlayer; MasterLevel: Byte) of object;
  TNotifyOutputChannelsEvent = procedure(Sender: TXaudioPlayer; Channels: Byte) of object;
  TNotifyOutputPortsEvent = procedure(Sender: TXaudioPlayer; Ports: Cardinal) of object;
  TNotifyOutputModuleEvent = procedure(Sender: TXaudioPlayer; ID: SmallInt) of object;
  TNotifyOutputModuleInfoEvent = procedure(Sender: TXaudioPlayer; ID: Byte; NbDevices: Byte; const Name: String; const Description: String) of object;
  TNotifyOutputDeviceInfoEvent = procedure(Sender: TXaudioPlayer; ModuleID: Byte; Index: Byte; Flags: Byte; const Name: String; const Description: String) of object;
  TNotifyOutputFilterInfoEvent = procedure(Sender: TXaudioPlayer; ID: Byte; const Name: String) of object;
  TNotifyCodecQualityEvent = procedure(Sender: TXaudioPlayer; Quality: Byte) of object;
  TNotifyCodecEqualizerEvent = procedure(Sender: TXaudioPlayer; var Equalizer: XA_EqualizerInfo) of object;
  TNotifyNotificationMask = procedure(Sender: TXaudioPlayer; Mask: Cardinal) of object;
  TNotifyDebugLevelEvent = procedure(Sender: TXaudioPlayer; Level: Byte) of object;
  TNotifyProgressEvent = procedure(Sender: TXaudioPlayer; Source: Byte; Code: Byte; Value: SmallInt; const Msg: String) of object;
  TNotifyDebugEvent = procedure(Sender: TXaudioPlayer; Source: Byte; Level: Byte; const Msg: String) of object;
  TNotifyErrorEvent = procedure(Sender: TXaudioPlayer; Source: Byte; Code: SmallInt; const Msg: String) of object;

{ TXaudioPlayer Class }
  TXaudioPlayer = class(TComponent)
  private
    { Private declarations }
    FPlayer:                           Pointer;
    FWindow:                           HWND;
    FOnNotifyMessage:                  TNotifyMessageEvent;
    FOnNotifyReady:                    TNotifyEvent;
    FOnNotifyAck:                      TNotifyAckEvent;
    FOnNotifyNack:                     TNotifyNackEvent;
    FOnNotifyPong:                     TNotifyPongEvent;
    FOnNotifyExited:                   TNotifyEvent;
    FOnNotifyPlayerState:              TNotifyPlayerStateEvent;
    FOnNotifyPlayerMode:               TNotifyPlayerModeEvent;
    FOnNotifyPlayerEnvironmentInteger: TNotifyEnvironmentIntegerEvent;
    FOnNotifyPlayerEnvironmentString:  TNotifyEnvironmentStringEvent;
    FOnNotifyInputState:               TNotifyInputStateEvent;
    FOnNotifyInputName:                TNotifyInputNameEvent;
    FOnNotifyInputCaps:                TNotifyInputCapsEvent;
    FOnNotifyInputPosition:            TNotifyInputPositionEvent;
    FOnNotifyInputPositionRange:       TNotifyInputPositionRangeEvent;
    FOnNotifyInputTimecode:            TNotifyInputTimecodeEvent;
    FOnNotifyInputTimecodeGranularity: TNotifyInputTimecodeGranularityEvent;
    FOnNotifyInputDuration:            TNotifyInputDurationEvent;
    FOnNotifyInputStreamInfo:          TNotifyInputStreamInfoEvent;
    FOnNotifyInputModule:              TNotifyInputModuleEvent;
    FOnNotifyInputModuleInfo:          TNotifyInputModuleInfoEvent;
    FOnNotifyInputDeviceInfo:          TNotifyInputDeviceInfoEvent;
    FOnNotifyInputFilterInfo:          TNotifyInputFilterInfoEvent;
    FOnNotifyOutputState:              TNotifyOutputStateEvent;
    FOnNotifyOutputName:               TNotifyOutputNameEvent;
    FOnNotifyOutputCaps:               TNotifyOutputCapsEvent;
    FOnNotifyOutputVolume:             TNotifyOutputVolumeEvent;
    FOnNotifyOutputBalance:            TNotifyOutputBalanceEvent;
    FOnNotifyOutputPcmLevel:           TNotifyOutputPcmLevelEvent;
    FOnNotifyOutputMasterLevel:        TNotifyOutputMasterLevelEvent;
    FOnNotifyOutputChannels:           TNotifyOutputChannelsEvent;
    FOnNotifyOutputPorts:              TNotifyOutputPortsEvent;
    FOnNotifyOutputModule:             TNotifyOutputModuleEvent;
    FOnNotifyOutputModuleInfo:         TNotifyOutputModuleInfoEvent;
    FOnNotifyOutputDeviceInfo:         TNotifyOutputDeviceInfoEvent;
    FOnNotifyOutputFilterInfo:         TNotifyOutputFilterInfoEvent;
    FOnNotifyCodecQuality:             TNotifyCodecQualityEvent;
    FOnNotifyCodecEqualizer:           TNotifyCodecEqualizerEvent;
    FOnNotifyNotificationMask:         TNotifyNotificationMask;
    FOnNotifyDebugLevel:               TNotifyDebugLevelEvent;
    FOnNotifyProgress:                 TNotifyProgressEvent;
    FOnNotifyDebug:                    TNotifyDebugEvent;
    FOnNotifyError:                    TNotifyErrorEvent;
    procedure SetPriority(Priority: Cardinal);
    function GetPriority: Cardinal;

  protected
    { Protected declarations }
    function TranslateMessage(var msg_from: TMessage; var msg_to: XA_Message): Integer;
    procedure OnWindowsMessage(var win32_msg: TMessage);

  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

  published
    { Published declarations }
    function ErrorString(code: Integer): string;
    procedure Exit;
    procedure Ping(Tag: Cardinal);
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure InputSeek(Offset: Cardinal; Range: Cardinal);
    procedure InputOpen(const Name: string);
    procedure InputClose;
    procedure InputSendMessage(MsgType: Word; const Data: Pointer; Size: Cardinal);
    procedure InputAddFilter(const Name: String; Where: Integer);
    procedure InputRemoveFilter(const Name: String; ID: Integer);
    procedure InputFilterList;
    procedure InputModuleRegister(const Name: String);
    procedure InputModuleQuery(ID: Cardinal);
    procedure InputModulesList;
    procedure OutputOpen(const Name: String);
    procedure OutputClose;
    procedure OutputMute;
    procedure OutputUnmute;
    procedure OutputReset;
    procedure OutputDrain;
    procedure OutputSendMessage(MsgType: Word; const Data: Pointer; Size: Cardinal);
    procedure OutputAddFilter(const Name: String; Where: Integer);
    procedure OutputRemoveFilter(const Name: String; ID: Integer);
    procedure OutputFilterList;
    procedure OutputModuleRegister(const Name: String);
    procedure OutputModuleQuery(ID: Cardinal);
    procedure OutputModulesList;
    procedure SetPlayerMode(Mode: Cardinal);
    procedure GetPlayerMode;
    procedure SetPlayerEnvironmentInteger(const Name: String; Value: Integer);
    procedure GetPlayerEnvironmentInteger(const Name: String);
    procedure SetPlayerEnvironmentString(const Name: String; const Value: String);
    procedure GetPlayerEnvironmentString(const Name: String);
    procedure UnsetPlayerEnvironment(const Name: String);
    procedure SetInputModule(ID: Cardinal);
    procedure GetInputModule;
    procedure SetInputName(const Name: String);
    procedure GetInputName;
    procedure SetInputPositionRange(Range: Cardinal);
    procedure GetInputPositionRange;
    procedure SetInputTimecodeGranularity(Granularity: Cardinal);
    procedure GetInputTimecodeGranularity;
    procedure SetOutputModule(ID: Cardinal);
    procedure GetOutputModule;
    procedure SetOutputName(const Name: String);
    procedure GetOutputName;
    procedure SetOutputVolume(Balance: Byte; PcmLevel: Byte; MasterLevel: Byte);
    procedure GetOutputVolume;
    procedure SetOutputChannels(Channels: Cardinal);
    procedure GetOutputChannels;
    procedure SetOutputPorts(Ports: Cardinal);
    procedure GetOutputPorts;
    procedure SetCodecQuality(Quality: Byte);
    procedure GetCodecQuality;
    procedure SetCodecEqualizer(Equalizer: PXA_EqualizerInfo);
    procedure GetCodecEqualizer;
    procedure SetNotificationMask(Mask: Cardinal);
    procedure GetNotificationMask;
    procedure SetDebugLevel(Level: Integer);
    procedure GetDebugLevel;
    property Priority:Cardinal read GetPriority write SetPriority;
    property OnNotifyMessage:TNotifyMessageEvent read FOnNotifyMessage write FOnNotifyMessage;
    property OnNotifyReady:TNotifyEvent read FOnNotifyReady write FOnNotifyReady;
    property OnNotifyAck:TNotifyAckEvent read FOnNotifyAck write FOnNotifyAck;
    property OnNotifyNack:TNotifyNackEvent read FOnNotifyNack write FOnNotifyNack;
    property OnNotifyPong:TNotifyPongEvent read FOnNotifyPong write FOnNotifyPong;
    property OnNotifyExited:TNotifyEvent read FOnNotifyExited write FOnNotifyExited;
    property OnNotifyPlayerState:TNotifyPlayerStateEvent read FOnNotifyPlayerState write FOnNotifyPlayerState;
    property OnNotifyPlayerMode:TNotifyPlayerModeEvent read FOnNotifyPlayerMode write FOnNotifyPlayerMode;
    property OnNotifyPlayerEnvironmentInteger:TNotifyEnvironmentIntegerEvent read FOnNotifyPlayerEnvironmentInteger write FOnNotifyPlayerEnvironmentInteger;
    property OnNotifyPlayerEnvironmentString:TNotifyEnvironmentStringEvent read FOnNotifyPlayerEnvironmentString write FOnNotifyPlayerEnvironmentString;
    property OnNotifyInputState:TNotifyInputStateEvent read FOnNotifyInputState write FOnNotifyInputState;
    property OnNotifyInputName:TNotifyInputNameEvent read FOnNotifyInputName write FOnNotifyInputName;
    property OnNotifyInputCaps:TNotifyInputCapsEvent read FOnNotifyInputCaps write FOnNotifyInputCaps;
    property OnNotifyInputPosition:TNotifyInputPositionEvent read FOnNotifyInputPosition write FOnNotifyInputPosition;
    property OnNotifyInputPositionRange:TNotifyInputPositionRangeEvent read FOnNotifyInputPositionRange write FOnNotifyInputPositionRange;
    property OnNotifyInputTimecode:TNotifyInputTimecodeEvent read FOnNotifyInputTimecode write FOnNotifyInputTimecode;
    property OnNotifyInputTimecodeGranularity:TNotifyInputTimecodeGranularityEvent read FOnNotifyInputTimecodeGranularity write FOnNotifyInputTimecodeGranularity;
    property OnNotifyInputDuration:TNotifyInputDurationEvent read FOnNotifyInputDuration write FOnNotifyInputDuration;
    property OnNotifyInputStreamInfo:TNotifyInputStreamInfoEvent read FOnNotifyInputStreamInfo write FOnNotifyInputStreamInfo;
    property OnNotifyInputModule:TNotifyInputModuleEvent read FOnNotifyInputModule write FOnNotifyInputModule;
    property OnNotifyInputModuleInfo:TNotifyInputModuleInfoEvent read FOnNotifyInputModuleInfo write FOnNotifyInputModuleInfo;
    property OnNotifyInputDeviceInfo:TNotifyInputDeviceInfoEvent read FOnNotifyInputDeviceInfo write FOnNotifyInputDeviceInfo;
    property OnNotifyInputFilterInfo:TNotifyInputFilterInfoEvent read FOnNotifyInputFilterInfo write FOnNotifyInputFilterInfo;
    property OnNotifyOutputState:TNotifyOutputStateEvent read FOnNotifyOutputState write FOnNotifyOutputState;
    property OnNotifyOutputName:TNotifyOutputNameEvent read FOnNotifyOutputName write FOnNotifyOutputName;
    property OnNotifyOutputCaps:TNotifyOutputCapsEvent read FOnNotifyOutputCaps write FOnNotifyOutputCaps;
    property OnNotifyOutputVolume:TNotifyOutputVolumeEvent read FOnNotifyOutputVolume write FOnNotifyOutputVolume;
    property OnNotifyOutputBalance:TNotifyOutputBalanceEvent read FOnNotifyOutputBalance write FOnNotifyOutputBalance;
    property OnNotifyOutputPcmLevel:TNotifyOutputPcmLevelEvent read FOnNotifyOutputPcmLevel write FOnNotifyOutputPcmLevel;
    property OnNotifyOutputMasterLevel:TNotifyOutputMasterLevelEvent read FOnNotifyOutputMasterLevel write FOnNotifyOutputMasterLevel;
    property OnNotifyOutputChannels:TNotifyOutputChannelsEvent read FOnNotifyOutputChannels write FOnNotifyOutputChannels;
    property OnNotifyOutputPorts:TNotifyOutputPortsEvent read FOnNotifyOutputPorts write FOnNotifyOutputPorts;
    property OnNotifyOutputModule:TNotifyOutputModuleEvent read FOnNotifyOutputModule write FOnNotifyOutputModule;
    property OnNotifyOutputModuleInfo:TNotifyOutputModuleInfoEvent read FOnNotifyOutputModuleInfo write FOnNotifyOutputModuleInfo;
    property OnNotifyOutputDeviceInfo:TNotifyOutputDeviceInfoEvent read FOnNotifyOutputDeviceInfo write FOnNotifyOutputDeviceInfo;
    property OnNotifyOutputFilterInfo:TNotifyOutputFilterInfoEvent read FOnNotifyOutputFilterInfo write FOnNotifyOutputFilterInfo;
    property OnNotifyCodecQuality:TNotifyCodecQualityEvent read FOnNotifyCodecQuality write FOnNotifyCodecQuality;
    property OnNotifyCodecEqualizer:TNotifyCodecEqualizerEvent read FOnNotifyCodecEqualizer write FOnNotifyCodecEqualizer;
    property OnNotifyNotificationMask:TNotifyNotificationMask read FOnNotifyNotificationMask write FOnNotifyNotificationMask;
    property OnNotifyDebugLevel:TNotifyDebugLevelEvent read FOnNotifyDebugLevel write FOnNotifyDebugLevel;
    property OnNotifyProgress:TNotifyProgressEvent read FOnNotifyProgress write FOnNotifyProgress;
    property OnNotifyDebug:TNotifyDebugEvent read FOnNotifyDebug write FOnNotifyDebug;
    property OnNotifyError:TNotifyErrorEvent read FOnNotifyError write FOnNotifyError;

  end;

procedure Register;

function player_new(var Player: Pointer; var Args: HWND): Integer; stdcall;
function player_delete(Player: Pointer): Integer; stdcall;
function player_set_priority(Player: Pointer; Priority: Cardinal): Integer; stdcall;
function player_get_priority(Player: Pointer): Integer; stdcall;
function xaudio_error_string(Code: Integer): PChar; stdcall;
function xaudio_get_api_version(ApiID: Integer): Cardinal; stdcall;
function control_message_send_N(Player: Pointer; Code: Integer): Integer; stdcall;
function control_message_send_P(Player: Pointer; Code: Integer; Pointer1: Pointer): Integer; stdcall
function control_message_send_S(Player: Pointer; Code: Integer; Str1: PChar): Integer; stdcall;
function control_message_send_SS(Player: Pointer; Code: Integer; Str1, Str2: PChar): Integer; stdcall;
function control_message_send_SI(Player: Pointer; Code: Integer; Str1: PChar; Int1: Integer): Integer; stdcall;
function control_message_send_IPI(Player: Pointer; Code: Integer; Int1: Integer; Ptr1: Pointer; Int2: Integer): Integer; stdcall;
function control_message_send_I(Player: Pointer; Code: Integer; Int1: Integer): Integer; stdcall;
function control_message_send_II(Player: Pointer; Code: Integer; Int1, Int2: Integer): Integer; stdcall;
function control_message_send_III(Player: Pointer; Code: Integer; Int1, Int2, Int3: Integer): Integer; stdcall;
function control_message_send_IIII(Player: Pointer; Code: Integer; Int1, Int2, Int3, Int4: Integer): Integer; stdcall;
function control_win32_params_to_message(Player: Pointer; var xa_msg: XA_Message; code: UINT; w_param: WPARAM; l_param: LPARAM): Integer; stdcall;

implementation

procedure Register;
begin
  RegisterComponents('Xaudio', [TXaudioPlayer]);
end;

function player_new;                            external 'xaudio.dll';
function player_delete;                         external 'xaudio.dll';
function player_set_priority;                   external 'xaudio.dll';
function player_get_priority;                   external 'xaudio.dll';
function xaudio_error_string;                   external 'xaudio.dll';
function xaudio_get_api_version;                external 'xaudio.dll';
function control_message_send_N;                external 'xaudio.dll';
function control_message_send_P;                external 'xaudio.dll';
function control_message_send_S;                external 'xaudio.dll';
function control_message_send_SS;               external 'xaudio.dll';
function control_message_send_SI;               external 'xaudio.dll';
function control_message_send_IPI;              external 'xaudio.dll';
function control_message_send_I;                external 'xaudio.dll';
function control_message_send_II;               external 'xaudio.dll';
function control_message_send_III;              external 'xaudio.dll';
function control_message_send_IIII;             external 'xaudio.dll';
function control_win32_params_to_message;       external 'xaudio.dll';

{********************************************************************}
constructor TXaudioPlayer.Create(AOwner:TComponent);
var
  ApiVersion: Cardinal;
  ApiMajor: Cardinal;
  ApiMinor: Cardinal;

begin
  { check that the DLL we have has a compatible version }
  ApiVersion := xaudio_get_api_version(XA_API_ID_ASYNC);
  ApiMajor := (ApiVersion shr 16) and $FF;
  ApiMinor := (ApiVersion shr 8) and $FF;
  if (ApiMajor <> XA_ASYNC_API_MAJOR) or (ApiMinor <> XA_ASYNC_API_MINOR) then
    MessageDlg('Xaudio DLL API Mismatch'#13'Expected ' +
               IntToStr(XA_ASYNC_API_MAJOR) + ',' + IntToStr(XA_ASYNC_API_MINOR) +
               #13'Got ' +
               IntToStr(ApiMajor) + ',' + IntToStr(ApiMinor), mtError, [mbOK], 0);
  inherited Create(AOwner);
  FWindow := AllocateHWnd(OnWindowsMessage);
  if player_new(FPlayer, FWindow) <> 0 then
     MessageDlg('Cannot Start Xaudio Player', mtError, [mbOK], 0);
  end;

{********************************************************************}
destructor TXaudioPlayer.Destroy;
begin
  player_delete(FPlayer);
  DeAllocatehWnd(FWindow);
  inherited Destroy;
end;

{************************************************************}
function TXaudioPlayer.TranslateMessage(var msg_from: TMessage; var msg_to: XA_Message): Integer;
begin
  Result := control_win32_params_to_message(FPlayer, msg_to, msg_from.Msg, msg_from.WParam, msg_from.LParam);
end;

{************************************************************}
procedure TXaudioPlayer.SetPriority(Priority: Cardinal);
begin
  player_set_priority(FPlayer, Priority);
end;

{************************************************************}
function TXaudioPlayer.GetPriority: Cardinal;
begin
  Result := player_get_priority(FPlayer);
end;

{************************************************************}
function TXaudioPlayer.ErrorString(code: Integer): string;
begin
  Result := xaudio_error_string(code);
end;

{************************************************************}
procedure TXaudioPlayer.OnWindowsMessage(var win32_msg: TMessage);
var
  xa_msg: XA_Message;
begin
  TranslateMessage(win32_msg, xa_msg);

  { if there is a hook function, call it now }
  if Assigned(FOnNotifyMessage) then
    FOnNotifyMessage(self, xa_msg);

  { dispatch the message to the registered event handler }
  case xa_msg.code of
    XA_MSG_NOTIFY_READY:
       begin
         if Assigned(FOnNotifyReady) then
           FOnNotifyReady(self);
       end;

    XA_MSG_NOTIFY_ACK:
      begin
        if Assigned(FOnNotifyAck) then
          FOnNotifyAck(self, xa_msg.ack);
      end;

    XA_MSG_NOTIFY_NACK:
      begin
        if Assigned(FOnNotifyNack) then
          FOnNotifyNack(self, xa_msg.nack.command, xa_msg.nack.code);
      end;

    XA_MSG_NOTIFY_PONG:
      begin
        if Assigned(FOnNotifyPong) then
          FOnNotifyPong(self, xa_msg.tag);
      end;

    XA_MSG_NOTIFY_EXITED:
      begin
        if Assigned(FOnNotifyExited) then
          FOnNotifyExited(self);
      end;

    XA_MSG_NOTIFY_PLAYER_STATE:
      begin
        if Assigned(FOnNotifyPlayerState) then
          FOnNotifyPlayerState(self, xa_msg.state);
      end;

    XA_MSG_NOTIFY_PLAYER_MODE:
      begin
        if Assigned(FOnNotifyPlayerMode) then
          FOnNotifyPlayerMode(self, xa_msg.mode);
      end;

    XA_MSG_NOTIFY_PLAYER_ENVIRONMENT_INTEGER:
      begin
        if Assigned(FOnNotifyPlayerEnvironmentInteger) then
          FOnNotifyPlayerEnvironmentInteger(self,
                                            xa_msg.environment_info.name,
                                            xa_msg.environment_info.integer_value);
      end;

    XA_MSG_NOTIFY_PLAYER_ENVIRONMENT_STRING:
      begin
        if Assigned(FOnNotifyPlayerEnvironmentString) then
          FOnNotifyPlayerEnvironmentString(self,
                                           xa_msg.environment_info.name,
                                           xa_msg.environment_info.string_value);
      end;

    XA_MSG_NOTIFY_INPUT_STATE:
      begin
        if Assigned(FOnNotifyInputState) then
          FOnNotifyInputState(self, xa_msg.state);
      end;

    XA_MSG_NOTIFY_INPUT_NAME:
      begin
        if Assigned(FOnNotifyInputName) then
          FOnNotifyInputName(self, xa_msg.name);
      end;

    XA_MSG_NOTIFY_INPUT_CAPS:
      begin
        if Assigned(FOnNotifyInputCaps) then
          FOnNotifyInputCaps(self, xa_msg.caps);
      end;

    XA_MSG_NOTIFY_INPUT_POSITION:
      begin
        if Assigned(FOnNotifyInputPosition) then
          FOnNotifyInputPosition(self,
                                 xa_msg.position.offset,
                                 xa_msg.position.range);
      end;

    XA_MSG_NOTIFY_INPUT_POSITION_RANGE:
      begin
        if Assigned(FOnNotifyInputPositionRange) then
          FOnNotifyInputPositionRange(self, xa_msg.range);
      end;

    XA_MSG_NOTIFY_INPUT_TIMECODE:
      begin
        if Assigned(FOnNotifyInputTimecode) then
          FOnNotifyInputTimecode(self,
                                 xa_msg.timecode.h,
                                 xa_msg.timecode.m,
                                 xa_msg.timecode.s,
                                 xa_msg.timecode.f);
      end;

    XA_MSG_NOTIFY_INPUT_TIMECODE_GRANULARITY:
      begin
        if Assigned(FOnNotifyInputTimecodeGranularity) then
          FOnNotifyInputTimecodeGranularity(self, xa_msg.granularity);
      end;

    XA_MSG_NOTIFY_INPUT_DURATION:
      begin
        if Assigned(FOnNotifyInputDuration) then
          FOnNotifyInputDuration(self, xa_msg.duration);
      end;

    XA_MSG_NOTIFY_INPUT_STREAM_INFO:
      begin
        if Assigned(FOnNotifyInputStreamInfo) then
          FOnNotifyInputStreamInfo(self,
                                   xa_msg.stream_info.level,
                                   xa_msg.stream_info.layer,
                                   xa_msg.stream_info.bitrate,
                                   xa_msg.stream_info.frequency,
                                   xa_msg.stream_info.mode);
      end;

    XA_MSG_NOTIFY_INPUT_MODULE:
      begin
        if Assigned(FOnNotifyInputModule) then
          FOnNotifyInputModule(self, xa_msg.module_id);
      end;

    XA_MSG_NOTIFY_INPUT_MODULE_INFO:
      begin
        if Assigned(FOnNotifyInputModuleInfo) then
          FOnNotifyInputModuleInfo(self,
                                   xa_msg.module_info.id,
                                   xa_msg.module_info.nb_devices,
                                   xa_msg.module_info.name,
                                   xa_msg.module_info.description);
      end;

    XA_MSG_NOTIFY_INPUT_DEVICE_INFO:
      begin
        if Assigned(FOnNotifyInputDeviceInfo) then
          FOnNotifyInputDeviceInfo(self,
                                   xa_msg.device_info.module_id,
                                   xa_msg.device_info.index,
                                   xa_msg.device_info.flags,
                                   xa_msg.device_info.name,
                                   xa_msg.device_info.description);
      end;

    XA_MSG_NOTIFY_INPUT_FILTER_INFO:
      begin
        if Assigned(FOnNotifyInputFilterInfo) then
          FOnNotifyInputFilterInfo(self,
                                   xa_msg.filter_info.id,
                                   xa_msg.filter_info.name);
      end;

    XA_MSG_NOTIFY_OUTPUT_STATE:
      begin
        if Assigned(FOnNotifyOutputState) then
          FOnNotifyOutputState(self, xa_msg.state);
      end;

    XA_MSG_NOTIFY_OUTPUT_NAME:
      begin
        if Assigned(FOnNotifyOutputName) then
          FOnNotifyOutputName(self, xa_msg.name);
      end;

    XA_MSG_NOTIFY_OUTPUT_CAPS:
      begin
        if Assigned(FOnNotifyOutputCaps) then
          FOnNotifyOutputCaps(self, xa_msg.caps);
      end;

    XA_MSG_NOTIFY_OUTPUT_VOLUME:
      begin
        if Assigned(FOnNotifyOutputVolume) then
          FOnNotifyOutputVolume(self,
                                xa_msg.volume.master_level,
                                xa_msg.volume.pcm_level,
                                xa_msg.volume.balance);
      end;

    XA_MSG_NOTIFY_OUTPUT_BALANCE:
      begin
        if Assigned(FOnNotifyOutputBalance) then
          FOnNotifyOutputBalance(self, xa_msg.volume.balance);
      end;

    XA_MSG_NOTIFY_OUTPUT_PCM_LEVEL:
      begin
        if Assigned(FOnNotifyOutputPcmLevel) then
          FOnNotifyOutputPcmLevel(self, xa_msg.volume.pcm_level);
      end;

    XA_MSG_NOTIFY_OUTPUT_MASTER_LEVEL:
      begin
        if Assigned(FOnNotifyOutputMasterLevel) then
          FOnNotifyOutputMasterLevel(self, xa_msg.volume.master_level);
      end;

    XA_MSG_NOTIFY_OUTPUT_CHANNELS:
      begin
        if Assigned(FOnNotifyOutputChannels) then
          FOnNotifyOutputChannels(self, xa_msg.channels);
      end;

    XA_MSG_NOTIFY_OUTPUT_PORTS:
      begin
        if Assigned(FOnNotifyOutputPorts) then
          FOnNotifyOutputPorts(self, xa_msg.ports);
      end;

    XA_MSG_NOTIFY_OUTPUT_MODULE:
      begin
        if Assigned(FOnNotifyOutputModule) then
          FOnNotifyOutputModule(self, xa_msg.module_id);
      end;

    XA_MSG_NOTIFY_OUTPUT_MODULE_INFO:
      begin
        if Assigned(FOnNotifyOutputModuleInfo) then
          FOnNotifyOutputModuleInfo(self,
                                    xa_msg.module_info.id,
                                    xa_msg.module_info.nb_devices,
                                    xa_msg.module_info.name,
                                    xa_msg.module_info.description);
      end;

    XA_MSG_NOTIFY_OUTPUT_DEVICE_INFO:
      begin
        if Assigned(FOnNotifyOutputDeviceInfo) then
          FOnNotifyOutputDeviceInfo(self,
                                    xa_msg.device_info.module_id,
                                    xa_msg.device_info.index,
                                    xa_msg.device_info.flags,
                                    xa_msg.device_info.name,
                                    xa_msg.device_info.description);
      end;

    XA_MSG_NOTIFY_OUTPUT_FILTER_INFO:
      begin
        if Assigned(FOnNotifyOutputFilterInfo) then
          FOnNotifyOutputFilterInfo(self,
                                    xa_msg.filter_info.id,
                                    xa_msg.filter_info.name);
      end;

    XA_MSG_NOTIFY_CODEC_QUALITY:
      begin
        if Assigned(FOnNotifyCodecQuality) then
          FOnNotifyCodecQuality(self, xa_msg.quality);
        end;

    XA_MSG_NOTIFY_CODEC_EQUALIZER:
      begin
        if Assigned(FOnNotifyCodecEqualizer) then
          FOnNotifyCodecEqualizer(self, xa_msg.equalizer^);
      end;

    XA_MSG_NOTIFY_NOTIFICATION_MASK:
      begin
        if Assigned(FOnNotifyNotificationMask) then
          FOnNotifyNotificationMask(self, xa_msg.notification_mask);
      end;

    XA_MSG_NOTIFY_DEBUG_LEVEL:
      begin
        if Assigned(FOnNotifyDebugLevel) then
          FOnNotifyDebugLevel(self, xa_msg.debug_level);
      end;

    XA_MSG_NOTIFY_PROGRESS:
      begin
        if Assigned(FOnNotifyProgress) then
          FOnNotifyProgress(self,
                            xa_msg.progress.source,
                            xa_msg.progress.code,
                            xa_msg.progress.value,
                            xa_msg.progress.msg);
      end;

    XA_MSG_NOTIFY_DEBUG:
      begin
        if Assigned(FOnNotifyDebug) then
          FOnNotifyDebug(self,
                         xa_msg.debug.source,
                         xa_msg.debug.level,
                         xa_msg.debug.msg);
      end;

    XA_MSG_NOTIFY_ERROR:
      begin
        if Assigned(FOnNotifyError) then
          FOnNotifyError(self,
                         xa_msg.error.source,
                         xa_msg.error.code,
                         xa_msg.error.msg);
      end;

  end;
end;

{************************************************************}
procedure TXaudioPlayer.Exit;
begin
  control_message_send_N(FPlayer, XA_MSG_COMMAND_EXIT);
end;

{************************************************************}
procedure TXaudioPlayer.Ping(Tag: Cardinal);
begin
  control_message_send_I(FPlayer, XA_MSG_COMMAND_PING, Tag);
end;

{************************************************************}procedure TXaudioPlayer.Play;
begin
  control_message_send_N(FPlayer, XA_MSG_COMMAND_PLAY);
end;

{************************************************************}
procedure TXaudioPlayer.Pause;
begin
  control_message_send_N(FPlayer, XA_MSG_COMMAND_PAUSE);
end;

{************************************************************}
procedure TXaudioPlayer.Stop;
begin
  control_message_send_N(FPlayer, XA_MSG_COMMAND_STOP);
end;

{************************************************************}
procedure TXaudioPlayer.InputSeek(Offset: Cardinal; Range: Cardinal);
begin
  control_message_send_II(FPlayer, XA_MSG_COMMAND_SEEK, Offset, Range);
end;

{************************************************************}
procedure TXaudioPlayer.InputOpen(const Name: String);
begin
  control_message_send_S(FPlayer, XA_MSG_COMMAND_INPUT_OPEN, PChar(Name));
end;

{************************************************************}
procedure TXaudioPlayer.InputClose;
begin
  control_message_send_N(FPlayer, XA_MSG_COMMAND_INPUT_CLOSE);
end;

{************************************************************}
procedure TXaudioPlayer.InputSendMessage(MsgType: Word; const Data: Pointer; Size: Cardinal);
begin
  control_message_send_IPI(FPlayer, XA_MSG_COMMAND_INPUT_SEND_MESSAGE, MsgType, Data, Size);
end;

{************************************************************}
procedure TXaudioPlayer.InputAddFilter(const Name: String; Where: Integer);
begin
  control_message_send_SI(FPlayer, XA_MSG_COMMAND_INPUT_ADD_FILTER, PChar(Name), Where);
end;

{************************************************************}
procedure TXaudioPlayer.InputRemoveFilter(const Name: String; ID: Integer);
begin
  control_message_send_SI(FPlayer, XA_MSG_COMMAND_INPUT_REMOVE_FILTER, PChar(Name), ID);
end;

{************************************************************}
procedure TXaudioPlayer.InputFilterList;
begin
  control_message_send_N(FPlayer, XA_MSG_COMMAND_INPUT_FILTERS_LIST);
end;

{************************************************************}
procedure TXaudioPlayer.InputModuleRegister(const Name: String);
begin
  control_message_send_S(FPlayer, XA_MSG_COMMAND_INPUT_MODULE_REGISTER, PChar(Name));
end;

{************************************************************}
procedure TXaudioPlayer.InputModuleQuery(ID: Cardinal);
begin
  control_message_send_I(FPlayer, XA_MSG_COMMAND_INPUT_MODULE_QUERY, ID);
end;

{************************************************************}
procedure TXaudioPlayer.InputModulesList;
begin
  control_message_send_N(FPlayer, XA_MSG_COMMAND_INPUT_MODULES_LIST);
end;

{************************************************************}
procedure TXaudioPlayer.OutputOpen(const Name: String);
begin
  control_message_send_S(FPlayer, XA_MSG_COMMAND_OUTPUT_OPEN, PChar(Name));
end;

{************************************************************}
procedure TXaudioPlayer.OutputClose;
begin
  control_message_send_N(FPlayer, XA_MSG_COMMAND_OUTPUT_CLOSE);
end;

{************************************************************}
procedure TXaudioPlayer.OutputMute;
begin
  control_message_send_N(FPlayer, XA_MSG_COMMAND_OUTPUT_MUTE);
end;

{************************************************************}
procedure TXaudioPlayer.OutputUnmute;
begin
  control_message_send_N(FPlayer, XA_MSG_COMMAND_OUTPUT_UNMUTE);
end;

{************************************************************}
procedure TXaudioPlayer.OutputReset;
begin
  control_message_send_N(FPlayer, XA_MSG_COMMAND_OUTPUT_RESET);
end;

{************************************************************}
procedure TXaudioPlayer.OutputDrain;
begin
  control_message_send_N(FPlayer, XA_MSG_COMMAND_OUTPUT_DRAIN);
end;

{************************************************************}
procedure TXaudioPlayer.OutputSendMessage(MsgType: Word; const Data: Pointer; Size: Cardinal);
begin
  control_message_send_IPI(FPlayer, XA_MSG_COMMAND_OUTPUT_SEND_MESSAGE, MsgType, Data, Size);
end;

{************************************************************}
procedure TXaudioPlayer.OutputAddFilter(const Name: String; Where: Integer);
begin
  control_message_send_SI(FPlayer, XA_MSG_COMMAND_OUTPUT_ADD_FILTER, PChar(Name), Where);
end;

{************************************************************}
procedure TXaudioPlayer.OutputRemoveFilter(const Name: String; ID: Integer);
begin
  control_message_send_SI(FPlayer, XA_MSG_COMMAND_OUTPUT_REMOVE_FILTER, PChar(Name), ID);
end;

{************************************************************}
procedure TXaudioPlayer.OutputFilterList;
begin
  control_message_send_N(FPlayer, XA_MSG_COMMAND_OUTPUT_FILTERS_LIST);
end;

{************************************************************}
procedure TXaudioPlayer.OutputModuleRegister(const Name: String);
begin
  control_message_send_S(FPlayer, XA_MSG_COMMAND_OUTPUT_MODULE_REGISTER, PChar(Name));
end;

{************************************************************}
procedure TXaudioPlayer.OutputModuleQuery(ID: Cardinal);
begin
  control_message_send_I(FPlayer, XA_MSG_COMMAND_OUTPUT_MODULE_QUERY, ID);
end;

{************************************************************}
procedure TXaudioPlayer.OutputModulesList;
begin
  control_message_send_N(FPlayer, XA_MSG_COMMAND_OUTPUT_MODULES_LIST);
end;

{************************************************************}
procedure TXaudioPlayer.SetPlayerMode(Mode: Cardinal);
begin
  control_message_send_I(FPlayer, XA_MSG_SET_PLAYER_MODE, Mode);
end;

{************************************************************}
procedure TXaudioPlayer.GetPlayerMode;
begin
  control_message_send_N(FPlayer, XA_MSG_GET_PLAYER_MODE);
end;

{************************************************************}
procedure TXaudioPlayer.SetPlayerEnvironmentInteger(const Name: String; Value: Integer);
begin
  control_message_send_SI(FPlayer, XA_MSG_SET_PLAYER_ENVIRONMENT_INTEGER, PChar(Name), Value);
end;

{************************************************************}
procedure TXaudioPlayer.GetPlayerEnvironmentInteger(const Name: String);
begin
  control_message_send_S(FPlayer, XA_MSG_GET_PLAYER_ENVIRONMENT_INTEGER, PChar(Name));
end;

{************************************************************}
procedure TXaudioPlayer.SetPlayerEnvironmentString(const Name: String; const Value: String);
begin
  control_message_send_SS(FPlayer, XA_MSG_SET_PLAYER_ENVIRONMENT_STRING, PChar(Name), PChar(Value));
end;

{************************************************************}
procedure TXaudioPlayer.GetPlayerEnvironmentString(const Name: String);
begin
  control_message_send_S(FPlayer, XA_MSG_GET_PLAYER_ENVIRONMENT_STRING, PChar(Name));
end;

{************************************************************}
procedure TXaudioPlayer.UnsetPlayerEnvironment(const Name: String);
begin
  control_message_send_S(FPlayer, XA_MSG_UNSET_PLAYER_ENVIRONMENT, PChar(Name));
end;

{************************************************************}
procedure TXaudioPlayer.SetInputModule(ID: Cardinal);
begin
  control_message_send_I(FPlayer, XA_MSG_SET_INPUT_MODULE, ID);
end;

{************************************************************}
procedure TXaudioPlayer.GetInputModule;
begin
  control_message_send_N(FPlayer, XA_MSG_GET_INPUT_MODULE);
end;

{************************************************************}
procedure TXaudioPlayer.SetInputName(const Name: String);
begin
  control_message_send_S(FPlayer, XA_MSG_SET_INPUT_NAME, PChar(Name));
end;

{************************************************************}
procedure TXaudioPlayer.GetInputName;
begin
  control_message_send_N(FPlayer, XA_MSG_GET_INPUT_NAME);
end;

{************************************************************}
procedure TXaudioPlayer.SetInputPositionRange(Range: Cardinal);
begin
  control_message_send_I(FPlayer, XA_MSG_SET_INPUT_POSITION_RANGE, Range);
end;

{************************************************************}
procedure TXaudioPlayer.GetInputPositionRange;
begin
  control_message_send_N(FPlayer, XA_MSG_GET_INPUT_POSITION_RANGE);
end;

{************************************************************}
procedure TXaudioPlayer.SetInputTimecodeGranularity(Granularity: Cardinal);
begin
  control_message_send_I(FPlayer, XA_MSG_SET_INPUT_TIMECODE_GRANULARITY, Granularity);
end;

{************************************************************}
procedure TXaudioPlayer.GetInputTimecodeGranularity;
begin
  control_message_send_N(FPlayer, XA_MSG_GET_INPUT_TIMECODE_GRANULARITY);
end;

{************************************************************}
procedure TXaudioPlayer.SetOutputModule(ID: Cardinal);
begin
  control_message_send_I(FPlayer, XA_MSG_SET_OUTPUT_MODULE, ID);
end;

{************************************************************}
procedure TXaudioPlayer.GetOutputModule;
begin
  control_message_send_N(FPlayer, XA_MSG_GET_OUTPUT_MODULE);
end;

{************************************************************}
procedure TXaudioPlayer.SetOutputName(const Name: String);
begin
  control_message_send_S(FPlayer, XA_MSG_SET_OUTPUT_NAME, PChar(Name));
end;

{************************************************************}
procedure TXaudioPlayer.GetOutputName;
begin
  control_message_send_N(FPlayer, XA_MSG_GET_OUTPUT_NAME);
end;

{************************************************************}
procedure TXaudioPlayer.SetOutputVolume(Balance: Byte; PcmLevel: Byte; MasterLevel: Byte);
begin
  control_message_send_III(FPlayer, XA_MSG_SET_OUTPUT_VOLUME, Balance, PcmLevel, MasterLevel);
end;

{************************************************************}
procedure TXaudioPlayer.GetOutputVolume;
begin
  control_message_send_N(FPlayer, XA_MSG_GET_OUTPUT_VOLUME);
end;

{************************************************************}
procedure TXaudioPlayer.SetOutputChannels(Channels: Cardinal);
begin
  control_message_send_I(FPlayer, XA_MSG_SET_OUTPUT_CHANNELS, Channels);
end;

{************************************************************}
procedure TXaudioPlayer.GetOutputChannels;
begin
  control_message_send_N(FPlayer, XA_MSG_GET_OUTPUT_CHANNELS);
end;

{************************************************************}
procedure TXaudioPlayer.SetOutputPorts(Ports: Cardinal);
begin
  control_message_send_I(FPlayer, XA_MSG_SET_OUTPUT_PORTS, Ports);
end;

{************************************************************}
procedure TXaudioPlayer.GetOutputPorts;
begin
  control_message_send_N(FPlayer, XA_MSG_GET_OUTPUT_PORTS);
end;

{************************************************************}
procedure TXaudioPlayer.SetCodecQuality(Quality: Byte);
begin
  control_message_send_I(FPlayer, XA_MSG_SET_CODEC_QUALITY, Quality);
end;

{************************************************************}
procedure TXaudioPlayer.GetCodecQuality;
begin
  control_message_send_N(FPlayer, XA_MSG_GET_CODEC_QUALITY);
end;

{************************************************************}
procedure TXaudioPlayer.SetCodecEqualizer(Equalizer: PXA_EqualizerInfo);
begin
  control_message_send_P(FPlayer, XA_MSG_SET_CODEC_EQUALIZER, Pointer(Equalizer));
end;

{************************************************************}
procedure TXaudioPlayer.GetCodecEqualizer;
begin
  control_message_send_N(FPlayer, XA_MSG_GET_CODEC_EQUALIZER);
end;

{************************************************************}
procedure TXaudioPlayer.SetNotificationMask(Mask: Cardinal);
begin
  control_message_send_I(FPlayer, XA_MSG_SET_NOTIFICATION_MASK, Mask);
end;

{************************************************************}
procedure TXaudioPlayer.GetNotificationMask;
begin
  control_message_send_N(FPlayer, XA_MSG_GET_NOTIFICATION_MASK);
end;

{************************************************************}
procedure TXaudioPlayer.SetDebugLevel(Level: Integer);
begin
  control_message_send_I(FPlayer, XA_MSG_SET_DEBUG_LEVEL, Level);
end;

{************************************************************}
procedure TXaudioPlayer.GetDebugLevel;
begin
  control_message_send_N(FPlayer, XA_MSG_GET_DEBUG_LEVEL);
end;

{************************************************************}
end.
