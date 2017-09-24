unit MP3Handler;

interface

  uses
    VoyagerInterfaces, Classes, Controls, MP3Player, MPlayer;

  type
    TMP3Handler =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler )
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fPlayers : TStringList;
        // IMetaURLHandler
        private
          function getName    : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL( URL : TURL ) : THandlingAbility;
          function Instantiate : IURLHandler;
        // IURLHandler
        private
          function  HandleURL( URL : TURL ) : TURLHandlingResult;
          function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler( URLHandler : IMasterURLHandler );
        private
          fMasterURLHandler : IMasterURLHandler;
        private
          //procedure OnNotify( Sender : TXaudioPlayer; State : Integer );
          procedure PlayerState(Sender : TMP3Player; PrevState, NewState : TMP3PlayerState);
        private
          fLastVolume : integer;
          fDisabled   : boolean;
      end;

  const
    tidMetaHandler_Sound = 'MP3Handler';

  const
    htmlAction_Play       = 'PLAY';
    htmlAction_Stop       = 'STOP';
    htmlAction_Pause      = 'PAUSE';
    htmlAction_Resume     = 'RESUME';
    htmlAction_SetVolume  = 'SETVOLUME';
    htmlParmName_MediaId  = 'MediaId';
    htmlParmName_MediaURL = 'MediaURL';
    htmlParmName_Volume   = 'Volume';
    htmlParmName_UseCache = 'UseCache';
    htmlParmName_Rewind   = 'Rewind';
    htmlParmName_Loop     = 'Loop';

  const
    evnAnswerMediaStatus  = 6000;
    evnMediaStatusChanged = 6001;

  type
    TMediaStatusInfo =
      record
        MediaId : string;
        Status  : TMPNotifyValues;
      end;

implementation

  uses
    URLParser, SysUtils, Events, Forms, SoundLib;


  // TMP3Handler

  constructor TMP3Handler.Create;
    begin
      inherited Create;
      fPlayers    := TStringList.Create;
      fLastVolume := -1;
    end;

  destructor TMP3Handler.Destroy;
    var
      i : integer;
    begin
      for i := 0 to pred(fPlayers.Count) do
        TMediaPlayer(fPlayers.Objects[i]).Free;
      fPlayers.Free;
      inherited;
    end;

  function TMP3Handler.getName : string;
    begin
      result := tidMetaHandler_Sound;
    end;

  function TMP3Handler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable, hopNonVisual];
    end;

  function TMP3Handler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TMP3Handler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TMP3Handler.HandleURL( URL : TURL ) : TURLHandlingResult;

    function GetCachePath : string;
      begin
        fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, result );
      end;

    function Play( URL : string ) : TURLHandlingResult;
      var
        MediaId  : string;
        MediaURL : string;
        idx      : integer;
        Player   : TMP3Player;
      begin
        MediaId  := uppercase(URLParser.GetParmValue( URL, htmlParmName_MediaId ));
        MediaURL := uppercase(URLParser.GetParmValue( URL, htmlParmName_MediaURL ));
        if (MediaId <> '') and (MediaURL <> '')
          then
            begin
              try
                idx := fPlayers.IndexOf( MediaId );
                if idx >= 0
                  then Player := TMP3Player(fPlayers.Objects[idx])
                  else
                    try
                      Player               := TMP3Player.Create;
                      Player.DirectSound   := GetDSoundInstance;
                      Player.OnStateChange := PlayerState;

                      fPlayers.AddObject( MediaId, Player );
                    except
                      fDisabled := true;
                      Player    := nil;
                    end;
                if not fDisabled
                  then
                    begin
                      Player.Loop := (uppercase(URLParser.GetParmValue( URL, htmlParmName_Loop )) = 'YES');

                      if ExtractFilePath(MediaURL) = ''
                        then Player.FileName := GetCachePath + 'Sound\' + MediaURL
                        else Player.FileName := MediaURL;

                      Player.Play;
                      result := urlHandled;
                    end
                  else result := urlHandled;
              except
                result := urlError;
              end;
            end
          else result := urlError;
      end;

    function Stop( URL : string ) : TURLHandlingResult;
      var
        MediaId : string;
        idx     : integer;
      begin
        MediaId := uppercase(URLParser.GetParmValue( URL, htmlParmName_MediaId ));
        idx := fPlayers.IndexOf( MediaId );
        if idx >= 0
          then TMP3Player(fPlayers.Objects[idx]).Stop;
        result := urlHandled;
      end;

    function Pause : TURLHandlingResult;
      var
        MediaId : string;
        idx     : integer;
      begin
        MediaId := uppercase(URLParser.GetParmValue( URL, htmlParmName_MediaId ));
        idx := fPlayers.IndexOf( MediaId );
        if idx >= 0
          then TMP3Player(fPlayers.Objects[idx]).Pause;
        result := urlHandled;
      end;

    function Resume : TURLHandlingResult;
      var
        MediaId : string;
        idx     : integer;
      begin
        MediaId := uppercase(URLParser.GetParmValue( URL, htmlParmName_MediaId ));
        idx := fPlayers.IndexOf( MediaId );
        if idx >= 0
          then TMP3Player(fPlayers.Objects[idx]).Play;
        result := urlHandled;
      end;

    function SetVolume( URL : string ) : TURLHandlingResult;
      var
        MediaId : string;
        Volume  : integer;
        idx     : integer;
      begin
        MediaId := uppercase(URLParser.GetParmValue( URL, htmlParmName_MediaId ));
        Volume  := StrToInt(URLParser.GetParmValue( URL, htmlParmName_Volume ));
        idx := fPlayers.IndexOf( MediaId );
        if idx >= 0
          then TMP3Player(fPlayers.Objects[idx]).Volume:= Volume;
        fLastVolume := Volume;
        result := urlHandled;
      end;

    var
      Action : string;
    begin
      Action := URLParser.GetURLAction( URL );
      if not fDisabled
        then
          begin
            if Action = htmlAction_Play
              then result := Play( URL )
              else
                if Action = htmlAction_Stop
                  then result := Stop( URL )
                  else
                    if Action = htmlAction_Pause
                      then result := Pause
                      else
                        if Action = htmlAction_Resume
                          then result := Resume
                          else
                            if Action = htmlAction_SetVolume
                              then result := SetVolume( URL )
                              else result := urlNotHandled;
          end
        else result := urlHandled;
    end;

  function TMP3Handler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    begin
      result := evnHandled;
    end;

  function TMP3Handler.getControl : TControl;
    begin
      result := nil;
    end;

  procedure TMP3Handler.setMasterURLHandler( URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
    end;

  procedure TMP3Handler.PlayerState(Sender : TMP3Player; PrevState, NewState : TMP3PlayerState);
    function GetMediaIdOf( Player : TObject ) : string;
      var
        i : integer;
      begin
        i := 0;
        while (i < fPlayers.Count) and (fPlayers.Objects[i] <> Player) do
          inc( i );
        if i < fPlayers.Count
          then result := fPlayers[i]
          else result := '';
      end;

    var
      Info : TMediaStatusInfo;
    begin
      Info.MediaId := GetMediaIdOf( Sender );
      case NewState of
        mpsStopped :
          if (Info.MediaId <> '')
            then
              begin
                Info.Status := nvSuccessful;
                fMasterURLHandler.HandleEvent( evnMediaStatusChanged, Info );
              end;
      end;
    end;

end.

