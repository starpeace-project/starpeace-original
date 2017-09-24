unit SoundHandler;

interface

  uses
    VoyagerInterfaces, Classes, Controls, MPlayer;

  type
    TSoundHandler =
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
          procedure OnNotify( Sender : TObject );
      end;

  const
    tidMetaHandler_Sound = 'SoundHandler';

  const
    htmlAction_Play       = 'PLAY';
    htmlAction_Stop       = 'STOP';
    htmlAction_Pause      = 'PAUSE';
    htmlAction_Resume     = 'RESUME';
    htmlAction_SetVolume  = 'SETVOLUME';
    htmlParmName_MediaId  = 'MediaId';
    htmlParmName_MediaURL = 'MediaURL';
    htmlParmName_UseCache = 'UseCache';
    htmlParmName_Rewind   = 'Rewind';
    htmlParmName_Volume   = 'Volume';
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
    URLParser, SysUtils, Events;


  // TSoundHandler

  constructor TSoundHandler.Create;
    begin
      inherited Create;
      fPlayers := TStringList.Create;
    end;

  destructor TSoundHandler.Destroy;
    var
      i : integer;
    begin
      for i := 0 to pred(fPlayers.Count) do
        TMediaPlayer(fPlayers.Objects[i]).Free;
      fPlayers.Free;
      inherited;
    end;

  function TSoundHandler.getName : string;
    begin
      result := tidMetaHandler_Sound;
    end;

  function TSoundHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable, hopNonVisual];
    end;

  function TSoundHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TSoundHandler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TSoundHandler.HandleURL( URL : TURL ) : TURLHandlingResult;

    function GetCachePath : string;
      begin
        fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, result );
      end;

    function Play( URL : string ) : TURLHandlingResult;
      var
        MediaId  : string;
        MediaURL : string;
        idx      : integer;
        Player   : TMediaPlayer;
      begin
        MediaId  := uppercase(URLParser.GetParmValue( URL, htmlParmName_MediaId ));
        MediaURL := uppercase(URLParser.GetParmValue( URL, htmlParmName_MediaURL ));
        if (MediaId <> '') and (MediaURL <> '')
          then
            begin
              idx := fPlayers.IndexOf( MediaId );
              if idx >= 0
                then Player := TMediaPlayer(fPlayers.Objects[idx])
                else
                  begin
                    Player := TMediaPlayer.Create( nil );
                    Player.OnNotify := OnNotify;
                    Player.Visible := false;
                    Player.Parent := TWinControl(fMasterURLHandler.getControl);
                    fPlayers.AddObject( MediaId, Player );
                  end;
              Player.FileName := GetCachePath + '/Sound/' + MediaURL;
              Player.Tag      := integer(uppercase(URLParser.GetParmValue( URL, htmlParmName_Loop )) = 'YES');
              Player.Open;
              Player.Notify := true;
              Player.Play;
              result := urlHandled;
            end
          else result := urlError;
      end;

    function Stop( URL : string ) : TURLHandlingResult;
      var
        MediaId : string;
        // Rewind  : boolean;
        idx     : integer;
      begin
        MediaId := uppercase(URLParser.GetParmValue( URL, htmlParmName_MediaId ));
        // Rewind  := StrToBoolean(URLParser.GetParmValue( URL, htmlParmName_Rewind ));
        idx := fPlayers.IndexOf( MediaId );
        if idx >= 0
          then TMediaPlayer(fPlayers.Objects[idx]).Stop;
            {
            if Rewind
              then TMediaPlayer(fPlayers.Objects[idx]).Stop
              else TMediaPlayer(fPlayers.Objects[idx]).PauseOnly;
            }
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
          then TMediaPlayer(fPlayers.Objects[idx]).Pause;
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
          then TMediaPlayer(fPlayers.Objects[idx]).Resume;
        result := urlHandled;
      end;

    var
      Action : string;
    begin
      Action := URLParser.GetURLAction( URL );
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
                    else result := urlNotHandled;
    end;

  function TSoundHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    begin
      result := evnHandled;
    end;

  function TSoundHandler.getControl : TControl;
    begin
      result := nil;
    end;

  procedure TSoundHandler.setMasterURLHandler( URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
    end;

  procedure TSoundHandler.OnNotify( Sender : TObject );

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
      if Info.MediaId <> ''
        then
          begin
            Info.Status := (Sender as TMediaPlayer).NotifyValue;
            if (Sender as TMediaPlayer).Tag = 1
              then
                begin
                  (Sender as TMediaPlayer).Rewind;
                  (Sender as TMediaPlayer).Play;
                end;
            fMasterURLHandler.HandleEvent( evnMediaStatusChanged, Info );
          end;
    end;


end.

