unit JukeBox;

interface

  uses
    VoyagerInterfaces, Classes, Controls;

  type
    IPlayList =
      interface
        function  SelectedFile : string;
        procedure SelectNextFile;
        procedure Reset;
        procedure JumpToFile( index    : integer );
        procedure AddFile   ( filename : string );
        procedure AddDir    ( path     : string );
        procedure DelFile   ( index    : integer );
      end;

  type
    TJukeBox =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler )
        public
          destructor Destroy; override;
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
          fPlayList         : IPlayList;
      end;

  const
    tidMetaHandler_JukeBox = 'JukeBox';

  const
    evnGetPlaylist = 5600;

implementation

  uses
    MP3Handler, Events, URLParser, SysUtils, MPlayer;

  destructor TJukeBox.Destroy;
    begin
      inherited;
    end;
   
  function TJukeBox.getName : string;
    begin
      result := tidMetaHandler_JukeBox;
    end;

  function TJukeBox.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable, hopNonVisual];
    end;

  function TJukeBox.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TJukeBox.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TJukeBox.HandleURL( URL : TURL ) : TURLHandlingResult;
    var
      action : string;
    begin
      action := URLParser.GetURLAction( URL );
      if action = htmlAction_Play
        then
          begin
            fMasterURLHandler.HandleURL( '?frame_Id=MP3Handler&frame_Action=Play&MediaId=MainSoundTrack&MediaURL=' + fPlayList.SelectedFile );
            result := urlHandled;
          end
        else
          if action = htmlAction_Stop
            then
              begin
                fMasterURLHandler.HandleURL( '?frame_Id=MP3Handler&frame_Action=Stop&MediaId=MainSoundTrack' );
                result := urlHandled;
              end
            else result := urlNotHandled;
    end;

  function TJukeBox.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      MediaStatusInfo : TMediaStatusInfo absolute info;
    begin
      case EventId of
        evnMediaStatusChanged :
          if (MediaStatusInfo.MediaId = uppercase('MainSoundTrack')) and (MediaStatusInfo.Status = nvSuccessful)
            then
              begin
                fPlayList.SelectNextFile;
                fMasterURLHandler.HandleURL( '?frame_Id=MP3Handler&frame_Action=Play&MediaId=MainSoundTrack&MediaURL=' + fPlayList.SelectedFile );
                result := evnHandled;
              end
            else result := evnNotHandled;
        else
          result := evnNotHandled;
      end;
    end;

  function TJukeBox.getControl : TControl;
    begin
      result := nil;
    end;

  procedure TJukeBox.setMasterURLHandler( URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
      fMasterURLHandler.HandleEvent( evnGetPlayList, fPlayList );
    end;


end.

