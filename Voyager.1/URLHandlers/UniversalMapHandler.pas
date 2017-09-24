unit UniversalMapHandler;

interface

  uses
    VoyagerInterfaces, Classes, Controls, MPlayer, UniversalMapViewer, VoyagerServerInterfaces;

  type
    TUniversalMapHandler =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler )
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fControl  : TUniversalMapView;
          fUserName : string;
          fPassword : string;
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
          fClientView       : IClientView;
      end;

  const
    tidMetaHandler_UniversalMapHandler = 'UniversalMapHandler';

  const
    htmlAction_SetUserInfo = 'SETUSERINFO';
    htmlParmName_Username  = 'Username';
    htmlParmName_Password  = 'Password';
    htmlParmName_Logon     = 'Logon';

implementation

  uses
    URLParser, SysUtils, Events, ServerCnxEvents, Forms, URLUtils;


  // TUniversalMapHandler

  constructor TUniversalMapHandler.Create;
    begin
      inherited Create;
      fControl := TUniversalMapView.Create( nil );
    end;

  destructor TUniversalMapHandler.Destroy;
    begin
      fControl.Free;
      inherited;
    end;

  function TUniversalMapHandler.getName : string;             
    begin
      result := tidMetaHandler_UniversalMapHandler;
    end;

  function TUniversalMapHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable];
    end;

  function TUniversalMapHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TUniversalMapHandler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TUniversalMapHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    var
      AnchorData : TAnchorData;
    begin
      AnchorData := GetAnchorData( URL );
      fControl.Logon := URLParser.StrToBoolean(URLParser.GetParmValue( URL, htmlParmName_Logon ));
      if AnchorData.Action = htmlAction_SetUserInfo
        then
          begin
            if (fClientView = nil) or (fClientView.getUserMasterName = '')
              then fUserName := GetParmValue( URL, htmlParmName_Username )
              else fUserName := fClientView.getUserMasterName;
            fPassword := GetParmValue( URL, htmlParmName_Password );
            fControl.Username := fUserName;
            fControl.Password := fPassword;
            fControl.Start;
          end;
      result := urlHandled;
    end;

  function TUniversalMapHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    begin
      result := evnHandled;
      case EventId of
        evnHandlerExposed :
          begin
            fMasterURLHandler.HandleEvent( evnAnswerClientView, fClientView );
            if fClientView <> nil
              then
                begin
                  fUserName := fClientView.getUserMasterName;
                  fPassword := fClientView.getUserPassword;
                  fControl.Username := fUserName;
                  fControl.Password := fPassword;
                  fControl.Start;
                end;
          end;
        evnLogonCompleted :
          fControl.Jump.Enabled := true;
      end;
    end;

  function TUniversalMapHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TUniversalMapHandler.setMasterURLHandler( URLHandler : IMasterURLHandler );
    var
      cache : string;
    begin
      fMasterURLHandler := URLHandler;
      fControl.MasterURLHandler := URLHandler;
      URLHandler.HandleEvent( evnAnswerPrivateCache, cache );
      fControl.Cache := cache;
    end;


end.



