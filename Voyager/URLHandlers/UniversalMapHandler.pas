unit UniversalMapHandler;

interface

  uses
    VoyagerInterfaces, Classes, Controls, MPlayer, UniversalMapViewer, VoyagerServerInterfaces, VCLUtils;

  type
    TUniversalMapHandler =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler )
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fControl  : TUniversalMapView;
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
          procedure setMasterURLHandler( const URLHandler : IMasterURLHandler );
        private
          fMasterURLHandler : IMasterURLHandler;
          fClientView       : IClientView;
      end;

  const
    tidMetaHandler_UniversalMapHandler = 'UniversalMapHandler';

  const
    htmlAction_SetUserInfo = 'SETUSERINFO';
    // htmlParmName_Username  = 'Username';
    // htmlParmName_Password  = 'Password';
    htmlParmName_Logon     = 'Logon';

implementation

  uses
    URLParser, SysUtils, Events, ServerCnxEvents, Forms, URLUtils, ChangeLog, ClientMLS, Config, ServerCnxHandler;


  // TUniversalMapHandler

  constructor TUniversalMapHandler.Create;
    begin
      inherited Create;
      fControl := TUniversalMapView.Create( nil );
    end;

  destructor TUniversalMapHandler.Destroy;
    begin
      RemoveComponentFreeAndNil(fControl); //.rag .Free;
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
            //if (fClientView = nil)
       //       then
                begin
                  fControl.Username := GetParmValue( URL, htmlParmName_Username );
                  fControl.MasterUser := GetParmValue( URL, htmlParmName_MasterUserName );
//                end
//              else
  //              begin
              //    fControl.Username := fClientView.getUserName;
                //  fControl.MasterUser := fClientView.getUserMasterName;
                end;
            fControl.Password := GetParmValue( URL, htmlParmName_Password );
            fControl.Start;
          end;
      result := urlHandled;
    end;

  function TUniversalMapHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
{    var
      cachepath : string;
      fConfigHolder : IConfigHolder;}
    begin
      result := evnHandled;
      case EventId of
        evnHandlerExposed :
          begin
            fMasterURLHandler.HandleEvent( evnAnswerClientView, fClientView );
            if fClientView <> nil
              then
                begin
                  fControl.Username := fClientView.getUserName;
                  fControl.Password := fClientView.getUserPassword;
                  fControl.Start;
                end;
            {
            fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, cachepath );
            fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, fConfigHolder );
            ChangeLogView.LogPath := cachepath + 'news\news' + ActiveLanguage + '.rtf';
            if not FileExists( ChangeLogView.LogPath )
              then ChangeLogView.LogPath := cachepath + 'news\news0.rtf';
            ChangeLogView.LastEntry := fConfigHolder.ReadInteger( false, fClientView.getUserMasterName, 'LastNewsEntry', 0 );
            ChangeLogView.RenderNews;

            if ChangeLogView.NewEntries and (ChangeLogView.ShowModal = mrOk)
              then fConfigHolder.WriteInteger( false, fClientView.getUserMasterName, 'LastNewsEntry', ChangeLogView.LastEntry );
            }
          end;
        evnHandlerUnexposed:
          fControl.RemoveWeb;
        evnLogonCompleted :
          begin
            fControl.Jump.Enabled := true;
            fControl.EnableDblClick := true;
          end;
        evnShutDown :  //.rag
          begin
            fMasterURLHandler := nil;
            fClientView       := nil;
            RemoveComponentFreeAndNil(fControl); //.rag .Free;
          end;
      end;
    end;

  function TUniversalMapHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TUniversalMapHandler.setMasterURLHandler( const URLHandler : IMasterURLHandler );
    var
      cache : string;
    begin
      fMasterURLHandler := URLHandler;
      fControl.MasterURLHandler := URLHandler;
      URLHandler.HandleEvent( evnAnswerPrivateCache, cache );
      fControl.Cache := cache;
    end;


end.



