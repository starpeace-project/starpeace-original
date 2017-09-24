unit BackgroundHandler;

interface

  uses
    VoyagerInterfaces, Classes, Controls, MPlayer, BackgroundHandlerViewer, VCLUtils;

  type
    TBackgroundHandler =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler )
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fControl : TBackgroundHandlerView;
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
          fCache            : string;
      end;

  const
    tidMetaHandler_BackgroundHandler = 'BackgroundHandler';


implementation

  uses
    URLParser, SysUtils, Events, Forms, JPGtoBMP, Graphics;


  // TBackgroundHandler

  constructor TBackgroundHandler.Create;
    begin
      inherited Create;
      fControl := TBackgroundHandlerView.Create( nil );
    end;

  destructor TBackgroundHandler.Destroy;
    begin
//      fControl.Free;
      RemoveComponentFreeAndNil(fControl); //.rag .Free;
      inherited;
    end;

  function TBackgroundHandler.getName : string;
    begin
      result := tidMetaHandler_BackgroundHandler;
    end;

  function TBackgroundHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable];
    end;

  function TBackgroundHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TBackgroundHandler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TBackgroundHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      result := urlHandled;
    end;

  function TBackgroundHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    begin
      result := evnHandled;
      case EventId of
        evnHandlerExposed :
          begin
          end;
        evnShutDown:
          fMasterURLHandler := nil;  
      end;
    end;

  function TBackgroundHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TBackgroundHandler.setMasterURLHandler( const URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
      URLHandler.HandleEvent( evnAnswerPrivateCache, fCache );
      fControl.Cache := fCache;
    end;


end.



