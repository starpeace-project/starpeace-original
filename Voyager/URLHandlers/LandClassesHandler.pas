unit LandClassesHandler;

interface

  uses
    VoyagerInterfaces, Controls, LandInfo;

  type
    TLandClassesHandler =
      class( TLandClassesInfo, IMetaURLHandler, IURLHandler )
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
      end;

  const
    tidMetaHandlerName_LandClasses = 'LandClassesHandler';

  // Events

  const
    evnAswerLandClassesHandler = 80;

implementation

  uses
    Land;


  // TLandClassesHandler

  function TLandClassesHandler.getName : string;
    begin
      result := tidMetaHandlerName_LandClasses;
    end;

  function TLandClassesHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopNonVisual];
    end;

  function TLandClassesHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TLandClassesHandler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TLandClassesHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      result := urlNotHandled;
    end;

  function TLandClassesHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      LandClassesInfo : ILandClassesInfo absolute info;
    begin
      if EventId = evnAswerLandClassesHandler
        then
          begin
            LandClassesInfo := self;
            result := evnHandled;
          end
        else result := evnNotHandled;
    end;

  function TLandClassesHandler.getControl : TControl;
    begin
      result := nil;
    end;

  procedure TLandClassesHandler.setMasterURLHandler( const URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
    end;


end.


