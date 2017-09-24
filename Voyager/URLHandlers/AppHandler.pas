unit AppHandler;

interface

  uses
    VoyagerInterfaces, Forms, Controls;

  type
    TAppHandler =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler )
        public
          constructor Create( anAppWindow : TForm );
        private
          fAppWindow : TForm;
        // IMetaURLHandler
        private
          function getName : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL( URL : TURL ) : THandlingAbility;
          function Instantiate : IURLHandler;
        // IURLHandler
        private
          function HandleURL( URL : TURL ) : TURLHandlingResult;
          function HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function getControl : TControl;
          procedure setMasterURLHandler( URLHandler : IMasterURLHandler );
        private
          fMasterURLHandler : IMasterURLHandler;
      end;

  const
    tidMetaHandlerName_App = 'AppHandler';

  const
    htmlAction_Close = 'CLOSE';

  const
    evnCanClose = 20;

implementation

  uses
    URLParser;

  constructor TAppHandler.Create( anAppWindow : TForm );
    begin
      inherited Create;
      fAppWindow := anAppWindow;
    end;
    
  function TAppHandler.getName : string;
    begin
      result := tidMetaHandlerName_App;
    end;

  function TAppHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopNonVisual];
    end;

  function TAppHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TAppHandler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TAppHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    var
      Action : string;
      cancel : boolean;
    begin
      Action := URLParser.GetURLAction( URL );
      if Action = htmlAction_Close
        then
          begin
            cancel := false;
            fMasterURLHandler.HandleEvent( evnCanClose, cancel );
            if not cancel
              then fAppWindow.Close;
            result := urlHandled;
          end
        else result := urlNotHandled;
    end;

  function TAppHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    begin
      result := evnNotHandled;
    end;

  function TAppHandler.getControl : TControl;
    begin
      result := nil;
    end;

  procedure TAppHandler.setMasterURLHandler( URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
    end;

    
end.
