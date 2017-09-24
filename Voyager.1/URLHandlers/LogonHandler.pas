unit LogonHandler;

interface

  uses
    VoyagerInterfaces, Classes, Controls, MPlayer, LogonHandlerViewer;

  type
    TLogonHandler =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler )
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fControl : TLogonHandlerView;
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
          fStarted          : boolean;
      end;

  const
    tidMetaHandler_LogonHandler = 'LogonHandler';


implementation

  uses
    URLParser, SysUtils, Events, Forms;


  // TLogonHandler

  constructor TLogonHandler.Create;
    begin
      inherited Create;
      fControl := TLogonHandlerView.Create( nil );
    end;

  destructor TLogonHandler.Destroy;
    begin
      fControl.Free;
      inherited;
    end;

  function TLogonHandler.getName : string;
    begin
      result := tidMetaHandler_LogonHandler;
    end;

  function TLogonHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable];
    end;

  function TLogonHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TLogonHandler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TLogonHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      result := urlHandled;
    end;

  function TLogonHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    begin
      result := evnHandled;
      case EventId of
        evnHandlerExposed :
          if not fStarted
            then
              begin
                fControl.Start;
                fStarted := true;
              end;
      end;
    end;

  function TLogonHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TLogonHandler.setMasterURLHandler( URLHandler : IMasterURLHandler );
    var
      cache : string;
    begin
      fMasterURLHandler := URLHandler;
      fControl.MasterURLHandler := URLHandler;
      URLHandler.HandleEvent( evnAnswerPrivateCache, cache );
      fControl.Cache := cache;
    end;


end.



