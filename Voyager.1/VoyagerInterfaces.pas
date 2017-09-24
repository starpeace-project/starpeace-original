unit VoyagerInterfaces;

interface

  uses
    Controls;

  type
    TURL                           = string;
    TURLHandlerOption              = (hopNonVisual, hopCacheable, hopCannotCloseIfBusy, hopEnabledWhenCached);
    TURLHandlerOptions             = set of TURLHandlerOption;
    THandlingAbility               = integer;
    TURLHandlingResult             = (urlNotHandled, urlHandled, urlError);
    TEventHandlingResult           = (evnNotHandled, evnHandled, evnError);
    TEventId                       = integer;
    TMetaHandlerRegistrationResult = (mhrRegistered, mhrDuplicated, mhrUknownError);

  type
    // Interfaces defined:

    IMetaURLHandler   = interface;
    IURLHandler       = interface;
    IMasterURLHandler = interface;

    IMetaURLHandler =
      interface
        function getName    : string;
        function getOptions : TURLHandlerOptions;
        function getCanHandleURL( URL : TURL ) : THandlingAbility;
        function Instantiate : IURLHandler;
      end;

    IURLHandler =
      interface
        function  HandleURL( URL : TURL ) : TURLHandlingResult;
        function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
        function  getControl : TControl;
        procedure setMasterURLHandler( URLHandler : IMasterURLHandler );
      end;

    IMasterURLHandler =
      interface( IURLHandler )
        function  RegisterMetaHandler( aMetaURLHandler : IMetaURLHandler ) : TMetaHandlerRegistrationResult;
        procedure RegisterDefaultHandler( Handler : string );
        procedure RegisterExclusion( Excluder, ToExclude : string; mutual : boolean );
        procedure ReportNavigation( Handler : IURLHandler; URL : TURL; localBack, localForward : boolean );
        function  getURLIsLocal( URL : TURL ) : boolean;
      end;


implementation

end.

