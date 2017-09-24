unit RoadsHandler;

interface

  uses
    Controls,
    Classes,
    VoyagerInterfaces,
    VoyagerServerInterfaces,
    CircuitsRenderer,
    CircuitsDownloader;

  type
    TRoadId = TCircuitId;

  type
    TOnAreaRefreshedNotification = procedure ( const RoadsData : ICircuitsData ) of object;

  type
    IRoadsHandler =
      interface ['{FD0207E0-EF3C-11d1-8B24-008029E5CA8C}']
        procedure RefreshArea( x, y, dx, dy : integer; OnAreaRefreshed : TOnAreaRefreshedNotification );
        procedure asyncRefreshArea( x, y, dx, dy : integer; OnAreaRefreshed : TOnAreaRefreshedNotification );
        function  RoadIdAt( x, y : integer ) : TRoadId;
      end;

  type
    TRoadsHandler =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler, IRoadsHandler )
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fMasterURLHandler : IMasterURLHandler;
          fClientView       : IClientView;
          fRoadsDownloader  : ICircuitsDownloader;
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
        // IRoadsHandler
        private
          procedure RefreshArea( x, y, dx, dy : integer; OnAreaRefreshed : TOnAreaRefreshedNotification );
          procedure asyncRefreshArea( x, y, dx, dy : integer; OnAreaRefreshed : TOnAreaRefreshedNotification );
          function  RoadIdAt( x, y : integer ) : TRoadId;
      end;

  const
    tidMetaHandler_Roads = 'RoadsHandler';

  const
    evnAnswerRoadsHandler = 7000;
    //evnAnswerClientView   = 7500;

implementation

  uses
    SysUtils;

  type
    TAreaRoadsRefresherThread =
      class( TThread )
        private
          fRoadsDownloader : ICircuitsDownloader;
          fX, fY, fDX, fDY : integer;
          fOnAreaRefreshed : TOnAreaRefreshedNotification;
        public
          constructor Create( const RoadsDownloader : ICircuitsDownloader; x, y, dx, dy : integer; OnAreaRefreshed : TOnAreaRefreshedNotification );
        protected
          procedure Execute; override;
      end;

  // TAreaRoadsRefresherThread

  constructor TAreaRoadsRefresherThread.Create( const RoadsDownloader : ICircuitsDownloader; x, y, dx, dy : integer; OnAreaRefreshed : TOnAreaRefreshedNotification );
    begin
      fRoadsDownloader := RoadsDownloader;
      fX := x;
      fY := y;
      fDX := dx;
      fDY := dy;
      fOnAreaRefreshed := OnAreaRefreshed;
      inherited Create( false )
    end;

  procedure TAreaRoadsRefresherThread.Execute;
    var
      RoadsData : ICircuitsData;
    begin
      RoadsData := RenderCircuits( fRoadsDownloader.CircuitsInArea( fX, fY, fDX, fDY ), fDX, fDY );
      if Assigned( fOnAreaRefreshed )
        then
          fOnAreaRefreshed( RoadsData )
    end;

  // TRoadsHandler

  constructor TRoadsHandler.Create;
    begin
      inherited Create
    end;

  destructor TRoadsHandler.Destroy;
    begin
      inherited
    end;

  function TRoadsHandler.getName : string;
    begin
      Result := tidMetaHandler_Roads
    end;

  function TRoadsHandler.getOptions : TURLHandlerOptions;
    begin
      Result := [ hopCacheable, hopNonVisual ]
    end;

  function TRoadsHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      Result := 0;
    end;

  function TRoadsHandler.Instantiate : IURLHandler;
    begin
      Result := Self
    end;

  function TRoadsHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      Result := urlNotHandled;
    end;

  function TRoadsHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    begin
      if EventId = evnAnswerRoadsHandler
        then
          begin
            IRoadsHandler( info ) := Self as IRoadsHandler;
            Result := evnHandled
          end
        else
          Result := evnNotHandled
    end;

  function TRoadsHandler.getControl : TControl;
    begin
      result := nil;
    end;

  procedure TRoadsHandler.setMasterURLHandler( URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
      fMasterURLHandler.HandleEvent( evnAnswerClientView, fClientView );
      fRoadsDownloader := TCircuitsDownloader.Create( fClientView, ckRoads )
    end;

  procedure TRoadsHandler.RefreshArea( x, y, dx, dy : integer; OnAreaRefreshed : TOnAreaRefreshedNotification );
    var
      fRoadsData : ICircuitsData;
    begin
      fRoadsData := RenderCircuits( fRoadsDownloader.CircuitsInArea( x, y, dx, dy ), dx, dy );
      if Assigned( OnAreaRefreshed )
        then
          OnAreaRefreshed( fRoadsData )
    end;

  procedure TRoadsHandler.asyncRefreshArea( x, y, dx, dy : integer; OnAreaRefreshed : TOnAreaRefreshedNotification );
    begin
      TAreaRoadsRefresherThread.Create( fRoadsDownloader, x, y, dx, dy, OnAreaRefreshed )
    end;

  function TRoadsHandler.RoadIdAt( x, y : integer ) : TRoadId;
    var
      fRoadsData : ICircuitsData;
    begin
      fRoadsData := RenderCircuits( fRoadsDownloader.CircuitsInArea( x, y, 1, 1 ), 1, 1 );
      Result := fRoadsData[ 0, 0 ]
    end;

end.
