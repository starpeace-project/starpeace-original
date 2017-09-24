unit RDOServer;

interface

  uses
    SyncObjs, RDOInterfaces, RDOObjectRegistry, RDOObjectServer, RDOQueryServer;

  type
    TRDOClientConnectEvent    = procedure ( const ClientConnection : IRDOConnection ) of object;
    TRDOClientDisconnectEvent = procedure ( const ClientConnection : IRDOConnection ) of object;

  type
    TRDOServer =
      class
        private
          fObjectRegistry     : TRDOObjectsRegistry;
          fQueryServer        : IRDOQueryServer;
          fObjectServer       : TRDOObjectServer;
          fOnClientConnect    : TRDOClientConnectEvent;
          fOnClientDisConnect : TRDOClientDisconnectEvent;
        public
          constructor Create( ServerConnection : IRDOServerConnection; MaxQueryThreads : integer; QueryCritSection : TCriticalSection );
          destructor  Destroy; override;
        public
          procedure RegisterObject( ObjectName : string; ObjectId : integer );
          procedure RegisterLogAgents( Agents : ILogAgents );
        public
          procedure SetCriticalSection( CriticalSection : TCriticalSection );
          procedure Lock;
          procedure UnLock;
        private
          function GetBusy : boolean;
          procedure SetBusy(value : boolean);
        public
          property OnClientConnect    : TRDOClientConnectEvent    read fOnClientConnect    write fOnClientConnect;
          property OnClientDisconnect : TRDOClientDisconnectEvent read fOnClientDisconnect write fOnClientDisconnect;
          property Busy               : boolean                   read GetBusy             write SetBusy;
      end;

implementation

  uses
    Windows;

  type
    TRDOConnectionsServerSink =
      class( TInterfacedObject, IRDOConnectionServerEvents )
        private
          fRDOServer : TRDOServer;
          constructor Create( RDOServer : TRDOServer );
          procedure OnClientConnect( const ClientConnection : IRDOConnection );
          procedure OnClientDisconnect( const ClientConnection : IRDOConnection );
      end;

  // TRDOConnectionsServerSink

  constructor TRDOConnectionsServerSink.Create( RDOServer : TRDOServer );
    begin
      inherited Create;
      fRDOServer := RDOServer
    end;

  procedure TRDOConnectionsServerSink.OnClientConnect( const ClientConnection : IRDOConnection );
    begin
      with fRDOServer do
        if Assigned( fOnClientConnect )
          then
            fOnClientConnect( ClientConnection )
    end;

  procedure TRDOConnectionsServerSink.OnClientDisconnect( const ClientConnection : IRDOConnection );
    begin
      with fRDOServer do
        if Assigned( fOnClientDisconnect )
          then
            fOnClientDisconnect( ClientConnection )
    end;

  // TRDOServer

  constructor TRDOServer.Create( ServerConnection : IRDOServerConnection; MaxQueryThreads : integer; QueryCritSection : TCriticalSection );
    var
      ConnServer : IRDOConnectionsServer;
      Sink       : TRDOConnectionsServerSink;
    begin
      inherited Create;
      fObjectRegistry := TRDOObjectsRegistry.Create;
      fObjectServer := TRDOObjectServer.Create( fObjectRegistry, QueryCritSection );
      fQueryServer := TRDOQueryServer.Create( fObjectServer );
      ServerConnection.SetQueryServer( fQueryServer );
      ServerConnection.MaxQueryThreads := MaxQueryThreads;
      if Succeeded( ServerConnection.QueryInterface( IRDOConnectionsServer, ConnServer ) )
        then
          begin
            Sink := TRDOConnectionsServerSink.Create(Self);
            ConnServer.InitEvents(Sink);
          end;
    end;

  destructor TRDOServer.Destroy;
    begin
      fObjectRegistry.Free;
      fObjectServer.Free;
      inherited Destroy
    end;

  procedure TRDOServer.RegisterObject( ObjectName : string; ObjectId : integer );
    begin
      fObjectRegistry.RegisterObject( ObjectName, ObjectId )
    end;

  procedure TRDOServer.RegisterLogAgents( Agents : ILogAgents );
    begin
      try
        (fQueryServer as IRDOLog).RegisterAgents( Agents );
      except
      end;
    end;

  procedure TRDOServer.SetCriticalSection( CriticalSection : TCriticalSection );
    begin
      fObjectServer.SetCriticalSection( CriticalSection )
    end;

  procedure TRDOServer.Lock;
    begin
      fObjectServer.Lock
    end;

  procedure TRDOServer.UnLock;
    begin
      fObjectServer.UnLock
    end;

  function TRDOServer.GetBusy : boolean;
    begin
      result := fQueryServer.Busy;
    end;

  procedure TRDOServer.SetBusy(value : boolean);
    begin
      fQueryServer.Busy := value;
    end;


end.
