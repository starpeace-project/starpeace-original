unit RDORootServer;

interface

  uses
    SyncObjs, RDOServer, RDOInterfaces;

  type
    {$M+}
    TRDORootServer =
      class
        public
          constructor Create(ConnectionsServer : IRDOConnectionsServer; MaxQueryThreads : integer; QueryCritSection : TCriticalSection; const RootName : string);
          destructor  Destroy; override;
        private
          fConnectionsServer : IRDOConnectionsServer;
          fRDOServer         : TRDOServer;
        private
          function GetId : integer;
        public
          property ConnectionsServer : IRDOConnectionsServer read fConnectionsServer;
          property RDOServer         : TRDOServer            read fRDOServer;
        published
          property Id : integer read GetId;
      end;
    {$M-}

implementation

  // TRDORootServer

  constructor TRDORootServer.Create(ConnectionsServer : IRDOConnectionsServer; MaxQueryThreads : integer; QueryCritSection : TCriticalSection; const RootName : string);
    begin
      inherited Create;
      fConnectionsServer := ConnectionsServer;
      fRDOServer := TRDOServer.Create(fConnectionsServer as IRDOServerConnection, MaxQueryThreads, QueryCritSection);
      fRDOServer.RegisterObject(RootName, integer(self));
      fConnectionsServer.StartListening
    end;

  destructor TRDORootServer.Destroy;
    begin
      fConnectionsServer.StopListening;
      fRDOServer.Free;
      inherited
    end;

  function TRDORootServer.GetId : integer;
    begin
      result := integer(self);
    end;

end.
