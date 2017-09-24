unit GMServerRDOMger;

interface

  uses
    GMServer, WinSockRDOConnectionsServer, RDOObjectProxy, RDOServer, RDOInterfaces, GMKernel;

  type
    TGMServerRDOMger =
      class
        public
          procedure SetupRDO( RDOPort : integer );
          procedure DoneRDO;
          function  GetIntServerConnection( ClientId : integer; ISId : integer; out RDOConnection : IRDOConnection ) : IIServerConnection;
          function  GetGameMaster( ClientId : integer; GMId : integer; out RDOConnection : IRDOConnection ) : IGameMaster;
        private
          fRDOConnectionsServer : IRDOConnectionsServer;
          fRDOServer            : TRDOServer;
          fGameMasterServer     : TGMServer;
        public
          property GameMasterServer : TGMServer read fGameMasterServer;
      end;

  var
    TheRD0Mger : TGMServerRDOMger;

  procedure InitRD0Mger;
  procedure DneRD0Mger;

implementation

  uses
    SysUtils, Logs;

  type
    TIServerConnection =
      class(TInterfacedObject, IIServerConnection)
        public
          constructor Create( aProxy : variant );
        private
          function  GameMasterMsg( ClientId : TCustomerId; Msg : WideString; Info : integer ) : OleVariant;
          procedure GMNotify( ClientId : TCustomerId; notID : integer; Info : WideString );
        private
          fProxy : variant;
      end;

    TGameMaster =
      class(TInterfacedObject, IGameMaster)
        public
          constructor Create( aProxy : variant );
        private
          function  AddCustomer( ISId : TIServerId; CustomerId : TCustomerId; ClientInfo : widestring ) : olevariant;
          procedure CustomerMsg( ISId : TIServerId; CustomerId : TCustomerId; Msg : WideString );
          procedure UnRegisterCustomer( ISId : TIServerId; aCustomerId : TCustomerId );
          procedure UnRegisterIServer( aIsId : TIServerId );
        private
          fProxy : variant;
      end;

  // TIServerConnection

  constructor TIServerConnection.Create( aProxy : variant );
    begin
      inherited Create;
      fProxy := aProxy;
    end;

  function TIServerConnection.GameMasterMsg( ClientId : TCustomerId; Msg : WideString; Info : integer ) : OleVariant;
    begin
      result := fProxy.GameMasterMsg( ClientId, Msg, Info );
    end;

  procedure TIServerConnection.GMNotify( ClientId : TCustomerId; notID : integer; Info : WideString );
    begin
      fProxy.GMNotify( ClientId, notID, Info );
    end;

  // TGameMaster

  constructor TGameMaster.Create( aProxy : variant );
    begin
      inherited Create;
      fProxy := aProxy;
    end;

  function TGameMaster.AddCustomer( ISId : TIServerId; CustomerId : TCustomerId; ClientInfo : widestring ) : olevariant;
    begin
      result := fProxy.AddCustomer( ISId, CustomerId, ClientInfo );
    end;

  procedure TGameMaster.CustomerMsg( ISId : TIServerId; CustomerId : TCustomerId; Msg : WideString );
    begin
      fProxy.CustomerMsg( ISId, CustomerId, Msg );
    end;

  procedure TGameMaster.UnRegisterCustomer( ISId : TIServerId; aCustomerId : TCustomerId );
    begin
      fProxy.UnRegisterCustomer( ISId, aCustomerId );
    end;

  procedure TGameMaster.UnRegisterIServer( aIsId : TIServerId );
    begin
      fProxy.UnRegisterIServer( aIsId );
    end;

  // TGMServerRDOMger

  procedure TGMServerRDOMger.SetupRDO( RDOPort : integer );
   const
     MaxThreads = 5; // ?
    begin
      fGameMasterServer     := TGMServer.Create;

      fRDOConnectionsServer := TWinSockRDOConnectionsServer.Create( RDOPort );
      fRDOServer := TRDOServer.Create( fRDOConnectionsServer as IRDOServerConnection, MaxThreads, nil );
      fRDOServer.RegisterObject( tidRDOHook_GMServer, integer(fGameMasterServer) );
      fRDOConnectionsServer.StartListening;

      fRDOServer.OnClientDisconnect := fGameMasterServer.OnClientDisconnect;
    end;

  procedure TGMServerRDOMger.DoneRDO;
    begin
      fRDOConnectionsServer.StopListening;
      fRDOServer.Free;
      fGameMasterServer.Free;
    end;

  function TGMServerRDOMger.GetIntServerConnection( ClientId : integer; ISId : integer; out RDOConnection : IRDOConnection ) : IIServerConnection;
    var
      Proxy      : variant;
      Connection : IRDOConnection;
    begin
      try
        Proxy         := TRDOObjectProxy.Create as IDispatch;
        Connection    := fRDOConnectionsServer.GetClientConnectionById( ClientId );
        if Connection <> nil
          then
            begin
              Proxy.Timeout := 3*60*1000;
              Proxy.WaitForAnswer := false;
              Proxy.SetConnection( Connection );
              Proxy.BindTo( ISId );

              RDOConnection := Connection;
              result        := TIServerConnection.Create( Proxy );
            end
          else
            begin
              RDOConnection := nil;
              result        := nil;
            end;
      except
        RDOConnection := nil;
        result        := nil;
      end;
    end;

  function TGMServerRDOMger.GetGameMaster( ClientId : integer; GMId : integer; out RDOConnection : IRDOConnection ) : IGameMaster;
    var
      Proxy      : variant;
      Connection : IRDOConnection;
      str        : string;
    begin
      try
        Proxy      := TRDOObjectProxy.Create as IDispatch;
        Connection := fRDOConnectionsServer.GetClientConnectionById( ClientId );
        if Connection <> nil
          then
            begin
              Proxy.Timeout := 3*60*1000;
              Proxy.WaitForAnswer := false;
              Proxy.SetConnection( Connection );
              Proxy.BindTo( GMId );

              RDOConnection := Connection;
              result        := TGameMaster.Create( Proxy );
            end
          else
            begin
              try
                str := TObject(ClientId).ClassName;
              except
                str := 'NIENTE!!';
              end;
              Log( getLogId, str + ' GetClientConnection failed for client address ' + IntToStr(ClientId) );
              RDOConnection := nil;
              result        := nil;
            end;
      except
        Log( getLogId, 'failure binding ' + IntToStr(ClientId) );
        RDOConnection := nil;
        result        := nil;
      end;
    end;

  procedure InitRD0Mger;
    begin
      TheRD0Mger := TGMServerRDOMger.Create;
    end;

  procedure DneRD0Mger;
    begin
      TheRD0Mger.Free;
    end;

end.
