unit GMIServerRDOMger;

interface

  uses
    WinSockRDOConnection, RDOObjectProxy, RDOInterfaces, RDOServer, GMIntServer, GMKernel;

  const
    CLIENT_TIME_OUT = 3*60*1000;

  type
    TGMIServerRDOMger =
      class
        public
          procedure SetupRDO( ServerAddr : widestring; ServerPort : integer );
          procedure DoneRDO;
          procedure getClientInfo( out Addr : widestring; out Port : integer );
        private
          fRDOConnection : IRDOConnectionInit;
          fGMServerProxy : variant;
          fIntServer     : TGMInterfaceServer;
          fEventsServer  : TRDOServer;
        public
          property IntServer : TGMInterfaceServer read fIntServer;
      end;

  var
    TheIServerRDOMger : TGMIServerRDOMger;

  procedure InitIServerRDOMger;
  procedure DoneIServerRDOMger;

implementation

  uses
    SysUtils;

  type
    TGMServer =
      class(TInterfacedObject, IGMServer)
        public
          constructor Create( aProxy : variant );
        private //IGMServerConnection
          procedure RegisterInterfaceServer( IsId : TIServerId; ClientAddress : widestring; ClientPort : integer; Info : WideString; out IdOnServer : Olevariant );
          procedure UnRegisterCustomer( ISId : TIServerId; aCustomerId : TCustomerId );
          procedure DisconnectUser( ISId : TIServerId; ClientId : TCustomerId; GMId : TGameMasterId );
          function  ConnectToGameMaster( ISId : TIServerId; ClientId : TCustomerId; ClientInfo: widestring; GameMasters : widestring; out GMName : OleVariant; out PendingRequest : OleVariant; out GameMaster : OleVariant ) : OleVariant;
          function  SendGMMessage( ISId : TIServerId; ClientId : TCustomerId; GMId : TGameMasterId; Msg : WideString ) : OleVariant;
        private
          fProxy : variant;
      end;

  // TGMServer

  constructor TGMServer.Create( aProxy : variant );
    begin
      inherited Create;
      fProxy := aProxy;
    end;

  procedure TGMServer.RegisterInterfaceServer( IsId : TIServerId; ClientAddress : widestring; ClientPort : integer; Info : WideString; out IdOnServer : Olevariant );
    begin
      fProxy.RegisterInterfaceServer( IsId, ClientAddress, ClientPort, Info, IdOnServer );
    end;

  procedure TGMServer.UnRegisterCustomer( ISId : TIServerId; aCustomerId : TCustomerId );
    begin
      fProxy.UnRegisterCustomer( ISId, aCustomerId );
    end;

  procedure TGMServer.DisconnectUser( ISId : TIServerId; ClientId : TCustomerId; GMId : TGameMasterId );
    begin
      fProxy.DisconnectUser( ISId, ClientId, GMId );
    end;

  function TGMServer.ConnectToGameMaster( ISId : TIServerId; ClientId : TCustomerId; ClientInfo: widestring; GameMasters : widestring; out GMName : OleVariant; out PendingRequest : OleVariant; out GameMaster : OleVariant ) : OleVariant;
    begin
      result := fProxy.ConnectToGameMaster( ISId, ClientId, ClientInfo, GameMasters, GMName, PendingRequest, GameMaster );
    end;

  function TGMServer.SendGMMessage( ISId : TIServerId; ClientId : TCustomerId; GMId : TGameMasterId; Msg : WideString ) : OleVariant;
    begin
      result := fProxy.SendGMMessage( ISId, ClientId, GMId, Msg );
    end;

  // TGMIServerRDOMger

  procedure TGMIServerRDOMger.SetupRDO( ServerAddr : widestring; ServerPort : integer );
    begin
      fRDOConnection        := TWinSockRDOConnection.Create;
      fRDOConnection.Server := ServerAddr;
      fRDOConnection.Port   := ServerPort;
      if fRDOConnection.Connect( CLIENT_TIME_OUT )
        then
          begin
            fGMServerProxy := TRDOObjectProxy.Create as IDispatch;
            fGMServerProxy.SetConnection( fRDOConnection );
            fGMServerProxy.Timeout       := CLIENT_TIME_OUT;
            fGMServerProxy.WaitForAnswer := false;
            fGMServerProxy.BindTo( tidRDOHook_GMServer );

            fEventsServer := TRDOServer.Create( fRDOConnection as IRDOServerConnection, 1, nil );

            fIntServer     := TGMInterfaceServer.Create;
            fIntServer.setGMServer( TGMServer.Create( fGMServerProxy ));
          end
        else raise Exception.Create( 'Cannnot connect to GM server' );
    end;

  procedure TGMIServerRDOMger.DoneRDO;
    begin
    end;

  procedure TGMIServerRDOMger.getClientInfo( out Addr : widestring; out Port : integer );
    begin
      Addr := (fRDOConnection as IRDOConnection).LocalAddress;
      Port := (fRDOConnection as IRDOConnection).LocalPort;
    end;


  procedure InitIServerRDOMger;
    begin
      TheIServerRDOMger := TGMIServerRDOMger.Create;
    end;

  procedure DoneIServerRDOMger;
    begin
      TheIServerRDOMger.Free;
    end;

end.
