unit GMURDOMger;

interface

  uses
    RDOInterfaces, WinSockRDOConnection, RDOServer, RDOObjectProxy, GameMaster, GMKernel;

  const
    GMUT_TIME_OUT = 3*60*1000;

  type
    TGMUtRDOMger =
      class
        public
          destructor Destroy; override;
        public
          function SetupRDO(ServerAddr : widestring; ServerPort : integer; Name, Password : string) : boolean;
          procedure DoneRDO;
          procedure getClientInfo( out ClientAddr : widestring; out ClientPort : integer );
        private
          fRDOConnection : IRDOConnectionInit;
          fGMServerProxy : variant;
          fGameMaster    : TGameMaster;
          fEventsServer  : TRDOServer;
          fOnDisconnect  : TRDOClientDisconnectEvent;
          procedure OnServerDisconnected( const ClientConnection : IRDOConnection );
          procedure syncOnServerDisconnect( const params : array of const );
          function GetIsConnected : boolean;
        public
          property GameMaster   : TGameMaster read fGameMaster;
          property OnDisconnect : TRDOClientDisconnectEvent read fOnDisconnect write fOnDisconnect;
          property IsConnected  : boolean read GetIsConnected;
      end;

  var
    TheGMURDOMger : TGMUtRDOMger;

  procedure InitRDOMger;
  procedure DoneRDOMger;

implementation

  uses
    SysUtils, Threads, Dialogs;

  type
    TGMServer =
      class(TInterfacedObject, IGMServerConnection)
        public
          constructor Create( aProxy : variant );
        private //IGMServerConnection
          function  RegisterGameMaster( ClientId : integer; GMId : TGameMasterId; GMName : widestring; GMPassword : widestring ) : OleVariant;
          procedure NotifyGMStatus( GMId : TGameMasterId; Status : integer; Customers : integer; Pending : integer );
          procedure UserNotification( ISId : TIServerId; CustomerId : TCustomerId; notID : integer; Info : WideString );
          function  SendMessage( ISId : TIServerId; CustomerId : TCustomerId; Msg : WideString; Info : integer ) : OleVariant;
        private
          fProxy : variant;
      end;

  // TGMServer

  constructor TGMServer.Create( aProxy : variant );
    begin
      inherited Create;
      fProxy := aProxy;
    end;

  function TGMServer.RegisterGameMaster( ClientId : integer; GMId : TGameMasterId; GMName : widestring; GMPassword : widestring ) : OleVariant;
    begin
      try
        result := fProxy.RegisterGameMaster( ClientId, GMId, GMName, GMpassword );
      except
        result := INVALID_GAMEMASTER;
      end;
    end;

  procedure TGMServer.NotifyGMStatus( GMId : TGameMasterId; Status : integer; Customers : integer; Pending : integer );
    begin
      fProxy.NotifyGMStatus( GMId, Status, Customers, Pending );
    end;

  procedure TGMServer.UserNotification( ISId : TIServerId; CustomerId : TCustomerId; notID : integer; Info : WideString );
    begin
      fProxy.UserNotification( ISId, CustomerId, notID, Info );
    end;

  function TGMServer.SendMessage( ISId : TIServerId; CustomerId : TCustomerId; Msg : WideString; Info : integer ) : OleVariant;
    begin
      result := fProxy.SendMessage( ISId, CustomerId, Msg, Info );
    end;

  // TGMUtRDOMger

  destructor TGMUtRDOMger.Destroy;
    begin
      if fRDOConnection <> nil
        then
          begin
            fRDOConnection.Disconnect;
            fRDOConnection := nil;
            fGMServerProxy := null;
            fEventsServer.Free;
          end;
      inherited;
    end;

  function TGMUtRDOMger.SetupRDO(ServerAddr : widestring; ServerPort : integer; Name, Password : string) : boolean;
    var
      ClId : integer;
      wfa  : boolean;
    begin
      fRDOConnection := TWinSockRDOConnection.Create('Game Master');
      fRDOConnection.Server := ServerAddr;
      fRDOConnection.Port   := ServerPort;
      if fRDOConnection.Connect( GMUT_TIME_OUT )
        then
          begin
            fGMServerProxy := TRDOObjectProxy.Create as IDispatch;
            fGMServerProxy.SetConnection( fRDOConnection );
            wfa := fGMServerProxy.WaitForAnswer;
            try
              fGMServerProxy.WaitForAnswer := true;
              fGMServerProxy.Timeout       := GMUT_TIME_OUT;
              if fGMServerProxy.BindTo( tidRDOHook_GMServer )
                then
                  begin
                    fEventsServer := TRDOServer.Create( fRDOConnection as IRDOServerConnection, 1, nil );

                    (fRDOConnection as IRDOConnection).OnDisconnect := OnServerDisconnected;

                    fGameMaster := TGameMaster.Create( Name, Password );
                    ClId := fGMServerProxy.RDOCnntId;
                    fGameMaster.setGMConnection(ClId, TGMServer.Create( fGMServerProxy ) );
                    result := fGameMaster.Valid;
                  end
                else raise Exception.Create( 'Cannot bind to GM Server!' );
            finally
              fGMServerProxy.WaitForAnswer := wfa;
            end;
          end
        else raise Exception.Create( 'Cannot connect to GM Server!' );
    end;

  procedure TGMUtRDOMger.DoneRDO;
    begin
      fRDOConnection := nil;
      fGMServerProxy := NULL;
      fGameMaster.Free;
      fEventsServer.Free;
      fEventsServer := nil;
    end;

  procedure TGMUtRDOMger.getClientInfo( out ClientAddr : widestring; out ClientPort : integer );
    begin
      ClientAddr := (fRDOConnection as IRDOConnection).LocalAddress;
      ClientPort := (fRDOConnection as IRDOConnection).LocalPort;
    end;

  procedure TGMUtRDOMger.OnServerDisconnected( const ClientConnection : IRDOConnection );
    begin
      Join( syncOnServerDisconnect, [0] );
    end;

  function TGMUtRDOMger.GetIsConnected : boolean;
    begin
      result := fRDOConnection = nil;
    end;

  procedure TGMUtRDOMger.syncOnServerDisconnect( const params : array of const );
    begin
      if Assigned(fOnDisconnect)
        then fOnDisconnect( nil );
      DoneRDO;
    end;

  procedure InitRDOMger;
    begin
      TheGMURDOMger := TGMUtRDOMger.Create;
    end;

  procedure DoneRDOMger;
    begin
      TheGMURDOMger.Free;
    end;

end.
