unit WinSockRDOConnectionsServer;

interface

  uses
    Windows,
    Classes,
    SyncObjs,
    SmartThreads,
    SocketComp,
    RDOInterfaces;

  type
    TQueryToService =
      record
        Text   : string;
        Socket : TCustomWinSocket;
      end;

    PQueryToService = ^TQueryToService;

  type
    TWinSockRDOConnectionsServer = class;

    TServicingQueryThread =
      class( TSmartThread )
        private
          fConnectionsServer : TWinSockRDOConnectionsServer;
          fQueryToService    : PQueryToService;
        public
          constructor Create( theConnServer : TWinSockRDOConnectionsServer );
          procedure   Execute; override;
        public
          property QueryToService : PQueryToService read fQueryToService write fQueryToService;
      end;

    TWinSockRDOConnectionsServer =
      class( TInterfacedObject, IRDOServerConnection, IRDOConnectionsServer )
        private
          fQueryServer     : IRDOQueryServer;
          fSocketComponent : TServerSocket;
          fMsgLoopThread   : TSmartThread;
          fEventSink       : IRDOConnectionServerEvents;
          fMaxQueryThreads : integer;
          fQueryCount      : integer;
          fQueryCountLock  : TCriticalSection;
          fTerminateEvent  : THandle;
          fQueryThreads    : TList;
//          fQueryThreadsSem : THandle;
          fNoQueryRunning  : THandle;
        public
          constructor Create( Prt : integer );
          destructor  Destroy; override;
        protected
          procedure SetQueryServer( const QueryServer : IRDOQueryServer );
          function  GetMaxQueryThreads : integer;
          procedure SetMaxQueryThreads( MaxQueryThreads : integer );
        protected
          procedure StartListening;
          procedure StopListening;
          function  GetClientConnection( const ClientAddress : string; ClientPort : integer ) : IRDOConnection;
          procedure InitEvents( const EventSink : IRDOConnectionServerEvents );
        private
          procedure GrowThreadCache( GrowBy : integer );
          procedure ReduceThreadCache( ReduceBy : integer );
          function  GetFreeQueryThread : TServicingQueryThread;
        private
          procedure ClientConnected( Sender : TObject; Socket : TCustomWinSocket );
          procedure ClientDisconnected( Sender : TObject; Socket : TCustomWinSocket );
          procedure DoClientRead( Sender : TObject; Socket : TCustomWinSocket );
          procedure HandleError( Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer );
      end;

implementation

  uses
    SysUtils,
    WinSock,
    RDOProtocol,
    RDOUtils,
    {$IFDEF Logs}
    LogFile,
    {$ENDIF}
    ErrorCodes,
    RDOQueryServer,
    WinSockRDOServerClientConnection;

  const
    DefMaxQueryThreads = 5;

  const
    ThreadCacheDelta = 5;

  type
    TSocketData =
      record
        RDOConnection : IRDOConnection;
        Buffer        : string;
      end;

    PSocketData = ^TSocketData;

  type
    TMsgLoopThread =
      class( TSmartThread )
        private
          fRDOConnServ : TWinSockRDOConnectionsServer;
        public
          constructor Create( RDOConnServ : TWinSockRDOConnectionsServer );
          procedure   Execute; override;
      end;

  // TServicingQueryThread

  constructor TServicingQueryThread.Create( theConnServer : TWinSockRDOConnectionsServer );
    begin
      fConnectionsServer := theConnServer;
      inherited Create( true )
    end;

  procedure TServicingQueryThread.Execute;
    var
      QueryResult : string;
    begin
      while not Terminated do
        with fConnectionsServer do
          try
            fQueryCountLock.Acquire;
            try
              inc( fQueryCount );
              if fQueryCount = 1
                then
                  ResetEvent( fNoQueryRunning );
            finally
              fQueryCountLock.Release
            end;
            QueryResult := fQueryServer.ExecQuery( fQueryToService.Text );
            if QueryResult <> ''
              then
                try
                  fQueryToService.Socket.SendText( AnswerId + QueryResult );
                  {$IFDEF Logs}
                  LogThis( 'Result : ' + QueryResult );
                  {$ENDIF}
                except
                  {$IFDEF Logs}
                  LogThis( 'Error sending query result' )
                  {$ENDIF}
                end
              else
                begin
                  {$IFDEF Logs}
                  LogThis( 'No result' )
                  {$ENDIF}
                end
          finally
            Dispose( fQueryToService );
//            ReleaseSemaphore( fQueryThreadsSem, 1, nil );
            fQueryCountLock.Acquire;
            try
              dec( fQueryCount );
              if fQueryCount = 0
                then
                  SetEvent( fNoQueryRunning );
            finally
              fQueryCountLock.Release
            end;
            Suspend
          end
    end;

  // TMsgLoopThread

  constructor TMsgLoopThread.Create( RDOConnServ : TWinSockRDOConnectionsServer );
    begin
      fRDOConnServ := RDOConnServ;
//      Priority := tpHighest;
      inherited Create( false )
    end;

  procedure TMsgLoopThread.Execute;
    var
      Msg : TMsg;
    begin
      with fRDOConnServ do
        begin
          try
            with fSocketComponent do
              if not Active
                then
                  Active := true
          except
            {$IFDEF Logs}
            LogThis( 'Error establishing connection' );
            {$ENDIF}
            Terminate
          end;
          while not Terminated do
            if PeekMessage( Msg, 0, 0, 0, PM_REMOVE )
              then
                DispatchMessage( Msg )
              else
                MsgWaitForMultipleObjects( 1, fTerminateEvent, false, INFINITE, QS_ALLINPUT );
          with fSocketComponent do
            Active := false
        end
    end;

  // TWinSockRDOConnectionsServer

  constructor TWinSockRDOConnectionsServer.Create( Prt : integer );
    begin
      inherited Create;
      fSocketComponent := TServerSocket.Create( nil );
      with fSocketComponent do
        begin
          Active := false;
          ServerType := stNonBlocking;
          Port := Prt;
          OnClientConnect := ClientConnected;
          OnClientDisconnect := ClientDisconnected;
          OnClientRead := DoClientRead;
          OnClientError := HandleError
        end;
      fQueryCountLock := TCriticalSection.Create;
      fMaxQueryThreads := DefMaxQueryThreads;
      fQueryThreads := TList.Create;
      fTerminateEvent := CreateEvent( nil, true, false, nil );
      fNoQueryRunning := CreateEvent( nil, true, true, nil )
    end;

  destructor TWinSockRDOConnectionsServer.Destroy;
    begin
      StopListening;
      fSocketComponent.Free;
      fQueryThreads.Free;
      CloseHandle( fTerminateEvent );
      CloseHandle( fNoQueryRunning );
      inherited Destroy
    end;

  procedure TWinSockRDOConnectionsServer.SetQueryServer( const QueryServer : IRDOQueryServer );
    begin
      fQueryServer := QueryServer
    end;

  function TWinSockRDOConnectionsServer.GetMaxQueryThreads : integer;
    begin
      Result := fMaxQueryThreads
    end;

  procedure TWinSockRDOConnectionsServer.SetMaxQueryThreads( MaxQueryThreads : integer );
    begin
      fMaxQueryThreads := MaxQueryThreads
    end;

  procedure TWinSockRDOConnectionsServer.StartListening;
    begin
      ResetEvent( fTerminateEvent );
      GrowThreadCache( ThreadCacheDelta );
//      fQueryThreadsSem := CreateSemaphore( nil, fMaxQueryThreads, fMaxQueryThreads, nil );
      fMsgLoopThread := TMsgLoopThread.Create( Self );
    end;

  procedure TWinSockRDOConnectionsServer.StopListening;
    begin
      SetEvent( fTerminateEvent );
      fMsgLoopThread.Free;
      fMsgLoopThread := nil;
      WaitForSingleObject( fNoQueryRunning, INFINITE );
//      CloseHandle( fQueryThreadsSem );
      ReduceThreadCache( fQueryThreads.Count )
    end;

  function TWinSockRDOConnectionsServer.GetClientConnection( const ClientAddress : string; ClientPort : integer ) : IRDOConnection;
    var
      ConnIdx        : integer;
      ConnCount      : integer;
      FoundConn      : IRDOConnection;
      UseIPAddr      : boolean;
      CurrConn       : TCustomWinSocket;
      CurConnRmtAddr : string;
    begin
      ConnIdx := 0;
      with fSocketComponent do
        begin
          ConnCount := Socket.ActiveConnections;
          FoundConn := nil;
          if inet_addr( PChar( ClientAddress ) ) = INADDR_NONE
            then
              UseIPAddr := false
            else
              UseIPAddr := true;
          while ( ConnIdx < ConnCount ) and ( FoundConn = nil ) do
            begin
              CurrConn := Socket.Connections[ ConnIdx ];
              if UseIPAddr
                then
                  CurConnRmtAddr := CurrConn.RemoteAddress
                else
                  CurConnRmtAddr := CurrConn.RemoteHost;
              if ( CurConnRmtAddr = ClientAddress )  and ( CurrConn.RemotePort = ClientPort )
                then
                  FoundConn := PSocketData( CurrConn.Data ).RDOConnection;
              inc( ConnIdx )
            end
        end;
      Result := FoundConn
    end;

  procedure TWinSockRDOConnectionsServer.InitEvents( const EventSink : IRDOConnectionServerEvents );
    begin
      fEventSink := EventSink
    end;

  procedure TWinSockRDOConnectionsServer.GrowThreadCache( GrowBy : integer );
    var
      ThreadCount : integer;
    begin
      for ThreadCount := 1 to GrowBy do
        fQueryThreads.Add( TServicingQueryThread.Create( Self ) )
    end;

  procedure TWinSockRDOConnectionsServer.ReduceThreadCache( ReduceBy : integer );
    var
      ThreadIdx     : integer;
      ThreadCount   : integer;
      KilledThreads : integer;
      aQueryThread  : TServicingQueryThread;
    begin
      ThreadIdx := 0;
      ThreadCount := fQueryThreads.Count;
      KilledThreads := 0;
      while ( KilledThreads < ReduceBy ) and ( ThreadIdx < ThreadCount ) do
        begin
          aQueryThread := TServicingQueryThread( fQueryThreads[ ThreadIdx ] );
          if aQueryThread.Suspended
            then
              begin
                fQueryThreads[ ThreadIdx ] := nil;
                aQueryThread.Free;
                inc( KilledThreads )
              end;
          inc( ThreadIdx )
        end;
      if KilledThreads > 0
        then
          begin
            fQueryThreads.Pack;
            fQueryThreads.Capacity := fQueryThreads.Count
          end
    end;

  function TWinSockRDOConnectionsServer.GetFreeQueryThread : TServicingQueryThread;
    var
      ThreadIdx       : integer;
      ThreadCount     : integer;
      FreeThreadFound : boolean;
    begin
      FreeThreadFound := false;
      ThreadIdx := 0;
      ThreadCount := fQueryThreads.Count;
      while ( ThreadIdx < ThreadCount ) and not FreeThreadFound do
        if TServicingQueryThread( fQueryThreads[ ThreadIdx ] ).Suspended
          then
            FreeThreadFound := true
          else
            inc( ThreadIdx );
      if FreeThreadFound
        then
          Result := TServicingQueryThread( fQueryThreads[ ThreadIdx ] )
        else
          begin
            GrowThreadCache( ThreadCacheDelta );
            Result := TServicingQueryThread( fQueryThreads[ ThreadCount ] )
          end
    end;

  procedure TWinSockRDOConnectionsServer.ClientConnected( Sender : TObject; Socket : TCustomWinSocket );
    var
      SocketData : PSocketData;
    begin
      New( SocketData );
      SocketData.RDOConnection := TWinSockRDOServerClientConnection.Create( Socket );
      Socket.Data := SocketData;
      if fEventSink <> nil
        then
          fEventSink.OnClientConnect( SocketData.RDOConnection )
    end;

  procedure TWinSockRDOConnectionsServer.ClientDisconnected( Sender : TObject; Socket : TCustomWinSocket );
    var
      SocketData : PSocketData;
    begin
      SocketData := PSocketData( Socket.Data );
      if fEventSink <> nil
        then
          fEventSink.OnClientDisconnect( SocketData.RDOConnection );
      Dispose( SocketData )
    end;

  procedure TWinSockRDOConnectionsServer.DoClientRead( Sender : TObject; Socket : TCustomWinSocket );
    var
      NonWSPCharIdx  : integer;
      QueryToService : PQueryToService;
      QueryText      : string;
      QueryThread    : TServicingQueryThread;
      ReadError      : boolean;
      ReceivedText   : string;
      SocketData     : PSocketData;
      ServClienConn  : IRDOServerClientConnection;
    begin
      ReadError := false;
      try
        ReceivedText := Socket.ReceiveText;
        {$IFDEF Logs}
        if ReceivedText <> ''
          then
            LogThis( 'Read : ' + ReceivedText )
        {$ENDIF}
      except
        ReadError := true
      end;
      if not ReadError and ( fQueryServer <> nil )
        then
          begin
            SocketData := PSocketData( Socket.Data );
            SocketData.Buffer := SocketData.Buffer + ReceivedText;
            QueryText := GetQueryText( SocketData.Buffer );
            while QueryText <> '' do
              begin
                NonWSPCharIdx := 1;
                SkipSpaces( QueryText, NonWSPCharIdx );
                if QueryText[ NonWSPCharIdx ] = CallId
                  then
                    begin
                      Delete( QueryText, NonWSPCharIdx, 1 );
                      {$IFDEF Logs}
                      LogThis( 'Query : ' + QueryText );
                      {$ENDIF}
                      New( QueryToService );
                      QueryToService.Text := QueryText;
                      QueryToService.Socket := Socket;
//                      WaitForSingleObject( fQueryThreadsSem, INFINITE );
                      QueryThread := GetFreeQueryThread;
                      QueryThread.QueryToService := QueryToService;
                      QueryThread.Resume
                    end
                  else
                    if QueryText[ NonWSPCharIdx ] = AnswerId
                      then
                        begin
                          try
                            Delete( QueryText, NonWSPCharIdx, 1 );
                            ServClienConn := SocketData.RDOConnection as IRDOServerClientConnection;
                            ServClienConn.OnQueryResultArrival( QueryText )
                          except
                          end
                        end;
                QueryText := GetQueryText( SocketData.Buffer )
              end
          end
        else
          {$IFDEF Logs}
          LogThis( 'Error while reading from socket' )
          {$ENDIF}
    end;

  procedure TWinSockRDOConnectionsServer.HandleError( Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : Integer );
    begin
      ErrorCode := 0;
      {$IFDEF Logs}
      case ErrorEvent of
        eeGeneral:
          LogThis( 'General socket error' );
        eeSend:
          LogThis( 'Error writing to socket' );
        eeReceive:
          LogThis( 'Error reading from socket' );
        eeConnect:
          LogThis( 'Error establishing connection' );
        eeDisconnect:
          LogThis( 'Error closing connection' );
        eeAccept:
          LogThis( 'Error accepting connection' )
      end
      {$ENDIF}
    end;

end.
