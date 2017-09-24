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
    TWinSockRDOConnectionsServer =
      class( TInterfacedObject, IRDOServerConnection, IRDOConnectionsServer )
        private
          fQueryServer     : IRDOQueryServer;
          fSocketComponent : TServerSocket;
          fMsgLoopThread   : TSmartThread;
          fEventSink       : IRDOConnectionServerEvents;
          fQueryQueue      : TList;
          fQueryWaiting    : THandle;
          fQueryQueueLock  : TCriticalSection;
          fMaxQueryThreads : integer;
          fQueryThreads    : TList;
          fTerminateEvent  : THandle;
          // these variables are only for debug
          fReceivedQueries : integer;
          //
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

  type
    TSocketData =
      record
        RDOConnection : IRDOConnection;
        Buffer        : string;
      end;

    PSocketData = ^TSocketData;

  type
    TQueryToService =
      record
        Text   : string;
        Socket : TCustomWinSocket;
      end;

    PQueryToService = ^TQueryToService;

  type
    TServicingQueryThread =
      class( TSmartThread )
        private
          fConnectionsServer : TWinSockRDOConnectionsServer;
        public
          constructor Create( theConnServer : TWinSockRDOConnectionsServer );
          procedure   Execute; override;
      end;

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
      inherited Create( false )
    end;

  procedure TServicingQueryThread.Execute;
    var
      QueryResult       : string;
      QueryToService    : PQueryToService;
      QueryThreadEvents : array [ 1 .. 2 ] of THandle;
    begin
      with fConnectionsServer do
        begin
          QueryThreadEvents[ 1 ] := fQueryWaiting;
          QueryThreadEvents[ 2 ] := fTerminateEvent;
          while not Terminated do
            begin
              WaitForMultipleObjects( 2, @QueryThreadEvents[ 1 ], false, INFINITE );
              fQueryQueueLock.Acquire;
              try
                if fQueryQueue.Count <> 0
                  then
                    begin
                      QueryToService := fQueryQueue[ 0 ];
                      fQueryQueue.Delete( 0 )
                    end
                  else
                    begin
                      QueryToService := nil;
                      ResetEvent( fQueryWaiting )
                    end
              finally
                fQueryQueueLock.Release
              end;
              if QueryToService <> nil
                then
                  try
                    // fQueryServer.Lock;
                    QueryResult := fQueryServer.ExecQuery( QueryToService.Text );
                    // fQueryServer.Unlock;
                    if QueryResult <> ''
                      then
                        try
                          QueryToService.Socket.SendText( AnswerId + QueryResult );
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
                    Dispose( QueryToService )
                  end
            end
        end
    end;

  // TMsgLoopThread

  constructor TMsgLoopThread.Create( RDOConnServ : TWinSockRDOConnectionsServer );
    begin
      fRDOConnServ := RDOConnServ;
//      FreeOnTerminate := true;
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
            LogThis( 'Error establishing connection' )
            {$ENDIF}
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
      fQueryQueue := TList.Create;
      fQueryThreads := TList.Create;
      fQueryWaiting := CreateEvent( nil, true, false, nil );
      fQueryQueueLock := TCriticalSection.Create;
      fTerminateEvent := CreateEvent( nil, true, false, nil )
    end;

  destructor TWinSockRDOConnectionsServer.Destroy;

    procedure FreeQueryQueue;
      var
        QueryIdx : integer;
      begin
        for QueryIdx := 0 to fQueryQueue.Count - 1 do
          Dispose( PQueryToService( fQueryQueue[ QueryIdx ] ) );
        fQueryQueue.Free
      end;

    begin
      StopListening;
      fSocketComponent.Free;
      FreeQueryQueue;
      CloseHandle( fQueryWaiting );
      CloseHandle( fTerminateEvent );
      fQueryQueueLock.Free;
      {$IFDEF Logs}
      LogThis( 'Received queries : ' + IntToStr( fReceivedQueries ) );
      {$ENDIF}
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
    var
      ThreadIdx : integer;
    begin
      ResetEvent( fTerminateEvent );
      fMsgLoopThread := TMsgLoopThread.Create( Self );
      for ThreadIdx := 1 to fMaxQueryThreads do
        fQueryThreads.Add( TServicingQueryThread.Create( Self ) )
    end;

  procedure TWinSockRDOConnectionsServer.StopListening;

    procedure FreeQueryThreads;
      var
        ThreadIdx    : integer;
        aQueryThread : TSmartThread;
      begin
        for ThreadIdx := 0 to fQueryThreads.Count - 1 do
          begin
            aQueryThread := TSmartThread( fQueryThreads[ ThreadIdx ] );
            aQueryThread.Free
          end;
        fQueryThreads.Free
      end;

    begin
      SetEvent( fTerminateEvent );
//      fMsgLoopThread.Terminate;
      fMsgLoopThread.Free;
      fMsgLoopThread := nil;
      FreeQueryThreads
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
                      inc( fReceivedQueries );
                      {$IFDEF Logs}
                      LogThis( 'Query : ' + QueryText );
                      {$ENDIF}
                      New( QueryToService );
                      QueryToService.Text := QueryText;
                      QueryToService.Socket := Socket;
                      fQueryQueueLock.Acquire;
                      try
                        fQueryQueue.Add( QueryToService );
                        if fQueryQueue.Count = 1
                          then
                            SetEvent( fQueryWaiting )
                      finally
                        fQueryQueueLock.Release
                      end
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
