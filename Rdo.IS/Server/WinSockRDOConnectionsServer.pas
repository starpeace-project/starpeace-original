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
        public
          constructor Create( Prt : integer; QuearyThrPrior : TThreadPriority = tpNormal);
          destructor  Destroy; override;
        protected // IRDOServerConnection
          procedure SetQueryServer( const QueryServer : IRDOQueryServer );
          function  GetMaxQueryThreads : integer;
          procedure SetMaxQueryThreads( MaxQueryThreads : integer );
        protected // IRDOConnectionsServer
          procedure StartListening;
          procedure StopListening;
          function  GetClientConnection( const ClientAddress : string; ClientPort : integer ) : IRDOConnection;
          function  GetClientConnectionById( Id : integer ) : IRDOConnection;
          procedure InitEvents( const EventSink : IRDOConnectionServerEvents );
        private
          procedure ClientConnected( Sender : TObject; Socket : TCustomWinSocket );
          procedure ClientDisconnected( Sender : TObject; Socket : TCustomWinSocket );
          procedure DoClientRead( Sender : TObject; Socket : TCustomWinSocket );
          procedure HandleError( Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer );
          procedure RemoveQuery(Socket : TCustomWinSocket);
          function  GetStuckQueries : string;
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
          fLabel           : string;
          fThreadPriority  : TThreadPriority;
        public
          property  theLabel : string read fLabel write fLabel;
      end;

  const
    MaxDebugServers = 5;
    MaxQueryLength  = 1024*1024; // 1 Mb

  var
    DebugServers : array[0..MaxDebugServers] of TWinSockRDOConnectionsServer;
    ServerCount  : integer = 0;

implementation

  uses
    SysUtils,
    Messages,
    ActiveX,
    WinSock,
    RDOProtocol,
    RDOUtils,
    {$IFDEF LogsEnabled}
    LogFile,
    {$ENDIF}
    Logs,
    ErrorCodes,
    RDOQueryServer,
    WinSockRDOServerClientConnection;

  const
    QUERY_COUNT_ENQUIRE = 'GetQueryCount;';

  type
    TSocketWrap =
      class(TInterfacedObject, IWinSocketWrap)
        public
          constructor Create(aSocket : TCustomWinSocket);
          destructor  Destroy; override;
        public
          function  isValid : boolean;
          function  getSocket : TCustomWinSocket;
          function  getConnection : IRDOConnection;
          function  getConnectionData : pointer;
          procedure setConnectionData(info : pointer);
          procedure Invalidate;
          procedure Lock;
          procedure Unlock;
        private
          fSocket : TCustomWinSocket;
          fLock   : TCriticalSection;
      end;

  type
    TSocketData =
      record
        RDOConnection : IRDOConnection;
        Buffer        : string;
        Owner         : IWinSocketWrap;
        Data          : pointer;
      end;

    PSocketData = ^TSocketData;

  type
    TQueryToService =
      record
        Valid  : boolean;
        Text   : string;
        //Socket : TCustomWinSocket;
        Socket : IWinSocketWrap;
      end;

    PQueryToService = ^TQueryToService;

  type
    TQueryThread =
      class( TSmartThread )
        private
          fConnectionsServer : TWinSockRDOConnectionsServer;
          fLock              : TCriticalSection;
          fQueryToService    : PQueryToService;
          fStatus            : integer;
          fQueryStatus       : integer;
          fRDOCallCnt        : integer;
          fExecClientSocket  : IWinSocketWrap;
        public
          constructor Create( theConnServer : TWinSockRDOConnectionsServer; Prior : TThreadPriority);
          destructor  Destroy; override;
          procedure   Execute; override;
        public
          procedure Lock;
          procedure Unlock;
          procedure CheckQuery(Socket : TCustomWinSocket);
        public
          property TheLock : TCriticalSection read fLock;
        public
          procedure Dispatch(var Message); override;
      end;

  type
    TMsgLoopThread =
      class( TSmartThread )
        private
          fRDOConnServ : TWinSockRDOConnectionsServer;
        public
          constructor Create(RDOConnServ : TWinSockRDOConnectionsServer; Prior : TThreadPriority);
          procedure   Execute; override;
      end;

  // TSocketWrap

  constructor TSocketWrap.Create(aSocket : TCustomWinSocket);
    begin
      inherited Create;
      fSocket := aSocket;
      fLock := TCriticalSection.Create;
    end;

  destructor TSocketWrap.Destroy;
    begin
      fLock.Free;
      inherited;
    end;

  function TSocketWrap.isValid   : boolean;
    begin
      Lock;
      try
        result := fSocket <> nil;
      finally
        Unlock;
      end;
    end;

  function TSocketWrap.getSocket : TCustomWinSocket;
    begin
      Lock;
      try
        result := fSocket;
      finally
        Unlock;
      end;
    end;

  function TSocketWrap.getConnection : IRDOConnection;
    begin
      Lock;
      try
        if (fSocket <> nil) and (fSocket.Data <> nil)
          then result := PSocketData(fSocket.Data).RDOConnection
          else result := nil;
      finally
        Unlock;
      end;
    end;

  function TSocketWrap.getConnectionData : pointer;
    begin
      Lock;
      try
        if (fSocket <> nil) and (fSocket.Data <> nil)
          then result := PSocketData(fSocket.Data).Data
          else result := nil;
      finally
        Unlock;
      end;
    end;

  procedure TSocketWrap.setConnectionData(info : pointer);
    begin
      Lock;
      try
        if (fSocket <> nil) and (fSocket.Data <> nil)
          then PSocketData(fSocket.Data).Data := info;
      finally
        Unlock;
      end;
    end;

  procedure TSocketWrap.Invalidate;
    begin
      Lock;
      try
        fSocket := nil;
      finally
        Unlock;
      end;
    end;

  procedure TSocketWrap.Lock;
    begin
      fLock.Enter;
    end;

  procedure TSocketWrap.Unlock;
    begin
      fLock.Leave;
    end;


  // TQueryThread

  constructor TQueryThread.Create(theConnServer : TWinSockRDOConnectionsServer; Prior : TThreadPriority);
    begin
      inherited Create( true );
      fConnectionsServer := theConnServer;
      fLock := TCriticalSection.Create;
      Priority := Prior;
      Resume;
    end;

  destructor TQueryThread.Destroy;
    begin
      Terminate;
      WaitFor;
      fLock.Free;
      inherited;
    end;

  procedure TQueryThread.Execute;
    var
      QueryResult       : string;
      QueryText         : string;
      Sckt              : integer;
      QueryThreadEvents : array [ 1 .. 2 ] of THandle;
      WaitRes           : integer;
      WinSock           : TCustomWinSocket;
    begin
      CoInitialize(nil);
      try
        with fConnectionsServer do
          begin
            QueryThreadEvents[ 1 ] := fQueryWaiting;
            QueryThreadEvents[ 2 ] := fTerminateEvent;
            while not Terminated do
              try
                fStatus := 1; {1}
                WaitRes := WaitForMultipleObjects( 2, @QueryThreadEvents[ 1 ], false, INFINITE );
                fStatus := 2; {2}
                if WaitRes = WAIT_OBJECT_0 + 1
                  then Terminate;
                if not Terminated and (WaitRes = WAIT_OBJECT_0)
                  then
                    begin
                      fQueryQueueLock.Acquire;
                      fStatus := 3; {3}
                      try
                        if fQueryQueue.Count <> 0
                          then
                            begin
                              fQueryToService := fQueryQueue[ 0 ];
                              fQueryQueue.Delete( 0 );
                            end
                          else
                            begin
                              fQueryToService := nil;
                              ResetEvent( fQueryWaiting );
                            end
                      finally
                        fQueryQueueLock.Release
                      end;

                      // Catch the query
                      fStatus := 4; {4}
                      Lock;
                      fStatus := 5; {5}
                      try
                        if (fQueryToService <> nil) and fQueryToService.Valid
                          then
                            begin
                              QueryText := fQueryToService.Text;
                              Sckt := integer(fQueryToService.Socket.getSocket);
                              fExecClientSocket := fQueryToService.Socket;
                            end
                          else
                            begin
                              QueryText := '';
                              Sckt := 0;
                              fExecClientSocket := nil;
                            end;
                      finally
                        Unlock;
                      end;

                      fStatus := 6; {6}
                      // Check if there is something to excecute
                      if QueryText <> ''
                        then
                          begin
                            // Execute the query
                            try
                              QueryResult := fQueryServer.ExecQuery(QueryText, Sckt, fQueryStatus, fRDOCallCnt);
                              fExecClientSocket := nil;
                            except
                              QueryResult := '';
                            end;

                            // Check result
                            if QueryResult = ''
                              then
                                begin
                                  {$IFDEF LogsEnabled}
                                  LogThis( 'No result' );
                                  {$ENDIF}
                                end
                              else
                                begin
                                  // Check if can send result back
                                  fStatus := 7; {7}
                                  Lock;
                                  try
                                    fStatus := 8; {8}
                                    if (fQueryToService <> nil) and fQueryToService.Valid and fQueryToService.Socket.isValid
                                      then
                                        try
                                          fQueryToService.Socket.Lock;
                                          try
                                            WinSock := fQueryToService.Socket.getSocket;
                                            if WinSock <> nil
                                              then WinSock.SendText( AnswerId + QueryResult ); // >> ??
                                          finally
                                            fQueryToService.Socket.Unlock;
                                          end;
                                          {$IFDEF LogsEnabled}
                                          LogThis( 'Result : ' + QueryResult );
                                          {$ENDIF}
                                        except
                                          {$IFDEF LogsEnabled}
                                          LogThis( 'Error sending query result' )
                                          {$ENDIF}
                                        end;
                                  finally
                                    Unlock;
                                  end;
                                end;

                            // Free the Query
                            fStatus := 9; {9}
                            Lock;
                            fStatus := 10; {10}
                            try
                              if fQueryToService <> nil
                                then
                                  begin
                                    fQueryToService.Socket := nil;
                                    Dispose( fQueryToService );
                                    fQueryToService := nil;
                                  end
                            finally
                              Unlock;
                            end;
                          end;
                    end;
              except
                {$IFDEF USELogs}
                Logs.Log('Survival', DateTimeToStr(Now) + 'Un handled exception in the Servicing Query Thread loop! Status: ' + IntToStr(fStatus) + ' ' + QueryText)
                {$ENDIF}
              end;
          end
      finally
        CoUninitialize;
      end;
    end;

  procedure TQueryThread.Lock;
    begin
      fLock.Enter;
    end;

  procedure TQueryThread.Unlock;
    begin
      fLock.Leave;
    end;

  procedure TQueryThread.CheckQuery(Socket : TCustomWinSocket);
    begin
      Lock;
      try
        //if (fQueryToService <> nil) and (fQueryToService.Socket = Socket)
          //then fQueryToService.Valid := false; // lockear
      finally
        Unlock;
      end;
    end;

  procedure TQueryThread.Dispatch(var Message);
    var
      msg  : TMessage absolute Message;
    begin
      case msg.msg of
        MSG_GETTHREADDATA :
          begin
            if fExecClientSocket <> nil
              then msg.Result := integer(fExecClientSocket.getConnectionData)
              else msg.Result := integer(nil);
          end;
        MSG_SETTHREADDATA :
          begin
            if fExecClientSocket <> nil
              then fExecClientSocket.setConnectionData(pointer(msg.LParam))
          end;
      else
        inherited Dispatch(Message);
      end;
    end;

  // TMsgLoopThread

  constructor TMsgLoopThread.Create(RDOConnServ : TWinSockRDOConnectionsServer; Prior : TThreadPriority);
    begin
      inherited Create( true );
      fRDOConnServ := RDOConnServ;
      Priority     := Prior;
      Resume;
    end;

  procedure TMsgLoopThread.Execute;
    var
      Msg   : TMsg;
      alive : boolean;
    begin
      with fRDOConnServ do
        begin
          try
            with fSocketComponent do
              if not Active
                then
                  Active := true
          except
            {$IFDEF USELogs}
            Logs.Log('Survival', 'Error establishing connection' )
            {$ENDIF}
          end;
          alive := true;
          while not Terminated and alive do
            if PeekMessage( Msg, 0, 0, 0, PM_REMOVE )
              then
                try
                  DispatchMessage( Msg )
                except
                {$IFDEF USELogs}
                  on E : Exception do
                    Logs.Log('Survival', 'Loop thread error: '  + E.Message);
                {$ENDIF}
                end
              else alive := MsgWaitForMultipleObjects( 1, fTerminateEvent, false, INFINITE, QS_ALLINPUT ) = WAIT_OBJECT_0 + 1;
          fSocketComponent.Active := false;
        end
    end;

  // TWinSockRDOConnectionsServer

  constructor TWinSockRDOConnectionsServer.Create( Prt : integer; QuearyThrPrior : TThreadPriority);
    begin
      inherited Create;
      fSocketComponent := TServerSocket.Create( nil );
      with fSocketComponent do
        begin
          Active := false;
          //ServerType := stNonBlocking;
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
      fTerminateEvent := CreateEvent( nil, true, false, nil );
      // Temporary debug patch..
      fThreadPriority := QuearyThrPrior;
      DebugServers[ServerCount] := self;
      inc(ServerCount);
    end;

  destructor TWinSockRDOConnectionsServer.Destroy;

    procedure FreeQueryQueue;
      var
        QueryIdx : integer;
        Query    : PQueryToService;
      begin
        for QueryIdx := 0 to fQueryQueue.Count - 1 do
          begin
            Query := PQueryToService(fQueryQueue[ QueryIdx ]);
            Dispose(Query);
          end;
        fQueryQueue.Free
      end;

    begin
      StopListening;
      fSocketComponent.Free;
      fQueryThreads.Free;
      FreeQueryQueue;
      CloseHandle( fQueryWaiting );
      CloseHandle( fTerminateEvent );
      fQueryQueueLock.Free;
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
      i : integer;
    begin
      ResetEvent( fTerminateEvent );
      fMsgLoopThread := TMsgLoopThread.Create(Self, fThreadPriority);
      for i := 1 to fMaxQueryThreads do
        fQueryThreads.Add(TQueryThread.Create(Self, fThreadPriority));
    end;

  procedure TWinSockRDOConnectionsServer.StopListening;

    procedure FreeQueryThreads;
      var
        i            : integer;
        aQueryThread : TSmartThread;
      begin
        for i := 0 to pred(fQueryThreads.Count) do
          begin
            aQueryThread := TSmartThread( fQueryThreads[i] );
            aQueryThread.Free;
          end;
        fQueryThreads.Clear;
      end;

    begin
      SetEvent( fTerminateEvent );
      if fMsgLoopThread <> nil
        then
          begin
            fMsgLoopThread.Free;
            fMsgLoopThread := nil;
          end;
      if fQueryThreads <> nil
        then FreeQueryThreads;
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
          Socket.Lock;
          try
            ConnCount := Socket.ActiveConnections;
            FoundConn := nil;
            if inet_addr( PChar( ClientAddress ) ) = u_long(INADDR_NONE)
              then UseIPAddr := false
              else UseIPAddr := true;
            while ( ConnIdx < ConnCount ) and ( FoundConn = nil ) do
              begin
                CurrConn := Socket.Connections[ ConnIdx ];
                if UseIPAddr
                  then CurConnRmtAddr := CurrConn.RemoteAddress
                  else CurConnRmtAddr := CurrConn.RemoteHost;
                if ( CurConnRmtAddr = ClientAddress )  and ( CurrConn.RemotePort = ClientPort )
                  then
                    begin
                      // >> 16/8/01 Add the connection when requested if not created (avoid creating useless threads)
                      FoundConn := PSocketData( CurrConn.Data ).RDOConnection;
                      if FoundConn = nil
                        then
                          begin
                            FoundConn := TWinSockRDOServerClientConnection.Create(PSocketData(CurrConn.Data).Owner);
                            PSocketData(CurrConn.Data).RDOConnection := FoundConn;
                          end;
                    end
                  else inc(ConnIdx);
              end
          finally
            Socket.Unlock;
          end;
        end;
      Result := FoundConn
    end;

  function TWinSockRDOConnectionsServer.GetClientConnectionById( Id : integer ) : IRDOConnection;
    var
      ConnIdx        : integer;
      ConnCount      : integer;
      FoundConn      : IRDOConnection;
      CurrConn       : TCustomWinSocket;
    begin
      ConnIdx := 0;
      with fSocketComponent do
        begin
          Socket.Lock;
          try
            ConnCount := Socket.ActiveConnections;
            FoundConn := nil;
            while ( ConnIdx < ConnCount ) and ( FoundConn = nil ) do
              begin
                CurrConn := Socket.Connections[ ConnIdx ];
                if integer(CurrConn) = Id
                  then
                    begin
                      // >> 16/8/01 Add the connection when requested if not created (avoid creating useless threads)
                      FoundConn := PSocketData(CurrConn.Data).RDOConnection;
                      if FoundConn = nil
                        then
                          begin
                            FoundConn := TWinSockRDOServerClientConnection.Create(PSocketData(CurrConn.Data).Owner);
                            PSocketData(CurrConn.Data).RDOConnection := FoundConn;
                          end;
                    end
                  else inc(ConnIdx);
              end;
          finally
            Socket.Unlock;
          end;
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
      SocketData.Owner := TSocketWrap.Create(Socket);
      // >> 16/8/01
      SocketData.RDOConnection := nil; //SocketData.RDOConnection := TWinSockRDOServerClientConnection.Create(SocketData.Owner);//, fTerminateEvent);
      Socket.Data := SocketData;
      {if fEventSink <> nil
        then fEventSink.OnClientConnect(SocketData.RDOConnection);}
      // >> 16/8/01
    end;

  procedure TWinSockRDOConnectionsServer.ClientDisconnected( Sender : TObject; Socket : TCustomWinSocket );
    var
      SocketData : PSocketData;
    begin
      SocketData := PSocketData( Socket.Data );
      SocketData.Owner.Invalidate;
      try
        if fEventSink <> nil
          then fEventSink.OnClientDisconnect( SocketData.RDOConnection );
      except
        {$IFDEF USELogs}
        Logs.Log('Survival', DateTimeToStr(Now) + ' TWinSockRDOConnectionsServer.ClientDisconnected (1)' );
        {$ENDIF}
      end;
      try
        if (SocketData.RDOConnection <> nil) and Assigned(SocketData.RDOConnection.OnDisconnect)
          then SocketData.RDOConnection.OnDisconnect(SocketData.RDOConnection);
      except
        {$IFDEF USELogs}
        Logs.Log('Survival', DateTimeToStr(Now) + ' TWinSockRDOConnectionsServer.ClientDisconnected (2)' );
        {$ENDIF}
      end;
      {$IFDEF USELogs}
      //Logs.Log('Survival', 'RemoveQuery start..' );
      {$ENDIF}
      RemoveQuery(Socket); // >> check this
      {$IFDEF USELogs}
      //Logs.Log('Survival', 'RemoveQuery end' );
      {$ENDIF}
      try
        SocketData.Owner := nil;
        dispose(SocketData);
      except
        {$IFDEF USELogs}
        Logs.Log('Survival', DateTimeToStr(Now) + ' TWinSockRDOConnectionsServer.ClientDisconnected (3)' );
        {$ENDIF}
      end;
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
      try
        ReadError := false;
        try
          ReceivedText := Socket.ReceiveText;
          {$IFDEF LogsEnabled}
          if ReceivedText <> ''
            then LogThis( 'Read : ' + ReceivedText )
          {$ENDIF}
        except
          on E : Exception do
            begin
              ReadError := true;
              Logs.Log('Survival', DateTimeToStr(Now) + ' Error while reading from socket ' + E.Message);
            end;
        end;
        if not ReadError and ( fQueryServer <> nil )
          then
            begin
              SocketData := PSocketData( Socket.Data );
              SocketData.Buffer := SocketData.Buffer + ReceivedText;
              QueryText := GetQueryText( SocketData.Buffer );
              if Length(SocketData.Buffer) > MaxQueryLength // >> this is to avoid data bulk hacks..
                then
                  begin
                    SocketData.Buffer := '';
                    Logs.Log('RDO', DateTimeToStr(Now) + ' Socket sending too much data for one query, address: ' + Socket.RemoteAddress);
                    Socket.Close;
                  end
                else
                  while QueryText <> '' do
                    begin
                      NonWSPCharIdx := 1;
                      SkipSpaces( QueryText, NonWSPCharIdx );
                      if QueryText[ NonWSPCharIdx ] = CallId
                        then
                          begin
                            Delete( QueryText, NonWSPCharIdx, 1 );
                            {$IFDEF LogsEnabled}
                            LogThis( 'Query : ' + QueryText );
                            {$ENDIF}
                            New( QueryToService );
                            QueryToService.Text   := QueryText;
                            QueryToService.Socket := PSocketData(Socket.Data).Owner;
                            QueryToService.Valid  := true;
                            fQueryQueueLock.Acquire;
                            try
                              if pos(QUERY_COUNT_ENQUIRE, QueryText) = 0
                                then
                                  begin
                                    if not fQueryServer.Busy
                                      then
                                        begin
                                          fQueryQueue.Add( QueryToService );
                                          if fQueryQueue.Count > 0
                                            then SetEvent( fQueryWaiting );
                                        end
                                      else Socket.SendText(AnswerId + CreateErrorMessage(errServerBusy));
                                  end
                                else
                                  begin
                                    Dispose(QueryToService);
                                    try
                                      Socket.SendText('res = ' + IntToStr(fQueryQueue.Count) + ' ' + GetStuckQueries);
                                    except
                                      Socket.SendText('Internal error!');
                                    end;
                                  end;
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
                                  if SocketData.RDOConnection <> nil // >> 16/8/01
                                    then
                                      begin
                                        ServClienConn := SocketData.RDOConnection as IRDOServerClientConnection;
                                        ServClienConn.OnQueryResultArrival( QueryText )
                                      end;
                                except
                                end
                              end;
                      QueryText := GetQueryText( SocketData.Buffer )
                    end;
            end;
      except
        {$IFDEF USELogs}
        on E : Exception do
          Logs.Log('Survival', 'Error in DoClientRead ' + E.Message)
        {$ENDIF}
      end;
    end;

  procedure TWinSockRDOConnectionsServer.HandleError( Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : Integer );
    begin
      ErrorCode := 0;
      {$IFDEF USELogs}
      case ErrorEvent of
        eeGeneral:
          Logs.Log('Survival', 'General socket error' );
        eeSend:
          Logs.Log('Survival', 'Error writing to socket' );
        eeReceive:
          Logs.Log('Survival', 'Error reading from socket' );
        eeConnect:
          Logs.Log('Survival', 'Error establishing connection' );
        eeDisconnect:
          Logs.Log('Survival', 'Error closing connection' );
        eeAccept:
          Logs.Log('Survival', 'Error accepting connection' )
      end
      {$ENDIF}
    end;

  procedure TWinSockRDOConnectionsServer.RemoveQuery(Socket : TCustomWinSocket);
    //var
      //i     : integer;
      //Query : PQueryToService;
    begin
      {
      try
        fQueryQueueLock.Enter;
        try
          if fQueryQueue.Count > 0
            then Logs.Log('Survival', 'Remove from Queue start..' );
          i := 0;
          while i < fQueryQueue.Count do
            begin
              Query := PQueryToService(fQueryQueue[i]);
              if Query.Socket = Socket
                then
                  begin
                    Dispose(Query);
                    fQueryQueue.Delete(i);
                  end
                else inc(i);
            end;
          if fQueryQueue.Count > 0
            then Logs.Log('Survival', 'Remove from Queue end..' );
        finally
          fQueryQueueLock.Leave;
        end;
        Logs.Log('Survival', 'Remove from Threads start..' );
        for i := 0 to pred(fQueryThreads.Count) do
          TQueryThread(fQueryThreads[i]).CheckQuery(Socket);
        Logs.Log('Survival', 'Remove from Threads end..' );
      except
      end;
      }
    end;

  function TWinSockRDOConnectionsServer.GetStuckQueries : string;
    var
      i      : integer;
      Q      : PQueryToService;
      Thread : TQueryThread;
      status : string;
    begin
      result := '';
      for i := 0 to pred(fQueryThreads.Count) do
        try
          Thread := TQueryThread(fQueryThreads[i]);
          status := IntToStr(Thread.fStatus) + ',' + IntToStr(Thread.fQueryStatus) + ',' + IntToStr(Thread.fRDOCallCnt);
          Q      := Thread.fQueryToService;
          if Q <> nil
            then result := result + IntToStr(i) + '(' + status + '):' + Q.Text + ' '
            else result := result + IntToStr(i) + '(' + status + '): None ';
        except
        end;
    end;

initialization
  fillchar(DebugServers, sizeof(DebugServers), 0);
end.
