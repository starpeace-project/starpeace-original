unit WinSockRDOConnectionsServer;

interface

  uses
    Windows, Classes, SyncObjs, SmartThreads, SocketComp, RDOInterfaces,
    RDOQueries;

  type
    TWinSockRDOConnectionsServer =
      class(TInterfacedObject, IRDOServerConnection, IRDOConnectionsServer)
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
        public
          constructor Create(Prt : integer);
          destructor  Destroy; override;
        protected
          procedure SetQueryServer(const QueryServer : IRDOQueryServer);
          function  GetMaxQueryThreads : integer;
          procedure SetMaxQueryThreads(MaxQueryThreads : integer);
        protected
          procedure StartListening;
          procedure StopListening;
          function  GetClientConnection(const ClientAddress : string; ClientPort : integer) : IRDOConnection;
          function  GetClientConnectionById(Id : integer) : IRDOConnection;
          procedure InitEvents(const EventSink : IRDOConnectionServerEvents);
        private
          procedure ClientConnected(Sender : TObject; Socket : TCustomWinSocket);
          procedure ClientDisconnected(Sender : TObject; Socket : TCustomWinSocket);
          procedure DoClientRead(Sender : TObject; Socket : TCustomWinSocket);
          procedure HandleError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
          procedure RemoveQuery(Socket : TCustomWinSocket);
          function  GetStuckQueries : string;
      end;

  const
    MaxDebugServers = 5;

  var
    DebugServers : array[0..MaxDebugServers] of TWinSockRDOConnectionsServer;
    ServerCount  : integer = 0;

implementation

  uses
    SysUtils,
    WinSock,
    RDOProtocol,
    RDOUtils,
    {$IFDEF LogsEnabled}
    LogFile,
    {$ENDIF}
    ErrorCodes,
    RDOQueryServer,
    WinSockRDOServerClientConnection;

  const
    QUERY_COUNT_ENQUIRE = 'GetQueryCount';

  type
    PQueryToService = ^TQueryToService;
    TQueryToService =
      record
        Valid  : boolean;
        Query  : TRDOQuery;
        Socket : TCustomWinSocket;
      end;

  type
    PSocketData = ^TSocketData;
    TSocketData =
      record
        RDOConnection : IRDOConnection;
        Stream        : TQueryStream;
      end;

  procedure ReleaseQueryToService(QueryToService : PQueryToService);
    begin
      if QueryToService <> nil
        then
          begin
            QueryToService.Query.Free;
            dispose(QueryToService);
          end;
    end;

  type
    TServicingQueryThread =
      class(TSmartThread)
        private
          fConnectionsServer : TWinSockRDOConnectionsServer;
          fLock              : TCriticalSection;
          fQueryToService    : PQueryToService;
          fStatus            : integer;
          fQueryStatus       : integer;
        public
          constructor Create(theConnServer : TWinSockRDOConnectionsServer);
          destructor  Destroy; override;
          procedure   Execute; override;
        public
          procedure Lock;
          procedure Unlock;
          procedure CheckQuery(Socket : TCustomWinSocket);
        public
          property TheLock : TCriticalSection read fLock;
      end;

  type
    TMsgLoopThread =
      class(TSmartThread)
        private
          fRDOConnServ : TWinSockRDOConnectionsServer;
        public
          constructor Create(RDOConnServ : TWinSockRDOConnectionsServer);
          procedure   Execute; override;
      end;

  // TServicingQueryThread

  constructor TServicingQueryThread.Create(theConnServer : TWinSockRDOConnectionsServer);
    begin
      inherited Create(true);
      fConnectionsServer := theConnServer;
      fLock := TCriticalSection.Create;
      Resume;
    end;

  destructor TServicingQueryThread.Destroy;
    begin
      Terminate;
      WaitFor;
      fLock.Free;
      inherited;
    end;

  procedure TServicingQueryThread.Execute;
    var
      Query             : TRDOQuery;
      QueryResult       : TRDOQuery;
      Sckt              : integer;
      QueryThreadEvents : array [1 .. 2] of THandle;
      WaitRes           : integer;
    begin
      with fConnectionsServer do
        begin
          QueryThreadEvents[1] := fQueryWaiting;
          QueryThreadEvents[2] := fTerminateEvent;
          while not Terminated do
            try
              fStatus := 1; {1}
              WaitRes := WaitForMultipleObjects(2, @QueryThreadEvents[1], false, INFINITE);
              fStatus := 2; {2}
              if not Terminated and (WaitRes = WAIT_OBJECT_0)
                then
                  begin
                    fQueryQueueLock.Acquire;
                    fStatus := 3; {3}
                    try
                      if fQueryQueue.Count <> 0
                        then
                          begin
                            fQueryToService := fQueryQueue[0];
                            fQueryQueue.Delete(0);
                          end
                        else
                          begin
                            fQueryToService := nil;
                            ResetEvent(fQueryWaiting);
                          end
                    finally
                      fQueryQueueLock.Release
                    end;

                    // Catch the query
                    fStatus := 4; {4}
                    Lock;
                    fStatus := 5; {5}
                    try
                      if fQueryToService <> nil
                        then
                          begin
                            // remove the query from the record
                            Query := fQueryToService.Query;
                            fQueryToService.Query := nil;
                            if fQueryToService.Valid
                              then Sckt := integer(fQueryToService.Socket)
                              else
                                begin
                                  Query.Free;
                                  Query := nil;
                                  Sckt  := 0;
                                end;
                          end
                        else
                          begin
                            Query := nil;
                            Sckt  := 0;
                          end;
                    finally
                      Unlock;
                    end;

                    fStatus := 6; {6}
                    // Check if there is something to excecute
                    if Query <> nil
                      then
                        begin
                          // Execute and release the query
                          try
                            try
                              QueryResult := fQueryServer.ExecQuery(Query, Sckt, fQueryStatus);
                            finally
                              Query.Free;
                            end;
                          except
                            QueryResult := nil;
                          end;

                          // Check result
                          if QueryResult = nil
                            then
                              begin
                                {$IFDEF LogsEnabled}
                                LogThis('No result');
                                {$ENDIF}
                              end
                            else
                              begin
                                // Check if can send result back
                                fStatus := 7; {7}
                                Lock;
                                try
                                  fStatus := 8; {8}
                                  if (fQueryToService <> nil) and fQueryToService.Valid and fQueryToService.Socket.Connected
                                    then
                                      try
                                        RDOUtils.SendQuery(QueryResult, fQueryToService.Socket);
                                        {$IFDEF LogsEnabled}
                                        LogThis('Result : ' + QueryResult.ToStr);
                                        {$ENDIF}
                                      except
                                        {$IFDEF LogsEnabled}
                                        LogThis('Error sending query result')
                                        {$ENDIF}
                                      end;
                                finally
                                  Unlock;
                                  QueryResult.Free;
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
                                  ReleaseQueryToService(fQueryToService);
                                  fQueryToService := nil;
                                end
                          finally
                            Unlock;
                          end;
                        end;
                  end;
            except
              {$IFDEF LogsEnabled}
              LogThis('Un handled exception in the Servicing Query Thread loop!')
              {$ENDIF}
            end;
        end
    end;

  procedure TServicingQueryThread.Lock;
    begin
      fLock.Enter;
    end;

  procedure TServicingQueryThread.Unlock;
    begin
      fLock.Leave;
    end;

  procedure TServicingQueryThread.CheckQuery(Socket : TCustomWinSocket);
    begin
      Lock;
      try
        if (fQueryToService <> nil) and (fQueryToService.Socket = Socket)
          then fQueryToService.Valid := false;
      finally
        Unlock;
      end;
    end;

  // TMsgLoopThread

  constructor TMsgLoopThread.Create(RDOConnServ : TWinSockRDOConnectionsServer);
    begin
      inherited Create(true);
      fRDOConnServ := RDOConnServ;
      Priority     := tpHigher;
      Resume;
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
                then Active := true
          except
            {$IFDEF LogsEnabled}
            LogThis('Error establishing connection')
            {$ENDIF}
          end;
          while not Terminated do
            if PeekMessage(Msg, 0, 0, 0, PM_REMOVE)
              then
                try
                  DispatchMessage(Msg)
                except
                {$IFDEF LogsEnabled}
                  on E : Exception do
                    LogThis('Loop thread error: '  + E.Message);
                {$ENDIF}
                end
              else MsgWaitForMultipleObjects(1, fTerminateEvent, false, INFINITE, QS_ALLINPUT);
          with fSocketComponent do
            Active := false
        end
    end;

  // TWinSockRDOConnectionsServer

  constructor TWinSockRDOConnectionsServer.Create(Prt : integer);
    begin
      inherited Create;
      fSocketComponent := TServerSocket.Create(nil);
      with fSocketComponent do
        begin
          Active := false;
          Port := Prt;
          OnClientConnect := ClientConnected;
          OnClientDisconnect := ClientDisconnected;
          OnClientRead := DoClientRead;
          OnClientError := HandleError
        end;
      fQueryQueue := TList.Create;
      fQueryThreads := TList.Create;
      fQueryWaiting := CreateEvent(nil, true, false, nil);
      fQueryQueueLock := TCriticalSection.Create;
      fTerminateEvent := CreateEvent(nil, true, false, nil);
      // Temporary debug patch..
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
            Query := PQueryToService(fQueryQueue[QueryIdx]);
            dispose(Query);
          end;
        fQueryQueue.Free
      end;

    begin
      StopListening;
      fSocketComponent.Free;
      FreeQueryQueue;
      CloseHandle(fQueryWaiting);
      CloseHandle(fTerminateEvent);
      fQueryQueueLock.Free;                   
      inherited Destroy
    end;
                             
  procedure TWinSockRDOConnectionsServer.SetQueryServer(const QueryServer : IRDOQueryServer);
    begin
      fQueryServer := QueryServer
    end;

  function TWinSockRDOConnectionsServer.GetMaxQueryThreads : integer;
    begin
      Result := fMaxQueryThreads
    end;

  procedure TWinSockRDOConnectionsServer.SetMaxQueryThreads(MaxQueryThreads : integer);
    begin
      fMaxQueryThreads := MaxQueryThreads
    end;

  procedure TWinSockRDOConnectionsServer.StartListening;
    var
      ThreadIdx : integer;
    begin
      ResetEvent(fTerminateEvent);
      fMsgLoopThread := TMsgLoopThread.Create(Self);
      for ThreadIdx := 1 to fMaxQueryThreads do
        fQueryThreads.Add(TServicingQueryThread.Create(Self))
    end;

  procedure TWinSockRDOConnectionsServer.StopListening;

    procedure FreeQueryThreads;
      var
        ThreadIdx    : integer;
        aQueryThread : TSmartThread;
      begin
        for ThreadIdx := 0 to fQueryThreads.Count - 1 do
          begin
            aQueryThread := TSmartThread(fQueryThreads[ThreadIdx]);
            aQueryThread.Free
          end;
        fQueryThreads.Free;
        fQueryThreads := nil
      end;

    begin
      SetEvent(fTerminateEvent);
      if fMsgLoopThread <> nil
       then
         begin
           fMsgLoopThread.Free;
           fMsgLoopThread := nil
         end;
      if fQueryThreads <> nil
        then
          FreeQueryThreads
    end;

  function TWinSockRDOConnectionsServer.GetClientConnection(const ClientAddress : string; ClientPort : integer) : IRDOConnection;
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
            if inet_addr(PChar(ClientAddress)) = u_long(INADDR_NONE)
              then
                UseIPAddr := false
              else
                UseIPAddr := true;
            while (ConnIdx < ConnCount) and (FoundConn = nil) do
              begin
                CurrConn := Socket.Connections[ConnIdx];
                if UseIPAddr
                  then
                    CurConnRmtAddr := CurrConn.RemoteAddress
                  else
                    CurConnRmtAddr := CurrConn.RemoteHost;
                if (CurConnRmtAddr = ClientAddress)  and (CurrConn.RemotePort = ClientPort)
                  then
                    FoundConn := PSocketData(CurrConn.Data).RDOConnection;
                inc(ConnIdx)
              end
          finally
            Socket.Unlock;
          end;
        end;
      Result := FoundConn
    end;

  function TWinSockRDOConnectionsServer.GetClientConnectionById(Id : integer) : IRDOConnection;
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
            while (ConnIdx < ConnCount) and (FoundConn = nil) do
              begin
                CurrConn := Socket.Connections[ConnIdx];
                if integer(CurrConn) = Id
                  then FoundConn := PSocketData(CurrConn.Data).RDOConnection
                  else inc(ConnIdx);
              end;
          finally
            Socket.Unlock;
          end;
        end;
      Result := FoundConn
    end;

  procedure TWinSockRDOConnectionsServer.InitEvents(const EventSink : IRDOConnectionServerEvents);
    begin
      fEventSink := EventSink
    end;

  procedure TWinSockRDOConnectionsServer.ClientConnected(Sender : TObject; Socket : TCustomWinSocket);
    var
      SocketData : PSocketData;
    begin
      New(SocketData);
      SocketData.RDOConnection := TWinSockRDOServerClientConnection.Create(Socket);
      SocketData.Stream        := TQueryStream.Create;
      Socket.Data              := SocketData;
      if fEventSink <> nil
        then
          fEventSink.OnClientConnect(SocketData.RDOConnection)
    end;

  procedure TWinSockRDOConnectionsServer.ClientDisconnected(Sender : TObject; Socket : TCustomWinSocket);
    var
      SocketData : PSocketData;
    begin
      SocketData := PSocketData(Socket.Data);
      if fEventSink <> nil
        then fEventSink.OnClientDisconnect(SocketData.RDOConnection);
      if Assigned(SocketData.RDOConnection.OnDisconnect)
        then SocketData.RDOConnection.OnDisconnect(SocketData.RDOConnection);
      {$IFDEF LogsEnabled}
      LogThis('RemoveQuery start..');
      {$ENDIF}
      RemoveQuery(Socket);
      {$IFDEF LogsEnabled}
      LogThis('RemoveQuery end');
      {$ENDIF}
      SocketData.Stream.Free;
      SocketData.RDOConnection := nil;
      dispose(SocketData);
    end;

  procedure TWinSockRDOConnectionsServer.DoClientRead(Sender : TObject; Socket : TCustomWinSocket);
    var
      QueryToService : PQueryToService;
      ReadError      : boolean;
      SocketData     : PSocketData;
      ServClienConn  : IRDOServerClientConnection;
      Query          : TRDOQuery;
      QueryReady     : boolean;
    begin
      SocketData := PSocketData(Socket.Data);
      try
        QueryReady := SocketData.Stream.Receive(Socket);
        ReadError  := false;
      except
        QueryReady := false;
        ReadError  := true;
        Socket.Close; // >> ???
      end;
      if not ReadError and (fQueryServer <> nil)
        then
          begin
            if QueryReady
              then
                begin
                  SocketData.Stream.Position := 0;
                  Query := TRDOQuery.Read(SocketData.Stream);
                  SocketData.Stream.Clear;
                  if (Query.QKind <> qkAnswer) and (Query.QKind <> qkError)
                    then
                      begin
                        new(QueryToService);
                        QueryToService.Query  := Query;
                        QueryToService.Socket := Socket;
                        QueryToService.Valid  := true;
                        fQueryQueueLock.Enter;
                        try
                          if Query.Name <> QUERY_COUNT_ENQUIRE
                            then
                              begin
                                if not fQueryServer.Busy
                                  then
                                    begin
                                      fQueryQueue.Add(QueryToService);
                                      if fQueryQueue.Count > 0
                                        then SetEvent(fQueryWaiting);
                                    end
                                  else
                                    begin
                                      Query := TRDOQuery.Create(qkError, Query.Id);
                                      Query.PushParam(integer(errServerBusy));
                                      ReleaseQueryToService(QueryToService);
                                      try
                                        RDOUtils.SendQuery(Query, Socket);
                                      finally
                                        Query.Free;
                                      end;
                                    end;
                              end
                            else
                              begin
                                ReleaseQueryToService(QueryToService);
                                Query := TRDOQuery.Create(qkAnswer, Query.Id);
                                Query.PushParam(GetStuckQueries);
                                try
                                  RDOUtils.SendQuery(Query, Socket);
                                finally
                                  Query.Free;
                                end;
                              end;
                        finally
                          fQueryQueueLock.Release
                        end
                      end
                    else
                      begin
                        ServClienConn := SocketData.RDOConnection as IRDOServerClientConnection;
                        ServClienConn.OnQueryResultArrival(Query);
                      end;
                end;
          end
        else
          {$IFDEF LogsEnabled}
          LogThis('Error while reading from socket');
          {$ENDIF}
    end;

  procedure TWinSockRDOConnectionsServer.HandleError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : Integer);
    begin
      ErrorCode := 0;
      {$IFDEF LogsEnabled}
      case ErrorEvent of
        eeGeneral:
          LogThis('General socket error');
        eeSend:
          LogThis('Error writing to socket');
        eeReceive:
          LogThis('Error reading from socket');
        eeConnect:
          LogThis('Error establishing connection');
        eeDisconnect:
          LogThis('Error closing connection');
        eeAccept:
          LogThis('Error accepting connection')
      end
      {$ENDIF}
    end;

  procedure TWinSockRDOConnectionsServer.RemoveQuery(Socket : TCustomWinSocket);
    var
      i     : integer;
      Query : PQueryToService;
    begin
      try
        fQueryQueueLock.Enter;
        try
          i := 0;
          while i < fQueryQueue.Count do
            begin
              Query := PQueryToService(fQueryQueue[i]);
              if Query.Socket = Socket
                then
                  begin
                    ReleaseQueryToService(Query);
                    fQueryQueue.Delete(i);
                  end
                else inc(i);
            end;
        finally
          fQueryQueueLock.Leave;
        end;
        // Check Query Threads
        for i := 0 to pred(fQueryThreads.Count) do
          TServicingQueryThread(fQueryThreads[i]).CheckQuery(Socket);
      except
      end;
    end;

  function TWinSockRDOConnectionsServer.GetStuckQueries : string;
    var
      i      : integer;
      Q      : PQueryToService;
      Thread : TServicingQueryThread;
      status : string;
    begin
      result := '';
      for i := 0 to pred(fQueryThreads.Count) do
        try
          Thread := TServicingQueryThread(fQueryThreads[i]);
          status := IntToStr(Thread.fStatus) + ',' + IntToStr(Thread.fQueryStatus);
          Q      := Thread.fQueryToService;
          if (Q <> nil) and (Q.Query <> nil)
            then result := result + IntToStr(i) + '(' + status + '):' + Q.Query.ToStr + ' '
            else result := result + IntToStr(i) + '(' + status + '): None ';
        except
        end;
    end;

initialization

  FillChar(DebugServers, sizeof(DebugServers), 0);

end.
