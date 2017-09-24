unit WinSockRDOConnection;

interface

  uses
    SmartThreads,
    Classes,
    ComObj,
    Windows,
    {$IFDEF AutoServer}
    RDOClient_TLB,
    {$ENDIF}
    RDOInterfaces,
    SocketComp,
    SyncObjs,
    RDOQueries;

  type
    TWinSockRDOConnection =
      {$IFDEF AutoServer}
      class(TAutoObject, IRDOConnectionInit, IRDOServerConnection, IRDOConnection)
      {$ELSE}
      class(TInterfacedObject, IRDOConnectionInit, IRDOServerConnection, IRDOConnection)
      {$ENDIF}
        private
          fConnected          : boolean;
          fPort               : integer;
          fServer             : string;
          fUnsentQueries      : TList;
          fSentQueries        : TList;
          fSenderThread       : TSmartThread;
          fMsgLoopThread      : TSmartThread;
          fUnsentQueriesLock  : TCriticalSection;
          fSentQueriesLock    : TCriticalSection;
          fSocketComponent    : TClientSocket;
          fReceivedData       : TQueryStream;
          fConnectionEvent    : THandle;
          fUnsentQueryWaiting : THandle;
          fQueryServer        : IRDOQueryServer;
          fQueryQueue         : TList;
          fQueryWaiting       : THandle;
          fQueryQueueLock     : TCriticalSection;
          fQueryThreads       : TList;
          fMaxQueryThreads    : integer;
          fTerminateEvent     : THandle;
          fOnConnect          : TRDOClientConnectEvent;
          fOnDisconnect       : TRDOClientDisconnectEvent;
          procedure DoRead(Sender : TObject; Socket : TCustomWinSocket);
          procedure HandleError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
          procedure HandleConnect(Sender : TObject; Socket : TCustomWinSocket);
          procedure HandleDisconnect(Sender : TObject; Socket : TCustomWinSocket);
        protected
          function  Get_Server : WideString; safecall;
          procedure Set_Server(const Value : WideString); safecall;
          function  Get_Port : Integer; safecall;
          procedure Set_Port(Value : Integer); safecall;
          function  Connect(TimeOut : Integer) : WordBool; safecall;
          procedure Disconnect; safecall;
        protected
          procedure SetQueryServer(const QueryServer : IRDOQueryServer);
          function  GetMaxQueryThreads : integer;
          procedure SetMaxQueryThreads(MaxQueryThreads : integer);
        public
          function  Alive : boolean;
          function  SendReceive(Query : TRDOQuery; out ErrorCode : integer; TimeOut : integer) : TRDOQuery; stdcall;
          procedure Send(Query : TRDOQuery); stdcall;
          function  GetLocalAddress : string; stdcall;
          function  GetLocalHost : string; stdcall;
          function  GetLocalPort : integer; stdcall;
          function  GetOnConnect : TRDOClientConnectEvent;
          procedure SetOnConnect(OnConnectHandler : TRDOClientConnectEvent);
          function  GetOnDisconnect : TRDOClientDisconnectEvent;
          procedure SetOnDisconnect(OnDisconnectHandler : TRDOClientDisconnectEvent);
          {$IFDEF AutoServer}
        protected
          procedure Initialize; override;
          {$ELSE}
        public
          constructor Create;
          {$ENDIF}
          destructor Destroy; override;
      end;

implementation

  uses
    {$IFDEF AutoServer}
    ComServ,
    {$ENDIF}
    SysUtils,
    WinSock,
    RDOUtils,
    RDOProtocol,
    {$IFDEF Logs}
    LogFile,
    {$ENDIF}
    ErrorCodes;

  // Query id generation routines and variables

  const
    DefRDOPort = 5000;

  const
    DefMaxQueryThreads = 5;

  var
    LastQueryId   : word;

  function GenerateQueryId : integer;
    begin
      Result := LastQueryId;
      LastQueryId := (LastQueryId + 1) mod 65536
    end;

  type
    PQueryToSend = ^TQueryToSend;
    TQueryToSend =
      record
        Query       : TRDOQuery;
        WaitForAnsw : boolean;
        Result      : TRDOQuery;
        Event       : THandle;
        ErrorCode   : integer;
      end;


  // Delphi classes associated to the threads in charge of the connection

  type
    TSenderThread =
      class(TSmartThread)
        private
          fConnection : TWinSockRDOConnection;
          constructor Create(theConnection : TWinSockRDOConnection);
        protected
          procedure Execute; override;
      end;

  type
    TMsgLoopThread =
      class(TSmartThread)
        private
          fConnection : TWinSockRDOConnection;
        public
          constructor Create(theConnection : TWinSockRDOConnection);
        protected
          procedure Execute; override;
        public
          property Connection : TWinSockRDOConnection read fConnection write fConnection;
        private
          procedure TerminateLoop(Sender : TObject);
      end;

  type
    TServicingQueryThread =
      class(TSmartThread)
        private
          fConnection  : TWinSockRDOConnection;
          fQueryStatus : integer;
        public
          constructor Create(theConnection : TWinSockRDOConnection);
          procedure   Execute; override;
      end;

  // TSenderThread

  constructor TSenderThread.Create(theConnection : TWinSockRDOConnection);
    begin
      inherited Create(true);
      fConnection := theConnection;
      Resume;
    end;

  procedure TSenderThread.Execute;
    var
      QueryToSend        : PQueryToSend;
      SenderThreadEvents : array [ 1 .. 2 ] of THandle;
    begin
      with fConnection do
        begin
          SenderThreadEvents[ 1 ] := fUnsentQueryWaiting;
          SenderThreadEvents[ 2 ] := fTerminateEvent;
          while not Terminated do
            begin
              WaitForMultipleObjects(2, @SenderThreadEvents[ 1 ], false, INFINITE);
              if not Terminated
                then
                  begin
                    fUnsentQueriesLock.Acquire;
                    try
                      if fUnsentQueries.Count <> 0
                        then
                          begin
                            QueryToSend := fUnsentQueries[ 0 ];
                            fUnsentQueries.Delete(0)
                          end
                        else
                          begin
                            QueryToSend := nil;
                            ResetEvent(fUnsentQueryWaiting)
                          end
                    finally
                      fUnsentQueriesLock.Release
                    end;
                  end
                else QueryToSend := nil;
              if QueryToSend <> nil
                then
                  if QueryToSend.WaitForAnsw
                    then
                      begin
                        fSentQueriesLock.Acquire;
                        try
                          fSentQueries.Add(QueryToSend)
                        finally
                          fSentQueriesLock.Release
                        end;
                        try
                          RDOUtils.SendQuery(QueryToSend.Query, fSocketComponent.Socket);
                        except
                          {$IFDEF Logs}
                          LogThis('Error sending query');
                          {$ENDIF}
                          fSentQueriesLock.Acquire;
                          try
                            fSentQueries.Remove(QueryToSend)
                          finally
                            fSentQueriesLock.Release
                          end;
                          QueryToSend.ErrorCode := errSendError;
                          SetEvent(QueryToSend.Event)
                        end
                      end
                    else
                      begin
                        try
                          RDOUtils.SendQuery(QueryToSend.Query, fSocketComponent.Socket);
                        except
                          {$IFDEF Logs}
                          LogThis('Error sending query');
                          {$ENDIF}
                        end;
                        QueryToSend.Query.Free;
                        Dispose(QueryToSend);
                      end;
            end
        end
    end;

  // TMsgLoopThread

  constructor TMsgLoopThread.Create(theConnection : TWinSockRDOConnection);
    begin
      inherited Create(true);
      fConnection     := theConnection;
      FreeOnTerminate := true;
      OnTerminate     := TerminateLoop;
      Resume;
    end;

  procedure TMsgLoopThread.Execute;
    var
      Msg : TMsg;
    begin
      try
        try
          fConnection.fSocketComponent.Open;
        except
          Terminate;
          {$IFDEF Logs}
          LogThis('Error establishing connection')
          {$ENDIF}
        end;
        while not Terminated and (fConnection <> nil) do
          if PeekMessage(Msg, 0, 0, 0, PM_REMOVE)
            then
              try
                DispatchMessage(Msg);
              except
                // aguanta como un macho!!!
              end
            else
              if fConnection <> nil
                then MsgWaitForMultipleObjects(1, fConnection.fTerminateEvent, false, INFINITE, QS_ALLINPUT)
      except
        // aguanta como un macho!!!
      end;
    end;

  procedure TMsgLoopThread.TerminateLoop(Sender : TObject);
    begin
      if fConnection <> nil
        then
          begin
            fConnection.fMsgLoopThread := nil;
            fConnection := nil;
          end;
    end;

  // TServicingQueryThread

  constructor TServicingQueryThread.Create(theConnection : TWinSockRDOConnection);
    begin
      fConnection := theConnection;
      inherited Create(false)
    end;

  procedure TServicingQueryThread.Execute;
    var
      QueryResult       : TRDOQuery;
      QueryToService    : TRDOQuery;
      QueryThreadEvents : array [ 1 .. 2 ] of THandle;
    begin
      with fConnection do
        begin
          QueryThreadEvents[ 1 ] := fQueryWaiting;
          QueryThreadEvents[ 2 ] := fTerminateEvent;
          while not Terminated do
            begin
              WaitForMultipleObjects(2, @QueryThreadEvents[ 1 ], false, INFINITE);
              if not Terminated
                then
                  begin
                    fQueryQueueLock.Acquire;
                    try
                      if fQueryQueue.Count <> 0
                        then
                          begin
                            QueryToService := fQueryQueue[ 0 ];
                            fQueryQueue.Delete(0)
                          end
                        else
                          begin
                            QueryToService := nil;
                            ResetEvent(fQueryWaiting)
                          end
                    finally
                      fQueryQueueLock.Release
                    end;
                  end
                else QueryToService := nil;
              if QueryToService <> nil
                then
                  begin
                    try
                      QueryResult := fQueryServer.ExecQuery(QueryToService, integer(fSocketComponent.Socket), fQueryStatus);
                    finally
                      QueryToService.Free;
                    end;
                    if QueryResult <> nil
                      then
                        try
                          try
                            RDOUtils.SendQuery(QueryResult, fSocketComponent.Socket);
                          finally
                            QueryResult.Free;
                          end;
                          {$IFDEF Logs}
                          LogThis('Result : ' + QueryResult.ToStr);
                          {$ENDIF}
                        except
                          {$IFDEF Logs}
                          LogThis('Error sending query result')
                          {$ENDIF}
                        end
                      else
                        begin
                          {$IFDEF Logs}
                          LogThis('No result')
                          {$ENDIF}
                        end
                  end
            end
        end
    end;

  // TWinSockRDOConnection

  {$IFDEF AutoServer}
  procedure TWinSockRDOConnection.Initialize;
  {$ELSE}
  constructor TWinSockRDOConnection.Create;
  {$ENDIF}
    begin
      inherited;
      fSocketComponent := TClientSocket.Create(nil);
      fSocketComponent.Active := false;
      fUnsentQueriesLock := TCriticalSection.Create;
      fSentQueriesLock := TCriticalSection.Create;
      fUnsentQueries := TList.Create;
      fSentQueries := TList.Create;
      fPort := DefRDOPort;
      fUnsentQueryWaiting := CreateEvent(nil, true, false, nil);
      fQueryQueue := TList.Create;
      //fQueryThreads := TList.Create;
      //fMaxQueryThreads := DefMaxQueryThreads;
      fQueryWaiting := CreateEvent(nil, true, false, nil);
      fQueryQueueLock := TCriticalSection.Create;
      fTerminateEvent := CreateEvent(nil, true, false, nil);
      fReceivedData   := TQueryStream.Create;
    end;

  destructor TWinSockRDOConnection.Destroy;

    procedure FreeQueryQueue;
      var
        QueryIdx : integer;
      begin
        for QueryIdx := 0 to fQueryQueue.Count - 1 do
          TRDOQuery(fQueryQueue[QueryIdx]).Free;
        fQueryQueue.Free;
      end;

    begin
      Disconnect;
      fUnsentQueriesLock.Free;
      fSentQueriesLock.Free;
      fSocketComponent.Free;
      fUnsentQueries.Free;
      fSentQueries.Free;
      CloseHandle(fUnsentQueryWaiting);
      FreeQueryQueue;
      CloseHandle(fQueryWaiting);
      fQueryQueueLock.Free;
      CloseHandle(fTerminateEvent);
      inherited
    end;

  function TWinSockRDOConnection.Get_Server : WideString;
    begin
      Result := fServer
    end;

  procedure TWinSockRDOConnection.Set_Server(const Value : WideString);
    begin
      fServer := Value
    end;

  function TWinSockRDOConnection.Get_Port : Integer;
    begin
      Result := fPort
    end;

  procedure TWinSockRDOConnection.Set_Port(Value : Integer);
    begin
      fPort := Value
    end;

  function TWinSockRDOConnection.GetLocalHost : string;
    begin
      Result := fSocketComponent.Socket.LocalHost
    end;

  function TWinSockRDOConnection.GetLocalAddress : string;
    begin
      Result := fSocketComponent.Socket.LocalAddress
    end;

  function TWinSockRDOConnection.GetLocalPort : integer;
    begin
      Result := fSocketComponent.Socket.LocalPort
    end;

  function TWinSockRDOConnection.GetOnConnect : TRDOClientConnectEvent;
    begin
      result := fOnConnect;
    end;

  procedure TWinSockRDOConnection.SetOnConnect(OnConnectHandler : TRDOClientConnectEvent);
    begin
      fOnConnect := OnConnectHandler
    end;

  function TWinSockRDOConnection.GetOnDisconnect : TRDOClientDisconnectEvent;
    begin
      result := fOnDisconnect;
    end;

  procedure TWinSockRDOConnection.SetOnDisconnect(OnDisconnectHandler : TRDOClientDisconnectEvent);
    begin
      fOnDisconnect := OnDisconnectHandler
    end;

  function TWinSockRDOConnection.Connect(TimeOut : Integer) : WordBool;
    var
      WaitRes : cardinal;
    begin
      Result := true;
      try
        ResetEvent(fTerminateEvent);
        with fSocketComponent do
          if not Active
            then
              begin
                //ClientType := ctNonBlocking;
                if inet_addr(PChar(fServer)) = u_long(INADDR_NONE) // >> Delphi 4
                  then Host := fServer
                  else Address := fServer;
                Port := fPort;
                OnRead := DoRead;
                OnError := HandleError;
                OnConnect := HandleConnect;
                OnDisconnect := HandleDisconnect;
                fConnectionEvent := CreateEvent(nil, false, false, nil);
                try
                  fMsgLoopThread := TMsgLoopThread.Create(Self);
                  WaitRes := WaitForSingleObject(fConnectionEvent, TimeOut);
                  if WaitRes = WAIT_OBJECT_0
                    then
                      begin
                        fSenderThread := TSenderThread.Create(Self);
                        SetMaxQueryThreads(fMaxQueryThreads);
                      end
                    else
                      begin
                        fMsgLoopThread.Terminate;
                        SetEvent(fTerminateEvent);
                        fMsgLoopThread := nil
                      end;
                  Result := WaitRes = WAIT_OBJECT_0
                finally
                  CloseHandle(fConnectionEvent)
                end
              end
      except
        Disconnect;
        Result := false
      end
    end;

  procedure TWinSockRDOConnection.Disconnect;

    procedure FreeQueryThreads;
      var
        ThreadIdx    : integer;
        aQueryThread : TSmartThread;
      begin
        for ThreadIdx := 0 to fQueryThreads.Count - 1 do
          begin
            aQueryThread := TSmartThread(fQueryThreads[ ThreadIdx ]);
            aQueryThread.Free
          end;
        fQueryThreads.Free;
      end;

    begin
      if fMsgLoopThread <> nil
        then
          begin
            TMsgLoopThread(fMsgLoopThread).fConnection := nil;
            fMsgLoopThread := nil
          end;
      SetEvent(fTerminateEvent);
      if fSocketComponent.Active
        then
          try
            fSocketComponent.Close;
          except
            {$IFDEF Logs}
            LogThis('Error closing connection')
            {$ENDIF}
          end;
      if fSenderThread <> nil
        then
          begin
            fSenderThread.Free;
            fSenderThread := nil
          end;
      if fQueryThreads <> nil
        then
          begin
            FreeQueryThreads;
            fQueryThreads := nil
          end;
    end;

  procedure TWinSockRDOConnection.SetQueryServer(const QueryServer : IRDOQueryServer);
    begin
      fQueryServer := QueryServer
    end;

  function TWinSockRDOConnection.GetMaxQueryThreads : integer;
    begin
      Result := fMaxQueryThreads
    end;

  procedure TWinSockRDOConnection.SetMaxQueryThreads(MaxQueryThreads : integer);
    var
      i : integer;
    begin
      if fConnected
        then
          begin
            if (fQueryThreads = nil) and (MaxQueryThreads > 0)
              then
                begin
                  fMaxQueryThreads := MaxQueryThreads;
                  fQueryThreads := TList.Create;
                  for i := 1 to fMaxQueryThreads do
                    fQueryThreads.Add(TServicingQueryThread.Create(Self));
                end;
          end
        else fMaxQueryThreads := MaxQueryThreads;
    end;

  function TWinSockRDOConnection.Alive : boolean;
    begin
      result := fConnected;
    end;

  function TWinSockRDOConnection.SendReceive(Query : TRDOQuery; out ErrorCode : integer; TimeOut : integer) : TRDOQuery;
    var
      theQuery : PQueryToSend;
      Events   : array [0..1] of THandle;
    begin
      result := nil;
      if fConnected
        then
          try
            New(theQuery);
            try
              Query.Id := GenerateQueryId;
              theQuery.Query := Query;
              {$IFDEF Logs}
              LogThis('Sending and waiting: ' + Query.ToStr);
              {$ENDIF}
              theQuery.WaitForAnsw := true;
              theQuery.Result := nil;
              theQuery.Event := CreateEvent(nil, false, false, nil);
              try
                theQuery.ErrorCode := errNoError;
                try
                  fUnsentQueriesLock.Acquire;
                  try
                    fUnsentQueries.Add(theQuery);
                    if fUnsentQueries.Count = 1
                      then SetEvent(fUnsentQueryWaiting);
                  finally
                    fUnsentQueriesLock.Release;
                  end;
                  Events[0] := theQuery.Event;
                  Events[1] := fTerminateEvent;
                  case WaitForMultipleObjects(2, @Events[0], false, TimeOut) of
                    WAIT_OBJECT_0 :
                      begin
                        result    := theQuery.Result;
                        ErrorCode := theQuery.ErrorCode;
                        {$IFDEF Logs}
                        LogThis('Result : ' + result.ToStr)
                        {$ENDIF}
                      end;
                    WAIT_OBJECT_0 + 1 :
                      ErrorCode := errQueryTimedOut;
                    else // WAIT_TIMEOUT
                      begin
                        {$IFDEF Logs}
                        LogThis('Query timed out');
                        {$ENDIF}
                        ErrorCode := errQueryTimedOut;
                      end;
                  end;
                except
                  ErrorCode := errQueryQueueOverflow
                end;
              finally
                // remove the query
                fSentQueriesLock.Acquire;
                try
                  fSentQueries.Remove(theQuery);
                  CloseHandle(theQuery.Event);
                finally
                  fSentQueriesLock.Release
                end;
              end;
            finally
              dispose(theQuery);
            end;
          except
            result    := nil;
            ErrorCode := errUnknownError
          end
        else result := nil;
    end;

  procedure TWinSockRDOConnection.Send(Query : TRDOQuery);
    var
      theQuery : PQueryToSend;
    begin
      if fConnected
        then
          begin
            new(theQuery);
            theQuery.Query := Query;
            {$IFDEF Logs}
            LogThis('Sending : ' + Query.ToStr);
            {$ENDIF}
            theQuery.WaitForAnsw := false;
            fUnsentQueriesLock.Acquire;
            try
              try
                fUnsentQueries.Add(theQuery);
                if fUnsentQueries.Count = 1
                  then SetEvent(fUnsentQueryWaiting);
              except
                Query.Free;
                dispose(theQuery);
              end
            finally
              fUnsentQueriesLock.Release
            end
          end;
    end;

  procedure TWinSockRDOConnection.DoRead(Sender : TObject; Socket : TCustomWinSocket);

    function FindServicedQuery(Query : TRDOQuery) : PQueryToSend;
      var
        QueryIdx      : integer;
        SentQueries   : integer;
      begin
        QueryIdx := 0;
        SentQueries := fSentQueries.Count;
        while (QueryIdx < SentQueries) and (Query.Id <> PQueryToSend(fSentQueries[ QueryIdx ]).Query.Id) do
          inc(QueryIdx);
        if QueryIdx < SentQueries
          then result := fSentQueries[QueryIdx]
          else result := nil;
      end;

    var
      ServicedQuery  : PQueryToSend;
      Query          : TRDOQuery;
      flag           : boolean;

    begin
      flag := false;
      try
        flag := fReceivedData.Receive(Socket);
      except
      {$IFDEF Logs}
        on E : ERDOCorruptQuery do
          LogThis('Invalid query size found');
        else
          LogThis('Unknow error reading from socket');
      {$ENDIF}
      end;
      if flag
        then
          begin
            fReceivedData.Position := 0;
            Query := TRDOQuery.Read(fReceivedData);
            fReceivedData.Clear;
            if Query.QKind = qkAnswer
              then
                begin
                  ServicedQuery := FindServicedQuery(Query);
                  fSentQueriesLock.Acquire;
                  try
                    if ServicedQuery <> nil
                      then
                        begin
                          ServicedQuery.Result := Query;
                          fSentQueries.Remove(ServicedQuery);
                          ServicedQuery.ErrorCode := errNoError;
                          SetEvent(ServicedQuery.Event);
                        end
                      else Query.Free;
                  finally
                    // parche
                    fSentQueriesLock.Release;
                    //if fSentQueriesLock <> nil
                      //then fSentQueriesLock.Release;
                  end;
                end
              else
                if fQueryServer <> nil
                  then
                    begin
                      fQueryQueueLock.Acquire;
                      try
                        fQueryQueue.Add(Query);
                        if fQueryQueue.Count = 1
                          then SetEvent(fQueryWaiting);
                      finally
                        fQueryQueueLock.Release
                      end
                    end
                  else Query.Free; // >> should reply with an error code
          end;
    end;

  procedure TWinSockRDOConnection.HandleError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
    begin
      case ErrorEvent of
        {$IFDEF Logs}
        eeGeneral:
          LogThis('General socket error');
        eeSend:
          LogThis('Error writing to socket');
        eeReceive:
          LogThis('Error reading from socket');
        {$ENDIF}
        eeConnect:
          begin
            ErrorCode := 0;
            {$IFDEF Logs}
            LogThis('Error establishing connection')
            {$ENDIF}
          end;
        eeDisconnect:
          begin
            ErrorCode := 0;
            {$IFDEF Logs}
            LogThis('Error closing connection')
            {$ENDIF}
          end;
        {$IFDEF Logs}
        eeAccept:
          LogThis('Error accepting connection')
        {$ENDIF}
      end
    end;

  procedure TWinSockRDOConnection.HandleConnect(Sender : TObject; Socket : TCustomWinSocket);
    begin
      fConnected := true;
      SetEvent(fConnectionEvent);
      if Assigned(fOnConnect)
        then fOnConnect(Self as IRDOConnection);
    end;

  procedure TWinSockRDOConnection.HandleDisconnect(Sender : TObject; Socket : TCustomWinSocket);
    begin
      fConnected := false;
      if Assigned(fOnDisconnect)
        then fOnDisconnect(Self as IRDOConnection);
      Disconnect;
    end;

initialization
  LastQueryId := 0;
  {$IFDEF AutoServer}
  TAutoObjectFactory.Create(ComServer, TWinSockRDOConnection, Class_WinSockRDOConnection, ciMultiInstance)
  {$ENDIF}
end.
