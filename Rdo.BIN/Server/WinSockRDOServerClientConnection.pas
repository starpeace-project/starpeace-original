unit WinSockRDOServerClientConnection;

interface

  uses
    SmartThreads,
    Classes,
    ComObj,
    Windows,
    RDOInterfaces,
    SocketComp,
    SyncObjs,
    RDOQueries;

  type
    TWinSockRDOServerClientConnection =
      class(TInterfacedObject, IRDOConnection, IRDOServerClientConnection)
        private
          fUnsentQueries       : TList;
          fSentQueries         : TList;
          fSenderThread        : TSmartThread;
          fUnsentQueriesLock   : TCriticalSection;
          fSentQueriesLock     : TCriticalSection;
          fSocket              : TCustomWinSocket;
          fUnsentQueryWaiting  : THandle;
          fTerminateEvent      : THandle;
          fOnConnect           : TRDOClientConnectEvent;
          fOnDisconnect        : TRDOClientDisconnectEvent;
        protected
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
        protected
          procedure OnQueryResultArrival(Query : TRDOQuery);
        private
          procedure ReleaseQueues;
        public
          constructor Create(Socket : TCustomWinSocket);
          destructor  Destroy; override;
      end;

implementation

  uses
    SysUtils,
    RDOUtils,
    RDOProtocol,
    {$IFDEF Logs}
    LogFile,
    {$ENDIF}
    ErrorCodes;

  // Query id generation routines and variables

  var
    LastQueryId : word = 0;

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
          fConnection : TWinSockRDOServerClientConnection;
          constructor Create(theConnection : TWinSockRDOServerClientConnection);
        protected
          procedure Execute; override;
      end;

  // TSenderThread

  constructor TSenderThread.Create(theConnection : TWinSockRDOServerClientConnection);
    begin
      fConnection := theConnection;
      inherited Create(false)
    end;

  procedure TSenderThread.Execute;
    var
      QueryToSend        : PQueryToSend;
      SenderThreadEvents : array [1..2] of THandle;
    begin
      with fConnection do
        begin
          SenderThreadEvents[1] := fUnsentQueryWaiting;
          SenderThreadEvents[2] := fTerminateEvent;
          while not Terminated do
            begin
              WaitForMultipleObjects(2, @SenderThreadEvents[1], false, INFINITE);
              if not Terminated
                then
                  begin
                    fUnsentQueriesLock.Acquire;
                    try
                      if fUnsentQueries.Count <> 0
                        then
                          begin
                            QueryToSend := fUnsentQueries[0];
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
                          if fSocket.Connected
                            then SendQuery(QueryToSend.Query, fSocket);
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
                          if fSocket.Connected
                            then SendQuery(QueryToSend.Query, fSocket);
                        except
                          {$IFDEF Logs}
                          LogThis('Error sending query');
                          {$ENDIF}
                        end;
                        QueryToSend.Query.Free;
                        dispose(QueryToSend);
                      end;
            end
        end
    end;

  // TWinSockRDOServerClientConnection

  constructor TWinSockRDOServerClientConnection.Create(Socket : TCustomWinSocket);
    begin
      inherited Create;
      fSocket := Socket;
      fUnSentQueriesLock := TCriticalSection.Create;
      fSentQueriesLock := TCriticalSection.Create;
      fUnsentQueries := TList.Create;
      fSentQueries := TList.Create;
      //fSenderThread := TSenderThread.Create(Self);
      fUnsentQueryWaiting := CreateEvent(nil, true, false, nil);
      fTerminateEvent := CreateEvent(nil, true, false, nil)
    end;

  destructor TWinSockRDOServerClientConnection.Destroy;
    begin
      SetEvent(fTerminateEvent);
      fSenderThread.Free;
      ReleaseQueues;
      fUnSentQueriesLock.Free;
      fSentQueriesLock.Free;
      fUnsentQueries.Free;
      fSentQueries.Free;
      CloseHandle(fUnsentQueryWaiting);
      CloseHandle(fTerminateEvent);
      inherited
    end;

  procedure TWinSockRDOServerClientConnection.ReleaseQueues;
    var
      i           : integer;
      QueryToSend : PQueryToSend;
    begin
      try
        fUnsentQueriesLock.Enter;
        try
          for i := 0 to pred(fUnsentQueries.Count) do
            TRDOQuery(fUnsentQueries[i]).Free;
          fUnsentQueries.Clear;
        finally
          fUnsentQueriesLock.Leave;
        end;
      except
      end;
      try
        fSentQueriesLock.Enter;
        try
          for i := 0 to pred(fSentQueries.Count) do
            begin
              QueryToSend := PQueryToSend(fUnsentQueries[i]);
              QueryToSend.Query.Free;
              QueryToSend.Result.Free;
              dispose(QueryToSend);
            end;
          fSentQueries.Clear;
        finally
          fSentQueriesLock.Leave;
        end;
      except
      end;
    end;

  function TWinSockRDOServerClientConnection.Alive : boolean;
    begin
      result := fSocket.Connected;
    end;

  function TWinSockRDOServerClientConnection.SendReceive(Query : TRDOQuery; out ErrorCode : integer; TimeOut : integer) : TRDOQuery;
    var
      theQuery : PQueryToSend;
      Events   : array [1..2] of THandle;
    begin
      result := nil;
      try
        new(theQuery);
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
                // create the thread when needed
                if fSenderThread = nil
                  then fSenderThread := TSenderThread.Create(self);
              finally
                fUnsentQueriesLock.Release;
              end;
              Events[1] := theQuery.Event;
              Events[2] := fTerminateEvent;
              case WaitForMultipleObjects(2, @Events[1], false, TimeOut) of
                WAIT_OBJECT_0 :
                  begin
                    result    := theQuery.Result;
                    ErrorCode := theQuery.ErrorCode;
                    {$IFDEF Logs}
                    if result <> nil
                      then LogThis('Result : ' +  result.ToStr)
                      else LogThis('Result : Unknown');
                    {$ENDIF}
                  end;
                WAIT_OBJECT_0 + 1 :
                  ErrorCode := errQueryTimedOut;
                else // WAIT_TIMEOUT :
                  begin
                    {$IFDEF Logs}
                    LogThis('Query timed out');
                    {$ENDIF}
                    ErrorCode := errQueryTimedOut;
                    fSentQueriesLock.Acquire;
                    try
                      fSentQueries.Remove(theQuery)
                    finally
                      fSentQueriesLock.Release
                    end;
                  end;
              end;
            except
              ErrorCode := errQueryQueueOverflow;
            end
          finally
            CloseHandle(theQuery.Event)
          end
        finally
          Dispose(theQuery)
        end
      except
        ErrorCode := errUnknownError;
      end;
    end;

  procedure TWinSockRDOServerClientConnection.Send(Query : TRDOQuery);
    var
      theQuery : PQueryToSend;
    begin
      New(theQuery);
      theQuery.Query := Query;
      {$IFDEF Logs}
      LogThis('Sending : ' + Query.ToStr);
      {$ENDIF}
      theQuery.WaitForAnsw := false;
      theQuery.Result := nil;
      fUnsentQueriesLock.Acquire;
      try
        try
          fUnsentQueries.Add(theQuery);
          if fUnsentQueries.Count = 1
            then
              SetEvent(fUnsentQueryWaiting)
        except
          Dispose(theQuery)
        end
      finally
        fUnsentQueriesLock.Release
      end
    end;

  function TWinSockRDOServerClientConnection.GetLocalAddress : string;
    begin
      Result := fSocket.LocalAddress
    end;

  function TWinSockRDOServerClientConnection.GetLocalHost : string;
    begin
      Result := fSocket.LocalHost
    end;

  function TWinSockRDOServerClientConnection.GetLocalPort : integer;
    begin
      Result := fSocket.LocalPort
    end;

  function TWinSockRDOServerClientConnection.GetOnConnect : TRDOClientConnectEvent;
    begin
      result := fOnConnect;
    end;

  procedure TWinSockRDOServerClientConnection.SetOnConnect(OnConnectHandler : TRDOClientConnectEvent);
    begin
      fOnConnect := OnConnectHandler;
    end;

  function TWinSockRDOServerClientConnection.GetOnDisconnect : TRDOClientDisconnectEvent;
    begin
      result := fOnDisconnect;
    end;

  procedure TWinSockRDOServerClientConnection.SetOnDisconnect(OnDisconnectHandler : TRDOClientDisconnectEvent);
    begin
      fOnDisconnect := OnDisconnectHandler;
    end;

  procedure TWinSockRDOServerClientConnection.OnQueryResultArrival(Query : TRDOQuery);

    function FindServicedQuery : PQueryToSend;
      var
        QueryIdx    : integer;
        SentQueries : integer;
      begin
        SentQueries := fSentQueries.Count;
        QueryIdx    := 0;
        while (QueryIdx < SentQueries) and (Query.Id <> PQueryToSend(fSentQueries[QueryIdx]).Query.Id) do
          inc(QueryIdx);
        if QueryIdx < SentQueries
          then result := PQueryToSend(fSentQueries[QueryIdx])
          else result := nil;
      end;

    var
      ServicedQuery : PQueryToSend;

    begin
      fSentQueriesLock.Acquire;
      try
        ServicedQuery := FindServicedQuery;
        if ServicedQuery <> nil
          then
            begin
              fSentQueries.Remove(ServicedQuery);
              ServicedQuery.ErrorCode := GetQueryErrorCode(Query);  //errNoError;
              ServicedQuery.Result := Query;
              SetEvent(ServicedQuery.Event);
            end
          else Query.Free;
      finally
        fSentQueriesLock.Release
      end
    end;

end.
