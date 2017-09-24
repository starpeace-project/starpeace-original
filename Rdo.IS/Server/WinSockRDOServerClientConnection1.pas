unit WinSockRDOServerClientConnection;

interface

  uses
    SmartThreads,
    Classes,
    ComObj,
    Windows,
    RDOInterfaces,
    SocketComp,
    SyncObjs;

  type
    TWinSockRDOServerClientConnection =
      class( TInterfacedObject, IRDOConnection, IRDOServerClientConnection )
        public
          constructor Create(Sock : IWinSocketWrap; termEvent : THandle); //Create( Socket : TCustomWinSocket );
          destructor  Destroy; override;
        protected // IRDOConnection
          function  Alive : boolean;
          function  SendReceive( const QueryText : string; out ErrorCode : integer; TimeOut : integer ) : string; stdcall;
          procedure Send( const QueryText : string ); stdcall;
          function  GetLocalAddress : string; stdcall;
          function  GetLocalHost : string; stdcall;
          function  GetLocalPort : integer; stdcall;
          function  GetOnConnect : TRDOClientConnectEvent;
          procedure SetOnConnect( OnConnectHandler : TRDOClientConnectEvent );
          function  GetOnDisconnect : TRDOClientDisconnectEvent;
          procedure SetOnDisconnect( OnDisconnectHandler : TRDOClientDisconnectEvent );
        protected // IRDOServerClientConnection
          procedure OnQueryResultArrival( const QueryResult : string );
        private
          fUnsentQueries       : TList;
          fSentQueries         : TList;
          fSenderThread        : TSmartThread;
          fSentQueriesLock     : TCriticalSection;
          fSocket              : IWinSocketWrap; //TCustomWinSocket;
          fUnsentQueryWaiting  : THandle;
          fTerminateEvent      : THandle;
          fOnConnect           : TRDOClientConnectEvent;
          fOnDisconnect        : TRDOClientDisconnectEvent;
        private
          function  GetTimeOut : integer;
          procedure SetTimeOut(TimeOut : integer);
      end;

implementation

  uses
    SysUtils,
    RDOUtils,
    RDOProtocol,
    {$IFDEF Logs}
    LogFile,
    {$ENDIF}
    ErrorCodes,
    Logs;

  // Query id generation routines and variables

  var
    LastQueryId : word;

  function GenerateQueryId : integer;
    begin
      Result := LastQueryId;
      LastQueryId := ( LastQueryId + 1 ) mod 65536
    end;

  type
    TQueryToSend =
      record
        Id          : word;
        Text        : string;
        WaitForAnsw : boolean;
        Result      : string;
        Event       : THandle;
        ErrorCode   : integer;
      end;

    PQueryToSend = ^TQueryToSend;

  // TWinSockRDOServerClientConnection

  constructor TWinSockRDOServerClientConnection.Create(Sock : IWinSocketWrap; termEvent : THandle);
    begin
      inherited Create;
      fSocket := Sock;
      fSentQueriesLock := TCriticalSection.Create;
      fUnsentQueries := TList.Create;
      fSentQueries := TList.Create;
      fUnsentQueryWaiting := CreateEvent( nil, true, false, nil );
      fTerminateEvent := termEvent;
    end;

  destructor TWinSockRDOServerClientConnection.Destroy;
    begin
      SetEvent( fTerminateEvent );
      fSenderThread.Free;
      fSentQueriesLock.Free;
      fUnsentQueries.Free;
      fSentQueries.Free;
      CloseHandle( fUnsentQueryWaiting );
      inherited
    end;

  function TWinSockRDOServerClientConnection.Alive : boolean;
    begin
      result := fSocket.isValid;
    end;

  function TWinSockRDOServerClientConnection.SendReceive( const QueryText : string; out ErrorCode : integer; TimeOut : integer ) : string;
    var
      theQuery  : PQueryToSend;
      WaitRes   : cardinal;
      WinSocket : TCustomWinSocket;
    begin
      try
        New( theQuery );
        try
          theQuery.Id   := GenerateQueryId;
          theQuery.Text := QueryText;
          theQuery.WaitForAnsw := true;
          theQuery.Result := '';
          theQuery.Event := CreateEvent( nil, false, false, nil );
          theQuery.ErrorCode := errNoError;
          try
            // Add to Sent Queue
            fSentQueriesLock.Acquire;
            try
              fSentQueries.Add(theQuery);
            finally
              fSentQueriesLock.Release
            end;

            // Send the Query
            try
              fSocket.Lock;
              try
                WinSocket := fSocket.getSocket;
                if WinSocket <> nil
                  then WinSocket.SendText(CallId + Blank + IntToStr( theQuery.Id ) + Blank + theQuery.Text);
              finally
                fSocket.Unlock;
              end;
            except
              fSentQueriesLock.Acquire;
              try
                fSentQueries.Remove(theQuery)
              finally
                fSentQueriesLock.Release
              end
            end;

            // Wait for result
            WaitRes := WaitForSingleObject( theQuery.Event, TimeOut );
            if WaitRes = WAIT_OBJECT_0
              then
                begin
                  Result    := theQuery.Result;
                  ErrorCode := theQuery.ErrorCode;
                end
              else
                begin
                  Result    := '';
                  ErrorCode := errQueryTimedOut;
                end

          finally
            fSentQueriesLock.Acquire;
            try
              fSentQueries.Remove(theQuery);
              CloseHandle(theQuery.Event);
            finally
              fSentQueriesLock.Release
            end;
          end;

        finally
          Dispose(theQuery);
        end
      except
        ErrorCode := errUnknownError
      end
    end;

  procedure TWinSockRDOServerClientConnection.Send( const QueryText : string );
    var
      WinSocket : TCustomWinSocket;
    begin
      try
        fSocket.Lock;
        try
          WinSocket := fSocket.getSocket;
          if WinSocket <> nil
            then WinSocket.SendText(CallId + Blank + QueryText);
        finally
          fSocket.Unlock;
        end;
      except
        Logs.Log('Survival', '(10)- Error sending query'); // (10)
      end;
    end;

  function TWinSockRDOServerClientConnection.GetLocalAddress : string;
    var
      WinSocket : TCustomWinSocket;
    begin
      try
        fSocket.Lock;
        try
          WinSocket := fSocket.getSocket;
          if WinSocket <> nil
            then Result := WinSocket.LocalAddress
            else Result := '';
        finally
          fSocket.Unlock;
        end;
      except
        Result := '';
      end;
    end;

  function TWinSockRDOServerClientConnection.GetLocalHost : string;
    var
      WinSocket : TCustomWinSocket;
    begin
      try
        fSocket.Lock;
        try
          WinSocket := fSocket.getSocket;
          if WinSocket <> nil
            then Result := WinSocket.LocalHost
            else Result := '';
        finally
          fSocket.Unlock;
        end;
      except
        Result := '';
      end;
    end;

  function TWinSockRDOServerClientConnection.GetLocalPort : integer;
    var
      WinSocket : TCustomWinSocket;
    begin
      try
        fSocket.Lock;
        try
          WinSocket := fSocket.getSocket;
          if WinSocket <> nil
            then Result := WinSocket.LocalPort
            else Result := 0;
        finally
          fSocket.Unlock;
        end;
      except
        Result := 0;
      end;
    end;

  function TWinSockRDOServerClientConnection.GetOnConnect : TRDOClientConnectEvent;
    begin
      result := fOnConnect;
    end;

  procedure TWinSockRDOServerClientConnection.SetOnConnect( OnConnectHandler : TRDOClientConnectEvent );
    begin
      fOnConnect := OnConnectHandler;
    end;

  function TWinSockRDOServerClientConnection.GetOnDisconnect : TRDOClientDisconnectEvent;
    begin
      result := fOnDisconnect;
    end;

  procedure TWinSockRDOServerClientConnection.SetOnDisconnect( OnDisconnectHandler : TRDOClientDisconnectEvent );
    begin
      fOnDisconnect := OnDisconnectHandler;
    end;

  procedure TWinSockRDOServerClientConnection.OnQueryResultArrival( const QueryResult : string );

    function FindServicedQuery( QueryText : string ) : PQueryToSend;
      var
        CharIdx       : integer;
        IdDigits      : string;
        QueryId       : integer;
        QueryIdx      : integer;
        SentQueries   : integer;
        ServicedQuery : PQueryToSend;
        QueryTextLen  : integer;
      begin
        IdDigits := '';
        CharIdx := 1;
        SkipSpaces( QueryText, CharIdx );
        IdDigits := ReadNumber( QueryText, CharIdx );
        try
          QueryId := StrToInt( IdDigits );
        except
          QueryId := -1
        end;
        if QueryId <> -1
          then
            begin
              QueryIdx := 0;
              SentQueries := fSentQueries.Count;
              while ( QueryIdx < SentQueries ) and ( QueryId <> PQueryToSend( fSentQueries[ QueryIdx ] ).Id ) do
                inc( QueryIdx );
              if QueryIdx < SentQueries
                then
                  begin
                    ServicedQuery := fSentQueries[ QueryIdx ];
                    QueryTextLen := Length( QueryText );
                    while CharIdx <= QueryTextLen do
                      begin
                        ServicedQuery.Result := ServicedQuery.Result + QueryText[ CharIdx ];
                        inc( CharIdx )
                      end;
                    Result := ServicedQuery
                  end
                else
                  Result := nil
            end
          else
            Result := nil;
      end;

    var
      ServicedQuery : PQueryToSend;
    begin
      fSentQueriesLock.Acquire;
      try
        ServicedQuery := FindServicedQuery( QueryResult );
        if ServicedQuery <> nil
          then
            begin
              fSentQueries.Remove( ServicedQuery );
              ServicedQuery.ErrorCode := errNoError;
              SetEvent( ServicedQuery.Event )
            end
      finally
        fSentQueriesLock.Release
      end
    end;

  function TWinSockRDOServerClientConnection.GetTimeOut : integer;
    begin
      result := 60*1000;
    end;

  procedure TWinSockRDOServerClientConnection.SetTimeOut(TimeOut : integer);
    begin
    end;

end.
