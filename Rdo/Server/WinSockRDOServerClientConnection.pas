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
          constructor Create( Socket : IWinSocketWrap );
          destructor  Destroy; override;
        protected // IRDOConnection
          function  Alive : boolean;
          function  SendReceive( const QueryText : string; out ErrorCode : integer; TimeOut : integer ) : string; stdcall;
          procedure Send( const QueryText : string ); stdcall;
          function  GetRemoteAddress : string; stdcall;
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
          fUnsentQueriesLock   : TCriticalSection;
          fSentQueriesLock     : TCriticalSection;
          fSocket              : IWinSocketWrap;//TCustomWinSocket;
          fUnsentQueryWaiting  : THandle;
          fTerminateEvent      : THandle;
          fOnConnect           : TRDOClientConnectEvent;
          fOnDisconnect        : TRDOClientDisconnectEvent;
          fData                : pointer;
        private
          function  GetTimeOut : integer;
          procedure SetTimeOut(TimeOut : integer);
          procedure SetData(data : pointer);
          function  GetData : pointer;
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

  // Delphi classes associated to the threads in charge of the connection

  type
    TSenderThread =
      class( TSmartThread )
        private
          fConnection : TWinSockRDOServerClientConnection;
          constructor Create( theConnection : TWinSockRDOServerClientConnection );
        protected
          procedure Execute; override;
      end;

  // TSenderThread

  constructor TSenderThread.Create( theConnection : TWinSockRDOServerClientConnection );
    begin
      fConnection := theConnection;
      inherited Create( false )
    end;

  procedure TSenderThread.Execute;
    var
      QueryToSend        : PQueryToSend;
      SenderThreadEvents : array [ 1 .. 2 ] of THandle;
      WinSocket          : TCustomWinSocket;
    begin
      with fConnection do
        begin
          SenderThreadEvents[ 1 ] := fUnsentQueryWaiting;
          SenderThreadEvents[ 2 ] := fTerminateEvent;
          while not Terminated do
            begin
              WaitForMultipleObjects( 2, @SenderThreadEvents[ 1 ], false, INFINITE );
              fUnsentQueriesLock.Acquire;
              try
                if fUnsentQueries.Count <> 0
                  then
                    begin
                      QueryToSend := fUnsentQueries[ 0 ];
                      fUnsentQueries.Delete( 0 )
                    end
                  else
                    begin
                      QueryToSend := nil;
                      ResetEvent( fUnsentQueryWaiting )
                    end
              finally
                fUnsentQueriesLock.Release
              end;
              if QueryToSend <> nil
                then
                  if QueryToSend.WaitForAnsw
                    then
                      begin
                        fSentQueriesLock.Acquire;
                        try
                          fSentQueries.Add( QueryToSend )
                        finally
                          fSentQueriesLock.Release
                        end;
                        try
                          fSocket.Lock;
                          try
                            WinSocket := fSocket.getSocket;
                            if WinSocket <> nil
                              then WinSocket.SendText( CallId + Blank + IntToStr( QueryToSend.Id ) + Blank + QueryToSend.Text );
                          finally
                            fSocket.Unlock;
                          end;
                        except
                          {$IFDEF Logs}
                          LogThis( 'Error sending query' );
                          {$ENDIF}
                          fSentQueriesLock.Acquire;
                          try
                            fSentQueries.Remove( QueryToSend )
                          finally
                            fSentQueriesLock.Release
                          end;
                          QueryToSend.ErrorCode := errSendError;
                          SetEvent( QueryToSend.Event )
                        end
                      end
                    else
                      begin
                        try
                          fSocket.Lock;
                          try
                            WinSocket := fSocket.getSocket;
                            if WinSocket <> nil
                              then WinSocket.SendText( CallId + Blank + QueryToSend.Text );
                          finally
                            fSocket.Unlock;
                          end;
                        except
                          {$IFDEF Logs}
                          LogThis( 'Error sending query' );
                          {$ENDIF}
                        end;
                        Dispose( QueryToSend )
                      end
            end
        end
    end;

  // TWinSockRDOServerClientConnection

  constructor TWinSockRDOServerClientConnection.Create(Socket : IWinSocketWrap);
    begin
      inherited Create;
      fSocket := Socket;
      fUnSentQueriesLock := TCriticalSection.Create;
      fSentQueriesLock := TCriticalSection.Create;
      fUnsentQueries := TList.Create;
      fSentQueries := TList.Create;
      fSenderThread := TSenderThread.Create( Self );
      fUnsentQueryWaiting := CreateEvent( nil, true, false, nil );
      fTerminateEvent := CreateEvent( nil, true, false, nil )
    end;

  destructor TWinSockRDOServerClientConnection.Destroy;
    begin
      fSocket.Invalidate;
      SetEvent( fTerminateEvent );
      fSenderThread.Free;
      fUnSentQueriesLock.Free;
      fSentQueriesLock.Free;
      fUnsentQueries.Free;
      fSentQueries.Free;
      CloseHandle( fUnsentQueryWaiting );
      CloseHandle( fTerminateEvent );
      inherited
    end;

  function TWinSockRDOServerClientConnection.Alive : boolean;
    begin
      result := fSocket.isValid;
    end;

  function TWinSockRDOServerClientConnection.SendReceive( const QueryText : string; out ErrorCode : integer; TimeOut : integer ) : string;
    var
      theQuery : PQueryToSend;
      WaitRes  : cardinal;
    begin
      try
        New( theQuery );
        try
          theQuery.Id := GenerateQueryId;
          theQuery.Text := QueryText;
          {$IFDEF Logs}
          LogThis( 'Sending and waiting: ' + QueryText );
          {$ENDIF}
          theQuery.WaitForAnsw := true;
          theQuery.Result := '';
          theQuery.Event := CreateEvent( nil, false, false, nil );
          try
            theQuery.ErrorCode := errNoError;
            fUnsentQueriesLock.Acquire;
            try
              fUnsentQueries.Add( theQuery );
              if fUnsentQueries.Count = 1
                then
                  SetEvent( fUnsentQueryWaiting );
              fUnsentQueriesLock.Release;
              WaitRes := WaitForSingleObject( theQuery.Event, TimeOut );
              if WaitRes = WAIT_OBJECT_0
                then
                  begin
                    Result := theQuery.Result;
                    ErrorCode := theQuery.ErrorCode;
                    {$IFDEF Logs}
                    LogThis( 'Result : ' + Result )
                    {$ENDIF}
                  end
                else
                  begin
                    Result := '';
                    {$IFDEF Logs}
                    LogThis( 'Query timed out' );
                    {$ENDIF}
                    ErrorCode := errQueryTimedOut;
                    fSentQueriesLock.Acquire;
                    try
                      fSentQueries.Remove( theQuery )
                    finally
                      fSentQueriesLock.Release
                    end
                  end
            except
              fUnsentQueriesLock.Release;
              ErrorCode := errQueryQueueOverflow
            end
          finally
            CloseHandle( theQuery.Event )
          end
        finally
          Dispose( theQuery )
        end
      except
        ErrorCode := errUnknownError
      end
    end;

  procedure TWinSockRDOServerClientConnection.Send( const QueryText : string );
    var
      theQuery : PQueryToSend;
    begin
      New( theQuery );
      theQuery.Text := QueryText;
      {$IFDEF Logs}
      LogThis( 'Sending : ' + QueryText );
      {$ENDIF}
      theQuery.WaitForAnsw := false;
      fUnsentQueriesLock.Acquire;
      try
        try
          fUnsentQueries.Add( theQuery );
          if fUnsentQueries.Count = 1
            then
              SetEvent( fUnsentQueryWaiting )
        except
          Dispose( theQuery )
        end
      finally
        fUnsentQueriesLock.Release
      end
    end;

  function TWinSockRDOServerClientConnection.GetRemoteAddress : string;
    var
      WinSocket : TCustomWinSocket;
    begin
      try
        fSocket.Lock;
        try
          WinSocket := fSocket.getSocket;
          if WinSocket <> nil
            then Result := WinSocket.RemoteAddress
            else Result := '';
        finally
          fSocket.Unlock;
        end;
      except
        Result := '';
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

  procedure TWinSockRDOServerClientConnection.SetData(data : pointer);
    begin
      fData := data;
    end;

  function TWinSockRDOServerClientConnection.GetData : pointer;
    begin
      result := fData;
    end;

end.
