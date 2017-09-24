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
    SyncObjs;

  type
    TQueryToService =
      record
        QueryText : string;
      end;

    PQueryToService = ^TQueryToService;

  type
    TWinSockRDOConnection = class;

    TServicingQueryThread =
      class( TSmartThread )
        private
          fConnection     : TWinSockRDOConnection;
          fQueryToService : PQueryToService;
        public
          constructor Create( theConnection : TWinSockRDOConnection );
          procedure   Execute; override;
        public
          property QueryToService : PQueryToService read fQueryToService write fQueryToService;
      end;

    TWinSockRDOConnection =
      {$IFDEF AutoServer}
      class( TAutoObject, IRDOConnectionInit, IRDOServerConnection, IRDOConnection )
      {$ELSE}
      class( TInterfacedObject, IRDOConnectionInit, IRDOServerConnection, IRDOConnection )
      {$ENDIF}
        private
          fPort               : integer;
          fServer             : string;
          fUnsentQueries      : TList;
          fSentQueries        : TList;
          fSenderThread       : TSmartThread;
          fMsgLoopThread      : TSmartThread;
          fUnsentQueriesLock  : TCriticalSection;
          fSentQueriesLock    : TCriticalSection;
          fSocketComponent    : TClientSocket;
          fReceivedText       : string;
          fConnectionEvent    : THandle;
          fUnsentQueryWaiting : THandle;
          fQueryServer        : IRDOQueryServer;
          fTerminateEvent     : THandle;
          fMaxQueryThreads    : integer;
          fQueryThreads       : TList;
          // fQueryThreadsSem    : THandle;
          fNoQueryRunning     : THandle;
          fQueryCount         : integer;
          fQueryCountLock     : TCriticalSection;
        private
          procedure GrowThreadCache( GrowBy : integer );
          procedure ReduceThreadCache( ReduceBy : integer );
          function  GetFreeQueryThread : TServicingQueryThread;
        private
          procedure DoRead( Sender : TObject; Socket : TCustomWinSocket );
          procedure HandleError( Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer );
          procedure HandleConnect( Sender : TObject; Socket : TCustomWinSocket );
          procedure HandleDisconnect( Sender : TObject; Socket : TCustomWinSocket );
        protected
          function  Get_Server : WideString; safecall;
          procedure Set_Server( const Value : WideString ); safecall;
          function  Get_Port : Integer; safecall;
          procedure Set_Port( Value : Integer ); safecall;
          function  Connect( TimeOut : Integer ) : WordBool; safecall;
          procedure Disconnect; safecall;
        protected
          procedure SetQueryServer( const QueryServer : IRDOQueryServer );
          function  GetMaxQueryThreads : integer;
          procedure SetMaxQueryThreads( MaxQueryThreads : integer );
        protected
          function  SendReceive( const QueryText : string; out ErrorCode : integer; TimeOut : integer ) : string; stdcall;
          procedure Send( const QueryText : string ); stdcall;
          function  GetLocalAddress : string; stdcall;
          function  GetLocalHost : string; stdcall;
          function  GetLocalPort : integer; stdcall;
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

  const
    DefMaxQueryThreads = 5;

  const
    ThreadCacheDelta = 5;

  // Query id generation routines and variables

  const
    DefRDOPort = 5000;

  var
    LastQueryId   : word;

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
          fConnection : TWinSockRDOConnection;
          constructor Create( theConnection : TWinSockRDOConnection );
        protected
          procedure Execute; override;
      end;

  type
    TMsgLoopThread =
      class( TSmartThread )
        private
          fConnection : TWinSockRDOConnection;
          constructor Create( theConnection : TWinSockRDOConnection );
        protected
          procedure Execute; override;
      end;

  // TSenderThread

  constructor TSenderThread.Create( theConnection : TWinSockRDOConnection );
    begin
      fConnection := theConnection;
      inherited Create( false )
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
                          fSocketComponent.Socket.SendText( CallId + Blank + IntToStr( QueryToSend.Id ) + Blank + QueryToSend.Text )
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
                          fSocketComponent.Socket.SendText( CallId + Blank + QueryToSend.Text )
                        except
                          {$IFDEF Logs}
                          LogThis( 'Error sending query' );
                          {$ENDIF}
                        end;
                        Dispose( QueryToSend )
                      end;
            end
        end
    end;

  // TMsgLoopThread

  constructor TMsgLoopThread.Create( theConnection : TWinSockRDOConnection );
    begin
      fConnection := theConnection;
//      Priority := tpHighest;
      inherited Create( false )
    end;

  procedure TMsgLoopThread.Execute;
    var
      Msg : TMsg;
    begin
      try
        fConnection.fSocketComponent.Open;
      except
        {$IFDEF Logs}
        LogThis( 'Error establishing connection' )
        {$ENDIF}
      end;
      while not Terminated do
        begin
          if PeekMessage( Msg, 0, 0, 0, PM_REMOVE )
            then
              DispatchMessage( Msg )
            else
              MsgWaitForMultipleObjects( 1, fConnection.fTerminateEvent, false, INFINITE, QS_ALLINPUT )
        end
    end;

  // TServicingQueryThread

  constructor TServicingQueryThread.Create( theConnection : TWinSockRDOConnection );
    begin
      fConnection := theConnection;
      inherited Create( true )
    end;

  procedure TServicingQueryThread.Execute;
    var
      QueryResult  : string;
    begin
      while not Terminated do
        with fConnection do
          try
            fQueryCountLock.Acquire;
            try
              inc( fQueryCount );
              if fQueryCount = 1
                then
                  ResetEvent( fNoQueryRunning )
            finally
              fQueryCountLock.Release
            end;
            QueryResult := fQueryServer.ExecQuery( fQueryToService.QueryText );
            if QueryResult <> ''
              then
                try
                  fSocketComponent.Socket.SendText( AnswerId + QueryResult );
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
                end;
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

  // TWinSockRDOConnection

  {$IFDEF AutoServer}
  procedure TWinSockRDOConnection.Initialize;
  {$ELSE}
  constructor TWinSockRDOConnection.Create;
  {$ENDIF}
    begin
      inherited;
      fSocketComponent := TClientSocket.Create( nil );
      fSocketComponent.Active := false;
      fUnsentQueriesLock := TCriticalSection.Create;
      fSentQueriesLock := TCriticalSection.Create;
      fUnsentQueries := TList.Create;
      fSentQueries := TList.Create;
      fPort := DefRDOPort;
      fQueryCountLock := TCriticalSection.Create;
      fMaxQueryThreads := DefMaxQueryThreads;
      fQueryThreads := TList.Create;
      fUnsentQueryWaiting := CreateEvent( nil, true, false, nil );
      fNoQueryRunning := CreateEvent( nil, true, true, nil );
      fTerminateEvent := CreateEvent( nil, true, false, nil )
    end;

  destructor TWinSockRDOConnection.Destroy;
    begin
      Disconnect;
      fUnsentQueriesLock.Free;
      fSentQueriesLock.Free;
      fSocketComponent.Free;
      fUnsentQueries.Free;
      fSentQueries.Free;
      fQueryCountLock.Free;
      fQueryThreads.Free;
      CloseHandle( fUnsentQueryWaiting );
      CloseHandle( fNoQueryRunning );
      CloseHandle( fTerminateEvent );
      inherited
    end;

  function TWinSockRDOConnection.Get_Server : WideString;
    begin
      Result := fServer
    end;

  procedure TWinSockRDOConnection.Set_Server( const Value : WideString );
    begin
      fServer := Value
    end;

  function TWinSockRDOConnection.Get_Port : Integer;
    begin
      Result := fPort
    end;

  procedure TWinSockRDOConnection.Set_Port( Value : Integer );
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

  function TWinSockRDOConnection.Connect( TimeOut : Integer ) : WordBool;
    var
      WaitRes : cardinal;
    begin
      Result := true;
      try
        ResetEvent( fTerminateEvent );
        with fSocketComponent do
          if not Active
            then
              begin
                ClientType := ctNonBlocking;
                if inet_addr( PChar( fServer ) ) = INADDR_NONE
                  then
                    Host := fServer
                  else
                    Address := fServer;
                Port := fPort;
                OnRead := DoRead;
                OnError := HandleError;
                OnConnect := HandleConnect;
                OnDisconnect := HandleDisconnect;
                fConnectionEvent := CreateEvent( nil, false, false, nil );
                try
                  fMsgLoopThread := TMsgLoopThread.Create( Self );
                  WaitRes := WaitForSingleObject( fConnectionEvent, TimeOut );
                  if WaitRes = WAIT_OBJECT_0
                    then
                      begin
                        fSenderThread := TSenderThread.Create( Self );
                        GrowThreadCache( ThreadCacheDelta )
                      end
                    else
                      begin
                        SetEvent( fTerminateEvent );
                        fMsgLoopThread.Free;
                        fMsgLoopThread := nil
                      end;
                  Result := WaitRes = WAIT_OBJECT_0;
{                  if Result = True
                    then
                      fQueryThreadsSem := CreateSemaphore( nil, fMaxQueryThreads, fMaxQueryThreads, nil )}
                finally
                  CloseHandle( fConnectionEvent )
                end
              end
      except
        Disconnect;
        Result := false
      end
    end;

  procedure TWinSockRDOConnection.Disconnect;
    begin
      SetEvent( fTerminateEvent );
      if fSocketComponent.Active
        then
          try
            fSocketComponent.Close
          except
            {$IFDEF Logs}
            LogThis( 'Error closing connection' )
            {$ENDIF}
          end;
      if fMsgLoopThread <> nil
        then
          begin
            fMsgLoopThread.Free;
            fMsgLoopThread := nil
          end;
      if fSenderThread <> nil
        then
          begin
            fSenderThread.Free;
            fSenderThread := nil
          end;
      WaitForSingleObject( fNoQueryRunning, INFINITE );
//      CloseHandle( fQueryThreadsSem );
      ReduceThreadCache( fQueryThreads.Count )
    end;

  procedure TWinSockRDOConnection.GrowThreadCache( GrowBy : integer );
    var
      ThreadCount : integer;
    begin
      for ThreadCount := 1 to GrowBy do
        fQueryThreads.Add( TServicingQueryThread.Create( Self ) )
    end;

  procedure TWinSockRDOConnection.ReduceThreadCache( ReduceBy : integer );
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

  function TWinSockRDOConnection.GetFreeQueryThread : TServicingQueryThread;
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

  procedure TWinSockRDOConnection.SetQueryServer( const QueryServer : IRDOQueryServer );
    begin
      fQueryServer := QueryServer
    end;

  function TWinSockRDOConnection.GetMaxQueryThreads : integer;
    begin
      Result := fMaxQueryThreads
    end;

  procedure TWinSockRDOConnection.SetMaxQueryThreads( MaxQueryThreads : integer );
    begin
      fMaxQueryThreads := MaxQueryThreads
    end;

  function TWinSockRDOConnection.SendReceive( const QueryText : string; out ErrorCode : integer; TimeOut : integer ) : string;
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

  procedure TWinSockRDOConnection.Send( const QueryText : string );
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

  procedure TWinSockRDOConnection.DoRead( Sender : TObject; Socket : TCustomWinSocket );

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
      ServicedQuery  : PQueryToSend;
      QueryText      : string;
      QueryToService : PQueryToService;
      QueryThread    : TServicingQueryThread;
      NonWSPCharIdx  : integer;
    begin
      try
        fReceivedText := fReceivedText + Socket.ReceiveText
      except
        {$IFDEF Logs}
        LogThis( 'Error reading from socket' )
        {$ENDIF}
      end;
      QueryText := GetQueryText( fReceivedText );
      if QueryText <> ''
        then
          repeat
            NonWSPCharIdx := 1;
            SkipSpaces( QueryText, NonWSPCharIdx );
            if QueryText[ NonWSPCharIdx ] = AnswerId
              then
                begin
                  Delete( QueryText, NonWSPCharIdx, 1 );
                  fSentQueriesLock.Acquire;
                  try
                    ServicedQuery := FindServicedQuery( QueryText );
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
                end
              else
                if QueryText[ NonWSPCharIdx ] = CallId
                  then
                    begin
                      Delete( QueryText, NonWSPCharIdx, 1 );
                      if fQueryServer <> nil
                        then
                          begin
                            New( QueryToService );
                            QueryToService.QueryText := QueryText;
//                            WaitForSingleObject( fQueryThreadsSem, INFINITE );
                            QueryThread := GetFreeQueryThread;
                            QueryThread.QueryToService := QueryToService;
                            QueryThread.Resume
                          end
                        else
                          Socket.SendText( AnswerId + CreateErrorMessage( errRDOServerNotInitialized ) )
                    end;
            QueryText := GetQueryText( fReceivedText )
          until QueryText = ''
    end;

  procedure TWinSockRDOConnection.HandleError( Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer );
    begin
      case ErrorEvent of
        {$IFDEF Logs}
        eeGeneral:
          LogThis( 'General socket error' );
        eeSend:
          LogThis( 'Error writing to socket' );
        eeReceive:
          LogThis( 'Error reading from socket' );
        {$ENDIF}
        eeConnect:
          begin
            ErrorCode := 0;
            {$IFDEF Logs}
            LogThis( 'Error establishing connection' )
            {$ENDIF}
          end;
        eeDisconnect:
          begin
            ErrorCode := 0;
            {$IFDEF Logs}
            LogThis( 'Error closing connection' )
            {$ENDIF}
          end;
        {$IFDEF Logs}
        eeAccept:
          LogThis( 'Error accepting connection' )
        {$ENDIF}
      end
    end;

  procedure TWinSockRDOConnection.HandleConnect( Sender : TObject; Socket : TCustomWinSocket );
    begin
      SetEvent( fConnectionEvent )
    end;

  procedure TWinSockRDOConnection.HandleDisconnect( Sender : TObject; Socket : TCustomWinSocket );
    begin
      // could be used to generate a connection lost event
    end;

initialization
  LastQueryId := 0;
  {$IFDEF AutoServer}
  TAutoObjectFactory.Create( ComServer, TWinSockRDOConnection, Class_WinSockRDOConnection, ciMultiInstance )
  {$ENDIF}
end.
