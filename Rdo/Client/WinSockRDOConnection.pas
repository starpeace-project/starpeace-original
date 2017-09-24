unit WinSockRDOConnection;

interface

  uses
    {$IFNDEF AutoServer}
    {$IFNDEF CacheManager}
    SmartThreads,
    {$ENDIF}
    {$ENDIF}
    Classes,
    ComObj,
    Windows,
    {$IFDEF AutoServer}
    RDOClient_TLB,
    {$ENDIF}
    RDOInterfaces,
    SocketComp,
    SyncObjs;

  {$IFDEF AutoServer}
  type
    TSmartThread = TThread;
  {$ELSE}
  {$IFDEF CacheManager}
  type
    TSmartThread = TThread;
  {$ENDIF}
  {$ENDIF}

  type
    TWinSockRDOConnection =
      {$IFDEF AutoServer}
      class(TAutoObject, IRDOConnectionInit, IRDOServerConnection, IRDOConnection)
      {$ELSE}
      class( TInterfacedObject, IRDOConnectionInit, IRDOServerConnection, IRDOConnection )
      {$ENDIF}
          {$IFDEF AutoServer}
        public
          procedure Initialize; override;
          {$ELSE}
        public
          constructor Create(aLabel : string);
          {$ENDIF}
          destructor Destroy; override;
        protected // IRDOConnectionInit
          function  Get_Server : WideString; safecall;
          procedure Set_Server( const Value : WideString ); safecall;
          function  Get_Port : Integer; safecall;
          procedure Set_Port( Value : Integer ); safecall;
          function  Connect( TimeOut : Integer ) : WordBool; safecall;
          procedure Disconnect; safecall;
        protected // IRDOServerConnection
          procedure SetQueryServer( const QueryServer : IRDOQueryServer );
          function  GetMaxQueryThreads : integer;
          procedure SetMaxQueryThreads( MaxQueryThreads : integer );
        public // IRDOConnection
          function  Alive : boolean;
          function  SendReceive( const QueryText : string; out ErrorCode : integer; TimeOut : integer ) : string; stdcall;
          procedure Send( const QueryText : string ); stdcall;
          function  GetLocalAddress : string; stdcall;
          function  GetRemoteAddress : string; stdcall;
          function  GetLocalHost : string; stdcall;
          function  GetLocalPort : integer; stdcall;
          function  GetOnConnect : TRDOClientConnectEvent;
          procedure SetOnConnect( OnConnectHandler : TRDOClientConnectEvent );
          function  GetOnDisconnect : TRDOClientDisconnectEvent;
          procedure SetOnDisconnect( OnDisconnectHandler : TRDOClientDisconnectEvent );
          function  GetTimeOut : integer;
          procedure SetTimeOut(TimeOut : integer);
          procedure SetData(data : pointer);
          function  GetData : pointer;
        private
          procedure CloseSocket;
          procedure KillThreads;
        private
          fTimeout            : integer;
          fConnected          : boolean;
          fPort               : integer;
          fServer             : string;
          //fUnsentQueries      : TList;
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
          fQueryQueue         : TList;
          fQueryWaiting       : THandle;
          fQueryQueueLock     : TCriticalSection;
          fQueryThreads       : TList;
          fMaxQueryThreads    : integer;
          fTerminateEvent     : THandle;
          fOnConnect          : TRDOClientConnectEvent;
          fOnDisconnect       : TRDOClientDisconnectEvent;
          fData               : pointer;
          procedure DoRead( Sender : TObject; Socket : TCustomWinSocket );
          procedure HandleError( Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer );
          procedure HandleConnect( Sender : TObject; Socket : TCustomWinSocket );
          procedure HandleDisconnect( Sender : TObject; Socket : TCustomWinSocket );
        private
          fLabel : string;
        private
          procedure OnTerminate(Sender: TObject);
        public
          property theLabel : string read fLabel write fLabel;
      end;

implementation

  uses
    {$IFDEF AutoServer}
    ComServ,
    {$ENDIF}
    SysUtils,
    ActiveX,
    WinSock,
    RDOUtils,
    RDOProtocol,
    {$IFDEF USELogs}
    Logs,
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

  type
    TQueryToService =
      record
        QueryText : string;
      end;

    PQueryToService = ^TQueryToService;

  // Delphi classes associated to the threads in charge of the connection

  type
    TMsgLoopThread =
      class( TSmartThread )
        private
          fConnection     : TWinSockRDOConnection;
          fTerminateEvent : THandle;
        private
          constructor Create(const theConnection : TWinSockRDOConnection );
        protected
          procedure Execute; override;
        public
          procedure ForceTerminate;
        public
          property Connection : TWinSockRDOConnection read fConnection write fConnection;
      end;

  type
    TQueryThread =
      class( TSmartThread )
        private
          fConnection  : TWinSockRDOConnection;
          fQueryStatus : integer;
          fRDOCallCnt  : integer;
        public
          constructor Create( theConnection : TWinSockRDOConnection );
          procedure   Execute; override;
      end;


  // TMsgLoopThread

  constructor TMsgLoopThread.Create(const theConnection : TWinSockRDOConnection );
    begin
      inherited Create(true);
      fConnection     := theConnection;
      FreeOnTerminate := true;
      fTerminateEvent := CreateEvent(nil, true, false, nil);
      Resume;
    end;

  procedure TMsgLoopThread.Execute;
    var
      Msg   : TMsg;
      alive : boolean;
    begin
      try
        try
          fConnection.fSocketComponent.Open;
        except
          Terminate;
          {$IFDEF USELogs}
          Logs.Log( 'Survival', '(3)- Error establishing connection' ); // (3)
          {$ENDIF}
        end;
        alive := true;
        while not Terminated and (fConnection <> nil) and alive do
          if PeekMessage( Msg, 0, 0, 0, PM_REMOVE )
            then
              try
                DispatchMessage( Msg )
              except
                {$IFDEF USELogs}
                Logs.Log( 'Survival', '(4)- Internal TMsgLoopThread error' ); // (4)
                {$ENDIF}
              end
            else alive := MsgWaitForMultipleObjects( 1, fTerminateEvent, false, INFINITE, QS_ALLINPUT ) = WAIT_OBJECT_0+1;
      except
      end;
    end;

  procedure TMsgLoopThread.ForceTerminate;
    begin
      SetEvent(fTerminateEvent);
    end;

  // TQueryThread

  constructor TQueryThread.Create( theConnection : TWinSockRDOConnection );
    begin
      fConnection := theConnection;
      inherited Create( false )
    end;

  procedure TQueryThread.Execute;
    var
      QueryResult       : string;
      QueryToService    : PQueryToService;
      QueryThreadEvents : array [ 1 .. 2 ] of THandle;
    begin
      CoInitialize(nil);
      try
        with fConnection do
          begin
            QueryThreadEvents[ 1 ] := fQueryWaiting;
            QueryThreadEvents[ 2 ] := fTerminateEvent;
            while not Terminated do
              begin
                // Check if the Termination event was signaled
                if WaitForMultipleObjects( 2, @QueryThreadEvents[ 1 ], false, INFINITE ) = WAIT_OBJECT_0 + 1
                  then Terminate;
                // sos
                if not Terminated
                  then
                    begin
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
                    end
                  else QueryToService := nil;
                if QueryToService <> nil
                  then
                    begin
                      QueryResult := fQueryServer.ExecQuery( QueryToService.QueryText, integer(fSocketComponent.Socket), fQueryStatus, fRDOCallCnt );
                      Dispose( QueryToService );
                      if QueryResult <> ''
                        then
                          try
                            fSocketComponent.Socket.SendText( AnswerId + QueryResult );
                            {$IFDEF Logs}
                            Logs.Log( 'Survival', 'Result : ' + QueryResult );
                            {$ENDIF}
                          except
                            {$IFDEF USELogs}
                            Logs.Log( 'Survival', '(6)- Socket error sending query result' ) // (6)
                            {$ENDIF}
                          end
                        else
                          begin
                            {$IFDEF Logs}
                            Logs.Log( 'Survival', 'No result' )
                            {$ENDIF}
                          end
                    end
              end
          end
      finally
        CoUninitialize;
      end;
    end;

  // TWinSockRDOConnection

  {$IFDEF AutoServer}
  procedure TWinSockRDOConnection.Initialize;
    const
      aLabel = 'AutoCnx';
    begin
      inherited Initialize;
  {$ELSE}
  constructor TWinSockRDOConnection.Create;
    begin
      inherited Create;
  {$ENDIF}
      fLabel := aLabel;
      assert(aLabel<>'', 'TWinSockRDOConnection');
      fSocketComponent := TClientSocket.Create( nil );
      fSocketComponent.Active := false;
      fUnsentQueriesLock := TCriticalSection.Create;
      fSentQueriesLock := TCriticalSection.Create;
      fSentQueries := TList.Create;
      fPort := DefRDOPort;
      fConnectionEvent := CreateEvent( nil, false, false, nil );
      fUnsentQueryWaiting := CreateEvent( nil, true, false, nil );
      fQueryQueue := TList.Create;
      fQueryThreads := TList.Create;
      fMaxQueryThreads := DefMaxQueryThreads;
      fQueryWaiting := CreateEvent( nil, true, false, nil );
      fQueryQueueLock := TCriticalSection.Create;
      fTerminateEvent := CreateEvent( nil, true, false, nil );
    end;

  procedure TWinSockRDOConnection.SetData(data : pointer);
    begin
      fData := data;
    end;

  function TWinSockRDOConnection.GetData : pointer;
    begin
      result := fData;
    end;

  destructor TWinSockRDOConnection.Destroy;

    procedure FreeQueryQueue;
      var
        i : integer;
      begin
        for i := 0 to pred(fQueryQueue.Count) do
          Dispose( PQueryToService( fQueryQueue[i] ) );
        fQueryQueue.Free;
      end;

    begin
      try
        Disconnect;
        fUnsentQueriesLock.Free;
        fSentQueriesLock.Free;
        fSocketComponent.Free;
        fSentQueries.Free;
        CloseHandle( fConnectionEvent );
        CloseHandle( fUnsentQueryWaiting );
        fQueryThreads.Free;
        FreeQueryQueue;
        CloseHandle( fQueryWaiting );
        fQueryQueueLock.Free;
        CloseHandle( fTerminateEvent );
      except
        {$ifopt d+}
        Outputdebugstring('TWinSockRDOConnection.Destroy, Error'); 
        {$endif}
      end;
      inherited;
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

  function TWinSockRDOConnection.GetRemoteAddress : string;
    begin
      Result := fSocketComponent.Socket.RemoteAddress;
    end;

  function TWinSockRDOConnection.GetLocalPort : integer;
    begin
      Result := fSocketComponent.Socket.LocalPort
    end;

  function TWinSockRDOConnection.GetOnConnect : TRDOClientConnectEvent;
    begin
      result := fOnConnect;
    end;

  procedure TWinSockRDOConnection.SetOnConnect( OnConnectHandler : TRDOClientConnectEvent );
    begin
      fOnConnect := OnConnectHandler
    end;

  function TWinSockRDOConnection.GetOnDisconnect : TRDOClientDisconnectEvent;
    begin
      result := fOnDisconnect;
    end;

  procedure TWinSockRDOConnection.SetOnDisconnect( OnDisconnectHandler : TRDOClientDisconnectEvent );
    begin
      fOnDisconnect := OnDisconnectHandler
    end;

  function TWinSockRDOConnection.GetTimeOut : integer;
    begin
      result := fTimeout;
    end;

  procedure TWinSockRDOConnection.SetTimeOut(TimeOut : integer);
    begin
      fTimeout := TimeOut;
    end;

  procedure TWinSockRDOConnection.CloseSocket;
    begin
      if fSocketComponent.Active
        then
          try
            fSocketComponent.Close;
          except
            {$IFDEF USELogs}
            Logs.Log( 'Survival', '(8)- Error closing connection' ); // (8)
            {$ENDIF}
          end;
    end;

  procedure TWinSockRDOConnection.KillThreads;

    procedure FreeQueryThreads;
      var
        i            : integer;
        aQueryThread : TSmartThread;
      begin
        for i := 0 to pred(fQueryThreads.Count) do
          begin
            aQueryThread := TSmartThread(fQueryThreads[i]);
            aQueryThread.Free;
          end;
        fQueryThreads.Clear;
      end;

    begin
      SetEvent( fTerminateEvent );
      if fMsgLoopThread <> nil
        then
          with TMsgLoopThread(fMsgLoopThread) do
            begin
              ForceTerminate;
              OnTerminate := nil;
              Terminate;
              fMsgLoopThread := nil;
           end;

      if fSenderThread <> nil
        then
          begin
            fSenderThread.Free;
            fSenderThread := nil;
          end;
      if fQueryThreads <> nil
        then FreeQueryThreads;
    end;

  function TWinSockRDOConnection.Connect( TimeOut : Integer ) : WordBool;
    var
      waitres : cardinal;
      Events  : array [0..1] of THandle;
    begin
      fTimeout := TimeOut;
      Result := true;
      try
        with fSocketComponent do
          if not Active
            then
              begin
                //ClientType := ctNonBlocking;
                if inet_addr( PChar( fServer ) ) = u_long(INADDR_NONE) // >> Delphi 4
                  then
                    Host := fServer
                  else
                    Address := fServer;
                Port := fPort;
                OnRead := DoRead;
                OnError := HandleError;
                OnConnect := HandleConnect;
                OnDisconnect := HandleDisconnect;

                if fMsgLoopThread<>nil
                  then
                    begin
                      SetEvent( fTerminateEvent );
                      fMsgLoopThread.Terminate;
                    end;

                ResetEvent( fTerminateEvent );
                ResetEvent( fConnectionEvent );
                fMsgLoopThread := TMsgLoopThread.Create( Self );
                fMsgLoopThread.OnTerminate := OnTerminate;
                Events[0] := fConnectionEvent;
                Events[1] := fTerminateEvent;
                waitres := WaitForMultipleObjects( 2, @Events[0], false, TimeOut );
                if waitres = WAIT_OBJECT_0
                  then //fSenderThread := TSenderThread.Create( Self )
                  else
                    begin
                      SetEvent( fTerminateEvent );
                      if fMsgLoopThread<>nil
                        then fMsgLoopThread.Terminate;
                    end;
                Result := waitres = WAIT_OBJECT_0;
              end;
      except
        Disconnect;
        Result := false;
      end
    end;

  procedure TWinSockRDOConnection.Disconnect;
    begin
      CloseSocket;
      KillThreads;
    end;

  procedure TWinSockRDOConnection.SetQueryServer( const QueryServer : IRDOQueryServer );
    var
      i : integer;
    begin
      fQueryServer := QueryServer;
      for i := 1 to fMaxQueryThreads do
        fQueryThreads.Add(TQueryThread.Create(Self));
    end;

  function TWinSockRDOConnection.GetMaxQueryThreads : integer;
    begin
      Result := fMaxQueryThreads
    end;

  procedure TWinSockRDOConnection.SetMaxQueryThreads( MaxQueryThreads : integer );
    begin
      fMaxQueryThreads := MaxQueryThreads
    end;

  function TWinSockRDOConnection.Alive : boolean;
    begin
      result := fConnected;
    end;

  function TWinSockRDOConnection.SendReceive( const QueryText : string; out ErrorCode : integer; TimeOut : integer ) : string;
    var
      theQuery : PQueryToSend;
      Events   : array [0..1] of THandle;
    begin
      if fConnected
        then
          try
            new(theQuery);
            try
              theQuery.Id := GenerateQueryId;
              theQuery.Text := QueryText;
              {$IFDEF Logs}
              Logs.Log( 'Survival', 'Sending and waiting: ' + QueryText );
              {$ENDIF}
              theQuery.WaitForAnsw := true;
              theQuery.Result := '';
              theQuery.Event := CreateEvent( nil, false, false, nil );
              try
                theQuery.ErrorCode := errNoError;
                try
                  // Add the query to the wait queue
                  fSentQueriesLock.Acquire;
                  try
                    fSentQueries.Add( theQuery )
                  finally
                    fSentQueriesLock.Release
                  end;
                  // send it to the net
                  try
                    fSocketComponent.Socket.SendText( CallId + Blank + IntToStr( theQuery.Id ) + Blank + theQuery.Text )
                  except
                    {$IFDEF USELogs}
                    Logs.Log( 'Survival', '(1)- Error sending query' ); // (1)
                    {$ENDIF}
                    fSentQueriesLock.Acquire;
                    try
                      SetEvent(theQuery.Event);
                      fSentQueries.Remove(theQuery);
                    finally
                      fSentQueriesLock.Release;
                    end;
                  end;

                  //fSocketComponent.Socket.
                  Events[0] := theQuery.Event;
                  Events[1] := fTerminateEvent;
                  case WaitForMultipleObjects(2, @Events[0], false, TimeOut) of
                    WAIT_OBJECT_0 :
                      begin
                        Result    := theQuery.Result;
                        ErrorCode := theQuery.ErrorCode;
                        {$IFDEF Logs}
                        Logs.Log( 'Survival', 'Result : ' + Result )
                        {$ENDIF}
                      end;
                    WAIT_OBJECT_0 + 1:
                      begin
                        result    := '';
                        ErrorCode := errQueryTimedOut;
                      end;
                    WAIT_TIMEOUT :
                      begin
                        Result := '';
                        {$IFDEF USELogs}
                        Logs.Log( 'Survival', DateTimeToStr(Now) + ' <' + fLabel + '> (10)- Query timed out '  + theQuery.Text + ' Time: ' + IntToStr(TimeOut)); // (10)
                        {$ENDIF}
                        ErrorCode := errQueryTimedOut;
                      end;
                  end;
                except
                  //fUnsentQueriesLock.Release;
                  ErrorCode := errQueryQueueOverflow;
                end;
              finally
                // remove the query
                fSentQueriesLock.Acquire;
                try
                  fSentQueries.Remove(theQuery);
                  CloseHandle(theQuery.Event);
                finally
                  fSentQueriesLock.Release;
                end;
              end;
            finally
              dispose(theQuery);
            end;
          except
            ErrorCode := errUnknownError;
          end;
    end;

  procedure TWinSockRDOConnection.Send(const QueryText : string);
    begin
      if fConnected
        then
          try
            fSocketComponent.Socket.SendText(CallId + Blank + QueryText);
          except
            {$IFDEF USELogs}
            Logs.Log('Survival', '(10)- Error sending query'); // (10)
            {$ENDIF}
          end;
    end;

  procedure TWinSockRDOConnection.DoRead( Sender : TObject; Socket : TCustomWinSocket );

    function FindQuery( QueryText : string ) : PQueryToSend;
      var
        CharIdx      : integer;
        IdDigits     : string;
        QueryId      : integer;
        QueryIdx     : integer;
        SentQueries  : integer;
        Query        : PQueryToSend;
        QueryTextLen : integer;
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
                    Query := fSentQueries[ QueryIdx ];
                    QueryTextLen := Length( QueryText );
                    while CharIdx <= QueryTextLen do
                      begin
                        Query.Result := Query.Result + QueryText[ CharIdx ];
                        inc( CharIdx );
                      end;
                    Result := Query;
                  end
                else Result := nil;
            end
          else Result := nil;
      end;

    var
      ServicedQuery  : PQueryToSend;
      QueryText      : string;
      QueryToService : PQueryToService;
      NonWSPCharIdx  : integer;
    begin
      try
        fReceivedText := fReceivedText + Socket.ReceiveText;
      except
        {$IFDEF USELogs}
        Logs.Log( 'Survival', '(11)- Error reading from socket' ); // (11)
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
                    ServicedQuery := FindQuery( QueryText );
                    if ServicedQuery <> nil
                      then
                        begin
                          fSentQueries.Remove( ServicedQuery );
                          ServicedQuery.ErrorCode := errNoError;
                          SetEvent( ServicedQuery.Event );
                        end
                      else
                        {$IFDEF USELogs}
                        Logs.Log( 'Survival', DateTimeToStr(Now) + ' Ignored result: ' + QueryText + ' in ' + fLabel);
                        {$ENDIF}
                  finally
                    fSentQueriesLock.Release;
                  end;
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
                            fQueryQueueLock.Acquire;
                            try
                              fQueryQueue.Add( QueryToService );
                              if fQueryQueue.Count = 1
                                then SetEvent( fQueryWaiting );
                            finally
                              fQueryQueueLock.Release;
                            end;
                          end
                        else Socket.SendText( AnswerId + CreateErrorMessage( errRDOServerNotInitialized ) );
                    end;
            QueryText := GetQueryText( fReceivedText );
          until QueryText = '';
    end;

  procedure TWinSockRDOConnection.HandleError( Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer );
    var
      aux : string;
    begin
      ErrorCode := 0;
      aux := DateTimeToStr(Now) + ' ' + theLabel;
      case ErrorEvent of
        {$IFDEF USELogs}
        eeGeneral:
          Logs.Log( 'Survival', aux + ' General socket error' );
        eeSend:
          Logs.Log( 'Survival', aux + ' Error writing to socket' );
        eeReceive:
          Logs.Log( 'Survival', aux + ' Error reading from socket' );
        {$ENDIF}
        eeConnect:
          begin
            ErrorCode := 0;
            SetEvent( fTerminateEvent );
            {$IFDEF USELogs}
            Logs.Log( 'Survival', aux + ' Cannot connect to Server: ' + fServer + ' Port: ' + IntToStr(fPort));
            {$ENDIF}
          end;
        eeDisconnect:
          begin
            ErrorCode := 0;
            {$IFDEF USELogs}
            Logs.Log( 'Survival', aux + ' Error closing connection' );
            {$ENDIF}
          end;
        {$IFDEF USELogs}
        eeAccept:
          Logs.Log( 'Survival', aux + ' Error accepting connection' );
        {$ENDIF}
      end
    end;

  procedure TWinSockRDOConnection.HandleConnect( Sender : TObject; Socket : TCustomWinSocket );
    begin
      fConnected := true;
      SetEvent( fConnectionEvent );
      if Assigned( fOnConnect )
        then fOnConnect( Self as IRDOConnection );
    end;

  procedure TWinSockRDOConnection.HandleDisconnect( Sender : TObject; Socket : TCustomWinSocket );
    begin
      {$IFDEF USELogs}
      Logs.Log( 'Survival', 'Start disconnecting: (' + theLabel + ') ' + DateTimeToStr(Now));
      {$ENDIF}
      try
        fConnected := false;
        if Assigned( fOnDisconnect )
          then fOnDisconnect( Self as IRDOConnection );
        KillThreads;
      finally
        {$IFDEF USELogs}
        Logs.Log( 'Survival', 'End disconnecting: (' + theLabel + ') ');
        {$ENDIF}
      end;
    end;

  procedure TWinSockRDOConnection.OnTerminate(Sender: TObject);
    begin
      fMsgLoopThread := nil;
    end;

initialization
  LastQueryId := 0;
  {$IFDEF AutoServer}
  TAutoObjectFactory.Create( ComServer, TWinSockRDOConnection, Class_WinSockRDOConnection, ciMultiInstance, tmApartment);
  {$ENDIF}
end.

