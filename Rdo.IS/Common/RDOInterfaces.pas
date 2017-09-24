unit RDOInterfaces;

interface

  uses
    RDOProtocol, SocketComp;

  const
    MSG_GETTHREADDATA = $1001;
    MSG_SETTHREADDATA = $1002;

  {$IFNDEF AutoServer}
  type
    IRDOConnectionInit =
      interface [ '{410CAF20-64BA-11d1-AF26-008029E5CA8C}' ]
        function  Get_Server : WideString; safecall;
        procedure Set_Server( const Value : WideString ); safecall;
        function  Get_Port : Integer; safecall;
        procedure Set_Port( Value : Integer ); safecall;
        function  Connect( TimeOut : Integer ) : WordBool; safecall;
        procedure Disconnect; safecall;
        property  Server       : WideString read Get_Server    write Set_Server;
        property  Port         : Integer    read Get_Port      write Set_Port;
      end;
  {$ENDIF}

  type
    IRDOConnection = interface;
    ILogAgent      = interface;
    ILogAgents     = interface;

    TRDOClientConnectEvent    = procedure ( const ClientConnection : IRDOConnection ) of object;
    TRDOClientDisconnectEvent = procedure ( const ClientConnection : IRDOConnection ) of object;

    IRDOConnection =
      interface [ '{5BDB3080-7467-11d1-AF26-008029E5CA8C}' ]
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
        function  GetTimeOut : integer;
        procedure SetTimeOut(TimeOut : integer);
        procedure SetData(data : pointer);
        function  GetData : pointer;
        property  LocalAddress  : string  read GetLocalAddress;
        property  RemoteAddress : string  read GetRemoteAddress;
        property  LocalHost     : string  read GetLocalHost;
        property  LocalPort     : integer read GetLocalPort;
        property  OnConnect     : TRDOClientConnectEvent    read GetOnConnect    write SetOnConnect;
        property  OnDisconnect  : TRDOClientDisconnectEvent read GetOnDisconnect write SetOnDisconnect;
        property  TimeOut       : integer    read GetTimeOut write SetTimeOut;
      end;

    pinteger = ^integer;

    IRDOQueryServer =
      interface
        function  ExecQuery( const QueryText : string; ConnId : integer; var QueryStatus, RDOCallCnt : integer ) : string;
        function  GetBusy : boolean;
        function  GetStatus : integer;
        procedure SetBusy(value : boolean);
        property  Busy : boolean read GetBusy write SetBusy;
        property  Status : integer read GetStatus;
      end;

    IRDOServerConnection =
      interface [ '{1107CE00-7468-11d1-AF26-008029E5CA8C}' ]
        procedure SetQueryServer( const QueryServer : IRDOQueryServer );
        function  GetMaxQueryThreads : integer;
        procedure SetMaxQueryThreads( MaxQueryThreads : integer );
        property  MaxQueryThreads : integer read GetMaxQueryThreads write SetMaxQueryThreads;
      end;

    IRDOConnectionServerEvents =
      interface
        procedure OnClientConnect( const ClientConnection : IRDOConnection );
        procedure OnClientDisconnect( const ClientConnection : IRDOConnection );
      end;

    IRDOConnectionsServer =
      interface [ '{9AA0B820-7468-11d1-AF26-008029E5CA8C}' ]
        procedure StartListening;
        procedure StopListening;
        function  GetClientConnection( const ClientAddress : string; ClientPort : integer ) : IRDOConnection;
        function  GetClientConnectionById( Id : integer ) : IRDOConnection;
        procedure InitEvents( const EventSink : IRDOConnectionServerEvents );
      end;

    IRDOServerClientConnection =
      interface [ '{45AA7020-7492-11d1-AF26-008029E5CA8C}' ]
        procedure OnQueryResultArrival( const QueryResult : string );
      end;

    ILockObject =
      interface ['{4B337CC0-57CE-11d1-AF26-008029E5CA8C}']
        procedure Lock;
        procedure UnLock;
      end;

    TLogQueryEvent = procedure(Query : string) of object;

    ILogAgent =
      interface
        function  GetId : string;
        function  GetLogId(Obj : TObject) : string;
        function  GetObject(ObjId : string) : TObject;
        procedure LogQuery(Query : string);
      end;

    ILogAgents =
      interface
        function  LogableMethod(aName : string) : boolean;
        function  GetLogAgentById(Id : string) : ILogAgent;
        function  GetLogAgentByClass(aClass : TClass) : ILogAgent;
        procedure RegisterMethods(Names : array of string);
        procedure RegisterAgent(aClassName : string; anAgent : ILogAgent);
      end;

    IRDOLog =
      interface ['{4BA37CC0-57CE-11d1-AF26-008029E5CA8C}']
        procedure RegisterAgents(Agents : ILogAgents);
        procedure ExecLogQuery(Query : string);
      end;

    IWinSocketWrap =
      interface
        function  isValid   : boolean;
        function  getSocket : TCustomWinSocket;
        function  getConnection : IRDOConnection;
        function  getConnectionData : pointer;
        procedure setConnectionData(info : pointer);
        procedure Invalidate;
        procedure Lock;
        procedure Unlock;
      end;

implementation

end.
