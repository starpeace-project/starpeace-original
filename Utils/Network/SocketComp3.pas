{*******************************************************}
{                                                       }
{       Merchise's Variation of Borland Sockets         }
{                                                       }
{       Copyright (c) 1997 Merchise                     }
{                                                       }
{*******************************************************}

unit SocketComp;

interface

  uses
    SysUtils,
    Windows,
    Messages,
    Classes,
    WinSock,
    SyncObjs,
    Collection,
    SpoolPackets;

  const
    CM_SOCKETMESSAGE = WM_USER + $0001;
    CM_DEFERFREE     = WM_USER + $0002;

  type
    ESocketError = class(Exception);

    TCMSocketMessage =
      record
        Msg         : Cardinal;
        Socket      : TSocket;
        SelectEvent : Word;
        SelectError : Word;
        Result      : Longint;
      end;

    TCustomWinSocket       = class;
    TCustomSocket          = class;
    TServerAcceptThread    = class;
    TServerClientThread    = class;
    TServerWinSocket       = class;
    TServerClientWinSocket = class;

    TServerType = (stNonBlocking, stThreadBlocking);
    TClientType = (ctNonBlocking, ctBlocking);
    TAsyncStyle = (asRead, asWrite, asOOB, asAccept, asConnect, asClose);
    TAsyncStyles = set of TAsyncStyle;
    TSocketEvent =
      (
        seLookup,
        seConnecting,
        seConnect,
        seDisconnect,
        seListen,
        seAccept,
        seWrite,
        seRead
     );
    TErrorEvent = (eeGeneral, eeSend, eeReceive, eeConnect, eeDisconnect, eeAccept);

    TSocketEventEvent  = procedure (Sender : TObject; Socket : TCustomWinSocket; SocketEvent : TSocketEvent) of object;
    TSocketErrorEvent  = procedure (Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer) of object;
    TGetSocketEvent    = procedure (Sender : TObject; Socket : TSocket; var ClientSocket : TServerClientWinSocket) of object;
    TGetThreadEvent    = procedure (Sender : TObject; ClientSocket : TServerClientWinSocket; var SocketThread : TServerClientThread) of object;
    TSocketNotifyEvent = procedure (Sender : TObject; Socket : TCustomWinSocket) of object;

    TCustomWinSocket =
      class
        private
          fSocket        : TSocket;
          fConnected     : boolean;
          fHandle        : HWnd;
          fAddr          : TSockAddrIn;
          fAsyncStyles   : TAsyncStyles;
          fOnSocketEvent : TSocketEventEvent;
          fOnErrorEvent  : TSocketErrorEvent;
          fSocketLock    : TCriticalSection;
          fData          : Pointer;
          fSpoolBuffer   : TCollection;
          fTCPBufSize    : integer;
          fWriteEnabled  : boolean;
        public
          procedure DefaultHandler(var Message); override;
          procedure WndProc(var Message : TMessage);
          procedure CMSocketMessage(var Message : TCMSocketMessage); message CM_SOCKETMESSAGE;
          procedure CMDeferFree(var Message); message CM_DEFERFREE;
          procedure DeferFree;
          procedure DoSetAsyncStyles;
          function  GetHandle : HWnd;
          function  GetLocalHost : string;
          function  GetLocalAddress : string;
          function  GetLocalPort : integer;
          function  GetRemoteHost : string;
          function  GetRemoteAddress : string;
          function  GetRemotePort : integer;
          function  GetRemoteAddr : TSockAddrIn;
          function  CheckLastErrorSeverity : boolean;
          function  ClearSpool : boolean;
          procedure AsyncSelectSocket;
        protected
          function  InitSocket(var Name, Address, Service : string; Port : Word; Client : boolean) : TSockAddrIn;
          procedure Event(Socket : TCustomWinSocket; SocketEvent: TSocketEvent); dynamic;
          procedure Error(Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer); dynamic;
          procedure SetAsyncStyles(Value : TASyncStyles);
          procedure Listen(var Name, Address, Service : string; Port : Word; QueueSize : integer);
          procedure Open(var Name, Address, Service : string; Port : Word);
          procedure Accept(Socket : TSocket); virtual;
          procedure Connect(Socket : TSocket); virtual;
          procedure Disconnect(Socket : TSocket); virtual;
          procedure Read(Socket : TSocket); virtual;
          procedure Write(Socket : TSocket); virtual;
        public
          constructor Create(aSocket : TSocket);
          destructor  Destroy; override;
        public
          procedure Close;
          procedure Lock;
          procedure Unlock;
          function  LookupName(const Name : string) : TInAddr;
          function  LookupService(const Service : string) : integer;
          function  ReceiveLength : integer;
          function  ReceiveBuf(var Buf; Count : integer) : integer;
          function  ReceiveText : string;
          function  SendBuf(var Buf; Count : integer) : integer;
          procedure SendText(const S : string);
          procedure SendEncryptedText(const S : string);
        public
          property LocalHost     : string            read GetLocalHost;
          property LocalAddress  : string            read GetLocalAddress;
          property LocalPort     : Integer           read GetLocalPort;
          property RemoteHost    : string            read GetRemoteHost;
          property RemoteAddress : string            read GetRemoteAddress;
          property RemotePort    : Integer           read GetRemotePort;
          property RemoteAddr    : TSockAddrIn       read GetRemoteAddr;
          property Connected     : boolean           read fConnected;
          property Addr          : TSockAddrIn       read fAddr;
          property ASyncStyles   : TAsyncStyles      read fAsyncStyles     write SetAsyncStyles;
          property Handle        : HWnd              read GetHandle;
          property SocketHandle  : TSocket           read fSocket;
          property OnSocketEvent : TSocketEventEvent read fOnSocketEvent   write fOnSocketEvent;
          property OnErrorEvent  : TSocketErrorEvent read fOnErrorEvent    write fOnErrorEvent;
          property Data          : Pointer           read FData            write FData;
      end;

    TClientWinSocket =
      class(TCustomWinSocket)
        private
          fClientType : TClientType;
        protected
          procedure Connect(Socket : TSocket); override;
          procedure SetClientType(Value : TClientType);
        public
          property ClientType : TClientType read fClientType write SetClientType;
      end;

    TServerClientWinSocket =
      class(TCustomWinSocket)
        private
          fServerWinSocket : TServerWinSocket;
        public
          constructor Create(Socket : TSocket; ServerWinSocket : TServerWinSocket);
          destructor  Destroy; override;
        public
          property ServerWinSocket : TServerWinSocket read fServerWinSocket;
      end;

    TThreadNotifyEvent = procedure (Sender : TObject; Thread : TServerClientThread) of object;

    TServerWinSocket =
      class(TCustomWinSocket)
        private
          fServerType         : TServerType;
          fThreadCacheSize    : integer;
          fConnections        : TCollection;
          fActiveThreads      : TList;
          fListLock           : TCriticalSection;
          fServerAcceptThread : TServerAcceptThread;
          fOnGetSocket        : TGetSocketEvent;
          fOnGetThread        : TGetThreadEvent;
          fOnThreadStart      : TThreadNotifyEvent;
          fOnThreadEnd        : TThreadNotifyEvent;
          fOnClientConnect    : TSocketNotifyEvent;
          fOnClientDisconnect : TSocketNotifyEvent;
          fOnClientRead       : TSocketNotifyEvent;
          fOnClientWrite      : TSocketNotifyEvent;
          fOnClientError      : TSocketErrorEvent;
          procedure AddClient(aClient : TServerClientWinSocket);
          procedure RemoveClient(aClient : TServerClientWinSocket);
          procedure AddThread(aThread : TServerClientThread);
          procedure RemoveThread(aThread : TServerClientThread);
          procedure ClientEvent(Sender : TObject; Socket : TCustomWinSocket; SocketEvent : TSocketEvent);
          procedure ClientError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
          function  GetActiveConnections : integer;
          function  GetActiveThreads : integer;
          function  GetConnections(Index : integer) : TCustomWinSocket;
          function  GetIdleThreads : integer;
        protected
          procedure Accept(Socket : TSocket); override;
          procedure Disconnect(Socket : TSocket); override;
          function  DoCreateThread(ClientSocket : TServerClientWinSocket) : TServerClientThread; virtual;
          procedure Listen(var Name, Address, Service : string; Port : Word; QueueSize : integer);
          procedure SetServerType(Value : TServerType);
          procedure SetThreadCacheSize(Value : integer);
          procedure ThreadEnd(aThread : TServerClientThread); dynamic;
          procedure ThreadStart(aThread : TServerClientThread); dynamic;
          function  GetClientSocket(Socket : TSocket) : TServerClientWinSocket; dynamic;
          function  GetServerThread(ClientSocket : TServerClientWinSocket) : TServerClientThread; dynamic;
          procedure ClientRead(Socket : TCustomWinSocket); dynamic;
          procedure ClientWrite(Socket : TCustomWinSocket); dynamic;
          procedure ClientConnect(Socket : TCustomWinSocket); dynamic;
          procedure ClientDisconnect(Socket : TCustomWinSocket); dynamic;
          procedure ClientErrorEvent(Socket : TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode : integer); dynamic;
        public
          constructor Create(aSocket : TSocket);
          destructor  Destroy; override;
          function GetClientThread(ClientSocket : TServerClientWinSocket) : TServerClientThread;
        public
          property ActiveConnections  : integer read GetActiveConnections;
          property ActiveThreads      : integer read GetActiveThreads;
          property Connections[ Index : integer ] : TCustomWinSocket read GetConnections;
          property IdleThreads        : integer            read GetIdleThreads;
          property ServerType         : TServerType        read fServerType         write SetServerType;
          property ThreadCacheSize    : integer            read fThreadCacheSize    write SetThreadCacheSize;
          property OnGetSocket        : TGetSocketEvent    read fOnGetSocket        write fOnGetSocket;
          property OnGetThread        : TGetThreadEvent    read fOnGetThread        write fOnGetThread;
          property OnThreadStart      : TThreadNotifyEvent read fOnThreadStart      write fOnThreadStart;
          property OnThreadEnd        : TThreadNotifyEvent read fOnThreadEnd        write fOnThreadEnd;
          property OnClientConnect    : TSocketNotifyEvent read fOnClientConnect    write fOnClientConnect;
          property OnClientDisconnect : TSocketNotifyEvent read fOnClientDisconnect write fOnClientDisconnect;
          property OnClientRead       : TSocketNotifyEvent read fOnClientRead       write fOnClientRead;
          property OnClientWrite      : TSocketNotifyEvent read fOnClientWrite      write fOnClientWrite;
          property OnClientError      : TSocketErrorEvent  read fOnClientError      write fOnClientError;
      end;

    TServerAcceptThread =
      class(TThread)
        private
          fServerSocket: TServerWinSocket;
        public
          constructor Create(CreateSuspended : boolean; aSocket : TServerWinSocket);
          procedure   Execute; override;
        public
          property ServerSocket: TServerWinSocket read FServerSocket;
      end;

    TServerClientThread =
      class(TThread)
        private
          fClientSocket : TServerClientWinSocket;
          fServerSocket : TServerWinSocket;
          fException    : Exception;
          fEvent        : TSimpleEvent;
          fKeepInCache  : boolean;
          fData         : pointer;
          procedure HandleEvent(Sender : TObject; Socket : TCustomWinSocket; SocketEvent : TSocketEvent);
          procedure HandleError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
          procedure DoHandleException;
          procedure DoRead;
          procedure DoWrite;
        protected
          procedure DoTerminate; override;
          procedure Execute; override;
          procedure ClientExecute; virtual;
          procedure Event(SocketEvent : TSocketEvent); virtual;
          procedure Error(ErrorEvent : TErrorEvent; var ErrorCode: integer); virtual;
          procedure HandleException; virtual;
          procedure ReActivate(aSocket : TServerClientWinSocket);
          function  StartConnect : boolean;
          function  EndConnect   : boolean;
        public
          constructor Create(CreateSuspended : boolean; aSocket : TServerClientWinSocket);
          destructor  Destroy; override;
        public
          property ClientSocket : TServerClientWinSocket read fClientSocket;
          property ServerSocket : TServerWinSocket       read fServerSocket;
          property KeepInCache  : boolean                read fKeepInCache  write fKeepInCache;
          property Data         : pointer                read fData         write fData;
      end;

    TCustomSocket =
      class(TComponent)
        private
          fActive       : boolean;
          fOnLookup     : TSocketNotifyEvent;
          fOnConnect    : TSocketNotifyEvent;
          fOnConnecting : TSocketNotifyEvent;
          fOnDisconnect : TSocketNotifyEvent;
          fOnListen     : TSocketNotifyEvent;
          fOnAccept     : TSocketNotifyEvent;
          fOnRead       : TSocketNotifyEvent;
          fOnWrite      : TSocketNotifyEvent;
          fOnError      : TSocketErrorEvent;
          fPort         : Integer;
          fAddress      : string;
          fHost         : string;
          fService      : string;
          procedure DoEvent(Sender : TObject; Socket : TCustomWinSocket; SocketEvent : TSocketEvent);
          procedure DoError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
        protected
          procedure Event(Socket : TCustomWinSocket; SocketEvent: TSocketEvent); virtual;
          procedure Error(Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer); virtual;
          procedure DoActivate(Value : boolean); virtual; abstract;
          procedure Loaded; override;
          procedure SetActive(Value : boolean);
          procedure SetAddress(Value : string);
          procedure SetHost(Value : string);
          procedure SetPort(Value : integer);
          procedure SetService(Value : string);
          property Active       : boolean            read fActive       write SetActive;
          property Address      : string             read fAddress      write SetAddress;
          property Host         : string             read fHost         write SetHost;
          property Port         : integer            read fPort         write SetPort;
          property Service      : string             read fService      write SetService;
          property OnLookup     : TSocketNotifyEvent read fOnLookup     write fOnLookup;
          property OnConnecting : TSocketNotifyEvent read fOnConnecting write fOnConnecting;
          property OnConnect    : TSocketNotifyEvent read fOnConnect    write fOnConnect;
          property OnDisconnect : TSocketNotifyEvent read fOnDisconnect write fOnDisconnect;
          property OnListen     : TSocketNotifyEvent read fOnListen     write fOnListen;
          property OnAccept     : TSocketNotifyEvent read fOnAccept     write fOnAccept;
          property OnRead       : TSocketNotifyEvent read fOnRead       write fOnRead;
          property OnWrite      : TSocketNotifyEvent read fOnWrite      write fOnWrite;
          property OnError      : TSocketErrorEvent  read fOnError      write fOnError;
        public
          procedure Open;
          procedure Close;
      end;

{    TWinSocketStream = class(TStream)
    private
      FSocket: TCustomWinSocket;
      FTimeout: Longint;
      FEvent: TSimpleEvent;
    public
      constructor Create(ASocket: TCustomWinSocket; TimeOut: Longint);
      destructor Destroy; override;
      function WaitForData(Timeout: Longint): boolean;
      function Read(var Buffer; Count: Longint): Longint; override;
      function Write(const Buffer; Count: Longint): Longint; override;
      function Seek(Offset: Longint; Origin: Word): Longint; override;
      property TimeOut: Longint read FTimeout write FTimeout;
    end; }

    TClientSocket =
      class(TCustomSocket)
        private
          fClientSocket : TClientWinSocket;
        protected
          procedure DoActivate(Value : boolean); override;
          function  GetClientType : TClientType;
          procedure SetClientType(Value : TClientType);
        public
          constructor Create(aOwner : TComponent); override;
          destructor  Destroy; override;
        public
          property Socket : TClientWinSocket read FClientSocket;
        published
          property Active;
          property Address;
          property ClientType : TClientType read GetClientType write SetClientType;
          property Host;
          property Port;
          property Service;
          property OnLookup;
          property OnConnecting;
          property OnConnect;
          property OnDisconnect;
          property OnRead;
          property OnWrite;
          property OnError;
      end;

    TCustomServerSocket =
      class(TCustomSocket)
        protected
          fServerSocket : TServerWinSocket;
          procedure DoActivate(Value : boolean); override;
          function  GetServerType : TServerType;
          function  GetGetThreadEvent : TGetThreadEvent;
          function  GetGetSocketEvent : TGetSocketEvent;
          function  GetThreadCacheSize : integer;
          function  GetOnThreadStart : TThreadNotifyEvent;
          function  GetOnThreadEnd : TThreadNotifyEvent;
          function  GetOnClientEvent(Index : integer): TSocketNotifyEvent;
          function  GetOnClientError : TSocketErrorEvent;
          procedure SetServerType(Value : TServerType);
          procedure SetGetThreadEvent(Value : TGetThreadEvent);
          procedure SetGetSocketEvent(Value : TGetSocketEvent);
          procedure SetThreadCacheSize(Value : integer);
          procedure SetOnThreadStart(Value : TThreadNotifyEvent);
          procedure SetOnThreadEnd(Value : TThreadNotifyEvent);
          procedure SetOnClientEvent(Index : integer; Value : TSocketNotifyEvent);
          procedure SetOnClientError(Value : TSocketErrorEvent);
        protected
          property ServerType         : TServerType        read GetServerType      write SetServerType;
          property ThreadCacheSize    : integer            read GetThreadCacheSize write SetThreadCacheSize;
          property OnGetThread        : TGetThreadEvent    read GetGetThreadEvent  write SetGetThreadEvent;
          property OnGetSocket        : TGetSocketEvent    read GetGetSocketEvent  write SetGetSocketEvent;
          property OnThreadStart      : TThreadNotifyEvent read GetOnThreadStart   write SetOnThreadStart;
          property OnThreadEnd        : TThreadNotifyEvent read GetOnThreadEnd     write SetOnThreadEnd;
          property OnClientConnect    : TSocketNotifyEvent index 2 read GetOnClientEvent write SetOnClientEvent;
          property OnClientDisconnect : TSocketNotifyEvent index 3 read GetOnClientEvent write SetOnClientEvent;
          property OnClientRead       : TSocketNotifyEvent index 0 read GetOnClientEvent write SetOnClientEvent;
          property OnClientWrite      : TSocketNotifyEvent index 1 read GetOnClientEvent write SetOnClientEvent;
          property OnClientError      : TSocketErrorEvent          read GetOnClientError write SetOnClientError;
        public
          destructor Destroy; override;
      end;

    TServerSocket =
      class(TCustomServerSocket)
        public
          constructor Create(aOwner: TComponent); override;
        public
          property Socket : TServerWinSocket read fServerSocket;
        published
          property Active;
          property Port;
          property Service;
          property ServerType;
          property ThreadCacheSize default 10;
          property OnListen;
          property OnAccept;
          property OnGetThread;
          property OnGetSocket;
          property OnThreadStart;
          property OnThreadEnd;
          property OnClientConnect;
          property OnClientDisconnect;
          property OnClientRead;
          property OnClientWrite;
          property OnClientError;
      end;

  threadvar
    SocketErrorProc : procedure (ErrorCode : integer);

  var
    ByteSent : integer = 0;
    ByteRecv : integer = 0;

implementation

  uses
    Consts, Forms, WebConst, MathUtils;

  var
    WSAData: TWSAData;

  function TCPIPBufSize(Socket : TSocket) : integer;
    var
      sz : integer;
    begin
      sz := sizeof(Result);
      getsockopt(Socket, SOL_SOCKET, SO_SNDBUF, @Result, sz)
    end;

  function ConnectionLost(Error : integer) : boolean;
    begin
      case Error of
        WSAEOPNOTSUPP, WSAESHUTDOWN, WSAECONNABORTED, WSAENETRESET, WSAECONNRESET:
          Result := true;
        else
          Result := false
      end
    end;

  function CheckSocketResult(ResultCode : integer; const Op : string) : integer;
    begin
      if ResultCode <> 0
        then
          begin
            Result := WSAGetLastError;
            if Result <> WSAEWOULDBLOCK then
              if Assigned(SocketErrorProc)
                then SocketErrorProc(Result);
          end
        else
          Result := 0
    end;

  procedure Startup;
    var
      ErrorCode: Integer;
    begin
      ErrorCode := WSAStartup($0101, WSAData);
      if ErrorCode <> 0
        then
          raise ESocketError.CreateFmt(sWindowsSocketError,
            [SysErrorMessage(ErrorCode), ErrorCode, 'WSAStartup']);
    end;

  procedure Cleanup;
    var
      ErrorCode: Integer;
    begin
      ErrorCode := WSACleanup;
      if ErrorCode <> 0
        then
          raise ESocketError.CreateFmt(sWindowsSocketError,
            [SysErrorMessage(ErrorCode), ErrorCode, 'WSACleanup']);
    end;

  { TCustomWinSocket }

  constructor TCustomWinSocket.Create(aSocket: TSocket);
    begin
      inherited Create;
      Startup;
      fSpoolBuffer := TCollection.Create(0, rkBelonguer);
      fSocketLock := TCriticalSection.Create;
      fAsyncStyles := [ asRead, asWrite, asConnect, asClose ];
      fSocket := aSocket;
      fAddr.sin_family := PF_INET;
      fAddr.sin_addr.s_addr := INADDR_ANY;
      fAddr.sin_port := 0;
      fConnected := fSocket <> INVALID_SOCKET;
      if fConnected
        then fTCPBufSize := TCPIPBufSize(fSocket)
    end;

  destructor TCustomWinSocket.Destroy;
    begin
      fOnSocketEvent := nil;  { disable events }
      if fConnected and (fSocket <> INVALID_SOCKET)
        then Disconnect(fSocket);
      if FHandle <> 0
        then
          DeallocateHWnd(fHandle);
      fSocketLock.Free;
      fSpoolBuffer.Free;
      Cleanup;
      inherited Destroy
    end;

  procedure TCustomWinSocket.Accept(Socket: TSocket);
    begin
    end;

  procedure TCustomWinSocket.Close;
    begin
      Disconnect(fSocket)
    end;

  procedure TCustomWinSocket.Connect(Socket: TSocket);
    begin
    end;

  procedure TCustomWinSocket.Lock;
    begin
      fSocketLock.Enter
    end;

  procedure TCustomWinSocket.Unlock;
    begin
      fSocketLock.Leave
    end;

  procedure TCustomWinSocket.CMSocketMessage(var Message : TCMSocketMessage);

    function CheckError: boolean;
      var
        ErrorEvent: TErrorEvent;
        ErrorCode: Integer;
      begin
        if Message.SelectError <> 0
          then
            begin
              result := false;
              if ConnectionLost(Message.SelectError)
                then Disconnect(Message.Socket)
                else
                  begin
                    ErrorCode := Message.SelectError;
                    case Message.SelectEvent of
                      FD_CONNECT:
                        ErrorEvent := eeConnect;
                      FD_CLOSE:
                        ErrorEvent := eeDisconnect;
                      FD_READ:
                        ErrorEvent := eeReceive;
                      FD_WRITE:
                        ErrorEvent := eeSend;
                      FD_ACCEPT:
                        ErrorEvent := eeAccept;
                      else
                        ErrorEvent := eeGeneral;
                    end;
                    Error(Self, ErrorEvent, ErrorCode);
                  end;
            end
          else
            Result := True
      end;

  begin
    with Message do
      if CheckError
        then
          case SelectEvent of
            FD_CONNECT:
              Connect(Socket);
            FD_CLOSE:
              Disconnect(Socket);
            FD_READ:
              Read(Socket);
            FD_WRITE:
              Write(Socket);
            FD_ACCEPT:
              Accept(Socket)
          end
  end;

  procedure TCustomWinSocket.CMDeferFree(var Message);
    begin
      Free;
    end;

  procedure TCustomWinSocket.DeferFree;
    begin
      if FHandle <> 0
        then
          PostMessage(fHandle, CM_DEFERFREE, 0, 0)
    end;

  procedure TCustomWinSocket.DoSetAsyncStyles;
    var
      Msg      : integer;
      Wnd      : HWnd;
      Blocking : Longint;
    begin
      Msg := 0;
      Wnd := 0;
      if FAsyncStyles <> []
        then
          begin
            Msg := CM_SOCKETMESSAGE;
            Wnd := Handle;
          end;
      WSAAsyncSelect(fSocket, Wnd, Msg, longint(byte(fAsyncStyles)));
      if fASyncStyles = []
        then
          begin
            Blocking := 0;
            ioctlsocket(fSocket, FIONBIO, Blocking);
          end
    end;

  function TCustomWinSocket.GetHandle : HWnd;
    begin
      if fHandle = 0
        then
          fHandle := AllocateHwnd(WndProc);
      Result := fHandle
    end;

  function TCustomWinSocket.GetLocalAddress : string;
    var
      SockAddrIn : TSockAddrIn;
      Size       : integer;
    begin
      Lock;
      try
        Result := '';
        if fSocket <> INVALID_SOCKET
          then
            begin
              Size := SizeOf(SockAddrIn);
              if getsockname(fSocket, SockAddrIn, Size) = 0
                then
                  Result := inet_ntoa(SockAddrIn.sin_addr)
            end
      finally
        Unlock
      end
    end;

  function TCustomWinSocket.GetLocalHost : string;
    var
      LocalName : array[ 0 .. 255 ] of char;
    begin
      Lock;
      try
        Result := '';
        if fSocket <> INVALID_SOCKET
          then
            if gethostname(LocalName, SizeOf(LocalName)) = 0
              then
                Result := LocalName
      finally
        Unlock
      end
    end;

  function TCustomWinSocket.GetLocalPort: integer;
    var
      SockAddrIn : TSockAddrIn;
      Size       : Integer;
    begin
      Lock;
      try
        Result := -1;
        if fSocket <> INVALID_SOCKET
          then
            begin
              Size := SizeOf(SockAddrIn);
              if getsockname(FSocket, SockAddrIn, Size) = 0
                then
                  Result := ntohs(SockAddrIn.sin_port)
            end
      finally
        Unlock
      end
    end;

  function TCustomWinSocket.GetRemoteHost: string;
    var
      SockAddrIn : TSockAddrIn;
      Size       : integer;
      HostEnt    : PHostEnt;
    begin
      Lock;
      try
        Result := '';
        if fConnected
          then
            begin
              Size := SizeOf(SockAddrIn);
              CheckSocketResult(getpeername(fSocket, SockAddrIn, Size), 'getpeername');
              HostEnt := gethostbyaddr(@SockAddrIn.sin_addr.s_addr, 4, PF_INET);
              if HostEnt <> nil
                then
                  Result := HostEnt.h_name
            end
      finally
        Unlock
      end
    end;

  function TCustomWinSocket.GetRemoteAddress: string;
    var
      SockAddrIn : TSockAddrIn;
      Size       : integer;
    begin
      Lock;
      try
        Result := '';
        if fConnected
          then
            begin
              Size := SizeOf(SockAddrIn);
              CheckSocketResult(getpeername(fSocket, SockAddrIn, Size), 'getpeername');
              Result := inet_ntoa(SockAddrIn.sin_addr)
            end
      finally
        Unlock
      end
    end;

  function TCustomWinSocket.CheckLastErrorSeverity : boolean;
    begin
      if ConnectionLost(WSAGetLastError)
        then
          begin
            Disconnect(fSocket);
            result := true
          end
        else result := false;
    end;

  function TCustomWinSocket.ClearSpool : boolean;

    function SendPacket(Packet : TSpooledPacket) : boolean;
      var
        TotalSent : integer;
        BytesSent : integer;
        toSend    : integer;
        continue  : boolean;
      begin
        continue  := true;
        TotalSent := 0;
        while continue and (Packet.Count > 0) and (TotalSent < fTCPBufSize) do
          begin
            toSend    := min(Packet.Count, fTCPBufSize - TotalSent);
            BytesSent := send(fSocket, Packet.Buffer^, toSend, 0);
            if BytesSent <> SOCKET_ERROR
              then
                begin
                  Packet.Move(BytesSent);
                  inc(TotalSent, BytesSent);
                end
              else continue := false;
          end;
        result := Packet.Count = 0;
      end;

    var
      Packet  : TSpooledPacket;

    begin
      Lock;
      try
        if fConnected
          then
            begin
              Packet := TSpooledPacket(fSpoolBuffer[0]);
              SendPacket(Packet);
              if Packet.Count = 0
                then fSpoolBuffer.AtDelete(0);
              result := true;
              {
              SndFail := false;
              repeat
                Packet := TSpooledPacket(fSpoolBuffer[ 0 ]);
                if SendPacket(Packet)
                  then
                    if Packet.Count = 0
                      then fSpoolBuffer.AtDelete(0)
                      else fWriteEnabled := false
                  else SndFail := true
              until not fWriteEnabled or SndFail or (fSpoolBuffer.Count = 0);
              if SndFail
                then CheckLastErrorSeverity;
              result := fSpoolBuffer.Count = 0;
              }
            end
          else result := false;
      finally
        Unlock
      end
   end;

  procedure TCustomWinSocket.AsyncSelectSocket;
    begin
      WSAAsyncSelect(fSocket, Handle, CM_SOCKETMESSAGE, longint(byte (fAsyncStyles - [ asConnect ])))
    end;

  function TCustomWinSocket.GetRemotePort: Integer;
    var
      SockAddrIn : TSockAddrIn;
      Size       : integer;
    begin
      Lock;
      try
        Result := 0;
        if fConnected
          then
            begin
              Size := SizeOf(SockAddrIn);
              CheckSocketResult(getpeername(fSocket, SockAddrIn, Size), 'getpeername');
              Result := ntohs(SockAddrIn.sin_port)
            end
      finally
        Unlock
      end
    end;

  function TCustomWinSocket.GetRemoteAddr: TSockAddrIn;
    var
      Size: Integer;
    begin
      Lock;
      try
        FillChar(Result, SizeOf(Result), 0);
        if fConnected then
          begin
            Size := SizeOf(Result);
            if getpeername(fSocket, Result, Size) <> 0
              then
                FillChar(Result, SizeOf(Result), 0)
          end
      finally
        Unlock
      end
    end;

  function TCustomWinSocket.LookupName(const Name : string) : TInAddr;
    var
      HostEnt : PHostEnt;
      InAddr  : TInAddr;
    begin
      HostEnt := gethostbyname(PChar(Name));
      FillChar(InAddr, SizeOf(InAddr), 0);
      if HostEnt <> nil
        then
          begin
            with InAddr, HostEnt^ do
              begin
                S_un_b.s_b1 := h_addr^[ 0 ];
                S_un_b.s_b2 := h_addr^[ 1 ];
                S_un_b.s_b3 := h_addr^[ 2 ];
                S_un_b.s_b4 := h_addr^[ 3 ]
              end
          end;
      Result := InAddr
    end;

  function TCustomWinSocket.LookupService(const Service : string) : integer;
    var
      ServEnt : PServEnt;
    begin
      ServEnt := getservbyname(PChar(Service), 'tcp');
      if ServEnt <> nil
        then
          Result := ntohs(ServEnt.s_port)
        else
          Result := 0
    end;

  function TCustomWinSocket.InitSocket(var Name, Address, Service : string; Port : Word; Client : boolean) : TSockAddrIn;
    begin
      Result.sin_family := PF_INET;
      if Name <> ''
        then
          Result.sin_addr := LookupName(Name)
        else
          if Address <> ''
            then
              Result.sin_addr.s_addr := inet_addr(PChar(Address))
            else
              if not Client
                then
                  Result.sin_addr.s_addr := INADDR_ANY
                else
                  raise ESocketError.Create(sNoAddress); // >> Sospechoso
      if Service <> ''
        then
          Result.sin_port := htons(LookupService(Service))
        else
          Result.sin_port := htons(Port)
    end;

  procedure TCustomWinSocket.Listen(var Name, Address, Service: string; Port: Word; QueueSize : integer);
    var
      SockAddrIn: TSockAddrIn;
    begin
      if fConnected
        then
          raise ESocketError.Create(sCannotListenOnOpen);
      fSocket := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
      if FSocket = INVALID_SOCKET
        then
          raise ESocketError.Create(sCannotCreateSocket);
      try
        SockAddrIn := InitSocket(Name, Address, Service, Port, False);
        CheckSocketResult(bind(fSocket, SockAddrIn, SizeOf(SockAddrIn)), 'bind');
        DoSetASyncStyles;
        if QueueSize > SOMAXCONN
          then
            QueueSize := SOMAXCONN;
        Event(Self, seListen);
        CheckSocketResult(Winsock.listen(fSocket, QueueSize), 'listen');
        fConnected := True;
        fTCPBufSize := TCPIPBufSize(fSocket)
      except
        Disconnect(fSocket);
        raise
      end
    end;

  procedure TCustomWinSocket.Open(var Name, Address, Service: string; Port : Word);
    var
      SockAddrIn: TSockAddrIn;
    begin
      if fConnected
        then
          raise ESocketError.Create(sSocketAlreadyOpen);
      fSocket := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
      if fSocket = INVALID_SOCKET
        then
          raise ESocketError.Create(sCannotCreateSocket);
      try
        Event(Self, seLookUp);
        SockAddrIn := InitSocket(Name, Address, Service, Port, True);
        DoSetASyncStyles;
        Event(Self, seConnecting);
        CheckSocketResult(WinSock.connect(fSocket, SockAddrIn, SizeOf(SockAddrIn)), 'connect');
        if not (asConnect in fAsyncStyles)
          then
            begin
              fConnected := fSocket <> INVALID_SOCKET;
              Event(Self, seConnect)
            end;
        fTCPBufSize := TCPIPBufSize(fSocket)
      except
        Disconnect(fSocket);
        raise
      end
    end;

  procedure TCustomWinSocket.Disconnect(Socket : TSocket);
    var
      wasConnected : boolean;
    begin
      wasConnected := fConnected;
      if (Socket <> INVALID_SOCKET) and (Socket = fSocket)
        then
          begin
            Lock;
            try
              fConnected := false;
              fSocket := INVALID_SOCKET;
              fSpoolBuffer.DeleteAll;
              closesocket(Socket);
            finally
              Unlock;
            end;
            if wasConnected
              then Event(Self, seDisconnect);
          end;
    end;

  procedure TCustomWinSocket.DefaultHandler(var Message);
    begin
      with TMessage(Message) do
        if FHandle <> 0
          then Result := CallWindowProc(@DefWindowProc, fHandle, Msg, wParam, lParam);
    end;

  procedure TCustomWinSocket.Event(Socket : TCustomWinSocket; SocketEvent : TSocketEvent);
    begin
      if Assigned(fOnSocketEvent)
        then fOnSocketEvent(Self, Socket, SocketEvent);
    end;

  procedure TCustomWinSocket.Error(Socket:  TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
    begin
      if Assigned(fOnErrorEvent)
        then fOnErrorEvent(Self, Socket, ErrorEvent, ErrorCode)
    end;

  procedure TCustomWinSocket.SendText(const s: string);
    begin
      SendBuf(pointer(S)^, Length(S))
    end;

  procedure TCustomWinSocket.SendEncryptedText(const S : string);
    begin
      SendBuf(pointer(S)^, Length(S))
    end;

  function TCustomWinSocket.SendBuf(var Buf; Count: Integer): Integer;
    begin
      Lock;
      if Count > 0
        then inc(ByteSent, Count);
      try
        if fConnected and (Count > 0)
          then
            begin
              fSpoolBuffer.Insert(TSpooledPacket.Create(PChar(@Buf), Count, true));
              AsyncSelectSocket;
              result := Count;
            end
          else result := 0;
      finally
        Unlock
      end
    end;

  procedure TCustomWinSocket.SetAsyncStyles(Value : TASyncStyles);
    begin
      if Value <> fASyncStyles
        then
          begin
            fASyncStyles := Value;
            if FSocket <> INVALID_SOCKET
              then
                DoSetAsyncStyles
          end
    end;

  procedure TCustomWinSocket.Read(Socket : TSocket);
    begin
      if (fSocket <> INVALID_SOCKET) and (Socket = FSocket)
        then
          Event(Self, seRead)
    end;

  function TCustomWinSocket.ReceiveBuf(var Buf; Count: Integer) : Integer;
    var
      ErrorCode: Integer;
    begin
      Lock;
      if Count > 0
        then inc(ByteRecv, Count);
      try
        Result := 0;
        if (Count = -1) and fConnected
          then
            ioctlsocket(fSocket, FIONREAD, Longint(Result))
          else
            begin
              if not FConnected then Exit;
              Result := recv(FSocket, Buf, Count, 0);
              if Result = SOCKET_ERROR then
                begin
                  ErrorCode := WSAGetLastError;
                  if ErrorCode <> WSAEWOULDBLOCK
                    then
                      begin
                        Error(Self, eeReceive, ErrorCode);
                        Disconnect(FSocket);
                        {
                        if ErrorCode <> 0
                          then
                            raise ESocketError.CreateFmt(sWindowsSocketError, [ SysErrorMessage(ErrorCode), ErrorCode, 'recv' ])
                        }
                      end
                end
            end
      finally
        Unlock
      end
    end;

  function TCustomWinSocket.ReceiveLength : integer;
    begin
      Result := ReceiveBuf(pointer(nil)^, -1)
    end;

  function TCustomWinSocket.ReceiveText : string;
    begin
      SetLength(Result, ReceiveBuf(pointer(nil)^, -1));
      ReceiveBuf(Pointer(Result)^, Length(Result))
    end;

  procedure TCustomWinSocket.WndProc(var Message : TMessage);
    begin
      try
        Dispatch(Message)
      except
        Application.HandleException(Self)
      end
    end;

  procedure TCustomWinSocket.Write(Socket : TSocket);
    begin
      Lock;
      try
        fWriteEnabled := true;
        if (fSocket <> INVALID_SOCKET) and (Socket = fSocket)
          then
            if fSpoolBuffer.Count > 0
              then
                begin
                  Event(Self, seWrite);
                  ClearSpool;
                  AsyncSelectSocket
                end
              else Event(Self, seWrite)
      finally
        Unlock;
      end;
    end;

  { TClientWinSocket }

  procedure TClientWinSocket.Connect(Socket : TSocket);
    begin
      inherited;
      fConnected := True;
      Event(Self, seConnect)
    end;

  procedure TClientWinSocket.SetClientType(Value : TClientType);
    begin
      if Value <> FClientType
        then
          if not fConnected
            then
              begin
                fClientType := Value;
                if fClientType = ctBlocking
                  then
                    ASyncStyles := []
                  else
                    ASyncStyles := [ asRead, asWrite, asConnect, asClose ]
              end
            else
              raise ESocketError.Create(sCantChangeWhileActive)
    end;

  { TServerClientWinSocket }

  constructor TServerClientWinSocket.Create(Socket : TSocket; ServerWinSocket : TServerWinSocket);
    begin
      fServerWinSocket := ServerWinSocket;
      if Assigned(fServerWinSocket)
        then
          begin
            fServerWinSocket.AddClient(Self);
            if FServerWinSocket.AsyncStyles <> []
              then
                OnSocketEvent := FServerWinSocket.ClientEvent;
            OnErrorEvent := fServerWinSocket.ClientError // added by Pepe to fix a bug in the original code
          end;
      inherited Create(Socket);
      if fServerWinSocket.ASyncStyles <> []
        then
          DoSetAsyncStyles;
      if fConnected
        then
          Event(Self, seConnect)
    end;

  destructor TServerClientWinSocket.Destroy;
    begin
      if Assigned(fServerWinSocket)
        then
          fServerWinSocket.RemoveClient(Self);
      inherited Destroy
    end;

  { TServerWinSocket }

  constructor TServerWinSocket.Create(ASocket: TSocket);
    begin
      fConnections := TCollection.Create(0, rkUse);
      fActiveThreads := TList.Create;
      fListLock := TCriticalSection.Create;
      inherited Create(aSocket);
      fAsyncStyles := [ asAccept ]
    end;

  destructor TServerWinSocket.Destroy;
    begin
      inherited Destroy;
      fConnections.Free;
      fActiveThreads.Free;
      fListLock.Free
    end;

  procedure TServerWinSocket.AddClient(AClient: TServerClientWinSocket);
    begin
      fListLock.Enter;
      try
        if fConnections.IndexOf(aClient) < 0 then
          fConnections.Insert(aClient);
      finally
        fListLock.Leave;
      end
    end;

  procedure TServerWinSocket.RemoveClient(AClient: TServerClientWinSocket);
    begin
      fListLock.Enter;
      try
        fConnections.Extract(aClient)
      finally
        fListLock.Leave;
      end
    end;

  procedure TServerWinSocket.AddThread(AThread: TServerClientThread);
  begin
    fListLock.Enter;
    try
      if fActiveThreads.IndexOf(aThread) < 0
        then
          begin
            fActiveThreads.Add(aThread);
            if fActiveThreads.Count <= fThreadCacheSize
              then
                aThread.KeepInCache := true
          end
    finally
      fListLock.Leave
    end
  end;

  procedure TServerWinSocket.RemoveThread(aThread : TServerClientThread);
    begin
      fListLock.Enter;
      try
        fActiveThreads.Remove(AThread);
      finally
        fListLock.Leave;
      end
    end;

  procedure TServerWinSocket.ClientEvent(Sender: TObject; Socket: TCustomWinSocket;
    SocketEvent: TSocketEvent);
  begin
    case SocketEvent of
      seAccept,
      seLookup,
      seConnecting,
      seListen:
        begin end;
      seConnect: ClientConnect(Socket);
      seDisconnect: ClientDisconnect(Socket);
      seRead: ClientRead(Socket);
      seWrite: ClientWrite(Socket);
    end;
  end;

  procedure TServerWinSocket.ClientError(Sender: TObject; Socket: TCustomWinSocket;
    ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  begin
    ClientErrorEvent(Socket, ErrorEvent, ErrorCode);
  end;

  function TServerWinSocket.GetActiveConnections: Integer;
  begin
    Result := fConnections.Count;
  end;

  function TServerWinSocket.GetConnections(Index: Integer): TCustomWinSocket;
  begin
    Result := TCustomWinSocket(fConnections[Index]);
  end;

  function TServerWinSocket.GetActiveThreads: Integer;
  var
    I: Integer;
  begin
    FListLock.Enter;
    try
      Result := 0;
      for I := 0 to FActiveThreads.Count - 1 do
        if TServerClientThread(FActiveThreads[I]).ClientSocket <> nil then
          Inc(Result);
    finally
      FListLock.Leave;
    end;
  end;

  function TServerWinSocket.GetIdleThreads: Integer;
  var
    I: Integer;
  begin
    FListLock.Enter;
    try
      Result := 0;
      for I := 0 to FActiveThreads.Count - 1 do
        if TServerClientThread(FActiveThreads[I]).ClientSocket = nil then
          Inc(Result);
    finally
      FListLock.Leave;
    end;
  end;

  procedure TServerWinSocket.Accept(Socket: TSocket);
  var
    ClientSocket: TServerClientWinSocket;
    ClientWinSocket: TSocket;
    Addr: TSockAddrIn;
    Len: Integer;
    OldOpenType, NewOpenType: Integer;
  begin
    Len := SizeOf(OldOpenType);
    if getsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, PChar(@OldOpenType),
      Len) = 0 then
    try
      if FServerType = stThreadBlocking then
      begin
        NewOpenType := SO_SYNCHRONOUS_NONALERT;
        setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, PChar(@NewOpenType), Len);
      end;
      Len := SizeOf(Addr);
      ClientWinSocket := WinSock.accept(Socket, @Addr, @Len);
      if ClientWinSocket <> INVALID_SOCKET then
      begin
        ClientSocket := GetClientSocket(ClientWinSocket);
        if Assigned(FOnSocketEvent) then
          FOnSocketEvent(Self, ClientSocket, seAccept);
        if FServerType = stThreadBlocking then
        begin
          ClientSocket.ASyncStyles := [];
          GetServerThread(ClientSocket);
        end;
      end;
    finally
      Len := SizeOf(OldOpenType);
      setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, PChar(@OldOpenType), Len);
    end;
  end;

  procedure TServerWinSocket.Disconnect(Socket: TSocket);
  var
    SaveCacheSize: Integer;
  begin
    Lock;
    try
      SaveCacheSize := ThreadCacheSize;
      try
        ThreadCacheSize := 0;
        while FActiveThreads.Count > 0 do
          with TServerClientThread(FActiveThreads.Last) do
          begin
            FreeOnTerminate := False;
            Terminate;
            FEvent.SetEvent;
            if (ClientSocket <> nil) and ClientSocket.Connected then
              ClientSocket.Close;
            WaitFor;
            Free;
          end;
        fConnections.DeleteAll;
        if FServerAcceptThread <> nil then
          FServerAcceptThread.Terminate;
        inherited Disconnect(Socket);
        FServerAcceptThread.Free;
        FServerAcceptThread := nil;
      finally
        ThreadCacheSize := SaveCacheSize;
      end;
    finally
      Unlock;
    end;
  end;

  function TServerWinSocket.DoCreateThread(ClientSocket: TServerClientWinSocket): TServerClientThread;
  begin
    Result := TServerClientThread.Create(False, ClientSocket);
  end;

  procedure TServerWinSocket.Listen(var Name, Address, Service: string; Port: Word;
    QueueSize: Integer);
  begin
    inherited Listen(Name, Address, Service, Port, QueueSize);
    if FConnected and (ServerType = stThreadBlocking) then
      FServerAcceptThread := TServerAcceptThread.Create(False, Self);
  end;

  procedure TServerWinSocket.SetServerType(Value: TServerType);
  begin
    if Value <> FServerType then
      if not FConnected then
      begin
        FServerType := Value;
        if FServerType = stThreadBlocking then
          ASyncStyles := []
        else ASyncStyles := [asAccept];
      end else raise ESocketError.Create(sCantChangeWhileActive);
  end;

  procedure TServerWinSocket.SetThreadCacheSize(Value: Integer);
  var
    Start, I: Integer;
  begin
    if Value <> FThreadCacheSize then
    begin
      if Value < FThreadCacheSize then
        Start := Value
      else Start := FThreadCacheSize;
      FThreadCacheSize := Value;
      FListLock.Enter;
      try
        for I := 0 to FActiveThreads.Count - 1 do
          with TServerClientThread(FActiveThreads[I]) do
            KeepInCache := I < Start;
      finally
        FListLock.Leave;
      end;
    end;
  end;

  function TServerWinSocket.GetClientSocket(Socket: TSocket): TServerClientWinSocket;
  begin
    Result := nil;
    if Assigned(FOnGetSocket) then FOnGetSocket(Self, Socket, Result);
    if Result = nil then
      Result := TServerClientWinSocket.Create(Socket, Self);
  end;

  procedure TServerWinSocket.ThreadEnd(AThread: TServerClientThread);
  begin
    if Assigned(FOnThreadEnd) then FOnThreadEnd(Self, AThread);
  end;

  procedure TServerWinSocket.ThreadStart(AThread: TServerClientThread);
  begin
    if Assigned(FOnThreadStart) then FOnThreadStart(Self, AThread);
  end;

  function TServerWinSocket.GetServerThread(ClientSocket: TServerClientWinSocket): TServerClientThread;
  var
    I: Integer;
  begin
    Result := nil;
    FListLock.Enter;
    try
      for I := 0 to FActiveThreads.Count - 1 do
        if TServerClientThread(FActiveThreads[I]).ClientSocket = nil then
        begin
          Result := FActiveThreads[I];
          Result.ReActivate(ClientSocket);
          Break;
        end;
    finally
      FListLock.Leave;
    end;
    if Result = nil then
    begin
      if Assigned(FOnGetThread) then FOnGetThread(Self, ClientSocket, Result);
      if Result = nil then Result := DoCreateThread(ClientSocket);
    end;
  end;

  function TServerWinSocket.GetClientThread(ClientSocket: TServerClientWinSocket): TServerClientThread;
  var
    I: Integer;
  begin
    Result := nil;
    FListLock.Enter;
    try
      for I := 0 to FActiveThreads.Count - 1 do
        if TServerClientThread(FActiveThreads[I]).ClientSocket = ClientSocket then
        begin
          Result := FActiveThreads[I];
          Break;
        end;
    finally
      FListLock.Leave;
    end;
  end;

  procedure TServerWinSocket.ClientConnect(Socket: TCustomWinSocket);
  begin
    if Assigned(FOnClientConnect) then FOnClientConnect(Self, Socket);
  end;

  procedure TServerWinSocket.ClientDisconnect(Socket: TCustomWinSocket);
    begin
      if Assigned(FOnClientDisconnect)
        then fOnClientDisconnect(Self, Socket);
      if ServerType = stNonBlocking
        then Socket.DeferFree;
    end;

  procedure TServerWinSocket.ClientRead(Socket: TCustomWinSocket);
  begin
    if Assigned(FOnClientRead) then FOnClientRead(Self, Socket);
  end;

  procedure TServerWinSocket.ClientWrite(Socket: TCustomWinSocket);
  begin
    if Assigned(FOnClientWrite) then FOnClientWrite(Self, Socket);
  end;

  procedure TServerWinSocket.ClientErrorEvent(Socket: TCustomWinSocket;
    ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  begin
    if Assigned(FOnClientError) then FOnClientError(Self, Socket, ErrorEvent, ErrorCode);
  end;

  { TServerAcceptThread }

  constructor TServerAcceptThread.Create(CreateSuspended: boolean;
    ASocket: TServerWinSocket);
  begin
    FServerSocket := ASocket;
    inherited Create(CreateSuspended);
  end;

  procedure TServerAcceptThread.Execute;
  begin
    while not Terminated do
      FServerSocket.Accept(FServerSocket.SocketHandle);
  end;

  { TServerClientThread }

  constructor TServerClientThread.Create(CreateSuspended: boolean;
    ASocket: TServerClientWinSocket);
  begin
    FreeOnTerminate := True;
    FEvent := TSimpleEvent.Create;
    inherited Create(True);
    Priority := tpHigher;
    ReActivate(ASocket);
    if not CreateSuspended then Resume;
  end;

  destructor TServerClientThread.Destroy;
  begin
    FClientSocket.Free;
    FEvent.Free;
    inherited Destroy;
  end;

  procedure TServerClientThread.ReActivate(ASocket: TServerClientWinSocket);
  begin
    FClientSocket := ASocket;
    if Assigned(FClientSocket) then
    begin
      FServerSocket := FClientSocket.ServerWinSocket;
      FServerSocket.AddThread(Self);
      FClientSocket.OnSocketEvent := HandleEvent;
      FClientSocket.OnErrorEvent := HandleError;
      FEvent.SetEvent;
    end;
  end;

  procedure TServerClientThread.DoHandleException;
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    if FException is Exception then
    begin
      Application.ShowException(FException);
    end else
      SysUtils.ShowException(FException, nil);
  end;

  procedure TServerClientThread.DoRead;
  begin
    ClientSocket.ServerWinSocket.Event(ClientSocket, seRead);
  end;

  procedure TServerClientThread.DoTerminate;
  begin
    if Assigned(FServerSocket) then
      FServerSocket.RemoveThread(Self);
  end;

  procedure TServerClientThread.DoWrite;
  begin
    FServerSocket.Event(ClientSocket, seWrite);
  end;

  procedure TServerClientThread.HandleEvent(Sender: TObject; Socket: TCustomWinSocket;
    SocketEvent: TSocketEvent);
  begin
    Event(SocketEvent);
  end;

  procedure TServerClientThread.HandleError(Sender: TObject; Socket: TCustomWinSocket;
    ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  begin
    Error(ErrorEvent, ErrorCode);
  end;

  procedure TServerClientThread.Event(SocketEvent: TSocketEvent);
  begin
    FServerSocket.ClientEvent(Self, ClientSocket, SocketEvent);
  end;

  procedure TServerClientThread.Error(ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  begin
    FServerSocket.ClientError(Self, ClientSocket, ErrorEvent, ErrorCode);
  end;

  procedure TServerClientThread.HandleException;
  begin
    FException := Exception(ExceptObject);
    try
      if not (FException is EAbort) then
        Synchronize(DoHandleException);
    finally
      FException := nil;
    end;
  end;

  procedure TServerClientThread.Execute;
  begin
    FServerSocket.ThreadStart(Self);
    try
      try
        while True do
        begin
          if StartConnect then ClientExecute;
          if EndConnect then Break;
        end;
      except
        HandleException;
        KeepInCache := False;
      end;
    finally
      FServerSocket.ThreadEnd(Self);
    end;
  end;

  procedure TServerClientThread.ClientExecute;
  var
    FDSet: TFDSet;
    TimeVal: TTimeVal;
  begin
    while not Terminated and ClientSocket.Connected do
    begin
      FD_ZERO(FDSet);
      FD_SET(ClientSocket.SocketHandle, FDSet);
      TimeVal.tv_sec := 0;
      TimeVal.tv_usec := 500;
      if (select(0, @FDSet, nil, nil, @TimeVal) > 0) and not Terminated then
        if ClientSocket.ReceiveBuf(FDSet, -1) = 0 then Break
        else Synchronize(DoRead);
      if (select(0, nil, @FDSet, nil, @TimeVal) > 0) and not Terminated then
        Synchronize(DoWrite);
    end;
  end;

  function TServerClientThread.StartConnect: boolean;
  begin
    if FEvent.WaitFor(INFINITE) = wrSignaled then
      FEvent.ResetEvent;
    Result := not Terminated;
  end;

  function TServerClientThread.EndConnect: boolean;
  begin
    FClientSocket.Free;
    FClientSocket := nil;
    Result := Terminated or not KeepInCache;
  end;

  { TCustomSocket }

  procedure TCustomSocket.DoEvent(Sender: TObject; Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
    begin
      Event(Socket, SocketEvent);
    end;

  procedure TCustomSocket.DoError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    begin
      Error(Socket, ErrorEvent, ErrorCode);
    end;

  procedure TCustomSocket.Event(Socket: TCustomWinSocket; SocketEvent: TSocketEvent);
    begin
      case SocketEvent of
        seLookup: if Assigned(FOnLookup) then FOnLookup(Self, Socket);
        seConnecting: if Assigned(FOnConnecting) then FOnConnecting(Self, Socket);
        seConnect:
          begin
            FActive := True;
            if Assigned(FOnConnect) then FOnConnect(Self, Socket);
          end;
        seListen:
          begin
            FActive := True;
            if Assigned(FOnListen) then FOnListen(Self, Socket);
          end;
        seDisconnect:
          begin
            FActive := False;
            if Assigned(FOnDisconnect) then FOnDisconnect(Self, Socket);
          end;
        seAccept: if Assigned(FOnAccept) then FOnAccept(Self, Socket);
        seRead: if Assigned(FOnRead) then FOnRead(Self, Socket);
        seWrite: if Assigned(FOnWrite) then FOnWrite(Self, Socket);
      end;
    end;

  procedure TCustomSocket.Error(Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : Integer);
    begin
      if Assigned(fOnError)
        then
          fOnError(Self, Socket, ErrorEvent, ErrorCode)
    end;

  procedure TCustomSocket.SetActive(Value: boolean);
    begin
      if Value <> fActive
        then
          begin
            if (csDesigning in ComponentState) or (csLoading in ComponentState)
              then
                fActive := Value;
            if not (csLoading in ComponentState)
              then
                DoActivate(Value)
          end
    end;

  procedure TCustomSocket.Loaded;
    begin
      inherited Loaded;
      DoActivate(fActive)
    end;

  procedure TCustomSocket.SetAddress(Value: string);
    begin
      if CompareText(Value, FAddress) <> 0
        then
          begin
            if not (csLoading in ComponentState) and fActive
              then
                raise ESocketError.Create(sCantChangeWhileActive);
            fAddress := Value
          end
    end;

  procedure TCustomSocket.SetHost(Value: string);
    begin
      if CompareText(Value, fHost) <> 0
        then
          begin
            if not (csLoading in ComponentState) and fActive
              then
                raise ESocketError.Create(sCantChangeWhileActive);
            fHost := Value
          end
    end;

  procedure TCustomSocket.SetPort(Value: Integer);
    begin
      if FPort <> Value
        then
          begin
            if not (csLoading in ComponentState) and FActive
              then
                raise ESocketError.Create(sCantChangeWhileActive);
            fPort := Value
          end
    end;

  procedure TCustomSocket.SetService(Value: string);
    begin
      if CompareText(Value, FService) <> 0
        then
          begin
            if not (csLoading in ComponentState) and FActive
              then
                raise ESocketError.Create(sCantChangeWhileActive);
            fService := Value
          end
    end;

  procedure TCustomSocket.Open;
    begin
      Active := True
    end;

  procedure TCustomSocket.Close;
    begin
      Active := False
    end;

{ TWinSocketStream }

{constructor TWinSocketStream.Create(ASocket: TCustomWinSocket; TimeOut: Longint);
begin
  if ASocket.ASyncStyles <> [] then
    raise ESocketError.Create(sSocketMustBeBlocking);
  FSocket := ASocket;
  FTimeOut := TimeOut;
  FEvent := TSimpleEvent.Create;
  inherited Create;
end;

destructor TWinSocketStream.Destroy;
begin
  FEvent.Free;
  inherited Destroy;
end;

function TWinSocketStream.WaitForData(Timeout: Longint): boolean;
var
  FDSet: TFDSet;
  TimeVal: TTimeVal;
begin
  TimeVal.tv_sec := Timeout div 1000;
  TimeVal.tv_usec := (Timeout mod 1000) * 1000;
  FD_ZERO(FDSet);
  FD_SET(FSocket.SocketHandle, FDSet);
  Result := select(0, @FDSet, nil, nil, @TimeVal) > 0;
end;

function TWinSocketStream.Read(var Buffer; Count: Longint): Longint;
var
  Overlapped: TOverlapped;
  ErrorCode: Integer;
begin
  FSocket.Lock;
  try
    FillChar(OVerlapped, SizeOf(Overlapped), 0);
    Overlapped.hEvent := FEvent.Handle;
    if not ReadFile(FSocket.SocketHandle, Buffer, Count, Integer(Result),
      @Overlapped) and (GetLastError <> ERROR_IO_PENDING) then
    begin
      ErrorCode := GetLastError;
      raise ESocketError.CreateFmt(sSocketIOError, [sSocketRead, ErrorCode,
        SysErrorMessage(ErrorCode)]);
    end;
    if FEvent.WaitFor(FTimeOut) <> wrSignaled then
      Result := 0
    else
    begin
      GetOverlappedResult(FSocket.SocketHandle, Overlapped, Integer(Result), False);
      FEvent.ResetEvent;
    end;
  finally
    FSocket.Unlock;
  end;
end;

function TWinSocketStream.Write(const Buffer; Count: Longint): Longint;
var
  Overlapped: TOverlapped;
  ErrorCode: Integer;
begin
  FSocket.Lock;
  try
    FillChar(OVerlapped, SizeOf(Overlapped), 0);
    Overlapped.hEvent := FEvent.Handle;
    if not WriteFile(FSocket.SocketHandle, Buffer, Count, Integer(Result),
      @Overlapped) and (GetLastError <> ERROR_IO_PENDING) then
    begin
      ErrorCode := GetLastError;
      raise ESocketError.CreateFmt(sSocketIOError, [sSocketWrite, ErrorCode,
        SysErrorMessage(ErrorCode)]);
    end;
    if FEvent.WaitFor(FTimeOut) <> wrSignaled then
      Result := 0
    else GetOverlappedResult(FSocket.SocketHandle, Overlapped, Integer(Result), False);
  finally
    FSocket.Unlock;
  end;
end;

function TWinSocketStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := 0;
end;}

  { TClientSocket }

  constructor TClientSocket.Create(AOwner: TComponent);
    begin
      inherited Create(aOwner);
      fClientSocket := TClientWinSocket.Create(INVALID_SOCKET);
      fClientSocket.OnSocketEvent := DoEvent;
      fClientSocket.OnErrorEvent := DoError
    end;

  destructor TClientSocket.Destroy;
    begin
      fClientSocket.Free;
      inherited Destroy
    end;

  procedure TClientSocket.DoActivate(Value : boolean);
    begin
      if (Value <> fClientSocket.Connected) and not (csDesigning in ComponentState)
        then
          begin
            if fClientSocket.Connected
              then
                begin
                  fClientSocket.fConnected := false;
                  fClientSocket.Disconnect(fClientSocket.fSocket);
                end
              else
                fClientSocket.Open(fHost, fAddress, fService, fPort)
          end
    end;

  function TClientSocket.GetClientType : TClientType;
    begin
      Result := fClientSocket.ClientType
    end;

  procedure TClientSocket.SetClientType(Value : TClientType);
    begin
      fClientSocket.ClientType := Value
    end;

  { TCustomServerSocket }

  destructor TCustomServerSocket.Destroy;
    begin
      fServerSocket.Free;
      inherited Destroy
    end;

  procedure TCustomServerSocket.DoActivate(Value : boolean);
    begin
      if (Value <> fServerSocket.Connected) and not (csDesigning in ComponentState)
        then
          begin
            if fServerSocket.Connected
              then
                fServerSocket.Disconnect(fServerSocket.SocketHandle)
              else
                fServerSocket.Listen(fHost, fAddress, fService, fPort, 5)
          end
    end;

  function TCustomServerSocket.GetServerType: TServerType;
    begin
      Result := fServerSocket.ServerType
    end;

  procedure TCustomServerSocket.SetServerType(Value: TServerType);
    begin
      fServerSocket.ServerType := Value
    end;

  function TCustomServerSocket.GetGetThreadEvent: TGetThreadEvent;
    begin
      Result := fServerSocket.OnGetThread
    end;

  procedure TCustomServerSocket.SetGetThreadEvent(Value: TGetThreadEvent);
    begin
      fServerSocket.OnGetThread := Value
    end;

  function TCustomServerSocket.GetGetSocketEvent: TGetSocketEvent;
    begin
      Result := fServerSocket.OnGetSocket
    end;

  procedure TCustomServerSocket.SetGetSocketEvent(Value: TGetSocketEvent);
    begin
      fServerSocket.OnGetSocket := Value
    end;

  function TCustomServerSocket.GetThreadCacheSize: Integer;
    begin
      Result := fServerSocket.ThreadCacheSize
    end;

  procedure TCustomServerSocket.SetThreadCacheSize(Value: Integer);
    begin
      fServerSocket.ThreadCacheSize := Value
    end;

  function TCustomServerSocket.GetOnThreadStart: TThreadNotifyEvent;
    begin
      Result := fServerSocket.OnThreadStart
    end;

  function TCustomServerSocket.GetOnThreadEnd: TThreadNotifyEvent;
    begin
      Result := fServerSocket.OnThreadEnd
    end;

  procedure TCustomServerSocket.SetOnThreadStart(Value: TThreadNotifyEvent);
    begin
      fServerSocket.OnThreadStart := Value
    end;

  procedure TCustomServerSocket.SetOnThreadEnd(Value: TThreadNotifyEvent);
    begin
      fServerSocket.OnThreadEnd := Value
    end;

  function TCustomServerSocket.GetOnClientEvent(Index: Integer): TSocketNotifyEvent;
    begin
      case Index of
        0: Result := FServerSocket.OnClientRead;
        1: Result := FServerSocket.OnClientWrite;
        2: Result := FServerSocket.OnClientConnect;
        3: Result := FServerSocket.OnClientDisconnect
      end
    end;

  procedure TCustomServerSocket.SetOnClientEvent(Index : Integer; Value : TSocketNotifyEvent);
    begin
      case Index of
        0: fServerSocket.OnClientRead := Value;
        1: fServerSocket.OnClientWrite := Value;
        2: fServerSocket.OnClientConnect := Value;
        3: fServerSocket.OnClientDisconnect := Value
      end
    end;

  function TCustomServerSocket.GetOnClientError: TSocketErrorEvent;
    begin
      Result := FServerSocket.OnClientError
    end;

  procedure TCustomServerSocket.SetOnClientError(Value: TSocketErrorEvent);
    begin
      fServerSocket.OnClientError := Value
    end;

  { TServerSocket }

  constructor TServerSocket.Create(aOwner : TComponent);
    begin
      inherited Create(aOwner);
      fServerSocket := TServerWinSocket.Create(INVALID_SOCKET);
      fServerSocket.OnSocketEvent := DoEvent;
      fServerSocket.OnErrorEvent := DoError;
      fServerSocket.ThreadCacheSize := 10
    end;

end.
