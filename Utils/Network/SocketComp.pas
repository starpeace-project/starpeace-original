{*******************************************************}
{                                                       }
{       Merchise's Variation of Borland Sockets         }
{                                                       }
{       Copyright (c) 1997-2000 Merchise                }
{                                                       }
{*******************************************************}

unit SocketComp;

interface

  uses
    Windows, WinSock, SysUtils, Messages, Classes, SyncObjs, SpoolPackets;

  const
    CM_SOCKETMESSAGE = WM_USER + $0001;
    CM_DEFERFREE     = WM_USER + $0002;

    EProxyError                = 99001;
    ESocksRequestFailed        = 99002;      // Request rejected or failed.
    ESocksRequestServerFailed  = 99003;      // Request rejected because SOCKS server cannot connect.
    ESocksRequestIdentFailed   = 99004;      // Request rejected because the client program and identd report different user-ids.
    ESocksUnknownError         = 99005;      // Unknown socks error.
    ESocksServerRespondError   = 99006;      // Socks server didn''t respond.
    ESocksAuthMethodError      = 99007;      // Invalid socks authentication method.
    ESocksAuthError            = 99008;      // Authentication error to socks server.

    ESocksServerGeneralError           = 99009;      // General SOCKS server failure.
    ESocksServerPermissionError        = 99010;      // Connection not allowed by ruleset.
    ESocksServerNetUnreachableError    = 99011;      // Network unreachable.
    ESocksServerHostUnreachableError   = 99012;      // Host unreachable.
    ESocksServerConnectionRefusedError = 99013;      // Connection refused.
    ESocksServerTTLExpiredError        = 99014;      // TTL expired.
    ESocksServerCommandError           = 99015;      // Command not supported.
    ESocksServerAddressError           = 99016;      // Address type not supported.
                                            
  type                                      
    ESocketError = class(Exception);

    
    TCMSocketMessage =
      record
        Msg         : Cardinal;
        Socket      : TSocket;
        SelectEvent : Word;
        SelectError : Word;
        result      : Longint;
      end;

    TSocksVersion = (svNoSocks, svSocks4, svSocks4A, svSocks5);
    TSocksAuthentication = (saNoAuthentication, saUsernamePassword);
    PSockInfo = ^TSockInfo;
    TSockInfo =
      packed record
        fAuthentication: TSocksAuthentication;
        fAddr     : TInAddr;
        fPort     : word;
        fUserID   : string;
        fPassword : string;
        fVersion  : TSocksVersion;
      end;
  
    TProxyInfo =
      packed record
        fAddr : string;
        fPort : word;
      end;
      
    TCustomWinSocket       = class;
    TCustomSocket          = class;
    TServerWinSocket       = class;
    TServerClientWinSocket = class;

    TAsyncStyles = integer;
    TSocketEvent =
      (
        seLookup,
        seConnecting,
        seConnect,
        seDisconnect,
        seListen,
        seAccept,
        seWrite,
        seRead,
        seOOB
      );
    TErrorEvent = (eeGeneral, eeSend, eeReceive, eeConnect, eeDisconnect, eeAccept, eeOOB);
    TProxyState = (pxNone, pxConnecting, pxResolved, pxAuthentication, pxResolvedHost);

    TSocketEventEvent  = procedure (Sender : TObject; Socket : TCustomWinSocket; SocketEvent : TSocketEvent) of object;
    TSocketErrorEvent  = procedure (Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer) of object;
    TGetSocketEvent    = procedure (Sender : TObject; Socket : TSocket; var ClientSocket : TServerClientWinSocket) of object;
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
          fData          : pointer;
          fSpoolBuffer   : TList;
          fTCPBufSize    : integer;
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
          procedure Event(Socket : TCustomWinSocket; SocketEvent : TSocketEvent); dynamic;
          procedure Error(Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer); dynamic;
          procedure SetAsyncStyles(Value : TASyncStyles);
          procedure Listen(var Name, Address, Service : string; Port : Word; QueueSize : integer);
          procedure Open(var Name, Address, Service : string; Port : Word);
          procedure Accept(Socket : TSocket); virtual;
          procedure Connect(Socket : TSocket); virtual;
          procedure Disconnect(Socket : TSocket); virtual;
          procedure Read(Socket : TSocket); virtual;
          procedure Write(Socket : TSocket); virtual;
          procedure ReadOOB(Socket : TSocket); virtual;
        public
          constructor Create(aSocket : TSocket);
          destructor  Destroy; override;
        public
          procedure Close;
          procedure Lock;
          procedure Unlock;
          class function  LookupName(const Name : string) : TInAddr;
          class function  LookupService(const Service : string) : integer;
          function  ReceiveLength : integer;
          function  ReceiveBuf(var Buf; Count : integer) : integer;
          function  ReceiveText : string;
          function  PeekBuf(var Buf; Count : integer) : integer;
          function  PeekText : string;
          function  SendBuf(var Buf; Count : integer) : integer;
          procedure SendText(const S : string);
          function  SendOOB(var Buf; Count : integer) : integer;
          function  ReceiveOOB(var Buf; Count : integer) : integer;
          procedure SendEncryptedText(const S : string);
        public
          property LocalHost     : string            read GetLocalHost;
          property LocalAddress  : string            read GetLocalAddress;
          property LocalPort     : integer           read GetLocalPort;
          property RemoteHost    : string            read GetRemoteHost;
          property RemoteAddress : string            read GetRemoteAddress;
          property RemotePort    : integer           read GetRemotePort;
          property RemoteAddr    : TSockAddrIn       read GetRemoteAddr;
          property Connected     : boolean           read fConnected;
          property Addr          : TSockAddrIn       read fAddr;
          property ASyncStyles   : TAsyncStyles      read fAsyncStyles     write SetAsyncStyles;
          property Handle        : HWnd              read GetHandle;
          property SocketHandle  : TSocket           read fSocket;
          property OnSocketEvent : TSocketEventEvent read fOnSocketEvent   write fOnSocketEvent;
          property OnErrorEvent  : TSocketErrorEvent read fOnErrorEvent    write fOnErrorEvent;
          property Data          : pointer           read FData            write FData;
        private
          fSockInfo      : PSockInfo;
          fProxyState    : TProxyState;
          fProxyInfo     : TProxyInfo;
        private
          procedure ConnectSock4;
          function  ResolveSock4: boolean;
          procedure ConnectSock5;
          procedure ResolveSock5;
          procedure ResolveSock5_A(Authen: boolean);
          function  ResolveSock5_B: boolean;
      end;

    TClientWinSocket =
      class(TCustomWinSocket)
        protected
          procedure Connect(Socket : TSocket); override;
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

    TServerWinSocket =
      class(TCustomWinSocket)
        private
          fConnections        : TList;
          fListLock           : TCriticalSection;
          fOnGetSocket        : TGetSocketEvent;
          fOnClientConnect    : TSocketNotifyEvent;
          fOnClientDisconnect : TSocketNotifyEvent;
          fOnClientRead       : TSocketNotifyEvent;
          fOnClientReadOOB    : TSocketNotifyEvent;
          fOnClientWrite      : TSocketNotifyEvent;
          fOnClientError      : TSocketErrorEvent;
          procedure AddClient(aClient : TServerClientWinSocket);
          procedure RemoveClient(aClient : TServerClientWinSocket);
          procedure ClientEvent(Sender : TObject; Socket : TCustomWinSocket; SocketEvent : TSocketEvent);
          procedure ClientError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
          function  GetActiveConnections : integer;
          function  GetConnections(index : integer) : TCustomWinSocket;
        protected
          procedure Accept(Socket : TSocket); override;
          procedure Disconnect(Socket : TSocket); override;
          function  GetClientSocket(Socket : TSocket) : TServerClientWinSocket; dynamic;
          procedure ClientRead(Socket : TCustomWinSocket); dynamic;
          procedure ClientReadOOB(Socket : TCustomWinSocket); dynamic;
          procedure ClientWrite(Socket : TCustomWinSocket); dynamic;
          procedure ClientConnect(Socket : TCustomWinSocket); dynamic;
          procedure ClientDisconnect(Socket : TCustomWinSocket); dynamic;
          procedure ClientErrorEvent(Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer); dynamic;
        public
          constructor Create(aSocket : TSocket);
          destructor  Destroy; override;
        public
          property ActiveConnections  : integer read GetActiveConnections;
          property Connections[index : integer] : TCustomWinSocket read GetConnections;
          property OnGetSocket        : TGetSocketEvent    read fOnGetSocket        write fOnGetSocket;
          property OnClientConnect    : TSocketNotifyEvent read fOnClientConnect    write fOnClientConnect;
          property OnClientDisconnect : TSocketNotifyEvent read fOnClientDisconnect write fOnClientDisconnect;
          property OnClientRead       : TSocketNotifyEvent read fOnClientRead       write fOnClientRead;
          property OnClientReadOOB    : TSocketNotifyEvent read fOnClientReadOOB    write fOnClientReadOOB;
          property OnClientWrite      : TSocketNotifyEvent read fOnClientWrite      write fOnClientWrite;
          property OnClientError      : TSocketErrorEvent  read fOnClientError      write fOnClientError;
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
          fOnOOB        : TSocketNotifyEvent;
          fOnWrite      : TSocketNotifyEvent;
          fOnError      : TSocketErrorEvent;
          fPort         : integer;
          fAddress      : string;
          fHost         : string;
          fService      : string;
          procedure DoEvent(Sender : TObject; Socket : TCustomWinSocket; SocketEvent : TSocketEvent);
          procedure DoError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
        protected
          procedure Event(Socket : TCustomWinSocket; SocketEvent : TSocketEvent); virtual;
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
          property OnOOB        : TSocketNotifyEvent read fOnOOB        write fOnOOB;
          property OnWrite      : TSocketNotifyEvent read fOnWrite      write fOnWrite;
          property OnError      : TSocketErrorEvent  read fOnError      write fOnError;
        public
          procedure Open;
          procedure Close;
      end;

    TClientSocket =
      class(TCustomSocket)
        private
          fClientSocket : TClientWinSocket;
        protected
          procedure DoActivate(Value : boolean); override;
        public
          constructor Create(aOwner : TComponent); override;
          destructor  Destroy; override;
        public
          property Socket : TClientWinSocket read fClientSocket;
        published
          property Active;
          property Address;
          property Host;
          property Port;
          property Service;
          property OnLookup;
          property OnConnecting;
          property OnConnect;
          property OnDisconnect;
          property OnRead;
          property OnOOB;
          property OnWrite;
          property OnError;
      end;

    TCustomServerSocket =
      class(TCustomSocket)
        protected
          fServerSocket : TServerWinSocket;
          procedure DoActivate(Value : boolean); override;
          function  GetGetSocketEvent : TGetSocketEvent;
          function  GetOnClientEvent(index : integer) : TSocketNotifyEvent;
          function  GetOnClientError : TSocketErrorEvent;
          procedure SetGetSocketEvent(Value : TGetSocketEvent);
          procedure SetOnClientEvent(index : integer; Value : TSocketNotifyEvent);
          procedure SetOnClientError(Value : TSocketErrorEvent);
        protected
          property OnGetSocket        : TGetSocketEvent    read GetGetSocketEvent  write SetGetSocketEvent;
          property OnClientConnect    : TSocketNotifyEvent index 2 read GetOnClientEvent write SetOnClientEvent;
          property OnClientDisconnect : TSocketNotifyEvent index 3 read GetOnClientEvent write SetOnClientEvent;
          property OnClientRead       : TSocketNotifyEvent index 0 read GetOnClientEvent write SetOnClientEvent;
          property OnClientReadOOB    : TSocketNotifyEvent index 4 read GetOnClientEvent write SetOnClientEvent;
          property OnClientWrite      : TSocketNotifyEvent index 1 read GetOnClientEvent write SetOnClientEvent;
          property OnClientError      : TSocketErrorEvent          read GetOnClientError write SetOnClientError;
        public
          destructor Destroy; override;
      end;

    TServerSocket =
      class(TCustomServerSocket)
        public
          constructor Create(aOwner : TComponent); override;
        public
          property Socket : TServerWinSocket read fServerSocket;
        published
          property Active;
          property Port;
          property Service;
          property OnListen;
          property OnAccept;
          property OnGetSocket;
          property OnClientConnect;
          property OnClientDisconnect;
          property OnClientRead;
          property OnClientReadOOB;
          property OnClientWrite;
          property OnClientError;
      end;

  threadvar
    SocketErrorProc : procedure (ErrorCode : integer);

  procedure Register;
    
  var
    ByteSent : integer = 0;
    ByteRecv : integer = 0;
    ActSent  : integer = 0;
    GeneralSockInfo : TSockInfo;
implementation

  uses
  {$IFDEF VER140}           { Delphi 6.0 } //.rag
    RTLConsts,
  {$ELSE}
   Consts,
  {$ENDIF}
    Forms, Logs, WebConst;


  type
    TIdSocksRequest = 
      packed record
        Version: Byte;
        OpCode: Byte;
        Port: Word;
        IpAddr: TInAddr;
        UserId: shortstring;
      end;

    TIdSocksResponse = 
      packed record
        Version: Byte;
        OpCode: Byte;
        Port: Word;
        IpAddr: TInAddr;
      end;
    
  var
    WSAData : TWSAData;

  function min(a, b : integer) : integer;
    begin
      if a < b
        then result := a
        else result := b;
    end;

  function max(a, b : integer) : integer;
    begin
      if a < b
        then result := b
        else result := a;
    end;

  function FreeList(List : TList) : boolean;
    var
      i : integer;
    begin
      try
        for i := 0 to pred(List.Count) do
          TObject(List[i]).Free;
        result := true;
      except
        result := false;
      end;
    end;

  function TCPIPBufSize(Socket : TSocket) : integer;
    var
      sz : integer;
    begin
      sz := sizeof(result);
      getsockopt(Socket, SOL_SOCKET, SO_SNDBUF, @result, sz)
    end;

  function ConnectionLost(Error : integer) : boolean;
    begin
      case Error of
        WSAEOPNOTSUPP, WSAESHUTDOWN, WSAECONNABORTED, WSAENETRESET, WSAECONNRESET :
          result := true;
        else
          result := false
      end
    end;

  function CheckSocketResult(resultCode : integer; const Op : string) : integer;
    begin
      if resultCode <> 0
        then
          begin
            result := WSAGetLastError;
            if result <> WSAEWOULDBLOCK then
              if Assigned(SocketErrorProc)
                then SocketErrorProc(result);
          end
        else result := 0
    end;

  procedure Startup;
    var
      ErrorCode : integer;
    begin
      ErrorCode := WSAStartup($0101, WSAData);
      if ErrorCode <> 0
        then raise ESocketError.CreateFmt(sWindowsSocketError, [SysErrorMessage(ErrorCode), ErrorCode, 'WSAStartup']);
    end;

  procedure Cleanup;
    var
      ErrorCode : integer;
    begin
      ErrorCode := WSACleanup;
      if ErrorCode <> 0
        then raise ESocketError.CreateFmt(sWindowsSocketError, [SysErrorMessage(ErrorCode), ErrorCode, 'WSACleanup']);
    end;


  { TCustomWinSocket }

  constructor TCustomWinSocket.Create(aSocket : TSocket);
    begin
      inherited Create;
      fSockInfo := @GeneralSockInfo;
      Startup;
      fSpoolBuffer := TList.Create;
      fSocketLock := TCriticalSection.Create;
      fAsyncStyles := FD_READ or FD_WRITE or FD_CONNECT or FD_CLOSE or FD_OOB;
      fSocket := aSocket;
      fAddr.sin_family := PF_INET;
      fAddr.sin_addr.s_addr := INADDR_ANY;
      fAddr.sin_port := 0;
      fConnected := fSocket <> INVALID_SOCKET;
      if fConnected
        then fTCPBufSize := TCPIPBufSize(fSocket);
    end;

  destructor TCustomWinSocket.Destroy;
    begin
      fOnSocketEvent := nil;  { disable events }
      if (fSocket <> INVALID_SOCKET)
        then
          begin
            if fConnected
              then Disconnect(fSocket)
              else closesocket(fSocket);
          end;

      if fHandle <> 0
        then DeallocateHWnd(fHandle);
      fSocketLock.Free;
      fSpoolBuffer.Free;
      Cleanup;
      inherited Destroy
    end;

  procedure TCustomWinSocket.Accept(Socket : TSocket);
    begin
    end;

  procedure TCustomWinSocket.Close;
    begin
      Disconnect(fSocket)
    end;

  procedure TCustomWinSocket.Connect(Socket : TSocket);
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

    function CheckError : boolean;
      var
        ErrorEvent : TErrorEvent;
        ErrorCode : integer;
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
                      FD_CONNECT :
                        ErrorEvent := eeConnect;
                      FD_CLOSE :
                        ErrorEvent := eeDisconnect;
                      FD_READ :
                        ErrorEvent := eeReceive;
                      FD_WRITE :
                        ErrorEvent := eeSend;
                      FD_ACCEPT :
                        ErrorEvent := eeAccept;
                      FD_OOB :
                        ErrorEvent := eeOOB;
                      else
                        ErrorEvent := eeGeneral;
                    end;
                    Error(Self, ErrorEvent, ErrorCode);
                  end;
            end
          else result := true
      end;

  begin
    with Message do
      if CheckError
        then
          case SelectEvent of
            FD_CONNECT :
              if fSockInfo.fVersion = svNoSocks
                then Connect(Socket);
            FD_CLOSE :
              Disconnect(Socket);
            FD_READ :
                if fSockInfo.fVersion = svNoSocks
                  then Read(Socket)
                  else 
                    begin
                      case fProxyState of
                        pxNone: 
                          Read(Socket);
                        pxConnecting:;
                        pxResolved:
                          begin
                            case fSockInfo.fVersion of
                              svSocks4, svSocks4A:
                                if ResolveSock4
                                  then 
                                    begin
                                      Connect(Socket);
                                      Write(Socket);
                                    end;
                              svSocks5:
                                ResolveSock5;
                            end;
                          end;
                        pxAuthentication:
                          ResolveSock5_A(false);
                        pxResolvedHost:
                          if ResolveSock5_B
                            then 
                              begin
                                Connect(Socket);
                                Write(Socket);
                              end;
                      end;
                    end;
            FD_WRITE :
              begin
                if fSockInfo.fVersion = svNoSocks
                  then Write(Socket)
                  else 
                    begin
                      case fProxyState of
                        pxNone: 
                          Write(Socket);
                        pxConnecting:
                          begin
                            case fSockInfo.fVersion of
                              svSocks4, svSocks4A:
                                ConnectSock4;
                              svSocks5:
                                ConnectSock5;
                            end;
                          end;
                        pxResolved:;
                      end;
                    end;
              end;
            FD_ACCEPT :
              Accept(Socket);
            FD_OOB :
              ReadOOB(Socket);
          end
  end;

  procedure TCustomWinSocket.ConnectSock4;
    var
      req        : TIdSocksRequest;
      p          : pchar;
      e          : integer;
    begin
      with req do
        begin
          Version := 4;
          OpCode  := 1;
          Port    := fProxyInfo.fPort;
          if fSockInfo.fVersion = svSocks4A 
            then IpAddr.S_addr := inet_addr('0.0.0.1')
            else 
              begin
                IpAddr.S_addr := inet_addr(pchar(fProxyInfo.fAddr));
                if IpAddr.S_addr=-1
                  then IpAddr := LookupName(fProxyInfo.fAddr);
              end;
                
          req.UserId := fSockInfo.fUserID;
          p := @req.UserId[1];
          inc(p, Length(req.UserId)+1);
          p[0] := #0;
          inc(p);
          if fSockInfo.fVersion = svSocks4A
            then 
              begin
                e := Length(fProxyInfo.fAddr);
                move(fProxyInfo.fAddr[1], p[0], e);
                inc(p, e);
                p[0] := #0;
                inc(p);
              end;
        end;
      if Winsock.send(fSocket, req, integer(p-@req)-2, 0)<>-1
        then fProxyState := pxResolved
        else 
          begin
            e := EProxyError;
            Error(Self, eeConnect, e);
            fProxyState := pxNone;
          end;
    end;

function TCustomWinSocket.ResolveSock4: boolean;
  var
    res : TIdSocksResponse;
    e   : integer;
    //A   : pointer;
  begin
    result := false;
    if winsock.recv(fSocket, res, sizeof(res), 0)=sizeof(res)
      then
        begin
          case res.OpCode of
            90: e := 0;
            91: e := ESocksRequestFailed;
            92: e := ESocksRequestServerFailed;
            93: e := ESocksRequestIdentFailed;
            else e := ESocksUnknownError;
          end;
          if e<>0
            then Error(Self, eeConnect, e)
            else result := true;
        end
      else 
        begin
          e := EProxyError;
          Error(Self, eeConnect, e);
        end;
    fProxyState := pxNone;
  end;

  procedure TCustomWinSocket.ConnectSock5;
    var
      A : array[0..255] of char;
      E : integer;
    begin
     A[0] := #5;
      A[1] := #1;
      if fSockInfo.FAuthentication = saNoAuthentication 
        then A[2] := #0
        else A[2] := #2;
        
      if Winsock.send(fSocket, A, 3, 0)<>-1
        then fProxyState := pxResolved
        else 
          begin
            e := EProxyError;
            Error(Self, eeConnect, e);
            fProxyState := pxNone;
          end;
    end;

  procedure TCustomWinSocket.ResolveSock5;
    var
      A : array[0..355] of char;
      p : pchar;
      AuthMethod : char;
      E : integer;
    begin
      fProxyState := pxNone;
      if fSockInfo.FAuthentication = saNoAuthentication 
        then AuthMethod := #0
        else AuthMethod := #2;
        
      if (winsock.recv(fSocket, A, 2, 0)=2) and (AuthMethod=A[1])
        then
          begin
            if AuthMethod=#2
              then 
                begin
                  A[0] := #1; 
                  A[2] := char(Length(fSockInfo.fUserID));
                  E    := ord(a[2]);
                  p    := A + E + 3;
                  move(fSockInfo.fUserID[1], A[3], E);
                  E := length(fSockInfo.fPassword);
                  p[0] := char(E);
                  inc(p);
                  move(fSockInfo.fUserID[1], p[0], E);
                  inc(p, e);
                  if Winsock.send(fSocket, A, integer(p-A), 0)<>-1
                    then fProxyState := pxAuthentication
                    else 
                      begin
                        e := EProxyError;
                        Error(Self, eeConnect, e);
                      end;
                end
              else ResolveSock5_A(true);
          end
        else 
          begin
            e := EProxyError;
            Error(Self, eeConnect, e);
          end;
    end;
    
  procedure TCustomWinSocket.ResolveSock5_A(Authen: boolean);
    var
      A : array[0..355] of char;
      p : pchar;
      E : integer;
    begin
      if Authen or ((winsock.recv(fSocket, A, 2, 0)=2) and (A[1]=#0))
        then
          begin
            A[0] := #5; 
            A[1] := #1; 
            A[2] := #0; 
            A[3] := #3;   
            e    := length(fProxyInfo.fAddr);
            A[4] := char(e);
            p := @A[5];
            move(fProxyInfo.fAddr[1], p[0], e);
            inc(p, e);
            move(fProxyInfo.fPort, p[0], sizeof(fProxyInfo.fPort));
            inc(p, sizeof(fProxyInfo.fPort));
            if Winsock.send(fSocket, A, integer(p-A), 0)<>-1
              then fProxyState := pxResolvedHost
              else 
                begin
                  e := ESocksUnknownError;
                  Error(Self, eeConnect, e);
                end;
          end
        else
          begin
            e := ESocksAuthError;
            Error(Self, eeConnect, e);
          end;
    end;

  function TCustomWinSocket.ResolveSock5_B: boolean;
    var
      A : PByteArray;
      e : integer;
    begin
      ioctlsocket(fSocket, FIONREAD, e);
      getmem(A, e);
      try
        if (e>0) and (winsock.recv(fSocket, A^, e, 0)>2)
          then
            case A[1] of
              0: e := 0;
              1: e := ESocksServerGeneralError;
              2: e := ESocksServerPermissionError;
              3: e := ESocksServerNetUnreachableError;
              4: e := ESocksServerHostUnreachableError;
              5: e := ESocksServerConnectionRefusedError;
              6: e := ESocksServerTTLExpiredError;
              7: e := ESocksServerCommandError;
              8: e := ESocksServerAddressError;
              else e := ESocksUnknownError;
            end
          else e := ESocksUnknownError;
      finally
        freemem(A);
      end;  
      result := e=0;
      if not result
        then Error(Self, eeConnect, e);
      fProxyState := pxNone
    end;
    
  procedure TCustomWinSocket.CMDeferFree(var Message);
    begin
      Free;
    end;

  procedure TCustomWinSocket.DeferFree;
    begin
      if fHandle <> 0
        then PostMessage(fHandle, CM_DEFERFREE, 0, 0);
    end;

  procedure TCustomWinSocket.DoSetAsyncStyles;
    var
      Msg      : integer;
      Wnd      : HWnd;
      Blocking : Longint;
    begin
      Msg := 0;
      Wnd := 0;
      if fAsyncStyles <> 0
        then
          begin
            Msg := CM_SOCKETMESSAGE;
            Wnd := Handle;
          end;
      WSAAsyncSelect(fSocket, Wnd, Msg, fAsyncStyles);
      if fASyncStyles = 0
        then
          begin
            Blocking := 0;
            ioctlsocket(fSocket, FIONBIO, Blocking);
          end
    end;

  function TCustomWinSocket.GetHandle : HWnd;
    begin
      if fHandle = 0
        then fHandle := AllocateHwnd(WndProc);
      result := fHandle
    end;

  function TCustomWinSocket.GetLocalAddress : string;
    var
      SockAddrIn : TSockAddrIn;
      Size       : integer;
    begin
      Lock;
      try
        result := '';
        if fSocket <> INVALID_SOCKET
          then
            begin
              Size := SizeOf(SockAddrIn);
              if getsockname(fSocket, SockAddrIn, Size) = 0
                then
                  result := inet_ntoa(SockAddrIn.sin_addr)
            end
      finally
        Unlock
      end
    end;

  function TCustomWinSocket.GetLocalHost : string;
    var
      LocalName : array[0 .. 255] of char;
    begin
      Lock;
      try
        result := '';
        if fSocket <> INVALID_SOCKET
          then
            if gethostname(LocalName, SizeOf(LocalName)) = 0
              then
                result := LocalName
      finally
        Unlock
      end
    end;

  function TCustomWinSocket.GetLocalPort : integer;
    var
      SockAddrIn : TSockAddrIn;
      Size       : integer;
    begin
      Lock;
      try
        result := -1;
        if fSocket <> INVALID_SOCKET
          then
            begin
              Size := SizeOf(SockAddrIn);
              if getsockname(FSocket, SockAddrIn, Size) = 0
                then
                  result := ntohs(SockAddrIn.sin_port)
            end
      finally
        Unlock
      end
    end;

  function TCustomWinSocket.GetRemoteHost : string;
    var
      SockAddrIn : TSockAddrIn;
      Size       : integer;
      HostEnt    : PHostEnt;
    begin
      Lock;
      try
        result := '';
        if fConnected
          then
            begin
              Size := SizeOf(SockAddrIn);
              CheckSocketResult(getpeername(fSocket, SockAddrIn, Size), 'getpeername');
              HostEnt := gethostbyaddr(@SockAddrIn.sin_addr.s_addr, 4, PF_INET);
              if HostEnt <> nil
                then
                  result := HostEnt.h_name
            end
      finally
        Unlock
      end
    end;

  function TCustomWinSocket.GetRemoteAddress : string;
    var
      SockAddrIn : TSockAddrIn;
      Size       : integer;
    begin
      Lock;
      try
        result := '';
        if fConnected
          then
            begin
              Size := SizeOf(SockAddrIn);
              CheckSocketResult(getpeername(fSocket, SockAddrIn, Size), 'getpeername');
              result := inet_ntoa(SockAddrIn.sin_addr)
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
        ErrorCode : integer;
      begin
        continue  := true;
        TotalSent := 0;
        while continue and (Packet.Count > 0) and (TotalSent < fTCPBufSize) do
          begin
            toSend    := min(Packet.Count, fTCPBufSize - TotalSent);
            BytesSent := send(fSocket, Packet.Buffer^, toSend, 0);
            inc(ActSent, BytesSent);
            if BytesSent <> SOCKET_ERROR
              then
                begin
                  Packet.Move(BytesSent);
                  inc(TotalSent, BytesSent);
                end
              else
                begin
                  continue  := false;
                  ErrorCode := WSAGetLastError;
                  Error(self, eeSend, ErrorCode);
                end;
          end;
        result := Packet.Count = 0;
      end;

    var
      Packet : TSpooledPacket;

    begin
      Lock;
      try
        if fConnected
          then
            begin
              Packet := TSpooledPacket(fSpoolBuffer[0]);
              SendPacket(Packet);
              if Packet.Count = 0
                then
                  begin
                    Packet.Free;
                    fSpoolBuffer.Delete(0);
                  end;
              result := true;
            end
          else result := false;
      finally
        Unlock
      end
   end;

  procedure TCustomWinSocket.AsyncSelectSocket;
    begin
      WSAAsyncSelect(fSocket, Handle, CM_SOCKETMESSAGE, fAsyncStyles and not FD_CONNECT);
    end;

  function TCustomWinSocket.GetRemotePort : integer;
    var
      SockAddrIn : TSockAddrIn;
      Size       : integer;
    begin
      Lock;
      try
        result := 0;
        if fConnected
          then
            begin
              Size := SizeOf(SockAddrIn);
              CheckSocketResult(getpeername(fSocket, SockAddrIn, Size), 'getpeername');
              result := ntohs(SockAddrIn.sin_port);
            end;
      finally
        Unlock;
      end
    end;

  function TCustomWinSocket.GetRemoteAddr : TSockAddrIn;
    var
      Size : integer;
    begin
      Lock;
      try
        FillChar(result, SizeOf(result), 0);
        if fConnected then
          begin
            Size := SizeOf(result);
            if getpeername(fSocket, result, Size) <> 0
              then
                FillChar(result, SizeOf(result), 0)
          end
      finally
        Unlock
      end
    end;

  class function TCustomWinSocket.LookupName(const Name : string) : TInAddr;
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
                S_un_b.s_b1 := h_addr^[0];
                S_un_b.s_b2 := h_addr^[1];
                S_un_b.s_b3 := h_addr^[2];
                S_un_b.s_b4 := h_addr^[3];
              end
          end;
      result := InAddr;
    end;

  class function TCustomWinSocket.LookupService(const Service : string) : integer;
    var
      ServEnt : PServEnt;
    begin
      ServEnt := getservbyname(PChar(Service), 'tcp');
      if ServEnt <> nil
        then
          result := ntohs(ServEnt.s_port)
        else
          result := 0
    end;

  function TCustomWinSocket.InitSocket(var Name, Address, Service : string; Port : Word; Client : boolean) : TSockAddrIn;
    begin
      //Logs.Log( 'Survival', DateTimeToStr(Now) + 'Name' + 'service'  );
      result.sin_family := PF_INET;
      if Name <> ''
        then result.sin_addr := LookupName(Name)
        else
          if Address <> ''
            then result.sin_addr.s_addr := inet_addr(PChar(Address))
            else
              if not Client
                then result.sin_addr.s_addr := INADDR_ANY
                else raise ESocketError.Create(sNoAddress); // >> Sospechoso //.Para Eso estan los assert"
      if Service <> ''
        then result.sin_port := htons(LookupService(Service))
        else result.sin_port := htons(Port);
    end;

  procedure TCustomWinSocket.Listen(var Name, Address, Service : string; Port : Word; QueueSize : integer);
    var
      SockAddrIn : TSockAddrIn;
    begin
      if fConnected
        then raise ESocketError.Create(sCannotListenOnOpen);
      fSocket := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
      if FSocket = INVALID_SOCKET
        then raise ESocketError.Create(sCannotCreateSocket);
      try
        SockAddrIn := InitSocket(Name, Address, Service, Port, false);
        CheckSocketResult(bind(fSocket, SockAddrIn, SizeOf(SockAddrIn)), 'bind');
        DoSetASyncStyles;
        if QueueSize > SOMAXCONN
          then QueueSize := SOMAXCONN;
        Event(Self, seListen);
        CheckSocketResult(Winsock.listen(fSocket, QueueSize), 'listen');
        fConnected := true;
        fTCPBufSize := TCPIPBufSize(fSocket);
      except
        Disconnect(fSocket);
        raise;
      end
    end;

  procedure TCustomWinSocket.Open(var Name, Address, Service : string; Port : Word);
    var
      SockAddrIn : TSockAddrIn;
    begin
      if fConnected
        then raise ESocketError.Create(sSocketAlreadyOpen);
      fSocket := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
      if fSocket = INVALID_SOCKET
        then raise ESocketError.Create(sCannotCreateSocket);
      try
        Event(Self, seLookUp);
        with fSockInfo^ do
          if fVersion = svNoSocks
            then 
              begin
                SockAddrIn := InitSocket(Name, Address, Service, Port, true);
                fProxyState := pxNone;
              end
            else 
              begin
                with SockAddrIn do
                  begin
                    sin_family := PF_INET;
                    sin_port   := htons(fPort); 
                    sin_addr   := fAddr;
                  end;
                  
                fProxyState := pxConnecting;
                with fProxyInfo do
                  begin
                    if Name<>''
                      then fAddr := Name
                      else fAddr := Address;
                    
                    if Service <> ''
                      then fPort := htons(LookupService(Service))
                      else fPort := htons(Port);
                  end;
                end;
                
        DoSetASyncStyles;
        Event(Self, seConnecting);
        CheckSocketResult(WinSock.connect(fSocket, SockAddrIn, SizeOf(SockAddrIn)), 'connect');
        
        if not fAsyncStyles and FD_CONNECT <> 0
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
              FreeList(fSpoolBuffer);
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
        if fHandle <> 0
          then result := CallWindowProc(@DefWindowProc, fHandle, Msg, wParam, lParam);
    end;

  procedure TCustomWinSocket.Event(Socket : TCustomWinSocket; SocketEvent : TSocketEvent);
    begin
      if Assigned(fOnSocketEvent)
        then fOnSocketEvent(Self, Socket, SocketEvent);
    end;

  procedure TCustomWinSocket.Error(Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
    begin
      if Assigned(fOnErrorEvent)
        then fOnErrorEvent(Self, Socket, ErrorEvent, ErrorCode)
    end;

  procedure TCustomWinSocket.SendText(const s : string);
    begin
      SendBuf(pointer(S)^, Length(S))
    end;

  function TCustomWinSocket.SendOOB(var Buf; Count : integer) : integer;
    begin
      result := send(fSocket, Buf, Count, MSG_OOB);
    end;

  function TCustomWinSocket.ReceiveOOB(var Buf; Count : integer) : integer;
    begin
      result := recv(fSocket, Buf, Count, MSG_OOB);
    end;

  procedure TCustomWinSocket.SendEncryptedText(const S : string);
    begin
      SendBuf(pointer(S)^, Length(S))
    end;

  function TCustomWinSocket.SendBuf(var Buf; Count : integer) : integer;
    begin
      Lock;
      if Count > 0
        then inc(ByteSent, Count);
      try
        if fConnected and (Count > 0)
          then
            begin
              fSpoolBuffer.Add(TSpooledPacket.Create(PChar(@Buf), Count, true));
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
        then Event(Self, seRead)
    end;

  function TCustomWinSocket.ReceiveBuf(var Buf; Count : integer) : integer;
    var
      ErrorCode : integer;
    begin
      Lock;
      try
        if (Count > 0) and fConnected
          then
            begin
              result := recv(FSocket, Buf, Count, 0);
              if result = SOCKET_ERROR
                then
                  begin
                    ErrorCode := WSAGetLastError;
                    if ErrorCode <> WSAEWOULDBLOCK
                      then
                        begin
                          Error(Self, eeReceive, ErrorCode);
                          Disconnect(FSocket);
                        end;
                    result := 0;
                  end;
              inc(ByteRecv, result);
            end
          else result := 0;
      finally
        Unlock
      end
    end;

  function TCustomWinSocket.ReceiveLength : integer;
    begin
      if not fConnected
        then result := 0
        else
          if ioctlsocket(fSocket, FIONREAD, result) <> 0
            then result := 0;
    end;

  function TCustomWinSocket.ReceiveText : string;
    var
      len : integer;
    begin
      len := ReceiveLength;
      if len > 0
        then
          begin
            SetLength(result, len);
            len := ReceiveBuf(result[1], len);
            if len <> length(result)
              then SetLength(result, max(0, len));
          end
        else result := '';
    end;

  function TCustomWinSocket.PeekBuf(var Buf; Count : integer) : integer;
    var
      ErrorCode : integer;
    begin
      Lock;
      try
        if (Count > 0) and fConnected
          then
            begin
              result := recv(FSocket, Buf, Count, MSG_PEEK);
              if result = SOCKET_ERROR
                then
                  begin
                    ErrorCode := WSAGetLastError;
                    if ErrorCode <> WSAEWOULDBLOCK
                      then
                        begin
                          Error(Self, eeReceive, ErrorCode);
                          Disconnect(FSocket);
                        end;
                    result := 0;
                  end;
            end
          else result := 0;
      finally
        Unlock
      end
    end;

  function TCustomWinSocket.PeekText : string;
    var
      len : integer;
    begin
      len := ReceiveLength;
      if len > 0
        then
          begin
            SetLength(result, len);
            len := PeekBuf(result[1], len);
            if len <> length(result)
              then SetLength(result, max(0, len));
          end
        else result := '';
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
        if (fSocket <> INVALID_SOCKET) and (Socket = fSocket)
          then
            if fSpoolBuffer.Count > 0
              then
                begin
                  Event(Self, seWrite);
                  ClearSpool;
                  AsyncSelectSocket
                end
              else Event(Self, seWrite);
      finally
        Unlock;
      end;
    end;

  procedure TCustomWinSocket.ReadOOB(Socket : TSocket);
    begin
      if (fSocket <> INVALID_SOCKET) and (Socket = FSocket)
        then Event(Self, seOOB)
    end;

{ TClientWinSocket }

  procedure TClientWinSocket.Connect(Socket : TSocket);
    begin
      inherited;
      fConnected := true;
      Event(Self, seConnect)
    end;

  { TServerClientWinSocket }

  constructor TServerClientWinSocket.Create(Socket : TSocket; ServerWinSocket : TServerWinSocket);
    begin
      fServerWinSocket := ServerWinSocket;
      if Assigned(fServerWinSocket)
        then
          begin
            fServerWinSocket.AddClient(Self);
            if FServerWinSocket.AsyncStyles <> 0
              then OnSocketEvent := FServerWinSocket.ClientEvent;
            OnErrorEvent := fServerWinSocket.ClientError // added by Pepe to fix a bug in the original code
          end;
      inherited Create(Socket);
      if fServerWinSocket.ASyncStyles <> 0
        then DoSetAsyncStyles;
      if fConnected
        then Event(Self, seConnect)
    end;

  destructor TServerClientWinSocket.Destroy;
    begin
      if Assigned(fServerWinSocket)
        then fServerWinSocket.RemoveClient(Self);
      inherited Destroy;
    end;


  { TServerWinSocket }

  constructor TServerWinSocket.Create(ASocket : TSocket);
    begin
      fConnections := TList.Create;
      fListLock    := TCriticalSection.Create;
      inherited Create(aSocket);
      fAsyncStyles := FD_ACCEPT;
    end;

  destructor TServerWinSocket.Destroy;
    begin
      inherited Destroy; // >> Pigs!!!!!!!!!!!!!!!!
      fConnections.Free;
      fListLock.Free
    end;

  procedure TServerWinSocket.AddClient(AClient : TServerClientWinSocket);
    begin
      fListLock.Enter;
      try
        if fConnections.indexOf(aClient) < 0
          then fConnections.Add(aClient);
      finally
        fListLock.Leave;
      end
    end;

  procedure TServerWinSocket.RemoveClient(AClient : TServerClientWinSocket);
    begin
      fListLock.Enter;
      try
        fConnections.Remove(aClient)
      finally
        fListLock.Leave;
      end
    end;

  procedure TServerWinSocket.ClientEvent(Sender : TObject; Socket : TCustomWinSocket; SocketEvent : TSocketEvent);
    begin
      case SocketEvent of
        seAccept,
        seLookup,
        seConnecting,
        seListen :
          begin end;
        seConnect : ClientConnect(Socket);
        seDisconnect : ClientDisconnect(Socket);
        seRead : ClientRead(Socket);
        seOOB : ClientReadOOB(Socket);
        seWrite : ClientWrite(Socket);
      end;
    end;

  procedure TServerWinSocket.ClientError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
    begin
      ClientErrorEvent(Socket, ErrorEvent, ErrorCode);
    end;

  function TServerWinSocket.GetActiveConnections : integer;
    begin
      result := fConnections.Count;
    end;

  function TServerWinSocket.GetConnections(index : integer) : TCustomWinSocket;
    begin
      result := TCustomWinSocket(fConnections[index]);
    end;

  procedure TServerWinSocket.Accept(Socket : TSocket);
    var
      ClientSocket    : TServerClientWinSocket;
      ClientWinSocket : TSocket;
      Addr            : TSockAddrIn;
      len             : integer;
    begin
      len := SizeOf(Addr);
      ClientWinSocket := WinSock.accept(Socket, @Addr, @len);
      if ClientWinSocket <> INVALID_SOCKET
        then
          begin
            ClientSocket := GetClientSocket(ClientWinSocket);
            if Assigned(fOnSocketEvent)
              then fOnSocketEvent(Self, ClientSocket, seAccept);
          end;
    end;

  procedure TServerWinSocket.Disconnect(Socket : TSocket);
    var
      Client : TServerClientWinSocket;
      i      : integer;
    begin
      Lock;
      try
        for i := pred(fConnections.Count) downto 0 do
          try
            Client := TServerClientWinSocket(fConnections[i]);
            Client.Close;
          except
            // >> must log something...
          end;
        fConnections.Clear;
        inherited Disconnect(Socket);
      finally
        Unlock;
      end;
    end;

  function TServerWinSocket.GetClientSocket(Socket : TSocket) : TServerClientWinSocket;
    begin
      if Assigned(fOnGetSocket)
        then fOnGetSocket(Self, Socket, result)
        else result := nil;
      if result = nil
        then result := TServerClientWinSocket.Create(Socket, Self);
    end;

  procedure TServerWinSocket.ClientConnect(Socket : TCustomWinSocket);
    begin
      if Assigned(fOnClientConnect)
        then fOnClientConnect(Self, Socket);
    end;

  procedure TServerWinSocket.ClientDisconnect(Socket : TCustomWinSocket);
    begin
      if Assigned(FOnClientDisconnect)
        then fOnClientDisconnect(Self, Socket);
      Socket.DeferFree;
    end;

  procedure TServerWinSocket.ClientRead(Socket : TCustomWinSocket);
    begin
      if Assigned(FOnClientRead)
        then FOnClientRead(Self, Socket);
    end;

  procedure TServerWinSocket.ClientReadOOB(Socket : TCustomWinSocket);
    begin
      if Assigned(fOnClientReadOOB)
        then fOnClientReadOOB(Self, Socket);
    end;

  procedure TServerWinSocket.ClientWrite(Socket : TCustomWinSocket);
    begin
      if Assigned(FOnClientWrite)
        then FOnClientWrite(Self, Socket);
    end;

  procedure TServerWinSocket.ClientErrorEvent(Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
    begin
      if Assigned(FOnClientError)
        then FOnClientError(Self, Socket, ErrorEvent, ErrorCode);
    end;


  { TCustomSocket }

  procedure TCustomSocket.DoEvent(Sender : TObject; Socket : TCustomWinSocket; SocketEvent : TSocketEvent);
    begin
      Event(Socket, SocketEvent);
    end;

  procedure TCustomSocket.DoError(Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
    begin
      Error(Socket, ErrorEvent, ErrorCode);
    end;

  procedure TCustomSocket.Event(Socket : TCustomWinSocket; SocketEvent : TSocketEvent);
    begin
      case SocketEvent of
        seLookup :
          if Assigned(fOnLookup) then
            fOnLookup(Self, Socket);
        seConnecting :
          if Assigned(fOnConnecting)
            then fOnConnecting(Self, Socket);
        seConnect :
          begin
            fActive := true;
            if Assigned(fOnConnect)
              then fOnConnect(Self, Socket);
          end;
        seListen :
          begin
            fActive := true;
            if Assigned(fOnListen)
              then fOnListen(Self, Socket);
          end;
        seDisconnect :
          begin
            fActive := false;
            if Assigned(fOnDisconnect)
              then fOnDisconnect(Self, Socket);
          end;
        seAccept :
          if Assigned(fOnAccept)
            then fOnAccept(Self, Socket);
        seRead :
          if Assigned(fOnRead)
            then fOnRead(Self, Socket);
        seOOB :
          if Assigned(fOnOOB)
            then fOnOOB(Self, Socket);
        seWrite :
          if Assigned(fOnWrite)
            then fOnWrite(Self, Socket);
      end;
    end;

  procedure TCustomSocket.Error(Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer);
    begin
      if Assigned(fOnError)
        then fOnError(Self, Socket, ErrorEvent, ErrorCode);
    end;

  procedure TCustomSocket.SetActive(Value : boolean);
    begin
      if Value <> fActive
        then
          begin
            if (csDesigning in ComponentState) or (csLoading in ComponentState)
              then fActive := Value;
            if not (csLoading in ComponentState)
              then DoActivate(Value);
          end
    end;

  procedure TCustomSocket.Loaded;
    begin
      inherited Loaded;
      DoActivate(fActive);
    end;

  procedure TCustomSocket.SetAddress(Value : string);
    begin
      if CompareText(Value, FAddress) <> 0
        then
          begin
            if not (csLoading in ComponentState) and fActive
              then raise ESocketError.Create(sCantChangeWhileActive);
            fAddress := Value
          end
    end;

  procedure TCustomSocket.SetHost(Value : string);
    begin
      if CompareText(Value, fHost) <> 0
        then
          begin
            if not (csLoading in ComponentState) and fActive
              then raise ESocketError.Create(sCantChangeWhileActive);
            fHost := Value;
          end;
    end;

  procedure TCustomSocket.SetPort(Value : integer);
    begin
      if FPort <> Value
        then
          begin
            if not (csLoading in ComponentState) and fActive
              then raise ESocketError.Create(sCantChangeWhileActive);
            fPort := Value;
          end
    end;

  procedure TCustomSocket.SetService(Value : string);
    begin
      if CompareText(Value, FService) <> 0
        then
          begin
            if not (csLoading in ComponentState) and fActive
              then raise ESocketError.Create(sCantChangeWhileActive);
            fService := Value;
          end
    end;

  procedure TCustomSocket.Open;
    begin
      Active := true;
    end;

  procedure TCustomSocket.Close;
    begin
      Active := false;
    end;

  { TClientSocket }

  constructor TClientSocket.Create(AOwner : TComponent);
    begin
      inherited Create(aOwner);
      fClientSocket := TClientWinSocket.Create(INVALID_SOCKET);
      fClientSocket.OnSocketEvent := DoEvent;
      fClientSocket.OnErrorEvent  := DoError;
    end;

  destructor TClientSocket.Destroy;
    begin
      fClientSocket.Free;
      inherited Destroy;
    end;

  procedure TClientSocket.DoActivate(Value : boolean);
    begin
      if (Value <> fClientSocket.Connected) and not (csDesigning in ComponentState)
        then
          begin
            if fClientSocket.Connected
              then
                begin
                  fActive := false;
                  fClientSocket.fConnected := false;
                  fClientSocket.Disconnect(fClientSocket.fSocket);
                end
              else
                begin
                  fClientSocket.Open(fHost, fAddress, fService, fPort);
                end;
          end;
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
              then fServerSocket.Disconnect(fServerSocket.SocketHandle)
              else fServerSocket.Listen(fHost, fAddress, fService, fPort, 5);
          end
    end;

  function TCustomServerSocket.GetGetSocketEvent : TGetSocketEvent;
    begin
      result := fServerSocket.OnGetSocket;
    end;

  procedure TCustomServerSocket.SetGetSocketEvent(Value : TGetSocketEvent);
    begin
      fServerSocket.OnGetSocket := Value;
    end;

  function TCustomServerSocket.GetOnClientEvent(index : integer) : TSocketNotifyEvent;
    begin
      case index of
        0 : result := fServerSocket.OnClientRead;
        1 : result := fServerSocket.OnClientWrite;
        2 : result := fServerSocket.OnClientConnect;
        3 : result := fServerSocket.OnClientDisconnect;
        4 : result := fServerSocket.OnClientReadOOB;
      end;
    end;

  procedure TCustomServerSocket.SetOnClientEvent(index : integer; Value : TSocketNotifyEvent);
    begin
      case index of
        0 : fServerSocket.OnClientRead := Value;
        1 : fServerSocket.OnClientWrite := Value;
        2 : fServerSocket.OnClientConnect := Value;
        3 : fServerSocket.OnClientDisconnect := Value;
        4 : fServerSocket.OnClientReadOOB := Value;
      end;
    end;

  function TCustomServerSocket.GetOnClientError : TSocketErrorEvent;
    begin
      result := fServerSocket.OnClientError;
    end;

  procedure TCustomServerSocket.SetOnClientError(Value : TSocketErrorEvent);
    begin
      fServerSocket.OnClientError := Value;
    end;

  { TServerSocket }
  constructor TServerSocket.Create(aOwner : TComponent);
    begin
      inherited Create(aOwner);
      fServerSocket := TServerWinSocket.Create(INVALID_SOCKET);
      fServerSocket.OnSocketEvent := DoEvent;
      fServerSocket.OnErrorEvent := DoError;
    end;

procedure Register;
  begin
    RegisterComponents( 'Merchise', [TCustomSocket, TClientSocket, TCustomServerSocket] );
  end;
    
initialization
  GeneralSockInfo.fVersion := svNoSocks
end.
