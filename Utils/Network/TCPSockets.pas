unit TCPSockets;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    WinSock, Collection, SyncObjs;

  const
    WM_ASYNCSELECT         = WM_USER + 0;
    WM_ASYNCGETSERVBYNAME  = WM_USER + 1;
    WM_ASYNCGETHOSTBYNAME  = WM_USER + 2;
    WM_SOCKET_BASEACTION   = WM_USER + 3;
    WM_SOCKET_READ         = WM_SOCKET_BASEACTION + FD_READ;
    WM_SOCKET_WRITE        = WM_SOCKET_BASEACTION + FD_WRITE;
    WM_SOCKET_OOB          = WM_SOCKET_BASEACTION + FD_OOB;
    WM_SOCKET_CONNECT      = WM_SOCKET_BASEACTION + FD_CONNECT;
    WM_SOCKET_CLOSE        = WM_SOCKET_BASEACTION + FD_CLOSE;
    WM_SOCKET_ACCEPT       = WM_SOCKET_BASEACTION + FD_ACCEPT;

  const
    PacketSize = 1024;
    BackLog    =  5;
    NoPort     = -1;

  const
    flgPeek = MSG_PEEK;
    flgGet  = 0;
    flgSend = 0;

  const
    MaxHostNameSize   = 512;
    MaxHostStructSize = 2 * MaxGetHostStruct;

  type
    TSocketOptions = word;

  const
    one             = 1;
    soConnected     = one;
    soConnecting    = one shl 1;
    soResolvingHost = soConnecting;
    soSpooled       = one shl 2;
    soListening     = one shl 3;

  const
    InitSpoolSize  = 5;
    DeltaSpoolSize = 5;

  type
    TSpooledPacket = class; // Socket spool packet
    TNetworkSocket = class; // Abtract network socket
    TTCPLine       = class; // Client socket
    TTCPBinder     = class; // Server socket

    TPacket = array[0..PacketSize] of char;

    ESocketException = class(Exception);

    TSpooledPacket =
      class
        public
          constructor Create(aBuffer : pchar; aSize : integer; OwnsPacket : boolean);
          destructor  Destroy; override;
        private
          fBuffer     : pchar;
          fSize       : integer;
          fOwnsPacket : boolean;
        public
          procedure SetSize(aSize : integer);
        public
          property Size   : integer read fSize write SetSize;
          property Buffer : pchar   read fBuffer;
      end;

    TSocketErrorEvent = procedure(Sender : TObject; const text : string; code : integer) of object;

    TNetworkSocket =
      {$IFNDEF SOCKET_OBJECT}
      class(TControl)
        public
          constructor Create(AOwner : TComponent); override;
      {$ELSE}
      class(TObject)
        public
          constructor Create;
      {$ENDIF}
          destructor  Destroy; override;
        private
          fPort       : integer;
          fOptions    : TSocketOptions;
          fSocket     : TSocket;
          fHandle     : HWND;
          fLastError  : integer;
          fOnError    : TSocketErrorEvent;
          fSocketLock : TCriticalSection;
        private
          procedure SetOption(anOption : TSocketOptions; value : boolean);
          function  GetOption(anOption : TSocketOptions) : boolean;
        protected
          property  Options[anOption : TSocketOptions] : boolean read GetOption write SetOption;
          property  Handle : THandle read fHandle;
          property  Socket : TSocket read fSocket write fSocket;
        private
          procedure SetPort(aPort : integer); virtual;
        {$IFNDEF SOCKET_OBJECT}
        published
        {$ELSE}
        public
        {$ENDIF}
          property  Port    : integer           read fPort write SetPort;
          property  OnError : TSocketErrorEvent read fOnError write fOnError;
        public
          property  LastError : integer read fLastError;
        public
          procedure Lock;
          procedure UnLock;
          procedure ResetNotifications; virtual;
          procedure ProcessMessages;
        private
          procedure MessageProcessor(var aMsg : TMessage);
          procedure WMAsyncSelect(var aMsg : TMessage); message WM_ASYNCSELECT;
        protected
          procedure ReportError(const aText : string; code : integer); virtual;
          procedure AllocateHandler;
          procedure DeallocateHandler;
      end;

    TDataArrivalEvent = procedure(Sender : TTCPLine; ByteTotal : integer) of object;

    TTCPLine =
      class(TNetworkSocket)
        public
          constructor Create(AOwner : TComponent); override;
          destructor  Destroy; override;
        private
          fServerName     : string;
          fSockAddrIn     : TSockAddrIn;
          fPHostBuff      : pointer;
          fSpoolBuffer    : TCollection;
          fBytesRecieved  : integer;
          fWriteAllowed   : boolean;
          fTCPBufSize     : integer;
          fOnClose        : TNotifyEvent;
          fOnConnect      : TNotifyEvent;
          fOnFailConnect  : TNotifyEvent;
          fOnDataArrival  : TDataArrivalEvent;
          fOnWriteAllowed : TNotifyEvent;
        protected
          constructor AssignSocket(AOwner : TComponent; aSocket : TSocket);
        public
          procedure   Connect(const ServerName : string; Port : integer);
          procedure   Disconnect;
          function    DataAvail : integer;
          function    Read(var buffer; size : integer) : integer;
          function    Peek(var buffer; size : integer) : integer;
          procedure   Write(var buffer; size : integer);
          function    ReadString(var str : string; CharCount : integer) : integer;
          function    PeekString(var str : string; CharCount : integer) : integer;
          function    ReadLine(var line : string) : boolean;
          function    PeekLine(var line : string) : boolean;
          procedure   WriteString(var str : string);
          procedure   WriteLine(const aLine : string);
          procedure   ResetNotifications; override;
        private
          function    GetSpooled : boolean;
          function    GetConnecting : boolean;
          procedure   SetSpooled(value : boolean);
          function    GetRemoteAddress : string;
          function    GetLocalAddress : string;
          function    IsConnected : boolean;
        {$IFNDEF SOCKET_OBJECT}
        published
        {$ELSE}
        public
        {$ENDIF}
          property    Spooled : boolean read GetSpooled write SetSpooled;
        public
          property    Connected     : boolean           read IsConnected;
          property    Connecting    : boolean           read GetConnecting;
          property    RemoteHost    : string            read GetRemoteAddress;
          property    LocalHost     : string            read GetLocalAddress;
          property    BytesRecieved : integer           read fBytesRecieved;
        private
          procedure   SetWriteAllowed(WriteAllowedProc : TNotifyEvent);
        {$IFNDEF SOCKET_OBJECT}
        published
        {$ELSE}
        public
        {$ENDIF}
          property    OnConnect      : TNotifyEvent      read fOnConnect      write fOnConnect;
          property    OnFailConnect  : TNotifyEvent      read fOnFailConnect  write fOnFailConnect;
          property    OnDataArrival  : TDataArrivalEvent read fOnDataArrival  write fOnDataArrival;
          property    OnWriteAllowed : TNotifyEvent      read fOnWriteAllowed write SetWriteAllowed;
          property    OnClose        : TNotifyEvent      read fOnClose        write fOnClose;
        protected
          procedure   HostFound(found : boolean); virtual;
          function    CheckLastErrorSeverity : boolean;
          function    CreateSocket : boolean;
          function    AsyncSelectSocket : integer;
          function    LinkEvent : boolean;
          function    MakeConnection : boolean;
          function    AsyncConnect : boolean;
          function    AsyncGetHostName : boolean;
          function    TryToConnect : boolean;
          procedure   ClearSpool;
        private
          procedure   WMAsyncGetHostByName(var aMsg : TMessage); message WM_ASYNCGETHOSTBYNAME;
          procedure   WMRead(var aMsg : TMessage);               message WM_SOCKET_READ;
          procedure   WMWrite(var aMsg : TMessage);              message WM_SOCKET_WRITE;
          procedure   WMClose(var aMsg : TMessage);              message WM_SOCKET_CLOSE;
          procedure   WMConnect(var aMsg : TMessage);            message WM_SOCKET_CONNECT;
        private
          function    ReadData(var buffer; CharCount, flag : integer) : integer;
          function    SendPacket(Packet : TSpooledPacket) : boolean;
        protected
          procedure   ReportError(const aText : string; code : integer); override;
      end;

    TAcceptEvent = procedure(Sender : TObject; TCPLine : TTCPLine) of Object;

    TTCPBinder =
      class(TNetworkSocket)
        private
          fOnAccept  : TAcceptEvent;
        private
          procedure   SetPort(aPort : integer); override;
          function    GetListening : boolean;
          procedure   SetListening(lstn : boolean);
        {$IFNDEF SOCKET_OBJECT}
        published
        {$ELSE}
        public
        {$ENDIF}
          property    OnAccept  : TAcceptEvent read fOnAccept    write fOnAccept;
          property    Listening : boolean      read GetListening write SetListening;
        public
          procedure   StartListening;
          procedure   StopListening;
          procedure   ResetNotifications; override;
        private
          procedure   WMAccept(var aMsg : TMessage); message WM_SOCKET_ACCEPT;
      end;

  type
    TIP = integer;

  const
    NoIP = INADDR_NONE;

  function  GetIP(Addr : string) : TIP;
  procedure AddIP(Addr : string; IP : TIP);

  procedure Register;

implementation

  // TSpooledPacket

  constructor TSpooledPacket.Create(aBuffer : pchar; aSize : integer; OwnsPacket : boolean);
    begin
      inherited Create;
      fOwnsPacket := OwnsPacket;
      if not OwnsPacket
        then
          fBuffer := aBuffer
        else
          begin
            GetMem(fBuffer, aSize);
            move(aBuffer[0], fBuffer[0], aSize);
          end;
      fSize := aSize;
    end;

  destructor TSpooledPacket.Destroy;
    begin
      SetSize(0);
      inherited;
    end;

  procedure TSpooledPacket.SetSize(aSize : integer);
    var
      aux : pchar;
    begin
      if aSize < fSize
        then
          if fOwnsPacket
            then
              if aSize <= 0
                then
                  begin
                    FreeMem(fBuffer);
                    fBuffer := nil;
                    fSize   := 0;
                  end
                else
                  begin
                    GetMem(aux, aSize);
                    move(fBuffer[0], aux[0], aSize);
                    FreeMem(fBuffer);
                    fBuffer := aux;
                    fSize   := aSize;
                  end
            else
              begin
                fBuffer := fBuffer + (fSize - aSize);
                fSize   := aSize;
              end;
    end;


  function ConnectionLost( error : integer ) : boolean;
    begin
      case error of
        WSAEOPNOTSUPP, WSAESHUTDOWN, WSAECONNABORTED, WSAENETRESET, WSAECONNRESET:
          result := true;
        else
          result := false;
      end;
    end;

  // TNetworkSocket

{$IFNDEF SOCKET_OBJECT}

  constructor TNetworkSocket.Create(AOwner: TComponent);
    begin
      inherited;
      fSocket := INVALID_SOCKET;
      fSocketLock := TCriticalSection.Create;
    end;

{$ELSE}

  constructor TNetworkSocket.Create;
    begin
      inherited;
      fSocket := INVALID_SOCKET;
      fSocketLock := TCriticalSection.Create;
    end;

{$ENDIF}

  destructor TNetworkSocket.Destroy;
    begin
      CloseSocket(fSocket);
      DeallocateHandler;
      fSocketLock.Free;
      fSocketLock := nil;
      inherited;
    end;

  procedure TNetworkSocket.Lock;
    begin
      if fSocketLock <> nil
        then fSocketLock.Enter;
    end;

  procedure TNetworkSocket.Unlock;
    begin
      if fSocketLock <> nil
        then fSocketLock.Leave;
    end;

  procedure TNetworkSocket.SetOption(anOption : TSocketOptions; value : boolean);
    begin
      if value
        then fOptions := fOptions or anOption
        else fOptions := fOptions and not anOption;
    end;

  function TNetworkSocket.GetOption(anOption : TSocketOptions) : boolean;
    begin
      result := (fOptions and anOption) <> 0;
    end;

  procedure TNetworkSocket.SetPort(aPort : integer);
    begin
      fPort := aPort;
    end;

  procedure TNetworkSocket.ResetNotifications;
    begin
      fOnError := nil;
    end;

  procedure TNetworkSocket.ProcessMessages;
    var
      Msg : TMsg;
    begin
      if PeekMessage(Msg, 0, 0, 0, PM_REMOVE)
        then DispatchMessage(Msg);
    end;

  procedure TNetworkSocket.MessageProcessor(var aMsg : TMessage);
    begin
      fLastError := WSAGetSelectError(aMsg.lParam);
      if fLastError = 0
        then Dispatch(aMsg)
        else ReportError('Error: ' + IntToStr(fLastError), fLastError);
    end;

  procedure TNetworkSocket.WMAsyncSelect(var aMsg : TMessage);
    begin
      aMsg.Msg := WM_SOCKET_BASEACTION + LoWord(aMsg.lParam);
      Dispatch(aMsg);
    end;

  procedure TNetworkSocket.ReportError(const aText : string; code : integer);
    begin
      if Assigned(OnError)
        then OnError(Self, aText, fLastError);
    end;

  procedure TNetworkSocket.AllocateHandler;
    begin
      DeallocateHandler;
      fHandle := AllocateHWnd(MessageProcessor);
    end;

  procedure TNetworkSocket.DeallocateHandler;
    begin
      if fHandle <> THandle(0)
        then
          begin
            DeallocateHWnd(fHandle);
            fHandle := THandle(0);
          end;
    end;


  // TTCPLine

  constructor TTCPLine.Create(AOwner: TComponent);
    begin
      inherited Create(AOwner);
      fSpoolBuffer := TCollection.Create(InitSpoolSize, DeltaSpoolSize, rkBelonguer);
      Options[soSpooled] := true;
    end;

  constructor TTCPLine.AssignSocket(AOwner: TComponent; aSocket : TSocket);
    begin
      inherited Create(AOwner);
      Socket := aSocket;
      AllocateHandler;
      LinkEvent;
      fSpoolBuffer := TCollection.Create(InitSpoolSize, DeltaSpoolSize, rkBelonguer);
      Options[soConnected or soSpooled] := true;
    end;

  destructor TTCPLine.Destroy;
    begin
      ResetNotifications;
      if GetOption(soConnected)
        then Disconnect;
      if fPHostBuff <> nil
        then FreeMem(fPHostBuff);
      fSpoolBuffer.Free;
      inherited Destroy;
    end;

  procedure TTCPLine.Connect(const ServerName : string; Port : integer);
    begin
      Lock;
      try
        Disconnect;
        fServerName := ServerName;
        fPort       := Port;
        AllocateHandler;
        if not AsyncConnect
          then
            begin
              Options[soConnecting] := false;
              if Assigned(fOnFailConnect)
                then fOnFailConnect(Self);
            end
          else Options[soConnecting] := false;
      finally
        Unlock;
      end;
    end;

  procedure TTCPLine.Disconnect;
    begin
      Lock;
      try
        if Socket <> INVALID_SOCKET
          then
            begin
              DeallocateHandler;
              ShutDown(Socket, 2);
              CloseSocket(Socket);
              Socket := INVALID_SOCKET;
              fSpoolBuffer.DeleteAll;
              if GetOption(soConnected) and Assigned(fOnClose)
                then fOnClose(Self);
              Options[soConnecting + soConnected] := false;
            end;
      finally
        Unlock;
      end;
    end;

  function TTCPLine.DataAvail : integer;
    begin
      Lock;
      try
        if fSocket <> INVALID_SOCKET
          then
            if ioctlsocket(Socket, FIONREAD, result) <> 0
              then result := 0
          else result := 0
      finally
        Unlock;
      end;
    end;

  function TTCPLine.ReadData(var buffer; CharCount, flag : integer) : integer;
    begin
      Lock;
      try
        result := recv(Socket, buffer, CharCount, flag);
        if result > 0
          then dec(fBytesRecieved, result)
          else CheckLastErrorSeverity;
      finally
        Unlock;
      end;
    end;

  function TTCPLine.Read(var buffer; size : integer) : integer;
    begin
      result := ReadData(buffer, size, flgGet);
    end;

  function TTCPLine.Peek(var buffer; size : integer) : integer;
    begin
      result := ReadData(buffer, size, flgPeek);
    end;

  procedure TTCPLine.Write(var buffer; size : integer);
    begin
      Lock;
      try
        if Options[soSpooled]
          then
            begin
              fSpoolBuffer.Insert(TSpooledPacket.Create(pchar(@buffer), size, true));
              AsyncSelectSocket; // Can I write now?
            end
          else
            if (Send(Socket, buffer, size, flgSend) = SOCKET_ERROR) and not CheckLastErrorSeverity
              then fLastError := WSAGetLastError;
      finally
        Unlock;
      end;
    end;

  function TTCPLine.ReadString(var str : string; CharCount : integer) : integer;
    begin
      if CharCount > 0
        then
          begin
            SetLength(str, CharCount);
            result := Read(str[1], CharCount);
            if result <= 0
              then str := ''
              else
                if result < CharCount
                  then SetLength(str, result);
          end
        else
          begin
            str := '';
            result := 0;
          end;
    end;

  function TTCPLine.PeekString(var str : string; CharCount : integer) : integer;
    begin
      if CharCount > 0
        then
          begin
            SetLength(str, CharCount);
            result := Peek(str, CharCount);
            if result <= 0
              then str := ''
              else
                if result < CharCount
                  then SetLength(str, result);
          end
        else
          begin
            str := '';
            result := 0;
          end;
    end;

  function TTCPLine.ReadLine(var line : string) : boolean;
    var
      packet     : TPacket;
      BytesRead  : integer;
      AuxStr     : PChar;
      Garbage    : array[0..1] of char;
    begin
      BytesRead := Peek(packet, sizeof(packet) - 1);
      if BytesRead > 0
        then
          begin
            packet[BytesRead] := #0;
            AuxStr := StrScan(packet, ^M);
            if AuxStr <> nil
              then
                begin
                  ReadString(line, AuxStr - packet);
                  if AuxStr[1] = ^J
                    then Read(Garbage, 2)
                    else Read(Garbage, 1);
                  result := true;
                end
              else
                begin
                  ReadString(line, BytesRead);
                  result := false;
                end;
          end
        else
          begin
            line   := '';
            result := false;
          end;
    end;

  function TTCPLine.PeekLine(var line : string) : boolean;
    var
      packet     : TPacket;
      BytesRead  : integer;
      AuxStr     : PChar;
    begin
      BytesRead := Peek(packet, sizeof(packet) - 1);
      if BytesRead > 0
        then
          begin
            packet[BytesRead] := #0;
            AuxStr := StrScan(packet, ^M);
            if AuxStr <> nil
              then
                begin
                  PeekString(line, AuxStr - packet);
                  result := true;
                end
              else
                begin
                  PeekString(line, BytesRead);
                  result := false;
                end;
          end
        else
          begin
            line   := '';
            result := false;
          end;
    end;

  procedure TTCPLine.WriteString(var str : string);
    begin
      if str <> ''
        then Write(str[1], length(str));
    end;

  procedure TTCPLine.WriteLine(const aLine : string);
    var
      aux : string;
    begin
      aux := aLine + #$D#$A; // >>
      WriteString(aux);
    end;

  procedure TTCPLine.ResetNotifications;
    begin
      inherited;
      fOnClose       := nil;
      fOnConnect     := nil;
      fOnFailConnect := nil;
      fOnDataArrival := nil;
    end;

  function TTCPLine.GetSpooled : boolean;
    begin
      result := GetOption(soSpooled);
    end;

  function TTCPLine.GetConnecting : boolean;
    begin
      result := GetOption(soConnecting);
    end;

  procedure TTCPLine.SetSpooled(value : boolean);
    begin
      Options[soSpooled] := value;
    end;

  function TTCPLine.GetRemoteAddress : string;
    var
      SockAddr : TSockAddrIn;
      len      : integer;
    begin
      len := sizeof(SockAddr);
      if GetPeerName(Socket, SockAddr, len) = 0
        then result := inet_ntoa(SockAddr.sin_addr)
        else result := '';
    end;

  function TTCPLine.GetLocalAddress : string;
    var
      buffer : array[0..MaxHostNameSize] of char;
    begin
      if GetHostName(buffer, MaxHostNameSize) = 0
        then result := buffer
        else result := '';
    end;

  function TTCPLine.IsConnected : boolean;
    begin
      result := GetOption(soConnected) and (Socket <> INVALID_SOCKET) and (AsyncSelectSocket <> WSAENETDOWN);
      Options[soConnected] := result;
    end;

  procedure TTCPLine.SetWriteAllowed(WriteAllowedProc : TNotifyEvent);
    begin
      fOnWriteAllowed := WriteAllowedProc;
      if Assigned(fOnWriteAllowed) and fWriteAllowed
        then fOnWriteAllowed(Self);
    end;

  procedure TTCPLine.WMAsyncGetHostByName(var aMsg : TMessage);
    begin
      HostFound(WSAGetAsyncError(aMsg.lParam) = 0);
    end;

  procedure TTCPLine.HostFound(found : boolean);
    begin
      if found
        then
          begin
            fSockAddrIn.sin_addr.s_un_b.s_b1 := PHostEnt(fPHostBuff).h_addr_list^[0];
            fSockAddrIn.sin_addr.s_un_b.s_b2 := PHostEnt(fPHostBuff).h_addr_list^[1];
            fSockAddrIn.sin_addr.s_un_b.s_b3 := PHostEnt(fPHostBuff).h_addr_list^[2];
            fSockAddrIn.sin_addr.s_un_b.s_b4 := PHostEnt(fPHostBuff).h_addr_list^[3];
            if TryToConnect
              then Options[soConnecting] := true
              else
                begin
                  Options[soConnecting] := false;
                  if Assigned(fOnFailConnect)
                    then fOnFailConnect(Self);
                end;
          end
        else
          begin
            Options[soConnecting] := false;
            if Assigned(fOnFailConnect)
              then fOnFailConnect(Self);
          end;
    end;

  function TTCPLine.AsyncConnect : boolean;
    begin
      fSockAddrIn.sin_port := htons(fPort);
      fSockAddrIn.sin_addr.s_addr := inet_addr(PChar(fServerName));
      if fSockAddrIn.sin_addr.s_addr = NoIP
        then
          try
            if fPHostBuff = nil
              then GetMem(fPHostBuff, MaxHostStructSize);
            result := AsyncGetHostName;
          except
            result := false;
          end
        else result := TryToConnect;
    end;

  function TTCPLine.AsyncGetHostName : boolean;
    begin
      repeat
        result := WSAAsyncGetHostByName(Handle, WM_ASYNCGETHOSTBYNAME, PChar(fServerName), fPHostBuff, MaxHostStructSize) > 0;
      until result or CheckLastErrorSeverity;
    end;

  function TTCPLine.TryToConnect : boolean;
    begin
      fSockAddrIn.sin_family := AF_INET;
      result := CreateSocket and LinkEvent and MakeConnection;
    end;

  function TTCPLine.CreateSocket : boolean;
    begin
      repeat
        Socket := WinSock.Socket(PF_INET, SOCK_STREAM, 0);
      until (Socket <> INVALID_SOCKET) or CheckLastErrorSeverity;
      result := Socket <> INVALID_SOCKET;
    end;

  function TTCPLine.AsyncSelectSocket : integer;
    begin
      result := WSAASyncSelect(Socket, fHandle, WM_ASYNCSELECT, FD_WRITE or FD_READ or FD_CLOSE or FD_CONNECT);
    end;

  function TTCPLine.LinkEvent : boolean;
    begin
      repeat
        result := AsyncSelectSocket = 0;
      until result or CheckLastErrorSeverity;
    end;

  function TTCPLine.MakeConnection : boolean;
    begin
      result := (WinSock.Connect(Socket, fSockAddrIn, sizeof(fSockAddrIn)) = 0) or (WSAGetLastError = WSAEWOULDBLOCK) or (WSAGetLastError = WSAEINPROGRESS);
    end;

  function TTCPLine.CheckLastErrorSeverity : boolean;
    begin
      if ConnectionLost( WSAGetLastError )
        then
          begin
            Disconnect;
            result := true;
          end
        else result := true;
    end;

  procedure TTCPLine.WMWrite(var aMsg : TMessage);
    begin
      fWriteAllowed := true;
      if Options[soSpooled] and (fSpoolBuffer.Count > 0)
        then
          begin
            if Assigned(fOnWriteAllowed)
              then fOnWriteAllowed(Self);
            ClearSpool;
            AsyncSelectSocket; // Can I write again?
          end
        else
          if Assigned(fOnWriteAllowed)
            then fOnWriteAllowed(Self);
    end;

  procedure TTCPLine.WMRead(var aMsg : TMessage);
    var
      buffer : TPacket;
    begin
      if ioctlsocket(Socket, FIONREAD, fBytesRecieved) = 0
        then
          begin
            if fBytesRecieved > 0
              then
                if Assigned(fOnDataArrival)
                  then fOnDataArrival(Self, fBytesRecieved)
                  else
                    while Read(buffer, PacketSize) = PacketSize do;
          end
        else CheckLastErrorSeverity;
    end;

  procedure TTCPLine.WMClose(var aMsg : TMessage);
    begin
      Disconnect;
    end;

  procedure TTCPLine.WMConnect(var aMsg : TMessage);
    var
      sz : integer;
    begin
      if not GetOption(soConnected)
        then
          begin
            sz := sizeof(fTCPBufSize);
            getsockopt(fSocket, SOL_SOCKET, SO_SNDBUF, @fTCPBufSize, sz);
            Options[soConnected]  := true;
            Options[soConnecting] := false;
            if Assigned(fOnConnect)
              then fOnConnect(Self);
          end;
    end;

  function TTCPLine.SendPacket(Packet : TSpooledPacket) : boolean;
    var
      BytesSent  : integer;
      BytesLeft  : integer;
      aux        : pchar;
    begin
      BytesLeft := Packet.Size;
      aux := Packet.Buffer;
      if  fTCPBufSize > BytesLeft
        then BytesSent := Send(Socket, aux^, BytesLeft, flgSend)
        else BytesSent := Send(Socket, aux^, fTCPBufSize, flgSend);
      if BytesSent <> SOCKET_ERROR
        then
          begin
            dec(BytesLeft, BytesSent);
            Packet.Size := BytesLeft;
          end;
      result := Packet.Size = 0;
      fWriteAllowed := false;
    end;

  procedure TTCPLine.ReportError(const aText : string; code : integer);
    begin
      if GetOption(soConnecting)
        then
          begin
            Options[soConnecting] := false;
            if Assigned(fOnFailConnect)
              then fOnFailConnect(Self);
          end
        else
          if ConnectionLost(code)
            then Disconnect
            else inherited;
    end;

  procedure TTCPLine.ClearSpool;
    var
      Packet  : TSpooledPacket;
      total   : integer;
      SndFail : boolean;
    begin
      Lock;
      try
        if GetOption(soConnected)
          then
            begin
              total   := 0;
              SndFail := false;
              repeat
                Packet := TSpooledPacket(fSpoolBuffer[0]);
                inc(total, Packet.Size);
                if SendPacket(Packet)
                  then fSpoolBuffer.AtDelete(0)
                  else SndFail := true;
              until SndFail or (total > fTCPBufSize) or (fSpoolBuffer.Count = 0);
              if SndFail
                then CheckLastErrorSeverity;
            end;
      finally
        Unlock;
      end;
    end;

  // TTCPBinder

  procedure TTCPBinder.SetPort(aPort : integer);
    begin
      Lock;
      try
        fPort := aPort;
      {$IFNDEF SOCKET_OBJECT}
        if not (csDesigning in ComponentState)
          then
            if Listening
              then
                begin
                  StopListening;
                  StartListening;
                end
      {$ELSE}
        if Listening
          then
            begin
              StopListening;
              StartListening;
            end
      {$ENDIF}
      finally
        Unlock;
      end;
    end;

  function TTCPBinder.GetListening : boolean;
    begin
      result := Options[soListening];
    end;

  procedure TTCPBinder.SetListening(lstn : boolean);
    begin
      Lock;
      try
      {$IFNDEF SOCKET_OBJECT}
        if not (csDesigning in ComponentState)
          then
            if lstn
              then StartListening
              else StopListening
          else Options[soListening] := lstn;
      {$ELSE}
        if lstn
          then StartListening
          else StopListening;
      {$ENDIF}
      finally
        Unlock;
      end;
    end;

  procedure TTCPBinder.StartListening;
    const
      MaxBacklog = 5;
      error = 'Winsock error : ';
    var
      Addr : TSockAddr;
    begin
      Lock;
      try
        if not Listening
          then
            begin
              Socket := WinSock.Socket(PF_INET, SOCK_STREAM, 0);
              if Socket = INVALID_SOCKET
                then raise ESocketException.Create(error + IntToStr(WSAGetLastError));
              AllocateHandler;
              WSAASyncSelect(Socket, Handle, WM_ASYNCSELECT, FD_ACCEPT);
              Addr.Sin_Family := PF_INET;
              Addr.Sin_Addr.S_Addr := INADDR_ANY;
              Addr.sin_port := htons(fPort);
              if Bind(Socket, Addr, sizeof(Addr)) = SOCKET_ERROR
                then raise ESocketException.Create(error + IntToStr(WSAGetLastError));
              if Listen(Socket, MaxBacklog) = SOCKET_ERROR
                then raise ESocketException.Create(error + IntToStr(WSAGetLastError))
                else Options[soListening] := true;
            end;
      finally
        Unlock;
      end;
    end;

  procedure TTCPBinder.StopListening;
    begin
      Lock;
      try
        if Listening
          then
            begin
              CloseSocket(Socket);
              Socket := INVALID_SOCKET;
              DeallocateHandler;
              Options[soListening] := false;
            end;
      finally
        Unlock;
      end;
    end;

  procedure TTCPBinder.ResetNotifications;
    begin
      inherited;
      fOnAccept := nil;
    end;

  procedure TTCPBinder.WMAccept(var aMsg : TMessage);
    var
      TCPLine  : TTCPLine;
      Addr     : TSockAddr;
      AccSck   : TSocket;
      SizeAddr : integer;
    begin
      SizeAddr := sizeof(Addr);
      AccSck   := Accept(Socket, @Addr, @SizeAddr);
      if (AccSck <> INVALID_SOCKET) and Assigned(fOnAccept)
        then
          begin
            TCPLine := TTCPLine.AssignSocket(Owner, AccSck);
            if Assigned(fOnAccept)
              then fOnAccept(Self, TCPLine)
              else TCPLine.Free;
          end;
    end;

  procedure Register;
    begin
      RegisterComponents('Internet', [TTCPLine, TTCPBinder]);
    end;


  // Address registrations

  type
    TIPCache =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fAddresses : TStringList;
        public
          procedure SetIP(Addr : string; anIP : TIP);
          function  GetIP(Addr : string) : TIP;
      end;

  // TIPCache

  constructor TIPCache.Create;
    begin
      inherited;
      fAddresses := TStringList.Create;
      with fAddresses do
        begin
          Sorted     := true;
          Duplicates := dupError;
        end;
    end;

  destructor TIPCache.Destroy;
    begin
      fAddresses.Free;
      inherited;
    end;

  procedure TIPCache.SetIP(Addr : string; anIP : TIP);
    var
      index : integer;
    begin
      index := fAddresses.IndexOf(Addr);
      if index <> -1
        then fAddresses.Objects[index] := pointer(anIP)
        else fAddresses.AddObject(Addr, pointer(anIP));
    end;

  function TIPCache.GetIP(Addr : string) : TIP;
    var
      index : integer;
    begin
      result := inet_addr(PChar(Addr));
      if result = NoIP
        then
          begin
            index := fAddresses.IndexOf(Addr);
            if index <> -1
              then result := integer(fAddresses.Objects[index])
              else result := NoIP;
          end;
    end;

  // Winsock Initialization

  var
    InitData : TWSAData;
    IPCache  : TIPCache = nil;

  function GetIP(Addr : string) : TIP;
    begin
      result := IPCache.GetIP(Addr);
    end;

  procedure AddIP(Addr : string; IP : TIP);
    begin
      IPCache.SetIP(Addr, IP);
    end;

initialization

  if WSAStartup($101, InitData) <> 0
    then raise ESocketException.Create('WinSock.dll init failed');

  // IPCache := TIPCache.Create;

finalization

  WSACancelBlockingCall;
  WSACleanup;
  // IPCache.Free;

end.

