unit Sockets;

interface

  uses
    Windows, Winsock, Classes, SysUtils, SyncObjs, Collection, SmartThreads,
    SpoolPackets;

  const
    MaxBackLog     = 5;
    flgPeek        = MSG_PEEK;
    flgGet         = 0;
    PacketSize     = 8*1024;
    DefaultBufSize = 1024;

  type
    TSocketOptions = integer;

  const
    soRead      = 1 shl 0;
    soWrite     = 1 shl 1;
    soReadWrite = soRead or soWrite;

  type
    TSocket       = class;
    TServerSocket = class;
    TClientSocket = class;
    TWinSocket    = Winsock.TSocket;

    TSocket =
      class(TSmartThread)
        public
          constructor Create;
          destructor  Destroy; override;
        protected
          fSocket : TWinSocket;
          fLock   : TCriticalSection;
        public
          procedure Close;   virtual;
          procedure Lock;   virtual;
          procedure Unlock; virtual;
      end;

    TAcceptEvent = procedure(Sender : TServerSocket; Socket : TClientSocket) of object;

    TServerSocket =
      class(TSocket)
        public
          constructor Create(Port : integer; AcceptEvent : TAcceptEvent);
        private
          fOnAccept : TAcceptEvent;
        protected
          procedure Execute; override;
      end;

    TPacket = array[0..PacketSize] of char;

    TSocketSetIndex = (ssiRead, ssiWrite, ssiError);
    TSocketSets     = array[TSocketSetIndex] of PFDSet;

    TClientSocketEvent      = procedure(Sender : TClientSocket)                  of object;
    TClientSocketReadEvent  = procedure(Sender : TClientSocket; Total : integer) of object;
    TClientSocketErrorEvent = procedure(Sender : TClientSocket; Error : integer) of object;

    TClientSocket =
      class(TSocket)
        public
          constructor Create(const Addr : string; Port : integer);
          constructor Accept(Socket : TWinSocket);
          destructor  Destroy; override;
        protected
          procedure   Connect(const Addr : string; Port : integer);
          procedure   Execute; override;
        private
          fSpoolBuffer : TCollection;
          fTCPBufSize  : integer;
          fConnected   : boolean;
          fOptions     : TSocketOptions;
          fOnClose     : TClientSocketEvent;
          fOnRead      : TClientSocketReadEvent;
          fOnWrite     : TClientSocketEvent;
          fOnError     : TClientSocketErrorEvent;
        public
          function  Read(var buf; count : integer) : integer;
          function  Peek(var buf; count : integer) : integer;
          procedure Write(var buf; count : integer);
          function  DataAvail : integer;
          function  ReadString(var str : string; CharCount : integer) : integer;
          function  PeekString(var str : string; CharCount : integer) : integer;
          function  ReadLine(var line : string) : boolean;
          procedure WriteString(var str : string);
          procedure WriteLine(const aLine : string);
          procedure ResetNotifications;
        private
          function  ReadData(var buffer; CharCount, flag : integer) : integer;
          procedure ClearSpool;
          function  SendPacket(Packet : TSpooledPacket) : boolean;
        public
          property Options : TSocketOptions          read fOptions write fOptions;
          property OnClose : TClientSocketEvent      read fOnClose write fOnClose;
          property OnRead  : TClientSocketReadEvent  read fOnRead  write fOnRead;
          property OnWrite : TClientSocketEvent      read fOnWrite write fOnWrite;
          property OnError : TClientSocketErrorEvent read fOnError write fOnError;
      end;

    ESocketError = class(Exception);

  // Initialize library

  function  Initialize : integer;

  // Unitialize library

  procedure Unitialize;


implementation

  // TSocket

  constructor TSocket.Create;
    begin
      inherited Create(true);
      FreeOnTerminate := true;
      fSocket := INVALID_SOCKET;
      fLock := TCriticalSection.Create;
    end;

  destructor TSocket.Destroy;
    begin
      Shutdown(fSocket, 2);
      CloseSocket(fSocket);
      fSocket := INVALID_SOCKET;
      fLock.Free;
    end;

  procedure TSocket.Close;
    begin
      Terminate;
      WaitFor;
    end;

  procedure TSocket.Lock;
    begin
      fLock.Enter;
    end;

  procedure TSocket.Unlock;
    begin
      fLock.Leave;
    end;

  // TServerSocket

  constructor TServerSocket.Create(Port : integer; AcceptEvent : TAcceptEvent);
    var
      Addr : TSockAddr;
    begin
      inherited Create;
      fOnAccept := AcceptEvent;
      fSocket := WinSock.Socket(PF_INET, SOCK_STREAM, 0);
      if fSocket = INVALID_SOCKET
        then raise ESocketError.Create('Cannot create the socket');
      Addr.Sin_Family := PF_INET;
      Addr.Sin_Addr.S_Addr := INADDR_ANY;
      Addr.sin_port := htons(Port);
      if (Bind(fSocket, Addr, sizeof(Addr)) = SOCKET_ERROR) or (Listen(fSocket, MaxBackLog) = SOCKET_ERROR)
        then raise ESocketError.Create('Cannot bind to port ' + IntToStr(Port));
    end;

  procedure TServerSocket.Execute;
    var
      AccSckt  : TWinSocket;
      Sockets  : PFDSet;
      Addr     : TSockAddr;
      AddrSize : integer;
      NewSckt  : TClientSocket;
      TimeOut  : TTimeVal;
    begin
      GetMem(Sockets, sizeof(integer) + sizeof(fSocket));
      while not Terminated do
        begin
          Sockets.fd_count := 1;
          Sockets.fd_array[0] := fSocket;
          TimeOut.tv_sec  := 0;
          TimeOut.tv_usec := 500; // Half a second
          if Select(0, Sockets, nil, nil, @TimeOut) = 1
            then
              begin
                AddrSize := sizeof(Addr);
                AccSckt  := Accept(fSocket, @Addr, @AddrSize);
                if AccSckt <> INVALID_SOCKET
                  then
                    begin
                      Lock;
                      try
                        if Assigned(fOnAccept)
                          then
                            begin
                              NewSckt := TClientSocket.Accept(AccSckt);
                              fOnAccept(Self, NewSckt)
                            end
                          else
                            begin
                              Shutdown(AccSckt, 2);
                              CloseSocket(AccSckt);
                            end;
                      finally
                        Unlock;
                      end;
                    end
                  else Terminate;
              end;
        end;
      FreeMem(Sockets);
    end;

  // TClientSocket

  constructor TClientSocket.Create(const Addr : string; Port : integer);
    begin
      inherited Create;
      fSpoolBuffer := TCollection.Create(0, rkBelonguer);
      fSocket := WinSock.Socket(PF_INET, SOCK_STREAM, 0);
      Priority := tpLower;
      if fSocket = INVALID_SOCKET
        then raise ESocketError.Create('Cannot create the socket');
      Connect(Addr, Port);
      fOptions := soReadWrite;
    end;

  procedure TClientSocket.Connect(const Addr : string; Port : integer);
    var
      SockAddr  : TSockAddrIn;
      HostEntry : PHostEnt;
      size      : integer;
    begin
      SockAddr.sin_family := AF_INET;
      SockAddr.sin_port := htons(Port);
      SockAddr.sin_addr.s_addr := inet_addr(pchar(Addr));
      if SockAddr.sin_addr.s_addr = INADDR_NONE
        then
          begin
            HostEntry := GetHostByName(pchar(Addr));
            if HostEntry <> nil
              then
                begin
                  SockAddr.sin_addr.s_un_b.s_b1 := HostEntry.h_addr_list^[0];
                  SockAddr.sin_addr.s_un_b.s_b2 := HostEntry.h_addr_list^[1];
                  SockAddr.sin_addr.s_un_b.s_b3 := HostEntry.h_addr_list^[2];
                  SockAddr.sin_addr.s_un_b.s_b4 := HostEntry.h_addr_list^[3];
                end
              else raise ESocketError.Create('Invalid address');
          end;
      if Winsock.Connect(fSocket, SockAddr, sizeof(SockAddr)) <> 0
        then raise ESocketError.Create('Cannot connect to ' + Addr)
        else
          begin
            size := sizeof(fTCPBufSize);
            if GetSockOpt(fSocket, SOL_SOCKET, SO_SNDBUF, @fTCPBufSize, size) <> 0
              then fTCPBufSize := DefaultBufSize;
            fConnected := true;
          end;
    end;

  constructor TClientSocket.Accept(Socket : TWinSocket);
    var
      size : integer;
    begin
      inherited Create;
      fSpoolBuffer := TCollection.Create(0, rkBelonguer);
      CloseSocket(fSocket);
      fSocket := Socket;
      size := sizeof(fTCPBufSize);
      if GetSockOpt(fSocket, SOL_SOCKET, SO_SNDBUF, @fTCPBufSize, size) <> 0
        then fTCPBufSize := DefaultBufSize;
      fConnected := true;
    end;

  destructor TClientSocket.Destroy;
    begin
      fSpoolBuffer.Free;
      inherited;
    end;

  procedure TClientSocket.Execute;
    var
      res        : integer;
      RecvSize   : integer;
      LastError  : integer;
      Packet     : TPacket;
      SocketSets : TSocketSets;
      TimeOut    : TTimeVal;
    begin
      if fOptions and soRead <> 0
        then new(SocketSets[ssiRead])
        else SocketSets[ssiRead] := nil;
      if fOptions and soWrite <> 0
        then new(SocketSets[ssiWrite])
        else SocketSets[ssiWrite] := nil;
      new(SocketSets[ssiError]);
      while not Terminated and fConnected do
        begin
          // Sign socket to recv read notifications
          if fOptions and soRead <> 0
            then
              begin
                SocketSets[ssiRead ].fd_count    := 1;
                SocketSets[ssiRead ].fd_array[0] := fSocket;
              end;
          // Sign socket to recv write notifications
          if fOptions and soWrite <> 0
            then
              begin
                SocketSets[ssiWrite].fd_count    := 1;
                SocketSets[ssiWrite].fd_array[0] := fSocket;
              end;
          // Sign socket to errors write notifications
          SocketSets[ssiError].fd_count    := 1;
          SocketSets[ssiError].fd_array[0] := fSocket;
          // ask winsock about last events
          TimeOut.tv_sec  := 0;
          TimeOut.tv_usec := 500; // Half a second
          res := Select(fSocket, SocketSets[ssiRead], SocketSets[ssiWrite], SocketSets[ssiError], @TimeOut);
          // execute events
          Lock;
          try
            if (res <> SOCKET_ERROR) and not Terminated and fConnected
              then
                begin
                  if res > 0
                    then
                      begin
                        // Check if there is data to read
                        if (fOptions and soRead <> 0) and (SocketSets[ssiRead].fd_count = 1) and not Terminated
                          then
                            begin
                              RecvSize := 0;
                              if ((IOCtlSocket(fSocket, FIONREAD, RecvSize) = SOCKET_ERROR) and (WSAGetLastError = WSAENETDOWN)) or (RecvSize = 0)
                                then
                                  fConnected := false
                                else
                                  begin
                                    if Assigned(fOnRead)
                                      then fOnRead(Self, RecvSize)
                                      else recv(fSocket, Packet, sizeof(Packet), flgGet);
                                  end
                            end;
                        // Check if it is allowed to write
                        if (fOptions and soWrite <> 0) and (SocketSets[ssiWrite].fd_count = 1) and fConnected and not Terminated
                          then
                            begin
                              if Assigned(fOnWrite)
                                then fOnWrite(Self);
                              if fSpoolBuffer.Count > 0
                                then ClearSpool;
                            end;
                        // Check if there are errors
                        if (SocketSets[ssiError].fd_count = 1) and fConnected and not Terminated
                          then
                            begin
                              LastError := WSAGetLastError;
                              case LastError of
                                WSAENETDOWN, WSAESHUTDOWN, WSAECONNABORTED, WSAENETRESET, WSAECONNRESET :
                                  fConnected := false;
                                else
                                  if (LastError <> 0) and Assigned(fOnError)
                                    then fOnError(Self, LastError);
                              end;
                            end;
                      end
                end
              else
                case WSAGetLastError of
                  WSAENETDOWN, WSAESHUTDOWN, WSAECONNABORTED, WSAENETRESET, WSAECONNRESET :
                    fConnected := false;
                end;
          finally
            Unlock;
          end;
        end;
      if fOptions and soRead <> 0
        then dispose(SocketSets[ssiRead]);
      if fOptions and soWrite <> 0
        then dispose(SocketSets[ssiWrite]);
      dispose(SocketSets[ssiError]);
      if Assigned(fOnClose)
        then fOnClose(Self);
    end;

  function TClientSocket.Read(var buf; count : integer) : integer;
    begin
      result := ReadData(buf, count, flgGet);
    end;

  function TClientSocket.Peek(var buf; count : integer) : integer;
    begin
      result := ReadData(buf, count, flgPeek);
    end;

  procedure TClientSocket.Write(var buf; count : integer);
    begin
      Lock;
      try
        fSpoolBuffer.Insert(TSpooledPacket.Create(pchar(@buf), count, true));
      finally
        Unlock;
      end;
    end;

  function TClientSocket.DataAvail : integer;
    begin
      Lock;
      try
        if ioctlsocket(fSocket, FIONREAD, result) <> 0
          then result := 0
      finally
        Unlock;
      end;
    end;

  function TClientSocket.ReadString(var str : string; CharCount : integer) : integer;
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

  function TClientSocket.PeekString(var str : string; CharCount : integer) : integer;
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

  function TClientSocket.ReadLine(var line : string) : boolean;
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

  procedure TClientSocket.WriteString(var str : string);
    begin
      if str <> ''
        then Write(str[1], length(str));
    end;

  procedure TClientSocket.WriteLine(const aLine : string);
    var
      aux : string;
    begin
      aux := aLine + #$D#$A; // >>
      WriteString(aux);
    end;

  procedure TClientSocket.ResetNotifications;
    begin
      fOnClose := nil;
      fOnRead  := nil;
      fOnWrite := nil;
    end;

  function TClientSocket.ReadData(var buffer; CharCount, flag : integer) : integer;
    begin
      Lock;
      try
        result := recv(fSocket, buffer, CharCount, flag);
        if result < 0
          then result := 0;
      finally
        Unlock;
      end;
    end;

  procedure TClientSocket.ClearSpool;
    var
      Packet  : TSpooledPacket;
      total   : integer;
      SndFail : boolean;
    begin
      total   := 0;
      SndFail := false;
      repeat
        Packet := TSpooledPacket(fSpoolBuffer[0]);
        inc(total, Packet.Size);
        if SendPacket(Packet)
          then fSpoolBuffer.AtDelete(0)
          else SndFail := true;
      until SndFail or (total >= fTCPBufSize) or (fSpoolBuffer.Count = 0);
    end;

  function TClientSocket.SendPacket(Packet : TSpooledPacket) : boolean;
    var
      BytesSent  : integer;
      BytesLeft  : integer;
      aux        : pchar;
    begin
      BytesLeft := Packet.Size;
      aux       := Packet.Buffer;
      if BytesLeft < fTCPBufSize
        then BytesSent := Send(fSocket, aux^, BytesLeft, 0)
        else BytesSent := Send(fSocket, aux^, fTCPBufSize, 0);
      if BytesSent <> SOCKET_ERROR
        then
          begin
            dec(BytesLeft, BytesSent);
            Packet.Size := BytesLeft;
          end;
      result := Packet.Size = 0;
    end;

  function Initialize : integer;
    var
      WSAData : TWSAData;
    begin
      result := WSAStartup($0101, WSAData);
    end;

  procedure Unitialize;
    begin
      WSACleanup;
    end;

end.
