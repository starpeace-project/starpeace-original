unit smtp;

interface

  uses
    Windows, Winsock;

  type
    TSimpleSMTP =
      class
        public
          constructor Create(addr : string; port : integer = 25);
          destructor  Destroy; override;
        private
          fAddr   : string;
          fPort   : integer;
          fSocket : TSocket;
        public
          function Send(frm, rcpt, rcpt_cc, subject, msg : string) : integer;
        private
          function Connect : boolean;
          function SendCmd(text : string; wait : boolean = true) : integer;
      end;

implementation

  uses
    SysUtils, CompStringsParser;

  // TSimpleSMTP

  constructor TSimpleSMTP.Create(addr : string; port : integer);
    begin
      inherited Create;
      fAddr := addr;
      fPort := port;
    end;

  destructor TSimpleSMTP.Destroy;
    begin
      if fSocket <> 0
        then
          begin
            Winsock.shutdown(fSocket, 0);
            Winsock.closesocket(fSocket);
            fSocket := 0;
          end;
    end;

  function TSimpleSMTP.Send(frm, rcpt, rcpt_cc, subject, msg : string) : integer;
    var
      sock_addr : sockaddr_in;
      len       : integer;
      local_ip  : string;
      cmd       : string;
      aux       : string;
      p         : integer;
    begin
      if fSocket = 0
        then Connect;
      if fSocket <> 0
        then
          begin
           len := sizeof(sock_addr);
           Winsock.getsockname(fSocket, sock_addr, len);
            local_ip := StrPas(inet_ntoa(sock_addr.sin_addr));

            cmd := 'HELO [' + local_ip + ']' + ^M^J;
            result := SendCmd(cmd, true);
            if result <> 220
              then exit;

            cmd := 'MAIL FROM:' + '<' + frm + '>' + ^M^J;
            result := SendCmd(cmd);
            if result <> 250
              then exit;

            p := 1;
            aux := CompStringsParser.GetNextString(rcpt, p, [';', ',']);
            while aux <> '' do
              begin
                cmd := 'RCPT TO:' + '<' + aux + '>' + ^M^J;
                result := SendCmd(cmd);
                if result <> 250
                  then exit;
                aux := CompStringsParser.GetNextString(rcpt, p, [';', ',']);
              end;

            cmd := 'DATA' + ^M^J;
            SendCmd(cmd);

            cmd := 'Subject: ' + subject + ^M^J + 'From: ' + frm + ^M^J + 'To: ' + rcpt + ^M^J^M^J;
            SendCmd(cmd, false);
            SendCmd(msg, false);

            cmd := ^M^J + '.' + ^M^J;
            result := SendCmd(cmd);
            if result <> 250
              then exit;

            cmd := 'QUIT' + ^M^J;
            result := SendCmd(cmd);
          end
        else result := -1;
    end;

  function TSimpleSMTP.Connect : boolean;
    var
      SockAddrIn : TSockAddrIn;
      ipAddr     : in_addr;
      error      : dword;
      hostEnt    : PHostEnt;
    begin
      if inet_addr(PChar(fAddr)) = u_long(INADDR_NONE)
        then
          begin
            hostEnt := Winsock.gethostbyname(PChar(fAddr));
            if hostEnt <> nil
              then
                with ipAddr, hostEnt^ do
                  begin
                    S_un_b.s_b1 := h_addr^[0];
                    S_un_b.s_b2 := h_addr^[1];
                    S_un_b.s_b3 := h_addr^[2];
                    S_un_b.s_b4 := h_addr^[3];
                  end
              else ipAddr.S_addr := u_long(INADDR_NONE);
          end
        else ipAddr.S_addr := inet_addr(pchar(fAddr));
      if ipAddr.S_addr <> u_long(INADDR_NONE)
        then
          begin
            SockAddrIn.sin_family := PF_INET;
            SockAddrIn.sin_port   := htons(fPort);
            SockAddrIn.sin_addr   := ipAddr;
            fSocket := Winsock.socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
            if fSocket <> INVALID_SOCKET
              then error := WinSock.connect(fSocket, SockAddrIn, sizeof(SockAddrIn))
              else error := 0;
            if error <> 0
              then
                begin
                  Winsock.closesocket(fSocket);
                  fSocket := 0;
                end;
            result := true;
          end
        else result := false;
    end;

  function TSimpleSMTP.SendCmd(text : string; wait : boolean) : integer;
    var
      buffer : array[0..1023] of char;
      res    : string;
    begin
      FillChar(buffer, sizeof(buffer), 0);
      WinSock.send(fSocket, text[1], length(text), 0);
      if wait
        then
          begin
            WinSock.recv(fSocket, buffer[0], 1024, 0);
            res := copy(buffer, 0, 3);
            try
              result := StrToInt(res);
            except
              result := -1;
            end;
          end
        else result := 0;
    end;

end.
