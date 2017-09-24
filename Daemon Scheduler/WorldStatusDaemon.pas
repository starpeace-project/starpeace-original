unit WorldStatusDaemon;

interface

  uses
    DirServerSession, Daemons;

  function CreateDaemon(const DSAddr : string; DSPort : integer) : IDaemon;

implementation

  uses
    Windows, Classes, SysUtils, Forms, RDOInterfaces, RDOObjectProxy, WinSockRDOConnection,
    Protocol, ISP3, IniFiles, SyncObjs, Logs;

  const
    cLogId = 'World Status Daemon';

  type
    TVoidThread =
      class(TThread)
          procedure Execute; override;
      end;

    TWorldStatusDaemon =
      class(TBasicDaemon)
        public
          constructor Create(const DSAddr : string; DSPort : integer);
          destructor  Destroy;                                                       override;
        protected // IDaemon
          function  GetName : string;                                                override;
          function  GetDescription : string;                                         override;
        protected
          procedure Execute;                                                         override;
          procedure SynchronizedExecute;                                             override;
          procedure ReadConfiguration(IniFile : TIniFile);                           override;
          function  GetLogId : string;                                               override;
        private
          procedure CheckWorldsStatus;
          procedure InitSMTPControl;
          procedure ReportWorldDown(const Area, World, IP : string; port : integer);
        private
          fISConnTimeOut : integer;
          fISCallTimeOut : integer;
        private
          fMessagesToSend     : TList;
          fMessagesToSendLock : TCriticalSection;
        private
          {
          fISConn  : IRDOConnectionInit;
          fISProxy : variant;
          }
        private
          fSMTP              : TSMTP;
          fSMTPServerAddress : string;
          fSMTPServerPort    : integer;
          fSMTPServerTimeOut : integer;
          fReportAddresses   : TStringList;
          fSMTPConnected     : boolean;
          fError             : boolean;
          fVoidThread        : TVoidThread;

          procedure SMTPDocInput(Sender: TObject; const DocInput: DocInput);
          procedure SMTPError(Sender: TObject; Number: Smallint; var Description: WideString; Scode: Integer; const Source,
                                              HelpFile: WideString; HelpContext: Integer; var CancelDisplay: WordBool);
          procedure SMTPStateChanged(Sender: TObject; State: Smallint);
      end;

procedure TVoidThread.Execute;
  begin
    while not Terminated do
      begin
        application.ProcessMessages;
        sleep(6000);
      end;
  end;

  function CreateDaemon(const DSAddr : string; DSPort : integer) : IDaemon;
    begin
      Result := TWorldStatusDaemon.Create(DSAddr, DSPort);
    end;

  // Utilities

  function NoParam : variant;
    begin
      TVarData(Result).VType := varError;
      TVarData(Result).VError := DWORD(DISP_E_PARAMNOTFOUND);
    end;

  const
    cDefaultPeriod = 60*1000;

  const
    cDefSMTPServerTimeOut = 10000;

  const
    cDefISConnTimeOut = 30000;
    cDefISCallTimeOut = 20000;

  type
    PMessageRec = ^TMessageRec;
    TMessageRec =
      record
        addrto      : string;
        messagetext : string;
      end;

  // TWorldStatusDaemon

  constructor TWorldStatusDaemon.Create(const DSAddr : string; DSPort : integer);
    begin
      inherited;
      fISConnTimeOut := cDefISConnTimeOut;
      fISCallTimeOut := cDefISCallTimeOut;
      fMessagesToSend := TList.Create;
      fMessagesToSendLock := TCriticalSection.Create;
      fVoidThread := TVoidThread.Create(false);
      {
      fISConn := TWinSockRDOConnection.Create;
      fISProxy := TRDOObjectProxy.Create as IDispatch;
      fISProxy.TimeOut := fISCallTimeOut;
      fISProxy.SetConnection(fISConn);
      }
      fSMTPServerTimeOut := cDefSMTPServerTimeOut;
      if (fReportAddresses <> nil) and (fReportAddresses.Count > 0)
        then
          begin
            Log(cLogId, 'Initializing mail component');
            InitSMTPControl;
          end;
    end;

  destructor TWorldStatusDaemon.Destroy;
    begin
      if assigned(fSMTP) and fSMTPConnected
        then fSMTP.Quit;
      {
      fISProxy := NULL;
      fISConn := nil;
      }
      fMessagesToSendLock.Free;
      fMessagesToSend.Free;
      fVoidThread.Free;
      inherited;
    end;

  function TWorldStatusDaemon.GetName : string;
    begin
      Result := cLogId;
    end;

  function TWorldStatusDaemon.GetDescription : string;
    begin
      Result := 'World status checker daemon';
    end;

  procedure TWorldStatusDaemon.Execute;
    begin
      inherited;
      CheckWorldsStatus;
    end;

  procedure TWorldStatusDaemon.SynchronizedExecute;

    function SendMessage(const addrto, messagetext : string) : boolean;
      var
        tickcount : integer;
      begin
        if not fSMTPConnected
          then
            begin
              fError := false;
              fSMTP.Connect(NoParam, NoParam);
              tickcount := GetTickCount;
              while not fSMTPConnected and not fError and ((longint(GetTickCount) - tickcount) <= fSMTPServerTimeOut) do
                Application.ProcessMessages;
            end;
        if fSMTPConnected
          then
            with fSMTP do
              begin
                DocInput.Headers.Clear;
                DocInput.Headers.Add('To', addrto);
                DocInput.Headers.Add('From', 'worldstatusdaemon@starpeace.net');
                //DocInput.Headers.Add('CC', CC.Text);
                DocInput.Headers.Add('Subject', 'World Down');
                DocInput.Headers.Add('Message-Id', Format('%s_%s', [cLogId, DateTimeToStr(Now)]));
                DocInput.Headers.Add('Content-Type', 'TEXT/PLAIN charset=US-ASCII');
                SendDoc(NoParam, DocInput.Headers, messagetext, '', '');
                {
                while Busy do;
                  Application.ProcessMessages;
                }
                Result := true;
              end
          else Result := false;
      end;

    var
      messagerec : PMessageRec;
    begin
      inherited;
      if (fSMTP <> nil) and not fSMTP.Busy
        then
          begin
            fMessagesToSendLock.Enter;
            try
              if fMessagesToSend.Count > 0
                then
                  begin
                    messagerec := fMessagesToSend[0];
                    fMessagesToSend.Delete(0);
                  end
                else messagerec := nil;
            finally
              fMessagesToSendLock.Leave;
            end;
            if messagerec <> nil
              then
                begin
                  Log(cLogId, 'Mailing message to: ' + messagerec.addrto);
                  try
                    if not SendMessage(messagerec.addrto, messagerec.messagetext)
                      then Log(cLogId, 'Error while trying to send mail to: ' + messagerec.addrto);
                  except
                    on E : Exception do
                      Log(cLogId, 'Exception "' + E.Message + '" generated while trying to send mail to: ' + messagerec.addrto);
                  end;
                end;
          end;
    end;

  procedure TWorldStatusDaemon.ReadConfiguration(IniFile : TIniFile);
    var
      reportaddrs : string;

    procedure ParseReportAddrs;
      var
        address  : string;
        commapos : integer;

      function Trim(const str : string) : string;
        var
          i, j : integer;
          k    : integer;
        begin
          if str <> ''
            then
              begin
                i := 1;
                while str[i] = ' ' do
                  inc(i);
                j := length(str);
                while str[j] = ' ' do
                  dec(j);
                k := 1;
                SetLength(Result, j - i + 1);
                while i <= j do
                  begin
                    Result[k] := str[i];
                    inc(k);
                    inc(i);
                  end;
              end
            else Result := str;
        end;

      begin
        commapos := pos(',', reportaddrs);
        while commapos > 0 do
          begin
            address := copy(reportaddrs, 1, commapos - 1);
            address := Trim(address);
            fReportAddresses.Add(address);
            delete(reportaddrs, 1, commapos);
            commapos := pos(',', reportaddrs);
          end;
        if reportaddrs <> ''
          then
            begin
              address := Trim(reportaddrs);
              fReportAddresses.Add(address);
            end;
      end;

    begin
      inherited;
      fISConnTimeOut := IniFile.ReadInteger('General', 'ISConnTimeOut', fISConnTimeOut);
      fISCallTimeOut := IniFile.ReadInteger('General', 'ISCallTimeOut', fISCallTimeOut);
      fSMTPServerAddress := IniFile.ReadString('General', 'SMTPServer', '206.186.157.8');
      fSMTPServerPort := IniFile.ReadInteger('General', 'SMTPPort', 25);
      fSMTPServerTimeOut := IniFile.ReadInteger('General', 'SMTPServerTimeOut', fSMTPServerTimeOut);
      reportaddrs := IniFile.ReadString('General', 'ReportAddresses', '');
      if reportaddrs <> ''
        then
          begin
            fReportAddresses := TStringList.Create;
            ParseReportAddrs;
          end;
    end;

  function TWorldStatusDaemon.GetLogId : string;
    begin
      Result := cLogId;
    end;

  procedure TWorldStatusDaemon.CheckWorldsStatus;
    var
      basekey    : widestring;
      key        : widestring;
      Areas      : TStringList;
      Worlds     : TStringList;
      i, j       : integer;
      worldwasok : boolean;
      worldok    : boolean;
      IP         : string;
      port       : integer;
      ISConn     : IRDOConnectionInit;
      ISProxy    : olevariant;

    function CheckInterfaceServer(const ISAddress : string; Port : integer) : boolean;
      var
        usercount : integer;
      begin
        ISConn.Server := ISAddress;
        ISConn.Port := Port;
        if ISConn.Connect(fISConnTimeOut{cISConnTimeOut}) and ISProxy.BindTo(tidRDOHook_InterfaceServer)
          then
            try
              try
                usercount := ISProxy.UserCount; // >> other checks could be done
                if usercount > 0
                  then Result := true
                  else Result := false;
              finally
                ISConn.Disconnect;
              end;
            except
              Result := false;
            end
          else Result := false;

{        Result := false;
        try
          try
            conected := false;
            ISConn.Server := ISAddress;
            ISConn.Port := Port;
            if ISConn.Connect(fISConnTimeOut)
              then
                begin
                  conected := true;
                  if ISProxy.BindTo(tidRDOHook_InterfaceServer)
                    then
                      try
                        try
                          usercount := ISProxy.UserCount; // >> other checks could be done
                          if usercount > 0
                            then Result := true
                            else Result := false;
                        finally
                          //ISConn.Disconnect;
                          //ISConn := nil;
                        end;
                      except
                        Result := false;
                      end
                    else Result := false;
                end;
          except
            Result := false;
          end;
        finally
           ISConn.Disconnect;
        end;}
      end;

    begin
      try
        Areas := TStringList.Create;
        try
          Worlds := TStringList.Create;
          try
            basekey := 'Root/Areas';
            if fSession.SetCurrentKey(basekey)
              then
                begin
                  Areas.Text := fSession.GetKeyNames;
                  for i := 0 to pred(Areas.Count) do
                    begin
                      key := basekey + '/' + Areas[i] + '/Worlds';
                      if fSession.SetCurrentKey(key)
                        then
                          begin
                            Worlds.Text := fSession.GetKeyNames;
                            //for k:= 0 to 200000 do
                            for j := 0 to pred(Worlds.Count) do
                              begin
                                key := key + '/' + Worlds[j] + '/Interface';
                                if fSession.SetCurrentKey(key)
                                  then
                                    begin
                                      ISConn := TWinSockRDOConnection.Create('IS');
                                      try
                                        ISProxy := IDispatch(TRDOObjectProxy.Create);
                                        try
                                          ISProxy.SetConnection(ISConn);
                                          ISProxy.TimeOut := fISCallTimeOut;
                                          IP := fSession.ReadString('IP');
                                          port := fSession.ReadInteger('Port');
                                          worldwasok := fSession.ReadBoolean('Running');
                                          worldok := CheckInterfaceServer(IP, port);
                                          if not worldok
                                            then
                                              begin
                                                Log(cLogId, 'World ' + Worlds[j] + ' in area ' + Areas[i] + ' found down at ' + DateTimeToStr(Now));
                                                if worldwasok
                                                  then ReportWorldDown(Areas[i], Worlds[j], IP, port);
                                              end
                                            else Log(cLogId, 'World ' + Worlds[j] + ' in area ' + Areas[i] + ' was ok at ' + DateTimeToStr(Now));
                                          if worldwasok <> worldok
                                            then fSession.WriteBoolean('Running', worldok);
                                        finally
                                          ISProxy := unassigned;
                                        end;
                                      finally
                                        ISConn := Nil;
                                      end;
                                    end
                                  else Log(cLogId, 'Key ' + key + ' not found, or unable to be set');
                                key := basekey + '/' + Areas[i] + '/Worlds';
                              end;
                          end
                        else Log(cLogId, 'Key ' + key + ' not found, or unable to be set');
                    end;
                end
              else Log(cLogId, 'Key ' + basekey + ' not found, or unable to be set');
          finally
            Worlds.Free;
          end;
        finally
          Areas.Free;
        end;
      except
        on e : Exception do
          Log(cLogId, 'Exception "' + e.Message + '" while checking worlds');
        else
          Log(cLogId, 'Unknown exception while checking worlds');
      end;
    end;

  procedure TWorldStatusDaemon.InitSMTPControl;
    var
      tickcount : integer;
    begin
      fSMTP := TSMTP.Create(nil);
      fSMTP.OnDocInput := SMTPDocInput;
      fSMTP.OnStateChanged := SMTPStateChanged;
      fSMTP.OnError := SMTPError;
      fSMTP.RemoteHost := fSMTPServerAddress;
      fSMTP.RemotePort := fSMTPServerPort;
      fSMTP.Connect(NoParam, NoParam);
      tickcount := GetTickCount;
      while not fSMTPConnected and not fError and ((longint(GetTickCount) - tickcount) <= fSMTPServerTimeOut) do
        Application.ProcessMessages;
    end;

  procedure TWorldStatusDaemon.ReportWorldDown(const Area, World, IP : string; port : integer);
    var
      i           : integer;
      messagetext : string;
      messagerec  : PMessageRec;
    begin
      try
        if (fReportAddresses <> nil) and (fReportAddresses.Count > 0)
          then
            begin
              messagetext := 'World ' + World + ' in area ' + Area + ' is down!' + #10#13 + 'Server address is ' + IP + ' and server port is ' + IntToStr(port) + '.';
              fMessagesToSendLock.Enter;
              try
                for i := 0 to pred(fReportAddresses.Count) do
                  begin
                    new(messagerec);
                    messagerec.addrto := fReportAddresses[i];
                    messagerec.messagetext := messagetext;
                    fMessagesToSend.Add(messagerec);
                    Log(cLogId, 'Message to: ' + fReportAddresses[i] + ' enqueued');
                  end;
              finally
                fMessagesToSendLock.Leave;
              end;
            end;
      except
      end;
    end;

  procedure TWorldStatusDaemon.SMTPDocInput(Sender: TObject; const DocInput: DocInput);
    begin
      case DocInput.State of
        icDocBegin:; // Initiating document transfer
        icDocHeaders:; // Sending headers
        icDocData:; // Sending ...
        icDocEnd:; // Document completely sent or error
      end;
    end;

  procedure TWorldStatusDaemon.SMTPError(Sender: TObject; Number: Smallint; var Description: WideString; Scode: Integer; const Source,
                                         HelpFile: WideString; HelpContext: Integer; var CancelDisplay: WordBool);
    begin
      CancelDisplay := true;
      fError := true;
      Log(cLogId, 'SMTP error');
    end;

  procedure TWorldStatusDaemon.SMTPStateChanged(Sender: TObject; State: Smallint);
    begin
      case State of
        prcConnecting:;
        prcResolvingHost:;
        prcHostResolved:;
        prcConnected:
          begin
            fSMTPConnected := true;
            Log(cLogId, 'Successfully connected to SMTP server');
          end;
        prcDisconnecting:;
        prcDisconnected:
          begin
            fSMTPConnected := false;
            Log(cLogId, 'Disconnected from SMTP server');
          end;
      end;
    end;

end.
