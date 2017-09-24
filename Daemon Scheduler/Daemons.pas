unit Daemons;

interface

  uses
    DirServerSession, SyncObjs, IniFiles, RDOInterfaces, RDOObjectProxy;

  type
    IDaemon =
      interface
        procedure InitDaemon(const DSAddr : string; DSPort : integer);
        function  GetName : string;
        function  GetDescription : string;
        function  IsRunning : boolean;
        procedure Run;
        procedure SynchronizedRun;
        function  GetPeriod : integer;
        procedure SetPeriod(period : integer);
        function  LastRun : integer;
        function  ShowPropertiesUI : boolean;
      end;

  type
    TDaemonCreatorRoutine = function(const DSAddr : string; const DSPort : integer) : IDaemon;

  const
    cDaemonCreatorRoutineName = 'CreateDaemon';

  type
    TBasicDaemon =
      class(TInterfacedObject, IDaemon)
        public
          constructor Create(const DSAddr : string; DSPort : integer);
          destructor  Destroy;                                           override;
        protected // IDaemon
          procedure InitDaemon(const DSAddr : string; DSPort : integer); virtual;
          function  GetName : string;                                    virtual;
          function  GetDescription : string;                             virtual;
          function  IsRunning : boolean;
          procedure Run;
          procedure SynchronizedRun;
          function  GetPeriod : integer;
          procedure SetPeriod(period : integer);
          function  LastRun : integer;
          function  ShowPropertiesUI : boolean;                          virtual;
        private
          fDSAddr         : string;
          fDSPort         : integer;
          fDSConnection   : IRDOConnectionInit;
          fDSProxy        : variant;
          fConnected      : boolean;
          fConnTimeOut    : integer;
          fCallTimeOut    : integer;
          fServerTruceLen : integer;
          fLock           : TCriticalSection;
          fPeriod         : integer;
          fRunning        : boolean;
          fLastRun        : integer;
        protected
          fSession        : IDirServerSession;
        private
          function  InitDSConnection(const DSAddr : string; DSPort : integer) : boolean;
          function  InitSession : boolean;
          procedure FreeDSConnectionObjs;
          procedure ReadConfigData;
          procedure OnDisconnectFromDS(const ClientConnection : IRDOConnection);
        protected
          procedure Execute;                                             virtual;
          procedure SynchronizedExecute;                                 virtual;
          procedure ReadConfiguration(IniFile : TIniFile);               virtual;
          function  GetLogId : string;                                   virtual;
      end;

implementation

  uses
    Windows, SysUtils, WinsockRDOConnection, Logs;

  const
    cDefaultPeriod = 60*1000; //1*1000;

  const
    cDefConnTimeOut    = 20000;
    cDefCallTimeOut    = 120000;
    cDefServerTruceLen = 5*60*1000;//60*1000;

  const
    cDSObjectName = 'DirectoryServer';

  // TBasicDaemon

  constructor TBasicDaemon.Create(const DSAddr : string; DSPort : integer);
    begin
      inherited Create;
      fDSAddr := DSAddr;
      fDSPort := DSPort;
      fConnTimeOut := cDefConnTimeOut;
      fCallTimeOut := cDefCallTimeOut;
      fServerTruceLen := cDefServerTruceLen;
      fLock := TCriticalSection.Create;
      fPeriod := cDefaultPeriod;
      ReadConfigData;
    end;

  destructor TBasicDaemon.Destroy;
    begin
      fLock.Free;
      FreeDSConnectionObjs;
      inherited;
    end;

  procedure TBasicDaemon.InitDaemon(const DSAddr : string; DSPort : integer);
    begin
      fDSAddr := DSAddr;
      fDSPort := DSPort;
    end;

  function TBasicDaemon.GetName : string;
    begin
      Result := 'Basic daemon';
    end;

  function TBasicDaemon.GetDescription : string;
    begin
      Result := 'Basic daemon implementation';
    end;

  function TBasicDaemon.IsRunning : boolean;
    begin
      fLock.Enter;
      try
        Result := fRunning;
      finally
        fLock.Leave;
      end;
    end;

  procedure TBasicDaemon.Run;
    var
      executed     : boolean;
      initickcount : integer;
    const
      DebugCntLost : longint = 0;
    begin
      fLock.Enter;
      try
        fRunning := true;
      finally
        fLock.Leave;
      end;
      initickcount := GetTickCount;
      if fConnected
        then
          if InitSession
            then
              try
                try
                  Execute;
                  executed := true;
                except
                  executed := false;
                end;
              finally
                fSession := nil;
              end
            else executed := false
        else
          begin
            FreeDSConnectionObjs;
            Inc(DebugCntLost);
            if InitDSConnection(fDSAddr, fDSPort) and InitSession
              then
                try
                  try
                    Execute;
                    executed := true;
                  except
                    executed := false;
                  end;
                finally
                  fSession := nil;
                end
              else executed := false;
          end;
      fLock.Enter;
      try
        if executed
          then fLastRun := GetTickCount
          else
            if fPeriod < fServerTruceLen
              then inc(fLastRun, (longint(GetTickCount) - initickcount) + 2*fPeriod)
              else inc(fLastRun, (longint(GetTickCount) - initickcount) + fServerTruceLen);
        fRunning := false;
      finally
        fLock.Leave;
      end;
    end;

  procedure TBasicDaemon.SynchronizedRun;
    begin
      SynchronizedExecute;
    end;

  function TBasicDaemon.GetPeriod : integer;
    begin
      Result := fPeriod;
    end;

  procedure TBasicDaemon.SetPeriod(period : integer);
    begin
      fPeriod := period;
    end;

  function TBasicDaemon.LastRun : integer;
    begin
      fLock.Enter;
      try
        Result := fLastRun;
      finally
        fLock.Leave;
      end;
    end;

  function TBasicDaemon.ShowPropertiesUI : boolean;
    begin
      Result := false;
    end;

  function TBasicDaemon.InitDSConnection(const DSAddr : string; DSPort : integer) : boolean;
    begin
      try
        fDSConnection := TWinsockRDOConnection.Create('DS');
        fDSConnection.Server := DSAddr;
        fDSConnection.Port := DSPort;
        (fDSConnection as IRDOConnection).OnDisconnect := OnDisconnectFromDS;
        Log(GetLogId, 'Trying to connect to Directory Server at IP=' + DSAddr + ' and Port=' + IntToStr(DSPort));
        if fDSConnection.Connect(fConnTimeOut)
          then
            begin
              Log(GetLogId, 'Sucessfully connected to Directory Server at IP=' + DSAddr + ' and Port=' + IntToStr(DSPort));
              fConnected := true;
              fDSProxy := IDispatch(TRDOObjectProxy.Create);// as IDispatch;
              fDSProxy.SetConnection(fDSConnection);
              fDSProxy.WaitForAnswer := true;
              fDSProxy.TimeOut := fCallTimeOut;
              if fDSProxy.BindTo(cDSObjectName)
                then
                  begin
                    Log(GetLogId, 'Successfully bound to ' + cDSObjectName + ' object');
                    Result := true;
                  end
                else
                  begin
                    Log(GetLogId, 'Couldn''t bind to ' + cDSObjectName + ' object');
                    Result := false;
                  end;
            end
          else
            begin
              Log(GetLogId, 'Couldn''t connect to the Directory Server');
              Result := false;
            end;
      except
        Result := false;
        Log(GetLogId, 'Exception generated while trying to connect to Directory Server');
      end;
    end;

  function TBasicDaemon.InitSession : boolean;
    var
      sessionid    : integer;
      SessionProxy : variant;
    begin
      try
        sessionid := fDSProxy.RDOOpenSession;
        if sessionid <> 0
          then
            begin
              SessionProxy := IDispatch(TRDOObjectProxy.Create);// as IDispatch;
              SessionProxy.SetConnection(fDSConnection);
              SessionProxy.WaitForAnswer := true;
              SessionProxy.TimeOut := fCallTimeOut;
              if SessionProxy.BindTo(sessionid)
                then
                  begin
                    fSession := TDirServerSession.Create(SessionProxy);
                    Result := true;
                  end
                else
                  begin
                    Log(GetLogId, 'Couldn''t bind to session object');
                    Result := false;
                  end;
            end
          else
            begin
              Log(GetLogId, 'Couldn''t get session from Directory Server');
              Result := false;
            end;
      except
        Log(GetLogId, 'Couldn''t get session from Directory Server');
        Result := false;
      end;
    end;

  procedure TBasicDaemon.FreeDSConnectionObjs;
    begin
      fDSProxy := unassigned;
      fDSConnection := nil;
    end;

  procedure TBasicDaemon.ReadConfigData;
    const
      cMaxNameLength = 300;
    var
      IniFile    : TIniFile;
      modulename : string;
      fnamelen   : integer;

    function GenerateIniName(const modulename : string) : string;
      var
        inifilename : string;
        i           : integer;
      begin
        inifilename := modulename;
        i := length(inifilename);
        while inifilename[i] <> '.' do
          begin
            dec(i);
            setlength(inifilename, i);
          end;
        inifilename := inifilename + 'ini';
        Result := inifilename;
      end;

    begin
      setlength(modulename, cMaxNameLength);
      fnamelen := GetModuleFileName(HInstance, pchar(modulename), cMaxNameLength);
      if fnamelen > 0
        then
          begin
            setlength(modulename, fnamelen);
            try
              IniFile := TIniFile.Create(GenerateIniName(modulename));
              try
                fPeriod := IniFile.ReadInteger('General', 'Period', fPeriod);
                ReadConfiguration(IniFile);
              finally
                IniFile.Free;
              end;
            except
            end;
          end;
    end;

  procedure TBasicDaemon.OnDisconnectFromDS(const ClientConnection : IRDOConnection);
    begin
      fConnected := false;
      Log(GetLogId, 'Disconnected from Directory Server');
    end;

  procedure TBasicDaemon.Execute;
    begin
    end;

  procedure TBasicDaemon.SynchronizedExecute;
    begin
    end;

  procedure TBasicDaemon.ReadConfiguration(IniFile : TIniFile);
    begin
      fPeriod := IniFile.ReadInteger('General', 'Period', fPeriod);
      fConnTimeOut := IniFile.ReadInteger('General', 'ConnTimeOut', fConnTimeOut);
      fCallTimeOut := IniFile.ReadInteger('General', 'CallTimeOut', fCallTimeOut);
      fServerTruceLen := IniFile.ReadInteger('General', 'ServerTruceLen', fServerTruceLen);
    end;

  function TBasicDaemon.GetLogId : string;
    begin
      Result := 'Basic Daemon';
    end;

end.
