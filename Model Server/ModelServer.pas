{$DEFINE USELogs}
unit ModelServer;

interface

  uses
    Windows, Events, Classes, Collection, Protocol, Kernel, World, Population, RDOServer, ExtCtrls,
    BackupObjects, SyncObjs, RDOInterfaces, MailServerInterfaces, NewsServerInterfaces, Accounts,
    ActorTypes, ActorPool, Seasons, ClassStorageInt, CacheAgent, Variants;

  const
    tidRDOHook_World = 'World';

  const
    tidRegKey_ModelServer     = '\SOFTWARE\Oceanus\FIVE\ModelServer';
    tidRegKey_ModelExtensions = tidRegKey_ModelServer + '\Extensions';

  const
    tidFileName_WorldIniFile = 'world.ini';

  const
    tidIniSection_General     = 'General';
    tidIniSection_WorldTowns  = 'Towns';
    tidIniValue_Name          = 'Name';
    tidIniValue_xSize         = 'Width';
    tidIniValue_ySize         = 'Height';
    tidIniValue_TownCount     = 'Count';
    tidIniValue_TownName      = 'TownName';
    tidIniValue_TownCluster   = 'TownCluster';
    tidIniValue_TownX         = 'TownX';
    tidIniValue_TownY         = 'TownY';

  type
    TBackupMode = (bmdInGame, bmdShutdown);

  //const
    //BackupPeriod = 30;  // in minutes

  type
    TModelServer =
      class(TObject, IMailServer, INewsServer, IConfigHandler)
        public
          constructor Create;
          destructor  Destroy; override;
        public
          procedure InitWorld( aName : string; aNotEvent : TBackupReaderNotify; aBaseDir, aCacheHost, aSatellitePath : string; aDAPort, aCSPort, aCCPort : integer; aMailHost : string; aMailPort, aNewsPort : integer );
          procedure InitInterfaceServerEvents( ServerName : string; ServerPort : integer );
          procedure InitMailServer( ServerName : string; ServerPort : integer; WorldName : string );
          procedure InitNewsServer( ServerName : string; ServerPort : integer; WorldName : string );
          procedure InitDirServer( AreaName, ServerName : string; ServerPort : integer );
          procedure SetPurgeTyccons(count : integer);
        private
          procedure StartCacheServer( aCSPort : integer );
          procedure StartDirectAccessServer( aDAPort : integer );
        private
          fBaseDir      : string;
          fWorld        : TInhabitedWorld;
          fDAConn       : IRDOConnectionsServer;
          fDAServ       : TRDOServer;
          fDALockConn   : IRDOConnectionsServer;
          fDALockServ   : TRDOServer;
          fISConn       : IRDOConnectionInit;
          fISEvents     : OleVariant;
          fBPS          : integer;
          fCurDt        : TTimeDelta;
          fLocalHost    : string;
          fDAPort       : integer;
          fDALockPort   : integer;
          fForceBackup  : boolean;
          fBackupError  : boolean;
          fSaveCount    : integer;
          fSatCount     : integer;
          fCacheOptions : TCacheOptions;
          fSatRenderComplete : TEvent;
          fSatelliteMapPath  : string;
          fTycconsToPurge : integer;
          fCacheLogVer : integer;
        private
          function GetHoursADay : integer;
        public
          property TheWorld     : TInhabitedWorld read fWorld;
          property BPS          : integer         read fBPS; // Blocks per second
          property CurDt        : TTimeDelta      read fCurDt;
          property HoursADay    : integer         read GetHoursADay;
          property ForceBackup  : boolean         read fForceBackup  write fForceBackup;
          property CacheOptions : TCacheOptions   read fCacheOptions write fCacheOptions;
          property SatelliteMapPath : string      read fSatelliteMapPath write fSatelliteMapPath;
          property BackupError  : boolean         read fBackupError;
        private
          fExtensionList : TStrings;
        public
          property ExtensionList : TStrings read fExtensionList;
        private
          procedure LoadExtensions( Notify : TBackupReaderNotify );
          procedure LoadWorldExtensions( World : TWorld; Notify : TBackupReaderNotify; WorldLoaded : boolean );
        private
          fTimer             : TTimer;
          fSimTimer          : TTimer;
          fIntTimer          : TTimer;
          fCargoTimer        : TTimer;
          fSimEv             : TEvent;
          fBackupEv          : TEvent;
          fIntEv             : TEvent;
          fCargoEv           : TEvent;
          fSimThread         : TThread;
          fLastBackup        : integer;
          {$IFNDEF REMOVETRANSPORT}
          fCargoRenderThread : TThread;
          {$ENDIF}
          fIntegrationThread : TThread;
          fIntegrationLock   : TCriticalSection;
          fISProxyLock       : TCriticalSection;
          fDSProxyLock       : TCriticalSection;
        private
          procedure SetTimeRes( aVirtTimeRate, aSimRate, anIntRate : integer );
          procedure OnTimer( Sender : TObject );
          procedure OnSimTimer( Sender : TObject );
          procedure OnIntTimer( Sender : TObject );
          procedure OnCargoTimer( Sender : TObject );
        private
          procedure OnAreaChanged( x, y, dx, dy : integer );
          procedure OnFacilityChanged( Facility : TFacility; FacilityChange : TFacilityChange );
          procedure OnTycoonsChanged;
          procedure OnDateChanged( Date : TDateTime );
          procedure OnSeasonChanged( Season : TSeason );
          procedure OnEndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
          procedure OnTycoonRetired( name : string );
          //procedure OnPoolActed( PoolId : TActorPoolId; ViewerId : TViewerId; TickCount : cardinal; TickData : TStream );
          procedure OnSendNotification( TycoonId : integer; Kind : integer; Title, Body : string; Options : integer );
          procedure OnModelStatusChange( status : integer );
          procedure OnISDisconnect( const ClientConnection : IRDOConnection );
          procedure OnDSDisconnect( const ClientConnection : IRDOConnection );
          procedure OnMailDisconnect( const ClientConnection : IRDOConnection );
          procedure OnNewsDisconnect( const ClientConnection : IRDOConnection );
        private
          procedure StoreBackup( mode : TBackupMode; ErrorLevel : integer );
          procedure CreateSatelliteMaps;
        private
          fMailConn   : IRDOConnectionInit;
          fMailServer : OleVariant;
          fMailId     : integer;
          fNewsConn   : IRDOConnectionInit;
          fNewsServer : OleVariant;
          fDirConn    : IRDOConnectionInit;
          fDirServer  : OleVariant;
        // IUnknown
        private
          function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function _AddRef  : integer; stdcall;
          function _Release : integer; stdcall;
        // IMailServer
        private
          function NewMailAccount( Account, Alias, FwdAddr : string; KeepMsg : boolean ) : boolean;
          function DeleteAccount ( Account : string ) : boolean;
          function SetForwardRule( Account, FwdAddr : string; KeepMsg : boolean ) : boolean;
          function SendMessage( From, Dest, Subject : string; Lines : TStringList; html : boolean ) : boolean;
          function SendHTMLMessage( From, Dest, Subject, URL : string ) : boolean;
          procedure UpdateDate(NewDate : TDateTime);
        // INewsServer
        private
          procedure CreateNewsCenter  ( World : string );
          procedure CreateNewspaper   ( World, Name, Style, Town : string );
          procedure GenerateNewspapers( World : widestring; Date : TDateTime );
          function  RenewCache(Agent, ObjId : string) : TObjectCache;
        private
          // fBackupEvent : TEvent;
        private
          fMDXNodes : TCollection;
        public
          property MailConn : IRDOConnectionInit read fMailConn;
          property NewsConn : IRDOConnectionInit read fNewsConn;
          property DirConn  : IRDOConnectionInit read fDirConn;
        // IConfigHandler
        private
          function GetInteger( id : integer ) : integer;
          function GetFloat( id : integer ) : double;
          function GetString( id : integer ) : string;
          function GetMoney( id : integer ) : TMoney;
          function GetIntegerArray( id : integer; i, j, k, l : integer ) : integer;
          function GetFloatArray( id : integer; i, j, k, l : integer ) : double;
          function GetStringArray( id : integer; i, j, k, l : integer ) : string;
          function GetMoneyArray( id : integer; i, j, k, l : integer ) : TMoney;
          function GetConfigParm( strId, defVal : string ) : string;
        private
          fAccountExpires : integer;
          fLevelLimit     : integer;
          fConfigInfo     : TStringList;
          fIntegration    : boolean;
        public
          property Integration : boolean read fIntegration write fIntegration;
        private
          fAutoRestart  : boolean;
          fRestartDate  : TDateTime;
          fMaintDue     : boolean;
          fWarnMaint    : boolean;
          fTimeToMaint  : TDateTime;
          fLastDowntime : integer; // in minutes
          fLastDateUp   : TDateTime;
          fMSReadyEvent : TEvent;
        private
          function  GetMaxInvestors : integer;
          procedure SetMaxInvestors(value : integer);
        public
          property AutoRestart  : boolean   read fAutoRestart    write fAutoRestart;
          property RestartDate  : TDateTime read fRestartDate    write fRestartDate;
          property MaintDue     : boolean   read fMaintDue       write fMaintDue;
          property LastDateUp   : TDateTime read fLastDateUp     write fLastDateUp;
          property MaxInvestors : integer   read GetMaxInvestors write SetMaxInvestors;
        public
          procedure CheckMaintenanceSchedule;
          procedure RegisterLastGoodBackup(name : string);
          procedure SetMSReadyEvent(signal : boolean);
          function  ReportMaintenance(shortMsg, longMsg : string; eta : TDateTime) : boolean;
          procedure ReportMaintenanceToIS(eta : TDateTime);
          procedure CalculateDownTime;
      end;

  var
    BackupRate : integer = 45;
    WorldDt    : integer = 0;

  const
    TheModelServer : TModelServer = nil;

implementation

  uses
    SmartThreads, ClassStorage, SysUtils, Trade, ModelServerCache, ComObj, Logs,
    Registry, IniFiles, KernelCache, PoliticsCache, WinSockRDOConnectionsServer, HostNames,
    Construction, WinSockRDOConnection, RDOObjectProxy, Surfaces, Banks, BasicTaxes, Politics,
    BackupInterfaces, PopulatedBlock, WorkCenterBlock, CollectionBackup, BasicPolitics,
    PyramidalModifier, PublicFacility, StdFluids, MailProtocol, LargeMatrix, BasicCurriculum,
    Circuits, ResearchCenter, Headquarters, ConnectedBlock, SpontaneousBuildings, StdTaxes,
    OfficeBlock, BasicAccounts, Newspapers, Broadcast, StdBroadcast, Transport,
    StreamToStr, VCLBackup, TownPolitics, WorldPolitics, Ministers, TranscendBlock,
    EvaluatedBlock, Inventions, Rankings, StdRankings, Forms, ServiceBlock, Land,
    MetaInstances, CompStringsParser, TycoonLevels, JPEG, Graphics, Profiler, BasicEvents,
    Languages, SimMLS, Taxes, Plotter, FileCtrl, MathUtils, MatrixCircuits, MediaGates, LoggedUserData,
    Messages, MediaNameHistory, EconomyRelay, Tasks, Tutorial, TycoonVotes,
    Favorites, DelphiStreamUtils;

  const
    InitialWaitTime = 1*30*1000;

  function MinutesSince(ticks : cardinal) : integer;
    begin
      result := (Windows.GetTickCount - ticks) div (1000*60);
    end;

  function IntToThreadPriority(value : integer) : TThreadPriority;
    begin
      case value of
        0: result := tpIdle;
        1: result := tpLowest;
        2: result := tpLower;
        3: result := tpNormal;
        4: result := tpHigher;
        5: result := tpHighest;
        6: result := tpTimeCritical;
        else result := tpNormal;
      end;
    end;


  // TBackupThread

  type
    TBackupThread =
      class( TSmartThread )
        public
          constructor Create( aServer : TModelServer; aMode : TBackupMode );
        private
          fServer : TModelServer;
          fMode   : TBackupMode;
        protected
          procedure Execute; override;
      end;

    constructor TBackupThread.Create( aServer : TModelServer; aMode : TBackupMode );
      begin
        inherited Create( true );
        fServer  := aServer;
        fMode    := aMode;
        Priority := tpLowest;
        FreeOnTerminate := true;
        Resume;
      end;

    procedure TBackupThread.Execute;
      begin
        fServer.StoreBackup( fMode, 0 );
      end;


  // TSimTread

  type
    TSimThread =
      class( TSmartThread )
        private
          fServer : TModelServer;
        protected
          procedure Execute; override;
        private
          fThreadInfo : pointer;
        public
          procedure Dispatch(var Message); override;
      end;

    procedure TSimThread.Execute;
      var
        SimStart    : integer;
        OldPriority : TThreadPriority;
        lstBkTick   : integer;
        warnTick    : integer;
      begin
        lstBkTick := Windows.GetTickCount;
        warnTick  := 0;

        // This allows any secure operation inside the simulation thread.
        LoggedUserData.LogSystem;

        fServer.fWorld.ForceDt(2); // Worlds start with DT = 2

        while not Terminated do
          try
            if (fServer.fSimEv.WaitFor( fServer.fSimTimer.Interval ) <> wrTimeout) and not Terminated
              then
                begin
                  fServer.fSimEv.ResetEvent;
                  SimStart := DateTimeToTimeStamp(Time).time;
                  if WorldDt > 0
                    then fServer.fWorld.ForceDt(WorldDt);
                  fServer.fWorld.Simulate;

                  // Maitenance
                  fServer.CheckMaintenanceSchedule;
                  if fServer.fAutoRestart
                    then
                      begin
                        if fServer.fWarnMaint and (MinutesSince(warnTick) > 5)
                          then
                            begin
                              fServer.ReportMaintenanceToIS(fServer.fTimeToMaint);
                              warnTick := Windows.GetTickCount;
                            end;
                        if fServer.fMaintDue
                          then
                            begin
                              fServer.ReportMaintenance('Server down for maintenance', 'Server down for maintenance..', Now + EncodeTime(0, fServer.fLastDowntime + 5, 0, 0));
                              fServer.SetMSReadyEvent(false);
                              fServer.ReportMaintenanceToIS(0); // Good bye..
                              Terminate;
                            end;
                      end;

                  if not Terminated and (fServer.ForceBackup or fServer.TheWorld.ForceBackup or (MinutesSince(lstBkTick) > BackupRate))       //(SimCount mod (BackupRate*40*1000 div integer(fServer.fSimTimer.Interval)) = 0)
                    then
                      begin
                        // Wait for file transfer operation uff!!
                        fServer.fIntegrationLock.Enter;
                        try
                          // Create the thread
                          {
                          TBackupThread.Create( fServer, bmdInGame );
                          fServer.fBackupEv.WaitFor( INFINITE );
                          }
                          ModelServerCache.InformBackup(true);
                          fServer.OnModelStatusChange( mstBusy );
                          fServer.TheWorld.WorldLock.Enter;
                          try
                            OldPriority := Priority;
                            Priority    := tpHigher;
                            fServer.StoreBackup( bmdInGame, 0 );
                            Priority := OldPriority;
                          finally
                            fServer.TheWorld.WorldLock.Leave;
                            ModelServerCache.InformBackup(false);
                            if fServer.fBackupError
                              then fServer.OnModelStatusChange( mstError )
                              else fServer.OnModelStatusChange( mstNotBusy );
                          end;
                        finally
                          fServer.fIntegrationLock.Leave;
                          fServer.ForceBackup := false;
                          fServer.TheWorld.ForceBackup := false;
                          lstBkTick := Windows.GetTickCount;
                        end;
                      end;
                  if not Terminated and (DateTimeToTimeStamp(Time).time > SimStart)
                    then
                      begin
                        fServer.fBPS   := 1000*fServer.fWorld.Facilities.Count div (DateTimeToTimeStamp(Time).time - SimStart);
                        fServer.fCurDt := ITimer(fServer.fWorld).dt;
                      end;
                end;
          except
          end;
      end;

  procedure TSimThread.Dispatch(var Message);
    var
      msg  : TMessage absolute Message;
    begin
      case msg.msg of
        MSG_GETTHREADDATA :
          begin
            msg.Result := integer(fThreadInfo);
          end;
        MSG_SETTHREADDATA :
          begin
            fThreadInfo := pointer(msg.LParam);
          end;
      else
        inherited Dispatch(Message);
      end;
    end;


  {$IFNDEF REMOVETRANSPORT}
  // TCargoRenderThread
  type
    TCargoRenderThread =
      class( TSmartThread )
        public
          constructor Create( aServer : TModelServer );
        private
          fServer : TModelServer;
        protected
          procedure Execute; override;
      end;

    constructor TCargoRenderThread.Create( aServer : TModelServer );
      begin
        inherited Create( true );
        fServer := aServer;
      end;

    procedure TCargoRenderThread.Execute;
      begin
        while not Terminated do
          if fServer.fCargoEv.WaitFor( fServer.fCargoTimer.Interval ) <> wrTimeout
            then
              begin
                Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' <TRANSPORT' );
                fServer.fCargoEv.ResetEvent;
                try
                  fServer.fWorld.CargoSystem.Render( rqHigh );
                except
                  on E : Exception do
                    Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' ERROR in Cargo Surface: ' + E.Message );
                end;
                Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' TRANSPORT>' );
              end;
      end;
  {$ENDIF}

  // TIntegrationThread

  type
    TIntegrationThread =
      class( TSmartThread )
        public
          constructor Create( aServer : TModelServer );
        private
          fServer          : TModelServer;
          fIntegrationPool : TIntegratorPool;
          fSurfacePool     : TSurfacePool;
        protected
          procedure Execute; override;
      end;

    constructor TIntegrationThread.Create( aServer : TModelServer );

      procedure SetupSurfaceMaps;
        var
          i : integer;
        begin
          for i := 0 to pred(fSurfacePool.Surfaces.Count) do
            TSurface(fSurfacePool.Surfaces[i]).SetSize( fServer.fWorld.xSize, fServer.fWorld.ySize );
        end;

      begin
        inherited Create( true );
        fServer := aServer;
        fIntegrationPool := TIntegratorPool(TheClassStorage.ClassById[tidClassFamily_SurfacePools, tidSurfacePool_Integrators]);
        fSurfacePool     := TSurfacePool(TheClassStorage.ClassById[tidClassFamily_SurfacePools, tidSurfacePool_Surfaces]);
        Priority         := tpIdle;
        SetupSurfaceMaps;
      end;

    procedure TIntegrationThread.Execute;
      var
        waitTime : integer;
        firstInt : boolean;
      begin
        waitTime := fServer.fIntTimer.Interval;
        firstInt := true;
        fServer.fIntTimer.Interval := InitialWaitTime;
        Priority := tpHigher;      
        while not Terminated do
          try
            if (fServer.fIntEv.WaitFor( waitTime ) <> wrTimeout) and not Terminated
              then
                begin
                  if fServer.Integration
                    then
                      begin
                        Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' <INTEGRATION' );
                        fServer.fIntegrationLock.Enter;
                        try
                          fServer.fIntEv.ResetEvent;
                          Profiler.ProcStarted( prfKind_Int, prfId_UpdateModifiers );
                          fSurfacePool.Update;
                          Profiler.ProcEnded( prfKind_Int, prfId_UpdateModifiers );
                          Profiler.ProcStarted( prfKind_Int, prfId_Integrate );
                          fIntegrationPool.IntegrateAll;
                          Profiler.ProcEnded( prfKind_Int, prfId_Integrate );
                        finally
                          fServer.fIntegrationLock.Leave;
                        end;
                        Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' INTEGRATION>' );
                      end;
                  fServer.fIntEv.ResetEvent;
                  if firstInt
                    then
                      begin
                        firstInt := false;
                        Priority := IntToThreadPriority(StrToInt(fServer.GetConfigParm('IntSpeed', '0')));
                        fServer.SetMSReadyEvent(true);
                      end;
                end;
            fServer.fIntTimer.Interval := waitTime;
          except
          end;
      end;


  // TModelServer

  constructor TModelServer.Create;

    procedure InitConfig;
      var
        filename : string;
      begin                                       
        try
          filename := ExtractFilePath(paramstr(0)) + 'worldconfig.ini';
          fConfigInfo := TStringList.Create;
          if FileExists( filename )
            then fConfigInfo.LoadFromFile( filename );
          if fConfigInfo.Values['AccountExpires'] <> ''
            then fAccountExpires := StrToInt( fConfigInfo.Values['AccountExpires'] )
            else fAccountExpires := -1;
          if fConfigInfo.Values['LevelLimit'] <> ''
            then fLevelLimit := StrToInt( fConfigInfo.Values['LevelLimit'] )
            else fLevelLimit := high(fLevelLimit);
        except
        end;
        SetGlobalConfigHandler( self );
      end;

    begin
      inherited Create;
      //InitLogs;
      fExtensionList := TStringList.Create;
      // >> fBackupEvent := TEvent.Create(nil, true, true, '');
      InitConfig;
      fIntegration  := true;
      fMSReadyEvent := TEvent.Create(nil, true, false, 'ModelServerReady');
    end;

  destructor TModelServer.Destroy;

    procedure UpdateModelExtensionList;
      var
        Reg : TRegistry;
        i   : integer;
      begin
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          Reg.DeleteKey( tidRegKey_ModelExtensions );
          if Reg.OpenKey( tidRegKey_ModelExtensions, true )
            then
              for i := 0 to pred(fExtensionList.Count) do
                Reg.WriteString( ExtractFileName(fExtensionList[i]), fExtensionList[i] )
            else raise Exception.Create( 'Cannot open Key ' + tidRegKey_ModelExtensions );
        finally
          Reg.Free;
        end;
      end;

    begin
      try
        {$IFNDEF REMOVETRANSPORT}
        // Stop cargo rendering thread
        if fCargoRenderThread <> nil
          then
            begin
              fCargoRenderThread.Terminate;
              fCargoEv.SetEvent;
              WaitForSingleObject( fCargoRenderThread.Handle, 2*fCargoTimer.Interval );
              fCargoRenderThread.Free;
            end;
        {$ENDIF}    
        // Stop integration thread
        if fIntegrationThread <> nil
          then
            begin
              fIntegrationThread.Terminate;
              fIntEv.SetEvent;
              WaitForSingleObject( fIntegrationThread.Handle, 60000 );
              fIntegrationThread.Free;
            end;
        // Stop simulation thread
        if fSimThread <> nil
          then
            begin
              fSimThread.Terminate;
              fSimEv.SetEvent;
              WaitForSingleObject( fSimThread.Handle, 60000 );
            end;
        // Free timers and events
        fTimer.Free;
        fSimTimer.Free;
        fIntTimer.Free;
        fCargoTimer.Free;
        fSimEv.Free;
        fBackupEv.Free;
        fIntEv.Free;
        // Free world
        if (fWorld <> nil) and not fBackupError
          then
            begin
              try
                ModelServerCache.InformBackup(true);
                try
                  StoreBackup( bmdShutdown, 0 );
                finally
                  ModelServerCache.InformBackup(false);
                end;
              except
                MessageBox( 0, 'Could not save world!', 'Backup Error', MB_ICONWARNING or MB_OK );
              end;
              //UpdateModelExtensionList;
              //fWorld.Free;
            end;
        fIntegrationLock.Free;
        fISProxyLock.Free;
        fDSProxyLock.Free;
        fSatRenderComplete.Free;
        ClassStorage.DoneTheClassStorage;
        ModelServerCache.DoneModelServerCache;
        fExtensionList.Free;
        Kernel.DoneVisualClasses;
        //DoneLogs;
        SetGlobalConfigHandler(nil);
        inherited;
      except
      end;
    end;

  procedure TModelServer.InitWorld( aName : string; aNotEvent : TBackupReaderNotify; aBaseDir, aCacheHost, aSatellitePath : string; aDAPort, aCSPort, aCCPort : integer; aMailHost : string; aMailPort, aNewsPort : integer );

    procedure CreateWorldFromIniFile;
      var
        IniFile : TIniFile;
        Name    : string;
        xSize   : integer;
        ySize   : integer;
      begin
        IniFile := TIniFile.Create( fBaseDir + aName + '.ini'  );
        try
          Name   := IniFile.ReadString( tidIniSection_General, tidIniValue_Name, '' );
          xSize  := IniFile.ReadInteger( tidIniSection_General, tidIniValue_xSize, 0 );
          ySize  := IniFile.ReadInteger( tidIniSection_General, tidIniValue_ySize, 0 );
          if (Name <> '') and (xSize > 0) and (ySize > 0)
            then fWorld := TPoliticalWorld.Create( Name, xSize, ySize )
            else raise Exception.Create( 'Invalid World INI file!' );
        finally
          IniFile.Free;
        end;
      end;

    procedure InitWorldFromIniFile( NotEvent : TBackupReaderNotify );
      var
        MaxLen : integer;

      procedure CreateRoads( x, y, Length, cellSize : integer; hor : boolean );

        procedure CreateRoadSeg( x, y, Length, cellSize, dx, dy : integer );
          var
            ix, iy    : integer;
            finalX    : integer;
            finalY    : integer;
            roads     : TCircuitMap;
            ErrorCode : TErrorCode;
            count     : integer;
          begin
            finalX := x + dx*Length*cellSize;
            finalY := y + dy*Length*cellSize;
            while finalX < 0 do
              inc( finalX, cellSize );
            while finalY < 0 do
              inc( finalY, cellSize );
            while finalX >= fWorld.xSize do
              dec( finalX, cellSize );
            while finalY >= fWorld.ySize do
              dec( finalY, cellSize );
            while (Length > 0) and (LandClassOf( fWorld.GetGroundMap( finalX, finalY ) ) = lncZoneD) do
              begin
                inc( finalX, -cellSize*dx );
                inc( finalY, -cellSize*dy );
                dec( Length );
              end;
            if (Length > 0)
              then
                begin
                  roads := fWorld.CircuitById[cirRoads];
                  roads.CreateSegment( x, y, finalX, finalY, 1, ErrorCode );
                  if ErrorCode = NOERROR
                    then
                      begin
                        ix := x;
                        iy := y;
                        if dy = 0
                          then count := abs(finalX - x) div cellSize
                          else count := abs(finalY - y) div cellSize;
                        while (count > 0) do
                          begin
                            inc( ix, cellSize*dx );
                            inc( iy, cellSize*dy );
                            if dy = 0
                              then CreateRoads( ix, iy, abs(ix - x) div cellSize, cellSize, false )
                              else CreateRoads( ix, iy, abs(iy - y) div cellSize, cellSize, true );
                            dec( count );
                          end;
                      end;
                end;
          end;

        begin
          if (Length > 0) and (random(MaxLen - Length) <= (MaxLen - Length) div 2)
            then
              begin
                if hor
                  then
                    begin
                      CreateRoadSeg( x, y, Length div 2, cellSize, -1, 0 );
                      CreateRoadSeg( x, y, Length div 2, cellSize, 1, 0 );
                    end
                  else
                    begin
                      CreateRoadSeg( x, y, Length div 2, cellSize, 0, -1 );
                      CreateRoadSeg( x, y, Length div 2, cellSize, 0, 1 );
                    end
              end;
          fWorld.InvalidateRoads;
        end;

      procedure InitTowns( IniFile : TIniFile );
        var
          count   : integer;
          i       : integer;
          x, y    : integer;
          Name    : string;
          Cluster : string;
          Town    : TTown;
        begin
          count := IniFile.ReadInteger( tidIniSection_WorldTowns, tidIniValue_TownCount, 0 );
          MaxLen := min( 30, max( 15, 30 - 2*count ) );
          for i := 0 to pred(count) do
            begin
              Name    := IniFile.ReadString( tidIniSection_WorldTowns, tidIniValue_TownName + IntToStr(i), '' );
              Cluster := IniFile.ReadString( tidIniSection_WorldTowns, tidIniValue_TownCluster + IntToStr(i), '' );
              x       := IniFile.ReadInteger( tidIniSection_WorldTowns, tidIniValue_TownX + IntToStr(i), -1 );
              y       := IniFile.ReadInteger( tidIniSection_WorldTowns, tidIniValue_TownY + IntToStr(i), -1 );
              if (Name <> '') and (Cluster <> '') and (x >= 0) and (y >= 0)
                then
                  begin
                    NotEvent( 'Initializing town: ' + Name + '...', 100*(i + 1) div count );
                    Town := TPoliticalTown.Create( Name, Cluster, Cluster + 'TownHall', Cluster + 'TradeCenter', x, y, fWorld );
                    fWorld.InsertTown( Town );
                    CacheObject( Town, noKind, noInfo );
                    CreateRoads( x - 1, y - 1, MaxLen, 7, true );
                    CreateRoads( x - 1, y - 1, MaxLen, 7, false );
                  end
                else raise Exception.Create( 'Invalid Town INI info (' + IntToStr(i) + ')' );
            end;
          // fWorld.InitTownMap;
        end;

      var
        IniFile : TIniFile;
      begin
        NotEvent( 'Opening INI file...', 0 );
        IniFile := TIniFile.Create( fBaseDir + aName + '.ini'  );
        try
          fWorld.Loaded( NotEvent );
          NotEvent( 'Initializing clusters...', 0 );
          fWorld.InitClusters;
          NotEvent( 'Initializing towns...', 0 );
          InitTowns( IniFile );
          fWorld.TycoonsToPurge := fTycconsToPurge;
        finally
          IniFile.Free;
        end;
      end;

    procedure StoreMetaTexts;

      procedure StoreInvClientInfo( LangId : TLanguageId );
        var
          i, count : integer;
          Inv      : TInvention;
          Stream   : TFileStream;
          Cats     : TStringList;
          resp     : string;
          idx      : integer;
          j        : integer;
        begin
          Stream := TFileStream.Create( 's:\temp\language\research.' + LangId + '.dat', fmCreate );
          Cats   := TStringList.Create;
          try
            count := TheClassStorage.ClassCount[tidClassFamily_Inventions];
            Stream.Write( count, sizeof(count) );
            for i := 0 to pred(count) do
              begin
                Inv := TInvention(TheClassStorage.ClassByIdx[tidClassFamily_Inventions, i]);
                Inv.StoreClientInfo( Stream, LangId );
                resp := UpperCase(Inv.Resp_MLS.Values[LangId]);
                idx  := Cats.IndexOf(resp);
                if (idx = -1) and (Inv.Id <> '')
                  then
                    begin
                      j := 0;
                      while (j < Cats.Count) and (TInvention(Cats.Objects[j]).CatPos < Inv.CatPos) do
                        inc(j);
                      if j < Cats.Count
                        then Cats.InsertObject(j, resp, Inv)
                        else Cats.AddObject(resp, Inv);
                    end;
              end;
            DelphiStreamUtils.WriteString(Stream, Cats.Text);
          finally
            Stream.Free;
          end
        end;

      var
        SimDict : TDictionary;
        i       : integer;
      begin
        SimDict := Languages.CreateDictionary;
        try
          SimDict.Store( 'S:\Temp\Language\sim.lang' );
        finally
          SimDict.Free;
        end;
        //MetaInstances.StoreTexts( tidClassFamily_Facilities, 'S:\Temp\Language\metafacilities.lang' );
        //MetaInstances.StoreTexts( tidClassFamily_Fluids, 's:\temp\language\metafluids.lang' );
        //MetaInstances.StoreTexts( tidClassFamily_Ratings, 's:\temp\language\metaratings.lang' );
        //MetaInstances.StoreTexts( tidClassFamily_Projects, 's:\temp\language\metaprojects.lang' );
        //MetaInstances.StoreTexts( tidClassFamily_PublicFacilities, 's:\temp\language\metapublic.lang' );
        //MetaInstances.StoreTexts( tidClassFamily_Accounts, 's:\temp\language\metaaccounts.lang' );
        //MetaInstances.StoreTexts( tidClassFamily_Services, 's:\temp\language\metacommerce.lang' );
        //MetaInstances.StoreTexts( tidClassFamily_Ministries, 's:\temp\language\metaministries.lang' );
        //MetaInstances.StoreTexts( tidClassFamily_FacilityKinds, 's:\temp\language\metakinds.lang' );
        //MetaInstances.StoreTexts( tidClassFamily_TycoonLevels, 's:\temp\language\metalevels.lang' );
        //MetaInstances.StoreTexts( tidClassFamily_Inventions, 's:\temp\language\metaresearch.lang' );
        for i := 0 to pred(LangList.Count) do
          begin
            StoreInvClientInfo(LangList[i]);
            //MetaInstances.StoreMissingTexts( tidClassFamily_Inventions, 'S:\Temp\Language\metaresearch.missing.' + LangList[i] + '.lang', LangList[i] );
          end;
      end;

    procedure LoadMetaTexts;

      procedure LoadLanguage( path : string; LangId : TLanguageId );
        var
          Rec   : TSearchRec;
          found : integer;
        begin
          path  := path + 'ms\' +LangId + '\';
          found := FindFirst( path + '*.lang', faAnyFile, Rec );
          try
            while found = 0 do
              begin
                MetaInstances.RetrieveTexts( path + Rec.Name );
                found := FindNext( Rec );
              end;
          finally
            FindClose( Rec );
          end;
        end;

      var
        i : integer;
      begin
        for i := 0 to pred(LangList.Count) do
          LoadLanguage( ExtractFilePath(paramstr(0)) + 'Languages\', LangList[i] );
      end;

    function GetBackupFilePath : string;
      var
        Reg : TRegistry;
        str : string;
      begin
        if ParamStr(1) <> ''                           
          then
            begin
              Reg := TRegistry.Create;
              try
                Reg.RootKey := HKEY_LOCAL_MACHINE;
                if Reg.OpenKey(tidRegKey_ModelServer, false)
                  then
                    begin
                      str := Reg.ReadString('LastGoodBackup');
                      if str <> ''
                        then result := fBaseDir + str
                        else result := fBaseDir + aName + '.world'
                    end
                  else
                    raise Exception.Create( 'Cannot open Key ' + tidRegKey_ModelServer);
              finally
                Reg.Free;
              end;
            end
          else result := fBaseDir + aName + '.world'
      end;

    var
      Reader    : IBackupReader;
      WorldURL  : string;
      UseBackup : boolean;
      LogFile   : string;
      bckFile   : string;

    begin
      Log( 'Survival', TimeToStr( Now ) + ' WORLD LOADED' );

      RandSeed    := 100;
      fDAPort     := aDAPort;
      fDALockPort := aDAPort + 1;
      fLocalHost  := GetLocalAddress;

      fSatelliteMapPath := aSatellitePath + '\' + aName + '\';

      // Create the world
      if aBaseDir[length(aBaseDir)] <> '\'
        then fBaseDir := aBaseDir + '\'
        else fBaseDir := aBaseDir;

      Log( 'Survival', TimeToStr( Now ) + ' Registering at Cache Server...' );
      if not ModelServerCache.RegisterWorld( aName, aCacheHost, aCSPort, aCCPort, WorldURL )
        then raise Exception.Create( 'Cannot register world in cache server side' );
      Log( 'Survival', TimeToStr( Now ) + ' Registering at Cache Server Sucessfull.' );

      ModelServerCache.InitModelServerCache;
      ClassStorage.InitTheClassStorage;
      Kernel.InitVisualClasses;

      aNotEvent( 'Readying multilanguage support...', 0 );
      Log( 'Survival', TimeToStr( Now ) + ' Reading language files...' );
      SimMLS.LoadMLS;
      Log( 'Survival', TimeToStr( Now ) + ' Reading language files done.' );

      aNotEvent( 'Registering Basic Metainstances...', 0 );
      Log( 'Survival', TimeToStr( Now ) + ' Registering Basic Metainstances...' );
      BasicAccounts.RegisterAccounts;
      Kernel.RegisterSurfaces;
      Population.RegisterSurfaces;
      StdBroadcast.RegisterSurfaces;
      KernelCache.RegisterCachers;
      PoliticsCache.RegisterCachers;
      Rankings.RegisterCachers;
      Newspapers.RegisterCachers;
      Population.RegisterMetaFluids;
      Population.RegisterMetaInstances;
      StdFluids.RegisterMetaFluids;
      TycoonLevels.RegisterLevels;

      VCLBackup.RegisterBackup;
      Languages.RegisterBackup;
      CollectionBackup.RegisterBackup;
      Kernel.RegisterBackup;
      Accounts.RegisterBackup;
      BasicTaxes.RegisterBackup;
      BasicCurriculum.RegisterBackup;
      World.RegisterBackup;
      ConnectedBlock.RegisterBackup;
      Population.RegisterBackup;
      Politics.RegisterBackup;
      BasicPolitics.RegisterBackup;
      TownPolitics.RegisterBackup;
      WorldPolitics.RegisterBackup;
      Ministers.RegisterBackup;
      OfficeBlock.RegisterBackup;
      ResearchCenter.RegisterBackup;
      Headquarters.RegisterBackup;
      Broadcast.RegisterBackup;
      Trade.RegisterBackup;
      Construction.RegisterBackup;
      PopulatedBlock.RegisterBackup;
      WorkCenterBlock.RegisterBackup;
      Surfaces.RegisterBackup;
      Circuits.RegisterBackup;
      PyramidalModifier.RegisterBackup;
      PublicFacility.RegisterBackup;
      SpontaneousBuildings.RegisterBackup;
      Banks.RegisterBackup;
      LargeMatrix.RegisterBackup;                          
      Events.RegisterBackup;
      BasicEvents.RegisterBackup;
      MatrixCircuits.RegisterBackup;
      TranscendBlock.RegisterBackup;
      MediaGates.RegisterBackup;
      MediaNameHistory.RegisterBackup;
      EconomyRelay.RegisterBackup;
      TycoonVotes.RegisterBackup;
      Favorites.RegisterBackup;

      // Register Invention Classes
      EvaluatedBlock.RegisterInventionClass;
      PopulatedBlock.RegisterInventionClass;
      Broadcast.RegisterInventionClass;
      //ServiceBlock.RegisterInventionClass;

      fDSProxyLock  := TCriticalSection.Create;

      WorldPolitics.RegisterCapitol;

      Log( 'Survival', TimeToStr( Now ) + ' Loading extensions...' );
      LoadExtensions( aNotEvent );
      Log( 'Survival', TimeToStr( Now ) + ' Loading extensions complete.' );

      Kernel.RegisterTownParameters;
      Population.RegisterTownParameters;
      PopulatedBlock.RegisterTownParameters;
      BasicPolitics.RegisterPolitics;
      WorldPolitics.RegisterPolitics;
      Broadcast.RegisterTownParameters;

      // >> REM
      Tutorial.RegisterTasks;
      Tasks.RegisterBackup;
      Tutorial.RegisterBackup;

      ModelServerCache.EnabledLogs := false;

      // Register Taxes
      Log( 'Survival', TimeToStr( Now ) + ' Registering taxes...' );
      StdTaxes.RegisterTaxes;
      Log( 'Survival', TimeToStr( Now ) + ' Registering taxes complete.' );

      Log( 'Survival', TimeToStr( Now ) + ' Loading meta texts...' );
      aNotEvent( 'Loading texts...', 0 );
      LoadMetaTexts;
      Log( 'Survival', TimeToStr( Now ) + ' Loading meta texts complete.' );

      // Evaluate texts
      Log( 'Survival', TimeToStr( Now ) + 'Evaluating texts...' );
      EvaluateTexts( tidClassFamily_Fluids );
      EvaluateTexts( tidClassFamily_FacilityKinds );
      EvaluateTexts( tidClassFamily_Blocks );
      EvaluateTexts( tidClassFamily_Services );
      EvaluateTexts( tidClassFamily_FacilityKinds );
      EvaluateTexts( tidClassFamily_Facilities );
      EvaluateTexts( tidClassFamily_Taxes );
      Log( 'Survival', TimeToStr( Now ) + 'Evaluating texts complete.' );

      // Post register politics                                                
      WorldPolitics.PostRegisterPolitics;

      // Register Taxes
      Log( 'Survival', TimeToStr( Now ) + ' Registering taxes...' );
      StdTaxes.RegisterTaxesToAccounts;
      Log( 'Survival', TimeToStr( Now ) + ' Registering taxes complete.' );

      EvaluateTexts( tidClassFamily_Accounts );
      StdRankings.RegisterRankings;

      // Cache meta-facilities
      Log( 'Survival', TimeToStr( Now ) + 'Caching metainstances...' );
      CloneFamily( tidClassFamily_FacilityKinds );
      CacheFamily( tidClassFamily_FacilityKinds );

      CloneFamily( tidClassFamily_Facilities );
      CacheFamily( tidClassFamily_Facilities );

      CloneFamily( tidClassFamily_Services );
      CacheFamily( tidClassFamily_Services );
      Log( 'Survival', TimeToStr( Now ) + 'Caching metainstances complete.' );

      bckFile := GetBackupFilePath;

      UseBackup := FileExists(bckFile{fBaseDir + aName + '.world'});
      if UseBackup
        then
          begin
            aNotEvent( 'Preparing to read world...', 0 );
            Reader := OpenBackup(bckFile{fBaseDir + aName + '.world'},  aNotEvent);
            Log( 'Survival', TimeToStr( Now ) + 'Reading world...' );
            Reader.ReadObject( 'World', fWorld, nil );
            fCacheLogVer := Reader.ReadInteger('CacheLogVer', 0);
            Log( 'Survival', TimeToStr( Now ) + 'Reading world complete.' );
            Reader := nil;
            fWorld.DirProxyLock := fDSProxyLock;
          end
        else CreateWorldFromIniFile;

      if fCacheLogVer <= 0
        then fCacheLogVer := MSVersion;

      LogFile := fBaseDir + aName + '.cache';
      try
        ModelServerCache.SetLogPath(LogFile);
        ModelServerCache.WarpLog(fCacheLogVer - 1);
      finally
        DeleteFile(LogFile);
      end;

      ModelServerCache.SetLogVer(fCacheLogVer);

      Log( 'Survival', TimeToStr( Now ) + ' Loading map info...' );
      fWorld.LoadLandInfo( fBaseDir + aName + '.bmp' );
      Log( 'Survival', TimeToStr( Now ) + ' Loading map info OK.' );
      Log( 'Survival', TimeToStr( Now ) + ' Loading town map...' );
      //fWorld.InitTownMap( fBaseDir + aName + '.towns.bmp' );
      Log( 'Survival', TimeToStr( Now ) + ' Loading town map OK.' );

      // Set Cache options
      fWorld.CacheOptions := CacheOptions;

      // Set World base URL
      fWorld.WorldURL := WorldURL;

      // Start RDO servers
      StartDirectAccessServer( aDAPort );
      StartCacheServer( aCSPort );

      // Start Mail Server
      InitMailServer( aMailHost, aMailPort, aName );

      // Start News Server
      InitNewsServer( aMailHost, aNewsPort, aName );

      // Create new world after Mail and RDO servers are running
      if not UseBackup
        then
          begin
            InitWorldFromIniFile( aNotEvent );
            Log( 'Survival', TimeToStr( Now ) + 'Initializing Town Map...' );
            fWorld.InitTownMap( fBaseDir + aName + '.towns.bmp' );
            LoadWorldExtensions( fWorld, aNotEvent, false );
          end
        else
          begin
            Log( 'Survival', TimeToStr( Now ) + 'Initializing Town Map...' );
            fWorld.InitTownMap( fBaseDir + aName + '.towns.bmp' );
            Log( 'Survival', TimeToStr( Now ) + 'Initializing model objects...' );
            fWorld.Loaded( aNotEvent );
            Log( 'Survival', TimeToStr( Now ) + 'Initializing model objects complete.' );
            LoadWorldExtensions( fWorld, aNotEvent, true );
          end;

      CacheObject( fWorld, noKind, noInfo );
      ModelServerCache.EnabledLogs := true;

      // Init Simulation clockwork
      fTimer      := TTimer.Create( nil );
      fSimTimer   := TTimer.Create( nil );
      fIntTimer   := TTimer.Create( nil );
      fCargoTimer := TTimer.Create( nil );
      fSimEv      := TEvent.Create( nil, true, false, '' );
      fBackupEv   := TEvent.Create( nil, true, false, '' );
      fIntEv      := TEvent.Create( nil, true, false, '' );
      fCargoEv    := TEvent.Create( nil, true, false, '' );
      fSatRenderComplete := TEvent.Create( nil, true, false, '' );
      fSimThread  := TSimThread.Create( true );
      //fSimThread.Priority := tpLower; //tpHigher;
      TSimThread(fSimThread).fServer := self;
      fTimer.OnTimer := OnTimer;
      fSimTimer.OnTimer := OnSimTimer;
      fIntTimer.OnTimer := OnIntTimer;
      fCargoTimer.OnTimer := OnCargoTimer;
      fCargoTimer.Interval := 12*180000;
      SetTimeRes(2*2400, 2*2400, 5*60*1000); // SetTimeRes( 2*2400, 2*2400, 4*120000 );
      fSimThread.Priority := IntToThreadPriority(StrToInt(GetConfigParm('SimSpeed','3'))); // SimSpeed;
      fSimThread.Resume;

      // Init Integration thread
      fIntegrationThread := TIntegrationThread.Create( self );
      fIntegrationLock   := TCriticalSection.Create;
      fISProxyLock       := TCriticalSection.Create;
      fDSProxyLock       := TCriticalSection.Create;
      fIntegrationThread.Priority := IntToThreadPriority(StrToInt(GetConfigParm('IntSpeed', '0'))); //tpIdle;
      fIntegrationThread.Resume;

      {$IFNDEF REMOVETRANSPORT}
      // Init cargo render thread
      fCargoRenderThread := TCargoRenderThread.Create( self );
      fCargoRenderThread.Priority := IntToThreadPriority(StrToInt(GetConfigParm('TransSpeed', '0'))); //tpLowest
      fCargoRenderThread.Resume;
      {$ENDIF}

      // Update land surfaces
      Kernel.UpdateLandSurfaces( fWorld );
      aNotEvent( 'Done.', 0 );

      // Assign RegisterIS
      fWorld.OnRegisterIS := InitInterfaceServerEvents;

      // Storing texts
      //Log( 'Survival', TimeToStr( Now ) + ' Storing texts...' );
      //StoreMetaTexts;
      //Log( 'Survival', TimeToStr( Now ) + ' Storing texts complete.' );

      // Satellite
      Log( 'Survival', TimeToStr( Now ) + ' Creating satellite map...' );
      CreateSatelliteMaps;
      Log( 'Survival', TimeToStr( Now ) + ' Creating satellite map complete.' );

      // Servers Start Listening
      fDAConn.StartListening;
      fDALockConn.StartListening;

      // Downtime
      CalculateDownTime;
    end;

  procedure TModelServer.StartCacheServer( aCSPort : integer );
    var
      ServerConn : TWinSockRDOConnectionsServer;
      CacheSpeed : TThreadPriority;
    begin
      CacheSpeed := IntToThreadPriority(StrToInt(GetConfigParm('CacheSpeed','3')));
      ServerConn := TWinSockRDOConnectionsServer.Create(aCSPort, CacheSpeed);
      try
        ModelServerCache.CreateCacheServer( ServerConn, 3{5}, fWorld.WorldLock );
        ModelServerCache.MSCacher.OnRenewCache := RenewCache;
      except
        ServerConn.Free;
        raise;
      end;
    end;

  procedure TModelServer.StartDirectAccessServer( aDAPort : integer );
    var
      DASpeed : TThreadPriority;
    begin
      DASpeed     := IntToThreadPriority(StrToInt(GetConfigParm('DASpeed','3')));
      fDAConn     := TWinSockRDOConnectionsServer.Create(aDAPort, DASpeed); // >> 100
      fDAServ     := TRDOServer.Create(fDAConn as IRDOServerConnection, 8{14}, nil);
      fDALockConn := TWinSockRDOConnectionsServer.Create(aDAPort + 1, DASpeed); // >> 20
      fDALockServ := TRDOServer.Create(fDALockConn as IRDOServerConnection, 1, nil);
      fDAServ.RegisterObject(tidRDOHook_World, integer(fWorld));
      //fDAConn.StartListening;
      fDALockServ.RegisterObject(tidRDOHook_World, integer(fWorld));
      //fDALockConn.StartListening;
      fDALockServ.SetCriticalSection(fWorld.WorldLock);
    end;

  procedure TModelServer.InitInterfaceServerEvents( ServerName : string; ServerPort : integer );
    begin
      fISConn        := TWinSockRDOConnection.Create( 'ISCnx' );
      fISConn.Server := ServerName;
      fISConn.Port   := ServerPort;
      if fISConn.Connect( 10000 )
        then
          begin
            fISEvents := TRDOObjectProxy.Create as IDispatch;
            fISEvents.SetConnection( fISConn );
            if fISEvents.BindTo(tidRDOHook_InterfaceEvents)
              then
                begin
                  //fISEvents.WaitForAnswer    := false;
                  fWorld.OnAreaChanged       := OnAreaChanged;
                  fWorld.OnFacilityChanged   := OnFacilityChanged;
                  fWorld.OnDateChanged       := OnDateChanged;
                  fWorld.OnSeasonChanged     := OnSeasonChanged;
                  fWorld.OnTycoonsChanged    := OnTycoonsChanged;
                  fWorld.OnEndOfPeriod       := OnEndOfPeriod;
                  fWorld.OnTycoonRetired     := OnTycoonRetired;
                  //fWorld.OnPoolActed         := OnPoolActed;
                  fWorld.OnSendNotification  := OnSendNotification;
                  fWorld.OnModelStatusChange := OnModelStatusChange;
                  (fISConn as IRDOConnection).OnDisconnect := OnISDisconnect;
                end
              else raise Exception.Create( '' );
          end
        else raise Exception.Create( '' );
    end;

  procedure TModelServer.InitMailServer( ServerName : string; ServerPort : integer; WorldName : string );
    var
      ClientId : integer;
    begin
      fMailConn        := TWinSockRDOConnection.Create( 'MailCnx' );
      fMailConn.Server := ServerName;
      fMailConn.Port   := ServerPort;
      fMailServer      := TRDOObjectProxy.Create as IDispatch;
      if fMailConn.Connect(10000)
        then
          begin
            fMailServer.TimeOut := 20000;
            (fMailConn as IRDOConnection).OnDisconnect := OnMailDisconnect;
            fMailServer.SetConnection(fMailConn);
            if fMailServer.BindTo(tidRDOHook_MailServer)
              then
                begin
                  fMailId := fMailServer.RegisterWorld(WorldName);
                  if fMailId <> 0
                    then
                      begin
                        fMailServer.BindTo(fMailId);
                        ClientId := fMailServer.RDOCnntId;
                        if fMailServer.SetDAConnectionById(ClientId) //SetDAConnection((fMailConn as IRDOConnection).LocalAddress, fDALockPort)
                          then
                            begin
                              if fMailServer.BindTo(tidRDOHook_MailServer)
                                then fWorld.MailServer := self
                                else
                                  begin
                                    fMailConn := nil;
                                    fMailServer := Unassigned;
                                  end
                            end
                          else
                            begin
                              //raise Exception.Create( '' );
                              fMailConn := nil;
                              fMailServer := Unassigned;
                            end;
                      end
                    else
                      begin
                        //raise Exception.Create( '' );
                        fMailConn := nil;
                        fMailServer := Unassigned;
                      end;
                end
              else
                begin
                  fMailConn := nil;
                  fMailServer := Unassigned;
                end;
          end
        else
          begin
            fMailConn := nil; //raise Exception.Create( '' );
            fMailServer := Unassigned;
          end;
    end;

  procedure TModelServer.InitNewsServer( ServerName : string; ServerPort : integer; WorldName : string );
    var
      i : integer;
    begin
      fNewsConn        := TWinSockRDOConnection.Create( 'NewsCnx' );
      fNewsConn.Server := ServerName;
      fNewsConn.Port   := ServerPort;
      fNewsServer      := TRDOObjectProxy.Create as IDispatch;
      if fNewsConn.Connect( 10000 )
        then
          begin
            (fNewsConn as IRDOConnection).OnDisconnect := OnNewsDisconnect;
            fNewsServer.SetConnection( fNewsConn );
            if fNewsServer.BindTo(tidRDOHook_NewsServer)
              then
                begin
                  fWorld.NewsServer := self;
                  for i := 0 to pred(LangList.Count) do
                    fNewsServer.RDOCreateNewsCenter( WorldName, fLocalHost, fDALockPort, LangList[i] );
                end
              else
                begin
                  fNewsConn := nil;
                  fNewsServer := Unassigned;
                end;
          end
        else
          begin
            //raise Exception.Create( '' );
            fNewsConn := nil;
            fNewsServer := Unassigned;
          end;
    end;

  procedure TModelServer.InitDirServer( AreaName, ServerName : string; ServerPort : integer );
    var
      session : integer;
    begin
      fDSProxyLock.Enter;
      try
        fDirConn        := TWinSockRDOConnection.Create( 'DSCnx' );
        fDirConn.Server := ServerName;
        fDirConn.Port   := ServerPort;
        fDirServer      := TRDOObjectProxy.Create as IDispatch;
        if fDirConn.Connect( 60000 )
          then
            begin
              (fDirConn as IRDOConnection).OnDisconnect := OnDSDisconnect;
              fDirServer.SetConnection( fDirConn );
              if fDirServer.BindTo('DirectoryServer')
                then
                  begin
                    fDirServer.TimeOut := 20000;
                    session            := fDirServer.RDOOpenSession;
                  end
                else session := 0;
              if session <> 0
                then
                  begin
                    fDirServer.BindTo( session );
                    fDirServer.RDOSetExpires(false);
                    fWorld.DirProxy := fDirServer;
                    fWorld.Area     := AreaName;
                  end
                else
                  begin
                    fDirConn := nil;
                    fDirServer := Unassigned;
                    fWorld.DirProxy := Unassigned;
                    raise Exception.Create( '' );
                  end;
            end
          else
            begin
              fDirConn := nil;
              fDirServer := Unassigned;
              fWorld.DirProxy := Unassigned;
              raise Exception.Create( '' );
            end;
      finally
        fDSProxyLock.Leave;
      end;
    end;

  procedure TModelServer.SetPurgeTyccons(count : integer);
    begin
      fTycconsToPurge := count;
      if fWorld <> nil
        then fWorld.TycoonsToPurge := count;
    end;

  type
    PMDXNode = ^TMDXNode;
    TMDXNode =
      record
        Id              : string;
        MDX             : THandle;
        Depends         : TStringList;
        SubNodes        : TCollection;
        registered      : boolean;
        postregistered  : boolean;
        worldregistered : boolean;
      end;

  function TModelServer.GetHoursADay : integer;
    begin
      if TheWorld <> nil
        then result := round(TheWorld.HoursADay)
        else result := 0;
    end;

  procedure TModelServer.LoadExtensions( Notify : TBackupReaderNotify );
    var
      mdxI     : integer;
      mdxCount : integer;
    {
    function GetMDXsFilenames : TStringList;
      var
        Reg    : TRegistry;
        Values : TStrings;
        i      : integer;
      begin
        result := TStringList.Create;
        Reg    := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey( tidRegKey_ModelExtensions, false )
            then
              begin
                Values := TStringList.Create;
                Reg.GetValueNames( Values );
                for i := 0 to pred(Values.Count) do
                  result.Add( Reg.ReadString( Values[i] ));
                Values.Free;
              end;
        finally
          Reg.Free;
        end;
      end;
    }

    function GetMDXsFilenames : TStringList;
      var
        path      : string;
        SearchRec : TSearchRec;
        found     : integer;
      begin
        result := TStringList.Create;
        path   := ExtractFilePath( paramstr(0) );
        found  := FindFirst( path + '*.mdx', faArchive, SearchRec );
        try
          while found = 0 do
            begin
              result.Add( path + SearchRec.Name );
              found := FindNext( SearchRec );
            end;
        finally
          FindClose( SearchRec );
        end;
      end;

    function LoadMDXs( filenames : TStringList ) : TCollection;
      var
        i    : integer;
        Node : PMDXNode;
      begin
        mdxI := 0;
        mdxCount := 3*filenames.Count;
        result := TCollection.Create( 0, rkUse );
        for i := 0 to pred(filenames.Count) do
          try
            Notify( 'Loading Model extensions...', 100*succ(mdxI) div mdxCount );
            inc( mdxI );
            new( Node );
            Node.MDX             := LoadLibrary( pchar(filenames[i] ));
            Node.Id              := GetMDXId( Node.MDX );
            Node.Depends         := GetMDXDependances( Node.MDX );
            Node.SubNodes        := TCollection.Create( 0, rkUse );
            Node.registered      := false;
            Node.postregistered  := false;
            Node.worldregistered := false;
            result.Insert( TObject(Node) );
          except
            raise Exception.Create( 'Could not load MDX: ' + filenames[i] )
          end;
      end;

    function FindMDXNode( MDXNodes : TCollection; MDXId : string ) : PMDXNode;
      var
        i : integer;
      begin
        i := 0;
        while (i < MDXNodes.Count) and (PMDXNode(MDXNodes[i]).Id <> MDXId) do
          inc( i );
        if i < MDXNodes.Count
          then result := PMDXNode(MDXNodes[i])
          else result := nil;
      end;

    procedure BuildMDXTree( MDXNodes : TCollection );
      var
        i, j         : integer;
        MDXNode      : PMDXNode;
        SuperMDXNode : PMDXNode;
      begin
        for i := 0 to pred(MDXNodes.Count) do
          begin
            MDXNode := PMDXNode(MDXNodes[i]);
            for j := 0 to pred(MDXNode.Depends.Count) do
              begin
                SuperMDXNode := FindMDXNode( MDXNodes, MDXNode.Depends[j] );
                if SuperMDXNode <> nil
                  then SuperMDXNode.SubNodes.Insert( TObject(MDXNode) );
              end;
          end;
      end;

    procedure RegisterMDXTree( MDXRootNode : PMDXNode );
      var
        i : integer;
      begin
        if not MDXRootNode.registered
          then
            begin
              Notify( 'Registering extension: ' + MDXRootNode.Id + '...', 100*succ(mdxI) div mdxCount );
              inc( mdxI );
              RegisterMDX( MDXRootNode.MDX );
              MDXRootNode.registered := true;
              for i := 0 to pred(MDXRootNode.SubNodes.Count) do
                RegisterMDXTree( PMDXNode(MDXRootNode.SubNodes[i]) );
            end;
      end;

    procedure RegisterMDXNodes( MDXNodes : TCollection );
      var
        i : integer;
      begin
        for i := 0 to pred(MDXNodes.Count) do
          begin
            if PMDXNode(MDXNodes[i]).Depends.Count = 0
              then RegisterMDXTree( PMDXNode(MDXNodes[i]) )
          end;
      end;

    procedure PostRegisterMDXTree( MDXRootNode : PMDXNode );
      var
        i : integer;
      begin
        if not MDXRootNode.postregistered
          then
            begin
              Notify( 'Post-registering extension: ' + MDXRootNode.Id + '...', 100*succ(mdxI) div mdxCount );
              inc( mdxI );
              PostRegisterMDX( MDXRootNode.MDX );
              MDXRootNode.postregistered := true;
              for i := 0 to pred(MDXRootNode.SubNodes.Count) do
                PostRegisterMDXTree( PMDXNode(MDXRootNode.SubNodes[i]) );
            end;
      end;

    procedure PostRegisterMDXNodes( MDXNodes : TCollection );
      var
        i : integer;
      begin
        for i := 0 to pred(MDXNodes.Count) do
          begin
            if PMDXNode(MDXNodes[i]).Depends.Count = 0
              then PostRegisterMDXTree( PMDXNode(MDXNodes[i]) )
          end;
      end;

    procedure DisposeMDXNodes( MDXNodes : TCollection );
      var
        i       : integer;
        MDXNode : PMDXNode;
      begin
        for i := 0 to pred(MDXNodes.Count) do
          begin
            MDXNode := PMDXNode(MDXNodes[i]);
            MDXNode.Depends.Free;
            MDXNode.SubNodes.Free;
            dispose( MDXNode );
          end;
      end;

    begin
      fMDXNodes := LoadMDXs( GetMDXsFilenames );
      BuildMDXTree( fMDXNodes );
      RegisterMDXNodes( fMDXNodes );
      Inventions.CreateInventions(ExtractFilePath(ParamStr(0)) + 'Inventions\');
      PostRegisterMDXNodes( fMDXNodes );
    end;
                                      
  procedure TModelServer.LoadWorldExtensions( World : TWorld; Notify : TBackupReaderNotify; WorldLoaded : boolean );

    procedure RegisterWorldMDXTree( MDXRootNode : PMDXNode );
      var
        i : integer;
      begin
        if not MDXRootNode.worldregistered
          then
            begin
              Notify( 'Including World extensions from: ' + MDXRootNode.Id + '...', 0 );
              RegisterWorldMDX( MDXRootNode.MDX, World, WorldLoaded );
              MDXRootNode.worldregistered := true;
              for i := 0 to pred(MDXRootNode.SubNodes.Count) do
                RegisterWorldMDXTree( PMDXNode(MDXRootNode.SubNodes[i]) );
            end;
      end;

    procedure RegisterWorldMDXNodes( MDXNodes : TCollection );
      var
        i : integer;
      begin
        for i := 0 to pred(MDXNodes.Count) do
          begin
            if PMDXNode(MDXNodes[i]).Depends.Count = 0
              then RegisterWorldMDXTree( PMDXNode(MDXNodes[i]) )
          end;
      end;

    procedure RegisterExtensionsInRDO;
      var
        i  : integer;                                  
        id : string;
      begin
        for i := 0 to pred(World.WorldExtensions.Count) do
          begin                                                
            id := TWorldExtension(World.WorldExtensions[i]).GetId;
            fDAServ.RegisterObject( id, integer(World.WorldExtensions[i]) );
            fDALockServ.RegisterObject( id, integer(World.WorldExtensions[i]) );
          end;
      end;

    begin
      RegisterWorldMDXNodes( fMDXNodes );
      RegisterExtensionsInRDO;
    end;

  procedure TModelServer.SetTimeRes( aVirtTimeRate, aSimRate, anIntRate : integer );
    begin
      fTimer.Interval    := aVirtTimeRate;
      fSimTimer.Interval := aSimRate;
      fIntTimer.Interval := anIntRate;
    end;

  procedure TModelServer.OnTimer( Sender : TObject );
    begin
      fWorld.VirtualTimeTick( 1 );
    end;

  procedure TModelServer.OnSimTimer( Sender : TObject );
    begin
      fSimEv.SetEvent;
    end;

  procedure TModelServer.OnIntTimer( Sender : TObject );
    begin
      fIntEv.SetEvent;
    end;

  procedure TModelServer.OnCargoTimer( Sender : TObject );
    begin
      fCargoEv.SetEvent;
    end;

  procedure TModelServer.OnAreaChanged( x, y, dx, dy : integer );
    begin
      try
        fISProxyLock.Enter;
        try
          if not VarIsEmpty(fISEvents)
            then fISEvents.RefreshArea( x, y, dx, dy );
        finally
          fISProxyLock.Leave;
        end;
      except
      end;
    end;

  procedure TModelServer.OnFacilityChanged( Facility : TFacility; FacilityChange : TFacilityChange );
    begin
      try
        fISProxyLock.Enter;
        try
          if not VarIsEmpty(fISEvents)
            then fISEvents.RefreshObject( integer(Facility), integer(FacilityChange) );
        finally
          fISProxyLock.Leave;
        end;
      except
      end;
    end;

  procedure TModelServer.OnTycoonsChanged;
    begin
      try
        fISProxyLock.Enter;
        try
          if not VarIsEmpty(fISEvents)
            then fISEvents.RefreshTycoons( 0 );
        finally
          fISProxyLock.Leave;
        end;
      except
      end;
    end;

  procedure TModelServer.OnDateChanged( Date : TDateTime );
    begin
      try
        fISProxyLock.Enter;
        try
          if not VarIsEmpty(fISEvents)
            then fISEvents.RefreshDate( Date );
        finally
          fISProxyLock.Leave;
        end;
      except
      end;
    end;

  procedure TModelServer.OnSeasonChanged( Season : TSeason );
    begin
      try
        fISProxyLock.Enter;
        try
          if not VarIsEmpty(fISEvents)
            then fISEvents.RefreshSeason( integer(Season) );
        finally
          fISProxyLock.Leave;
        end;
      except
      end;
    end;

  procedure TModelServer.OnEndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
    begin
      try
        if TheWorld.ForceReconnect
          then
            begin
              InitNewsServer( fNewsConn.Server, fNewsConn.Port, TheWorld.Name );
              InitDirServer( fWorld.Area, fDirConn.Server, fDirConn.Port );
              TheWorld.ForceReconnect := false;
            end;
        fISProxyLock.Enter;
        try
          if (PeriodType = perYear) and not VarIsEmpty(fISEvents)
            then fISEvents.EndOfPeriod( 0 );
        finally
          fISProxyLock.Leave;
        end;
      except
      end;
    end;

  procedure TModelServer.OnTycoonRetired( name : string );
    begin
      try
        fISProxyLock.Enter;
        try
          if not VarIsEmpty(fISEvents)
            then fISEvents.TycoonRetired( name );
        finally
          fISProxyLock.Leave;
        end;
      except
      end;
    end;

  {
  procedure TModelServer.OnPoolActed( PoolId : TActorPoolId; ViewerId : TViewerId; TickCount : cardinal; TickData : TStream );
    var
      TickDataStr : string;
    begin
      try
        fISProxyLock.Enter;
        try
          if not VarIsEmpty(fISEvents)
            then
              begin
                TickDataStr := StreamToString( TickData );
                fISEvents.SendTickData( integer(PoolId), integer(ViewerId), integer(TickCount), widestring(TickDataStr) );
              end;
        finally
          fISProxyLock.Leave;
        end;
      except
      end;
    end;
  }

  procedure TModelServer.OnSendNotification( TycoonId : integer; Kind : integer; Title, Body : string; Options : integer );
    begin
      try
        fISProxyLock.Enter;
        try
          if not VarIsEmpty(fISEvents)
            then fISEvents.SendNotification( TycoonId, Kind, widestring(Title), widestring(Body), Options );
        finally
          fISProxyLock.Leave;
        end;
      except
      end;
    end;

  procedure TModelServer.OnModelStatusChange( status : integer );
    begin
      try
        fISProxyLock.Enter;
        try
          if not VarIsEmpty(fISEvents)
            then
              begin
                fISEvents.WaitForAnswer := true;
                try
                  fISEvents.ModelStatusChanged( status );
                finally
                  fISEvents.WaitForAnswer := false;
                end;
              end;
        finally
          fISProxyLock.Leave;
        end;
      except
      end;
    end;

  procedure TModelServer.OnISDisconnect( const ClientConnection : IRDOConnection );
    begin
      try
        fISProxyLock.Enter;
        try
          fISEvents := Unassigned;
        finally
          fISProxyLock.Leave;
        end;
      except
      end;
    end;

  procedure TModelServer.OnDSDisconnect( const ClientConnection : IRDOConnection );
    begin
      try
        fDSProxyLock.Enter;
        try
          fDirConn        := nil;
          fDirServer      := Unassigned;
          fWorld.DirProxy := Unassigned;
        finally
          fDSProxyLock.Leave;
        end;
      except
      end;
    end;

  procedure TModelServer.OnMailDisconnect( const ClientConnection : IRDOConnection );
    begin
      try
        fMailConn   := nil;
        fMailServer := Unassigned;
      except
      end;
    end;

  procedure TModelServer.OnNewsDisconnect( const ClientConnection : IRDOConnection );
    begin
      try
        fNewsConn   := nil;
        fNewsServer := Unassigned;
      except
      end;
    end;

  procedure TModelServer.StoreBackup( mode : TBackupMode; ErrorLevel : integer );
    const
      MaxBackupRetries = 2;
    var
      Writer    : IBackupWriter;
      filename  : string;
    begin
      try
        fWorld.StopVirtualTime;
        try
          case mode of
            bmdShutdown :
              begin
                fWorld.SplitBackup := false;
                try
                  filename := fWorld.Name + '.world';
                  Writer := CreateBackup( fBaseDir + filename, false, nil );
                  try
                    Writer.WriteObject( 'World', fWorld );
                    Writer.WriteInteger('CacheLogVer', fCacheLogVer);
                    inc(fCacheLogVer);
                    ModelServerCache.SetLogVer(fCacheLogVer);
                  finally
                    Writer := nil;
                  end;
                  RegisterLastGoodBackup(filename);
                except
                end;
                try
                  filename := fWorld.Name + '.verb';
                  Writer := CreateBackup( fBaseDir + filename, true, nil );
                  try
                    Writer.WriteObject( 'World', fWorld );
                    Writer.WriteInteger('CacheLogVer', fCacheLogVer);
                    inc(fCacheLogVer);
                    ModelServerCache.SetLogVer(fCacheLogVer);
                  finally
                    Writer := nil;
                  end;
                except
                end;
              end;
            else
              begin
                //OnModelStatusChange( mstBusy );
                Application.ProcessMessages;
                try
                  if fLastBackup < 20 // 100
                    then inc( fLastBackup )
                    else fLastBackup := 1;
                  if not fBackupError
                    then filename := fWorld.Name + '.' + IntToStr( fLastBackup ) + '.back'
                    else filename := fWorld.Name + '.ERROR.' + IntToStr( fLastBackup ) + '.back';
                  inc( fSaveCount );
                  try
                    if {(fSaveCount mod 20 <> 0) and} not fBackupError
                      then
                        begin
                          Log( tidLog_Survival, TimeToStr( Now ) + ' - Starting binary backup.' );
                          try
                            Writer := CreateBackup( fBaseDir + filename, false, nil );
                            try
                              Writer.WriteObject( 'World', fWorld );
                              Writer.WriteInteger('CacheLogVer', fCacheLogVer);
                              inc(fCacheLogVer);
                              ModelServerCache.SetLogVer(fCacheLogVer);
                            finally
                              Writer := nil;
                            end;
                            RegisterLastGoodBackup(filename);
                          except
                            Log( tidLog_Survival, 'Error backing up world.' );
                            raise;
                          end;
                          Log( tidLog_Survival, TimeToStr( Now ) + ' - End binary backup.' );
                        end
                      else
                        begin
                          Log( tidLog_Survival, TimeToStr( Now ) + ' - Starting verbose backup.' );
                          try
                            Writer := CreateBackup( fBaseDir + filename, true, nil );
                            try
                              Writer.WriteObject( 'World', fWorld );
                              Writer.WriteInteger('CacheLogVer', fCacheLogVer);
                              inc(fCacheLogVer);
                              ModelServerCache.SetLogVer(fCacheLogVer);
                            finally
                              Writer := nil;
                            end;
                            //RegisterLastGoodBackup(filename);
                          except
                            Log( tidLog_Survival, 'Error backing up verbose world.' );
                            raise;
                          end;
                          Log( tidLog_Survival, TimeToStr( Now ) + ' - End verbose backup.' );
                        end;
                    if fSatCount mod 10 = 0
                      then
                        begin
                          CallSync( CreateSatelliteMaps );
                          fSatRenderComplete.WaitFor( INFINITE );
                          fSatRenderComplete.ResetEvent;
                        end;
                    inc( fSatCount );
                  finally
                    //OnModelStatusChange( mstNotBusy );
                  end;
                  fBackupError := false;
                except
                  if ErrorLevel < MaxBackupRetries
                    then StoreBackup( mode, ErrorLevel + 1 )
                    else
                      begin
                        fBackupError := true;
                        //OnModelStatusChange( mstError );
                      end;
                end;
              end;
          end;
        finally
          fWorld.ResumeVirtualTime;
        end;
      except
      end;
    end;

  procedure TModelServer.CreateSatelliteMaps;
    type
      TRGB =
        packed record
          r, g, b, x : byte;
        end;

    procedure AddClouds( Map : TBitmap );
      const
        MapWeight       = 200;
        MapShadowWeight = 350;
      var
        Clouds : TBitmap;
        x, y   : integer;
        color  : TColor;
        alpha  : integer;

      function cU( x : integer ) : integer;
        begin
          if x >= 0
            then result := x mod Clouds.Width
            else result := Clouds.Width + (x mod Clouds.Width);
        end;

      function cV( y : integer ) : integer;
        begin
          if y >= 0
            then result := y mod Clouds.Height
            else result := Clouds.Height + (y mod Clouds.Height);
        end;

      begin
        Clouds := TBitmap.Create;
        try
          Clouds.LoadFromFile( ExtractFilePath(Application.ExeName) + 'Maps\clouds' + IntToStr(random(2)) + '.bmp' );
          for y := 0 to pred(Map.Height) do
            for x := 0 to pred(Map.Width) do
              begin
                color := Map.Canvas.Pixels[x, y];

                // draw shadow
                alpha := TRGB(Clouds.Canvas.Pixels[cU(x - 40), cV(y - 40)]).g;
                TRGB(color).r := (MapShadowWeight*TRGB(color).r) div (alpha + MapShadowWeight);
                TRGB(color).g := (MapShadowWeight*TRGB(color).g) div (alpha + MapShadowWeight);
                TRGB(color).b := (MapShadowWeight*TRGB(color).b) div (alpha + MapShadowWeight);

                // draw cloud
                alpha := TRGB(Clouds.Canvas.Pixels[cU(x), cV(y)]).g;
                TRGB(color).r := (MapWeight*TRGB(color).r + alpha*$FF) div (alpha + MapWeight);
                TRGB(color).g := (MapWeight*TRGB(color).g + alpha*$FF) div (alpha + MapWeight);
                TRGB(color).b := (MapWeight*TRGB(color).b + alpha*$FF) div (alpha + MapWeight);
                Map.Canvas.Pixels[x, y] := color;
              end;
        finally
          Clouds.Free;
        end;    
      end;

    const
      Scale = 2;
    var
      Map                    : TBitmap;
      SmallMap               : TBitmap;
      x, y                   : integer;
      xi, yi                 : integer;
      rSum, gSum, bSum, xSum : integer;
      col                    : TRGB;
      jpgImage               : TJPEGImage;
    begin
      try
        try
          Map := fWorld.GenerateSatelliteMap( 0 );
          try
            SmallMap := TBitmap.Create;
            try
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Creating destination bitmap...' );
              SmallMap.PixelFormat := pf32bit;
              SmallMap.Width  := Map.Width div Scale;
              SmallMap.Height := Map.Height div Scale;
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Scaling bitmap...' );
              for y := 0 to pred(SmallMap.Width) do
                for x := 0 to pred(SmallMap.Height) do
                  begin
                    rSum := 0;
                    gSum := 0;
                    bSum := 0;
                    xSum := 0;
                    for yi := 0 to pred(Scale) do
                      for xi := 0 to pred(Scale) do
                        with TRGB(Map.Canvas.Pixels[Scale*x + xi, Scale*y + yi]) do
                          begin
                            rSum := rSum + r;
                            gSum := gSum + g;
                            bSum := bSum + b;
                            xSum := xSum + x;
                          end;
                    col.r := rSum div sqr(Scale);
                    col.g := gSum div sqr(Scale);
                    col.b := bSum div sqr(Scale);
                    col.x := xSum div sqr(Scale);
                    SmallMap.Canvas.Pixels[x, y] := TColor(col);
                  end;
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Adding clouds...' );
              AddClouds( SmallMap );
              jpgImage := TJPEGImage.Create;
              try
                jpgImage.Assign( SmallMap );
                ForceDirectories(fSatelliteMapPath);
                jpgImage.SaveToFile( fSatelliteMapPath + '\general.jpg' );
              finally
                jpgImage.Free;
              end;
            finally
              SmallMap.Free;
            end;
          finally
            Map.Free;
          end;
        except
        end;
      finally
        fSatRenderComplete.SetEvent;
      end;
    end;

  function TModelServer.QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TModelServer._AddRef  : integer; stdcall;
    begin
      result := 1;
    end;

  function TModelServer._Release : integer; stdcall;
    begin
      result := 1;
    end;

  function TModelServer.NewMailAccount(Account, Alias, FwdAddr : string; KeepMsg : boolean) : boolean;
    begin
      try
        if not VarIsEmpty(fMailServer)
          then result := fMailServer.NewMailAccount(fMailId, Account, Alias, Fwdaddr, KeepMsg)
          else result := false;
      except
        result := false;
      end;
    end;

  function TModelServer.DeleteAccount(Account : string) : boolean;
    begin
      try
        if not VarIsEmpty(fMailServer)
          then result := fMailServer.DeleteAccount(fMailId, Account)
          else result := false;
      except
        result := false;
      end;
    end;

  function TModelServer.SetForwardRule(Account, FwdAddr : string; KeepMsg : boolean) : boolean;
    begin
      try
        if not VarIsEmpty(fMailServer)
          then result := fMailServer.SetForwardRule(fMailId, Account, FwdAddr, KeepMsg)
          else result := false;
      except
        result := false;
      end;
    end;

  function TModelServer.SendMessage(From, Dest, Subject : string; Lines : TStringList; html : boolean) : boolean;
    var
      Id : integer;
      i  : integer;
    begin
      try
        if not VarIsEmpty(fMailServer)
          then
            begin
              Id := fMailServer.NewMail(From, Dest, Subject);
              if Id <> 0
                then
                  begin
                    fMailServer.BindTo(Id);
                    if html
                      then fMailServer.AddHeaders('ContentType=text/html');
                    for i := 0 to pred(Lines.Count) do
                      fMailServer.AddLine(Lines[i]);
                    if fMailServer.BindTo(tidRDOHook_MailServer)
                      then
                        begin
                          result := fMailServer.Post(fWorld.Name, Id);
                          fMailServer.CloseMessage(Id);
                        end
                      else result := false;
                  end
                else result := false;
            end
          else result := false;
      except
        result := false;
      end;
    end;

  function TModelServer.SendHTMLMessage(From, Dest, Subject, URL : string) : boolean;
    var
      List : TStringList;
    begin
      try
        List := TStringList.Create;
        try
          List.Add('<HEAD>');
          List.Add('<META HTTP-EQUIV="REFRESH" CONTENT="0; URL=' + URL + '">');
          List.Add('</HEAD>');
          result := SendMessage(From, Dest, Subject, List, true);
        finally
          List.Free;
        end;
      except
        result := false;
      end;
    end;

  procedure TModelServer.UpdateDate(NewDate : TDateTime);
    var
      RemoteId : integer;
    begin
      if not VarIsEmpty(fMailServer)
        then
          begin
            RemoteId := fMailServer.RemoteObjectID;
            fMailServer.BindTo(fMailId);
            fMailServer.SetDate(NewDate);
            fMailServer.BindTo(RemoteId);
          end;
    end;

  procedure TModelServer.CreateNewsCenter( World : string );
    var
      i : integer;
    begin
      if not VarIsEmpty(fNewsServer)
        then
          try
            for i := 0 to pred(LangList.Count) do
              if not VarIsEmpty(fNewsServer)
                then fNewsServer.RDOCreateNewsCenter( World, fLocalHost, fDALockPort, LangList[i] );
          except
          end;
    end;

  procedure TModelServer.CreateNewspaper( World, Name, Style, Town : string );
    var
      i : integer;
    begin
      if not VarIsEmpty(fNewsServer)
        then
          try
            for i := 0 to pred(LangList.Count) do
              if not VarIsEmpty(fNewsServer)
                then fNewsServer.RDOCreateNewspaper( World, Name, Style, Town, fLocalHost, fDALockPort, LangList[i] );
          except
          end;
    end;

  procedure TModelServer.GenerateNewspapers( World : widestring; Date : TDateTime );
    var
      i : integer;
    begin
      if not VarIsEmpty(fNewsServer)
        then
          try
            fNewsServer.WaitForAnswer := false;
            for i := 0 to pred(LangList.Count) do
              if not VarIsEmpty(fNewsServer)
                then fNewsServer.RDOGenerateNewspapers( World, Date, fLocalHost, fDALockPort, LangList[i] );
          except
          end;
    end;

  function TModelServer.RenewCache(Agent, ObjId : string) : TObjectCache;
    const
      Agents : array[0..5] of string =
        (
          'Facility',
          'Input',
          'Output',
          'Tycoon',
          'Company',
          'Town'
        );
      var
        idx : integer;
        Facility : TFacility;
        MetaGate : string;
        Input    : TInput;
        Output   : TOutput;
        Tycoon   : TTycoon;
        Company  : TCompany;
        Town     : TTown;
        p        : integer;
        x, y     : integer;

    procedure GetXY;
      var
        aux : string;
      begin
        aux := CompStringsParser.GetNextStringUpTo(ObjId, p, ',');
        x := StrToInt(aux);
        inc(p);
        aux := CompStringsParser.GetNextStringUpTo(ObjId, p, ',');
        y := StrToInt(aux);
      end;

    function GetFacilityAt(x, y : integer) : TFacility;
      begin
        if (x >= 0) and (y >= 0) and (x < fWorld.xSize) and (y < fWorld.ySize)
          then result := fWorld.FacilityAt(x, y)
          else result := nil;
      end;

    begin
      idx := 0;
      while (idx < 6) and (Agent <> Agents[idx]) do
        inc(idx);
      p := 1;
      case idx of
        0 : // Facility
          begin
            GetXY;
            Facility := GetFacilityAt(x, y);
            if Facility <> nil
              then result := ModelServerCache.UpdateObjectCacheEx(Facility, noKind, noInfo, true)
              else result := nil;
          end;
        1 : // Input
          begin
            GetXY;
            Facility := GetFacilityAt(x, y);
            if Facility <> nil
              then
                begin
                  inc(p);
                  MetaGate := CompStringsParser.GetNextStringUpTo(ObjId, p, #0);
                  Input    := Facility.CurrBlock.InputsByName[MetaGate];
                  if Input <> nil
                    then result := ModelServerCache.UpdateObjectCacheEx(Input, noKind, noInfo, true)
                    else result := nil;
                end
              else result := nil;
          end;
        2 : // Output
          begin
            GetXY;
            Facility := GetFacilityAt(x, y);
            if Facility <> nil
              then
                begin
                  inc(p);
                  MetaGate := CompStringsParser.GetNextStringUpTo(ObjId, p, #0);
                  Output   := Facility.CurrBlock.OutputsByName[MetaGate];
                  if Output <> nil
                    then result := ModelServerCache.UpdateObjectCacheEx(Output, noKind, noInfo, true)
                    else result := nil;
                end
              else result := nil;
          end;
        3 : // Tycoon
          begin
            Tycoon := fWorld.TycoonByName[ObjId];
            if Tycoon <> nil
              then result := ModelServerCache.UpdateObjectCacheEx(Tycoon, noKind, noInfo, true)
              else result := nil;
          end;
        4 : // Company
          begin
            Company := fWorld.CompanyByName[ObjId];
            if Company <> nil
              then result := ModelServerCache.UpdateObjectCacheEx(Company, noKind, noInfo, true)
              else result := nil;
          end;
        5 : // Town
          begin
            Town := fWorld.TownByName[ObjId];
            if Town <> nil
              then result := ModelServerCache.UpdateObjectCacheEx(Town, noKind, noInfo, true)
              else result := nil;
          end;
        else result := nil;
      end;
      // >> Logs.Log('survival', 'Renew cache: Agent=' + Agent + ', Object=' + ObjId); // >>
    end;

  function TModelServer.GetInteger( id : integer ) : integer;
    begin
      case id of
        CFGID_AccountExpires :
          result := fAccountExpires;
        CFGID_LevelLimit :
          result := fLevelLimit;
        else result := 0;
      end;
    end;

  function TModelServer.GetFloat( id : integer ) : double;
    begin
      result := 0;
    end;

  function TModelServer.GetString( id : integer ) : string;
    begin
      result := '';
    end;

  function TModelServer.GetMoney( id : integer ) : TMoney;
    begin
      result := 0;
    end;

  function TModelServer.GetIntegerArray( id : integer; i, j, k, l : integer ) : integer;
    begin
      result := 0;
    end;

  function TModelServer.GetFloatArray( id : integer; i, j, k, l : integer ) : double;
    begin
      result := 0;
    end;

  function TModelServer.GetStringArray( id : integer; i, j, k, l : integer ) : string;
    begin
      result := '';
    end;

  function TModelServer.GetMoneyArray( id : integer; i, j, k, l : integer ) : TMoney;
    begin
      result := 0;
    end;

  function TModelServer.GetConfigParm( strId, defVal : string ) : string;
    var
      value : string;
    begin
      value := fConfigInfo.Values[strId];
      if value <> ''
        then result := value
        else result := defVal;
    end;

  function TModelServer.GetMaxInvestors : integer;
    begin
      if fWorld <> nil
        then result := fWorld.MaxInvestors
        else result := 0;
    end;

  procedure TModelServer.SetMaxInvestors(value : integer);
    begin
      if fWorld <> nil
        then fWorld.MaxInvestors := value;
    end;

  procedure TModelServer.CheckMaintenanceSchedule;
    begin
      if fAutoRestart and not fMaintDue
        then
          begin
            fTimeToMaint := realmax(0, fRestartDate - Now);
            fWarnMaint   := fTimeToMaint < 1/24; // >> less than an hour..
            fMaintDue    := Now >= fRestartDate;
          end;
    end;

  procedure TModelServer.RegisterLastGoodBackup(name : string);
    var
      Reg : TRegistry;
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey(tidRegKey_ModelServer, true)
          then Reg.WriteString('LastGoodBackup', name);
      finally
        Reg.Free;
      end;
    end;

  procedure TModelServer.SetMSReadyEvent(signal : boolean);
    begin
      if signal
        then fMSReadyEvent.SetEvent
        else fMSReadyEvent.ResetEvent;
    end;

  function TModelServer.ReportMaintenance(shortMsg, longMsg : string; eta : TDateTime) : boolean;
    begin
      result := fWorld.ReportMaintenance(shortMsg, longMsg, eta);
    end;

  procedure TModelServer.ReportMaintenanceToIS(eta : TDateTime);
    begin
      try
        fISProxyLock.Enter;
        try
          if not VarIsEmpty(fISEvents)
            then fISEvents.ReportMaintenance(eta, fLastDowntime);
        finally
          fISProxyLock.Leave;
        end;
      except
      end;
    end;

  procedure TModelServer.CalculateDownTime;
    var
      delta : TDateTime;
      hh, mm, ss, ms : word;
    begin
      if fLastDateUp <> 0
        then delta := Now - fLastDateUp
        else delta := 0;
      if delta > 1
        then fLastDowntime := 45
        else
          begin
            DecodeTime(delta, hh, mm, ss, ms);
            if hh > 1
              then fLastDowntime := 45
              else fLastDowntime := max(5, min(45, mm));
          end;
    end;

end.


