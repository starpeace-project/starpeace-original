unit ModelServer;

interface

  uses
    Windows, Classes, Protocol, Kernel, Population, RDOServer, ExtCtrls, BackupObjects,
    SyncObjs, RDOInterfaces, MailServerInterfaces;

  const
    tidRDOHook_World = 'World';

  const
    tidRegKey_ModelExtensions = '\SOFTWARE\Oceanus\FIVE\ModelServer\Extensions';

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

  const
    BackupPeriod = 30;  // in minutes

  type
    TModelServer =
      class(TObject, IMailServer)
        public
          constructor Create;
          destructor  Destroy; override;
        public
          procedure InitWorld( aName : string; aNotEvent : TBackupReaderNotify; aBaseDir, aCacheHost : string; aDAPort, aCSPort, aCCPort : integer; SimSpeed : TThreadPriority; aMailHost : string; aMailPort : integer );
          procedure InitInterfaceServerEvents( ServerName : string; ServerPort : integer );
          procedure InitMailServer( ServerName : string; ServerPort : integer; WorldName : string );
        private
          procedure StartCacheServer( aCSPort : integer );
          procedure StartDirectAccessServer( aDAPort : integer );
        private
          fBaseDir    : string;
          fWorld      : TInhabitedWorld;
          fDAConn     : IRDOConnectionsServer;
          fDAServ     : TRDOServer;
          fDALockConn : IRDOConnectionsServer;
          fDALockServ : TRDOServer;
          fISConn     : IRDOConnectionInit;
          fISEvents   : OleVariant;
          fBPS        : integer;
          fDAPort     : integer;
          fDALockPort : integer;
        public
          property TheWorld : TInhabitedWorld read fWorld;
          property BPS      : integer         read fBPS; // Blocks per second
        private
          fExtensionList : TStrings;
        public
          property ExtensionList : TStrings read fExtensionList;
        public
          procedure RegisterModelExtension( filename : string );
        private
          procedure LoadExtensions;
        private
          fTimer      : TTimer;
          fSimTimer   : TTimer;
          fIntTimer   : TTimer;
          fSimEv      : TEvent;
          fIntEv      : TEvent;
          fSimThread  : TThread;
          fLastBackup : integer;
        private
          procedure SetTimeRes( aVirtTimeRate, aSimRate, anIntRate : integer );
          procedure OnTimer( Sender : TObject );
          procedure OnSimTimer( Sender : TObject );
          procedure OnIntTimer( Sender : TObject );
        private
          fIntegrationThread : TThread;
          fIntegrationLock   : TCriticalSection;
        private
          procedure OnAreaChanged( x, y, dx, dy : integer );
          procedure OnFacilityChanged( Facility : TFacility; FacilityChange : TFacilityChange );
          procedure OnTycoonsChanged;
          procedure OnDateChanged( Date : TDateTime );
          procedure OnEndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
          procedure OnTycoonRetired( name : string );
        private
          procedure StoreBackup( mode : TBackupMode );
        private
          fMailConn   : IRDOConnectionInit;
          fMailServer : OleVariant;
          fMailId     : integer;
        // IUnknown
        private
          function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function _AddRef  : integer; stdcall;
          function _Release : integer; stdcall;
        // IMailServer
        private
          function NewMailAccount(Account, Alias, FwdAddr : string; KeepMsg : boolean) : boolean;
          function DeleteAccount (Account : string) : boolean;
          function SetForwardRule(Account, FwdAddr : string; KeepMsg : boolean) : boolean;
          function SendMessage(From, Dest, Subject : string; Lines : TStringList; html : boolean) : boolean;
          function SendHTMLMessage(From, Dest, Subject, URL : string) : boolean;
        private
          // fBackupEvent : TEvent;
      end;

  const
    TheModelServer : TModelServer = nil;

implementation

  uses
    SmartThreads, ClassStorage, SysUtils, Trade, ModelServerCache, ComObj,
    Registry, IniFiles, KernelCache, WinSockRDOConnectionsServer, HostNames,
    Construction, WinSockRDOConnection, RDOObjectProxy, Surfaces,
    BackupInterfaces, PopulatedBlock, WorkCenterBlock, Collection, CollectionBackup,
    PyramidalModifier, PublicFacility, StdFluids, World, MailProtocol,
    Circuits, ResearchCenter, Headquarters, ConnectedBlock;


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
        Priority := tpHigher;
        // >> FreeOnTerminate := true;
        Resume;
      end;

    procedure TBackupThread.Execute;
      begin
        fServer.StoreBackup( fMode );
      end;


  // TSimTread

  type
    TSimThread =
      class( TSmartThread )
        private
          fServer : TModelServer;
        protected
          procedure Execute; override;
      end;

    procedure TSimThread.Execute;
      var
        SimStart : integer;
        SimCount : integer;
        Backup   : TBackupThread;
      begin
        SimCount := 1;
        while not Terminated do
          try
            if fServer.fSimEv.WaitFor( fServer.fSimTimer.Interval ) <> wrTimeout
              then
                begin
                  fServer.fSimEv.ResetEvent;
                  SimStart := DateTimeToTimeStamp(Time).time;
                  fServer.fWorld.Simulate;
                  if SimCount mod (BackupPeriod*60*1000 div integer(fServer.fSimTimer.Interval)) = 0
                  //if SimCount mod (10*1000 div fServer.fSimTimer.Interval) = 0
                    then
                      begin
                        // Wait for file transfer operation uff!!
                        // >> fServer.fBackupEvent.WaitFor( INFINITE );
                        fServer.fIntegrationLock.Enter;
                        // >> fServer.fBackupEvent.ResetEvent;
                        // Create the thread
                        Backup := TBackupThread.Create( fServer, bmdInGame );
                        // >> fServer.fBackupEvent.WaitFor( INFINITE );
                        WaitForSingleObject( Backup.Handle, INFINITE );
                        Backup.Free;
                        fServer.fIntegrationLock.Leave;
                        // Set the event to avoid re-entrance here uff!!
                        // >> fServer.fBackupEvent.ResetEvent;
                      end;
                  inc( SimCount );
                  if DateTimeToTimeStamp(Time).time > SimStart
                    then fServer.fBPS := 1000*fServer.fWorld.Facilities.Count div (DateTimeToTimeStamp(Time).time - SimStart)
                end;
          except
          end;
      end;


  // TIntegrationThread

  type
    TIntegrationThread =
      class( TSmartThread )
        public
          constructor Create;
        private
          fServer          : TModelServer;
          fIntegrationPool : TIntegratorPool;
        protected
          procedure Execute; override;
      end;

    constructor TIntegrationThread.Create;
      begin
        inherited Create( true );
        fIntegrationPool := TIntegratorPool(TheClassStorage.ClassById[tidClassFamily_SurfacePools, tidSurfacePool_Integrators]);
        Priority := tpIdle;
      end;

    procedure TIntegrationThread.Execute;
      begin
        while not Terminated do
          try
            fServer.fIntegrationLock.Enter;
            if fServer.fIntEv.WaitFor( fServer.fIntTimer.Interval ) <> wrTimeout
              then
                begin
                  fServer.fIntEv.ResetEvent;
                  fIntegrationPool.IntegrateAll;
                end;
            fServer.fIntegrationLock.Leave;
          except
          end;
      end;


  // TModelServer

  constructor TModelServer.Create;
    begin
      inherited Create;
      fExtensionList := TStringList.Create;
      // >> fBackupEvent := TEvent.Create(nil, true, true, '');
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
        // Stop integration thread
        if fIntegrationThread <> nil
          then
            begin
              fIntegrationThread.Terminate;
              WaitForSingleObject( fIntegrationThread.Handle, 60000 );
              fIntegrationThread.Free;
            end;
        // Stop simulation thread
        if fSimThread <> nil
          then
            begin
              fSimThread.Terminate;
              WaitForSingleObject( fSimThread.Handle, 60000 );
            end;
        // Free timers and events
        fTimer.Free;
        fSimTimer.Free;
        fIntTimer.Free;
        fSimEv.Free;
        fIntEv.Free;
        // Free world
        if fWorld <> nil
          then
            begin
              try
                StoreBackup( bmdShutdown );
              except
                MessageBox( 0, 'Could not save world!', 'Backup Error', MB_ICONWARNING or MB_OK );
              end;
              //UpdateModelExtensionList;
              //fWorld.Free;
            end;
        fIntegrationLock.Free;
        ClassStorage.DoneTheClassStorage;
        ModelServerCache.DoneModelServerCache;
        fExtensionList.Free;
        inherited;
      except
      end;
    end;

  procedure TModelServer.InitWorld( aName : string; aNotEvent : TBackupReaderNotify; aBaseDir, aCacheHost : string; aDAPort, aCSPort, aCCPort : integer; SimSpeed : TThreadPriority; aMailHost : string; aMailPort : integer );

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
            then fWorld := TInhabitedWorld.Create( Name, xSize, ySize )
            else raise Exception.Create( 'Invalid World INI file!' );
        finally
          IniFile.Free;
        end;
      end;

    procedure InitWorldFromIniFile;

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
          for i := 0 to pred(count) do
            begin
              Name    := IniFile.ReadString( tidIniSection_WorldTowns, tidIniValue_TownName + IntToStr(i), '' );
              Cluster := IniFile.ReadString( tidIniSection_WorldTowns, tidIniValue_TownCluster + IntToStr(i), '' );
              x       := IniFile.ReadInteger( tidIniSection_WorldTowns, tidIniValue_TownX + IntToStr(i), -1 );
              y       := IniFile.ReadInteger( tidIniSection_WorldTowns, tidIniValue_TownY + IntToStr(i), -1 );
              if (Name <> '') and (Cluster <> '') and (x >= 0) and (y >= 0)
                then
                  begin
                    Town := TInhabitedTown.Create( Name, Cluster, Cluster + 'TownHall', Cluster + 'TradeCenter', x, y, fWorld );
                    fWorld.Towns.Insert( Town );
                    CacheObject( Town );
                  end
                else raise Exception.Create( 'Invalid Town INI info (' + IntToStr(i) + ')' );
            end;
        end;

      var
        IniFile : TIniFile;
      begin
        IniFile := TIniFile.Create( fBaseDir + aName + '.ini'  );
        try
          fWorld.InitClusters;
          InitTowns( IniFile );
        finally
          IniFile.Free;
        end;
      end;

    var
      Reader    : IBackupReader;
      WorldURL  : string;
      UseBackup : boolean;

    begin
      fDAPort     := aDAPort;
      fDALockPort := aDAPort + 1;

      // Create the world
      if aBaseDir[length(aBaseDir)] <> '\'
        then fBaseDir := aBaseDir + '\'
        else fBaseDir := aBaseDir;

      if not RegisterWorld( aName, aCacheHost, aCSPort, aCCPort, WorldURL )
        then raise Exception.Create( 'Cannot register world in cache server side' );

      ModelServerCache.InitModelServerCache;
      ClassStorage.InitTheClassStorage;
      Kernel.RegisterSurfaces;
      Population.RegisterSurfaces;
      KernelCache.RegisterCachers;
      Population.RegisterMetaFluids;
      Population.RegisterMetaInstances;
      StdFluids.RegisterMetaFluids;

      WorkCenterBlock.RegisterTownParameters;

      CollectionBackup.RegisterBackup;
      Kernel.RegisterBackup;
      World.RegisterBackup;
      ConnectedBlock.RegisterBackup;
      Population.RegisterBackup;
      ResearchCenter.RegisterBackup;
      Headquarters.RegisterBackup;
      Trade.RegisterBackup;
      Construction.RegisterBackup;
      PopulatedBlock.RegisterBackup;
      WorkCenterBlock.RegisterBackup;
      Surfaces.RegisterBackup;
      Circuits.RegisterBackup;
      PyramidalModifier.RegisterBackup;
      PublicFacility.RegisterBackup;

      LoadExtensions;

      UseBackup := FileExists( fBaseDir + aName + '.world' );
      if UseBackup
        then
          begin
            Reader := OpenBackup( fBaseDir + aName + '.world',  aNotEvent );
            Reader.ReadObject( 'World', fWorld, nil );
            Reader := nil;
          end
        else CreateWorldFromIniFile;

      fWorld.LoadLandInfo( fBaseDir + aName + '.bmp' );

      // Set World base URL
      fWorld.WorldURL := WorldURL;

      // Start RDO servers
      StartDirectAccessServer( aDAPort );
      StartCacheServer( aCSPort );

      // Start Mail Server
      InitMailServer( aMailHost, aMailPort, aName );

      // Create new world after Mail and RDO servers are running
      if not UseBackup
        then InitWorldFromIniFile;

      // Init Simulation clockwork
      fTimer     := TTimer.Create( nil );
      fSimTimer  := TTimer.Create( nil );
      fIntTimer  := TTimer.Create( nil );
      fSimEv     := TEvent.Create( nil, true, false, '' );
      fIntEv     := TEvent.Create( nil, true, false, '' );
      fSimThread := TSimThread.Create( true );
      TSimThread(fSimThread).fServer := self;
      fTimer.OnTimer := OnTimer;
      fSimTimer.OnTimer := OnSimTimer;
      fIntTimer.OnTimer := OnIntTimer;
      SetTimeRes( 1500, 1500, 30000 );
      fSimThread.Priority := SimSpeed;
      fSimThread.Resume;

      // Init Integration thread
      fIntegrationThread := TIntegrationThread.Create;
      fIntegrationLock   := TCriticalSection.Create;
      TIntegrationThread(fIntegrationThread).fServer := self;
      fIntegrationThread.Resume;
    end;

  procedure TModelServer.StartCacheServer( aCSPort : integer );
    var
      ServerConn : TWinSockRDOConnectionsServer;
    begin
      ServerConn := TWinSockRDOConnectionsServer.Create( aCSPort );
      try
        ModelServerCache.CreateCacheServer( ServerConn, 1, fWorld.WorldLock );
      except
        ServerConn.Free;
        raise;
      end;
    end;

  procedure TModelServer.StartDirectAccessServer( aDAPort : integer );
    begin
      fDAConn     := TWinSockRDOConnectionsServer.Create( aDAPort ); // >> 100
      fDAServ     := TRDOServer.Create( fDAConn as IRDOServerConnection, 7, nil );
      fDALockConn := TWinSockRDOConnectionsServer.Create( aDAPort + 1 ); // >> 20
      fDALockServ := TRDOServer.Create( fDALockConn as IRDOServerConnection, 5, nil );
      fDAServ.RegisterObject(tidRDOHook_World, integer(fWorld));
      fDAConn.StartListening;
      fDALockServ.RegisterObject(tidRDOHook_World, integer(fWorld));
      fDALockConn.StartListening;
      fDALockServ.SetCriticalSection( fWorld.WorldLock );
    end;

  procedure TModelServer.InitInterfaceServerEvents( ServerName : string; ServerPort : integer );
    begin
      fISConn        := TWinSockRDOConnection.Create;
      fISConn.Server := ServerName;
      fISConn.Port   := ServerPort;
      fISEvents      := TRDOObjectProxy.Create as IDispatch;
      if fISConn.Connect( 10000 )
        then
          begin
            fISEvents.SetConnection( fISConn );
            fISEvents.BindTo( tidRDOHook_InterfaceEvents );
            fWorld.OnAreaChanged     := OnAreaChanged;
            fWorld.OnFacilityChanged := OnFacilityChanged;
            fWorld.OnDateChanged     := OnDateChanged;
            fWorld.OnTycoonsChanged  := OnTycoonsChanged;
            fWorld.OnEndOfPeriod     := OnEndOfPeriod;
            fWorld.OnTycoonRetired   := OnTycoonRetired;
          end
        else raise Exception.Create( '' );
    end;

  procedure TModelServer.InitMailServer( ServerName : string; ServerPort : integer; WorldName : string );
    begin
      fMailConn        := TWinSockRDOConnection.Create;
      fMailConn.Server := ServerName;
      fMailConn.Port   := ServerPort;
      fMailServer      := TRDOObjectProxy.Create as IDispatch;
      if fMailConn.Connect( 10000 )
        then
          begin
            fMailServer.SetConnection(fMailConn);
            fMailServer.BindTo(tidRDOHook_MailServer);
            fMailId := fMailServer.RegisterWorld(WorldName);
            if fMailId <> 0
              then
                begin
                  fMailServer.BindTo(fMailId);
                  if fMailServer.SetDAConnection((fMailConn as IRDOConnection).LocalAddress, fDALockPort)
                    then
                      begin
                        fMailServer.BindTo(tidRDOHook_MailServer);
                        fWorld.MailServer := self;
                      end
                    else raise Exception.Create( '' );
                end
              else raise Exception.Create( '' );
          end
        else raise Exception.Create( '' );
    end;

  procedure TModelServer.RegisterModelExtension( filename : string );
    var
      MDX : THandle;
    begin
      MDX := LoadLibrary( pchar(filename) );
      if MDX <> 0
        then
          begin
            RegisterMDX( MDX );
            fExtensionList.Add( filename );
          end
        else raise Exception.Create( 'Cannot load extension file: ' + filename );
    end;

  procedure TModelServer.LoadExtensions;

    type
      PMDXNode = ^TMDXNode;
      TMDXNode =
        record
          Id         : string;
          MDX        : THandle;
          Depends    : TStringList;
          SubNodes   : TCollection;
          registered : boolean;
        end;

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

    function LoadMDXs( filenames : TStringList ) : TCollection;
      var
        i    : integer;
        Node : PMDXNode;
      begin
        result := TCollection.Create( 0, rkUse );
        for i := 0 to pred(filenames.Count) do
          try
            new( Node );
            Node.MDX        := LoadLibrary( pchar(filenames[i] ));
            Node.Id         := GetMDXId( Node.MDX );
            Node.Depends    := GetMDXDependances( Node.MDX );
            Node.SubNodes   := TCollection.Create( 0, rkUse );
            Node.registered := false;
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
          if PMDXNode(MDXNodes[i]).Depends.Count = 0
            then RegisterMDXTree( PMDXNode(MDXNodes[i]) )
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

    var
      MDXNodes : TCollection;
    begin
      MDXNodes := LoadMDXs( GetMDXsFilenames );
      BuildMDXTree( MDXNodes );
      RegisterMDXNodes( MDXNodes );
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
    
  procedure TModelServer.OnAreaChanged( x, y, dx, dy : integer );
    begin
      try
        fISEvents.RefreshArea( x, y, dx, dy );
      except
      end;
    end;

  procedure TModelServer.OnFacilityChanged( Facility : TFacility; FacilityChange : TFacilityChange );
    begin
      try
        fISEvents.RefreshObject( integer(Facility), integer(FacilityChange) );
      except
      end;
    end;

  procedure TModelServer.OnTycoonsChanged;
    begin
      try
        fISEvents.RefreshTycoons( 0 );
      except
      end;
    end;

  procedure TModelServer.OnDateChanged( Date : TDateTime );
    begin
      try
        fISEvents.RefreshDate( Date );
      except
      end;
    end;
    
  procedure TModelServer.OnEndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
    begin
      try
        if PeriodType = perYear
          then fISEvents.EndOfPeriod( 0 );
      except
      end;
    end;

  procedure TModelServer.OnTycoonRetired( name : string );
    begin
      try
        fISEvents.TycoonRetired( name );
      except
      end;
    end;

  procedure TModelServer.StoreBackup( mode : TBackupMode );
    var
      Writer    : IBackupWriter;
      filename  : string;
      MemStream : TStream;
      BckStream : TStream;
    begin
      try
        case mode of
          bmdShutdown :
            begin
              filename := fWorld.Name + '.world';
              Writer := CreateBackup( fBaseDir + filename, false );
              try
                Writer.WriteObject( 'World', fWorld );
              finally
                Writer := nil;
              end;
              filename := fWorld.Name + '.verb';
              Writer := CreateBackup( fBaseDir + filename, true );
              try
                Writer.WriteObject( 'World', fWorld );
              finally
                Writer := nil;
              end;
            end;
          else
            begin
              if fLastBackup < 10
                then inc( fLastBackup )
                else fLastBackup := 1;
              filename := fWorld.Name + '.' + IntToStr( fLastBackup ) + '.back';
              MemStream := CreateBinaryBackup( fWorld );
              try
                // Set the event to start the simulation engine again! uff!!
                // >> fBackupEvent.SetEvent;
                BckStream := TFileStream.Create( fBaseDir + filename, fmCreate );
                try
                  BckStream.CopyFrom( MemStream, MemStream.Size );
                finally
                  // Lets say we've finished the file transfer operation. uff!!
                  // >> fBackupEvent.SetEvent;
                  BckStream.Free;
                end;
              finally
                MemStream.Free;
              end;
            end;
        end;
      except
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
        result := fMailServer.NewMailAccount(fMailId, Account, Alias, Fwdaddr, KeepMsg);
      except
        result := false;
      end;
    end;

  function TModelServer.DeleteAccount(Account : string) : boolean;
    begin
      try
        result := fMailServer.DeleteAccount(fMailId, Account);
      except
        result := false;
      end;
    end;

  function TModelServer.SetForwardRule(Account, FwdAddr : string; KeepMsg : boolean) : boolean;
    begin
      try
        result := fMailServer.SetForwardRule(fMailId, Account, FwdAddr, KeepMsg);
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
        Id := fMailServer.NewMail(From, Dest, Subject);
        if Id <> 0
          then
            begin
              fMailServer.BindTo(Id);
              if html
                then fMailServer.AddHeaders('ContentType=text/html');
              for i := 0 to pred(Lines.Count) do
                fMailServer.AddLine(Lines[i]);
              fMailServer.BindTo(tidRDOHook_MailServer);
              result := fMailServer.Post(fWorld.Name, Id);
              fMailServer.CloseMessage(Id);
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


end.




