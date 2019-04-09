unit World;

{$DEFINE USELogs}
{ $DEFINE DebugPolitics}

interface

  uses
    Classes, Collection, Kernel, Windows, AsxCriticalSections, BackupInterfaces, Accounts,
    Surfaces, Protocol, SyncObjs, MailServerInterfaces, NewsServerInterfaces, LargeMatrix,
    Circuits, MatrixCircuits, Land, TransportInterfaces, Transport, BackupObjects, ActorPool, ActorTypes,
    CacheAgent, Seasons, Graphics, Events, Languages, Persistent, Inventions,
    MediaNameHistory, EconomyRelay, Matrix, Variants;

  type
    TObjectReport = WideString;

  const
    MaxWorldSize = 10000;

  const
    defWorldXSize = 500;
    defWorldYSize = 500;

  const
    RoadTolerance       = 3;
    MaxRoadRefreshDelay = 10;
    MediaReleasePeriod  = 90; //7;

  const
    MaxCompaniesAllowed = 26;
    MaxDaysToPurge      = 10;

  const
    tidURL_SpecialMailMessages = 'Visual/Voyager/Mail/SpecialMessages/';
    tidMailMsgURL_WelcomeMsg   = tidURL_SpecialMailMessages + 'MsgWelcome.asp';
    tidMailMsgURL_ZonedMsg     = tidURL_SpecialMailMessages + 'MsgZoned.asp';

  const
    MaxWipeArea = 100;
    IBBreaker   = high(word);

  type
    PObjectMap = ^TObjectMap;
    PGroundMap = ^TGroundMap;
    PTownMap   = ^TTownMap;
    TObjectMap = array[0..MaxWorldSize*MaxWorldSize] of TObjId;
    TGroundMap = array[0..MaxWorldSize*MaxWorldSize] of TLandVisualClassId;
    TTownMap   = array[0..MaxWorldSize*MaxWorldSize] of byte;

  type
    TOnRegisterIS        = procedure( HostName : string; Port : integer ) of object;
    TOnAreaChanged       = procedure( x, y, dx, dy : integer ) of object;
    TOnFacilityChanged   = procedure( Facility : TFacility; FacilityChange : TFacilityChange ) of object;
    TOnTycoonsChanged    = procedure of object;
    TOnDateChanged       = procedure( Date : TDateTime ) of object;
    TOnSeasonChanged     = procedure( Season : TSeason ) of object;
    TOnEndOfPeriod       = procedure( PeriodType : TPeriodType; PeriodCount : integer ) of object;
    TOnTycoonRetired     = procedure( name : string ) of object;
    TOnSendNotification  = procedure( TycoonId : integer; Kind : integer; Title, Body : string; Options : integer ) of object;
    TOnModelStatusChange = procedure( Status : integer ) of object;

  type
    TPassedPeriods = array[TPeriodType] of integer;

  type
    TTycoonViewport =
      class
        public
          constructor Create( anId : integer );
        private
          fId      : integer;
          fX1, fY1 : integer;
          fX2, fY2 : integer;
        public
          property Id : integer read fId;
          property x1 : integer read fX1 write fX1;
          property y1 : integer read fY1 write fY1;
          property x2 : integer read fX2 write fX2;
          property y2 : integer read fY2 write fY2;
      end;

    TAwakenTycoon =
      class( TObject, IViewer )
        public
          constructor Create( aTycoon : TTycoon );
          destructor  Destroy; override;
        private
          fTycoon    : TTycoon;
          fViewports : TLockableCollection;
        private
          function GetViewport( Id : integer ) : TTycoonViewport;
        public
          property Viewport[Id : integer] : TTycoonViewport read GetViewport;
        // IViewer
        private
          function getId : TViewerId;
          function IsAwareOf( Actor : IServerActor ) : boolean;
        // extra stuff
        private
          fTimesAwaken : integer;
        private
          function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function _AddRef  : integer; stdcall;
          function _Release : integer; stdcall;
      end;

  const
    msgAnswerActorInViewport = 100;

  type
    TZoningMessage =
      class
        public
          constructor Create(aZoned, lang : string); virtual;
          destructor  Destroy; override;
        public
          fZoned      : string;
          fLang       : string;
          fZoners     : TStringList;
          fFacNames   : TStringList;
          fFacCompany : TStringList;
          fFacX       : TStringList;
          fFacY       : TStringList;
          fTime       : TStringList;
      end;

  type
    TViewportAwarenessInfo =
      record
        MsgId      : word;
        Viewport   : TTycoonViewport;
        InViewport : boolean;
      end;

  type
    TPFCoverage =
      record
        Name  : string;
        Ratio : word;
      end;

    TServiceCoverage =
      record
        Name     : string;
        Demand   : TFluidValue;
        Offer    : TFluidValue;
        Capacity : TFluidValue;
        Ratio    : word;
        Quality  : TPercent;
        Price    : TPercent;
      end;

  type
    TWorld = class;

    TWorldExtension =
      class( TLockable )
        public
          function  GetId : string; virtual;
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); virtual;
          procedure Loaded( aWorld : TWorld ); virtual;
        protected
          procedure CompanyCreated ( Company  : TCompany );  virtual;
          procedure TycoonCreated  ( Tycoon   : TTycoon );   virtual;
          procedure FacilityCreated( Facility : TFacility ); virtual;
          procedure CompanyDeleted ( Company  : TCompany );  virtual;
          procedure TycoonDeleted  ( Tycoon   : TTycoon );   virtual;
          procedure FacilityDeleted( Facility : TFacility ); virtual;
      end;

    TCloningOption  = (clonLimitToCompany, clonLimitToTown);
    TCloningOptions =
      record
        Scope   : set of TCloningOption;
        Options : integer;
      end;

    TCacheOption  = (coFacilities, coTycoons, coCompanies, coTowns, coLinks);
    TCacheOptions = set of TCacheOption;

    TSeasonDate =
      record
        day   : byte;
        month : byte;
      end;
    TSeasonDates = array[TSeason] of TSeasonDate;

    {$M+}
    TWorld =
      class( TPersistent, ITimer, IMapRefresh, IWorldLocator, IRoadHandler, IModelFactory, ILandInfo, IMatrix )
        public
          constructor Create( aName : string; axSize : integer; aySize : integer ); virtual;
          destructor  Destroy; override;
        private
          fName           : string;
          fWorldURL       : string;
          fArea           : string;
          fxSize          : integer;
          fySize          : integer;
          fTowns          : TLockableCollection;
          fFacilities     : TLockableCollection;
          fCompanies      : TLockableCollection;
          fTycoons        : TLockableCollection;
          fCircuits       : TLockableCollection;
          fActorPools     : TLockableCollection;
          fSurfacePool    : TSurfacePool;
          fMainBank       : TBank;
          fCargoSystem    : TCargoSystem;
          fAwakenTycoons  : TLockableCollection;
          fMinSalaries    : array[TPeopleKind] of TPercent;
          fForceBackup    : boolean;
          fForceReconnect : boolean;
          fCacheOptions   : TCacheOptions;
          fDirProxy       : OleVariant;
          fDirProxyLock   : TCriticalSection;
          fSimHours       : TTimeDelta;
          fSimDays        : integer;
          fTycoonsToPurge : integer;
          fPurgeIndex     : integer;
          fUpdateNobIndex : integer;
          fUpdateDemoIndex: integer;
          fCloneQueue     : TLockableCollection;
          fBuildLock      : TCriticalSection;
          fNewCompanyLock : TCriticalSection;
          fWorldSuppliers : TLockableCollection;
          fEcconomyRelay  : TEconomyRelay;
          fZoningMessages : TLockableCollection;
          //fTycoonIndex    : integer;
        private
          function  GetWorldName : string;
          function  GetYear      : integer;
          function  GetUserCount : integer;
          procedure SetForceCommand( command : integer );
          function  GetHoursADay : single;
          function  GetNumberOfDays : integer;
        protected
          function GetTotalPopulation : integer; virtual;
        published
          property Name         : string              read fName;
          property xSize        : integer             read fxSize;
          property ySize        : integer             read fySize;
          property Towns        : TLockableCollection read fTowns;
          property Facilities   : TLockableCollection read fFacilities;
          property Companies    : TLockableCollection read fCompanies;
          property Tycoons      : TLockableCollection read fTycoons;
          property Circuits     : TLockableCollection read fCircuits;
          property ZoningMessages: TLockableCollection read fZoningMessages;
          property WorldURL     : string              read fWorldURL write fWorldURL;
          property CurrYear     : integer             read GetYear;
          property TotalPop     : integer             read GetTotalPopulation;
          property UserCount    : integer             read GetUserCount;
          property ForceBackup  : boolean             read fForceBackup    write fForceBackup;
          property ForceCommand : integer             write SetForceCommand;
          property ForceReconnect : boolean           read fForceReconnect write fForceReconnect;
          property CacheOptions : TCacheOptions       read fCacheOptions   write fCacheOptions;
          property DirProxy     : OleVariant          read fDirProxy       write fDirProxy;
          property DirProxyLock : TCriticalSection    read fDirProxyLock   write fDirProxyLock;
          property Area         : string              read fArea           write fArea;
          property HoursADay    : single              read GetHoursADay;
          property TycoonsToPurge : integer           read fTycoonsToPurge write fTycoonsToPurge;
          //property TycoonIndex  : integer           read fTycoonIndex write fTycoonIndex;
        public
          property CargoSystem : TCargoSystem read fCargoSystem;
        private
          function  GetPopulation( kind : TPeopleKind ) : TFluidValue;
          function  GetUnemployment( kind : TPeopleKind ) : TPercent;
          function  GetWealth( kind : TPeopleKind ) : integer;
          function  GetAvgSalary( kind : TPeopleKind ) : TPercent;
          function  GetMinSalary( kind : TPeopleKind ) : TPercent;
          function  GetResDemand( kind : TPeopleKind ) : TFluidValue;
          function  GetResRent( kind : TPeopleKind ) : integer;
          function  GetResQidx( kind : TPeopleKind ) : integer;
          function  GetWorkDemand( kind : TPeopleKind ) : TFluidValue;
          function  GetPrivWorkDemand( kind : TPeopleKind ) : TFluidValue;
          function  GetPFCoverage( Id : string ) : TPFCoverage;
          function  GetServiceInfoById( Id : string ) : TServiceCoverage;
          procedure SetMinSalary( kind : TPeopleKind; value : TPercent );
          function  GetGQOL          : TPercent;
          function  GetGQOS          : TPercent;
          function  GetGeneralWealth : integer;
          function  GetNetFacilities : integer;
        public
          property Population   [kind : TPeopleKind] : TFluidValue read GetPopulation;
          property Unemployment [kind : TPeopleKind] : TPercent    read GetUnemployment;
          property Wealth       [kind : TPeopleKind] : integer     read GetWealth;
          property AvgSalary    [kind : TPeopleKind] : TPercent    read GetAvgSalary;
          property MinSalary    [kind : TPeopleKind] : TPercent    read GetMinSalary write SetMinSalary;
          property WorkDemand   [kind : TPeopleKind] : TFluidValue read GetWorkDemand;
          property PrivWorkDemand   [kind : TPeopleKind] : TFluidValue read GetPrivWorkDemand;
          property ResDemand    [kind : TPeopleKind] : TFluidValue read GetResDemand;
          property ResRent      [kind : TPeopleKind] : integer     read GetResRent;
          property ResQidx      [kind : TPeopleKind] : integer     read GetResQidx;
          property PublicFacInfo[Id : string] : TPFCoverage        read GetPFCoverage;
          property ServiceInfo  [Id : string] : TServiceCoverage   read GetServiceInfoById;
          property GQOL          : TPercent read GetGQOL;
          property GQOS          : TPercent read GetGQOS;
          property GeneralWealth : integer  read GetGeneralWealth;
          property NetFacilities : integer  read GetNetFacilities;
        public
          procedure CheckConnections;
          procedure Simulate;
        private
          procedure NotifyEndOfPeriods;
          procedure UpdateRankingsInDirectory;
          procedure UpdateHoursADay(PeriodType : TPeriodType);
          function  DaysOut(Tycoon : TTycoon) : integer;
          function  GetDaysExpired(Tycoon : TTycoon; def : integer) : integer;
          procedure PurgeTycoons(count : integer);
          procedure UpdateTycoonsNobility(count : integer);
          procedure UpdateTycoonsDemoStatus(count : integer);
          procedure UpdateMailServerDate;
          procedure UpdateContestData;
        protected
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); virtual;
        private
          function GetTycoonByName ( name : string    ) : TTycoon;
          function GetTycoonById   ( Id   : TTycoonId ) : TTycoon;
          function GetCompanyByName( name : string    ) : TCompany;
          function GetClusterByName( name : string    ) : TCluster;
          function GetTownByName   ( name : string    ) : TTown;
          function GetTownById     ( Id   : TTownId   ) : TTown;
          function GetCircuitById  ( Id   : integer   ) : TCircuitMap;
          function GetAwakenTycoon ( Id   : TTycoonId ) : TAwakenTycoon;
          function GetTycoonCollection : TLockableCollection;
        public
          property TycoonByName  [name : string]    : TTycoon     read GetTycoonByName;
          property TycoonById    [Id   : TTycoonId] : TTycoon     read GetTycoonById;
          property CompanyByName [name : string]    : TCompany    read GetCompanyByName;
          property ClusterbyName [name : string]    : TCluster    read GetClusterByName;
          property TownByName    [name : string]    : TTown       read GetTownByName;
          property TownById      [Id   : TTownId]   : TTown       read GetTownById;
          property CircuitById   [Id   : integer]   : TCircuitMap read GetCircuitById;
        public
          function  NewFacility( FacilityId : string; CompanyId : TCompanyId; x, y : integer; out Facility : TFacility ) : TErrorCode;
          function  DelFacility( x, y : integer ) : TErrorCode;
          function  FacilityAt ( x, y : integer ) : TFacility;
          function  GetCompany ( CompanyId : TCompanyId ) : TCompany;
          function  GetObjectsInArea( x, y, dx, dy : integer ) : TObjectReport;
          procedure CloneFacility( Source : TFacility; Options : TCloningOptions );
          procedure InitClusters;
          procedure InsertTown( Town : TTown ); virtual;
          function  FacilitiesIn( x1, y1, x2, y2 : integer) : boolean;
        public
          function  FacAllowsEffect( x, y : integer; FxId : word; FxCreator : string ) : boolean;
          function  CircuitAllowsEffect( CircId : TCircuitId; x1, y1, x2, y2 : integer; FxId : word; FxStrength : single; FxCreator : string ) : boolean;
          function  TycoonAllowsEffect( TycoonName : widestring; FxId : word; FxCreator : string ) : boolean;
          function  CreateFacEffect( x, y : integer; FxId : word; FxStrength : single; FxCreator : string ) : TErrorCode;
          function  CreateCircuitEffect( CircId : TCircuitId; x1, y1, x2, y2 : integer; FxId : word; FxStrength : single; FxCreator : string ) : TErrorCode;
          function  CreateTycoonEffect( TycoonName : widestring; FxId : word; FxStrength : single; FxCreator : string ) : TErrorCode;
        published
          procedure RDORegisterIS( HostName : widestring; Port : integer );
          function  RDONewFacility( FacilityId : widestring; CompanyId : integer; x, y : integer ) : OleVariant;
          function  RDODelFacility( x, y : integer ) : OleVariant;
          function  RDODelFacilityOfCompany(x, y : integer; name : widestring) : OleVariant;
          function  RDOFacilityAt ( x, y : integer ) : OleVariant;
          function  RDOBlockAt ( x, y : integer ) : OleVariant;
          function  RDOGetCompany ( CompanyId : integer ) : OleVariant;
          function  RDOGetObjectsInArea( x, y, dx, dy : integer ) : OleVariant;
          function  RDOGetFacilityConnections( Facility : TFacility ) : OleVariant;
          procedure RDOFacilityGotFocus ( Facility : TFacility );
          procedure RDOFacilityLostFocus( Facility : TFacility );
          function  RDOConnectFacilities( Owner : TTycoon; Facility1, Facility2 : TFacility ) : OleVariant;
          function  RDOAccountStatus( name, password : widestring ) : OleVariant;
          function  RDONewTycoon( name, password : widestring ) : OleVariant;
          function  RDOGetTycoon( name, password : widestring ) : OleVariant;
          function  RDODelTycoon( name, password : widestring ) : OleVariant;
          function  RDOResetTycoon( name : widestring ) : OleVariant;
          function  RDOResetTycoonEx( name, password : widestring ) : OleVariant;
          procedure RDOCacheTycoonLinks(name : widestring);
          procedure RDOCacheCompanyLinks(name : widestring);
          function  RDODelCompany( name : widestring ) : OleVariant;
          function  RDOGetRidOfCompany(cpnName, tycoonName, password : widestring ) : OleVariant;
          function  RDOGetCompanyList( username : widestring ) : OleVariant;
          function  RDOGetCompanyOwnerRole( username : widestring; index : integer ) : OleVariant;
          function  RDOGetCompanyName( username : widestring; index : integer ) : OleVariant;
          function  RDOGetCompanyCluster( username : widestring; index : integer ) : OleVariant;
          function  RDOGetCompanyId( username : widestring; index : integer ) : OleVariant;
          function  RDOGetCompanyFacilityCount( username : widestring; index : integer ) : OleVariant;
          function  RDOGetCompanyProfit( username : widestring; index : integer ) : OleVariant;
          function  RDOGetCompanyCount( username : widestring ) : OleVariant;
          function  RDONewCompany( username, name, clustername : widestring ) : OleVariant;
          function  RDOObjectStatusText( kind : TStatusKind; Id : TObjId; ToTycoon : TObjId ) : OleVariant;
          function  RDOAllObjectStatusText( Id : TObjId; ToTycoon : TObjId ) : OleVariant;
          function  RDOContextStatusText( ToTycoon : TObjId; x, y : integer ) : OleVariant;
          function  RDOCreateCircuitSeg( CircuitId, TycoonId, x1, y1, x2, y2, cost : integer ) : OleVariant;
          function  RDOBreakCircuitAt( CircuitId, TycoonId, x, y : integer ) : OleVariant;
          function  RDOWipeCircuit( CircuitId, TycoonId, x1, y1, x2, y2 : integer ) : OleVariant;
          function  RDOSegmentsInArea( CircuitId, x1, y1, x2, y2 : integer ) : OleVariant;
          function  RDOWhoDidRoadAt(x, y : integer) : OleVariant;
          function  RDOGetSurface( SurfaceId : widestring; x1, y1, x2, y2 : integer ) : OleVariant;
          function  RDODefineZone( TycoonId, ZoneId, x1, y1, x2, y2 : integer ) : OleVariant;
          procedure RDOAwakeTycoon( TycoonId : integer );
          procedure RDOSleepTycoon( TycoonId : integer );
          procedure RDOSetTycoonViewport( TycoonId, ViewportId, x1, y1, x2, y2 : integer );
          function  RDOGetTycoonCookies(TycoonId : integer) : OleVariant;
          function  RDOGetTycoonCookie( TycoonId : integer; CookieId : widestring ) : OleVariant;
          procedure RDOSetTycoonCookie( TycoonId : integer; CookieId, CookieValue : widestring );
          procedure RDOCloneFacility(x, y, options, useless, TycoonId : integer);
          function  RDOGetNearestTownHall( x, y : integer ) : OleVariant;
          function  RDOPickEvent( TycoonId : integer ) : OleVariant;
          function  RDOAssignLevel(tycoonName, sysPassword, Level : widestring) : OleVariant;
          function  RDOFireMayor(townName, sysPassword : widestring; reelect : integer) : OleVariant;
          function  RDOCreateFacEffect( x, y : integer; FxId : word; FxStrength : single; FxCreator : widestring ) : OleVariant;
          function  RDOCreateCircuitEffect( CircId : TCircuitId; x1, y1, x2, y2 : integer; FxId : word; FxStrength : single; FxCreator : widestring ) : OleVariant;
          function  RDOCreateTycoonEffect( TycoonName : widestring; FxId : word; FxStrength : single; FxCreator : widestring ) : OleVariant;
          function  RDOAbandonRoles(name, password : widestring) : OleVariant;
          procedure RDOUpdateCurriculumItems( AllItems : wordbool );
          procedure RDOGenerateCustomLog(LogId : integer);
          procedure RDOSendEvent(msg, url : widestring);
        published
          procedure RDOLogonClient(name, password : widestring);
          procedure RDOOptimizeRoads(password : widestring);
          procedure RDOForceElections(password : widestring);
          procedure RDOResetTournament(password : widestring);
          procedure RDOAddCurrItem(tycoon, desc : widestring; prest : integer);
        private
          function LandSize : TPoint;
          function LandVisualClassAt( x, y : integer ) : TLandVisualClassId;
          function LandClassAt( x, y : integer ) : TLandClass;
          function LandTypeAt( x, y : integer ) : TLandType;
        public
          procedure LoadLandInfo( filename : string );
        private
          fMailServer : IMailServer;
          fNewsServer : INewsServer;
        private
          procedure SetMailServer( aMailServer : IMailServer );
        public
          property MailServer : IMailServer read fMailServer write SetMailServer;
          property NewsServer : INewsServer read fNewsServer write fNewsServer;
        private
          function  GetMessagesPath : string;
          function  GetWorldURL     : string;
          function  GetMainBank     : TBank;
          procedure SendNotification( TycoonId : integer; Kind : integer; Title, Body : string; Options : integer );
        protected
          function  GetMainDealer : TMoneyDealer; virtual; abstract;
        public
          procedure SendEvent( Event : TEvent );
          procedure SearchForSuppliers( Block : TBlock ); virtual;
          procedure AddCurriculumItem( Tycoon : TTycoon; Item : TCurriculumItem );
        private
          fVirtHour         : integer;
          fVirtDay          : integer;
          fVirtMonth        : integer;
          fVirtYear         : integer;
          fAbsDate          : TVirtDateAbs;
          fYearCount        : integer;
          fPeriods          : TPassedPeriods;
          fYearsToElections : integer;
          fSeason           : TSeason;
          fSeasonProgress   : integer;
          fTick             : integer;
          fTimeTick         : integer;
        public
          procedure VirtualTimeTick( TimeElapsed : TVirtDateAbs );
          procedure ForceDt(aDt : TVirtDateAbs);
        private
          function GetVirtualTime    : TDateTime;
          function GetVirtualTimeAbs : TVirtDateAbs;
          function dt                : TTimeDelta;
          function GetSeason         : TSeason;
          function GetSeasonProgress : single;
          function getTickId         : integer;
        private
          fTimeStopped : integer;
          fTimeLock    : TCriticalSection;
        public
          procedure StopVirtualTime;
          procedure ResumeVirtualTime;
          function  TimeStopped : boolean;
        private
          function GetSeasonInt : integer;
        published
          property VirtualTime      : TDateTime    read GetVirtualTime;
          property VirtualTimeAbs   : TVirtDateAbs read GetVirtualTimeAbs;
          property YearsToElections : integer      read fYearsToElections;
          property Season           : integer      read GetSeasonInt;
        private
          fObjectMap        : PObjectMap;
          fGroundMap        : PGroundMap;
          fTownMap          : PTownMap;
          fRoads            : TCircuitMap;
          fRoadRefreshDelay : integer;
          fMediaReleaseDays : integer;
        public
          function OnAuthorizeBreak( x, y : integer; OwnerId, BreakerId : TOwnerId ) : boolean;
          function OnRenderExtraSegInfo( Segment : TSegment ) : string;
        public
          function  GetObjectMap( x, y : integer ) : TObjId;
          function  GetGroundMap( x, y : integer ) : TLandVisualClassId;
          procedure SetObjectMap( x, y : integer; Item : TObjId );
          procedure SetGroundMap( x, y : integer; Item : TLandVisualClassId );
          function  GetTownMap( x, y : integer ) : byte;
        public
          property ObjectMap[x, y : integer] : TObjId             read GetObjectMap write SetObjectMap;
          property GroundMap[x, y : integer] : TLandVisualClassId read GetGroundMap write SetGroundMap;
          property TownMap[x, y : integer]   : byte               read GetTownMap;
        public
          function  FacAllowedInClear(MetaFacility : TMetaFacility; x, y : integer; Company : TCompany ) : boolean;                        virtual;
          function  AreaIsClear( x, y, dx, dy : integer; Company : TCompany ) : boolean;                        virtual;
          function  LandClassFound( x1, y1, x2, y2 : integer; LandClass : TLandClass ) : boolean;               virtual;
          function  MatchesZone(x, y, dx, dy : integer; ZoneType : TZoneType) : boolean;                        virtual;
          function  AreaIsZoned(x, y, dx, dy : integer) : boolean;                                              virtual;
          procedure InitTownMap( TownMapFile : string );
        protected
          function ZoneMatches( ZoneA, ZoneB : TZoneType ) : boolean; virtual;
        protected
          procedure UpdateInMap   ( Facility : TFacility );
          procedure RemoveFromMap ( Facility : TFacility );
          procedure InsertFacility( Facility : TFacility );
        public
          function NearestTown( x, y : integer ) : TTown;
        private
          csMapLock  : TCriticalsection; //TAsymetrixCriticalSection;
          //csFacilityList : TRTLCriticalSection;
        private
          fOnRegisterIS        : TOnRegisterIS;
          fOnAreaChanged       : TOnAreaChanged;
          fOnFacilityChanged   : TOnFacilityChanged;
          fOnTycoonsChanged    : TOnTycoonsChanged;
          fOnDateChanged       : TOnDateChanged;
          fOnSeasonChanged     : TOnSeasonChanged;
          fOnEndOfPeriod       : TOnEndOfPeriod;
          fOnTycoonRetired     : TOnTycoonRetired;
          fOnPoolActed         : TOnSendTickData;
          fOnSendNotification  : TOnSendNotification;
          fOnModelStatusChange : TOnModelStatusChange;
          fLastUpdate          : TVirtDateAbs;
          fLastSimulation      : TVirtDateAbs;
          fLastCompId          : TCompanyId;
          fLastTycoonId        : TTycoonId;
          fdt                  : single;
        public
          property OnRegisterIS        : TOnRegisterIS        write fOnRegisterIS;
          property OnAreaChanged       : TOnAreaChanged       write fOnAreaChanged;
          property OnFacilityChanged   : TOnFacilityChanged   write fOnFacilityChanged;
          property OnTycoonsChanged    : TOnTycoonsChanged    write fOnTycoonsChanged;
          property OnDateChanged       : TOnDateChanged       write fOnDateChanged;
          property OnSeasonChanged     : TOnSeasonChanged     write fOnSeasonChanged;
          property OnEndOfPeriod       : TOnEndOfPeriod       write fOnEndOfPeriod;
          property OnTycoonRetired     : TOnTycoonRetired     write fOnTycoonRetired;
          property OnPoolActed         : TOnSendTickData      write fOnPoolActed;
          property OnSendNotification  : TOnSendNotification  write fOnSendNotification;
          property OnModelStatusChange : TOnModelStatusChange write fOnModelStatusChange;
        protected
          procedure RefreshArea( x, y, dx, dy : integer );
          procedure RefeshFacility( Facility : TFacility; FacilityChange : TFacilityChange );
        private
          procedure NearestCircuitsToArea( CircuitId : TCircuitId; Area : TRect; var Circuits : TCollection );
          function  AreasAreConnected( CircuitId : TCircuitId; Area1, Area2 : TRect ) : boolean;
          function  GetCargoSystem( CircuitId : TCircuitId ) : ICargoSystem;
          procedure InitCargoSystem; virtual;
        protected
          function NewCompany( name : string; Cluster : TCluster; Tycoon : TTycoon ) : TCompany;
          function NewTycoon( name, password : string ) : TTycoon;
          function NewTycoonRole( RoleId, name : string; TycoonClass : CTycoon; InitialBudget : TMoney ) : TTycoon;
          function NewNewspaper( Name, Style, Town : string ) : boolean;
        private
          fNewMeat          : TLockableCollection;
          fDeadMeat         : TLockableCollection;
          fDeadTycoons      : TLockableCollection;
          fDeadCompanies    : TLockableCollection;
          fPendingDeletions : TLockableCollection;
        public
          procedure DeleteTycoon( Tycoon : TTycoon; erase : boolean );
          procedure RequestDeleteTycoon( Tycoon : TTycoon; erase : boolean );
          procedure ResetTycoon( Tycoon : TTycoon );
          procedure DeleteCompany( Company : TCompany );
          procedure RequestDeleteCompany( Company : TCompany );
          procedure DeleteFacility ( Facility : TFacility );
          procedure RequestDeletion( Facility : TFacility );
        protected
          procedure CompanyCreated ( Company  : TCompany );  virtual;
          procedure TycoonCreated  ( Tycoon   : TTycoon );   virtual;
          procedure FacilityCreated( Facility : TFacility ); virtual;
          procedure CompanyDeleted ( Company  : TCompany );  virtual;
          procedure TycoonDeleted  ( Tycoon   : TTycoon );   virtual;
          procedure FacilityDeleted( Facility : TFacility ); virtual;
        private
          fZones : TByteLargeMatrix;
        private
          fWorldLock : TCriticalSection;
        public
          property WorldLock : TCriticalSection read fWorldLock;
        private
          procedure SynchronizeClock;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure Loaded( Notify : TBackupReaderNotify ); virtual;
          procedure TimeWarp;
        private
          procedure SendActorPoolData( PoolId : TActorPoolId; ViewerId : TViewerId; TickCount : cardinal; TickData : TStream );
        public
          procedure RegisterActorPool  ( ActorPool : TServerActorPool );
          procedure UnregisterActorPool( ActorPool : TServerActorPool );
        private
          function GetActorPool( Id : TActorPoolId ) : TServerActorPool;
        public
          property ActorPoolById[Id : TActorPoolId] : TServerActorPool read GetActorPool;
        private
          function YearsWithoutConnecting( Tycoon : TTycoon ) : integer;
        private
          fWorldExtensions : TCollection;
        private
          function GetWorldExtension( Id : string ) : TWorldExtension;
        public
          property WorldExtension[Id : string] : TWorldExtension read GetWorldExtension;
          property WorldExtensions : TCollection read fWorldExtensions;
        public
          procedure RegisterWorldExtension( Extension : TWorldExtension );
        private
          fLandBitmap : TBitmap;
        public
          function GenerateSatelliteMap( kind : integer ) : TBitmap;
        protected
          function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function _AddRef  : integer; stdcall;
          function _Release : integer; stdcall;
        private
          procedure DSLock;
          procedure DSUnlock;
        protected
          procedure QueueFacilityToClone(aFac : TFacility; anOptions : TCloningOptions);
          procedure RemoveFacilityToClone(aFac : TFacility);
          procedure CloneFacilities;
        private
          fCacheLogVer : integer;
        public
          property CacheLogVer : integer read fCacheLogVer write fCacheLogVer;
        private
          fWaterQuestInv : TInvention;
        private
          fInvestorCount : integer;
          fVisitorCount  : integer;
          fMaxInvestors  : integer;
        published
          property InvestorCount : integer read fInvestorCount;
          property VisitorCount  : integer read fVisitorCount;
          property MaxInvestors  : integer read fMaxInvestors write fMaxInvestors;
        private
          fInputTotal  : integer;
          fOutputTotal : integer;
          fInCnnts     : integer;
          fOutCnnts    : integer;
          fMaxIn       : integer;
          fMaxOut      : integer;
          fSimReport   : string;
        published
          property SimReport : string read fSimReport;
        private
          procedure ClearSimVars;
          procedure CountInputConnections(count : integer);
          procedure CountOutputConnections(count : integer);
        private
          fNotify      : TBackupReaderNotify;
          fSplitBackup : boolean;
        public
          property SplitBackup : boolean read fSplitBackup write fSplitBackup;
        private
          fMediaNameHistory : TMediaNameHistory;
        public
          function UpdateNobility(Tycoon : TTycoon) : boolean;
        public
          procedure RegisterTitle(name : string);
          function  NumberTitle(name, sep : string) : string;

        //Tournament
        private
          fTournamentLength    : integer;
          fTornamentStart      : integer;
          fElectionsOn         : boolean;
          fSendingMoneyOn      : boolean;
          fAlliesPageOn        : boolean;
          fTranscendingOn      : boolean;
        public
          property TournamentLength : integer read fTournamentLength;
          property TornamentStart   : integer read fTornamentStart;
          property ElectionsOn      : boolean read fElectionsOn;
          property SendingMoneyOn   : boolean read fSendingMoneyOn;
          property AlliesPageOn     : boolean read fAlliesPageOn;
          property TranscendingOn   : boolean read fTranscendingOn;

        private
          fFightFacColonies : boolean;
          fRandomSeasons    : boolean;
          fEndSeasonDates   : TSeasonDates;
        protected
          procedure InitSeasonDates;
          procedure GenerateSeasonDates;
        private
          fBldMagnaIfNoble   : boolean;
          fResetPrestigeLost : integer;
        published
          function GetConfigParm(name, def : widestring) : olevariant;
          procedure SendZoningMessages(number : integer);
        private
          fZoneMessSent : integer;
        published
          property ZoneMessSent : integer read fZoneMessSent;
        private
          function IsDemoAccount(tycoon : string) : boolean;
        private
          fLandSquareCost : integer;
        private
          function GetLandSquareCost : integer;
          function MinLevelToBuildAdvanced : integer;
        protected
          procedure ClearTycoonVotes;
          procedure ReportWithdrawal(Loc, Tycoon : TObject);
          procedure SendEmail(from, rcpt, subject, url : string);
        public
          procedure ReportCapitolMoved(OldLoc, NewLoc : TObject);
        private
          procedure ReportZoning(Tycoon : TTycoon; Fac : TFacility);
          procedure InitTownRoadCounter;
          procedure ReportRoadBlock(x, y : integer);
        public
          procedure InvalidateRoads;
        private
          // Matrix implementation for Town Maps
          function  getCols : integer;
          function  getRows : integer;
          procedure setDimensions( n, m : integer );
          function  getElement( i, j : integer ) : single;
          procedure setElement( i, j : integer; value : single );
        public
          procedure SleepTycoons;
        published
          function GetTownNames : OleVariant;
        public
          function ReportMaintenance(shortMsg, longMsg : string; eta : TDateTime) : boolean;
        published
          procedure RDOForceRole(roleName, tycoonName, password : widestring; assume : wordbool);
        private
          procedure RedeemBank(Bank : TBank);
      end;
    {$M-}

  const
    MaxYearsWithoutConnecting = 100;


  procedure RegisterBackup;

  // World level MDX

  const
    tidProcName_RegisterWorldMDX  = 'RegisterWorldExtension';

  type
    PRegisterWorldMDXProc = ^TRegisterWorldMDXProc;
    TRegisterWorldMDXProc = procedure( World : TWorld; WorldLoaded : boolean );

  procedure RegisterWorldMDX( MDX : THandle; World : TWorld; WorldLoaded : boolean );

implementation

  uses
    SysUtils, ClassStorage, ClassStorageInt, MathUtils, RDOUtils, VisualClassesData, SimHints, Effects,
    ModelServerCache, MapCompress, BasicAccounts, MatrixLayer, BasicCurriculum, Politics, BasicEvents,
    Logs, Population, ServiceInfo, CacheCommon, Rankings, StdRankings, Profiler, TycoonLevels,
    RankProtocol, DirectoryServerProtocol, StdCurriculum, {RoadLogs,} MetaInstances,
    BackupThread, TownPolitics, LoggedUserData, TranscendBlock, ReachMatrix, FavProtocol,
    CloneOptions;

  const
    UpdateFreq = 2;
    MaxFailure = 7;

  const
    tidUltraSecretPassword = 'este socio se va del aire';
    tidSystemPassword      = 'k@$tr@c0s@';

  const
    SeasonIntro = 10;

  // TCloneFacilityInfo

  type
    TCloneFacilityInfo =
      class
        public
          constructor Create(aFac : TFacility; anOptions : TCloningOptions);
        private
          fFacility : TFacility;
          fOptions  : TCloningOptions;
      end;

  constructor TCloneFacilityInfo.Create(aFac : TFacility; anOptions : TCloningOptions);
    begin
      inherited Create;
      fFacility := aFac;
      fOptions  := anOptions;
    end;


  // TTycoonViewport

  constructor TTycoonViewport.Create( anId : integer );
    begin
      inherited Create;
      fId := anId;
    end;

  // TZoningMessage

  constructor TZoningMessage.Create(aZoned, lang : string);
    begin
      inherited Create;
      fZoned := aZoned;
      fLang  := lang;
      fZoners := TStringList.Create;
      fFacNames := TStringList.Create;
      fFacCompany := TStringList.Create;
      fFacX := TStringList.Create;
      fFacY := TStringList.Create;
      fTime := TStringList.Create;
    end;

  destructor  TZoningMessage.Destroy;
    begin
      fZoners.Free;
      fFacNames.Free;
      fFacCompany.Free;
      fFacX.Free;
      fFacY.Free;
      fTime.Free;
    end;

  // TAwakenTycoon

  constructor TAwakenTycoon.Create( aTycoon : TTycoon );
    begin
      inherited Create;
      fTycoon := aTycoon;
      fViewports := TLockableCollection.Create( 0, rkBelonguer );
    end;

  destructor TAwakenTycoon.Destroy;
    begin
      fViewports.Free;
      inherited;
    end;

  function TAwakenTycoon.GetViewport( Id : integer ) : TTycoonViewport;
    var
      i : integer;
    begin
      fViewports.Lock;
      try
        i := 0;
        while (i < fViewports.Count) and (TTycoonViewport(fViewports[i]).Id <> Id) do
          inc( i );
        if i < fViewports.Count
          then result := TTycoonViewport(fViewports[i])
          else
            begin
              result := TTycoonViewport.Create( Id );
              fViewports.Insert( result );
            end
      finally
        fViewports.Unlock;
      end;
    end;

  function TAwakenTycoon.getId : TViewerId;
    begin
      result := fTycoon.Id;
    end;

  function TAwakenTycoon.IsAwareOf( Actor : IServerActor ) : boolean;

    function ActorInViewport( Viewport : TTycoonViewport ) : boolean;
      var
        ViewportAwarenessInfo : TViewportAwarenessInfo;
      begin
        try
          if Actor.getContext <> nil
            then
              begin
                ViewportAwarenessInfo.MsgId      := msgAnswerActorInViewport;
                ViewportAwarenessInfo.Viewport   := Viewport;
                ViewportAwarenessInfo.InViewport := false;
                TObject(Actor.getContext).Dispatch( ViewportAwarenessInfo );
                result := ViewportAwarenessInfo.InViewport;
              end
            else result := false;
        except
          result := false;
        end;
      end;

    var
      i : integer;
    begin
      fViewports.Lock;
      try
        i := 0;
        while (i < fViewports.Count) and not ActorInViewport( TTycoonViewport(fViewports[i]) ) do
          inc( i );
        result := i < fViewports.Count;
      finally
        fViewports.Unlock;
      end;
    end;

  function TAwakenTycoon.QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TAwakenTycoon._AddRef  : integer; stdcall;
    begin
      result := 1;
    end;

  function TAwakenTycoon._Release : integer; stdcall;
    begin
      result := 1;
    end;


  // TWorldExtension

  function TWorldExtension.GetId : string;
    begin
      result := '';
    end;

  procedure TWorldExtension.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
    begin
    end;

  procedure TWorldExtension.Loaded( aWorld : TWorld );
    begin
    end;
    
  procedure TWorldExtension.CompanyCreated( Company : TCompany );
    begin
    end;

  procedure TWorldExtension.TycoonCreated( Tycoon : TTycoon );
    begin
    end;

  procedure TWorldExtension.FacilityCreated( Facility : TFacility );
    begin
    end;

  procedure TWorldExtension.CompanyDeleted( Company  : TCompany );
    begin
    end;

  procedure TWorldExtension.TycoonDeleted( Tycoon : TTycoon );
    begin
    end;

  procedure TWorldExtension.FacilityDeleted( Facility : TFacility );
    begin
    end;


  // TWorld

  constructor TWorld.Create( aName : string; axSize : integer; aySize : integer );

    procedure InitRankings;
      var
        i     : integer;
        count : integer;
      begin
        count := TheClassStorage.ClassCount[tidClassFamily_Rankings];
        for i := 0 to pred(count) do
          CacheObject( TheClassStorage.ClassByIdx[tidClassFamily_Rankings, i], noKind, noInfo );
      end;

    begin
      inherited Create;
      csMapLock  := TCriticalSection.Create; // TAsymetrixCriticalSection.Create;
      fWorldLock := TCriticalSection.Create;
      fTimeLock  := TCriticalSection.Create;
      //InitializeCriticalSection( csFacilityList );
      fDt    := 1;
      fName  := aName;
      fxSize := axSize;
      fySize := aySize;
      getmem( fObjectMap, xSize*ySize*sizeof(TObjId));
      getmem( fGroundMap, xSize*ySize*sizeof(TLandVisualClassId));
      fillchar( fObjectMap^, xSize*ySize*sizeof(TObjId), 0 );
      fillchar( fGroundMap^, xSize*ySize*sizeof(TLandVisualClassId), 0 );
      fTowns         := TLockableCollection.Create( 0, rkBelonguer );
      fCompanies     := TLockableCollection.Create( 0, rkBelonguer );
      fFacilities    := TLockableCollection.Create( 0, rkBelonguer );
      fTycoons       := TLockableCollection.Create( 0, rkBelonguer );
      fAwakenTycoons := TLockableCollection.Create( 0, rkBelonguer );
      fActorPools    := TLockableCollection.Create( 0, rkUse );
      fCircuits      := TLockableCollection.Create( 0, rkBelonguer );
      fNewMeat       := TLockableCollection.Create( 0, rkBelonguer );
      fDeadMeat      := TLockableCollection.Create( 0, rkBelonguer );
      fDeadTycoons   := TLockableCollection.Create( 0, rkBelonguer );
      fDeadCompanies := TLockableCollection.Create( 0, rkBelonguer );
      fZoningMessages:= TLockableCollection.Create( 0, rkBelonguer );
      fZoneMessSent  := 0;
      //fTycoonIndex   := 0;
      fPendingDeletions := TLockableCollection.Create( 0, rkUse );
      fWorldExtensions  := TCollection.Create( 0, rkBelonguer );
      fVirtDay       := 9;
      fVirtMonth     := 3;
      fVirtYear      := YearZero;
      fSurfacePool   := TSurfacePool(TheClassStorage.ClassById[tidClassFamily_SurfacePools, tidSurfacePool_Surfaces]);
      fRoads         := TMatrixCircuitMap.Create( cirRoads, TNode, TSegment, TCircuit );
      fRoads.SetSize( xSize, ySize );
      fRoads.OnAuthorizeBreak := OnAuthorizeBreak;
      fRoads.OnRenderExtraSegInfo := OnRenderExtraSegInfo;
      fCircuits.Insert( fRoads );
      fMainBank := TBank.Create( nil );
      fMainBank.Name  := 'Bank of IFEL';
      fMainBank.Timer   := self;
      fMainBank.Locator := self;
      InitCargoSystem;
      fZones := TByteLargeMatrix.Create( ySize, xSize, 32 );
      InitRankings;
      fCloneQueue := TLockableCollection.Create(0, rkBelonguer);
      fBuildLock := TCriticalSection.Create;
      fNewCompanyLock := TCriticalSection.Create;
      fWorldSuppliers := TLockableCollection.Create(0, rkUse);
      fSplitBackup    := true;
      fMediaNameHistory := TMediaNameHistory.Create;
      InitSeasonDates;
      fRandomSeasons    := TheGlobalConfigHandler.GetConfigParm('RamdomSeasons', '0') = '1';
      if TheGlobalConfigHandler.GetConfigParm('RelayEconomy', '0') = '1'
        then fEcconomyRelay := TEconomyRelay.Create
        else fEcconomyRelay := nil;
    end;

  destructor TWorld.Destroy;
    begin
      fZones.Free;
      csMapLock.Enter; //BeginWrite( INFINITE );
      try
        freemem( fObjectMap, xSize*ySize*sizeof(fObjectMap[0]));
        freemem( fGroundMap, xSize*ySize*sizeof(fGroundMap[0]));
        fCargoSystem.Free;
        fMainBank.Free;
        fTycoons.Free;
        fAwakenTycoons.Free;
        fActorPools.Free;
        fCircuits.Free;
        fCompanies.Free;
        fFacilities.Free;
        fNewMeat.Free;
        fDeadMeat.Free;
        fDeadTycoons.Free;
        fDeadCompanies.Free;
        fPendingDeletions.Free;
        fWorldExtensions.Free;
        fLandBitmap.Free;
        fTowns.Free;
        fWorldSuppliers.Free;
        fZoningMessages.Free;
        fBuildLock.Free;
        fNewCompanyLock.Free;
      finally
        csMapLock.Leave; //EndWrite;
        csMapLock.Free;
      end;
      fWorldLock.Free;
      fCloneQueue.Free;
      //DeleteCriticalSection( csFacilityList );
      inherited;
    end;

  function TWorld.GetWorldName : string;
    begin
      result := Name;
    end;

  function TWorld.GetYear : integer;
    begin
      result := fVirtYear;
    end;

  function TWorld.GetUserCount : integer;
    begin
      result := Tycoons.Count;
    end;

  procedure TWorld.SetForceCommand( command : integer );
    begin
      case command of
        0 : ForceBackup    := true;
        1 : ForceReconnect := true;
        2 : UpdateRankingsInDirectory;
      end;
    end;

  function TWorld.GetHoursADay : single;
    begin
      if (fSimHours = 0) or (fSimDays < 2)
        then result := 24
        else result := realmin(24, realmax(4, fSimHours/fSimDays));
    end;

  function TWorld.GetNumberOfDays : integer;
    const                            //jan feb mar apr may jun jul aug sep oct nov dec
      months : array[1..12] of byte = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
    var
      i : integer;
    begin
      result := fVirtDay;
      for i := 1 to pred(fVirtMonth) do
        inc(result, months[i]);
    end;

  function TWorld.GetTotalPopulation : integer;
    begin
      result := 0;
    end;

  function TWorld.GetPopulation( kind : TPeopleKind ) : TFluidValue;
    var
      i : integer;
    begin
      result := 0;
      Towns.Lock;
      try
        for i := 0 to pred(Towns.Count) do
          result := result + TTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock).Population[kind];
      finally
        Towns.Unlock;
      end;
    end;

  function TWorld.GetUnemployment( kind : TPeopleKind ) : TPercent;
    var
      sum   : double;
      count : double;
      i     : integer;
      TH    : TTownHall;
    begin
      sum   := 0;
      count := 0;
      Towns.Lock;
      try
        for i := 0 to pred(Towns.Count) do
          begin
            TH    := TTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock);
            sum   := sum + TH.Population[kind]*TH.Unemployment[kind];
            count := count + TH.Population[kind];
          end;
        if count > 0
          then result := round(100*sum/count)
          else result := 0;
      finally
        Towns.Unlock;
      end;
    end;

  function TWorld.GetWealth( kind : TPeopleKind ) : integer;
    var
      sum   : double;
      count : double;
      i     : integer;
      TH    : TTownHall;
    begin
      sum   := 0;
      count := 0;
      Towns.Lock;
      try
        for i := 0 to pred(Towns.Count) do
          begin
            TH    := TTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock);
            sum   := sum + TH.Population[kind]*TH.Wealth[kind];
            count := count + TH.Population[kind];
          end;
        if count > 0
          then result := round(100*sum/count)
          else result := 0;
      finally
        Towns.Unlock;
      end;
    end;

  function TWorld.GetAvgSalary( kind : TPeopleKind ) : TPercent;
    var
      sum   : double;
      count : double;
      i     : integer;
      TH    : TTownHall;
    begin
      sum   := 0;
      count := 0;
      Towns.Lock;
      try
        for i := 0 to pred(Towns.Count) do
          begin
            TH    := TTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock);
            sum   := sum + TH.Population[kind]*TH.Salary[kind];
            count := count + TH.Population[kind];
          end;
        if count > 0
          then result := round(100*sum/count)
          else result := 0;
      finally
        Towns.Unlock;
      end;
    end;

  function TWorld.GetMinSalary( kind : TPeopleKind ) : TPercent;
    begin
      result := fMinSalaries[kind];
    end;

  function TWorld.GetResDemand( kind : TPeopleKind ) : TFluidValue;
    var
      i  : integer;
      TH : TTownHall;
    begin
      result := 0;
      Towns.Lock;
      try
        for i := 0 to pred(Towns.Count) do
          begin
            TH     := TTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock);
            result := result + TH.ResDemand[kind];
          end;
      finally
        Towns.Unlock;
      end;
    end;

  function TWorld.GetResRent( kind : TPeopleKind ) : integer;
    var
      sum   : double;
      count : double;
      i     : integer;
      TH    : TTownHall;
    begin
      sum   := 0;
      count := 0;
      Towns.Lock;
      try
        for i := 0 to pred(Towns.Count) do
          begin
            TH    := TTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock);
            sum   := sum + TH.Population[kind]*TH.ResRent[kind];
            count := count + TH.Population[kind];
          end;
        if count > 0
          then result := round(sum/count)
          else result := 0;
      finally
        Towns.Unlock;
      end;
    end;

  function TWorld.GetResQidx( kind : TPeopleKind ) : integer;
    var
      sum   : double;
      count : double;
      i     : integer;
      TH    : TTownHall;
    begin
      sum   := 0;
      count := 0;
      Towns.Lock;
      try
        for i := 0 to pred(Towns.Count) do
          begin
            TH    := TTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock);
            sum   := sum + TH.Population[kind]*TH.ResQidx[kind];
            count := count + TH.Population[kind];
          end;
        if count > 0
          then result := round(sum/count)
          else result := 0;
      finally
        Towns.Unlock;
      end;
    end;

  function TWorld.GetWorkDemand( kind : TPeopleKind ) : TFluidValue;
    var
      i     : integer;
      TH    : TTownHall;
    begin
      result := 0;
      Towns.Lock;
      try
        for i := 0 to pred(Towns.Count) do
          begin
            TH     := TTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock);
            result := result + TH.ResDemand[kind];
          end;
      finally
        Towns.Unlock;
      end;
    end;

  function TWorld.GetPrivWorkDemand( kind : TPeopleKind ) : TFluidValue;
    var
      i     : integer;
      TH    : TTownHall;
    begin
      result := 0;
      Towns.Lock;
      try
        for i := 0 to pred(Towns.Count) do
          begin
            TH     := TTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock);
            result := result + TH.ResDemand[kind];
          end;
      finally
        Towns.Unlock;
      end;
    end;

  function TWorld.GetPFCoverage( Id : string ) : TPFCoverage;
    var
      PFMetaInfo : TMetaPublicFacilityInfo;
      PFInfo     : TPublicFacilityInfo;
      sum        : double;
      count      : double;
      TH         : TTownHall;
      i          : integer;
    begin
      PFMetaInfo := TMetaPublicFacilityInfo(TheClassStorage.ClassById[tidClassFamily_PublicFacilities, Id]);
      if PFMetaInfo <> nil
        then
          begin
            sum   := 0;
            count := 0;
            Towns.Lock;
            try
              for i := 0 to pred(Towns.Count) do
                begin
                  TH    := TTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock);
                  PFInfo := TH.PublicFacilities[PFMetaInfo];
                  if PFInfo <> nil
                    then
                      begin
                        sum   := sum + PFInfo.Strength;
                        count := count + TH.TotalPopulation;
                      end;
                end;
              result.Name := PFMetaInfo.Name;
              if round(count) > 0
                then result.Ratio := round(100*sum/count)
                else result.Ratio := 0;
            finally
              Towns.Unlock;
            end;
          end
        else
          begin
            result.Name  := '';
            result.Ratio := 0;
          end;
    end;

  function TWorld.GetServiceInfoById( Id : string ) : TServiceCoverage;
    var
      ServiceInfo : TServiceInfo;
      sumDemand   : double;
      sumOffer    : double;
      sumCapacity : double;
      sumQuality  : double;
      sumPrice    : double;
      count       : double;
      TH          : TTownHall;
      i           : integer;
    begin
      sumDemand   := 0;
      sumOffer    := 0;                 
      sumCapacity := 0;
      sumQuality  := 0;
      sumPrice    := 0;
      count       := 0;
      Towns.Lock;
      try
        ServiceInfo := nil;
        for i := 0 to pred(Towns.Count) do
          begin
            TH := TTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock);    
            ServiceInfo := TServiceInfo(TH.ServiceInfoById[Id]);
            if ServiceInfo <> nil
              then
                begin
                  sumDemand   := sumDemand + TH.TotalPopulation*ServiceInfo.Demand;
                  sumOffer    := sumOffer + TH.TotalPopulation*ServiceInfo.Offer;
                  sumCapacity := sumCapacity + TH.TotalPopulation*ServiceInfo.Capacity;
                  sumQuality  := sumQuality + TH.TotalPopulation*ServiceInfo.Quality;
                  sumPrice    := sumPrice + TH.TotalPopulation*ServiceInfo.Price;
                  count       := count + TH.TotalPopulation;
                end;
          end;
        if count > 0
          then
            begin
              result.Name     := ServiceInfo.Kind.Name_MLS.Values[langDefault]; // >> MLS Alarm
              result.Demand   := sumDemand/count;
              result.Offer    := sumOffer/count;
              result.Capacity := sumCapacity/count;
              result.Quality  := round(100*sumQuality/count);
              result.Price    := round(100*sumPrice/count);
              if result.Demand > 0
                then result.Ratio := round(100*result.Offer/result.Demand)
                else result.Ratio := 0;
            end
          else fillchar( result, sizeof(result), 0 );
      finally
        Towns.Unlock;
      end;
    end;

  procedure TWorld.SetMinSalary( kind : TPeopleKind; value : TPercent );
    begin
      fMinSalaries[kind] := value;
    end;

  function TWorld.GetGQOL : TPercent;
    var
      sum   : double;
      count : double;
      i     : integer;
      TH    : TTownHall;
    begin
      sum   := 0;
      count := 0;
      Towns.Lock;
      try
        for i := 0 to pred(Towns.Count) do
          begin
            TH    := TTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock);
            sum   := sum + TH.TotalPopulation*TH.GQOL;
            count := count + TH.TotalPopulation;
          end;
        if count > 0
          then result := round(100*sum/count)
          else result := 0;
      finally
        Towns.Unlock;
      end;
    end;

  function TWorld.GetGQOS : TPercent;
    var
      sum   : double;
      count : double;
      i     : integer;
      TH    : TTownHall;
    begin
      sum   := 0;
      count := 0;
      Towns.Lock;
      try
        for i := 0 to pred(Towns.Count) do
          begin
            TH    := TTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock);
            sum   := sum + TH.TotalPopulation*TH.GQOS;
            count := count + TH.TotalPopulation;
          end;
        if count > 0
          then result := round(100*sum/count)
          else result := 0;
      finally
        Towns.Unlock;
      end;
    end;

  function TWorld.GetGeneralWealth : integer;
    var
      sum   : double;
      count : double;
      i     : integer;
      TH    : TTownHall;
    begin
      sum   := 0;
      count := 0;
      Towns.Lock;
      try
        for i := 0 to pred(Towns.Count) do
          begin
            TH    := TTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock);
            sum   := sum + TH.TotalPopulation*(100*TH.AvgWealth);
            count := count + TH.TotalPopulation;
          end;
        if count > 0
          then result := round(100*sum/count)
          else result := 0;
      finally
        Towns.Unlock;
      end;
    end;

  function TWorld.GetNetFacilities : integer;
    var
      i : integer;
    begin
      result := 0;
      fFacilities.Lock;
      try
        for i := 0 to pred(fFacilities.Count) do
          inc(result, TFacility(fFacilities[i]).UpgradeLevel);
      finally
        fFacilities.Unlock;
      end;
    end;

  procedure TWorld.CheckConnections;
    var
      i : integer;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Render roads...' );
      InitTownRoadCounter;
      fRoads.Render(ReportRoadBlock);
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Checking connections...' );
      for i := 0 to pred(Facilities.Count) do
        begin
          fWorldLock.Enter;
          try
            TFacility(Facilities[i]).CheckCircuits;
          finally
            fWorldLock.Leave;
          end;
        end;
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' End checking circuits, now outputs!!' );
      fWorldLock.Enter;
      try
        for i := 0 to pred(Facilities.Count) do
          TFacility(Facilities[i]).CheckConnections( chkOutputs );
      finally
        fWorldLock.Leave;
      end;
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' OK Checking connections!' );
    end;

  procedure TWorld.Simulate;

    procedure WipeFacility( Facility : TFacility );
      var
        i : integer;
      begin
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Checking for money returns... ' + Format('Name: %s [%d, %d] Kind: %s', [Facility.Name, Facility.xPos, Facility.yPos, Facility.MetaFacility.Name] ));
        fWorldLock.Enter;
        try
          if not Facility.Deleted and (Facility.Company <> nil)
            then
              if Facility.ToBeDemolished = 1
                then
                  begin
                    // Refunded only if it was built in a zoned place
                    if Facility.Trouble and facForbiddenZone = 0
                      then Facility.Company.GenMoney(Facility.TCCost, accIdx_Compensations);
                  end
                else
                  // Refunded according the owner's level
                  if (Facility.Company.Owner <> nil) and (Facility.Company.Owner.Level <> nil)
                    then Facility.Company.GenMoney( Facility.Company.Owner.Level.MoneyBackOnDemolish*Facility.TCCost, accIdx_Compensations );
        finally
          fWorldLock.Leave;
        end;
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Notifying facility deletion... ' );
        FacilityDeleted( Facility );
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Stopping facility... ' );
        fWorldLock.Enter;
        try
          Facility.Deleted := true;
          Facility.Stopped := true;
          Facility.Uncache(true); // >> In background
        finally
          fWorldLock.Leave;
        end;
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Notifying tycoons... ' );
        fTycoons.Lock;
        try
          for i := 0 to pred(fTycoons.Count) do
            try
              TTycoon(fTycoons[i]).FacilityDestroyed( Facility );
            except
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' ERROR Del Facility -> Notifying tycoon: ' + IntToStr(i) );
            end;
        finally
          fTycoons.Unlock;
        end;

        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Notifying company... ' );
        try
          if Facility.Company <> nil
            then Facility.Company.FacilityDestroyed( Facility );
        except
          Logs.Log( tidLog_Survival, TimeToStr(Now) + ' ERROR Del Facility -> Notifying company.' );
        end;
      end;

    procedure TranscendFacility( Facility : TFacility );
      var
        DestCompany : TCompany;
      begin
        try
          if Facility.Town.Mayor.Companies.Count > 0
            then
              begin
                DestCompany := TCompany(Facility.Town.Mayor.Companies[0]);
                try
                  if (DestCompany <> nil) and (DestCompany.Facilities.IndexOf(Facility) = noIndex)
                    then
                      begin
                        if Facility.Company <> nil
                          then Facility.Company.FacilityDestroyed( Facility );
                        DestCompany.FacilityCreated( Facility );
                        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Transcend Facility -> OK.' );
                      end;
                except
                  Logs.Log( tidLog_Survival, TimeToStr(Now) + ' ERROR Transcend Facility -> Notifying company.' );
                end;
              end
            else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' ERROR Transcend Facility -> Could not find neutral company.' );
        except
          Logs.Log( tidLog_Survival, TimeToStr(Now) + ' ERROR Transcend Facility -> Unknown.' );
        end;
      end;

    procedure ReleaseMeat;
      var
        Fac : TFacility;
      const
        PendDeletionsMax = 3;
      var
        PendDeletionsCount : integer;
        i                  : integer;
      begin
        Facilities.Lock;
        try
          fDeadTycoons.Lock;
          try
            i := 0;
            while (i < fDeadTycoons.Count) do
              try
                if TTycoon(fDeadTycoons[i]).Companies.Count = 0
                  then
                    begin
                      DeleteTycoon( TTycoon(fDeadTycoons[i]), TTycoon(fDeadTycoons[i]).Deleted );
                      fDeadTycoons.AtExtract( i );
                    end
                  else inc( i );
              except
                try
                  fDeadTycoons.AtExtract( i );
                except
                end;
              end;
          finally
            fDeadTycoons.Unlock;
          end;
          fDeadCompanies.Lock;
          try
            i := 0;
            while (i < fDeadCompanies.Count) do
              try
                if TCompany(fDeadCompanies[i]).Facilities.Count = 0
                  then
                    begin
                      DeleteCompany( TCompany(fDeadCompanies[i]) );
                      fDeadCompanies.AtExtract( i );
                    end
                  else inc( i );
              except
                try
                  fDeadCompanies.AtExtract( i );
                except
                end;
              end;
          finally
            fDeadCompanies.Unlock;
          end;
          fPendingDeletions.Lock;
          try
            PendDeletionsCount := 0;
            while (fPendingDeletions.Count > 0) and (PendDeletionsCount < PendDeletionsMax) do
              begin
                try
                  DeleteFacility( TFacility(fPendingDeletions[0]) );
                  inc( PendDeletionsCount );
                except
                end;
                fPendingDeletions.AtExtract( 0 );
              end;
            for i := 0 to pred(fPendingDeletions.Count) do
              begin
                Fac := TFacility(fPendingDeletions[i]);
                if not Fac.CurrBlock.MetaBlock.Transcends(Fac)
                  then Facilities.Extract(Fac); // >>
              end;
          finally
            fPendingDeletions.Unlock;
          end;
          fDeadMeat.Lock;
          try
            while fDeadMeat.Count > 0 do
              begin
                Fac := TFacility(fDeadMeat[0]);
                try
                  RemoveFacilityToClone(Fac);
                  if Fac.CurrBlock.MetaBlock.Transcends(Fac)
                    then TranscendFacility(Fac)
                    else WipeFacility(Fac);
                  Logs.Log( 'Survival', 'DEAD MEAT: wiping successfull!' );
                except
                  Logs.Log( 'Survival', 'DEAD MEAT: Error wiping!' );
                end;

                // Do not demolish the Mausoleums
                if not Fac.CurrBlock.MetaBlock.Transcends(Fac)
                  then Facilities.Extract(Fac);

                try
                  Logs.Log( 'Survival', 'DEAD MEAT: Destroying Facility...' );
                  //fDeadMeat.AtDelete( 0 ); // >>> !!!!!!!!!!!
                  fDeadMeat.AtExtract( 0 );
                  Logs.Log( 'Survival', 'DEAD MEAT: Success Destroying!' );
                except
                  Logs.Log( 'Survival', 'DEAD MEAT: Error!' );
                end;
              end;
          finally
            fDeadMeat.Unlock;
          end;
          fNewMeat.Lock;
          try
            while fNewMeat.Count > 0 do
              begin
                Logs.Log( 'Survival', 'NEW MEAT: Inserting Facility...' );
                Fac := TFacility(fNewMeat[0]);
                try
                  Logs.Log( 'Survival', TimeToStr(Now) + ' NEW MEAT: Looking for suppliers.' );
                  SearchForSuppliers( Fac.CurrBlock );
                  Logs.Log( 'Survival', TimeToStr(Now) + ' NEW MEAT: Looking for suppliers OK.' );
                except
                  Logs.Log( 'Survival', TimeToStr(Now) + ' NEW MEAT: Exception looking for suppliers.' );
                end;
                InsertFacility( Fac );
                fNewMeat.AtExtract( 0 );
                Logs.Log( 'Survival', 'NEW MEAT: Inserting Done.' );
              end;
          finally
            fNewMeat.Unlock;
          end;
        finally
          Facilities.Unlock;
        end;
      end;

    procedure SimulateFacilities;

      function GetDt( oldDt : TTimeDelta ) : TTimeDelta;
        const
          MaxDtChange = 0.05;
        begin
          if fTick > 0
            then result := fTimeTick/fTick
            else result := 1;
          result := oldDt + realmax( -MaxDtChange, realmin( MaxDtChange, result - oldDt ));
        end;

      var
        i      : integer;
        update : boolean;
        desc   : string;

      begin
        update := VirtualTimeAbs - fLastUpdate >= UpdateFreq;
        fdt := realmax(1, GetDt(fdt));
        if (fdt >= 1) and (dt < 100)
          then
            begin
              fLastSimulation := VirtualTimeAbs;
              if update
                then fLastUpdate := fLastSimulation;

              // Clear sim params
              ClearSimVars;

              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' SIM-Spread' );
              for i := 0 to pred(Facilities.Count) do
                with TFacility(Facilities[i]) do
                  begin
                    fWorldLock.Enter;
                    try
                      Profiler.ProcStarted( prfKind_Sim, prfId_SpreadOutputs );
                      inc(fInputTotal, CurrBlock.InputCount);
                      inc(fOutputTotal, CurrBlock.OutputCount);
                      SpreadOutputs;
                      Profiler.ProcEnded( prfKind_Sim, prfId_SpreadOutputs );
                    finally
                      fWorldLock.Leave;
                    end;
                  end;

              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' SIM-Collect' );
              for i := 0 to pred(Facilities.Count) do
                with TFacility(Facilities[i]) do
                  begin
                    fWorldLock.Enter;
                    try
                      Profiler.ProcStarted( prfKind_Sim, prfId_CollectInputs );
                      CollectInputs;
                      Profiler.ProcEnded( prfKind_Sim, prfId_CollectInputs );
                    finally
                      fWorldLock.Leave;
                    end;
                  end;

              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' SIM-SpreadExt' );
              for i := 0 to pred(Facilities.Count) do
                with TFacility(Facilities[i]) do
                  begin
                    fWorldLock.Enter;
                    try
                      Profiler.ProcStarted( prfKind_Sim, prfId_SpreadOutputExtra );
                      SpreadOutputExtra;
                      Profiler.ProcEnded( prfKind_Sim, prfId_SpreadOutputExtra );
                    finally
                      fWorldLock.Leave;
                    end;
                  end;

              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' SIM-CollectExt' );
              for i := 0 to pred(Facilities.Count) do
                with TFacility(Facilities[i]) do
                  begin
                    fWorldLock.Enter;
                    try
                      Profiler.ProcStarted( prfKind_Sim, prfId_CollectInputExtra );
                      CollectInputExtra;
                      Profiler.ProcEnded( prfKind_Sim, prfId_CollectInputExtra );
                    finally
                      fWorldLock.Leave;
                    end;
                  end;

              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' SIM-Facs' );
              for i := 0 to pred(Facilities.Count) do
                with TFacility(Facilities[i]) do
                  try
                    fWorldLock.Enter;
                    try
                      Profiler.ProcStarted( prfKind_Sim, prfId_Sim );
                      Simulate;
                      Profiler.ProcEnded( prfKind_Sim, prfId_Sim );
                      // >>> REMOVE!!!
                      //fNotify('Simulating...', round(100*i/Facilities.Count));
                      // >>>
                      if update
                        then RefeshFacility( TFacility(Facilities[i]), fchStatus );
                    finally
                      fWorldLock.Leave;
                    end;
                  except
                    on e : exception do
                      try
                        desc := MetaFacility.Name + '(' + IntToStr(xPos) + ', ' + IntToStr(yPos) + ')';
                        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' ' + desc + ' ' + e.Message + ' Error simulating facility ' + IntToStr(i));
                      except
                        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' (big corruption) Error simulating facility ' + IntToStr(i) );
                      end;
                  end;

              fSimReport := Format(
                'Facilities: %d' + ^M^J +
                'Inputs: %d' + ^M^J +
                'Outputs: %d' + ^M^J +
                'Input Connections: %d' + ^M^J +
                'Output Connections: %d' + ^M^J +
                'Input Max: %d' + ^M^J +
                'Output Max: %d' + ^M^J,
                [Facilities.Count,
                 fInputTotal,
                 fOutputTotal,
                 fInCnnts,
                 fOutCnnts,
                 fMaxIn,
                 fMaxOut]);
            end;
      end;

    procedure SimulateTowns;
      var
        i : integer;
      begin
        if dt > 0
          then
            for i := 0 to pred(Towns.Count) do
              try
                TTown(Towns[i]).UpdateParameters;
              except
              end;
      end;

    procedure SimulateCompanies;
      var
        i : integer;
        C : TCompany;
      begin
        if dt > 0
          then
            begin
              Companies.Lock;
              try
                for i := 0 to pred(Companies.Count) do
                  try
                    C := TCompany(Companies[i]);
                    C.UpdateParameters;
                    C.ChargeResearches( dt );
                  except
                  end;
              finally
                Companies.Unlock;
              end;
            end;
      end;

    procedure SimulateTycoons;
      var
        i : integer;
      begin
        Tycoons.Lock;
        try
          for i := 0 to pred(Tycoons.Count) do
            try
              TTycoon(Tycoons[i]).UpdateParameters;
            except
              on E : Exception do
                if fTick mod 10 = 0
                  then Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error updating tycoon: ' + IntToStr(i) + ' (' + E.Message + ')' );
            end;
        finally
          Tycoons.Unlock;
        end;
      end;

    begin
      try
        inc( fTick );
        //Logs.LogMemReport(tidLog_Survival);
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' <' + IntToStr(fTick) );
        // Reset profiler
        if fTick mod 30 = 0
          then
            begin
              Profiler.LogResults( prfKind_Sim, 'SimProfile' );
              Profiler.LogResults( prfKind_InputCollect, 'SimProfile' );
              Profiler.LogResults( prfKind_Int, 'SimProfile' );
              Profiler.Reset( prfKind_Sim );
              Profiler.Reset( prfKind_InputCollect );
              Profiler.Reset( prfKind_Int );
            end;

        // Update the Rankings in the Directory Server
        if fPeriods[perYear] > 0
          then
            begin
              try
                Logs.Log( tidLog_Survival, TimeToStr(Now) + 'SIM-Update Rankings' );
                StopVirtualTime;
                try
                  UpdateRankingsInDirectory;
                finally
                  ResumeVirtualTime;
                end;
                Logs.Log( tidLog_Survival, TimeToStr(Now) + 'SIM-Update Rankings Done!' );
              except
                Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error updating Rankings in Dir..' );
              end;
            end;

        fWorldLock.Enter;
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' SIM-Tycoons' );
        try
          //Logs.LogMemReport(tidLog_Survival);
          SimulateTowns;
          SimulateCompanies;
          SimulateTycoons;
        finally
          fWorldLock.Leave;
        end;
        Facilities.Lock;
        try
          fWorldLock.Enter;
          try
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' SIM-Cloning' );
            try
              CloneFacilities;
            except
            end;

            Logs.Log( tidLog_Survival, TimeToStr(Now) + 'SIM-Cleaning' );
            try
              //Logs.LogMemReport(tidLog_Survival);
              ReleaseMeat;
              //Logs.LogMemReport(tidLog_Survival);
            except
            end;
            try
              NotifyEndOfPeriods;
              //Logs.LogMemReport(tidLog_Survival);
            except
              on e : exception do
                try
                  Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error notifying ENDOFPERIODS. ' + e.Message );
                except
                end;
            end;
          finally
            fWorldLock.Leave;
          end;
          try
            SimulateFacilities;
            //Logs.LogMemReport(tidLog_Survival);
          except
          end;
          Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Check roads' );
          try
            if fRoadRefreshDelay > MaxRoadRefreshDelay
              then
                begin
                  CheckConnections;
                  //Logs.LogMemReport(tidLog_Survival);
                  fRoadRefreshDelay := 0;
                end
              else
                if fRoadRefreshDelay > 0
                  then inc( fRoadRefreshDelay, round(dt) );
          except
          end;
        finally
          Facilities.Unlock;
        end;
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Notification' );
        try
          if assigned(fOnTycoonsChanged)
            then
              begin
                fWorldLock.Enter;
                try
                  fOnTycoonsChanged;
                finally
                  fWorldLock.Leave;
                end;
              end;
        except
        end;
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' ' + IntToStr(fTick) + '>' );
        //Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Connections per Input:      ' + IntToStr(avgCnx div avgCnxCnt) );
        //Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Used Connections per Input: ' + IntToStr(avgCnxUsed div avgCnxCnt) );
        //Logs.LogMemReport(tidLog_Survival);
      except
      end;
    end;

  procedure TWorld.NotifyEndOfPeriods;

    procedure NotifyFacilities( PeriodType : TPeriodType; PeriodCount : integer );
      var
        i    : integer;
        Fac  : TFacility;
        x, y : integer;
      begin
        if PeriodType <> perHour
          then
            begin
              x := 0;
              y := 0;
              Facilities.Lock;
              try
                for i := 0 to pred(Facilities.Count) do
                  try
                    Fac := TFacility(Facilities[i]);
                    x   := Fac.xPos;
                    y   := Fac.yPos;
                    Fac.EndOfPeriod( PeriodType, PeriodCount );
                  except
                    on e : exception do
                      try
                        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error at Facility.EndOfPeriod (' + IntToStr(x) + ',' + IntToStr(y) +') ' + e.Message );
                      except
                      end;
                  end;
              finally
                Facilities.Unlock;
              end;
            end;
      end;

    procedure NotifyCompanies( PeriodType : TPeriodType; PeriodCount : integer );
      var
        i : integer;
      begin
        Companies.Lock;
        try
          for i := 0 to pred(Companies.Count) do
            try
              TCompany(Companies[i]).EndOfPeriod( PeriodType, PeriodCount );
            except
              on e : exception do
                try
                  Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error at Company.EndOfPeriod (' + IntToStr(i) + ') ' + e.Message );
                except
                end;
            end;
        finally
          Companies.Unlock;
        end;
      end;

    procedure NotifyTowns( PeriodType : TPeriodType; PeriodCount : integer );
      var
        i : integer;
      begin
        Towns.Lock;
        try
          for i := 0 to pred(Towns.Count) do
            try
              TTown(Towns[i]).EndOfPeriod( PeriodType, PeriodCount );
            except
              on e : exception do
                try
                  Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error at Town.EndOfPeriod (' + IntToStr(i) + ') ' + e.Message );
                except
                end;
            end;
        finally
          Towns.Unlock;
        end;
      end;

    procedure NotifyTycoons( PeriodType : TPeriodType; PeriodCount : integer );
      var
        i      : integer;
        Tycoon : TTycoon;
        InvCnt : integer;
        VisCnt : integer;
      begin
        Tycoons.Lock;
        try
          InvCnt := 0;
          VisCnt := 0;
          for i := pred(Tycoons.Count) downto 0 do
            try
              Tycoon := TTycoon(Tycoons[i]);
              Tycoon.EndOfPeriod( PeriodType, PeriodCount );
              case PeriodType of
                perDay :
                  if tyoMainRole in Tycoon.Options
                    then
                      if Tycoon.Companies.Count > 0
                        then inc(InvCnt)
                        else inc(VisCnt);
                perYear :
                  if tyoMainRole in Tycoon.Options
                    then
                      if (YearsWithoutConnecting( Tycoon ) > MaxYearsWithoutConnecting) //or AccountExpired( Tycoon )
                        then RequestDeleteTycoon( Tycoon, true )
                        else
                          if not Tycoon.IsRole and (Tycoon.FailureLevel >= MaxFailure)
                            then
                              begin
                                SendEvent(
                                  TEvent.Create(
                                    0,
                                    VirtualTimeAbs,
                                    VirtualTime,
                                    10000 + Tycoon.FacCount,
                                    1000,
                                    InstantiateMultiString( mtidTycoonBankrupt, [Tycoon.Name] ),
                                    //NullString, //Tycoon.Name + ' was declared bankrupt.',
                                    '', '' ));
                                ResetTycoon( Tycoon );
                              end;
              end;
            except
              on e : exception do
                try
                  Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error at Tycoon.EndOfPeriod (' + IntToStr(i) + ') ' + e.Message );
                except
                end;
            end;
          case PeriodType of
            perDay :
              begin
                InterlockedExchange(fInvestorCount, InvCnt);
                InterlockedExchange(fVisitorCount, VisCnt);
              end;
          end;
        finally
          Tycoons.Unlock;
        end;
      end;

    procedure AdvanceTycoonLevels;
      var
        i      : integer;
        Tycoon : TTycoon;
      begin
        Tycoons.Lock;
        try
          for i := pred(Tycoons.Count) downto 0 do
            try
              Tycoon := TTycoon(Tycoons[i]);
              Tycoon.CheckForNextLevel;
            except
            end;
        finally
          Tycoons.Unlock;
        end;
      end;

    procedure NotifyExtensions( PeriodType : TPeriodType; PeriodCount : integer );
      var
        i : integer;
      begin
        for i := 0 to pred(fWorldExtensions.Count) do
          TWorldExtension(fWorldExtensions[i]).EndOfPeriod( PeriodType, PeriodCount );
      end;

    var
      period : TPeriodType;
    begin
      StopVirtualTime;
      try
        for period := low(period) to high(period) do
          if fPeriods[period] > 0
            then
              begin
                {$ifopt d+}
                if period = perYear
                  then Logs.Log('EOY', TimeToStr(Now) + ' EndOfPeriod');
                {$endif}

                EndOfPeriod( period, fPeriods[period] );

                {$ifopt d+}
                if period = perYear
                  then Logs.Log('EOY', TimeToStr(Now) + ' towns');
                {$endif}

                NotifyTowns( period, fPeriods[period] );

                {$ifopt d+}
                if period = perYear
                  then Logs.Log('EOY', TimeToStr(Now) + ' tycoons');
                {$endif}

                NotifyTycoons( period, fPeriods[period] );

                {$ifopt d+}
                if period = perYear
                  then Logs.Log('EOY', TimeToStr(Now) + ' companies');
                {$endif}

                NotifyCompanies( period, fPeriods[period] );

                {$ifopt d+}
                if period = perYear
                  then Logs.Log('EOY', TimeToStr(Now) + ' facilities');
                {$endif}

                NotifyFacilities( period, fPeriods[period] );

                {$ifopt d+}
                if period = perYear
                  then Logs.Log('EOY', TimeToStr(Now) + ' advance tycoons');
                {$endif}

                if period = perYear
                  then AdvanceTycoonLevels;

                {$ifopt d+}
                if period = perYear
                  then Logs.Log('EOY', TimeToStr(Now) + ' main bank');
                {$endif}

                fMainBank.EndOfPeriod( period, fPeriods[period] );

                {$ifopt d+}
                if period = perYear
                  then Logs.Log('EOY', TimeToStr(Now) + ' OnEndOfPeriod');
                {$endif}

                if assigned(fOnEndOfPeriod)
                  then fOnEndOfPeriod( period, fPeriods[period] );

                {$ifopt d+}
                if period = perYear
                  then Logs.Log('EOY', TimeToStr(Now) + ' Extensions');
                {$endif}

                NotifyExtensions( period, fPeriods[period] );

                {$ifopt d+}
                if period = perYear
                  then Logs.Log('EOY', TimeToStr(Now) + ' update hours');
                {$endif}

                fPeriods[period] := 0;
                UpdateHoursADay(period);

                // Updating Contest Data on August 1st every year.
                if (period = perDay) and (fVirtDay = 29) and (fVirtMonth = 12) and (TournamentLength > 0) and (fVirtYear - YearZero >= fTornamentStart) and (fVirtYear - YearZero <= fTournamentLength)
                  then
                    begin
                      Logs.Log('tour', TimeToStr(Now) + 'Tournament Begin Updating..');
                      UpdateContestData;
                      Logs.Log('tour', TimeToStr(Now) + 'Tournament Updated..');
                    end;

                {$ifopt d+}
                if period = perYear
                  then Logs.Log('EOY', TimeToStr(Now) + ' done');
                {$endif}
              end;
      finally
        ResumeVirtualTime;
      end;
    end;

  procedure TWorld.UpdateRankingsInDirectory;
    var
      Ranking : TRanking;
      i       : integer;
      count   : integer;
      key     : string;
      result  : TStringList;
      waitForAnws : boolean;
      rkIdx : integer;
    begin
      DSLock;
      try
        if not VarIsEmpty(DirProxy) and not VarIsNull(DirProxy)
          then
            try
              key := 'Root/Areas/' + Area + '/Worlds/' + Name + '/Model/Rankings';
              if DirProxy.RDOCreateFullPathKey( key, true ) and DirProxy.RDOSetCurrentKey(key)
                then
                  begin
                    result := TStringList.Create;
                    try
                      count := TheClassStorage.ClassCount[tidClassFamily_Rankings];
                      //result.Values[tidRankings_RankCount] := IntToStr(count);
                      rkIdx := 0;
                      if fTournamentLength = 0
                        then
                          for i := 0 to pred(count) do
                            begin
                              Ranking := TRanking(TheClassStorage.ClassByIdx[tidClassFamily_Rankings, i]);
                              if (Ranking.SuperRanking <> '') and Ranking.Serializable
                                then
                                  begin
                                    Ranking.Serialize(IntToStr(rkIdx), result);
                                    inc(rkIdx);
                                  end;
                            end;
                      result.Values[tidRankings_RankCount] := IntToStr(rkIdx);
                      result.Text := result.Text + tidTerminator_EndOfRanking;
                      waitForAnws := DirProxy.WaitForAnswer;
                      DirProxy.WaitForAnswer := true;
                      try
                        DirProxy.RDOWriteString( 'Ranking', result.Text );
                        DirProxy.RDOWriteString( 'DateStamp', DateToStr(VirtualTime) );
                        //Logs.Log('Rankings', result.Text);
                      finally
                        DirProxy.WaitForAnswer := waitForAnws;
                      end;
                    finally
                      result.Free;
                    end;
                  end;
            except
            end;
      finally
        DSUnlock;
      end;
    end;

  procedure TWorld.UpdateHoursADay(PeriodType : TPeriodType);
    begin
      case PeriodType of
        perHour :
          begin
            fSimHours := fSimHours + fdt;
          end;
        perDay :
          inc(fSimDays);
        perYear :
          begin
            fSimHours := 0;
            fSimDays  := 0;
          end;
      end;
    end;

  function TWorld.DaysOut(Tycoon : TTycoon) : integer;
    var
      lastOnline : string;
      lastDate   : TDateTime;
    begin
      Tycoon.Lock;
      try
        lastOnline := Tycoon.Cookie[tidCookie_LastTimeOnline];
        if lastOnline <> ''
          then
            begin
              lastDate := StrToDateTime(lastOnline);
              result   := round(Now - lastDate);
            end
          else result := 31; // days..
      finally
        Tycoon.Unlock;
      end;
    end;

  function TWorld.GetDaysExpired(Tycoon : TTycoon; def : integer) : integer;
    {var
      key     : string;
      accSt   : integer;
      ExpDate : TDateTime;}
    begin
      try
        DSLock;
        try
          result := DirProxy.RDOGetExpDays(Tycoon.Name);
        finally
          DSUnlock;
        end;
      except
        result := def;
      end;
    end;

  procedure TWorld.PurgeTycoons(count : integer);
    var
      tycIdx : integer;
      purged : integer;
      Tycoon : TTycoon;
      days   : integer;
      accExp : integer;
    begin
      try
        fTycoons.Lock;
        try
          purged := 0;
          tycIdx := fPurgeIndex;
          while (purged < count) and (tycIdx < fTycoons.Count) do
            begin
              Tycoon := TTycoon(fTycoons[tycIdx]);
              if (not Tycoon.IsOnline) and (not Tycoon.IsRole) and (not Tycoon.Deleted)
                then
                  begin
                    days := DaysOut(Tycoon);
                    if days > MaxDaysToPurge
                      then accExp := GetDaysExpired(Tycoon, 0)
                      else accExp := 0;
                    if accExp > MaxDaysToPurge
                      then
                        begin
                          Logs.Log( tidLog_Survival, TimeToStr(Now) + Format(' Purging user %s expire by %d days.', [Tycoon.Name, accExp]));
                          RequestDeleteTycoon(Tycoon, true); // adios!
                          inc(purged);
                        end;
                  end;
              inc(tycIdx);
            end;
          {if tycIdx < fTycoons.Count
            then fPurgeIndex := tycIdx
            else fPurgeIndex := 0;}
          fPurgeIndex := tycIdx;
        finally
          fTycoons.Unlock;
        end;
      except
      end;
    end;

  procedure TWorld.UpdateTycoonsNobility(count : integer);
    var
      tycIdx  : integer;
      updated : integer;
      Tycoon  : TTycoon;
      error   : boolean;
    begin
      try
        fTycoons.Lock;
        try
          error   := false;
          updated := 0;
          tycIdx  := fUpdateNobIndex;
          while not error and (updated < count) and (tycIdx < fTycoons.Count) do
            begin
              Tycoon := TTycoon(fTycoons[tycIdx]);
              if not Tycoon.IsRole and not Tycoon.Deleted
                then
                  if UpdateNobility(Tycoon)
                    then
                      begin
                        inc(tycIdx);
                        inc(updated);
                      end
                    else error := true
                else inc(tycIdx);
            end;
          if tycIdx >= fTycoons.Count
            then tycIdx := 0;
          fUpdateNobIndex := tycIdx;
        finally
          fTycoons.Unlock;
        end;
      except
      end;
    end;

  procedure TWorld.UpdateTycoonsDemoStatus(count : integer);
    var
      tycIdx  : integer;
      updated : integer;
      Tycoon  : TTycoon;
    begin
      try
        tycIdx := fUpdateDemoIndex;
        fTycoons.Lock;
        try
          updated := 0;
          while (updated < count) and (tycIdx < fTycoons.Count) do
            begin
              Tycoon := TTycoon(fTycoons[tycIdx]);
              if not Tycoon.IsRole and not Tycoon.Deleted and Tycoon.IsDemo
                then
                  begin
                    Tycoon.IsDemo := IsDemoAccount(Tycoon.Name);
                    inc(updated);
                  end;
              inc(tycIdx);
            end;
        finally
          fTycoons.Unlock;
          if tycIdx >= fTycoons.Count
            then tycIdx := 0;
          fUpdateDemoIndex := tycIdx;
        end;
      except
      end;
    end;

  procedure TWorld.UpdateMailServerDate;
    begin
      try
        fMailServer.UpdateDate(EncodeDate(fVirtYear, fVirtMonth, fVirtDay));
      except
        Logs.Log('Survival', TimeToStr(Now) + ' Date ' + IntToStr(fVirtYear) + '/' + IntToStr(fVirtMonth) + '/' + IntToStr(fVirtDay));
      end;
    end;

  type
    RankInfo =
      record
        id   : string;
        val1 : byte;
        val2 : byte;
        val3 : byte;
      end;

  const
    tbContestPoints : array[0..5] of RankInfo =
    (
      (id: 'NTA'; val1: 0; val2: 0; val3: 0), // NTA
      (id: '4';   val1: 8; val2: 6; val3: 4), // Industry
      (id: '3';   val1: 8; val2: 6; val3: 4), // Commerce
      (id: '7';   val1: 3; val2: 2; val3: 1), // Firms
      (id: '6';   val1: 3; val2: 2; val3: 1), // Offices
      (id: '2';   val1: 3; val2: 2; val3: 1)  // Residentials
    );

  procedure TWorld.UpdateContestData;

    procedure AddPts(Tycoon  : TTycoon; field : string; count : integer);
      var
        aux : string;
      begin
        aux := Tycoon.Cookie[field];
        if aux <> ''
          then aux := IntToStr(StrToInt(aux) + count)
          else aux := IntToStr(count);
        Tycoon.Cookie[field] := aux;
        Logs.Log('tour', Tycoon.Name + '[' + field + '] + ' + IntToStr(count) + '; total=' + aux);
      end;

    var
      i       : integer;
      Ranking : TRanking;
      count   : integer;
      Tycoon  : TTycoon;
      hiLevel : integer;
      loans   : TMoney;
      loanPts : int64;
      p       : integer;
    begin
      // Award point based on the ranking..
      for i := low(tbContestPoints) to high(tbContestPoints) do
        begin
          Ranking := TRanking(TheClassStorage.ClassById[tidClassFamily_Rankings, tbContestPoints[i].id]);
          if Ranking <> nil
            then
              begin
                Logs.Log('tour', 'Ranking -> ' + tbContestPoints[i].id);
                p := 0;
                count := Ranking.Objects.Count;
                if count > 0
                  then
                    try
                      if Ranking.IsRankeable(Ranking.Objects[p])
                        then AddPts(TTycoon(Ranking.Objects[p]), 'rkPts', tbContestPoints[i].val1);
                      inc(p);
                      while (p < count) and Ranking.IsRankeable(Ranking.Objects[p]) and (Ranking.CompareObjects(Ranking.Objects[p-1], Ranking.Objects[p], self) = 0) do
                        begin
                          AddPts(TTycoon(Ranking.Objects[p]), 'rkPts', tbContestPoints[i].val1);
                          inc(p);
                        end;
                    except
                    end;
                if count > p
                  then
                    try
                      if Ranking.IsRankeable(Ranking.Objects[p])
                        then AddPts(TTycoon(Ranking.Objects[p]), 'rkPts', tbContestPoints[i].val2);
                      inc(p);
                      while (p < count) and Ranking.IsRankeable(Ranking.Objects[p]) and (Ranking.CompareObjects(Ranking.Objects[p-1], Ranking.Objects[p], self) = 0) do
                        begin
                          AddPts(TTycoon(Ranking.Objects[p]), 'rkPts', tbContestPoints[i].val2);
                          inc(p);
                        end;
                    except
                    end;
                if count > p
                  then
                    try
                      if Ranking.IsRankeable(Ranking.Objects[p])
                        then AddPts(TTycoon(Ranking.Objects[p]), 'rkPts', tbContestPoints[i].val3);
                      inc(p);
                      while (p < count) and Ranking.IsRankeable(Ranking.Objects[p]) and (Ranking.CompareObjects(Ranking.Objects[p-1], Ranking.Objects[p], self) = 0) do
                        begin
                          AddPts(TTycoon(Ranking.Objects[p]), 'rkPts', tbContestPoints[i].val3);
                          inc(p);
                        end;
                    except
                    end;
              end;
        end;
      hiLevel := 0;
      fTycoons.Lock;
      try
        for i := 0 to pred(fTycoons.Count) do
          begin
            Tycoon := TTycoon(fTycoons[i]);
            if not Tycoon.IsRole and (Tycoon.Level <> nil) and (Tycoon.Level.Tier > hiLevel)
              then hiLevel := Tycoon.Level.Tier;
          end;
        // Award Level Points
        for i := 0 to pred(fTycoons.Count) do
          begin
            Tycoon := TTycoon(fTycoons[i]);
            if not Tycoon.IsRole and (Tycoon.Level <> nil)
              then
                begin
                  if (hiLevel > 0) and (Tycoon.Level.Tier = hiLevel)
                    then Tycoon.Cookie['lvPts'] := '40' // const
                    else Tycoon.Cookie['lvPts'] := '';
                  loans := Tycoon.LoanAmount - 100000000;
                  if loans > 0
                    then loanPts := max(0, min(10000, round(loans/(50*1000*1000))))
                    else loanPts := 0;
                  if loanPts > 0
                    then Tycoon.Cookie['bnkPts'] := IntToStr(-loanPts)
                    else Tycoon.Cookie['bnkPts'] := '';
                end;
          end;
      finally
        fTycoons.Unlock;
      end;
    end;

  procedure TWorld.SendZoningMessages(number : integer);
    var
      i, j : integer;
      Msg  : TZoningMessage;
      URL  : string;
      jStr : string;
      from : string;
    begin
      ZoningMessages.Lock;
      try
        from := 'mailer@Global' + self.Name + '.net';
        i := 0;
        while ((number = 0) or (i < number)) and (i < ZoningMessages.Count) do
          begin
            Msg := TZoningMessage(ZoningMessages[0]);
            URL := GetWorldURL + '/' + Msg.fLang + '/' + tidMailMsgURL_ZonedMsg + '?Zoned=' + Msg.fZoned + '&BuildNo=' + IntToStr(Msg.fZoners.Count);
            for j := 0 to pred(Msg.fZoners.Count) do
              begin
                jStr := IntToStr(j);
                URL  := URL + '&Zoner' + jStr + '=' + Msg.fZoners[j] +
                  '&BuildName' + jStr + '=' + Msg.fFacNames[j] +
                  '&BuildCompany' + jStr + '=' + Msg.fFacCompany[j] +
                  '&BuildX' + jStr + '=' + Msg.fFacX[j] +
                  '&BuildY' + jStr + '=' + Msg.fFacY[j] +
                  '&BuildDate' + jStr + '=' + Msg.fTime[j];
              end;
            MailServer.SendHTMLMessage(from, Msg.fZoned + '@' + self.name + '.net', 'Zoning Alert!', URL);
            fZoneMessSent := fZoneMessSent + 1;
            inc(i);
            Logs.Log( tidLog_Survival, TimeToStr(Now) + Format('Sent Zoning Message to : %s' , [Msg.fZoned]));
            ZoningMessages.AtDelete(0);
          end;
      finally
        ZoningMessages.Unlock;
      end;
    end;

  procedure TWorld.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );

    procedure UpdateRankings;
      var
        i     : integer;
        count : integer;
      begin
        count := TheClassStorage.ClassCount[tidClassFamily_Rankings];
        for i := 0 to pred(count) do
          TRanking(TheClassStorage.ClassByIdx[tidClassFamily_Rankings, i]).Update( self );
      end;

    procedure UpdateWeatherEnvelopes;
      var
        i     : integer;
        count : integer;
      begin
        count := TheClassStorage.ClassCount[tidClassFamily_WeatherEnvelopes];
        for i := 0 to pred(count) do
          TWeatherEnvelope(TheClassStorage.ClassByIdx[tidClassFamily_WeatherEnvelopes, i]).Update( fSeason, GetSeasonProgress );
      end;

    begin
      case PeriodType of
        perDay :
          begin
            Log(tidLog_Survival, TimeToStr(Now) + ' Period(day) ');
            if fMediaReleaseDays + fPeriods[PeriodType] >= MediaReleasePeriod
              then
                begin
                  if fNewsServer <> nil
                    then
                      try
                        Log(tidLog_Survival, TimeToStr(Now) + ' Period(newspaper) ');
                        fNewsServer.GenerateNewspapers( Name, VirtualTime );
                        fMediaReleaseDays := 0;
                      except
                        Log(tidLog_Survival, TimeToStr(Now) + ' Error in newspaper generation. ');
                      end;
                end
              else inc( fMediaReleaseDays, fPeriods[PeriodType] );
            Log( tidLog_Survival, DateToStr( VirtualTime ) + ' - ' + TimeToStr( Now ) );
            UpdateRankings;
            UpdateWeatherEnvelopes;
            if (TycoonsToPurge > 0) and (fDeadMeat.Count < 5)
              then PurgeTycoons(TycoonsToPurge);

            UpdateTycoonsNobility  (1 + fTycoons.Count div 365);
            UpdateTycoonsDemoStatus(1 + fTycoons.Count div 365);

            try
              SendZoningMessages(1);
            except
              Log(tidLog_Survival, TimeToStr(Now) + ' Error in SendZoningMessages(1).');
            end;
            try
              UpdateMailServerDate;
            except
              Log(tidLog_Survival, TimeToStr(Now) + ' Error in Update Mail Server Date.');
            end;
          end;
        perMonth :
          begin
            Log(tidLog_Survival, TimeToStr(Now) + ' Period(month) ');
            if fEcconomyRelay <> nil
              then
                try
                  Log('Relay', fEcconomyRelay.RenderText);
                except
                end;
          end;
        perYear :
          begin
            Log(tidLog_Survival, TimeToStr(Now) + ' Period(year) ');
            //UpdateRankingsInDirectory;
            //Log(tidLog_Survival, TimeToStr(Now) + ' Rankings done!');
            // Wrap Indexes
            if fPurgeIndex >= fTycoons.Count
              then fPurgeIndex := 0;
            if fUpdateNobIndex >= fTycoons.Count
              then fUpdateNobIndex := 0;
            if fUpdateDemoIndex >= fTycoons.Count
              then fUpdateDemoIndex := 0;
          end;
      end;
      if fEcconomyRelay <> nil
        then fEcconomyRelay.Update( self );
    end;

  procedure TWorld.TimeWarp;
    var
      PolTycoon : TTycoon;
      i : integer;
    begin
      Logs.Log( tidLog_TimeWarp, '--------------- Time Warp for ' + Name + ' on  ' + TimeToStr(Now) + ' ---------------');
      PolTycoon := GetTycoonByName('President of ' + GetWorldName);
      if PolTycoon.MasterRole <> nil
        then
          if PolTycoon.MasterRole.Name = PolTycoon.Name
          then
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': nobody is ' + PolTycoon.Name )
          else
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + PolTycoon.MasterRole.Name + ' is ' + PolTycoon.Name );
      PolTycoon := GetTycoonByName('Minister of Health');
      if PolTycoon.MasterRole <> nil
        then
          if PolTycoon.MasterRole.Name = PolTycoon.Name
          then
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': nobody has been appointed as ' + PolTycoon.Name )
          else
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + PolTycoon.MasterRole.Name + ' is ' + PolTycoon.Name );
      PolTycoon := GetTycoonByName('Minister of Education');
      if PolTycoon.MasterRole <> nil
        then
          if PolTycoon.MasterRole.Name = PolTycoon.Name
          then
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': nobody has been appointed as ' + PolTycoon.Name )
          else
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + PolTycoon.MasterRole.Name + ' is ' + PolTycoon.Name );
      PolTycoon := GetTycoonByName('Minister of Defense');
      if PolTycoon.MasterRole <> nil
        then
          if PolTycoon.MasterRole.Name = PolTycoon.Name
          then
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': nobody has been appointed as ' + PolTycoon.Name )
          else
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + PolTycoon.MasterRole.Name + ' is ' + PolTycoon.Name );
      PolTycoon := GetTycoonByName('Minister of Agriculture');
      if PolTycoon.MasterRole <> nil
        then
          if PolTycoon.MasterRole.Name = PolTycoon.Name
          then
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': nobody has been appointed as ' + PolTycoon.Name )
          else
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + PolTycoon.MasterRole.Name + ' is ' + PolTycoon.Name );
      PolTycoon := GetTycoonByName('Minister of Light Industry');
      if PolTycoon.MasterRole <> nil
        then
          if PolTycoon.MasterRole.Name = PolTycoon.Name
          then
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': nobody has been appointed as ' + PolTycoon.Name )
          else
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + PolTycoon.MasterRole.Name + ' is ' + PolTycoon.Name );
      PolTycoon := GetTycoonByName('Minister of Heavy Industry');
      if PolTycoon.MasterRole <> nil
        then
          if PolTycoon.MasterRole.Name = PolTycoon.Name
          then
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': nobody has been appointed as ' + PolTycoon.Name )
          else
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + PolTycoon.MasterRole.Name + ' is ' + PolTycoon.Name );
      PolTycoon := GetTycoonByName('Minister of Commerce');
      if PolTycoon.MasterRole <> nil
        then
          if PolTycoon.MasterRole.Name = PolTycoon.Name
          then
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': nobody has been appointed as ' + PolTycoon.Name )
          else
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + PolTycoon.MasterRole.Name + ' is ' + PolTycoon.Name );
      PolTycoon := GetTycoonByName('Minister of Housing');
      if PolTycoon.MasterRole <> nil
        then
          if PolTycoon.MasterRole.Name = PolTycoon.Name
          then
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': nobody has been appointed as ' + PolTycoon.Name )
          else
            Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + PolTycoon.MasterRole.Name + ' is ' + PolTycoon.Name );
      Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + Name + '''s population is ' + FloatToStr(Round(TotalPop)) + ' - LC: ' + FloatToStr(GetPopulation(pkLow)) + ' - MC: ' + FloatToStr(GetPopulation(pkMiddle)) + ' - HC: ' + FloatToStr(GetPopulation(pkHigh)));
      Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + Name + '''s QOL is ' + FloatToStr(GQOL) + '%');
      Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + Name + '''s Commerce General Index is ' + FloatToStr(GQOS) + '%');
      Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + Name + '''s unemployment is: LC: ' + FloatToStr(Unemployment[pkLow]) + '% - MC: ' + FloatToStr(Unemployment[pkMiddle]) + '% - HC: ' + FloatToStr(Unemployment[pkHigh]) + '%');
      Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + Name + '''s Residential Vacancies are ' + ': LC: ' + FloatToStr(Round(ResDemand[pkLow])) + ' - MC: ' + FloatToStr(Round(ResDemand[pkMiddle])) + ' - HC: ' + FloatToStr(Round(ResDemand[pkHigh])));
      Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + Name + '''s Residental Rent is: LC: ' + FloatToStr(ResRent[pkLow]) + '% - MC: ' + FloatToStr(ResRent[pkMiddle]) + '% - HC: ' + FloatToStr(ResRent[pkHigh]) + '%');
      Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + Name + '''s Residential Quality is: LC: ' + FloatToStr(ResQidx[pkLow]) + '% - MC: ' + FloatToStr(ResQidx[pkMiddle]) + '% - HC: ' + FloatToStr(ResQidx[pkHigh]) + '%');
      Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + Name + '''s Jobs Vacancies are ' + ': LC: ' + FloatToStr(Round(WorkDemand[pkLow])) + ' - MC: ' + FloatToStr(Round(WorkDemand[pkMiddle])) + ' - HC: ' + FloatToStr(Round(WorkDemand[pkHigh])));
      Logs. Log( tidLog_TimeWarp, TimeToStr(Now) + ': ' + Name + '''s Average Salaries are: LC: ' + FloatToStr(AvgSalary[pkLow]) + '% - MC: ' + FloatToStr(AvgSalary[pkMiddle]) + '% - HC: ' + FloatToStr(AvgSalary[pkHigh]) + '%');
      for i := 0 to (Towns.Count - 1) do
        if Towns[i] <> nil
        then
          TTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock).TimeWarp;
    end;

  function TWorld.GetTycoonByName( name : string ) : TTycoon;
    var
      i : integer;
    begin
      Tycoons.Lock;
      try
        i := 0;
        name := trim(uppercase(name));
        while (i < Tycoons.Count) and (uppercase(TTycoon(Tycoons[i]).Name) <> name) do
          inc( i );
        if i < Tycoons.Count
          then result := TTycoon(Tycoons[i])
          else result := nil;
      finally
        Tycoons.Unlock;
      end;
    end;

  function TWorld.GetTycoonById( Id : TTycoonId )  : TTycoon;
    var
      i : integer;
    begin
      Tycoons.Lock;
      try
        i := 0;
        while (i < Tycoons.Count) and (TTycoon(Tycoons[i]).Id <> Id) do
          inc( i );
        if i < Tycoons.Count
          then result := TTycoon(Tycoons[i])
          else result := nil;
      finally
        Tycoons.Unlock;
      end;
    end;

  function TWorld.GetCompanyByName( name : string ) : TCompany;
    var
      i : integer;
    begin
      Companies.Lock;
      try
        i := 0;
        name := uppercase(name);
        while (i < Companies.Count) and (uppercase(TCompany(Companies[i]).Name) <> name) do
          inc( i );
        if i < Companies.Count
          then result := TCompany(Companies[i])
          else result := nil;
      finally
        Companies.Unlock;
      end;
    end;

  function TWorld.GetTownByName( name : string ) : TTown;
    var
      i : integer;
    begin
      i := 0;
      while (i < Towns.Count) and (TTown(Towns[i]).Name <> name) do
        inc( i );
      if i < Towns.Count
        then result := TTown(Towns[i])
        else result := nil;
    end;

  function TWorld.GetTownById( Id : TTownId ) : TTown;
    var
      i : integer;
    begin
      i := 0;
      while (i < Towns.Count) and (TTown(Towns[i]).Id <> Id) do
        inc( i );
      if i < Towns.Count
        then result := TTown(Towns[i])
        else result := nil;
    end;

  function TWorld.GetClusterByName( name : string ) : TCluster;
    begin
      try
        result := TCluster(TheClassStorage.ClassById[tidClassFamily_Clusters, name]);
      except
        result := nil;
      end
    end;

  function TWorld.GetCircuitById( Id : integer ) : TCircuitMap;
    var
      i : integer;
    begin
      i := 0;
      while (i < fCircuits.Count) and (TCircuitMap(fCircuits[i]).Id <> Id) do
        inc( i );
      if i < fCircuits.Count
        then result := TCircuitMap(fCircuits[i])
        else result := nil;
    end;

  function TWorld.GetAwakenTycoon( Id : TTycoonId ) : TAwakenTycoon;
    var
      i : integer;
    begin
      fAwakenTycoons.Lock;
      try
        i := 0;
        while (i < fAwakenTycoons.Count) and (TAwakenTycoon(fAwakenTycoons[i]).fTycoon.Id <> Id) do
          inc( i );
        if i < fAwakenTycoons.Count
          then result := TAwakenTycoon(fAwakenTycoons[i])
          else result := nil;
      finally
        fAwakenTycoons.Unlock;
      end;                                    
    end;

  function TWorld.GetTycoonCollection : TLockableCollection;
    begin
      result := fTycoons;
    end;

  function TWorld.NewFacility( FacilityId : string; CompanyId : TCompanyId; x, y : integer; out Facility : TFacility ) : TErrorCode;

    function TranscendenceIsOk( Company : TCompany; MetaFacility : TMetaFacility ) : boolean;
      begin
        result := (Company = nil) or (Company.Owner = nil) or
                  (not Company.Owner.WillTranscend) or
                  (Company.Owner.WillTranscend and not MetaFacility.TypicalStage.MetaBlock.IsTransBlock)
      end;

    var
      MetaFacility : TMetaFacility;
      Company      : TCompany;
      Town         : TTown;
      //aux          : string;
      //count        : integer;
      refundable     : boolean;
      PrevUniqueness : TUniquenessMask;
    begin
      // Facilities.Lock; // Attepmt to fix deadlock 9/30/00
      try
        //Lock;
        try
          try
            Company := GetCompany( CompanyId );
            MetaFacility := TMetaFacility(TheClassStorage.ClassById['Facilities', FacilityId]);
            if (MetaFacility <> nil)
               and (Company <> nil)
               and ((Company.Owner = nil) or (Company.Owned and (Company.Owner.Level <> nil) and ((MetaFacility.SlotCount = 0) or (Company.Owner.FacCount < Company.Owner.Level.FacLimit))))
               and (Company.UniquenessMask and MetaFacility.UniquenessMask = 0)
               or  (CompanyId = 0)
              then
                begin
                  //MetaFacility := TMetaFacility(TheClassStorage.ClassById['Facilities', FacilityId]);
                  if (MetaFacility <> nil) and ((MetaFacility.Cluster = nil) or (MetaFacility.Cluster = Company.Cluster)) and TranscendenceIsOk( Company, MetaFacility )
                    then
                      begin
                        // To avoid two facilities to overlap in the map...
                        fBuildLock.Enter;
                        try
                          if AreaIsClear( x, y, MetaFacility.XSize, MetaFacility.YSize, Company )// and ()
                            then
                              if not fFightFacColonies or FacAllowedInClear(MetaFacility, x, y, Company)
                                then
                                  if (mfcIgnoreZoning in MetaFacility.Options) or MatchesZone(x, y, MetaFacility.XSize, MetaFacility.YSize, MetaFacility.ZoneType)
                                    then
                                      begin
                                        Logs.Log( tidLog_Survival, TimeToStr(Now) + ': Instantiating Facility ' + FacilityId );
                                        fWorldLock.Enter;
                                        try
                                          Facility := MetaFacility.Instantiate;
                                        finally
                                          fWorldLock.Leave;
                                        end;
                                        if Facility <> nil
                                          then
                                            begin
                                              fWorldLock.Enter;
                                              try
                                                Facility.xPos := x;
                                                Facility.yPos := y;
                                                Facility.CreationDate := GetVirtualTimeAbs;
                                              finally
                                                fWorldLock.Leave;
                                              end;
                                              if Company <> nil
                                                then
                                                  begin
                                                    Logs.Log( tidLog_Survival, TimeToStr(Now) + ': Reporting Facility ' + FacilityId );
                                                    PrevUniqueness := Company.UniquenessMask;
                                                    Company.FacilityCreated( Facility );
                                                    if PrevUniqueness <> Company.UniquenessMask
                                                      then InvalidateCache( Company, false );
                                                    Logs.Log( tidLog_Survival, TimeToStr(Now) + ': End Reporting Facility ' );
                                                    fWorldLock.Enter;
                                                    try
                                                      if Company.Owner <> nil
                                                        then
                                                          begin
                                                            try
                                                              Company.Owner.Favorites.RDONewItem( '', fvkLink, Facility.Name, ComposeLinkCookie( Facility.Name, Facility.xPos, Facility.yPos, true ) );
                                                            except
                                                              Logs.Log( tidLog_Survival, TimeToStr(Now) + ': ERROR adding favorites link!' );
                                                            end;
                                                          end;
                                                    finally
                                                      fWorldLock.Leave;
                                                    end;
                                                  end;
                                              if mfcInTown in MetaFacility.Options
                                                then
                                                  begin
                                                    Logs.Log( tidLog_Survival, TimeToStr(Now) + ': Sending Event.' );
                                                    Town := NearestTown( x, y );
                                                    Logs.Log( tidLog_Survival, TimeToStr(Now) + ': Entering WorldLock' );
                                                    refundable := (Town <> nil) and (Town.Mayor = nil) or (Town.Mayor.SuperRole = nil) or AreaIsZoned(x, y, MetaFacility.XSize, MetaFacility.YSize);
                                                    fWorldLock.Enter;
                                                    try
                                                      Facility.Town := Town;
                                                      if not refundable
                                                        then Facility.ReportTrouble(facForbiddenZone);

                                                      // Tutorial Stuff
                                                      Facility.NotifyNewBlockToOwners;
                                                    finally
                                                      fWorldLock.Leave;
                                                      Logs.Log( tidLog_Survival, TimeToStr(Now) + ': WorldLock exit.' );
                                                    end;
                                                    if (Company.Owner <> nil) and (Town <> nil)
                                                      then
                                                        SendEvent(
                                                          TFacEvent.Create(
                                                            VirtualTimeAbs,
                                                            VirtualTime,
                                                            60 + round(MetaFacility.Price/100000) ,
                                                            0,
                                                            MetaFacility.Id,
                                                            CloneMultiString( mtidFacBuilt ),
                                                            Company.Owner.Name,
                                                            Town.Name,
                                                            //NullString, //Company.Owner.Name + ' built %s near ' + Town.Name + '.',
                                                            '',
                                                            'http://local.asp?frame_Id=MapIsoView&frame_Action=SELECT&x=' + IntToStr(x) + '&y=' + IntToStr(y) ));
                                                    Logs.Log( tidLog_Survival, TimeToStr(Now) + ': End Sending Event.' );
                                                  end
                                                else
                                                  begin
                                                    {$IFNDEF NOCACHE}
                                                    Logs.Log( tidLog_Survival, TimeToStr(Now) + ': Caching Facility ' + FacilityId );
                                                    CacheObject( Facility, noKind, noInfo );
                                                    Logs.Log( tidLog_Survival, TimeToStr(Now) + ': End Caching Facility ' + FacilityId );
                                                    {$ENDIF}
                                                  end;
                                              Logs.Log( tidLog_Survival, TimeToStr(Now) + ': Inserting Facility in NewMeat' );
                                              fNewMeat.Insert( Facility );
                                              Logs.Log( tidLog_Survival, TimeToStr(Now) + ': Updating map' );
                                              UpdateInMap( Facility );
                                              Logs.Log( tidLog_Survival, TimeToStr(Now) + ': Refreshing area' );
                                              RefreshArea( x, y, MetaFacility.xSize, MetaFacility.ySize );
                                              if (Company <> nil) and (Company.Owner <> nil)
                                                then
                                                  begin
                                                    //Logs.Log( tidLog_Survival, TimeToStr(Now) + ': Dispatching useless event to Owner' );
                                                    //NewBlockMsg.Msg   := msgKernel_NewBlock;
                                                    //NewBlockMsg.Block := Facility.CurrBlock;
                                                    //Company.Owner.Dispatch( NewBlockMsg );
                                                  end;
                                              result := NOERROR;
                                            end
                                          else result := ERROR_CannotInstantiate
                                      end
                                    else result := ERROR_ZoneMissmatch
                                else result := ERROR_BuildingTooClose
                            else result := ERROR_AreaNotClear;
                        finally
                          fBuildLock.Leave;
                        end;
                      end
                    else result := ERROR_UnknownClass;
                end
              else
                if (Company <> nil) and (Company.Owner <> nil) and Company.Owned and (Company.Owner.Level <> nil)
                  then result := ERROR_TooManyFacilities
                  else result := ERROR_UnknownCompany;
          except
            on EClassStorageException do
              result := ERROR_UnknownClass;
            on E : Exception do
              begin
                result := ERROR_Unknown;
                Logs.Log( tidLog_Survival, TimeToStr(Now) + '- New Facilily, exception raised: ' + E.Message );
              end;
          end;
        finally
          //Unlock;
        end;
      finally
        // Facilities.Unlock;
      end;
    end;

  function TWorld.DelFacility( x, y : integer ) : TErrorCode;
    var
      Facility : TFacility;
    begin
      try
        Facility := FacilityAt( x, y );
        if Facility <> nil
          then
            begin
              if (Facility.Company <> nil) and Facility.CheckOpAuthenticity
                then
                  begin
                    DeleteFacility( Facility );
                    result := NOERROR;
                  end
                else result := ERROR_Unknown;
            end
          else
            begin
              result := ERROR_FacilityNotFound;
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Facility not found, x: ' + IntToStr(x) + ' y: ' + IntToStr(y) );
            end;
      except
        result := ERROR_Unknown;
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Exception deleting facility, x: ' + IntToStr(x) + ' y: ' + IntToStr(y) );
      end;
    end;

  function TWorld.FacilityAt( x, y : integer ) : TFacility;
    begin
      csMapLock.Enter; //BeginRead( INFINITE );
      try
        result := ObjectMap[x, y];
      finally
        csMapLock.Leave; //EndRead;
      end;
    end;

  function TWorld.GetCompany( CompanyId : TCompanyId ) : TCompany;
    var
      i : integer;
    begin
      fCompanies.Lock;
      try
        i := 0;
        while (i < fCompanies.Count) and (TCompany(fCompanies[i]).Id <> CompanyId) do
          inc( i );
        if i < fCompanies.Count
          then result := TCompany(fCompanies[i])
          else result := nil;
      finally
        fCompanies.Unlock;
      end;
    end;

  function TWorld.GetObjectsInArea( x, y, dx, dy : integer ) : TObjectReport;

    function ProfitToStr( Facility : TFacility ) : string;
      begin
        if Facility.MoneyDelta < 0
          then result := '1'
          else result := '0';
      end;

    function OptionsToStr(Facility : TFacility) : string;
      var
        opt : byte;
      begin
        opt := (1 + (Facility.UpgradeLevel div 10)) shl 4;
        if Facility.MoneyDelta < 0
          then opt := opt or 1;
        result := IntToStr(opt);
      end;

    function CompanyToStr( Company : TCompany ) : string;
      begin
        if Company <> nil
          then result := IntToStr(Company.Id)
          else result := '0';
      end;

    function TycoonToStr( Company : TCompany ) : string;
      begin
        if (Company <> nil) and (Company.Owner <> nil)
          then result := IntToStr(Company.Owner.Id)
          else result := '0';
      end;

    var
      xi, yi   : integer;
      i        : integer;
      Facility : TFacility;
      Found    : TCollection;
    begin
      try
        csMapLock.Enter; //BeginRead( INFINITE );
        try
          Found := TCollection.Create( 0, rkUse );
          for xi := x to x + dx do
            for yi := y to y + dy do
              begin
                Facility := ObjectMap[xi, yi];
                if (Facility <> nil) and (Found.IndexOf( Facility ) = NoIndex)
                  then Found.Insert( Facility );
              end;
        finally
          csMapLock.Leave; //EndRead;
        end;
        result := '';
        for i := 0 to pred(Found.Count) do
          with TFacility(Found[i]) do
            begin
              if result <> ''
                then result := result + LineBreak;
              result :=
                result +
                IntToStr(VisualClass) + LineBreak +
                TycoonToStr(Company) + LineBreak +
                OptionsToStr(TFacility(Found[i])) + LineBreak + //ProfitToStr(TFacility(Found[i])) + LineBreak +
                IntToStr(xPos) + LineBreak +
                IntToStr(yPos);
            end;
        Found.Free;
      except
        result := '';
      end;
    end;

  procedure TWorld.InitClusters;
    var
      Cluster : TCluster;
      i       : integer;
    begin
      for i := 0 to pred(TheClassStorage.ClassCount[tidClassFamily_Clusters]) do
        begin
          Cluster := TCluster(TheClassStorage.ClassByIdx[tidClassFamily_Clusters, i]);
          if Cluster.Company = nil
            then
              begin
                inc( fLastCompId );
                Cluster.Company := TCompany.Create( fLastCompId );
                Cluster.Company.Name := Cluster.Id;
                fCompanies.Insert( Cluster.Company );
                Cluster.Company.Cluster := Cluster;
              end;
        end;
    end;

  procedure TWorld.InsertTown( Town : TTown );
    begin
      fTowns.Insert( Town );
    end;

  function TWorld.FacilitiesIn( x1, y1, x2, y2 : integer) : boolean;
    var
      x, y : integer;
    begin
      result := false;
      csMapLock.Enter; //BeginRead( INFINITE );
      try
        x := x1;
        while not result and (x <= x2) do
          begin
            y := y1;
            while not result and (y <= y2) do
              begin
                result := ObjectMap[x, y] <> nil;
                inc(y);
              end;
            inc(x);
          end;
      finally
        csMapLock.Leave; //EndRead;
      end;
    end;

  function TWorld.FacAllowsEffect( x, y : integer; FxId : word; FxCreator : string ) : boolean;
    var
      Fac : TFacility;
    begin
      Fac := FacilityAt( x, y );
      if Fac <> nil
        then
          begin
            Fac.Lock;
            try
              result := Fac.EffectId = fxNone
            finally
              Fac.Unlock;
            end;
          end
        else result := false;
    end;

  function TWorld.CircuitAllowsEffect( CircId : TCircuitId; x1, y1, x2, y2 : integer; FxId : word; FxStrength : single; FxCreator : string ) : boolean;
    begin
      result := true;
    end;

  function TWorld.TycoonAllowsEffect( TycoonName : widestring; FxId : word; FxCreator : string ) : boolean;
    var
      Tycoon : TTycoon;
    begin
      Tycoon := GetTycoonByName( TycoonName );
      if Tycoon <> nil
        then
          begin
            Tycoon.Lock;
            try
              result := Tycoon.EffectId = fxNone;
            finally
              Tycoon.Unlock;
            end;
          end
        else result := false;
    end;

  function TWorld.CreateFacEffect( x, y : integer; FxId : word; FxStrength : single; FxCreator : string ) : TErrorCode;
    var
      Fac : TFacility;
    begin
      Fac := FacilityAt( x, y );
      if Fac <> nil
        then
          begin
            Fac.Lock;
            try
              if Fac.EffectId = fxNone
                then
                  begin
                    Fac.EffectId       := FxId;
                    Fac.EffectProgress := 0;
                    Fac.EffectCreator  := FxCreator;
                    Fac.EffectStrength := FxStrength;
                    result := FX_NOERROR;
                  end
                else result := FX_ERROR_Busy;
            finally
              Fac.Unlock;
            end;
          end
        else result := FX_ERROR_Unknown
    end;

  function TWorld.CreateCircuitEffect( CircId : TCircuitId; x1, y1, x2, y2 : integer; FxId : word; FxStrength : single; FxCreator : string ) : TErrorCode;
    begin
      result := FX_NOERROR;
      case FxId of
        fxDemolish :
          RDOWipeCircuit( CircId, IBBreaker, x1, y1, x2, y2 );
      end;
    end;

  function TWorld.CreateTycoonEffect( TycoonName : widestring; FxId : word; FxStrength : single; FxCreator : string ) : TErrorCode;
    var
      Tycoon : TTycoon;
    begin
      Tycoon := GetTycoonByName( TycoonName );
      if Tycoon <> nil
        then
          begin
            Tycoon.Lock;
            try
              if Tycoon.EffectId = fxNone
                then
                  begin
                    Tycoon.EffectId       := FxId;
                    Tycoon.EffectCreator  := FxCreator;
                    Tycoon.EffectProgress := 0;
                    Tycoon.EffectStrength := FxStrength;
                    result := FX_NOERROR;
                  end
                else result := FX_ERROR_Busy
            finally
              Tycoon.Unlock;
            end;
          end
        else result := FX_ERROR_Unknown
    end;

  procedure TWorld.CloneFacility( Source : TFacility; Options : TCloningOptions );
    var
      Tycoon   : TTycoon;
      Company  : TCompany;
      Facility : TFacility;
      cmpIdx   : integer;
      facIdx   : integer;
      Msg      : TMsgFacCloned;
    begin
      //Facilities.Lock;
      try
        if (Source <> nil) and (Source.Company <> nil) and (Source.Company.Owner <> nil)
          then
            begin
              Tycoon  := Source.Company.Owner;
              Msg.Msg := msgKernel_FacilityCloned;
              Msg.Fac := Source;
              try
                Tycoon.Dispatch(Msg);
              except
                // >> log error..
              end;
              Tycoon.Companies.Lock;
              try
                for cmpIdx := pred(Tycoon.Companies.Count) downto 0 do
                  begin
                    Company := TCompany(Tycoon.Companies[cmpIdx]);
                    if (not(clonLimitToCompany in Options.Scope) or (Company = Source.Company))
                      then
                        begin
                          Company.Facilities.Lock;
                          try
                            for facIdx := pred(Company.Facilities.Count) downto 0 do
                              begin
                                Facility := TFacility(Company.Facilities[facIdx]);
                                if (Facility <> Source) and (not(clonLimitToTown in Options.Scope) or (Facility.Town = Source.Town)) and (Facility.MetaFacility.FacId = Source.MetaFacility.FacId)
                                  then
                                    begin
                                      fWorldLock.Enter;
                                      try
                                        Facility.CopySettingsFrom(Source, Options.Options);
                                      finally
                                        fWorldLock.Leave;
                                      end;
                                    end
                              end;
                          finally
                            Company.Facilities.Unlock;
                          end;
                        end;
                  end;
              finally
                Tycoon.Companies.Unlock;
              end;
            end;
      finally
        //Facilities.Unlock;
      end;
    end;

  procedure TWorld.RDORegisterIS( HostName : widestring; Port : integer );
    begin
      if assigned(fOnRegisterIS)
        then fOnRegisterIS( HostName, Port );
    end;

  function TWorld.RDONewFacility( FacilityId : widestring; CompanyId : integer; x, y : integer ) : OleVariant;
    var
      Useless : TFacility;
    begin
      //Logs.LogMemReport(tidLog_Survival);
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' New Facility: ' + FacilityId + ' Company: ' + IntToStr(CompanyId) + ' x: ' + IntToStr(x) + ' y: ' + IntToStr(y) );
      result := NewFacility( FacilityId, CompanyId, x, y, Useless );
      Logs.Log( tidLog_Survival,  'OK!');
      //Logs.LogMemReport(tidLog_Survival);
    end;

  function TWorld.RDODelFacility( x, y : integer ) : OleVariant;
    begin
      //Logs.LogMemReport(tidLog_Survival);
      try
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Del Facility, x: ' + IntToStr(x) + ' y: ' + IntToStr(y) );
        result := DelFacility( x, y );
        Logs.Log( tidLog_Survival, 'OK!');
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Delete facilty error!');
      end;
      //Logs.LogMemReport(tidLog_Survival);
    end;

  function TWorld.RDODelFacilityOfCompany(x, y : integer; name : widestring) : OleVariant;
    var
      Company : TCompany;
      i       : integer;
      Fac     : TFacility;
      found   : boolean;
    begin
      result := false;
      //Logs.LogMemReport(tidLog_Survival);
      try
        Company := CompanyByName[name];
        if (Company <> nil) and Company.CheckOpAuthenticity
          then
            begin
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Del Facility of Company, x: ' + IntToStr(x) + ' y: ' + IntToStr(y) );
              found := false;
              Fac   := nil;
              Company.Lock;
              try
                i := 0;
                while (i < Company.Facilities.Count) and not found do
                  begin
                    Fac := TFacility(Company.Facilities[i]);
                    found := (Fac.XPos = x) and (Fac.YPos = y);
                    inc(i);
                  end;
              finally
                Company.Unlock;
              end;
              if found
                then
                  begin
                    DeleteFacility(Fac);
                    result := true;
                  end;
              Logs.Log( tidLog_Survival, 'OK!');
            end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Delete facilty of Company error!');
      end;
      //Logs.LogMemReport(tidLog_Survival);
    end;

  function TWorld.RDOFacilityAt( x, y : integer ) : OleVariant;
    begin
      try
        if (x >= 0) and (x <= fXSize) and (y >= 0) and (y <= fYSize)
          then result := integer(FacilityAt( x, y ))
          else result := integer(0);
      except
        result := integer(0);
        Logs.Log( tidLog_Survival, TimeToStr(Now) + Format('Error getting in FacilityAt (%d,%d)', [x, y]));
      end;
    end;

  function TWorld.RDOBlockAt( x, y : integer ) : OleVariant;
    var
      Fac : TFacility;
    begin
      try
        if (x >= 0) and (x <= fXSize) and (y >= 0) and (y <= fYSize)
          then
            begin
              Fac := FacilityAt(x, y);
              if Fac <> nil
                then result := integer(Fac.CurrBlock)
                else result := 0;
            end
          else result := integer(0);
      except
        result := integer(0);
        Logs.Log( tidLog_Survival, TimeToStr(Now) + Format('Error getting in FacilityAt (%d,%d)', [x, y]));
      end;
    end;

  function TWorld.RDOGetCompany( CompanyId : integer ) : OleVariant;
    begin
      result := integer(GetCompany( CompanyId ));
    end;

  function TWorld.RDOGetObjectsInArea( x, y, dx, dy : integer ) : OleVariant;
    begin
      try
        result := GetObjectsInArea( x, y, dx, dy );
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, TimeToStr(Now) + Format(' Error getting object in area (%d,%d) - (%d,%d)', [x, y, x + dx, y + dy]));
      end;
    end;

  function TWorld.RDOGetFacilityConnections( Facility : TFacility ) : OleVariant;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - GetFacilityConnections' );
      try
        WorldLock.Enter;
        try
          result := Facility.RDOGetConnectionReport;
        finally
          WorldLock.Leave;
        end;
      except
        result := '';
      end;
    end;
    
  procedure TWorld.RDOFacilityGotFocus( Facility : TFacility );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - GF' );
      try
        if Facility <> nil
          then Facility.GetFocus;
      except
      end;
    end;

  procedure TWorld.RDOFacilityLostFocus( Facility : TFacility );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - LF' );
      try
        if (Facility <> nil) and (Facility.CurrBlock.Facility = Facility)
          then Facility.LostFocus;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - LF Error' );
      end;
    end;
                      
  function TWorld.RDOConnectFacilities( Owner : TTycoon; Facility1, Facility2 : TFacility ) : OleVariant;
    begin
      //Logs.LogMemReport(tidLog_Survival);
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Connect Facilities' );
      WorldLock.Enter;
      try
        try
          if (Facility1.AccessLevelOf( Owner ) = acsFull) or (Facility2.AccessLevelOf( Owner ) = acsFull)
            then
              begin
                result := widestring(Facility1.ConnectTo( Facility2 ));
                if result = ''
                  then result := 'There is nothing to trade!';
              end
            else result := 'You are not allowed to do that!';
          Logs.Log( tidLog_Survival,  'OK!');
        except
          result := ERROR_Unknown;
          Logs.Log( tidLog_Survival,  'ERROR in Connect Facilities');
        end;
      finally
        WorldLock.Leave;
      end;
      //Logs.LogMemReport(tidLog_Survival);
    end;

  function TWorld.RDOAccountStatus( name, password : widestring ) : OleVariant;

    function CheckForPasswordChange( Tycoon : TTycoon ) : integer;
      var
        key, dspass : string;
        i           : integer;
        TycoonRole  : TTycoon;
      begin
        try
          DSLock;
          try
            key := GetUserPath( Tycoon.Name );
            if DirProxy.RDOSetCurrentKey( key )
              then
                begin
                  dspass := uppercase(DirProxy.RDOReadString( 'Password' ));
                  if dspass = uppercase(password)
                    then
                      begin
                        Tycoon.Password := dspass;
                        // Change also the password to the roles
                        for i := 0 to pred(Tycoon.Roles.Count) do
                          begin
                            TycoonRole := TTycoon(Tycoon.Roles[i]);
                            TycoonRole.Password := dspass;
                          end;
                        result := ACCOUNT_Valid;
                        Logs.Log( 'Excentric', DateToStr(Now) + ' Password change for ' + Name + ' effective.' );
                      end
                    else result := ACCOUNT_InvalidPassword
                end
              else result := ACCOUNT_UnknownError
          finally
            DSUnlock;
          end;
        except
          result := ACCOUNT_UnknownError
        end;
      end;

    var
      Tycoon : TTycoon;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - AccountsStatus' );
      try
        Tycoon := TycoonByName[name];
        if Tycoon = nil
          then result := ACCOUNT_Unexisting
          else
            if Tycoon.Password = uppercase(password)
              then result := ACCOUNT_Valid
              else result := CheckForPasswordChange( Tycoon ) //result := ACCOUNT_InvalidPassword
      except
        result := ACCOUNT_UnknownError;
      end
    end;

  function TWorld.RDONewTycoon( name, password : widestring ) : OleVariant;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' New Tycoon: ' + Name + ', ' + password );
      if (name <> '') and (name[1] <> #0) and ValidName(name)
        then
          try
            if TycoonByName[name] = nil
              then
                begin
                  NewTycoon( name, password );
                  MailServer.SendHTMLMessage(
                    'mailer@Global' + self.fName + '.net',
                    name + '@' + self.Name + '.net',
                    'Welcome to ' + self.Name,
                      WorldURL + tidMailMsgURL_WelcomeMsg +
                      '?WorldName=' + self.Name +
                      '&UserName=' + name);
                  result := NOERROR;
                end
              else result := ERROR_TycoonNameNotUnique
          except
            result := ERROR_Unknown;
          end
        else result := ERROR_InvalidUserName;
      Logs.Log( tidLog_Survival,  'OK!');
    end;

  function TWorld.RDOGetTycoon( name, password : widestring ) : OleVariant;
    var
      Tycoon : TTycoon;
    begin
      try
        Tycoon := TycoonByName[name];
        if (Tycoon <> nil) and ((lowercase(Tycoon.Password) = lowercase(password)) or (password = tidSystemPassword))
          then result := integer(Tycoon)
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TWorld.RDODelTycoon( name, password : widestring ) : OleVariant;
    var
      Tycoon : TTycoon;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Del Tycoon: ' + Name + ', ' + password );
      try
        Tycoon := TycoonByName[name];
        if Tycoon <> nil
          then
            if (Tycoon.Password = uppercase(password)) or (password = tidSystemPassword)
              then
                begin
                  RequestDeleteTycoon(Tycoon, true);
                  result := NOERROR;
                end
              else result := ERROR_InvalidPassword
          else result := ERROR_UnknownTycoon
      except
        result := ERROR_Unknown;
      end;
      Logs.Log( tidLog_Survival,  'OK!');
    end;

  function TWorld.RDOResetTycoon( name : widestring ) : OleVariant;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Reset Tycoon: ' + Name + ' (Failed Attempt!!)');
    end;

  function TWorld.RDOResetTycoonEx( name, password : widestring ) : OleVariant;
    var
      Tycoon : TTycoon;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Reset Tycoon: ' + Name );
      try
        Tycoon := TycoonByName[name];
        if (Tycoon <> nil) and (UpperCase(Tycoon.Password) = UpperCase(password))
          then
            begin
              SendEvent(
                TEvent.Create(
                  0,
                  VirtualTimeAbs,
                  VirtualTime,
                  10000 + Tycoon.FacCount,
                  1000,
                  InstantiateMultiString( mtidAccountReset, [Tycoon.Name] ),
                  //NullString, //Tycoon.Name + ' made an account reset.',
                  '', '' ));
              ResetTycoon( Tycoon );
              result := NOERROR;
            end
          else
            begin
              Logs.Log( tidLog_Survival,  'ERROR in Reset Tycoon (Trying to hack)');
              result := ERROR_UnknownTycoon;
            end;
        Logs.Log( tidLog_Survival,  'OK!');
      except
        result := ERROR_Unknown;
        Logs.Log( tidLog_Survival,  'ERROR in Reset Tycoon');
      end;
    end;

  procedure TWorld.RDOCacheTycoonLinks(name : widestring);
    var
      Tycoon  : TTycoon;
      Company : TCompany;
      i       : integer;
    begin
      Logs.Log(tidLog_Survival, TimeToStr(Now) + ' Cache Tycoon Links: ' + name);
      try
        Tycoon := TycoonByName[name];
        if Tycoon <> nil
          then
            begin
              ModelServerCache.UncacheObject(Tycoon, noKind, noInfo);
              ModelServerCache.CacheObject(Tycoon, noKind, noInfo);
              Tycoon.Lock;
              try
                for i := 0 to pred(Tycoon.Companies.Count) do
                  begin
                    Company := TCompany(Tycoon.Companies[i]);
                    ModelServerCache.UpdateObjectCache(Company, noKind, noInfo);
                    Company.CacheLinks;
                  end;
              finally
                Tycoon.Unlock;
              end;
            end;
      except
        Logs.Log( tidLog_Survival, 'Error in Cache Tycoon Links of ' + name);
      end;
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - OK! Cached tycoon Links of ' + name);
    end;

  procedure TWorld.RDOCacheCompanyLinks(name : widestring);
    var
      Company : TCompany;
    begin
      Logs.Log(tidLog_Survival, TimeToStr(Now) + ' Cache Company Links: ' + name);
      try
        Company := CompanyByName[name];
        if Company <> nil
          then Company.CacheLinks;
      except
        Logs.Log( tidLog_Survival, 'Error in Cache Company Links');
      end;
      Logs.Log( tidLog_Survival, TimeToStr(Now) + 'OK! Cache Company Links..');
    end;

  function TWorld.RDODelCompany( name : widestring ) : OleVariant;
    var
      Company : TCompany;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Del Company: ' + Name );
      try
        Company := CompanyByName[name];
        if (Company <> nil) and Company.Privated and Company.CheckOpAuthenticity
          then
            begin
              RequestDeleteCompany(Company);
              result := NOERROR;
            end
          else result := ERROR_UnknownCompany
      except
        result := ERROR_Unknown;
      end;
      Logs.Log( tidLog_Survival, 'OK!');
    end;

  function TWorld.RDOGetRidOfCompany(cpnName, tycoonName, password : widestring ) : OleVariant;
    var
      Tycoon  : TTycoon;
      Company : TCompany;
    begin
      try
        Tycoon  := TycoonByName[tycoonName];
        Company := CompanyByName[cpnName];
        if (Tycoon <> nil) and Tycoon.CheckOpAuthenticity and (Company <> nil) and (Uppercase(Tycoon.Password) = Uppercase(password))
          then Tycoon.Companies.Extract(Company);
      except
      end;
    end;

  function TWorld.RDOGetCompanyList( username : widestring ) : OleVariant;
    var
      Tycoon : TTycoon;
      report : string;
      i      : integer;
    begin
      try
        Tycoon := TycoonByName[username];
        if Tycoon <> nil
          then
            begin
              report := '';
              if Tycoon.Companies.Count > 0
                then
                  begin
                    Companies.Lock;
                    try
                      for i := 0 to pred(Tycoon.Companies.Count) do
                        with TCompany(Tycoon.Companies[i]) do
                          report := '[' + Name + ',' + IntToStr(Id) + ']'
                    finally
                      Companies.Unlock;
                    end
                  end
                else report := '[]';
              result := report;
            end
          else result := ERROR_UnknownTycoon;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TWorld.RDOGetCompanyOwnerRole( username : widestring; index : integer ) : OleVariant;
    var
      Tycoon : TTycoon;
    begin
      try
        Tycoon := TycoonByName[username];
        if (Tycoon <> nil) and (Tycoon.AllCompaniesCount > 0)
          then result := Tycoon.AllCompanies[index].Owner.Name
          else result := ERROR_Unknown;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TWorld.RDOGetCompanyName( username : widestring; index : integer ) : OleVariant;
    var
      Tycoon : TTycoon;
    begin
      try
        Tycoon := TycoonByName[username];
        if (Tycoon <> nil) and (Tycoon.AllCompaniesCount > 0)
          then result := Tycoon.AllCompanies[index].Name
          else result := ERROR_Unknown;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TWorld.RDOGetCompanyCluster( username : widestring; index : integer ) : OleVariant;
    var
      Tycoon : TTycoon;
    begin
      try
        Tycoon := TycoonByName[username];
        if (Tycoon <> nil) and (Tycoon.AllCompaniesCount > 0)
          then result := Tycoon.AllCompanies[index].Cluster.Id
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TWorld.RDOGetCompanyId( username : widestring; index : integer ) : OleVariant;
    var
      Tycoon : TTycoon;
    begin
      try
        Tycoon := TycoonByName[username];
        if (Tycoon <> nil) and (Tycoon.AllCompaniesCount > 0)
          then result := Tycoon.AllCompanies[index].Id
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TWorld.RDOGetCompanyFacilityCount( username : widestring; index : integer ) : OleVariant;
    var
      Tycoon : TTycoon;
    begin
      try
        Tycoon := TycoonByName[username];
        if (Tycoon <> nil) and (Tycoon.AllCompaniesCount > 0)
          then result := Tycoon.AllCompanies[index].Facilities.Count
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TWorld.RDOGetCompanyProfit( username : widestring; index : integer ) : OleVariant;
    var
      Tycoon : TTycoon;
    begin
      try
        Tycoon := TycoonByName[username];
        if (Tycoon <> nil) and (Tycoon.AllCompaniesCount > 0)
          then result := Tycoon.AllCompanies[index].Accounts.MasterAccount.Value
          else result := 0;
        result := 0;
      except
        result := 0;
      end;
    end;

  function TWorld.RDOGetCompanyCount( username : widestring ) : OleVariant;
    var
      Tycoon : TTycoon;
    begin             
      try
        Tycoon := TycoonByName[username];
        if Tycoon <> nil
          then result := Tycoon.AllCompaniesCount
          else result := 0
      except
        result := ERROR_Unknown;
      end;
    end;

  function TWorld.RDONewCompany( username, name, clustername : widestring ) : OleVariant;

    function TycoonIsNoble( Tycoon : TTycoon ) : boolean;
      begin
        try
          if (Tycoon.NobPoints > 0) or Tycoon.UpdateNobility
            then result := Tycoon.NobPoints >= 100
            else result := false;
        except
          result := false;
        end;
      end;

    var
      Tycoon  : TTycoon;
      Cluster : TCluster;
      Company : TCompany;
      prest   : integer;

    begin
      //Logs.LogMemReport(tidLog_Survival);
      name := trim(name);
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' New Company: ' + Name + ', ' + username + ', ' + clustername );
      if ValidName(name) and (Length(name) <= 50) // Long names not allowed.
        then
          try
            fNewCompanyLock.Enter;
            try
              if CompanyByName[name] = nil
                then
                  begin
                    Cluster := ClusterByName[clustername];
                    if Cluster <> nil
                      then
                        begin
                          Tycoon := TycoonByName[username];
                          if (Tycoon <> nil) and (Tycoon.Level <> nil) and ((Tycoon.Level.Tier >= Cluster.Tier) or (fBldMagnaIfNoble and TycoonIsNoble(Tycoon))) and (Tycoon.Companies.Count < MaxCompaniesAllowed) // >> Check this later..
                            then
                              begin
                                Company := NewCompany( name, Cluster, Tycoon );
                                if (Company <> nil) and (Tycoon.Companies.Count = 1)
                                  then
                                    begin
                                      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Add Cur. Item..');
                                      if Tycoon.CountItemsInCurriculum(crrKind_JoinedWorld) = 0
                                        then prest := 5
                                        else prest := 0;
                                      Tycoon.AddItemToCurriculum(
                                        TOpenItem.Create(
                                          '',
                                          crrKind_JoinedWorld,
                                          InstantiateMultiString( mtidJoinedWorld, [self.Name, fVirtYear] ),
                                          20,
                                          prest));
                                      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Done Cur. Item..');
                                    end;
                                if Company <> nil
                                  then result := '[' + Company.Name + ',' + IntToStr(Company.Id) + ']'
                                  else result := ERROR_InvalidCompanyId;
                              end
                            else result := ERROR_UnknownTycoon
                        end
                      else result := ERROR_UnknownCluster
                  end
                else result := ERROR_CompanyNameNotUnique
            finally
              fNewCompanyLock.Leave;
            end;
          except
            result := ERROR_Unknown;
          end
        else result := ERROR_InvalidCompanyId;
      Logs.Log( tidLog_Survival,  'OK! Created: ' + name);
      //Logs.LogMemReport(tidLog_Survival);
    end;

  function TWorld.RDOObjectStatusText( kind : TStatusKind; Id : TObjId; ToTycoon : TObjId ) : OleVariant;
    var
      Facility : TFacility;
    begin
      try
        if Id <> nil
          then
            begin
              Facility := TFacility(Id);
              Facility.Lock;
              try
                result := Facility.StatusText[kind, TTycoon(ToTycoon)];
              finally
                Facility.Unlock;
              end;
            end
          else result := '';
      except
        result := '';
      end;
    end;

  function TWorld.RDOAllObjectStatusText( Id : TObjId; ToTycoon : TObjId ) : OleVariant;
    var
      Facility : TFacility;
      kind     : TStatusKind;
    begin
      try
        if Id <> nil
          then
            begin
              Facility := TFacility(Id);     
              Facility.Lock;
              try         
                result := '';
                for kind := low(kind) to high(kind) do
                  result := result + Facility.StatusText[kind, TTycoon(ToTycoon)] + StatusTextSeparator;
              finally
                Facility.Unlock;
              end;
            end
          else result := '';
      except
        result := '';
      end;
    end;
    
  function TWorld.RDOContextStatusText( ToTycoon : TObjId; x, y : integer ) : OleVariant;
    var
      Town : TTown;
    begin
      try
        WorldLock.Enter;
        try
          Town := NearestTown( x, y );
          if Town <> nil
            then result := Town.GetContextStatusStr( ToTycoon )   
            else result := ''
        finally
          WorldLock.Leave;
        end;
      except
        result := '';
      end;
    end;

  function TWorld.RDOCreateCircuitSeg( CircuitId, TycoonId, x1, y1, x2, y2, cost : integer ) : OleVariant;
    var
      tmp        : integer;
      CircuitMap : TCircuitMap;
      ErrorCode  : TCircuitErrorCode;
      Tycoon     : TTycoon;
      OwnerId    : integer;
      tiles      : integer;
    begin
      //RoadLogs.CreateCircuitSeg(CircuitId, TycoonId, x1, y1, x2, y2);
      //Logs.LogMemReport(tidLog_Survival);
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' CreateCircuitSeg: ' + IntToStr(CircuitId) + ', ' + IntToStr(TycoonId) + ', ' + IntToStr(x1) + ', ' + IntToStr(y1) + ', ' + IntToStr(x2) + ', ' + IntToStr(y2) );
      fCircuits.Lock; //fWorldLock.Enter;
      try
        CircuitMap := CircuitById[CircuitId];
        if CircuitMap <> nil
          then
            try
              Tycoon := TTycoon(TycoonId);
              tiles  := abs(x2 - x1) + abs(y2 - y1);
              // To build a road you have to be at least entrepreneur
              if ObjectIs(TTycoon.ClassName, Tycoon) and (Tycoon.Level.Tier > 0) and (Tycoon.Budget - Tycoon.LoanAmount > cost) and ((fVirtYear - YearZero < 50) or Tycoon.CanBuildRoad(tiles))
                then
                  begin
                    OwnerId := Tycoon.Id;
                    CircuitMap.CreateSegment( x1, y1, x2, y2, OwnerId, ErrorCode );
                    case ErrorCode of
                      CIRCUIT_NOERROR :
                        begin
                          tmp := x1;
                          x1  := min( x1, x2 );
                          x2  := max( tmp, x2 );
                          tmp := y1;
                          y1  := min( y1, y2 );
                          y2  := max( tmp, y2 );
                          RefreshArea( x1, y1, x2 - x1 + 1, y2 - y1 + 1 );
                          Tycoon.GenMoney( -cost, accIdx_RoadConstruction );
                          result := NOERROR;
                          Tycoon.RoadBlocks := Tycoon.RoadBlocks + tiles;
                        end;
                      CIRCUIT_ERROR_InvalidSegment :
                        result := ERROR_CannotCreateSeg;
                      else
                        result := ERROR_Unknown;
                    end;
                    inc(fRoadRefreshDelay, 1); //fRoadRefreshDelay := 1;
                  end
                else result := ERROR_CannotCreateSeg;
            except
              result := ERROR_Unknown;
            end
          else result := ERROR_UnknownCircuit;
      finally
        fCircuits.Unlock; //fWorldLock.Leave;
      end;
      Logs.Log( tidLog_Survival,  'CreateCircuitSeg: OK!');
      //Logs.LogMemReport(tidLog_Survival);
    end;

  function TWorld.RDOBreakCircuitAt( CircuitId, TycoonId, x, y : integer ) : OleVariant;
    var
      CircuitMap  : TCircuitMap;
      ErrorCode   : TCircuitErrorCode;
      OwnerId     : integer;
      crRoadDelay : integer;
    begin
      //RoadLogs.BreakCircuitAt(CircuitId, TycoonId, x, y);
      //Logs.LogMemReport(tidLog_Survival);
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' BreakCircuit: ' + IntToStr(CircuitId) + ', ' + IntToStr(TycoonId) + ', ' + IntToStr(x) + ', ' + IntToStr(y) );
      fCircuits.Lock; //fWorldLock.Enter;
      crRoadDelay := fRoadRefreshDelay;
      fRoadRefreshDelay := 0;
      try
        CircuitMap := CircuitById[CircuitId];
        if CircuitMap <> nil
          then
            try
              OwnerId := TTycoon(TycoonId).Id;
              CircuitMap.BreakSegmentInPoint( x, y, OwnerId, ErrorCode );
              case ErrorCode of
                CIRCUIT_NOERROR :
                  begin
                    RefreshArea( x - 1, y - 1, 3, 3 );
                    result := NOERROR;
                  end;
                CIRCUIT_ERROR_AccessDenied :
                  result := ERROR_AccessDenied;
                else
                  result := ERROR_Unknown;
              end;
              if CircuitMap = fRoads
                then inc(crRoadDelay, 1); //fRoadRefreshDelay := 1;
            except
              result := ERROR_Unknown;
            end
          else result := ERROR_UnknownCircuit;
      finally
        fRoadRefreshDelay := crRoadDelay;
        fCircuits.Unlock; //fWorldLock.Leave;
      end;
      Logs.Log( tidLog_Survival,  'BreakCircuit: OK!');
      //Logs.LogMemReport(tidLog_Survival);
    end;

  function TWorld.RDOWipeCircuit( CircuitId, TycoonId, x1, y1, x2, y2 : integer ) : OleVariant;
    var
      CircuitMap : TCircuitMap;
      ErrorCode  : TCircuitErrorCode;
      OwnerId    : integer;
      x, y       : integer;
      area       : integer;
    begin
      area := abs(x1 - x2)*abs(y1 - y2);
      //Logs.LogMemReport(tidLog_Survival);
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' WipingCircuit: ' + IntToStr(CircuitId) + ', ' + IntToStr(TycoonId) + ', ' + IntToStr(x1) + ', ' + IntToStr(y1) + ', ' + IntToStr(x2) + ', ' + IntToStr(y2) );
      fCircuits.Lock; //fWorldLock.Enter;
      fRoadRefreshDelay := 0;
      try
        try
          CanonicalizeSeg( x1, y1, x2, y2 );
          OwnerId := TTycoon(TycoonId).Id;
          CircuitMap := CircuitById[CircuitId];
          if (CircuitMap <> nil) and (area < MaxWipeArea)
            then
              for y := y1 to y2 do
                for x := x1 to x2 do
                  try
                    CircuitMap.BreakSegmentInPoint( x, y, OwnerId, ErrorCode );
                  except
                  end
            else result := ERROR_UnknownCircuit;
          if CircuitMap = fRoads
            then fRoadRefreshDelay := 1;
          RefreshArea( x1, y1, x2 - x1 + 1, y2 - y1 + 1 );
          Logs.Log( tidLog_Survival,  'WipingCircuit: OK!');
          result := NOERROR;
        except
          Logs.Log( tidLog_Survival,  'ERROR in WipingCircuit');
          result := ERROR_Unknown;
        end;
      finally
        fCircuits.Unlock; //fWorldLock.Leave;
      end;
      //Logs.LogMemReport(tidLog_Survival);
    end;

  function TWorld.RDOSegmentsInArea( CircuitId, x1, y1, x2, y2 : integer ) : OleVariant;
    var
      CircuitMap : TCircuitMap;
    begin
      CircuitMap := CircuitById[CircuitId];
      if CircuitMap <> nil
        then
          try
            result := CircuitMap.SegsInArea( x1, y1, x2, y2 );
          except
            result := ERROR_Unknown;
          end
        else result := ERROR_UnknownCircuit;
    end;

  function TWorld.RDOWhoDidRoadAt(x, y : integer) : OleVariant;
    var
      CircuitMap : TCircuitMap;
      OwnerId    : integer;
      Tycoon     : TTycoon;
    begin
      Tycoon := nil;
      CircuitMap := CircuitById[cirRoads];
      if CircuitMap <> nil
        then
          try
            OwnerId := CircuitMap.GetRoadOwner(x, y);
            if OwnerId >= 0
              then Tycoon := TycoonById[OwnerId];
          except
          end;
      if Tycoon <> nil
        then result := Tycoon.Name
        else result := 'Unknown';
    end;

  function TWorld.RDOGetSurface( SurfaceId : widestring; x1, y1, x2, y2 : integer ) : OleVariant;

    function GetCargoId( SurfaceId : string ) : integer;
      var
        p : integer;
      begin
        try
          p := pos( 'CARGO.', SurfaceId );
          if p > 0
            then
              begin
                Delete( SurfaceId, 1, length( 'CARGO.' ));
                result := StrToInt(SurfaceId);
              end
            else result := NoIndex;
        except
          result := NoIndex;
        end;
      end;

    var
      CargoId : integer;
      Layer   : TCargoLayer;
      Surface : TSurface;
      image   : string;
    begin
      try
        if SurfaceId = tidSurface_Zones
          then
            if CompressMap( fZones, Rect( x1, y1, x2, y2 ), image, 1 )
              then result := image
              else result := ERROR_Unknown
          else
            if SurfaceId = tidSurface_Towns
              then
                if CompressMap( self, Rect( x1, y1, x2, y2 ), image, 1 )
                  then result := image
                  else result := ERROR_Unknown
              else
                begin
                  CargoId := GetCargoId( SurfaceId );
                  if CargoId <> NoIndex
                    then
                      begin
                        Layer := fCargoSystem.Layer[CargoId];
                        if Layer <> nil
                          then
                            if CompressMap( Layer, Rect( x1, y1, x2, y2 ), image, 2 )
                              then result := image
                              else result := ERROR_Unknown
                          else result := ERROR_Unknown;
                      end
                    else
                      begin
                        Surface := fSurfacePool.Surface[SurfaceId];
                        if Surface <> nil
                          then
                            if CompressMap( Surface, Rect( x1, y1, x2, y2 ), image, 1 )
                              then result := image
                              else result := ERROR_Unknown
                          else result := ERROR_Unknown;
                      end;
                end;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TWorld.RDODefineZone( TycoonId, ZoneId, x1, y1, x2, y2 : integer ) : OleVariant;
    const
      ZoningCost = 200;

    var
      Tycoon : TTycoon;

    function ZonerHasAccess( x, y : integer ) : boolean;
      var
        Town : TTown;
      begin
        Town   := NearestTown( x, y );
        result := Tycoon.ContainsRole( Town.Mayor.Id );
      end;

    var
      x, y      : integer;
      cost      : TMoney;
      Fac       : TFacility;
      zRole     : boolean;
      RchMatrix : TReachMatrix;
      ZonedFacs : TCollection;
    begin
      //Logs.LogMemReport(tidLog_Survival);
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Defining Zone: ' + IntToStr(ZoneId) + ', ' + IntToStr(TycoonId) + ', ' + IntToStr(x1) + ', ' + IntToStr(y1) + ', ' + IntToStr(x2) + ', ' + IntToStr(y2) );
      if TZoneType(ZoneId) <> znNone
        then RchMatrix := fRoads.GetReachMatrix(x1, y1, x2, y2, 7{RoadTolerance})
        else RchMatrix := nil;
      ZonedFacs := TCollection.Create(0, rkUse);
      fWorldLock.Enter;
      try
        try
          Tycoon := TycoonById[TycoonId];
          if Tycoon <> nil
            then
              begin
                Logs.Log( tidLog_Survival, TimeToStr(Now) + Format('Zoner: %s, Id: %d', [Tycoon.Name, TycoonId]));
                // Zone and collect facilities
                fZones.Lock;
                try
                  zRole := Tycoon.MasterRole.HasZonerRole;
                  cost  := 0;
                  for y := y1 to y2 do
                    for x := x1 to x2 do
                      if (fZones[y, x] <> TZoneType(ZoneId)) and ((TZoneType(ZoneId) = znNone) or (RchMatrix[x - x1, y - y1] <> rchNone)) and (zRole or ZonerHasAccess( x, y )) //and (LandClassOf(GroundMap[x, y]) <> lncZoneD)
                        then
                          begin
                            Fac := FacilityAt( x, y );
                            if (Fac <> nil) and (Fac.Company <> nil) and Fac.Company.Privated and not (mfcIgnoreZoning in Fac.MetaFacility.Options)
                              then
                                begin
                                  if Protocol.ZoneMatches( ZoneId, Fac.MetaFacility.ZoneType )
                                    then
                                      begin
                                        Fac.ToBeDemolished := 0;
                                        ZonedFacs.Delete(Fac);
                                      end
                                    else
                                      if ZonedFacs.IndexOf(Fac) = noIndex
                                        then ZonedFacs.Insert(Fac);
                                end;
                            fZones[y, x] := TZoneType(ZoneId);
                            cost := cost + ZoningCost;
                          end;
                  Tycoon.GenMoney( -cost, accIdx_Construction );
                  RefreshArea( x1, y1, x2 - x1, y2 - y1 );
                  result := NOERROR;
                finally
                  fZones.Unlock;
                end;
                // Adjust facilities zoning
                while ZonedFacs.Count > 0 do
                  begin
                    Fac := TFacility(ZonedFacs[0]);
                    ZonedFacs.AtDelete(0);
                    if not MatchesZone(Fac.xPos, Fac.yPos, Fac.MetaFacility.XSize, Fac.MetaFacility.YSize, Fac.MetaFacility.ZoneType)
                      then ReportZoning(Tycoon, Fac);
                  end;
              end
            else result := ERROR_Unknown;
        except
          result := ERROR_Unknown;
        end;
      finally
        fWorldLock.Leave;
        RchMatrix.Free;
        ZonedFacs.Free;
      end;
      Logs.Log( tidLog_Survival,  'OK!');
      //Logs.LogMemReport(tidLog_Survival);
    end;

  procedure TWorld.RDOAwakeTycoon( TycoonId : integer );
    var
      AwakenTycoon : TAwakenTycoon;
      Tycoon       : TTycoon;
      i            : integer;
    begin
      //Logs.LogMemReport(tidLog_Survival);
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' AwakeTycoon: ' + IntToStr(TycoonId) );
      fWorldLock.Enter;
      try
        try
          AwakenTycoon := GetAwakenTycoon( TycoonId );
          if AwakenTycoon = nil
            then
              begin
                Tycoon := TycoonById[TycoonId];
                if Tycoon <> nil
                  then
                    begin
                      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Tycoon logged on: ' + Tycoon.Name + '(' + IntToStr(TycoonId) + ')' );
                      try
                        AwakenTycoon := TAwakenTycoon.Create( Tycoon );
                        fAwakenTycoons.Insert( AwakenTycoon );
                        fActorPools.Lock;
                        try
                          for i := 0 to pred(fActorPools.Count) do
                            TServerActorPool(fActorPools[i]).AddViewer( AwakenTycoon );
                        finally
                          fActorPools.Unlock;
                        end;
                      finally
                        Tycoon.Awake;
                      end;
                      Logs.Log( tidLog_Survival,  'OK!');
                    end;
              end;
          if AwakenTycoon <> nil
            then inc( AwakenTycoon.fTimesAwaken );
        except
          Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error Awaking Tycoon');
        end;
      finally
        fWorldLock.Leave;
      end;
      Logs.Log( tidLog_Survival,  'OK!');
      //Logs.LogMemReport(tidLog_Survival);
    end;

  procedure TWorld.RDOSleepTycoon( TycoonId : integer );
    var
      Tycoon       : TTycoon;
      AwakenTycoon : TAwakenTycoon;
      i            : integer;
      ViewPort     : TTycoonViewport;
    begin
      //Logs.LogMemReport(tidLog_Survival);
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' SleepTycoon: ' + IntToStr(TycoonId) );
      fWorldLock.Enter;
      try
        try
          Tycoon := TycoonById[TycoonId];
          if Tycoon <> nil
            then
              begin
                Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Tycoon logged off: ' + Tycoon.Name + '(' + IntToStr(TycoonId) + ')' );
                Tycoon.Sleep;
                Logs.Log( tidLog_Survival,  'OK!');
              end;
          AwakenTycoon := GetAwakenTycoon( TycoonId );
          if AwakenTycoon <> nil
            then
              begin
                dec( AwakenTycoon.fTimesAwaken );
                if AwakenTycoon.fTimesAwaken = 0
                  then
                    begin
                      fActorPools.Lock;
                      try
                        for i := 0 to pred(fActorPools.Count) do
                          TServerActorPool(fActorPools[i]).DelViewer( AwakenTycoon );
                      finally
                        fActorPools.Unlock;
                      end;

                      // Save position in the cookies

                      Viewport := AwakenTycoon.Viewport[0];
                      if (Viewport <> nil) and (Viewport.x1 <> Viewport.x2) and (Viewport.y1 <> Viewport.y2)
                        then
                          begin
                            Tycoon.Cookie[tidLastViewX + '0'] := IntToStr((Viewport.x1 + Viewport.x2) div 2);
                            Tycoon.Cookie[tidLastViewY + '0'] := IntToStr((Viewport.y1 + Viewport.y2) div 2);
                          end;

                      fAwakenTycoons.Delete( AwakenTycoon );
                    end;
              end;
        Logs.Log( tidLog_Survival,  'OK!');
        except
          on E : Exception do
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error SleepTycoon ' + E.Message );
        end;
      finally
        fWorldLock.Leave;
      end;
      //Logs.LogMemReport(tidLog_Survival);
    end;

  procedure TWorld.RDOSetTycoonViewport( TycoonId, ViewportId, x1, y1, x2, y2 : integer );
    var
      AwakenTycoon : TAwakenTycoon;
      Viewport     : TTycoonViewport;
    begin
      try
        AwakenTycoon := GetAwakenTycoon( TycoonId );
        if AwakenTycoon <> nil
          then
            begin
              if (x1 <> x2) and (y1 <> y2)
                then
                  begin
                    Viewport    := AwakenTycoon.Viewport[ViewportId];
                    Viewport.x1 := x1;
                    Viewport.y1 := y1;
                    Viewport.x2 := x2;
                    Viewport.y2 := y2;
                  end;
              {
              if (x1 <> x2) and (y1 <> y2)
                then
                  begin
                    AwakenTycoon.fTycoon.Cookie[tidLastViewX + IntToStr(ViewportId)] := IntToStr((x1 + x2) div 2);
                    AwakenTycoon.fTycoon.Cookie[tidLastViewY + IntToStr(ViewportId)] := IntToStr((y1 + y2) div 2);
                  end;
              }
            end;
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error setting ViewPort of ' + IntToStr(TycoonId) + ' ' + E.Message);
      end;
    end;

  function TWorld.RDOGetTycoonCookies(TycoonId : integer) : OleVariant;
    var
      Tycoon : TTycoon;
    begin
      fWorldLock.Enter;
      try
        try
          Tycoon := TycoonById[TycoonId];
          if Tycoon <> nil
            then result := Tycoon.Cookies
            else result := '';
        except
          result := '';
        end;
      finally
        fWorldLock.Leave;
      end;
    end;

  function TWorld.RDOGetTycoonCookie( TycoonId : integer; CookieId : widestring ) : OleVariant;
    var
      Tycoon : TTycoon;
    begin
      fWorldLock.Enter;
      try
        try
          Tycoon := TycoonById[TycoonId];
          if Tycoon <> nil
            then
              if CookieId <> ''
                then result := Tycoon.Cookie[CookieId] // Get Specific Cookie
                else result := Tycoon.Cookies          // Get All Cookies
            else result := '';
        except
          result := '';
        end;
      finally
        fWorldLock.Leave;
      end;
    end;

  procedure TWorld.RDOSetTycoonCookie( TycoonId : integer; CookieId, CookieValue : widestring );
    var
      Tycoon : TTycoon;
    begin
      fWorldLock.Enter;
      try
        try
          Tycoon := TycoonById[TycoonId];
          if Tycoon <> nil
            then Tycoon.Cookie[CookieId] := CookieValue;
        except
        end;
      finally
        fWorldLock.Leave;
      end;
    end;

  procedure TWorld.RDOCloneFacility( x, y, options, useless, TycoonId : integer );
    var
      Facility : TFacility;
      Tycoon   : TTycoon;
      OptRec   : TCloningOptions;
    begin
      //Logs.LogMemReport(tidLog_Survival);
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' CloneFacility: ' + IntToStr(TycoonId) );
      try
        Facility := FacilityAt( x, y );
        Tycoon   := TycoonById[TycoonId];
        if (Facility <> nil) and (Tycoon <> nil) and (Facility.Company <> nil) and (Facility.Company.Owner.MasterRole = Tycoon.MasterRole)
          then
            begin
              OptRec.Scope := [];
              if options and cloneOption_SameCompany <> 0
                then OptRec.Scope := OptRec.Scope + [clonLimitToCompany];
              if options and cloneOption_SameTown <> 0
                then OptRec.Scope := OptRec.Scope + [clonLimitToTown];
              //CloneFacility(Facility, Options);
              OptRec.Options := options;
              QueueFacilityToClone(Facility, OptRec);
            end;
        Logs.Log( tidLog_Survival,  'OK!');
      except
        Logs.Log( tidLog_Survival,  'ERROR in CloneFacility');
      end;
      //Logs.LogMemReport(tidLog_Survival);
    end;

  function TWorld.RDOGetNearestTownHall( x, y : integer ) : OleVariant;
    var
      Town : TTown;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Getting nearest town hall.' );
      try
        Town := NearestTown( x, y );
        if Town <> nil
          then result := IntToStr(Town.xPos) + ',' + IntToStr(Town.yPos)
          else result := '';
        Logs.Log( tidLog_Survival,  'OK!');
      except
        Logs.Log( tidLog_Survival,  'ERROR in Getting nearest');
      end;
    end;

  function TWorld.RDOPickEvent( TycoonId : integer ) : OleVariant;
    var
      Tycoon : TTycoon;
      Event  : TEvent;
    begin
      try
        // fWorldLock.Enter;
        try
          try
            Tycoon := TycoonById[TycoonId];
            if Tycoon <> nil
              then
                begin
                  Event := Tycoon.PickEvent;
                  if Event <> nil
                    then
                      try
                        result := Event.Render
                      finally
                        Event.Free;
                      end
                    else result := '';
                end;
          except
            Logs.Log( tidLog_Survival, 'ERROR getting event (PickEvent)!');
          end;
        finally
          // fWorldLock.Leave;
        end;
      except
      end;
    end;

  function TWorld.RDOAssignLevel(tycoonName, sysPassword, Level : widestring) : OleVariant;
    var
      Tycoon      : TTycoon;
      TycoonLevel : TTycoonLevel;
    begin
      try
        Tycoon := TycoonByName[tycoonName];
        if (Tycoon <> nil) and (sysPassword = tidSystemPassword)
          then
            begin
              TycoonLevel := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, Level]);
              if TycoonLevel <> nil
                then
                  begin
                    Tycoon.Level := TycoonLevel;
                    result := true;
                  end
                else result := false;
            end
           else result := false;
      except
        result := false;
        Logs.Log( tidLog_Survival, 'ERROR in RDOAssignLevel!');
      end;
    end;

  function TWorld.RDOFireMayor(townName, sysPassword : widestring; reelect : integer) : OleVariant;
    var
      Town : TPoliticalTown;
    begin
      try
        Town := TPoliticalTown(TownByName[townName]);
        if (Town <> nil) and (sysPassword = tidSystemPassword)
          then
            begin
              Town.FireMayor;
              if reelect = 1
                then Town.ElectMayor;
              result := true;
            end
           else result := false;
      except
        result := false;
        Logs.Log( tidLog_Survival, 'ERROR in RDOFireMayor!');
      end;
    end;

  function TWorld.RDOCreateFacEffect( x, y : integer; FxId : word; FxStrength : single; FxCreator : widestring ) : OleVariant;
    begin
      result := CreateFacEffect( x, y, FxId, FxStrength, FxCreator );
    end;

  function TWorld.RDOCreateCircuitEffect( CircId : TCircuitId; x1, y1, x2, y2 : integer; FxId : word; FxStrength : single; FxCreator : widestring ) : OleVariant;
    begin
      result := CreateCircuitEffect( CircId, x1, y1, x2, y2, FxId, FxStrength, FxCreator );
    end;

  function TWorld.RDOCreateTycoonEffect( TycoonName : widestring; FxId : word; FxStrength : single; FxCreator : widestring ) : OleVariant;
    begin
      result := CreateTycoonEffect( TycoonName, FxId, FxStrength, FxCreator );
    end;

  function TWorld.RDOAbandonRoles(name, password : widestring) : OleVariant;
    var
      Tycoon : TTycoon;
    begin
      Logs.Log(tidLog_Survival, TimeToStr(Now) + ' Abandon role: ' + name);
      try
        Tycoon := TycoonByName[name];
        fWorldLock.Enter;
        try
          if (Tycoon <> nil) and ((lowercase(Tycoon.Password) = lowercase(password)) or (password = tidSystemPassword))
            then Tycoon.AbandomRoles;
        finally
          fWorldLock.Leave;
        end;
        result := true;
      except
        result := false;
        Logs.Log( tidLog_Survival, 'ERROR in RDOAbandonRoles!');
      end;
    end;

  procedure TWorld.RDOUpdateCurriculumItems( AllItems : wordbool );
    var
      Tycoon : TTycoon;
      i      : integer;
    begin
      fTycoons.Lock;
      try
        for i := 0 to pred(fTycoons.Count) do
          try
            Tycoon := TTycoon(fTycoons[i]);
            Tycoon.PublishCurriculum( AllItems );
          except
            Logs.Log( tidLog_Survival, 'Error publishing curriculum for tycoon #' + IntToStr(i) );
          end;
      finally
        fTycoons.Unlock;
      end;
    end;

  procedure TWorld.RDOGenerateCustomLog( LogId : integer );
    var
      Tycoon  : TTycoon;
      i       : integer;
      LogName : string;
      days    : integer;
      accExp  : integer;
    begin
      fTycoons.Lock;
      try
        LogName := 'CustomLog' + IntToStr( LogId );
        Logs.Log( LogName, '-' );
        for i := 0 to pred(fTycoons.Count) do
          try
            Tycoon := TTycoon(fTycoons[i]);
            case LogId of
              0 :
                if (Tycoon.Companies <> nil) and (Tycoon.Companies.Count > 0) and not Tycoon.IsRole
                  then Logs.Log( LogName, Tycoon.Name + #9 + IntToStr(Tycoon.Companies.Count) + #9 + IntToStr(Tycoon.FacCount) + #9 + IntToStr(Tycoon.Level.Tier) + #9 + CurrToStr(Tycoon.Budget));
              1 :
                if (not Tycoon.IsOnline) and (not Tycoon.IsRole) and (not Tycoon.Deleted)
                  then
                    begin
                      days := DaysOut(Tycoon);
                      if days > MaxDaysToPurge
                        then accExp := GetDaysExpired(Tycoon, 30) // >>
                        else accExp := 0;
                      if accExp > MaxDaysToPurge
                        then Logs.Log( LogName, Format('%s' + #9 + '%d days.', [Tycoon.Name, accExp]));
                    end;
            end;
          except
            Logs.Log( tidLog_Survival, 'Error logging for tycoon #' + IntToStr(i) );
          end;
        Logs.Log( LogName, '-' );
      finally
        fTycoons.Unlock;
      end;
    end;

  procedure TWorld.RDOSendEvent(msg, url : widestring);
    var
      mStr : TMultiString;
    begin
      mStr := TMultiString.Create;
      mStr.Values['0'] := msg;
      mStr.Values['1'] := msg;
      mStr.Values['2'] := msg;
      try
        SendEvent(
          TEvent.Create(
            0,
            VirtualTimeAbs,
            VirtualTime,
            10000,
            1000,
            mStr,
            '',
            url));
      except
      end;
    end;

  procedure TWorld.RDOLogonClient(name, password : widestring);
    var
      Tycoon : TTycoon;
    begin
      try
        Tycoon := TycoonByName[name];
        if (Tycoon <> nil) and (lowercase(Tycoon.Password) = lowercase(password))
          then LoggedUserData.SetUserData(Tycoon.MasterRole);
      except
        Logs.Log( tidLog_Survival, 'ERROR in RDOLogonClient!');
      end;
    end;

  procedure TWorld.RDOOptimizeRoads(password : widestring);
    begin
      if UpperCase(password) = UpperCase(tidSystemPassword)
        then fRoads.RemoveUselessSegments(FacilitiesIn, 3);
    end;

  procedure TWorld.RDOForceElections(password : widestring);
    begin
      if (fPeriods[perPoliticalMandate] = 0) and (UpperCase(password) = UpperCase(tidSystemPassword))
        then fPeriods[perPoliticalMandate] := 1;
    end;

  procedure TWorld.RDOResetTournament(password : widestring);
    var
      i      : integer;
      Tycoon : TTycoon;
    begin
      if UpperCase(password) = UpperCase(tidSystemPassword)
        then
          begin
            fTycoons.Lock;
            try
              for i := 0 to pred(fTycoons.Count) do
                begin
                  Tycoon := TTycoon(fTycoons[i]);
                  Tycoon.Cookie['rkPts']  := '';
                  Tycoon.Cookie['lvPts']  := '';
                  Tycoon.Cookie['bnkPts'] := '';
                end;
            finally
              fTycoons.Unlock;
            end;
          end;
    end;

  procedure TWorld.RDOAddCurrItem(tycoon, desc : widestring; prest : integer);
    var
      Item   : TCurriculumItem;
      Target : TTycoon;
    begin
      Item   := TCustomItem.Create('', crrKind_CustomItem, prest, desc);
      Target := TycoonByName[tycoon];
      if Target <> nil
        then
          begin
            Target.AddItemToCurriculum(Item);
            ModelServerCache.UpdateObjectCache(Target, -1, -1);
          end;
    end;

  function TWorld.LandSize : TPoint;
    begin
      result.x := xSize;
      result.y := ySize;
    end;

  function TWorld.LandVisualClassAt( x, y : integer ) : TLandVisualClassId;
    begin
      result := GroundMap[x, y];
    end;

  function TWorld.LandClassAt( x, y : integer ) : TLandClass;
    begin
      result := Land.LandClassOf( GroundMap[x, y] );
    end;

  function TWorld.LandTypeAt( x, y : integer ) : TLandType;
    begin
      result := Land.LandTypeOf( GroundMap[x, y] );
    end;

  procedure TWorld.LoadLandInfo( filename : string );
    var
      x, y : integer;
      line : PByteArray;
    begin
      fLandBitmap := TBitmap.Create;
      fLandBitmap.LoadFromFile( filename );
      getmem( fGroundMap, ySize*xSize*sizeof(fGroundMap[0]) );
      fillchar( fGroundMap^, ySize*xSize*sizeof(fGroundMap[0]), NoLand );
      for y := 0 to pred(ySize) do
        begin
          line := fLandBitmap.ScanLine[y];
          for x := 0 to pred(xSize) do
            fGroundMap[xSize*y + x] := line[x];
        end;
    end;

  procedure TWorld.SetMailServer( aMailServer : IMailServer );

    procedure UpdateTycoons;
      var
        i : integer;
      begin
        for i := 0 to pred(Tycoons.Count) do
          begin
            TTycoon(Tycoons[i]).MailServer := self.MailServer;
            TTycoon(Tycoons[i]).Timer      := self;
          end;
      end;

    begin
      fMailServer := aMailServer;
      UpdateTycoons;
      UpdateMailServerDate;
    end;

  function TWorld.GetMessagesPath : string;
    begin
      result := WorldURL + tidURL_SpecialMailMessages;
    end;

  function TWorld.GetWorldURL : string;
    begin
      result := WorldURL;
    end;

  function TWorld.GetMainBank : TBank;
    begin
      result := fMainBank;
    end;

  procedure TWorld.SendNotification( TycoonId : integer; Kind : integer; Title, Body : string; Options : integer );
    begin
      if assigned(fOnSendNotification)
        then fOnSendNotification( TycoonId, Kind, Title, Body, Options );
    end;

  procedure TWorld.SendEvent( Event : TEvent );
    var
      Tycoon : TTycoon;
      Clone  : TEvent;
      i      : integer;
    begin
      try
        fTycoons.Lock;
        try
          for i := 0 to pred(fTycoons.Count) do
            try
              Tycoon := TTycoon(fTycoons[i]);
              if Tycoon.IsOnline and (Tycoon.Name <> Event.Sender)
                then
                  begin
                    Clone := Event.Clone;
                    Tycoon.RecordEvent( Clone );
                  end;
            except
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error distributing event to tycoon ' + IntToStr(i) );
            end;
          Event.Free;
        finally
          fTycoons.Unlock;
        end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error distributing event.' );
      end;
    end;

  procedure TWorld.SearchForSuppliers( Block : TBlock );
    const
      MaxAutoCnxs = 15;

    var
      Tycoon  : TTycoon;

    procedure SearchOwner;
      var
        Company : TCompany;
        cmpIdx  : integer;
        facIdx  : integer;
        Fac     : TFacility;
      begin
        for cmpIdx := pred(Tycoon.Companies.Count) downto 0 do
          begin
            Company := TCompany(Tycoon.Companies[cmpIdx]);
            Company.Facilities.Lock;
            try
              for facIdx := pred(Company.Facilities.Count) downto 0 do
                begin
                  Fac := TFacility(Company.Facilities[facIdx]);
                  if (Fac <> Block.Facility) and (Fac.CurrBlock.Role in [rolDistributer, rolCompExport, rolCompInport])
                    then Block.ConnectTo(Fac.CurrBlock, true);
                end;
            finally
              Company.Facilities.Unlock;
            end;
          end;
      end;

    procedure SearchWorld;
      var
        Coll   : TCollection;
        iFac   : integer;
        Fac    : TFacility;
        iIn    : integer;
        iOut   : integer;
        Input  : Kernel.TInput;
        Output : Kernel.TOutput;
        iColl  : integer;
      begin
        for iIn := 0 to pred(Block.InputCount) do
          begin
            Input := Block.Inputs[iIn];
            if (Input.MetaInput <> nil) and (Input.MetaInput.MetaFluid <> nil) and (mfTradeable in Input.MetaInput.MetaFluid.Options) and not (mfConstruction in Input.MetaInput.MetaFluid.Options)
              then
                begin
                  Coll := TCollection.Create(0, rkUse);
                  try
                    fWorldSuppliers.Lock;
                    try
                      for iFac := 0 to pred(fWorldSuppliers.Count) do
                        begin
                          Fac := TFacility(fWorldSuppliers[iFac]);
                          if (Block.Facility <> Fac) and (Block.Facility.Town = Fac.Town) and not Fac.CriticalTrouble and (Fac.Company <> nil) and (Fac.Company.Owner <> nil)
                            then
                              for iOut := 0 to pred(Fac.CurrBlock.OutputCount) do
                                begin
                                  Output := Fac.CurrBlock.Outputs[iOut];
                                  if (Output.MetaOutput <> nil) and (Input.MetaInput.MetaFluid = Output.MetaOutput.MetaFluid) and (Output.FluidData.Q > 0) and (Output.ConnectionCount < 500)
                                    then
                                      begin
                                        iColl := 0;
                                        while (iColl < Coll.Count) and (Input.GetConnectionPrecedence(Output) > Input.GetConnectionPrecedence(TOutput(Coll[iColl]))) do
                                          inc( iColl );
                                        if iColl < MaxAutoCnxs
                                          then Coll.AtInsert( iColl, Fac.CurrBlock.Outputs[iOut] );
                                      end;
                                end;
                        end;
                      for iColl := 0 to pred(min(Coll.Count, MaxAutoCnxs)) do
                        begin
                          Output := TOutput(Coll[iColl]);
                          Input.ConnectTo( Output );
                        end;
                      if Coll.Count > 0
                        then Input.CheckConnections;
                    finally
                      fWorldSuppliers.Unlock;
                    end;
                  finally
                    Coll.Free;
                  end;
                end;
          end;
      end;

    var
      tier : integer;

    begin
      // Get Owner
      if Block.Facility.Company <> nil
        then Tycoon := Block.Facility.Company.Owner
        else Tycoon := nil;
      // Search suppliers in the owner stuff
      if (Tycoon <> nil) and (Tycoon.Level <> nil)
        then
          begin
            SearchOwner;
            tier := Tycoon.Level.Tier;
            // Search in World Suppliers if Owner is lower than Tycoon and the Block is not a Warehouse or is a Newbie probably in the Tutorial
            if (tier < 2) and ({(tier = 0) or} not (Block.Role in [rolDistributer, rolCompExport, rolCompInport]))
              then SearchWorld;
          end;
    end;

  procedure TWorld.AddCurriculumItem( Tycoon : TTycoon; Item : TCurriculumItem );
    var
      y, m, d, h, mn, s, msec : word;
      id  : string;
      key : string;
      i   : integer;
      useless : olevariant;
      Nob, NewNob : integer;
      waitForAnws : boolean;
    begin
      try
        DSLock;
        try
          if not VarIsEmpty(DirProxy) and not VarIsNull(DirProxy)
            then
              begin
                waitForAnws := DirProxy.WaitForAnswer;
                try
                  DirProxy.WaitForAnswer := true;
                  Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Publishing curriculum item.' );
                  case Item.Kind of
                    crrKind_JoinedWorld, crrKind_LeftWorld, crrKind_Transcended :
                      begin
                        id  := Area + '_' + self.Name;
                        key := GetUserPath( Tycoon.Name ) + '/AccountInfo/Worlds/' + id;
                        if Item.Kind = crrKind_JoinedWorld
                          then
                            begin
                              if fTournamentLength = 0
                                then useless := DirProxy.RDOCreateFullPathKey( key, true );
                            end
                          else useless := DirProxy.RDODeleteFullPathNode( key )
                      end;
                  end;
                  Logs.Log(tidLog_Survival, TimeToStr(Now) + ' AccountInfo Key created.. ');
                  DecodeTime( Now, h, mn, s, msec );
                  DecodeDate( Now, y, m, d );
                  id  := '_' + IntToStr(y) + '_' + IntToStr(m) + '_' + IntToStr(d) + '_' + IntToStr(h) + '_' + IntToStr(mn) + '_' + IntToStr(s) + '_' + IntToStr(msec) + '_' + IntToStr(random(1000));
                  key := GetUserPath( Tycoon.Name ) + '/Curriculum/' + id;
                  if DirProxy.RDOCreateFullPathKey(key, true) and DirProxy.RDOSetCurrentKey(key)
                    then
                      begin
                        Logs.Log(tidLog_Survival, TimeToStr(Now) + ' Curriculum Item Key created.. ');
                        for i := 0 to pred(LangList.Count) do
                          DirProxy.RDOWriteString( 'Desc' + LangList[i], Item.Desc[LangList[i]] );
                        DirProxy.RDOWriteInteger( 'Prestige', integer(round(Item.Prestige)) );
                        DirProxy.RDOWriteInteger( 'Kind', Item.Kind );
                        DirProxy.RDOWriteDate( 'Date', VirtualTime );
                        DirProxy.RDOWriteString( 'World', Name );
                        DirProxy.RDOWriteString( 'Area', Area );
                        Logs.Log(tidLog_Survival, TimeToStr(Now) + ' Done writing Item data.. ');
                        if Item.Kind = crrKind_Transcended
                          then
                            begin
                              key := GetUserPath( Tycoon.Name );
                              if DirProxy.RDOSetCurrentKey( key )
                                then
                                  begin
                                    Nob := DirProxy.RDOReadInteger( tidDSId_Nobpoints );
                                    inc( Nob, 50*max(0, Item.Importance - 3) );
                                    DirProxy.RDOWriteInteger( tidDSId_Nobpoints, Nob );
                                    NewNob := DirProxy.RDOReadInteger( tidDSId_Nobpoints );
                                    Item.Published := NewNob = Nob;
                                    if Item.Published
                                      then Tycoon.NobPoints := NewNob;
                                  end
                                else Item.Published := false;
                            end
                          else Item.Published := true;
                      end;
                finally
                  DirProxy.WaitForAnswer := waitForAnws;
                end;
              end
            else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error could not publishing curriculum item.' );
        finally
          DSUnlock;
        end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error could not publishing curriculum item. (exception launched)' );
      end;
    end;

  function TWorld.GetVirtualTime : TDateTime;
    begin
      try
        if (fVirtYear > 0) and (fVirtMonth > 0) and (fVirtMonth <= 12) and (fVirtDay > 0) and (fVirtDay <= 31)
          then result := EncodeDate( fVirtYear, fVirtMonth, fVirtDay )
          else result := EncodeDate( 2000, 1, 1 );
      except
        result := EncodeDate( 2000, 1, 1 );
      end;
    end;

  function TWorld.GetVirtualTimeAbs : TVirtDateAbs;
    begin
      result := fAbsDate;
    end;

  function TWorld.dt : TTimeDelta;
    begin
      result := fdt;
    end;

  function TWorld.GetSeason : TSeason;
    begin
      result := fSeason;
    end;

  function TWorld.GetSeasonProgress : single;
    begin
      result := realmin( 1, fSeasonProgress/SeasonIntro );
    end;

  function TWorld.getTickId : integer;
    begin
      result := fTimeTick;
    end;

  procedure TWorld.StopVirtualTime;
    begin
      fTimeLock.Enter;
      try
        inc( fTimeStopped );
      finally;
        fTimeLock.Leave;
      end;
    end;

  procedure TWorld.ResumeVirtualTime;
    begin
      fTimeLock.Enter;
      try
        dec( fTimeStopped );
      finally
        fTimeLock.Leave;
      end;
    end;

  function TWorld.TimeStopped : boolean;
    begin
      fTimeLock.Enter;
      try
        result := fTimeStopped > 0;
      finally
        fTimeLock.Leave;
      end;
    end;

  function TWorld.GetSeasonInt : integer;
    begin
      result := integer(fSeason);
    end;

  procedure TWorld.VirtualTimeTick( TimeElapsed : TVirtDateAbs );

    function GetMonthLength( Month, Year : integer ) : integer;
      const
        MonthLength : array[1..12] of integer =
          (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
      begin
        if Month <> 2
          then result := MonthLength[Month]
          else
            if Year mod 4 = 0
              then result := 29
              else result := 28;
      end;

    function SeasonChanged : boolean;
      begin
        with fEndSeasonDates[fSeason] do
          result := (fVirtMonth = month) and (fVirtDay = day);
        if result and (fSeason = seasFall) and fRandomSeasons
          then GenerateSeasonDates;
        {
        case fVirtMonth of
          3  : result := fVirtDay = 10;
          6  : result := fVirtDay = 10;
          9  : result := fVirtDay = 10;
          12 : result := fVirtDay = 10;
          else result := false;
        end;
        }
      end;

    var
      daypassed : boolean;
      newseason : boolean;
    begin
      if not TimeStopped
        then
          begin
            inc( fTimeTick, TimeElapsed );
            inc( fAbsDate, TimeElapsed );
            daypassed := false;
            newseason := false;
            while TimeElapsed > 0 do
              begin
                if fVirtHour = 23
                  then
                    begin
                      {$IFDEF DebugPolitics}
                      // Speed up elections
                      dec( fYearsToElections );
                      if fYearsToElections <= 0
                        then
                          begin
                            fYearsToElections := PeriodLength;
                            inc( fPeriods[perPoliticalMandate] );
                          end;
                      {$ENDIF}
                      fVirtHour := 0;
                      if fVirtDay = GetMonthLength( fVirtMonth, fVirtYear )
                        then
                          begin
                            fVirtDay := 1;
                            if fVirtMonth = 12
                              then
                                begin
                                  fVirtMonth := 1;
                                  inc( fVirtYear );
                                  inc( fPeriods[perYear] );
                                  {$IFNDEF DebugPolitics}
                                  dec( fYearsToElections );
                                  if fYearsToElections <= 0
                                    then
                                      begin
                                        fYearsToElections := PeriodLength;
                                        inc( fPeriods[perPoliticalMandate] );
                                      end;
                                  {$ENDIF}
                                end
                              else
                                begin
                                  inc( fVirtMonth );
                                  inc( fPeriods[perMonth] );
                                end
                          end
                        else
                          begin
                            inc( fVirtDay );
                            inc( fPeriods[perDay] );
                          end;
                      daypassed := true;
                      if SeasonChanged
                        then
                          begin
                            if fSeason < high(fSeason)
                              then inc( fSeason )
                              else fSeason := low(fSeason);
                            newseason := true;
                            if fSeason = seasWinter
                              then fEcconomyRelay.Reset;
                          end
                        else inc( fSeasonProgress );
                    end
                  else
                    begin
                      inc( fVirtHour );
                      inc( fPeriods[perHour] );
                    end;
                dec( TimeElapsed );
              end;
            if daypassed and assigned(fOnDateChanged)
              then fOnDateChanged( VirtualTime );
            if newseason and assigned(fOnSeasonChanged)
              then fOnSeasonChanged( GetSeason );
          end;
    end;

  procedure TWorld.ForceDt(aDt : TVirtDateAbs);
    begin
      fDt := aDt;
    end;

  function TWorld.OnAuthorizeBreak( x, y : integer; OwnerId, BreakerId : TOwnerId ) : boolean;

    var
      Breaker : TTycoon;

    function BreakerIsTheMayor : boolean;
      var
        Town : TTown;
      begin
        Town   := NearestTown( x, y );
        result := Town.Mayor.MasterRole = Breaker;
      end;

    begin
      Breaker := TycoonById[BreakerId].MasterRole;
      result  := (BreakerId = IBBreaker) or (OwnerId = BreakerId) or (TycoonById[OwnerId] = nil) or Breaker.HasZonerRole or BreakerIsTheMayor;
    end;

  function TWorld.OnRenderExtraSegInfo( Segment : TSegment ) : string;
    var
      kind  : TCargoKind;
      Layer : TCargoLayer;
    begin
      result := '';
      for kind := low(kind) to high(kind) do
        begin
          Layer := fCargoSystem.Layer[ord(kind)];
          result := result +
            IntToStr(Layer.Value[Segment.NodeA.x, Segment.NodeA.y]) + LineBreak +
            IntToStr(Layer.Value[Segment.NodeB.x, Segment.NodeB.y]) + LineBreak;
        end;
    end;
    
  function TWorld.GetObjectMap( x, y : integer ) : TObjId;
    begin
      result := fObjectMap[x + fxSize*y];
    end;

  function TWorld.GetGroundMap( x, y : integer ) : TLandVisualClassId;
    begin
      result := fGroundMap[x + fxSize*y];
    end;

  procedure TWorld.SetObjectMap( x, y : integer; Item : TObjId );
    begin
      fObjectMap[x + fxSize*y] := Item;
    end;

  procedure TWorld.SetGroundMap( x, y : integer; Item : TLandVisualClassId );
    begin
      fGroundMap[x + fxSize*y] := Item;
    end;

  function TWorld.GetTownMap( x, y : integer ) : byte;
    begin
      result := fTownMap[x + fxSize*y]
      {if (x >= 0) and (x < fxSize) and (y >= 0) and (y < fySize)
        then result := fTownMap[x + fxSize*y]
        else raise Exception.CreateFmt('x, y out of range.. [%d, %d]', [x, y]);}
    end;

  function TWorld.FacAllowedInClear(MetaFacility : TMetaFacility; x, y : integer; Company : TCompany ) : boolean;
    var
      dist     : integer;
      xi, yi   : integer;
      mnx, mxx : integer;
      mny, mxy : integer;
      Fac      : TFacility;
      MB       : TMetaBlock;
      samComp  : boolean;
    begin
      if (MetaFacility <> nil) and (MetaFacility.TypicalStage <> nil) and (MetaFacility.TypicalStage.MetaBlock <> nil)
        then MB := MetaFacility.TypicalStage.MetaBlock
        else MB := nil;
      if MB.MinColDist > 0
        then
          begin
            dist := MB.MinColDist;
            samComp := MB.ColIsSameComp;
            mnx := max(0, x - dist);
            mxx := min(pred(fxSize), x + MetaFacility.XSize + dist - 1);

            mny := max(0, y - dist);
            mxy := min(pred(fYSize), y + MetaFacility.YSize + dist - 1);

            csMapLock.Enter; //BeginRead( INFINITE );
            try
              yi     := mny;
              result := true;
              while (yi <= mxy) and result do
                begin
                  xi := mnx;
                  while (xi <= mxx) and result do
                    begin
                      Fac := ObjectMap[xi, yi];
                      result := (Fac = nil) or (samComp and (Fac.Company <> Company)) or (Fac.MetaFacility.Id <> MetaFacility.Id);
                      inc(xi);
                    end;
                  inc(yi);
                end;
            finally
              csMapLock.Leave; //EndRead;
            end;
          end
        else result := true;
    end;

  function TWorld.AreaIsClear( x, y, dx, dy : integer; Company : TCompany ) : boolean;

    function FindWaterAllowed( Company : TCompany ) : boolean;
      begin
        if fWaterQuestInv = nil
          then fWaterQuestInv := TInvention(TheClassStorage.ClassById[tidClassFamily_Inventions, tidInvention_WaterQuest]);
        result := (fWaterQuestInv <> nil) and Company.HasInvention[fWaterQuestInv.NumId];
      end;

    var
      dxi, dyi     : integer;
      waterallowed : boolean;

    begin
      waterallowed := (Company <> nil) and FindWaterAllowed( Company );
      csMapLock.Enter; //BeginRead( INFINITE );
      try
        dyi    := 0;
        result := true;
        while (dyi < dy) and result do
          begin
            dxi := 0;
            while (dxi < dx) and result do
              begin
                result :=
                  (ObjectMap[x + dxi, y + dyi] = nil) and
                  ((LandClassOf(GroundMap[x + dxi, y + dyi]) <> lncZoneD) or waterallowed);
                inc( dxi );
              end;
            inc( dyi );
          end;
      finally
        csMapLock.Leave; //EndRead;
      end;
      if result
        then
          begin
            result := fRoads.AreaIsClear( Rect(x, y, x + dx - 1, y + dy - 1) );
            if not result
              then Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Area not clear because of roads..' );
          end
        else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Area not clear because of Objects..' );
    end;

  function TWorld.LandClassFound( x1, y1, x2, y2 : integer; LandClass : TLandClass ) : boolean;
    var
      xi, yi : integer;
    begin
      csMapLock.Enter; //BeginRead( INFINITE );
      try
        yi := y1;
        result := false;
        while (yi <= y2) and not result do
          begin
            xi := x1;
            while (xi <= x2) and not result do
              begin
                result := LandClassOf(GroundMap[xi, yi]) = LandClass;
                inc(xi);
              end;
            inc(yi);
          end;
      finally
        csMapLock.Leave; //EndRead;
      end;
    end;

  function TWorld.MatchesZone( x, y, dx, dy : integer; ZoneType : TZoneType ) : boolean;
    var
      xi, yi : integer;
      iZone  : TZoneType;
    begin
      fZones.Lock;
      try
        result := false;
        yi     := y;
        repeat
          xi := x;
          repeat
            iZone  := fZones[yi, xi];
            result := result or ZoneMatches(iZone, ZoneType);
            inc( xi );
          until result or (xi = x + dx);
          inc( yi );
        until result or (yi = y + dy)
      finally
        fZones.Unlock;
      end;
    end;

  function TWorld.AreaIsZoned(x, y, dx, dy : integer) : boolean;
    var
      xi, yi : integer;
      zoned  : integer;
    begin
      try
        fZones.Lock;
        try
          zoned  := 0;
          for xi := x to pred(x + dx) do
            for yi := y to pred(y + dy) do
              if fZones[yi, xi] <> znNone
                then inc(zoned)
                else dec(zoned);
          result := zoned >= 0;
        finally
          fZones.Unlock;
        end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in AreaIsZoned..' );
        result := true;
      end;
    end;

  procedure TWorld.InitTownMap( TownMapFile : string );
    var
      x, y    : integer;
      Town    : TTown;                                       
      line    : PByteArray;
      Bitmap  : TBitmap;
      TownMap : PTownMap;
    begin
      getmem( TownMap, xSize*ySize*sizeof(byte));
      fillchar( TownMap^, xSize*ySize*sizeof(byte), 0 );
      if (TownMapFile = '') or not FileExists( TownMapFile )
        then
          begin
            for y := 0 to pred(ySize) do
              for x := 0 to pred(xSize) do
                begin
                  Town := NearestTown( x, y );
                  if Town <> nil
                    then TownMap[x + fxSize*y] := Town.Id;
                end;
          end
        else
          begin
            Bitmap := TBitmap.Create;
            Bitmap.LoadFromFile( TownMapFile );
            for y := 0 to pred(ySize) do
              begin
                line := Bitmap.ScanLine[y];
                for x := 0 to pred(xSize) do
                  TownMap[xSize*y + x] := line[x] + 1;
              end;
          end;
      fTownMap := TownMap;
    end;

  function TWorld.ZoneMatches( ZoneA, ZoneB : TZoneType ) : boolean;
    begin
      result := Protocol.ZoneMatches( ZoneA, ZoneB );
    end;

  procedure TWorld.UpdateInMap( Facility : TFacility );
    var
      xi, yi : integer;
    begin
      csMapLock.Enter; //BeginWrite( INFINITE );
      try
        for yi := Facility.yPos to Facility.yPos + Facility.MetaFacility.YSize - 1 do
          for xi := Facility.xPos to Facility.xPos + Facility.MetaFacility.XSize - 1 do
            ObjectMap[xi, yi] := Facility;
      finally
        csMapLock.Leave; //EndWrite;
      end;
    end;

  procedure TWorld.RemoveFromMap( Facility : TFacility );
    var
      xi, yi : integer;
    begin
      csMapLock.Enter; //BeginWrite( INFINITE );
      try
        for xi := Facility.xPos to Facility.xPos + Facility.MetaFacility.XSize - 1 do
          for yi := Facility.yPos to Facility.yPos + Facility.MetaFacility.YSize - 1 do
            if ObjectMap[xi, yi] = Facility
              then ObjectMap[xi, yi] := nil;
      finally
        csMapLock.Leave; //EndWrite;
      end;
    end;

  procedure TWorld.InsertFacility( Facility : TFacility );
    var
      i     : integer;
      count : integer;
      level : integer;
    begin
      Facilities.Lock;
      try
        count := Facilities.Count;
        i     := 0;
        level := Facility.MetaFacility.Level;
        while (i < count) and (TFacility(Facilities[i]).MetaFacility.Level <= level) do
          inc( i );
        Facilities.AtInsert( i, Facility );
      finally
        Facilities.Unlock;
      end;
      // Add to World Suppliers if Warehouse
      if Facility.CurrBlock.Role in [rolDistributer, rolCompExport, rolCompInport]
        then fWorldSuppliers.Insert(Facility);
    end;

  function TWorld.NearestTown( x, y : integer ) : TTown;
    var
      mindist : integer;
      i       : integer;
      TownId  : TTownId;
    begin
      if fTownMap = nil
        then
          if fTowns.Count > 0
            then
              begin
                result  := TTown(fTowns[0]);
                mindist := dist( result.xPos, result.yPos, x, y );
                for i := 1 to pred(fTowns.Count) do
                  with TTown(fTowns[i]) do
                    if abs(xPos - x) + abs(yPos - y) < mindist
                      then
                        begin
                          result  := TTown(fTowns[i]);
                          mindist := abs(xPos - x) + abs(yPos - y);
                        end;
              end
            else result := nil
        else
          begin
            TownId := TownMap[x, y];
            result := TownById[TownId];
          end;
    end;

  procedure TWorld.RefreshArea( x, y, dx, dy : integer );
    begin
      if assigned(fOnAreaChanged)                    
        then fOnAreaChanged( x, y, dx, dy );
    end;

  procedure TWorld.RefeshFacility( Facility : TFacility; FacilityChange : TFacilityChange );
    begin
      if Facility.Focused and assigned(fOnFacilityChanged)
        then fOnFacilityChanged( Facility, FacilityChange );
    end;
    
  procedure TWorld.NearestCircuitsToArea( CircuitId : TCircuitId; Area : TRect; var Circuits : TCollection );
    var
      Circuit   : TCircuitMap;
      ErrorCode : TCircuitErrorCode;
    begin
      try
        Circuit := CircuitById[CircuitId];
        if Circuit <> nil
          then Circuit.NearestCircuitsToArea( Area, RoadTolerance, Circuits, ErrorCode );
      except
      end;
    end;

  function TWorld.AreasAreConnected( CircuitId : TCircuitId; Area1, Area2 : TRect ) : boolean;
    var
      Circuit   : TCircuitMap;
      ErrorCode : TCircuitErrorCode;
      useless   : TRect;
    begin
      try
        Circuit := CircuitById[CircuitId];
        if Circuit <> nil
          then
            begin
              InflateRect( Area1, RoadTolerance, RoadTolerance );
              InflateRect( Area2, RoadTolerance, RoadTolerance );
              result := IntersectRect( useless, Area1, Area2 ) or Circuit.AreasAreConnected( Area1, Area2, 0, ErrorCode );
            end
          else result := false;
      except
        result := false;
      end;
    end;

  function TWorld.GetCargoSystem( CircuitId : TCircuitId ) : ICargoSystem;
    begin
      result := fCargoSystem;          
    end;                                          
                                              
  procedure TWorld.InitCargoSystem;
    begin
      fCargoSystem := TCargoSystem.Create( fxSize, fySize );
      fCargoSystem.AddLayer( ord(carPeople), TMatrixLayer );
      fCargoSystem.AddLayer( ord(carLight),  TMatrixLayer );      
      fCargoSystem.AddLayer( ord(carHeavy),  TMatrixLayer );
      // hook profiler here too!
      Profiler.RequestProfile( prfKind_Sim, prfId_CollectInputs, 'Collect inputs' );
      Profiler.RequestProfile( prfKind_Sim, prfId_SpreadOutputs, 'Spread outputs' );
      Profiler.RequestProfile( prfKind_Sim, prfId_CollectInputExtra, 'Collect input extra' );
      Profiler.RequestProfile( prfKind_Sim, prfId_SpreadOutputExtra, 'Distribution' );//'Spread output extra' );
      Profiler.RequestProfile( prfKind_Sim, prfId_Sim, 'Simulation' );
      Profiler.RequestProfile( prfKind_InputCollect, prfId_CollectLoopInit, 'CollectLoopInit' );
      Profiler.RequestProfile( prfKind_InputCollect, prfId_dQHandling, 'dQHandling' );
      Profiler.RequestProfile( prfKind_InputCollect, prfId_CollectFinalizing, 'CollectFinalizing' );
      Profiler.RequestProfile( prfKind_Int, prfId_UpdateModifiers, 'Modifiers' );
      Profiler.RequestProfile( prfKind_Int, prfId_Integrate, 'Integrators' );
    end;

  function TWorld.NewCompany( name : string; Cluster : TCluster; Tycoon : TTycoon ) : TCompany;

    procedure UpdateRankings(Tycoon : TTycoon);
      var
        i     : integer;
        count : integer;
      begin
        count := TheClassStorage.ClassCount[tidClassFamily_Rankings];
        for i := 0 to pred(count) do
          TRanking(TheClassStorage.ClassByIdx[tidClassFamily_Rankings, i]).AddObject(Tycoon);
      end;

    var
      key          : string;
      cid          : integer;
      waitForAnws  : boolean;
      canJoinWorld : boolean;
    begin
      canJoinWorld := true;
      DSLock;
      try
        Logs.Log( tidLog_Survival, TimeToStr(Now) + Format(' %s created new company.', [name]));
        try
          if (fTournamentLength = 0) and (Tycoon.Companies.Count = 0) and not Tycoon.IsRole and not VarIsEmpty( DirProxy )
            then
              begin
                canJoinWorld := DirProxy.RDOCanJoinNewWorld(Tycoon.Name);
                Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Checked Can Join World..');
                if canJoinWorld
                  then
                    begin
                      key := GetUserPath( Tycoon.Name ) + '/Worlds/' + Area + '/' + self.Name;
                      if DirProxy.RDOCreateFullPathKey(key, true) and DirProxy.RDOSetCurrentKey(key)
                        then
                          begin
                            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Create World record on DS..');
                            waitForAnws := DirProxy.WaitForAnswer;
                            DirProxy.WaitForAnswer := true;
                            try
                              DirProxy.RDOWriteDate( 'Joined', Now );
                              DirProxy.RDOWriteDate( 'Date', GetVirtualTime );
                            finally
                              DirProxy.WaitForAnswer := waitForAnws;
                            end;
                            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Done with DS..');
                          end
                        else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Failed writing to DS.');
                      // UpdateRankings(Tycoon);
                      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Rankings Updated..');
                    end;
              end;
        except
        end;
      finally
        DSUnlock;
      end;

      // Check if he can create a new company.
      if canJoinWorld
        then
          begin
            // Avoid mixing companies IDs
            fBuildLock.Enter;
            try
              inc(fLastCompId);
              cid := fLastCompId;
            finally
              fBuildLock.Leave;
            end;

            if Cluster.CompanyClass <> nil
              then result := Cluster.CompanyClass.Create(cid)
              else result := TCompany.Create(cid);

            result.Name    := name;
            result.Cluster := Cluster;
            result.Owner   := Tycoon;
            Tycoon.Companies.Insert( result );
            Companies.Insert( result );

            // If it is the first company then update the rankings
            if Tycoon.Companies.Count = 1
              then UpdateRankings(Tycoon);

            {if MailServer <> nil
              then
                begin
                  MailServer.NewMailAccount( 'company@' + name + '.' + 'com', name, Tycoon.Name + '@' + self.Name + '.com', false );
                  MailServer.NewMailAccount( 'CEO@' + name + '.' + 'com', name, Tycoon.Name + '@' + self.Name + '.com', false );
                end;}
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Caching Company..');
            CacheObject( result, noKind, noInfo );
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Company Cached..');
            CompanyCreated( result );
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Company Reported..');
          end
        else
          begin
            result := nil;
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Trying to create a Company being in another world ' + Tycoon.Name);
          end;
    end;

  function TWorld.NewTycoon( name, password : string ) : TTycoon;

    function PickTown : TTown;
      var
        i   : integer;
        cnt : integer;
      begin
        cnt := Towns.Count;
        i := 0;
        repeat
          result := TTown(Towns[random(cnt)]);
          inc(i);
        until result.HasMayor or (i = 3*cnt);
      end;

    var
      HomeTown : TTown;

    begin
      inc( fLastTycoonId );
      result := TTycoon.Create( fLastTycoonId );
      result.WorldLocator := self;
      result.MailServer   := MailServer;
      result.Name         := trim(name);
      result.Password     := uppercase(password);
      result.Budget       := InitialBudget;
      result.Options      := [tyoMainRole];
      result.AddItemToCurriculum( TOwnershipItem.Create( TOwnershipItem.ClassName, 0 ) );
      Tycoons.Insert( result );
      MailServer.NewMailAccount( name + '@' + self.Name + '.net', name, '', true );
      CacheObject( result, noKind, noInfo );
      TycoonCreated( result );
      HomeTown := PickTown;
      if HomeTown <> nil
        then
          begin
            result.Cookie[tidLastViewX + '0'] := IntToStr(HomeTown.xPos);
            result.Cookie[tidLastViewY + '0'] := IntToStr(HomeTown.yPos);
          end;
      SendEvent(
        TEvent.Create(
          0,
          VirtualTimeAbs,
          VirtualTime,
          1000,
          1000,
          InstantiateMultiString( mtidMsgJoinedWorld, [result.Name, self.Name] ),
          '', '' ));
      result.IsDemo := IsDemoAccount(name);
    end;

  function TWorld.NewTycoonRole( RoleId, name : string; TycoonClass : CTycoon; InitialBudget : TMoney ) : TTycoon;
    begin
      inc( fLastTycoonId );
      result := TycoonClass.Create( fLastTycoonId );
      result.WorldLocator := self;
      result.MailServer   := MailServer;
      result.Name         := name;
      result.Budget       := InitialBudget;
      result.AddItemToCurriculum( TOwnershipItem.Create( TOwnershipItem.ClassName, 0 ) );
      Tycoons.Insert( result );
      try
        if MailServer <> nil
          then MailServer.NewMailAccount( name + '@' + self.Name + '.' + 'org', name, '', true );
      except
      end;
      CacheObject( result, noKind, noInfo );
      TycoonCreated( result );
    end;

  function TWorld.NewNewspaper( Name, Style, Town : string ) : boolean;
    begin
      try
        if fNewsServer <> nil
          then
            begin
              fNewsServer.CreateNewspaper( self.Name, Name, Style, Town );
              result := true;
            end
          else result := false;
      except
        result := false;
      end;
    end;

  procedure TWorld.DeleteTycoon( Tycoon : TTycoon; erase : boolean );
    var
      i            : integer;
      AwakenTycoon : TAwakenTycoon;
    begin
      if erase
        then Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Deleting tycoon: ' + Tycoon.Name )
        else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Reseting tycoon: ' + Tycoon.Name );
      try
        Tycoon.Transcending := false;
        Tycoon.ResetTutorial;
        Tycoon.LicenceLevel := 0;
        TycoonDeleted( Tycoon );
        if erase
          then
            begin
              AwakenTycoon := GetAwakenTycoon( Tycoon.Id );
              if AwakenTycoon <> nil
                then fAwakenTycoons.Delete( AwakenTycoon );
            end;
        fTowns.Lock;
        try
          for i := 0 to pred(fTowns.Count) do
            try
              TTown(fTowns[i]).TycoonDeleted( Tycoon );
            except
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error notifying town ' + IntToStr(i) );
            end;
        finally
          fTowns.Unlock;
        end;
        if erase
          then
            begin
              fTycoons.Lock;
              try
                for i := 0 to pred(fTycoons.Count) do
                  try
                    TTycoon(fTycoons[i]).TycoonDeleted( Tycoon );
                  except
                    Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error notifying tycoon ' + IntToStr(i) );
                  end;
              finally
                fTycoons.Unlock;
              end;
            end;
        if Tycoon.SuperRole <> nil
          then Tycoon.SuperRole.AbandomRole( Tycoon );
        for i := pred(Tycoon.Roles.Count) downto 0 do
          Tycoon.AbandomRole( TTycoon(Tycoon.Roles[i]) );

        if erase
          then
            try
              Tycoon.AddItemToCurriculum(
                TOpenItem.Create(
                  '',
                  crrKind_LeftWorld,
                  InstantiateMultiString( mtidBankrupt, [CurrYear] ),
                  20,
                  0));
            except
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error deleting DS key in DeleteTycoon');
            end;

        if erase
          then
            begin
              ModelServerCache.BackgroundUncache(Tycoon);
              fTycoons.Extract(Tycoon); //fTycoons.Delete( Tycoon );
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Tycoon disposed.' );
            end;
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Tycoon deleted.' );
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error deleting tycoon.' );
      end;
    end;

  procedure TWorld.RequestDeleteTycoon( Tycoon : TTycoon; erase : boolean );
    var
      i       : integer;
      Company : TCompany;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' ReqDeleting tycoon: ' + Tycoon.Name );
      try
        Tycoon.Deleted := erase;
        fDeadTycoons.Insert( Tycoon );
        Tycoon.Companies.Lock;
        try
          for i := 0 to pred(Tycoon.Companies.Count) do
            begin
              Company := TCompany(Tycoon.Companies[i]);
              if Company.Privated
                then
                  begin
                    RequestDeleteCompany(TCompany(Tycoon.Companies[i]));
                    Company.Deleted := true;
                  end
                else TCompany(Tycoon.Companies[i]).Owner := nil;
            end;
        finally
          Tycoon.Companies.Unlock;
        end;
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Tycoon ReqDeleted.' );
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error ReqDeleting tycoon.' );
      end;
    end;

  procedure TWorld.ResetTycoon( Tycoon : TTycoon );

    function GetInitialBudget : TMoney;
      var
        key : string;
        Nob : integer;
      begin
        try
          if fTournamentLength = 0
            then
              begin
                DSLock;
                try
                  key := GetUserPath( Tycoon.Name );
                  if DirProxy.RDOSetCurrentKey( key )
                    then
                      begin
                        Nob := DirProxy.RDOReadInteger( tidDSId_Nobpoints );
                        result := InitialBudget*realmax(100, Nob)/100
                      end
                    else result := InitialBudget;
                finally
                  DSUnlock;
                end;
              end
            else result := InitialBudget;
        except
          result := InitialBudget;
        end;
      end;

    var
      TycoonBudget : TMoney;
      prestigeLost : integer;

    begin
      try
        if not Tycoon.Transcending
          then
            begin
              if (Tycoon.Level.Tier = 0) //and (Tycoon.CountItemsInCurriculum(crrKind_LeftWorld) < 3)
                then prestigeLost := 0
                else prestigeLost := fResetPrestigeLost;
              Tycoon.AddItemToCurriculum(
                TOpenItem.Create(
                  '',
                  crrKind_LeftWorld,
                  InstantiateMultiString( mtidBankrupt, [CurrYear] ),
                  //SimHints.GetHintText( hidBankrupt, [CurrYear] ),
                  20,
                  -prestigeLost));
              TycoonBudget := InitialBudget;
            end
          else
            begin
              Tycoon.AddItemToCurriculum(
                TOpenItem.Create(
                  '',
                  crrKind_Transcended,
                  InstantiateMultiString( mtidTranscended, [CurrYear] ),
                  //SimHints.GetHintText( hidBankrupt, [CurrYear] ),
                  Tycoon.Level.Tier,
                  Tycoon.Level.Tier*200 ));
              TycoonBudget := GetInitialBudget;
            end;
        Tycoon.AbandomRoles;
        Tycoon.ResetTutorial;
        Tycoon.LicenceLevel := 0;
        RequestDeleteTycoon( Tycoon, false );
        Tycoon.CancelLoans;
        Tycoon.Budget := TycoonBudget;
        Tycoon.FailureLevel := 0;
        Tycoon.ResetLevel;
        Tycoon.Accounts.Reset;
        ModelServerCache.BackgroundInvalidateCache(Tycoon); //CacheObject( Tycoon, noKind, noInfo )
      except
      end;
    end;

  procedure TWorld.DeleteCompany( Company : TCompany );
    begin
      CompanyDeleted( Company );
      Company.Owner.Companies.Delete( Company );
      //fCompanies.Delete( Company );
      fCompanies.Extract( Company );
      ModelServerCache.UncacheObject(Company, noKind, noInfo);
    end;

  procedure TWorld.RequestDeleteCompany( Company : TCompany );
    var
      i   : integer;
      Fac : TFacility;
    begin
      try
        fDeadCompanies.Insert( Company );
        fPendingDeletions.Lock;
        try
          for i := 0 to pred(Company.Facilities.Count) do
            begin
              Fac := TFacility(Company.Facilities[i]);
              if Fac <> nil
                then
                  begin
                    if not Fac.CurrBlock.MetaBlock.Transcends(Fac)
                      then
                        begin
                          Fac.Deleted := true;
                          Fac.Stopped := true;
                        end;
                    RequestDeletion(Fac);
                  end;
            end;
        finally
          fPendingDeletions.Unlock;
        end;
      except
      end;
    end;

  procedure TWorld.DeleteFacility( Facility : TFacility );
    var
      x, y   : integer;
      dx, dy : integer;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Deleting Facility: ' + Facility.Name );
      if fDeadMeat.IndexOf( Facility ) = NoIndex
        then
          try
            if not Facility.CurrBlock.MetaBlock.Transcends(Facility)
              then
                begin
                  Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Notifying to clients... ' );
                  if (Facility.Company <> nil) and (Facility.Company.Owner <> nil)
                    then Facility.Company.Owner.RemoveFacilityLink(Facility);
                  RefeshFacility( Facility, fchDestruction );
                  x  := Facility.xPos;
                  y  := Facility.yPos;
                  dx := Facility.MetaFacility.xSize + 1;
                  dy := Facility.MetaFacility.ySize + 1;
                  Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Removing from map... ' );
                  RemoveFromMap( Facility );
                end
              else
                begin
                  x  := Facility.xPos;
                  y  := Facility.yPos;
                  dx := 0;
                  dy := 0;
                end;
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Inserting in DeadMeat... ' );
            fDeadMeat.Insert( Facility );
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Refreshing area... ' );
            if not Facility.CurrBlock.MetaBlock.Transcends(Facility)
              then RefreshArea( x, y, dx, dy );

            // Extract from World Suppliers list
            if Facility.CurrBlock.Role in [rolDistributer, rolCompExport, rolCompInport]
              then fWorldSuppliers.Extract(Facility);

            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Facility Delete OK.' );
          except
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error Deleting facility.' );
          end
        else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error Deleting facility: Facility already in Dead Meat!' );
    end;

  procedure TWorld.RequestDeletion( Facility : TFacility );
    begin
      fPendingDeletions.Insert( Facility );
    end;

  procedure TWorld.CompanyCreated( Company : TCompany );
    var
      i : integer;
    begin
      for i := 0 to pred(fWorldExtensions.Count) do
        TWorldExtension(fWorldExtensions[i]).CompanyCreated( Company );
    end;

  procedure TWorld.TycoonCreated( Tycoon : TTycoon );

    procedure UpdateRankings;
      var
        i     : integer;
        count : integer;
      begin
        count := TheClassStorage.ClassCount[tidClassFamily_Rankings];
        for i := 0 to pred(count) do
          TRanking(TheClassStorage.ClassByIdx[tidClassFamily_Rankings, i]).AddObject( Tycoon );
      end;

    var
      i : integer;
    begin
      for i := 0 to pred(fWorldExtensions.Count) do
        TWorldExtension(fWorldExtensions[i]).TycoonCreated( Tycoon );
      if tyoMainRole in Tycoon.Options
        then
          begin
            UpdateRankings;
            Tycoon.Level := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, tidTycoonLevel_Apprentice]);
          end
        else Tycoon.Level := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, tidTycoonLevel_Legend]);
      Tycoon.Timer        := self;
      Tycoon.WorldLocator := self;
      Tycoon.MailServer   := MailServer;
    end;

  procedure TWorld.FacilityCreated( Facility : TFacility );
    var
      i : integer;
    begin
      for i := 0 to pred(fWorldExtensions.Count) do
        TWorldExtension(fWorldExtensions[i]).FacilityCreated( Facility );
    end;

  procedure TWorld.CompanyDeleted( Company  : TCompany );
    var
      i : integer;
    begin
      for i := 0 to pred(fWorldExtensions.Count) do
        TWorldExtension(fWorldExtensions[i]).CompanyDeleted( Company );
    end;

  procedure TWorld.TycoonDeleted( Tycoon : TTycoon );

    procedure UpdateRankings;
      var
        i     : integer;
        count : integer;
      begin
        count := TheClassStorage.ClassCount[tidClassFamily_Rankings];
        for i := 0 to pred(count) do
          TRanking(TheClassStorage.ClassByIdx[tidClassFamily_Rankings, i]).DelObject( Tycoon );
      end;

    var
      i : integer;
    begin
      if tyoMainRole in Tycoon.Options
        then UpdateRankings;
      for i := 0 to pred(fWorldExtensions.Count) do
        TWorldExtension(fWorldExtensions[i]).TycoonDeleted( Tycoon );
    end;

  procedure TWorld.FacilityDeleted( Facility : TFacility );
    var
      i        : integer;
      BlockDel : TMsgBlockDeleted;
      FacDel   : TMsgFacilityDeleted;
    begin
      BlockDel.Msg    := msgKernel_BlockDeleted;
      BlockDel.Block  := Facility.CurrBlock;
      FacDel.Msg      := msgKernel_FacilityDeleted;
      FacDel.Facility := Facility;
      fTycoons.Lock;
      try
        for i := 0 to pred(fTycoons.Count) do
          begin
            fTycoons[i].Dispatch( BlockDel );
            fTycoons[i].Dispatch( FacDel );
          end;
      finally
        fTycoons.Unlock;
      end;
      for i := 0 to pred(fWorldExtensions.Count) do
        TWorldExtension(fWorldExtensions[i]).FacilityDeleted( Facility );
    end;

  procedure TWorld.SynchronizeClock;
    var
      current : TDateTime;
      start   : TDateTime;
      days    : integer;
      hours   : integer;
    begin
      current := EncodeDate(fVirtYear, fVirtMonth, fVirtDay);
      start   := EncodeDate(YearZero, 3, 9);
      days    := trunc(current - start);
      hours   := days*24;
      if fAbsDate <> hours
        then fAbsDate := hours;
    end;

  procedure TWorld.LoadFromBackup( Reader : IBackupReader );

    procedure UpdateTycoons;
      var
        i : integer;
      begin
        for i := 0 to pred(Tycoons.Count) do
          with TTycoon(Tycoons[i]) do
            begin
              Timer        := self;
              WorldLocator := self;
              MailServer   := self.MailServer;
            end;
      end;

    procedure UpdateCompanies;
      var
        i, j : integer;
      begin
        for i := 0 to pred(Companies.Count) do
          with TCompany(Companies[i]) do
            begin
              if Facilities = nil
                then
                  begin
                    Facilities := TLockableCollection.Create( 0, rkUse );
                    for j := 0 to pred(self.Facilities.Count) do
                      if TFacility(self.Facilities[j]).Company = Companies[i]
                        then Facilities.Insert( self.Facilities[j] );
                  end;
            end;
      end;

    procedure LinkClusterCompanies;
      var
        i : integer;
      begin
        for i := 0 to pred(TheClassStorage.ClassCount[tidClassFamily_Clusters]) do
          with TCluster(TheClassStorage.ClassByIdx[tidClassFamily_Clusters, i]) do
            Company := CompanyByName[Id];
      end;

    procedure WipeRedZones;
      var
        i, j : integer;
      begin
        for i := 0 to pred(fZones.Rows) do
          for j := 0 to pred(fZones.Cols) do
            if fZones[i, j] = znReserved
              then fZones[i, j] := znNone;
      end;

    var
      kind      : TPeopleKind;
      BckReader : IBackupReader;
      BckPath   : string;
      i, cnt    : integer;
      Obj       : TObject;
      Fixups    : TObject;
      RoadOwnId : integer;
    begin
      inherited;
      csMapLock := TCriticalSection.Create; //TAsymetrixCriticalSection.Create;
      fWorldLock := TCriticalSection.Create;
      fTimeLock  := TCriticalSection.Create;
      fWorldSuppliers := TLockableCollection.Create(0, rkUse);
      //InitializeCriticalSection( csFacilityList );
      fName  := Reader.ReadString( 'Name', 'Unknown' );
      fxSize := Reader.ReadInteger( 'xSize', defWorldXSize );
      fySize := Reader.ReadInteger( 'ySize', defWorldYSize );
      getmem( fObjectMap, xSize*ySize*sizeof(TObjId));
      getmem( fGroundMap, xSize*ySize*sizeof(TLandVisualClassId));
      fillchar( fObjectMap^, xSize*ySize*sizeof(TObjId), 0 );
      fillchar( fGroundMap^, xSize*ySize*sizeof(TLandVisualClassId), 0 );
      Log( 'Survival', 'Reading towns...' );
      Reader.ReadObject( 'Towns', fTowns, nil );
      if fTowns = nil
        then fTowns := TLockableCollection.Create( 0, rkBelonguer )
        else fTowns.Pack;
      Log( 'Survival', 'Reading towns OK.' );
      Log( 'Survival', 'Reading companies...' );
      Reader.ReadObject( 'Companies', fCompanies, nil );
      if fCompanies = nil
        then fCompanies := TLockableCollection.Create( 0, rkBelonguer )
        else fCompanies.Pack;
      Log( 'Survival', 'Reading companies OK.' );
      Log( 'Survival', 'Reading facilities...' );
      Reader.ReadObject( 'Facilities', fFacilities, nil );
      if fFacilities = nil
        then
          begin
            fFacilities := TLockableCollection.Create(0, rkBelonguer);
            BckPath     := Reader.ReadString('FacsPath', '');
            BckReader   := BackupObjects.OpenBackupFromPath(BckPath, nil, false);
            Fixups      := BckReader.GetFixups;
            BckReader.SetFixups(Reader.GetFixups);
            cnt         := BckReader.ReadInteger('', 0);
            for i := 0 to pred(cnt) do
              begin
                BckReader.ReadObject('', Obj, nil);
                fFacilities.Insert(Obj);
              end;
            BckReader.SetFixups(Fixups);
          end
        else fFacilities.Pack;
      Log( 'Survival', 'Reading facilities OK.' );
      Log( 'Survival', 'Net facilities: ' + IntToStr(NetFacilities) );
      Log( 'Survival', 'Reading tycoons...' );
      Reader.ReadObject( 'Tycoons', fTycoons, nil );
      if fTycoons = nil
        then fTycoons := TLockableCollection.Create( 0, rkBelonguer )
        else fTycoons.Pack;
      Log( 'Survival', 'Reading tycoons OK.' );
      Log( 'Survival', 'Updating tycoons...' );
      UpdateTycoons;
      Log( 'Survival', 'Updating tycoons OK.' );
      Log( 'Survival', 'Updating companies...' );
      UpdateCompanies;
      Log( 'Survival', 'Updating companies OK.' );
      Log( 'Survival', 'Reading circuits...' );
      Reader.ReadObject( 'Circuits', fCircuits, nil );
      if fCircuits = nil
        then
          begin
            fCircuits := TLockableCollection.Create( 0, rkBelonguer );
            fRoads    := TMatrixCircuitMap.Create( cirRoads, TNode, TSegment, TCircuit ); // TCircuitMap.Create( cirRoads, TNode, TSegment, TCircuit );
            fCircuits.Insert( fRoads );
          end
        else
          begin
            fRoads := CircuitById[cirRoads];
            fRoads.NodeClass    := TNode;
            fRoads.SegmentClass := TSegment;
            fRoads.CircuitClass := TCircuit;
          end;
      fRoads.SetSize( xSize, ySize );
      Log( 'Survival', 'Fixing circuits...' );
      if TheGlobalConfigHandler.GetConfigParm('RepairRoads', 'yes') = 'yes'
        then
          begin
            RoadOwnId := StrToInt(TheGlobalConfigHandler.GetConfigParm('BannedRoadsId', '0'));
            fRoads.IgonreId := RoadOwnId;
            fRoads.Fix;
          end;
      Log( 'Survival', 'Reading circuits OK.' );
      {
      Railroads := TCircuitMap.Create( cirRailRoads, TNode, TSegment, TCircuit );
      Railroads.OnAuthorizeBreak := OnAuthorizeBreak;
      Railroads.OnRenderExtraSegInfo := OnRenderExtraSegInfo;
      fCircuits.Insert( Railroads );
      }
      fRoads.OnAuthorizeBreak := OnAuthorizeBreak;
      fRoads.OnRenderExtraSegInfo := OnRenderExtraSegInfo;
      Log( 'Survival', 'Packing circuits...' );
      fCircuits.Pack;
      Log( 'Survival', 'Packing circuits OK.' );
      Log( 'Survival', 'Reading Main Bank...' );
      Reader.ReadObject( 'MainBank', fMainBank, nil );
      if fMainBank = nil
        then fMainBank := TBank.Create( nil );
      fMainBank.Name  := 'Bank of IFEL';
      fMainBank.Timer := self;
      fMainBank.Locator := self;
      fMainBank.Term  := 20;
      Log( 'Survival', 'Reading Main OK.' );
      Log( 'Survival', 'Reading Zones...' );
      Reader.ReadObject( 'Zones', fZones, nil );
      if fZones = nil
        then fZones := TByteLargeMatrix.Create( ySize, xSize, 32 );
        //else WipeRedZones;
      Log( 'Survival', 'Reading Zones OK.' );
      Log( 'Survival', 'Reading Extensions...' );
      Reader.ReadObject( 'WorldExtensions', fWorldExtensions, nil );
      if fWorldExtensions = nil
        then fWorldExtensions := TCollection.Create( 0, rkBelonguer );
      Log( 'Survival', 'Reading extensions OK.' );
      if fZoningMessages = nil
      then
        fZoningMessages := TLockableCollection.Create( 0, rkBelonguer);
      fZoneMessSent     := 0;
      //fTycoonIndex      := 0;
      fVirtDay          := Reader.ReadInteger( 'VirtDay', 1 );
      fVirtMonth        := Reader.ReadInteger( 'VirtMonth', 1 );
      fVirtYear         := Reader.ReadInteger( 'VirtYear', YearZero );
      fAbsDate          := Reader.ReadInteger( 'AbsDate', 0 );
      fYearCount        := Reader.ReadInteger( 'YearCount', 0 );
      fSeason           := TSeason(Reader.ReadInteger( 'Season', 0 ));
      fSeasonProgress   := SeasonIntro; // >>
      fRandomSeasons    := TheGlobalConfigHandler.GetConfigParm('RamdomSeasons', '0') = '1';
      if fRandomSeasons
        then
          begin
            fEndSeasonDates[seasWinter].day   := Reader.ReadByte('wn.d', 10);
            fEndSeasonDates[seasWinter].month := Reader.ReadByte('wn.m', 3);

            fEndSeasonDates[seasSpring].day   := Reader.ReadByte('sp.d', 10);
            fEndSeasonDates[seasSpring].month := Reader.ReadByte('sp.m', 6);

            fEndSeasonDates[seasSummer].day   := Reader.ReadByte('sm.d', 10);
            fEndSeasonDates[seasSummer].month := Reader.ReadByte('sm.m', 9);

            fEndSeasonDates[seasFall].day     := Reader.ReadByte('fl.d', 10);
            fEndSeasonDates[seasFall].month   := Reader.ReadByte('fl.m', 12);
          end
        else
          begin
            InitSeasonDates;
          end;
      fTimeStopped      := Reader.ReadInteger( 'TimeStopped', 0 );
      fLastUpdate       := Reader.ReadInteger( 'LastUpdate', 0 );
      fLastCompId       := Reader.ReadInteger( 'LastCompId', 0 );
      fLastTycoonId     := Reader.ReadInteger( 'LastTycoonId', 0 );
      fLastSimulation   := Reader.ReadInteger( 'LastSimulation', 0 );
      fYearsToElections := Reader.ReadInteger( 'YearsToElections', 0 );
      fdt               := Reader.ReadInteger( 'dt', 1 );
      fNewMeat          := TLockableCollection.Create( 0, rkBelonguer );
      fDeadMeat         := TLockableCollection.Create( 0, rkBelonguer );
      fDeadTycoons      := TLockableCollection.Create( 0, rkBelonguer );
      fDeadCompanies    := TLockableCollection.Create( 0, rkBelonguer );
      fPendingDeletions := TLockableCollection.Create( 0, rkUse );

      {// ?????
      if fVirtMonth = 2
        then
          begin
            fVirtDay := 7;
            fSeason := seasWinter;
          end;
      }

      for kind := low(kind) to high(kind) do
        fMinSalaries[kind] := Reader.ReadByte( 'MinSalary.' + PeopleKindPrefix[kind], 0 );
      Log( 'Survival', 'Linking cluster companies...' );
      LinkClusterCompanies;
      //Log( 'Survival', 'Updating facility map...' );
      //UpdateFacilitiesInMap;
      Log( 'Survival', 'Initializing surface pool...' );
      fSurfacePool := TSurfacePool(TheClassStorage.ClassById[tidClassFamily_SurfacePools, tidSurfacePool_Surfaces]);
      Log( 'Survival', 'Initializing cargo system...' );
      InitCargoSystem;
      Log( 'Survival', 'Creating awaken tycoons collection...' );
      fAwakenTycoons := TLockableCollection.Create( 0, rkBelonguer );
      Log( 'Survival', 'Creating actor pool collection...' );
      fActorPools := TLockableCollection.Create( 0, rkBelonguer );
      fTimeStopped := 0;
      fSplitBackup := true;

      //Reader.ReadObject('MediaHistory', fMediaNameHistory, nil);
      fMediaNameHistory := nil; // >> ***
      if fMediaNameHistory = nil
        then fMediaNameHistory := TMediaNameHistory.Create;

      if TheGlobalConfigHandler.GetConfigParm('RelayEconomy', '0') = '1'
        then
          begin
            Reader.ReadObject('EconomyRelay', fEcconomyRelay, nil);
            if fEcconomyRelay = nil
              then fEcconomyRelay := TEconomyRelay.Create;
          end
        else fEcconomyRelay := nil;
      SynchronizeClock;  
    end;

  procedure TWorld.StoreToBackup( Writer : IBackupWriter );
    var
      kind      : TPeopleKind;
      aux       : string;
      Facs      : TCollection;
      BckPath   : string;
      FacWriter : IBackupWriter;
      i         : integer;
    begin
      inherited;
      Writer.WriteString( 'Name', fName );
      Writer.WriteInteger( 'xSize', fxSize );
      Writer.WriteInteger( 'ySize', fySize );
      //Logs.LogMemReport('Survival');
      Logs.Log( 'Survival', TimeToStr(Now) + ' Storing Towns..' );
      Writer.WriteLooseObject( 'Towns', fTowns );
      //Logs.LogMemReport('Survival');
      Logs.Log( 'Survival', TimeToStr(Now) + ' Storing Companies..' );
      Writer.WriteLooseObject( 'Companies', fCompanies );
      //Logs.LogMemReport('Survival');

      Logs.Log( 'Survival', TimeToStr(Now) + ' Storing Facilities..' );
      if not fSplitBackup
        then Writer.WriteLooseObject('Facilities', fFacilities)
        else
          begin
            Writer.WriteLooseObject('Facilities', nil);
            BckPath := Writer.Path + '.facs';
            Writer.WriteString('FacsPath', BckPath);
          end;

      //Logs.LogMemReport('Survival');
      Logs.Log( 'Survival', TimeToStr(Now) + ' Storing Tycoons..' );
      Writer.WriteLooseObject( 'Tycoons', fTycoons );
      //Logs.LogMemReport('Survival');

      Logs.Log( 'Survival', TimeToStr(Now) + ' Storing Circuits..' );
      fCircuits.Lock;
      try
        for i := 0 to pred(fCircuits.Count) do
          TCircuitMap(fCircuits[i]).AutoRepair;
        Writer.WriteLooseObject( 'Circuits', fCircuits );
      finally
        fCircuits.Unlock;
      end;
      //Logs.LogMemReport('Survival');

      Logs.Log( 'Survival', TimeToStr(Now) + ' Storing MainBank..' );
      Writer.WriteObject( 'MainBank', fMainBank );
      //Logs.LogMemReport('Survival');
      Logs.Log( 'Survival', TimeToStr(Now) + ' Storing Zones..' );
      Writer.WriteObject( 'Zones', fZones );
      //Logs.LogMemReport('Survival');
      Logs.Log( 'Survival', TimeToStr(Now) + ' Storing WorldExtensions..' );
      Writer.WriteObject( 'WorldExtensions', fWorldExtensions );
      Logs.Log( 'Survival', TimeToStr(Now) + ' Storing World Miscs..' );
      //Logs.LogMemReport('Survival');
      Writer.WriteInteger( 'VirtDay', fVirtDay );
      Writer.WriteInteger( 'VirtMonth', fVirtMonth );
      Writer.WriteInteger( 'VirtYear', fVirtYear );
      Writer.WriteInteger( 'AbsDate', fAbsDate );
      Writer.WriteInteger( 'YearCount', fYearCount );
      Writer.WriteInteger( 'Season', integer(fSeason) );
      if fRandomSeasons
        then
          begin
            Writer.WriteByte('wn.d', fEndSeasonDates[seasWinter].day);
            Writer.WriteByte('wn.m', fEndSeasonDates[seasWinter].month);

            Writer.WriteByte('sp.d', fEndSeasonDates[seasSpring].day);
            Writer.WriteByte('sp.m', fEndSeasonDates[seasSpring].month);

            Writer.WriteByte('sm.d', fEndSeasonDates[seasSummer].day);
            Writer.WriteByte('sm.m', fEndSeasonDates[seasSummer].month);

            Writer.WriteByte('fl.d', fEndSeasonDates[seasFall].day);
            Writer.WriteByte('fl.m', fEndSeasonDates[seasFall].month);
          end;
      Writer.WriteInteger( 'TimeStopped', fTimeStopped );
      Writer.WriteInteger( 'LastUpdate', fLastUpdate );
      Writer.WriteInteger( 'LastCompId', fLastCompId );
      Writer.WriteInteger( 'LastTycoonId', fLastTycoonId );
      Writer.WriteInteger( 'LastSimulation', fLastSimulation );
      Writer.WriteInteger( 'YearsToElections', fYearsToElections );
      Writer.WriteInteger( 'dt', round(fdt) );
      //Logs.LogMemReport('Survival');
      for kind := low(kind) to high(kind) do
        begin
          aux := 'MinSalary.' + PeopleKindPrefix[kind];
          Writer.WriteByte( aux, fMinSalaries[kind] );
        end;
      //Logs.Log( 'Survival', TimeToStr(Now) + ' End Storing World..' );
      aux := '';
      //Logs.LogMemReport('Survival');

      // Clone the BackupWriter and write all the facilities...
      if fSplitBackup
        then
          begin
            Facs := TCollection.Create(fFacilities.Count, rkUse);
            Facs.InsertColl(fFacilities);
            FacWriter := BackupObjects.CreateBackup(BckPath, false, nil, false);
            TBackupThread.Create(FacWriter, Facs, fWorldLock, tpLower);
          end;
      //Writer.WriteObject('MediaHistory', fMediaNameHistory);
      if fEcconomyRelay <> nil
        then Writer.WriteObject('EconomyRelay', fEcconomyRelay);
    end;

  procedure TWorld.Loaded( Notify : TBackupReaderNotify );

    procedure SendToCache( collName : string; Collection : TCollection );
      var
        i : integer;
      begin
        for i := 0 to pred(Collection.Count) do
          try
            CacheObject( Collection[i], noKind, noInfo );
            Notify( 'Caching ' + collName + '...', 100*succ(i) div Collection.Count );
          except
          end;
      end;

    procedure CreateLinks;
      var
        i   : integer;
        Fac : TFacility;
      begin
        for i := 0 to pred(Facilities.Count) do
          try
            Fac := TFacility(Facilities[i]);
            Fac.CreateCacheLinks;
            Notify('Creating links...', 100*succ(i) div Facilities.Count);
          except
          end;
      end;

    procedure InitRankings;
      var
        i, j  : integer;
        count : integer;
      begin
        count := TheClassStorage.ClassCount[tidClassFamily_Rankings];
        for i := 0 to pred(count) do
          begin
            for j := 0 to pred(fTycoons.Count) do
              with TTycoon(fTycoons[j]) do
                if tyoMainRole in Options
                  then TRanking(TheClassStorage.ClassByIdx[tidClassFamily_Rankings, i]).AddObject( fTycoons[j] );
            CacheObject( TheClassStorage.ClassByIdx[tidClassFamily_Rankings, i], noKind, noInfo );
          end;
      end;

    procedure CheckTycoons;
      var
        i, j   : integer;
        Tycoon : TTycoon;
        CurTyc : TTycoon;
        times  : integer;
        nm     : string;
      begin
        if TheGlobalConfigHandler.GetConfigParm('RemoveVisitors', '1') = '1'
          then
            // remove vistors
            for i := pred(fTycoons.Count) downto 0 do
              begin
                Tycoon := TTycoon(fTycoons[i]);
                if not Tycoon.HasLegacy
                  then DeleteTycoon(Tycoon, true);
              end;

        // remove duplicates
        i := 0;
        while i < fTycoons.Count do
          begin
            times  := 1;
            Tycoon := TTycoon(fTycoons[i]);
            nm     := trim(UpperCase(Tycoon.Name));
            j := succ(i);
            while j < fTycoons.Count do
              begin
                CurTyc := TTycoon(fTycoons[j]);
                if trim(UpperCase(CurTyc.Name)) = nm
                  then
                    begin
                      inc(times);
                      fTycoons.AtExtract(j);
                    end
                  else inc(j);
              end;
            if times > 1
              then Logs.Log('Tycoons', Format('Tycoon "%s" repeated %d times', [Tycoon.Name, times]));
            inc(i);
          end;
        (*
        totFac := 0;
        i := pred(fTycoons.Count);
        while i >= 0 do
          begin
            Tycoon := TTycoon(fTycoons[i]);
            facCnt := 0;
            Tycoon.Companies.Lock;
            try
              for j := 0 to pred(Tycoon.Companies.Count) do
                inc( facCnt, TCompany(Tycoon.Companies[j]).Facilities.Count );
            finally
              Tycoon.Companies.Unlock;
            end;
            inc(totFac, facCnt);
            if facCnt > 1500
              then Logs.Log('ExpTycoons', Format('%s >> %d', [Tycoon.Name, FacCnt]));
            dec(i);
          end;
        Logs.Log('ExpTycoons', Format('Total: %d', [totFac]));
        *)
      end;

    procedure CheckCompanies;
      var
        Comp : TCompany;
        Fac  : TFacility;
        i, j : integer;
        flag : boolean;
      begin
        for i := pred(fCompanies.Count) downto 0 do
          begin
            Comp := TCompany(fCompanies[i]);
            if Comp.Privated and (Comp.Owner = nil)
              then
                begin
                  flag := false;
                  for j := pred(Comp.Facilities.Count) downto 0 do
                    begin
                      Fac := TFacility(Comp.Facilities[j]);
                      if not (mfcAceptsNoOwner in Fac.MetaFacility.Options)
                        then
                          begin
                            flag := true;
                            Self.fFacilities.Extract(Fac);
                            Comp.Facilities.Extract(Fac);
                          end;
                    end;
                  if flag and (Comp.Facilities.Count = 0)
                     then fCompanies.Extract(Comp);
                end;
          end;
      end;

    procedure UpdateFacilitiesInMap;
      var
        i   : integer;
        Fac : TFacility;
      begin
        for i := 0 to pred(fFacilities.Count) do
          try
            Fac := TFacility(fFacilities[i]);
            // Update in map if it is a valid facility.
            if (Fac.Company <> nil) and not Fac.Deleted
              then UpdateInMap(Fac);
            Notify('Updating facilities in map...', 100*i div fFacilities.Count);
          except
            Log( 'Survival', 'Error updating map ' + IntToStr(i) );
          end;
      end;

    var
      i   : integer;
      Fac : TFacility;
      Cmp : TCompany;
    begin
      fNotify := Notify;
      Log( 'Survival', 'Initializing towns...' );
      for i := 0 to pred(Towns.Count) do
        try
          TTown(Towns[i]).Loaded;
          Notify( 'Initializing towns...', 100*i div Towns.Count );
        except
          Log( 'Survival', 'Error initializing object ' + IntToStr(i) );
        end;

      Log( 'Survival', 'Initializing facilities...' );
      for i := 0 to pred(Facilities.Count) do
        try
          Fac := TFacility(Facilities[i]);
          Fac.Loaded;
          // Add facility to World Suppliers if Warehouse
          if (Fac.CurrBlock <> nil) and (Fac.CurrBlock.Role in [rolDistributer, rolCompExport, rolCompInport])
            then fWorldSuppliers.Insert(Fac);
          Notify( 'Initializing facilities...', 100*i div Facilities.Count );
        except
          Log( 'Survival', 'Error initializing object ' + IntToStr(i) );
        end;

      Log('Survival', 'Checking Companies...');
      CheckCompanies;

      Log( 'Survival', 'Updating facility map...' );
      UpdateFacilitiesInMap;

      Log( 'Survival', 'Initializing tycoons...' );
      for i := 0 to pred(Tycoons.Count) do
        try
          if ValidName(TTycoon(Tycoons[i]).Name)
            then
              begin
                TTycoon(Tycoons[i]).Loaded;
                if (tyoMainRole in TTycoon(Tycoons[i]).Options) and (YearsWithoutConnecting( TTycoon(Tycoons[i]) ) > MaxYearsWithoutConnecting)
                  then RequestDeleteTycoon( TTycoon(Tycoons[i]), true );
                Notify( 'Initializing tycoons...', 100*i div Tycoons.Count );
              end;
        except
          Log( 'Survival', 'Error initializing object ' + IntToStr(i) );
        end;

      // Config params
      fMaxInvestors     := StrToInt(TheGlobalConfigHandler.GetConfigParm('MaxInvestors', '200'));
      fFightFacColonies := TheGlobalConfigHandler.GetConfigParm('FightFacColonies', 'yes') = 'yes';

      //Tournaments params

      // CHANGE LATER
      fTournamentLength := StrToInt(TheGlobalConfigHandler.GetConfigParm('TornamentLength', '0'));
      fTornamentStart   := StrToInt(TheGlobalConfigHandler.GetConfigParm('TornamentStart',  '0'));
      fElectionsOn      := TheGlobalConfigHandler.GetConfigParm('ElectionsOn',    '1') = '1';
      fSendingMoneyOn   := TheGlobalConfigHandler.GetConfigParm('SendingMoney',   '1') = '1';
      fAlliesPageOn     := TheGlobalConfigHandler.GetConfigParm('AlliesPageOn',   '1') = '1';
      fTranscendingOn   := TheGlobalConfigHandler.GetConfigParm('TranscendingOn', '1') = '1';

      Log( 'Survival', 'Caching towns...' );
      if coTowns in CacheOptions
        then SendToCache( 'Towns', fTowns );

      Log( 'Survival', 'Caching towns...' );
      if coTycoons in CacheOptions
        then
          for i := 0 to pred(fTycoons.Count) do
            begin
              Notify('Caching Tycoons...', 100*succ(i) div fTycoons.Count);
              if ValidName(TTycoon(Tycoons[i]).Name)
                then CacheObject(Tycoons[i], noKind, noInfo);
            end;

      // Companies
      Log( 'Survival', 'Initializing companies...' );
      for i := 0 to pred(fCompanies.Count) do
        begin
          Cmp := TCompany(fCompanies[i]);
          Cmp.Loaded;
          if coCompanies in CacheOptions
            then
              begin
                Notify('Caching Companies...', 100*succ(i) div fCompanies.Count);
                if ValidName(Cmp.Name)
                  then CacheObject(Cmp, noKind, noInfo);
              end;
        end;
      {Log('Survival', 'Checking companies...');
      for i := pred(fCompanies.Count) downto 0 do
        begin
          Cmp := TCompany(fCompanies[i]);
          if (Cmp.Owner = nil) and Cmp.Privated
            then Log('Survival', 'Abandon company: ' + Cmp.Name);
        end;}

      // Facilities: Heavy Cache, not recommended..
      Log( 'Survival', 'Caching facilities...' );
      if coFacilities in CacheOptions
        then SendToCache( 'Facilities', fFacilities )
        else
          if coLinks in CacheOptions
            then CreateLinks;

      Log( 'Survival', 'Initializing actor pool...' );
      try
        Log( 'Survival', 'Actor pools: ' + IntToStr(fActorPools.Count) );
        for i := 0 to pred(fActorPools.Count) do
          try
            TServerActorPool(fActorPools[i]).SendTickData := SendActorPoolData;
          except
            Log( 'Survival', 'Error initializating actor pool ' + IntToStr(i) );
          end;
      except
        Log( 'Survival', 'Error initializing actor pool.' );
      end;
      Log( 'Survival', 'Initializing extensions...' );
      for i := 0 to pred(fWorldExtensions.Count) do
        TWorldExtension(fWorldExtensions[i]).Loaded( self );
      Log( 'Survival', 'Initializing rankings...' );
      InitRankings;
      Log( 'Survival', 'Checking tycoons.' );
      CheckTycoons;
      Log( 'Survival', 'Check Connections.' );
      // Init Circuits
      CheckConnections;
      Log( 'Survival', 'Initializing complete.' );
      // Clone Queue
      fCloneQueue := TLockableCollection.Create(0, rkBelonguer);
      // Build Lock
      fBuildLock := TCriticalSection.Create;
      fNewCompanyLock := TCriticalSection.Create;

      // Config params
      fMaxInvestors      := StrToInt(TheGlobalConfigHandler.GetConfigParm('MaxInvestors', '200'));
      fFightFacColonies  := TheGlobalConfigHandler.GetConfigParm('FightFacColonies', 'no')  = 'yes';
      fBldMagnaIfNoble   := TheGlobalConfigHandler.GetConfigParm('MagnaAtApprentice', 'yes') = 'yes';
      fResetPrestigeLost := StrToInt(TheGlobalConfigHandler.GetConfigParm('ResetPrestigeLost', '100'));

    end;

  procedure TWorld.SendActorPoolData( PoolId : TActorPoolId; ViewerId : TViewerId; TickCount : cardinal; TickData : TStream );
    begin
      if assigned(fOnPoolActed)
        then fOnPoolActed( PoolId, ViewerId, TickCount, TickData );
    end;

  procedure TWorld.RegisterActorPool( ActorPool : TServerActorPool );
    begin
      ActorPool.SendTickData := SendActorPoolData;
      fActorPools.Insert( ActorPool );
    end;

  procedure TWorld.UnregisterActorPool( ActorPool : TServerActorPool );
    begin
      fActorPools.Delete( ActorPool );
    end;

  function TWorld.GetActorPool( Id : TActorPoolId ) : TServerActorPool;
    var
      i : integer;
    begin
      i := 0;
      while (i < fActorPools.Count) and (TServerActorPool(fActorPools[i]).Id <> Id) do
        inc( i );
      if i < fActorPools.Count
        then result := TServerActorPool(fActorPools[i])
        else result := nil;
    end;

  function TWorld.YearsWithoutConnecting( Tycoon : TTycoon ) : integer;
    {var
      cookie    : string;
      lastCnx   : TDateTime;
      lastMonth : word;
      thisMonth : word;
      useless   : word;}
    begin
      {
      try
        cookie := Tycoon.Cookie[tidCookie_LastTimeOnline];
        if cookie <> ''
          then
            begin
              lastCnx := StrToDate( cookie );
              DecodeDate( lastCnx, useless, lastMonth, useless );
              DecodeDate( Now, useless, thisMonth, useless );
              if abs(thisMonth - lastMonth) > 1
                then result := 200
                else result := 0;
            end
          else result := 0;
      except
        result := 0;
      end;
      }
      result := 0;
    end;

  { Correct
  function TWorld.YearsWithoutConnecting( Tycoon : TTycoon ) : integer;
    var
      cookie   : string;
      lastCnx  : TDateTime;
      lastYear : word;
      useless  : word;
    begin
      try
        cookie := Tycoon.Cookie[tidCookie_LastVirtTimeOnline];
        if cookie <> ''
          then
            begin
              lastYear := StrToInt( cookie );
              result := fVirtYear - lastYear;
            end
          else result := 0;
      except
        result := 0;
      end;
    end;
  }

  function TWorld.GetWorldExtension( Id : string ) : TWorldExtension;
    var
      i : integer;
    begin
      i := 0;
      while (i < fWorldExtensions.Count) and (TWorldExtension(fWorldExtensions[i]).GetId <> Id) do
        inc( i );
      if i < fWorldExtensions.Count
        then result := TWorldExtension(fWorldExtensions[i])
        else result := nil;
    end;

  procedure TWorld.RegisterWorldExtension( Extension : TWorldExtension );
    begin
      fWorldExtensions.Insert( Extension );
    end;

(*  function TWorld.GenerateSatelliteMap( kind : integer ) : TBitmap;
    type
      TRGB =
        packed record
          r, g, b, x : byte;
        end;
    const
      ColorCount = 8;
      TownColors : array[0..ColorCount - 1] of TColor =
        (clFuchsia, clYellow, clMaroon, clOlive, clBlue, clLime, clNavy, clPurple);
      {
      TownColors : array[0..ColorCount - 1] of TColor =
        ($00444444,
         $00CCCCCC,
         $00666666,
         $00EEEEEE,
         $00888888,
         $00AAAAAA,
         $00222222);
      }
    var                                                               
      x, y, i   : integer;
      Segs      : TCollection;
      Fac       : TFacility;
      towncolor : TColor;
      color     : TColor;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Creating bitmap...' );
      result := TBitmap.Create;
      result.PixelFormat := pf32bit;
      result.Width  := fLandBitmap.Width;
      result.Height := fLandBitmap.Height;
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Copying Land Bitmap...' );
      result.Canvas.Draw( 0, 0, fLandBitmap );
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Rendering facilities' );
      for x := 0 to pred(result.Width) do
        for y := 0 to pred(result.Height) do
          if Land.LandClassOf( GroundMap[x, y] ) <> lncZoneD
            then
              begin
                Fac := FacilityAt( x, y );
                if (Fac <> nil) and (Fac.MetaFacility <> nil)
                  then
                    begin
                      case Fac.MetaFacility.ZoneType of
                        znHiResidential :
                          color := $00BBFFC0;
                        znMidResidential :
                          color := $0043A34F;
                        znLoResidential :
                          color := $001E4823;
                        znIndustrial :
                          color := $0088D9D7;
                        znCommercial :
                          color := $00D87449;
                        else
                          if (Fac.Company = nil) or (Fac.Company.Owner = nil) or (Fac.Company.Owner.SuperRole <> nil)
                            then color := clWhite
                            else color := clGray;
                      end;
                      result.Canvas.Pixels[x, y] := color;
                    end
                  else
                    if (x > 0) and (x < xSize) and (y > 0) and (y < ySize) and
                       ((TownMap[x, y] <> TownMap[x + 1, y + 1]) or (TownMap[x, y] <> TownMap[x - 1, y - 1]) or
                        (TownMap[x, y] <> TownMap[x - 1, y + 1]) or (TownMap[x, y] <> TownMap[x + 1, y - 1]))
                      then
                        begin
                          towncolor := clWhite;
                          color     := result.Canvas.Pixels[x, y];
                          TRGB(towncolor).r := (integer(TRGB(towncolor).r) + 2*TRGB(color).r) div 3;
                          TRGB(towncolor).g := (integer(TRGB(towncolor).g) + 2*TRGB(color).g) div 3;
                          TRGB(towncolor).b := (integer(TRGB(towncolor).b) + 2*TRGB(color).b) div 3;
                          TRGB(towncolor).x := (integer(TRGB(towncolor).x) + 2*TRGB(color).x) div 3;
                          result.Canvas.Pixels[x, y] := towncolor;
                        end;
              end
          else
            if Land.LandTypeOf( GroundMap[x, y] ) = ldtCenter
              then
                begin
                  color := result.Canvas.Pixels[x, y];
                  {
                  TRGB(color).r := TRGB(color).r div 3;
                  TRGB(color).g := TRGB(color).g div 3;
                  TRGB(color).b := TRGB(color).b div 3;
                  TRGB(color).x := TRGB(color).x div 3;
                  }
                  result.Canvas.Pixels[x, y] := color;
                end;
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Rendering roads...' );
      result.Canvas.Pen.Color := $333333;
      result.Canvas.Pen.Style := psSolid;
      Segs := TCollection.Create( 0, rkUse );
      fRoads.FindSegsInArea( 0, 0, result.Height, result.Height, Segs );
      for i := 0 to pred(Segs.Count) do
        with TSegment(Segs[i]) do
          begin
            result.Canvas.MoveTo( NodeA.x, NodeA.y );
            result.Canvas.LineTo( NodeB.x, NodeB.y );
          end;
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Sending Event...' );
      try
        SendEvent(
          TEvent.Create(
            0,
            VirtualTimeAbs,
            VirtualTime,
            100,
            1000,
            InstantiateMultiString( mtidSatelliteUpdated, [Name] ),
            //NullString, //'The satellite map of ' + Name + ' has been updated.',
            '', '' ));
      except
      end;
    end;*)

  function TWorld.GenerateSatelliteMap( kind : integer ) : TBitmap;
    type
      TRGB =
        packed record
          r, g, b, x : byte;
        end;
    const
      ColorCount = 8;
      TownColors : array[0..ColorCount - 1] of TColor =
        (clFuchsia, clYellow, clMaroon, clOlive, clBlue, clLime, clNavy, clPurple);
      MapSize = 1000;
    var
      x, y, i   : integer;
      Segs      : TCollection;
      Fac       : TFacility;
      towncolor : TColor;
      color     : TColor;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Creating bitmap...' );
      result := TBitmap.Create;
      result.PixelFormat := pf32bit;
      result.Width  := MapSize;
      result.Height := MapSize;
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Copying Land Bitmap...' );
      result.Canvas.StretchDraw( Rect(0, 0, fLandBitmap.Width, fLandBitmap.Height), fLandBitmap );
      // Draw( 0, 0, fLandBitmap );
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Rendering facilities' );
      for x := 0 to pred(xSize) do
        for y := 0 to pred(ySize) do
          begin
            result.Canvas.Pixels[MapSize*x div xSize, MapSize*y div ySize] := fLandBitmap.Canvas.Pixels[x, y];
            Fac := FacilityAt( x, y );
            if (Fac <> nil) and (Fac.MetaFacility <> nil)
              then
                begin
                  case Fac.MetaFacility.ZoneType of
                    znHiResidential :
                      color := $00BBFFC0;
                    znMidResidential :
                      color := $0043A34F;
                    znLoResidential :
                      color := $001E4823;
                    znIndustrial :
                      color := $0088D9D7;
                    znCommercial :
                      color := $00D87449;
                    else
                      if (Fac.Company = nil) or (Fac.Company.Owner = nil) or (Fac.Company.Owner.SuperRole <> nil)
                        then color := clWhite
                        else color := clGray;
                  end;
                  result.Canvas.Pixels[MapSize*x div xSize, MapSize*y div ySize] := color;
                end
              else
                if (x > 0) and (x < pred(xSize)) and (y > 0) and (y < pred(ySize)) and
                   ((TownMap[x, y] <> TownMap[x + 1, y + 1]) or (TownMap[x, y] <> TownMap[x - 1, y - 1]) or
                    (TownMap[x, y] <> TownMap[x - 1, y + 1]) or (TownMap[x, y] <> TownMap[x + 1, y - 1]))
                  then
                    begin
                      towncolor := clWhite;
                      color     := result.Canvas.Pixels[MapSize*x div xSize, MapSize*y div ySize];
                      TRGB(towncolor).r := (integer(TRGB(towncolor).r) + 2*TRGB(color).r) div 3;
                      TRGB(towncolor).g := (integer(TRGB(towncolor).g) + 2*TRGB(color).g) div 3;
                      TRGB(towncolor).b := (integer(TRGB(towncolor).b) + 2*TRGB(color).b) div 3;
                      TRGB(towncolor).x := (integer(TRGB(towncolor).x) + 2*TRGB(color).x) div 3;
                      result.Canvas.Pixels[MapSize*x div xSize, MapSize*y div ySize] := towncolor;
                    end;
          end;
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Rendering roads...' );
      result.Canvas.Pen.Color := $333333;
      result.Canvas.Pen.Style := psSolid;
      Segs := TCollection.Create( 0, rkUse );
      fRoads.FindSegsInArea( 0, 0, result.Height, result.Height, Segs );
      for i := 0 to pred(Segs.Count) do
        with TSegment(Segs[i]) do
          begin
            result.Canvas.MoveTo( MapSize*NodeA.x div xSize, MapSize*NodeA.y div xSize );
            result.Canvas.LineTo( MapSize*NodeB.x div xSize, MapSize*NodeB.y div xSize );
          end;
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Sending Event...' );
      try
        SendEvent(
          TEvent.Create(
            0,
            VirtualTimeAbs,
            VirtualTime,
            100,
            1000,
            InstantiateMultiString( mtidSatelliteUpdated, [Name] ),
            //NullString, //'The satellite map of ' + Name + ' has been updated.',
            '', '' ));
      except
      end;
    end;

  function TWorld.QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TWorld._AddRef  : integer; stdcall;
    begin
      result := 1;
    end;

  function TWorld._Release : integer; stdcall;
    begin
      result := 1;
    end;

  procedure TWorld.DSLock;
    begin
      if fDirProxyLock <> nil
        then fDirProxyLock.Enter;
    end;

  procedure TWorld.DSUnlock;
    begin
      if fDirProxyLock <> nil
        then fDirProxyLock.Leave;
    end;

  procedure TWorld.QueueFacilityToClone(aFac : TFacility; anOptions : TCloningOptions);
    var
      i  : integer;
      CI : TCloneFacilityInfo;
    begin
      try
        fCloneQueue.Lock;
        try
          i := pred(fCloneQueue.Count);
          while (i >= 0) and (TCloneFacilityInfo(fCloneQueue[i]).fFacility <> aFac) do
            dec(i);
          if i < 0
            then fCloneQueue.Insert(TCloneFacilityInfo.Create(aFac, anOptions))
            else
              begin
                CI := TCloneFacilityInfo(fCloneQueue[i]);
                CI.fOptions := anOptions;
              end;
        finally
          fCloneQueue.Unlock;
        end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in QueueFacilityToClone..' );
      end;
    end;

  procedure TWorld.RemoveFacilityToClone(aFac : TFacility);
    var
      i : integer;
    begin
      try
        fCloneQueue.Lock;
        try
          i := pred(fCloneQueue.Count);
          while (i >= 0) and (TCloneFacilityInfo(fCloneQueue[i]).fFacility <> aFac) do
            dec(i);
          if i >= 0
            then fCloneQueue.AtDelete(i);
        finally
          fCloneQueue.Unlock;
        end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in RemoveFacilityToClone..' );
      end;
    end;

  procedure TWorld.CloneFacilities;
    var
      CI      : TCloneFacilityInfo;
      cloning : boolean;
      text    : string;
    begin
      try
        cloning := true;
        while cloning do
          begin
            // Get the facility to clone.
            fCloneQueue.Lock;
            try
              if fCloneQueue.Count > 0
                then CI := TCloneFacilityInfo(fCloneQueue.AtExtract(0))
                else CI := nil;
            finally
              fCloneQueue.Unlock;
            end;
            // Clone the facility if any
            if CI <> nil
              then
                begin
                  text := CI.fFacility.Name + ' x: ' + IntToStr(CI.fFacility.xPos) + ' y: ' + IntToStr(CI.fFacility.yPos);
                  Logs.Log(tidLog_Survival, TimeToStr(Now) + ' Now Cloning: ' + text);
                  CloneFacility(CI.fFacility, CI.fOptions);
                  Logs.Log(tidLog_Survival, TimeToStr(Now) + ' Cloned: ' + text);
                  CI.Free;
                end
              else cloning := false;
          end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in CloneFacilities..' );
      end;
    end;

  procedure TWorld.ClearSimVars;
    begin
      fInputTotal  := 0;
      fOutputTotal := 0;
      fInCnnts     := 0;
      fOutCnnts    := 0;
      fMaxIn       := 0;
      fMaxOut      := 0;
    end;

  procedure TWorld.CountInputConnections(count : integer);
    begin
      inc(fInCnnts, count);
      fMaxIn := max(fMaxIn, count);
    end;

  procedure TWorld.CountOutputConnections(count : integer);
    begin
      inc(fOutCnnts, count);                         
      fMaxOut := max(fMaxOut, count);
    end;

  function TWorld.UpdateNobility(Tycoon : TTycoon) : boolean;
    var
      key : string;
      nob : integer;
    begin
      try
        DSLock;
        try
          if (Tycoon <> nil) and not Tycoon.IsRole and not VarIsEmpty(DirProxy) and not VarIsNull(DirProxy)
            then
              begin
                key := GetUserPath(Tycoon.Name);
                if DirProxy.RDOSetCurrentKey(key)
                  then
                    begin
                      nob := DirProxy.RDOReadInteger('NobPoints');
                      Tycoon.NobPoints := max(0, nob); // max(5, nob);
                    end;
              end;
        finally
          DSUnlock;
        end;
        result := true;
      except
        result := false;
      end;
    end;

  procedure TWorld.RegisterTitle(name : string);
    begin
      if fMediaNameHistory <> nil
        then fMediaNameHistory.Add(name);
    end;

  function TWorld.NumberTitle(name, sep : string) : string;
    begin
      if fMediaNameHistory <> nil
        then result := fMediaNameHistory.AddNumber(name, sep)
        else result := name;
    end;

  procedure TWorld.InitSeasonDates;
    begin
      fEndSeasonDates[seasWinter].day := 10;
      fEndSeasonDates[seasWinter].month := 3;

      fEndSeasonDates[seasSpring].day := 10;
      fEndSeasonDates[seasSpring].month := 6;

      fEndSeasonDates[seasSummer].day := 10;
      fEndSeasonDates[seasSummer].month := 9;

      fEndSeasonDates[seasFall].day := 10;
      fEndSeasonDates[seasFall].month := 12;
    end;

  procedure TWorld.GenerateSeasonDates;

    function RndSeasonLength(m, mdf : integer) : integer;
      var
        angle : integer;
      begin
        angle  := random(9000); // 100*PI
        result := round(m + mdf*cos(angle));
      end;

    var
      yLen  : integer;
      sLens : array[TSeason] of integer;
      s     : TSeason;
      sum   : integer;

      // decode parms
      dd    : word;
      mm    : word;
      yy    : word;
      dt    : TDateTime;
    begin
      if fRandomSeasons
        then
          begin
            yLen := 365 + 15 - random(30);
            sLens[seasWinter] := RndSeasonLength(100, 30);
            sLens[seasSpring] := RndSeasonLength(90,  30);
            sLens[seasSummer] := RndSeasonLength(90,  30);
            sLens[seasFall]   := RndSeasonLength(90,  30);
            sum := 0;
            for s := low(s) to high(s) do
              inc(sum, sLens[s]);
            for s := low(s) to high(s) do
              sLens[s] := round(yLen*sLens[s]/sum);
            // calc dates
            dt := EncodeDate(fVirtYear, fVirtMonth, fVirtDay);
            // winter
            dt := dt + sLens[seasWinter];
            DecodeDate(dt, yy, mm, dd);
            fEndSeasonDates[seasWinter].day := dd;
            fEndSeasonDates[seasWinter].month := mm;
            // spring
            dt := dt + sLens[seasSpring];
            DecodeDate(dt, yy, mm, dd);
            fEndSeasonDates[seasSpring].day := dd;
            fEndSeasonDates[seasSpring].month := mm;
            // summer
            dt := dt + sLens[seasSummer];
            DecodeDate(dt, yy, mm, dd);
            fEndSeasonDates[seasSummer].day := dd;
            fEndSeasonDates[seasSummer].month := mm;
            // fall
            dt := dt + sLens[seasFall];
            DecodeDate(dt, yy, mm, dd);
            fEndSeasonDates[seasFall].day := dd;
            fEndSeasonDates[seasFall].month := mm;
          end
        else InitSeasonDates;
    end;

  function TWorld.GetConfigParm(name, def : widestring) : olevariant;
    begin
      result := TheGlobalConfigHandler.GetConfigParm(name, def);
    end;

  function TWorld.IsDemoAccount(tycoon : string) : boolean;
    var
      key : string;
    begin
      try
        DSLock;
        try
          key := GetUserPath(tycoon);
          if not VarIsEmpty(DirProxy) and not VarIsNull(DirProxy) and DirProxy.RDOSetCurrentKey(key)
            then result := DirProxy.RDOReadBoolean('demo')
            else result := true;
        finally
          DSUnlock;
        end;
      except
        result := true;
      end;
    end;

  function TWorld.GetLandSquareCost : integer;
    begin
      if fLandSquareCost = 0
        then fLandSquareCost := StrToInt(TheGlobalConfigHandler.GetConfigParm('LandSquareCost', '10000'));
      result := fLandSquareCost;
    end;

  function TWorld.MinLevelToBuildAdvanced : integer;
    begin
      if fBldMagnaIfNoble
        then result := 0
        else result := 4;
    end;

  procedure TWorld.ClearTycoonVotes;
    var
      i : integer;
    begin
      Tycoons.Lock;
      try
        for i := 0 to pred(fTycoons.Count) do
          TTycoon(fTycoons[i]).ClearVotes;
      finally
        Tycoons.Unlock;
      end;
    end;

  procedure TWorld.ReportWithdrawal(Loc, Tycoon : TObject);
    var
      i         : integer;
      curTycoon : TTycoon;
    begin
      Tycoons.Lock;
      try
        for i := 0 to pred(fTycoons.Count) do
          begin
            curTycoon := TTycoon(fTycoons[i]);
            if curTycoon.Votes[Loc] = Tycoon
              then curTycoon.Votes[Loc] := nil;
          end;
      finally
        Tycoons.Unlock;
      end;
    end;

  procedure TWorld.SendEmail(from, rcpt, subject, url : string);
    begin
      MailServer.SendHTMLMessage(from, rcpt, subject, url);
    end;

  procedure TWorld.ReportCapitolMoved(OldLoc, NewLoc : TObject);
    var
      i         : integer;
      curTycoon : TTycoon;
    begin
      Tycoons.Lock;
      try
        for i := 0 to pred(fTycoons.Count) do
          begin
            curTycoon := TTycoon(fTycoons[i]);
            curTycoon.Votes.UpdateLocation(OldLoc, NewLoc);
          end;
      finally
        Tycoons.Unlock;
      end;
    end;

  procedure TWorld.ReportZoning(Tycoon : TTycoon; Fac : TFacility);
    var
      newIn     : boolean;
      i, j      : integer;
      ZonerComp : string;
      DemDate   : TDateTime;
      ZoneMess  : TZoningMessage;
    begin
      Fac.ToBeDemolished := Fac.MetaFacility.DemoLapse;
      // Prepare message to email
      try
        ZoningMessages.Lock;
        try
          newIn := true;
          for i := 0 to (ZoningMessages.Count - 1) do
            if TZoningMessage(ZoningMessages[i]).fZoned = Fac.Company.Owner.Name
              then
                if TZoningMessage(ZoningMessages[i]).fZoners.Count < 10
                  then
                    begin
                      ZonerComp := Tycoon.Name;
                      if (Tycoon.MasterRole <> nil) then
                        begin
                          if (Tycoon.MasterRole.Name <> Tycoon.Name)
                            then zonerComp := zonerComp + ' - ' + Tycoon.MasterRole.Name;
                        end;
                      if Tycoon.Roles <> nil
                        then
                          begin
                            Tycoon.Roles.Lock;
                            try
                              for j := 0 to (Tycoon.Roles.Count - 1) do
                                if (TTycoon(Tycoon.Roles[j]).Name <> Tycoon.Name)
                                  then zonerComp := ZonerComp + ' - ' + TTycoon(Tycoon.Roles[j]).Name;
                            finally
                              Tycoon.Roles.UnLock
                            end;
                          end;
                      TZoningMessage(ZoningMessages[i]).fZoners.Add(ZonerComp);
                      TZoningMessage(ZoningMessages[i]).fFacNames.Add(Fac.Name);
                      TZoningMessage(ZoningMessages[i]).fFacCompany.Add(Fac.Company.Name);
                      TZoningMessage(ZoningMessages[i]).fFacX.Add(IntToStr(Fac.XPos));
                      TZoningMessage(ZoningMessages[i]).fFacY.Add(IntToStr(Fac.YPos));
                      DemDate := EncodeDate(fVirtYear, fVirtMonth, fVirtDay) + 30*Fac.ToBeDemolished;
                      TZoningMessage(ZoningMessages[i]).fTime.Add(FormatDateTime( 'mmm d, yyyy', DemDate));
                      NewIn := false;
                      Logs.Log( tidLog_Survival, TimeToStr(Now) + Format('Updated Zoning Message for : %s' , [Fac.Company.Owner.Name]));
                    end;
          if NewIn
            then
              begin
                zonerComp := Tycoon.Name;
                if (Tycoon.MasterRole <> nil)
                  then
                    begin
                      if (Tycoon.MasterRole.Name <> Tycoon.Name)
                        then zonerComp := zonerComp + ' - ' + Tycoon.MasterRole.Name;
                    end;
                if Tycoon.Roles <> nil
                  then
                    begin
                      Tycoon.Roles.Lock;
                      try
                        for j := 0 to (Tycoon.Roles.Count - 1) do
                          if (TTycoon(Tycoon.Roles[j]).Name <> Tycoon.Name)
                            then zonerComp := ZonerComp + ' - ' + TTycoon(Tycoon.Roles[j]).Name;
                      finally
                        Tycoon.Roles.UnLock
                      end;
                    end;
                with Fac.Company do
                  ZoneMess := TZoningMessage.Create(Owner.Name, Owner.Language);
                ZoneMess.fZoners.Add(zonerComp);
                ZoneMess.fFacNames.Add(Fac.Name);
                ZoneMess.fFacCompany.Add(Fac.Company.Name);
                ZoneMess.fFacX.Add(IntToStr(Fac.XPos));
                ZoneMess.fFacY.Add(IntToStr(Fac.YPos));
                DemDate := EncodeDate(fVirtYear, fVirtMonth, fVirtDay) + 30*Fac.ToBeDemolished;
                ZoneMess.fTime.Add(FormatDateTime( 'mmm d, yyyy', DemDate));
                ZoningMessages.Insert(ZoneMess);
                Logs.Log( tidLog_Survival, TimeToStr(Now) + Format('Created Zoning Message for : %s' , [Fac.Company.Owner.Name]));
              end;
        finally
          ZoningMessages.Unlock;
        end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in ReportZoning.');
      end;
    end;

  procedure TWorld.InitTownRoadCounter;
    var
      i : integer;
    begin
      Towns.Lock;
      try
        for i := 0 to pred(Towns.Count) do
          TTown(Towns[i]).RoadBlocks := 0;
      finally
        Towns.Unlock;
      end;
    end;

  procedure TWorld.ReportRoadBlock(x, y : integer);
    var
      id   : TTownId;
      Town : TTown;
    begin
      id   := GetTownMap(x, y);
      Town := TownById[id];
      if Town <> nil
        then Town.RoadBlocks := Town.RoadBlocks + 1;
    end;

  procedure TWorld.InvalidateRoads;
    begin
      fRoadRefreshDelay := MaxRoadRefreshDelay + 1;
    end;

  function TWorld.getCols : integer;
    begin
      result := xSize;
    end;

  function TWorld.getRows : integer;
    begin
      result := ySize;
    end;

  procedure TWorld.setDimensions( n, m : integer );
    begin
    end;

  function TWorld.getElement( i, j : integer ) : single;
    begin
      result := GetTownMap( j, i );
    end;

  procedure TWorld.setElement( i, j : integer; value : single );
    begin
    end;

  procedure TWorld.SleepTycoons;
    var
      i : integer;
    begin
      try
        fTycoons.Lock;
        try
          for i := pred(fTycoons.Count) downto 0 do
            TTycoon(fTycoons[i]).Sleep;
        finally
          fTycoons.Unlock;
        end;
        fAwakenTycoons.DeleteAll;
      except
      end;
    end;

  function TWorld.GetTownNames : OleVariant;
    var
      tNames : TStringList;
      i      : integer;
    begin
      try
        tNames := TStringList.Create;
        Towns.Lock;
        try
          for i := 0 to pred(Towns.Count) do
            tNames.Add(TTown(Towns[i]).Name);
          result := tNames.Text;
        finally
          Towns.Unlock;
          tNames.Free;
        end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error getting Town Names.');
      end;
    end;

  function TWorld.ReportMaintenance(shortMsg, longMsg : string; eta : TDateTime) : boolean;
    var
      key : string;
    begin
      try
        DSLock;
        try
          key := 'Root/Areas/' + Area + '/Worlds/' + Name + '/Interface';
          if DirProxy.RDOSetCurrentKey(key)
            then
              begin
                DirProxy.WaitForAnswer := true;
                DirProxy.RDOWriteString('DowntimeShort', shortMsg);
                DirProxy.RDOWriteString('DowntimeLong',  longMsg);
                DirProxy.RDOWriteDate('DowntimeETA', eta);
                DirProxy.WaitForAnswer := false;
                //DirProxy.RDOWriteDate('DowntimeDate', Now);
                result := true;
              end
            else result := false;
        finally
          DSUnlock;
        end;
      except
        result := false;
        Logs.Log(tidLog_Survival, TimeToStr(Time) + ' Error reporting maintenance');
      end;
    end;

  procedure TWorld.RDOForceRole(roleName, tycoonName, password : widestring; assume : wordbool);
    var
      Role   : TTycoon;
      Tycoon : TTycoon;
    begin
      try
        Role   := TycoonByName[roleName];
        Tycoon := TycoonByName[tycoonName];
        if (password = tidSystemPassword) and (Role <> nil) and (Tycoon <> nil) and Role.IsRole and not Tycoon.IsRole
          then
            if assume
              then Tycoon.AssumeRole(Role)
              else Tycoon.AbandomRole(Role);
      except
        Logs.Log(tidLog_Survival, TimeToStr(Time) + ' Error in RDOForceRole..');
      end;
    end;

  procedure TWorld.RedeemBank(Bank : TBank);
    var
      i    : integer;
      Loan : TLoan;
    begin
      for i := pred(Bank.Loans.Count) downto 0 do
        begin
          Loan          := TLoan(Bank.Loans.AtExtract(i));
          Loan.Bank     := fMainBank;
          Loan.Interest := max(fMainBank.Interest, Loan.Interest);
          fMainBank.Loans.Insert(Loan);
        end;
    end;

  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TWorld );
    end;


  // MDXs

  procedure RegisterWorldMDX( MDX : THandle; World : TWorld; WorldLoaded : boolean );
    var
      RegisterWorldMDXProc : TRegisterWorldMDXProc;
    begin
      RegisterWorldMDXProc := GetProcAddress( MDX, tidProcName_RegisterWorldMDX );
      if assigned(RegisterWorldMDXProc)
        then RegisterWorldMDXProc( World, WorldLoaded );
    end;


end.





