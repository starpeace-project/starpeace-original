unit Kernel;

{ $DEFINE OLD_INV}
{$DEFINE MLSStoreOnlyMissingTexts}

interface

  uses
    Windows, Collection, Classes, MetaInstances, Plotter, Land, ClassStorageInt, CacheAgent,
    Surfaces, BackupInterfaces, Protocol, SyncObjs, Persistent, MailServerInterfaces,
    Circuits, CacheCommon, Accounts, TransportInterfaces, VisualClassManager, Taxes,
    MapStringToObject, Rankings, Inventions, Seasons, Events, Languages, LoggedUserData,
    Tasks, Tutorial, TycoonVotes, Favorites, CloneOptions, Variants;

  const
    NoPos = -1;
    UnresolvedIdx = -2;

  const
    TimeUnits = 10;
    YearZero  = 2000;
    MaxLoan   = 200*1000*1000;

  const
    LandSquareCost = 10*1000;

  const
    DemolitionLapse = 10*12; // in months

  const
    tidClassFamily_Fluids         = 'Fluids';
    tidClassFamily_Blocks         = 'Blocks';
    tidClassFamily_Facilities     = 'Facilities';
    tidClassFamily_FacilityKinds  = 'FacilityKinds';
    tidClassFamily_Ordinances     = 'Ordinances';
    tidClassFamily_Inventions     = 'Inventions';
    tidClassFamily_InvClasses     = 'InvClasses';
    tidClassFamily_Clusters       = 'Clusters';
    tidClassFamily_TownParameters = 'TownParameters';
    tidClassFamily_TycoonLevels   = 'TycoonLevels';
    tidCluster_Undefined          = 'General';
    tidFacilityKind_Undefined     = 'General';

  const
    tidEnvironment_Beauty    = 'Beauty';
    tidEnvironment_Pollution = 'Pollution';
    tidEnvironment_Crime     = 'Crime';
    tidEnvironment_QOL       = 'QOL';
    tidEnvironment_BAP       = 'BAP';

  const
    tidSuperFacKind_TradeCenter = 'Trade Center';
    tidSuperFacKind_Residential = 'Residentials';
    tidSuperFacKind_Industry    = 'Industries';
    tidSuperFacKind_Public      = 'Public';
    tidSuperFacKind_Headquarter = 'Headquarters';
    tidSuperFacKind_Farm        = 'Farms';
    tidSuperFacKind_Service     = 'Services';
    tidSuperFacKind_Warehouse   = 'Warehouses';
    tidSuperFacKind_Business    = 'Offices';
    tidSuperFacKind_Special     = 'Special';

  const
    tidTownParameter_ServiceStrength   = 'ServiceStrength';
    tidTownParameter_ServiceCount      = 'ServiceCount';
    tidTownParameter_ServiceSales      = 'ServiceSales';
    tidTownParameter_ServiceCapacity   = 'ServiceCapacity';
    tidTownParameter_ServicePrice      = 'ServicePrice';
    tidTownParameter_ServiceQuality    = 'ServiceQuality';
    tidTownParameter_InputValue        = 'InputValue';
    tidTownParameter_InputCapacity     = 'InputCapacity';
    tidTownParameter_InputQuality      = 'InputQuality';
    tidTownParameter_InputPrice        = 'InputPrice';
    tidTownParameter_InputMaxPrice     = 'InputMaxPrice';
    tidTownParameter_OutputValue       = 'OutputValue';
    tidTownParameter_OutputCapacity    = 'OutputCapacity';
    tidTownParameter_OutputQuality     = 'OutputQuality';
    tidTownParameter_OutputPrice       = 'OutputPrice';

  const
    tidRole_Tycoon = 'Tycoon';
    tidRole_Mayor  = 'Mayor';


  // Model events

  const
    msgKernel_NewBlock        = 1000;
    msgKernel_NewFacility     = 1001;
    msgKernel_NewCompany      = 1002;
    msgKernel_NewTycoon       = 1003;
    msgKernel_BlockDeleted    = 1004;
    msgKernel_FacilityDeleted = 1005;
    msgKernel_CompanyDeleted  = 1006;
    msgKernel_TycoonDeleted   = 1007;
    msgKernel_FacilityCloned  = 1008;
    msgKernel_FacUpgraded     = 1009;
    msgKernel_SellToAll       = 1010;
    msgKernel_FacsConnect     = 1011;
    msgKernel_FacsHireOffer   = 1012;


  const
    facNoTrouble        = $00;
    facNeedsConnection  = $01;
    facNeedsBudget      = $02;
    facStoppedByTycoon  = $04;
    facInsuficientInput = $08;
    facStoppedByAdmin   = $10;
    facNeedsWorkForce   = $20;
    facNeedCompSupport  = $40;
    facForbiddenZone    = $80;

    facCriticalTrouble = facNeedsBudget or facStoppedByTycoon; // or facNeedsConnection;

  const
    InitialBudget      = 100*1000*1000.0; // $100,000,000
    MayorInitialBudget = 1000*1000*1000.0;

  const
    tcnDescSeparator = #10#13;

  // Contract priorities

  const
    cprEmergency = 0;
    cprSameOwner = 1;
    cprNewbieA   = 2;
    cprAlly      = 3;
    cprGuild     = 4;
    cprNewbieB   = 5;
    cprNormal    = 6;
    cprMonopoly  = 7;

  type
    any = 0..0;

  type
    PPercent     = ^TPercent;
    POffset      = ^TOffset;
    PFluidValue  = ^TFluidValue;
    PVariety     = ^TVariety;
    PAdmitance   = ^TAdmitance;
    PQuality     = ^TQuality;
    PObjId       = ^TObjId;
    PVirtDateAbs = ^TVirtDateAbs;
    PVirtDate    = ^TVirtDate;

    TPercent     = byte;
    TOffset      = integer;
    TFluidValue  = single;
    TVariety     = integer;
    TAdmitance   = word;
    TQuality     = TPercent;
    TObjId       = pointer;
    TVirtDateAbs = integer;
    TVirtDate    = TDateTime;
    TTimeDelta   = single;
    TPrestige    = single;
    TWeight      = integer;

    PPoint = ^TPoint;
    TPoint =
      record
        x, y : integer;
      end;

    PFluidData = ^TFluidData;
    TFluidData =
      packed record
        Q : TFluidValue;
        K : TQuality;
      end;

    PInputData = ^TPullInputData;
    TInputData =
      packed record
        Q : TFluidValue;
        K : TQuality;
      end;

    PPushInputData = ^TPushInputData;
    TPushInputData =
      packed record
        Q : TFluidValue;
        K : TQuality;
        S : TAdmitance;
      end;

    PPullInputData = ^TPullInputData;
    TPullInputData = TInputData;

    POutputData = ^TOutputData;
    TOutputData =
      packed record
        Q     : TFluidValue;
        K     : TQuality;
        Extra : TFluidData;
      end;

    TFacTypeSet = set of byte;

  const
    qIlimited   = $7FFFFFFF;
    qZero       = 1e-6;
    sIlimited   = $FFFF;
    kIlimited   = $FF;
    qUnassigned = qIlimited - 1;

  const
    fluidZero      : TFluidData  = (Q : 0; K : 0);
    inputZero      : TInputData  = (Q : 0; K : 0);
    outputZero     : TOutputData = (Q : 0; K : 0);
    fluidIlimited  : TFluidData  = (Q : qIlimited; K : kIlimited);
    inputIlimited  : TInputData  = (Q : qIlimited; K : kIlimited);
    outputIlimited : TOutputData = (Q : qIlimited; K : kIlimited);

  function FluidData    ( Q : TFluidValue; K : TQuality ) : TFluidData;
  function InputData    ( Q : TFluidValue; K : TQuality ) : TInputData;
  function PushInputData( Q : TFluidValue; K : TQuality; S : TAdmitance ) : TPushInputData;
  function PullInputData( Q : TFluidValue; K : TQuality ) : TPullInputData;
  function OutputData   ( Q : TFluidValue; K : TQuality ) : TOutputData;

  procedure LoadFluidData     ( name : string; var FluidData;  Reader : IBackupReader );
  procedure StoreFluidData    ( name : string; var FluidData;  Writer : IBackupWriter );
  procedure LoadInputData     ( name : string; var InputData;  Reader : IBackupReader );
  procedure StoreInputData    ( name : string; var InputData;  Writer : IBackupWriter );
  procedure LoadPushInputData ( name : string; var InputData;  Reader : IBackupReader );
  procedure StorePushInputData( name : string; var InputData;  Writer : IBackupWriter );
  procedure LoadPullInputData ( name : string; var InputData;  Reader : IBackupReader );
  procedure StorePullInputData( name : string; var InputData;  Writer : IBackupWriter );
  procedure LoadOutputData    ( name : string; var OutputData; Reader : IBackupReader );
  procedure StoreOutputData   ( name : string; var OutputData; Writer : IBackupWriter );

  function AverageK( F1, F2 : PFluidData ) : TQuality;

  function IntegrateValues(Value1, Value2, Weight1, Weight2 : single) : single;

  type
    TPeopleKind     = (pkHigh, pkMiddle, pkLow);
    TPeopleArray    = array[TPeopleKind] of TFluidData;
    TPeopleInArray  = array[TPeopleKind] of TPushInputData;
    TPeopleOutArray = array[TPeopleKind] of TOutputData;

  const
    PeopleKindPrefix  : array[TPeopleKind] of string = ('hi', 'mid', 'lo');
    {
    PeopleKindName    : array[TPeopleKind] of string = ('High class', 'Middle class', 'Low class');
    WorkForceKindName : array[TPeopleKind] of string = ('Executives', 'Professionals', 'Workers');
    }

  type
    TPeriodType  = (perHour, perDay, perMonth, perYear, perPoliticalMandate);
    TMinistryId  = integer;

  // Gate options

  type
    TGateAutoconnectOptions = byte;

  const
    goptSameCompany  = $01;
    goptSameTycoon   = $02;
    goptAllies       = $04;
    goptEnemies      = $08;
    goptCost         = $10;
    goptQuality      = $20;

  const
    SupFacIntervalMin = 50;
    SupFacIntervalMax = 100;

  type
    // Classes defined

    {$M+}
    TMetaFacility = class;
    TEvlStage     = class;
    TMetaBlock    = class;
    TMetaFluid    = class;
    TMetaGate     = class;
    TMetaInput    = class;
    TMetaOutput   = class;

    TFacility   = class;
    TBlock      = class;
    TGate       = class;
    TInput      = class;
    TOutput     = class;
    TPushInput  = class;
    TPullInput  = class;
    TPushOutput = class;
    TPullOutput = class;

    //TCompanyResearch  = class;


    TCompanyDirection = class;
    TTownParameter    = class;

    TBank           = class;
    TCluster        = class;
    TMoneyDealer    = class;
    TCompany        = class;
    TTown           = class;
    TTycoon         = class;
    TTycoonContext  = class;
    TCurriculumItem = class;

    // Metaclasses defined

    CMetaInstance = class of TMetaInstance;
    CMetaFacility = class of TMetaFacility;
    CEvlStage     = class of TEvlStage;
    CMetaBlock    = class of TMetaBlock;
    CMetaFluid    = class of TMetaFluid;
    CMetaGate     = class of TMetaGate;
    CMetaInput    = class of TMetaInput;
    CMetaOutput   = class of TMetaOutput;

    CFacility   = class of TFacility;
    CBlock      = class of TBlock;
    CGate       = class of TGate;
    CInput      = class of TInput;
    COutput     = class of TOutput;
    CPushInput  = class of TPushInput;
    CPullInput  = class of TPullInput;
    CPushOutput = class of TPushOutput;
    CPullOutput = class of TPullOutput;

    CCluster = class of TCluster;
    CCompany = class of TCompany;
    CTown    = class of TTown;
    CTycoon  = class of TTycoon;

    ITimer =
      interface
        function GetVirtualTime    : TDateTime;
        function GetVirtualTimeAbs : TVirtDateAbs;
        function dt                : TTimeDelta;
        function GetSeason         : TSeason;           
        function GetSeasonProgress : single;
        function getTickId         : integer;
      end;

    IMapRefresh =
      interface
        procedure RefreshArea( x, y, dx, dy : integer );
        procedure RefeshFacility( Facility : TFacility; FacilityChange : TFacilityChange );
      end;

    IWorldLocator =
      interface
        function  GetWorldName : string;
        function  GetTotalPopulation : integer;
        function  FacilityAt( x, y : integer ) : TFacility;
        function  GetTycoonByName( name : string ) : TTycoon;
        function  GetTycoonCollection : TLockableCollection;
        function  GetMessagesPath : string;
        function  GetWorldURL : string;
        function  GetMainBank : TBank;
        function  GetMainDealer : TMoneyDealer;
        procedure SendNotification( TycoonId : integer; Kind : integer; Title, Body : string; Options : integer );
        procedure SendEvent( Event : TEvent );
        procedure SearchForSuppliers( Block : TBlock );
        procedure AddCurriculumItem( Tycoon : TTycoon; Item : TCurriculumItem );
        function  GetMinSalary( Kind : TPeopleKind ) : TPercent;
        function  GetHoursADay : single;
        function  GetNumberOfDays : integer;
        function  LandClassFound( x1, y1, x2, y2 : integer; LandClass : TLandClass ) : boolean;
        procedure CountInputConnections(count : integer);
        procedure CountOutputConnections(count : integer);
        function  UpdateNobility(Tycoon : TTycoon) : boolean;
        procedure RegisterTitle(name : string);
        function  NumberTitle(name, sep : string) : string;
        function  IsDemoAccount(tycoon : string) : boolean;
        function  GetLandSquareCost : integer;
        function  MinLevelToBuildAdvanced : integer;
        procedure ReportWithdrawal(Loc, Tycoon : TObject);
        procedure SendEmail(from, rcpt, subject, url : string);
        procedure RedeemBank(Bank : TBank);
      end;

    IRoadHandler =
      interface
        procedure NearestCircuitsToArea( CircuitId : TCircuitId; Area : TRect; var Circuits : TCollection );
        function  AreasAreConnected( CircuitId : TCircuitId; Area1, Area2 : TRect ) : boolean;
        function  GetCargoSystem( CircuitId : TCircuitId ) : ICargoSystem;
      end;

    IModelFactory =
      interface
        function  NewCompany( name : string; Cluster : TCluster; Tycoon : TTycoon ) : TCompany;
        function  NewTycoon( name, password : string ) : TTycoon;
        function  NewTycoonRole( RoleId, name : string; TycoonClass : CTycoon; InitialBudget : TMoney ) : TTycoon;
        function  NewNewspaper( Name, Style, Town : string ) : boolean;
        procedure RequestDeletion( Facility : TFacility );
        procedure RequestDeleteTycoon( Tycoon : TTycoon; erase : boolean );
        procedure ResetTycoon( Tycoon : TTycoon );
      end;

    // A FacilityKind groups a set of MetaFacilities. These facilities have
    // several things in common, like inventions.

    //TFacilityRole = (rolNeutral, rolProducer, rolDistributer, rolBuyer);

    TFacilityKind =
      class( TMetaInstance )
        public
          constructor Create( anId : string );
        private
          fClusterName : string;
          fName        : string;
          fName_MLS    : TMultiString;
          fTechnology  : string;
          fSuperType   : string;
          fRole        : TFacilityRole;
        published
          property ClusterName : string        read fClusterName write fClusterName;
          property Name        : string        read fName        write fName;
          property Name_MLS    : TMultiString  read fName_MLS;
          property Technology  : string        read fTechnology  write fTechnology;
          property SuperType   : string        read fSuperType   write fSuperType;
          property Role        : TFacilityRole read fRole        write fRole;
        public
          procedure RetrieveTexts( Container : TDictionary ); override;
          procedure StoreTexts   ( Container : TDictionary ); override;
      end;

    // A MetaFacility describes the properties of a set of Facilities.
    // It requires a FacilityClass (beside other arguments that are explained
    // below) to be created. This FacilityClass will be used to instantiate
    // new facilities using the "Instantiate" function.
    // Featuring:
    //   Name : string
    //     A readable name for the kind of Facility
    //   EvlStages : collection
    //     An array of evolutionary stages. Each stage is an instance of TEvlStage.
    //     This array describes the possible stages in the development of a
    //     certain facility.
    //   TypicalStage : TEvlStage                                                 
    //     It is the most known (or typical) stage in the development of a                        
    //     facility of this kind.
    //   Level : integer
    //     This is a sort of "Simulation Priority", where lower values are the
    //     ones to simulate first.
    //   xSize, ySize : integer
    //     Size of facilities of this kind
    //   InTown : boolean
    //     If a facility of this kind requires to be physically allocated in one town
    //   function Instantiate : TFacility
    //     Creates an instance if this kind of facility

    TMetaFacilityOption  = (mfcInTown, mfcGenerateName, mfcShowCompanyInText, mfcShowProfitInText, mfcStopDisabled, mfcIgnoreZoning, mfcForbiddenRename, mfcAceptsNoOwner);
    TMetaFacilityOptions = set of TMetaFacilityOption;
    TUniquenessMask      = integer;

    TMetaFacility =
      class( TMetaInstance )
        public
          constructor Create  ( anId, aName : string; aVisualClass : TVisualClassId; aFacilityClass : CFacility );
          constructor CopyFrom( aMetaFacility : TMetaFacility; NewId : string ); virtual;
          destructor  Destroy; override;
        private
          fName           : string;
          fDesc           : string;
          fRequires       : string;
          fPluralName     : string;
          fName_MLS       : TMultiString;
          fDesc_MLS       : TMultiString;
          fRequires_MLS   : TMultiString;
          fPluralName_MLS : TMultiString;
          fEvlStages      : TCollection;
          fFacilityClass  : CFacility;
          fFacilityKind   : TFacilityKind;
          fTypicalStage   : TEvlStage;
          fLevel          : integer;
          fXSize          : integer;
          fYSize          : integer;
          fInTown         : boolean;
          fCluster        : TCluster;
          fOptions        : TMetaFacilityOptions;
          fVisualClass    : TVisualClassId;
          fTechnologyKind : string;
          fTechnology     : TInvention;
          fZoneType       : TZoneType;
          fFacId          : integer;
          fMinistryId     : TMinistryId;
          fCloneSource    : TMetaFacility;
          fNeedsBudget    : boolean;
          fEstimPrice     : TMoney;
          fSlotCount      : byte;
          fDemoLapse      : integer;
          fDepOnTech      : boolean;
          fUniquenessMask : TUniquenessMask;
          fReqCmpSupplies : boolean;
        private
          function  EstimatePrice : TMoney;
          function  GetClusterName : string;
          procedure SetClusterName( aName : string );
          function  GetFacilityKind : string;
          procedure SetFacilityKind( aName : string );
        published
          property Name           : string               read fName           write fName;
          property Desc           : string               read fDesc           write fDesc;
          property Reqs           : string               read fRequires       write fRequires;
          property Name_MLS       : TMultiString         read fName_MLS;
          property Desc_MLS       : TMultiString         read fDesc_MLS;
          property Requires_MLS   : TMultiString         read fRequires_MLS;
          property PluralName_MLS : TMultiString         read fPluralName_MLS;
          property EvlStages      : TCollection          read fEvlStages;
          property FacilityClass  : CFacility            read fFacilityClass;
          property FacilityKind   : string               read GetFacilityKind write SetFacilityKind;
          property Kind           : TFacilityKind        read fFacilityKind;
          property TypicalStage   : TEvlStage            read fTypicalStage   write fTypicalStage;
          property Level          : integer              read fLevel          write fLevel;
          property XSize          : integer              read fXSize          write fXSize;
          property YSize          : integer              read fYSize          write fYSize;
          property Cluster        : TCluster             read fCluster;
          property ClusterName    : string               read GetClusterName  write SetClusterName;
          property TechnologyKind : string               read fTechnologyKind write fTechnologyKind;
          property Technology     : TInvention           read fTechnology     write fTechnology;
          property Options        : TMetaFacilityOptions read fOptions        write fOptions;
          property ZoneType       : TZoneType            read fZoneType;
          property VisualClass    : TVisualClassId       read fVisualClass;
          property Price          : TMoney               read EstimatePrice;
          property FacId          : integer              read fFacId          write fFacId;
          property MinistryId     : TMinistryId          read fMinistryId     write fMinistryId;
          property PluralName     : string               read fPluralName     write fPluralName;
          property NeedsBudget    : boolean              read fNeedsBudget    write fNeedsBudget;
          property SlotCount      : byte                 read fSlotCount      write fSlotCount;
          property DemoLapse      : integer              read fDemoLapse      write fDemoLapse;
          property DepOnTech      : boolean              read fDepOnTech      write fDepOnTech;
          property UniquenessMask : TUniquenessMask      read fUniquenessMask write fUniquenessMask;
          property CloneSource    : TMetaFacility        read fCloneSource;
          property ReqCmpSupplies : boolean              read fReqCmpSupplies write fReqCmpSupplies;
        public
          function Instantiate : TFacility; virtual;
        private
          procedure EvlStagesModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
        public
          procedure Register( ClassFamily : TClassFamilyId );
          procedure RequiresInvention(Invention : TInvention);
        public
          procedure RetrieveTexts( Container : TDictionary ); override;
          procedure StoreTexts   ( Container : TDictionary ); override;
          procedure EvaluateTexts; override;
          procedure CloneTexts; override;
        private
          fConstTime : integer;
        private
          function  GetConstructionTime : integer;
          function  GetUpgradeTime : integer;
          function  GetUpgradeCost : TMoney;
          function  GetUpgradeHourCost : TMoney;
        public
          property ConstructionTime : integer read GetConstructionTime;
          property UpgradeTime      : integer read GetUpgradeTime;
          property UpgradeCost      : TMoney  read GetUpgradeCost;
          property UpgradeHourCost  : TMoney  read GetUpgradeHourCost;
        private
          fWaterQuestInv : TInvention;
        public
          property WaterQuestInv : TInvention read fWaterQuestInv write fWaterQuestInv;
      end;

    // A EvlStage describes a particular stage within the develpment of facilities
    // of certain kind. It has:
    //   Name : string
    //     Readable name of the stage (e.g. "Construction")
    //   Description : string
    //     A readable description of the stage. Here is described how the facility
    //     will evolve to the next stage.
    //   MetaBlock : TMetaBlock
    //     A reference to the kind of block that will be used to simulate the
    //     facility in this stage.

    TEvlStage =
      class
        public
          constructor Create( anId,
                              aName,
                              aDescription : string;
                              aMetaBlock   : TMetaBlock );
        private
          fId           : string;
          fName         : string;
          fDescription  : string;
          fMetaBlock    : TMetaBlock;
          fMetaFacility : TMetaFacility;
        private
          function GetIndex : integer;
        published
          property Id           : string        read fId;
          property Name         : string        read fName;
          property Description  : string        read fDescription;
          property MetaBlock    : TMetaBlock    read fMetaBlock;
          property MetaFacility : TMetaFacility read fMetaFacility;
          property Index        : integer       read GetIndex;
      end;

    // TMetaCompanyInput

    TMetaCompanyInput =
      class
        public
          constructor Create(aFluid : TMetaFluid; aMax : TFluidValue; CanEdit : boolean);
        private
          fFluid    : TMetaFluid;
          fMax      : TFluidValue;
          fEditable : boolean;
        public
          property Fluid    : TMetaFluid  read fFluid;
          property Max      : TFluidValue read fMax;
          property Editable : boolean     read fEditable;
      end;

    // A MetaBlock instance contains information for a group (or kind) of
    // blocks. Blocks are used to simulate Facilities. Each MetaBlock features:
    //   NumId : TMetaBlockNumId
    //     This is an integer used to identify this kind of block. The Client
    //     will receive this Ids from the Server.
    //   BlockClass : CBlock
    //     This is the Delphi class of the blocks belonging to this kind of block.
    //     It is used to instantiate the block in the "Instantiate" function.
    //   MetaInputs : collection
    //     A collection of metainstances describing the inputs of a block of this
    //     kind. These metainstances are instances of TMetaInput.
    //   MetaOutputs : collection
    //     Same thing but this time with outputs.
    //   xSize, ySize : integer
    //     Size of the blocks of this kind.
    //   function Instantiate( aFacility : TFacility ) : TBlock
    //     Creates a new instance of this kind of block using BlockClass as its
    //     Delphi class and the MetaBlock as its metainstance.

    TMetaBlock =
      class( TMetaInstance )
        public
          constructor Create( anId           : string;
                              aSupplyAccount : TAccountId;
                              aProdAccount   : TAccountId;
                              aBlockClass    : CBlock );
          destructor  Destroy; override;
        private
          fBlockClass       : CBlock;
          fMetaInputs       : TNotifiedCollection;
          fMetaOutputs      : TNotifiedCollection;
          fBeauty           : TSurfaceValue;
          fBeautyStrength   : TSurfaceValue;
          fVisualStages     : integer;
          fSupplyAccount    : TAccountId;
          fProdAccount      : TAccountId;
          fDesc             : string;
          fDesc_MLS         : TMultiString;
          fPrestige         : TPrestige;
          fInventions       : TCollection;
          fRecordCost       : boolean;
          fCnntRepairable   : boolean;
          fMaxUpgrade       : integer;
          fTranscends       : boolean;
          fMinColDist       : byte;
          fColIsSameComp    : boolean;
          fUsagePerInv      : integer;
          fManyUpgrades : boolean;
        published
          property BlockClass     : CBlock              read fBlockClass;
          property MetaInputs     : TNotifiedCollection read fMetaInputs;
          property MetaOutputs    : TNotifiedCollection read fMetaOutputs;
          property Beauty         : TSurfaceValue       read fBeauty         write fBeauty;
          property BeautyStrength : TSurfaceValue       read fBeautyStrength write fBeautyStrength;
          property VisualStages   : integer             read fVisualStages   write fVisualStages;
          property Desc           : string              read fDesc           write fDesc;
          property Desc_MLS       : TMultiString        read fDesc_MLS;
          property Prestige       : TPrestige           read fPrestige       write fPrestige;
          property RecordCost     : boolean             read fRecordCost     write fRecordCost;
          property CnntRepairable : boolean             read fCnntRepairable write fCnntRepairable;
          property IsTransBlock   : boolean             read fTranscends     write fTranscends;
          property MinColDist     : byte                read fMinColDist     write fMinColDist;
          property ColIsSameComp  : boolean             read fColIsSameComp  write fColIsSameComp;
          property UsagePerInv    : integer             read fUsagePerInv    write fUsagePerInv;
          property ManyUpgrades : boolean           read fManyUpgrades write fManyUpgrades;
        public
          function Transcends(Facility : TFacility) : boolean; virtual;
        private
          function GetMetaInputIndex  ( name : string ) : integer;
          function GetMetaOutputIndex ( name : string ) : integer;
          function GetMetaInputByName ( name : string ) : TMetaInput;
          function GetMetaOutputByName( name : string ) : TMetaOutput;
        public
          property SupplyAccount : TAccountId read fSupplyAccount write fSupplyAccount;
          property ProdAccount   : TAccountId read fProdAccount   write fProdAccount;
          property InputIndex  [name : string] : integer     read GetMetaInputIndex;
          property OutputIndex [name : string] : integer     read GetMetaOutputIndex;
          property InputByName [name : string] : TMetaInput  read GetMetaInputByName;
          property OutputByName[name : string] : TMetaOutput read GetMetaOutputByName;
          property MaxUpgrade : integer read fMaxUpgrade write fMaxUpgrade;
        public
          function Instantiate( aFacility : TFacility ) : TBlock; virtual;
        private
          procedure OnMetaInputsModified(Operation : TCollectionOperation; Index : integer; Item : TObject);
          procedure OnMetaOutputsModified(Operation : TCollectionOperation; Index : integer; Item : TObject);
        protected
          function  ModifyStageStack( Stage : TMetaBlock ) : boolean; virtual;
          procedure ModifyMetaFacility( MetaFacility : TMetaFacility ); virtual;
        private
          fWeatherEnvelopes : TCollection;
          fCompanyInputs    : TCollection;
        public
          property WeatherEnvelopes : TCollection read fWeatherEnvelopes;
          property CompanyInputs : TCollection read fCompanyInputs;
        public
          function  RegisterWeatherEnvelope( Kind : string ) : integer;
          function  WeatherEnvelopeIndex( Kind : string ) : integer;
          procedure RegisterCompanyInput( Kind : string; aMax : TFluidValue; Editable : boolean );
        public
          procedure Register( ClassFamily : TClassFamilyId );
          procedure DeclareInvention(Invention : TInvention); virtual;
          function  UsesInvention(Invention : TInvention) : boolean;
        public
          property Inventions : TCollection read fInventions;
        public
          procedure RetrieveTexts( Container : TDictionary ); override;
          procedure StoreTexts   ( Container : TDictionary ); override;
          procedure EvaluateTexts; override;
          procedure StoreExtraInfoToCache( Cache : TObjectCache ); virtual;
      end;

    TMetaFluidOption  = (mfTradeable, mfImportable, mfStorable, mfConstruction, mfAdvertisement, mfWorkForce, mfPeople, mfService, mfCompanyFluid, mfForceTC);
    TMetaFluidOptions = set of TMetaFluidOption;

    // A MetaFluid describes a particular kind of fluid like food, clothes,
    // people. Each MetaFluid has:
    //   Name : string
    //     A readable name.
    //   Description : string
    //     A description of the fluid.
    //   TransCost : integer
    //     Transportation cost
    //   MarketPrice : TMoney
    //     Price (per unit) that is considered a fair price.

    TMetaFluid =
      class( TMetaInstance )
        public
          constructor Create( anId,
                              aName,
                              aDescription : string;
                              aUnitName    : string;
                              aFluidName   : string;
                              aFluidFact   : TFluidValue;
                              aTransCost   : TMoney;
                              aWeight      : TWeight;
                              aMarketPrice : TMoney;
                              anOutputCls  : COutput = nil);
        private
          fName            : string;
          fDescription     : string;
          fUnitName        : string;
          fFluidName       : string;
          fName_MLS        : TMultiString;
          fDescription_MLS : TMultiString;
          fUnitName_MLS    : TMultiString;
          fFluidName_MLS   : TMultiString;
          fFluidFact       : TFluidValue;
          fTransCost       : TMoney;
          fMarketPrice     : TMoney;
          fWeight          : TWeight;
          fOptions         : TMetaFluidOptions;
          fUNId            : integer;
          fCnxLimit        : integer;
          fStorageVol      : TFluidValue;
          fOutputClass     : COutput;
        published
          //property Name            : string            read fName;
          //property Description     : string            read fDescription;
          //property FluidName       : string            read fFluidName;
          //property UnitName        : string            read fUnitName;
          property Name_MLS        : TMultiString      read fName_MLS;
          property Description_MLS : TMultiString      read fDescription_MLS;
          property UnitName_MLS    : TMultiString      read fUnitName_MLS;
          property FluidName_MLS   : TMultiString      read fFluidName_MLS;
          property TransCost       : TMoney            read fTransCost;
          property Weight          : TWeight           read fWeight;
          property MarketPrice     : TMoney            read fMarketPrice;
          property Options         : TMetaFluidOptions read fOptions  write fOptions;
          property UNId            : integer           read fUNId     write fUNId;
          property CnxLimit        : integer           read fCnxLimit write fCnxLimit;
          property OutputClass     : COutput           read fOutputClass;
          property StorageVol      : TFluidValue       read fStorageVol write fStorageVol;
        public
          function FormatValue      ( Value : TFluidValue; LangId : TLanguageId ) : string;
          function FormatSingleValue( Value : TFluidValue; LangId : TLanguageId ) : string;
          function FormatValueAbs   ( Value : TFluidValue; LangId : TLanguageId ) : string;
          function ConvertToUnits   ( Value : TFluidValue ) : TFluidValue;
        public
          procedure RetrieveTexts( Container : TDictionary ); override;
          procedure StoreTexts   ( Container : TDictionary ); override;
      end;

    // A MetaGate is a metainstance that describes a kind of input or output.
    // MetaInputs and MetaOutputs are especializations of this class.
    // Each MetaGate has:
    //   Name : string
    //     Identifies the MetaGate. It must be readable.
    //   GateClass : CGate
    //     Delphi class that implements the actual gate. It is used to instantiate
    //     the gate in the "Instantiate" function.
    //   MetaFluid : TMetaFluid
    //     Kind of fluid that fluids trough the gate.
    //   Size : integer
    //     This is an estimated of the number of connections that gates of this
    //     kind will have under normal circumstances.
    //   Offset : TOffset
    //     Address within the block instance where the gate information can be
    //     found. This allows the simulation engine to access the information
    //     about gates and at the same time keeps simple the implementation
    //     of blocks. This way input and output data can be declared as private
    //     fields.
    //   function Instantiate( aBlock : TBlock ) : TGate
    //     Creates an instance of this kind of gate. The Delphi class GateClass
    //     is used to create the instance.

    TMetaGateOption  = (mgoptCacheable, mgoptEditable);
    TMetaGateOptions = set of TMetaGateOption;
    TMetaGate =
      class
        public
          constructor Create( aName          : string;
                              aGateClass     : CGate;
                              aMetaFluid     : TMetaFluid;
                              aSize          : integer;
                              anOptions      : TMetaGateOptions;
                              aFluidDataSize : integer;
                              anOffset       : TOffset );
        private
          fName          : string;
          fGateClass     : CGate;
          fMetaFluid     : TMetaFluid;
          fSize          : integer;
          fOffset        : TOffset;
          fFluidDataSize : integer;
          fOptions       : TMetaGateOptions;
          fIndex         : integer;
        published
          property Name          : string      read fName;
          property GateClass     : CGate       read fGateClass;
          property MetaFluid     : TMetaFluid  read fMetaFluid;
          property Size          : integer     read fSize;
          property Offset        : TOffset     read fOffset;
          property FluidDataSize : integer     read fFluidDataSize;
          property Index         : integer     read fIndex;
        public
          property Options : TMetaGateOptions read fOptions write fOptions;
        public
          function Instantiate( aBlock : TBlock ) : TGate; virtual;
      end;

    // MetaInput inherits MetaGate. It adds the following features:
    //   MinFluid : TInputData
    //     Lowest fluid data allowed by the input.
    //   MaxFluid : TInputData
    //     Highest fluid data allowed by the input.
    //   DefFluid : TInputData
    //     This is the default value of the input gate.
    //   overrides function Instantiate( aBlock ) : TBlock
    //     Instantiate is overriden to set the default fluid value in the new
    //     input gate that is being created.

    TMetaInputLevel  = (mglBasic, mglAditional);
    TMetaInputLevels = set of TMetaInputLevel;
    TMetaInput =
      class( TMetaGate )
        public
          constructor Create( aName          : string;
                              aMinFluid      : TInputData;
                              aMaxFluid      : TInputData;
                              aDefFluid      : TInputData;
                              aMaxCapacity   : TFluidValue;
                              anInputClass   : CInput;
                              aMetaFluid     : TMetaFluid;
                              aSize          : integer;
                              aLevel         : TMetaInputLevel;
                              anOptions      : TMetaGateOptions;
                              aFluidDataSize : integer;
                              anOffset       : TOffset );
        private
          fLevel       : TMetaInputLevel;
          fMinFluid    : TInputData;
          fMaxFluid    : TInputData;
          fDefFluid    : TInputData;
          fMaxCapacity : TFluidValue;
        private
          function GetMinFluid : PInputData;
          function GetMaxFluid : PInputData;
          function GetDefFluid : PInputData;
        published
          property Level       : TMetaInputLevel read fLevel write fLevel;
        public // cannot be published
          property MinFluid    : PInputData  read GetMinFluid;
          property MaxFluid    : PInputData  read GetMaxFluid;
          property DefFluid    : PInputData  read GetDefFluid;
        public
          property MaxCapacity : TFluidValue read fMaxCapacity write fMaxCapacity;
        public
          function Instantiate( aBlock : TBlock ) : TGate; override;
      end;

    // MetaOutput inherits MetaGate. It only adds one feature:
    //   MaxFluid : TFluidData
    //     Highest fluid the output can produce.

    TMetaOutput =
      class( TMetaGate )
        public
          constructor Create( aName          : string;
                              aMaxFluid      : TFluidData;
                              anOutputClass  : COutput;
                              aMetaFluid     : TMetaFluid;
                              aSize          : integer;
                              anOptions      : TMetaGateOptions;
                              aFluidDataSize : integer;
                              anOffset       : TOffset );
        private
          fMaxFluid  : TFluidData;
        published
          property MaxFluid  : TFluidData read fMaxFluid;
      end;

    TLockable =
      class( TPersistent )
        public
          constructor Create;
          destructor Destroy; override;
        private
          fLock : TCriticalSection;
        public
          procedure Lock;
          procedure Unlock;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;


    TBlockModification = (bmVisualChange, bmEvolve);
    TOnBlockModified   = procedure( Modification : TBlockModification ) of object;
    TMoneyReason       = TAccountId;

    // A Facility is something that can be builded, can evolve from one stage to
    // another and can be simulated. Facilities feature:
    //   Name : string
    //     Readable (e.g. "Merchise Farm No. 32"). Facilities are often named by
    //     their owners. It is possible to generate a default name using the
    //     MetaFacility.Name, the Company.Name and some other data. This name
    //     must be unique.
    //   CreationDate : TVirtDate
    //     Date of creation.
    //   xPos, yPos : integer
    //     Where in the map the facility is located.
    //   Company : TCompany
    //     Company that owns the facility.
    //   Town : TTown
    //     Town the facility belongs to. Town is assigned once, when the
    //     facility is created, according the closeness to other towns.
    //   MetaFacility : TMetaFacility
    //     Reference to the metainstance that describes the kind of facility
    //     this facility is instance.
    //   CurrBlock : TBlock
    //     Facilities are simulated by blocks. Each facility uses only one block
    //     to simulate itself at a given time. CurrBlock is a reference to that
    //     block.
    //   procedure Simulate
    //     This is called by the World object to simulate the facility.
    //     Basically, TFacility.Simulate delegates this call to a call to
    //     CurrBlock.Simulate.

    TAccessLevel = (acsFull, acsModerate, acsGuest);
    TConnectionCheckLevel = (chkInputs, chkOutputs, chkBoth);

    TFacility =
      class( TLockable )
        public
          constructor Create( aMetaFacility : TMetaFacility ); virtual;
          destructor  Destroy; override;
        private
          fName            : string;
          fCreationDate    : TVirtDateAbs;
          fXPos            : integer;
          fYPos            : integer;
          fCompany         : TCompany;
          fTown            : TTown;
          fMetaFacility    : TMetaFacility;
          fCurrStage       : integer;
          fCurrBlock       : TBlock;
          fMoneyGraph      : TPlotter;
          fPeriodMoney     : TMoney;
          fLastPeriodMoney : TMoney;
          fMoneyDelta      : integer;
          fTrouble         : byte;
          //fCompanyResearch : TCompanyResearch;
          fCompanyDir      : TCompanyDirection;
          fToBeDemolished  : byte;
          fCost            : TMoney;
          fTCCost          : TMoney;
          fNetProfit       : TMoney;
          fDeleted         : boolean;
          fEffectId        : word;
          fEffectCreator   : string;
          fEffectProgress  : single;
          fEffectStrength  : single;
        private
          function  GetName : string;
          procedure SetName( aName : string );
          procedure SetCompany( aCompany : TCompany );
          procedure SetTown( aTown : TTown );
          function  GetBudget : TMoney;
          function  GetCriticalTrouble : boolean;
          function  GetStopped : boolean;
          procedure SetStopped( Value : boolean );
          function  GetAge : TVirtDateAbs;
          procedure SetDeleted( Value : boolean );
        protected
          function GetVisualClassId : TVisualClassId; virtual;
          function ComputeROI : integer; virtual;
          function GetUpgradeLevel : byte;
          function GetUpgradePerc : byte;
        published
          property Name            : string         read GetName       write SetName;
          property CreationDate    : TVirtDateAbs   read fCreationDate write fCreationDate;
          property XPos            : integer        read fXPos         write fXPos;
          property YPos            : integer        read fYPos         write fYPos;
          property Company         : TCompany       read fCompany      write SetCompany;
          property Town            : TTown          read fTown         write SetTown;
          property MetaFacility    : TMetaFacility  read fMetaFacility;
          property CurrBlock       : TBlock         read fCurrBlock;
          property MoneyGraph      : TPlotter       read fMoneyGraph;
          property MoneyDelta      : integer        read fMoneyDelta;
          property Budget          : TMoney         read GetBudget;
          property PeriodMoney     : TMoney         read fPeriodMoney;
          property CriticalTrouble : boolean        read GetCriticalTrouble;
          property Trouble         : byte           read fTrouble write fTrouble;
          property Stopped         : boolean        read GetStopped write SetStopped;
          property VisualClass     : TVisualClassId read GetVisualClassId;
          property Age             : TVirtDateAbs   read GetAge;
          property ToBeDemolished  : byte           read fToBeDemolished write fToBeDemolished;
          property Cost            : TMoney         read fCost;
          property TCCost          : TMoney         read fTCCost write fTCCost;
          property NetProfit       : TMoney         read fNetProfit;
          property ROI             : integer        read ComputeROI;
          property CurrStage       : integer        read fCurrStage;
          property UpgradeLevel    : byte           read GetUpgradeLevel;
          property UpgradePerc     : byte           read GetUpgradePerc;
          property EffectId        : word           read fEffectId       write fEffectId;
          property EffectCreator   : string         read fEffectCreator  write fEffectCreator;
          property EffectProgress  : single         read fEffectProgress write fEffectProgress;
          property EffectStrength  : single         read fEffectStrength write fEffectStrength;
        public
          //property CompanyResearch : TCompanyResearch  read fCompanyResearch;
          property CompanyDir      : TCompanyDirection read fCompanyDir;
          property Deleted         : boolean           read fDeleted write SetDeleted;
        public
          procedure Simulate;
          procedure CollectInputs;
          procedure CollectInputExtra;
          procedure SpreadOutputs;
          procedure SpreadOutputExtra;
          procedure CheckCircuits;
          procedure CheckConnections( ConnectionCheckLevel : TConnectionCheckLevel );
          procedure Loaded;
        public
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); virtual;
        public
          function  ConnectTo     ( Facility : TFacility ) : string;
          procedure DisconnectFrom( Facility : TFacility );
        published
          procedure RDOConnectInput    ( FluidId, Suppliers : widestring );
          procedure RDOConnectOutput   ( FluidId, Clients   : widestring );
          procedure RDODisconnectInput ( FluidId, Suppliers : widestring );
          procedure RDODisconnectOutput( FluidId, Clients   : widestring );
          procedure RDOSetOutputPrice   ( FluidId : widestring; Price : integer );
          procedure RDOSetInputOverPrice( FluidId : widestring; SupplierIdx, OverPrice : integer );
          procedure RDOSetInputMaxPrice ( FluidId : widestring; MaxPrice : integer );
          procedure RDOSetInputMinK     ( FluidId : widestring; MinK : integer );
          procedure RDOSetInputSortMode (FluidId : widestring; mode : integer);
          function  RDOGetConnectionReport : OleVariant;
          procedure RDOConnectToTycoon     ( TycoonId, FacTypes : integer; SetAsDefault : wordbool );
          procedure RDODisconnectFromTycoon( TycoonId, FacTypes : integer; RemoveAsDefault : wordbool );
          procedure RDOConnectToTradeCenter;
          procedure RDODisconnectFromTradeCenter;
          procedure RDOStartUpgrade;
          procedure RDOStartUpgrades(count : integer);
          procedure RDOStopUpgrade;
          procedure RDODowngrade;
          procedure RDODowngradeMany(count : integer);
        private
          procedure ModifyConnection( Org, Dest : widestring; Connect, Input : boolean );
        private
          fFocusCount : word;
        private
          function GetFocused : boolean;
        public
          property Focused : boolean read GetFocused;
        public
          procedure GetFocus;
          procedure LostFocus;
          procedure VisualUpdate;
        public
          procedure ReportTrouble( aTrouble : byte );
          procedure ClearTrouble( aTrouble : byte );
          function  AccessLevelOf( Tycoon : TTycoon ) : TAccessLevel;
          function  CheckOpAuthenticity : boolean;
        private
          procedure BlockModified( Modification : TBlockModification );
          function  NewBlock : TBlock;
        public
          procedure NotifyNewBlockToOwners;
        protected
          procedure GenMoney( Money : TMoney; Reason : TMoneyReason );
        public
          procedure StatusChanged( Change : TFacilityChange ); virtual;
        public
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; virtual;
          property StatusText[kind : TStatusKind; ToTycoon : TTycoon] : string read GetStatusText;
        public
          procedure Cache(isNew, Background : boolean);
          procedure UnCache(background : boolean);
          procedure UpdateCache(background : boolean);
          function  GetCacheName : string;
        public
          procedure CopySettingsFrom(Facility : TFacility; Options : integer); virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          function  HasTechnology : boolean;
        public
          fRequiresLandTech : boolean;
        public
          procedure CreateCacheLinks;
      end;

    PGateArray        = ^TGateArray;
    TGateArray        = array[any] of TGate;
    TEvaluationResult = (evrNormal, evrEvolve, evrError);
    TAreaMode         = (amdExcludeBlock, amdIncludeBlock);

    PCompanyInputData = ^TCompanyInputData;
    TCompanyInputData =
      record
        Q   : TFluidValue;
        K   : TQuality;
        Max : TFluidValue;
      end;
    PCompanyInputArray = ^TCompanyInputArray;
    TCompanyInputArray = array[any] of TCompanyInputData;

    // A Block is a sort of simulation unit. It has several inputs and outputs
    // that can be connected to the outputs and inputs of other blocks respectively.
    // Featuring:
    //   MetaBlock : TMetaBlock
    //     Reference to a metaintance of class TMetaBlock that describes this kind
    //     of block.
    //   Facility : TFacility
    //     Facility that contains this block.
    //   xPos, yPos : integer
    //     Block position in the Map. This data is directly derived from the
    //     Facility object.
    //   Inputs[index : integer] : TInput
    //     Inputs this block has. Each input is an instance of TInput (or from a
    //     descendant class).
    //   Outputs[index : integer] : TOutput
    //     Outputs this block has. Each input is an instance of TOutput (or from
    //     a descendant class).
    //   InputsByName[name : string] : TInput
    //     Allows to retrieve an input from its name.
    //   OutputsByName[name : string] : TOutput
    //     Allows to retrieve an output from its name.
    //   InputCount : integer
    //     Number of inputs in the block.
    //   OutputCount : integer
    //     Number of outputs in the block.
    //   procedure Simulate
    //     Simulates the block. To do so it calls CollectInputs, Evaluate,
    //     SpreadOutputs and ResetInputs in this order.
    //   procedure Evaluate
    //     Evaluate computes the values of outputs according to inputs. Block
    //     transferential function is here.
    //   procedure CollectInputs
    //     This will call "Evaluate" for each input.
    //   procedure SpreadOutputs
    //     This will call "Evaluate" for each output.
    //   procedure ResetInputs
    //     This will set input values to zero.
    //   procedure AutoConnect
    //     AutoConnect is called by the world when the block is inserted in the
    //     simulation list to give it chance to automatically connect to other
    //     blocks.
    //   procedure BlockGenMoney( Money : TMoney; Reason : TMoneyReason )
    //     This should be called only from "Evaluate" implementations. This will
    //     increase (or decrease) the budget of the owner of the block.
    //   function Offset( var Field ) : TOffset
    //     Returns the offset within the block of some field. This is used to make
    //     inputs and outputs point to private fields that are used to evaluate
    //     the block. Offsets are stored in MetaGate objects.

    TTradeLevel = (tlvSameOnwner, tlvPupil, tlvAllies, tlvAnyone);

    TBlock =
      class( TPersistent )
        protected
          constructor Create( aMetaBlock : TMetaBlock; aFacility : TFacility ); virtual;
        public
          destructor Destroy; override;
        private
          fMetaBlock : TMetaBlock;
          fFacility  : TFacility;
        private
          function GetXPos    : integer;
          function GetYPos    : integer;
          function GetXOrigin : integer;
          function GetYOrigin : integer;
        published
          property MetaBlock : TMetaBlock read fMetaBlock;
          property Facility  : TFacility  read fFacility write fFacility;
          property xPos      : integer    read GetXPos;
          property yPos      : integer    read GetYPos;
          property xOrigin   : integer    read GetXOrigin;
          property yOrigin   : integer    read GetYOrigin;
        private
          fInputs      : PGateArray;
          fOutputs     : PGateArray;
          fInputCount  : byte;
          fOutputCount : byte;
        private
          function GetInput ( index : integer ) : TInput;
          function GetOutput( index : integer ) : TOutput;
          function GetInputIndex  ( name : string ) : integer;
          function GetOutputIndex ( name : string ) : integer;
          function GetInputByName ( name : string ) : TInput;
          function GetOutputByName( name : string ) : TOutput;
          function GetInputCount  : integer;
          function GetOutputCount : integer;
        public
          property Inputs       [index : integer] : TInput  read GetInput;
          property Outputs      [index : integer] : TOutput read GetOutput;
          property InputIndex   [name : string] : integer read GetInputIndex;
          property OutputIndex  [name : string] : integer read GetOutputIndex;
          property InputsByName [name : string] : TInput  read GetInputByName;
          property OutputsByName[name : string] : TOutput read GetOutputByName;
          property InputCount  : integer read GetInputCount;
          property OutputCount : integer read GetOutputCount;
        protected
          function GetSurfaceValue( SurfaceId : TSurfaceId ) : TSurfaceValue; virtual;
        public
          property SurfaceValue[SurfaceId : TSurfaceId] : TSurfaceValue read GetSurfaceValue;
        private
          function GetDt : TTimeDelta;
        public
          property dt : TTimeDelta read GetDt;
        public
          procedure Simulate;
          procedure Stop; virtual;
          procedure Resume; virtual;
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); virtual;
        protected
          function  Evaluate : TEvaluationResult; virtual;
        public
          function  ConnectTo     ( Block : TBlock; Symetrical : boolean ) : string; virtual;
          procedure DisconnectFrom( Block : TBlock; Symetrical : boolean );          virtual;
        public
          procedure CollectInputs;                    virtual;
          procedure CollectInputExtra;                virtual;
          procedure SpreadOutputs;                    virtual;
          procedure SpreadOutputExtra;                virtual;
          procedure ResetInputs;                      virtual;
          procedure AutoConnect( loaded : boolean );  virtual;
          procedure CheckCircuits;                    virtual;
          procedure CheckConnections( ConnectionCheckLevel : TConnectionCheckLevel ); virtual;
        protected
          procedure SetCargoValue( CargoKind : TCargoKind; CargoValue : single ); virtual;
          procedure DelCargoValue( CargoKind : TCargoKind ); virtual;
          function  GetCircuits( CircuitId : TCircuitId ) : TCollection; virtual;
        public
          property Circuits[CircuitId : TCircuitId] : TCollection read GetCircuits;
        protected
          function  GetVisualClassId : TVisualClassId; virtual;
          function  GetRole : TFacilityRole; virtual;
          procedure RefreshVisualClassInfo;
        protected
          fVisualClass : TVisualClassId;
          fTradeLevel  : TTradeLevel;
        protected
          property vVisualClassId : TVisualClassId read fVisualClass write fVisualClass;
        public
          property VisualClassId : TVisualClassId read fVisualClass;
          property Role          : TFacilityRole  read GetRole;
          property TradeLevel    : TTradeLevel    read fTradeLevel;
        public
          procedure StoreLinksToCache( Cache : TObjectCache ); virtual;
          procedure StoreToCache( Cache : TObjectCache ); virtual;
          procedure RemoveFromCache(background : boolean);
        private
          fAcceptCloning : boolean;
        public
          property AcceptCloning : boolean read fAcceptCloning write fAcceptCloning;
        protected
          procedure CopySettingsFrom(Block : TBlock; Options : integer); virtual;
          function  RenderCloneMenu(lang : string) : string; virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        protected
          procedure BlockModified( Modification : TBlockModification );
          procedure BlockLoaded; virtual;
          procedure Deleted; virtual;
        public
          procedure BlockGenMoney( Money : TMoney; Reason : TMoneyReason );
        protected
          function  GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; virtual;
          function  GetArea( Ratio : integer; mode : TAreaMode ) : TRect;
          procedure FacNameChanged; virtual;
        private
          fCompanyInputArray : PCompanyInputArray;
        private
          function GetCompanyInputCount : integer;
          function GetCompanyInput( index : integer ) : PCompanyInputData;
        public
          property CompanyInputCount : integer read GetCompanyInputCount;
          property CompanyInputs[index : integer] : PCompanyInputData read GetCompanyInput;
        private
          function GetWeatherEnvelopValue( idx : integer ) : single;
        public
          property WeatherEnvelopValue[idx : integer] : single read GetWeatherEnvelopValue;
        private
          fOffsetRef : record end;
        protected
          function Offset( var Field ) : TOffset;
        private
          procedure CleanExtraInfo;
        protected
          procedure SetTradeLevel(aTradeLevel : TTradeLevel);
        published
          procedure RDOSetCompanyInputDemand(index, perc : integer);
          procedure RDOSetTradeLevel( aTradeLevel : integer );
        published
          property RDOAcceptCloning : boolean read fAcceptCloning write fAcceptCloning;
        private
          //procedure RebuildInput(index : integer);
          //procedure RebuildOutput(index : integer);
        private
          fUpgradeLevel : byte;
          fUpgradeHours : byte;
          fPendingUpgs  : byte;
        private
          function GetUpgrading   : boolean;
          function GetUpgradePerc : TPercent;
        protected
          function GetUpgradeCost : TMoney; virtual;
          function GetActualMaxUpgrade : integer; virtual;
          function GetUpgradeHourCost : TMoney;
        public
          property UpgradeCost     : TMoney   read GetUpgradeCost;
          property UpgradeHourCost : TMoney   read GetUpgradeHourCost;
          property UpgradeLevel    : byte     read fUpgradeLevel;
          property UpgradeHours    : byte     read fUpgradeHours;
          property PendingUpgrades : byte     read fPendingUpgs;
          property UpgradePerc     : TPercent read GetUpgradePerc;
          property Upgrading       : boolean  read GetUpgrading;
        private
          procedure AdvanceUpgrading;
          procedure StartUpgrading(count : integer = 1);
          procedure StopUpgrading;
          procedure Downgrade(count : integer = 1);
        protected
          procedure RecalculateInventionsEffect; virtual;
          function  UsesInvention(Invention : TInvention) : boolean;
          procedure ReportInventions;
      end;

    TConnectResult    = (cnxValid, cnxRejected, cnxDuplicated, cnxForbiden, cnxGateMaxed);
    TTransportPayMode = (tnpPositive, tnpPositiveShare, tnpNull, tnpNegativeShare, tnpNegative);

    PExtraConnectionInfo = ^TExtraConnectionInfo;
    TExtraConnectionInfo =
      record
        LastFluid : TFluidValue;
        //YearValue : TFluidValue;     
        //YearCost  : TMoney;
        OverPrice : TPercent;
        Connected : boolean;
        Priority  : TPercent;
        Distance  : smallint;
      end;

    TGate =
      class( TPersistent )
        protected
          constructor Create( aMetaGate : TMetaGate; aBlock : TBlock ); virtual;
        public
          destructor  Destroy; override;
        protected
          fFluidData   : PFluidData;
          fBlock       : TBlock;
          fConnections : TCollection;
          fOptions     : TGateAutoconnectOptions;
        private
          function GetConnectionCount : integer;
        protected
          function GetMetaFluid : TMetaFluid; virtual; abstract;
          function GetLastFluid : TFluidValue; virtual;
          function GetDefaultFluidDataSize : integer; virtual; abstract;
        public
          property FluidData       : PFluidData  read fFluidData;
          property LastFluid       : TFluidValue read GetLastFluid;
          property Block           : TBlock      read fBlock;
          property ConnectionCount : integer     read GetConnectionCount;
          property Options         : TGateAutoconnectOptions read fOptions write fOptions;
          property MetaFluid       : TMetaFluid read GetMetaFluid;
        protected
          property vConnections : TCollection read fConnections;
        protected
          function  GetLastCost : TMoney; virtual;
          procedure SetLastCost( aLastCost : TMoney ); virtual;
        public
          property LastCost : TMoney read GetLastCost;
        protected
          property vLastCost : TMoney read GetLastCost write SetLastCost;
        public
          procedure SetFluidData( aFluidData : PFluidData ); virtual;
          procedure InsertConnection( Connection : TGate ); virtual;
          procedure DeleteConnection( Connection : TGate ); virtual;
          procedure ConnectionChanged( Connection : TGate ); virtual;
          function  GetUniversalPrecedence( Connection : TGate ) : integer; virtual;
          function  GetConnectionPrecedence( Connection : TGate ) : integer; virtual;
          function  ConnectionAllowed( Connection : TGate ) : TConnectResult; virtual;
          function  GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo; virtual;
          procedure CheckConnections; virtual;
        public
          property ExtraConnectionInfo[index : integer] : PExtraConnectionInfo read GetExtraConnectionInfo;
        public
          function  ConnectTo( Gate : TGate ) : TConnectResult; virtual;
          procedure DisconnectFrom( Gate : TGate );
          procedure DisconnectAll;
          procedure SortConnections; virtual;
        protected
          class function BestCollection( aSize : integer ) : TCollection; virtual;
        protected
          procedure AutoConnect( loaded : boolean ); virtual;
        public
          procedure StoreToCache( Cache : TObjectCache ); virtual;
          procedure InvalidateCache(connections : boolean); virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        protected
          procedure Loaded; virtual;
        private
          procedure HookFluidData( aMetaGate : TMetaGate );
          function  ConnectionAllowedByPolicy( Gate : TGate ) : boolean;
          function  GetOwnerPermissionMapId : integer;
        protected
          procedure AutoRepair(Block : TBlock; index : integer); virtual;
        public
          function  GetCurrentTick : integer;
          procedure UpdateFluid; virtual;
      end;

    TInput =
      class( TGate )
        protected
          constructor Create( aMetaGate : TMetaGate; aBlock : TBlock ); override;
        private
          fMetaInput      : TMetaInput;
          fActualMaxFluid : TInputData;
          fMaxCapacity    : TFluidValue;
        private
          function GetActualMaxFluid : PInputData;
        protected
          function GetCapacity : TFluidValue; virtual;
        public
          property MetaInput      : TMetaInput  read fMetaInput;
          property ActualMaxFluid : PInputData  read GetActualMaxFluid;
          property MaxCapacity    : TFluidValue read fMaxCapacity write fMaxCapacity;
          property Capacity       : TFluidValue read GetCapacity;
        protected
          function GetMetaFluid : TMetaFluid; override;
          function GetDefaultFluidDataSize : integer; override;
        private
          function GetConnection( index : integer ) : TOutput;
        public
          property Connections[index : integer] : TOutput read GetConnection;
        protected
          procedure Collect;      virtual;
          procedure CollectExtra; virtual;
        private
          fLastValue : TInputData;
        public
          property LastValue : TInputData read fLastValue;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure InsertConnection( Connection : TGate ); override;
          function GetConnectionPrecedence( Connection : TGate ) : integer; override;
          function ConnectionAllowed( Connection : TGate ) : TConnectResult; override;
        published
          procedure RDOSetInputFluidPerc(Perc : integer);
        protected
          procedure AutoRepair(Block : TBlock; index : integer); override;
          procedure SetSortMode(mode : byte); virtual;
          function  IsActive : boolean; virtual;
      end;

    TOutputPrice = word;

    TOutput =
      class( TGate )
        protected
          constructor Create( aMetaGate : TMetaGate; aBlock : TBlock ); override;
        private
          fMetaOutput : TMetaOutput;
          fPricePerc  : TOutputPrice;
        private
          function  GetPrice : TMoney;
          procedure SetPricePerc( aPricePerc : TOutputPrice );
        published
          property MetaOutput : TMetaOutput  read fMetaOutput;
          property Price      : TMoney       read GetPrice;
          property PricePerc  : TOutputPrice read fPricePerc write SetPricePerc;
        protected
          function GetDemand : TFluidValue; virtual;
        public
          property Demand : TFluidValue read GetDemand;
        protected
          function GetMetaFluid : TMetaFluid; override;
          function GetDefaultFluidDataSize : integer; override;
        private
          function GetConnection( index : integer ) : TInput;
        public
          property Connections[index : integer] : TInput read GetConnection;
        protected
          procedure Spread; virtual;
          procedure SpreadExtra; virtual;
        public
          function PriceToDeliver( Value : TFluidValue; Input : TInput; ExtraConnectionInfo : PExtraConnectionInfo; TransportPayMode : TTransportPayMode ) : TMoney;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure InsertConnection( Connection : TGate ); override;
          function GetConnectionPrecedence( Connection : TGate ) : integer; override;
          function ConnectionAllowed( Connection : TGate ) : TConnectResult; override;
        protected
          procedure AutoRepair(Block : TBlock; index : integer); override;
      end;

    TPushInput =
      class( TInput )
        protected
          constructor Create( aMetaGate : TMetaGate; aBlock : TBlock ); override;
        protected
          procedure Collect; override;
          function  GetDefaultFluidDataSize : integer; override;
        private
          fSkipped : boolean;
      end;

    TPullInput =
      class( TInput )
        public
          constructor Create( aMetaGate : TMetaGate; aBlock : TBlock ); override;
          destructor  Destroy; override;
        protected
          function GetCapacity : TFluidValue; override;
        protected
          procedure Collect;      override;
          procedure CollectExtra; override;
        protected
          procedure CheckIndex;
          function  IndexAtOutput( Output : TOutput; OutputIdx : integer ) : integer;
          procedure DoCollect( CollectExtra : boolean );
        private
          fLastCost : TMoney;
          fMaxPrice : word;
          fMinK     : TPercent;
        protected
          function  GetLastCost : TMoney; override;
          procedure SetLastCost( aLastCost : TMoney ); override;
        public
          property MaxPrice : word     read fMaxPrice write fMaxPrice;
          property MinK     : TPercent read fMinK     write fMinK;
        public
          procedure InsertConnection( Connection : TGate ); override;
          function  GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo; override;
          procedure CheckConnections; override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure StoreToCache( Cache : TObjectCache ); override;
        private
          //fParValue    : TTownParameter;
          //fParCapacity : TTownParameter;
          //fParQuality  : TTownParameter;
          //fParPrice    : TTownParameter;
          //fParMaxPrice : TTownParameter;
        protected
          procedure AutoConnect( loaded : boolean ); override;
          procedure Loaded; override;
          class function BestCollection( aSize : integer ) : TCollection; override;
        published
          procedure OnConnectionsModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
        private
          fInvIndex : TCollection;
        public
          function TransferAllowed( Output : TPullOutput ) : boolean;
        private
          fSelected : boolean;
        public
          property Selected : boolean read fSelected write fSelected;
        published
          procedure RDOSelSelected(value : WordBool);
        protected
          function  IsActive : boolean; override;
      end;

    TPushOutput =
      class( TOutput )
        protected
          procedure Spread; override;
      end;

    TSlice =
      class
        public
          constructor Create;
        public
          val    : TFluidValue;
          taken  : boolean;
          tickId : word;
          extra  : TFluidValue;
        public
          ExtraConnectionInfo : TExtraConnectionInfo;
      end;

    TPullOutput =
      class( TOutput )
        protected
          constructor Create( aMetaGate : TMetaGate; aBlock : TBlock ); override;
        public
          destructor Destroy; override;
        private
          fSlices : TCollection;
        protected
          property vSlices : TCollection read fSlices;
        published
          procedure OnConnectionsModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
        protected
          procedure Spread; override;
          procedure SpreadExtra; override;
        protected
          fQAuxBuff : TFluidValue;
          fIdxAux   : integer;
          fDemand   : TFluidValue;
          fLastQ    : TFluidValue;
        protected
          function GetDemand : TFluidValue; override;
          function GetLastFluid : TFluidValue; override;
        public
          procedure ValuePulled ( Value : TFluidValue; Input : TInput; Idx, tick : integer ); virtual;
          procedure ReportDemand( Value : TFluidValue; Input : TInput );                virtual;
          procedure Slice       ( Value : TFluidValue );                                virtual;
          function  GetSliceFor ( Input : TInput; Idx : integer ) : TFluidValue;        virtual;
        private
          procedure ComputePriorities;
        public
          procedure ConnectionChanged( Connection : TGate ); override;
          procedure InsertConnection( Connection : TGate ); override;
          function  GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo; override;
          procedure CheckConnections; override;
        protected
          class function BestCollection( aSize : integer ) : TCollection; override;
        public
          procedure StoreToCache( Cache : TObjectCache ); override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        private
          //fParCapacity : TTownParameter;
          //fParValue    : TTownParameter;
          //fParQuality  : TTownParameter;
          fParPrice    : TTownParameter;
        protected
          procedure AutoConnect( loaded : boolean ); override;
          procedure CheckSlices;
          procedure AutoRepair(Block : TBlock; index : integer); override;
        public
          property LastQ : TFluidValue read fLastQ;
        public
          procedure UpdateFluid; override;
      end;

    TOrdinanceNumId = byte;
    TOrdinanceSet   = set of TOrdinanceNumId;
    TPublicOpinion  = array[TPeopleKind] of integer;
    TOrdinance =
      class( TMetaInstance )
        public
          constructor Create( anId, aName, aDesc : string; aNumId : TOrdinanceNumId; aPublicOpinion : array of integer; aCost : TMoney );
        private
          fNumId         : TOrdinanceNumId;
          fName          : string;
          fDesc          : string;
          fPublicOpinion : TPublicOpinion;
          fCost          : TMoney;
        public
          property NumId         : TOrdinanceNumId read fNumId;
          property Name          : string          read fName;
          property Desc          : string          read fDesc;
          property PublicOpinion : TPublicOpinion  read fPublicOpinion;
          property Cost          : TMoney          read fCost;
      end;

    TLoan =
      class( TPersistent )
        public
          destructor Destroy; override;
        private
          fInterest : TPercent;
          fAmount   : TMoney;
          fTerm     : integer;
          fDebtor   : TMoneyDealer;
          fBank     : TBank;
          fDate     : TVirtDateAbs;
          fSlice    : TMoney;
        public
          property Interest : TPercent     read fInterest write fInterest;
          property Amount   : TMoney       read fAmount;
          property Slice    : TMoney       read fSlice;
          property Term     : integer      read fTerm;
          property Debtor   : TMoneyDealer read fDebtor;
          property Bank     : TBank        read fBank write fBank;
          property Date     : TVirtDateAbs read fDate;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TBankRequestResult = (brqApproved, brqRejected, brqNotEnoughFunds);

    TBank =
      class( TLockable )
        public
          constructor Create( anOwner : TMoneyDealer );
          destructor  Destroy; override;
        private
          fOwner    : TMoneyDealer;
          fLoans    : TCollection;
          fName     : string;
          fInterest : TPercent;
          fTerm     : integer;
          fTimer    : ITimer;
          fLocator  : IWorldLocator;
        public
          property Owner    : TMoneyDealer  read fOwner;
          property Loans    : TCollection   read fLoans;
          property Name     : string        read fName     write fName;
          property Interest : TPercent      read fInterest write fInterest;
          property Term     : integer       read fTerm     write fTerm;
          property Timer    : ITimer        read fTimer    write fTimer;
          property Locator  : IWorldLocator read fLocator  write fLocator;
        public
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); virtual;
          function  EstimateLoan( Client : TMoneyDealer ) : TMoney;
          function  AskLoan( Client : TMoneyDealer; Amount : TMoney ) : TBankRequestResult;
          procedure DealerDeleted( Dealer : TMoneyDealer );
          procedure Loaded; virtual;
        protected
          function LoanApproved( Client : TMoneyDealer; Amount : TMoney ) : boolean; virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TMoneyDealer =
      class( TLockable )
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fAccounts   : TAccountingSystem;
          fNetProfit  : TMoney;
          fLastPeriod : TMoney;
          fLoans      : TLockableCollection;
          fTimer      : ITimer;
        protected
          function  GetBudget : TMoney;            virtual;
          procedure SetBudget( aBudget : TMoney ); virtual;
        published
          property Budget : TMoney read GetBudget write SetBudget;
        public
          property Accounts  : TAccountingSystem   read fAccounts;
          property Loans     : TLockableCollection read fLoans;
          property Timer     : ITimer              read fTimer        write fTimer;
        published
          property NetProfit : TMoney read fNetProfit;
        public
          procedure CancelLoans;
        public
          procedure GenMoney( Money : TMoney; Reason : TMoneyReason ); virtual;
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure Loaded; virtual;
        private
          fBankLoanPerc : TPercent;
        private
          function  GetBankLoanPerc : TPercent;
          procedure SetBankLoanPerc( Value : TPercent );
        protected
          function GetLoanLimit : TMoney; virtual;
          function GetLoanAmount : TMoney;
        public
          property BankLoanPerc : TPercent read GetBankLoanPerc write SetBankLoanPerc;
          property LoanLimit : TMoney read GetLoanLimit;
          property LoanAmount : TMoney read GetLoanAmount;
        protected
          function BankLoanLimit : TMoney;
        public
          procedure Check; virtual;
      end;

    TMetaTownParameter =
      class( TMetaInstance )
        public
          constructor Create( anId, aName : string; anAutoClear : boolean );
        private
          fName      : string;
          fName_MLS  : TMultiString;
          fAutoClear : boolean;
        public
          property Name      : string read fName;
          property Name_MLS  : TMultiString read fName_MLS;
          property AutoClear : boolean read fAutoClear;
        public
          procedure RetrieveTexts( Container : TDictionary ); override;
          procedure StoreTexts   ( Container : TDictionary ); override;
        public
          procedure Register;
      end;

    TTownParameter =
      class( TLockable )
        private
          constructor Create( aMetaTownParameter : TMetaTownParameter );
        public
          fMetaTownParameter : TMetaTownParameter;
          fValue             : double;
          fCurrValue         : double;
          fCount             : double;
          fCurrCount         : double;
        private
          function GetValue   : double;
          function GetAverage : double;
        public
          property MetaTownParameter : TMetaTownParameter read fMetaTownParameter;
          property Value             : double             read GetValue;
          property CurrValue         : double             read fCurrValue write fCurrValue;
          property Count             : double             read fCount;
          property Average           : double             read GetAverage;
        public
          procedure IncCount( amount : double );
        public
          procedure Update;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TTownId = integer;
    TTown =
      class( TMoneyDealer )
        public
          constructor Create( aName : string; aXPos, aYPos : integer; aCluster : TCluster; aModelFactory : IModelFactory; aMailServer : IMailServer );
          destructor  Destroy; override;
        private
          fId           : TTownId;
          fName         : string;
          fXPos         : integer;
          fYPos         : integer;
          fCluster      : TCluster;
          fCreated      : TVirtDate;
          fTimer        : ITimer;
          fMapRefresh   : IMapRefresh;
          fWorldLocator : IWorldLocator;
          fRoadHandler  : IRoadHandler;
          fModelFactory : IModelFactory;
          fOrdinanceSet : TOrdinanceSet;
          fOrdinances   : TLockableCollection;
          fParameters   : TLockableCollection;
          fParmHash     : TMapStringToObject;
          fMayor        : TTycoon;
          fTownCompany  : TCompany;
          fRoadBlocks   : integer;
        private
          function GetHasMayor : boolean;
        published
          property Id           : TTownId       read fId;
          property Name         : string        read fName         write fName;
          property xPos         : integer       read fXPos         write fXPos;
          property yPos         : integer       read fYPos         write fYPos;
          property Cluster      : TCluster      read fCluster;
          property Created      : TVirtDate     read fCreated      write fCreated;
          property Timer        : ITimer        read fTimer        write fTimer;
          property MapRefresh   : IMapRefresh   read fMapRefresh   write fMapRefresh;
          property WorldLocator : IWorldLocator read fWorldLocator write fWorldLocator;
          property RoadHandler  : IRoadHandler  read fRoadHandler  write fRoadHandler;
          property ModelFactory : IModelFactory read fModelFactory write fModelFactory;
          property Mayor        : TTycoon       read fMayor;
          property TownCompany  : TCompany      read fTownCompany;
          property HasMayor     : boolean       read GetHasMayor;
          property RoadBlocks   : integer       read fRoadBlocks   write fRoadBlocks;
        private
          function GetOrdinance( NumId : TOrdinanceNumId ) : boolean;
          function GetParameter( Id : string ) : TTownParameter;
        public
          property Ordinance[NumId : TOrdinanceNumId] : boolean read GetOrdinance;
          property Parameters[Id : string] : TTownParameter read GetParameter;
        public
          procedure UpdateParameters; virtual;
          function  GetContextStatusStr( ToTycoon : TTycoon ) : string; virtual;
        published
          procedure SetOrdinance( Id : string );
          procedure DelOrdinance( Id : string );
        private
          fTaxes       : TLockableCollection;
          fMinSalaries : array[TPeopleKind] of TPercent;
        private
          function  GetTaxes( TaxId : string ) : TTax;
          function  GetMinSalary( Kind : TPeopleKind ) : TPercent;
          function  GetMayorSalary( Kind : TPeopleKind ) : TPercent;
          procedure SetMinSalary( Kind : TPeopleKind; Value : TPercent );
        public
          property Taxes[TaxId : string] : TTax read GetTaxes;
          property AllTaxes : TLockableCollection read fTaxes;
          property MinSalary[kind : TPeopleKind] : TPercent read GetMinSalary write SetMinSalary;
          property MayorMinSalary[kind : TPeopleKind] : TPercent read GetMayorSalary write SetMinSalary;
        public
          procedure GenMoney( Money : TMoney; Reason : TMoneyReason ); override;
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure Loaded; override;
        private
          procedure InitParameters;
        public
          procedure TycoonDeleted( Tycoon : TTycoon ); virtual;
        private
          fRndNames : TStringList;
        private
          procedure GenerateMediaNames;
        public
          function GetRndName(index : integer) : string;
      end;

    TCluster =
      class( TMetaInstance )
        public
          constructor Create( anId : string );
        private
          fCompany      : TCompany;
          fName_MLS     : TMultiString;
          fCompanyClass : CCompany;
          fTier         : integer;
          fSpecialSeal  : boolean;
        public
          property Company      : TCompany     read fCompany      write fCompany;
          property CompanyClass : CCompany     read fCompanyClass write fCompanyClass;
          property Name_MLS     : TMultiString read fName_MLS;
          property Tier         : integer      read fTier         write fTier;
          property SpecialSeal  : boolean      read fSpecialSeal  write fSpecialSeal;
        public
          function NameNewspaper( TownName : string ) : string; virtual;
        public
          procedure RetrieveTexts( Container : TDictionary ); override;
          procedure StoreTexts   ( Container : TDictionary ); override;
      end;

{
    TCompanyResearch =
      class( TLockable )
        public
          constructor Create( aCompany : TCompany );
          destructor  Destroy; override;
        private
          fCompany      : TCompany;
          fInventions   : TCollection;
          fInventionSet : TInventionSet;
          fKind         : string;
          fLevel        : integer;
        published
          property Company      : TCompany    read fCompany;
          property Inventions   : TCollection read fInventions;
          property Kind         : string      read fKind;
          property Level        : integer     read fLevel;
        private
          function GetHasInvention( NumId : TInventionNumId ) : boolean;
          function GetCanResearch( Invention : TInvention ) : boolean;
        public
          property HasInvention[NumId : TInventionNumId] : boolean read GetHasInvention;
          property CanResearch[Invention : TInvention] : boolean read GetCanResearch;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure DeclareInvention(Invention : TInvention);
      end;
}

    TCompanyDirection =
      class( TLockable )
        public
          constructor Create( anId : string );
        private
          fId         : string;
          fStrength   : single;
          fDemand     : single;
          fLastDemand : single;
          fSupport    : single;
          fCount      : integer;
          fLastCount  : integer;
        public
          property Id         : string  read fId;
          property Strength   : single  read fStrength write fStrength;
          property Demand     : single  read fDemand   write fDemand;
          property LastDemand : single  read fLastDemand;
          property Support    : single  read fSupport;
          property Count      : integer read fCount write fCount;
          property LastCount  : integer read fLastCount;
        public
          procedure Update;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        private
          fComplFacs     : integer;
          fLastComplFacs : integer;
        public
          property ComplexFacs     : integer read fComplFacs write fComplFacs;
          property LastComplexFacs : integer read fLastComplFacs;
      end;

    TProjectKind = word;
    TProjectStatus = (prjRunning, prjStopped);
    TProject =
      class( TLockable )
        public
          constructor Create( aName : string; aKind : TProjectKind );
          destructor  Destroy; override;
        private
          fName    : string;
          fKind    : TProjectKind;
          fMembers : TLockableCollection;
          fStatus  : TProjectStatus;
        private
          procedure SetStatus( aStatus : TProjectStatus );
        public
          property Name   : string         read fName   write fName;
          property Kind   : TProjectKind   read fKind;
          property Status : TProjectStatus read fStatus write SetStatus;
        public
          procedure AddMember( Member : TFacility );
          procedure DelMember( Member : TFacility );
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TCompanyMetaFacilityInfo =
      class( TPersistent )
        private
          constructor Create( aMetaFacility : TMetaFacility );
        private
          fMetaFacility : TMetaFacility;
          fCounter      : integer;
        public
          property MetaFacility : TMetaFacility read fMetaFacility;
          property Counter      : integer       read fCounter;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TCompanyMetaFacilityList =
      class( TPersistent )
        public
          constructor Create( aCompany : TCompany );
          destructor  Destroy; override;
        private
          fCompany  : TCompany;
          fInfoList : TLockableCollection;
        private
          function GetMetaFacilityInfo( MetaFacility : TMetaFacility ) : TCompanyMetaFacilityInfo;
        public
          property Company : TCompany read fCompany;
          property MetaFacilityInfo[MetaFacility : TMetaFacility] : TCompanyMetaFacilityInfo read GetMetaFacilityInfo;
        public
          function GetNextFacilityNumber( MetaFacility : TMetaFacility ) : integer;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TCompanyInput =
      class
        public
          constructor Create( aKind : TMetaFluid );
          destructor  Destroy; override;
        private
          fKind   : TMetaFluid;
          fValue  : TFluidData;
          fDemand : TFluidValue;
          fRatio  : single;
          fInputs : TLockableCollection;
        public
          property Kind   : TMetaFluid  read fKind;
          property Value  : TFluidData  read fValue;
          property Demand : TFluidValue read fDemand write fDemand;
          property Ratio  : single      read fRatio;
        private
          procedure Spread;
      end;

    PCompanyInputIndex = ^TCompanyInputIndex;
    TCompanyInputIndex = array[0..1000] of TCompanyInput;

    TCompanyId = word;
    TCompany =
      class( TMoneyDealer )
        public
          constructor Create( anId : TCompanyId ); virtual;
          destructor  Destroy; override;
        private
          fOwner             : TTycoon;
          fFacilities        : TLockableCollection;
          fCluster           : TCluster;
          fName              : string;
          fCreated           : TVirtDate;
          fId                : TCompanyId;
          fAutoConnectLevels : TMetaInputLevels;
          fInventions        : TLockableCollection;
          fInventionSet      : TInventionSet;
          fDirections        : TLockableCollection;
          fProjects          : TLockableCollection;
          fPrivated          : boolean;
          fMetaFacilityList  : TCompanyMetaFacilityList;
          fCompanyInputs     : TLockableCollection;
          fDeleted           : boolean;
          fResearchCost      : TMoney;
          fUniquenessMask    : TUniquenessMask;
          fNewUniquenessMask : TUniquenessMask;
        protected
          function GetBudget : TMoney; override;
        private
          function GetTechnologyDescriptor : string;
          function GetOwned : boolean;
        published
          property Owner             : TTycoon                  read fOwner   write fOwner;
          property Owned             : boolean                  read GetOwned;
          property Cluster           : TCluster                 read fCluster write fCluster;
          property Name              : string                   read fName    write fName;
          property Created           : TVirtDate                read fCreated write fCreated;
          property Deleted           : boolean                  read fDeleted write fDeleted;
          property Id                : TCompanyId               read fId;
          property AutoConnectLevels : TMetaInputLevels         read fAutoConnectLevels write fAutoConnectLevels;
          property Facilities        : TLockableCollection      read fFacilities        write fFacilities;
          property Projects          : TLockableCollection      read fProjects;
          property Privated          : boolean                  read fPrivated;
          property MetaFacilityList  : TCompanyMetaFacilityList read fMetaFacilityList;
          property TechDescriptor    : string                   read GetTechnologyDescriptor;
          property ResearchCost      : TMoney                   read fResearchCost;
          property UniquenessMask    : TUniquenessMask          read fUniquenessMask;
          property NewUniquenessMask : TUniquenessMask          read fNewUniquenessMask write fNewUniquenessMask;
        public
          procedure FacilityCreated  ( Facility : TFacility );
          procedure FacilityDestroyed( Facility : TFacility );
        public
          procedure UpdateParameters; virtual;
        private
          function GetHasInvention( NumId : TInventionNumId ) : boolean;
          function GetProject( name : string ) : TProject;
          function GetDirection( id : string ) : TCompanyDirection;
        public
          property HasInvention[NumId : TInventionNumId] : boolean read GetHasInvention;
          property Inventions : TLockableCollection read fInventions;
          property Directions[id : string] : TCompanyDirection read GetDirection;
          property Project[name : string] : TProject read GetProject;
        public
          procedure DeclareInvention(Invention : TInvention);
          procedure ReportUsage(Invention  : TInvention; count : integer; use : boolean);  overload;
          procedure ReportUsage(Inventions : TCollection; count : integer; use : boolean); overload;
          function  CanResearch(Invention : TInvention) : boolean;
        protected
          procedure InitCompanyInputs; virtual;
          procedure RegisterInput  ( Input : TInput ); virtual;
          procedure UnregisterInput( Input : TInput ); virtual;
          function  FindCompanyInput( Kind : TMetaFluid ) : TCompanyInput;
        private
          fCompanyInputIndex : PCompanyInputIndex;
        private
          function GetCompanyInput( UNId : integer ) : TCompanyInput;
        public
          property CompanyInput[UNId : integer] : TCompanyInput read GetCompanyInput;
        public
          procedure GenMoney( Money : TMoney; Reason : TMoneyReason ); override;
        public
          procedure Loaded; override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          function  GetCacheName : string;
          function  FindInventionRecord(Invention : TInvention) : TInventionRecord;
          function  RetireInventionById(InventionId : string) : boolean;
          function  RetireInvention(Invention : TInvention) : boolean;
          procedure RecalculateInventions;
          procedure RecalculateInventionApplicationCost;
          function  CalcInventionUsage(Invention : TInvention) : integer;
          procedure ChargeResearches(dt : TTimeDelta);
        public
          procedure CacheLinks;
          function  CheckOpAuthenticity : boolean;
      end;

    TAutoConnection =
      class( TPersistent )
        public
          constructor Create( aMetaFluidId : string );
          destructor  Destroy; override;
        private
          fMetaFluidId        : string;
          fConnections        : TLockableCollection;
          fHireTradeCenter    : boolean;
          fSearchForSuppliers : boolean;
          fHireOnlyWarehouses : boolean;
        public
          property MetaFluidId        : string              read fMetaFluidId;
          property Connections        : TLockableCollection read fConnections;
          property HireTradeCenter    : boolean             read fHireTradeCenter;
          property SearchForSuppliers : boolean             read fSearchForSuppliers;
          property HireOnlyWarehouses : boolean             read fHireOnlyWarehouses;
        private
          procedure FacilityDestroyed( Facility : TFacility );
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TTycoonOption  = (tyoMainRole);
    TTycoonOptions = set of TTycoonOption;

    TPolicyStatus = (pstAlly, pstNeutral, pstEnemy);

    TCurriculumItem =
      class( TPersistent )
        public
          constructor Create( anId : string; aKind : integer );
        private
          fId        : string;
          fKind      : integer;
          fTycoon    : TTycoon;
          fPublished : boolean;
        protected
          function GetDesc( langId : TLanguageId ) : string;    virtual;
          function GetImportance : integer;   virtual;
          function GetPrestige   : TPrestige; virtual;
        public
          property Id         : string                 read fId;
          property Kind       : integer                read fKind;
          property Tycoon     : TTycoon                read fTycoon;
          property Desc[langId : TLanguageId] : string read GetDesc;
          property Importance : integer                read GetImportance;
          property Prestige   : TPrestige              read GetPrestige;
          property Published  : boolean                read fPublished write fPublished;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          class function IsGlobal : boolean; virtual; 
      end;

    TRankingInfo =
      class
        private
          fRanking  : TRanking;
          fPosition : integer;
        public
          property Ranking  : TRanking read fRanking;
          property Position : integer  read fPosition;
      end;

    TTycoonLevel =
      class( TMetaInstance )
        public
          constructor Create( anId, aNextLevelId : string; Tier : integer );
        private
          fName                : string;
          fDescription         : string;
          fCondition           : string;
          fName_MLS            : TMultiString;
          fDescription_MLS     : TMultiString;
          fCondition_MLS       : TMultiString;
          fMoneyBackOnDemolish : single;
          fPercOfResearchSubs  : single;
          fPrestigeBoost       : integer;
          fPriority            : integer;
          fFacLimit            : integer;
          fNextLevel           : TTycoonLevel;
          fTier                : integer;
          fFee                 : currency;
          fHourIncome          : currency;
          fPrevLevelID         : string;
        public
          property Name                : string       read fName                write fName;
          property Description         : string       read fDescription         write fDescription;
          property Condition           : string       read fCondition           write fCondition;
          property Name_MLS            : TMultiString read fName_MLS;
          property Description_MLS     : TMultiString read fDescription_MLS;
          property Condition_MLS       : TMultiString read fCondition_MLS;
          property MoneyBackOnDemolish : single       read fMoneyBackOnDemolish write fMoneyBackOnDemolish;
          property PercOfResearchSubs  : single       read fPercOfResearchSubs  write fPercOfResearchSubs;
          property PrestigeBoost       : integer      read fPrestigeBoost       write fPrestigeBoost;
          property Priority            : integer      read fPriority            write fPriority;
          property FacLimit            : integer      read fFacLimit            write fFacLimit;
          property NextLevel           : TTycoonLevel read fNextLevel           write fNextLevel;
          property Tier                : integer      read fTier                write fTier;
          property Fee                 : currency     read fFee                 write fFee;
          property HourIncome          : currency     read fHourIncome          write fHourIncome;
          property PrevLevelID         : string       read fPrevLevelID         write fPrevLevelID;
        public
          function AdvanceTycoon( Tycoon : TTycoon; out Reason : TMultiString ) : boolean; virtual; abstract;
        public
          procedure RetrieveTexts( Container : TDictionary ); override;
          procedure StoreTexts   ( Container : TDictionary ); override;
          procedure RetrieveDynamicTexts;
      end;

    PVoteArray = ^TVoteArray;
    TVoteArray = array[0..0] of TTycoon;

    TTycoonId = word;
    TTycoon =
      class( TMoneyDealer )
        public
          constructor Create( anId : TTycoonId ); virtual;
          destructor  Destroy; override;
        private
          fId              : TTycoonId;
          fOptions         : TTycoonOptions;
          fCompanies       : TLockableCollection;
          fRoles           : TLockableCollection;
          fSuperRole       : TTycoon;
          fName            : string;
          fPassword        : string;
          fBudget          : TMoney;
          fFailureLevel    : integer;
          fRanking         : integer;
          fRankingAvg      : integer;
          fFocus           : TPoint;
          fAutoConnections : TLockableCollection;
          fPolicies        : TLockableCollection;
          fWorldLocator    : IWorldLocator;
          fMailServer      : IMailServer;
          fPrestige        : TPrestige;
          fFacPrestige     : TPrestige;
          fResearchPrest   : TPrestige;
          fCurrFacPrestige : TPrestige;
          fResearchCount   : integer;
          fCurriculum      : TLockableCollection;
          fCookies         : TStringList;
          fRankings        : TLockableCollection;
          fEvents          : TLockableCollection;
          //fEffects         : TLockableCollection;
          fDeleted         : boolean;
          fTranscending    : boolean;
          fWillTranscend   : boolean;
          fLicenceLevel    : single;
          fLanguage        : TLanguageId;
          fFacilityCount   : integer;
          fCountUpgrades   : boolean;
          fArea            : integer;
          fEffectId        : word;
          fEffectCreator   : string;
          fEffectProgress  : single;
          fEffectStrength  : single;
          fNobPoints       : integer;
          fRoadBlocks      : integer;
          fFavorites       : TFavorites;
          fIsDemo          : boolean;
        protected
          function  GetBudget : TMoney; override;
          procedure SetBudget( aBudget : TMoney ); override;
        published
          function RDOGetBudget : OleVariant;
        private
          function  GetFocusX : integer;
          function  GetFocusY : integer;
        private
          function  GetMasterRole : TTycoon;
          function  GetRealName : string;
          function  GetAllCompaniesCount : integer;
          function  GetAllCompanies( index : integer ) : TCompany;
          function  GetCookie( Id : string ) : string;
          function  GetCookies : string;
          procedure SetCookie( Id, Value : string );
          function  GetRanking( Id : string ) : TRankingInfo;
          function  GetFacCount : integer;
          function  GetFacMax   : integer;
          function  GetAreaTax  : TMoney;
          function  GetNobPoints : integer;
        published
          property Id           : TTycoonId           read fId;
          property Options      : TTycoonOptions      read fOptions   write fOptions;
          property SuperRole    : TTycoon             read fSuperRole write fSuperRole;
          property MasterRole   : TTycoon             read GetMasterRole;
          property FocusX       : integer             read GetFocusX;
          property FocusY       : integer             read GetFocusY;
          property Companies    : TLockableCollection read fCompanies;
          property Roles        : TLockableCollection read fRoles;
          property Rankings     : TLockableCollection read fRankings;
          property Events       : TLockableCollection read fEvents;
          property Name         : string              read fName          write fName;
          property RealName     : string              read GetRealName;
          property Password     : string              read fPassword      write fPassword;
          property Ranking      : integer             read fRanking       write fRanking;
          property RankingAvg   : integer             read fRankingAvg    write fRankingAvg;
          property FailureLevel : integer             read fFailureLevel  write fFailureLevel;
          property FacCount     : integer             read GetFacCount;
          property Area         : integer             read fArea;
          property AreaTax      : TMoney              read GetAreaTax;
          property FacMax       : integer             read GetFacMax;
          property LicenceLevel : single              read fLicenceLevel  write fLicenceLevel;
          property Language     : TLanguageId         read fLanguage      write fLanguage;
          property CountUpgrades: boolean             read fCountUpgrades write fCountUpgrades;
          property NobPoints    : integer             read GetNobPoints   write fNobPoints;
          property IsDemo       : boolean             read fIsDemo        write fIsDemo;
          property RoadBlocks   : integer             read fRoadBlocks    write fRoadBlocks;
          property Favorites    : TFavorites          read fFavorites;
        published
          property EffectId        : word   read fEffectId       write fEffectId;
          property EffectCreator   : string read fEffectCreator  write fEffectCreator;
          property EffectProgress  : single read fEffectProgress write fEffectProgress;
          property EffectStrength  : single read fEffectStrength write fEffectStrength;
        protected
          class function GetIsRole : boolean; virtual;
          class function AllowedFacTypes : TFacTypeSet; virtual;
          class function DesignedZoner : boolean; virtual;
        private
          function GetHasZonerRole : boolean;
        public
          property Prestige        : TPrestige read fPrestige;
          property FacPrestige     : TPrestige read fFacPrestige;
          property ResearchPrest   : TPrestige read fResearchPrest;
          property CurrFacPrestige : TPrestige read fCurrFacPrestige write fCurrFacPrestige;
          property ResearchCount   : integer   read fResearchCount;
          property Deleted         : boolean   read fDeleted         write fDeleted;
          property Transcending    : boolean   read fTranscending    write fTranscending;
          property WillTranscend   : boolean   read fWillTranscend;
          property IsRole          : boolean   read GetIsRole;
          property HasZonerRole    : boolean   read GetHasZonerRole;
        public
          property AllCompanies[index : integer] : TCompany read GetAllCompanies;
          property AllCompaniesCount : integer              read GetAllCompaniesCount;
        public
          property RankingById[Id : string] : TRankingInfo read GetRanking;
        public
          property Cookie[id : string] : string read GetCookie write SetCookie;
          property Cookies : string read GetCookies;
        private
          function GetSecurityId : TSecurityId;
        public
          property AutoConnections : TLockableCollection read fAutoConnections;
          property Curriculum      : TLockableCollection read fCurriculum;
          property WorldLocator    : IWorldLocator       read fWorldLocator write fWorldLocator;
          property MailServer      : IMailServer         read fMailServer   write fMailServer;
          property SecurityId      : TSecurityId         read GetSecurityId;
        private
          fTimesAwaken : integer;
        public
          procedure Awake;
          procedure Sleep;
          function  IsOnline : boolean;
          procedure SendNotification( Kind : integer; Title, Body : string; Options : integer ); virtual;
          function  CountItemsInCurriculum( Kind : integer ) : integer;
          function  FindResearchItemsInCurriculum : boolean;
          procedure RecordEvent( Event : TEvent );
          function  PickEvent : TEvent;
        private
          fPolicyModified : boolean;
        private
          function  GetPolicyStatus( Tycoon : TTycoon ) : TPolicyStatus;
          procedure SetPolicyStatus( Tycoon : TTycoon; Status : TPolicyStatus );
        public
          property Policy[Tycoon : TTycoon] : TPolicyStatus read GetPolicyStatus write SetPolicyStatus;
          property PolicyModified : boolean read fPolicyModified write fPolicyModified;
        public
          procedure StorePoliciesToCache( Cache : TObjectCache ); virtual;
          procedure StoreRoleInfoToCache( Cache : TObjectCache ); virtual;
        public
          procedure AssumeRole ( Role : TTycoon );
          procedure AbandomRole( Role : TTycoon );
          procedure AbandomRoles;
          function  ContainsRole( RoleId : TTycoonId ) : boolean;
        public
          procedure AddItemToCurriculum( Item : TCurriculumItem );
        published
          function RDOAskLoan( AmountStr : widestring ) : OleVariant;
          function RDOPayLoan( AmountStr : widestring ) : OleVariant;
          function RDOSendMoney( ToTycoon, Reason, AmountStr : widestring ) : OleVariant;
          function RDOPayOff(index : integer) : OleVariant;
        private
          procedure UpdateAutoConnections;
          function  FindAutoConnection( FluidId : string ) : TAutoConnection;
          procedure ModifyAutoConnection( FluidId, Suppliers : widestring; add : boolean );
        published
          procedure RDOAddAutoConnection( FluidId, Suppliers : widestring );
          procedure RDODelAutoConnection( FluidId, Suppliers : widestring );
          procedure RDOHireTradeCenter( FluidId : widestring );
          procedure RDODontHireTradeCenter( FluidId : widestring );
          procedure RDOHireOnlyFromWarehouse( FluidId : widestring );
          procedure RDODontHireOnlyFromWarehouse( FluidId : widestring );
          procedure RDOSetPolicyStatus( ToTycoon : widestring; Status : integer );
          procedure RDOSetAdvanceToNextLevel( yes : integer );
          procedure RDODelCurItem(index : integer);
          procedure RDOSetCookie(cName, cValue : widestring);
        published // Favorites
          function RDOFavoritesNewItem    ( Location : widestring; Kind : integer; Name, Info : widestring ) : OleVariant;
          function RDOFavoritesDelItem    ( Location : widestring ) : OleVariant;
          function RDOFavoritesMoveItem   ( ItemLoc : widestring; Dest : widestring ) : OleVariant;
          function RDOFavoritesRenameItem ( ItemLoc : widestring; Name : widestring ) : OleVariant;
          function RDOFavoritesGetSubItems( ItemLoc : widestring ) : OleVariant;
        private
          fYearProfitPerHour : TMoney;
        public
          procedure ComputeLevelingData;
          procedure CheckForNextLevel;
          procedure CheckLosingLevel;
        public
          procedure GenMoney( Money : TMoney; Reason : TMoneyReason ); override;
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); override;
        public
          procedure FacilityDestroyed  ( Facility : TFacility );
          procedure NewOwnedFacility   ( Facility : TFacility );
          procedure RegisterSupplier   ( Facility : TFacility );
          procedure UnregisterSupplier ( Facility : TFacility );
          procedure AutoConnectFacility( Facility : TFacility ); virtual;
          procedure TycoonDeleted( Tycoon : TTycoon ); virtual;
          procedure PublishCurriculum( AllItems : boolean );
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure Loaded; override;
          procedure UpdateParameters; virtual;
          procedure InitRankings;
          procedure UpdateRankings;
        public
          function GetCacheName : string;
        public
          procedure ResetTutorial;
          procedure CancelTutorial;
        private
          procedure InstantiateTutorial;
        private
          fTutorial    : TTask;
          fTaskContext : TTycoonContext;
        public
          property Tutorial : TTask read fTutorial write fTutorial;
        private
          fLevel              : TTycoonLevel;
          fAdvanceToNextLevel : boolean;
          fLevelReqStatus     : TMultiString;
        public
          property Level              : TTycoonLevel read fLevel write fLevel;
          property LevelReqStatus     : TMultiString read fLevelReqStatus;
          property AdvanceToNextLevel : boolean      read fAdvanceToNextLevel;
          property YearProfitPerHour  : TMoney       read fYearProfitPerHour;
        public
          procedure ResetLevel;
        published
          procedure RDOActivateTutorial(Value : LongBool);
          function  RDOActiveTutorial(useless : integer) : OleVariant;
          procedure RDOKillTutorial;
        public
          procedure DefaultHandler(var Message); override;
        private
          procedure CountFacilities;
          procedure CountFacility(Facility : TFacility; created : boolean);
          function  ReachedBuildLimit : boolean;
        public
          procedure RemoveFacilityLink(Facility : TFacility);
          function  CheckOpAuthenticity : boolean;
          function  UpdateNobility : boolean;
          function  CanAdvanceLevel : boolean;
        private
          function GetCanBuildAdvanced : boolean;
        published
          property CanBuildAdvanced : boolean read GetCanBuildAdvanced;
        private
          fVotes   : TVoteSystem;
          fInTowns : TCollection;
        public
          property Votes : TVoteSystem read fVotes;
        public
          procedure ClearVotes;
          function  PaysTaxesInTown(Town : TTown) : boolean;
          procedure PaidTaxesInTown(Town : TTown);
          function  GetPaysTaxes : boolean;
        public
          function CanBuildRoad(tiles : integer) : boolean;
          function CanGetLoan(amount : TMoney) : boolean;
          property PayTaxes : boolean read GetPaysTaxes;
        protected
          function GetLoanLimit : TMoney; override;
        public
          function HasLegacy : boolean;
        private
          fTournamentOn : boolean;
        public
          property TournamentOn : boolean read fTournamentOn;
      end;

    TTycoonContext =
      class(TPersistent, ITaskContext)
        public
          constructor Create(aTycoon : TTycoon; aCompany : TCompany; aTown : TTown);
        private
          fTycoon  : TTycoon;
          fCompany : TCompany;
          fTown    : TTown;
        private
          function  QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function  _AddRef: integer; stdcall;
          function  _Release: integer; stdcall;
          function  getContext(id : integer) : TObject;
          procedure setContext(id : integer; Obj : TObject);
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    {$M-}

  const
    tidCookie_LastTimeOnline     = 'LastTimeOnline';
    tidCookie_LastVirtTimeOnline = 'LastVirtTimeOnline';

  const
    accIdx_LandTax = 431;

  // Misc routines

  //function GetInventionId( numId : TInventionNumId; kind : string ) : string;

  // Model events info

  type
    TMsgNewBlock =
      record
        Msg   : word;
        Block : TBlock;
      end;

    TMsgNewFacility =
      record
        Msg      : word;
        Facility : TFacility;
      end;

    TMsg_NewCompany =
      record
        Msg     : word;
        Company : TCompany;
      end;

    TMsgNewTycoon =
      record
        Msg    : word;
        Tycoon : TTycoon;
      end;

    TMsgBlockDeleted =
      record
        Msg   : word;
        Block : TBlock;
      end;

    TMsgFacilityDeleted =
      record
        Msg      : word;
        Facility : TFacility;
      end;

    TMsgCompanyDeleted =
      record
        Msg     : word;
        Company : TCompany;
      end;

    TMsgTycoonDeleted =
      record
        Msg    : word;
        Tycoon : TTycoon;
      end;

    TMsgFacCloned =
      record
        Msg : word;
        Fac : TFacility;
      end;

    TMsgFacUpgraded =
      record
        Msg : word;
        Fac : TFacility;
      end;

    TMsgSellToAll =
      record
        Msg      : word;
        Fac      : TFacility;
        FacTypes : integer;
      end;

    TMsgFacsConnect =
      record
        Msg  : word;
        Fac1 : TFacility;
        Fac2 : TFacility;
      end;

    TMsgFacHireOffer =
      record
        Msg  : word;
        Fac  : TFacility;
        Hire : boolean;
      end;

  const
    tidRankind_Main = 'NTA';

  // Quick connect facility ids

  const
    ftpWarehouses = $01;
    ftpFactories  = $02;
    ftpStores     = $04;


  // Model Extensions

  const
    tidProcName_GetMDXId          = 'ModelExtensionId';
    tidProcName_GetMDXDependances = 'GetDependances';
    tidProcName_RegisterMDX       = 'RegisterModelExtension';
    tidProcName_PostRegisterMDX   = 'PostRegisterModelExtension';

  type
    PGetMDXIdProc = ^TGetMDXIdProc;
    TGetMDXIdProc = function : string;

    PGetMDXDependancesProc = ^TGetMDXDependancesProc;
    TGetMDXDependancesProc = function : string;

    PRegisterMDXProc = ^TRegisterMDXProc;
    TRegisterMDXProc = procedure;

    PPostRegisterMDXProc = ^TPostRegisterMDXProc;
    TPostRegisterMDXProc = procedure;

  function  GetMDXId( MDX : THandle ) : string;
  function  GetMDXDependances( MDX : THandle ) : TStringList;
  procedure RegisterMDX( MDX : THandle );
  procedure PostRegisterMDX( MDX : THandle );


  // Registration of basic stuff

  procedure RegisterBackup;
  procedure RegisterSurfaces;
  procedure RegisterTownParameters;
  procedure UpdateLandSurfaces( Land : ILandInfo );


  // Visual Classes

  var
    VisualClasses : TClassManager = nil;

  procedure InitVisualClasses;
  procedure DoneVisualClasses;

  const
    tidLog_Survival = 'Survival';
    tidLog_TimeWarp = 'TimeWarp';

  // Config Ids

  const
    CFGID_AccountExpires = 10;
    CFGID_LevelLimit     = 11;

  // Profile Ids

  const
    prfKind_Sim              = 0;
    prfId_CollectInputs      = 1;
    prfId_SpreadOutputs      = 2;
    prfId_CollectInputExtra  = 3;
    prfId_SpreadOutputExtra  = 4;
    prfId_Sim                = 5;

    prfKind_InputCollect    = 1;
    prfId_CollectLoopInit   = 0;
    prfId_dQHandling        = 1;
    prfId_CollectFinalizing = 2;

    prfKind_Int           = 2;
    prfId_UpdateModifiers = 0;
    prfId_Integrate       = 1;


  function GetTaxableAccount( BaseAccount : TAccountId ) : string;
  function GetLevelOfTier( Tier : integer ) : TTycoonLevel;

  function CheckAuthenticity(Tycoon : TTycoon) : boolean;

  const
    mtidPeopleKindName    : array[TPeopleKind] of TRegMultiString = (nil, nil, nil);
    mtidWorkforceKindName : array[TPeopleKind] of TRegMultiString = (nil, nil, nil);

implementation

  uses                                                                                     
    ClassStorage, KernelCache, Construction, ModelServerCache, MathUtils,
    StrUtils, BasicAccounts, BasicTaxes, SysUtils, Population, SimHints,
    SpecialChars, LandSurfaces, VisualClassesData, BasicCurriculum,
    Politics, TownPolitics, Logs, Profiler, TycoonLevels, Math, StdCurriculum,
    MediaNameGenerator, FavProtocol, Standards;

  const
    defCluster = 'General';

  const
    CnxPermissionMap : array[0..1, TFacilityRole, TFacilityRole] of integer =
      // Newbies
      // rolNeutral, rolProducer, rolDistributer, rolBuyer, rolImporter, rolCompExport, rolCompImport
      ( ( (1,          1,           1,              1,        1,           2,             2),  // rolNeutral
          (1,          1,           1,              1,        1,           2,             2),  // rolProducer
          (1,          1,           0,              1,        0,           2,             2),  // rolDistributer
          (1,          1,           1,              1,        1,           2,             2),  // rolBuyer
          (1,          1,           0,              1,        1,           0,             0),  // rolImporter (Trade Centers)
          (2,          2,           2,              2,        0,           2,             2),  // rolCompExport
          (2,          2,           2,              2,        0,           2,             2)), // rolCompImport
      // Tycoons
        ( (1,          1,           1,              1,        0,           2,             2),  // rolNeutral
          (1,          1,           1,              1,        0,           2,             2),  // rolProducer
          (1,          1,           0,              1,        0,           2,             2),  // rolDistributer
          (1,          1,           1,              1,        0,           2,             2),  // rolBuyer
          (0,          0,           0,              0,        0,           0,             0),  // rolImporter
          (2,          2,           2,              2,        0,           2,             2),  // rolCompExport
          (2,          2,           2,              2,        0,           2,             2)));// rolCompImport

  const
    tidMailMsgURL_MoneySentNotification = 'MoneySendNotification.asp';

    mtidBlockPrestigePts         : TRegMultiString = nil;
    mtidStoppedBy                : TRegMultiString = nil;
    mtidStoppedNoCnxs            : TRegMultiString = nil;
    mtidStoppedMoney             : TRegMultiString = nil;
    mtidCnxHired                 : TRegMultiString = nil;
    mtidRejCompPol               : TRegMultiString = nil;
    mtidAlreadyHired             : TRegMultiString = nil;
    mtidCnxNotAllowed            : TRegMultiString = nil;
    mtidCnxLimited               : TRegMultiString = nil;
    mtidCnxRepInputHead          : TRegMultiString = nil;
    mtidCnxRepOutputHead         : TRegMultiString = nil;
    mtidCnxHeader                : TRegMultiString = nil;
    mtidMsgLoan                  : TRegMultiString = nil;
    mtidMayorTitle               : TRegMultiString = nil;
    mtidMayorEmail               : TRegMultiString = nil;
    mtidMsgMainResearchCompleted : TRegMultiString = nil;
    mtidMsgResearchCompleted     : TRegMultiString = nil;
    mtidMsgMainResearchSold      : TRegMultiString = nil;
    mtidMsgResearchSold          : TRegMultiString = nil;
    mtidMsgMoneyTransfer         : TRegMultiString = nil;
    mtidMsgPolicySetToEnemy      : TRegMultiString = nil;
    mtidMsgPolicySetToNeutral    : TRegMultiString = nil;
    mtidMsgPolicySetToAlly       : TRegMultiString = nil;
    mtidMsgWantLevelUpgrade      : TRegMultiString = nil;
    mtidMsgLevelAdvanced         : TRegMultiString = nil;
    mtidMsgLevelAdvFailed        : TRegMultiString = nil;
    mtidMsgBankruptDanger        : TRegMultiString = nil;
    mtidMsgHappyNewYear          : TRegMultiString = nil;
    mtidMsgLevelLost             : TRegMultiString = nil;

  const
    ERROR_HTML_InvalidGateName = 'Invalid name';
    ERROR_HTML_UnknownError    = 'Unknown error';

  function FluidData( Q : TFluidValue; K : TQuality ) : TFluidData;
    begin
      result.Q := Q;
      result.K := K;
    end;

  function InputData( Q : TFluidValue; K : TQuality ) : TInputData;
    begin
      result.Q := Q;
      result.K := K;
    end;

  function PushInputData( Q : TFluidValue; K : TQuality; S : TAdmitance ) : TPushInputData;
    begin
      result.Q := Q;
      result.K := K;
      result.S := S;
    end;

  function PullInputData( Q : TFluidValue; K : TQuality ) : TPullInputData;
    begin
      result.Q := Q;
      result.K := K;
    end;

  function OutputData( Q : TFluidValue; K : TQuality ) : TOutputData;
    begin
      result.Q := Q;
      result.K := K;
      result.Extra.Q := 0;
      result.Extra.K := 0;
    end;

  procedure LoadFluidData( name : string; var FluidData; Reader : IBackupReader );
    var
      absFluidData : TFluidData absolute FluidData;
    begin
      absFluidData.Q := Reader.ReadSingle( name + '.Q', 0 );
      absFluidData.K := Reader.ReadInteger( name + '.K', 0 );
    end;

  procedure StoreFluidData( name : string; var FluidData; Writer : IBackupWriter );
    var
      absFluidData : TFluidData absolute FluidData;
      aux          : string;
    begin
      aux := name + '.Q';
      Writer.WriteSingle( aux, absFluidData.Q );
      aux := name + '.K';
      Writer.WriteInteger( aux, absFluidData.K );
      aux := '';
    end;

  procedure LoadInputData( name : string; var InputData; Reader : IBackupReader );
    begin
      LoadFluidData( name, InputData, Reader );
    end;

  procedure StoreInputData( name : string; var InputData; Writer : IBackupWriter );
    begin
      StoreFluidData( name, InputData, Writer );
    end;

  procedure LoadPushInputData( name : string; var InputData; Reader : IBackupReader );
    var
      absInputData : TPushInputData absolute InputData;
    begin
      LoadInputData( name, InputData, Reader );
      absInputData.S := Reader.ReadInteger( name + '.S', 0 );
    end;

  procedure StorePushInputData( name : string; var InputData; Writer : IBackupWriter );
    var
      absInputData : TPushInputData absolute InputData;
    begin
      StoreInputData( name, InputData, Writer );
      Writer.WriteInteger( name + '.S', absInputData.S );
    end;

  procedure LoadPullInputData( name : string; var InputData; Reader : IBackupReader );
    begin
      LoadInputData( name, InputData, Reader );
    end;

  procedure StorePullInputData( name : string; var InputData; Writer : IBackupWriter );
    begin
      StoreInputData( name, InputData, Writer );
    end;

  procedure LoadOutputData( name : string; var OutputData; Reader : IBackupReader );
    var
      absOutputData : TOutputData absolute OutputData;
    begin
      LoadFluidData( name, OutputData, Reader );
      LoadFluidData( name + '.Extra', absOutputData.Extra, Reader );
    end;

  procedure StoreOutputData( name : string; var OutputData; Writer : IBackupWriter );
    var
      absOutputData : TOutputData absolute OutputData;
    begin
      StoreFluidData( name, OutputData, Writer );
      StoreFluidData( name + '.Extra', absOutputData.Extra, Writer );
    end;

  function AverageK( F1, F2 : PFluidData ) : TQuality;
    begin
      if F1.Q + F2.Q > 0
        then result := round((F1.Q*F1.K + F2.Q*F2.K)/(F1.Q + F2.Q))
        else result := 0;
    end;

  function IntegrateValues(Value1, Value2, Weight1, Weight2 : single) : single;
    begin
      result := (Weight1*Value1 + Weight2*Value2)/(Weight1 + Weight2);
    end;

  const
    MaxNeighborBlocks = 2;

  function CloseEnought(Block1, Block2 : TBlock) : boolean;
    var
      x1, y1 : integer;
      x2, y2 : integer;
      r1, r2 : TRect;
      dest   : TRect;
    begin
      try
        x1 := Block1.Facility.xPos + Block1.Facility.MetaFacility.XSize div 2;
        y1 := Block1.Facility.yPos + Block1.Facility.MetaFacility.YSize div 2;
        x2 := Block2.Facility.xPos + Block2.Facility.MetaFacility.XSize div 2;
        y2 := Block2.Facility.yPos + Block2.Facility.MetaFacility.YSize div 2;
        r1 := Rect(x1, y1, x1 + Block1.Facility.MetaFacility.XSize, y1 + Block1.Facility.MetaFacility.YSize);
        r2 := Rect(x2, y2, x2 + Block2.Facility.MetaFacility.XSize, y2 + Block2.Facility.MetaFacility.YSize);
        InflateRect(r1, MaxNeighborBlocks, MaxNeighborBlocks);
        InflateRect(r2, MaxNeighborBlocks, MaxNeighborBlocks);
        result := IntersectRect(dest, r1, r2);
      except
        result := false;
      end;
    end;

  // TFacilityKind

  constructor TFacilityKind.Create( anId : string );
    begin
      inherited Create( anId );
      Cacheable := true;
      fName_MLS := TMultiString.Create;
    end;

  procedure TFacilityKind.RetrieveTexts( Container : TDictionary );
    begin
      if fName_MLS = nil
        then fName_MLS := TMultiString.Create;
      fName_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Name'];
    end;

  procedure TFacilityKind.StoreTexts( Container : TDictionary );
    begin
      Container.Values[Family + '.' + Id + '.' + 'Name'] := fName;
    end;


  // TMetaFacility

  constructor TMetaFacility.Create( anId, aName : string; aVisualClass : TVisualClassId; aFacilityClass : CFacility );
    var
      VisualClass : TVisualClass;
    begin
      inherited Create( anId );
      fName          := aName;
      fFacilityClass := aFacilityClass;
      fEvlStages     := TNotifiedCollection.Create( 0, rkBelonguer );
      fInTown        := true;
      fOptions       := [mfcInTown, mfcGenerateName, mfcShowCompanyInText, mfcShowProfitInText];
      fVisualClass   := aVisualClass;
      try
        VisualClass    := VisualClasses.ClassById[aVisualClass];
        if VisualClass <> nil
          then fZoneType := VisualClass.ReadInteger( 'General', 'Zone', znNone )
          else fZoneType := znNone;
      except
        fZoneType := znNone;
      end;
      TNotifiedCollection(fEvlStages).OnModified := EvlStagesModified;
      Cacheable := true;
      fPluralName := fName + 's';
      fName_MLS := TMultiString.Create;
      fDesc_MLS := TMultiString.Create;
      fRequires_MLS := TMultiString.Create;
      fPluralName_MLS := TMultiString.Create;
      fNeedsBudget := true;
      fSlotCount := 1;
      fDemoLapse := DemolitionLapse;
      fDepOnTech := true;
      fReqCmpSupplies := true;
    end;

  constructor TMetaFacility.CopyFrom( aMetaFacility : TMetaFacility; NewId : string );
    begin
      inherited Create( NewId );
      Cacheable       := aMetaFacility.Cacheable;
      fName           := aMetaFacility.fName;
      fDesc           := aMetaFacility.fDesc;
      fRequires       := aMetaFacility.fRequires;
      fEvlStages      := aMetaFacility.fEvlStages;
      fFacilityClass  := aMetaFacility.fFacilityClass;
      fFacilityKind   := aMetaFacility.fFacilityKind;
      fTypicalStage   := aMetaFacility.fTypicalStage;
      fLevel          := aMetaFacility.fLevel;
      fXSize          := aMetaFacility.fXSize;
      fYSize          := aMetaFacility.fYSize;
      fInTown         := aMetaFacility.fInTown;
      fCluster        := aMetaFacility.fCluster;
      fOptions        := aMetaFacility.fOptions;
      fVisualClass    := aMetaFacility.fVisualClass;
      fTechnologyKind := aMetaFacility.fTechnologyKind;
      fZoneType       := aMetaFacility.fZoneType;
      fFacId          := aMetaFacility.fFacId;
      fMinistryId     := aMetaFacility.fMinistryId;
      fPluralName     := aMetaFacility.fPluralName;
      fName_MLS       := CloneMultiString( aMetaFacility.Name_MLS );
      fDesc_MLS       := CloneMultiString( aMetaFacility.fDesc_MLS );
      fRequires_MLS   := CloneMultiString( aMetaFacility.fRequires_MLS );
      fPluralName_MLS := CloneMultiString( aMetaFacility.fPluralName_MLS );
      fCloneSource    := aMetaFacility;
      fNeedsBudget    := aMetaFacility.fNeedsBudget;
      fSlotCount      := aMetaFacility.fSlotCount;
      fDemoLapse      := aMetaFacility.fDemoLapse;
      fDepOnTech      := aMetaFacility.fDepOnTech;
      fReqCmpSupplies := aMetaFacility.ReqCmpSupplies;
    end;

  destructor TMetaFacility.Destroy;
    begin
      fEvlStages.Free;
      inherited;
    end;

  function TMetaFacility.EstimatePrice : TMoney;
    begin
      if fEstimPrice = 0
        then
          if (EvlStages.Count > 1) and (TEvlStage(EvlStages[0]).MetaBlock.ClassName = TMetaBlockUnderConstruction.ClassName)
            then fEstimPrice := TMetaBlockUnderConstruction(TEvlStage(EvlStages[0]).MetaBlock).EstimatedPrice
            else fEstimPrice := 0;
      result := fEstimPrice;
    end;

  function TMetaFacility.GetClusterName : string;
    begin
      if fCluster <> nil
        then result := fCluster.Id
        else result := tidCluster_Undefined;
    end;

  procedure TMetaFacility.SetClusterName( aName : string );
    begin
      fCluster := TCluster(TheClassStorage.ClassById[tidClassFamily_Clusters, aName]);
    end;

  function TMetaFacility.GetFacilityKind : string;
    begin
      if fFacilityKind <> nil
        then result := fFacilityKind.Id
        else result := tidFacilityKind_Undefined;
    end;

  procedure TMetaFacility.SetFacilityKind( aName : string );
    begin
      if aName <> ''
        then fFacilityKind := TFacilityKind(TheClassStorage.ClassById[tidClassFamily_FacilityKinds, aName])
        else fFacilityKind := nil
    end;

  function TMetaFacility.Instantiate : TFacility;
    begin
      result := fFacilityClass.Create( self );
    end;

  procedure TMetaFacility.EvlStagesModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
    begin
      TEvlStage(Item).fMetaFacility := self;
      TypicalStage := TEvlStage(Item);
    end;

  procedure TMetaFacility.Register( ClassFamily : TClassFamilyId );

    procedure ApplyStageModifiers;
      var
        i, j : integer;
      begin
        for i := 0 to pred(EvlStages.Count) do
          begin
            j := 0;
            while (j < i) and not TEvlStage(EvlStages[i]).MetaBlock.ModifyStageStack( TEvlStage(EvlStages[j]).MetaBlock ) do
              inc( j );
            TEvlStage(EvlStages[i]).MetaBlock.ModifyMetaFacility( self );
          end;
      end;

    procedure CheckSize;
      var
        VisualClass : TVisualClass;
        xSz, ySz    : integer;
      begin
        VisualClass := VisualClasses.ClassById[fVisualClass];
        if VisualClass <> nil
          then
            begin
              xSz := VisualClass.ReadInteger('General', 'xSize', 0);
              ySz := VisualClass.ReadInteger('General', 'ySize', 0);
              if (xSz <> xSize) or (ySz <> ySize)
                then Logs.Log('Classes', Format('MetaFacility %s (%d, %d) VisualClass (%d, %d) ID: %d Image: %s', [Name, xSize, ySize, xSz, ySz, fVisualClass, VisualClass.ReadString('MapImages', '64x32x0', '?')]));
            end;
      end;

    begin
      //CheckSize;
      ApplyStageModifiers;
      inherited Register( ClassFamily );
    end;

  procedure TMetaFacility.RequiresInvention(Invention : TInvention);
    {var
      i   : integer;
      lid : string;}
    begin
      fTechnology := Invention;
      {for i := 0 to pred(LangList.Count) do
        begin
          lid := LangList[i];
          //fRequires_MLS.Values[lid] := SimHints.GetHintText(mtidTechRequired.Values[lid], [Invention.Name_MLS.Values[lid], Invention.Resp_MLS.Values[lid]]);
          fRequires_MLS.Values[lid] := SimHints.GetHintText(mtidTechRequired.Values[lid], [Invention.Name, Invention.Resp]);
        end;  }
    end;

  procedure TMetaFacility.RetrieveTexts( Container : TDictionary );
    begin
      inherited;

      // Create multi strings
      if fName_MLS = nil
        then fName_MLS := TMultiString.Create;
      if fDesc_MLS = nil
        then fDesc_MLS := TMultiString.Create;
      if fRequires_MLS = nil
        then fRequires_MLS := TMultiString.Create;
      if fPluralName_MLS = nil
        then fPluralName_MLS := TMultiString.Create;

      // Assign multi strings
      if fCloneSource = nil
        then
          begin
            fName_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Name'];
            fPluralName_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'PluralName'];
          end
        {else
          begin
            fName_MLS       := CloneMultiString( fCloneSource.Name_MLS );
            fDesc_MLS       := CloneMultiString( fCloneSource.fDesc_MLS );
            fRequires_MLS   := CloneMultiString( fCloneSource.fRequires_MLS );
            fPluralName_MLS := CloneMultiString( fCloneSource.fPluralName_MLS );
          end;}
    end;

  procedure TMetaFacility.StoreTexts( Container : TDictionary );
    begin
      inherited;
      if fCloneSource = nil
        then
          begin
            {$IFNDEF MLSStoreOnlyMissingTexts}
            Container.Values[Family + '.' + Id + '.' + 'Name'] := fName;
            Container.Values[Family + '.' + Id + '.' + 'PluralName'] := fPluralName;
            {$ELSE}
            if fName_MLS.Values[Container.LangId] = ''
              then Container.Values[Family + '.' + Id + '.' + 'Name'] := fName;
            if fPluralName_MLS.Values[Container.LangId] = ''
              then Container.Values[Family + '.' + Id + '.' + 'PluralName'] := fPluralName;
            {$ENDIF}
          end;
    end;

  procedure TMetaFacility.EvaluateTexts;
    var
      i   : integer;
      aux : string;
      lid : string;
    begin
      inherited;
      for i := 0 to pred(LangList.Count) do
        begin
          lid := LangList[i];
          if fTechnology <> nil
            then aux := SimHints.GetHintText(mtidTechRequired.Values[lid], [fTechnology.Name_MLS.Values[lid], fTechnology.Resp_MLS.Values[lid]])
            else aux := '';
          Requires_MLS.Values[lid] := aux;
          Desc_MLS.Values[lid] := TypicalStage.MetaBlock.Desc_MLS.Values[LangList[i]];
        end;
    end;

  procedure TMetaFacility.CloneTexts;
    begin
      if fCloneSource <> nil
        then
          begin
            fName_MLS.Free;
            fName_MLS := CloneMultiString( fCloneSource.fName_MLS );
            fDesc_MLS.Free;
            fDesc_MLS := CloneMultiString( fCloneSource.fDesc_MLS );
            fRequires_MLS.Free;
            fRequires_MLS := CloneMultiString( fCloneSource.fRequires_MLS );
            fPluralName_MLS.Free;
            fPluralName_MLS := CloneMultiString( fCloneSource.fPluralName_MLS );
          end;
    end;

  function TMetaFacility.GetConstructionTime : integer;
    var
      MetaBlock : TMetaBlock;
    begin
      if fConstTime > 0
        then result := fConstTime
        else
          if fEvlStages.Count > 0
            then
              begin
                MetaBlock := TEvlStage(fEvlStages[0]).MetaBlock;
                if ObjectIs('TMetaBlockUnderConstruction', MetaBlock)
                  then fConstTime := TMetaBlockUnderConstruction(MetaBlock).ConstTime;
                result := fConstTime;
              end
            else result := 0;
    end;

  function TMetaFacility.GetUpgradeTime : integer;
    begin
      result := round(ConstructionTime*0.50); // >> 50% of the construction time
    end;

  function TMetaFacility.GetUpgradeCost : TMoney;
    begin
      result := EstimatePrice;
    end;

  function TMetaFacility.GetUpgradeHourCost : TMoney;
    begin
      if GetUpgradeTime > 0
        then result := GetUpgradeCost/GetUpgradeTime
        else result := 0;
    end;


  // TEvlStage

  constructor TEvlStage.Create( anId, aName, aDescription : string; aMetaBlock : TMetaBlock );
    begin
      inherited Create;
      fId          := anId;
      fName        := aName;
      fDescription := aDescription;
      fMetaBlock   := aMetablock;
    end;

  function TEvlStage.GetIndex : integer;
    begin
      result := fMetaFacility.fEvlStages.IndexOf( self );
    end;


  // TMetaCompanyInput

  constructor TMetaCompanyInput.Create(aFluid : TMetaFluid; aMax : TFluidValue; CanEdit : boolean);
    begin
      inherited Create;
      fFluid    := aFluid;
      fMax      := aMax;
      fEditable := CanEdit;
    end;

  // TMetaBlock

  constructor TMetaBlock.Create( anId           : string;
                                 aSupplyAccount : TAccountId;
                                 aProdAccount   : TAccountId;
                                 aBlockClass    : CBlock );
    begin
      inherited Create( anId );
      fBlockClass  := aBlockClass;
      fMetaInputs  := TNotifiedCollection.Create( 0, rkBelonguer );
      fMetaOutputs := TNotifiedCollection.Create( 0, rkBelonguer );
      fMetaInputs.OnModified  := OnMetaInputsModified;
      fMetaOutputs.OnModified := OnMetaOutputsModified;
      fVisualStages := 1;
      fPrestige := 0.3;
      fBeautyStrength := 10;
      fSupplyAccount := aSupplyAccount;
      fProdAccount := aProdAccount;
      fInventions       := TCollection.Create( 0, rkUse );
      fCompanyInputs    := TCollection.Create( 0, rkUse );
      fWeatherEnvelopes := TCollection.Create( 0, rkUse );
      fDesc_MLS         := TMultiString.Create;
      fCnntRepairable   := true;
      fMaxUpgrade       := 1;
      fMinColDist       := 0;
      fColIsSameComp    := true;
      fUsagePerInv      := 1;
      fManyUpgrades := true;
    end;

  destructor TMetaBlock.Destroy;
    begin
      fWeatherEnvelopes.Free;
      fCompanyInputs.Free;
      fMetaInputs.Free;
      fMetaOutputs.Free;
      fInventions.Free;
      inherited;
    end;

  function TMetaBlock.Transcends(Facility : TFacility) : boolean;
    begin
      result := false;
    end;

  function TMetaBlock.GetMetaInputIndex( name : string ) : integer;
    begin
      result := pred(fMetaInputs.Count);
      while (result >= 0) and (TMetaInput(fMetaInputs[result]).Name <> name) do
        dec( result );
    end;

  function TMetaBlock.GetMetaOutputIndex ( name : string ) : integer;
    begin
      result := pred(fMetaOutputs.Count);
      while (result >= 0) and (TMetaOutput(fMetaOutputs[result]).Name <> name) do
        dec( result );
    end;

  function TMetaBlock.GetMetaInputByName( name : string ) : TMetaInput;
    var
      idx : integer;
    begin
      idx := InputIndex[name];
      if idx <> NoIndex
        then result := TMetaInput(fMetaInputs[idx])
        else result := nil;
    end;

  function TMetaBlock.GetMetaOutputByName( name : string ) : TMetaOutput;
    var
      idx : integer;
    begin
      idx := OutputIndex[name];
      if idx <> NoIndex
        then result := TMetaOutput(fMetaOutputs[idx])
        else result := nil;
    end;

  function TMetaBlock.Instantiate( aFacility : TFacility ) : TBlock;
    begin
      result := fBlockClass.Create( self, aFacility );
    end;

  procedure TMetaBlock.OnMetaInputsModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
    begin
      if Operation = opInsertion
        then TMetaInput(Item).fIndex := Index;
    end;

  procedure TMetaBlock.OnMetaOutputsModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
    begin
      if Operation = opInsertion
        then TMetaOutput(Item).fIndex := Index;
    end;

  function TMetaBlock.ModifyStageStack( Stage : TMetaBlock ) : boolean;
    begin
      result := true;
    end;

  procedure TMetaBlock.ModifyMetaFacility( MetaFacility : TMetaFacility );
    begin
    end;

  function TMetaBlock.RegisterWeatherEnvelope( Kind : string ) : integer;
    var
      WE : TWeatherEnvelope;
    begin
      WE := TWeatherEnvelope(TheClassStorage.ClassById[tidClassFamily_WeatherEnvelopes, Kind]);
      if WE <> nil
        then
          begin
            result := fWeatherEnvelopes.Count;
            fWeatherEnvelopes.Insert( WE );
          end
        else result := noIndex;
    end;

  function TMetaBlock.WeatherEnvelopeIndex( Kind : string ) : integer;
    begin
      result := pred(fWeatherEnvelopes.Count);
      while (result >= 0) and (TWeatherEnvelope(fWeatherEnvelopes[result]).Id <> Kind) do
        dec(result);
    end;

  procedure TMetaBlock.RegisterCompanyInput( Kind : string; aMax : TFluidValue; Editable : boolean );
    var
      MF : TMetaFluid;
    begin
      MF := TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, Kind]);
      if MF <> nil
        then fCompanyInputs.Insert(TMetaCompanyInput.Create(MF, aMax, Editable));
    end;

  procedure TMetaBlock.Register( ClassFamily : TClassFamilyId );
    begin
      inherited Register( ClassFamily );
    end;

  procedure TMetaBlock.DeclareInvention(Invention : TInvention);
    begin
      fInventions.Insert(Invention);
      if Invention <> nil
        then Invention.Implementable := true;
    end;

  function TMetaBlock.UsesInvention(Invention : TInvention) : boolean;
    begin
      result := fInventions.IndexOf(Invention) <> noIndex;
    end;

  procedure TMetaBlock.RetrieveTexts( Container : TDictionary );
    begin
      inherited;
    end;

  procedure TMetaBlock.StoreTexts( Container : TDictionary );
    begin
      inherited;
    end;

  procedure TMetaBlock.EvaluateTexts;
    var
      i : integer;
    begin
      inherited;
      if round(Prestige) > 1
        then
          begin
            if Desc <> ''
              then Desc := Desc + ' ';
            Desc := Desc + IntToStr(round(Prestige)) + ' prestige points.';
            if fDesc_MLS = nil
              then fDesc_MLS := TMultiString.Create;
            for i := 0 to pred(LangList.Count) do
              try
                fDesc_MLS.Add( Format( mtidBlockPrestigePts[i], [round(Prestige)] ));
              except
              end;
          end;
    end;

  procedure TMetaBlock.StoreExtraInfoToCache( Cache : TObjectCache );
    begin
    end;


  // TMetaFluid

  constructor TMetaFluid.Create( anId,
                                 aName,
                                 aDescription : string;
                                 aUnitName    : string;
                                 aFluidName   : string;
                                 aFluidFact   : TFluidValue;
                                 aTransCost   : TMoney;
                                 aWeight      : TWeight;
                                 aMarketPrice : TMoney;
                                 anOutputCls  : COutput );
    begin
      inherited Create( anId );
      fName        := aName;
      fUnitName    := aUnitName;
      fFluidName   := aFluidName;
      fFluidFact   := aFluidFact;
      fTransCost   := aTransCost; // put zero to avoid it..
      fWeight      := aWeight;
      fMarketPrice := aMarketPrice;
      fCnxLimit    := 20;
      fName_MLS := TMultiString.Create;
      fDescription_MLS := TMultiString.Create;
      fUnitName_MLS := TMultiString.Create;
      fFluidName_MLS := TMultiString.Create;
      fOutputClass := anOutputCls;
    end;

  function TMetaFluid.FormatValue( Value : TFluidValue; LangId : TLanguageId ) : string;
    begin
      try
        result := Format( '%d', [ceil(Value*fFluidFact)] ) + ' ' + fFluidName_MLS.Values[LangId];
      except
        result := 'Unknown';
      end;
    end;

  function TMetaFluid.FormatSingleValue( Value : TFluidValue; LangId : TLanguageId ) : string;
    begin
      try
        result := Format( '%d', [ceil(Value)] ) + ' ' + fUnitName_MLS.Values[LangId];
      except
        result := 'Unknown';
      end;
    end;

  function TMetaFluid.FormatValueAbs( Value : TFluidValue; LangId : TLanguageId ) : string;
    begin
      try
        result := Format( '%d', [ceil(Value*fFluidFact)]) + ' ' + fUnitName_MLS.Values[LangId];
      except
        result := 'Unknown';
      end;
    end;

  function TMetaFluid.ConvertToUnits( Value : TFluidValue ) : TFluidValue;
    begin
      result := Value*fFluidFact;
    end;

  procedure TMetaFluid.RetrieveTexts( Container : TDictionary );
    begin
      inherited;
      if fName_MLS = nil
        then fName_MLS := TMultiString.Create;
      if fDescription_MLS = nil
        then fDescription_MLS := TMultiString.Create;
      if fUnitName_MLS = nil
        then fUnitName_MLS := TMultiString.Create;
      if fFluidName_MLS = nil
        then fFluidName_MLS := TMultiString.Create;
      fName_MLS.Values[Container.LangId]        := Container.Values[Family + '.' + Id + '.' + 'Name'];
      fDescription_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Desc'];
      fUnitName_MLS.Values[Container.LangId]    := Container.Values[Family + '.' + Id + '.' + 'UnitName'];
      fFluidName_MLS.Values[Container.LangId]   := Container.Values[Family + '.' + Id + '.' + 'FluidName'];
    end;

  procedure TMetaFluid.StoreTexts( Container : TDictionary );
    begin
      inherited;
      Container.Values[Family + '.' + Id + '.' + 'Name']      := fName;
      Container.Values[Family + '.' + Id + '.' + 'Desc']      := fDescription;
      Container.Values[Family + '.' + Id + '.' + 'UnitName']  := fUnitName;
      Container.Values[Family + '.' + Id + '.' + 'FluidName'] := fFluidName;
    end;


  // TMetaGate

  constructor TMetaGate.Create( aName          : string;
                                aGateClass     : CGate;
                                aMetaFluid     : TMetaFluid;
                                aSize          : integer;
                                anOptions      : TMetaGateOptions;
                                aFluidDataSize : integer;
                                anOffset       : TOffset );
    begin
      inherited Create;
      fName          := aName;
      fGateClass     := aGateClass;
      fMetaFluid     := aMetaFluid;
      fSize          := aSize;
      fOptions       := anOptions;
      fFluidDataSize := aFluidDataSize;
      fOffset        := anOffset;
      if ClassIs(TPushInput.ClassName, fGateClass) and (sizeof(TPushInputData) <> aFluidDataSize)
        then Logs.Log( 'ClassInfo', TimeToStr(Now) + ' - Invalid record size ' + Name );
      if ClassIs(TPullInput.ClassName, fGateClass) and (sizeof(TPullInputData) <> aFluidDataSize)
        then Logs.Log( 'ClassInfo', TimeToStr(Now) + ' - Invalid record size ' + Name );
      if ClassIs(TPushOutput.ClassName, fGateClass) and (sizeof(TOutputData) <> aFluidDataSize)
        then Logs.Log( 'ClassInfo', TimeToStr(Now) + ' - Invalid record size ' + Name );
      if ClassIs(TPullOutput.ClassName, fGateClass) and (sizeof(TOutputData) <> aFluidDataSize)
        then Logs.Log( 'ClassInfo', TimeToStr(Now) + ' - Invalid record size ' + Name );
    end;

  function TMetaGate.Instantiate( aBlock : TBlock ) : TGate;
    begin
      result := fGateClass.Create( self, aBlock );
    end;



  // TMetaInput

  constructor TMetaInput.Create( aName          : string;
                                 aMinFluid      : TInputData;
                                 aMaxFluid      : TInputData;
                                 aDefFluid      : TInputData;
                                 aMaxCapacity   : TFluidValue;
                                 anInputClass   : CInput;
                                 aMetaFluid     : TMetaFluid;
                                 aSize          : integer;
                                 aLevel         : TMetaInputLevel;
                                 anOptions      : TMetaGateOptions;
                                 aFluidDataSize : integer;
                                 anOffset       : TOffset );
    begin
      inherited Create( aName, anInputClass, aMetaFluid, aSize, anOptions, aFluidDataSize, anOffset );
      fLevel       := aLevel;
      fMinFluid    := aMinFluid;
      fMaxFluid    := aMaxFluid;
      fDefFluid    := aDefFluid;
      fMaxCapacity := aMaxCapacity;
    end;

  function TMetaInput.GetMinFluid : PInputData;
    begin
      result := @fMinFluid;
    end;

  function TMetaInput.GetMaxFluid : PInputData;
    begin
      result := @fMaxFluid;
    end;

  function TMetaInput.GetDefFluid : PInputData;
    begin
      result := @fDefFluid;
    end;

  function TMetaInput.Instantiate( aBlock : TBlock ) : TGate;
    begin
      result := inherited Instantiate( aBlock );
      PInputData(TInput(result).FluidData)^ := DefFluid^;
    end;


  // TMetaOutput

  constructor TMetaOutput.Create( aName          : string;
                                  aMaxFluid      : TFluidData;
                                  anOutputClass  : COutput;
                                  aMetaFluid     : TMetaFluid;
                                  aSize          : integer;
                                  anOptions      : TMetaGateOptions;
                                  aFluidDataSize : integer;
                                  anOffset       : TOffset );
    begin
      inherited Create( aName, anOutputClass, aMetaFluid, aSize, anOptions, aFluidDataSize, anOffset );
      fMaxFluid  := aMaxFluid;
    end;


  // TLockable

  constructor TLockable.Create;
    begin
      inherited Create;
      fLock := TCriticalSection.Create;
    end;

  destructor TLockable.Destroy;
    begin
      fLock.Free;
      fLock := nil;
      inherited;
    end;

  procedure TLockable.Lock;
    begin
      if fLock <> nil
        then fLock.Enter;
    end;

  procedure TLockable.Unlock;
    begin
      if fLock <> nil
        then fLock.Leave;
    end;

  procedure TLockable.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fLock := TCriticalSection.Create;
    end;

  procedure TLockable.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
    end;


  // TFacility

  constructor TFacility.Create( aMetaFacility : TMetaFacility );
    begin
      inherited Create;
      fMetaFacility := aMetaFacility;
      fCurrStage    := 0;
      fCurrBlock    := NewBlock;
    end;

  destructor TFacility.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      Logs.Log( 'Survival', 'Start Destroying Facility... ' );
      if fMetaFacility <> nil
        then Logs.Log( 'Survival', 'Destroying Facility ' + Name );
      try
        fCurrBlock.Free;
        Logs.Log( 'Survival', 'OK!' );
      except
        if fMetaFacility <> nil
          then Logs.Log( 'Survival', 'Error destroying facility ' + Name + '!' );
      end;
      inherited;
      Logs.Log( 'Survival', 'Destroy OK!' );
    end;

  function TFacility.GetName : string;
    var
      lang : TLanguageId;
    begin
      Lock;
      try
        if fName <> ''
          then result := fName
          else
            begin
              if (Company <> nil) and (Company.Owner <> nil)
                then lang := Company.Owner.Language
                else lang := langDefault;
              result := MetaFacility.Name_MLS.Values[lang]
            end;
      finally
        Unlock;
      end;
    end;

  procedure TFacility.SetName( aName : string );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Renaming Facility.' );
      Lock;
      try
        if CheckOpAuthenticity and (fName <> aName) and not (mfcForbiddenRename in MetaFacility.Options)
          then
            begin
              UnCache(false);
              fName := trim(aName);
              if Length(fName) > 50
                then SetLength(fName, 50);
              Cache(true, true);
              fCurrBlock.FacNameChanged;
            end;
      finally
        Unlock;
      end;
    end;

  procedure TFacility.SetCompany( aCompany : TCompany );
    var
      lang : TLanguageId;
    begin
      fCompany := aCompany;
      if fCompany <> nil
        then
          begin
            if (Company <> nil) and (Company.Owner <> nil)
              then lang := Company.Owner.Language
              else lang := langDefault;
            if mfcGenerateName in MetaFacility.Options
              then fName := MetaFacility.Name_MLS.Values[lang] + ' ' + IntToStr(fCompany.MetaFacilityList.GetNextFacilityNumber(MetaFacility));
            if (MetaFacility.TechnologyKind <> '') and (MetaFacility.Technology <> nil) // >> Check this
              then
                begin
                  //fCompanyResearch := fCompany.Research[MetaFacility.TechnologyKind];
                  //fCompanyDir := fCompany.Directions[MetaFacility.Technology.Kind]; >> New Hq Revolution..
                  fCompanyDir := fCompany.Directions[tidInventionKind_Direction];
                end;
          end;                                             
    end;

  procedure TFacility.SetTown( aTown : TTown );
    begin
      Lock;
      try
        fTown := aTown;                               
        if fTown <> nil       
          then
            begin
              CurrBlock.AutoConnect( false );
              CheckCircuits;
              CheckConnections( chkBoth );
            end;
        Cache(true, true); // >> No one elses cares about this facility so it can wait, if the owners inspects it the cache will be refreshed
      finally
        Unlock;
      end;
    end;

  function TFacility.GetBudget : TMoney;
    begin
      Lock;
      try
        if Company <> nil
          then result := Company.Budget
          else result := 0;
      finally
        Unlock;
      end;
    end;

  function TFacility.GetCriticalTrouble : boolean;
    begin
      result := fTrouble and facCriticalTrouble <> 0;
    end;

  function TFacility.GetStopped : boolean;
    begin
      Lock;
      try
        result := fTrouble and facStoppedByTycoon <> 0;
      finally
        Unlock;
      end;
    end;

  procedure TFacility.SetStopped( Value : boolean );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Stopping Facility.' );
      if CheckOpAuthenticity and (not Value or not (mfcStopDisabled in MetaFacility.Options))
        then
          begin
            Lock;
            try
              if Value <> Stopped
                then
                  begin
                    if Value
                      then
                        begin
                          fTrouble := fTrouble or facStoppedByTycoon;
                          fCurrBlock.Stop;
                        end
                      else
                        begin
                          fCurrBlock.Resume;
                          fTrouble := fTrouble and not facStoppedByTycoon;
                        end;
                    if not fDeleted
                      then UpdateCache(true);
                  end;
            finally
              Unlock;
            end;
          end;
    end;

  function TFacility.GetAge : TVirtDateAbs;
    begin
      try
        if Town <> nil
          then result := Town.Timer.GetVirtualTimeAbs - fCreationDate
          else result := 0;
      except
        result := -1;
      end;
    end;

  procedure TFacility.SetDeleted(Value : boolean);
    begin
      try
        if fDeleted <> Value
          then
            begin
              fDeleted := Value;
              if (fCurrBlock <> nil) and Value
                then fCurrBlock.Deleted;
            end;
      except
        Logs.Log( 'Survival', TimeToStr(Now) + ' Error in SetDelete of ' + Name);
      end;
    end;

  function TFacility.GetVisualClassId : TVisualClassId;

    function VisualClassBase : TVisualClassId;
      var
        i : integer;
      begin
        result := MetaFacility.fVisualClass;
        i := 0;
        while (i < fCurrStage) do
          begin
            inc( result, TEvlStage(MetaFacility.EvlStages[i]).MetaBlock.VisualStages );
            inc( i );
          end;
      end;

    begin
      Lock;
      try
        result := VisualClassBase + fCurrBlock.VisualClassId;
      finally
        Unlock;
      end;
    end;

  function TFacility.ComputeROI : integer;
    var
      facAge : integer;
      apph   : TMoney;
      hours  : single;
    begin
      facAge := GetAge;
      if (NetProfit > 0) and (facAge > 0)
        then
          begin
            apph   := NetProfit/facAge; // avg profit per hour
            hours  := Cost/apph;        // number of hours
            result := round(hours/(24*365));
          end
        else result := -1;
    end;

  function TFacility.GetUpgradeLevel : byte;
    begin
      if fCurrBlock <> nil
        then result := fCurrBlock.UpgradeLevel
        else result := 1;
    end;

  function TFacility.GetUpgradePerc : byte;
    begin
      if fCurrBlock <> nil
        then result := fCurrBlock.UpgradePerc
        else result := 0;
    end;

  procedure TFacility.Simulate;
    begin
      Lock;
      try
        if not Deleted
          then
            begin
              if Company <> nil
                then Company.NewUniquenessMask := Company.NewUniquenessMask or MetaFacility.UniquenessMask;  
              fCurrBlock.Simulate;
              fCurrBlock.RefreshVisualClassInfo;
            end;
      finally
        Unlock;
      end;
    end;

  procedure TFacility.CollectInputs;
    begin
      Lock;
      try
        if Town <> nil
          then
            try
              fMoneyDelta := round((fPeriodMoney - fLastPeriodMoney)/Town.Timer.dt);
            except
              fMoneyDelta := 0;
            end;
        fLastPeriodMoney := fPeriodMoney;
        if Trouble and facStoppedByTycoon <> 0
          then Trouble := facStoppedByTycoon
          else
            begin
              // clear troubles but keep the forbidden zone trouble
              if fTrouble and facForbiddenZone <> 0
                then fTrouble := facForbiddenZone
                else fTrouble := facNoTrouble;
              fCurrBlock.CollectInputs;
            end;
        if (Budget <= 0) and MetaFacility.NeedsBudget
          then ReportTrouble(facNeedsBudget);
      finally
        Unlock;
      end;
    end;

  procedure TFacility.CollectInputExtra;
    begin
      Lock;
      try
        if not CriticalTrouble
          then fCurrBlock.CollectInputExtra;
      finally
        Unlock;
      end;
    end;

  procedure TFacility.SpreadOutputs;
    begin
      Lock;
      try
        if not CriticalTrouble
          then fCurrBlock.SpreadOutputs;
      finally
        Unlock;
      end;
    end;

  procedure TFacility.SpreadOutputExtra;
    begin
      Lock;
      try
        if not CriticalTrouble
          then fCurrBlock.SpreadOutputExtra;
      finally
        Unlock;
      end;
    end;

  procedure TFacility.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );

    procedure ChargeTaxes;
      var
        TaxId : string;
        Tax   : TTax;
      begin
        if (Town <> nil) and (Company <> nil) and (Company.Owner <> nil)
          then
            begin
              TaxId := GetTaxableAccount( CurrBlock.MetaBlock.ProdAccount );
              if TaxId <> ''
                then
                  begin
                    Tax := Town.Taxes[TaxId];
                    if Tax <> nil
                      then
                        begin
                          Tax.Evaluate( fPeriodMoney, Company.Owner, Town );
                          if (fPeriodMoney > 0) and not Company.Owner.IsDemo
                            then Company.Owner.PaidTaxesInTown(Town);
                        end;
                  end;
            end;
      end;

    function WaterAllowed : boolean;
      begin
        if MetaFacility.WaterQuestInv = nil
          then MetaFacility.WaterQuestInv := TInvention(TheClassStorage.ClassById[tidClassFamily_Inventions, tidInvention_WaterQuest]);
        result := (MetaFacility.WaterQuestInv <> nil) and Company.HasInvention[MetaFacility.WaterQuestInv.NumId];
      end;

    begin
      Lock;
      try
        CurrBlock.EndOfPeriod( PeriodType, PeriodCount );
        case PeriodType of
          perYear :
            begin
              CurrBlock.CleanExtraInfo;
              if fMoneyGraph = nil
                then fMoneyGraph := TPlotter.Create;
              fMoneyGraph.Plot( 0, round(fPeriodMoney/1000), round(fPeriodMoney/1000) );
              if not Deleted
                then ChargeTaxes;
              fPeriodMoney := 0;
            end;
          perMonth :
            begin
              if fToBeDemolished > 0
                then
                  if fToBeDemolished = 1
                    then Town.ModelFactory.RequestDeletion( self )
                    else dec( fToBeDemolished );
              if (fToBeDemolished <> 1) and fRequiresLandTech and not WaterAllowed and not CurrBlock.MetaBlock.Transcends(self)
                then
                  begin
                    Logs.Log( 'Survival', TimeToStr(Now) + Format(' Request Delete On-Water Facility: (%d, %d)', [xPos, yPos]));
                    Town.ModelFactory.RequestDeletion( self );
                  end;
            end;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TFacility.CheckCircuits;
    begin
      fCurrBlock.CheckCircuits;
    end;

  procedure TFacility.CheckConnections( ConnectionCheckLevel : TConnectionCheckLevel );
    begin
      fCurrBlock.CheckConnections( ConnectionCheckLevel );
    end;

  procedure TFacility.Loaded;
    begin
      try
        if Company <> nil
          then Company.NewUniquenessMask := Company.NewUniquenessMask or MetaFacility.UniquenessMask;
        fCurrBlock.BlockLoaded;
        if (Company = nil) and (xPos > 0) and (yPos > 0)
          then Town.ModelFactory.RequestDeletion( self )
          else
            begin
              fCurrBlock.RecalculateInventionsEffect;
              if mfcForbiddenRename in MetaFacility.Options
                then fName := '';
            end;
      except
        Logs.Log( 'Survival', TimeToStr(Now) + ' Error in TFacility.Loaded ' + Name);
      end;
    end;

  function TFacility.ConnectTo( Facility : TFacility ) : string;
    begin
      try
        if not Facility.Deleted and (Facility.Company <> nil) and not Facility.Company.Deleted
          then
            begin
              result := fCurrBlock.ConnectTo( Facility.CurrBlock, true );
              CheckConnections( chkBoth );
              ModelServerCache.BackgroundInvalidateCache(self); //UpdateCache(true);
            end
          else result := '';
      except
        Logs.Log( 'Survival', TimeToStr(Now) + ' Error in ConnectTo facility ' + Name + ' to ' + Facility.Name);
        result := '';
      end;
    end;

  procedure TFacility.DisconnectFrom( Facility : TFacility );
    begin
      try
        fCurrBlock.DisconnectFrom( Facility.CurrBlock, true );
        ModelServerCache.BackgroundInvalidateCache(self); //UpdateCache(true);
      except
        Logs.Log( 'Survival', TimeToStr(Now) + ' Error in Disconnecting facility ' + Name + ' from ' + Facility.Name);
      end;
    end;

  type
    TGateDesc =
      class
        x, y : integer;
        constructor Create( aX, aY : integer );
      end;

    constructor TGateDesc.Create( aX, aY : integer );
      begin
        inherited Create;
        x := aX;
        y := aY;
      end;

  function ParseGateList( GateList : string ) : TCollection;
    var
      List : TStringList;
      p    : integer;
      i    : integer;
    begin
      List := TStringList.Create;
      repeat
        p := system.pos( ',', GateList );
        if p <> 0
          then
            begin
              List.Add( copy( GateList, 1, pred(p) ));
              GateList := copy( GateList, succ(p), length(GateList) - p );
            end;
      until (p = 0);
      if not odd(List.Count)
        then
          begin
            result := TCollection.Create( 0, rkBelonguer );
            for i := 0 to pred(List.Count div 2) do
              result.Insert( TGateDesc.Create( StrToInt(List[2*i]), StrToInt(List[2*i + 1]) ));
          end
        else result := nil;
      List.Free;
    end;

  procedure TFacility.RDOConnectInput( FluidId, Suppliers : widestring );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Input connected: ' + FluidId + ' to ' + Suppliers );
      if CheckOpAuthenticity
        then ModifyConnection( FluidId, Suppliers, true, true );
    end;

  procedure TFacility.RDOConnectOutput( FluidId, Clients : widestring );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Output connected: ' + FluidId + ' to ' + Clients );
      if CheckOpAuthenticity
        then ModifyConnection( FluidId, Clients, true, false );
    end;

  procedure TFacility.RDODisconnectInput( FluidId, Suppliers : widestring );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Input disconnect: ' + FluidId + ' from ' + Suppliers );
      if CheckOpAuthenticity
        then ModifyConnection( FluidId, Suppliers, false, true );
    end;

  procedure TFacility.RDODisconnectOutput( FluidId, Clients : widestring );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Output disconnect: ' + FluidId + ' from ' + Clients );
      if CheckOpAuthenticity
        then ModifyConnection( FluidId, Clients, false, false );
    end;

  procedure TFacility.RDOSetOutputPrice( FluidId : widestring; Price : integer );
    var
      Output : TOutput;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Output price set: ' + FluidId + ' to ' + IntToStr(Price) );
      try
        Lock;
        try
          Output := CurrBlock.OutputsByName[FluidId];
          if (Output <> nil) and CheckOpAuthenticity
            then
              begin
                Output.PricePerc := Price;
                Output.InvalidateCache(false);
              end;
        finally
          Unlock;
        end;
        //UpdateCache(true);
        Logs.Log( tidLog_Survival, 'OK.' );
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, ' Error in SetOutputPrice ' + E.Message );
      end;
    end;

  procedure TFacility.RDOSetInputOverPrice( FluidId : widestring; SupplierIdx, OverPrice : integer );
    var
      Input    : TInput;
      ExtraCnx : PExtraConnectionInfo;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Input overprice set: ' + FluidId + ' to ' + IntToStr(OverPrice) );
      try
        Lock;
        try
          Input := CurrBlock.InputsByName[FluidId];
          if (Input <> nil) and CheckOpAuthenticity
            then
              begin
                ExtraCnx := Input.ExtraConnectionInfo[SupplierIdx];
                if ExtraCnx <> nil
                  then
                    begin
                      Input.ExtraConnectionInfo[SupplierIdx]^.OverPrice := OverPrice;
                      Input.InvalidateCache(false);
                    end;
              end;
        finally
          Unlock;
        end;
        //UpdateCache(true);
        Logs.Log( tidLog_Survival, 'OK.' );
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in SetOverPrice ' + E.Message );
      end;
    end;

  procedure TFacility.RDOSetInputMaxPrice( FluidId : widestring; MaxPrice : integer );
    var
      Input : TPullInput;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Input max price set: ' + FluidId + ' to ' + IntToStr(MaxPrice) );
      try
        Lock;
        try
          Input := TPullInput(CurrBlock.InputsByName[FluidId]);
          if CheckOpAuthenticity and (Input <> nil) and ObjectIs('TPullInput', Input)
            then
              begin
                Input.MaxPrice := MaxPrice;
                Input.InvalidateCache(false);
              end;
        finally
          Unlock;
        end;
        //UpdateCache(true);
        Logs.Log( tidLog_Survival, 'OK.' );
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, E.Message );
      end;
    end;

  procedure TFacility.RDOSetInputMinK( FluidId : widestring; MinK : integer );
    var
      Input : TPullInput;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Input min K set: ' + FluidId + ' to ' + IntToStr(MinK) );
      try
        Lock;
        try
          Input := TPullInput(CurrBlock.InputsByName[FluidId]);
          if (Input <> nil) and CheckOpAuthenticity
            then
              begin
                Input.MinK := MinK;
                Input.InvalidateCache(false);
              end;
        finally
          Unlock;
        end;
        //UpdateCache(true);
        Logs.Log( tidLog_Survival, 'OK.' );
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, E.Message );
      end;
    end;

  procedure TFacility.RDOSetInputSortMode(FluidId : widestring; mode : integer);
    var
      Input : TPullInput;
    begin
      Logs.Log(tidLog_Survival, TimeToStr(Now) + ' Changing Sort Mode.. ');
      try
        Lock;
        try
          Input := TPullInput(CurrBlock.InputsByName[FluidId]);
          if (Input <> nil) and CheckOpAuthenticity
            then
              begin
                Input.SetSortMode(mode);
                Input.InvalidateCache(true);
                Town.MapRefresh.RefeshFacility(self, fchStructure);
              end;
        finally
          Unlock;
        end;
        Logs.Log( tidLog_Survival, 'OK.' );
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, E.Message );
      end;
    end;

  function TFacility.RDOGetConnectionReport : OleVariant;
    var
      i, j   : integer;
      report : widestring;
      Gate   : TGate;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Get connection report' );
      try
        Lock;
        try
          with CurrBlock do
            begin
              report := IntToStr(OutputCount) + LineBreak;
              for i := 0 to pred(OutputCount) do
                begin
                  report := report + IntToStr(Outputs[i].ConnectionCount) + LineBreak;
                  if [mfWorkForce, mfPeople]*Outputs[i].MetaOutput.MetaFluid.Options <> []
                    then report := report + '0' + LineBreak
                    else report := report + '1' + LineBreak;
                  for j := 0 to pred(Outputs[i].ConnectionCount) do
                    begin
                      Gate   := Outputs[i].Connections[j];
                      report := report + IntToStr(Gate.Block.xPos) + LineBreak + IntToStr(Gate.Block.yPos) + LineBreak;
                    end;
                end;
              report := report + IntToStr(InputCount) + LineBreak;
              for i := 0 to pred(InputCount) do
                begin
                  report := report + IntToStr(Inputs[i].ConnectionCount) + LineBreak;
                  if [mfWorkForce, mfPeople]*Inputs[i].MetaInput.MetaFluid.Options <> []
                    then report := report + '0' + LineBreak
                    else
                      if Inputs[i].MetaInput.Level = mglBasic
                        then report := report + '1' + LineBreak
                        else report := report + '2' + LineBreak;
                  for j := 0 to pred(Inputs[i].ConnectionCount) do
                    begin
                      Gate   := Inputs[i].Connections[j];
                      report := report + IntToStr(Gate.Block.xPos) + LineBreak + IntToStr(Gate.Block.yPos) + LineBreak;
                    end;
                end;
            end;
        finally
          Unlock;
        end;
      except
        report := '0' + LineBreak + '0';
        Logs.Log( tidLog_Survival, TimeToStr(Now) + 'Get connection report error.' );
      end;
      result := report;
      Logs.Log( tidLog_Survival, TimeToStr(Now) + 'Get connection report OK.' );
    end;

  procedure TFacility.RDOConnectToTycoon( TycoonId, FacTypes : integer; SetAsDefault : wordbool );
    var
      Tycoon  : TTycoon;
      i, j    : integer;
      ki, ko  : integer;
      flag    : boolean;
      Msg     : TMsgSellToAll;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Connect to Tycoon: ' + IntToStr(TycoonId) );
      try
        flag := false;
        Lock;
        try
          Tycoon := TTycoon(pointer(TycoonId));
          if not Tycoon.Deleted
            then
              for i := 0 to pred(Tycoon.Companies.Count) do
                for j := 0 to pred(TCompany(Tycoon.Companies[i]).fFacilities.Count) do
                  with TFacility(TCompany(Tycoon.Companies[i]).fFacilities[j]) do
                    begin
                      if not Deleted and
                         (FacTypes and ftpWarehouses <> 0) and (MetaFacility.Kind.Role in [rolDistributer, rolCompExport, rolCompInport]) or
                         (FacTypes and ftpFactories <> 0) and (MetaFacility.Kind.Role = rolProducer) or
                         (FacTypes and ftpStores <> 0) and (MetaFacility.Kind.Role = rolBuyer)
                        then
                          begin
                            for ko := 0 to pred(self.CurrBlock.OutputCount) do
                              for ki := 0 to pred(CurrBlock.InputCount) do
                                if (CurrBlock.Inputs[ki].MetaInput.MetaFluid = self.CurrBlock.Outputs[ko].MetaOutput.MetaFluid) and
                                   (mfTradeable in CurrBlock.Inputs[ki].MetaInput.MetaFluid.Options) and
                                   (CurrBlock.Inputs[ki].ConnectTo( self.CurrBlock.Outputs[ko] ) = cnxValid)
                                  then flag := true;
                          end;
                    end;
          if flag
            then
              begin
                CheckConnections( chkBoth );
                ModelServerCache.BackgroundCache(self, false); //UpdateCache(true)
              end;
        finally
          Unlock;
        end;
        if SetAsDefault
          then Tycoon.RegisterSupplier( self );

        Msg.Msg := msgKernel_SellToAll;
        Msg.FacTypes := FacTypes;
        Msg.Fac := self;
        try
          Tycoon.Dispatch(Msg);
        except
          // log errors..
        end;
        Logs.Log( tidLog_Survival, 'OK.' );
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, E.Message );
      end;
    end;

  procedure TFacility.RDODisconnectFromTycoon( TycoonId, FacTypes : integer; RemoveAsDefault : wordbool );
    var
      Tycoon  : TTycoon;
      i, j    : integer;
      ki, ko  : integer;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Disconnect from Tycoon: ' + IntToStr(TycoonId) );
      try
        Lock;
        try
          Tycoon := TTycoon(pointer(TycoonId));
          for i := 0 to pred(Tycoon.Companies.Count) do
            for j := 0 to pred(TCompany(Tycoon.Companies[i]).fFacilities.Count) do
              with TFacility(TCompany(Tycoon.Companies[i]).fFacilities[j]) do
                begin
                  for ko := 0 to pred(self.CurrBlock.OutputCount) do
                    for ki := 0 to pred(CurrBlock.InputCount) do
                      if CurrBlock.Inputs[ki].MetaInput.MetaFluid = self.CurrBlock.Outputs[ko].MetaOutput.MetaFluid
                        then CurrBlock.Inputs[ki].DisconnectFrom( self.CurrBlock.Outputs[ko] );
                end;
          ModelServerCache.BackgroundCache(self, false);
        finally
          Unlock;
        end;
        if RemoveAsDefault
          then Tycoon.UnregisterSupplier( self );
        Logs.Log( tidLog_Survival, 'OK.' );
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, E.Message );
      end;
    end;

  procedure TFacility.RDOConnectToTradeCenter;
    var
      TradeCenter : TBlock;
      ki, ko      : integer;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Connect to Trade Center' );
      try
        Lock;
        try
          TradeCenter := TInhabitedTown(Town).TradeCenter.CurrBlock;
          for ki := 0 to pred(CurrBlock.InputCount) do
            for ko := 0 to pred(TradeCenter.OutputCount) do
              if CurrBlock.Inputs[ki].MetaInput.MetaFluid = TradeCenter.Outputs[ko].MetaOutput.MetaFluid
                then CurrBlock.Inputs[ki].ConnectTo( TradeCenter.Outputs[ko] );
        finally
          Unlock;
        end;
        Logs.Log( tidLog_Survival, 'OK.' );
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, E.Message );
      end;
    end;

  procedure TFacility.RDODisconnectFromTradeCenter;
    var
      TradeCenter : TBlock;
      ki, ko      : integer;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Disconnect from Trade Center' );
      try
        Lock;
        try
          TradeCenter := TInhabitedTown(Town).TradeCenter.CurrBlock;
          for ki := 0 to pred(CurrBlock.InputCount) do
            for ko := 0 to pred(TradeCenter.OutputCount) do
              if CurrBlock.Inputs[ki].MetaInput.MetaFluid = TradeCenter.Outputs[ko].MetaOutput.MetaFluid
                then CurrBlock.Inputs[ki].DisconnectFrom( TradeCenter.Outputs[ko] );
        finally
          Unlock;
        end;
        Logs.Log( tidLog_Survival, 'OK.' );
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, E.Message );
      end;
    end;

  procedure TFacility.RDOStartUpgrade;
    begin
      RDOStartUpgrades(1);
    end;

  procedure TFacility.RDOStartUpgrades(count : integer);
    var
      Block : TBlock;
    begin
      if CheckOpAuthenticity
        then
          try
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Facility Start Upgrade count: ' + IntToStr(count));
            Block := CurrBlock;
            if (Block.Facility = self) and (Block.MetaBlock <> nil) and (Block.UpgradeLevel < Block.MetaBlock.MaxUpgrade) and (Company <> nil) and (Company.Owner <> nil) and (not Company.Owner.CountUpgrades or (Company.Owner.FacCount < Company.Owner.Level.FacLimit))
              then Block.StartUpgrading(count);
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Facility Start Upgrade OK!' );
          except
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in Facility Start Upgrade..' );
          end;
    end;

  procedure TFacility.RDOStopUpgrade;
    var
      Block : TBlock;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Facility Stop Upgrade..' );
      if CheckOpAuthenticity
        then
          try
            Block := CurrBlock;
            if (Block.Facility = self) and (Block.UpgradeHours > 0)
              then Block.StopUpgrading;
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Facility Stop Upgrade OK!' );
          except
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in Facility Stop Upgrade..' );
          end;
    end;

  procedure TFacility.RDODowngrade;
    begin
      RDODowngradeMany(1);
    end;

  procedure TFacility.RDODowngradeMany(count : integer);
    var
      Block : TBlock;
    begin
      if CheckOpAuthenticity
        then
          try
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Facility Downgrade count: ' + IntToStr(count));
            Block := CurrBlock;
            if (Block.Facility = self) and (Block.UpgradeLevel > 1)
              then Block.Downgrade(count);
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Facility Downgrade OK!' );
          except
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in Facility Downgrade..' );
          end;
    end;

  procedure TFacility.ModifyConnection( Org, Dest : widestring; Connect, Input : boolean );
    var
      OrgGate  : TGate;
      DestGate : TGate;
      DestFac  : TFacility;
      Gates    : TCollection;
      i        : integer;
      Msg      : TMsgFacHireOffer;
    begin
      Logs.Log( tidLog_Survival, 'Now modifying connection...' );
      try
        Lock;
        try
          if Input
            then OrgGate := CurrBlock.InputsByName[Org]
            else OrgGate := CurrBlock.OutputsByName[Org];

          if OrgGate <> nil
            then
              begin
                Gates := ParseGateList( Dest );
                if Gates <> nil
                  then
                    try
                      for i := 0 to pred(Gates.Count) do
                        with TGateDesc(Gates[i]) do
                          begin
                            DestFac := Town.WorldLocator.FacilityAt( x, y );
                            if (DestFac <> nil) and (not DestFac.Deleted) //and (not DestFac.CriticalTrouble)
                              then
                                begin
                                  if Input
                                    then DestGate := DestFac.CurrBlock.OutputsByName[Org]
                                    else DestGate := DestFac.CurrBlock.InputsByName[Org];
                                  if DestGate <> nil
                                    then
                                      if Connect
                                        then
                                          begin
                                            OrgGate.ConnectTo( DestGate );
                                            if Input
                                              then TOutput(DestGate).CheckConnections
                                              else TOutput(OrgGate).CheckConnections;
                                          end
                                        else OrgGate.DisconnectFrom( DestGate );
                                end
                          end;
                    finally
                      Gates.Free;
                    end;
              end;

          Msg.Msg  := msgKernel_FacsHireOffer;
          Msg.Fac  := self;
          Msg.Hire := Input;
          if (Company <> nil) and (Company.Owner <> nil)
            then Company.Owner.Dispatch(Msg);

        finally
          Unlock;
        end;
        OrgGate.InvalidateCache(true);
        Logs.Log( tidLog_Survival, 'OK.' );
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, E.Message );
      end;
    end;

  procedure TFacility.GetFocus;
    begin
      Lock;
      try
        if fFocusCount < high(fFocusCount)
          then inc( fFocusCount );
      finally
        Unlock;
      end;
    end;

  procedure TFacility.LostFocus;
    begin
      Lock;
      try
        if fFocusCount > 0
          then dec( fFocusCount );
      finally
        Unlock;
      end;
    end;

  procedure TFacility.VisualUpdate;
    begin
      Lock;
      try
        if Town <> nil
          then Town.MapRefresh.RefreshArea( xPos, yPos, MetaFacility.xSize + 1, MetaFacility.ySize + 1 );
      finally
        Unlock;
      end;
    end;

  procedure TFacility.ReportTrouble( aTrouble : byte );
    begin
      Lock;
      try
        Trouble := Trouble or aTrouble;
      finally
        Unlock;
      end;
    end;

  procedure TFacility.ClearTrouble( aTrouble : byte );
    begin
      Lock;
      try
        fTrouble := fTrouble and not aTrouble;
      finally
        Unlock;
      end;
    end;

  function TFacility.AccessLevelOf( Tycoon : TTycoon ) : TAccessLevel;
    begin
      if (Company <> nil) and ((Company.Owner <> nil) and (Tycoon.MasterRole = Company.Owner.MasterRole) or not Company.Privated)
        then result := acsFull
        else result := acsGuest
    end;

  function TFacility.CheckOpAuthenticity : boolean;
    begin
      result := (fCompany <> nil) and fCompany.CheckOpAuthenticity;
    end;

  function TFacility.GetFocused : boolean;
    begin
      Lock;
      try
        result := fFocusCount > 0;
      finally
        Unlock;
      end;
    end;

  procedure TFacility.BlockModified( Modification : TBlockModification );
    begin
      Lock;
      try
        case Modification of
          bmVisualChange :
            Town.MapRefresh.RefreshArea( xPos, yPos, MetaFacility.xSize, MetaFacility.ySize );
          bmEvolve :
            begin
              fNetProfit := 0;
              fPeriodMoney := 0;
              try
                if (Company <> nil) and (Company.Owner <> nil)
                  then Company.Owner.CountFacility( self, false );
              except
              end;

              UnCache(false); // >> No choice

              // fCurrBlock.Free;
              fCurrBlock.Deleted;
              inc(fCurrStage);
              fCurrBlock := NewBlock;
              fCurrBlock.RecalculateInventionsEffect;
              CheckCircuits;
              fCurrBlock.AutoConnect( false );
              try
                Logs.Log( 'Survival', TimeToStr(Now) + ' BLOCK EVOLVE: looking for suppliers.' );
                TInhabitedTown(Town).World.SearchForSuppliers( fCurrBlock ); // >> EL TOTI
                Logs.Log( 'Survival', TimeToStr(Now) + ' BLOCK EVOLVE: looking for suppliers OK.' );
              except
                Logs.Log( 'Survival', TimeToStr(Now) + ' BLOCK EVOLVE: Exception looking for suppliers.' );
              end;
              CheckConnections( chkBoth );
              Cache(false, true); // >> Not necesary!!
              try
                if (Company <> nil) and (Company.Owner <> nil)
                  then Company.Owner.CountFacility( self, true );
              except
              end;
              Town.MapRefresh.RefreshArea( xPos, yPos, MetaFacility.xSize, MetaFacility.ySize );
              // Tutorial stuff
              NotifyNewBlockToOwners;
              {
              NewBlockMsg.Msg   := msgKernel_NewBlock;
              NewBlockMsg.Block := fCurrBlock;
              Dispatch( NewBlockMsg );
              if Company <> nil
                then
                  begin
                    Company.Dispatch( NewBlockMsg );
                    if Company.Owner <> nil
                      then Company.Owner.Dispatch( NewBlockMsg );
                  end;
              }
            end;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TFacility.GenMoney( Money : TMoney; Reason : TMoneyReason );
    begin
      Lock;
      try
        fPeriodMoney := fPeriodMoney + Money;
        fNetProfit   := fNetProfit + Money;
        if Company <> nil
          then Company.GenMoney( Money, Reason );
      finally
        Unlock;
      end;
    end;

  procedure TFacility.StatusChanged( Change : TFacilityChange );
    begin
      if (Town <> nil) and (Town.MapRefresh <> nil)
        then Town.MapRefresh.RefeshFacility( self, Change );
    end;

  function TFacility.NewBlock : TBlock;
    begin
      result := TEvlStage(MetaFacility.EvlStages[fCurrStage]).MetaBlock.Instantiate( self );
    end;

  procedure TFacility.NotifyNewBlockToOwners;
    var
      NewBlockMsg : TMsgNewBlock;
    begin
      NewBlockMsg.Msg   := msgKernel_NewBlock;
      NewBlockMsg.Block := fCurrBlock;
      Dispatch( NewBlockMsg );
      if Company <> nil
        then
          begin
            Company.Dispatch( NewBlockMsg );
            if Company.Owner <> nil
              then Company.Owner.Dispatch( NewBlockMsg );
          end;
    end;

  function TFacility.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;

    function RenderCircuitStr( Circuits : TCollection ) : string;
      var
        i : integer;
      begin
        result := '';
        if Circuits <> nil
          then
            for i := 0 to pred(Circuits.Count) do
              result := result + IntToStr(integer(Circuits[i])) + ',';
      end;

    var
      StatusStr : string;
      mls_Name  : string;
      lang      : TLanguageId;
    begin
      Lock;
      try
        result := '';
        case kind of
          sttMain :
            begin
              if (Company <> nil) and (mfcShowCompanyInText in MetaFacility.Options)
                then result := Name + tcnDescSeparator + Company.Name
                else result := Name;
              StatusStr := fCurrBlock.GetStatusText( kind, ToTycoon );
              if StatusStr <> ''
                then result := result + tcnDescSeparator + StatusStr;
              if CurrBlock.Upgrading
                then result := result + tcnDescSeparator + Format(mtidUpgrading.Values[ToTycoon.Language], [UpgradePerc]);
              if (Company <> nil) and Company.Privated and (Company.Owner <> nil) and (mfcShowProfitInText in MetaFacility.Options)
                then result := result + tcnDescSeparator + '(' + FormatMoney( fMoneyDelta ) + '/h)';
            end;
          sttSecondary :
            begin
              if (Company <> nil) and (Company.Owner <> nil)
                then lang := Company.Owner.Language
                else lang := langDefault;
              if system.pos( MetaFacility.Name_MLS.Values[lang], Name ) = 0
                then mls_Name := MetaFacility.Name_MLS.Values[lang]
                else mls_Name := '';
              if mls_Name <> ''
                then result :=  mls_Name + '.';
              if (Company <> nil)
                then
                  if Trouble and facStoppedByTycoon <> 0
                    then StatusStr := Format( mtidStoppedBy.Values[ToTycoon.Language], [Company.Owner.Name] )
                    else
                      if Trouble and facNeedsConnection <> 0
                        then StatusStr := mtidStoppedNoCnxs.Values[ToTycoon.Language]
                        else
                          if Trouble and facNeedsBudget <> 0
                            then StatusStr := mtidStoppedMoney.Values[ToTycoon.Language]
                            else StatusStr := '';
              result :=
                result + ' ' +
                StatusStr + ' ' +
                fCurrBlock.GetStatusText( kind, ToTycoon );
            end;
          sttHint :
            if ToBeDemolished = 0
              then
                begin
                  result := fCurrBlock.GetStatusText( kind, ToTycoon );
                  if (result = '') and (Company <> nil)
                    then
                      begin
                        if AccessLevelOf( ToTycoon ) = acsGuest
                          then
                            if Company.Owner <> nil
                              then result := GetHintText( mtidHintsDenied.Values[ToTycoon.Language], [Company.Owner.Name] )
                              else result := GetHintText( mtidHintsDenied.Values[ToTycoon.Language], [Company.Cluster.Id] )
                          else result := GetHintText( mtidVisitWebSite.Values[ToTycoon.Language], [0] );
                      end;
                end
              else result := GetHintText( mtidFacilityWillBeDemolished.Values[ToTycoon.Language], [Town.Name, ToBeDemolished, FormatMoney(Cost)] );
        end;
        result := Trim( result );
      finally
        Unlock;
      end
    end;

  procedure TFacility.Cache(isNew, Background : boolean);
    begin
      if not Deleted
        then
          begin
            Lock;
            try
              if isNew
                then
                  if Background
                    then ModelServerCache.BackgroundCache(self, true)
                    else ModelServerCache.CacheObject(self, noKind, recLinks)
                else
                  if Background
                    then ModelServerCache.BackgroundCache(self, false)
                    else ModelServerCache.CacheObject(self, noKind, noInfo);
            finally
              Unlock;
            end;
      end;
    end;

  procedure TFacility.UnCache(background : boolean);
    begin
      Lock; // >> Mysterious Deadlock!!
      try
        CurrBlock.RemoveFromCache(background);
        if Background
          then ModelServerCache.BackgroundUncache(self)
          else ModelServerCache.UnCacheObject(self, noKind, noInfo);
      finally
        Unlock;
      end;
    end;

  procedure TFacility.UpdateCache(background : boolean);
    begin
      //UnCache(false); // ??????????????????
      Cache(false, background);
    end;

  function TFacility.GetCacheName : string;
    begin
      result := MetaFacility.Id + NameSeparator + IntToStr(xPos) + NameSeparator + IntToStr(yPos);
    end;

  procedure TFacility.CopySettingsFrom(Facility : TFacility; Options : integer);
    begin
      if (fCurrBlock <> nil) and fCurrBlock.AcceptCloning
        then fCurrBlock.CopySettingsFrom(Facility.CurrBlock, Options);
    end;

  procedure TFacility.LoadFromBackup( Reader : IBackupReader );
    var
      i : integer;
    begin
      inherited;
      try
        fName := trim(Reader.ReadString( 'Name', '' ));
        fCreationDate := Reader.ReadInteger( 'CreationDate' , 0);
        fXPos := Reader.ReadInteger( 'xPos', 0 );
        fYPos := Reader.ReadInteger( 'yPos', 0 );
        Reader.ReadObject( 'Company', fCompany, nil );
        Reader.ReadObject( 'Town', fTown, nil );
        fMetaFacility := TMetaFacility(TheClassStorage.ClassById[tidClassFamily_Facilities, Reader.ReadString( 'MetaFacility', '' )]);
        if (fMetaFacility <> nil) and (mfcForbiddenRename in fMetaFacility.Options)
          then fName := '';
        fCurrStage    := Reader.ReadInteger( 'CurrentStage', 0 );
        Reader.ReadObject( 'CurrBlock', fCurrBlock, nil );
        Reader.ReadObject( 'MoneyGraph', fMoneyGraph, nil );
        fPeriodMoney    := Reader.ReadCurrency( 'PeriodMoney', 0 );
        //>>>fPeriodMoney    := 0; // >>
        fTrouble        := Reader.ReadByte( 'Trouble', 0 );
        fToBeDemolished := Reader.ReadByte( 'ToBeDemolished', 0 );
        fCost           := Reader.ReadCurrency( 'Cost', 0 );
        fTCCost         := Reader.ReadCurrency('TCCost', -1);
        // Compatibility trick
        if fTCCost < 0
          then fTCCost := fCost;

        fNetProfit := Reader.ReadCurrency('NetProfit', 0);
        if fNetProfit = 0
          then fNetProfit := fPeriodMoney;

        // Initialize things
        if (fCompany <> nil) and (MetaFacility.Technology <> nil)
          then
            try
              //fCompanyDir := fCompany.Directions[MetaFacility.Technology.Kind];
              fCompanyDir := fCompany.Directions[tidInventionKind_Direction];
            except
              fCompanyDir := nil;
            end;

        // Skip reserved space
        for i := 1 to 10 do
          Reader.ReadInteger('Rsvd' + IntToStr(i), 0);

      except
        fToBeDemolished := 1;
        Logs.Log( 'Survival', TimeToStr(Now) + 'Error loading facility ' + Name + '(' + IntToStr(fxPos) + ',' + IntToStr(fyPos) + ')');
        raise;
      end;
    end;

  procedure TFacility.StoreToBackup( Writer : IBackupWriter );
    var
      i : integer;
    begin
      try
        inherited;
        Lock;
        try
          Writer.WriteString( 'Name', fName );
          Writer.WriteInteger( 'CreationDate', fCreationDate );
          Writer.WriteInteger( 'xPos', fXPos );
          Writer.WriteInteger( 'yPos', fYPos );
          Writer.WriteObjectRef( 'Company', fCompany );
          Writer.WriteObjectRef( 'Town', fTown );
          Writer.WriteString( 'MetaFacility', fMetaFacility.Id );
          Writer.WriteInteger( 'CurrentStage', fCurrStage );
          Writer.WriteObject( 'CurrBlock', fCurrBlock );
          Writer.WriteLooseObject( 'MoneyGraph', fMoneyGraph );
          Writer.WriteCurrency( 'PeriodMoney', fPeriodMoney );
          Writer.WriteByte( 'Trouble', fTrouble );
          Writer.WriteByte( 'ToBeDemolished', fToBeDemolished );
          Writer.WriteCurrency( 'Cost', fCost );
          Writer.WriteCurrency( 'TCCost', fTCCost );
          Writer.WriteCurrency('NetProfit', fNetProfit);
          // Save reserved space
          for i := 1 to 10 do
            Writer.WriteInteger('Rsvd' + IntToStr(i), 0);
        finally
          Unlock;
        end;
      except
        Logs.Log( 'Backup', TimeToStr(Now) + 'Error backing up facility ' + Name + '(' + IntToStr(xPos) + ',' + IntToStr(yPos) + ')');
        raise;
      end;
    end;

  function TFacility.HasTechnology : boolean;
    begin
      result := not MetaFacility.DepOnTech or (MetaFacility.Technology = nil) or ((Company <> nil) and Company.HasInvention[MetaFacility.Technology.NumId]);
    end;

  procedure TFacility.CreateCacheLinks;
    var
      link : string;
      path : string;
    begin
      if (Company <> nil) and (Company.Owner <> nil) and (MetaFacility.Kind <> nil)
        then
          begin
            path := GetGlobalPath(KernelCache.TFacilityCacheAgent.GetPath(self, noKind, noInfo));
            link := GetGlobalPath(tidCachePath_Tycoons + Company.Owner.Name + '.five\Companies\' + Company.Name + '\Facilities\' + MetaFacility.Kind.SuperType + '\');
            ModelServerCache.CacheObjectLinks(path, link);
          end;
    end;


  // TBlock

  constructor TBlock.Create( aMetaBlock : TMetaBlock; aFacility : TFacility );
    var
      i : integer;
    begin
      inherited Create;
      fMetaBlock   := aMetaBlock;
      fFacility    := aFacility;
      fInputCount  := aMetaBlock.MetaInputs.Count;
      fOutputCount := aMetaBlock.MetaOutputs.Count;
      getmem( fInputs, InputCount*sizeof(fInputs[0]) );
      getmem( fOutputs, fOutputCount*sizeof(fOutputs[0]) );
      for i := 0 to pred(InputCount) do
        begin
          fInputs[i] := TMetaGate(aMetaBlock.MetaInputs[i]).Instantiate( self );
          if (Inputs[i].MetaInput.MetaFluid <> nil) and (mfCompanyFluid in Inputs[i].MetaInput.MetaFluid.Options) and (Facility <> nil) and (Facility.Company <> nil)
            then Facility.Company.RegisterInput( Inputs[i] );
        end;
      for i := 0 to pred(OutputCount) do
        fOutputs[i] := TMetaGate(aMetaBlock.MetaOutputs[i]).Instantiate( self );
      getmem( fCompanyInputArray, MetaBlock.CompanyInputs.Count*sizeof(TCompanyInputData) );
      fillchar( fCompanyInputArray^, MetaBlock.CompanyInputs.Count*sizeof(TCompanyInputData), 0 );
      fAcceptCloning := true;
      fUpgradeLevel  := 1;
    end;

  destructor TBlock.Destroy;

    procedure UnregisterCompanyInputs;
      var
        i : integer;
      begin
        for i := 0 to pred(InputCount) do
          if (mfCompanyFluid in Inputs[i].MetaInput.MetaFluid.Options) and (Facility <> nil) and (Facility.Company <> nil)
            then Facility.Company.UnregisterInput( Inputs[i] );
      end;

    procedure FreeGateArray( GateArray : PGateArray; Size : integer );
      var
        i : integer;
      begin
        for i := 0 to pred(Size) do
          begin
            Logs.Log( 'Survival', 'Deleting connections ' + IntToStr(i) + ' of ' + IntToStr(Size) );
            GateArray[i].Free;
            Logs.Log( 'Survival', 'Ok delete connections!' );
          end;
        freemem( GateArray, Size*sizeof(GateArray[0]) );
      end;

    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      try
        UnregisterCompanyInputs;
      except
      end;
      try
        FreeGateArray( fInputs, InputCount );
      except
      end;
      try
        FreeGateArray( fOutputs, OutputCount );
      except
      end;
      freemem( fCompanyInputArray, MetaBlock.CompanyInputs.Count*sizeof(TCompanyInputData) );
      inherited;
    end;

  function TBlock.GetXPos : integer;
    begin
      result := Facility.xPos;
    end;

  function TBlock.GetYPos : integer;
    begin
      result := Facility.yPos;
    end;

  function TBlock.GetXOrigin : integer;
    begin
      result := xPos + Facility.MetaFacility.xSize div 2;
    end;

  function TBlock.GetYOrigin : integer;
    begin
      result := yPos + Facility.MetaFacility.ySize div 2;
    end;

  function TBlock.GetInput( index : integer ) : TInput;
    begin
      result := TInput(fInputs[index]);
    end;

  function TBlock.GetOutput( index : integer ) : TOutput;
    begin
      result := TOutput(fOutputs[index]);
    end;

  function TBlock.GetInputIndex( name : string ) : integer;
    begin
      result := pred(InputCount);
      while (result >= 0) and ((Inputs[result].MetaInput = nil) or (Inputs[result].MetaInput.Name <> name)) do
        dec( result );
    end;

  function TBlock.GetOutputIndex( name : string ) : integer;
    begin
      result := pred(OutputCount);
      while (result >= 0) and ((Outputs[result].MetaOutput = nil) or (Outputs[result].MetaOutput.Name <> name)) do
        dec( result );
    end;

  function TBlock.GetInputByName( name : string ) : TInput;
    var
      idx : integer;
    begin
      idx := InputIndex[name];
      if idx <> NoIndex
        then result := Inputs[idx]
        else result := nil;
    end;

  function TBlock.GetOutputByName( name : string ) : TOutput;
    var
      idx : integer;
    begin
      idx := OutputIndex[name];
      if idx <> NoIndex
        then result := Outputs[idx]
        else result := nil;
    end;

  function TBlock.GetInputCount : integer;
    begin
      result := fInputCount;
    end;

  function TBlock.GetOutputCount : integer;
    begin
      result := fOutputCount;
    end;

  function TBlock.GetSurfaceValue( SurfaceId : TSurfaceId ) : TSurfaceValue;
    begin
      result := 0;
    end;

  function TBlock.GetDt : TTimeDelta;
    begin
      if (Facility <> nil) and (Facility.Town <> nil)
        then result := Facility.Town.Timer.dt
        else result := 1;
    end;

  procedure TBlock.Simulate;
    begin
      case Evaluate of
        evrNormal :
          begin
            ResetInputs;
            AdvanceUpgrading;
          end;
        evrEvolve :
          Facility.BlockModified( bmEvolve );
      end;
    end;

  procedure TBlock.Stop;
    begin
    end;

  procedure TBlock.Resume;
    begin
    end;

  procedure TBlock.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
    begin
    end;

  function TBlock.Evaluate : TEvaluationResult;
                                                     
    procedure CollectCompanyInputs;
      var
        i     : integer;
        CI    : TCompanyInput;
        theDt : TTimeDelta;
      begin
        theDt := dt;
        for i := 0 to pred(MetaBlock.CompanyInputs.Count) do
          begin
            CI := Facility.Company.CompanyInput[TMetaCompanyInput(MetaBlock.CompanyInputs[i]).Fluid.UNId];
            if CI <> nil
              then
                begin
                  if theDt > 0
                    then CompanyInputs[i].Q := CompanyInputs[i].Max*realmin(1, CI.Ratio/theDt)
                    else CompanyInputs[i].Q := CompanyInputs[i].Max;
                  CompanyInputs[i].K := CI.Value.K;
                  CI.Demand := CI.Demand + CompanyInputs[i].Max;
                end   
              else CompanyInputs[i].Q := 0;
          end;
      end;

    procedure RequestCompSupport;

      function CountConnections : integer;
        var
          i : integer;
        begin
          result := 0;
          for i := 0 to pred(InputCount) do
            if mfTradeable in Inputs[i].MetaInput.MetaFluid.Options
              then inc( result, Inputs[i].ConnectionCount );
          for i := 0 to pred(OutputCount) do
            if mfTradeable in Outputs[i].MetaOutput.MetaFluid.Options
              then inc( result, Outputs[i].ConnectionCount );
        end;

      const
        CnxToSupport = 0.5;

      var
        count : integer;
      begin
        if Facility.CompanyDir <> nil
          then
            begin
              count := CountConnections;
              Facility.CompanyDir.Demand := Facility.CompanyDir.Demand + CnxToSupport*count;
            end;
      end;

    procedure UpdateOutputs;
      var
        i : integer;
      begin
        for i := 0 to pred(OutputCount) do
          Outputs[i].UpdateFluid;
      end;

    begin
      result := evrNormal;
      if not Facility.CriticalTrouble and (Facility.Company <> nil) and (Facility.Company.Owner <> nil)
        then Facility.Company.Owner.CurrFacPrestige := Facility.Company.Owner.CurrFacPrestige + realmax(1, Facility.UpgradeLevel)*MetaBlock.Prestige;
      if Facility.Company <> nil
        then CollectCompanyInputs;
      UpdateOutputs;
    end;

  function TBlock.ConnectTo( Block : TBlock; symetrical : boolean ) : string;
    var
      i, j      : integer;
      outputres : string;
      lang      : TLanguageId;
    begin
      if (Block.Facility <> nil) and (Block.Facility.Company <> nil) and (Block.Facility.Company.Owner <> nil)
        then lang := Block.Facility.Company.Owner.Language
        else lang := langDefault;
      result := '';
      for i := 0 to pred(InputCount) do
        for j := 0 to pred(Block.OutputCount) do
          if (mfTradeable in Inputs[i].MetaInput.MetaFluid.Options) and (Inputs[i].MetaInput.MetaFluid = Block.Outputs[j].MetaOutput.MetaFluid)
            then
              begin
                if result <> ''
                  then result := result + LineBreak;
                result := result + '   ' + Inputs[i].MetaInput.MetaFluid.Name_MLS.Values[lang] + ': ';
                case Inputs[i].ConnectTo( Block.Outputs[j] ) of
                  cnxValid :
                    result := result + mtidCnxHired.Values[lang];
                  cnxRejected :
                    result := result + mtidRejCompPol.Values[lang];
                  cnxDuplicated :
                    result := result + mtidAlreadyHired.Values[lang];
                  cnxForbiden :
                    result := result + mtidCnxNotAllowed.Values[lang];
                  cnxGateMaxed :
                    result := result + Format( mtidCnxLimited.Values[lang], [Inputs[i].MetaFluid.CnxLimit] )
                    //result + mtidCnxLimited 'cannot have more than ' + IntToStr(Inputs[i].MetaFluid.CnxLimit) + ' connections';
                end;
              end;
      if symetrical
        then
          begin
            if result <> ''
              then result := mtidCnxRepInputHead.Values[lang] + LineBreak + result;
            outputres := Block.ConnectTo( self, false );
            if outputres <> ''
              then result := result + LineBreak + mtidCnxRepOutputHead.Values[lang] + LineBreak + outputres;
            if result <> ''
              then result := mtidCnxHeader.Values[lang] + ' ' + Block.Facility.Name + LineBreak + result
              else result := '';
            CheckConnections( chkBoth );
          end;
    end;

  procedure TBlock.DisconnectFrom( Block : TBlock; Symetrical : boolean );
    var
      i, j : integer;
    begin
      for i := 0 to pred(InputCount) do
        for j := 0 to pred(Block.OutputCount) do
          if Inputs[i].MetaInput.MetaFluid = Block.Outputs[j].MetaOutput.MetaFluid
            then Inputs[i].DisconnectFrom( Block.Outputs[j] );
      if Symetrical
        then Block.DisconnectFrom( self, false );
    end;

  procedure TBlock.CollectInputs;
    var
      i     : integer;
      Input : TInput;
      WL    : IWorldLocator;
    begin
      //Input := nil;
      if Facility.Town <> nil
        then WL := Facility.Town.WorldLocator
        else WL := nil;
      for i := 0 to pred(InputCount) do
        try
          Input := Inputs[i];
          Input.Collect;
          if WL <> nil
            then WL.CountInputConnections(Input.ConnectionCount);
        except
          on E : Exception do
            try
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in Collect (' + IntToStr(xPos) + ',' + IntToStr(yPos) + ') ' + ' Input: ' + IntToStr(i) + ' Fac: ' + Facility.Name + ' ' + E.Message);
              //Input.AutoRepair(self, i);
            except
              //RebuildInput(i);
              Logs.Log(tidLog_Survival, TimeToStr(Now) + ' Error in Input.AutoRepair');
            end;
        end;
    end;

  procedure TBlock.CollectInputExtra;
    var
      i     : integer;
      Input : TInput;
    begin
      //Input := nil;
      for i := 0 to pred(InputCount) do
        try
          Input := Inputs[i];
          Input.CollectExtra;
          Input.FluidData.Q := realmax(0, Input.FluidData.Q);
        except
          on E : Exception do
            try
              Logs.log( tidlog_survival, TimeToStr(now) + ' error in collectinputextra (' + inttostr(xpos) + ',' + inttostr(ypos) + ') ' + ' input: ' + inttostr(i) + ' Fac: ' + Facility.Name + ' ' + E.Message);
              //Input.AutoRepair(self, i);
            except
              //RebuildInput(i);
              Logs.log( tidlog_survival, TimeToStr(now) + ' Error in Input.AutoRepair');
            end;
        end;
    end;

  procedure TBlock.SpreadOutputs;
    var
      i      : integer;
      Output : TOutput;
      WL     : IWorldLocator;
    begin
      //Output := nil;
      if Facility.Town <> nil
        then WL := Facility.Town.WorldLocator
        else WL := nil;
      for i := 0 to pred(OutputCount) do
        try
          Output := Outputs[i];
          Output.Spread;
          if WL <> nil
            then WL.CountOutputConnections(Output.ConnectionCount);
        except
          on E : Exception do
            try
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in SpreadOutputs (' + IntToStr(xPos) + ',' + IntToStr(yPos) + ') ' + ' Output: ' + IntToStr(i) + ' ObjId: ' + IntToStr(integer(Facility)) + ' ' + E.Message);
              //Output.AutoRepair(self, i);
            except
              //RebuildOutput(i);
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in Output.AutoRepair');
            end;
        end;
    end;

  procedure TBlock.SpreadOutputExtra;
    var
      i : integer;
      Output : TOutput;
    begin
      //Output := nil;
      for i := 0 to pred(OutputCount) do
        try
          Output := Outputs[i];
          Output.SpreadExtra;
        except
          on E : Exception do
            try
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in SpreadOutputExtra (' + IntToStr(xPos) + ',' + IntToStr(yPos) + ') ' + ' Output: ' + IntToStr(i) + ' ObjId: ' + IntToStr(integer(Facility)) + ' ' + E.Message);
              //Output.AutoRepair(self, i);
            except
              //RebuildOutput(i);
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in Output.AutoRepair');
            end;
        end;
    end;

  procedure TBlock.ResetInputs;
    var
      i : integer;
    begin
      for i := 0 to pred(InputCount) do
        try
          {$IFNDEF RELEASE}
          Inputs[i].fLastValue := PInputData(Inputs[i].FluidData)^;
          {$ENDIF}
          Inputs[i].FluidData.Q := 0;
        except
          on E : Exception do
            try
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in ResetInputs (' + IntToStr(xPos) + ',' + IntToStr(yPos) + ') ' + ' Input: ' + IntToStr(i) + ' ObjId: ' + IntToStr(integer(Facility)) + ' ' + E.Message);
            except
            end;
        end;
    end;

  procedure TBlock.AutoConnect( loaded : boolean );
    var
      i  : integer;
      x1 : integer;
      y1 : integer;
      x2 : integer;
      y2 : integer;
    begin
      if not loaded
        then
          begin
            if (Facility.Company <> nil) and (Facility.Company.Owner <> nil)
              then
                begin
                  if (Facility.Company.Owner.Level.Tier > 0) and (Role = rolProducer)
                    then fTradeLevel := tlvSameOnwner
                    else fTradeLevel := tlvAnyone;
                  Facility.Company.Owner.NewOwnedFacility( Facility );
                  Facility.Company.Owner.AutoConnectFacility( Facility );
                end;
          end;
      if Facility.Town <> nil
        then
          begin
            for i := 0 to pred(InputCount) do
              Inputs[i].AutoConnect( loaded );

            for i := 0 to pred(OutputCount) do
              Outputs[i].AutoConnect( loaded );

            x1 := Facility.xPos;
            y1 := Facility.yPos;
            x2 := x1 + pred(Facility.MetaFacility.XSize);
            y2 := y1 + pred(Facility.MetaFacility.YSize);
            if Facility.Town.WorldLocator.LandClassFound(x1, y1, x2, y2, lncZoneD)
              then Facility.fRequiresLandTech := true
              else Facility.fRequiresLandTech := false; // Silverio's programming wave :)
          end;
       // Report this new block will use researches
       if Facility.Company <> nil
         then Facility.Company.ReportUsage(MetaBlock.Inventions, MetaBlock.UsagePerInv, true);
    end;

  procedure TBlock.CheckCircuits;
    var
      Roads : TCollection;
      Area  : TRect;
    begin
      try
        Roads := Circuits[cirRoads];
        if Roads <> nil
          then
            begin
              Area := GetArea( 0, amdIncludeBlock );
              Roads.DeleteAll;
              if Facility.Town <> nil
                then Facility.Town.RoadHandler.NearestCircuitsToArea( cirRoads, Area, Roads );
            end;
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in CheckCircuits (' + IntToStr(xPos) + ',' + IntToStr(yPos) + ') ' + E.Message);
      end;
    end;

  procedure TBlock.CheckConnections( ConnectionCheckLevel : TConnectionCheckLevel );
    var
      i      : integer;
      Output : TOutput;
      Input  : TInput;
    begin
      //Output := nil;
      //Input  := nil;
      if (ConnectionCheckLevel = chkOutputs) or (ConnectionCheckLevel = chkBoth)
        then
          for i := 0 to pred(OutputCount) do
            try
              Output := Outputs[i];
              Output.CheckConnections;
            except
              on E : Exception do
                begin
                  try
                    Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in CheckConnections Output ' + IntToStr(i) + ' (' + IntToStr(xPos) + ', ' + IntToStr(yPos) + ') ' + E.Message);
                    //Output.AutoRepair(self, i);
                  except
                    //RebuildOutput(i);
                    Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in Output.AutoRepair');
                  end;
                end;
            end;
      if (ConnectionCheckLevel = chkInputs) or (ConnectionCheckLevel = chkBoth)
        then
          for i := 0 to pred(InputCount) do
            try
              Input := Inputs[i];
              Input.CheckConnections;
            except
              on E : Exception do
                begin
                  try
                    Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in CheckConnections Input ' + IntToStr(i) + ' (' + IntToStr(xPos) + ', ' + IntToStr(yPos) + ') ' + E.Message);
                    //Input.AutoRepair(self, i);
                  except
                    //RebuildInput(i);
                    Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in Input.AutoRepair');
                  end;
                end;
            end;
    end;

  procedure TBlock.SetCargoValue( CargoKind : TCargoKind; CargoValue : single );
    begin
    end;

  procedure TBlock.DelCargoValue( CargoKind : TCargoKind );
    begin
    end;

  function TBlock.GetCircuits( CircuitId : TCircuitId ) : TCollection;
    begin
      result := nil;
    end;

  function TBlock.GetVisualClassId : TVisualClassId;
    begin
      result := 0;                                  
    end;

  function TBlock.GetRole : TFacilityRole;
    begin
      if (Facility <> nil) and
         (Facility.MetaFacility <> nil) and
         (Facility.MetaFacility.Kind <> nil)
        then result := Facility.MetaFacility.Kind.Role
        else result := rolNeutral;
    end;
    
  procedure TBlock.RefreshVisualClassInfo;
    var
      NewVisualClass : TVisualClassId;
    begin
      NewVisualClass := GetVisualClassId;
      if fVisualClass <> NewVisualClass
        then
          begin
            fVisualClass := NewVisualClass;
            Facility.BlockModified( bmVisualChange );
          end;
    end;

  procedure TBlock.StoreLinksToCache( Cache : TObjectCache );
    var
      i : integer;
    begin
      with Cache do
        begin
          for i := 0 to pred(InputCount) do
            if mgoptCacheable in Inputs[i].MetaInput.Options
              then
                try
                  CacheObject( Inputs[i], noKind, noInfo ); //>>
                except
                  // >>
                end;
          for i := 0 to pred(OutputCount) do
            if mgoptCacheable in Outputs[i].MetaOutput.Options
              then
                try
                  CacheObject( Outputs[i], noKind, noInfo );
                except
                  // >>
                end;
        end;
    end;

  procedure TBlock.StoreToCache( Cache : TObjectCache );
    var
      i      : integer;
      cnt    : integer;
      map    : array[0..512] of char;
      iStr   : string;
      Input  : TInput;
      Output : TOutput;
      CI     : PCompanyInputData;
    begin
      with Cache do
        begin
          cnt := 0;
          for i := 0 to pred(InputCount) do
            begin
              Input := Inputs[i];
              if mgoptCacheable in Input.MetaInput.Options
                then
                  begin
                    if Input.IsActive
                      then map[cnt] := '1'
                      else map[cnt] := '0';
                    iStr := IntToStr(cnt);
                    StoreMultiStringToCache('Input' + iStr + '.', Input.MetaInput.MetaFluid.Name_MLS, Cache);
                    WriteString('InputPath' + iStr, GetObjectPath(Input, noKind, noInfo));
                    inc(cnt);
                  end;
            end;
          map[cnt] := #0;
          WriteInteger('InputCount', cnt);
          WriteString('GateMap', map);
          cnt := 0;
          for i := 0 to pred(OutputCount) do
            begin
              Output := Outputs[i];
              if mgoptCacheable in Output.MetaOutput.Options
                then
                  begin
                    iStr := IntToStr(cnt);
                    //WriteString('Output' + IntToStr(cnt), Outputs[i].MetaOutput.MetaFluid.Name_MLS.Values[langDefault]); // >> MLS2
                    StoreMultiStringToCache('Output' + iStr + '.', Output.MetaOutput.MetaFluid.Name_MLS, Cache);
                    WriteString('OutputPath' + iStr, GetObjectPath(Output, noKind, noInfo));
                    inc(cnt);
                  end;
            end;
          WriteInteger('OutputCount', cnt);
          // Company Inputs
          WriteInteger('cInputCount', MetaBlock.CompanyInputs.Count);
          for i := 0 to pred(MetaBlock.CompanyInputs.Count) do
            begin
              CI   := CompanyInputs[i];
              iStr := IntToStr(i);
              //WriteString('cInput' + iStr, TMetaCompanyInput(MetaBlock.CompanyInputs[i]).Fluid.Name_MLS.Values[langDefault] ); // >> MLS2
              StoreMultiStringToCache('cInput' + iStr + '.', TMetaCompanyInput(MetaBlock.CompanyInputs[i]).Fluid.Name_MLS, Cache);
              WriteInteger('cInputDem' + iStr, ceil(TMetaCompanyInput(MetaBlock.CompanyInputs[i]).Fluid.ConvertToUnits(CI.Max)));
              WriteInteger('cInputSup' + iStr, ceil(TMetaCompanyInput(MetaBlock.CompanyInputs[i]).Fluid.ConvertToUnits(CI.Q)));
              WriteInteger('cInputK' + iStr, CI.K);
              if (CI.Max > 0) and (CI.Q > 0)
                then WriteInteger('cInputRatio' + iStr, min(100, round(100*smartround(CI.Q)/smartround(CI.Max))))
                else WriteInteger('cInputRatio' + iStr, 0);
              if TMetaCompanyInput(MetaBlock.CompanyInputs[i]).Editable
                then
                  begin
                    WriteString('cEditable' + iStr, 'yes');
                    WriteFloat('cInputMax' + iStr, round(TMetaCompanyInput(MetaBlock.CompanyInputs[i]).Fluid.ConvertToUnits((1 + (UpgradeLevel - 1)/10)*TMetaCompanyInput(MetaBlock.CompanyInputs[i]).Max)));
                  end
                else WriteFloat('cInputMax' + iStr, round(TMetaCompanyInput(MetaBlock.CompanyInputs[i]).Fluid.ConvertToUnits(UpgradeLevel*TMetaCompanyInput(MetaBlock.CompanyInputs[i]).Max)));
              StoreMultiStringToCache( 'cUnits' + iStr + '.', TMetaCompanyInput(MetaBlock.CompanyInputs[i]).Fluid.UnitName_MLS, Cache );
            end;
          WriteInteger( 'TradeRole', integer(Role) );
          WriteInteger( 'TradeLevel', integer(TradeLevel) );

          WriteInteger('UpgradeLevel', UpgradeLevel);
          WriteInteger('Upgrading', UpgradeHours);
          WriteInteger('Pending', PendingUpgrades);
          WriteInteger('MaxUpgrade', GetActualMaxUpgrade);
          WriteCurrency('NextUpgCost', UpgradeCost);

          for i := 0 to pred(Languages.LangList.Count) do
            WriteString('CloneMenu' + LangList[i], RenderCloneMenu(LangList[i]));
        end;
    end;

  procedure TBlock.RemoveFromCache(background : boolean);
    var
      i : integer;
    begin
      for i := 0 to pred(InputCount) do
        if mgoptCacheable in Inputs[i].MetaInput.Options
          then
            if background
              then ModelServerCache.BackgroundUncache(Inputs[i])
              else ModelServerCache.UnCacheObject(Inputs[i], noKind, noInfo);
      for i := 0 to pred(OutputCount) do
        if mgoptCacheable in Outputs[i].MetaOutput.Options
          then
            if background
              then ModelServerCache.BackgroundUncache(Outputs[i])
              else ModelServerCache.UnCacheObject(Outputs[i], noKind, noInfo);
    end;

  procedure TBlock.CopySettingsFrom(Block : TBlock; Options : integer);
    var
      i, j    : integer;
      InputA  : TPullInput;
      InputB  : TPullInput;
      OutputA : TPullOutput;
      OutputB : TPullOutput;
      cnxMod  : boolean;
    begin
      if ObjectIs(ClassName, Block)
        then
          begin
            cnxMod := false;
            if Options and cloneOption_Suppliers <> 0
              then
                for i := 0 to pred(InputCount) do
                  if (mfTradeable in Inputs[i].MetaInput.MetaFluid.Options) and ObjectIs( TPullInput.ClassName, Inputs[i] )
                    then
                      begin
                        InputB := TPullInput(Block.InputsByName[Inputs[i].MetaInput.Name]);
                        if (InputB <> nil) and ObjectIs( TPullInput.ClassName, InputB )
                          then
                            begin
                              InputA := TPullInput(Inputs[i]);
                              InputA.MaxPrice := InputB.MaxPrice;
                              InputA.MinK     := InputB.MinK;
                              InputA.DisconnectAll;
                              for j := 0 to pred(InputB.ConnectionCount) do
                                InputA.ConnectTo( InputB.Connections[j] );
                              cnxMod := true;
                            end;
                      end;
            if Options and cloneOption_Clients <> 0
              then
                for i := 0 to pred(OutputCount) do
                  if (mfTradeable in Outputs[i].MetaOutput.MetaFluid.Options) and ObjectIs( TPullOutput.ClassName, Outputs[i] )
                    then
                      begin
                        OutputB := TPullOutput(Block.OutputsByName[Outputs[i].MetaOutput.Name]);
                        if (OutputB <> nil) and ObjectIs( TPullOutput.ClassName, OutputB )
                          then
                            begin
                              OutputA := TPullOutput(Outputs[i]);
                              OutputA.DisconnectAll;
                              for j := 0 to pred(OutputB.ConnectionCount) do
                                OutputA.ConnectTo( OutputB.Connections[j] );
                              cnxMod := true;
                            end;
                      end;
            if cnxMod
              then CheckConnections( chkBoth );
          end
        else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Trying to clone two different kind of blocks.' );
    end;

  function TBlock.RenderCloneMenu(lang : string) : string;
    begin
      result := '';
    end;

  procedure TBlock.LoadFromBackup( Reader : IBackupReader );

    procedure LoadCompInputs;
      var
        count  : integer;
        list   : TStringList;
        id     : string;
        max    : TFluidValue;
        data   : PCompanyInputData;
        i, idx : integer;
      begin
        getmem( fCompanyInputArray, MetaBlock.CompanyInputs.Count*sizeof(TCompanyInputData) );
        fillchar( fCompanyInputArray^, MetaBlock.CompanyInputs.Count*sizeof(TCompanyInputData), 0 );

        count := Reader.ReadInteger( 'CompInpCount', 0 );
        if count > 0
          then
            begin
              list := TStringList.Create;
              try
                for i := 0 to pred(count) do
                  begin
                    id  := Reader.ReadString( 'CompInpId' + IntToStr(i), '' );
                    max := Reader.ReadSingle( 'CompInpMax' + IntToStr(i), 0 );
                    new( data );
                    data.Max := max;
                    list.AddObject( id, TObject(data) );
                  end;
                for i := 0 to pred(MetaBlock.CompanyInputs.Count) do
                  begin
                    idx := list.IndexOf( TMetaCompanyInput(MetaBlock.CompanyInputs[i]).Fluid.Id );
                    if idx <> NoIndex
                      then fCompanyInputArray[i].Max := PCompanyInputData(list.Objects[idx]).Max;
                  end;
              finally
                for i := 0 to pred(count) do
                  dispose( PCompanyInputData(list.Objects[i]) );
                list.Free;
              end;
            end;
      end;

    var
      i : integer;
    begin
      inherited;
      fMetaBlock := TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, Reader.ReadString( 'MetaBlock', '' )]);
      Reader.ReadObject( 'Facility', fFacility, nil );

      fInputCount := Reader.ReadInteger( 'InputCount', 0 );
      getmem( fInputs, fInputCount*sizeof(fInputs[0]) );
      for i := 0 to pred(InputCount) do
        Reader.ReadObject( 'Input' + IntToStr(i), fInputs[i], nil );

      fOutputCount := Reader.ReadInteger( 'OutputCount', 0 );
      getmem( fOutputs, fOutputCount*sizeof(fOutputs[0]) );
      for i := 0 to pred(OutputCount) do
        Reader.ReadObject( 'Output' + IntToStr(i), fOutputs[i], nil );

      fTradeLevel := TTradeLevel(Reader.ReadByte( 'TradeLevel', byte(tlvAnyone) ));
      fAcceptCloning := Reader.ReadBoolean( 'AcceptCloning', true );

      LoadCompInputs;

      fUpgradeLevel := Reader.ReadByte( 'UpgradeLevel', 1 );
      fUpgradeHours := Reader.ReadByte( 'UpgradeHours', 0 );
    end;

  procedure TBlock.StoreToBackup( Writer : IBackupWriter );

    procedure StoreCompInputs;
      var
        i   : integer;
        aux : string;
      begin
        Writer.WriteInteger( 'CompInpCount', MetaBlock.CompanyInputs.Count );
        for i := 0 to pred(MetaBlock.CompanyInputs.Count) do
          begin
            aux := 'CompInpId' + IntToStr(i);
            Writer.WriteString( aux, TMetaCompanyInput(MetaBlock.CompanyInputs[i]).Fluid.Id );
            aux := 'CompInpMax' + IntToStr(i);
            Writer.WriteSingle( aux, CompanyInputs[i].Max );
          end;
      end;

    var
      i      : integer;
      Input  : TInput;
      Output : TOutput;
      aux    : string;

    begin
      inherited;
      Writer.WriteString( 'MetaBlock', fMetaBlock.Id );
      Writer.WriteObjectRef( 'Facility', Facility );
      Writer.WriteInteger( 'InputCount', InputCount );
      for i := 0 to pred(InputCount) do
        begin
          aux := 'Input' + IntToStr(i);
          Input := Inputs[i];
          if fMetaBlock.CnntRepairable
            then Writer.WriteUnsafeObject(aux, Input)
            else Writer.WriteObject(aux, Input);
        end;
      Writer.WriteInteger( 'OutputCount', OutputCount );
      for i := 0 to pred(OutputCount) do
        begin
          aux := 'Output' + IntToStr(i);
          Output := Outputs[i];
          if fMetaBlock.CnntRepairable
            then Writer.WriteUnsafeObject(aux, Output)
            else Writer.WriteObject(aux, Output);
        end;
      aux := '';
      Writer.WriteByte( 'TradeLevel', byte(fTradeLevel) );
      Writer.WriteBoolean( 'AcceptCloning', fAcceptCloning );
      StoreCompInputs;
      Writer.WriteByte( 'UpgradeLevel', fUpgradeLevel );
      Writer.WriteByte( 'UpgradeHours', fUpgradeHours );
    end;

  procedure TBlock.BlockModified( Modification : TBlockModification );
    begin
      if Facility <> nil
        then Facility.BlockModified( Modification );
    end;

  procedure TBlock.BlockLoaded;

    procedure CheckConnections( Gate : TGate; SuperClass : CGate );
      var
        i : integer;
      begin
        Gate.fConnections.Pack;
        for i := pred(Gate.fConnections.Count) downto 0 do
          if not MetaInstances.ObjectIs( SuperClass.ClassName, Gate.fConnections[i] )
            then
              begin
                try
                  Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Fixing Model Inconsistency ' + Gate.fConnections[i].ClassName + ' at ' + Gate.ClassName );
                except
                  Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Fixing Model Inconsistency!' );
                end;
                Gate.fConnections.AtExtract( i );
              end;
      end;

    procedure FixInputs;
      var
        i, j      : integer;
        NewArray  : PGateArray;
        MetaInput : TMetaInput;
        Input     : TInput;
        NewInput  : TInput;
        DeadMeat  : TCollection;
      begin
        DeadMeat := nil;
        try
          // First remove connections held by outdated inputs
          for i := 0 to pred(InputCount) do
            begin
              Input := Inputs[i];
              if (Input <> nil) and ((Input.MetaInput = nil) or (Input.MetaInput.GateClass.ClassName <> Input.ClassName))
                then
                  begin
                    // >> Input.DisconnectAll;
                    if DeadMeat = nil
                      then DeadMeat := TCollection.Create( 0, rkBelonguer );
                    DeadMeat.Insert(Input);
                    // >> Input.fMetaInput := nil; // >> dead sentence
                  end
                else
                  if Input = nil
                    then
                      begin
                        MetaInput := TMetaInput(MetaBlock.MetaInputs[i]);
                        fInputs[i] := TInput(MetaInput.Instantiate( self ));
                      end;
            end;

          getmem( NewArray, MetaBlock.MetaInputs.Count*sizeof(NewArray[0]) );
          for i := 0 to pred(MetaBlock.MetaInputs.Count) do
            begin
              MetaInput := TMetaInput(MetaBlock.MetaInputs[i]);
              Input := InputsByName[MetaInput.Name];
              if (Input = nil) or ((DeadMeat <> nil) and (DeadMeat.IndexOf(Input) <> noIndex))
                then
                  begin
                    NewInput := TInput(MetaInput.Instantiate( self ));
                    if Input <> nil
                      then
                        begin
                          for j := 0 to pred(Input.ConnectionCount) do
                            NewInput.ConnectTo(Input.Connections[j]);
                          Input.DisconnectAll;
                        end;
                  end
                else NewInput := Input;
              NewArray[i] := NewInput;
              if (NewInput <> Input) and (Role in [rolDistributer, rolCompExport, rolCompInport]) and ObjectIs('TPullInput', NewInput)
                then TPullInput(NewInput).Selected := false;
            end;

          // Set the new array
          freemem( fInputs, InputCount*sizeof(fInputs[0]) );
          fInputs := NewArray;

          fInputCount := MetaBlock.MetaInputs.Count;
        finally
          if DeadMeat <> nil
            then DeadMeat.Free;
        end;
        for i := 0 to pred(InputCount) do
          CheckConnections( Inputs[i], TOutput );
      end;

    procedure FixOutputs;
      var
        i, j       : integer;
        NewArray   : PGateArray;
        MetaOutput : TMetaOutput;
        Output     : TOutput;
        NewOutput  : TOutput;
        DeadMeat   : TCollection;
      begin
        DeadMeat := nil;
        try
          // First remove connections held by outdated Outputs
          for i := 0 to pred(OutputCount) do
            begin
              Output := Outputs[i];
              if (Output <> nil) and ((Output.MetaOutput = nil) or (Output.MetaOutput.GateClass.ClassName <> Output.ClassName))
                then
                  begin
                    // >> Outputs[i].DisconnectAll;
                    if DeadMeat = nil
                      then DeadMeat := TCollection.Create(0, rkBelonguer);
                    DeadMeat.Insert(Output);
                  end
                else
                  if Output = nil
                    then
                      begin
                        MetaOutput := TMetaOutput(MetaBlock.MetaOutputs[i]);
                        fOutputs[i] := TOutput(MetaOutput.Instantiate( self ));
                      end;
            end;
          // Create new Output array
          getmem( NewArray, MetaBlock.MetaOutputs.Count*sizeof(NewArray[0]) );
          for i := 0 to pred(MetaBlock.MetaOutputs.Count) do
            begin
              MetaOutput := TMetaOutput(MetaBlock.MetaOutputs[i]);
              Output     := OutputsByName[MetaOutput.Name];
              if (Output = nil) or ((DeadMeat <> nil) and (DeadMeat.IndexOf(Output) <> noIndex))
                then
                  begin
                    NewOutput := TOutput(MetaOutput.Instantiate(self));
                    if Output <> nil
                      then
                        begin
                          for j := 0 to pred(Output.ConnectionCount) do
                            NewOutput.ConnectTo(Output.Connections[j]);
                          Output.DisconnectAll;
                        end;
                  end
                else Newoutput := Output;
              NewArray[i] := NewOutput;
            end;
          // Set the new array
          freemem( fOutputs, OutputCount*sizeof(fOutputs[0]) );
          fOutputs     := NewArray;
          fOutputCount := MetaBlock.MetaOutputs.Count;
        finally
          if DeadMeat <> nil
            then DeadMeat.Free;
        end;
        for i := 0 to pred(OutputCount) do
          CheckConnections( Outputs[i], TInput );
      end;

    var
      i : integer;
    begin
      FixInputs;
      FixOutputs;
      for i := 0 to pred(InputCount) do
        Inputs[i].Loaded;
      for i := 0 to pred(OutputCount) do
        Outputs[i].Loaded;
      AutoConnect( true );
      fUpgradeLevel := min(MetaBlock.MaxUpgrade, max(1, fUpgradeLevel));
    end;

  procedure TBlock.Deleted;
    var
      i : integer;
    begin
      try
        for i := 0 to pred(InputCount) do
          if (mfCompanyFluid in Inputs[i].MetaInput.MetaFluid.Options) and (Facility <> nil) and (Facility.Company <> nil)
            then Facility.Company.UnregisterInput( Inputs[i] );
        for i := 0 to pred(InputCount) do
          Inputs[i].DisconnectAll;
        for i := 0 to pred(OutputCount) do
          Outputs[i].DisconnectAll;
        if Facility.Company <> nil
          then Facility.Company.ReportUsage(MetaBlock.Inventions, MetaBlock.UsagePerInv, false);
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in block deleted.. ');
      end;
      inherited;
    end;
                       
  procedure TBlock.BlockGenMoney( Money : TMoney; Reason : TMoneyReason );
    begin
      if Facility <> nil
        then Facility.GenMoney( Money, Reason );
    end;

  function TBlock.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    begin
      result := '';
    end;

  function TBlock.GetArea( Ratio : integer; mode : TAreaMode ) : TRect;
    begin
      case mode of
        amdExcludeBlock :
          result := Rect(
            xPos + Facility.MetaFacility.xSize div 2 - Ratio,
            yPos + Facility.MetaFacility.ySize div 2 - Ratio,
            xPos + Facility.MetaFacility.xSize div 2 + Ratio,
            yPos + Facility.MetaFacility.ySize div 2 + Ratio );
        else
          result := Rect(
            xPos - Ratio,
            yPos - Ratio,
            xPos + Facility.MetaFacility.xSize + Ratio,
            yPos + Facility.MetaFacility.ySize + Ratio );              
      end;
    end;

  procedure TBlock.FacNameChanged;
    begin
    end;

  function TBlock.GetWeatherEnvelopValue( idx : integer ) : single;
    begin
      if (MetaBlock.WeatherEnvelopes <> nil) and (idx < MetaBlock.WeatherEnvelopes.Count)
        then result := TWeatherEnvelope(MetaBlock.WeatherEnvelopes[idx]).CurrValue
        else result := 1;
    end;
    
  function TBlock.GetCompanyInputCount : integer;
    begin
      result := MetaBlock.CompanyInputs.Count;
    end;

  function TBlock.GetCompanyInput( index : integer ) : PCompanyInputData;
    begin
      result := @fCompanyInputArray[index];
    end;

  function TBlock.Offset( var Field ) : TOffset;
    begin
      result := integer(@Field) - integer(@fOffsetRef)
    end;

  procedure TBlock.CleanExtraInfo;
    //var
      //ExtraInfo : PExtraConnectionInfo;
      //i, j      : integer;
    begin
      {for i := 0 to pred(InputCount) do
        for j := 0 to pred(Inputs[i].ConnectionCount) do
          begin
            ExtraInfo := Inputs[i].ExtraConnectionInfo[j];
            if ExtraInfo <> nil
              then
                begin
                  ExtraInfo.YearValue := 0;
                  ExtraInfo.YearCost  := 0;
                end;
          end;}
    end;

  procedure TBlock.RDOSetCompanyInputDemand(index, perc : integer);
    var
      value : TFluidValue;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'SetCompanyInputDemand' );
      if Facility.CheckOpAuthenticity
        then
          try
            if (index >= 0) and (index < CompanyInputCount)
              then
                begin
                  value := (1 + (realmax(1, UpgradeLevel) - 1)/10)*TMetaCompanyInput(MetaBlock.CompanyInputs[index]).Max;
                  CompanyInputs[index].Max := (min(perc, 100)/100)*value;
                  ModelServerCache.InvalidateCache(Facility, false);
                end;
          except
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in SetCompanyInputDemand' );
          end;
    end;

  procedure TBlock.SetTradeLevel(aTradeLevel : TTradeLevel);
    var
      i : integer;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'SetTradeLevel' );
      try
        fTradeLevel := aTradeLevel;
        for i := 0 to pred(InputCount) do
          Inputs[i].SortConnections;
        for i := 0 to pred(OutputCount) do
          Outputs[i].SortConnections;
        ModelServerCache.BackgroundInvalidateCache(Facility); //UpdateCache(true)
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in SetTradeLevel..' );
      end;
    end;

  procedure TBlock.RDOSetTradeLevel(aTradeLevel : integer);
    begin
      if Facility.CheckOpAuthenticity
        then SetTradeLevel(TTradeLevel(aTradeLevel));
    end;

  {procedure TBlock.RebuildInput(index : integer);
    var
      MetaInput : TMetaInput;
    begin
      if MetaBlock.CnntRepairable
        then
          begin
            MetaInput := TMetaInput(MetaBlock.MetaInputs[index]);
            fInputs[index] := MetaInput.Instantiate(self);
          end;
    end;}

  {procedure TBlock.RebuildOutput(index : integer);
    var
      MetaOutput : TMetaOutput;
    begin
      if MetaBlock.CnntRepairable
        then
          begin
            MetaOutput := TMetaOutput(MetaBlock.MetaOutputs[index]);
            fOutputs[index] := MetaOutput.Instantiate(self);
          end;
    end;}

  function TBlock.GetUpgrading : boolean;
    begin
      result := (fUpgradeHours > 0) and (fUpgradeHours <= 100);
    end;

  function TBlock.GetUpgradePerc : TPercent;
    begin
      result := round(100*pred(fUpgradeHours)/Facility.MetaFacility.UpgradeTime);
    end;

  function TBlock.GetUpgradeCost : TMoney;
    begin
      result := Facility.MetaFacility.UpgradeCost;
    end;

  function TBlock.GetActualMaxUpgrade : integer;
    begin
      result := MetaBlock.MaxUpgrade;
    end;

  function TBlock.GetUpgradeHourCost : TMoney;
    begin
      result := GetUpgradeCost/Facility.MetaFacility.UpgradeTime;
    end;

  procedure TBlock.AdvanceUpgrading;
    var
      upgCost : TMoney;
      Tycoon  : TTycoon;
      delta   : integer;
      Msg     : TMsgFacUpgraded;
    begin
      try
        if not Facility.CriticalTrouble and (fUpgradeHours > 0) and (Facility.Company <> nil) and (not Facility.Company.Owner.CountUpgrades or not Facility.Company.Owner.ReachedBuildLimit)
          then
            begin
              // Get the correct number of hours
              if Facility.Town.WorldLocator.GetHoursADay <> 0
                then delta := max(0, round(24/Facility.Town.WorldLocator.GetHoursADay))
                else delta := 1;

              // Correct the delta
              if fUpgradeHours + delta > Facility.MetaFacility.UpgradeTime
                then delta := Facility.MetaFacility.UpgradeTime - fUpgradeHours;

              inc(fUpgradeHours, delta);
              upgCost := delta*UpgradeHourCost; //Facility.MetaFacility.UpgradeHourCost;

              Facility.fCost   := Facility.fCost   + upgCost;
              Facility.fTCCost := Facility.fTCCost + upgCost;
              Facility.Company.GenMoney(-upgCost, accIdx_Construction);

              if fUpgradeHours >= Facility.MetaFacility.UpgradeTime
                then
                  begin
                    fPendingUpgs := max(0, fPendingUpgs - 1);
                    if (fPendingUpgs > 0) and (fUpgradeLevel + 1 < MetaBlock.MaxUpgrade)
                      then fUpgradeHours := 1
                      else fUpgradeHours := 0;
                    Tycoon := Facility.Company.Owner;
                    Tycoon.CountFacility(Facility, false);
                    inc(fUpgradeLevel);
                    Tycoon.CountFacility(Facility, true);

                    ModelServerCache.UpdateObjectCache(Facility, noKind, noInfo); // >> ???

                    Facility.Town.MapRefresh.RefeshFacility(Facility, fchStructure);
                    Facility.Town.MapRefresh.RefreshArea(xPos, yPos, Facility.MetaFacility.xSize, Facility.MetaFacility.ySize);

                    Msg.Msg := msgKernel_FacUpgraded;
                    Msg.Fac := Facility;
                    try
                      Facility.Company.Owner.Dispatch(Msg);
                    except
                      // >> log errors
                    end;
                  end;
            end;
      except
        Logs.Log( 'Survival', TimeToStr(Now) + ' Error in AdvanceUpgrading..');
      end;
    end;

  procedure TBlock.StartUpgrading(count : integer);
    var
      upgCost : TMoney;
    begin
      if (fUpgradeHours = 0) and (fUpgradeLevel < MetaBlock.MaxUpgrade)
        then
          begin
            fPendingUpgs  := max(0, min(MetaBlock.MaxUpgrade - fUpgradeLevel, count)); // count - 1
            fUpgradeHours := 1;
            upgCost := UpgradeHourCost; //Facility.MetaFacility.UpgradeHourCost;
            Facility.fCost   := Facility.fCost   + upgCost;
            Facility.fTCCost := Facility.fTCCost + upgCost; // New!!
            Facility.Company.GenMoney(-upgCost, accIdx_Construction);

            ModelServerCache.UpdateObjectCache(Facility, noKind, noInfo); //ModelServerCache.BackgroundInvalidateCache(Facility); //ModelServerCache.UpdateObjectCache(Facility, noKind, noInfo)
            Facility.Town.MapRefresh.RefeshFacility(Facility, fchStructure);
            Facility.Town.MapRefresh.RefreshArea(xPos, yPos, Facility.MetaFacility.xSize, Facility.MetaFacility.ySize);
          end;
    end;

  procedure TBlock.StopUpgrading;
    var
      upgCost : TMoney;
    begin
      try
        if (fUpgradeHours > 0) and (fUpgradeHours <= Facility.MetaFacility.UpgradeTime)
          then
            begin
              upgCost := fUpgradeHours*UpgradeHourCost; //Facility.MetaFacility.UpgradeHourCost;
              Facility.fCost   := realmax(0, Facility.fCost - upgCost);
              Facility.fTCCost := realmax(0, Facility.fTCCost - upgCost); // New!!
              Facility.Company.GenMoney(upgCost, accIdx_Compensations);

              ModelServerCache.UpdateObjectCache(Facility, noKind, noInfo); //ModelServerCache.BackgroundInvalidateCache(Facility); //ModelServerCache.UpdateObjectCache(Facility, noKind, noInfo)
              Facility.Town.MapRefresh.RefeshFacility(Facility, fchStructure);
              Facility.Town.MapRefresh.RefreshArea(xPos, yPos, Facility.MetaFacility.xSize, Facility.MetaFacility.ySize);
            end;
      finally
        fPendingUpgs  := 0;
        fUpgradeHours := 0;
      end;
    end;

  procedure TBlock.Downgrade(count : integer);
    var
      Tycoon  : TTycoon;
      upgCost : TMoney;
    begin
      if not Upgrading and (fUpgradeLevel > 1) and (Facility.Company <> nil)
        then
          begin
            Tycoon := Facility.Company.Owner;
            if Tycoon <> nil
              then
                begin
                  upgCost := realmax(0, count*Facility.MetaFacility.UpgradeCost);
                  Facility.fCost   := realmax(0, Facility.fCost   - upgCost);
                  Facility.fTCCost := realmax(0, Facility.fTCCost - upgCost); // New!!
                  Tycoon.CountFacility(Facility, false);
                  fUpgradeLevel := max(1, fUpgradeLevel - count);
                  Tycoon.CountFacility(Facility, true);
                  Facility.Company.GenMoney(Tycoon.Level.MoneyBackOnDemolish*upgCost, accIdx_Compensations);

                  ModelServerCache.UpdateObjectCache(Facility, noKind, noInfo);
                  Facility.Town.MapRefresh.RefeshFacility(Facility, fchStructure);
                  Facility.Town.MapRefresh.RefreshArea(xPos, yPos, Facility.MetaFacility.xSize, Facility.MetaFacility.ySize);
                end;
          end;
    end;

  procedure TBlock.RecalculateInventionsEffect;
    begin
    end;

  function TBlock.UsesInvention(Invention : TInvention) : boolean;
    begin
      result := MetaBlock.UsesInvention(Invention);
    end;

  procedure TBlock.ReportInventions;
    begin
      // >>
    end;

  // TGate

  constructor TGate.Create( aMetaGate : TMetaGate; aBlock : TBlock );
    begin
      inherited Create;
      fBlock       := aBlock;
      fConnections := BestCollection( aMetaGate.Size );
      fOptions     := goptCost;
      HookFluidData( aMetaGate );
    end;

  destructor TGate.Destroy;
    var
      i : integer;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      for i := 0 to pred(fConnections.Count) do
        TGate(fConnections[i]).fConnections.Delete( self );
      fConnections.Free;
      inherited;
    end;

  function TGate.GetConnectionCount : integer;
    begin
      result := fConnections.Count;
    end;

  function TGate.GetLastCost : TMoney;
    begin
      result := 0;
    end;

  function TGate.GetLastFluid : TFluidValue;
    begin
      result := FluidData.Q;
    end;

  procedure TGate.SetLastCost( aLastCost : TMoney );
    begin
    end;

  procedure TGate.SetFluidData( aFluidData : PFluidData );
    begin
      fFluidData := aFluidData;
    end;

  procedure TGate.InsertConnection( Connection : TGate );
    begin
      fConnections.Insert( Connection );
    end;

  procedure TGate.DeleteConnection( Connection : TGate );
    begin
      fConnections.Delete( Connection );
    end;

  procedure TGate.ConnectionChanged( Connection : TGate );
    begin
      if mfTradeable in MetaFluid.Options
        then
          if ConnectionAllowedByPolicy( Connection ) and
            (ConnectionAllowed( Connection ) = cnxValid) and
            (Connection.ConnectionAllowed( self ) = cnxValid)
            then
              begin
                fConnections.Extract( Connection );
                InsertConnection( Connection );
              end
            else DisconnectFrom( Connection );
    end;

  function TGate.GetUniversalPrecedence( Connection : TGate ) : integer;
    var
      Status   : TPolicyStatus;
      priority : integer;
    begin
      if (Block.Facility.Company <> nil) and (Block.Facility.Company.Owner <> nil) and
         (Connection.Block.Facility.Company <> nil) and (Connection.Block.Facility.Company.Owner <> nil)
        then
          if Block.Facility.Company.Owner = Connection.Block.Facility.Company.Owner
            then result := cprSameOwner
            else
              begin
                if Block.Facility.Company.Owner.Level <> nil
                  then priority := Block.Facility.Company.Owner.Level.Priority
                  else priority := cprNormal;
                if priority <> cprNewbieA
                  then
                    begin
                      Status := Block.Facility.Company.Owner.Policy[Connection.Block.Facility.Company.Owner];
                      if Status = pstAlly
                        then result := cprAlly
                        else result := priority;
                    end
                  else result := cprNewbieA;
              end
        else result := cprNormal;
    end;

  const
    PriceRange = 400 + 400; //400% max price plus 400% of overpricing

  function TGate.GetConnectionPrecedence( Connection : TGate ) : integer;
    var
      level    : integer;
    begin
      level  := GetUniversalPrecedence(Connection);
      result := PriceRange*level;
    end;

  function TGate.ConnectionAllowed( Connection : TGate ) : TConnectResult;
    begin
      {
      if ((MetaFluid = nil) or
         (MetaFluid.CnxLimit <= 0) or
         (Block.Facility = nil) or
         (Block.Facility.MetaFacility = nil) or
         (Block.Facility.MetaFacility.Kind = nil) or
         not (mfTradeable in MetaFluid.Options) or
         (Block.Role in [rolDistributer, rolCompExport, rolCompInport]) or
         (Block.Facility.Company = nil) or
         (Block.Facility.Company.Owner = nil) or
         (Connection.Block.Role in [rolDistributer, rolCompExport, rolCompInport]) or
         (fConnections.Count <= MetaFluid.CnxLimit)) and
         (CnxPermissionMap[GetOwnerPermissionMapId][Block.Role, Connection.Block.Role] <> 0)
        then result := cnxValid
        else result := cnxGateMaxed;
      }
      // >> >> Change this to enforce the Limit of Connections
      if (MetaFluid = nil) or
         (MetaFluid.CnxLimit <= 0) or
         (Block.Facility = nil) or
         (Block.Facility.MetaFacility = nil) or
         (Block.Facility.MetaFacility.Kind = nil) or
         not (mfTradeable in MetaFluid.Options) or
         (Block.Role in [rolDistributer, rolCompExport, rolCompInport]) or
         (Block.Facility.Company = nil) or
         (Block.Facility.Company.Owner = nil) or
         (Connection.Block.Role in [rolDistributer, rolCompExport, rolCompInport]) or
         (fConnections.Count <= MetaFluid.CnxLimit) or
         (CnxPermissionMap[GetOwnerPermissionMapId][Block.Role, Connection.Block.Role] = 0)
        then result := cnxValid
        else result := cnxGateMaxed;
    end;

  function TGate.GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo;
    begin
      result := nil;
    end;

  procedure TGate.CheckConnections;             
    begin
    end;

  function TGate.ConnectTo( Gate : TGate ) : TConnectResult;
    begin
      if (MetaFluid = nil) or
         not (mfTradeable in MetaFluid.Options) or
         (CnxPermissionMap[GetOwnerPermissionMapId][Block.Role, Gate.Block.Role] <> 0) and
         (CnxPermissionMap[Gate.GetOwnerPermissionMapId][Block.Role, Gate.Block.Role] <> 0)
        then
          if (MetaFluid = nil) or
             not (mfTradeable in MetaFluid.Options) or
             (ConnectionAllowedByPolicy( Gate ) and Gate.ConnectionAllowedByPolicy( self ))
            then
              if fConnections.IndexOf( Gate ) = NoIndex
                then
                  begin
                    result := ConnectionAllowed( Gate );
                    if result = cnxValid
                      then
                        begin
                          result := Gate.ConnectionAllowed( self );
                          if result = cnxValid
                            then
                              begin
                                InsertConnection( Gate );
                                Gate.InsertConnection( self );
                              end;
                        end;
                  end
                else result := cnxDuplicated
            else result := cnxRejected
        else result := cnxForbiden;
    end;

  procedure TGate.DisconnectFrom( Gate : TGate );
    begin
      if Gate <> nil
        then Gate.DeleteConnection( self );
      DeleteConnection( Gate );
    end;

  procedure TGate.DisconnectAll;
    begin
      while fConnections.Count > 0 do
        DisconnectFrom( TGate(fConnections[0]) )        
    end;

  procedure TGate.SortConnections;
    var
      i : integer;
    begin
      for i := pred(fConnections.Count) downto 0 do
        TGate(fConnections[i]).ConnectionChanged( self );
      for i := pred(fConnections.Count) downto 0 do
        ConnectionChanged( TGate(fConnections[i]) );
    end;

  class function TGate.BestCollection( aSize : integer ) : TCollection;
    begin
      result := TCollection.Create( 0, rkUse );
    end;

  procedure TGate.AutoConnect( loaded : boolean );
    begin
    end;

  procedure TGate.StoreToCache( Cache : TObjectCache );
    begin
    end;

  procedure TGate.InvalidateCache(connections : boolean);
    var
      i    : integer;
      path : string;
    begin
      ModelServerCache.BackgroundInvalidateCache(self);
      if connections
        then
          for i := 0 to pred(ConnectionCount) do
            if i mod SubObjForFile = 0
              then
                begin
                  path := ModelServerCache.GetObjectPath(self, 0, i);
                  ModelServerCache.InvalidateObjectByPath(path);
                end;
    end;

  procedure TGate.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Block', fBlock, nil );
      Reader.ReadObject( 'Connections', fConnections, nil );
      fOptions := Reader.ReadByte( 'Options', 0 );
    end;

  procedure TGate.StoreToBackup( Writer : IBackupWriter );
    begin                                                                     
      inherited;
      Writer.WriteObjectRef( 'Block', fBlock );
      Writer.WriteLooseObject( 'Connections', fConnections );
      Writer.WriteByte( 'Options', fOptions );
    end;

  procedure TGate.Loaded;

    procedure ChopExtraConnections;
      var
        limit     : integer;
        warehouse : boolean;
        Gate      : TGate;
        cnt       : integer;
      begin
        {
        if (Block.Facility.MetaFacility <> nil) and
           (Block.Facility.MetaFacility.Kind <> nil) and
           not (Block.Role in [rolDistributer, rolCompExport, rolCompInport]) and
           (Block.Facility.Company <> nil) and
           (Block.Facility.Company.Owner <> nil) and
           (MetaFluid <> nil) and
           (mfTradeable in MetaFluid.Options) and
           (MetaFluid.CnxLimit > 0)
        }
        warehouse := (Block.Role in [rolDistributer, rolCompExport, rolCompInport]);
        if (Block.Facility.MetaFacility <> nil) and
           (Block.Facility.MetaFacility.Kind <> nil) and
           (Block.Facility.Company <> nil) and
           (Block.Facility.Company.Owner <> nil) and
           (MetaFluid <> nil) and
           (mfTradeable in MetaFluid.Options) and
           (MetaFluid.CnxLimit > 0)
          then
            begin
              if not warehouse
                then limit := MetaFluid.CnxLimit
                else limit := max(MetaFluid.CnxLimit, 1000);
              cnt := fConnections.Count;
              while (cnt > 0) and (fConnections.Count > limit) do
                begin
                  Gate := TGate(fConnections[pred(fConnections.Count)]);
                  if (Gate.Block.Facility <> nil) and (Gate.Block.Facility.Company <> nil) and (Block.Facility.Company.Owner <> Gate.Block.Facility.Company.Owner)
                    then DisconnectFrom(Gate);
                  dec(cnt);
                end;
            end;
      end;

    begin
      ChopExtraConnections;
    end;

  procedure TGate.HookFluidData( aMetaGate : TMetaGate );
    begin
      if aMetaGate.Offset >= 0
        then SetFluidData( PFluidData(integer(@Block.fOffsetRef) + aMetaGate.Offset) );
    end;

  function TGate.ConnectionAllowedByPolicy( Gate : TGate ) : boolean;
    const
      CnxPolicyTable : array[TPolicyStatus, TPolicyStatus, TTradeLevel] of boolean =
      //    Ally,                         Neutral,                      Enemy
      //    SO,    Pup,   Ally,  Any      SO,    Pup,   Ally,  Any      SO,    Pup,   Ally,  Any
        ( ((false, false, true,  true),  (false, false, true,  true),  (false, false, false, false)),  // Ally    // It Was ((false, false, true,  true),  (false, false, true,  true),  (false, false, false, false))
          ((false, false, false, true),  (false, false, false, true),  (false, false, false, false)),  // Neutral // It was ((false, false, true,  true),  (false, false, false, true),  (false, false, false, false)),  // Neutral
          ((false, false, false, false), (false, false, false, false), (false, false, false, false))); // Enemy
    var
      Owner, GateOwner : TTycoon;
    begin
      {
      result :=
        ((Block.Facility.Company = nil) or (Block.Facility.Company.Owner = nil) or
         (Gate.Block.Facility.Company = nil) or (Gate.Block.Facility.Company.Owner = nil)) or
         (Block.Facility.Company.Owner.Policy[Gate.Block.Facility.Company.Owner] <> pstEnemy) and
         (Gate.Block.Facility.Company.Owner.Policy[Block.Facility.Company.Owner] <> pstEnemy)
      }
      if (Block.Facility.Company <> nil) and (Block.Facility.Company.Owner <> nil) and
         (Gate.Block.Facility.Company <> nil) and (Gate.Block.Facility.Company.Owner <> nil)
        then
          begin
            Owner     := Block.Facility.Company.Owner;
            GateOwner := Gate.Block.Facility.Company.Owner;
            if Owner = GateOwner
              then result := true
              else result := (not Owner.IsDemo) and (not GateOwner.IsDemo) and CnxPolicyTable[Owner.Policy[GateOwner], GateOwner.Policy[Owner], Block.TradeLevel];
          end
        else result := true;
    end;

  function TGate.GetOwnerPermissionMapId : integer;
    begin
      if (Block <> nil) and
         (Block.Facility <> nil) and
         (Block.Facility.Company <> nil) and
         (Block.Facility.Company.Owner <> nil) and
         (Block.Facility.Company.Owner.Level <> nil) and
         (Block.Facility.Company.Owner.Level.Tier > 1) and
         (MetaFluid <> nil) and
         not (mfConstruction in MetaFluid.Options) and
         not (mfCompanyFluid in MetaFluid.Options) and
         (mfTradeable in MetaFluid.Options)
        then result := 1
        else result := 0;
    end;

  procedure TGate.AutoRepair(Block : TBlock; index : integer);
    begin
      fBlock := Block;
      if Block.MetaBlock.CnntRepairable
        then fConnections := BestCollection(0);
    end;

  function TGate.GetCurrentTick : integer;
    begin
      if (fBlock <> nil) and (fBlock.Facility <> nil) and (fBlock.Facility.Town <> nil)
        then result := fBlock.Facility.Town.Timer.getTickId
        else result := 0;
    end;

  procedure TGate.UpdateFluid;
    begin
    end;

  // TInput

  constructor TInput.Create( aMetaGate : TMetaGate; aBlock : TBlock );
    begin
      inherited;
      fMetaInput      := TMetaInput(aMetaGate);
      fActualMaxFluid := TMetaInput(aMetaGate).MaxFluid^;
      fMaxCapacity    := TMetaInput(aMetaGate).MaxCapacity;
    end;

  function TInput.GetActualMaxFluid : PInputData;
    begin
      result := @fActualMaxFluid;
    end;

  function TInput.GetCapacity : TFluidValue;
    begin
      if MetaInput.MaxFluid.Q < qIlimited
        then result := realmin(MaxCapacity, ActualMaxFluid.Q*Block.dt)
        else result := qIlimited;
    end;

  function TInput.GetConnection( index : integer ) : TOutput;
    begin
      result := TOutput(fConnections[index]);
    end;

  function TInput.GetMetaFluid : TMetaFluid;
    begin
      result := MetaInput.MetaFluid;
    end;

  function TInput.GetDefaultFluidDataSize : integer;
    begin
      result := sizeof(TInputData);
    end;

  procedure TInput.Collect;
    begin
    end;

  procedure TInput.CollectExtra;
    begin
    end;

  procedure TInput.LoadFromBackup( Reader : IBackupReader );
    var
      buffer      : array[0..32] of char; // >>
      MetaInputId : string;
    begin
      inherited;
      MetaInputId := Reader.ReadString( 'MetaInput', '' );
      if fBlock <> nil
        then fMetaInput := fBlock.MetaBlock.InputByName[MetaInputId]
        else fMetaInput := nil;
      LoadInputData( 'ActualMaxFluid', fActualMaxFluid, Reader );
      fMaxCapacity := Reader.ReadSingle( 'MaxCapacity', 0 );
      LoadInputData( 'LastValue', fLastValue, Reader );
      if fMetaInput <> nil
        then
          begin
            HookFluidData( fMetaInput );
            if FluidData <> nil
              then Reader.ReadBuffer( 'FluidData', FluidData^, @buffer, MetaInput.FluidDataSize )
              else Reader.ReadBuffer( 'FluidData', buffer, @buffer, MetaInput.FluidDataSize );
          end
        else
          if MetaInputId <> ''
            then Reader.ReadBuffer('FluidData', buffer, @buffer, IgnoredBufferSize)
            else Reader.ReadBuffer('FluidData', buffer, @buffer, GetDefaultFluidDataSize);
    end;

  procedure TInput.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'MetaInput', fMetaInput.Name );
      StoreInputData( 'ActualMaxFluid', fActualMaxFluid, Writer );
      Writer.WriteSingle( 'MaxCapacity', fMaxCapacity );
      StoreInputData( 'LastValue', fLastValue, Writer );
      Writer.WriteBuffer( 'FluidData', FluidData^, MetaInput.FluidDataSize );
    end;

  procedure TInput.InsertConnection( Connection : TGate );
    begin
      if MetaInstances.ObjectIs( TOutput.ClassName, Connection )
        then inherited InsertConnection( Connection )
        else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' ERROR Attempting to connect ' + ClassName + ' to ' + Connection.ClassName );
    end;
    
  function TInput.GetConnectionPrecedence( Connection : TGate ) : integer;
    var
      Output    : TOutput;
      Idx       : integer;
      ECI       : PExtraConnectionInfo;
      DlvPrice  : TMoney;
      PricePerc : word;
    begin
      Output := TOutput(Connection);
      Idx    := fConnections.IndexOf( Output );
      if Idx >= 0
        then ECI := ExtraConnectionInfo[Idx]
        else ECI := nil;
      DlvPrice := Output.PriceToDeliver( 1, self, ECI, tnpPositiveShare );
      if (MetaFluid <> nil) and (MetaFluid.MarketPrice > 0)
        then PricePerc := round(100*DlvPrice/MetaFluid.MarketPrice)
        else PricePerc := 0;
      result := inherited GetConnectionPrecedence( Connection ) + PricePerc;
    end;

  function TInput.ConnectionAllowed( Connection : TGate ) : TConnectResult;

    function NoCnxToCompExport : boolean;
      var
        MatchingOutput : TOutput;
        MatchingRole   : TFacilityRole;
        i              : integer;
      begin
        MatchingOutput := Block.OutputsByName[MetaFluid.Id];
        if MatchingOutput <> nil
          then
            begin
              result := true;
              i := 0;
              while (i < MatchingOutput.ConnectionCount) and result do
                begin
                  MatchingRole := MatchingOutput.Connections[i].Block.Role;
                  result := (MatchingRole <> rolCompExport) and (MatchingRole <> rolCompInport);
                  inc( i );
                end;
            end
          else result := true;
      end;

    function AllowedTrade( Connection : TGate ) : boolean;

      function SameOwner : boolean;
        begin
          result := (Block.Facility.Company = nil) or
                    (Block.Facility.Company.Owner = nil) or
                    (Connection.Block.Facility.Company = nil) or
                    (Connection.Block.Facility.Company.Owner = nil) or
                    (Block.Facility.Company.Owner = Connection.Block.Facility.Company.Owner);
        end;

      begin
        case Block.Role of
          rolDistributer :
            case Connection.Block.Role of
              rolCompExport, rolCompInport :
                result := NoCnxToCompExport;
              rolDistributer :
                result := false;
              else
                result := true;
            end;
          rolCompExport :
            result := (Connection.Block.Role <> rolCompExport) and (Connection.Block.Role <> rolCompInport) and SameOwner;
          rolCompInport :
            result := Connection.Block.Role <> rolCompInport;
          else
            result := true;
        end;
        if not IsActive
          then result := result and SameOwner;
        //if result and (mfConstruction in MetaInput.MetaFluid.Options) and (Block.Facility.Company <> nil) and (Block.Facility.Company.Owner <> nil)
      end;

    begin
      result := inherited ConnectionAllowed( Connection );
      if (result = cnxValid) and (CnxPermissionMap[GetOwnerPermissionMapId][Block.Role, Connection.Block.Role] = 2) and not AllowedTrade( Connection )
        then result := cnxForbiden;
    end;

  procedure TInput.RDOSetInputFluidPerc(Perc : integer);
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(Block.Facility.XPos) + ',' + IntToStr(Block.Facility.YPos) + ') ' + 'Setting Input fluid perc: ' + IntToStr(Perc) );
      if Block.Facility.CheckOpAuthenticity
        then ActualMaxFluid.Q := MetaInput.MaxFluid.Q*realmin(1, Perc/100);
      Logs.Log( tidLog_Survival, 'OK!' );
    end;

  procedure TInput.AutoRepair(Block : TBlock; index : integer);
    begin
      inherited;
      if index < Block.MetaBlock.MetaInputs.Count
        then fMetaInput := TMetaInput(Block.MetaBlock.MetaInputs[index]);
    end;

  procedure TInput.SetSortMode(mode : byte);
    begin
    end;

  function TInput.IsActive : boolean;
    begin
      result := true;
    end;


  // TOutput

  constructor TOutput.Create( aMetaGate : TMetaGate; aBlock : TBlock );
    begin
      inherited;
      fMetaOutput := TMetaOutput(aMetaGate);
      fPricePerc  := 100;
    end;

  function TOutput.GetPrice : TMoney;
    begin
      result := fPricePerc*MetaOutput.MetaFluid.MarketPrice/100;
    end;

  procedure TOutput.SetPricePerc( aPricePerc : TOutputPrice );
    var
      i     : integer;
      Input : TInput;
    begin
      fPricePerc := aPricePerc;
      for i := pred(fConnections.Count) downto 0 do
        begin
          Input := TInput(fConnections[i]);
          if ObjectIs('TInput', Input)
            then Input.ConnectionChanged( self )
            else Logs.Log( 'Survival', TimeToStr(Now) + ' Error in SetPricePerc, unknow object as TInput.' );
        end;
    end;

  function TOutput.GetDemand : TFluidValue;
    begin
      result := 0;
    end;

  function TOutput.GetMetaFluid : TMetaFluid;
    begin
      result := MetaOutput.MetaFluid;
    end;

  function TOutput.GetDefaultFluidDataSize : integer;
    begin
      result := sizeof(TOutputData);
    end;

  function TOutput.GetConnection( index : integer ) : TInput;
    begin
      result := TInput(fConnections[index]);
    end;

  function TOutput.PriceToDeliver( Value : TFluidValue; Input : TInput; ExtraConnectionInfo : PExtraConnectionInfo; TransportPayMode : TTransportPayMode ) : TMoney;
    var
      d         : integer;
      tCost     : TMoney;
      OverPrice : TPercent;
    begin
      if MetaOutput.MetaFluid <> nil
        then
          begin
            if (Input.Block.XPos <> NoPos) and (Input.Block.YPos <> NoPos)
              then d := dist(Block.XPos, Block.YPos, Input.Block.XPos, Input.Block.YPos)
              else d := 0;
            if ExtraConnectionInfo <> nil
              then
                begin
                  OverPrice := ExtraConnectionInfo.OverPrice;
                  ExtraConnectionInfo.Distance := LoWord(d);
                end
              else OverPrice := 0;
            case TransportPayMode of
              tnpPositive :
                tCost := Value*d*MetaOutput.MetaFluid.TransCost;
              tnpPositiveShare :
                tCost := Value*d*MetaOutput.MetaFluid.TransCost/2;
              tnpNull :
                tCost := 0;
              tnpNegativeShare :
                tCost := -Value*d*MetaOutput.MetaFluid.TransCost/2;
              tnpNegative :
                tCost := -Value*d*MetaOutput.MetaFluid.TransCost;
              else
                tCost := 0;
            end;
            result := (100 + OverPrice)*Value*Price/100 + tCost;
          end
        else result := 0;
    end;

  procedure TOutput.LoadFromBackup( Reader : IBackupReader );
    var
      buffer       : array[0..32] of char; // >>
      MetaOutputId : string;
    begin
      inherited;
      MetaOutputId := Reader.ReadString( 'MetaOutput', '' );
      fMetaOutput  := Block.MetaBlock.OutputByName[MetaOutputId];
      fPricePerc   := Reader.ReadInteger( 'Price',  100 );
      if fMetaOutput <> nil
        then
          begin
            HookFluidData( fMetaOutput );
            if FluidData <> nil
              then Reader.ReadBuffer( 'FluidData', FluidData^, @buffer, MetaOutput.FluidDataSize )
              else Reader.ReadBuffer( 'FluidData', buffer, @buffer, MetaOutput.FluidDataSize );
          end
        else Reader.ReadBuffer( 'FluidData', FluidData^, @buffer, IgnoredBufferSize );
    end;

  procedure TOutput.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;                                                  
      Writer.WriteString( 'MetaOutput', fMetaOutput.Name );
      Writer.WriteInteger( 'Price', fPricePerc );
      Writer.WriteBuffer( 'FluidData', FluidData^, MetaOutput.FluidDataSize );
    end;

  procedure TOutput.InsertConnection( Connection : TGate );
    begin
      if MetaInstances.ObjectIs( TInput.ClassName, Connection )
        then inherited InsertConnection( Connection )
        else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' ERROR Attempting to connect ' + ClassName + ' to ' + Connection.ClassName );
    end;

  function TOutput.GetConnectionPrecedence( Connection : TGate ) : integer;
    var
      Input     : TInput;
      Idx       : integer;
      ECI       : PExtraConnectionInfo;
      DlvPrice  : TMoney;
      PricePerc : word;
    begin
      Input := TInput(Connection);
      Idx   := fConnections.IndexOf( Input );
      if Idx <> noIndex
        then ECI := ExtraConnectionInfo[Idx]
        else ECI := nil;
      DlvPrice := PriceToDeliver( 1, Input, ECI, tnpNegativeShare );
      if (MetaFluid <> nil) and (MetaFluid.MarketPrice > 0)
        then PricePerc := round(100*DlvPrice/MetaFluid.MarketPrice)
        else PricePerc := 0;
      result := inherited GetConnectionPrecedence( Connection ) + PriceRange - PricePerc;
    end;

  function TOutput.ConnectionAllowed( Connection : TGate ) : TConnectResult;

    function NoCnxToCompExport : boolean;
      var
        MatchingInput : TInput;
        MatchingRole  : TFacilityRole;
        i             : integer;
      begin
        MatchingInput := Block.InputsByName[MetaFluid.Id];
        if MatchingInput <> nil
          then
            begin
              result := true;
              i := 0;
              while (i < MatchingInput.ConnectionCount) and result do
                begin
                  MatchingRole := MatchingInput.Connections[i].Block.Role;
                  result := (MatchingRole <> rolCompExport) and (MatchingRole <> rolCompInport);
                  inc( i );
                end;
            end
          else result := true;
      end;

    {
    function AllowedWarehouse( Connection : TGate ) : boolean;
      begin
        case Block.Role of
          rolDistributer :
            case Connection.Block.Role of
              rolCompExport, rolCompInport :
                result := NoCnxToCompExport;
              else
                result := false;
            end;
          rolCompExport :
            result := Connection.Block.Role <> rolCompExport;
          rolCompInport :
            result := Connection.Block.Role = rolDistributer;
          else
            result := true;
        end;
      end;
    }
    function AllowedTrade( Connection : TGate ) : boolean;

      function SameOwner : boolean;
        begin
          result := (Block.Facility.Company = nil) or
                    (Block.Facility.Company.Owner = nil) or
                    (Connection.Block.Facility.Company = nil) or
                    (Connection.Block.Facility.Company.Owner = nil) or
                    (Block.Facility.Company.Owner = Connection.Block.Facility.Company.Owner);
        end;

      begin
        case Block.Role of
          rolDistributer :
            case Connection.Block.Role of
              rolCompExport, rolCompInport :
                result := NoCnxToCompExport;
              rolDistributer :
                result := false;
              else
                result := true;
            end;
          rolCompExport :
            result := (Connection.Block.Role <> rolCompExport);
          rolCompInport :
            result := (Connection.Block.Role <> rolCompInport) and SameOwner
          else
            result := true;
        end;
      end;

    begin
      result := inherited ConnectionAllowed( Connection );
      if (result = cnxValid) and (CnxPermissionMap[GetOwnerPermissionMapId][Block.Role, Connection.Block.Role] = 2) and not AllowedTrade( Connection )
        then result := cnxForbiden;
    end;

  procedure TOutput.AutoRepair(Block : TBlock; index : integer);
    begin
      inherited;
      if index < Block.MetaBlock.MetaOutputs.Count
        then fMetaOutput := TMetaOutput(Block.MetaBlock.MetaOutputs[index]);
    end;

  procedure TOutput.Spread;
    begin
      FluidData.Q := realmax(0, FluidData.Q);
    end;

  procedure TOutput.SpreadExtra;
    begin
    end;


  // TPushInput

  constructor TPushInput.Create( aMetaGate : TMetaGate; aBlock : TBlock );
    begin
      inherited;
      PPushInputData(FluidData).S := sIlimited;
    end;

  procedure TPushInput.Collect;
    var
      QKsum    : TFluidValue;
      Qsum     : TFluidValue;
      AuxFluid : PFluidData;
      i        : integer;
    begin
      inherited;
      {$IFDEF EXTRACHECKINGS}
      fConnections.CheckPolymorphism( TPushOutput ); // >> Double check
      {$ENDIF}
      QKsum := 0;
      Qsum  := 0;
      for i := 0 to pred(ConnectionCount) do
        begin
          AuxFluid := Connections[i].FluidData;
          QKsum := QKsum + AuxFluid.Q*AuxFluid.K;
          Qsum  := Qsum + AuxFluid.Q;
        end;
      if Qsum > 0
        then FluidData.K := round(QKsum/Qsum)
        else FluidData.K := 0;
    end;

  function TPushInput.GetDefaultFluidDataSize : integer;
    begin
      result := sizeof(TPushInputData);
    end;
    

  // TPullInput

  constructor TPullInput.Create( aMetaGate : TMetaGate; aBlock : TBlock );
    begin
      inherited;
      fInvIndex := TCollection.Create( 0, rkUse );
      TNotifiedCollection(fConnections).OnModified := OnConnectionsModified;
      fMaxPrice := 400;
      fSelected := true;
    end;

  destructor TPullInput.Destroy;
    begin
      fInvIndex.Free;
      inherited;
    end;

  function TPullInput.GetCapacity : TFluidValue;
    begin
      if fSelected
        then result := inherited GetCapacity
        else result := 0;
    end;

  procedure TPullInput.Collect;
    begin
      inherited;
      {$IFDEF EXTRACHECKINGS}
      fConnections.CheckPolymorphism( TPullOutput ); // >> Double check
      {$ENDIF}
      DoCollect( false );
    end;

  procedure TPullInput.CollectExtra;
    begin
      DoCollect( true );
      if not Block.Facility.CriticalTrouble and not (Block.Role in [rolDistributer, rolCompExport, rolCompInport])
        then
          begin
            //fParValue.CurrValue    := fParValue.CurrValue   + FluidData.Q/Block.dt;
            //fParCapacity.CurrValue := fParCapacity.CurrValue + MetaInput.fMaxFluid.Q;
            //fParQuality.CurrValue  := fParQuality.CurrValue + FluidData.Q*FluidData.K/Block.dt;
            //fParQuality.IncCount( FluidData.Q/Block.dt );
            //fParPrice.CurrValue := fParPrice.CurrValue + LastCost;
            //fParPrice.IncCount( FluidData.Q );
            //fParMaxPrice.CurrValue := fParMaxPrice.CurrValue + FluidData.Q*MaxPrice/Block.dt;
            //fParMaxPrice.IncCount( FluidData.Q/Block.dt );
          end;
    end;

  procedure TPullInput.CheckIndex;
    var
      i      : integer;
      idx    : integer;
      Output : TOutput;
    begin
      if fConnections.Count <> fInvIndex.Count
        then
          begin
            fInvIndex.ExtractAll;
            fInvIndex.Capacity := fConnections.Count;
            for i := 0 to pred(fConnections.Count) do
              begin
                Output := TOutput(fConnections[i]);
                idx := Output.fConnections.IndexOf(self);
                fInvIndex.Insert(TObject(idx));
              end;
          end;
    end;

  function TPullInput.IndexAtOutput( Output : TOutput; OutputIdx : integer ) : integer;
    begin
      result := integer(fInvIndex[OutputIdx]);
      if (result = noIndex) or (result = UnresolvedIdx) or (result >= Output.ConnectionCount) or (self <> Output.Connections[result])
        then
          begin
            result := Output.fConnections.IndexOf( self );
            fInvIndex[OutputIdx] := TObject(result);
          end;
    end;

  procedure TPullInput.DoCollect( CollectExtra : boolean );
    var
      dQ     : TFluidValue;
      QKsum  : TFluidValue;
      Qsum   : TFluidValue;
      Cap    : TFluidValue;
      CurrF  : TFluidData;
      LastF  : TFluidData;
      i      : integer;
      Cost   : TMoney;
      ExInf  : PExtraConnectionInfo;
      Output : TPullOutput;
      Idx    : integer;
      tick   : integer;
    begin
      inherited;

      // Check the inv index
      CheckIndex;

      i := 0;
      if not CollectExtra
        then
          begin
            FluidData.Q := 0;
            vLastCost := 0;
          end;

      LastF := FluidData^;
      CurrF.Q := 0;
      CurrF.K := 0;
      Cap     := Capacity;

      // get tick
      if Block.Facility.Town <> nil
        then tick := Block.Facility.Town.Timer.getTickId
        else tick := 0;

      while (FluidData.Q < Cap) and (i < ConnectionCount) and not (Block.Facility.CriticalTrouble) do
        begin
          Profiler.ProcStarted( prfKind_InputCollect, prfId_CollectLoopInit );
          Output := TPullOutput(Connections[i]);
          // Get the index of the input in the output
          Idx := IndexAtOutput(Output, i);
          if Idx <> noIndex
            then
              begin
                dQ := Output.GetSliceFor( self, Idx );
                ExInf := Output.ExtraConnectionInfo[Idx];
                Output.fIdxAux := Idx;
              end
            else
              begin
                dQ := 0;
                ExInf := nil;
                Output.fIdxAux := noIndex; // >> This is useless anyways
              end;
          //Cost := 0;
          Profiler.ProcEnded( prfKind_InputCollect, prfId_CollectLoopInit );
          Profiler.ProcStarted( prfKind_InputCollect, prfId_dQHandling );
          if dQ > 0 // This also => ExInfo is not NULL
            then
              begin
                if FluidData.Q + dQ > Cap
                  then dQ := Cap - FluidData.Q;
                Cost := Output.PriceToDeliver( dQ, self, ExInf, tnpPositiveShare );
                if Cost > 0
                  then
                    if Block.Facility.Budget > 0
                      then
                        begin
                          if Block.MetaBlock.RecordCost and (mfConstruction in MetaInput.MetaFluid.Options)
                            then
                              begin
                                Block.BlockGenMoney( -Cost, accIdx_Construction );
                                Block.Facility.fCost := Block.Facility.fCost + Cost;
                                if Output.Block.Role = rolImporter
                                  then Block.Facility.fTCCost := Block.Facility.fTCCost + Cost;
                              end
                            else
                              if Block.MetaBlock.fSupplyAccount <> accIdx_None
                                then Block.BlockGenMoney( -Cost, Block.MetaBlock.fSupplyAccount );
                          vLastCost := vLastCost + Cost;
                          Cost := Output.PriceToDeliver( dQ, self, ExInf, tnpNegativeShare );
                          if Output.Block.MetaBlock.fProdAccount <> accIdx_None
                            then Output.Block.BlockGenMoney( Cost, Output.Block.MetaBlock.fProdAccount );
                        end
                      else
                        begin
                          Block.Facility.ReportTrouble( facNeedsBudget );
                          dQ   := 0;
                          //Cost := 0;
                        end;
                Output.fQAuxBuff := dQ;
                FluidData.Q := FluidData.Q + dQ;
                CurrF.Q := CurrF.Q + dQ;
              end;

          if (ExInf <> nil) and not CollectExtra and ExInf.Connected and TransferAllowed( Output ) and not Block.Facility.CriticalTrouble
            then Output.ReportDemand( Cap - FluidData.Q, self );

          if ExInf <> nil
            then
              begin
                if not CollectExtra
                  then ExInf.LastFluid := 0;
                ExInf.LastFluid := ExInf.LastFluid + dQ/Block.dt;
                //ExInf.YearValue := ExInf.YearValue + dQ;
                //ExInf.YearCost  := ExInf.YearCost  + Cost;
              end;

          if Idx = noIndex
            then fConnections.Extract(Output) // >> This fixes the symetry problem.
            else inc(i);

          Profiler.ProcEnded( prfKind_InputCollect, prfId_dQHandling );
        end;

      if not CollectExtra
        then
          while (i < ConnectionCount) do
            begin
              Output := TPullOutput(Connections[i]);
              Idx := IndexAtOutput( Output, i );
              if Idx <> noIndex
                then
                  begin
                    ExInf  := Output.ExtraConnectionInfo[Idx];
                    Output.fIdxAux := Idx;
                    if ExInf <> nil
                      then ExInf.LastFluid := 0;
                    inc(i);
                  end
                else fConnections.Extract(Output); // >> This fixes the symetry problem.
            end;
      QKsum := 0;
      Qsum  := 0;
      Profiler.ProcStarted( prfKind_InputCollect, prfId_CollectFinalizing );
      for i := 0 to pred(ConnectionCount) do
        begin
          Output := TPullOutput(Connections[i]);
          if Output.fIdxAux <> NoIndex
            then Idx := Output.fIdxAux
            else Idx := IndexAtOutput( Output, i );
          if Idx <> noIndex
            then
              begin
                if Output.fQAuxBuff > 0
                  then
                    begin
                      QKsum := QKsum + Output.fQAuxBuff*Output.FluidData.K;
                      Qsum  := Qsum + Output.fQAuxBuff;
                    end;
                if (FluidData.Q < Cap) and not Block.Facility.CriticalTrouble
                  then Output.ValuePulled(Cap, self, Idx, tick)
                  else Output.ValuePulled(Output.fQAuxBuff, self, Idx, tick);
              end;
          Output.fQAuxBuff := 0;
          Output.fIdxAux   := NoIndex;
        end;
      if Qsum > 0
        then CurrF.K := round(QKsum/Qsum)
        else CurrF.K := 0;
      if CollectExtra
        then FluidData.K := AverageK( @LastF, @CurrF ) // AverageK( FluidData, @CurrF )
        else FluidData.K := CurrF.K;
      if FluidData.Q < MetaInput.MinFluid.Q
        then Block.Facility.ReportTrouble( facInsuficientInput );
      Profiler.ProcEnded( prfKind_InputCollect, prfId_CollectFinalizing );
    end;

  function TPullInput.GetLastCost : TMoney;
    begin
      result := fLastCost;
    end;

  procedure TPullInput.SetLastCost( aLastCost : TMoney );
    begin
      fLastCost := aLastCost;
    end;

  procedure TPullInput.InsertConnection( Connection : TGate );
    var
      CnxVal : integer;
      i      : integer;
    begin
      if MetaInstances.ObjectIs( TPullOutput.ClassName, Connection )
        then
          begin
            CnxVal := GetConnectionPrecedence( Connection );
            i := 0;
            while (i < fConnections.Count) and (GetConnectionPrecedence( TGate(fConnections[i]) ) <= CnxVal) do
              inc( i );
            fConnections.AtInsert( i, Connection );
          end
        else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' ERROR Attempting to connect ' + ClassName + ' to ' + Connection.ClassName );
    end;

  function TPullInput.GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo;
    var
      InputIdx : integer;
    begin
      if index <> NoIndex
        then
          try
            InputIdx := Connections[index].fConnections.IndexOf(self);
            if InputIdx <> noIndex
              then result := Connections[index].ExtraConnectionInfo[InputIdx]
              else result := nil;
          except
            result := nil;
            Logs.Log( 'Survival', TimeToStr(Now) + ' Error in GetExtraConnectionInfo' );
          end
        else result := nil;
    end;

  procedure TPullInput.CheckConnections;
    var
      Roads1, Roads2 : TCollection;
      i              : integer;
      ExtraCnnt      : PExtraConnectionInfo;
      Blk            : TBlock;
      cntFlg         : boolean;
    begin
      inherited;
      Roads1 := Block.Circuits[cirRoads];
      if Roads1 <> nil
        then
          for i := 0 to pred(ConnectionCount) do
            begin
              ExtraCnnt := ExtraConnectionInfo[i];
              if ExtraCnnt <> nil
                then
                  begin
                    Blk := Connections[i].Block;
                    Roads2 := Blk.Circuits[cirRoads];
                    if Roads2 = nil
                      then ExtraCnnt.Connected := not ObjectIs('TConnectedBlock', Blk)
                      else ExtraCnnt.Connected := (Roads2.Matches( Roads1 ) > 0) or CloseEnought(Blk, self.Block);
                  end;
            end
        else
          begin
            cntFlg := not ObjectIs('TConnectedBlock', Block);
            for i := 0 to pred(ConnectionCount) do
              begin
                ExtraCnnt := ExtraConnectionInfo[i];
                if ExtraCnnt <> nil
                  then ExtraCnnt.Connected := cntFlg;
              end;
          end;
    end;

  procedure TPullInput.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fInvIndex := TCollection.Create(0, rkUse);
      fLastCost := Reader.ReadCurrency( 'LastCost', 0 );
      fMaxPrice := Reader.ReadWord( 'MaxPrice', 400 );
      fMinK     := Reader.ReadByte( 'MinK', 0 );
      fSelected := Reader.ReadBoolean('Sel', true);
    end;

  procedure TPullInput.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteCurrency( 'LastCost', fLastCost );
      Writer.WriteWord( 'MaxPrice', fMaxPrice );
      Writer.WriteByte( 'MinK', fMinK );
      Writer.WriteBoolean('Sel', fSelected);
    end;

  procedure TPullInput.StoreToCache( Cache : TObjectCache );
    begin
      inherited;
      Cache.WriteInteger( 'MaxPrice', fMaxPrice );
      Cache.WriteInteger( 'MinK', fMinK );
      Cache.WriteBoolean('Selected', fSelected);
    end;

  procedure TPullInput.AutoConnect( loaded : boolean );
    begin
      inherited;
      if MetaInput <> nil
        then
          begin
            //fParCapacity := Block.Facility.Town.Parameters[tidTownParameter_InputCapacity + MetaInput.MetaFluid.Id];
            //fParValue    := Block.Facility.Town.Parameters[tidTownParameter_InputValue + MetaInput.MetaFluid.Id];
            //fParQuality  := Block.Facility.Town.Parameters[tidTownParameter_InputQuality + MetaInput.MetaFluid.Id];
            //fParPrice    := Block.Facility.Town.Parameters[tidTownParameter_InputPrice + MetaInput.MetaFluid.Id];
            //fParMaxPrice := Block.Facility.Town.Parameters[tidTownParameter_InputMaxPrice + MetaInput.MetaFluid.Id];
          end;
    end;

  procedure TPullInput.Loaded;
    var
      newColl : TCollection;
      i, idx  : integer;
      Output  : TOutput;
    begin
      inherited;
      if not ObjectIs( TNotifiedCollection.ClassName, fConnections )
        then
          begin
            newColl := BestCollection( fConnections.Count );
            newColl.InsertColl( fConnections );
            fConnections.Free;
            fConnections := newColl;
          end;
      TNotifiedCollection(fConnections).OnModified := OnConnectionsModified;
      fInvIndex.ExtractAll;
      fInvIndex.Capacity := fConnections.Count;
      for i := 0 to pred(fConnections.Count) do
        begin
          Output := TOutput(fConnections[i]);
          idx    := Output.fConnections.IndexOf( self );
          fInvIndex.Insert( TObject(idx) );
        end;
    end;

  class function TPullInput.BestCollection( aSize : integer ) : TCollection;
    begin
      result := TNotifiedCollection.Create( 0, rkUse )
    end;

  procedure TPullInput.OnConnectionsModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
    begin
      case Operation of
        opInsertion :
          begin
            if fInvIndex.Count > Index
              then fInvIndex.AtInsert(Index, TObject(UnresolvedIdx));
          end;
        opDeletion, opExtraction :
          begin
            if fInvIndex.Count > Index
              then fInvIndex.AtDelete(Index);
          end;
      end;
    end;

  function TPullInput.TransferAllowed( Output : TPullOutput ) : boolean;
    begin
      result := ((Output.PricePerc <= MaxPrice) and (Output.FluidData.K >= MinK)) or
                ((Block.Facility.Company <> nil) and (Output.Block.Facility.Company <> nil) and
                 (Block.Facility.Company.Owner = Output.Block.Facility.Company.Owner));
    end;

  procedure TPullInput.RDOSelSelected(value : WordBool);
    begin
      if (Block <> nil) and (Block.Facility <> nil) and Block.Facility.CheckOpAuthenticity
        then
          begin
            fSelected := value;
            fActualMaxFluid.Q := 0;
            ModelServerCache.InvalidateCache(self, false);
          end;
    end;

  function TPullInput.IsActive : boolean;
    begin
      result := fSelected;
    end;


  // TPushOutput

  (*
  procedure TPushOutput.Spread;
    var
      Ssum    : double;
      Q, dQ   : double;
      Left    : double;
      Actual  : double;
      Fluid   : PPushInputData;
      skipped : integer;
      i       : integer;
      Cost    : TMoney;
    begin
      inherited;
      Q := FluidData.Q;
      Actual  := 0;
      skipped := 0;
      while (Q > 0) and (skipped < ConnectionCount) do
        begin
          Left := 0;
          Ssum := 0;
          for i := 0 to pred(ConnectionCount) do
            if not TPushInput(Connections[i]).fSkipped and not Connections[i].Block.Facility.CriticalTrouble
              then Ssum := Ssum + PPushInputData(Connections[i].FluidData).S;
          for i := 0 to pred(ConnectionCount) do
            if not TPushInput(Connections[i]).fSkipped and not Connections[i].Block.Facility.CriticalTrouble
              then
                begin
                  Fluid := PPushInputData(Connections[i].FluidData);
                  if Ssum > 0
                    then dQ := (Fluid.S*Q)/Ssum
                    else dQ := 0;
                  if Fluid.Q + dQ >= Connections[i].MetaInput.MinFluid.Q
                    then
                      begin
                        if Fluid.Q + dQ > Connections[i].Capacity
                          then
                            begin
                              Left := Left + Fluid.Q + dQ - Connections[i].Capacity;
                              dQ := Connections[i].Capacity - Fluid.Q;
                              TPushInput(Connections[i]).fSkipped := true;
                              inc( skipped );
                            end;
                        Fluid.Q := Fluid.Q + dQ;
                        Cost := PriceToDeliver( dQ, Connections[i], nil, tnpNull );
                        if Cost > 0
                          then
                            if Connections[i].Block.Facility.Budget > 0
                              then
                                begin
                                  if Block.MetaBlock.fProdAccount <> accIdx_None
                                    then Block.BlockGenMoney( Cost, Block.MetaBlock.fProdAccount );
                                  if mfConstruction in MetaOutput.MetaFluid.Options
                                    then Connections[i].Block.BlockGenMoney( -Cost, accIdx_Construction )
                                    else
                                      if Connections[i].Block.MetaBlock.fSupplyAccount <> accIdx_None
                                        then Connections[i].Block.BlockGenMoney( -Cost, Connections[i].Block.MetaBlock.fSupplyAccount );
                                end
                              else
                                begin
                                  Connections[i].Block.Facility.ReportTrouble( facNeedsBudget );
                                  Left := Left + dQ;
                                  dQ := 0;
                                  TPushInput(Connections[i]).fSkipped := true;
                                  inc( skipped );
                                end;
                        Actual := Actual + dQ;
                      end
                    else
                      begin
                        Connections[i].Block.Facility.ReportTrouble( facInsuficientInput );
                        TPushInput(Connections[i]).fSkipped := true;
                        inc( skipped );
                      end;
                end
              else
                begin
                  if not TPushInput(Connections[i]).fSkipped
                    then
                      begin
                        TPushInput(Connections[i]).fSkipped := true;
                        inc( skipped );
                      end;
                end;
          Q := Left;
        end;
      POutputData(FluidData).Extra.Q := FluidData.Q - Actual;
      POutputData(FluidData).Extra.K := FluidData.K;
      {
      i := pred(ConnectionCount);
      while (POutputData(FluidData).Extra.Q > 0) and (skipped < ConnectionCount) do
        begin
          if not TPushInput(Connections[i]).fSkipped
            then
              if PInputData(Connections[i].FluidData).Q < Connections[i].Capacity.Q
                then
                  begin
                    inc( PInputData(Connections[i].FluidData).Q );
                    dec( POutputData(FluidData).Extra.Q );
                  end
                else
                  begin
                    TPushInput(Connections[i]).fSkipped := true;
                    inc( skipped );
                  end;
          if i > 0
            then dec( i )
            else i := pred(ConnectionCount);
        end;
      }
      for i := 0 to pred(ConnectionCount) do
        TPushInput(Connections[i]).fSkipped := false;
    end;
  *)

  procedure TPushOutput.Spread;
    var
      Ssum    : double;
      Q, dQ   : double;
      Left    : double;
      Actual  : double;
      Fluid   : PPushInputData;
      skipped : integer;
      i       : integer;
      Cost    : TMoney;
      Input   : TPushInput;
    begin
      inherited;
      Q := FluidData.Q;
      Actual  := 0;
      skipped := 0;
      {$IFDEF EXTRACHECKINGS}
      fConnections.CheckPolymorphism( TPushInput ); // >> Double check
      {$ENDIF}
      while (Q > 0) and (skipped < ConnectionCount) do
        begin
          Left := 0;
          Ssum := 0;
          for i := 0 to pred(ConnectionCount) do
            begin
              Input := TPushInput(Connections[i]);
              if not Input.fSkipped and not Input.Block.Facility.CriticalTrouble
                then Ssum := Ssum + PPushInputData(Input.FluidData).S;
            end;
          for i := 0 to pred(ConnectionCount) do
            begin
              if not TPushInput(Connections[i]).fSkipped and not Connections[i].Block.Facility.CriticalTrouble
                then
                  begin
                    Input := TPushInput(Connections[i]);
                    Fluid := PPushInputData(Input.FluidData);
                    if Ssum > 0
                      then dQ := (Fluid.S*Q)/Ssum
                      else dQ := 0;
                    if Fluid.Q + dQ >= Input.MetaInput.MinFluid.Q
                      then
                        begin
                          if Fluid.Q + dQ > Input.Capacity
                            then
                              begin
                                Left := Left + Fluid.Q + dQ - Connections[i].Capacity;
                                dQ := Input.Capacity - Fluid.Q;
                                Input.fSkipped := true;
                                inc( skipped );
                              end;
                          Fluid.Q := Fluid.Q + dQ;
                          Cost := PriceToDeliver( dQ, Input, nil, tnpNull );
                          if Cost > 0
                            then
                              if Input.Block.Facility.Budget > 0
                                then
                                  begin
                                    if Block.MetaBlock.fProdAccount <> accIdx_None
                                      then Block.BlockGenMoney( Cost, Block.MetaBlock.fProdAccount );
                                    if mfConstruction in MetaOutput.MetaFluid.Options
                                      then Input.Block.BlockGenMoney( -Cost, accIdx_Construction )
                                      else
                                        if Input.Block.MetaBlock.fSupplyAccount <> accIdx_None
                                          then Input.Block.BlockGenMoney( -Cost, Input.Block.MetaBlock.fSupplyAccount );
                                  end
                                else
                                  begin
                                    Input.Block.Facility.ReportTrouble( facNeedsBudget );
                                    Left := Left + dQ;
                                    dQ := 0;
                                    TPushInput(Input).fSkipped := true;
                                    inc( skipped );
                                  end;
                          Actual := Actual + dQ;
                        end
                      else
                        begin
                          Input.Block.Facility.ReportTrouble( facInsuficientInput );
                          Input.fSkipped := true;
                          inc( skipped );
                        end;
                  end
                else
                  begin
                    Input := TPushInput(Connections[i]);
                    if not Input.fSkipped
                      then
                        begin
                          Input.fSkipped := true;
                          inc( skipped );
                        end;
                  end;
            end;
          Q := Left;
        end;
      POutputData(FluidData).Extra.Q := FluidData.Q - Actual;
      POutputData(FluidData).Extra.K := FluidData.K;
      {
      i := pred(ConnectionCount);
      while (POutputData(FluidData).Extra.Q > 0) and (skipped < ConnectionCount) do
        begin
          if not TPushInput(Connections[i]).fSkipped
            then
              if PInputData(Connections[i].FluidData).Q < Connections[i].Capacity.Q
                then
                  begin
                    inc( PInputData(Connections[i].FluidData).Q );
                    dec( POutputData(FluidData).Extra.Q );
                  end
                else
                  begin
                    TPushInput(Connections[i]).fSkipped := true;
                    inc( skipped );
                  end;
          if i > 0
            then dec( i )
            else i := pred(ConnectionCount);
        end;
      }
      for i := 0 to pred(ConnectionCount) do
        TPushInput(Connections[i]).fSkipped := false;
    end;


  // TSlice

  constructor TSlice.Create;
    begin
      inherited;
      ExtraConnectionInfo.Distance  := 0;
      ExtraConnectionInfo.LastFluid := 0;
    end;


  // TPullOutput

  constructor TPullOutput.Create( aMetaGate : TMetaGate; aBlock : TBlock );
    begin
      inherited;
      fSlices := TCollection.Create( 0, rkBelonguer );
      TNotifiedCollection(fConnections).OnModified := OnConnectionsModified;
      fIdxAux := NoIndex;
    end;

  destructor TPullOutput.Destroy;
    begin
      fSlices.Free;
      inherited;
    end;

  procedure TPullOutput.OnConnectionsModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
    var
      Slice : TSlice;
    begin
      case Operation of
        opInsertion :
          begin
            Slice := TSlice.Create;
            Slice.tickId := GetCurrentTick;
            fSlices.AtInsert( Index, Slice );
          end;
        opDeletion, opExtraction :
          fSlices.AtDelete( Index );
      end;
    end;

  procedure TPullOutput.Spread;
    const
      LazyCnxThreshold = 24*30*9; // 8 months
      LazyPurgeFreq    = 250;     // Aprox 11 days
      ChopFreq         = 100;
    var
      i     : integer;
      Fac   : TFacility;
      Comp  : TCompany;
      Owner : TTycoon;
      Input : TInput;
      tick  : word;
      stick : word;
      diff  : integer;
    begin
      inherited;
      {$IFDEF EXTRACHECKINGS}
      fConnections.CheckPolymorphism( TPullInput ); // >> Double check
      {$ENDIF}
      CheckSlices; // Just in case..

      // get tick
      Fac := Block.Facility;
      if Fac.Town <> nil
        then tick := loWord(Fac.Town.Timer.getTickId)
        else tick := 1;

      // Chop lazy connections in warehouses
      if ((tick + loByte(integer(self))) mod LazyPurgeFreq = 0) and (Block.Role in [rolDistributer, rolCompExport, rolCompInport])
        then
          begin
            // Get owner
            Comp := Fac.Company;
            if Comp <> nil
              then Owner := Comp.Owner
              else Owner := nil;
            // Check connections
            for i := pred(ConnectionCount) downto MetaOutput.MetaFluid.CnxLimit do
              begin
                stick := TSlice(fSlices[i]).tickId;
                diff  := tick - stick;
                if diff > LazyCnxThreshold
                  then
                    begin
                      Input := TInput(Connections[i]);
                      // Remove conection if not owned and it is not a wh
                      Comp := Input.Block.Facility.Company;
                      if (Comp <> nil) and (Comp.Owner <> Owner) and not (Input.Block.Role in [rolProducer, rolDistributer, rolCompExport, rolCompInport])
                        then DisconnectFrom(Input);
                    end;
              end;
          end;

      // Chop unallowed connections
      {if ((tick + loByte(integer(self))) mod ChopFreq = 0) and (Block.Role in [rolDistributer, rolCompExport, rolCompInport])
        then
          begin
            // Get owner
            Comp := Fac.Company;
            if Comp <> nil
              then Owner := Comp.Owner
              else Owner := nil;
            // Check connections
            for i := pred(ConnectionCount) downto MetaOutput.MetaFluid.CnxLimit do
              begin
                Input := TInput(Connections[i]);
                Comp := Input.Block.Facility.Company;
                if (Input.Block.Role = rolBuyer) and
                   (Comp <> nil) and (Comp.Owner <> Owner) and
                   (Comp.Owner <> nil) and not Comp.Owner.IsRole and
                   (Comp.Owner.Level.Tier > 1)
                  then DisconnectFrom(Input);
              end;
          end;}

      // Init slices
      fDemand := 0;
      POutputData(FluidData).Extra.Q := 0;
      POutputData(FluidData).Extra.K := FluidData.K;
      for i := 0 to pred(fSlices.Count) do
        with TSlice(fSlices[i]) do
          begin
            taken := false;
            val   := 0;
            //inc( tickId );
          end;
      Slice( FluidData.Q );
    end;

  procedure TPullOutput.SpreadExtra;
    var
      i     : integer;
      dist  : boolean;
      Input : TInput;
    begin
      inherited;
      CheckSlices; // Just in case..
      // Compute Extra and Demand
      // fDemand := 0;
      for i := 0 to pred(fSlices.Count) do
        with TSlice(fSlices[i]) do
          if val > 0
            then POutputData(FluidData).Extra.Q := POutputData(FluidData).Extra.Q + val;
            // else fDemand := fDemand + abs(val);

      // Fill last period needs with last period extra and clear slice info
      dist := Block.Role = rolDistributer;
      for i := 0 to pred(fSlices.Count) do
        begin
          Input := Connections[i];
          with TSlice(fSlices[i]) do
            if (val < 0) and (POutputData(FluidData).Extra.Q > 0) and not Input.Block.Facility.CriticalTrouble and ExtraConnectionInfo.Connected and TPullInput(Input).TransferAllowed( self )
              then
                begin
                  val := realmin( POutputData(FluidData).Extra.Q, -val );
                  if dist and (Input.Block.Role = rolDistributer)
                    then val := val/1000;
                  POutputData(FluidData).Extra.Q := POutputData(FluidData).Extra.Q - val;
                  fDemand := fDemand - val;
                  taken := false;
                end
              else val := 0;
        end;

      if not Block.Facility.CriticalTrouble and (Block.Role <> rolDistributer)
        then
          begin
            //fParValue.CurrValue    := fParValue.CurrValue + FluidData.Q/Block.dt;
            //fParCapacity.CurrValue := fParCapacity.CurrValue + MetaOutput.MaxFluid.Q;
            //fParQuality.CurrValue  := fParQuality.CurrValue + FluidData.K*FluidData.Q/Block.dt;
            //fParQuality.IncCount( FluidData.Q/Block.dt );
            fParPrice.CurrValue := fParPrice.CurrValue + Price*FluidData.Q/Block.dt;
            fParPrice.IncCount( FluidData.Q/Block.dt );
          end;
    end;

  function TPullOutput.GetDemand : TFluidValue;
    begin
      result := fDemand;
    end;

  function TPullOutput.GetLastFluid : TFluidValue;
    begin
      result := fLastQ;
    end;

  procedure TPullOutput.ValuePulled( Value : TFluidValue; Input : TInput; Idx, tick : integer );
    begin
      with TSlice(fSlices[Idx]) do
        begin
          taken := true;
          if (Value > 0) or (val > 0)
            then tickId := loWord(tick);
          if ExtraConnectionInfo.Connected and TPullInput(Input).TransferAllowed( self )
            then
              begin
                val := val - Value;
                if val > 0
                  then
                    begin
                      extra := extra + val;
                      Slice( val );
                      val := 0;
                    end
                  else
                    if val < 0
                      then extra := 0;
              end;
        end;
    end;

  procedure TPullOutput.ReportDemand( Value : TFluidValue; Input : TInput );
    begin
      fDemand := fDemand + Value;
    end;

  procedure TPullOutput.Slice( Value : TFluidValue );
    var
      i     : integer;
      v     : TFluidValue;
      Input : TInput;
      cap   : TFluidValue;
    begin
      i := 0;
      while (i < ConnectionCount) and (Value > qZero) do
        begin
          Input := Connections[i];
          with TSlice(fSlices[i]) do
            if not taken and not Input.Block.Facility.CriticalTrouble and ExtraConnectionInfo.Connected and TPullInput(Input).TransferAllowed( self )
              then
                begin
                  cap := realmax(0, Input.Capacity - Input.FluidData.Q - extra/2);
                  if val + Value > cap
                    then v := cap - val
                    else v := Value;
                  Value := Value - v;
                  val := val + v;
                end;
          inc( i );
        end;
      if Value <= qZero
        then Value := 0;
      POutputData(FluidData).Extra.Q := POutputData(FluidData).Extra.Q + Value;
    end;

  function TPullOutput.GetSliceFor( Input : TInput; Idx : integer ) : TFluidValue;

    function GetExtraSliceFor(Input : TInput; InputIdx : integer; theDt : single) : TFluidValue;
      begin
        result := realmin(POutputData(FluidData).Extra.Q, theDt*Input.ActualMaxFluid.Q);
      end;

    var
      theDt : single;

    begin
      theDt := Block.dt;
      if Idx = NoIndex
        then
          if ConnectTo( Input ) = cnxValid
            then
              begin
                Logs.Log( 'Survival', TimeToStr(Now) + ' Error in GetSliceFor, Unknown Input found x:' + IntToStr(Input.Block.xPos) + ' y:' + IntToStr(Input.Block.yPos));
                Idx := vConnections.IndexOf( Input );
              end;
      if Idx <> NoIndex
        then
          begin
            result := TSlice(fSlices[Idx]).val;
            if (result = 0) and not Connections[Idx].Block.Facility.CriticalTrouble and TSlice(fSlices[Idx]).ExtraConnectionInfo.Connected and TPullInput(Connections[Idx]).TransferAllowed( self )
              then
                begin
                  result := GetExtraSliceFor(Input, Idx, theDt);
                  TSlice(fSlices[Idx]).val := result;
                  POutputData(FluidData).Extra.Q := POutputData(FluidData).Extra.Q - result;
                end;
          end
        else result := 0;
    end;

  procedure TPullOutput.ComputePriorities;
    var
      i : integer;
    begin
      for i := 0 to pred(ConnectionCount) do
        with TSlice(fSlices[i]) do
          ExtraConnectionInfo.Priority := 100 - 100*succ(i) div ConnectionCount;
    end;

  procedure TPullOutput.ConnectionChanged( Connection : TGate );
    var
      ExtraInfo : TExtraConnectionInfo;
      Idx       : integer;
    begin
      if ConnectionAllowedByPolicy( Connection )
        then
          begin
            Idx := fConnections.IndexOf(Connection);
            if Idx <> noIndex
              then
                begin
                  ExtraInfo := ExtraConnectionInfo[Idx]^;
                  fConnections.Extract(Connection);
                  InsertConnection(Connection);
                  Idx := fConnections.IndexOf(Connection);
                  if Idx <> noIndex
                    then ExtraConnectionInfo[Idx]^ := ExtraInfo;
                end;
          end
        else DisconnectFrom( Connection );
      ComputePriorities;
    end;

  procedure TPullOutput.InsertConnection( Connection : TGate );
    var
      CnxVal : integer;
      i      : integer;
    begin
      if MetaInstances.ObjectIs( TPullInput.ClassName, Connection )
        then
          begin
            if mgoptEditable in MetaOutput.Options
              then
                begin
                  CnxVal := GetConnectionPrecedence( Connection );
                  i := 0;
                  while (i < fConnections.Count) and (GetConnectionPrecedence( TGate(fConnections[i]) ) <= CnxVal) do
                    inc( i );
                  fConnections.AtInsert( i, Connection );
                end
              else inherited;
            ComputePriorities;
          end
        else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' ERROR Attempting to connect ' + ClassName + ' to ' + Connection.ClassName );
    end;

  function TPullOutput.GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo;
    var
      Slice : TSlice;
    begin
      if index <> NoIndex
        then
          try
            Slice := TSlice(fSlices[index]);
            if Slice <> nil
              then result := @(Slice.ExtraConnectionInfo)
              else result := nil;
          except
            result := nil;
          end
        else result := nil;
    end;

  procedure TPullOutput.CheckConnections;
    var
      Roads1, Roads2 : TCollection;
      i              : integer;
      ExtraCnnt      : PExtraConnectionInfo;
      Blk            : TBlock;
    begin
      inherited;
      Roads1 := Block.Circuits[cirRoads];
      if Roads1 <> nil
        then
          for i := 0 to pred(ConnectionCount) do
            begin
              ExtraCnnt := ExtraConnectionInfo[i];
              if ExtraCnnt <> nil
                then
                  begin
                    Blk := Connections[i].Block;
                    Roads2 := Blk.Circuits[cirRoads];
                    if Roads2 = nil
                      then ExtraCnnt.Connected := not ObjectIs('TConnectedBlock', Blk)
                      else ExtraCnnt.Connected := (Roads2.Matches( Roads1 ) > 0) or CloseEnought(Blk, self.Block);
                  end;
            end;
    end;

  class function TPullOutput.BestCollection( aSize : integer ) : TCollection;
    begin
      result := TNotifiedCollection.Create( 0, rkUse )
    end;

  procedure TPullOutput.LoadFromBackup( Reader : IBackupReader );
    var
      Slice   : TSlice;
      i       : integer;
    begin
      inherited;
      fSlices := TCollection.Create( 0, rkBelonguer );
      for i := 0 to pred(fConnections.Count) do
        begin
          Slice  := TSlice.Create;
          Reader.ReadSingle( 'YearInfo' + IntToStr(i) + '.Value', 0 ); // Slice.ExtraConnectionInfo.YearValue := Reader.ReadSingle( 'YearInfo' + IntToStr(i) + '.Value', 0 );
          Reader.ReadCurrency( 'YearInfo' + IntToStr(i) + '.Cost', 0 ); //Slice.ExtraConnectionInfo.YearCost  := Reader.ReadCurrency( 'YearInfo' + IntToStr(i) + '.Cost', 0 );
          Slice.ExtraConnectionInfo.OverPrice := Reader.ReadByte( 'OverPrice' + IntToStr(i), 0 );
          Slice.ExtraConnectionInfo.Connected := Reader.ReadBoolean( 'Connected' + IntToStr(i), false );
          Slice.ExtraConnectionInfo.Priority  := Reader.ReadByte( 'Priority' + IntToStr(i), 0 );
          fSlices.Insert( Slice );
        end;
      TNotifiedCollection(fConnections).OnModified := OnConnectionsModified;
      ComputePriorities;  // >> temporary!
      fIdxAux   := NoIndex;
    end;

  procedure TPullOutput.StoreToBackup( Writer : IBackupWriter );
    var
      i   : integer;
      aux : string;
    begin
      inherited;
      CheckSlices; // Just in case..
      for i := 0 to pred(fConnections.Count) do
        begin
          aux := 'YearInfo' + IntToStr(i) + '.Value';
          Writer.WriteSingle( aux, 0{TSlice(fSlices[i]).ExtraConnectionInfo.YearValue} );
          aux := 'YearInfo' + IntToStr(i) + '.Cost';
          Writer.WriteCurrency( aux, 0{TSlice(fSlices[i]).ExtraConnectionInfo.YearCost} );
          aux := 'OverPrice' + IntToStr(i);
          Writer.WriteByte( aux, TSlice(fSlices[i]).ExtraConnectionInfo.OverPrice );
          aux := 'Connected' + IntToStr(i);
          Writer.WriteBoolean( aux, TSlice(fSlices[i]).ExtraConnectionInfo.Connected );
          aux := 'Priority' + IntToStr(i);
          Writer.WriteByte( aux, TSlice(fSlices[i]).ExtraConnectionInfo.Priority );
        end;
      aux := '';
    end;

  procedure TPullOutput.StoreToCache( Cache : TObjectCache );
    begin
      inherited;
      if (MetaOutput.MetaFluid <> nil) and (MetaOutput.MetaFluid.MarketPrice > 0)
        then Cache.WriteInteger( 'AvgPrice', round(100*fParPrice.Average/MetaOutput.MetaFluid.MarketPrice) );
    end;

  procedure TPullOutput.AutoConnect( loaded : boolean );
    begin
      inherited;
      if MetaOutput <> nil
        then
          begin
            //fParValue    := Block.Facility.Town.Parameters[tidTownParameter_OutputValue + MetaOutput.MetaFluid.Id];
            //fParCapacity := Block.Facility.Town.Parameters[tidTownParameter_OutputCapacity + MetaOutput.MetaFluid.Id];
            //fParQuality  := Block.Facility.Town.Parameters[tidTownParameter_OutputQuality + MetaOutput.MetaFluid.Id];
            fParPrice := Block.Facility.Town.Parameters[tidTownParameter_OutputPrice + MetaOutput.MetaFluid.Id];
          end;
    end;

  procedure TPullOutput.CheckSlices;
    var
      i     : integer;
      Slice : TSlice;
    begin
      if fSlices.Count <> fConnections.Count
        then
          begin
            for i := pred(fSlices.Count) downto fConnections.Count do
              fSlices.AtDelete(i);
            for i := fSlices.Count to pred(fConnections.Count) do
              begin
                Slice := TSlice.Create;
                Slice.tickId := GetCurrentTick;
                fSlices.Insert(Slice);
              end;
            Logs.Log(tidLog_Survival, TimeToStr(Now) + ' Connections and Slices differ in facility: (' + IntToStr(Block.xPos) + ',' + IntToStr(Block.yPos) + ')');
          end;
    end;

  procedure TPullOutput.AutoRepair(Block : TBlock; index : integer);
    begin
      inherited;
      CheckSlices;
    end;

  procedure TPullOutput.UpdateFluid;
    begin
      fLastQ := realmax(0, FluidData.Q - POutputData(FluidData).Extra.Q);
    end;

  // TOrdinance

  constructor TOrdinance.Create( anId, aName, aDesc : string; aNumId : TOrdinanceNumId; aPublicOpinion : array of integer; aCost : TMoney );
    var
      k : TPeopleKind;
    begin
      inherited Create( anId );
      fName  := aName;
      fDesc  := aDesc;
      fNumId := aNumId;
      for k := low(k) to high(k) do
        fPublicOpinion[k] := aPublicOpinion[integer(k)];
      fCost := aCost;
    end;


  // TLoan

  destructor TLoan.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      if (fDebtor <> nil) and (fDebtor.Loans <> nil)
        then fDebtor.Loans.Delete( self );
      inherited;
    end;

  procedure TLoan.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fInterest := Reader.ReadByte( 'Interest', 0 );
      fAmount   := Reader.ReadCurrency( 'Amount', 0 );
      fSlice    := Reader.ReadCurrency( 'Slice', 0 );
      fTerm     := Reader.ReadInteger( 'Term', 0 );
      fDate     := Reader.ReadInteger( 'Date', 0 );
      Reader.ReadObject( 'Debtor', fDebtor, nil );
      Reader.ReadObject( 'Bank', fBank, nil );
    end;

  procedure TLoan.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteByte( 'Interest', fInterest );
      Writer.WriteCurrency( 'Amount', fAmount );
      Writer.WriteCurrency( 'Slice', fSlice );
      Writer.WriteInteger( 'Term', fTerm );
      Writer.WriteInteger( 'Date', fDate );
      Writer.WriteObjectRef( 'Debtor', fDebtor );
      Writer.WriteObjectRef( 'Bank', fBank );
    end;


  // TBank

  constructor TBank.Create( anOwner : TMoneyDealer );
    begin
      inherited Create;
      fOwner    := anOwner;
      fLoans    := TCollection.Create( 0, rkBelonguer );
      fTerm     := 20;
      fInterest := 2;
    end;

  destructor TBank.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fLoans.Free;
      inherited;
    end;

  procedure TBank.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
    var
      i     : integer;
      ToPay : TMoney;
    begin
      if PeriodType = perDay
        then
          begin
            Lock;
            try
              for i := pred(fLoans.Count) downto 0 do
                if TLoan(fLoans[i]).fDebtor = nil
                  then fLoans.AtDelete(i);
              for i := pred(fLoans.Count) downto 0 do
                with TLoan(fLoans[i]) do
                  if Timer.GetVirtualTimeAbs - fDate >= 24*365
                    then
                      begin
                        ToPay := fSlice + fInterest*fAmount/100;
                        fAmount := fAmount - fSlice;
                        fDebtor.GenMoney( -ToPay, accIdx_Bank_LoanPayments );
                        if Owner <> nil
                          then Owner.GenMoney( ToPay, accIdx_Bank_LoanIncome );
                        fDate := Timer.GetVirtualTimeAbs;
                        dec( fTerm );
                        if fTerm = 0 // Loan is paid
                          then
                            begin
                              fDebtor.Loans.Delete( fLoans[i] );
                              fLoans.AtDelete( i );
                            end;
                      end
            finally
              Unlock;
            end;
        end;
    end;

  { << OLD ONE!
  function TBank.EstimateLoan( Client : TMoneyDealer ) : TMoney;
    const
      MaxLoanAmount = 200*1000*1000;

    function MoneyPerYear : TMoney;
      var
        year, useless : word;
        hours         : integer;
        i             : integer;
        Amount        : TMoney;
      begin
        try
          // Compute number of hours in current year
          DecodeDate( fTimer.GetVirtualTime, year, useless, useless );
          hours := fTimer.GetVirtualTimeAbs - integer(year - YearZero)*365*24;

          // Compute net profit per year
          if Owner <> nil
            then
              begin
                if (Client.Accounts.AccountArray[accIdx_Construction] <> nil) and
                   (Client.Accounts.AccountArray[accIdx_Bank] <> nil) and
                   (hours <> 0)
                  then
                    result :=
                      365*24*(Client.Accounts.MasterAccount.Value
                        - Client.Accounts.AccountArray[accIdx_Construction].Value
                        - Client.Accounts.AccountArray[accIdx_TransfersIn].Value
                        - Client.Accounts.AccountArray[accIdx_TransfersOut].Value
                        - Client.Accounts.AccountArray[accIdx_Compensations].Value
                        - 2*Client.Accounts.AccountArray[accIdx_Bank].Value)/hours
                  else result := 365*24*Client.NetProfit;
              end
            else result := MaxLoanAmount;

          // Check existing loans
          Amount := 0;
          Client.Loans.Lock;
          try
            for i := 0 to pred(Client.Loans.Count) do
              Amount := Amount + TLoan(Client.Loans[i]).Amount;
          finally
            Client.Loans.Unlock;
          end;
          result := realmin( result, realmax(0, MaxLoanAmount - Amount) );
        except
          result := 0;
        end;
      end;

    var
      mpy   : extended;
      limit : extended;
    begin
      if Owner <> nil
        then limit := Owner.BankLoanLimit
        else limit := MaxLoan;
      mpy := MoneyPerYear;
      if mpy <= 0
        then result := 0
        else
          begin
            mpy := Term*trunc(mpy/1000000)*1000000;
            if mpy > limit
              then result := limit
              else result := mpy;
          end;
    end;
  }

  function TBank.EstimateLoan( Client : TMoneyDealer ) : TMoney;
    const
      MaxLoanAmount = 200*1000*1000*1000.0;
    begin
      if Owner <> nil
        then result := Owner.BankLoanLimit
        else result := currmax( 0, Client.LoanLimit - Client.GetLoanAmount );
    end;

  function TBank.AskLoan( Client : TMoneyDealer; Amount : TMoney ) : TBankRequestResult;
    var
      Loan      : TLoan;
      TotalDebt : TMoney;
    begin
      Lock;
      try
        if LoanApproved( Client, Amount )
          then
            begin
              TotalDebt := Client.GetLoanAmount + Amount;
              Loan := TLoan.Create;
              Loan.fBank     := self;
              Loan.fDebtor   := Client;
              Loan.fAmount   := Amount;
              if Owner <> nil
                then
                  begin
                    Loan.fTerm     := Term;
                    Loan.fInterest := Interest;
                  end
                else
                  begin
                    Loan.fTerm := max( 5, round(200 - TotalDebt/10000000));
                    Loan.fInterest := min( 50, round(TotalDebt/100000000) );
                  end;
              Loan.fDate     := Timer.GetVirtualTimeAbs;
              Loan.fSlice    := Amount/Loan.fTerm;
              fLoans.Insert( Loan );
              Client.Loans.Insert( Loan );
              Client.GenMoney( Amount, accIdx_Bank_Loans );
              if ObjectIs( TTycoon.ClassName, Client )
                then
                  Locator.SendEvent(
                    TEvent.Create(
                      0,
                      Timer.GetVirtualTimeAbs,
                      Timer.GetVirtualTime,
                      20000,
                      1000,
                      InstantiateMultiString( mtidMsgLoan, [TTycoon(Client).Name, FormatMoney(Amount), self.Name] ),
                      '', '' ));
              if Owner <> nil
                then
                  begin
                    if Owner.Budget >= Amount
                      then result := brqApproved
                      else
                        begin
                          result := brqNotEnoughFunds;
                          Amount := Owner.Budget;
                        end;
                    Owner.GenMoney( -Amount, accIdx_Bank_IssuedLoans );
                  end
                else result := brqApproved;
              ModelServerCache.UpdateObjectCache(Client, -1, -1);
            end
          else result := brqRejected;
      finally
        Unlock;
      end;
    end;

  procedure TBank.DealerDeleted( Dealer : TMoneyDealer );
    var
      i : integer;
    begin
      Lock;
      try
        for i := pred(fLoans.Count) downto 0 do
          if TLoan(fLoans[i]).Debtor = Dealer
            then
              begin
                if Owner <> nil
                  then Owner.GenMoney( TLoan(fLoans[i]).Amount, accIdx_Compensations );
                fLoans.AtDelete( i );
              end;
      finally
        Unlock;
      end;
    end;

  procedure TBank.Loaded;
    var
      i : integer;
    begin
      for i := pred(Loans.Count) downto 0 do
        if TLoan(Loans[i]).fDebtor = nil
          then Loans.AtDelete(i);
    end;

  function TBank.LoanApproved( Client : TMoneyDealer; Amount : TMoney ) : boolean;
    begin
      result := (Amount > 0) and (not ObjectIs(TTycoon.ClassName, Client) or TTycoon(Client).CanGetLoan(Amount)) and (EstimateLoan( Client ) >= Amount);
    end;

  procedure TBank.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Owner', fOwner, nil );
      Reader.ReadObject( 'Loans', fLoans, nil );
      fTerm := Reader.ReadInteger( 'Term', 1 );
      fInterest := Reader.ReadByte( 'Interest', 2 );
      //PackLoans; >> NOOHH!!!!!!!!!!
    end;

  procedure TBank.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObjectRef( 'Owner', fOwner );
      Writer.WriteObject( 'Loans', fLoans );
      Writer.WriteInteger( 'Term', fTerm );
      Writer.WriteByte( 'Interest', fInterest );
    end;


  // TMoneyDealer

  constructor TMoneyDealer.Create;
    begin
      inherited;
      fAccounts     := TAccountingSystem.Create;
      fLoans        := TLockableCollection.Create( 0, rkUse );
      fBankLoanPerc := 20;
    end;

  destructor TMoneyDealer.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      CancelLoans;
      fLoans.Free;
      fAccounts.Free;
      inherited;
    end;

  function TMoneyDealer.GetBudget : TMoney;
    begin
      result := 0;
    end;

  procedure TMoneyDealer.SetBudget( aBudget : TMoney );
    begin
    end;

  procedure TMoneyDealer.CancelLoans;
    var
      i     : integer;
      Banks : TCollection;
    begin
      Banks := TCollection.Create( 0, rkUse );
      for i := 0 to pred(fLoans.Count) do
        if Banks.IndexOf( TLoan(fLoans[i]).Bank ) = NoIndex
          then Banks.Insert( TLoan(fLoans[i]).Bank );
      for i := 0 to pred(Banks.Count) do
        TBank(Banks[i]).DealerDeleted( self );
      Banks.Free;
    end;

  procedure TMoneyDealer.GenMoney( Money : TMoney; Reason : TMoneyReason );
    var
      Account : TAccount;
    begin
      if (Reason > accIdx_None) and (Reason < MaxAccounts)
        then
          try
            Account := Accounts.AccountArray[Reason];
            if Account <> nil
              then Account.Value := Account.Value + Money
              else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Account not found: ' + IntToStr(Reason) );
              //raise Exception( 'Invalid account Id: ' + IntToStr(Reason) );
          except
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' error in GenMoney: ' + IntToStr(Reason) );
          end
        else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' GenMoney out of bounds: ' + IntToStr(Reason) );
    end;

  procedure TMoneyDealer.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
    begin
      case PeriodType of
        perHour :
          begin
            if (Timer <> niL) and (Timer.dt > 0)
              then fNetProfit := (Accounts.MasterAccount.Value - fLastPeriod)/Timer.dt
              else fNetProfit := (Accounts.MasterAccount.Value - fLastPeriod)/PeriodCount;
            fLastPeriod := Accounts.MasterAccount.Value;
          end;
        perYear :
          fAccounts.EndOfPeriod( false );
      end;
    end;

  procedure TMoneyDealer.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fNetProfit := Reader.ReadCurrency( 'NetProfit', 0 );
      Reader.ReadObject( 'Accounts', fAccounts, nil );
      if fAccounts = nil
        then fAccounts := TAccountingSystem.Create;
      Reader.ReadObject( 'Loans', fLoans, nil );
      if fLoans = nil
        then fLoans := TLockableCollection.Create( 0, rkBelonguer );
      fBankLoanPerc := Reader.ReadByte( 'BankLoanPerc', 20 );
    end;

  procedure TMoneyDealer.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteCurrency( 'NetProfit', fNetProfit );
      Writer.WriteUnsafeObject( 'Accounts', fAccounts );
      Writer.WriteObject( 'Loans', fLoans );//Writer.WriteUnsafeObject( 'Loans', fLoans );
      Writer.WriteByte( 'BankLoanPerc', fBankLoanPerc );
    end;

  procedure TMoneyDealer.Loaded;

    procedure PackLoans;
      var
        i : integer;
      begin
        for i := pred(Loans.Count) downto 0 do
          if (Loans[i] = nil) or (TLoan(Loans[i]).Bank = nil)
            then Loans.AtDelete( i );
      end;

    begin
      PackLoans;
    end;

  function TMoneyDealer.GetBankLoanPerc : TPercent;
    begin
      Lock;
      try
        result := fBankLoanPerc;
      finally
        Unlock;
      end;
    end;

  procedure TMoneyDealer.SetBankLoanPerc( Value : TPercent );
    begin
      Lock;
      try
        fBankLoanPerc := min(100, Value);
      finally
        Unlock;
      end;
    end;

  function TMoneyDealer.GetLoanLimit : TMoney;
    begin
      result := MaxLoan;
    end;

  function TMoneyDealer.GetLoanAmount : TMoney;
    var
      i      : integer;
      Amount : TMoney;
    begin
      try
        Amount := 0;
        Loans.Lock;
        try
          for i := 0 to pred(Loans.Count) do
            Amount := Amount + TLoan(Loans[i]).Amount;
        finally
          Loans.Unlock;
        end;
        result := Amount;
      except
        result := 0;
      end;
    end;


  function TMoneyDealer.BankLoanLimit : TMoney;
    begin
      result := realmin( 1, (BankLoanPerc/100) )*Budget;
    end;

  procedure TMoneyDealer.Check;
    begin
    end;

  // TMetaTownParameter

  constructor TMetaTownParameter.Create( anId, aName : string; anAutoClear : boolean );
    begin
      inherited Create( anId );
      fName      := aName;
      fAutoClear := anAutoClear;
      Cacheable  := false;
    end;

  procedure TMetaTownParameter.RetrieveTexts( Container : TDictionary );
    begin
      inherited;
      if fName_MLS = nil
        then fName_MLS := TMultiString.Create;
      fName_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Name'];
    end;

  procedure TMetaTownParameter.StoreTexts( Container : TDictionary );
    begin
      inherited;
      Container.Values[Family + '.' + Id + '.' + 'Name'] := fName;
    end;

  procedure TMetaTownParameter.Register;
    begin
      inherited Register( tidClassFamily_TownParameters );
    end;


  // TTownParameter

  constructor TTownParameter.Create( aMetaTownParameter : TMetaTownParameter );
    begin
      inherited Create;
      fMetaTownParameter := aMetaTownParameter;
    end;

  function TTownParameter.GetValue : double;
    begin
      Lock;
      try
        result := fValue;
      finally
        Unlock;
      end;
    end;

  function TTownParameter.GetAverage : double;
    begin
      if fCount > 0
        then result := Value/fCount
        else result := 0;
    end;

  procedure TTownParameter.IncCount( amount : double );
    begin
      Lock;
      try
        fCurrCount := fCurrCount + amount;
      finally
        Unlock;
      end;
    end;

  procedure TTownParameter.Update;
    begin
      Lock;
      try
        fValue := fCurrValue;
        fCount := fCurrCount;
        if MetaTownParameter.AutoClear
          then
            begin
              fCurrValue := 0;
              fCurrCount := 0;
            end;
      finally
        Unlock;
      end;
    end;

  procedure TTownParameter.LoadFromBackup( Reader : IBackupReader );
    var
      MetaParmId : string;
    begin
      inherited;
      MetaParmId := Reader.ReadString( 'MetaParmId', '' );
      fMetaTownParameter := TMetaTownParameter(TheClassStorage.ClassById[tidClassFamily_TownParameters, MetaParmId]);
      fValue     := Reader.ReadSingle( 'Value', 0 );
      fCurrValue := Reader.ReadSingle( 'CurrValue', 0 );
    end;

  procedure TTownParameter.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'MetaParmId', fMetaTownParameter.Id );
      Writer.WriteSingle( 'Value', Value );
      Writer.WriteSingle( 'CurrValue', fCurrValue );
    end;


  // TTown

  const
    fNextTownId : TTownId = 1;

  constructor TTown.Create( aName : string; aXPos, aYPos : integer; aCluster : TCluster; aModelFactory : IModelFactory; aMailServer : IMailServer );

    procedure InitTaxes;
      var
        count : integer;
        i     : integer;
        MT    : TMetaTax;
        Tax   : TTax;
      begin
        try
          count := TheClassStorage.ClassCount[tidClassFamily_Taxes];
        except
          count := 0;
        end;
        fTaxes := TLockableCollection.Create( count, rkBelonguer );
        for i := 0 to pred(count) do
          begin
            MT  := TMetaTax(TheClassStorage.ClassByIdx[tidClassFamily_Taxes, i]);
            Tax := MT.Instantiate;
            Tax.Subsidized := true;
            fTaxes.Insert( Tax );
          end;
      end;

    begin
      inherited Create;
      Name := aName;
      fId := fNextTownId;
      inc( fNextTownId );
      fXPos := aXPos;
      fYPos := aYPos;
      InitTaxes;
      InitParameters;
      fCluster      := aCluster;
      fOrdinances   := TLockableCollection.Create( 0, rkUse );
      fOrdinanceSet := [];
      fMayor        := aModelFactory.NewTycoonRole( tidRole_Mayor, Format( mtidMayorTitle.Values[langDefault], [aName]), TMayor, MayorInitialBudget );
      fTownCompany  := aModelFactory.NewCompany( aName, aCluster, fMayor );
      //aMailServer.NewMailAccount( mtidMayorEmail 'mayor@' + aName + '.gov', fMayor.Name, '', true );
      aMailServer.NewMailAccount( mtidMayorEmail.Values[langDefault] + '@' + aName + '.gov', fMayor.Name, '', true );
      fRndNames := TStringList.Create;
    end;

  destructor TTown.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fParameters.Free;
      fParmHash.Free;
      fOrdinances.Free;
      fTaxes.Free;
      inherited;
    end;

  function TTown.GetHasMayor : boolean;
    begin
      result := (fMayor <> nil) and (fMayor.SuperRole <> nil);
    end;

  function TTown.GetOrdinance( NumId : TOrdinanceNumId ) : boolean;
    begin
      result := NumId in fOrdinanceSet;
    end;

  function TTown.GetParameter( Id : string ) : TTownParameter;
    {
    var
      i : integer;
    }
    begin
      {
      i := 0;
      while (i < fParameters.Count) and (TTownParameter(fParameters[i]).MetaTownParameter.Id <> Id) do
        inc( i );
      if i < fParameters.Count
        then result := TTownParameter(fParameters[i])
        else result := nil;
      }
      result := TTownParameter(fParmHash[Id]);
    end;

  procedure TTown.UpdateParameters;
    var
      i : integer;
    begin
      fParameters.Lock;
      try
        for i := 0 to pred(fParameters.Count) do
          TTownParameter(fParameters[i]).Update;
      finally
        fParameters.Unlock;
      end;
    end;

  function TTown.GetContextStatusStr( ToTycoon : TTycoon ) : string;
    begin
      result := '';
    end;

  procedure TTown.SetOrdinance( Id : string );
    var
      Ordinance : TOrdinance;
    begin
      Ordinance := TOrdinance(TheClassStorage.ClassById[tidClassFamily_Ordinances, Id]);
      fOrdinanceSet := fOrdinanceSet + [Ordinance.NumId];
      fOrdinances.Insert( Ordinance );
    end;

  procedure TTown.DelOrdinance( Id : string );
    var
      Ordinance : TOrdinance;
    begin
      Ordinance := TOrdinance(TheClassStorage.ClassById[tidClassFamily_Ordinances, Id]);
      fOrdinanceSet := fOrdinanceSet - [Ordinance.NumId];
      fOrdinances.Delete( Ordinance );
    end;

  function TTown.GetTaxes( TaxId : string ) : TTax;
    var
      i : integer;
    begin
      i := 0;
      while (i < fTaxes.Count) and (TTax(fTaxes[i]).MetaTax.Id <> TaxId) do
        inc( i );
      if i < fTaxes.Count
        then result := TTax(fTaxes[i])
        else result := nil;
    end;

  function TTown.GetMinSalary( Kind : TPeopleKind ) : TPercent;
    begin
      result := max( fMinSalaries[Kind], fWorldLocator.GetMinSalary( Kind ) );
    end;

  function TTown.GetMayorSalary( Kind : TPeopleKind ) : TPercent;
    begin
      result := fMinSalaries[Kind];
    end;
    
  procedure TTown.SetMinSalary( Kind : TPeopleKind; Value : TPercent );
    begin
      fMinSalaries[Kind] := Value;
    end;

  procedure TTown.GenMoney( Money : TMoney; Reason : TMoneyReason );
    begin
      inherited;
      fTownCompany.GenMoney( Money, Reason );
    end;

  procedure TTown.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
    var
      i : integer;
    begin
      inherited;
      case PeriodType of
        perYear :
          for i := 0 to pred(fTaxes.Count) do
            TTax(fTaxes[i]).Reset;
        perMonth :
          GenerateMediaNames;
        perHour :
          if HasMayor
            then Mayor.GenMoney(-10*fRoadBlocks, accIdx_RoadMaintenance);  
      end;
      {
      if PeriodType = perYear
        then
          for i := 0 to pred(fOrdinances.Count) do
            GenMoney( TOrdinance(fOrdinances[i]).Cost, mnOtherExpenditures );
      }
    end;

  procedure TTown.LoadFromBackup( Reader : IBackupReader );

    procedure ReadTaxes;
      var
        count    : integer;
        i        : integer;
        NewTaxes : TLockableCollection;
        Tax      : TTax;
        MT       : TMetaTax;
      begin
        count := Reader.ReadInteger( 'TaxCount', 0 );
        fTaxes := TLockableCollection.Create( 0, rkUse );
        // Load stored taxes
        for i := 0 to pred(count) do
          begin
            Reader.ReadObject( 'Tax.' + IntToStr(i), Tax, nil );
            if Tax <> nil
              then fTaxes.Insert( Tax );
          end;
        // Compile new taxes
        try
          count := TheClassStorage.ClassCount[tidClassFamily_Taxes];
        except
          count := 0;
        end;
        NewTaxes := TLockableCollection.Create( 0, rkBelonguer );
        for i := 0 to pred(count) do
          begin
            MT  := TMetaTax(TheClassStorage.ClassByIdx[tidClassFamily_Taxes, i]);
            Tax := Taxes[MT.Id];
            if Tax = nil
              then Tax := MT.Instantiate;
            NewTaxes.Insert( Tax );
          end;
        fTaxes.Free;
        fTaxes := NewTaxes;
      end;

    var
      ClusterName : string;
      count       : integer;
      i           : integer;
      kind        : TPeopleKind;
    begin
      inherited;
      fid   := Reader.ReadInteger( 'TownId', 0 );
      fName := Reader.ReadString( 'Name', '' );
      xPos  := Reader.ReadInteger( 'xPos', 0 );
      yPos  := Reader.ReadInteger( 'yPos', 0 );
      ClusterName := Reader.ReadString( 'ClusterName', '' );
      if ClusterName = ''
        then ClusterName := 'PGI';
      fCluster := TCluster(TheClassStorage.ClassById[tidClassFamily_Clusters, ClusterName]);
      if fTaxes = nil
        then fTaxes := TLockableCollection.Create( 0, rkBelonguer );
      fOrdinances := TLockableCollection.Create( 0, rkUse );
      count := Reader.ReadInteger( 'OrdinanceCount', 0 );
      for i := 0 to pred(count) do
        SetOrdinance( Reader.ReadString( 'Ordinance[' + IntToStr(i) + ']', '' ) );
      Reader.ReadObject( 'Mayor', fMayor, nil );
      Reader.ReadObject( 'TownCompany', fTownCompany, nil );
      ReadTaxes;
      for kind := low(kind) to high(kind) do
        fMinSalaries[kind] := Reader.ReadByte( PeopleKindPrefix[kind] + 'MinSalary', 0 );
      InitParameters;          
    end;

  procedure TTown.StoreToBackup( Writer : IBackupWriter );

    procedure StoreTaxes;
      var
        i     : integer;
        count : integer;
        aux   : string;
      begin
        // First count modified taxes
        count := 0;
        for i := 0 to pred(fTaxes.Count) do
          if not TTax(fTaxes[i]).HasDefaultValue
            then inc( count );
        Writer.WriteInteger( 'TaxCount', count );

        // Then store modified taxed
        count := 0;
        for i := 0 to pred(fTaxes.Count) do
          if not TTax(fTaxes[i]).HasDefaultValue
            then
              begin
                aux := 'Tax.' + IntToStr(count);
                Writer.WriteObject( aux, fTaxes[i] );
                inc( count );
              end;
        aux := '';
      end;

    var
      i    : integer;
      kind : TPeopleKind;
      aux  : string;
    begin
      inherited;
      Writer.WriteInteger( 'TownId', fid );
      Writer.WriteString( 'Name', fName );
      Writer.WriteInteger( 'xPos', xPos );
      Writer.WriteInteger( 'yPos', yPos );
      Writer.WriteString( 'ClusterName', Cluster.Id );
      Writer.WriteInteger( 'OrdinanceCount', fOrdinances.Count );
      for i := 0 to pred(fOrdinances.Count) do
        begin
          aux := 'Ordinance[' + IntToStr(i) + ']';
          Writer.WriteString( aux, TOrdinance(fOrdinances).Id );
        end;
      Writer.WriteObjectRef( 'Mayor', fMayor );
      Writer.WriteObjectRef( 'TownCompany', fTownCompany );
      StoreTaxes;
      for kind := low(kind) to high(kind) do
        begin
          aux := PeopleKindPrefix[kind] + 'MinSalary';
          Writer.WriteByte( aux, fMinSalaries[kind] );
        end;
      aux := '';
    end;

  procedure TTown.Loaded;
    begin
      inherited;
      fRndNames := TStringList.Create;
    end;

  procedure TTown.InitParameters;
    var
      count : integer;
      i     : integer;
      MP    : TMetaTownParameter;
      Parm  : TTownParameter;
    begin
      try
        count := TheClassStorage.ClassCount[tidClassFamily_TownParameters];
      except
        count := 0;
      end;
      fParameters := TLockableCollection.Create( count, rkBelonguer );
      fParmHash   := TMapStringToObject.Create( mmUse );
      for i := 0 to pred(count) do
        begin
          MP := TMetaTownParameter(TheClassStorage.ClassByIdx[tidClassFamily_TownParameters, i]);
          Parm := TTownParameter.Create( MP );
          fParameters.Insert( Parm );
          fParmHash[Parm.MetaTownParameter.Id] := Parm;
        end;
    end;

  procedure TTown.TycoonDeleted( Tycoon : TTycoon );
    begin
    end;

  function TTown.GetRndName(index : integer) : string;
    var
      cnt : integer;
    begin
      Lock;
      try
        cnt := fRndNames.Count;
        if cnt = 0
          then GenerateMediaNames;
        if cnt > 0
          then
            begin
              index  := min(pred(cnt), index);
              result := fRndNames[index];
            end
          else result := '';
      finally
        Unlock;
      end;
    end;

  procedure TTown.GenerateMediaNames;
    var
      i : integer;
    begin
      Lock;
      try
        fRndNames.Clear;
        for i := 0 to 49 do
          fRndNames.Add(MediaNameGenerator.GenerateName(random(4), 0));
      finally
        Unlock;
      end;
    end;


  // TCluster

  constructor TCluster.Create( anId : string );
    begin
      inherited Create( anId );
      fName_MLS := TMultiString.Create;
    end;

  procedure TCluster.RetrieveTexts( Container : TDictionary );
    begin
      inherited;
      if fName_MLS = nil
        then fName_MLS := TMultiString.Create;
      fName_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Name'];
    end;

  procedure TCluster.StoreTexts( Container : TDictionary );
    begin
      inherited;
      Container.Values[Family + '.' + Id + '.' + 'Name'] := Id;
    end;

  function TCluster.NameNewspaper( TownName : string ) : string;
    begin
      result := TownName + ' Herald';
    end;


{
  // TCompanyResearch

  constructor TCompanyResearch.Create( aCompany : TCompany );
    begin
      inherited Create;
      fCompany      := aCompany;
      fInventions   := TCollection.Create( 0, rkUse );
      fInventionSet := TInventionSet.Create(5);
    end;

  destructor TCompanyResearch.Destroy;
    begin
      fInventions.Free;
      fInventionSet.Free;
      inherited;
    end;

  function TCompanyResearch.GetHasInvention( NumId : TInventionNumId ) : boolean;
    begin
      result := fInventionSet.Included(NumId);
    end;

  function TCompanyResearch.GetCanResearch( Invention : TInvention ) : boolean;
    var
      i : integer;
    begin
      if Invention.Req.Count = 0
        then result := true
        else
          begin
            i := 0;
            while (i < Invention.Req.Count) and HasInvention[TInvention(Invention.Req[i]).NumId] do
              inc(i);
            result := i = Invention.Req.Count;
          end;
    end;

  procedure TCompanyResearch.LoadFromBackup( Reader : IBackupReader );

    procedure LoadInventions;
      var
        count       : integer;
        i           : integer;
        InventionId : string;
        Invention   : TInvention;
      begin
        fInventions   := TCollection.Create( 0, rkUse );
        fInventionSet := TInventionSet.Create(0);
        count := Reader.ReadInteger( 'InventionCount', 0 );
        for i := 0 to pred(count) do
          begin
            InventionId := Reader.ReadString( 'Invention.' + IntToStr(i), '' );
            try
              //Invention := TInvention(TheClassStorage.ClassById[tidClassFamily_Inventions, InventionId]);
              Invention := FindInvention(InventionId);
              if Invention <> nil
                then
                  begin
                    fInventions.Insert( Invention );
                    fInventionSet.Include(Invention.NumId);
                  end;
            except
            end;
          end;
      end;
}

{
    procedure LoadInventions;
      var
        count       : integer;
        i           : integer;
        InventionId : string;
        Invention   : TInvention;
      begin
        fInventions   := TCollection.Create( 0, rkUse );
        fInventionSet := TInventionSet.Create(0);
        count := Reader.ReadInteger( 'InventionCount', 0 );
        for i := 0 to pred(count) do
          begin
            InventionId := Reader.ReadString( 'Invention.' + IntToStr(i), '' );
            try
              Invention := TInvention(TheClassStorage.ClassById[tidClassFamily_Inventions, InventionId]);
              if Invention <> nil
                then
                  begin
                    fInventions.Insert( Invention );
                    fInventionSet.Include(Invention.NumId);
                  end;
            except
            end;
          end;
      end;
}
{
    begin
      inherited;
      Reader.ReadObject( 'Company', fCompany, nil );
      fKind := Reader.ReadString( 'Kind', '' );
      LoadInventions;
    end;

  procedure TCompanyResearch.StoreToBackup( Writer : IBackupWriter );

    procedure StoreInventions;
      var
        i : integer;
      begin
        Writer.WriteInteger( 'InventionCount', fInventions.Count );
        for i := 0 to pred(fInventions.Count) do
          Writer.WriteString( 'Invention.' + IntToStr(i), TInvention(fInventions[i]).Id );
      end;

    begin
      inherited;
      Writer.WriteObjectRef( 'Company', fCompany );
      Writer.WriteString( 'Kind', fKind );
      StoreInventions;
    end;

  procedure TCompanyResearch.DeclareInvention(Invention : TInvention);
    begin
      fInventions.Insert( Invention );
      fLevel := fLevel + Invention.Level;
      fInventionSet.Include(Invention.NumId);
    end;
}

  // TCompanyDirection

  constructor TCompanyDirection.Create( anId : string );
    begin
      inherited Create;
      fId := anId;
    end;

  procedure TCompanyDirection.Update;
    var
      limit : single;
    begin
      if fCount < SupFacIntervalMin
        then limit := 1
        else
          if fCount > SupFacIntervalMax
            then limit := 0
            else limit := (SupFacIntervalMax - fCount)/(SupFacIntervalMax - SupFacIntervalMin);
      if fDemand > 0.000001 //round(fDemand) > 0
        then fSupport := realmax(limit, realmin(2, fStrength/fDemand))
        else fSupport := 2;
      fLastDemand := fDemand;
      fLastCount  := fCount;
      fDemand     := 0;
      fStrength   := 0;
      fCount      := 0;

      fLastComplFacs := fComplFacs;
      fComplFacs    := 0;
    end;

  procedure TCompanyDirection.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fId       := Reader.ReadString( 'Id', '' );
      fStrength := Reader.ReadSingle( 'Strength', 0 );
      fDemand   := Reader.ReadSingle( 'Demand', 0 );
    end;

  procedure TCompanyDirection.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'Id', fId );
      Writer.WriteSingle( 'Strength', fStrength );
      Writer.WriteSingle( 'Demand', fDemand );
    end;


  // TProject

  constructor TProject.Create( aName : string; aKind : TProjectKind );
    begin
      inherited Create;
      fName := aName;
      fMembers := TLockableCollection.Create( 0, rkUse );
    end;

  destructor TProject.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fMembers.Free;
      inherited;
    end;

  procedure TProject.SetStatus( aStatus : TProjectStatus );
    var
      i : integer;
    begin
      fStatus := aStatus;
      fMembers.Lock;
      try
        for i := 0 to pred(fMembers.Count) do
          TFacility(fMembers[i]).Stopped := (fStatus = prjStopped)
      finally
        fMembers.Unlock;
      end;
    end;

  procedure TProject.AddMember( Member : TFacility );
    begin
      fMembers.Insert( Member );
    end;

  procedure TProject.DelMember( Member : TFacility );
    begin
      fMembers.Delete( Member );
    end;

  procedure TProject.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fName := Reader.ReadString( 'Name', '' );
      fKind := Reader.ReadWord( 'Kind', 0 );
      Reader.ReadObject( 'Members', fMembers, nil );
    end;

  procedure TProject.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'Name', fName );
      Writer.WriteWord( 'Kind', fKind );
      Writer.WriteLooseObject( 'Members', fMembers );
    end;


  // TCompanyMetaFacilityInfo

  constructor TCompanyMetaFacilityInfo.Create( aMetaFacility : TMetaFacility );
    begin
      inherited Create;
      fMetaFacility := aMetaFacility;
    end;

  procedure TCompanyMetaFacilityInfo.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fMetaFacility := TMetaFacility(TheClassStorage.ClassById[tidClassFamily_Facilities, Reader.ReadString( 'MetaFacility', '' )]);
      fCounter := Reader.ReadInteger( 'Counter', 0 );
    end;

  procedure TCompanyMetaFacilityInfo.StoreToBackup ( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'MetaFacility', fMetaFacility.Id );
      Writer.WriteInteger( 'Counter', fCounter );
    end;


  // TCompanyMetaFacilityList

  constructor TCompanyMetaFacilityList.Create( aCompany : TCompany );
    begin
      inherited Create;
      fCompany  := aCompany;
      fInfoList := TLockableCollection.Create( 0, rkBelonguer );
    end;

  destructor TCompanyMetaFacilityList.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fInfoList.Free;
      inherited;
    end;

  function TCompanyMetaFacilityList.GetMetaFacilityInfo( MetaFacility : TMetaFacility ) : TCompanyMetaFacilityInfo;
    var
      i : integer;
    begin
      fInfoList.Lock;
      try
        i := 0;
        while (i < fInfoList.Count) and (TCompanyMetaFacilityInfo(fInfoList[i]).MetaFacility <> MetaFacility) do
          inc( i );
        if i < fInfoList.Count
          then result := TCompanyMetaFacilityInfo(fInfoList[i])
          else result := nil;
      finally
        fInfoList.Unlock;
      end;
    end;

  function TCompanyMetaFacilityList.GetNextFacilityNumber( MetaFacility : TMetaFacility ) : integer;
    var
      MFInfo : TCompanyMetaFacilityInfo;
    begin
      MFInfo := MetaFacilityInfo[MetaFacility];
      if MFInfo = nil
        then
          begin
            MFInfo := TCompanyMetaFacilityInfo.Create( MetaFacility );
            fInfoList.Insert( MFInfo );
          end;
      inc( MFInfo.fCounter );
      result := MFInfo.fCounter;
    end;

  procedure TCompanyMetaFacilityList.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Company', fCompany, nil );
      Reader.ReadObject( 'InfoList', fInfoList, nil );
    end;

  procedure TCompanyMetaFacilityList.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObjectRef( 'Company', fCompany );
      Writer.WriteLooseObject( 'InfoList', fInfoList );
    end;


  // TCompanyInput

{
    fKind   : TMetaFluid;
    fValue  : TFluidValue;
    fDemand : TFluidValue;
    fInputs : TLockableCollection;
}
  constructor TCompanyInput.Create( aKind : TMetaFluid );
    begin
      inherited Create;
      fKind := aKind;
      fInputs := TLockableCollection.Create( 0, rkUse );
    end;

  destructor TCompanyInput.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fInputs.Free;
      inherited;
    end;

  procedure TCompanyInput.Spread;
    const
      StartRatio = 0.8;
    type
      PSliceArray = ^TSliceArray;
      TSliceArray = array[any] of TFluidValue;
    var
      SliceArray : PSliceArray;

    procedure ProjectNextPeriod( var value : TFluidValue; AbilityRatio : single );

      function Ability( Input : TInput ) : single;
        var
          theDt : single;
        begin
          if Input.Block.Facility.CriticalTrouble
            then result := 0
            else
              begin
                theDt := Input.Block.dt;
                if Input.ActualMaxFluid.Q > 0
                  then result := Input.LastValue.Q/(theDt*Input.ActualMaxFluid.Q)
                  else result := StartRatio/2 + 0.1;
              end;
        end;

      var
        i     : integer;
        Input : TInput;
        slice : TFluidValue;
      begin
        i := 0;
        while (value > 0) and (i < fInputs.Count) do
          begin
            Input := TInput(fInputs[i]);
            if round(10*Ability( Input )) >= round(10*AbilityRatio)
              then
                begin
                  slice := realmin( value, Input.MetaInput.fMaxFluid.Q - SliceArray[i] );
                  value := value - slice;
                  SliceArray[i] := SliceArray[i] + slice;
                end;
            inc( i );
          end;
      end;

    procedure UpdateMaxFluids;
      var
        i : integer;
      begin
        for i := 0 to pred(fInputs.Count) do
          TInput(fInputs[i]).ActualMaxFluid.Q := SliceArray[i];
      end;

    var
      i     : integer;
      Qsum  : double;
      ratio : single;
    begin
      // Slice the fluid value
      fValue.Q := 0;
      Qsum     := 0;
      fInputs.Lock;
      try
        for i := 0 to pred(fInputs.Count) do
          begin
            fValue.Q := fValue.Q + TInput(fInputs[i]).LastValue.Q;
            Qsum     := Qsum + TInput(fInputs[i]).LastValue.K*TInput(fInputs[i]).LastValue.Q;
          end;
      finally
        fInputs.Unlock;
      end;
      if fValue.Q > 0
        then fValue.K := round(Qsum/fValue.Q)
        else fValue.K := 0;
      if fDemand > 0
        then fRatio := fValue.Q/fDemand
        else fRatio := 1;

      // Plan for next period
      getmem( SliceArray, fInputs.Count*sizeof(SliceArray[0]) );
      try
        fillchar( SliceArray^, fInputs.Count*sizeof(SliceArray[0]), 0 );
        ratio := StartRatio;
        repeat
          ProjectNextPeriod( fDemand, ratio );
          ratio := ratio - StartRatio/2;
        until (fDemand = 0) or (ratio < 0);
        UpdateMaxFluids;
        fDemand := 0;
      finally
        freemem( SliceArray, fInputs.Count*sizeof(SliceArray[0]) );
      end;
    end;

  // TCompany

  const
    MaxCompInputs = 100;

  constructor TCompany.Create( anId : TCompanyId );
    begin
      inherited Create;
      fId                := anId;
      fAutoConnectLevels := [mglBasic];
      fInventionSet      := TInventionSet.Create(0);
      fInventions        := TLockableCollection.Create( 0, rkBelonguer );
      fDirections        := TLockableCollection.Create( 0, rkBelonguer );
      fProjects          := TLockableCollection.Create( 0, rkBelonguer );
      fFacilities        := TLockableCollection.Create( 0, rkUse );
      fMetaFacilityList  := TCompanyMetaFacilityList.Create( self );
      fPrivated          := true;
      InitCompanyInputs;
    end;

  destructor TCompany.Destroy;
    begin
      freemem( fCompanyInputIndex, MaxCompInputs*sizeof(TCompanyInput) );
      fCompanyInputs.Free;
      fInventionSet.Free;
      fInventions.Free;
      fDirections.Free;
      fProjects.Free;
      fFacilities.Free;
      fMetaFacilityList.Free;
      inherited;
    end;

  function TCompany.GetBudget : TMoney;
    begin
      if Owner <> nil
        then result := Owner.Budget
        else result := 0;
    end;

  function TCompany.GetTechnologyDescriptor : string;
    var
      i         : integer;
      Invention : TInvention;
    begin
      result := '';
      for i := 0 to pred(fInventions.Count) do
        begin
          Invention := TInventionRecord(fInventions[i]).Invention;
          result    := result + Invention.Id + ';';
        end;
    end;

  function TCompany.GetOwned : boolean;
    begin
      result := not fDeleted and (fOwner <> nil) and (fOwner.Companies.IndexOf(self) <> noIndex);
    end;

  procedure TCompany.GenMoney( Money : TMoney; Reason : TMoneyReason );
    begin
      inherited;
      if (Owner <> nil) and not fDeleted
        then Owner.GenMoney( Money, Reason );
    end;

  procedure TCompany.FacilityCreated( Facility : TFacility );
    begin
      fNewUniquenessMask := fNewUniquenessMask or Facility.MetaFacility.UniquenessMask;
      fUniquenessMask    := fUniquenessMask or Facility.MetaFacility.UniquenessMask;
      fFacilities.Insert( Facility );
      Facility.Company := self;
      if Owner <> nil
        then
          begin
            Owner.CountFacility(Facility, true);
            if Facility.MetaFacility.TypicalStage.MetaBlock.IsTransBlock
              then Owner.fWillTranscend := true;
          end;
      if Facility.CurrBlock <> nil
        then Facility.CurrBlock.RecalculateInventionsEffect;
    end;

  procedure TCompany.FacilityDestroyed( Facility : TFacility );
    begin
      fNewUniquenessMask := fNewUniquenessMask and not Facility.MetaFacility.UniquenessMask;
      fUniquenessMask    := fUniquenessMask and not Facility.MetaFacility.UniquenessMask;
      Facility.Company := nil;
      fFacilities.Delete( Facility );
      if Owner <> nil
        then
          begin
            Owner.CountFacility(Facility, false);
            if Facility.MetaFacility.TypicalStage.MetaBlock.IsTransBlock
              then Owner.fWillTranscend := false;
          end;
    end;

  procedure TCompany.UpdateParameters;
    var
      i : integer;
    begin
      fUniquenessMask    := fNewUniquenessMask;
      fNewUniquenessMask := 0;
      for i := 0 to pred(fDirections.Count) do
        TCompanyDirection(fDirections[i]).Update;
      fCompanyInputs.Lock;
      try
        for i := 0 to pred(fCompanyInputs.Count) do
          TCompanyInput(fCompanyInputs[i]).Spread;
      finally
        fCompanyInputs.Unlock;
      end;
    end;

  function TCompany.GetHasInvention(NumId : TInventionNumId ) : boolean;
    begin
      result := fInventionSet.Included(NumId);
    end;

  function TCompany.GetProject( name : string ) : TProject;
    var
      i : integer;
    begin
      i := 0;
      while (i < fProjects.Count) and (TProject(fProjects[i]).Name <> name) do
        inc( i );
      if i < fProjects.Count
        then result := TProject(fProjects[i])
        else result := nil;
    end;

  function TCompany.GetDirection( id : string ) : TCompanyDirection;
    var
      i : integer;
    begin
      i := 0;
      while (i < fDirections.Count) and (TCompanyDirection(fDirections[i]).Id <> id) do
        inc( i );
      if i < fDirections.Count
        then result := TCompanyDirection(fDirections[i])
        else
          begin
            result := TCompanyDirection.Create( id );
            fDirections.Insert( result );
          end;
    end;

  procedure TCompany.DeclareInvention(Invention : TInvention);
    var
      LicFee : TMoney;
      lang   : TLanguageId;
      added  : boolean;
      InvRec : TInventionRecord;
    begin
      InvRec := nil;
      if (Owner <> nil)
        then lang := Owner.Language
        else lang := langDefault;
      Lock;
      try
        if not fInventionSet.Included(Invention.NumId)
          then
            begin
              // Pay for the licence
              LicFee := Invention.GetFeePrice(self);
              GenMoney(-LicFee, accIdx_ResearchCenter_Research);
              // Add the invention
              fInventionSet.Include(Invention.NumId);
              InvRec := TInventionRecord.Create(Invention, LicFee + Invention.Price);
              fInventions.Insert(InvRec);
              // Send notification
              if (Owner <> nil)
                then
                  if Invention.EnablesTech
                    then Owner.SendNotification( ntkGenericEvent, '', Format( mtidMsgMainResearchCompleted.Values[lang], [Invention.Name_MLS.Values[lang]] ), gevnId_RefreshBuildPage )
                    else Owner.SendNotification( ntkChatMessage, '', Format( mtidMsgResearchCompleted.Values[lang], [Invention.Name_MLS.Values[lang]] ), 0 );
              // Inc the licence level
              Owner.LicenceLevel := Owner.fLicenceLevel + Invention.LicLevel;
              added := true;
            end
          else added := false;
      finally
        Unlock;
      end;
      if added
        then
          begin
            ModelServerCache.BackgroundCache(self, false);
            // Make all the blocks to recalculate its inventions
            RecalculateInventions;
            // Calculate usage of the new invention
            InvRec.Usage := CalcInventionUsage(Invention);
            // Recalculate invention usage cost
            RecalculateInventionApplicationCost;
            // check if the company needs to recache
            if Invention.EnablesTech
              then InvalidateCache(self, false);
            if Invention.LicLevel > 0
              then InvalidateCache(Owner, false);
          end;
    end;

  procedure TCompany.ReportUsage(Invention : TInvention; count : integer; use : boolean);
    var
      InvRec : TInventionRecord;
    begin
      Lock;
      try
        InvRec := FindInventionRecord(Invention);
        if InvRec <> nil
          then
            if use
              then
                begin
                  InvRec.Usage  := InvRec.Usage + count;
                  fResearchCost := fResearchCost + InvRec.HourlyCostPerFac(StdHourlyCost);
                end
              else
                begin
                  InvRec.Usage  := InvRec.Usage - count;
                  fResearchCost := fResearchCost - InvRec.HourlyCostPerFac(StdHourlyCost);
                  if fResearchCost < 0
                    then fResearchCost := 0;
                end;
      finally
        Unlock;
      end;
    end;

  procedure TCompany.ReportUsage(Inventions : TCollection; count : integer; use : boolean);
    var
      i : integer;
    begin
      for i := 0 to pred(Inventions.Count) do
        ReportUsage(TInvention(Inventions[i]), count, use);
    end;

  function TCompany.CanResearch(Invention : TInvention) : boolean;
    var
      i : integer;
    begin
      if Invention.Req.Count = 0
        then result := true
        else
          begin
            i := 0;
            while (i < Invention.Req.Count) and HasInvention[TInvention(Invention.Req[i]).NumId] do
              inc(i);
            result := i = Invention.Req.Count;
          end;
    end;                       

  procedure TCompany.InitCompanyInputs;
    var
      i, j  : integer;         
    begin
      getmem( fCompanyInputIndex, MaxCompInputs*sizeof(TCompanyInput) );
      fillchar( fCompanyInputIndex^, MaxCompInputs*sizeof(TCompanyInput), 0 );
      fCompanyInputs := TLockableCollection.Create( 0, rkBelonguer );
      Facilities.Lock;
      try
        for i := 0 to pred(Facilities.Count) do
          with TFacility(Facilities[i]).CurrBlock do
            for j := 0 to pred(InputCount) do
              if (Inputs[j].MetaInput.MetaFluid <> nil) and (mfCompanyFluid in Inputs[j].MetaInput.MetaFluid.Options)
                then Self.RegisterInput( Inputs[j] );
      finally
        Facilities.Unlock;
      end;
    end;

  procedure TCompany.RegisterInput( Input : TInput );
    var
      CI : TCompanyInput;
    begin
      CI := FindCompanyInput( Input.MetaInput.MetaFluid );
      if CI <> nil
        then CI.fInputs.Insert( Input );
    end;

  procedure TCompany.UnregisterInput( Input : TInput );
    var
      CI : TCompanyInput;
    begin
      CI := FindCompanyInput( Input.MetaInput.MetaFluid );
      if CI <> nil
        then CI.fInputs.Delete( Input );
    end;

  function TCompany.FindCompanyInput( Kind : TMetaFluid ) : TCompanyInput;
    var
      i : integer;
    begin                                                                         
      fCompanyInputs.Lock;
      try
        i := 0;
        while (i < fCompanyInputs.Count) and (TCompanyInput(fCompanyInputs[i]).Kind <> Kind) do
          inc( i );
        if i < fCompanyInputs.Count
          then result := TCompanyInput(fCompanyInputs[i])
          else
            begin
              result := TCompanyInput.Create( Kind );
              fCompanyInputs.Insert( result );
              fCompanyInputIndex[Kind.UNId] := result;
            end;
      finally
        fCompanyInputs.Unlock;
      end;
    end;

  function TCompany.GetCompanyInput( UNId : integer ) : TCompanyInput;
    begin
      result := fCompanyInputIndex[UNId];
    end;

  procedure TCompany.Loaded;
    begin
      inherited;
      fFacilities.Pack;
      InitCompanyInputs;
      RecalculateInventionApplicationCost;
    end;

  procedure TCompany.LoadFromBackup( Reader : IBackupReader );
    var
      ClusterId : string;
      InvRec    : TInventionRecord;
      i         : integer;
      Invention : TInvention;
    begin
      inherited;
      Reader.ReadObject( 'Owner', fOwner, nil );
      ClusterId := Reader.ReadString( 'Cluster', defCluster );
      fCluster := TCluster(TheClassStorage.ClassById[tidClassFamily_Clusters, ClusterId]);
      fName := trim(Reader.ReadString( 'Name', 'Unknown' ));
      fCreated := Reader.ReadDouble( 'Created', 0 );
      fId := Reader.ReadInteger( 'Id', 0 );
      // >> Reader.ReadSet( 'AutoConnectLevels', fAutoConnectLevels );
      fAutoConnectLevels := [mglBasic];  // >> Fix this!!!
      //Logs.Log('Survival', 'Reading company: ' + fName);
      fInventions   := TLockableCollection.Create(0, rkBelonguer);
      fInventionSet := TInventionSet.Create(0);
      for i := 0 to pred(Reader.ReadInteger('InvCount', 0)) do
        try
          Invention := TInvention(TheClassStorage.ClassById[tidClassFamily_Inventions, Reader.ReadString('Inv' + IntToStr(i), '')]);
          if Invention <> nil
            then
              try
                InvRec := TInventionRecord.Create(Invention, Reader.ReadCurrency('Cost' + IntToStr(i), Invention.Price));
                if not fInventionSet.Included(Invention.NumId)
                  then
                    begin
                      fInventions.Insert(InvRec);
                      fInventionSet.Include(Invention.NumId);
                    end
                  else InvRec.Free;
              except
              end;
        except
        end;
      Reader.ReadObject( 'Directions', fDirections, nil );
      fDirections.Pack;
      Reader.ReadObject( 'Facilities', fFacilities, nil );
      // To pack facilities has to wait to loaded call
      fPrivated := Reader.ReadBoolean( 'Privated', false );
      Reader.ReadObject( 'MetaFacilityList', fMetaFacilityList, nil );
    end;

  procedure TCompany.StoreToBackup( Writer : IBackupWriter );
    var
      i   : integer;
      Rec : TInventionRecord;
      aux : string;
    begin
      inherited;
      Writer.WriteObjectRef( 'Owner', fOwner );
      Writer.WriteString( 'Cluster', fCluster.Id );
      Writer.WriteString( 'Name', fName );
      Writer.WriteDouble( 'Created', fCreated );
      Writer.WriteInteger( 'Id', fId );
      // >> Writer.WriteSet( 'AutoConnectLevels', fAutoConnectLevels );
      //Writer.WriteLooseObject( 'Researchs', fResearchs );
      Writer.WriteInteger('InvCount', fInventions.Count);
      for i := 0 to pred(fInventions.Count) do
        begin
          Rec := TInventionRecord(fInventions[i]);
          aux := 'Inv' + IntToStr(i);
          Writer.WriteString(aux, Rec.Invention.Id);
          aux := 'Cost' + IntToStr(i);
          Writer.WriteCurrency(aux, Rec.TotalCost);
        end;
      Writer.WriteLooseObject( 'Directions', fDirections );
      Writer.WriteLooseObject( 'Facilities', fFacilities );
      Writer.WriteBoolean( 'Privated', fPrivated );
      Writer.WriteLooseObject( 'MetaFacilityList', fMetaFacilityList );
      aux := '';
    end;

  function TCompany.GetCacheName : string;
    begin
      result := 'Co' + IntToStr(fId);
    end;

  function TCompany.FindInventionRecord(Invention : TInvention) : TInventionRecord;
    var
      i : integer;
    begin
      Lock;
      try
        i := pred(fInventions.Count);
        while (i >= 0) and (TInventionRecord(fInventions[i]).Invention <> Invention) do
          dec(i);
        if i >= 0
          then result := TInventionRecord(fInventions[i])
          else result := nil;
      finally
        Unlock;
      end;
    end;

  function TCompany.RetireInventionById(InventionId : string) : boolean;
    var
      Inv : TInvention;
    begin
      Inv := TInvention(TheClassStorage.ClassById[tidClassFamily_Inventions, InventionId]);
      if Inv <> nil
        then result := RetireInvention(Inv)
        else result := false;
    end;

  function TCompany.RetireInvention(Invention : TInvention) : boolean;
    var
      Stack : TCollection;
      Rec   : TInventionRecord;
      Inv   : TInvention;
      idx   : integer;
      i     : integer;

    procedure DoRetireInvention(Rec : TInventionRecord);
      begin
        Owner.LicenceLevel := Owner.LicenceLevel - Rec.Invention.LicLevel;
        GenMoney(Rec.TotalCost - Rec.Subsidy, accIdx_ResearchCenter_Research);
        Lock;
        try
          fInventionSet.Exclude(Rec.Invention.NumId);
        finally
          Unlock;
        end;
      end;

    var
      buidAffected : integer;
      licsAffected : integer;
    begin
      // Patch for reseted companies selling researches
      if Owned
        then Rec := FindInventionRecord(Invention)
        else Rec := nil;
      if Rec <> nil
        then
          try
            Stack := TCollection.Create(0, rkBelonguer);
            try
              // Retire the invention
              DoRetireInvention(Rec);
              // Add to stack
              Stack.Insert(Rec);
              // Remove from Inventions
              fInventions.Extract(Rec);
              // Iterate through the stack
              fInventions.Lock;
              idx := 0;
              buidAffected := 0;
              licsAffected := 0;
              try
                while idx < Stack.Count do
                  begin
                    Inv := TInventionRecord(Stack[idx]).Invention;
                    if Inv.EnablesTech
                      then inc(buidAffected);
                    if Inv.LicLevel > 0
                      then inc(licsAffected);
                    i := pred(fInventions.Count);
                    while i >= 0 do
                      begin
                        Rec := TInventionRecord(fInventions[i]);
                        if Rec.Invention.Req.IndexOf(Inv) <> noIndex
                          then
                            begin
                              DoRetireInvention(Rec);
                              Stack.Insert(Rec);
                              fInventions.Extract(Rec);
                              if Rec.Invention.EnablesTech
                                then inc(buidAffected);
                              if Rec.Invention.LicLevel > 0
                                then inc(licsAffected);
                            end;
                        dec(i);
                      end;
                    inc(idx);
                  end;
              finally
                fInventions.Unlock;
              end;
            finally
              Stack.Free;
            end;
            try
              ModelServerCache.BackgroundInvalidateCache(self); //ModelServerCache.CacheObject( self, noKind, noInfo )
              if buidAffected > 0
                then Owner.SendNotification( ntkGenericEvent, '', mtidMsgMainResearchSold.Values[Owner.Language], gevnId_RefreshBuildPage )
                else Owner.SendNotification( ntkChatMessage, '', mtidMsgResearchSold.Values[Owner.Language], 0 );
              if licsAffected > 0
                then ModelServerCache.BackgroundInvalidateCache(Owner);
            except
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in TCompany.RetireInvention Notifications name: ' + Name );
            end;
            result := true;
            // Make blocks to recalculate the research variables
            RecalculateInventions;
            // Recalculate invention usage cost
            RecalculateInventionApplicationCost;
          except
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in TCompany.RetireInvention name: ' + Name );
            result := false;
          end
        else result := false;
    end;

  procedure TCompany.RecalculateInventions;
    var
      i   : integer;
      Fac : TFacility;
    begin
      fFacilities.Lock;
      try
        for i := 0 to pred(fFacilities.Count) do
          begin
            Fac := TFacility(fFacilities[i]);
            if Fac.CurrBlock <> nil
              then Fac.CurrBlock.RecalculateInventionsEffect;
          end;
      finally
        fFacilities.Unlock;
      end;
    end;

  procedure TCompany.RecalculateInventionApplicationCost;
    var
      InvRec : TInventionRecord;
      i      : integer;
      cost   : TMoney;
    begin
      cost := 0;
      for i := 0 to pred(fInventions.Count) do
        begin
          InvRec := TInventionRecord(fInventions[i]);
          cost   := cost + InvRec.HourlyCost(StdHourlyCost);
        end;
      fResearchCost := cost;
    end;

  function TCompany.CalcInventionUsage(Invention : TInvention) : integer;
    var
      i   : integer;
      Fac : TFacility;
    begin
      result := 0;
      fFacilities.Lock;
      try
        for i := 0 to pred(fFacilities.Count) do
          begin
            Fac := TFacility(fFacilities[i]);
            if (Fac.CurrBlock <> nil) and Fac.CurrBlock.UsesInvention(Invention)
              then inc(result, Fac.CurrBlock.MetaBlock.UsagePerInv);
          end;
      finally
        fFacilities.Unlock;
      end;
    end;

  procedure TCompany.ChargeResearches(dt : TTimeDelta);
    begin
      GenMoney(-fResearchCost, accIdx_ResearchCenter_ImpCosts);
    end;

  procedure TCompany.CacheLinks;
    var
      i   : integer;
      Fac : TFacility;
    begin
      Lock;
      try
        for i := 0 to pred(fFacilities.Count) do
          begin
            Fac := TFacility(fFacilities[i]);
            Fac.CreateCacheLinks;
          end;
      finally
        Unlock;
      end;
      ModelServerCache.BackgroundCache(self, false);
    end;

  function TCompany.CheckOpAuthenticity : boolean;
    begin
      result := (fOwner <> nil) and fOwner.CheckOpAuthenticity;
    end;

  // TAutoConnection

  constructor TAutoConnection.Create( aMetaFluidId : string );
    begin
      inherited Create;
      fMetaFluidId        := aMetaFluidId;
      fConnections        := TLockableCollection.Create( 0, rkUse );
      fHireTradeCenter    := true;
      fHireOnlyWarehouses := false;
    end;

  destructor TAutoConnection.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fConnections.Free;
      inherited;
    end;

  procedure TAutoConnection.FacilityDestroyed( Facility : TFacility );
    var
      i : integer;
    begin
      fConnections.Lock;
      try
        i := 0;
        while (i < fConnections.Count) and (fConnections[i] <> Facility) do
          inc( i );
        if i < fConnections.Count
          then
            begin
              fConnections.AtDelete( i );
              UncacheObject( self, noKind, noInfo ); // >>
              CacheObject( self, noKind, noInfo );   // >>
            end;
      finally
        fConnections.Unlock;
      end;
    end;

  procedure TAutoConnection.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fMetaFluidId := Reader.ReadString( 'MetaFluidId', '' );
      Reader.ReadObject( 'Connections', fConnections, nil );
      fHireTradeCenter := Reader.ReadBoolean( 'HireTradeCenter', true );
      // >> Binary incompatibility
      //fSearchForSuppliers := Reader.ReadBoolean( 'SearchForSuppliers', true );
      fSearchForSuppliers := true;
      fHireOnlyWarehouses := Reader.ReadBoolean( 'HireOnlyWarehouses', false );
    end;

  procedure TAutoConnection.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'MetaFluidId', fMetaFluidId );
      Writer.WriteLooseObject( 'Connections', fConnections );
      Writer.WriteBoolean( 'HireTradeCenter', fHireTradeCenter );
      // >> Binary incompatibility
      //Writer.WriteBoolean( 'SearchForSuppliers', fSearchForSuppliers );
      Writer.WriteBoolean( 'HireOnlyWarehouses', fHireOnlyWarehouses );
    end;


  // TCurriculumItem

  constructor TCurriculumItem.Create( anId : string; aKind : integer );
    begin
      inherited Create;
      fId   := anId;
      fKind := aKind;
    end;

  function TCurriculumItem.GetDesc( langId : TLanguageId ) : string;
    begin
      result := '';
    end;

  function TCurriculumItem.GetImportance : integer;
    begin
      result := 0;
    end;

  function TCurriculumItem.GetPrestige : TPrestige;
    begin
      result := 0;
    end;

  procedure TCurriculumItem.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;                                                           
      fId := Reader.ReadString( 'Id', '' );
      fKind := Reader.ReadInteger( 'Kind', 0 );
      Reader.ReadObject( 'Tycoon', fTycoon, nil );
      fPublished := Reader.ReadBoolean( 'Published', true );
    end;

  procedure TCurriculumItem.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'Id', fId );
      Writer.WriteInteger( 'Kind', fKind );
      Writer.WriteObjectRef( 'Tycoon', fTycoon );
      Writer.WriteBoolean( 'Published', fPublished );
    end;

  class function TCurriculumItem.IsGlobal : boolean;
    begin
      result := true;
    end;                           
                                                          

  // TTycoonLevel                               

  constructor TTycoonLevel.Create( anId, aNextLevelId : string; Tier : integer );
    begin
      inherited Create( anId );
      if aNextLevelId <> ''
        then fNextLevel := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, aNextLevelId]);
      fName_MLS := TMultiString.Create;
      fDescription_MLS := TMultiString.Create;
      fCondition_MLS := TMultiString.Create;
      fPriority := cprNormal;
      fTier     := Tier;
    end;

  procedure TTycoonLevel.RetrieveDynamicTexts;

    procedure LoadLanguage( path : string; LangId : TLanguageId );
      var
        Rec   : TSearchRec;
        Container : TDictionary;
      begin
        path  := path + 'ms\' +LangId + '\';
        FindFirst( path + 'metalevels.lang', faAnyFile, Rec );
        try
          begin
            Container := TDictionary.Create( path + Rec.Name );
            try
              fName_MLS.Values[Container.LangId] := Container.Values[Family + '.' + tidTycoonLevel_OverLegend + '.' + 'Name'] + ' ' + DecToRom(Tier - 5);
              fDescription_MLS.Values[Container.LangId] := Format(Container.Values[Family + '.' + tidTycoonLevel_OverLegend + '.' + 'Desc'], [IntToStr(PrestigeBoost), IntToStr(FacLimit)]);
              fCondition_MLS.Values[Container.LangId] := Format(Container.Values[Family + '.' + tidTycoonLevel_OverLegend + '.' + 'Cond'], [FormatMoney(Fee), FormatMoney(HourIncome)]);
            finally
              Container.Free;
            end;
          end;
        finally
          FindClose( Rec );
        end;
      end;
    
    var
      i : integer;
    begin
      if fName_MLS = nil
        then fName_MLS := TMultiString.Create;
      if fDescription_MLS = nil
        then fDescription_MLS := TMultiString.Create;
      if fCondition_MLS = nil
        then fCondition_MLS := TMultiString.Create;
      for i := 0 to pred(LangList.Count) do
        LoadLanguage( ExtractFilePath(paramstr(0)) + 'Languages\', LangList[i] );
    end;
    
  procedure TTycoonLevel.RetrieveTexts( Container : TDictionary );

    function DecToRom(Dec: LongInt): String;
    const
      Nums : Array[1..13] of Integer =
        (1, 4, 5, 9, 10, 40, 50, 90, 100,
          400, 500, 900, 1000);
      RomanNums:  Array[1..13] of string =
        ('I', 'IV', 'V', 'IX', 'X', 'XL',
          'L', 'XC', 'C', 'CD', 'D', 'CM', 'M');
    var
      i: Integer;
    begin
      Result := '';
      for i := 13 downto 1 do
        while (Dec >= Nums[i]) do
        begin
          Dec := Dec - Nums[i];
          Result  := Result + RomanNums[i];
        end;
    end;
    
    begin
      inherited;
      if fName_MLS = nil
        then fName_MLS := TMultiString.Create;
      if fDescription_MLS = nil
        then fDescription_MLS := TMultiString.Create;
      if fCondition_MLS = nil
        then fCondition_MLS := TMultiString.Create;
      if Copy(Id, 0, length(tidTycoonLevel_OverLegend)) = tidTycoonLevel_OverLegend
      then
        begin
          fName_MLS.Values[Container.LangId] := Container.Values[Family + '.' + tidTycoonLevel_OverLegend + '.' + 'Name'] + ' ' + DecToRom(Tier - 5);
          fDescription_MLS.Values[Container.LangId] := Format(Container.Values[Family + '.' + tidTycoonLevel_OverLegend + '.' + 'Desc'], [IntToStr(PrestigeBoost), IntToStr(FacLimit)]);
          fCondition_MLS.Values[Container.LangId] := Format(Container.Values[Family + '.' + tidTycoonLevel_OverLegend + '.' + 'Cond'], [FormatMoney(Fee), FormatMoney(HourIncome)]);
        end
      else
        begin
          fName_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Name'];
          fDescription_MLS.Values[Container.LangId] := Format(Container.Values[Family + '.' + Id + '.' + 'Desc'], [IntToStr(FacLimit)]);
          fCondition_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Cond'];
        end;
    end;

  procedure TTycoonLevel.StoreTexts( Container : TDictionary );
    begin
      inherited;
      Container.Values[Family + '.' + Id + '.' + 'Name'] := fName;
      Container.Values[Family + '.' + Id + '.' + 'Desc'] := fDescription;
      Container.Values[Family + '.' + Id + '.' + 'Cond'] := fCondition;
    end;


  // TTycoon

  type
    TTycoonPolicy =
      class
        private
          fTycoon : TTycoon;
          fStatus : TPolicyStatus;
      end;

  constructor TTycoon.Create( anId : TTycoonId );
    begin
      inherited Create;
      fId              := anId;
      fCompanies       := TLockableCollection.Create( 0, rkUse );
      fRoles           := TLockableCollection.Create( 0, rkUse );
      fAutoConnections := TLockableCollection.Create( 0, rkBelonguer );
      fPolicies        := TLockableCollection.Create( 0, rkBelonguer );
      fCurriculum      := TLockableCollection.Create( 0, rkBelonguer );
      fEvents          := TLockableCollection.Create( 0, rkBelonguer );
      fCookies         := TStringList.Create;
      fLanguage        := langDefault;
      fCountUpgrades   := StrToInt(TheGlobalConfigHandler.GetConfigParm('CountUpgrades', '1')) = 1;
      fTournamentOn    := StrToInt(TheGlobalConfigHandler.GetConfigParm('TornamentLength', '0')) > 0;
      UpdateAutoConnections;
      if TheGlobalConfigHandler.GetConfigParm('Tutorial', 'disabled') = 'enabled' // >> enabled
        then InstantiateTutorial;
      InitRankings;
      AddItemToCurriculum( TResearchItem.Create( TResearchItem.ClassName, currKind_Research ) );
      fVotes := TVoteSystem.Create;
      fInTowns := TCollection.Create(0, rkUse);
      fFavorites := TFavorites.Create;
      fNobPoints := -1;
    end;

  destructor TTycoon.Destroy;
    begin
      fRankings.Free;
      fEvents.Free;
      fCompanies.Free;
      fRoles.Free;
      fAutoConnections.Free;
      fPolicies.Free;
      fCurriculum.Free;
      fTutorial.Free;
      fTaskContext.Free;
      fCookies.Free;
      fVotes.Free;
      fInTowns.Free;
      fFavorites.Free;
      inherited;
    end;

  function TTycoon.GetBudget : TMoney;
    begin
      result := fBudget;
    end;

  procedure TTycoon.SetBudget( aBudget : TMoney );
    begin
      fBudget := aBudget;
    end;

  function TTycoon.RDOGetBudget : OleVariant;
    begin
      result := GetBudget;
    end;

  function TTycoon.GetFocusX : integer;
    begin
      result := fFocus.x;
    end;

  function TTycoon.GetFocusY : integer;
    begin
      result := fFocus.y;
    end;

  function TTycoon.GetMasterRole : TTycoon;
    begin
      if SuperRole <> nil
        then result := SuperRole.MasterRole
        else result := self;
    end;

  function TTycoon.GetRealName : string;
    begin
      result := GetMasterRole.Name
    end;

  function TTycoon.GetAllCompaniesCount : integer;

    function CountCompanies( Tycoon : TTycoon ) : integer;
      var
        i : integer;
      begin
        result := Tycoon.Companies.Count;
        Tycoon.Roles.Lock;
        try
          for i := 0 to pred(Tycoon.Roles.Count) do
            result := result + CountCompanies( TTycoon(Tycoon.Roles[i]) );
        finally
          Tycoon.Roles.Unlock;
        end;
      end;

    begin
      result := CountCompanies( MasterRole );
    end;

  function TTycoon.GetAllCompanies( index : integer ) : TCompany;

    function CountCompanies( Tycoon : TTycoon ) : integer;
      var
        i : integer;
      begin
        result := Tycoon.Companies.Count;
        Tycoon.Roles.Lock;
        try
          for i := 0 to pred(Tycoon.Roles.Count) do
            result := result + CountCompanies( TTycoon(Tycoon.Roles[i]) );
        finally
          Tycoon.Roles.Unlock;
        end;
      end;

    function FindCompany( index : integer; Tycoon : TTycoon ) : TCompany;
      var
        i     : integer;
        count : integer;
        last  : integer;
      begin
        if index < Tycoon.Companies.Count
          then result := TCompany(Tycoon.Companies[index])
          else
            begin
              dec( index, Tycoon.Companies.Count );
              i      := 0;
              count  := 0;
              last   := 0;
              result := nil;
              repeat
                inc( count, CountCompanies( TTycoon(Tycoon.Roles[i]) ));
                if index < count
                  then result := FindCompany( index - last, TTycoon(Tycoon.Roles[i]) );
                last := count;
                inc( i );
              until (i = Tycoon.Roles.Count) or (result <> nil);
            end;
      end;

    begin
      result := FindCompany( index, MasterRole );
    end;

  function TTycoon.GetCookie( Id : string ) : string;
    begin
      result := fCookies.Values[Id];
    end;

  function TTycoon.GetCookies : string;
    begin
      result := fCookies.Text;
    end;

  procedure TTycoon.SetCookie( Id, Value : string );
    begin
      fCookies.Values[Id] := Value;
    end;

  function TTycoon.GetRanking( Id : string ) : TRankingInfo;
    var
      i : integer;
    begin
      fRankings.Lock;
      try
        i := 0;
        while (i < fRankings.Count) and (TRankingInfo(fRankings[i]).fRanking.Id <> Id) do
          inc( i );
        if i < fRankings.Count
          then result := TRankingInfo(fRankings[i])
          else result := nil;
      finally
        fRankings.Unlock;
      end;
    end;

  function TTycoon.GetFacCount : integer;
    begin
      if fFacilityCount = 0
        then CountFacilities;
      result := fFacilityCount;
    end;

  function TTycoon.GetFacMax : integer;
    begin
      if Level <> nil
        then result := Level.FacLimit
        else result := high(result)
    end;

  function TTycoon.GetAreaTax : TMoney;
    var
      WL  : IWorldLocator;
      tax : integer;
    begin
      if WL <> nil
        then tax := WL.GetLandSquareCost
        else tax := LandSquareCost;
      if (Level <> nil) and not IsRole
        then result := Level.Tier*fArea*tax
        else result := 0; // Visitors do not have level
    end;

  function TTycoon.GetNobPoints : integer;
    begin
      if fNobPoints < 0
        then
          if (fWorldLocator = nil) or not fWorldLocator.UpdateNobility(self)
            then fNobPoints := 0;
      result := fNobPoints;
    end;

  class function TTycoon.GetIsRole : boolean;
    begin
      result := false;
    end;

  class function TTycoon.AllowedFacTypes : TFacTypeSet;
    begin
      result := [0..255];
    end;

  class function TTycoon.DesignedZoner : boolean;     
    begin
      result := false;
    end;

  function TTycoon.GetHasZonerRole : boolean;
    var
      i      : integer;
      Tycoon : TTycoon;
    begin
      i := pred(fRoles.Count);
      result := DesignedZoner;
      while not result and (i >= 0) do
        begin
          Tycoon := TTycoon(fRoles[i]);
          result := (Tycoon <> self) and Tycoon.DesignedZoner;
          dec(i);
        end;
    end;

  function TTycoon.GetSecurityId : TSecurityId;

    function CollectSecurityId( Tycoon : TTycoon ) : string;
      var
        i : integer;
      begin
        result := SecIdItemSeparator + IntToStr(integer(Tycoon));
        for i := 0 to pred(Tycoon.Roles.Count) do
          result := result + SecIdItemSeparator + CollectSecurityId( TTycoon(Tycoon.Roles[i]) );
        result := result + SecIdItemSeparator;
      end;

    begin
      Lock;
      try
        result := CollectSecurityId( MasterRole );
      finally
        Unlock;
      end;
    end;

  procedure TTycoon.Awake;
    begin
      fTimesAwaken := 1;
    end;

  procedure TTycoon.Sleep;
    begin
      if fTimesAwaken = 1
        then
          begin
            Cookie[tidCookie_LastTimeOnline] := DateToStr( Now ); //DecodeDate( Timer.GetVirtualTime, lastYear, useless, useless );
            fTimesAwaken := 0;
          end;
    end;

  function TTycoon.IsOnline : boolean;
    begin
      result := fTimesAwaken > 0;
    end;

  procedure TTycoon.SendNotification( Kind : integer; Title, Body : string; Options : integer );
    begin
      fWorldLocator.SendNotification( Id, Kind, Title, Body, Options );
    end;

  function TTycoon.CountItemsInCurriculum( Kind : integer ) : integer;
    var
      i : integer;
    begin
      fCurriculum.Lock;
      try
        result := 0;
        for i := 0 to pred(fCurriculum.Count) do
          if TCurriculumItem(fCurriculum[i]).Kind = Kind
            then inc( result );
      finally
        fCurriculum.Unlock;
      end;
    end;

  function TTycoon.FindResearchItemsInCurriculum : boolean;
    var
      i : integer;
      cnt : integer;
    begin
      fCurriculum.Lock;
      try
        result := false;
        i      := 0;
        cnt    := fCurriculum.Count;
        while (i < cnt) and not ObjectIs(TResearchItem.ClassName, fCurriculum[i]) do
          inc(i);
        result := i < cnt;
      finally
        fCurriculum.Unlock;
      end;
    end;

  procedure TTycoon.RecordEvent( Event : TEvent );
    var
      i    : integer;
      prec : integer;
      del  : boolean;
    begin
      fEvents.Lock;
      try
        for i := pred(fEvents.Count) downto 0 do
          begin
            try
              del := (TEvent(fEvents[i]).TTL > 0) and (TEvent(fEvents[i]).DateTick + TEvent(fEvents[i]).TTL < Timer.GetVirtualTimeAbs);
            except
              del := false;
              fEvents.AtExtract( i );
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in TTycoon.RecordEvent..' );
            end;
            if del
              then fEvents.AtDelete( i );
          end;
        prec := Event.GetPrecedence;
        i    := 0;
        while (i < fEvents.Count) and not TEvent(fEvents[i]).CanAssimilate( Event ) do
          inc( i );
        if i < fEvents.Count
          then
            begin
              TEvent(fEvents[i]).Assimilate( Event );
              Event.Free;
            end
          else
            begin
              while (i < fEvents.Count) and (prec <= TEvent(fEvents[i]).GetPrecedence) do
                inc( i );
              fEvents.AtInsert( i, Event );
            end;
      finally
        fEvents.Unlock;
      end;
    end;
    
  function TTycoon.PickEvent : TEvent;
    begin
      fEvents.Lock;
      try
        if fTimesAwaken <= 0
          then fTimesAwaken := 1;
        if fEvents.Count > 0
          then
            begin
              result := TEvent(fEvents[0]);
              fEvents.AtExtract( 0 );
            end
          else result := nil;
      finally
        fEvents.Unlock;
      end;
    end;

  function TTycoon.GetPolicyStatus( Tycoon : TTycoon ) : TPolicyStatus;
    var
      i : integer;
    begin
      try
        fPolicies.Lock;
        try
          i := 0;
          while (i < fPolicies.Count) and (TTycoonPolicy(fPolicies[i]).fTycoon <> Tycoon) do
            inc( i );
          if i < fPolicies.Count
            then result := TTycoonPolicy(fPolicies[i]).fStatus
            else result := pstNeutral;
        finally
          fPolicies.Unlock;
        end;
      except
        result := pstNeutral;
      end;
    end;

  procedure TTycoon.SetPolicyStatus( Tycoon : TTycoon; Status : TPolicyStatus );
    var
      i  : integer;
      TP : TTycoonPolicy;
    begin
      fPolicies.Lock;
      try
        i := 0;
        while (i < fPolicies.Count) and (TTycoonPolicy(fPolicies[i]).fTycoon <> Tycoon) do
          inc( i );
        if i < fPolicies.Count
          then
            if Status <> pstNeutral
              then
                begin
                  TP := TTycoonPolicy(fPolicies[i]);
                  TP.fStatus := Status;
                end
              else fPolicies.AtDelete( i )
          else
            if Status <> pstNeutral
              then
                begin
                  TP := TTycoonPolicy.Create;
                  TP.fTycoon := Tycoon;
                  TP.fStatus := Status;
                  fPolicies.Insert( TP );
                end;
        fPolicyModified := true;
      finally
        fPolicies.Unlock;
      end;
    end;

  procedure TTycoon.StorePoliciesToCache( Cache : TObjectCache );
    var
      Tycoons : TLockableCollection;
      Tycoon  : TTycoon;
      i       : integer;
      idx     : integer;
      count   : integer;
      Status  : TPolicyStatus;
      StatusV : TPolicyStatus;
    begin
      Tycoons := fWorldLocator.GetTycoonCollection;
      Tycoons.Lock;
      try
        idx := 0;
        count := 0;
        for i := 0 to pred(Tycoons.Count) do
          begin
            Tycoon  := TTycoon(Tycoons[i]);
            Status  := Policy[Tycoon];
            StatusV := Tycoon.Policy[self];
            if (Tycoon <> self) and ((Status <> pstNeutral) or (StatusV <> pstNeutral))
              then
                begin
                  Cache.WriteString( 'PolTycoon' + IntToStr(idx), Tycoon.Name );
                  Cache.WriteInteger( 'PolTo' + IntToStr(idx), integer(Status) );
                  Cache.WriteInteger( 'PolFrom' + IntToStr(idx), integer(StatusV) );
                  inc( idx );
                  inc( count );
                end;
          end;
        Cache.WriteInteger( 'PolicyCount', count );
      finally
        Tycoons.Unlock;
      end;
    end;


  procedure TTycoon.StoreRoleInfoToCache( Cache : TObjectCache );
    begin
    end;

  procedure TTycoon.AssumeRole( Role : TTycoon );
    begin
      if Roles.IndexOf( Role ) = NoIndex
        then
          begin
            //UncacheObject( self, noKind, noInfo ); // LINKS KILLER!!!
            //UncacheObject( Role, noKind, noInfo ); // LINKS KILLER!!!
            Roles.Insert( Role );
            Role.SuperRole := self;
            Role.Password  := Password;
            ModelServerCache.BackgroundCache(self, false); //CacheObject( self, noKind, noInfo )
            ModelServerCache.BackgroundCache(Role, false); //CacheObject( Role, noKind, noInfo )
          end;
    end;

  procedure TTycoon.AbandomRole( Role : TTycoon );
    begin
      if Roles.IndexOf( Role ) <> NoIndex
        then
          begin
            //UncacheObject( self, noKind, noInfo ); // LINKlS KILLER!!!
            //UncacheObject( Role, noKind, noInfo ); // LINKS KILLER!!!
            Role.Password  := '';
            Role.SuperRole := nil;
            Roles.Delete( Role );
            ModelServerCache.BackgroundCache(self, false); //CacheObject( self, noKind, noInfo )
            ModelServerCache.BackgroundCache(Role, false); //CacheObject( Role, noKind, noInfo )
          end;
    end;

  procedure TTycoon.AbandomRoles;
    var
      i : integer;
    begin
      if SuperRole <> nil
        then SuperRole.AbandomRole(self);
      for i := pred(Roles.Count) downto 0 do
        AbandomRole(TTycoon(Roles[i]));
    end;

  function TTycoon.ContainsRole( RoleId : TTycoonId ) : boolean;

    function TycoonContainsRole( Tycoon : TTycoon ) : boolean;
      var
        i : integer;
      begin
        if RoleId = Tycoon.Id
          then result := true
          else
            begin
              i := 0;
              while (i < Tycoon.Roles.Count) and not TycoonContainsRole( TTycoon(Tycoon.Roles[i]) ) do
                inc( i );
              result := i < Tycoon.Roles.Count;
            end;
      end;

    begin
      result := TycoonContainsRole( MasterRole );
    end;

  procedure TTycoon.AddItemToCurriculum( Item : TCurriculumItem );
    var
      i : integer;
    begin
      Item.fTycoon := self;
      fCurriculum.Insert( Item );
      fCurriculum.Lock;
      try
        for i := 0 to pred(fCurriculum.Count) do
          CacheObject( fCurriculum[i], noKind, noInfo ); // >> ??
      finally
        fCurriculum.Unlock;
      end;
      if Item.IsGlobal
        then
          try
            WorldLocator.AddCurriculumItem( self, Item );
          except
          end;
    end;

  function TTycoon.RDOAskLoan( AmountStr : widestring ) : OleVariant;
    var
      Amount : TMoney;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' AskLoan: ' + Name + ', $' + AmountStr );
      Lock;
      try
        if true //>>CheckOpAuthenticity
          then
            begin
              Amount := StrToCurr( AmountStr );
              try
                if CanGetLoan(Amount) and (fWorldLocator.GetMainBank.AskLoan( self, Amount ) <> brqRejected)
                  then
                    begin
                      ModelServerCache.InvalidateCache(self, false); //CacheObject( self, noKind, noInfo )
                      result := NOERROR;
                    end
                  else result := ERROR_LoanNotGranted;
              except
                result := ERROR_Unknown;
              end;
            end;
      finally
        Unlock;
      end;
    end;

  function TTycoon.RDOPayLoan( AmountStr : widestring ) : OleVariant;
    begin
      result := NOERROR;
    end;

  function TTycoon.RDOSendMoney( ToTycoon, Reason, AmountStr : widestring ) : OleVariant;
    var
      Amount : TMoney;
      Dest   : TTycoon;
      Loan   : TMoney;
      AprFee : TMoney;
    begin
      Logs.Log( 'Money', TimeToStr(Now) + ' Sending money: ' + Name + ' to ' + ToTycoon + ', $' + AmountStr );
      try
        // >> Verify Security (ASP)
        Amount := StrToCurr( AmountStr );
        Dest   := WorldLocator.GetTycoonByName( ToTycoon );
        if (Amount > 0) and (Dest <> nil) and (not IsRole or Dest.IsRole) and Dest.CanGetLoan(Amount) and (not Dest.IsDemo) and (not self.IsDemo)
          then
            begin
              if (Dest.CountItemsInCurriculum( crrKind_Transcended ) = 0) and (Dest.NobPoints < 50) and (Dest.Level.Tier < 6)
                then
                  begin
                    Loan := LoanAmount;
                    Lock;
                    try
                      if Level.Tier = 0
                        then AprFee := InitialBudget
                        else AprFee := 0;
                      if Amount > fBudget - Loan - AprFee
                        then Amount := realmax( 0, fBudget - Loan - AprFee );
                      if Amount > 0
                        then
                          begin
                            GenMoney( -Amount, accIdx_TransfersOut );
                            ModelServerCache.BackgroundInvalidateCache(self); //CacheObject( self, noKind, noInfo )
                            try
                              AmountStr := Format( '%.0n', [Amount] );
                              MailServer.SendHTMLMessage(
                                Name,
                                Dest.Name,
                                '<b>' +
                                Format( mtidMsgMoneyTransfer.Values[Language], ['$' + AmountStr] )
                                // mtidMsgMoneyTransfer '$' + AmountStr + ' successfully transferred.'
                                + '</b>',
                                WorldLocator.GetMessagesPath +
                                  tidMailMsgURL_MoneySentNotification + '?' +
                                  'From=' + Name + '&' +
                                  'To=' + Dest.Name + '&' +
                                  'Reason=' + Reason + '&' +
                                  'Amount=' + AmountStr );
                              result := NOERROR;
                            except
                              result := ERROR_Unknown;
                            end;
                          end
                        else result := ERROR_InvalidMoneyValue;
                    finally
                      Unlock;
                    end;
                    Dest.Lock;
                    try
                      Dest.GenMoney( Amount, accIdx_TransfersIn );
                      ModelServerCache.InvalidateCache(Dest, false); //CacheObject( Dest, noKind, noInfo )
                    finally
                      Dest.Unlock
                    end;
                  end
                else result := ERROR_UnknownTycoon
            end
          else result := ERROR_InvalidMoneyValue
      except
        result := ERROR_Unknown;
      end;
    end;

  function TTycoon.RDOPayOff(index : integer) : OleVariant;
    var
      Loan   : TLoan;
      AprFee : TMoney;
      amt    : TMoney;
    begin
      amt := 0;
      try
        Lock;
        try
          if Level.Tier < 1
            then AprFee := 0 //InitialBudget
            else AprFee := 0;
          if index < fLoans.Count
            then
              begin
                Loan := TLoan(fLoans[index]);
                if Loan.Amount < Budget - AprFee
                  then
                    begin
                      GenMoney(-Loan.Amount{*(1 + Loan.Interest/100)}, accIdx_Bank_LoanPayments );
                      amt := Loan.fAmount;
                      Loan.fAmount := 0;
                      fLoans.Extract(Loan);
                    end
                  else Loan := nil;
              end
            else Loan := nil;
        finally
          Unlock;
        end;
        if Loan <> nil
          then
            begin
              if Loan.Bank <> nil
                then
                  begin
                    if Loan.Bank.Owner <> nil
                      then Loan.Bank.Owner.GenMoney(amt{*(1 + Loan.Interest/100)}, accIdx_Bank_LoanIncome);
                    Loan.Bank.Loans.Delete(Loan);
                  end;
              ModelServerCache.InvalidateCache(self, false);
              result := true;
            end
          else result := false;
      except
        result := false;
      end;
    end;

  procedure TTycoon.UpdateAutoConnections;
    var
      i              : integer;
      MetaFluid      : TMetaFluid;
      AutoConnection : TAutoConnection;
    begin
      for i := 0 to pred(TheClassStorage.ClassCount[tidClassFamily_Fluids]) do
        begin
          MetaFluid := TMetaFluid(TheClassStorage.ClassByIdx[tidClassFamily_Fluids, i]);
          if (mfTradeable in MetaFluid.Options) and (FindAutoConnection( MetaFluid.Id ) = nil)
            then
              begin
                AutoConnection := TAutoConnection.Create( MetaFluid.Id );
                fAutoConnections.Insert( AutoConnection );
              end;
        end;
    end;

  function TTycoon.FindAutoConnection( FluidId : string ) : TAutoConnection;
    var
      i : integer;
    begin
      fAutoConnections.Lock;
      try
        i := 0;
        while (i < fAutoConnections.Count) and (TAutoConnection(fAutoConnections[i]).fMetaFluidId <> FluidId) do
          inc( i );
        if i < fAutoConnections.Count
          then result := TAutoConnection(fAutoConnections[i])
          else result := nil;
      finally
        fAutoConnections.Unlock;
      end;
    end;

  procedure TTycoon.ModifyAutoConnection( FluidId, Suppliers : widestring; add : boolean );
    var
      Gates          : TCollection;
      i              : integer;
      Facility       : TFacility;
      AutoConnection : TAutoConnection;
      modified       : boolean;
    begin
      try
        AutoConnection := FindAutoConnection( FluidId );
        if AutoConnection <> nil
          then
            begin
              Gates := ParseGateList( Suppliers );
              modified := false;
              try
                for i := 0 to pred(Gates.Count) do
                  with TGateDesc(Gates[i]) do
                    begin
                      Facility := WorldLocator.FacilityAt( x, y );
                      if Facility <> nil
                        then
                          begin
                            if add
                              then AutoConnection.Connections.Insert( Facility )
                              else AutoConnection.Connections.Delete( Facility );
                            modified := true;
                          end;
                    end;
                if modified
                  then InvalidateCache( self, false );
              finally
                Gates.Free;
              end;
            end;
      except
      end;
    end;

  procedure TTycoon.RDOAddAutoConnection( FluidId, Suppliers : widestring );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Adding initial suppliers: ' + Name + ', ' + FluidId + ', ' + Suppliers );
      try
        ModifyAutoConnection( FluidId, Suppliers, true );
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error Adding initial suppliers..');
      end;
    end;

  procedure TTycoon.RDODelAutoConnection( FluidId, Suppliers : widestring );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Deleting initial suppliers: ' + Name + ', ' + FluidId + ', ' + Suppliers );
      try
        ModifyAutoConnection( FluidId, Suppliers, false );
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error deleting initial suppliers..');
      end;
    end;

  procedure TTycoon.RDOHireTradeCenter( FluidId : widestring );
    var
      AutoConnection : TAutoConnection;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Initial suppliers, include Trade Center: ' + Name + ', ' + FluidId );
      try
        AutoConnection := FindAutoConnection( FluidId );
        if AutoConnection <> nil
          then
            begin
              AutoConnection.fHireTradeCenter := true;
              ModelServerCache.BackgroundInvalidateCache(self); //CacheObject( self, noKind, noInfo );
            end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error hiring TradeCenter..');
      end;
    end;

  procedure TTycoon.RDODontHireTradeCenter( FluidId : widestring );
    var
      AutoConnection : TAutoConnection;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Initial suppliers, excluding Trade Center: ' + Name + ', ' + FluidId );
      try
        AutoConnection := FindAutoConnection( FluidId );
        if AutoConnection <> nil
          then
            begin
              AutoConnection.fHireTradeCenter := false;
              ModelServerCache.BackgroundInvalidateCache(self); //CacheObject( self, noKind, noInfo );
            end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error excluding TradeCenter..');
      end;
    end;

  procedure TTycoon.RDOHireOnlyFromWarehouse( FluidId : widestring );
    var
      AutoConnection : TAutoConnection;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Initial suppliers, hire only warehouses: ' + Name + ', ' + FluidId );
      try
        AutoConnection := FindAutoConnection( FluidId );
        if AutoConnection <> nil
          then
            begin
              AutoConnection.fHireOnlyWarehouses := true;
              ModelServerCache.BackgroundInvalidateCache(self); //CacheObject( self, noKind, noInfo );
            end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in hire only warehouses..');
      end;
    end;

  procedure TTycoon.RDODontHireOnlyFromWarehouse( FluidId : widestring );
    var
      AutoConnection : TAutoConnection;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Initial suppliers, hire all: ' + Name + ', ' + FluidId );
      try
        AutoConnection := FindAutoConnection( FluidId );
        if AutoConnection <> nil
          then
            begin
              AutoConnection.fHireOnlyWarehouses := false;
              ModelServerCache.BackgroundInvalidateCache(self); //CacheObject( self, noKind, noInfo );
            end;
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Initial suppliers, hire all OK! ' );
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in RDODontHireOnlyFromWarehouse');
      end;
    end;

  procedure TTycoon.RDOSetPolicyStatus( ToTycoon : widestring; Status : integer );
    var
      Tycoon : TTycoon;
      Msg    : TRegMultiString;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Setting policy status: ' + Name + ', ' + ToTycoon + ', ' + IntToStr(Status) );
      try
        Tycoon := fWorldLocator.GetTycoonByName( ToTycoon );
        if Tycoon <> nil
          then
            begin
              case TPolicyStatus(Status) of
                pstAlly    : Msg := mtidMsgPolicySetToAlly;
                pstNeutral : Msg := mtidMsgPolicySetToNeutral;
                pstEnemy   : Msg := mtidMsgPolicySetToEnemy;
                else Msg := NullString;
              end;
              Policy[Tycoon] := TPolicyStatus(Status);
              UpdateObjectCache(self, noKind, noInfo); //ModelServerCache.InvalidateCache(self);
              ModelServerCache.BackgroundInvalidateCache(Tycoon); //UpdateObjectCache( Tycoon, noKind, noInfo );
              WorldLocator.SendEvent(
                TEvent.Create(
                  0,
                  Timer.GetVirtualTimeAbs,
                  Timer.GetVirtualTime,
                  5000,
                  1000,
                  InstantiateMultiString( Msg, [Name, Tycoon.Name] ),
                  '', '' ));
            end;
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Setting policy status OK!' );
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in Setting policy status' );
      end;
    end;

  procedure TTycoon.RDOSetAdvanceToNextLevel( yes : integer );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Setting AdvanceToNextLevel: ' + Name + ', ' + IntToStr(yes) );
      try
        if fAdvanceToNextLevel or CanAdvanceLevel
          then
            begin
              fAdvanceToNextLevel := yes = 1;
              ModelServerCache.BackgroundInvalidateCache(self); //UpdateObjectCache( self, noKind, noInfo );
              if fAdvanceToNextLevel
                then
                  WorldLocator.SendEvent(
                    TEvent.Create(
                      0,
                      Timer.GetVirtualTimeAbs,
                      Timer.GetVirtualTime,
                      10000,
                      1000,
                      InstantiateMultiString( mtidMsgWantLevelUpgrade, [Name, Level.NextLevel.Name] ),
                      '', '' ));
              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Setting AdvanceToNextLevel OK!' );
            end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in AdvanceToNextLevel' );
      end;
    end;

  procedure TTycoon.RDODelCurItem(index : integer);
    begin
      Logs.Log(tidLog_Survival, TimeToStr(Now) + ' Deleting Curriculum Item: ' + Name);
      try
        fCurriculum.Lock;
        try
          if (index < fCurriculum.Count) and CheckOpAuthenticity
            then fCurriculum.AtDelete(index);
        finally
          fCurriculum.Unlock;
        end;
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Deleting Curriculum Item OK!' );
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in Delete Curriculum Item' );
      end;
    end;

  procedure TTycoon.RDOSetCookie(cName, cValue : widestring);
    begin
      if CheckOpAuthenticity
        then fCookies.Values[cName] := cValue;
    end;

  function TTycoon.RDOFavoritesNewItem( Location : widestring; Kind : integer; Name, Info : widestring ) : OleVariant;
    begin
      Lock;
      try
        Logs.Log('favorites', TimeToStr(Now) + Format(' "%s" NewItem "%s", %d, "%s", "%s"', [self.fName, Location, Kind, Name, Info]));
        result := fFavorites.RDONewItem( Location, Kind, Name, Info );
      finally
        Unlock;
      end;
    end;

  function TTycoon.RDOFavoritesDelItem( Location : widestring ) : OleVariant;
    begin
      Lock;
      try
        Logs.Log('favorites', TimeToStr(Now) + Format(' "%s" DelItem "%s"', [self.fName, Location]));
        result := fFavorites.RDODelItem( Location );
      finally
        Unlock;
      end;
    end;

  function TTycoon.RDOFavoritesMoveItem( ItemLoc : widestring; Dest : widestring ) : OleVariant;
    begin
      Lock;
      try
        Logs.Log('favorites', TimeToStr(Now) + Format(' "%s" MoveItem "%s", "%s"', [self.fName, ItemLoc, Dest]));
        result := fFavorites.RDOMoveItem( ItemLoc, Dest );
      finally
        Unlock;
      end;
    end;

  function TTycoon.RDOFavoritesRenameItem( ItemLoc : widestring; Name : widestring ) : OleVariant;
    begin
      Lock;
      try
        Logs.Log('favorites', TimeToStr(Now) + Format(' "%s" RenameItem "%s", "%s"', [self.fName, ItemLoc, Name]));
        result := fFavorites.RDORenameItem( ItemLoc, Name );
      finally
        Unlock;
      end;
    end;

  function TTycoon.RDOFavoritesGetSubItems( ItemLoc : widestring ) : OleVariant;
    begin
      Lock;
      try
        Logs.Log('favorites', TimeToStr(Now) + Format(' "%s" Get "%s", "%s"', [self.fName, ItemLoc, Name]));
        result := fFavorites.RDOGetSubItems( ItemLoc );
      finally
        Unlock;
      end;
    end;

  procedure TTycoon.ComputeLevelingData;
    var
      hours  : single;
      profit : TMoney;
    begin
      hours := WorldLocator.GetHoursADay;
      profit :=
        Accounts.MasterAccount.Value
        - Accounts.AccountArray[accIdx_Construction].Value
        - Accounts.AccountArray[accIdx_Taxes].Value
        - Accounts.AccountArray[accIdx_TransfersIn].Value
        - Accounts.AccountArray[accIdx_TransfersOut].Value
        - Accounts.AccountArray[accIdx_Bank].Value;
      if Level.Tier > 5
        then profit := profit + abs(Accounts.AccountArray[accIdx_ResearchCenter_Research].Value);
      fYearProfitPerHour := profit/(365*hours);
    end;

  procedure TTycoon.GenMoney( Money : TMoney; Reason : TMoneyReason );
    begin
      inherited;
      fBudget := fBudget + Money;
    end;

  procedure TTycoon.CheckForNextLevel;
    var
      useless, year : word;
      NewId : string;
      leveled : boolean;
      PrevID : string;
    begin
      leveled := false;
      if AdvanceToNextLevel
        then
          begin
            if fLevelReqStatus <> nil
              then
                begin
                  fLevelReqStatus.Free;
                  fLevelReqStatus := nil;
                end;
            if (fLevel.NextLevel <> nil) and fLevel.NextLevel.AdvanceTycoon( self, fLevelReqStatus )
              then
                begin
                  if TheGlobalConfigHandler.GetConfigParm('LifeAfterLegend', '0') = '1'
                  then
                    begin
                      PrevID := fLevel.Id;
                      fLevel := fLevel.NextLevel;
                      fLevel.PrevLevelID := PrevID;
                      if fLevel.NextLevel = nil
                      then
                        begin
                          NewId := CreateNextLevel(fLevel.Tier + 1); // extend Levels
                          CacheMetaObject(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, NewId], noKind, noInfo );
                          fLevel.fNextLevel := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, NewID]);
                        end;
                    end
                  else
                    fLevel := fLevel.NextLevel;
                  DecodeDate( Timer.GetVirtualTime, year, useless, useless );
                  AddItemToCurriculum(
                    TOpenItem.Create(
                      '',
                      1,
                      InstantiateMultiString( mtidLevelAchieved, [fLevel.Name, year] ),
                      //SimHints.GetHintText( hidLevelAchieved, [fLevel.Name, year] ),
                      20,
                      fLevel.PrestigeBoost ));
                  fAdvanceToNextLevel := false;
                  //CacheObject( self, noKind, noInfo );
                  ModelServerCache.BackgroundInvalidateCache(self); //UpdateObjectCache( self, noKind, noInfo );
                  fPolicyModified := true;
                  WorldLocator.SendEvent(
                    TEvent.Create(
                      0,
                      Timer.GetVirtualTimeAbs,
                      Timer.GetVirtualTime,
                      500000,
                      1000,
                      InstantiateMultiString( mtidMsgLevelAdvanced, [Name, Level.Name] ),
                      '', '' ));
                  leveled := true;
                end
              else
                begin
                  if (Level <> nil) and (Level.NextLevel <> nil)
                    then
                      WorldLocator.SendEvent(
                        TEvent.Create(
                          0,
                          Timer.GetVirtualTimeAbs,
                          Timer.GetVirtualTime,
                          10000,
                          -10,
                          InstantiateMultiString( mtidMsgLevelAdvFailed, [Name, Level.NextLevel.Name] ),
                          '', '' ));
                end;
          end
        else
          begin
            fLevelReqStatus.Free;
            fLevelReqStatus := nil;
          end;
      if not leveled and (fLevel <> nil) and (fLevel.Tier > 5)
        then CheckLosingLevel;
      //UpdateObjectCache( self, noKind, noInfo );
    end;

  procedure TTycoon.CheckLosingLevel;
    var
      useless, year : word;
      PrecLevel : string;
      LostStatus : TMultiString;
    begin
      PrecLevel := Level.Name;
      DecodeDate( Timer.GetVirtualTime, year, useless, useless );
      if TOverLegendLevel(fLevel).LoseLevel( self, year, LostStatus )
        then
          begin
            if fLevelReqStatus = nil
              then fLevelReqStatus := TMultiString.Create;
            // >> Important: Do a for to set all the values for each language or people in other languages may see incorrect text.
            if fLevelReqStatus.Values['0'] <> ''
            then
              fLevelReqStatus.Values['0'] := fLevelReqStatus.Values['0'] + '. ' + LostStatus.Values['0']
            else
              fLevelReqStatus.Values['0'] := LostStatus.Values['0'];
            AddItemToCurriculum(
              TOpenItem.Create(
                '',
                1,
                InstantiateMultiString( mtidLevelLost, [PrecLevel, year] ),
                //SimHints.GetHintText( hidLevelAchieved, [fLevel.Name, year] ),
                20,
                -(fLevel.NextLevel.PrestigeBoost + (fLevel.NextLevel.PrestigeBoost/2) ) ));
            //CacheObject( self, noKind, noInfo );
            ModelServerCache.BackgroundInvalidateCache(self); //UpdateObjectCache( self, noKind, noInfo );
            fPolicyModified := true;
            WorldLocator.SendEvent(
              TEvent.Create(
                0,
                Timer.GetVirtualTimeAbs,
                Timer.GetVirtualTime,
                500000,
                1000,
                InstantiateMultiString( mtidMsgLevelLost, [Name, PrecLevel] ),
                '', '' ));
            fAdvanceToNextLevel := false;
          end;
        //else
        //  if (fLevelReqStatus <> nil) and (fLevelReqStatus.Text <> '')
        //    then fLevelReqStatus := nil;
    end;

  procedure TTycoon.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );

    var
      ResComp  : TMoney;
      ServComp : TMoney;

    function HasValidCompany : boolean;
      begin
        result := (AllCompaniesCount > 0) and not GetAllCompanies(0).Deleted;
      end;

    procedure Compensate;
      var
        iComp   : integer;
        Company : TCompany;
        iInv    : integer;
        resVal  : TMoney;
      begin
        if Level <> nil
          then
            begin
              resVal := Accounts.AccountArray[accIdx_ResearchCenter_Research].Value + Accounts.AccountArray[accIdx_ResearchCenter_ImpCosts].Value;
              // Do not compensate if it was positive
              if resVal < 0
                then ResComp := Level.PercOfResearchSubs*abs(resVal)
                else ResComp := 0;
              Companies.Lock;
              try
                if Level.PercOfResearchSubs > 0
                  then
                    for iComp := 0 to pred(Companies.Count) do
                      begin
                        Company := TCompany(Companies[iComp]);
                        for iInv := 0 to pred(Company.Inventions.Count) do
                          with TInventionRecord(Company.Inventions[iInv]) do
                            Subsidy := realmax(Subsidy, Level.PercOfResearchSubs*TotalCost); // >> JPS' crying aloud reason
                      end;
              finally
                Companies.Unlock;
              end;
            end
          else ResComp := 0;
      end;

    procedure PayExtraTaxes;
      const
        PercPerValue   = 10000000;
        MinPerc        = 50;
      var
        MA     : TMetaAccount;
        MT     : TMetaTaxToAccount;
        A      : TAccount;
        TA     : TAccount;
        count  : integer;
        i      : integer;
        ratio  : single;
        tax    : TMoney;
        MainD  : TMoneyDealer;
        TotTax : TMoney;
      begin
        TotTax := 0;
        try
          count := TheClassStorage.ClassCount[tidClassFamily_Accounts];
          for i := 0 to pred(count) do
            begin
              MA := TMetaAccount(TheClassStorage.ClassByIdx[tidClassFamily_Accounts, i]);
              if MA.Taxable
                then
                  begin
                    A     := Accounts.AccountArray[MA.AccId];
                    ratio := realmin(MinPerc, A.Value/PercPerValue);
                    if ratio > 1
                      then
                        begin
                          MT := TMetaTaxToAccount(TheClassStorage.ClassById[tidClassFamily_Taxes, MA.Id]);
                          if MT <> nil
                            then
                              begin
                                TA  := Accounts.AccountArray[MT.TaxAccountId];
                                tax := (ratio/100)*A.Value;
                                GenMoney( -tax, TA.MetaAccount.AccId );
                                TotTax := TotTax + tax;
                                TA.SecValue := TA.SecValue - tax;
                                MainD := WorldLocator.GetMainDealer;
                                if MainD <> nil
                                  then MainD.GenMoney( tax, TA.MetaAccount.AccId );
                              end;
                        end;
                  end;
            end;

          ServComp := 0; // No refund on services
          {// Tax refund over services
          TA := Accounts.AccountArray[accIdx_ResearchCenter_Supplies];
          if TA <> nil
            then ServComp := realmin(TotTax, abs(TA.Value))
            else ServComp := 0;}

          {// Land tax
          TA := Accounts.AccountArray[accIdx_LandTax];
          if TA <> nil
            then LandTax := AreaTax
            else LandTax := 0;
          }
        except
          Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in PayExtraTaxes' );
        end;
      end;

    var
      invCache : boolean;

    begin
      case PeriodType of
        perYear :
          begin
            ResComp  := 0;
            ServComp := 0;
            Compensate;
            fAccounts.EndOfPeriod( true );
            if SuperRole = nil
              then PayExtraTaxes;
            ComputeLevelingData;
            //CheckForNextLevel;
          end;
      end;
      inherited;

      //{ Tutorial
      invCache := false;
      Lock;
      try
        if (fTutorial <> nil) and IsRole
          then
            begin
              fTutorial.Free;
              fTutorial := nil;
            end;
        if (fTutorial <> nil) and (Level <> nil) and (Level.Tier > 0)
          then
            begin
              fTutorial.HideTaskButton;
              fTutorial.Free;
              fTutorial := nil;
              invCache  := true;
              SetCookie('tutorial', 'done');
            end;
        if (PeriodType = perHour) and (fTutorial <> nil) and not IsRole and IsOnline and HasValidCompany and fTutorial.Active and (fLanguage = '0') and  fTutorial.TimeToSimulate(PeriodCount)
          then
            begin
              if fTutorial.Execute = trFinished
                then
                  begin
                    fTutorial.HideTaskButton;
                    fTutorial.Free;
                    fTutorial := nil;
                    invCache  := true;
                    SetCookie('tutorial', 'done');
                  end;
              if (fTutorial <> nil) and not fTutorial.NotTycoon
                then
                  begin
                    fTutorial.NotifyTycoon('');
                    fTutorial.NotTycoon := true;
                  end;
            end;
        if (fTutorial <> nil) and fTutorial.NotTycoon and not IsOnline
          then fTutorial.NotTycoon := false;
      finally
        Unlock;
      end;
      if invCache
        then ModelServerCache.InvalidateCache(self, false);
      //} Tutorial

      case PeriodType of
        perYear :
          begin
            // Pay compensations researches and services
            GenMoney(ResComp + ServComp, accIdx_Compensations);

            // Pay Land Tax
            GenMoney(-AreaTax, accIdx_LandTax);

            // Check bankrupcy
            if fBudget > 0
              then fFailureLevel := 0
              else
                begin
                  WorldLocator.SendEvent(
                    TEvent.Create(
                      0,
                      Timer.GetVirtualTimeAbs,
                      Timer.GetVirtualTime,
                      5000,
                      -10,
                      InstantiateMultiString( mtidMsgBankruptDanger, [Name] ),
                      '', '' ));
                  inc( fFailureLevel );
                end;
            if IsOnline
              then
                RecordEvent(
                  TEvent.Create(
                    0,
                    Timer.GetVirtualTimeAbs,
                    Timer.GetVirtualTime,
                    30,
                    100000,
                    InstantiateMultiString( mtidMsgHappyNewYear, [0] ),
                    '', '' ));
            fInTowns.DeleteAll;
            fRoadBlocks := 0;        
          end;
        perDay :
          begin
            // >>
            UpdateRankings;
          end;
        perMonth :
          PublishCurriculum( false );
      end;
    end;

  procedure TTycoon.NewOwnedFacility( Facility : TFacility );
    begin
      RegisterSupplier( Facility );
    end;

  procedure TTycoon.FacilityDestroyed( Facility : TFacility );
    begin
      UnregisterSupplier( Facility );
    end;

  procedure TTycoon.RegisterSupplier( Facility : TFacility );
    var
      i              : integer;
      AutoConnection : TAutoConnection;
    begin
      for i := 0 to pred(Facility.CurrBlock.OutputCount) do
        begin
          AutoConnection := FindAutoConnection( Facility.CurrBlock.Outputs[i].MetaOutput.Name );
          if (AutoConnection <> nil) and
             (AutoConnection.Connections.IndexOf( Facility ) = NoIndex) and
             ((Facility.MetaFacility.Kind.Role in [rolDistributer, rolCompExport, rolCompInport]) or
              not AutoConnection.HireOnlyWarehouses or
              not (mfStorable in Facility.CurrBlock.Outputs[i].MetaOutput.MetaFluid.Options))
            then AutoConnection.Connections.Insert( Facility );
        end;
    end;

  procedure TTycoon.UnregisterSupplier( Facility : TFacility );
    var
      i  : integer;
      AC : TAutoConnection;
    begin
      fAutoConnections.Lock;
      try
        i := pred(fAutoConnections.Count);
        while i >= 0 do
          begin
            AC := TAutoConnection(fAutoConnections[i]);
            try
              AC.FacilityDestroyed( Facility );
            except
              try
                fAutoConnections.AtExtract(i);
                Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error unregistering supplier ' + Facility.Name + ' to tycoon ' + fName );
              except
              end;
            end;
            dec(i);
          end;
      finally
        fAutoConnections.Unlock;
      end;
    end;

  procedure TTycoon.AutoConnectFacility( Facility : TFacility );
    var
      i, j           : integer;
      AutoConnection : TAutoConnection;
      Supplier       : TFacility;
      Output         : TOutput;
      Input          : TInput;
    begin
      for i := 0 to pred(Facility.CurrBlock.InputCount) do
        begin
          Input := Facility.CurrBlock.Inputs[i];
          if mfTradeable in Input.MetaInput.MetaFluid.Options
            then
              begin
                AutoConnection := FindAutoConnection(Input.MetaInput.Name);
                if AutoConnection <> nil
                  then
                    begin
                      AutoConnection.Connections.Lock;
                      try
                        AutoConnection.Connections.Pack;
                        for j := 0 to pred(AutoConnection.Connections.Count) do
                          begin
                            Supplier := TFacility(AutoConnection.Connections[j]);
                            if (Supplier <> Facility) and not Supplier.CriticalTrouble and not Supplier.Deleted
                              then
                                try
                                 Output := Supplier.CurrBlock.OutputsByName[Input.MetaInput.Name];
                                  if Output <> nil
                                    then Output.ConnectTo(Input);
                                except
                                end;
                          end;
                      finally
                        AutoConnection.Connections.Unlock;
                      end;
                      if AutoConnection.HireTradeCenter and (Input.MetaInput.Level = mglBasic)
                        then
                          begin
                            Supplier := TInhabitedTown(Facility.Town).TradeCenter;
                            if Supplier <> nil
                              then
                                begin
                                  Output := Supplier.CurrBlock.OutputsByName[Input.MetaInput.Name];
                                  if (Output <> nil)
                                    then Output.ConnectTo(Input);
                                end;
                          end;
                    end;
              end;
        end;
      //try
        // TInhabitedTown(Facility.Town).World.SearchForSuppliers( Facility.CurrBlock ); >> THIS WAS MOVED TO MAIN SIM CYCLE
      //except
      //end;
    end;

  procedure TTycoon.TycoonDeleted( Tycoon : TTycoon );
    var
      i : integer;
    begin
      try
        Lock;
        try
          fVotes.ClearVotes(Tycoon);
        finally
          Unlock;
        end;
        fPolicies.Lock;
        try
          i := 0;
          while (i < fPolicies.Count) and (TTycoonPolicy(fPolicies[i]).fTycoon <> Tycoon) do
            inc( i );
          if i < fPolicies.Count
            then fPolicies.AtDelete( i );
        finally
          fPolicies.Unlock;
        end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error excluding Tycoon Deleted..');
      end;
    end;

  procedure TTycoon.PublishCurriculum( AllItems : boolean );
    var
      Item : TCurriculumItem;
      i    : integer;
    begin
      fCurriculum.Lock;
      try
        for i := 0 to pred(fCurriculum.Count) do
          try
            Item := TCurriculumItem(fCurriculum[i]);
            if (not Item.Published or AllItems) and Item.IsGlobal
              then WorldLocator.AddCurriculumItem( self, Item );
          except
          end;
      finally
        fCurriculum.Unlock;
      end;
    end;

  procedure TTycoon.LoadFromBackup( Reader : IBackupReader );
    var
      count : integer;
      i     : integer;
      TP    : TTycoonPolicy;
      str   : string;
      //Obj   : TObject;
      LevelIn : boolean;
      NewLevID : string;
    begin
      inherited;
      fId := Reader.ReadInteger( 'Id', 0 );
      Reader.ReadBuffer( 'Options', fOptions, @fOptions, sizeof(fOptions) );
      Reader.ReadObject( 'Companies', fCompanies, nil );
      Reader.ReadObject( 'Roles', fRoles, nil );
      Reader.ReadObject( 'SuperRole', fSuperRole, nil );
      fName := trim(Reader.ReadString( 'Name', '' ));
      fPassword := Reader.ReadString( 'Password', '' );
      fBudget := Reader.ReadCurrency( 'Budget', 0 );
      fFailureLevel := Reader.ReadInteger( 'FailureLevel', 0 );
      Reader.ReadObject( 'AutoConnections', fAutoConnections, nil );
      if fAutoConnections = nil
        then fAutoConnections := TLockableCollection.Create( 0, rkBelonguer );
      UpdateAutoConnections;
      // >> Binary incompatibility!
      Reader.ReadObject( 'Events', fEvents, nil );
      {if fEvents = nil
        then }fEvents := TLockableCollection.Create( 0, rkBelonguer );
      fPolicies := TLockableCollection.Create( 0, rkBelonguer );
      count := Reader.ReadInteger( 'PolicyCount', 0 );
      for i := 0 to pred(count) do
        begin
          TP := TTycoonPolicy.Create;
          Reader.ReadObject( 'Tycoon.' + IntToStr(i), TP.fTycoon, nil );
          TP.fStatus := TPolicyStatus(Reader.ReadInteger( 'Status.' + IntToStr(i), integer(pstNeutral) ));
          if TP.fTycoon <> nil
            then fPolicies.Insert( TP );
        end;
      Reader.ReadObject( 'Curriculum', fCurriculum, nil );
      if fCurriculum = nil
        then
          begin
            fCurriculum := TLockableCollection.Create( 0, rkBelonguer );
            AddItemToCurriculum( TOwnershipItem.Create( TOwnershipItem.ClassName, 0 ) );
          end;
      if not FindResearchItemsInCurriculum // >> CountItemsInCurriculum( currKind_Research ) = 0
        then AddItemToCurriculum( TResearchItem.Create( TResearchItem.ClassName, currKind_Research ) );

      // Tutorial
      Reader.ReadObject('TaskContext', fTaskContext, nil); //Reader.ReadObject('TaskContext', Obj, nil);
      {if Obj <> nil
        then Obj.Free;}
      Reader.ReadObject('Tutorial', fTutorial, nil); //Reader.ReadObject('Tutorial', Obj, nil);
      {if Obj <> nil
        then Obj.Free;}

      {if fTutorial = nil // >> REM
        then InstantiateTutorial;}

      Reader.ReadObject( 'Cookies', fCookies, nil );
      if fCookies = nil
        then fCookies := TStringList.Create;
      str := Reader.ReadString( 'TycoonLevel', '' );
      if TheGlobalConfigHandler.GetConfigParm('LifeAfterLegend', '0') = '1'
      then
        begin
          LevelIn := False;
          while LevelIn = False do
            begin
              for i := 0 to (TheClassStorage.ClassCount[tidClassFamily_TycoonLevels] - 1) do
                if str = TTycoonLevel(TheClassStorage.ClassByIdx[tidClassFamily_TycoonLevels, i]).Id
                then
                  LevelIn := True;
              if LevelIn = False
              then
                begin
                  NewLevID := CreateNextLevel(TheClassStorage.ClassCount[tidClassFamily_TycoonLevels]);
                  CacheMetaObject(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, NewLevID], noKind, noInfo );// extend Level
                  for i := 0 to (TheClassStorage.ClassCount[tidClassFamily_TycoonLevels] - 1) do
                    if TTycoonLevel(TheClassStorage.ClassByIdx[tidClassFamily_TycoonLevels, i]).Tier = (TheClassStorage.ClassCount[tidClassFamily_TycoonLevels] - 2)
                    then
                      begin
                        TTycoonLevel(TheClassStorage.ClassByIdx[tidClassFamily_TycoonLevels, i]).NextLevel := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, NewLevID]);
                        TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, NewLevID]).PrevLevelID := TTycoonLevel(TheClassStorage.ClassByIdx[tidClassFamily_TycoonLevels, i]).Id;
                      end;
                end
              else
                if TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, str]).NextLevel = nil
                then
                  begin
                    NewLevID := CreateNextLevel(TheClassStorage.ClassCount[tidClassFamily_TycoonLevels]);
                    CacheMetaObject(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, NewLevID], noKind, noInfo );// extend Next Level
                    TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, str]).NextLevel := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, NewLevID]);
                    TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, NewLevID]).PrevLevelID := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, str]).Id;
                  end;
            end;
        end
      else
        begin
          LevelIn := False;
          for i := 0 to (TheClassStorage.ClassCount[tidClassFamily_TycoonLevels] - 1) do
            if str = TTycoonLevel(TheClassStorage.ClassByIdx[tidClassFamily_TycoonLevels, i]).Id
            then
              LevelIn := True;
          if LevelIn = False
          then
            str := tidTycoonLevel_Legend;
        end;
      fLevel := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, str]);
      fAdvanceToNextLevel := Reader.ReadBoolean( 'AdvanceToNextLevel', false );
      //fLevelReqStatus := Reader.ReadString( 'LevelReqStatus', '' );
      Reader.ReadObject( 'LevelReqStatus_MLS', fLevelReqStatus, nil );
      fLicenceLevel := Reader.ReadSingle('LicLevel', 0);
      fLanguage := Reader.ReadString( 'Language', langDefault );
      fTranscending := Reader.ReadBoolean( 'Transcending', false );
      fWillTranscend := Reader.ReadBoolean( 'WillTranscend', false );

      // Read Nobility
      str := fCookies.Values['NobPts'];
      if str <> ''
        then fNobPoints := StrToInt(str);
      fCookies.Values['NobPts'] := '';

      // Read Demo Status
      str := fCookies.Values['Demo'];
      fIsDemo := str <> '';
      fCookies.Values['Demo'] := '';

      // Votes
      Reader.ReadObject('Votes', fVotes, nil);
      if fVotes = nil
        then fVotes := TVoteSystem.Create;
      Reader.ReadObject('InTowns', fInTowns, nil);
      if fInTowns = nil
        then fInTowns := TCollection.Create(0, rkUse);

      Reader.ReadObject( 'Favorites', fFavorites, nil );
      if fFavorites = nil
        then fFavorites := TFavorites.Create;

      // Write reserved (1 taken 2/13/2002)
      for i := 3 to 10 do
        Reader.ReadInteger('Rsvd' + IntToStr(i), 0);
    end;

  procedure TTycoon.StoreToBackup( Writer : IBackupWriter );
    var
      i   : integer;
      aux : string;
    begin
      try
        inherited;
        Writer.WriteInteger( 'Id', fId );
        Writer.WriteBuffer( 'Options', fOptions, sizeof(fOptions) );
        Writer.WriteLooseObject( 'Companies', fCompanies );
        Writer.WriteLooseObject( 'Roles', fRoles );
        Writer.WriteObjectRef( 'SuperRole', fSuperRole );
        Writer.WriteString( 'Name', fName );
        Writer.WriteString( 'Password', fPassword );
        Writer.WriteCurrency( 'Budget', fBudget );
        Writer.WriteInteger( 'FailureLevel', fFailureLevel );
        Writer.WriteUnsafeLooseObject( 'AutoConnections', fAutoConnections );
        // >> Binary incompatibility!
        //Writer.WriteLooseObject( 'Events', fEvents );
        Writer.WriteLooseObject( 'Events', nil );
        Writer.WriteInteger( 'PolicyCount', fPolicies.Count );
        for i := 0 to pred(fPolicies.Count) do
          with TTycoonPolicy(fPolicies[i]) do
            begin
              aux := 'Tycoon.' + IntToStr(i);
              Writer.WriteObjectRef( aux, fTycoon );
              aux := 'Status.' + IntToStr(i);
              Writer.WriteInteger( aux, integer(fStatus) );
            end;

        Writer.WriteLooseObject( 'Curriculum', fCurriculum );
        Writer.WriteLooseObject( 'TaskContext', {nil}fTaskContext);
        Writer.WriteObject( 'Tutorial', {nil}fTutorial);

        // Write Nobility and Demo Status
        fCookies.Values['NobPts'] := IntToStr(fNobPoints);
        if fIsDemo
          then fCookies.Values['Demo'] := '1'
          else fCookies.Values['Demo'] := '';

        Writer.WriteUnsafeLooseObject('Cookies', fCookies);

        fCookies.Values['Demo']   := '';
        fCookies.Values['NobPts'] := '';

        if fLevel <> nil
          then Writer.WriteString( 'TycoonLevel', fLevel.Id )
          else Writer.WriteString( 'TycoonLevel', '' );
        Writer.WriteBoolean( 'AdvanceToNextLevel', fAdvanceToNextLevel );
        //Writer.WriteString( 'LevelReqStatus', fLevelReqStatus );
        Writer.WriteLooseObject( 'LevelReqStatus', fLevelReqStatus );
        Writer.WriteSingle('LicLevel', fLicenceLevel);
        Writer.WriteString( 'Language', fLanguage );
        Writer.WriteBoolean( 'Transcending', fTranscending );
        Writer.WriteBoolean( 'WillTranscend', fWillTranscend );
        aux := '';

        // Votes
        Writer.WriteLooseObject('Votes', fVotes);
        Writer.WriteLooseObject('InTowns', fInTowns);

        Writer.WriteLooseObject( 'Favorites', fFavorites );

        // Save reserved (1 taken)
        for i := 3 to 10 do
          Writer.WriteInteger('Rsvd' + IntToStr(i), 0);
      except
        Logs.Log( 'Survival', 'Error Storing tycoon: ' + fName );
        raise;
      end;
    end;

  procedure TTycoon.Loaded;

    procedure InitFavorites;
      var
        cnt, i : integer;
        name   : string;
        x, y   : integer;
        select : boolean;
        link   : string;
      begin
        if (fFavorites = nil) or not fFavorites.CheckIntegrity
          then fFavorites := TFavorites.Create;
        if Cookie['LinkCount'] <> ''
          then
            try
              cnt := StrToInt(Cookie['LinkCount']);
              for i := 0 to pred(cnt) do
                begin
                  link := 'Link' + IntToStr(i);
                  if ParseLinkCookie(Cookie[link], name, x, y, select)
                    then
                      begin
                        Favorites.RDONewItem('', fvkLink, name, ComposeLinkCookie(name, x, y, true));
                        //Cookie[link] := '';
                      end;
                end;
              Cookie['LinkCount'] := '';
            except
              Logs.Log( 'Survival', 'Error converting old favorite links: ' + fName );
            end;
      end;

    procedure InitLevel;
      const
        levelIds : array[0..6] of string =
          (tidTycoonLevel_Apprentice,
           tidTycoonLevel_Entrepreneur,
           tidTycoonLevel_Tycoon,
           tidTycoonLevel_Master,
           tidTycoonLevel_Paradigm,
           tidTycoonLevel_Legend,
           tidTycoonLevel_OverLegend);
      var
        count : integer;
        i     : integer;
      begin
        if fLevel = nil
          then
            begin
              count := FacCount;
              i     := 0;
              repeat
                fLevel := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, levelIds[i]]);
                inc( i );
              until (fLevel.FacLimit > count) or (i > 5);
            end;
      end;

    var
      i : integer;
      C : TCompany;

    begin
      inherited;
      // Pack all
      fCompanies.Pack;
      fRoles.Pack;
      fAutoConnections.Pack;
      fPolicies.Pack;
      fCurriculum.Pack;

      // Pack Facilities
      for i := pred(fCompanies.Count) downto 0 do
        begin
          C := TCompany(fCompanies[i]);
          if C.Owner = self
            then C.Facilities.Pack
            else
              begin
                // >> Remove unowned companies
                fCompanies.Extract(C);
              end;
        end;

      {// Why???
      for i := 0 to pred(fCurriculum.Count) do
        try
          CacheObject( fCurriculum[i], noKind, noInfo );
        except
        end;}

      {if fTutorial = nil // >> REM
        then InstantiateTutorial
        else
          begin
            if fTaskContext = nil
              then fTaskContext := TTycoonContext.Create(self, nil, nil);
            fTutorial.Loaded(fTaskContext);
          end;}

      if (fTutorial <> nil) and (fTaskContext <> nil)
        then fTutorial.Loaded(fTaskContext);

      InitRankings;
      InitLevel;
      fCountUpgrades := TheGlobalConfigHandler.GetConfigParm('CountUpgrades', '1') = '1';
      fTournamentOn  := StrToInt(TheGlobalConfigHandler.GetConfigParm('TornamentLength', '0')) > 0;
      CountFacilities;
      Cookie[tidCookie_LastVirtTimeOnline] := '';
      InitFavorites;
    end;

  procedure TTycoon.UpdateParameters;

    procedure UpdateCurriculum;
      var
        i, j          : integer;
        CurrPrestige  : TPrestige;
        Company       : TCompany;
      begin
        fFacPrestige   := fCurrFacPrestige;
        fResearchPrest := 0;
        fResearchCount := 0;
        fCurriculum.Lock;
        try
          CurrPrestige := 0;
          Companies.Lock;
          try
            for i := 0 to pred(Companies.Count) do
              begin
                Company := TCompany(Companies[i]);
                Company.Inventions.Lock;
                try
                  for j := 0 to pred(Company.Inventions.Count) do
                    begin
                      fResearchPrest := fResearchPrest + TInventionRecord(Company.Inventions[j]).Invention.Prestige;
                      inc(fResearchCount);
                    end;
                finally
                  Company.Inventions.Unlock;
                end;
              end;
          finally
            Companies.Unlock;
          end;
          for i := 0 to pred(fCurriculum.Count) do
            CurrPrestige := CurrPrestige + TCurriculumItem(fCurriculum[i]).Prestige;
          fPrestige := CurrPrestige;
        finally
          fCurriculum.Unlock;
        end;
        fCurrFacPrestige := 0;
      end;

    var
      j, k, l : integer;
    begin
      UpdateCurriculum;
      if PolicyModified
        then
          begin
            PolicyModified := false;
            Companies.Lock;
            try
              for j := 0 to pred(Companies.Count) do
                with TCompany(Companies[j]) do
                  begin
                    Facilities.Lock;
                    try
                      for k := 0 to pred(Facilities.Count) do
                        with TFacility(Facilities[k]) do
                          try
                            with CurrBlock do
                              begin
                                for l := 0 to pred(InputCount) do
                                  if mfTradeable in Inputs[l].MetaInput.MetaFluid.Options
                                    then Inputs[l].SortConnections;
                                for l := 0 to pred(OutputCount) do
                                  if mfTradeable in Outputs[l].MetaOutput.MetaFluid.Options
                                    then Outputs[l].SortConnections;
                              end
                          except
                            on E : Exception do
                              Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error in UpdateParameter.. ' + E.Message);
                          end;
                    finally
                      Facilities.Unlock;
                    end;
                  end;
            finally
              Companies.Unlock;
            end;
          end
    end;

  procedure TTycoon.InitRankings;
    var
      i        : integer;
      count    : integer;
      Ranking  : TRanking;
      RankInfo : TRankingInfo;
    begin
      fRankings := TLockableCollection.Create( 0, rkBelonguer );
      count := TheClassStorage.ClassCount[tidClassFamily_Rankings];
      for i := 0 to pred(count) do
        begin
          Ranking  := TRanking(TheClassStorage.ClassByIdx[tidClassFamily_Rankings, i]);
          if Ranking.AppliesTo( self )
            then
              begin
                RankInfo := TRankingInfo.Create;
                RankInfo.fRanking  := Ranking;
                RankInfo.fPosition := Ranking.RankingOf( self );
                fRankings.Insert( RankInfo );
              end;
        end;
    end;

  procedure TTycoon.UpdateRankings;

    const
      InfinitePos = 100000;

    var
      i     : integer;
      sum   : integer;
      count : integer;
    begin
      fRankings.Lock;
      try
        sum   := 0;
        count := 0;
        for i := 0 to pred(fRankings.Count) do
          with TRankingInfo(fRankings[i]) do
            begin
              if fRanking.IsRankeable( self )
                then fPosition := fRanking.RankingOf( self )
                else fPosition := InfinitePos;
              if (fPosition <> NoIndex) and not fRanking.IsOverall
                then
                  begin
                    sum := sum + fPosition;
                    inc( count );
                  end;
            end;
        if count > 0
          then fRankingAvg := InfinitePos - sum div count
          else fRankingAvg := InfinitePos;
        fRanking := RankingById[tidRankind_Main].fPosition;
      finally
        fRankings.Unlock;
      end;
    end;

  function TTycoon.GetCacheName : string;
    begin
      result := 'Ty' + IntToStr(fId);
    end;

  procedure TTycoon.ResetTutorial;
    begin
      Lock;
      try
        if fTutorial <> nil
          then
            begin
              fTutorial.Free;
              fTaskContext := nil;
              fTutorial := nil;
              if not fDeleted
                then InstantiateTutorial;  // if not deleted
            end;
      finally
        Unlock;
      end;
    end;

  procedure TTycoon.CancelTutorial;
    begin
      Lock;
      try
        if fTutorial <> nil
          then fTutorial.Cancel;
      finally
        Unlock;
      end;
    end;

  procedure TTycoon.InstantiateTutorial;
    var
      MetaTask : TMetaTask;
    begin
      Lock;
      try
        if (GetCookie('tutorial') = '') and {(Level <> nil) and (Level.Tier = 0) and} not IsRole and (fNobPoints < 10)
          then
            begin
              MetaTask := TMetaTask(TheClassStorage.ClassById[tidClassFamily_Tasks, tidTask_Tutorial]);
              if fTaskContext = nil
                then fTaskContext := TTycoonContext.Create(self, nil, nil);
              if MetaTask <> nil
                then fTutorial := MetaTask.Instantiate(nil, fTaskContext);
            end;
      finally
        Unlock;
      end;
    end;

  procedure TTycoon.ResetLevel;
    begin
      fLevel              := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, tidTycoonLevel_Apprentice]);
      fAdvanceToNextLevel := false;
      fLevelReqStatus     := nil;
    end;

  procedure TTycoon.RDOActivateTutorial(Value : LongBool);
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Activating tutorial: ' + Name );
      Lock;
      try
        if fTutorial <> nil
          then fTutorial.Active := Value;
      finally
        Unlock;
      end;
    end;

  function TTycoon.RDOActiveTutorial(useless : integer) : OleVariant;
    begin
      Lock;
      try
        result := (fTutorial <> nil) and fTutorial.Active;
      finally
        Unlock;
      end;
    end;

  procedure TTycoon.RDOKillTutorial;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Killing tutorial..' );
      try
        Lock;
        try
          if fTutorial <> nil
            then
              begin
                fTutorial.Free;
                fTutorial := nil;
                SetCookie('tutorial', 'done');
              end;
        finally
          Unlock;
        end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error killing tutorial..' );
      end;
    end;

  procedure TTycoon.DefaultHandler(var Message);
    begin
      Lock;
      try
        if fTutorial <> nil
          then fTutorial.Dispatch(Message);
      finally
        Unlock;
      end;
    end;

  procedure TTycoon.CountFacilities;
    var
      cmpIdx : integer;
      facIdx : integer;
      Comp   : TCompany;
      Fac    : TFacility;
      count  : integer;
      A      : integer;
    begin
      Lock;
      try
        count := 0;
        A     := 0;
        for cmpIdx := 0 to pred(fCompanies.Count) do
          begin
            Comp := TCompany(fCompanies[cmpIdx]);
            Comp.Lock;
            try
              for facIdx := 0 to pred(Comp.Facilities.Count) do
                begin
                  Fac := TFacility(Comp.Facilities[facIdx]);
                  if fCountUpgrades
                    then inc(count, Fac.MetaFacility.SlotCount*Fac.UpgradeLevel)
                    else inc(count, Fac.MetaFacility.SlotCount);
                  inc(A, Fac.MetaFacility.xSize*Fac.MetaFacility.ySize);
                end
            finally
              Comp.Unlock;
            end;
          end;
        fFacilityCount := count;
        fArea          := A;
      finally
        Unlock;
      end;
    end;

  procedure TTycoon.CountFacility(Facility : TFacility; created : boolean);
    begin
      if created
        then                           
          begin
            if fCountUpgrades
              then inc(fFacilityCount, Facility.MetaFacility.SlotCount*Facility.UpgradeLevel)
              else inc(fFacilityCount, Facility.MetaFacility.SlotCount);
            inc(fArea, Facility.MetaFacility.xSize*Facility.MetaFacility.ySize);
          end
        else
          begin
            if fCountUpgrades
              then dec(fFacilityCount, Facility.MetaFacility.SlotCount*Facility.UpgradeLevel)
              else dec(fFacilityCount, Facility.MetaFacility.SlotCount);
            dec(fArea, Facility.MetaFacility.xSize*Facility.MetaFacility.ySize);
          end;
    end;

  function TTycoon.ReachedBuildLimit : boolean;
    begin
      result := FacCount >= fLevel.FacLimit;
    end;

  procedure TTycoon.RemoveFacilityLink(Facility : TFacility);
    var
      link : string;
      aux  : string;
      i    : integer;
    begin
      link := Protocol.ComposeLinkCookie('', Facility.xPos, Facility.yPos, true);
      Lock;
      try
        if fFavorites <> nil
          then fFavorites.DeleteCord(Facility.xPos, Facility.yPos);
        for i := pred(fCookies.Count) downto 0 do
          begin
            aux := fCookies[i];
            if system.pos(link, aux) <> 0
              then fCookies.Delete(i);
          end;
      finally
        Unlock;
      end;
    end;

  function TTycoon.CheckOpAuthenticity : boolean;
    begin
      result := LoggedUserData.CheckAuthenticity(MasterRole);
    end;

  function TTycoon.UpdateNobility : boolean;
    begin
      result := (fWorldLocator <> nil) and fWorldLocator.UpdateNobility(self);
    end;

  function TTycoon.CanAdvanceLevel : boolean;
    var
      demo : boolean;
    begin
      demo := IsDemo;
      if demo
        then
          begin
            if fWorldLocator <> nil
              then
                begin
                  demo := fWorldLocator.IsDemoAccount(fName);
                  IsDemo := demo;
                end;
          end;
      result := not demo;
    end;

  function TTycoon.GetCanBuildAdvanced : boolean;
    var
      WL   : IWorldLocator;
      tier : integer;
    begin
      WL := WorldLocator;
      if Level <> nil
        then tier := Level.Tier
        else tier := 0;
      if WL <> nil
        then result := (tier >= 4) or ((tier >= WL.MinLevelToBuildAdvanced) and (GetNobPoints >= 100))
        else result := (tier >= 4);
    end;

  procedure TTycoon.ClearVotes;
    begin
      fVotes.Clear;
    end;

  function TTycoon.PaysTaxesInTown(Town : TTown) : boolean;
    begin
      result := not IsRole and (fInTowns.IndexOf(Town) <> noIndex);
    end;

  procedure TTycoon.PaidTaxesInTown(Town : TTown);
    begin
      if not IsRole and (fInTowns.IndexOf(Town) = noIndex)
        then fInTowns.Insert(Town);
    end;

  function TTycoon.GetPaysTaxes : boolean;
    begin
      result := (fInTowns <> nil) and (fInTowns.Count > 0);
    end;

  function TTycoon.CanBuildRoad(tiles : integer) : boolean;
    var
      Max : integer;
    begin
      if (fLevel <> nil) and (fLevel.Tier < 2) and (fNobPoints < 50)
        then Max := 30
        else Max := 0;
      result := (Max = 0) or (fRoadBlocks + tiles <= Max);
    end;

  function TTycoon.CanGetLoan(amount : TMoney) : boolean;
    begin
      result := true;
    end;

  function TTycoon.GetLoanLimit : TMoney;
    begin
      if (Level <> nil)
        then
          if not IsDemo
            then result := (Level.Tier + 1.0)*500*1000*1000.0
            else result := 200*1000*1000.0
        else result := inherited GetLoanLimit;
    end;

  function TTycoon.HasLegacy : boolean;
    begin
      result := IsRole or (Companies.Count > 0) or (Prestige > 0) or (CountItemsInCurriculum(crrKind_JoinedWorld) > 0);
    end;

  // TTycoonContext

  constructor TTycoonContext.Create(aTycoon : TTycoon; aCompany : TCompany; aTown : TTown);
    begin
      inherited Create;
      fTycoon  := aTycoon;
      fCompany := aCompany;
      fTown    := aTown;
    end;

  function TTycoonContext.QueryInterface(const IID: TGUID; out Obj): hresult;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TTycoonContext._AddRef: integer;
    begin
      result := 1;
    end;

  function TTycoonContext._Release: integer;
    begin
      result := 1;
    end;

  function TTycoonContext.getContext(id : integer) : TObject;
    begin
      case id of
        tcIdx_Tycoon :
          result := fTycoon;
        tcIdx_Company :
          result := fCompany;
        tcIdx_Town :
          result := fTown;
        else result := nil;
      end;
    end;

  procedure TTycoonContext.setContext(id : integer; Obj : TObject);
    begin
      case id of
        tcIdx_Tycoon :
          TObject(fTycoon) := Obj;
        tcIdx_Company :
          TObject(fCompany) := Obj;
        tcIdx_Town :
          TObject(fTown) := Obj;
      end;
    end;

  procedure TTycoonContext.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject('Tycoon', fTycoon, nil);
      Reader.ReadObject('Company', fCompany, nil);
      Reader.ReadObject('Town', fTown, nil);
    end;

  procedure TTycoonContext.StoreToBackup ( Writer : IBackupWriter );
    begin
      inherited;
      //Logs.Log( 'Survival', 'Start Storing TycoonContext' );
      Writer.WriteObjectRef('Tycoon', fTycoon);
      Writer.WriteObjectRef('Company', fCompany);
      Writer.WriteObjectRef('Town', fTown);
      //Logs.Log( 'Survival', 'End Storing TycoonContext' );
    end;


  // Utility functions

  function GetTaxableAccount( BaseAccount : TAccountId ) : string;
    var
      MA : TMetaAccount;
    begin
      MA := TMetaAccount(TheClassStorage.ClassById[tidClassFamily_Accounts, IntToStr(BaseAccount)]);
      if MA <> nil
        then
          begin
            while not MA.Taxable and (MA.MasterAccount <> nil) do
              MA := MA.MasterAccount;
            if MA.Taxable
              then result := MA.Id
              else result := '';
          end
        else result := '';
    end;

  function GetLevelOfTier( Tier : integer ) : TTycoonLevel;
    var
      count, i : integer;
    begin
      count := TheClassStorage.ClassCount[tidClassFamily_TycoonLevels];
      i     := 0;
      while (i < count) and (TTycoonLevel(TheClassStorage.ClassByIdx[tidClassFamily_TycoonLevels, i]).Tier <> Tier) do
        inc( i );
      if i < count
        then result := TTycoonLevel(TheClassStorage.ClassByIdx[tidClassFamily_TycoonLevels, i])
        else result := nil;
    end;

  function CheckAuthenticity(Tycoon : TTycoon) : boolean;
    begin
      result := (Tycoon <> nil) and LoggedUserData.CheckAuthenticity(Tycoon.MasterRole);
    end;

  // Model Extensions stuff

  function GetMDXId( MDX : THandle ) : string;
    var
      GetMDXIdProc : TGetMDXIdProc;
    begin
      GetMDXIdProc := GetProcAddress( MDX, tidProcName_GetMDXId );
      if assigned(GetMDXIdProc)
        then result := GetMDXIdProc
        else raise Exception.Create( tidProcName_GetMDXId + ' not found in MDX' );
    end;

  function GetMDXDependances( MDX : THandle ) : TStringList;
    var
      GetMDXDependancesProc : TGetMDXDependancesProc;
    begin
      GetMDXDependancesProc := GetProcAddress( MDX, tidProcName_GetMDXDependances );
      if assigned(GetMDXDependancesProc)
        then
          begin
            result := TStringList.Create;
            result.Text := GetMDXDependancesProc;
          end
        else raise Exception.Create( tidProcName_GetMDXDependances + ' not found in MDX' );
    end;

  procedure RegisterMDX( MDX : THandle );
    var
      RegisterMDXProc : TRegisterMDXProc;
    begin
      RegisterMDXProc := GetProcAddress( MDX, tidProcName_RegisterMDX );
      if assigned(RegisterMDXProc)
        then RegisterMDXProc;
    end;

  procedure PostRegisterMDX( MDX : THandle );
    var
      PostRegisterMDXProc : TPostRegisterMDXProc;
    begin
      PostRegisterMDXProc := GetProcAddress( MDX, tidProcName_PostRegisterMDX );
      if assigned(PostRegisterMDXProc)
        then PostRegisterMDXProc;
    end;


  // RegisterSurfaces

  var
    fBeautySurface : TLandSurface = nil;

  procedure RegisterSurfaces;
    begin
      InitSurfaces;
      fBeautySurface := TLandSurface.Create( tidEnvironment_Beauty, 'Beauty', [0.5, 0.1, 0, 10] );
      TSurface.Create( tidEnvironment_Pollution, 'Pollution' );
      TSurface.Create( tidEnvironment_Crime, 'Crime' );
      TSurface.Create( tidEnvironment_QOL, 'Comodities' );
      TSurface.Create( tidEnvironment_BAP, 'Business Area Points' );
    end;

  procedure UpdateLandSurfaces( Land : ILandInfo );
    begin
      fBeautySurface.Land := Land;
    end;


  // RegisterTownParameters

  procedure RegisterTownParameters;
    var
      count : integer;
      i     : integer;
      MF    : TMetaFluid;
    begin
      try
        count := TheClassStorage.ClassCount[tidClassFamily_Fluids];
      except
        count := 0;
      end;
      for i := 0 to pred(count) do
        begin
          MF := TMetaFluid(TheClassStorage.ClassByIdx[tidClassFamily_Fluids, i]);
          if mfTradeable in MF.Options
            then
              begin
                TMetaTownParameter.Create( tidTownParameter_InputValue + MF.Id, '', true ).Register;
                TMetaTownParameter.Create( tidTownParameter_InputCapacity + MF.Id, '', true ).Register;
                TMetaTownParameter.Create( tidTownParameter_InputQuality + MF.Id, '', true ).Register;
                TMetaTownParameter.Create( tidTownParameter_InputPrice + MF.Id, '', true ).Register;
                TMetaTownParameter.Create( tidTownParameter_InputMaxPrice + MF.Id, '', true ).Register;
                TMetaTownParameter.Create( tidTownParameter_OutputValue + MF.Id, '', true ).Register;
                TMetaTownParameter.Create( tidTownParameter_OutputCapacity + MF.Id, '', true ).Register;
                TMetaTownParameter.Create( tidTownParameter_OutputQuality + MF.Id, '', true ).Register;
                TMetaTownParameter.Create( tidTownParameter_OutputPrice + MF.Id, '', true ).Register;
              end;
        end;
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      Persistent.RegisterBackup;
      Plotter.RegisterBackup;
      RegisterClass( TFacility );
      RegisterClass( TBlock );
      RegisterClass( TGate );
      RegisterClass( TInput );
      RegisterClass( TOutput );
      RegisterClass( TPushInput );
      RegisterClass( TPullInput );
      RegisterClass( TPushOutput );
      RegisterClass( TPullOutput );
      //RegisterClass( TCompanyResearch );
      RegisterClass( TCompanyDirection );
      RegisterClass( TCompanyMetaFacilityInfo );
      RegisterClass( TCompanyMetaFacilityList );
      RegisterClass( TCompany );
      RegisterClass( TTownParameter );
      RegisterClass( TTown );
      RegisterClass( TAutoConnection );
      RegisterClass( TLoan );
      RegisterClass( TBank );
      RegisterClass( TTycoon );
      RegisterClass( TTycoonContext );
    end;


  // Visual Classes

  procedure InitVisualClasses;
    var
      Stream : TStream;
    begin
      Stream := TFileStream.Create( GetVisualRootPath + 'classes.bin', fmOpenRead );
      try
        VisualClasses := TClassManager.Load( Stream );
      finally
        Stream.Free;
      end;
    end;

  procedure DoneVisualClasses;
    begin
      VisualClasses.Free;
    end;

  procedure InitMLS;
    begin
      mtidPeopleKindName[pkHigh]   := TRegMultiString.Create( 'mtidPeopleKindNameHigh', 'High class' );
      mtidPeopleKindName[pkMiddle] := TRegMultiString.Create( 'mtidPeopleKindNameMiddle', 'Middle class' );
      mtidPeopleKindName[pkLow]    := TRegMultiString.Create( 'mtidPeopleKindNameLow', 'Low class' );

      mtidWorkforceKindName[pkHigh]   := TRegMultiString.Create( 'mtidWorkforceKindNameHigh', 'Executives' );
      mtidWorkforceKindName[pkMiddle] := TRegMultiString.Create( 'mtidWorkforceKindNameMiddle', 'Professionals' );
      mtidWorkforceKindName[pkLow]    := TRegMultiString.Create( 'mtidWorkforceKindNameLow', 'Workers' );

      mtidBlockPrestigePts         := TRegMultiString.Create( 'mtidBlockPrestigePts', '%d prestige points.' );
      mtidStoppedBy                := TRegMultiString.Create( 'mtidStoppedBy', 'Stopped by %s.' );
      mtidStoppedNoCnxs            := TRegMultiString.Create( 'mtidStoppedNoCnxs', 'Stopped: needs connections.' );
      mtidStoppedMoney             := TRegMultiString.Create( 'mtidStoppedMoney', 'Stopped: needs money.' );
      mtidCnxHired                 := TRegMultiString.Create( 'mtidCnxHired', 'hired' );
      mtidRejCompPol               := TRegMultiString.Create( 'mtidRejCompPol', 'rejected by company policy' );
      mtidAlreadyHired             := TRegMultiString.Create( 'mtidAlreadyHired', 'was already hired' );
      mtidCnxNotAllowed            := TRegMultiString.Create( 'mtidCnxNotAllowed', 'connection not allowed' );
      mtidCnxLimited               := TRegMultiString.Create( 'mtidCnxLimited', 'cannot have more than %d connections' );
      mtidCnxRepInputHead          := TRegMultiString.Create( 'mtidCnxRepInputHead', 'Supplies' );
      mtidCnxRepOutputHead         := TRegMultiString.Create( 'mtidCnxRepOutputHead', 'Clients' );
      mtidCnxHeader                := TRegMultiString.Create( 'mtidCnxHeader', 'Connection to' );
      mtidMsgLoan                  := TRegMultiString.Create( 'mtidMsgLoan', '%s borrowed %s from the %s.' );
      mtidMayorTitle               := TRegMultiString.Create( 'mtidMayorTitle', 'Mayor of %s' );
      mtidMayorEmail               := TRegMultiString.Create( 'mtidMayorEmail', 'mayor' );
      mtidMsgMainResearchCompleted := TRegMultiString.Create( 'mtidMsgMainResearchCompleted', 'Research "%s" completed. Check for new items in your Build page.' );
      mtidMsgResearchCompleted     := TRegMultiString.Create( 'mtidMsgResearchCompleted', 'Research "%s" completed.' );
      mtidMsgMainResearchSold      := TRegMultiString.Create( 'mtidMsgMainResearchSold', 'Research sold. Some items were disabled in your Build page.' );
      mtidMsgResearchSold          := TRegMultiString.Create( 'mtidMsgResearchSold', 'Research sold.' );
      mtidMsgMoneyTransfer         := TRegMultiString.Create( 'mtidMsgMoneyTransfer', '%s successfully transferred.' );
      mtidMsgPolicySetToEnemy      := TRegMultiString.Create( 'mtidMsgPolicySetToEnemy', '%s looks forward to the failure of %s.' );
      mtidMsgPolicySetToNeutral    := TRegMultiString.Create( 'mtidMsgPolicySetToNeutral', '%s becomes neutral to %s.' );
      mtidMsgPolicySetToAlly       := TRegMultiString.Create( 'mtidMsgPolicySetToAlly', '%s vowed loyalty to %s.' );
      mtidMsgWantLevelUpgrade      := TRegMultiString.Create( 'mtidMsgWantLevelUpgrade', '%s aspires to achieve level %s.' );
      mtidMsgLevelAdvanced         := TRegMultiString.Create( 'mtidMsgLevelAdvanced', '%s advanced to level %s.' );
      mtidMsgLevelAdvFailed        := TRegMultiString.Create( 'mtidMsgLevelAdvFailed', '%s failed to advance to level %s.' );
      mtidMsgBankruptDanger        := TRegMultiString.Create( 'mtidMsgBankruptDanger', '%s is under bankruptcy scrutiny.' );
      mtidMsgHappyNewYear          := TRegMultiString.Create( 'mtidMsgHappyNewYear', 'HAPPY NEW YEAR!' );
      mtidMsgLevelLost             := TRegMultiString.Create( 'mtidMsgLevelLost', '%s lost level %s.' );
    end;

initialization

  IsMultiThread := true;
  InitMLS;

end.


