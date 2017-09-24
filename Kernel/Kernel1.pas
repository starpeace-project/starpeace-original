unit Kernel;

interface

  uses
    Windows, Classes, MetaInstances, Collection, Plotter, Land, ClassStorageInt, CacheAgent,
    Surfaces, BackupInterfaces, Protocol, SyncObjs, Persistent, MailServerInterfaces,
    Circuits, CacheCommon, Accounts, TransportInterfaces, VisualClassManager, Taxes,
    MapStringToObject, Tasks, Inventions;

  const
    NoPos = -1;

  const
    TimeUnits = 10;
    YearZero  = 2000;
    MaxLoan   = 200*1000*1000;
     
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
    tidTownParameter_InputDemand       = 'InputDemand';
    tidTownParameter_InputCapacity     = 'InputCapacity';
    tidTownParameter_OutputValue       = 'OutputValue';
    tidTownParameter_OutputCapacity    = 'OutputCapacity';

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

  const
    facNoTrouble        = $00;
    facNeedsConnection  = $01;
    facNeedsBudget      = $02;
    facStoppedByTycoon  = $04;
    facInsuficientInput = $08;
    facStoppedByAdmin   = $10;
    facNeedsWorkForce   = $20;
    facNeedCompSupport  = $40;

    facCriticalTrouble = facNeedsBudget or facStoppedByTycoon; // or facNeedsConnection;

  const
    InitialBudget      = 100*1000*1000; // $100,000,000
    MayorInitialBudget = 500*1000*1000;

  const
    tcnDescSeparator = #10#13;

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

  const
    qIlimited   = $7FFFFFFF;
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

  type
    TPeopleKind     = (pkHigh, pkMiddle, pkLow);
    TPeopleArray    = array[TPeopleKind] of TFluidData;
    TPeopleInArray  = array[TPeopleKind] of TPushInputData;
    TPeopleOutArray = array[TPeopleKind] of TOutputData;

  const
    PeopleKindPrefix  : array[TPeopleKind] of string = ('hi', 'mid', 'lo');
    PeopleKindName    : array[TPeopleKind] of string = ('High class', 'Middle class', 'Low class');
    WorkForceKindName : array[TPeopleKind] of string = ('Executives', 'Professionals', 'Workers');

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

    TCompanyResearch  = class;
    TCompanyDirection = class;
    TTownParameter    = class;

    TBank          = class;
    TCluster       = class;
    TMoneyDealer   = class;
    TCompany       = class;
    TTown          = class;
    TTycoon        = class;
    TTycoonContext = class;

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
        function dt                : TVirtDateAbs;
      end;

    IMapRefresh =
      interface
        procedure RefreshArea( x, y, dx, dy : integer );
        procedure RefeshFacility( Facility : TFacility; FacilityChange : TFacilityChange );
      end;

    IWorldLocator =
      interface
        function  GetWorldName : string;
        function  FacilityAt( x, y : integer ) : TFacility;
        function  GetTycoonByName( name : string ) : TTycoon;
        function  GetTycoonCollection : TLockableCollection;
        function  GetMessagesPath : string;
        function  GetWorldURL : string;
        function  GetMainBank : TBank;
        procedure SendNotification( TycoonId : integer; Kind : integer; Title, Body : string; Options : integer );
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
          fTechnology  : string;
          fSuperType   : string;
          fRole        : TFacilityRole;
        published
          property ClusterName : string        read fClusterName write fClusterName;
          property Name        : string        read fName        write fName;
          property Technology  : string        read fTechnology  write fTechnology;
          property SuperType   : string        read fSuperType   write fSuperType;
          property Role        : TFacilityRole read fRole        write fRole;
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

    TMetaFacilityOption  = (mfcInTown, mfcGenerateName, mfcShowCompanyInText, mfcShowProfitInText);
    TMetaFacilityOptions = set of TMetaFacilityOption;

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
          fTechnology     : TInventionNumId;
          fZoneType       : TZoneType;
          fFacId          : integer;
          fMinistryId     : TMinistryId;
          fPluralName     : string;
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
          property EvlStages      : TCollection          read fEvlStages;
          property FacilityClass  : CFacility            read fFacilityClass;
          property FacilityKind   : string               read GetFacilityKind write SetFacilityKind;
          property Kind           : TFacilityKind        read fFacilityKind;
          property TypicalStage   : TEvlStage            read fTypicalStage   write fTypicalStage;
          property Level          : integer              read fLevel          write fLevel;
          property XSize          : integer              read fXSize          write fXSize;
          property YSize          : integer              read fYSize          write fYSize;
          property ClusterName    : string               read GetClusterName  write SetClusterName;
          property TechnologyKind : string               read fTechnologyKind write fTechnologyKind;
          property Technology     : TInventionNumId      read fTechnology     write fTechnology;
          property Options        : TMetaFacilityOptions read fOptions        write fOptions;
          property ZoneType       : TZoneType            read fZoneType;
          property VisualClass    : TVisualClassId       read fVisualClass;
          property Price          : TMoney               read EstimatePrice;
          property FacId          : integer              read fFacId          write fFacId;
          property MinistryId     : TMinistryId         read fMinistryId    write fMinistryId;
          property PluralName     : string               read fPluralName     write fPluralName;
        public
          function Instantiate : TFacility; virtual;
        private
          procedure EvlStagesModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
        public
          procedure Register( ClassFamily : TClassFamilyId );
          procedure RequiresInvention(Invention : TInvention);
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
          fBlockClass     : CBlock;
          fMetaInputs     : TNotifiedCollection;
          fMetaOutputs    : TNotifiedCollection;
          fBeauty         : TSurfaceValue;
          fBeautyStrength : TSurfaceValue;
          fVisualStages   : integer;
          fSupplyAccount  : TAccountId;
          fProdAccount    : TAccountId;
          fDesc           : string;
          fPrestige       : TPrestige;
          fInventions     : TCollection;
        published
          property BlockClass     : CBlock              read fBlockClass;
          property MetaInputs     : TNotifiedCollection read fMetaInputs;
          property MetaOutputs    : TNotifiedCollection read fMetaOutputs;
          property Beauty         : TSurfaceValue       read fBeauty         write fBeauty;
          property BeautyStrength : TSurfaceValue       read fBeautyStrength write fBeautyStrength;
          property VisualStages   : integer             read fVisualStages   write fVisualStages;
          property Desc           : string              read fDesc           write fDesc;
          property Prestige       : TPrestige           read fPrestige       write fPrestige;
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
        public
          function Instantiate( aFacility : TFacility ) : TBlock; virtual;
        private
          procedure OnMetaInputsModified(Operation : TCollectionOperation; Index : integer; Item : TObject);
          procedure OnMetaOutputsModified(Operation : TCollectionOperation; Index : integer; Item : TObject);
        protected
          function ModifyStageStack( Stage : TMetaBlock ) : boolean; virtual;
        public
          procedure Register( ClassFamily : TClassFamilyId );
          procedure DeclareInvention(Invention : TInvention); virtual;
        public
          property Invention : TCollection read fInventions;
      end;

    TMetaFluidOption  = (mfTradeable, mfImportable, mfStorable, mfConstruction, mfAdvertisement, mfWorkForce, mfPeople, mfService);
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
                              aMarketPrice : TMoney );
        private
          fName        : string;
          fDescription : string;
          fUnitName    : string;
          fFluidName   : string;
          fFluidFact   : TFluidValue;
          fTransCost   : TMoney;
          fMarketPrice : TMoney;
          fWeight      : TWeight;
          fOptions     : TMetaFluidOptions;
        published
          property Name        : string  read fName;
          property Description : string  read fDescription;
          property FluidName   : string  read fFluidName;
          property UnitName    : string  read fUnitName;
          property TransCost   : TMoney  read fTransCost;
          property Weight      : TWeight read fWeight;
          property MarketPrice : TMoney  read fMarketPrice;
          property Options     : TMetaFluidOptions read fOptions write fOptions;
        public
          function FormatValue   ( Value : TFluidValue ) : string;
          function FormatValueAbs( Value : TFluidValue ) : string;
          function ConvertToUnits( Value : TFluidValue ) : TFluidValue;
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
          property MinFluid    : PInputData      read GetMinFluid;
          property MaxFluid    : PInputData      read GetMaxFluid;
          property DefFluid    : PInputData      read GetDefFluid;
          property MaxCapacity : TFluidValue     read fMaxCapacity;
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
          fCompanyResearch : TCompanyResearch;
          fCompanyDir      : TCompanyDirection;
          fToBeDemolished  : byte;
          fCost            : TMoney;
          fNetProfit       : TMoney;
          fDeleted         : boolean;
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
        protected
          function GetVisualClassId : TVisualClassId; virtual;
          function ComputeROI : integer; virtual;
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
          property NetProfit       : TMoney         read fNetProfit;
          property ROI             : integer        read ComputeROI;
          property CurrStage       : integer        read fCurrStage;
        public
          property CompanyResearch : TCompanyResearch  read fCompanyResearch;
          property CompanyDir      : TCompanyDirection read fCompanyDir;
          property Deleted         : boolean           read fDeleted write fDeleted;
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
          function  RDOGetConnectionReport : OleVariant;
          procedure RDOConnectToTycoon     ( TycoonId, FacTypes : integer; SetAsDefault : wordbool );
          procedure RDODisconnectFromTycoon( TycoonId : integer; RemoveAsDefault : wordbool );
          procedure RDOConnectToTradeCenter;
          procedure RDODisconnectFromTradeCenter;
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
        private
          procedure BlockModified( Modification : TBlockModification );
          function  NewBlock : TBlock;
        protected
          procedure GenMoney( Money : TMoney; Reason : TMoneyReason );
        public
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; virtual;
          property StatusText[kind : TStatusKind; ToTycoon : TTycoon] : string read GetStatusText;
        public
          procedure Cache;
          procedure UnCache;
          procedure UpdateCache;
          function  GetCacheName : string;
        public
          procedure CopySettingsFrom( Facility : TFacility ); virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    PGateArray        = ^TGateArray;
    TGateArray        = array[any] of TGate;
    TEvaluationResult = (evrNormal, evrEvolve, evrError);
    TAreaMode         = (amdExcludeBlock, amdIncludeBlock);

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
          function GetDt : TVirtDateAbs;
        public
          property dt : TVirtDateAbs read GetDt;
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
          procedure RefreshVisualClassInfo;
        private
          fVisualClass : TVisualClassId;
        protected
          property vVisualClassId : TVisualClassId read fVisualClass write fVisualClass;
        public
          property VisualClassId : TVisualClassId read fVisualClass;
        public
          procedure StoreLinksToCache( Cache : TObjectCache ); virtual;
          procedure StoreToCache( Cache : TObjectCache ); virtual;
          procedure RemoveFromCache;
        protected
          procedure CopySettingsFrom( Block : TBlock ); virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        protected
          procedure BlockModified( Modification : TBlockModification );
          procedure BlockLoaded; virtual;
        public
          procedure BlockGenMoney( Money : TMoney; Reason : TMoneyReason );
        protected
          function  GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; virtual;
          function  GetArea( Ratio : integer; mode : TAreaMode ) : TRect;
          procedure FacNameChanged; virtual;
        private
          fOffsetRef : record end;
        protected
          function Offset( var Field ) : TOffset;
        private
          procedure CleanExtraInfo;
      end;

    TConnectResult    = (cnxValid, cnxRejected, cnxDuplicated, cnxForbiden);
    TTransportPayMode = (tnpPositive, tnpPositiveShare, tnpNull, tnpNegativeShare, tnpNegative); 

    PExtraConnectionInfo = ^TExtraConnectionInfo;
    TExtraConnectionInfo =
      record
        LastFluid : TFluidValue;
        YearValue : TFluidValue;
        YearCost  : TMoney;
        OverPrice : TPercent;
        Connected : boolean;
        Priority  : TPercent;
      end;

    TGate =
      class( TPersistent )
        protected
          constructor Create( aMetaGate : TMetaGate; aBlock : TBlock ); virtual;
        public
          destructor  Destroy; override;
        private
          fFluidData   : PFluidData;
          fBlock       : TBlock;
          fConnections : TCollection;
          fOptions     : TGateAutoconnectOptions;
        private
          function GetConnectionCount : integer;
        public
          property FluidData       : PFluidData read fFluidData;
          property Block           : TBlock     read fBlock;
          property ConnectionCount : integer    read GetConnectionCount;
          property Options         : TGateAutoconnectOptions read fOptions write fOptions;
        protected
          property vConnections : TCollection read fConnections;
        protected
          function  GetLastCost : TMoney; virtual;
          procedure SetLastCost( aLastCost : TMoney ); virtual;
        public
          property LastCost : TMoney read GetLastCost;
        protected
          property vLastCost : TMoney read GetLastCost write SetLastCost;
        protected
          procedure SetFluidData( aFluidData : PFluidData ); virtual;
          procedure InsertConnection( Connection : TGate ); virtual;
          procedure DeleteConnection( Connection : TGate ); virtual;
          procedure ConnectionChanged( Connection : TGate ); virtual;
          function  GetConnectionPrecedence( Connection : TGate ) : integer; virtual;
          function  ConnectionAllowed( Connection : TGate ) : TConnectResult; virtual;
          function  GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo; virtual;
          procedure CheckConnections; virtual;
        public
          property ExtraConnectionInfo[index : integer] : PExtraConnectionInfo read GetExtraConnectionInfo;
        public
          function  ConnectTo( Gate : TGate ) : TConnectResult;
          procedure DisconnectFrom( Gate : TGate );
          procedure DisconnectAll;
          procedure SortConnections; virtual;
        protected
          class function BestCollection( aSize : integer ) : TCollection; virtual;
        protected
          procedure AutoConnect( loaded : boolean ); virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        private
          procedure HookFluidData( aMetaGate : TMetaGate );
          function  ConnectionAllowedByPolicy( Gate : TGate ) : boolean;
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
          function GetCapacity       : TFluidValue;
        public
          property MetaInput      : TMetaInput  read fMetaInput;
          property ActualMaxFluid : PInputData  read GetActualMaxFluid;
          property MaxCapacity    : TFluidValue read fMaxCapacity write fMaxCapacity;
          property Capacity       : TFluidValue read GetCapacity;
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
        protected
          function GetConnectionPrecedence( Connection : TGate ) : integer; override;
        published
          procedure RDOSetInputFluidPerc(Perc : integer);
      end;

    TOutput =
      class( TGate )
        protected
          constructor Create( aMetaGate : TMetaGate; aBlock : TBlock ); override;
        private
          fMetaOutput : TMetaOutput;
          fPricePerc  : TPercent;
        private
          function  GetPrice : TMoney;
          procedure SetPricePerc( aPricePerc : TPercent );
        published
          property MetaOutput : TMetaOutput read fMetaOutput;
          property Price      : TMoney      read GetPrice;
          property PricePerc  : TPercent    read fPricePerc write SetPricePerc;
        protected
          function GetDemand : TFluidValue; virtual;
        public
          property Demand : TFluidValue read GetDemand;
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
        protected
          function  GetConnectionPrecedence( Connection : TGate ) : integer; override;
      end;

    TPushInput =
      class( TInput )
        protected
          constructor Create( aMetaGate : TMetaGate; aBlock : TBlock ); override;
        protected
          procedure Collect; override;
        private
          fSkipped : boolean;
      end;

    TPullInput =
      class( TInput )
        protected
          procedure Collect;      override;
          procedure CollectExtra; override;
        private
          procedure DoCollect( CollectExtra : boolean );
        private
          fLastCost : TMoney;
        protected
          function  GetLastCost : TMoney; override;
          procedure SetLastCost( aLastCost : TMoney ); override;
        protected
          procedure InsertConnection( Connection : TGate ); override;
          function  GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo; override;
          procedure CheckConnections; override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        private
          fDemand   : TTownParameter;
          fValue    : TTownParameter;
          fCapacity : TTownParameter;
        protected
          procedure AutoConnect( loaded : boolean ); override;
      end;

    TPushOutput =
      class( TOutput )
        protected
          procedure Spread; override;
      end;

    TSlice =
      class
        public
          val   : TFluidValue;
          taken : boolean;
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
        private
          fQAuxBuff : TFluidValue;
          fDemand   : TFluidValue;
        protected
          function GetDemand : TFluidValue; override;
        protected
          procedure ValuePulled ( Value : TFluidValue; Input : TInput ); virtual;
          procedure ReportDemand( Value : TFluidValue; Input : TInput ); virtual;
          procedure Slice       ( Value : TFluidValue );                 virtual;
          function  GetSliceFor ( Input : TInput ) : TFluidValue;        virtual;
        private
          procedure ComputePriorities;
        protected
          procedure ConnectionChanged( Connection : TGate ); override;
          procedure InsertConnection( Connection : TGate ); override;
          function  GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo; override;
          procedure CheckConnections; override;
        protected
          class function BestCollection( aSize : integer ) : TCollection; override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        private
          fValue    : TTownParameter;
          fCapacity : TTownParameter;
        protected
          procedure AutoConnect( loaded : boolean ); override;
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
          property Interest : TPercent     read fInterest;
          property Amount   : TMoney       read fAmount;
          property Slice    : TMoney       read fSlice;
          property Term     : integer      read fTerm;
          property Debtor   : TMoneyDealer read fDebtor;
          property Bank     : TBank        read fBank;
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
        public
          property Owner    : TMoneyDealer read fOwner;
          property Loans    : TCollection  read fLoans;
          property Name     : string       read fName     write fName;
          property Interest : TPercent     read fInterest write fInterest;
          property Term     : integer      read fTerm     write fTerm;
          property Timer    : ITimer       read fTimer    write fTimer;
        public
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); virtual;
          function  EstimateLoan( Client : TMoneyDealer ) : TMoney;
          function  AskLoan( Client : TMoneyDealer; Amount : TMoney ) : TBankRequestResult;
          procedure DealerDeleted( Dealer : TMoneyDealer );
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
        protected
          function  GetBudget : TMoney;            virtual;
          procedure SetBudget( aBudget : TMoney ); virtual;
        published
          property Budget : TMoney read GetBudget write SetBudget;
        public
          property Accounts  : TAccountingSystem   read fAccounts;
          property Loans     : TLockableCollection read fLoans;
        published
          property NetProfit : TMoney read fNetProfit;
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
        public
          property BankLoanPerc : TPercent read GetBankLoanPerc write SetBankLoanPerc;
        protected
          function BankLoanLimit : TMoney;
      end;

    TMetaTownParameter =
      class( TMetaInstance )
        public
          constructor Create( anId, aName : string; anAutoClear : boolean );
        private
          fName      : string;
          fAutoClear : boolean;
        public
          property Name      : string read fName;
          property AutoClear : boolean read fAutoClear;
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
        private
          function GetOrdinance( NumId : TOrdinanceNumId ) : boolean;
          function GetParameter( Id : string ) : TTownParameter;
        public
          property Ordinance[NumId : TOrdinanceNumId] : boolean read GetOrdinance;
          property Parameters[Id : string] : TTownParameter read GetParameter;
        public
          procedure UpdateParameters; virtual;
          function  GetContextStatusStr : string; virtual;
        published
          procedure SetOrdinance( Id : string );
          procedure DelOrdinance( Id : string );
        private
          fTaxes       : TLockableCollection;
          fMinSalaries : array[TPeopleKind] of TPercent;
        private
          function  GetTaxes( TaxId : string ) : TTax;
          function  GetMinSalary( Kind : TPeopleKind ) : TPercent;
          procedure SetMinSalary( Kind : TPeopleKind; Value : TPercent );
        public
          property Taxes[TaxId : string] : TTax read GetTaxes;
          property AllTaxes : TLockableCollection read fTaxes;
          property MinSalary[kind : TPeopleKind] : TPercent read GetMinSalary write SetMinSalary;
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
      end;

    TCluster =
      class( TMetaInstance )
        private
          fCompany      : TCompany;
          fCompanyClass : CCompany;
        public
          property Company      : TCompany read fCompany      write fCompany;
          property CompanyClass : CCompany read fCompanyClass write fCompanyClass;
        public
          function NameNewspaper( TownName : string ) : string; virtual;
      end;

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
      end;

    TCompanyDirection =
      class( TLockable )
        public
          constructor Create( anId : string );
        private
          fId       : string;
          fStrength : single;
          fDemand   : single;
          fSupport  : single;
        public
          property Id       : string read fId;
          property Strength : single read fStrength write fStrength;
          property Demand   : single read fDemand   write fDemand;
          property Support  : single read fSupport;
        public
          procedure Update;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
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
          fResearchs         : TLockableCollection;
          fDirections        : TLockableCollection;
          fProjects          : TLockableCollection;
          fPrivated          : boolean;
          fMetaFacilityList  : TCompanyMetaFacilityList;
        protected
          function GetBudget : TMoney; override;
        private
          function GetTechnologyDescriptor : string;
        published
          property Owner             : TTycoon                  read fOwner   write fOwner;
          property Cluster           : TCluster                 read fCluster write fCluster;
          property Name              : string                   read fName    write fName;
          property Created           : TVirtDate                read fCreated write fCreated;
          property Id                : TCompanyId               read fId;
          property AutoConnectLevels : TMetaInputLevels         read fAutoConnectLevels write fAutoConnectLevels;
          property Facilities        : TLockableCollection      read fFacilities        write fFacilities;
          property Researchs         : TLockableCollection      read fResearchs;
          property Projects          : TLockableCollection      read fProjects;
          property Privated          : boolean                  read fPrivated;
          property MetaFacilityList  : TCompanyMetaFacilityList read fMetaFacilityList;
          property TechDescriptor    : string                   read GetTechnologyDescriptor;
        public
          procedure FacilityCreated  ( Facility : TFacility );
          procedure FacilityDestroyed( Facility : TFacility );
        public
          procedure UpdateParameters; virtual;
        private
          function GetHasInvention( kind : string; NumId : TInventionNumId ) : boolean;
          function GetResearch( kind : string ) : TCompanyResearch;
          function GetProject( name : string ) : TProject;
          function GetDirection( id : string ) : TCompanyDirection;
        public
          property HasInvention[kind : string; NumId : TInventionNumId] : boolean read GetHasInvention;
          property Research[kind : string] : TCompanyResearch read GetResearch;
          property Directions[id : string] : TCompanyDirection read GetDirection;
          property Project[name : string] : TProject read GetProject;
        public
          procedure DeclareInvention( kind, Id : string );
        public
          procedure GenMoney( Money : TMoney; Reason : TMoneyReason ); override;
        public
          procedure Loaded; override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          function GetCacheName : string;
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
          fHireOnlyWarehouses : boolean;
        public
          property MetaFluidId        : string              read fMetaFluidId;
          property Connections        : TLockableCollection read fConnections;
          property HireTradeCenter    : boolean             read fHireTradeCenter;
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
          fId     : string;
          fKind   : integer;
          fTycoon : TTycoon;
        protected
          function GetDesc       : string;    virtual;
          function GetImportance : integer;   virtual;
          function GetPrestige   : TPrestige; virtual;
        public
          property Id         : string    read fId;
          property Kind       : integer   read fKind;
          property Tycoon     : TTycoon   read fTycoon;
          property Desc       : string    read GetDesc;
          property Importance : integer   read GetImportance;
          property Prestige   : TPrestige read GetPrestige;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

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
          fFocus           : TPoint;
          fAutoConnections : TLockableCollection;
          fPolicies        : TLockableCollection;
          fWorldLocator    : IWorldLocator;
          fMailServer      : IMailServer;
          fTimer           : ITimer;
          fPrestige        : TPrestige;
          fFacPrestige     : TPrestige;
          fCurrFacPrestige : TPrestige;
          fCurriculum      : TLockableCollection;
          fCookies         : TStringList;
        protected
          function  GetBudget : TMoney; override;
          procedure SetBudget( aBudget : TMoney ); override;
        published
          function RDOGetBudget : OleVariant;
        private
          function GetFocusX : integer;
          function GetFocusY : integer;
        private
          function  GetMasterRole : TTycoon;
          function  GetRealName : string;
          function  GetAllCompaniesCount : integer;
          function  GetAllCompanies( index : integer ) : TCompany;
          function  GetCookie( Id : string ) : string;
          procedure SetCookie( Id, Value : string );
        published
          property Id           : TTycoonId           read fId;
          property Options      : TTycoonOptions      read fOptions   write fOptions;
          property SuperRole    : TTycoon             read fSuperRole write fSuperRole;
          property MasterRole   : TTycoon             read GetMasterRole;
          property FocusX       : integer             read GetFocusX;
          property FocusY       : integer             read GetFocusY;
          property Companies    : TLockableCollection read fCompanies;
          property Roles        : TLockableCollection read fRoles;
          property Name         : string              read fName          write fName;
          property RealName     : string              read GetRealName;
          property Password     : string              read fPassword      write fPassword;
          property Ranking      : integer             read fRanking       write fRanking;
          property FailureLevel : integer             read fFailureLevel;
        public
          property Prestige        : TPrestige read fPrestige;
          property FacPrestige     : TPrestige read fFacPrestige;
          property CurrFacPrestige : TPrestige read fCurrFacPrestige  write fCurrFacPrestige;
        public
          property AllCompanies[index : integer] : TCompany read GetAllCompanies;
          property AllCompaniesCount : integer              read GetAllCompaniesCount;
        public
          property Cookie[id : string] : string read GetCookie write SetCookie;
        private
          function GetSecurityId : TSecurityId;
        public
          property AutoConnections : TLockableCollection read fAutoConnections;
          property Curriculum      : TLockableCollection read fCurriculum;
          property WorldLocator    : IWorldLocator       read fWorldLocator write fWorldLocator;
          property MailServer      : IMailServer         read fMailServer   write fMailServer;
          property Timer           : ITimer              read fTimer        write fTimer;
          property SecurityId      : TSecurityId         read GetSecurityId;
        private
          fTimesAwaken : integer;
        public
          procedure Awake;
          procedure Sleep;
          function  IsOnline : boolean;
          procedure SendNotification( Kind : integer; Title, Body : string; Options : integer ); virtual;
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
          function  ContainsRole( RoleId : TTycoonId ) : boolean;
        public
          procedure AddItemToCurriculum( Item : TCurriculumItem );
        published
          function RDOAskLoan( AmountStr : widestring ) : OleVariant;
          function RDOPayLoan( AmountStr : widestring ) : OleVariant;
          function RDOSendMoney( ToTycoon, Reason, AmountStr : widestring ) : OleVariant;
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
        public
          procedure GenMoney( Money : TMoney; Reason : TMoneyReason ); override;
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); override;
        public
          procedure FacilityDestroyed  ( Facility : TFacility );
          procedure NewOwnedFacility   ( Facility : TFacility );
          procedure RegisterSupplier   ( Facility : TFacility );
          procedure UnregisterSupplier ( Facility : TFacility );
          procedure AutoConnectFacility( Facility : TFacility );
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure Loaded; override;
          procedure UpdateParameters; virtual;
        public
          function GetCacheName : string;
        private
          procedure InstantiateTutorial;
        private
          fTutorial    : TTask;
          fTaskContext : TTycoonContext;
        public
          property Tutorial : TTask read fTutorial;
        published
          procedure RDOActivateTutorial(Value : LongBool);
          function  RDOActiveTutorial(useless : integer) : OleVariant;
        protected
          procedure DefaultHandler(var Message); override;
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
          function  QueryInterface(const IID: TGUID; out Obj): Integer; stdcall;
          function  _AddRef: Integer; stdcall;
          function  _Release: Integer; stdcall;
          function  getContext(id : integer) : TObject;
          procedure setContext(id : integer; Obj : TObject);
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;


    {$M-}

  // Misc routines

  function GetInventionId( numId : TInventionNumId; kind : string ) : string;

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

implementation

  uses
    ClassStorage, Construction, ModelServerCache, MathUtils, StrUtils, BasicAccounts, BasicTaxes,
    SysUtils, Population, SimHints, SpecialChars, LandSurfaces, VisualClassesData, BasicCurriculum,
    Tutorial, Politics, TownPolitics, Logs;

  const
    defCluster = 'General';

  const
    tidMailMsgURL_MoneySentNotification = 'MoneySendNotification.asp';

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
    begin
      Writer.WriteSingle( name + '.Q', absFluidData.Q );
      Writer.WriteInteger( name + '.K', absFluidData.K );
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


  // TFacilityKind

  constructor TFacilityKind.Create( anId : string );
    begin
      inherited Create( anId );
      Cacheable := true;
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
      if Name <> ''
        then fPluralName := Name + 's';
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
      fTechnology     := aMetaFacility.fTechnology;
      fZoneType       := aMetaFacility.fZoneType;
      fFacId          := aMetaFacility.fFacId;
      fMinistryId     := aMetaFacility.fMinistryId;
      fPluralName     := aMetaFacility.fPluralName;
    end;

  destructor TMetaFacility.Destroy;
    begin
      fEvlStages.Free;
      inherited;
    end;

  function TMetaFacility.EstimatePrice : TMoney;
    begin
      if (EvlStages.Count > 1) and (TEvlStage(EvlStages[0]).MetaBlock.ClassName = TMetaBlockUnderConstruction.ClassName)
        then result := TMetaBlockUnderConstruction(TEvlStage(EvlStages[0]).MetaBlock).EstimatedPrice
        else result := 0;
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
      fFacilityKind := TFacilityKind(TheClassStorage.ClassById[tidClassFamily_FacilityKinds, aName]);
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
          end;
      end;

    function GetTechReq : string;
      var
        TechId : TClassId;
        Tech   : TInvention;
      begin
        try
          if TechnologyKind <> ''
            then
              begin
                TechId := GetInventionId( Technology, TechnologyKind );
                Tech   := TInvention(TheClassStorage.ClassById[tidClassFamily_Inventions, TechId]);
                result := SimHints.GetHintText( hidTechRequired, [Tech.Name, Tech.Resp] );
              end
            else result := '';
        except
          result := '';
        end;
      end;

    begin
      ApplyStageModifiers;
      Desc := TypicalStage.MetaBlock.Desc + Desc;
      //Reqs := GetTechReq;
      inherited Register( ClassFamily );
    end;

  procedure TMetaFacility.RequiresInvention(Invention : TInvention);
    begin
      // >>
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
      fInventions := TCollection.Create(0, rkUse);
    end;

  destructor TMetaBlock.Destroy;
    begin
      fMetaInputs.Free;
      fMetaOutputs.Free;
      fInventions.Free;
      inherited;
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

  procedure TMetaBlock.Register( ClassFamily : TClassFamilyId );
    begin
      if round(Prestige) > 1
        then
          begin
            if Desc <> ''
              then Desc := Desc + ' ';
            Desc := Desc + IntToStr(round(Prestige)) + ' prestige points.';
          end;
      inherited Register( ClassFamily );
    end;

  procedure TMetaBlock.DeclareInvention(Invention : TInvention);
    begin
      fInventions.Insert(Invention);
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
                                 aMarketPrice : TMoney );
    begin
      inherited Create( anId );
      fName        := aName;
      fUnitName    := aUnitName;
      fFluidName   := aFluidName;
      fFluidFact   := aFluidFact;
      fTransCost   := aTransCost;
      fWeight      := aWeight;
      fMarketPrice := aMarketPrice;
    end;

  function TMetaFluid.FormatValue( Value : TFluidValue ) : string;
    begin
      try
        result := Format( '%.0n', [int(Value*fFluidFact)] ) + ' ' + fFluidName;
      except
        result := 'Unknown';
      end;
    end;

  function TMetaFluid.FormatValueAbs( Value : TFluidValue ) : string;
    begin
      try
        result := Format( '%.0n', [int(Value*fFluidFact)] ) + ' ' + fUnitName;
      except
        result := 'Unknown';
      end;
    end;

  function TMetaFluid.ConvertToUnits( Value : TFluidValue ) : TFluidValue;
    begin
      result := Value*fFluidFact;
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
      inherited;
    end;

  procedure TLockable.Lock;
    begin
      fLock.Enter;
    end;

  procedure TLockable.Unlock;
    begin
      fLock.Leave;
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
      Logs.Log( 'Survival', 'Start Destroying Facility... ' );
      Logs.Log( 'Survival', 'Destroying Facility ' + Name );
      try
        fCurrBlock.Free;
        Logs.Log( 'Survival', 'OK!' );
      except
        Logs.Log( 'Survival', 'Error destroying facility ' + Name + '!' );
      end;
      inherited;
      Logs.Log( 'Survival', 'Destroy OK!' );
    end;

  function TFacility.GetName : string;
    begin
      Lock;
      try
        if fName <> ''
          then result := fName
          else result := MetaFacility.Name;
      finally
        Unlock;
      end;
    end;

  procedure TFacility.SetName( aName : string );
    begin
      Lock;
      try
        if fName <> aName
          then
            begin
              UnCache;
              fName := aName;
              Cache;
              fCurrBlock.FacNameChanged;
            end;
      finally
        Unlock;
      end;
    end;
    
  procedure TFacility.SetCompany( aCompany : TCompany );
    begin
      fCompany := aCompany;
      if fCompany <> nil
        then
          begin
            if mfcGenerateName in MetaFacility.Options
              then fName := MetaFacility.Name + ' ' + IntToStr(fCompany.MetaFacilityList.GetNextFacilityNumber(MetaFacility));
            if MetaFacility.TechnologyKind <> ''
              then
                begin
                  fCompanyResearch := fCompany.Research[MetaFacility.TechnologyKind];
                  fCompanyDir      := fCompany.Directions[MetaFacility.TechnologyKind];
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
        Cache;
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
      Lock;
      try
        if Value <> Stopped
          then
            case Value of
              true:
                begin
                  fTrouble := fTrouble or facStoppedByTycoon;
                  fCurrBlock.Stop;
                end;
              false:
                begin
                  fCurrBlock.Resume;
                  fTrouble := fTrouble and not facStoppedByTycoon;
                end;
            end;
        if not fDeleted
          then UpdateCache;
      finally
        Unlock;
      end;
    end;

  function TFacility.GetAge : TVirtDateAbs;
    begin
      try
        result := Town.Timer.GetVirtualTimeAbs - fCreationDate;
      except
        result := -1;
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
    begin
      if (NetProfit > 0) and (Age > 0)
        then result := round( (Cost/(NetProfit/Age))/(24*365) )
        else result := -1;
    end;

  procedure TFacility.Simulate;
    begin
      Lock;
      try
        fCurrBlock.Simulate;
        fCurrBlock.RefreshVisualClassInfo;
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
              fMoneyDelta := round(fPeriodMoney - fLastPeriodMoney) div Town.Timer.dt;
            except
              fMoneyDelta := 0;
            end;
        fLastPeriodMoney := fPeriodMoney;
        if Trouble and facStoppedByTycoon <> 0
          then Trouble := facStoppedByTycoon
          else
            begin
              Trouble := facNoTrouble;
              fCurrBlock.CollectInputs;
            end;
        if Budget <= 0
          then ReportTrouble( facStoppedByTycoon );
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

      function GetTaxableAccount : string;
        var
          MA : TMetaAccount;
        begin
          MA := TMetaAccount(TheClassStorage.ClassById[tidClassFamily_Accounts, IntToStr(CurrBlock.MetaBlock.ProdAccount)]);
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

      var
        TaxId : string;
        Tax   : TTax;
      begin
        if (Town <> nil) and (Company <> nil) and (Company.Owner <> nil)
          then
            begin
              TaxId := GetTaxableAccount;
              if TaxId <> ''
                then
                  begin
                    Tax := Town.Taxes[TaxId];
                    if Tax <> nil
                      then Tax.Evaluate( fPeriodMoney, Company.Owner, Town );
                  end;
            end;
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
              ChargeTaxes;
              fPeriodMoney := 0;
            end;
          perMonth :
            if fToBeDemolished > 0
              then
                if fToBeDemolished = 1
                  then Town.ModelFactory.RequestDeletion( self )
                  else dec( fToBeDemolished );
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
        fCurrBlock.BlockLoaded;
      except
      end;
    end;

  function TFacility.ConnectTo( Facility : TFacility ) : string;
    begin
      try
        result := fCurrBlock.ConnectTo( Facility.CurrBlock, true );
        CheckConnections( chkBoth );
        UpdateCache;
      except
        result := '';
      end;
    end;

  procedure TFacility.DisconnectFrom( Facility : TFacility );
    begin
      try
        fCurrBlock.DisconnectFrom( Facility.CurrBlock, true );
        UpdateCache;
      except
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
      ModifyConnection( FluidId, Suppliers, true, true );
    end;

  procedure TFacility.RDOConnectOutput( FluidId, Clients : widestring );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Output connected: ' + FluidId + ' to ' + Clients );
      ModifyConnection( FluidId, Clients, true, false );
    end;

  procedure TFacility.RDODisconnectInput( FluidId, Suppliers : widestring );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Input disconnect: ' + FluidId + ' from ' + Suppliers );
      ModifyConnection( FluidId, Suppliers, false, true );
    end;
                              
  procedure TFacility.RDODisconnectOutput( FluidId, Clients : widestring );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Output disconnect: ' + FluidId + ' from ' + Clients );
      ModifyConnection( FluidId, Clients, false, false );
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
          if Output <> nil
            then Output.PricePerc := Price;
        finally
          Unlock;
        end;
        UpdateCache;
      except
      end;
    end;

  procedure TFacility.RDOSetInputOverPrice( FluidId : widestring; SupplierIdx, OverPrice : integer );
    var
      Input : TInput;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Input overprice set: ' + FluidId + ' to ' + IntToStr(OverPrice) );
      try
        Lock;
        try
          Input := CurrBlock.InputsByName[FluidId];
          if Input <> nil
            then Input.ExtraConnectionInfo[SupplierIdx]^.OverPrice := OverPrice;
        finally
          Unlock;
        end;
        UpdateCache;
      except
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
      end;
      result := report;
    end;

  procedure TFacility.RDOConnectToTycoon( TycoonId, FacTypes : integer; SetAsDefault : wordbool );
    var
      Tycoon  : TTycoon;
      i, j    : integer;
      ki, ko  : integer;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'Connect to Tycoon: ' + IntToStr(TycoonId) );
      try
        Lock;
        try
          Tycoon := TTycoon(pointer(TycoonId));
          for i := 0 to pred(Tycoon.Companies.Count) do
            for j := 0 to pred(TCompany(Tycoon.Companies[i]).fFacilities.Count) do
              with TFacility(TCompany(Tycoon.Companies[i]).fFacilities[j]) do
                begin
                  if (FacTypes and ftpWarehouses <> 0) and (MetaFacility.Kind.Role = rolDistributer) or
                     (FacTypes and ftpFactories <> 0) and (MetaFacility.Kind.Role = rolProducer) or
                     (FacTypes and ftpStores <> 0) and (MetaFacility.Kind.Role = rolBuyer)
                    then
                      begin
                        for ko := 0 to pred(self.CurrBlock.OutputCount) do
                          for ki := 0 to pred(CurrBlock.InputCount) do
                            if (CurrBlock.Inputs[ki].MetaInput.MetaFluid = self.CurrBlock.Outputs[ko].MetaOutput.MetaFluid) and
                               (mfTradeable in CurrBlock.Inputs[ki].MetaInput.MetaFluid.Options)
                              then CurrBlock.Inputs[ki].ConnectTo( self.CurrBlock.Outputs[ko] );
                      end;
                end;
          CheckConnections( chkBoth );
          UpdateCache;
        finally
          Unlock;
        end;
        if SetAsDefault
          then Tycoon.RegisterSupplier( self );
      except
      end;
    end;

  procedure TFacility.RDODisconnectFromTycoon( TycoonId : integer; RemoveAsDefault : wordbool );
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
          UpdateCache;
        finally
          Unlock;
        end;
        if RemoveAsDefault
          then Tycoon.UnregisterSupplier( self );
      except
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
      except
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
      except
      end;
    end;

  procedure TFacility.ModifyConnection( Org, Dest : widestring; Connect, Input : boolean );
    var
      OrgGate  : TGate;
      DestGate : TGate;
      DestFac  : TFacility;
      Gates    : TCollection;
      i        : integer;
    begin
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
                    for i := 0 to pred(Gates.Count) do
                      with TGateDesc(Gates[i]) do
                        begin
                          DestFac := Town.WorldLocator.FacilityAt( x, y );
                          if DestFac <> nil
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
                        end
              end;
        finally
          Unlock;
        end;
        UpdateCache;
      except
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
      if Name <> '' // >>
        then
          begin
            Lock;
            try
              if fFocusCount > 0
                then dec( fFocusCount );
            finally
              Unlock;
            end;
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
    var
      NewBlockMsg : TMsgNewBlock;
    begin
      Lock;
      try
        case Modification of
          bmVisualChange :
            Town.MapRefresh.RefreshArea( xPos, yPos, MetaFacility.xSize, MetaFacility.ySize );
          bmEvolve :
            begin
              fNetProfit := 0;
              UnCache;
              fCurrBlock.Free;
              inc( fCurrStage );
              fCurrBlock := NewBlock;
              fCurrBlock.AutoConnect( false );
              CheckCircuits;
              CheckConnections( chkBoth );
              Cache;
              Town.MapRefresh.RefreshArea( xPos, yPos, MetaFacility.xSize, MetaFacility.ySize );
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

  function TFacility.NewBlock : TBlock;
    begin
      result := TEvlStage(MetaFacility.EvlStages[fCurrStage]).MetaBlock.Instantiate( self );
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
              if (Company <> nil) and Company.Privated and (Company.Owner <> nil) and (mfcShowProfitInText in MetaFacility.Options)
                then result := result + tcnDescSeparator + '(' + FormatMoney( fMoneyDelta ) + '/h)';
            end;
          sttSecondary :
            begin
              if system.pos( MetaFacility.Name, Name ) = 0
                then result := MetaFacility.Name + '.'
                else result := '';
              if (Company <> nil)
                then
                  if Trouble and facStoppedByTycoon <> 0
                    then StatusStr := 'Stopped by ' + Company.Owner.Name + '. '
                    else
                      if Trouble and facNeedsConnection <> 0
                        then StatusStr := 'Stopped: Needs connections. '
                        else
                          if Trouble and facNeedsBudget <> 0
                            then StatusStr := 'Stopped: Needs money. '
                            else StatusStr := '';
              result :=
                result + ' ' +
                StatusStr + ' ' +
                fCurrBlock.GetStatusText( kind, ToTycoon );
              {
              if (Company <> nil) and Company.Privated
                then result := result + '   ' + FormatMoney( fPeriodMoney ) + ' this year. (' + FormatMoney( fMoneyDelta ) + '/h)';
              }
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
                              then result := GetHintText( hidHintsDenied, [Company.Owner.Name] )
                              else result := GetHintText( hidHintsDenied, [Company.Cluster.Id] )
                          else result := GetHintText( hidVisitWebSite, [0] );
                      end;
                end
              else result := GetHintText( hidFacilityWillBeDemolished, [Town.Name, ToBeDemolished, FormatMoney(Cost)] );
        end;
        result := Trim( result );
      finally
        Unlock;
      end
    end;

  procedure TFacility.Cache;
    begin
      Lock;
      try
        CacheObject( self );
      finally
        Unlock;
      end;
    end;

  procedure TFacility.UnCache;
    begin
      Lock; // >> Mysterious Deadlock!!
      try
        CurrBlock.RemoveFromCache;
        UnCacheObject( self );
      finally
        Unlock;
      end;
    end;

  procedure TFacility.UpdateCache;
    begin
      UnCache;
      Cache;
    end;

  function TFacility.GetCacheName : string;
    begin
      result := MetaFacility.Name + NameSeparator + IntToStr(xPos) + NameSeparator + IntToStr(yPos);
    end;

  procedure TFacility.CopySettingsFrom( Facility : TFacility );
    begin
      fCurrBlock.CopySettingsFrom( Facility.CurrBlock );
    end;
    
  procedure TFacility.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fName := Reader.ReadString( 'Name', '' );
      fCreationDate := Reader.ReadInteger( 'CreationDate' , 0);
      fXPos := Reader.ReadInteger( 'xPos', 0 );
      fYPos := Reader.ReadInteger( 'yPos', 0 );
      Reader.ReadObject( 'Company', fCompany, nil );
      Reader.ReadObject( 'Town', fTown, nil );
      fMetaFacility := TMetaFacility(TheClassStorage.ClassById[tidClassFamily_Facilities, Reader.ReadString( 'MetaFacility', '' )]);
      fCurrStage    := Reader.ReadInteger( 'CurrentStage', 0 );
      Reader.ReadObject( 'CurrBlock', fCurrBlock, nil );
      Reader.ReadObject( 'MoneyGraph', fMoneyGraph, nil );
      fPeriodMoney     := Reader.ReadCurrency( 'PeriodMoney', 0 );
      fTrouble         := Reader.ReadByte( 'Trouble', 0 );
      fToBeDemolished  := Reader.ReadByte( 'ToBeDemolished', 0 );
      fCost            := Reader.ReadCurrency( 'Cost', 0 );
      fNetProfit       := Reader.ReadCurrency( 'PeriodMoney', 0 );

      // Initialize things
      if (fCompany <> nil) and (MetaFacility.TechnologyKind <> '')
        then
          try
            fCompanyResearch := fCompany.Research[MetaFacility.TechnologyKind];
            fCompanyDir      := fCompany.Directions[MetaFacility.TechnologyKind];
          except
          end;
    end;

  procedure TFacility.StoreToBackup( Writer : IBackupWriter );
    begin
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
        Writer.WriteCurrency( 'PeriodMoney', fPeriodMoney );
      finally
        Unlock;
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
        fInputs[i] := TMetaGate(aMetaBlock.MetaInputs[i]).Instantiate( self );
      for i := 0 to pred(OutputCount) do
        fOutputs[i] := TMetaGate(aMetaBlock.MetaOutputs[i]).Instantiate( self );
    end;

  destructor TBlock.Destroy;

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
        FreeGateArray( fInputs, InputCount );
      except
      end;
      try
        FreeGateArray( fOutputs, OutputCount );
      except
      end;
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

  function TBlock.GetDt : TVirtDateAbs;
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
    begin
      result := evrNormal;
      if (Facility.Company <> nil) and (Facility.Company.Owner <> nil)
        then Facility.Company.Owner.CurrFacPrestige := Facility.Company.Owner.CurrFacPrestige + MetaBlock.Prestige;
    end;

  function TBlock.ConnectTo( Block : TBlock; symetrical : boolean ) : string;
    var
      i, j      : integer;
      outputres : string;
    begin
      result := '';
      for i := 0 to pred(InputCount) do
        for j := 0 to pred(Block.OutputCount) do
          if (mfTradeable in Inputs[i].MetaInput.MetaFluid.Options) and (Inputs[i].MetaInput.MetaFluid = Block.Outputs[j].MetaOutput.MetaFluid)
            then
              begin
                if result <> ''
                  then result := result + LineBreak;
                result := result + '   ' + Inputs[i].MetaInput.MetaFluid.Name + ': ';
                case Inputs[i].ConnectTo( Block.Outputs[j] ) of
                  cnxValid :
                    result := result + 'hired';
                  cnxRejected :
                    result := result + 'rejected by company policy';
                  cnxDuplicated :
                    result := result + 'was already hired';
                  cnxForbiden :
                    result := result + 'connection not allowed';
                end;
              end;
      if symetrical
        then
          begin
            if result <> ''
              then result := 'Inputs' + LineBreak + result;
            outputres := Block.ConnectTo( self, false );
            if outputres <> ''
              then result := result + LineBreak + 'Outputs' + LineBreak + outputres;
            if result <> ''
              then result := 'Connection to ' + Block.Facility.Name + LineBreak + result
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
      i : integer;
    begin
      for i := 0 to pred(InputCount) do
        try
          Inputs[i].Collect;
        except
        end;
    end;

  procedure TBlock.CollectInputExtra;
    var
      i : integer;
    begin
      for i := 0 to pred(InputCount) do
        try
          Inputs[i].CollectExtra;
        except
        end;
    end;

  procedure TBlock.SpreadOutputs;
    var
      i : integer;
    begin
      for i := 0 to pred(OutputCount) do
        try
          Outputs[i].Spread;
        except
        end;
    end;

  procedure TBlock.SpreadOutputExtra;
    var
      i : integer;
    begin
      for i := 0 to pred(OutputCount) do
        try
          Outputs[i].SpreadExtra;
        except
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
        end;
    end;

  procedure TBlock.AutoConnect( loaded : boolean );
    var
      i : integer;
    begin
      if not loaded
        then
          begin
            if (Facility.Company <> nil) and (Facility.Company.Owner <> nil)
              then
                begin
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
          end;
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
      end;
    end;

  procedure TBlock.CheckConnections( ConnectionCheckLevel : TConnectionCheckLevel );
    var
      i : integer;
    begin
      if (ConnectionCheckLevel = chkOutputs) or (ConnectionCheckLevel = chkBoth)
        then
          for i := 0 to pred(OutputCount) do
            try
              Outputs[i].CheckConnections;
            except
            end;
      if (ConnectionCheckLevel = chkInputs) or (ConnectionCheckLevel = chkBoth)
        then
          for i := 0 to pred(InputCount) do
            try
              Inputs[i].CheckConnections;
            except
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
                  CacheObject( Inputs[i] );
                except
                end;
          for i := 0 to pred(OutputCount) do
            if mgoptCacheable in Outputs[i].MetaOutput.Options
              then
                try
                  CacheObject( Outputs[i] );
                except
                end;
        end;
    end;

  procedure TBlock.StoreToCache( Cache : TObjectCache );
    var
      i   : integer;
      cnt : integer;
    begin
      with Cache do
        begin
          cnt := 0;
          for i := 0 to pred(InputCount) do
            if mgoptCacheable in Inputs[i].MetaInput.Options
              then
                begin
                  WriteString('Input' + IntToStr(cnt), Inputs[i].MetaInput.MetaFluid.Name);
                  WriteString('InputPath' + IntToStr(cnt), GetObjectPath(Inputs[i]));
                  try
                    CacheObject( Inputs[i] );
                  except
                  end;
                  inc(cnt);
                end;
          WriteInteger('InputCount', cnt);
          cnt := 0;
          for i := 0 to pred(OutputCount) do
            if mgoptCacheable in Outputs[i].MetaOutput.Options
              then
                begin
                  WriteString('Output' + IntToStr(cnt), Outputs[i].MetaOutput.MetaFluid.Name);
                  WriteString('OutputPath' + IntToStr(cnt), GetObjectPath(Outputs[i]));
                  try
                    CacheObject( Outputs[i] );
                  except
                  end;
                  inc(cnt);
                end;
          WriteInteger('OutputCount', cnt);
        end;
    end;

  procedure TBlock.RemoveFromCache;
    var
      i : integer;
    begin
      for i := 0 to pred(InputCount) do
        if mgoptCacheable in Inputs[i].MetaInput.Options
          then UnCacheObject( Inputs[i] );
      for i := 0 to pred(OutputCount) do
        if mgoptCacheable in Outputs[i].MetaOutput.Options
          then UnCacheObject( Outputs[i] );
    end;

  procedure TBlock.CopySettingsFrom( Block : TBlock );
    begin
    end;
    
  procedure TBlock.LoadFromBackup( Reader : IBackupReader );
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
    end;

  procedure TBlock.StoreToBackup( Writer : IBackupWriter );
    var
      i : integer;
    begin
      inherited;
      Writer.WriteString( 'MetaBlock', fMetaBlock.Id );
      Writer.WriteObjectRef( 'Facility', Facility );
      Writer.WriteInteger( 'InputCount', InputCount );
      for i := 0 to pred(InputCount) do
        Writer.WriteObject( 'Input' + IntToStr(i), Inputs[i] );
      Writer.WriteInteger( 'OutputCount', OutputCount );
      for i := 0 to pred(OutputCount) do
        Writer.WriteObject( 'Output' + IntToStr(i), Outputs[i] );
    end;

  procedure TBlock.BlockModified( Modification : TBlockModification );
    begin
      if Facility <> nil
        then Facility.BlockModified( Modification );
    end;

  procedure TBlock.BlockLoaded;


    procedure CheckConnections( Gate : TGate );
      begin
        Gate.fConnections.Pack;
      end;

    procedure FixInputs;
      var
        i         : integer;
        NewArray  : PGateArray;
        MetaInput : TMetaInput;
        Input     : TInput;
        DeadMeat  : TCollection;
      begin
        DeadMeat := nil;
        try
          // First remove connections held by outdated inputs
          for i := 0 to pred(InputCount) do
            if Inputs[i].MetaInput = nil
              then
                begin
                  Inputs[i].DisconnectAll;
                  if DeadMeat = nil
                    then DeadMeat := TCollection.Create( 0, rkBelonguer );
                  DeadMeat.Insert( Inputs[i] );
                end;
          // Create new input array
          getmem( NewArray, MetaBlock.MetaInputs.Count*sizeof(NewArray[0]) );
          for i := 0 to pred(MetaBlock.MetaInputs.Count) do
            begin
              MetaInput := TMetaInput(MetaBlock.MetaInputs[i]);
              Input := InputsByName[MetaInput.Name];
              if Input = nil
                then Input := TInput(MetaInput.Instantiate( self ));
              NewArray[i] := Input;
            end;
          // Set the new array
          freemem( fInputs, InputCount*sizeof(fInputs[0]) );
          fInputs     := NewArray;
          fInputCount := MetaBlock.MetaInputs.Count;
        finally
          if DeadMeat <> nil
            then DeadMeat.Free;
        end;
        for i := 0 to pred(InputCount) do
          CheckConnections( Inputs[i] );
      end;

    procedure FixOutputs;
      var
        i          : integer;
        NewArray   : PGateArray;
        MetaOutput : TMetaOutput;
        Output     : TOutput;
        DeadMeat   : TCollection;
      begin
        DeadMeat := nil;
        try
          // First remove connections held by outdated Outputs
          for i := 0 to pred(OutputCount) do
            if Outputs[i].MetaOutput = nil
              then
                begin
                  Outputs[i].DisconnectAll;
                  if DeadMeat = nil
                    then DeadMeat := TCollection.Create( 0, rkBelonguer );
                  DeadMeat.Insert( Outputs[i] );
                end;
          // Create new Output array
          getmem( NewArray, MetaBlock.MetaOutputs.Count*sizeof(NewArray[0]) );
          for i := 0 to pred(MetaBlock.MetaOutputs.Count) do
            begin
              MetaOutput := TMetaOutput(MetaBlock.MetaOutputs[i]);
              Output := OutputsByName[MetaOutput.Name];
              if Output = nil
                then Output := TOutput(MetaOutput.Instantiate( self ));
              NewArray[i] := Output;
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
          CheckConnections( Outputs[i] );
      end;

    begin
      FixInputs;
      FixOutputs;
      AutoConnect( true );
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
    
  function TBlock.Offset( var Field ) : TOffset;
    begin
      result := integer(@Field) - integer(@fOffsetRef)
    end;

  procedure TBlock.CleanExtraInfo;
    var
      ExtraInfo : PExtraConnectionInfo;
      i, j      : integer;
    begin
      for i := 0 to pred(InputCount) do
        for j := 0 to pred(Inputs[i].ConnectionCount) do
          begin
            ExtraInfo := Inputs[i].ExtraConnectionInfo[j];
            if ExtraInfo <> nil
              then
                begin
                  ExtraInfo.YearValue := 0;
                  ExtraInfo.YearCost  := 0;
                end;
          end;
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
      if ConnectionAllowedByPolicy( Connection )
        then
          begin
            fConnections.Extract( Connection );
            InsertConnection( Connection );
          end
        else DisconnectFrom( Connection );
    end;

  function TGate.GetConnectionPrecedence( Connection : TGate ) : integer;
    var
      Status : TPolicyStatus;
    begin
      if (Block.Facility.Company <> nil) and (Block.Facility.Company.Owner <> nil) and
         (Connection.Block.Facility.Company <> nil) and (Connection.Block.Facility.Company.Owner <> nil)
        then
          if Block.Facility.Company.Owner <> Connection.Block.Facility.Company.Owner
            then
              begin
                Status := Block.Facility.Company.Owner.Policy[Connection.Block.Facility.Company.Owner];
                if Status = pstAlly
                  then result := 1000
                  else result := 0;
              end
            else result := 10000
        else result := 0;
      {
      if (Block.Facility.Company <> nil) and (Connection.Block.Facility.Company <> nil)
        then
          result :=
            1000*integer(Options and goptSameCompany <> 0)*integer(Connection.Block.Facility.Company = Block.Facility.Company) +
            1000*integer(Options and goptSameTycoon <> 0)*integer(Connection.Block.Facility.Company.Owner = Block.Facility.Company.Owner)
        else result := 0;
      }
    end;

  function TGate.ConnectionAllowed( Connection : TGate ) : TConnectResult;
    begin
      result := cnxValid;
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
      if (Block.Facility.MetaFacility.Kind = nil) or
         (Block.Facility.MetaFacility.Kind.Role <> rolDistributer) or
         (Block.Facility.MetaFacility.Kind.Role <> Gate.Block.Facility.MetaFacility.Kind.Role)
        then
          if ConnectionAllowedByPolicy( Gate )
            then
              if fConnections.IndexOf( Gate ) = NoIndex
                then
                  begin
                    result := ConnectionAllowed( Gate );
                    if result = cnxValid
                      then
                        begin
                          InsertConnection( Gate );
                          Gate.InsertConnection( self );
                        end;
                  end
                else result := cnxDuplicated
            else result := cnxRejected
        else result := cnxForbiden;
    end;

  procedure TGate.DisconnectFrom( Gate : TGate );
    begin
      Gate.DeleteConnection( self );
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
        TInput(fConnections[i]).ConnectionChanged( self );
      for i := pred(fConnections.Count) downto 0 do
        ConnectionChanged( TInput(fConnections[i]) );
    end;

  class function TGate.BestCollection( aSize : integer ) : TCollection;
    begin
      result := TCollection.Create( 0, rkUse );
    end;

  procedure TGate.AutoConnect( loaded : boolean );
    begin
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

  procedure TGate.HookFluidData( aMetaGate : TMetaGate );
    begin
      if aMetaGate.Offset >= 0
        then SetFluidData( PFluidData(integer(@Block.fOffsetRef) + aMetaGate.Offset) );
    end;

  function TGate.ConnectionAllowedByPolicy( Gate : TGate ) : boolean;
    begin
      result :=
        ((Block.Facility.Company = nil) or (Block.Facility.Company.Owner = nil) or
         (Gate.Block.Facility.Company = nil) or (Gate.Block.Facility.Company.Owner = nil)) or
         (Block.Facility.Company.Owner.Policy[Gate.Block.Facility.Company.Owner] <> pstEnemy) and
         (Gate.Block.Facility.Company.Owner.Policy[Block.Facility.Company.Owner] <> pstEnemy)
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
      fMetaInput := fBlock.MetaBlock.InputByName[MetaInputId];
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
        else Reader.ReadBuffer( 'FluidData', buffer, @buffer, IgnoredBufferSize );
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

  function TInput.GetConnectionPrecedence( Connection : TGate ) : integer;
    var
      Output : TOutput;
      Idx    : integer;
    begin
      Output := TOutput(Connection);
      Idx    := fConnections.IndexOf( Output );
      result := inherited GetConnectionPrecedence( Connection ) - round(1000*integer(Options and goptCost <> 0)*Output.PriceToDeliver( 1, self, ExtraConnectionInfo[Idx], tnpPositive ))
    end;

  procedure TInput.RDOSetInputFluidPerc(Perc : integer);
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' - Fac(' + IntToStr(Block.Facility.XPos) + ',' + IntToStr(Block.Facility.YPos) + ') ' + 'Setting Input fluid perc: ' + IntToStr(Perc) );
      ActualMaxFluid.Q := MetaInput.MaxFluid.Q*realmin(1, Perc/100);
      Logs.Log( tidLog_Survival, 'OK!' );
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

  procedure TOutput.SetPricePerc( aPricePerc : TPercent );
    var
      i : integer;
    begin
      fPricePerc := aPricePerc;
      for i := pred(fConnections.Count) downto 0 do
        TInput(fConnections[i]).ConnectionChanged( self );
    end;

  function TOutput.GetDemand : TFluidValue; 
    begin
      result := 0;
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
            if ExtraConnectionInfo <> nil
              then OverPrice := ExtraConnectionInfo.OverPrice
              else OverPrice := 0;
            if (Input.Block.XPos <> NoPos) and (Input.Block.YPos <> NoPos)
              then d := dist(Block.XPos, Block.YPos, Input.Block.XPos, Input.Block.YPos)
              else d := 0;
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

  function TOutput.GetConnectionPrecedence( Connection : TGate ) : integer;
    var
      Input : TInput;
      Idx   : integer;
    begin
      Input := TInput(Connection);
      Idx   := fConnections.IndexOf( Input );
      result := inherited GetConnectionPrecedence( Connection ) + round(1000*integer(Options and goptCost <> 0)*PriceToDeliver( 1, Input, ExtraConnectionInfo[Idx], tnpNegative ));
    end;

  procedure TOutput.Spread;
    begin
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


  // TPullInput

  procedure TPullInput.Collect;
    begin
      DoCollect( false );
    end;

  procedure TPullInput.CollectExtra;
    begin
      DoCollect( true );
      fDemand.CurrValue   := fDemand.CurrValue   + ActualMaxFluid.Q;
      fValue.CurrValue    := fValue.CurrValue    + FluidData.Q;
      fCapacity.CurrValue := fCapacity.CurrValue + MetaInput.MaxFluid.Q;
    end;

  procedure TPullInput.DoCollect( CollectExtra : boolean );
    var
      dQ     : TFluidValue;
      QKsum  : TFluidValue;
      Qsum   : TFluidValue;
      CurrF  : TFluidData;
      i      : integer;
      Cost   : TMoney;
      ExInf  : PExtraConnectionInfo;
      Output : TPullOutput;
    begin
      inherited;
      i := 0;
      if not CollectExtra
        then
          begin
            FluidData.Q := 0;
            vLastCost := 0;
          end;
      CurrF.Q := 0;
      CurrF.K := 0;
      while (FluidData.Q < Capacity) and (i < ConnectionCount) and not (Block.Facility.CriticalTrouble) do
        begin
          Output := TPullOutput(Connections[i]);
          dQ     := Output.GetSliceFor( self );
          ExInf  := ExtraConnectionInfo[i];
          Cost   := 0;
          if dQ > 0                  
            then
              begin
                if FluidData.Q + dQ > Capacity
                  then dQ := Capacity - FluidData.Q;
                Cost := Output.PriceToDeliver( dQ, self, ExInf, tnpPositiveShare );
                if Cost > 0
                  then
                    if Block.Facility.Budget > 0
                      then
                        begin
                          if mfConstruction in MetaInput.MetaFluid.Options
                            then
                              begin
                                Block.BlockGenMoney( -Cost, accIdx_Construction );
                                Block.Facility.fCost := Block.Facility.fCost + Cost;
                              end
                            else
                              if Block.MetaBlock.fSupplyAccount <> accIdx_None
                                then Block.BlockGenMoney( -Cost, Block.MetaBlock.fSupplyAccount );
                          Cost := Output.PriceToDeliver( dQ, self, ExInf, tnpNegativeShare );
                          vLastCost := vLastCost + Cost;
                          if Output.Block.MetaBlock.fProdAccount <> accIdx_None
                            then Output.Block.BlockGenMoney( Cost, Output.Block.MetaBlock.fProdAccount );
                        end
                      else
                        begin
                          Block.Facility.ReportTrouble( facNeedsBudget );
                          dQ   := 0;
                          Cost := 0;
                        end;
                Output.fQAuxBuff := dQ;
                FluidData.Q := FluidData.Q + dQ;
                CurrF.Q := CurrF.Q + dQ;
              end;
          if not CollectExtra and ExInf.Connected and not Block.Facility.CriticalTrouble
            then Output.ReportDemand( Capacity - FluidData.Q, self );
          if ExInf <> nil
            then
              begin
                if not CollectExtra
                  then ExInf.LastFluid := 0;
                ExInf.LastFluid := ExInf.LastFluid + dQ/Block.dt;
                ExInf.YearValue := ExInf.YearValue + dQ;
                ExInf.YearCost  := ExInf.YearCost  + Cost;
              end;
          inc( i );
        end;
      QKsum := 0;
      Qsum  := 0;               
      for i := 0 to pred(ConnectionCount) do
        begin
          Output := TPullOutput(Connections[i]);
          QKsum  := QKsum + Output.fQAuxBuff*Output.FluidData.K;
          Qsum   := Qsum + Output.fQAuxBuff;
          if (FluidData.Q < Capacity) //and not Block.Facility.CriticalTrouble
            then Output.ValuePulled( Capacity, self )
            else Output.ValuePulled( Output.fQAuxBuff, self );
          Output.fQAuxBuff := 0;
        end;
      if Qsum > 0
        then CurrF.K := round(QKsum/Qsum)
        else CurrF.K := 0;
      if CollectExtra
        then FluidData.K := AverageK( FluidData, @CurrF )
        else FluidData.K := CurrF.K;
      if FluidData.Q < MetaInput.MinFluid.Q
        then Block.Facility.ReportTrouble( facInsuficientInput );
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
      CnxVal := GetConnectionPrecedence( Connection );
      i := 0;
      while (i < fConnections.Count) and (GetConnectionPrecedence( TGate(fConnections[i]) ) >= CnxVal) do
        inc( i );
      fConnections.AtInsert( i, Connection );
    end;

  function TPullInput.GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo;
    var
      InputIdx : integer;
    begin
      if index <> NoIndex
        then
          try
            InputIdx := Connections[index].fConnections.IndexOf( self );
            result := Connections[index].ExtraConnectionInfo[InputIdx];
          except
            result := nil;
          end
        else result := nil;
    end;

  procedure TPullInput.CheckConnections;
    var
      Roads1, Roads2 : TCollection;
      i              : integer;
    begin
      inherited;
      Roads1 := Block.Circuits[cirRoads];
      if Roads1 <> nil
        then
          for i := 0 to pred(ConnectionCount) do
            begin
              Roads2 := Connections[i].Block.Circuits[cirRoads];
              ExtraConnectionInfo[i].Connected := (Roads2 <> nil) and (Roads2.Matches( Roads1 ) > 0);
            end;
    end;
    
  procedure TPullInput.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fLastCost := Reader.ReadCurrency( 'LastCost', 0 );
      if MetaInput <> nil
        then
          begin
            fDemand   := Block.Facility.Town.Parameters[tidTownParameter_InputDemand + MetaInput.MetaFluid.Id];
            fValue    := Block.Facility.Town.Parameters[tidTownParameter_InputValue + MetaInput.MetaFluid.Id];
            fCapacity := Block.Facility.Town.Parameters[tidTownParameter_InputCapacity + MetaInput.MetaFluid.Id];
          end
    end;

  procedure TPullInput.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteCurrency( 'LastCost', fLastCost );
    end;

  procedure TPullInput.AutoConnect( loaded : boolean );
    begin
      inherited;
      fDemand   := Block.Facility.Town.Parameters[tidTownParameter_InputDemand + MetaInput.MetaFluid.Id];
      fValue    := Block.Facility.Town.Parameters[tidTownParameter_InputValue + MetaInput.MetaFluid.Id];
      fCapacity := Block.Facility.Town.Parameters[tidTownParameter_InputCapacity + MetaInput.MetaFluid.Id];
    end;


  // TPushOutput

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


  // TPullOutput

  constructor TPullOutput.Create( aMetaGate : TMetaGate; aBlock : TBlock );
    begin
      inherited;
      fSlices := TCollection.Create( 0, rkBelonguer );
      TNotifiedCollection(fConnections).OnModified := OnConnectionsModified;
    end;
    
  destructor TPullOutput.Destroy;
    begin
      fSlices.Free;
      inherited;
    end;
    
  procedure TPullOutput.OnConnectionsModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
    begin
      case Operation of
        opInsertion :
          fSlices.AtInsert( Index, TSlice.Create );
        opDeletion :
          fSlices.AtDelete( Index );
      end;
    end;

  procedure TPullOutput.Spread;
    var
      i : integer;
    begin
      inherited;
      fDemand := 0;
      POutputData(FluidData).Extra.Q := 0;
      POutputData(FluidData).Extra.K := FluidData.K;
      for i := 0 to pred(fSlices.Count) do
        with TSlice(fSlices[i]) do
          begin
            taken := false;
            val   := 0;
          end;
      Slice( FluidData.Q );
    end;

  procedure TPullOutput.SpreadExtra;
    var
      i : integer;
    begin       
      inherited;

      // Compute Extra and Demand
      // fDemand := 0;
      for i := 0 to pred(fSlices.Count) do
        with TSlice(fSlices[i]) do
          if val > 0
            then POutputData(FluidData).Extra.Q := POutputData(FluidData).Extra.Q + val;
            // else fDemand := fDemand + abs(val);

      // Fill last period needs with last period extra and clear slice info
      for i := 0 to pred(fSlices.Count) do
        with TSlice(fSlices[i]) do
          if (val < 0) and (POutputData(FluidData).Extra.Q > 0) and ExtraConnectionInfo.Connected
            then
              begin
                val := realmin( POutputData(FluidData).Extra.Q, -val );
                POutputData(FluidData).Extra.Q := POutputData(FluidData).Extra.Q - val;
                fDemand := fDemand - val;
                taken := false;
              end
            else val := 0;

      fValue.CurrValue    := fValue.CurrValue    + FluidData.Q;
      fCapacity.CurrValue := fCapacity.CurrValue + MetaOutput.MaxFluid.Q;
    end;

  function TPullOutput.GetDemand : TFluidValue;
    begin
      result := fDemand;
    end;

  procedure TPullOutput.ValuePulled( Value : TFluidValue; Input : TInput );
    var
      Idx : integer;
    begin
      Idx := fConnections.IndexOf( Input );
      with TSlice(fSlices[Idx]) do
        begin
          taken := true;
          if ExtraConnectionInfo.Connected
            then
              begin
                val := val - Value;
                if val > 0
                  then
                    begin
                      Slice( val );
                      val := 0;
                    end;
              end;
        end;
    end;

  procedure TPullOutput.ReportDemand( Value : TFluidValue; Input : TInput );
    begin
      fDemand := fDemand + Value;
    end;
    
  procedure TPullOutput.Slice( Value : TFluidValue );
    var
      i : integer;
      v : TFluidValue;
    begin
      i := 0;
      while (i < ConnectionCount) and (Value > 0) do
        begin
          with TSlice(fSlices[i]) do
            if not taken and not Connections[i].Block.Facility.CriticalTrouble and ExtraConnectionInfo.Connected
              then
                begin
                  if val + Value > Connections[i].Capacity
                    then v := Connections[i].Capacity - val
                    else v := Value;
                  Value := Value - v;
                  val := val + v;
                end;
          inc( i );
        end;
      POutputData(FluidData).Extra.Q := POutputData(FluidData).Extra.Q + Value;
    end;

  function TPullOutput.GetSliceFor( Input : TInput ) : TFluidValue;

    function GetExtraSliceFor( Input : TInput; InputIdx : integer ) : TFluidValue;
      begin        
        result := realmin( POutputData(FluidData).Extra.Q, Input.ActualMaxFluid.Q );
      end;

    var               
      Idx : integer;
    begin
      Idx := fConnections.IndexOf( Input );
      if Idx = NoIndex
        then
          if ConnectTo( Input ) = cnxValid
            then
              begin
                Logs.Log( 'Survival', 'Unknown Input found x:' + IntToStr(Input.Block.xPos) + ' y:' + IntToStr(Input.Block.yPos));
                Idx := vConnections.IndexOf( Input );
              end;
      if Idx <> NoIndex
        then
          begin
            result := TSlice(fSlices[Idx]).val;
            if (result = 0) and not Connections[Idx].Block.Facility.CriticalTrouble and TSlice(fSlices[Idx]).ExtraConnectionInfo.Connected
              then
                begin
                  result := GetExtraSliceFor( Input, Idx );
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
    begin
      inherited;
      ComputePriorities;
    end;
    
  procedure TPullOutput.InsertConnection( Connection : TGate );
    var
      CnxVal : integer;
      i      : integer;
    begin
      if mgoptEditable in MetaOutput.Options
        then
          begin
            CnxVal := GetConnectionPrecedence( Connection );
            i := 0;
            while (i < fConnections.Count) and (GetConnectionPrecedence( TGate(fConnections[i]) ) >= CnxVal) do
              inc( i );
            fConnections.AtInsert( i, Connection );
          end
        else inherited;
      ComputePriorities;
    end;

  function TPullOutput.GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo;
    begin
      if index <> NoIndex
        then
          try
            result := @(TSlice(fSlices[index]).ExtraConnectionInfo);
          except
            result := nil;
          end
        else result := nil;
    end;

  procedure TPullOutput.CheckConnections;
    var
      Roads1, Roads2 : TCollection;
      i              : integer;
    begin
      inherited;
      Roads1 := Block.Circuits[cirRoads];
      if Roads1 <> nil
        then
          for i := 0 to pred(ConnectionCount) do
            begin
              Roads2 := Connections[i].Block.Circuits[cirRoads];
              ExtraConnectionInfo[i].Connected := (Roads2 <> nil) and (Roads2.Matches( Roads1 ) > 0);
            end;
    end;
    
  class function TPullOutput.BestCollection( aSize : integer ) : TCollection;
    begin
      result := TNotifiedCollection.Create( 0, rkUse )
    end;

  procedure TPullOutput.LoadFromBackup( Reader : IBackupReader );
    var
      Slice : TSlice;
      i     : integer;
    begin
      inherited;
      fSlices := TCollection.Create( 0, rkBelonguer );
      for i := 0 to pred(fConnections.Count) do
        begin
          Slice := TSlice.Create;
          Slice.ExtraConnectionInfo.YearValue := Reader.ReadSingle( 'YearInfo' + IntToStr(i) + '.Value', 0 );
          Slice.ExtraConnectionInfo.YearCost  := Reader.ReadCurrency( 'YearInfo' + IntToStr(i) + '.Cost', 0 );
          Slice.ExtraConnectionInfo.OverPrice := Reader.ReadByte( 'OverPrice' + IntToStr(i), 0 );
          Slice.ExtraConnectionInfo.Connected := Reader.ReadBoolean( 'Connected' + IntToStr(i), false );
          Slice.ExtraConnectionInfo.Priority  := Reader.ReadByte( 'Priority' + IntToStr(i), 0 );
          fSlices.Insert( Slice );
        end;
      TNotifiedCollection(fConnections).OnModified := OnConnectionsModified;
      ComputePriorities;  // >> temporary!
      fValue    := Block.Facility.Town.Parameters[tidTownParameter_OutputValue + MetaOutput.MetaFluid.Id];
      fCapacity := Block.Facility.Town.Parameters[tidTownParameter_OutputCapacity + MetaOutput.MetaFluid.Id];
    end;

  procedure TPullOutput.StoreToBackup( Writer : IBackupWriter );
    var
      i : integer;
    begin
      inherited;
      for i := 0 to pred(fConnections.Count) do
        begin
          Writer.WriteSingle( 'YearInfo' + IntToStr(i) + '.Value', TSlice(fSlices[i]).ExtraConnectionInfo.YearValue );
          Writer.WriteCurrency( 'YearInfo' + IntToStr(i) + '.Cost', TSlice(fSlices[i]).ExtraConnectionInfo.YearCost );
          Writer.WriteByte( 'OverPrice' + IntToStr(i), TSlice(fSlices[i]).ExtraConnectionInfo.OverPrice );
          Writer.WriteBoolean( 'Connected' + IntToStr(i), TSlice(fSlices[i]).ExtraConnectionInfo.Connected );
          Writer.WriteByte( 'Priority' + IntToStr(i), TSlice(fSlices[i]).ExtraConnectionInfo.Priority );
        end;
    end;

  procedure TPullOutput.AutoConnect( loaded : boolean );
    begin
      inherited;
      fValue    := Block.Facility.Town.Parameters[tidTownParameter_OutputValue + MetaOutput.MetaFluid.Id];
      fCapacity := Block.Facility.Town.Parameters[tidTownParameter_OutputCapacity + MetaOutput.MetaFluid.Id];
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
      fDebtor.Loans.Delete( self );
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

  function TBank.EstimateLoan( Client : TMoneyDealer ) : TMoney;

    function MoneyPerYear : TMoney;
      var
        year, useless : word;
        hours         : integer;
      begin
        // Compute number of hours in current year
        DecodeDate( fTimer.GetVirtualTime, year, useless, useless );
        hours := fTimer.GetVirtualTimeAbs - integer(year - YearZero)*365*24;

        // Compute net profit per year
        if (Client.Accounts.AccountArray[accIdx_Construction] <> nil) and
           (Client.Accounts.AccountArray[accIdx_Bank] <> nil) and
           (hours <> 0)
          then result := 365*24*(Client.Accounts.MasterAccount.Value - Client.Accounts.AccountArray[accIdx_Construction].Value - 2*Client.Accounts.AccountArray[accIdx_Bank].Value)/hours
          else result := 365*24*Client.NetProfit;
      end;

    begin
      result := int(MoneyPerYear*Term/1000000)*1000000;
      if Owner <> nil
        then result := realmin( Owner.BankLoanLimit, result )
        else result := realmin( MaxLoan, result );
    end;

  function TBank.AskLoan( Client : TMoneyDealer; Amount : TMoney ) : TBankRequestResult;
    var
      Loan : TLoan;
    begin
      Lock;
      try
        if LoanApproved( Client, Amount )
          then
            begin
              Loan := TLoan.Create;
              Loan.fBank     := self;
              Loan.fDebtor   := Client;
              Loan.fAmount   := Amount;
              Loan.fTerm     := Term; //round((Amount/1000000)*Term);
              Loan.fInterest := Interest;
              Loan.fDate     := Timer.GetVirtualTimeAbs;
              Loan.fSlice    := Amount/Loan.fTerm;
              fLoans.Insert( Loan );
              Client.Loans.Insert( Loan );
              Client.GenMoney( Amount, accIdx_Bank_Loans );
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
                else result := brqApproved
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
            then fLoans.AtDelete( i );
      finally
        Unlock;    
      end;
    end;

  function TBank.LoanApproved( Client : TMoneyDealer; Amount : TMoney ) : boolean;
    begin
      result := EstimateLoan( Client ) >= Amount;
    end;

  procedure TBank.LoadFromBackup( Reader : IBackupReader );

    procedure PackLoans;
      var
        i : integer;
      begin
        for i := pred(Loans.Count) downto 0 do
          if TLoan(Loans[i]).fDebtor = nil
            then Loans.AtDelete( i );
      end;

    begin
      inherited;
      Reader.ReadObject( 'Owner', fOwner, nil );
      Reader.ReadObject( 'Loans', fLoans, nil );
      fTerm := Reader.ReadInteger( 'Term', 1 );
      fInterest := Reader.ReadByte( 'Interest', 2 );
      PackLoans;
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

  procedure TMoneyDealer.GenMoney( Money : TMoney; Reason : TMoneyReason );
    var
      Account : TAccount;
    begin
      if Reason <> accIdx_None
        then
          try
            Account := Accounts.AccountArray[Reason];
            if Account <> nil
              then Account.Value := Account.Value + Money
              else raise Exception( 'Invalid account Id: ' + IntToStr(Reason) );
          except
          end;
    end;

  procedure TMoneyDealer.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
    begin
      case PeriodType of
        perHour :
          begin
            fNetProfit  := (Accounts.MasterAccount.Value - fLastPeriod)/PeriodCount;
            fLastPeriod := Accounts.MasterAccount.Value;
          end;
        perYear :
          fAccounts.EndOfPeriod;
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
      Writer.WriteObject( 'Accounts', fAccounts );
      Writer.WriteObject( 'Loans', fLoans );
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
    
  function TMoneyDealer.BankLoanLimit : TMoney;
    begin
      result := realmin( 1, (BankLoanPerc/100) )*Budget;
    end;


  // TMetaTownParameter

  constructor TMetaTownParameter.Create( anId, aName : string; anAutoClear : boolean );
    begin
      inherited Create( anId );
      fName      := aName;
      fAutoClear := anAutoClear;
      Cacheable  := false;
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
      begin
        try
          count := TheClassStorage.ClassCount[tidClassFamily_Taxes];
        except
          count := 0;
        end;
        fTaxes := TLockableCollection.Create( count, rkBelonguer );
        for i := 0 to pred(count) do
          begin
            MT := TMetaTax(TheClassStorage.ClassByIdx[tidClassFamily_Taxes, i]);
            fTaxes.Insert( MT.Instantiate );
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
      fMayor        := aModelFactory.NewTycoonRole( tidRole_Mayor, 'Mayor of ' + aName, TMayor, MayorInitialBudget );
      fTownCompany  := aModelFactory.NewCompany( aName, aCluster, fMayor );
      aMailServer.NewMailAccount( 'mayor@' + aName + '.gov', fMayor.Name, '', true ); 
    end;

  destructor TTown.Destroy;
    begin
      fParameters.Free;
      fParmHash.Free;
      fOrdinances.Free;
      fTaxes.Free;
      inherited;
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

  function TTown.GetContextStatusStr : string;
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
      if PeriodType = perYear
        then
          for i := 0 to pred(fTaxes.Count) do
            TTax(fTaxes[i]).Reset;
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
      InitParameters;
    end;

  procedure TTown.StoreToBackup( Writer : IBackupWriter );

    procedure StoreTaxes;
      var
        i     : integer;
        count : integer;
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
                Writer.WriteObject( 'Tax.' + IntToStr(count), fTaxes[i] );
                inc( count );
              end;
      end;

    var
      i : integer;
    begin
      inherited;
      Writer.WriteInteger( 'TownId', fid );
      Writer.WriteString( 'Name', fName );
      Writer.WriteInteger( 'xPos', xPos );
      Writer.WriteInteger( 'yPos', yPos );
      Writer.WriteString( 'ClusterName', Cluster.Id );
      Writer.WriteInteger( 'OrdinanceCount', fOrdinances.Count );
      for i := 0 to pred(fOrdinances.Count) do
        Writer.WriteString( 'Ordinance[' + IntToStr(i) + ']', TOrdinance(fOrdinances).Id );
      Writer.WriteObjectRef( 'Mayor', fMayor );
      Writer.WriteObjectRef( 'TownCompany', fTownCompany );
      StoreTaxes;
    end;

  procedure TTown.Loaded;
    begin
      inherited;
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
    

  // TCluster

  function TCluster.NameNewspaper( TownName : string ) : string;
    begin
      result := TownName + ' Herald';
    end;


  // TCompanyResearch

  constructor TCompanyResearch.Create( aCompany : TCompany );
    begin
      inherited Create;
      fCompany    := aCompany;
      fInventions := TCollection.Create( 0, rkUse );
    end;

  destructor TCompanyResearch.Destroy;
    begin
      fInventions.Free;
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
      if Invention.Req.Count > 0
        then result := true
        else
          begin
            i := 0;
            while (i < Invention.Req.Count) and HasInvention[Invention.Req[i]] do
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
        fInventions := TCollection.Create( 0, rkUse );
        count := Reader.ReadInteger( 'InventionCount', 0 );
        for i := 0 to pred(count) do
          begin
            InventionId := Reader.ReadString( 'Invention.' + IntToStr(i), '' );
            try
              Invention := TInvention(TheClassStorage.ClassById[tidClassFamily_Inventions, InventionId]);
              fInventions.Insert( Invention );
              fInventionSet.Include(Invention.NumId); // >> !!!
            except
            end;
          end;
      end;
      
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


  // TCompanyDirection

  constructor TCompanyDirection.Create( anId : string );
    begin
      inherited Create;
      fId := anId;
    end;                                                       

  procedure TCompanyDirection.Update;
    begin
      if round(fDemand) > 0
        then fSupport := realmin(2, fStrength/fDemand) 
        else fSupport := 1;
      fDemand   := 0;
      fStrength := 0;
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
    
  
  // TCompany

  constructor TCompany.Create( anId : TCompanyId );
    begin
      inherited Create;
      fId                := anId;
      fAutoConnectLevels := [mglBasic];
      fResearchs         := TLockableCollection.Create( 0, rkBelonguer );
      fDirections        := TLockableCollection.Create( 0, rkBelonguer );
      fProjects          := TLockableCollection.Create( 0, rkBelonguer );
      fFacilities        := TLockableCollection.Create( 0, rkUse );
      fMetaFacilityList  := TCompanyMetaFacilityList.Create( self );
      fPrivated          := true;
    end;

  destructor TCompany.Destroy;
    begin
      fResearchs.Free;
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
      i, j      : integer;
      Research  : TCompanyResearch;
      Invention : TInvention;
    begin
      result := '';
      for i := 0 to pred(Researchs.Count) do
        begin
          Research := TCompanyResearch(Researchs[i]);
          if Research.Inventions <> nil
            then
              for j := 0 to pred(Research.Inventions.Count) do
                begin
                  Invention := TInvention(Research.Inventions[j]);
                  if Invention <> nil
                    then result := result + Invention.Id + ';';
                end;
        end;
    end;

  procedure TCompany.GenMoney( Money : TMoney; Reason : TMoneyReason );
    begin
      inherited;
      if Owner <> nil
        then Owner.GenMoney( Money, Reason );
    end;

  procedure TCompany.FacilityCreated( Facility : TFacility );
    begin
      fFacilities.Insert( Facility );
      Facility.Company := self;
    end;

  procedure TCompany.FacilityDestroyed( Facility : TFacility );
    begin
      Facility.Company := nil;
      fFacilities.Delete( Facility );
    end;

  procedure TCompany.UpdateParameters;
    var
      i : integer;
    begin
      for i := 0 to pred(fDirections.Count) do
        TCompanyDirection(fDirections[i]).Update;
    end;
    
  function TCompany.GetHasInvention( kind : string; NumId : TInventionNumId ) : boolean;
    var
      R : TCompanyResearch;
    begin
      R := Research[kind];
      result := (R <> nil) and R.HasInvention[NumId];
    end;

  function TCompany.GetResearch( kind : string ) : TCompanyResearch;
    var
      i : integer;
    begin
      try
        i := 0;
        while (i < fResearchs.Count) and (TCompanyResearch(fResearchs[i]).Kind <> kind) do
          inc( i );
        if i < fResearchs.Count
          then result := TCompanyResearch(fResearchs[i])
          else result := nil;
      except
        result := nil;
      end;
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

  procedure TCompany.DeclareInvention( kind, Id : string );
    var
      R : TCompanyResearch;
      I : TInvention;
    begin
      R := Research[kind];
      if R = nil
        then
          begin
            R := TCompanyResearch.Create( self );
            try
              R.fKind := kind;
              fResearchs.Insert( R );
            except
              R.Free;
              raise;
            end;
          end;
      try
        I := TInvention(TheClassStorage.ClassById[tidClassFamily_Inventions, Id]);
        R.Inventions.Insert( I );
        R.fInventionSet.Include(I.NumId);
        R.fLevel := R.fLevel + I.Level;
        ModelServerCache.CacheObject( self );
      except
        if R.Inventions.Count = 0
          then fResearchs.Delete( R );
        raise;
      end;
    end;

  procedure TCompany.Loaded;
    begin
      inherited;
      fFacilities.Pack;
    end;

  procedure TCompany.LoadFromBackup( Reader : IBackupReader );
    var
      ClusterId : string;
    begin
      inherited;
      Reader.ReadObject( 'Owner', fOwner, nil );
      ClusterId := Reader.ReadString( 'Cluster', defCluster );
      fCluster := TCluster(TheClassStorage.ClassById[tidClassFamily_Clusters, ClusterId]);
      fName := Reader.ReadString( 'Name', 'Unknown' );
      fCreated := Reader.ReadDouble( 'Created', 0 );
      fId := Reader.ReadInteger( 'Id', 0 );
      // >> Reader.ReadSet( 'AutoConnectLevels', fAutoConnectLevels );
      fAutoConnectLevels := [mglBasic];  // >> Fix this!!!
      Reader.ReadObject( 'Researchs', fResearchs, nil );
      fResearchs.Pack;
      Reader.ReadObject( 'Directions', fDirections, nil );
      fDirections.Pack;
      Reader.ReadObject( 'Facilities', fFacilities, nil );
      // To pack facilities has to wait to loaded call
      fPrivated := Reader.ReadBoolean( 'Privated', false );
      Reader.ReadObject( 'MetaFacilityList', fMetaFacilityList, nil );
    end;

  procedure TCompany.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObjectRef( 'Owner', fOwner );
      Writer.WriteString( 'Cluster', fCluster.Id );
      Writer.WriteString( 'Name', fName );
      Writer.WriteDouble( 'Created', fCreated );
      Writer.WriteInteger( 'Id', fId );
      // >> Writer.WriteSet( 'AutoConnectLevels', fAutoConnectLevels );
      Writer.WriteLooseObject( 'Researchs', fResearchs );
      Writer.WriteLooseObject( 'Directions', fDirections );
      Writer.WriteLooseObject( 'Facilities', fFacilities );
      Writer.WriteBoolean( 'Privated', fPrivated );
      Writer.WriteLooseObject( 'MetaFacilityList', fMetaFacilityList );
    end;

  function TCompany.GetCacheName : string;
    begin
      result := 'Co' + IntToStr(fId);
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
              UncacheObject( self );
              CacheObject( self );
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
      fHireOnlyWarehouses := Reader.ReadBoolean( 'HireOnlyWarehouses', false );
    end;

  procedure TAutoConnection.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'MetaFluidId', fMetaFluidId );
      Writer.WriteLooseObject( 'Connections', fConnections );
      Writer.WriteBoolean( 'HireTradeCenter', fHireTradeCenter );
      Writer.WriteBoolean( 'HireOnlyWarehouses', fHireOnlyWarehouses );
    end;


  // TCurriculumItem

  constructor TCurriculumItem.Create( anId : string; aKind : integer );
    begin
      inherited Create;
      fId   := anId;
      fKind := aKind;
    end;

  function TCurriculumItem.GetDesc : string;
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
    end;

  procedure TCurriculumItem.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'Id', fId );
      Writer.WriteInteger( 'Kind', fKind );
      Writer.WriteObjectRef( 'Tycoon', fTycoon );
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
      fCookies         := TStringList.Create;
      UpdateAutoConnections;
      InstantiateTutorial;
    end;

  destructor TTycoon.Destroy;
    begin
      fCompanies.Free;
      fRoles.Free;
      fAutoConnections.Free;
      fPolicies.Free;
      fCurriculum.Free;
      fTutorial.Free;
      fTaskContext.Free;
      fCookies.Free;
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

  procedure TTycoon.SetCookie( Id, Value : string );
    begin
      fCookies.Values[Id] := Value;
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
      inc( fTimesAwaken );
    end;

  procedure TTycoon.Sleep;
    begin
      dec( fTimesAwaken );
    end;

  function TTycoon.IsOnline : boolean;
    begin
      result := fTimesAwaken > 0;
    end;

  procedure TTycoon.SendNotification( Kind : integer; Title, Body : string; Options : integer );
    begin
      fWorldLocator.SendNotification( Id, Kind, Title, Body, Options );
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
      Status  : TPolicyStatus;
      idx     : integer;
    begin
      Tycoons := fWorldLocator.GetTycoonCollection;
      Tycoons.Lock;
      try
        Cache.WriteInteger( 'PolicyCount', Tycoons.Count - 1 );
        idx := 0;
        for i := 0 to pred(Tycoons.Count) do
          if Tycoons[i] <> self
            then
              begin
                Tycoon := TTycoon(Tycoons[i]);
                Cache.WriteString( 'PolTycoon' + IntToStr(idx), Tycoon.Name );
                Status := Policy[Tycoon];
                Cache.WriteInteger( 'PolTo' + IntToStr(idx), integer(Status) );
                Status := Tycoon.Policy[self];
                Cache.WriteInteger( 'PolFrom' + IntToStr(idx), integer(Status) );
                inc( idx );
              end;
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
            UncacheObject( self );
            UncacheObject( Role );
            Roles.Insert( Role );
            Role.SuperRole := self;
            Role.Password  := Password;
            CacheObject( self );
            CacheObject( Role );
          end;
    end;

  procedure TTycoon.AbandomRole( Role : TTycoon );
    begin
      if Roles.IndexOf( Role ) <> NoIndex
        then
          begin
            UncacheObject( self );
            UncacheObject( Role );
            Role.Password  := '';
            Role.SuperRole := nil;
            Roles.Delete( Role );
            CacheObject( self );
            CacheObject( Role );
          end;
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
          CacheObject( fCurriculum[i] );
      finally
        fCurriculum.Unlock;
      end;
    end;
    
  function TTycoon.RDOAskLoan( AmountStr : widestring ) : OleVariant;
    var
      Amount : TMoney;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' AskLoan: ' + Name + ', $' + AmountStr );
      Lock;
      try
        Amount := StrToCurr( AmountStr );
        try
          if fWorldLocator.GetMainBank.AskLoan( self, Amount ) <> brqRejected
            then
              begin
                CacheObject( self );
                result := NOERROR;
              end
            else result := ERROR_LoanNotGranted;
        except
          result := ERROR_Unknown;
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
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Sending money: ' + Name + ' to ' + ToTycoon + ', $' + AmountStr );
      try
        Amount := StrToCurr( AmountStr );
        if Amount > 0
          then
            begin
              Dest := WorldLocator.GetTycoonByName( ToTycoon );
              if Dest <> nil
                then
                  begin
                    Lock;
                    try
                      if Amount > fBudget
                        then Amount := fBudget;
                      GenMoney( -Amount, accIdx_TransfersOut );
                      CacheObject( self );
                      try
                        AmountStr := Format( '%.0n', [Amount] );
                        MailServer.SendHTMLMessage(
                          Name,
                          Dest.Name,
                          '<b>$' + AmountStr + ' successfully transferred.</b>',
                          WorldLocator.GetMessagesPath +
                            tidMailMsgURL_MoneySentNotification + '?' +
                            'From=' + Name + '&' +
                            'To=' + Dest.Name + '&' +
                            'Reason=' + Reason + '&' +
                            'Amount=' + AmountStr );
                      except
                      end;
                    finally
                      Unlock;
                    end;
                    Dest.Lock;
                    try
                      Dest.GenMoney( Amount, accIdx_TransfersIn );
                      CacheObject( Dest );
                    finally
                      Dest.Unlock
                    end;
                    result := NOERROR;
                  end
                else result := ERROR_UnknownTycoon
            end
          else result := ERROR_InvalidMoneyValue
      except
        result := ERROR_Unknown;
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
    begin
      try
        AutoConnection := FindAutoConnection( FluidId );
        if AutoConnection <> nil
          then
            begin
              Gates := ParseGateList( Suppliers );
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
                            CacheObject( self );
                          end;
                    end;
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
      ModifyAutoConnection( FluidId, Suppliers, true );
    end;

  procedure TTycoon.RDODelAutoConnection( FluidId, Suppliers : widestring );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Deleting initial suppliers: ' + Name + ', ' + FluidId + ', ' + Suppliers );
      ModifyAutoConnection( FluidId, Suppliers, false );
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
              CacheObject( self );
            end;
      except
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
              CacheObject( self );
            end;
      except
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
              CacheObject( self );
            end;
      except
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
              CacheObject( self );
            end;
      except
      end;
    end;

  procedure TTycoon.RDOSetPolicyStatus( ToTycoon : widestring; Status : integer );
    var
      Tycoon : TTycoon;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Setting policy status: ' + Name + ', ' + ToTycoon + ', ' + IntToStr(Status) );
      try
        Tycoon := fWorldLocator.GetTycoonByName( ToTycoon );
        if Tycoon <> nil
          then
            begin
              Policy[Tycoon] := TPolicyStatus(Status);
              UpdateObjectCache( self );  
              UpdateObjectCache( Tycoon );  
            end;
      except
      end;
    end;

  procedure TTycoon.GenMoney( Money : TMoney; Reason : TMoneyReason );
    begin
      inherited;
      fBudget := fBudget + Money;
    end;

  procedure TTycoon.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );

    function HasValidCompany : boolean;
      begin
        result := AllCompaniesCount > 0;
      end;

    begin
      inherited;
      if IsOnline and HasValidCompany and fTutorial.Active and fTutorial.TimeToSimulate(PeriodCount)
        then fTutorial.Execute;
      if PeriodType = perYear
        then
          if fBudget > 0
            then fFailureLevel := 0
            else inc( fFailureLevel );
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
             ((Facility.MetaFacility.Kind.Role = rolDistributer) or
              not AutoConnection.HireOnlyWarehouses or
              not (mfStorable in Facility.CurrBlock.Outputs[i].MetaOutput.MetaFluid.Options))
            then AutoConnection.Connections.Insert( Facility );
        end;
    end;

  procedure TTycoon.UnregisterSupplier( Facility : TFacility );
    var
      i : integer;
    begin
      fAutoConnections.Lock;
      try
        for i := 0 to pred(fAutoConnections.Count) do
          TAutoConnection(fAutoConnections[i]).FacilityDestroyed( Facility );
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
    begin
      for i := 0 to pred(Facility.CurrBlock.InputCount) do
        begin
          AutoConnection := FindAutoConnection( Facility.CurrBlock.Inputs[i].MetaInput.Name );
          if (AutoConnection <> nil) and (mfTradeable in Facility.CurrBlock.Inputs[i].MetaInput.MetaFluid.Options)
            then
              begin
                AutoConnection.Connections.Lock;
                try
                  for j := 0 to pred(AutoConnection.Connections.Count) do
                    if AutoConnection.Connections[j] <> Facility
                      then
                        begin
                          Supplier := TFacility(AutoConnection.Connections[j]);
                          Output   := Supplier.CurrBlock.OutputsByName[Facility.CurrBlock.Inputs[i].MetaInput.Name];
                          if (Output <> nil) and (Supplier.Town = Facility.Town)
                            then Output.ConnectTo( Facility.CurrBlock.Inputs[i] );
                        end;
                finally
                  AutoConnection.Connections.Unlock;
                end;
                if AutoConnection.HireTradeCenter and (Facility.CurrBlock.Inputs[i].MetaInput.Level = mglBasic)
                  then
                    begin
                      Supplier := TInhabitedTown(Facility.Town).TradeCenter;
                      if Supplier <> nil
                        then
                          begin
                            Output := Supplier.CurrBlock.OutputsByName[Facility.CurrBlock.Inputs[i].MetaInput.Name];
                            if (Output <> nil)
                              then Output.ConnectTo( Facility.CurrBlock.Inputs[i] );
                          end;
                    end;
              end;
        end;
    end;

  procedure TTycoon.LoadFromBackup( Reader : IBackupReader );
    var
      count : integer;
      i     : integer;
      TP    : TTycoonPolicy;
    begin
      inherited;
      fId := Reader.ReadInteger( 'Id', 0 );
      Reader.ReadBuffer( 'Options', fOptions, @fOptions, sizeof(fOptions) );
      Reader.ReadObject( 'Companies', fCompanies, nil );
      Reader.ReadObject( 'Roles', fRoles, nil );
      Reader.ReadObject( 'SuperRole', fSuperRole, nil );
      fName := Reader.ReadString( 'Name', '' );
      fPassword := Reader.ReadString( 'Password', '' );
      fBudget := Reader.ReadCurrency( 'Budget', 0 );
      fFailureLevel := Reader.ReadInteger( 'FailureLevel', 0 );
      Reader.ReadObject( 'AutoConnections', fAutoConnections, nil );
      if fAutoConnections = nil
        then fAutoConnections := TLockableCollection.Create( 0, rkBelonguer );
      UpdateAutoConnections;
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
      Reader.ReadObject( 'TaskContext', fTaskContext, nil );
      Reader.ReadObject( 'Tutorial', fTutorial, nil );
      if fTutorial = nil
        then InstantiateTutorial;
      Reader.ReadObject( 'Cookies', fCookies, nil );
      if fCookies = nil
        then fCookies := TStringList.Create;
    end;

  procedure TTycoon.StoreToBackup( Writer : IBackupWriter );
    var
      i : integer;
    begin
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
      Writer.WriteLooseObject( 'AutoConnections', fAutoConnections );
      Writer.WriteInteger( 'PolicyCount', fPolicies.Count );
      for i := 0 to pred(fPolicies.Count) do
        with TTycoonPolicy(fPolicies[i]) do
          begin
            Writer.WriteObjectRef( 'Tycoon.' + IntToStr(i), fTycoon );
            Writer.WriteInteger( 'Status.' + IntToStr(i), integer(fStatus) );
          end;
      Writer.WriteLooseObject( 'Curriculum', fCurriculum );
      Writer.WriteLooseObject( 'TaskContext', fTaskContext);
      Writer.WriteObject( 'Tutorial', fTutorial );
      Writer.WriteLooseObject( 'Cookies', fCookies );
    end;

  procedure TTycoon.Loaded;
    var
      i : integer;
    begin
      inherited;
      for i := 0 to pred(fCurriculum.Count) do
        CacheObject( fCurriculum[i] );
      if fTutorial = nil
        then InstantiateTutorial
        else
          begin
            if fTaskContext = nil
              then fTaskContext := TTycoonContext.Create(self, nil, nil);
            fTutorial.Loaded(fTaskContext);
          end;
    end;

  procedure TTycoon.UpdateParameters;

    procedure UpdateCurriculum;
      var
        i            : integer;
        CurrPrestige : TPrestige;
      begin
        fFacPrestige := fCurrFacPrestige;
        fCurriculum.Lock;
        try
          CurrPrestige := 0;
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
    
  function TTycoon.GetCacheName : string;
    begin
      result := 'Ty' + IntToStr(fId);
    end;

  procedure TTycoon.InstantiateTutorial;
    var
      MetaTask : TMetaTask;
    begin
      MetaTask := TMetaTask(TheClassStorage.ClassById[tidClassFamily_Tasks, tidTask_Tutorial]);
      if fTaskContext = nil
        then fTaskContext := TTycoonContext.Create(self, nil, nil);
      if MetaTask <> nil
        then fTutorial := MetaTask.Instantiate(nil, fTaskContext);
    end;

  procedure TTycoon.RDOActivateTutorial(Value : LongBool);
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Activating tutorial: ' + Name );
      if fTutorial <> nil
        then fTutorial.Active := Value;
    end;

  function TTycoon.RDOActiveTutorial(useless : integer) : OleVariant;
    begin
      result := fTutorial.Active;
    end;

  procedure TTycoon.DefaultHandler(var Message);
    begin
      if fTutorial <> nil
        then fTutorial.Dispatch(Message);
    end;

  // TTycoonContext

  constructor TTycoonContext.Create(aTycoon : TTycoon; aCompany : TCompany; aTown : TTown);
    begin
      inherited Create;
      fTycoon  := aTycoon;
      fCompany := aCompany;
      fTown    := aTown;
    end;

  function TTycoonContext.QueryInterface(const IID: TGUID; out Obj): Integer;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TTycoonContext._AddRef: Integer;
    begin
      result := 1;
    end;

  function TTycoonContext._Release: Integer;
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
      Reader.ReadObject('', fTycoon, nil);
      Reader.ReadObject('', fCompany, nil);
      Reader.ReadObject('', fTown, nil);
    end;

  procedure TTycoonContext.StoreToBackup ( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObjectRef('Tycoon', fTycoon);
      Writer.WriteObjectRef('Company', fCompany);
      Writer.WriteObjectRef('Town', fTown);
    end;

  // misc routines

  function GetInventionId( numId : TInventionNumId; kind : string ) : string;
    begin
      if numId > 0
        then result := kind + IntToStr(numId)
        else result := '';
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
                TMetaTownParameter.Create( tidTownParameter_InputDemand + MF.Id, '', true ).Register;
                TMetaTownParameter.Create( tidTownParameter_InputCapacity + MF.Id, '', true ).Register;
                TMetaTownParameter.Create( tidTownParameter_OutputValue + MF.Id, '', true ).Register;
                TMetaTownParameter.Create( tidTownParameter_OutputCapacity + MF.Id, '', true ).Register;
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
      RegisterClass( TCompanyResearch );
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

    
end.



