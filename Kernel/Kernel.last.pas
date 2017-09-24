unit Kernel;

interface

  uses
    Windows, Classes, Collection, Plotter, ClassStorageInt, CacheAgent, Surfaces,
    BackupInterfaces, Protocol, SyncObjs, Persistent;

  const
    NoPos = -1;

  const
    TimeUnits = 10;

  const
    tidClassFamily_Fluids        = 'Fluids';
    tidClassFamily_Blocks        = 'Blocks';
    tidClassFamily_Facilities    = 'Facilities';
    tidClassFamily_FacilityKinds = 'FacilityKinds';
    tidClassFamily_Ordinances    = 'Ordinances';
    tidClassFamily_Inventions    = 'Inventions';
    tidClassFamily_Clusters      = 'Clusters';
    tidCluster_Undefined         = 'General';
    tidFacilityKind_Undefined    = 'General';
    tidPrefix_TaxId              = 'factax_';
    tidTaxId_Land                = 'Land';
    tidTaxId_Money               = 'Money';
    tidTaxName_Land              = 'Land';
    tidTaxName_Money             = 'Money';
    tidEnvironment_Beauty        = 'Beauty';
    tidEnvironment_Pollution     = 'Pollution';
    tidEnvironment_Crime         = 'Crime';
    tidEnvironment_QOL           = 'QOL';
    tidSuperFacKind_Residential  = 'Residentials';
    tidSuperFacKind_Industry     = 'Industries';
    tidSuperFacKind_Public       = 'Public';
    tidSuperFacKind_Headquarter  = 'Headquarters';
    tidSuperFacKind_Farm         = 'Farms';
    tidSuperFacKind_Service      = 'Services';

  const
    facNoTrouble        = $00;
    facNeedsConnection  = $01;
    facNeedsBudget      = $02;
    facStoppedByTycoon  = $04;
    facInsuficientInput = $08;
    facStoppedByAdmin   = $10;
    facNeedsWorkForce   = $20;

    facCriticalTrouble = facNeedsBudget or facStoppedByTycoon; // or facNeedsConnection;

  const
    tcnDescSeparator = #10#13;

  type
    any = 0..0;

  type
    PPercent     = ^TPercent;
    PMoney       = ^TMoney;
    POffset      = ^TOffset;
    PFluidValue  = ^TFluidValue;
    PVariety     = ^TVariety;
    PAdmitance   = ^TAdmitance;
    PQuality     = ^TQuality;
    PObjId       = ^TObjId;
    PVirtDateAbs = ^TVirtDateAbs;
    PVirtDate    = ^TVirtDate;

    TPercent     = byte;
    TMoney       = currency;
    TOffset      = integer;
    TFluidValue  = single;
    TVariety     = integer;
    TAdmitance   = word;
    TQuality     = TPercent;
    TObjId       = pointer;
    TVirtDateAbs = integer;
    TVirtDate    = TDateTime;

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
    PeopleKindName    : array[TPeopleKind] of string = ('Rich people', 'Middle class', 'Workers');
    WorkForceKindName : array[TPeopleKind] of string = ('Directives', 'High-qualified workforce', 'Workers');


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
    TMetaInstance = class;
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

    TCluster = class;
    TCompany = class;
    TTown    = class;
    TTycoon  = class;

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
        function EndOfPeriod       : boolean;
        function PeriodCount       : integer;
      end;

    IMapRefresh =
      interface
        procedure RefreshArea( x, y, dx, dy : integer );
        procedure RefeshFacility( Facility : TFacility; FacilityChange : TFacilityChange );
      end;

    // MetaIntances are objects to be collected in the ClassStorage. They act as
    // classes of other objects. Important features:
    //   Id : string
    //     Class Identifier (must be unique withing one class family).
    //   Register( ClassFamily )
    //     Registers the MetaInstance in the class storage as [ClassFamily, Id]

    TMetaInstance =
      class
        public
          constructor Create( anId : string );
        private
          fId     : string;
          fFamily : string;
          fIndex  : integer;
        published
          property Id     : string  read fId;
          property Family : string  read fFamily;
          property Index  : integer read fIndex;
        public
          procedure Register( ClassFamily : TClassFamilyId );
      end;

    // A FacilityKind groups a set of MetaFacilities. These facilities have
    // several things in common, like inventions.

    TFacilityKind =
      class( TMetaInstance )
        private
          fName        : string;
          fClusterName : string;
          fTechnology  : string;
          fSuperType   : string;
        published
          property Name        : string read fName        write fName;
          property ClusterName : string read fClusterName write fClusterName;
          property Technology  : string read fTechnology  write fTechnology;
          property SuperType   : string read fSuperType   write fSuperType;
      end;

    // Inventions are developed by companies to improve production in general.
    // Each invention has an identifier (byte) that is unique within a
    // FacilityKind.

    TInventionNumId = byte;
    TInventionSet   = set of TInventionNumId;
    TInvention =
      class( TMetaInstance )
        public
          constructor Create( anId, aName, aKind : string );
        private
          fNumId : TInventionNumId;
          fName  : string;
          fKind  : string;
          fDesc  : string;
          fPrice : TMoney;
          fTime  : integer;
        published
          property NumId : TInventionNumId read fNumId;
          property Name  : string          read fName;
          property Kind  : string          read fKind;
          property Desc  : string          read fDesc  write fDesc;
          property Price : TMoney          read fPrice write fPrice;
          property Time  : integer         read fTime  write fTime;
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

    TMetaFacilityOption  = (mfcInTown, mfcTaxeable, mfcCacheable, mfcGenerateName);
    TMetaFacilityOptions = set of TMetaFacilityOption;

    TMetaFacility =
      class( TMetaInstance )
        public
          constructor Create( anId, aName : string; aVisualClass : TVisualClassId; aFacilityClass : CFacility );
          destructor  Destroy; override;
        private
          fName           : string;
          fDesc           : string;
          fEvlStages      : TCollection;
          fFacilityClass  : CFacility;
          fFacilityKind   : TFacilityKind;
          fTypicalStage   : TEvlStage;
          fLevel          : integer;
          fXSize          : integer;
          fYSize          : integer;
          fInTown         : boolean;
          fCluster        : TCluster;
          fFairTax        : TPercent;
          fOptions        : TMetaFacilityOptions;
          fVisualClass    : TVisualClassId;
          fTechnologyName : string;
        private
          function  EstimatePrice : TMoney;
          function  GetClusterName : string;
          procedure SetClusterName( aName : string );
          function  GetFacilityKind : string;
          procedure SetFacilityKind( aName : string );
          function  GetTaxId : string;
        published
          property Name           : string               read fName           write fName;
          property Desc           : string               read fDesc           write fDesc;
          property EvlStages      : TCollection          read fEvlStages;
          property FacilityClass  : CFacility            read fFacilityClass;
          property FacilityKind   : string               read GetFacilityKind write SetFacilityKind;
          property Kind           : TFacilityKind        read fFacilityKind;
          property TypicalStage   : TEvlStage            read fTypicalStage   write fTypicalStage;
          property Level          : integer              read fLevel          write fLevel;
          property XSize          : integer              read fXSize          write fXSize;
          property YSize          : integer              read fYSize          write fYSize;
          property ClusterName    : string               read GetClusterName  write SetClusterName;
          property TechnologyName : string               read fTechnologyName write fTechnologyName;
          property FairTax        : TPercent             read fFairTax        write fFairTax;
          property Options        : TMetaFacilityOptions read fOptions        write fOptions;
          property VisualClass    : TVisualClassId       read fVisualClass;
          property Price          : TMoney               read EstimatePrice;
          property TaxId          : string               read GetTaxId;
        public
          function Instantiate : TFacility; virtual;
        private
          procedure EvlStagesModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
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
          constructor Create( anId        : string;
                              aBlockClass : CBlock );
          destructor  Destroy; override;
        private
          fBlockClass     : CBlock;
          fMetaInputs     : TNotifiedCollection;
          fMetaOutputs    : TNotifiedCollection;
          fxSize          : integer;
          fySize          : integer;
          fBeauty         : TSurfaceValue;
          fBeautyStrength : TSurfaceValue;
        published
          property BlockClass     : CBlock              read fBlockClass;
          property MetaInputs     : TNotifiedCollection read fMetaInputs;
          property MetaOutputs    : TNotifiedCollection read fMetaOutputs;
          property xSize          : integer             read fxSize write fxSize;
          property ySize          : integer             read fySize write fySize;
          property Beauty         : TSurfaceValue       read fBeauty write fBeauty;
          property BeautyStrength : TSurfaceValue       read fBeautyStrength write fBeautyStrength;
        private
          function GetMetaInputByName( name : string ) : TMetaInput;
          function GetMetaOutputByName( name : string ) : TMetaOutput;
        public
          property InputByName [name : string] : TMetaInput  read GetMetaInputByName;
          property OutputByName[name : string] : TMetaOutput read GetMetaOutputByName;
        public
          function Instantiate( aFacility : TFacility ) : TBlock; virtual;
        private
          procedure OnMetaInputsModified(Operation : TCollectionOperation; Index : integer; Item : TObject);
          procedure OnMetaOutputsModified(Operation : TCollectionOperation; Index : integer; Item : TObject);
      end;

    TMetaFluidOption  = (mfTradeable, mfImportable, mfConstruction, mfWorkForce, mfPeople);
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
                              aMarketPrice : TMoney );
        private
          fName        : string;
          fDescription : string;
          fUnitName    : string;
          fFluidName   : string;
          fFluidFact   : TFluidValue;
          fTransCost   : TMoney;
          fMarketPrice : TMoney;
          fOptions     : TMetaFluidOptions;
        published
          property Name        : string  read fName;
          property Description : string  read fDescription;
          property FluidName   : string  read fFluidName;
          property UnitName    : string  read fUnitName;
          property TransCost   : TMoney  read fTransCost;
          property MarketPrice : TMoney  read fMarketPrice;
          property Options     : TMetaFluidOptions read fOptions write fOptions;
        public
          function FormatValue   ( Value : TFluidValue ) : string;
          function FormatValueAbs( Value : TFluidValue ) : string;
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
        published
          property Level       : TMetaInputLevel read fLevel write fLevel;
          property MinFluid    : TInputData      read fMinFluid;
          property MaxFluid    : TInputData      read fMaxFluid;
          property DefFluid    : TInputData      read fDefFluid;
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

    TBlockModification = (bmEvolve);
    TOnBlockModified   = procedure( Modification : TBlockModification ) of object;
    TMoneyReason       = (mnConstruction, mnSupplies, mnMaintenance, mnSalaries, mnTaxes, mnResearch, mnOtherExpenditures, mnProductSales, mnServiceSales, mnOtherIcomes);
    TPeriodMoney       = array[TMoneyReason] of TMoney;

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

    TFacility =
      class( TLockable )
        private
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
          fTrouble         : byte;
        private
          function  GetName : string;
          procedure SetName( aName : string );
          procedure SetCompany( aCompany : TCompany );
          procedure SetTown( aTown : TTown );
          function  GetBudget : TMoney;
          function  GetCriticalTrouble : boolean;
          function  GetStopped : boolean;
          procedure SetStopped( Value : boolean );
        protected
          function GetVisualClassId : TVisualClassId; virtual;
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
          property Budget          : TMoney         read GetBudget;
          property PeriodMoney     : TMoney         read fPeriodMoney;
          property CriticalTrouble : boolean        read GetCriticalTrouble;
          property Trouble         : byte           read fTrouble write fTrouble;
          property Stopped         : boolean        read GetStopped write SetStopped;
          property VisualClass     : TVisualClassId read GetVisualClassId;
        public
          procedure Simulate;
          procedure EndOfPeriod( PeriodCount : integer );
        published
          procedure RDOConnectInput    ( FluidId, Suppliers : widestring );
          procedure RDOConnectOutput   ( FluidId, Clients   : widestring );
          procedure RDODisconnectInput ( FluidId, Suppliers : widestring );
          procedure RDODisconnectOutput( FluidId, Clients   : widestring );
          function  RDOGetConnectionReport : OleVariant;
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
        public
          procedure ReportTrouble( aTrouble : byte );
          function  AccessLevelOf( Tycoon : TTycoon ) : TAccessLevel;
        private
          procedure BlockModified( Modification : TBlockModification );
          function  NewBlock : TBlock;
        protected
          procedure GenMoney( Money : TMoney; Reason : TMoneyReason );
        published
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; virtual;
          property StatusText[kind : TStatusKind; ToTycoon : TTycoon] : string read GetStatusText;
        public
          procedure Cache;
          procedure UnCache;
          procedure UpdateCache;
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
          destructor  Destroy; override;
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
          fInputs  : PGateArray;
          fOutputs : PGateArray;
        private
          function GetInput ( index : integer ) : TInput;
          function GetOutput( index : integer ) : TOutput;
          function GetInputByName ( name : string ) : TInput;
          function GetOutputByName( name : string ) : TOutput;
          function GetInputCount  : integer;
          function GetOutputCount : integer;
        public
          property Inputs [index : integer] : TInput  read GetInput;
          property Outputs[index : integer] : TOutput read GetOutput;
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
        public
          function  PreEvaluate  : TEvaluationResult; virtual;
          function  Evaluate     : TEvaluationResult; virtual;
          function  PostEvaluate : TEvaluationResult; virtual;
          procedure CollectInputs;                    virtual;
          procedure SpreadOutputs;                    virtual;
          procedure ResetInputs;                      virtual;
          procedure AutoConnect;                      virtual;
        public
          procedure StoreToCache( Cache : TObjectCache ); virtual;
          procedure RemoveFromCache;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        protected
          procedure BlockModified( Modification : TBlockModification );
        public
          procedure BlockGenMoney( Money : TMoney; Reason : TMoneyReason );
        protected
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; virtual;
          function GetArea( Ratio : integer; mode : TAreaMode ) : TRect;
        private
          fOffsetRef : record end;
        protected
          function Offset( var Field ) : TOffset;
        private
          procedure CleanExtraInfo;
      end;

    TConnectResult = (cnxValid, cnxRejected, cnxDuplicated);

    PExtraConnectionInfo = ^TExtraConnectionInfo;
    TExtraConnectionInfo =
      record
        LastFluid : TFluidValue;
        YearValue : TFluidValue;
        YearCost  : TMoney;
      end;

    TGate =
      class( TPersistent )
        protected
          constructor Create( aMetaGate : TMetaGate; aBlock : TBlock ); virtual;
          destructor  Destroy; override;
        private
          fFluidData   : PFluidData;
          fBlock       : TBlock;
          fConnections : TCollection;
          fOptions     : TGateAutoconnectOptions;
        private
          function GetConnectionCount : integer;
        published
          property FluidData       : PFluidData read fFluidData;
          property Block           : TBlock     read fBlock;
          property ConnectionCount : integer    read GetConnectionCount;
          property Options         : TGateAutoconnectOptions read fOptions write fOptions;
        protected
          procedure SetFluidData( aFluidData : PFluidData ); virtual;
          procedure InsertConnection( Connection : TGate ); virtual;
          procedure DeleteConnection( Connection : TGate ); virtual;
          procedure ConnectionChanged( Connection : TGate ); virtual;
          function  GetConnectionPrecedence( Connection : TGate ) : integer; virtual;
          function  ConnectionAllowed( Connection : TGate ) : TConnectResult; virtual;
          function  GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo; virtual;
        public
          property ExtraConnectionInfo[index : integer] : PExtraConnectionInfo read GetExtraConnectionInfo;
        public
          function  ConnectTo( Gate : TGate ) : TConnectResult;
          procedure DisconnectFrom( Gate : TGate );
        protected
          procedure PreEvaluate; virtual;
          procedure Evaluate; virtual;
          procedure PostEvaluate; virtual;
        protected
          class function BestCollection( aSize : integer ) : TCollection; virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        private
          procedure HookFluidData( aMetaGate : TMetaGate );
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
        published
          property MetaInput      : TMetaInput  read fMetaInput;
          property ActualMaxFluid : PInputData  read GetActualMaxFluid;
          property MaxCapacity    : TFluidValue read fMaxCapacity write fMaxCapacity;
          property Capacity       : TFluidValue read GetCapacity;
        private
          function GetConnection( index : integer ) : TOutput;
        published
          property Connections[index : integer] : TOutput read GetConnection;
        private
          fLastValue : TInputData;
        public
          property LastValue : TInputData read fLastValue;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        protected
          function GetConnectionPrecedence( Connection : TGate ) : integer; override;
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
        private
          function GetConnection( index : integer ) : TInput;
        published
          property Connections[index : integer] : TInput read GetConnection;
        public
          function PriceToDeliver( Value : TFluidValue; Input : TInput ) : TMoney;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        protected
          function GetConnectionPrecedence( Connection : TGate ) : integer; override;
      end;

    TPushInput =
      class( TInput )
        protected
          constructor Create( aMetaGate : TMetaGate; aBlock : TBlock ); override;
        protected
          procedure Evaluate; override;
        private
          fSkipped : boolean;
      end;

    TPullInput =
      class( TInput )
        protected
          procedure Evaluate; override;
        protected
          procedure InsertConnection( Connection : TGate ); override;
          function  GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo; override;
      end;

    TPushOutput =
      class( TOutput )
        protected
          procedure Evaluate; override;
      end;

    TPullOutput =
      class( TOutput )
        protected
          constructor Create( aMetaGate : TMetaGate; aBlock : TBlock ); override;
          destructor  Destroy; override;
        private
          fSlices : TCollection;
        published
          procedure OnConnectionsModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
        protected
          procedure PreEvaluate; override;
          procedure Evaluate; override;
        private
          fQAuxBuff   : TFluidValue;
        protected
          procedure ValuePulled( Value : TFluidValue; Input : TInput; IsSatisfied : boolean ); virtual;
          procedure Slice      ( Value : TFluidValue );                                        virtual;
          function  GetSliceFor( Input : TInput ) : TFluidValue;                               virtual;
        protected
          procedure InsertConnection( Connection : TGate ); override;
          function  GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo; override;
        protected
          class function BestCollection( aSize : integer ) : TCollection; override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TTaxInfo =
      class( TPersistent )
        public
          constructor Create( anId, aName : string; aTax : TPercent );
        private
          fId   : string;
          fName : string;
          fTax  : TPercent;
        published
          property Id   : string   read fId;
          property Name : string   read fName;
          property Tax  : TPercent read fTax write fTax;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
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

    TMoneyDealer =
      class( TLockable )
        private
          fPeriodMoney : TPeriodMoney;
        { >>
        protected
          function GetBudget : TMoney; virtual; abstract;
        published
          property Budget : TMoney read GetBudget;
        }
        public
          property PeriodMoney : TPeriodMoney read fPeriodMoney;
        public
          procedure GenMoney( Money : TMoney; Reason : TMoneyReason ); virtual;
          procedure EndOfPeriod( PeriodCount : integer ); virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TTownId = integer;
    TTown =
      class( TMoneyDealer )
        public
          constructor Create( aName : string; aXPos, aYPos : integer );
          destructor  Destroy; override;
        private
          fId           : TTownId;
          fName         : string;
          fXPos         : integer;
          fYPos         : integer;
          fCreated      : TVirtDate;
          fTimer        : ITimer;
          fMapRefresh   : IMapRefresh;
          fBudget       : TMoney;
          fOrdinanceSet : TOrdinanceSet;
          fOrdinances   : TLockableCollection;
        published
          property Id         : TTownId     read fId;
          property Name       : string      read fName       write fName;
          property xPos       : integer     read fXPos       write fXPos;
          property yPos       : integer     read fYPos       write fYPos;
          property Created    : TVirtDate   read fCreated    write fCreated;
          property Timer      : ITimer      read fTimer      write fTimer;
          property MapRefresh : IMapRefresh read fMapRefresh write fMapRefresh;
          property Budget     : TMoney      read fBudget     write fBudget;
        private
          function GetOrdinance( NumId : TOrdinanceNumId ) : boolean;
        public
          property Ordinance[NumId : TOrdinanceNumId] : boolean read GetOrdinance;
        published
          procedure SetOrdinance( Id : string ); 
          procedure DelOrdinance( Id : string ); 
        private
          fTaxes : TLockableCollection;
        private
          function GetTaxes( TaxId : string ) : TPercent;
        public
          property Taxes[TaxId : string] : TPercent read GetTaxes;
          property AllTaxes : TLockableCollection read fTaxes;
        protected
          procedure GenMoney( Money : TMoney; Reason : TMoneyReason ); override;
          procedure EndOfPeriod( PeriodCount : integer ); override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TCluster =
      class( TMetaInstance )
        private
          fCompany : TCompany;
        public
          property Company : TCompany read fCompany write fCompany;
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
        published
          property Company      : TCompany    read fCompany;
          property Inventions   : TCollection read fInventions;
          property Kind         : string      read fKind;
        private
          function GetHasInvention( NumId : TInventionNumId ) : boolean;
        public
          property HasInvention[NumId : TInventionNumId] : boolean read GetHasInvention;
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
        private
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
          constructor Create( anId : TCompanyId );
          destructor  Destroy; override;
        private
          fOwner             : TTycoon;
          fCluster           : TCluster;
          fName              : string;
          fCreated           : TVirtDate;
          fId                : TCompanyId;
          fAutoConnectLevels : TMetaInputLevels;
          fResearchs         : TLockableCollection;
          fProjects          : TLockableCollection;
          fPrivated          : boolean;
          fMetaFacilityList  : TCompanyMetaFacilityList;
        // >> Wait for Pepe!!!
        protected
          function GetBudget : TMoney;
        published
          property Budget : TMoney read GetBudget;
        published
          property Owner             : TTycoon                  read fOwner   write fOwner;
          property Cluster           : TCluster                 read fCluster write fCluster;
          property Name              : string                   read fName    write fName;
          property Created           : TVirtDate                read fCreated write fCreated;
          property Id                : TCompanyId               read fId;
          property AutoConnectLevels : TMetaInputLevels         read fAutoConnectLevels write fAutoConnectLevels;
          property Researchs         : TLockableCollection      read fResearchs;
          property Projects          : TLockableCollection      read fProjects;
          property Privated          : boolean                  read fPrivated;
          property MetaFacilityList  : TCompanyMetaFacilityList read fMetaFacilityList;
        private
          function GetHasInvention( kind : string; NumId : TInventionNumId ) : boolean;
          function GetResearch( kind : string ) : TCompanyResearch;
          function GetProject( name : string ) : TProject;
        public
          property HasInvention[kind : string; NumId : TInventionNumId] : boolean read GetHasInvention;
          property Research[kind : string] : TCompanyResearch read GetResearch;
          property Project[name : string] : TProject read GetProject;
        public
          procedure DeclareInvention( kind, Id : string );
        protected
          procedure GenMoney( Money : TMoney; Reason : TMoneyReason ); override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TPoliticalRole =
      class( TPersistent )
        protected
          class function IsPersonal    : boolean;  virtual;
                function GetPopularity : TPercent; virtual;
        public
          property Personal   : boolean  read IsPersonal;
          property Popularity : TPercent read GetPopularity;
      end;

    TTycoonId = word;
    TTycoon =
      class( TMoneyDealer )
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fCompanies      : TCollection;
          fName           : string;
          fPassword       : string;
          fBudget         : TMoney;
          fPoliticalRoles : TCollection;
          fFailureLevel   : integer;
          fFocus          : TPoint;
        { >>
        protected
          function GetBudget : TMoney; override;
        }
        private
          function GetFocusX : integer;
          function GetFocusY : integer;
        published
          property Budget : TMoney  read fBudget;
          property FocusX : integer read GetFocusX;
          property FocusY : integer read GetFocusY;
        published
          property Companies    : TCollection  read fCompanies;
          property Name         : string       read fName     write fName;
          property Password     : string       read fPassword write fPassword;
          property FailureLevel : integer      read fFailureLevel;
        public
          procedure AssumePoliticalRole ( Role : TPoliticalRole );
          procedure AbandonPoliticalRole( Role : TPoliticalRole );
        protected
          procedure GenMoney( Money : TMoney; Reason : TMoneyReason ); override;
          procedure EndOfPeriod( PeriodCount : integer ); override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;
    {$M-}

  const
    MoneyReasonId : array[TMoneyReason] of string =
      ( 'mnConstruction',
        'mnSupplies',
        'mnMaintenance',
        'mnSalaries',
        'mnTaxes',
        'mnResearch',
        'mnOtherExpenditures',
        'mnProductSales',
        'mnServiceSales',
        'mnOtherIcomes');


  // Model Extensions

  const
    tidProcName_GetMDXId          = 'ModelExtensionId';
    tidProcName_GetMDXDependances = 'GetDependances';
    tidProcName_RegisterMDX       = 'RegisterModelExtension';

  type
    PGetMDXIdProc = ^TGetMDXIdProc;
    TGetMDXIdProc = function : string;

    PGetMDXDependancesProc = ^TGetMDXDependancesProc;
    TGetMDXDependancesProc = function : string;

    PRegisterMDXProc = ^TRegisterMDXProc;
    TRegisterMDXProc = procedure;

  function  GetMDXId( MDX : THandle ) : string;
  function  GetMDXDependances( MDX : THandle ) : TStringList;
  procedure RegisterMDX( MDX : THandle );


  // Registration of basic stuff

  procedure RegisterBackup;
  procedure RegisterSurfaces;

implementation

  uses
    ClassStorage, Construction, ModelServerCache, MathUtils,
    StrUtils, SysUtils, Population, SimHints;

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
      absFluidData.Q := Reader.ReadSingle( name + '.Q' );
      absFluidData.K := Reader.ReadInteger( name + '.K' );
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
      absInputData.S := Reader.ReadInteger( name + '.S' );
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


  // TMetaInstance

  constructor TMetaInstance.Create( anId : string );
    begin
      inherited Create;
      fId := anId;
    end;
    
  procedure TMetaInstance.Register( ClassFamily : TClassFamilyId );
    begin
      fFamily := ClassFamily;
      TheClassStorage.RegisterClass( ClassFamily, fId, self );
      fIndex := TheClassStorage.ClassCount[ClassFamily];
      {$IFNDEF NOCACHE}
      CacheMetaObject( self );
      {$ENDIF}
    end;



  // TInvention

  constructor TInvention.Create( anId, aName, aKind : string );
    begin
      inherited Create( anId );
      fName := aName;
      fKind := aKind;
    end;


  // TMetaFacility

  constructor TMetaFacility.Create( anId, aName : string; aVisualClass : TVisualClassId; aFacilityClass : CFacility );
    begin
      inherited Create( anId );
      fName          := aName;
      fFacilityClass := aFacilityClass;
      fEvlStages     := TNotifiedCollection.Create( 0, rkBelonguer );
      fInTown        := true;
      fFairTax       := 10;
      fOptions       := [mfcInTown, mfcTaxeable, mfcGenerateName];
      fVisualClass   := aVisualClass;
      TNotifiedCollection(fEvlStages).OnModified := EvlStagesModified;
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

  function TMetaFacility.GetTaxId : string;
    begin
      result := tidPrefix_TaxId + Id;
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

  constructor TMetaBlock.Create( anId : string; aBlockClass : CBlock );
    begin
      inherited Create( anId );
      fBlockClass  := aBlockClass;
      fMetaInputs  := TNotifiedCollection.Create( 0, rkBelonguer );
      fMetaOutputs := TNotifiedCollection.Create( 0, rkBelonguer );
      fMetaInputs.OnModified  := OnMetaInputsModified;
      fMetaOutputs.OnModified := OnMetaOutputsModified;
    end;

  destructor TMetaBlock.Destroy;
    begin
      fMetaInputs.Free;
      fMetaOutputs.Free;
      inherited;
    end;

  function TMetaBlock.GetMetaInputByName( name : string ) : TMetaInput;
    var
      i : integer;
    begin
      i := 0;
      while (i < fMetaInputs.Count) and (TMetaInput(fMetaInputs[i]).Name <> name) do
        inc( i );
      if i < fMetaInputs.Count
        then result := TMetaInput(fMetaInputs[i])
        else result := nil;
    end;

  function TMetaBlock.GetMetaOutputByName( name : string ) : TMetaOutput;
    var
      i : integer;
    begin
      i := 0;
      while (i < fMetaOutputs.Count) and (TMetaOutput(fMetaOutputs[i]).Name <> name) do
        inc( i );
      if i < fMetaOutputs.Count
        then result := TMetaOutput(fMetaOutputs[i])
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


  // TMetaFluid

  constructor TMetaFluid.Create( anId,
                                 aName,
                                 aDescription : string;
                                 aUnitName    : string;
                                 aFluidName   : string;
                                 aFluidFact   : TFluidValue;
                                 aTransCost   : TMoney;
                                 aMarketPrice : TMoney );
    begin
      inherited Create( anId );
      fName        := aName;
      fUnitName    := aUnitName;
      fFluidName   := aFluidName;
      fFluidFact   := aFluidFact;
      fTransCost   := aTransCost;
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

  function TMetaInput.Instantiate( aBlock : TBlock ) : TGate;
    begin
      result := inherited Instantiate( aBlock );
      PInputData(TInput(result).FluidData)^ := DefFluid;
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
      fCurrBlock.Free;
      inherited;
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
        fName := aName;
      finally
        Unlock;
      end;
    end;
    
  procedure TFacility.SetCompany( aCompany : TCompany );
    begin
      fCompany := aCompany;
      if mfcGenerateName in MetaFacility.Options
        then fName := MetaFacility.Name + ' ' + IntToStr(fCompany.MetaFacilityList.GetNextFacilityNumber(MetaFacility));
    end;
    
  procedure TFacility.SetTown( aTown : TTown );
    begin
      Lock;
      try
        fTown := aTown;
        if fTown <> nil
          then CurrBlock.AutoConnect;
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
        UpdateCache;
      finally
        Unlock;
      end;
    end;

  function TFacility.GetVisualClassId : TVisualClassId;
    begin
      Lock;
      try
        result := MetaFacility.fVisualClass + fCurrStage;
      finally
        Unlock;
      end;
    end;
    
  procedure TFacility.Simulate;
    begin
      Lock;
      try
        if Trouble and facStoppedByTycoon <> 0
          then Trouble := facStoppedByTycoon
          else Trouble := facNoTrouble;
        fCurrBlock.Simulate;
      finally
        Unlock;
      end;
    end;
    
  procedure TFacility.EndOfPeriod( PeriodCount : integer );
    var
      Tax : TMoney;
    begin
      Lock;
      try
        CurrBlock.CleanExtraInfo;
        if mfcTaxeable in MetaFacility.Options
          then
            begin
              if fMoneyGraph = nil
                then fMoneyGraph := TPlotter.Create;
              fMoneyGraph.Plot( 0, round(fPeriodMoney/1000), round(fPeriodMoney/1000) );
              if fPeriodMoney > 0
                then Tax := Town.Taxes[MetaFacility.TaxId]*fPeriodMoney/100
                else Tax := 0;
              Tax := Tax + Town.Taxes[tidTaxId_Land]*(MetaFacility.xSize*MetaFacility.ySize)*100;
              GenMoney( -Tax, mnTaxes );
              Town.GenMoney( Tax, mnTaxes );
              fPeriodMoney := 0;
            end;
      finally
        Unlock;
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
      ModifyConnection( FluidId, Suppliers, true, true );
    end;
    {
    var
      Input    : TInput;
      Supplier : TFacility;
      Output   : TOutput;
      Gates    : TCollection;
      i        : integer;
    begin
      try
        Lock;
        try
          Input := CurrBlock.InputsByName[FluidId];
          if Input <> nil
            then
              begin
                Gates := ParseGateList( Suppliers );
                if Gates <> nil
                  then
                    for i := 0 to pred(Gates.Count) do
                      with TGateDesc(Gates[i]) do
                        begin
                          Supplier := TInhabitedTown(Town).World.FacilityAt( x, y ); // >> PATCH!!! Fix this later!
                          if Supplier <> nil
                            then
                              begin
                                Output := Supplier.CurrBlock.OutputsByName[FluidId];
                                if Output <> nil
                                  then Input.ConnectTo( Output );
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
    }

  procedure TFacility.RDOConnectOutput( FluidId, Clients : widestring );
    begin
      ModifyConnection( FluidId, Clients, true, false );
    end;

  procedure TFacility.RDODisconnectInput( FluidId, Suppliers : widestring );
    begin
      ModifyConnection( FluidId, Suppliers, false, true );
    end;
    {
    var
      Input    : TInput;
      Gates    : TCollection;
      Supplier : TFacility;
      Output   : TOutput;
      i        : integer;
    begin
      try
        Lock;
        try
          Input := CurrBlock.InputsByName[FluidId];
          if Input <> nil
            then
              begin
                Gates := ParseGateList( Suppliers );
                if Gates <> nil
                  then
                    for i := 0 to pred(Gates.Count) do
                      with TGateDesc(Gates[i]) do
                        begin
                          Supplier := TInhabitedTown(Town).World.FacilityAt( x, y ); // >> PATCH!!! Fix this later!
                          if Supplier <> nil
                            then
                              begin
                                Output := Supplier.CurrBlock.OutputsByName[FluidId];
                                if Output <> nil
                                  then Input.DisconnectFrom( Output );
                              end;
                        end;
              end;
        finally
          Unlock;
        end;
      except
      end;
    end;
    }

  procedure TFacility.RDODisconnectOutput( FluidId, Clients : widestring );
    begin
      ModifyConnection( FluidId, Clients, false, false );
    end;

  function TFacility.RDOGetConnectionReport : OleVariant;
    var
      i, j   : integer;
      report : widestring;
      Gate   : TGate;
    begin
      try
        Lock;
        try
          with CurrBlock do
            begin
              report := IntToStr(OutputCount) + LineBreak;
              for i := 0 to pred(OutputCount) do
                begin
                  report := report + IntToStr(Outputs[i].ConnectionCount) + LineBreak;
                  for j := 0 to pred(Outputs[i].ConnectionCount) do
                    begin
                      if [mfWorkForce, mfPeople]*Outputs[i].MetaOutput.MetaFluid.Options <> []
                        then report := report + '0' + LineBreak
                        else report := report + '1' + LineBreak;
                      Gate   := Outputs[i].Connections[j];
                      report := report + IntToStr(Gate.Block.xPos) + LineBreak + IntToStr(Gate.Block.yPos) + LineBreak;
                    end;
                end;
              report := report + IntToStr(InputCount) + LineBreak;
              for i := 0 to pred(InputCount) do
                begin
                  report := report + IntToStr(Inputs[i].ConnectionCount) + LineBreak;
                  for j := 0 to pred(Inputs[i].ConnectionCount) do
                    begin
                      if [mfWorkForce, mfPeople]*Inputs[i].MetaInput.MetaFluid.Options <> []
                        then report := report + '0' + LineBreak
                        else
                          if Inputs[i].MetaInput.Level = mglBasic
                            then report := report + '1' + LineBreak
                            else report := report + '2' + LineBreak;
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
                          DestFac := TInhabitedTown(Town).World.FacilityAt( x, y ); // >> PATCH!!! Fix this later!
                          if DestFac <> nil
                            then
                              begin
                                if Input
                                  then DestGate := DestFac.CurrBlock.OutputsByName[Org]
                                  else DestGate := DestFac.CurrBlock.InputsByName[Org];
                                if DestGate <> nil
                                  then
                                    if Connect
                                      then OrgGate.ConnectTo( DestGate )
                                      else OrgGate.DisconnectFrom( DestGate )
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
        inc( fFocusCount );
      finally
        Unlock;
      end;
    end;

  procedure TFacility.LostFocus;
    begin
      Lock;
      try
        dec( fFocusCount );
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

  function TFacility.AccessLevelOf( Tycoon : TTycoon ) : TAccessLevel;
    begin
      if (Tycoon = Company.Owner) or not Company.Privated
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
    begin
      Lock;
      try
        case Modification of
          bmEvolve :
            begin
              UnCache;
              fCurrBlock.Free;
              inc( fCurrStage );
              fCurrBlock := NewBlock;
              fCurrBlock.AutoConnect;
              Cache;
              Town.MapRefresh.RefreshArea( xPos, yPos, MetaFacility.xSize, MetaFacility.ySize );
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
        if Company <> nil
          then Company.GenMoney( Money, Reason );
        if Reason <> mnTaxes
          then fPeriodMoney := fPeriodMoney + Money;
      finally
        Unlock;
      end;
    end;

  function TFacility.NewBlock : TBlock;
    begin
      result := TEvlStage(MetaFacility.EvlStages[fCurrStage]).MetaBlock.Instantiate( self );
    end;

  function TFacility.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    var
      StatusStr : string;
    begin
      Lock;
      try
        result := '';
        case kind of
          sttMain :
            begin
              if Company <> nil
                then result := Name + tcnDescSeparator + Company.Name
                else result := Name;
              StatusStr := fCurrBlock.GetStatusText( kind, ToTycoon );
              if StatusStr <> ''
                then result := result + tcnDescSeparator + StatusStr;
            end;
          sttSecondary :
            begin
              if system.pos( MetaFacility.Name, Name ) = 0 
                then result := MetaFacility.Name
                else result := '';
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
              if (Company <> nil) and Company.Privated
                then result := result + '   $' + FormatMoney(IntToStr(round(fPeriodMoney))) + ' this year.';
            end;
          sttHint :
            begin
              result := fCurrBlock.GetStatusText( kind, ToTycoon );
              if result = ''
                then
                  begin
                    if AccessLevelOf( ToTycoon ) = acsGuest
                      then
                        if Company.Owner <> nil
                          then result := GetHintText( hidHintsDenied, [Company.Owner.Name] )
                          else result := GetHintText( hidHintsDenied, [Company.Cluster.Id] )
                      else result := GetHintText( hidVisitWebSite, [0] );
                  end;
            end;
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

  procedure TFacility.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fName := Reader.ReadString( 'Name' );
      fCreationDate := Reader.ReadInteger( 'CreationDate' );
      fXPos := Reader.ReadInteger( 'xPos' );
      fYPos := Reader.ReadInteger( 'yPos' );
      Reader.ReadObject( 'Company', fCompany );
      Reader.ReadObject( 'Town', fTown );
      fMetaFacility := TMetaFacility(TheClassStorage.ClassById[tidClassFamily_Facilities, Reader.ReadString( 'MetaFacility' )]);
      fCurrStage    := Reader.ReadInteger( 'CurrentStage' );
      Reader.ReadObject( 'CurrBlock', fCurrBlock );
      Reader.ReadObject( 'MoneyGraph', fMoneyGraph );
      fPeriodMoney := Reader.ReadCurrency( 'PeriodMoney' );
      fTrouble     := Reader.ReadByte( 'Trouble' );
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
      fMetaBlock := aMetaBlock;
      fFacility  := aFacility;
      getmem( fInputs, aMetaBlock.MetaInputs.Count*sizeof(fInputs[0]) );
      getmem( fOutputs, aMetaBlock.MetaOutputs.Count*sizeof(fOutputs[0]) );
      for i := 0 to pred(aMetaBlock.MetaInputs.Count) do
        fInputs[i] := TMetaGate(aMetaBlock.MetaInputs[i]).Instantiate( self );
      for i := 0 to pred(aMetaBlock.MetaOutputs.Count) do
        fOutputs[i] := TMetaGate(aMetaBlock.MetaOutputs[i]).Instantiate( self );
    end;

  destructor TBlock.Destroy;

    procedure FreeGateArray( GateArray : PGateArray; Size : integer );
      var
        i : integer;
      begin
        for i := 0 to pred(Size) do
          GateArray[i].Free;
        freemem( GateArray, Size*sizeof(GateArray[0]) );
      end;

    begin
      FreeGateArray( fInputs, InputCount );
      FreeGateArray( fOutputs, OutputCount );
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
      result := xPos + MetaBlock.xSize div 2;
    end;

  function TBlock.GetYOrigin : integer;
    begin
      result := yPos + MetaBlock.ySize div 2;
    end;

  function TBlock.GetInput( index : integer ) : TInput;
    begin
      result := TInput(fInputs[index]);
    end;

  function TBlock.GetOutput( index : integer ) : TOutput;
    begin
      result := TOutput(fOutputs[index]);
    end;

  function TBlock.GetInputByName( name : string ) : TInput;
    var
      i : integer;
    begin
      i := 0;
      while (i < InputCount) and (TMetaInput(fMetaBlock.MetaInputs[i]).Name <> name) do
        inc( i );
      if i < InputCount
        then result := Inputs[i]
        else result := nil;
    end;
    
  function TBlock.GetOutputByName( name : string ) : TOutput;
    var
      i : integer;
    begin
      i := 0;
      while (i < OutputCount) and (TMetaOutput(fMetaBlock.MetaOutputs[i]).Name <> name) do
        inc( i );
      if i < OutputCount
        then result := Outputs[i]
        else result := nil;
    end;
    
  function TBlock.GetInputCount : integer;
    begin
      result := fMetaBlock.MetaInputs.Count;
    end;
    
  function TBlock.GetOutputCount : integer;
    begin
      result := fMetaBlock.MetaOutputs.Count;
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

    procedure PreEvaluateOutputs;
      var
        i : integer;
      begin
        for i := 0 to pred(OutputCount) do
          Outputs[i].PreEvaluate;
      end;

    begin
      PreEvaluateOutputs;
      CollectInputs;
      case PreEvaluate of
        evrNormal :
          case Evaluate of
            evrNormal :
              case PostEvaluate of
                evrNormal :
                  begin
                    SpreadOutputs;
                    ResetInputs;
                  end;
                evrEvolve :
                  Facility.BlockModified( bmEvolve );
              end;
            evrEvolve :
              Facility.BlockModified( bmEvolve );
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

  function TBlock.PreEvaluate  : TEvaluationResult;
    begin
      result := evrNormal;
    end;

  function TBlock.Evaluate : TEvaluationResult;
    begin
      result := evrNormal;
    end;

  function TBlock.PostEvaluate : TEvaluationResult;
    begin
      result := evrNormal;
    end;

  procedure TBlock.CollectInputs;
    var
      i : integer;
    begin
      for i := 0 to pred(InputCount) do
        begin
          Inputs[i].PreEvaluate;
          Inputs[i].Evaluate;
        end;
    end;

  procedure TBlock.SpreadOutputs;
    var
      i : integer;
    begin
      for i := 0 to pred(OutputCount) do
        begin
          Outputs[i].Evaluate;
          Outputs[i].PostEvaluate;
        end;
    end;

  procedure TBlock.ResetInputs;
    var
      i : integer;
    begin
      for i := 0 to pred(InputCount) do
        begin
          {$IFNDEF RELEASE}
          Inputs[i].fLastValue := PInputData(Inputs[i].FluidData)^;
          {$ENDIF}
          Inputs[i].PostEvaluate;
          Inputs[i].FluidData.Q := 0;
        end;
    end;

  procedure TBlock.AutoConnect;
    begin
    end;
       
  procedure TBlock.StoreToCache( Cache : TObjectCache );
    var
      i : integer;
    begin
      with Cache do
        begin
          for i := 0 to pred(InputCount) do
            if mgoptCacheable in Inputs[i].MetaInput.Options
              then CacheObject( Inputs[i] );
          for i := 0 to pred(OutputCount) do
            if mgoptCacheable in Outputs[i].MetaOutput.Options
              then CacheObject( Outputs[i] );
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

  procedure TBlock.LoadFromBackup( Reader : IBackupReader );
    var
      count : integer;
      i     : integer;
    begin
      inherited;
      fMetaBlock := TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, Reader.ReadString( 'MetaBlock' )]);
      Reader.ReadObject( 'Facility', fFacility );

      count := Reader.ReadInteger( 'InputCount' );
      getmem( fInputs, max(fMetaBlock.MetaInputs.Count, count)*sizeof(fInputs[0]) );
      for i := 0 to pred(count) do
        Reader.ReadObject( 'Input' + IntToStr(i), fInputs[i] );
      for i := count to pred(fMetaBlock.MetaInputs.Count) do
        fInputs[i] := TMetaGate(fMetaBlock.MetaInputs[i]).Instantiate( self );

      count := Reader.ReadInteger( 'OutputCount' );
      getmem( fOutputs, max(fMetaBlock.MetaOutputs.Count, count)*sizeof(fOutputs[0]) );
      for i := 0 to pred(count) do
        Reader.ReadObject( 'Output' + IntToStr(i), fOutputs[i] );
      for i := count to pred(fMetaBlock.MetaOutputs.Count) do
        fOutputs[i] := TMetaGate(fMetaBlock.MetaOutputs[i]).Instantiate( self );
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
            xPos + MetaBlock.xSize div 2 - Ratio,
            yPos + MetaBlock.ySize div 2 - Ratio,
            xPos + MetaBlock.xSize div 2 + Ratio,
            yPos + MetaBlock.ySize div 2 + Ratio );
        else
          result := Rect(
            xPos - Ratio,
            yPos - Ratio,
            xPos + MetaBlock.xSize + Ratio,
            yPos + MetaBlock.ySize + Ratio );
      end;
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
      fConnections.Extract( Connection );
      InsertConnection( Connection );
    end;

  function TGate.GetConnectionPrecedence( Connection : TGate ) : integer;
    begin
      if (Block.Facility.Company <> nil) and (Connection.Block.Facility.Company <> nil)
        then
          result :=
            1000*integer(Options and goptSameCompany <> 0)*integer(Connection.Block.Facility.Company = Block.Facility.Company) +
            1000*integer(Options and goptSameTycoon <> 0)*integer(Connection.Block.Facility.Company.Owner = Block.Facility.Company.Owner)
        else result := 0;
    end;

  function TGate.ConnectionAllowed( Connection : TGate ) : TConnectResult;
    begin
      result := cnxValid;
    end;

  function TGate.GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo;
    begin
      result := nil;
    end;

  function TGate.ConnectTo( Gate : TGate ) : TConnectResult;
    begin
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
        else result := cnxDuplicated;
    end;

  procedure TGate.DisconnectFrom( Gate : TGate );
    begin
      Gate.DeleteConnection( self );
      DeleteConnection( Gate );
    end;

  procedure TGate.PreEvaluate;
    begin
    end;

  procedure TGate.Evaluate;
    begin
    end;

  procedure TGate.PostEvaluate;
    begin
    end;

  class function TGate.BestCollection( aSize : integer ) : TCollection;
    begin
      result := TCollection.Create( 0, rkUse );
    end;

  procedure TGate.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Block', fBlock );
      Reader.ReadObject( 'Connections', fConnections );
      fOptions := Reader.ReadByte( 'Options' );
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


  // TInput

  constructor TInput.Create( aMetaGate : TMetaGate; aBlock : TBlock );
    begin
      inherited;
      fMetaInput      := TMetaInput(aMetaGate);
      fActualMaxFluid := TMetaInput(aMetaGate).MaxFluid;
      fMaxCapacity    := TMetaInput(aMetaGate).MaxCapacity;
    end;

  function TInput.GetActualMaxFluid : PInputData;
    begin
      result := @fActualMaxFluid;
    end;

  function TInput.GetCapacity : TFluidValue;
    begin
      if MetaInput.MaxFluid.Q < qIlimited
        then result := realmin(MaxCapacity, realmin(ActualMaxFluid.Q, MetaInput.MaxFluid.Q)*Block.dt)
        else result := qIlimited;
    end;
    
  function TInput.GetConnection( index : integer ) : TOutput;
    begin
      result := TOutput(fConnections[index]);
    end;

  procedure TInput.LoadFromBackup( Reader : IBackupReader );
    var
      buffer : array[0..32] of char; // >>
    begin
      inherited;
      fMetaInput := TMetaInput(fBlock.MetaBlock.MetaInputs[Reader.ReadInteger('MetaInput')]);
      LoadInputData( 'ActualMaxFluid', fActualMaxFluid, Reader );
      fMaxCapacity := Reader.ReadSingle( 'MaxCapacity' );
      LoadInputData( 'LastValue', fLastValue, Reader );
      HookFluidData( fMetaInput );
      if FluidData <> nil
        then Reader.ReadBuffer( 'FluidData', FluidData^, MetaInput.FluidDataSize )
        else Reader.ReadBuffer( 'FluidData', buffer, MetaInput.FluidDataSize );
    end;

  procedure TInput.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteInteger( 'MetaInput', Block.MetaBlock.MetaInputs.IndexOf( fMetaInput ));
      StoreInputData( 'ActualMaxFluid', fActualMaxFluid, Writer );
      Writer.WriteSingle( 'MaxCapacity', fMaxCapacity );
      StoreInputData( 'LastValue', fLastValue, Writer );
      Writer.WriteBuffer( 'FluidData', FluidData^, MetaInput.FluidDataSize );
    end;

  function TInput.GetConnectionPrecedence( Connection : TGate ) : integer;
    var
      Output : TOutput;
    begin
      Output := TOutput(Connection);
      result := inherited GetConnectionPrecedence( Connection ) - round(1000*integer(Options and goptCost <> 0)*Output.PriceToDeliver( 1, self ))
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
      for i := 0 to pred(fConnections.Count) do
        TInput(fConnections[i]).ConnectionChanged( self );
    end;
    
  function TOutput.GetConnection( index : integer ) : TInput;
    begin
      result := TInput(fConnections[index]);
    end;

  function TOutput.PriceToDeliver( Value : TFluidValue; Input : TInput ) : TMoney;
    var
      d : integer;
    begin
      if MetaOutput.MetaFluid <> nil
        then
          begin
            if (Input.Block.XPos <> NoPos) and (Input.Block.YPos <> NoPos)
              then d := dist(Block.XPos, Block.YPos, Input.Block.XPos, Input.Block.YPos)
              else d := 0;
            result := Value*Price + Value*d*MetaOutput.MetaFluid.TransCost;
          end
        else result := 0;
    end;

  procedure TOutput.LoadFromBackup( Reader : IBackupReader );
    var
      buffer : array[0..32] of char; // >>
      index  : integer;
    begin
      inherited;
      index := Reader.ReadInteger('MetaOutput');
      if (Block <> nil) and (index < Block.MetaBlock.MetaOutputs.Count)
        then fMetaOutput := TMetaOutput(Block.MetaBlock.MetaOutputs[index]);
      fPricePerc := Reader.ReadInteger( 'Price' );
      HookFluidData( fMetaOutput );
      if FluidData <> nil
        then Reader.ReadBuffer( 'FluidData', FluidData^, MetaOutput.FluidDataSize )
        else Reader.ReadBuffer( 'FluidData', buffer, MetaOutput.FluidDataSize );
    end;

  procedure TOutput.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteInteger( 'MetaOutput', Block.MetaBlock.MetaOutputs.IndexOf( fMetaOutput ));
      Writer.WriteInteger( 'Price', fPricePerc );
      Writer.WriteBuffer( 'FluidData', FluidData^, MetaOutput.FluidDataSize );
    end;

  function TOutput.GetConnectionPrecedence( Connection : TGate ) : integer;
    var
      Input : TInput;
    begin
      Input := TInput(Connection);
      result := inherited GetConnectionPrecedence( Connection ) + round(integer(Options and goptCost <> 0)*PriceToDeliver( 1, Input ));
    end;


  // TPushInput

  constructor TPushInput.Create( aMetaGate : TMetaGate; aBlock : TBlock );
    begin
      inherited;
      PPushInputData(FluidData).S := sIlimited;
    end;

  procedure TPushInput.Evaluate;
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

  procedure TPullInput.Evaluate;
    var
      dQ    : TFluidValue;
      QKsum : TFluidValue;
      Qsum  : TFluidValue;
      i     : integer;
      Cost  : TMoney;
      ExInf : PExtraConnectionInfo;
    begin
      inherited;
      i := 0;
      FluidData.Q := 0;
      while (FluidData.Q <= Capacity) and (i < ConnectionCount) and not (Block.Facility.CriticalTrouble) do
        begin
          dQ := TPullOutput(Connections[i]).GetSliceFor( self );
          if dQ > 0
            then
              begin
                if FluidData.Q + dQ > Capacity
                  then dQ := Capacity - FluidData.Q;
                Cost := Connections[i].PriceToDeliver( dQ, self );
                if Cost > 0
                  then
                    if Block.Facility.Budget > 0
                      then
                        begin
                          ExInf := ExtraConnectionInfo[i];
                          if ExInf <> nil
                            then
                              begin
                                ExInf.LastFluid := dQ/Block.dt;
                                ExInf.YearValue := ExInf.YearValue + dQ;
                                ExInf.YearCost  := ExInf.YearCost + Cost;
                              end;
                          if mfConstruction in MetaInput.MetaFluid.Options
                            then Block.BlockGenMoney( -Cost, mnConstruction )
                            else Block.BlockGenMoney( -Cost, mnSupplies );
                          Connections[i].Block.BlockGenMoney( Cost, mnProductSales );
                        end
                      else
                        begin
                          Block.Facility.ReportTrouble( facNeedsBudget );
                          dQ := 0;
                        end;
                TPullOutput(Connections[i]).fQAuxBuff := dQ;
                FluidData.Q := FluidData.Q + dQ;
              end;
          inc( i );
        end;
      QKsum := 0;
      Qsum  := 0;
      for i := 0 to pred(ConnectionCount) do
        with TPullOutput(Connections[i]) do
          begin
            QKsum := QKsum + fQAuxBuff*FluidData.K;
            Qsum  := Qsum + fQAuxBuff;
            ValuePulled( fQAuxBuff, self, FluidData.Q >= Capacity );
            fQAuxBuff := 0;
          end;
      if Qsum > 0
        then FluidData.K := round(QKsum/Qsum)
        else FluidData.K := 0;
      if FluidData.Q < MetaInput.MinFluid.Q
        then Block.Facility.ReportTrouble( facInsuficientInput );
    end;

  procedure TPullInput.InsertConnection( Connection : TGate );
    var
      CnxVal : integer;
      i      : integer;
    begin
      CnxVal := GetConnectionPrecedence( Connection );
      i := 0;
      while (i < fConnections.Count) and (GetConnectionPrecedence( TGate(fConnections[i]) ) > CnxVal) do
        inc( i );
      fConnections.AtInsert( i, Connection );
    end;

  function TPullInput.GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo;
    var
      InputIdx : integer;
    begin
      try
        InputIdx := Connections[index].fConnections.IndexOf( self );
        result := Connections[index].ExtraConnectionInfo[InputIdx];
      except
        result := nil;
      end;
    end;
    

  // TPushOutput

  procedure TPushOutput.Evaluate;
    var
      Ssum    : integer;
      Q, dQ   : TFluidValue;
      Left    : TFluidValue;
      Actual  : TFluidValue;
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
            if not TPushInput(Connections[i]).fSkipped
              then inc( Ssum, PPushInputData(Connections[i].FluidData).S );
          for i := 0 to pred(ConnectionCount) do
            if not TPushInput(Connections[i]).fSkipped and not Connections[i].Block.Facility.CriticalTrouble
              then
                begin
                  Fluid := PPushInputData(Connections[i].FluidData);
                  if Ssum > 0
                    then dQ := Fluid.S*Q/Ssum
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
                        Cost := PriceToDeliver( dQ, Connections[i] );
                        if Cost > 0
                          then
                            if Connections[i].Block.Facility.Budget > 0
                              then
                                begin
                                  Block.BlockGenMoney( Cost, mnProductSales );
                                  if mfConstruction in MetaOutput.MetaFluid.Options
                                    then Connections[i].Block.BlockGenMoney( -Cost, mnConstruction )
                                    else Connections[i].Block.BlockGenMoney( -Cost, mnSupplies );
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


  // TSlices

  type
    TSlice =
      class
        public
          val       : TFluidValue;
          satisfied : boolean;
        public
          ExtraConnectionInfo : TExtraConnectionInfo;
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

  procedure TPullOutput.PreEvaluate;
    var
      i : integer;
    begin
      inherited;
      for i := 0 to pred(fSlices.Count) do
        with TSlice(fSlices[i]) do
          if satisfied
            then
              begin
                POutputData(FluidData).Extra.Q := POutputData(FluidData).Extra.Q + val;
                val := 0;
                satisfied := false;
              end;
      POutputData(FluidData).Extra.K := FluidData.K;
    end;

  procedure TPullOutput.Evaluate;
    begin
      inherited;
      POutputData(FluidData).Extra.Q := 0;
      Slice( FluidData.Q );
    end;

  procedure TPullOutput.ValuePulled( Value : TFluidValue; Input : TInput; IsSatisfied : boolean );
    var
      Idx : integer;
    begin
      Idx := fConnections.IndexOf( Input );
      with TSlice(fSlices[Idx]) do
        begin
          satisfied := IsSatisfied;
          val := realmin( 0, val - Value );
          if val > 0
            then
              begin
                Slice( val );
                val := 0;
              end;
        end;
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
            if not satisfied and not Connections[i].Block.Facility.CriticalTrouble
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
      result := TSlice(fSlices[Idx]).val;
      {
      if result = 0
        then
          begin
            result := GetExtraSliceFor( Input, Idx );
            TSlice(fSlices[Idx]).val := result;
            POutputData(FluidData).Extra.Q := POutputData(FluidData).Extra.Q - result;
          end;
      }
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
            while (i < fConnections.Count) and (GetConnectionPrecedence( TGate(fConnections[i]) ) > CnxVal) do
              inc( i );
            fConnections.AtInsert( i, Connection );
          end
        else inherited;
    end;

  function TPullOutput.GetExtraConnectionInfo( index : integer ) : PExtraConnectionInfo;
    begin
      try
        result := @(TSlice(fSlices[index]).ExtraConnectionInfo);
      except
        result := nil;
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
          Slice.ExtraConnectionInfo.YearValue := Reader.ReadSingle( 'YearInfo' + IntToStr(i) + '.Value' );
          Slice.ExtraConnectionInfo.YearCost  := Reader.ReadCurrency( 'YearInfo' + IntToStr(i) + '.Cost' );
          fSlices.Insert( Slice );
        end;
      TNotifiedCollection(fConnections).OnModified := OnConnectionsModified;
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
        end;
    end;


  // TTaxInfo

  constructor TTaxInfo.Create( anId, aName : string; aTax : TPercent );
    begin
      inherited Create;
      fId   := anId;
      fName := aName;
      fTax  := aTax;
    end;

  procedure TTaxInfo.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fId   := Reader.ReadString( 'Id' );
      fName := Reader.ReadString( 'Name' );
      fTax  := Reader.ReadByte( 'Tax' );
    end;

  procedure TTaxInfo.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'Id', fId );
      Writer.WriteString( 'Name', fName );
      Writer.WriteByte( 'Tax', fTax );
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


  // TMoneyDealer

  procedure TMoneyDealer.GenMoney( Money : TMoney; Reason : TMoneyReason );
    begin
      fPeriodMoney[Reason] := fPeriodMoney[Reason] + Money;
    end;

  procedure TMoneyDealer.EndOfPeriod( PeriodCount : integer );
    var
      reason : TMoneyReason;
    begin
      for reason := low(reason) to high(reason) do
        fPeriodMoney[reason] := 0;
    end;

  procedure TMoneyDealer.LoadFromBackup( Reader : IBackupReader );
    var
      i : TMoneyReason;
    begin
      inherited;
      for i := low(i) to high(i) do
        fPeriodMoney[i] := Reader.ReadCurrency( 'PeriodMoney.' + MoneyReasonId[i] );
    end;

  procedure TMoneyDealer.StoreToBackup( Writer : IBackupWriter );
    var
      i : TMoneyReason;
    begin
      inherited;
      for i := low(i) to high(i) do
        Writer.WriteCurrency( 'PeriodMoney.' + MoneyReasonId[i], fPeriodMoney[i] );
    end;


  // TTown

  const
    fNextTownId : TTownId = 1;

  constructor TTown.Create( aName : string; aXPos, aYPos : integer );

    procedure InitTaxes;
      var
        count : integer;
        i     : integer;
        MF    : TMetaFacility;
      begin
        count  := TheClassStorage.ClassCount[tidClassFamily_Facilities];
        fTaxes := TLockableCollection.Create( count, rkBelonguer );
        for i := 0 to pred(count) do
          begin
            MF := TMetaFacility(TheClassStorage.ClassByIdx[tidClassFamily_Facilities, i]);
            if mfcTaxeable in MF.Options
              then fTaxes.Insert( TTaxInfo.Create( MF.TaxId, MF.Name, MF.FairTax ));
          end;
        fTaxes.Insert( TTaxInfo.Create( tidTaxId_Land, tidTaxName_Land, 10 ));
        Name := aName;
      end;

    begin
      inherited Create;
      fId := fNextTownId;
      inc( fNextTownId );
      fXPos := aXPos;
      fYPos := aYPos;
      InitTaxes;
      fOrdinances := TLockableCollection.Create( 0, rkUse );
      fOrdinanceSet := [];
    end;

  destructor TTown.Destroy;
    begin
      fOrdinances.Free;
      fTaxes.Free;
      inherited;
    end;

  function TTown.GetOrdinance( NumId : TOrdinanceNumId ) : boolean;
    begin
      result := NumId in fOrdinanceSet;
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

  function TTown.GetTaxes( TaxId : string ) : TPercent;
    var
      i : integer;
    begin
      i := 0;
      while (i < fTaxes.Count) and ((fTaxes[i] as TTaxInfo).Id <> TaxId) do
        inc( i );
      if i < fTaxes.Count
        then result := (fTaxes[i] as TTaxInfo).Tax
        else result := 0;
    end;

  procedure TTown.GenMoney( Money : TMoney; Reason : TMoneyReason );
    begin
      inherited;
      fBudget := fBudget + Money;
    end;

  procedure TTown.EndOfPeriod( PeriodCount : integer );
    var
      i : integer;
    begin
      inherited;
      for i := 0 to pred(fOrdinances.Count) do
        GenMoney( TOrdinance(fOrdinances[i]).Cost, mnOtherExpenditures );
    end;

  procedure TTown.LoadFromBackup( Reader : IBackupReader );
    var
      count : integer;
      i     : integer;
    begin
      inherited;
      fid     := Reader.ReadInteger( 'TownId' );
      fName   := Reader.ReadString( 'Name' );
      xPos    := Reader.ReadInteger( 'xPos' );
      yPos    := Reader.ReadInteger( 'yPos' );
      fBudget := Reader.ReadCurrency( 'Budget' );
      Reader.ReadObject( 'Taxes', fTaxes );
      fOrdinances := TLockableCollection.Create( 0, rkUse );
      count := Reader.ReadInteger( 'OrdinanceCount' );
      for i := 0 to pred(count) do
        SetOrdinance( Reader.ReadString( 'Ordinance[' + IntToStr(i) + ']' ) );
    end;

  procedure TTown.StoreToBackup( Writer : IBackupWriter );
    var
      i : integer;
    begin
      inherited;
      Writer.WriteInteger( 'TownId', fid );
      Writer.WriteString( 'Name', fName );
      Writer.WriteInteger( 'xPos', xPos );
      Writer.WriteInteger( 'yPos', yPos );
      Writer.WriteCurrency( 'Budget', fBudget );
      Writer.WriteLooseObject( 'Taxes', fTaxes );
      Writer.WriteInteger( 'OrdinanceCount', fOrdinances.Count );
      for i := 0 to pred(fOrdinances.Count) do
        Writer.WriteString( 'Ordinance[' + IntToStr(i) + ']', TOrdinance(fOrdinances).Id );
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
      result := NumId in fInventionSet;
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
        count := Reader.ReadInteger( 'InventionCount' );
        for i := 0 to pred(count) do
          begin
            InventionId := Reader.ReadString( 'Invention.' + IntToStr(i) );
            try
              Invention := TInvention(TheClassStorage.ClassById[tidClassFamily_Inventions, InventionId]);
              fInventions.Insert( Invention ); 
            except
            end;
          end;
      end;
      
    begin
      inherited;
      Reader.ReadObject( 'Company', fCompany );
      fKind := Reader.ReadString( 'Kind' );
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
      fName := Reader.ReadString( 'Name' );
      fKind := Reader.ReadWord( 'Kind' );
      Reader.ReadObject( 'Members', fMembers );
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
      fMetaFacility := TMetaFacility(TheClassStorage.ClassById[tidClassFamily_Facilities, Reader.ReadString( 'MetaFacility' )]);
      fCounter := Reader.ReadInteger( 'Counter' );
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
      Reader.ReadObject( 'Company', fCompany );
      Reader.ReadObject( 'InfoList', fInfoList );
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
      fProjects          := TLockableCollection.Create( 0, rkBelonguer );
      fMetaFacilityList  := TCompanyMetaFacilityList.Create( self );
      fPrivated          := true;
    end;

  destructor TCompany.Destroy;
    begin
      fResearchs.Free;
      fProjects.Free;
      fMetaFacilityList.Free;
      inherited;
    end;

  function TCompany.GetBudget : TMoney;
    begin
      if Owner <> nil
        then result := Owner.Budget
        else result := 0;
    end;

  procedure TCompany.GenMoney( Money : TMoney; Reason : TMoneyReason );
    begin
      inherited;
      if Owner <> nil
        then Owner.GenMoney( Money, Reason );
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
      i := 0;
      while (i < fResearchs.Count) and (TCompanyResearch(fResearchs[i]).Kind <> kind) do
        inc( i );
      if i < fResearchs.Count
        then result := TCompanyResearch(fResearchs[i])
        else result := nil;
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
        R.fInventionSet := R.fInventionSet + [I.NumId];
        ModelServerCache.CacheObject( self );
      except
        if R.Inventions.Count = 0
          then fResearchs.Delete( R );
        raise;
      end;
    end;

  procedure TCompany.LoadFromBackup( Reader : IBackupReader );
    var
      ClusterId : string;
    begin
      inherited;
      Reader.ReadObject( 'Owner', fOwner );
      ClusterId := Reader.ReadString( 'Cluster' );
      fCluster := TCluster(TheClassStorage.ClassById[tidClassFamily_Clusters, ClusterId]);
      fName := Reader.ReadString( 'Name' );
      fCreated := Reader.ReadDouble( 'Created' );
      fId := Reader.ReadInteger( 'Id' );
      // >> Reader.ReadSet( 'AutoConnectLevels', fAutoConnectLevels );
      fAutoConnectLevels := [mglBasic];  // >> Fix this!!!
      Reader.ReadObject( 'Researchs', fResearchs );
      fPrivated := Reader.ReadBoolean( 'Privated' );
      Reader.ReadObject( 'MetaFacilityList', fMetaFacilityList );
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
      Writer.WriteBoolean( 'Privated', fPrivated );
      Writer.WriteLooseObject( 'MetaFacilityList', fMetaFacilityList );
    end;


  // TPoliticalRole

  class function TPoliticalRole.IsPersonal : boolean;
    begin
      result := true;
    end;

  function TPoliticalRole.GetPopularity : TPercent;
    begin
      result := 0;
    end;


  // TTycoon

  const
    InitialBudget = 100*1000*1000;


  constructor TTycoon.Create;
    begin
      inherited Create;
      fCompanies      := TCollection.Create( 0, rkUse );
      fPoliticalRoles := TLockableCollection.Create( 0, rkUse );
      fBudget         := InitialBudget;
    end;

  destructor TTycoon.Destroy;
    var
      i : integer;
    begin
      for i := 0 to pred(fPoliticalRoles.Count) do
        if TPoliticalRole(fPoliticalRoles[i]).Personal
          then fPoliticalRoles[i].Free;
      fPoliticalRoles.Free;
      fCompanies.Free;
      inherited;
    end;

  { >>
  function TTycoon.GetBudget : TMoney;
    begin
      result := fBudget;
    end;
  }

  function TTycoon.GetFocusX : integer;
    begin
      result := fFocus.x;
    end;

  function TTycoon.GetFocusY : integer;
    begin
      result := fFocus.y;
    end;

  procedure TTycoon.AssumePoliticalRole( Role : TPoliticalRole );
    begin
      fPoliticalRoles.Insert( Role );
    end;

  procedure TTycoon.AbandonPoliticalRole( Role : TPoliticalRole );
    begin
      fPoliticalRoles.Delete( Role );
    end;

  procedure TTycoon.GenMoney( Money : TMoney; Reason : TMoneyReason );
    begin
      inherited;
      fBudget := fBudget + Money;
    end;

  procedure TTycoon.EndOfPeriod( PeriodCount : integer );
    begin
      inherited;
      if fBudget > 0
        then fFailureLevel := 0
        else inc( fFailureLevel );
    end;

  procedure TTycoon.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Companies', fCompanies );
      fName := Reader.ReadString( 'Name' );
      fPassword := Reader.ReadString( 'Password' );
      fBudget := Reader.ReadCurrency( 'Budget' );
      Reader.ReadObject( 'PoliticalRoles', fPoliticalRoles );
      fFailureLevel := Reader.ReadInteger( 'FailureLevel' );
    end;

  procedure TTycoon.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteLooseObject( 'Companies', fCompanies );
      Writer.WriteString( 'Name', fName );
      Writer.WriteString( 'Password', fPassword );
      Writer.WriteCurrency( 'Budget', fBudget );
      Writer.WriteLooseObject( 'PoliticalRoles', fPoliticalRoles );
      Writer.WriteInteger( 'FailureLevel', fFailureLevel );
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
        then RegisterMDXProc
        else raise Exception.Create( tidProcName_RegisterMDX + ' not found in MDX' );
    end;


  // RegisterSurfaces

  procedure RegisterSurfaces;
    begin
      InitSurfaces;
      TSurface.Create( tidEnvironment_Beauty, 'Beauty' );
      TSurface.Create( tidEnvironment_Pollution, 'Pollution' );
      TSurface.Create( tidEnvironment_Crime, 'Crime' );
      TSurface.Create( tidEnvironment_QOL, 'Comodities' );
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      Persistent.RegisterBackup;
      Plotter.RegisterBackup;
      RegisterClass(TFacility);
      RegisterClass(TBlock);
      RegisterClass(TGate);
      RegisterClass(TInput);
      RegisterClass(TOutput);
      RegisterClass(TTaxInfo);
      RegisterClass(TPushInput);
      RegisterClass(TPullInput);
      RegisterClass(TPushOutput);
      RegisterClass(TPullOutput);
      RegisterClass(TCompanyResearch);
      RegisterClass(TCompanyMetaFacilityInfo);
      RegisterClass(TCompanyMetaFacilityList);
      RegisterClass(TCompany);
      RegisterClass(TTown);
      RegisterClass(TTycoon);
    end;

end.



