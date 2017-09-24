unit Population;

interface

  uses
    Classes, Kernel, World, Collection, Surfaces, BackupInterfaces, Newspapers,
    CacheAgent, Protocol, MetaInstances, Accounts, ConnectedBlock, Languages;

  const
    idWorldPopulator = 0;
    idTownHall       = 1;          

  const                                                        
    tidClassFamily_PublicFacilities = 'PublicFacilities';

  const
    tidFacilityKind_TownHall = 'TownHall';

  const
    TownHallBeautyStrength = 10;

  const                                                                   
    tidBlock_WorldPopulator = 'WorldPopulator';
    tidBlock_TownHall       = 'TownHall';
    tidFacility_TownHall    = 'TownHall';
    tidFluid_People         = 'People';
    tidFluid_Offices        = 'Offices';
    tidFluid_WorkForce      = 'WorkForce';
    tidGate_People          = tidFluid_People;
    tidGate_WorkForceIn     = 'WorkForceIn';
    tidGate_WorkForceOut    = 'WorkForceOut';
    tidGate_Inmigration     = 'Inmigration';
    tidGate_Emigration      = 'Emigration';
    tidGate_TownPerformance = 'TownPerformance';
    tidGate_WorkDemand      = 'WorkDemand';
    tidGate_Workers         = 'Workers';
    tidGate_WorkRecycleIn   = 'WorkRecycleIn';

    // Residential gates
    tidGate_ResInmigration  = 'ResInmigration';
    tidGate_ResEmigration   = 'ResEmigration';
    tidGate_ResDemand       = 'ResDemand';
    tidGate_RecycleIn       = 'RecycleIn';
    tidGate_RecycleOut      = 'RecycleOut';

    // Office gates
    tidGate_Offices         = 'Offices';
    tidGate_OfsInmigration  = 'OfsInmigration';
    tidGate_OfsEmigration   = 'OfsEmigration';
    tidGate_OfsDemand       = 'OfsDemand';
    tidGate_OfsRecycleIn    = 'OfsRecycleIn';
    tidGate_OfsRecycleOut   = 'OfsRecycleOut';

    tidEnvironment_People   = 'People';

  const
    tidFacility_SpontaneousBuilding = 'SpontaneousBuilding';
    tidBlock_SpontaneousBuilding    = 'SpontaneousBuilding';

  type
    TMetaPublicFacilityInfo =
      class( TMetaInstance )
        public
          constructor Create( anId : string );
        private
          fName        : string;
          fName_MLS    : TMultiString;
          fImportance  : integer;
          fSurfaceId   : string;
          fModStrength : integer;
          fModFact     : single;
        published
          property Name        : string       read fName        write fName;
          property Name_MLS    : TMultiString read fName_MLS;
          property Importance  : integer      read fImportance  write fImportance;
          property SurfaceId   : string       read fSurfaceId   write fSurfaceId;
          property ModStrength : integer      read fModStrength write fModStrength;
          property ModFact     : single       read fModFact     write fModFact;
        public
          procedure Register;
        public
          procedure RetrieveTexts( Container : TDictionary ); override;
          procedure StoreTexts   ( Container : TDictionary ); override;
      end;

    TPublicFacilityInfo =
      class
        private
          fKind         : TMetaPublicFacilityInfo;
          fStrength     : integer;
          fLastStrength : integer;
        published
          property Kind     : TMetaPublicFacilityInfo read fKind;
          property Strength : integer                 read fLastStrength;
        public
          procedure LoadFromBackup( Reader : IBackupReader ); virtual;
          procedure StoreToBackup ( Writer : IBackupWriter ); virtual;
      end;

  type
    // Classes defined

    TWorldPopulator = class;
    TTownHall       = class;
    TInhabitedTown  = class;
    TInhabitedWorld = class;

    // Metaclases

    CWorldPopulator = class of TWorldPopulator;
    CTownHall       = class of TTownHall;
    CInhabitedTown  = class of TInhabitedTown;
    CInhabitedWorld = class of TInhabitedWorld;

    TWorldPopulator =
      class( TBlock )
        private
          fInmigration     : TPeopleOutArray;
          fEmigration      : TPeopleInArray;
          fTownPerformance : TPeopleInArray;
          fTotalPopulation : integer;
        protected
          function Evaluate : TEvaluationResult; override;
      end;

    TTownHall =
      class( TConnectedBlock )
        protected
          constructor Create( aMetaBlock : TMetaBlock; aFacility : TFacility ); override;
        public
          destructor  Destroy; override;
        private
          fResDemand      : TPeopleInArray;
          fWorkDemand     : TPeopleInArray;
          fWorkers        : TPeopleInArray;
          fWorkRecycleIn  : TPeopleInArray;
          fInmigration    : TPeopleInArray;
          fPopulation     : TPeopleInArray;
          fRecycleOut     : TPeopleInArray;
          fWorkForceIn    : TPeopleOutArray;
          fWorkForceOut   : TPeopleOutArray;
          fEmigration     : TPeopleOutArray;
          fResInmigration : TPeopleOutArray;
          fResEmigration  : TPeopleOutArray;
          fPerformance    : TPeopleOutArray;
          fRecycleIn      : TPeopleOutArray;

          // Offices
          fOffices        : TPushInputData;
          fOfsInmigration : TOutputData;
          fOfsEmigration  : TOutputData;
          fOfsDemand      : TPushInputData;
          fOfsRecycleIn   : TOutputData;
          fOfsRecycleOut  : TPushInputData;

          // Ads
          fAdvertisement : TPullInputData;
          fLastAds       : TFluidData;
        protected
          function Evaluate : TEvaluationResult; override;
        public
          procedure AutoConnect( loaded : boolean ); override;
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); override;
          procedure StoreToCache( Cache : TObjectCache ); override;
        published
          procedure RDOSetTaxValue( TaxId : integer; Value : widestring );
          procedure RDOSetMinSalaryValue( PopKind, Value : integer );
        private
          fLastPop                 : TPeopleArray;
          fFloaters                : TPeopleArray;
          fInCauseResidentials     : TPeopleArray;
          fInCauseWork             : TPeopleArray;
          fInCauseQOL              : TPeopleArray;
          fOutCauseWork            : TPeopleArray;
          fOutCauseResidentials    : TPeopleArray;
          fOutCauseQOL             : TPeopleArray;
          fOutCauseUnemployment    : TPeopleArray;
          fOutCauseServices        : TPeopleArray;
          fOutCauseDisasters       : TPeopleArray;
          fdayInCauseResidentials  : TPeopleArray;
          fdayInCauseWork          : TPeopleArray;
          fdayInCauseQOL           : TPeopleArray;
          fdayOutCauseWork         : TPeopleArray;
          fdayOutCauseResidentials : TPeopleArray;
          fdayOutCauseQOL          : TPeopleArray;
          fdayOutCauseUnemployment : TPeopleArray;
          fdayOutCauseServices     : TPeopleArray;
          fdayOutCauseDisasters    : TPeopleArray;
          fStoredWorkDemand        : TPeopleArray;
          fStoredResDemand         : TPeopleArray;
          fResMovedOut             : array[TPeopleKind] of TTownParameter;
          fResRent                 : array[TPeopleKind] of TTownParameter;
          fResQidx                 : array[TPeopleKind] of TTownParameter;
          fResPop                  : array[TPeopleKind] of TTownParameter;
          fAvgSalary               : array[TPeopleKind] of TTownParameter;
          fSalRatio                : array[TPeopleKind] of TTownParameter;
          fTotalPop                : TFluidValue;
          fGQOL                    : single;
          fGQOS                    : single;
          fSpontaneousCounter      : integer;
          fSpontaneousBuildings    : TCollection;
          fDisasters               : TCollection;
          fOverallRating           : single;
        private
          function GetPopulation ( kind : TPeopleKind ) : TFluidValue;
          function GetWealth     ( kind : TPeopleKind ) : integer;
          function GetSalary     ( kind : TPeopleKind ) : TPercent;
          function GetSalaryRatio( kind : TPeopleKind ) : single;
          function GetResDemand  ( kind : TPeopleKind ) : TFluidValue;
          function GetWorkDemand ( kind : TPeopleKind ) : TFluidValue;
          function GetResRent    ( kind : TPeopleKind ) : integer;
          function GetResQidx    ( kind : TPeopleKind ) : integer;
        public
          property Population[kind : TPeopleKind] : TFluidValue read GetPopulation;
          property LastPop : TPeopleArray read fLastPop;
          property LastAds : TFluidData   read fLastAds;
          property Wealth[kind : TPeopleKind] : integer  read GetWealth;
          property Salary[kind : TPeopleKind] : TPercent read GetSalary;
          property SalaryRatio[kind : TPeopleKind] : single read GetSalaryRatio;
          property ResDemand[kind : TPeopleKind] : TFluidValue read GetResDemand;
          property WorkDemand[kind : TPeopleKind] : TFluidValue read GetWorkDemand;
          property ResRent[kind : TPeopleKind] : integer read GetResRent;
          property ResQidx[kind : TPeopleKind] : integer read GetResQidx;
          property OverallRating : single read fOverallRating;
        private
          function GetAvgWealth : single;
        published
          property TotalPopulation : TFluidValue read fTotalPop;
          property GQOL            : single      read fGQOL;
          property GQOS            : single      read fGQOS;
          property AvgWealth       : single      read GetAvgWealth;
        private
          fHomeless   : TPeopleArray;
          fUnemployed : TPeopleArray;
        private
          function GetHomelessPerc( kind : TPeopleKind ) : TPercent;
          function GetUnemployment( kind : TPeopleKind ) : TPercent;
        public
          property HomelessPerc[kind : TPeopleKind] : TPercent read GetHomelessPerc;
          property Unemployment[kind : TPeopleKind] : TPercent read GetUnemployment;
        public
          procedure ReportClosedBuilding( aBlock : TBlock );
        private
          fPublicFacilities : TCollection;
          fServiceInfo      : TCollection;
        private
          function GetServiceInfoById(Id : string) : TObject;
        public
          property ServiceInfoById[Id : string] : TObject read GetServiceInfoById;
        private
          function GetPublicFacility( Kind : TMetaPublicFacilityInfo ) : TPublicFacilityInfo;
        public
          property PublicFacilities[Kind : TMetaPublicFacilityInfo] : TPublicFacilityInfo read GetPublicFacility;
        public
          procedure ReportPublicFacility( Kind : TMetaPublicFacilityInfo; aStrength : integer );
        published
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
        private
          fBeautyModifier : TSurfaceModifier;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        private
          procedure InitServiceInfo;
        public
          procedure CreateSpontaneousBuilding;
          procedure DestroySpontaneousBuilding( Building : TFacility ); virtual;
        private
          function GetContextStatusStr( ToTycoon : TTycoon ) : string;
        public
          procedure TimeWarp;
      end;

    TInhabitedTown =
      class( TTown )
        public
          constructor Create( aName : string;
                              aClusterId, aTownHallClass, aTradeCenterClass : string;
                              aXPos, aYPos : integer;
                              aWorld : TInhabitedWorld );
        private
          fTownHall    : TFacility;
          fTradeCenter : TFacility;
          fWorld       : TInhabitedWorld;
          fNewspaper   : TNewspaper;
        published
          property TownHall    : TFacility       read fTownHall;
          property TradeCenter : TFacility       read fTradeCenter;
          property World       : TInhabitedWorld read fWorld;
        public
          function GetContextStatusStr( ToTycoon : TTycoon ) : string; override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure Loaded; override;
      end;

    TInhabitedWorld =
      class( TWorld )
        public
          constructor Create( aName : string; axSize : integer; aySize : integer ); override;
        private
          fPopulator : TFacility;
        protected
          function GetTotalPopulation : integer; override;
        public
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

  const
    tidTownParameter_ResMovedOut    = 'ResMovedOut';
    tidTownParameter_ResRent        = 'ResRent';
    tidTownParameter_ResQidx        = 'ResQidx';
    tidTownParameter_ResPop         = 'ResPop';
    tidTownParameter_Salary         = 'Salary';
    tidTownParameter_SalRatio       = 'SalRatio';
    tidTownParameter_PrivateWorkers = 'PrivateWorkers';

  const   
    InmigrationTimeSlope    = 10*TimeUnits;    // Tendence to move in (newcomers only).
    EmigrationTimeSlope     = 20*TimeUnits;    // Tendence to leave town.
    OfsInmigrationTimeSlope = 5*TimeUnits;     // Tendence for offices to move in
    OfsEmigrationTimeSlope  = 20*TimeUnits;    // Tendence for offices to move out
    OfficesPerInhabitant    = 0.03;            // Offices generated by one inhabitant
    WorkersPerOffice        = 2;               // Workers per office
    RandomEmigrants         = 0.001;           // % of people who leaves town for unknown reasons.
    RandomEmigrationProb    = 20;              // Prob of RandomEmigrants to decide to leave town.
    SelfServRatio           = 0.3;             // Percent of services population is able to create by theirselves
    MaxMovers               = 20;              // Maximum number of people that can move to town in an hour
    MinimalPop              = 1000;

    InResidentialsWeight = 10;
    InWorkWeight         = 300;
    InQOLWeight          = 30;
    InTotalWeight        = InResidentialsWeight + InWorkWeight + InQOLWeight;

    OutWorkWeight         : array[TPeopleKind] of integer = (30, 10, 10);
    OutResidentialsWeight : array[TPeopleKind] of integer = (50, 50, 50);
    OutQOLWeight          : array[TPeopleKind] of integer = (80, 80, 80);
    OutUnemploymentWeight : array[TPeopleKind] of integer = (70, 50, 5);
    OutServicesWeight     : array[TPeopleKind] of integer = (150, 140, 100);

  var
    OutTotalWeight : array[TPeopleKind] of integer;

  const
    EmigrationProb : array[TPeopleKind] of single  = (0.8, 0.38, 0.08);
    WorkersPercent : array[TPeopleKind] of integer = (30, 30, 40);
    FloatDisp      : array[TPeopleKind] of single  = (0.01, 0.02, 0.15);
    BirthProb      : array[TPeopleKind] of single  = (3e-6, 4e-6, 5e-6);
    DeathProb      : array[TPeopleKind] of single  = (1e-6, 1.5e-6, 2e-6);
    QOLvsUnemp     : array[TPeopleKind] of single  = (0.02, 0.06, 0.08);

  const
    PeoplePrice    : array[TPeopleKind] of TMoney = ( 1, 0.7, 0.4);
    WorkForcePrice : array[TPeopleKind] of TMoney = (20,   8,   2);

  function PeopleToWorkForce( PeopleKind : TPeopleKind; P  : TFluidValue ) : TFluidValue;
  function WorkForceToPeople( PeopleKind : TPeopleKind; WF : TFluidValue ) : TFluidValue;


  // Registration

  procedure RegisterTownHall( aClusterName, anId : string; aVisualClass : TVisualClassId; axSize, aySize : integer; aBeauty : TSurfaceValue; BlockClass : CBlock );
  procedure RegisterMetaFluids;
  procedure RegisterMetaInstances;
  procedure RegisterSurfaces;
  procedure RegisterBackup;
  procedure RegisterTownParameters;


implementation

  uses
    SysUtils, ClassStorage, PopulatedBlock, MathUtils, PyramidalModifier, BasicAccounts, Taxes,
    CacheCommon, SimHints, ServiceBlock, ModelServerCache, SpontaneousBuildings, Disasters,
    StdFluids, ServiceInfo, Logs, Events, BasicTaxes;

  // TMetaPublicFacilityInfo

  constructor TMetaPublicFacilityInfo.Create( anId : string );
    begin
      inherited Create( anId );
      fName_MLS := TMultiString.Create;
    end;
    
  procedure TMetaPublicFacilityInfo.Register;                 
    begin
      inherited Register( tidClassFamily_PublicFacilities );  
    end;

  procedure TMetaPublicFacilityInfo.RetrieveTexts( Container : TDictionary );
    begin
      inherited;
      if fName_MLS = nil
        then fName_MLS := TMultiString.Create;
      fName_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Name'];
    end;

  procedure TMetaPublicFacilityInfo.StoreTexts( Container : TDictionary );
    begin
      inherited;
      Container.Values[Family + '.' + Id + '.' + 'Name'] := fName;
    end;


  // TPublicFacilityInfo

  procedure TPublicFacilityInfo.LoadFromBackup( Reader : IBackupReader );
    begin
      fKind := TMetaPublicFacilityInfo(TheClassStorage.ClassById[tidClassFamily_PublicFacilities, Reader.ReadString( 'Kind', '' )]); // ??
      fStrength := Reader.ReadInteger( 'Strength', 0 );
    end;

  procedure TPublicFacilityInfo.StoreToBackup( Writer : IBackupWriter );
    begin
      Writer.WriteString( 'Kind', fKind.Id );
      Writer.WriteInteger( 'Strength', fStrength );
    end;


  // TWorldPopulator

  // The (Kt^2)% of the people that emigrate from one town leave the Map.
  // The (1 - Kt^2)% is redistrubuted to the existing towns.
  // Kt is the "Town Performance" quality.

  function TWorldPopulator.Evaluate : TEvaluationResult;
    var
      i : TPeopleKind;
    begin
      result := inherited Evaluate;
      fTotalPopulation := 0;
      for i := low(i) to high(i) do
        begin
          fInmigration[i].Q := (fTownPerformance[i].K)*(fEmigration[i].Q + fInmigration[i].Extra.Q)/100;
                               //fEmigration[i].Q + fInmigration[i].Extra.Q;
                               //round(sqrt(fTownPerformance[i].K)*(fEmigration[i].Q + fInmigration[i].Extra.Q)/10);
          fInmigration[i].K := fEmigration[i].K;
        end;
    end;


  // TTownHall

  // About the Evaluation:
  // 1. Population quality is the average of the existing population
  //    quality and the inmigration quality.
  // 2. Inmigrants increase population.
  // 3. Newcomers are generated by the town. Unlike inmigration they
  //    come from somewhere outside the map. Newcomers are computed
  //    according to this formula:
  //         Newcomers[i] = K[i]*(Pd[i]/(P[i] + Pd[i]))*(Pd[i]/Td)*dt
  //           where:
  //             i  = Population component = <High|Middle|Low>
  //             K  = Quality of demand = (Kl + Kr[i] + Kw[i])/3
  //               where:
  //                 Kl = General quality of life in town = <internal>
  //                 Kr = Quality of residential demand = <input>
  //                      :: Kr is the average of the Krs from each residential
  //                         in the town (ponderated by the demand value).
  //                         It depends on pollution levels, prices, crime,
  //                         closeness to stores, theatres or to any other facility
  //                         that affects the quality of life.
  //                 Kw = Quality of workforce demand = <input>
  //                      :: Kw is the same than Kr, but this time for work
  //                         centers. It depends mainly on salaries and
  //                         pollution.
  //             Pd = Population demand = min(Pr[i], Pw[i])
  //               where:
  //                 Pr = Residential demand = <input>
  //                 Pw = Workforce demand = <input>
  //             P = Current population = <internal>
  //             Td = Number of days that will take to solve any demand = <constant>
  //                  :: Note that this is not the "actual time" it will take
  //                     to solve de demand. It is only the "tendence" to do it.
  //                     That's why it is a constant.
  //             dt = Number of days where the newcomers are produced.
  // 4. Newcomers increase population.
  // 5. All population go to residentials.
  // 6. 60% of the population is considered as work force.
  // 7. People emigrate from town according to this criteria:
  //      Emigrants[i] = (1 - K[i])*E[i]*(P[i]/Te)*dt + Ex[i]*(Px[i]/Te)*td
  //        where:
  //          E  = disposition to emigrate = <constant>
  //          Te = Same as Td, but to emigrate = <constant>
  //          Ex = disposition of floating people to emigrate = <constant>
  //          Px = Floating population
  // 8. Inmigrants admitance is adjusted according to Kl (Quality of Life in Town)

  function PeopleToWorkForce( PeopleKind : TPeopleKind; P : TFluidValue ) : TFluidValue;
    begin
      result := WorkersPercent[PeopleKind]*P/100;
    end;

  function WorkForceToPeople( PeopleKind : TPeopleKind; WF : TFluidValue ) : TFluidValue;
    begin
      result := 100*WF/WorkersPercent[PeopleKind];
    end;

  constructor TTownHall.Create( aMetaBlock : TMetaBlock; aFacility : TFacility );

    procedure InitDisasters;
      var
        count, i : integer; 
      begin
        try
          count := TheClassStorage.ClassCount[tidClassFamily_Disasters];
        except
          count := 0;
        end;
        for i := 0 to pred(count) do
          with TMetaDisaster(TheClassStorage.ClassByIdx[tidClassFamily_Disasters, i]) do
            fDisasters.Insert( Instantiate( self ) );
      end;

    begin
      inherited;
      fPublicFacilities     := TCollection.Create( 0, rkBelonguer );
      fSpontaneousBuildings := TCollection.Create( 0, rkUse );
      fDisasters            := TCollection.Create( 0, rkBelonguer );
      InitServiceInfo;
      InitDisasters;
    end;

  destructor TTownHall.Destroy;
    begin
      fServiceInfo.Free;
      fPublicFacilities.Free;
      fSpontaneousBuildings.Free;
      fDisasters.Free;
      fBeautyModifier.Delete;
      inherited;
    end;

  function TTownHall.Evaluate : TEvaluationResult;

    function EvaluateDisasters : TDisasterResult;
      var
        i    : integer;
        kind : TPeopleKind;
      begin
        for kind := low(kind) to high(kind) do
          begin
            result.Dead[kind] := 0;
            result.Left[kind] := 0;
          end;
        for i := 0 to pred(fDisasters.Count) do
          with TDisaster(fDisasters[i]).Act( dt ) do
            for kind := low(kind) to high(kind) do
              begin
                result.Dead[kind] := result.Dead[kind] + Dead[kind];
                result.Left[kind] := result.Left[kind] + Left[kind];
              end;
      end;

    procedure ComputeServiceInfo;
      const
        OfferBoost = 1.25;
      var
        i : integer;
        k : TPeopleKind;
      begin
        for i := 0 to pred(fServiceInfo.Count) do
          with TServiceInfo(fServiceInfo[i]) do
            begin
              Demand := 0;
              for k := low(k) to high(k) do
                Demand := Demand + Population[k]*Kind.BuyProb[k];
              Demand := Demand/OfferBoost;
              Offer := Facility.Town.Parameters[Kind.TownParameterIds[tidTownParameter_ServiceSales]].Value;
              if Demand > 0
                then Ratio := Offer/Demand
                else Ratio := 0;
              Capacity := Facility.Town.Parameters[Kind.TownParameterIds[tidTownParameter_ServiceCapacity]].Value;
              try
                Quality := round(Facility.Town.Parameters[Kind.TownParameterIds[tidTownParameter_ServiceQuality]].Average);
                Price   := round(Facility.Town.Parameters[Kind.TownParameterIds[tidTownParameter_ServicePrice]].Average);
              except
              end;
            end;
      end;

    function GetGeneralQualityOfLife : integer;
      var
        i             : integer;
        count         : integer;
        kind          : TMetaPublicFacilityInfo;
        info          : TPublicFacilityInfo;
        CoveredPeople : TFluidValue;
        Importance    : integer;
      begin
        try
          count := TheClassStorage.ClassCount[tidClassFamily_PublicFacilities];
        except
          count := 0;
        end;
        if count > 0
          then
            begin
              CoveredPeople := 0;
              Importance    := 0;
              for i := 0 to pred(count) do
                begin
                  kind := TMetaPublicFacilityInfo(TheClassStorage.ClassByIdx[tidClassFamily_PublicFacilities, i]);
                  info := PublicFacilities[kind];
                  if info <> nil
                    then
                      begin
                        CoveredPeople      := CoveredPeople + realmin(info.Strength, TotalPopulation)*kind.Importance;
                        info.fLastStrength := info.fStrength;
                        info.fStrength     := 0;
                      end;
                  inc( Importance, kind.Importance );
                end;
              if TotalPopulation > 0
                then
                  if CoveredPeople < TotalPopulation*Importance
                    then result := round(100*CoveredPeople/(TotalPopulation*Importance))
                    else result := 100
                else result := 0;
            end
          else result := 100;
      end;

    function GetGeneralQualityOfServices : single;
      const
        OfferWeight    = 100;
        CapacityWeight = 10;
        TotalWeight    = OfferWeight + CapacityWeight;
      var
        sum      : double;
        count    : double;
        i        : integer;
        k        : TPeopleKind;
        ExtRatio : single;
      begin
        sum   := 0;
        count := 0;
        for i := 0 to pred(fServiceInfo.Count) do
           for k := low(k) to high(k) do
              with TServiceInfo(fServiceInfo[i]) do
                begin
                  if Demand > 0
                    then ExtRatio := (OfferWeight*Offer + CapacityWeight*Capacity)/(TotalWeight*Demand)
                    else ExtRatio := 1;
                  sum   := sum + Kind.Importance[k]*Population[k]*realmin(1, ExtRatio);
                  count := count + Kind.Importance[k]*Population[k];
                end;
        if count > 0
          then
            if fTotalPop > MinimalPop
              then result := sum/count
              else result := realmax(SelfServRatio, sum/count)
          else result := 0;
      end;

    function GetCostOfLife( k : TPeopleKind ) : TMoney;
      var
        i : integer;
      begin
        result := 0;
        for i := 0 to pred(fServiceInfo.Count) do
          with TServiceInfo(fServiceInfo[i]) do
            result := result + Kind.BuyProb[k]*Price*Kind.MarketPrice/100;
        // Add cost of life
        result := result + PeoplePrice[k]*(fResRent[k].Average/100);
      end;

    const
      SpontaneousFreq       = 20;
      MinOcuppancy          = 80;
      MaxBuildingsPerPeriod = 1.7;
      MinQuality            = 0.8;
      MaxUnemployment       = 10;

    function SpontaneousBuildingNeeded( Kl, Ks, Kf : single; Unemployment : TPercent ) : integer;
      {
      var
        AvgOcuppancy : integer;
        i            : integer;
      }
      begin
        {
        try
          if fSpontaneousBuildings.Count > 0
            then
              begin
                AvgOcuppancy := 0;
                for i := pred(fSpontaneousBuildings.Count) downto 0 do
                  with TPopulatedBlock(fSpontaneousBuildings[i]) do
                    try
                      inc( AvgOcuppancy, Occupancy );
                    except
                      fSpontaneousBuildings.AtExtract( i );
                    end;
                if fSpontaneousBuildings.Count > 0
                  then AvgOcuppancy := AvgOcuppancy div fSpontaneousBuildings.Count
                  else AvgOcuppancy := 0;
                if AvgOcuppancy > MinOcuppancy
                  then result := max( 1, round(AvgOcuppancy*MaxBuildingsPerPeriod/100) )
                  else result := 0;
              end
            else
              if (Unemployment <= MaxUnemployment) and ((Kl + Ks + (1 - Kf))/3 > MinQuality)
                then result := 1
                else result := 0;
        except
          result := 0;
        end;
        }
        result := 0;
      end;

    procedure CreateSpontaneousBuildings( count : integer );
      var
        i : integer;
      begin
        for i := 0 to pred(count) do
          CreateSpontaneousBuilding;
      end;

    var
      i                    : TPeopleKind;
      Kl                   : integer;
      Kf                   : single;
      Pd                   : TFluidValue;
      ExtraOut             : TFluidData;
      ExtraIn              : TFluidData;
      WorkDemand           : TFluidValue;
      PeopleWhoMoveIn      : TFluidValue;
      PeopleWhoLeave       : TFluidValue;
      ActualPop            : TFluidValue;
      ActualWorkers        : TFluidValue;
      InCauseResidentials  : TFluidValue;
      InCauseWork          : TFluidValue;
      InCauseQOL           : TFluidValue;
      OutCauseWork         : TFluidValue;
      OutCauseResidentials : TFluidValue;
      OutCauseQOL          : TFluidValue;
      OutCauseUnemployment : TFluidValue;
      OutCauseServices     : TFluidValue;
      OutCauseDisasters    : TDisasterResult;
      CostOfLife           : TMoney;
      ActSal               : TMoney;
      AvgSal               : TMoney;
      Ksal                 : single;
      KsalAvg              : single;
      FloatersRatio        : single;
      count                : integer;
      Po                   : TFluidValue;
      PrevSPW              : single;
    begin
      result := inherited Evaluate;
      ComputeServiceInfo;
      OutCauseDisasters := EvaluateDisasters;
      Kl        := GetGeneralQualityOfLife;
      fGQOS     := GetGeneralQualityOfServices;
      fGQOL     := Kl/100;
      fTotalPop := 0;
      KsalAvg   := 0;
      fLastAds.Q := fAdvertisement.Q;
      fLastAds.K := fAdvertisement.K;
      for i := low(i) to high(i) do
        begin
          if abs(fPopulation[i].Q - fResPop[i].Value) > 10*fPopulation[i].Q/100
            then fPopulation[i].Q := fResPop[i].Value;

          // Computing Cost of life
          CostOfLife := GetCostOfLife( i );
          if fPopulation[i].Q > 0
            then ActSal := (fPopulation[i].Q - Unemployment[i]*fPopulation[i].Q/100)*fAvgSalary[i].Average/fPopulation[i].Q
            else ActSal := fAvgSalary[i].Average;
          AvgSal     := ActSal*WorkForcePrice[i]/100;

          // Sending Power
          PrevSPW := fSalRatio[i].Value;
          if CostOfLife > 0
            then
              if PrevSPW > 0
                then fSalRatio[i].CurrValue := IntegrateValues(PrevSPW, AvgSal/CostOfLife, 0.9, 0.1)
                else fSalRatio[i].CurrValue := AvgSal/CostOfLife
            else fSalRatio[i].CurrValue := 1;

          Ksal := realmin( 1, fSalRatio[i].CurrValue );
          KsalAvg := KsalAvg + Ksal;

          // Computing unemployment and floating population
          fUnemployed[i].Q := fWorkForceIn[i].Extra.Q;
          fUnemployed[i].K := fWorkForceIn[i].Extra.K;
          fLastPop[i].Q    := fPopulation[i].Q + fResInmigration[i].Extra.Q;
          fLastPop[i].K    := fPopulation[i].K;
          fFloaters[i]     := fResInmigration[i].Extra;
          if fLastPop[i].Q > 0
            then FloatersRatio := fFloaters[i].Q/fLastPop[i].Q
            else FloatersRatio := 0;

          //Kf := realmin( 1, sqrt((HomelessPerc[i] + Unemployment[i])/(2*100)) );
          Kf := realmin( 1, sqrt(Unemployment[i]/100) );

          // Acepting homeless people
          ExtraIn.K := AverageK( @fPopulation[i], @fHomeless[i] );
          ExtraIn.Q := fHomeless[i].Q;
          fPopulation[i].Q := realmax( 0, fPopulation[i].Q - fHomeless[i].Q );
          fHomeless[i].Q := 0;
          fHomeless[i].K := 0;

          // Computing actual population
          ActualPop := fPopulation[i].Q + fResInmigration[i].Extra.Q + ExtraIn.Q;

          // Acepting Inmigration
          ExtraIn.K := AverageK( @ExtraIn, @fInmigration[i] );
          ExtraIn.Q := ExtraIn.Q + fInmigration[i].Q;

          // Overpopulation control
          ExtraOut.Q := realmax( 0, ExtraIn.Q - fResDemand[i].Q );
          ExtraIn.Q := ExtraIn.Q - ExtraOut.Q;
          ExtraOut.K := ExtraIn.K;

          // Storing info
          fStoredWorkDemand[i].Q := fWorkDemand[i].Q;
          fStoredResDemand[i].Q  := fResDemand[i].Q;

          // Generating newcomers
          WorkDemand := WorkForceToPeople(i, fWorkDemand[i].Q);
          case i of
            pkHigh :
              Pd := realmin( fResDemand[i].Q, 3*WorkDemand );
            pkMiddle :
              Pd := (5*fResDemand[i].Q + 5*WorkDemand)/10;
            else
              Pd := (8*fResDemand[i].Q + 2*WorkDemand)/10;
          end;
          Pd := realmax( 0, Pd - ExtraIn.Q );
          InCauseResidentials := realmin(MaxMovers, (InResidentialsWeight/InTotalWeight)*(fResDemand[i].K/100)*Pd*dt/InmigrationTimeSlope);
          InCauseWork         := realmin(MaxMovers, (InWorkWeight/InTotalWeight)*(1 - sqrt(Kf))*(fWorkDemand[i].K/100)*Pd*dt/InmigrationTimeSlope);
          InCauseQOL          := realmin(MaxMovers, (InQOLWeight/InTotalWeight)*(Kl/100)*Pd*dt/InmigrationTimeSlope);

          fInCauseResidentials[i].Q := fInCauseResidentials[i].Q + InCauseResidentials;
          fInCauseWork[i].Q         := fInCauseWork[i].Q         + InCauseWork;
          fInCauseQOL[i].Q          := fInCauseQOL[i].Q          + InCauseQOL;

          // fResInmigration[i].Q := realmin( Pd, ((Kl + fResDemand[i].K + fWorkDemand[i].K)*Ks*(1 - sqrt(Kf))*Pd*dt)/(300*InmigrationTimeSlope));

          fResInmigration[i].Q :=
            realmin( Pd,
              InCauseResidentials +
              InCauseWork +
              InCauseQOL );

          {
          if ActualPop + Pd > 0
            then fResInmigration[i].Q := realmin( Pd, ((Kl + fResDemand[i].K + fWorkDemand[i].K)*Ks*(1 - sqrt(Kf))*Pd*dt)/(300*InmigrationTimeSlope))
            else fResInmigration[i].Q := 0;
          }

          fResInmigration[i].K := 50;
          fResInmigration[i].K := AverageK( @fResInmigration[i], @ExtraIn );
          fResInmigration[i].Q := fResInmigration[i].Q + ExtraIn.Q;
          PeopleWhoMoveIn      := fResInmigration[i].Q;
          fResInmigration[i].Q := fResInmigration[i].Q + fResInmigration[i].Extra.Q;

          {
          >> DT TEMP CHANGE!
          // People who is born
          fResInmigration[i].Q := fResInmigration[i].Q + (Kl/100)*sqrt(Kf)*BirthProb[i]*fResInmigration[i].Q;

          // People who die
          fResInmigration[i].Q := fResInmigration[i].Q - sqrt(sqrt(Kf))*DeathProb[i]*fResInmigration[i].Q;
          }

          // Generate emigration signal
          OutCauseWork         := realmin(MaxMovers, realmax(0, (OutWorkWeight[i]/OutTotalWeight[i])*(1 - fWorkDemand[i].K*Ksal/100)*EmigrationProb[i]*ActualPop*dt/EmigrationTimeSlope ));
          OutCauseResidentials := realmin(MaxMovers, realmax(0, (OutResidentialsWeight[i]/OutTotalWeight[i])*(1 - fResDemand[i].K/100)*sqr(sqr(1 + FloatersRatio))*EmigrationProb[i]*ActualPop*dt/EmigrationTimeSlope));
          OutCauseQOL          := realmin(MaxMovers, realmax(0, (OutQOLWeight[i]/OutTotalWeight[i])*(1 - Kl/100)*EmigrationProb[i]*ActualPop*dt/EmigrationTimeSlope));
          OutCauseUnemployment := realmin(MaxMovers, realmax(0, (OutUnemploymentWeight[i]/OutTotalWeight[i])*(sqr(Kf) - QOLvsUnemp[i]*Kl/100)*EmigrationProb[i]*ActualPop*dt/EmigrationTimeSlope ));
          OutCauseServices     := realmin(MaxMovers, realmax(0, (OutServicesWeight[i]/OutTotalWeight[i])*sqrt(1 - fGQOS*Ksal)*EmigrationProb[i]*ActualPop*dt/EmigrationTimeSlope));

          fOutCauseWork[i].Q         := fOutCauseWork[i].Q         + OutCauseWork;
          fOutCauseResidentials[i].Q := fOutCauseResidentials[i].Q + OutCauseResidentials + fResMovedOut[i].Value;
          fOutCauseQOL[i].Q          := fOutCauseQOL[i].Q          + OutCauseQOL;
          fOutCauseUnemployment[i].Q := fOutCauseUnemployment[i].Q + OutCauseUnemployment;
          fOutCauseServices[i].Q     := fOutCauseServices[i].Q     + OutCauseServices;
          fOutCauseDisasters[i].Q    := fOutCauseDisasters[i].Q    + OutCauseDisasters.Left[i];

          PeopleWhoLeave :=
            OutCauseWork +
            OutCauseResidentials +
            OutCauseQOL +
            OutCauseUnemployment +
            OutCauseServices +
            OutCauseDisasters.Left[i] +
            OutCauseDisasters.Dead[i];

          fResEmigration[i].Q := realmin( PeopleWhoLeave + fResEmigration[i].Extra.Q, ActualPop );
          fResEmigration[i].K := fPopulation[i].K;

          // Simplify emigration and inmigration
          if fResInmigration[i].Q > fResEmigration[i].Q
            then
              begin
                fResInmigration[i].Q := fResInmigration[i].Q - fResEmigration[i].Q;
                fResEmigration[i].Q  := 0;
              end
            else
              begin
                fResEmigration[i].Q := fResEmigration[i].Q - fResInmigration[i].Q;
                fResInmigration[i].Q := 0;
              end;

          // Maintaining Population Recycle
          fRecycleIn[i].Q := fRecycleOut[i].Q;
          fRecycleIn[i].K := fRecycleOut[i].K;

          // Generate emigration
          fEmigration[i].Q := fResEmigration[i].Q;
          fEmigration[i].K := AverageK( @fEmigration[i], @ExtraOut );
          fEmigration[i].Q := fEmigration[i].Q + ExtraOut.Q;

          // Spreading workforce
          fWorkForceIn[i].Q  := PeopleToWorkForce(i, PeopleWhoMoveIn) + fWorkRecycleIn[i].Q;
          fWorkForceIn[i].K  := fResInmigration[i].K;
          fWorkForceOut[i].Q := PeopleToWorkForce(i, fResEmigration[i].Q);
          fWorkForceOut[i].K := fResEmigration[i].K;
          ActualWorkers      := PeopleToWorkForce( i, ActualPop );
          if fWorkers[i].Q <> ActualWorkers
            then
              if fWorkers[i].Q > ActualWorkers
                then fWorkForceOut[i].Q := fWorkForceOut[i].Q + (fWorkers[i].Q - ActualWorkers)
                else fWorkForceIn[i].Q  := fWorkForceIn[i].Q + (ActualWorkers - fWorkers[i].Q);
          if fWorkForceIn[i].Q > fWorkForceOut[i].Q
            then
              begin
                fWorkForceIn[i].Q  := fWorkForceIn[i].Q - fWorkForceOut[i].Q;
                fWorkForceOut[i].Q := 0;
              end
            else
              begin
                fWorkForceOut[i].Q := fWorkForceOut[i].Q - fWorkForceIn[i].Q;
                fWorkForceIn[i].Q  := 0;
              end;

          // Sending performance
          fPerformance[i].Q := ActualPop;
          if (fResDemand[i].K > 0) and (fWorkDemand[i].K > 0)
            then fPerformance[i].K := (Kl + fResDemand[i].K + fWorkDemand[i].K) div 3
            else fPerformance[i].K := 0;

          // Generate spontaneous buildings
          if i = pkLow
            then
              begin
                if fSpontaneousCounter = 0
                  then
                    begin
                      count := SpontaneousBuildingNeeded( fGQOL, fGQOS, Kf, Unemployment[i] );
                      CreateSpontaneousBuildings( count );
                      fSpontaneousCounter := SpontaneousFreq;
                    end
                  else fSpontaneousCounter := max( 0, fSpontaneousCounter - round(dt) );
              end;

          fTotalPop := fTotalPop + ActualPop;
          fLastPop[i].Q := ActualPop;
          fLastPop[i].K := fPopulation[i].K;
        end;

      // Simulating offices
      fOfsRecycleIn.Q   := fOfsRecycleOut.Q;
      fOfsRecycleIn.K   := fOfsRecycleOut.K;
      KsalAvg           := KsalAvg/3;
      Po                := realmin( realmax(0, OfficesPerInhabitant*fTotalPop - fOffices.Q), fOfsDemand.Q );  
      fOfsInmigration.Q := KsalAvg*Po*dt/OfsInmigrationTimeSlope;
      fOfsEmigration.Q  := fOffices.Q*(1 - KsalAvg)*(1 - fOffices.K/100)*dt/OfsEmigrationTimeSlope;
      if fOfsInmigration.Q > fOfsEmigration.Q
        then
          begin
            fOfsInmigration.Q := fOfsInmigration.Q - fOfsEmigration.Q;
            fOfsEmigration.Q  := 0;
          end
        else
          begin
            fOfsEmigration.Q  := fOfsEmigration.Q - fOfsInmigration.Q;
            fOfsInmigration.Q := 0;
          end;

      // Computing overall quaility
      fOverallRating := fGQOL*fGQOS*AvgWealth;
    end;

  procedure TTownHall.AutoConnect( loaded : boolean );
    var
      i        : TPeopleKind;
      AdvInput : TInput;
    begin
      inherited;
      with TInhabitedTown(Facility.Town).World.fPopulator.CurrBlock do
        begin
          for i := low(i) to high(i) do
            begin
              OutputsByName[PeopleKindPrefix[i] + tidGate_Inmigration].ConnectTo( self.InputsByName[PeopleKindPrefix[i] + tidGate_Inmigration] );
              InputsByName[PeopleKindPrefix[i] + tidGate_Emigration].ConnectTo( self.OutputsByName[PeopleKindPrefix[i] + tidGate_Emigration] );
              InputsByName[PeopleKindPrefix[i] + tidGate_TownPerformance].ConnectTo( self.OutputsByName[PeopleKindPrefix[i] + tidGate_TownPerformance] );
              fResMovedOut[i] := self.Facility.Town.Parameters[tidTownParameter_ResMovedOut + PeopleKindPrefix[i]];
              fResRent[i]     := self.Facility.Town.Parameters[tidTownParameter_ResRent + PeopleKindPrefix[i]];
              fResQidx[i]     := self.Facility.Town.Parameters[tidTownParameter_ResQidx + PeopleKindPrefix[i]];
              fResPop[i]      := self.Facility.Town.Parameters[tidTownParameter_ResPop + PeopleKindPrefix[i]];
              fAvgSalary[i]   := self.Facility.Town.Parameters[tidTownParameter_Salary + PeopleKindPrefix[i]];
              fSalRatio[i]    := self.Facility.Town.Parameters[tidTownParameter_SalRatio + PeopleKindPrefix[i]];
            end;
        end;
      fBeautyModifier :=
        TPyramidalModifier.Create(
          tidEnvironment_Beauty,
          Point(xOrigin, yOrigin),
          MetaBlock.Beauty,
          MetaBlock.BeautyStrength );
      fSpontaneousBuildings.Pack;
      AdvInput := InputsByName[tidGate_Advertisement];
      if AdvInput <> nil
        then
          begin
            AdvInput.ActualMaxFluid.Q := 0;
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Publicity gate patched!' );
          end
        else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Could not patch Publicity gate!' );
    end;

  procedure TTownHall.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
    var
      i : TPeopleKind;
    begin
      inherited;
      if PeriodType = perDay
        then
          for i := low(i) to high(i) do
            begin
              fdayInCauseResidentials[i].Q  := fInCauseResidentials[i].Q/PeriodCount;
              fdayInCauseWork[i].Q          := fInCauseWork[i].Q/PeriodCount;
              fdayInCauseQOL[i].Q           := fInCauseQOL[i].Q/PeriodCount;
              fdayOutCauseWork[i].Q         := fOutCauseWork[i].Q/PeriodCount;
              fdayOutCauseResidentials[i].Q := fOutCauseResidentials[i].Q/PeriodCount;
              fdayOutCauseQOL[i].Q          := fOutCauseQOL[i].Q/PeriodCount;
              fdayOutCauseUnemployment[i].Q := fOutCauseUnemployment[i].Q/PeriodCount;
              fdayOutCauseServices[i].Q     := fOutCauseServices[i].Q/PeriodCount;
              fdayOutCauseDisasters[i].Q    := fOutCauseDisasters[i].Q/PeriodCount;
              fInCauseResidentials[i].Q     := 0;
              fInCauseWork[i].Q             := 0;
              fInCauseQOL[i].Q              := 0;
              fOutCauseWork[i].Q            := 0;
              fOutCauseResidentials[i].Q    := 0;
              fOutCauseQOL[i].Q             := 0;
              fOutCauseUnemployment[i].Q    := 0;
              fOutCauseServices[i].Q        := 0;
              fOutCauseDisasters[i].Q       := 0;
            end;
    end;

  procedure TTownHall.StoreToCache( Cache : TObjectCache );

    procedure StorePublicFacilitiesCoverage( Cache : TObjectCache );
      var
        i             : integer;
        count         : integer;
        kind          : TMetaPublicFacilityInfo;
        info          : TPublicFacilityInfo;
        CoveredPeople : TFluidValue;
      begin
        try
          count := TheClassStorage.ClassCount[tidClassFamily_PublicFacilities];
        except
          count := 0;
        end;
        Cache.WriteInteger( 'covCount', count );
        for i := 0 to pred(count) do
          begin
            kind := TMetaPublicFacilityInfo(TheClassStorage.ClassByIdx[tidClassFamily_PublicFacilities, i]);
            if TotalPopulation > 0
              then
                begin
                  info := PublicFacilities[kind];
                  if info <> nil
                    then CoveredPeople := realmin(info.Strength, TotalPopulation)
                    else CoveredPeople := 0;
                  Cache.WriteInteger( 'covValue' + IntToStr(i), round(100*CoveredPeople/TotalPopulation) );
                end
              else Cache.WriteInteger( 'covValue' + IntToStr(i), 0 );
            StoreMultiStringToCache( 'covName' + IntToStr(i) + '.', kind.Name_MLS, Cache );
          end;
      end;

    procedure StoreServicesCoverage( Cache : TObjectCache );
      var
        i : integer;
      begin
        Cache.WriteInteger( 'srvCount', fServiceInfo.Count );
        for i := 0 to pred(fServiceInfo.Count) do
          with TServiceInfo(fServiceInfo[i]) do
            begin
              StoreMultiStringToCache( 'svrName' + IntToStr(i) + '.', Kind.Name_MLS, Cache );
              Cache.WriteCurrency( 'svrMarketPrice' + IntToStr(i), Kind.MarketPrice );
              Cache.WriteFloat   ( 'svrDemand'      + IntToStr(i), Demand );
              Cache.WriteFloat   ( 'svrOffer'       + IntToStr(i), Offer );
              Cache.WriteFloat   ( 'svrCapacity'    + IntToStr(i), Capacity );
              Cache.WriteFloat   ( 'svrRatio'       + IntToStr(i), Ratio );
              Cache.WriteInteger ( 'svrQuality'     + IntToStr(i), Quality );
              Cache.WriteInteger ( 'svrPrice'       + IntToStr(i), Price );
            end;
      end;

    procedure StoreProductInfo( Cache : TObjectCache );
      var
        count : integer;
        i     : integer;
        idx   : integer;
        MF    : TMetaFluid;
        TP    : TTownParameter;
      begin
        try
          count := TheClassStorage.ClassCount[tidClassFamily_Fluids];
        except
          count := 0;
        end;
        idx := 0;
        for i := 0 to pred(count) do
          begin
            MF := TMetaFluid(TheClassStorage.ClassByIdx[tidClassFamily_Fluids, i]);
            if mfTradeable in MF.Options
              then
                begin
                  // Cache.WriteString( 'prdName' + IntToStr(idx), MF.Name_MLS.Values[langDefault] + ' (' + MF.FluidName_MLS.Values[langDefault] + ')' ); // >> MLS2
                  StoreMultiStringToCache( 'prdName' + IntToStr(idx) + '.', MF.Name_MLS, Cache );
                  StoreMultiStringToCache( 'prdFluid' + IntToStr(idx) + '.', MF.FluidName_MLS, Cache );
                  // INPUTS
                  // tidTownParameter_InputValue
                  TP := Facility.Town.Parameters[tidTownParameter_InputValue + MF.Id];
                  Cache.WriteFloat( 'prd' + tidTownParameter_InputValue + IntToStr(idx), MF.ConvertToUnits(TP.Value) );
                  // tidTownParameter_InputDemand
                  TP := Facility.Town.Parameters[tidTownParameter_InputCapacity + MF.Id];
                  Cache.WriteFloat( 'prd' + tidTownParameter_InputCapacity + IntToStr(idx), MF.ConvertToUnits(TP.Value) );
                  // tidTownParameter_InputQuality
                  TP := Facility.Town.Parameters[tidTownParameter_InputQuality + MF.Id];
                  Cache.WriteString( 'prd' + tidTownParameter_InputQuality + IntToStr(idx), IntToStr(round(TP.Average)) + '%' );
                  // tidTownParameter_InputPrice
                  TP := Facility.Town.Parameters[tidTownParameter_InputPrice + MF.Id];
                  Cache.WriteString( 'prd' + tidTownParameter_InputPrice + IntToStr(idx), IntToStr(round(100*TP.Average/MF.MarketPrice)) + '%' );
                  // tidTownParameter_InputMaxPrice
                  TP := Facility.Town.Parameters[tidTownParameter_InputMaxPrice + MF.Id];
                  Cache.WriteString( 'prd' + tidTownParameter_InputMaxPrice + IntToStr(idx), IntToStr(round(TP.Average)) + '%' );
                  // OUTPUTS
                  // tidTownParameter_OutputValue
                  TP := Facility.Town.Parameters[tidTownParameter_OutputValue + MF.Id];
                  Cache.WriteFloat( 'prd' + tidTownParameter_OutputValue + IntToStr(idx), MF.ConvertToUnits(TP.Value) );
                  // tidTownParameter_OutputOffer
                  TP := Facility.Town.Parameters[tidTownParameter_OutputCapacity + MF.Id];
                  Cache.WriteFloat( 'prd' + tidTownParameter_OutputCapacity + IntToStr(idx), MF.ConvertToUnits(TP.Value) );
                  // tidTownParameter_OutputQuality
                  TP := Facility.Town.Parameters[tidTownParameter_OutputQuality + MF.Id];
                  Cache.WriteString( 'prd' + tidTownParameter_OutputQuality + IntToStr(idx), IntToStr(round(TP.Average)) + '%' );
                  // tidTownParameter_OutputPrice
                  TP := Facility.Town.Parameters[tidTownParameter_OutputPrice + MF.Id];
                  Cache.WriteString( 'prd' + tidTownParameter_OutputPrice + IntToStr(idx), IntToStr(round(100*TP.Average/MF.MarketPrice)) + '%' );
                  inc( idx );
                end;
          end;
        Cache.WriteInteger( 'prdCount', idx );
      end;

    procedure StoreTaxes( Cache : TObjectCache );
      var
        i : integer;
      begin
        if Facility.Town <> nil
          then
            with Facility.Town do
              begin
                Cache.WriteInteger( 'TaxCount', AllTaxes.Count );
                for i := 0 to pred(AllTaxes.Count) do
                  TTax(AllTaxes[i]).StoreToCache( 'Tax' + IntToStr(i), Cache );
              end;
      end;

    var
      i  : TPeopleKind;
      TP : TTownParameter;                                     
    begin
      inherited;
      for i := low(i) to high(i) do
        begin
          Cache.WriteString(ppTTL, CreateTTL( 0, 0, 2, 0 ));
          Cache.WriteInteger( PeopleKindPrefix[i] + 'Population', round(fLastPop[i].Q) );
          Cache.WriteInteger( PeopleKindPrefix[i] + 'PopulationK', round(fLastPop[i].K) );
          Cache.WriteInteger( PeopleKindPrefix[i] + 'WorkForce', round(PeopleToWorkForce(i, fLastPop[i].Q)) );
          Cache.WriteInteger( PeopleKindPrefix[i] + 'WorkForceK', round(fLastPop[i].K) );
          Cache.WriteInteger( PeopleKindPrefix[i] + 'Floating', round(fResInmigration[i].Extra.Q) );
          Cache.WriteInteger( PeopleKindPrefix[i] + 'FloatingPerc', HomelessPerc[i] );
          Cache.WriteInteger( PeopleKindPrefix[i] + 'Unemployed', round(fWorkForceIn[i].Extra.Q) );
          Cache.WriteInteger( PeopleKindPrefix[i] + 'Unemployment', Unemployment[i] );
          Cache.WriteInteger( PeopleKindPrefix[i] + 'ResDemand', round(InputsByName[PeopleKindPrefix[i] + tidGate_ResDemand].LastValue.Q ));
          Cache.WriteInteger( PeopleKindPrefix[i] + 'InCauseResidentials', round(fdayInCauseResidentials[i].Q) );
          Cache.WriteInteger( PeopleKindPrefix[i] + 'InCauseWork', round(fdayInCauseWork[i].Q) );
          Cache.WriteInteger( PeopleKindPrefix[i] + 'InCauseQOL', round(fdayInCauseQOL[i].Q) );
          Cache.WriteInteger( PeopleKindPrefix[i] + 'OutCauseWork', round(fdayOutCauseWork[i].Q) );
          Cache.WriteInteger( PeopleKindPrefix[i] + 'OutCauseResidentials', round(fdayOutCauseResidentials[i].Q) );
          Cache.WriteInteger( PeopleKindPrefix[i] + 'OutCauseQOL', round(fdayOutCauseQOL[i].Q) );
          Cache.WriteInteger( PeopleKindPrefix[i] + 'OutCauseUnemployment', round(fdayOutCauseUnemployment[i].Q) );
          Cache.WriteInteger( PeopleKindPrefix[i] + 'OutCauseServices', round(fdayOutCauseServices[i].Q) );
          if fResRent[i] <> nil
            then Cache.WriteInteger( PeopleKindPrefix[i] + 'RentPrice', round(fResRent[i].Average) );
          if fResQidx[i] <> nil
            then Cache.WriteInteger( PeopleKindPrefix[i] + 'ResQ', round(fResQidx[i].Average) );
          if fAvgSalary[i] <> nil
            then Cache.WriteInteger( PeopleKindPrefix[i] + 'Salary', round(fAvgSalary[i].Average) );
          if (Facility <> nil) and (Facility.Town <> nil)
            then
              begin
                Cache.WriteInteger( PeopleKindPrefix[i] + 'MinSalary', Facility.Town.MayorMinSalary[i] );
                Cache.WriteInteger( PeopleKindPrefix[i] + 'ActualMinSalary', Facility.Town.WorldLocator.GetMinSalary( i ) );
              end;
          if fSalRatio[i] <> nil
            then Cache.WriteInteger( PeopleKindPrefix[i] + 'SalaryValue', round(100*fSalRatio[i].Value) );
          if Facility.Town <> nil
            then
              begin
                TP := Facility.Town.Parameters[tidTownParameter_PrivateWorkers + PeopleKindPrefix[i]];
                Cache.WriteInteger( PeopleKindPrefix[i] + 'PrivateWorkDemand', max(0, round(TP.Value)) );       
                Cache.WriteInteger( PeopleKindPrefix[i] + 'WorkDemand', max(0, round(fStoredWorkDemand[i].Q - TP.Value)) );
              end;
          Cache.WriteInteger( PeopleKindPrefix[i] + 'ResDemand', round(fStoredResDemand[i].Q) );
        end;
      Cache.WriteInteger( 'QOL', round(fGQOL*100) );
      Cache.WriteInteger( 'GQOS', round(fGQOS*100) );

      StorePublicFacilitiesCoverage( Cache );
      StoreServicesCoverage( Cache );
      //StoreProductInfo( Cache );
      {
      if Facility.Town <> nil
        then StoreProductInfo( Cache );
      }
      StoreTaxes( Cache );
      Cache.WriteFloat( 'Ads', LastAds.Q );
      Cache.WriteFloat( 'AdsK', LastAds.K );
      if TInhabitedTown(Facility.Town).fNewspaper <> nil
        then Cache.WriteString( 'NewspaperName', TInhabitedTown(Facility.Town).fNewspaper.Name );
    end;

  procedure TTownHall.RDOSetTaxValue( TaxId : integer; Value : widestring );
    var
      Tax : TTax;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Setting Tax value: ' + Facility.Town.Name + ', ' + IntToStr(TaxId) + ', ' + Value );
      try
        Tax := Facility.Town.Taxes[IntToStr(TaxId)];
        if Tax <> nil
          then Tax.ParseValue( Value );
        if ObjectIs( TTaxToAccount.ClassName, Tax ) and Facility.CheckOpAuthenticity
          then
            begin
              if not TTaxToAccount(Tax).Subsidized
                then
                  Facility.Town.WorldLocator.SendEvent(
                    TEvent.Create(
                      0,
                      Facility.Town.Timer.GetVirtualTimeAbs,
                      Facility.Town.Timer.GetVirtualTime,
                      160000,
                      -10,
                      //NullString,//'Mayor of ' + Facility.Town.Name + ' set taxes for ' + Tax.MetaTax.name + ' at ' + value + '%.',
                      InstantiateMultiString( mtidSetTaxes, [Facility.Town.Name, Tax.MetaTax.name_MLS.Values['0'], value] ),
                      '', '' ))
                else
                  Facility.Town.WorldLocator.SendEvent(
                    TEvent.Create(
                      0,
                      Facility.Town.Timer.GetVirtualTimeAbs,
                      Facility.Town.Timer.GetVirtualTime,
                      160000,
                      -10,
                      //NullString,//'Mayor of ' + Facility.Town.Name + ' set a subsidy for ' + Tax.MetaTax.name + '.',
                      InstantiateMultiString( mtidSubsidy, [Facility.Town.Name, Tax.MetaTax.name_MLS.Values['0']] ),
                      '', '' ));
              ModelServerCache.BackgroundInvalidateCache(Facility.Town);
            end;
      except
      end;
      Logs.Log( tidLog_Survival,  'OK!');
    end;
    
  procedure TTownHall.RDOSetMinSalaryValue( PopKind, Value : integer );
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Setting Min Wage: ' + Facility.Town.Name + ', ' + IntToStr(integer(PopKind)) + ', ' + IntToStr(Value) );
      try
        if Facility.CheckOpAuthenticity
          then
            begin
              Facility.Town.MinSalary[TPeopleKind(PopKind)] := min(high(TPercent), Value);
              ModelServerCache.BackgroundInvalidateCache(Facility); //CacheObject( Facility, -1, -1 )
            end;
      except
      end;
      Logs.Log( tidLog_Survival,  'Setting Min Wage OK!');
    end;
    
  function TTownHall.GetPopulation(kind : TPeopleKind) : TFluidValue;
    begin
      result := trunc(fLastPop[kind].Q);
    end;

  function TTownHall.GetWealth( kind : TPeopleKind ) : integer;
    begin
      result := round(100*fSalRatio[kind].Value);
    end;

  function TTownHall.GetSalary( kind : TPeopleKind ) : TPercent;
    begin
      result := round(fAvgSalary[kind].Average);
    end;

  function TTownHall.GetSalaryRatio( kind : TPeopleKind ) : single;
    begin
      result := fSalRatio[kind].CurrValue
    end;
    
  function TTownHall.GetResDemand( kind : TPeopleKind ) : TFluidValue;
    begin
      result := fStoredResDemand[kind].Q;
    end;

  function TTownHall.GetWorkDemand( kind : TPeopleKind ) : TFluidValue;
    begin
      result := fStoredWorkDemand[kind].Q;
    end;

  function TTownHall.GetResRent( kind : TPeopleKind ) : integer;
    begin
      result := round(fResRent[kind].Average);
    end;

  function TTownHall.GetResQidx( kind : TPeopleKind ) : integer;
    begin
      result := round(fResQidx[kind].Average);
    end;

  function TTownHall.GetAvgWealth : single;
    var
      i : TPeopleKind;
    begin
      if round(TotalPopulation) > 0
        then
          begin
            result := 0;
            for i := low(i) to high(i) do
              result := result + fSalRatio[i].Value*fLastPop[i].Q;
            result := result/TotalPopulation;
          end
        else result := 0;
    end;
    
  function TTownHall.GetHomelessPerc( kind : TPeopleKind ) : TPercent;
    var
      TotalPop : TFluidValue;
    begin
      TotalPop := fPopulation[kind].Q + fResInmigration[kind].Extra.Q + fHomeless[kind].Q;
      if round(TotalPop) > 0
        then result := round((fResInmigration[kind].Extra.Q + fHomeless[kind].Q)*100/TotalPop)
        else result := 0;
    end;

  function TTownHall.GetUnemployment( kind : TPeopleKind ) : TPercent;
    begin
      if round(fLastPop[kind].Q) > 0
        then result := round( 100*realmin(1, WorkForceToPeople( kind, fUnemployed[kind].Q )/fLastPop[kind].Q ))
        else result := 0;
    end;

  procedure TTownHall.ReportClosedBuilding( aBlock : TBlock );
    var
      PeopleKind : TPeopleKind;
      Block      : TPopulatedBlock;
    begin
      Block      := TPopulatedBlock(aBlock);
      PeopleKind := TMetaPopulatedBlock(Block.MetaBlock).PeopleKind;
      fHomeless[PeopleKind].K := AverageK( @fHomeless[PeopleKind], @Block.People );
      fHomeless[PeopleKind].Q := fHomeless[PeopleKind].Q + Block.People.Q;
      fResDemand[PeopleKind].Q := fResDemand[PeopleKind].Q - Block.Outputs[0].FluidData.Q;
    end;

  function TTownHall.GetServiceInfoById(Id : string) : TObject;
    var
      i   : integer;
      cnt : integer;
    begin
      cnt := fServiceInfo.Count;
      i   := 0;
      while (i < cnt) and (TServiceInfo(fServiceInfo[i]).Kind.Id <> Id) do
        inc(i);
      if i < cnt
        then result := TServiceInfo(fServiceInfo[i])
        else result := nil;
    end;

  function TTownHall.GetPublicFacility( Kind : TMetaPublicFacilityInfo ) : TPublicFacilityInfo;
    var
      i : integer;
    begin
      i := 0;
      while (i < fPublicFacilities.Count) and (TPublicFacilityInfo(fPublicFacilities[i]).Kind <> Kind) do
        inc( i );
      if i < fPublicFacilities.Count
        then result := TPublicFacilityInfo(fPublicFacilities[i])
        else result := nil;
    end;

  procedure TTownHall.ReportPublicFacility( Kind : TMetaPublicFacilityInfo; aStrength : integer );
    var
      FacInfo : TPublicFacilityInfo;
    begin
      FacInfo := PublicFacilities[Kind];
      if FacInfo = nil
        then
          begin
            FacInfo       := TPublicFacilityInfo.Create;
            FacInfo.fKind := Kind;
            fPublicFacilities.Insert( FacInfo );
          end;
      inc( FacInfo.fStrength, aStrength );
    end;

  function TTownHall.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;

    function HighestUnemployment( out classname : string ) : integer;
      var
        kind : TPeopleKind;
      begin
        result := 0;
        for kind := low(kind) to high(kind) do
          if Unemployment[kind] >= result
            then
              begin
                classname := mtidWorkforceKindName[kind].Values[ToTycoon.Language];
                result    := Unemployment[kind];
              end;
      end;

    function GetMoveReport( kind : TPeopleKind ) : string;
      var
        PeopleIn  : integer;
        PeopleOut : integer;
        Perc      : integer;
      begin
        PeopleIn :=
          round(
            fdayInCauseResidentials[kind].Q +
            fdayInCauseWork[kind].Q +
            fdayInCauseQOL[kind].Q );
        PeopleOut :=
          round(
            fdayOutCauseWork[kind].Q +
            fdayOutCauseResidentials[kind].Q +
            fdayOutCauseQOL[kind].Q +
            fdayOutCauseUnemployment[kind].Q +
            fdayOutCauseServices[kind].Q +
            fdayOutCauseDisasters[kind].Q);
        if abs( PeopleIn - PeopleOut ) > 0
          then
            if PeopleIn > PeopleOut
              then
                begin
                  result := SimHints.GetHintText( mtidPeopleIn.Values[ToTycoon.Language], [PeopleIn - PeopleOut, mtidPeopleKindName[kind].Values[ToTycoon.Language]] );
                  Perc := round(100*fdayInCauseResidentials[kind].Q/PeopleIn);
                  if Perc > 0
                    then result := result + SimHints.GetHintText( mtidPeopleInRes.Values[ToTycoon.Language], [Perc] );
                  Perc := round(100*fdayInCauseWork[kind].Q/PeopleIn);
                  if Perc > 0
                    then result := result + SimHints.GetHintText( mtidPeopleInWork.Values[ToTycoon.Language], [Perc] );
                  Perc := round(100*fdayInCauseQOL[kind].Q/PeopleIn);
                  if Perc > 0
                    then result := result + SimHints.GetHintText( mtidPeopleInQOL.Values[ToTycoon.Language], [Perc] );
                  {
                  result :=
                    SimHints.GetHintText(
                      hidPeopleIn,
                      [PeopleIn - PeopleOut,
                       PeopleKindName[kind],
                       round(100*fdayInCauseResidentials[kind].Q/PeopleIn),
                       round(100*fdayInCauseWork[kind].Q/PeopleIn),
                       round(100*fdayInCauseQOL[kind].Q/PeopleIn)] );
                  }
                end
              else
                begin
                  result := SimHints.GetHintText( mtidPeopleOut.Values[ToTycoon.Language], [PeopleOut - PeopleIn, mtidPeopleKindName[kind].Values[ToTycoon.Language]] );
                  Perc := round(100*fdayOutCauseWork[kind].Q/PeopleOut);
                  if Perc > 0
                    then result := result + SimHints.GetHintText( mtidPeopleOutWork.Values[ToTycoon.Language], [Perc] );
                  Perc := round(100*fdayOutCauseResidentials[kind].Q/PeopleOut);
                  if Perc > 0
                    then result := result + SimHints.GetHintText( mtidPeopleOutRes.Values[ToTycoon.Language], [Perc] );
                  Perc := round(100*fdayOutCauseQOL[kind].Q/PeopleOut);
                  if Perc > 0
                    then result := result + SimHints.GetHintText( mtidPeopleOutQOL.Values[ToTycoon.Language], [Perc] );
                  Perc := round(100*fdayOutCauseUnemployment[kind].Q/PeopleOut);
                  if Perc > 0
                    then result := result + SimHints.GetHintText( mtidPeopleOutUnemp.Values[ToTycoon.Language], [Perc] );
                  Perc := round(100*fdayOutCauseServices[kind].Q/PeopleOut);
                  if Perc > 0
                    then result := result + SimHints.GetHintText( mtidPeopleOutServ.Values[ToTycoon.Language], [Perc] );
                  Perc := round(100*fdayOutCauseDisasters[kind].Q/PeopleOut);
                  if Perc > 0
                    then result := result + SimHints.GetHintText( mtidPeopleOutDisasters.Values[ToTycoon.Language], [Perc] );
                  {
                  result :=
                    SimHints.GetHintText(
                      hidPeopleOut,
                      [PeopleOut - PeopleIn,
                       PeopleKindName[kind],
                       round(100*fdayOutCauseWork[kind].Q/PeopleOut),
                       round(100*fdayOutCauseResidentials[kind].Q/PeopleOut),
                       round(100*fdayOutCauseQOL[kind].Q/PeopleOut),
                       round(100*fdayOutCauseUnemployment[kind].Q/PeopleOut),
                       round(100*fdayOutCauseServices[kind].Q/PeopleOut)] );
                  }
                end
          else result := SimHints.GetHintText( mtidNoMovements.Values[ToTycoon.Language], [mtidPeopleKindName[kind].Values[ToTycoon.Language]] ) + ' '
          //result := 'No ' + mtidPeopleKindName[kind].Values[ToTycoon.Language] + ' movements. ';
      end;

    var
      i : TPeopleKind;
      // unemployment : integer;
      // classname    : string;
    begin
      result := inherited GetStatusText( kind, ToTycoon );
      case kind of
        sttMain :
          result := SimHints.GetHintText( mtidTHMainText.Values[ToTycoon.Language], [Format( '%.0n', [int(TotalPopulation)] )] );
          //Format( '%.0n', [int(TotalPopulation)] ) + ' inhabitants';
        sttSecondary :
          begin                                           
            result := result + ' ';                                                                             
            for i := low(i) to high(i) do
              begin
                result := result + SimHints.GetHintText( mtidTHPopReport.Values[ToTycoon.Language], [Format( '%.0n', [int(fLastPop[i].Q)] ), mtidPeopleKindName[i].Values[ToTycoon.Language], Unemployment[i]] );
                //result + IntToStr(round(fLastPop[i].Q)) + ' ' + mtidPeopleKindName[i].Values[ToTycoon.Language] + ' (' + IntToStr(Unemployment[i]) + '% unemp.)'; //
                if i < high(i)
                  then result := result + ', '
              end;
            result := result + '.';
          end;
        sttHint :
          begin
            begin
              result :=
                GetMoveReport( pkHigh ) +
                GetMoveReport( pkMiddle ) +
                GetMoveReport( pkLow );
            end;
          end;
      end;
    end;

  procedure TTownHall.LoadFromBackup( Reader : IBackupReader );

    function FindDisaster( Id : string ) : TDisaster;
      var
        i : integer;
      begin
        i := 0;
        while (i < fDisasters.Count) and (TDisaster(fDisasters[i]).MetaDisaster.Id <> Id) do
          inc( i );
        if i < fDisasters.Count
          then result := TDisaster(fDisasters[i])
          else result := nil;
      end;

    procedure UpdateDisasters;
      var
        count, i : integer; 
      begin
        // First delete disasters that are no longer registered
        for i := pred(fDisasters.Count) downto 0 do
          if TDisaster(fDisasters[i]).MetaDisaster = nil
            then fDisasters.AtDelete( i );
        // Add new disasters
        try
          count := TheClassStorage.ClassCount[tidClassFamily_Disasters];
        except
          count := 0;
        end;
        for i := 0 to pred(count) do
          with TMetaDisaster(TheClassStorage.ClassByIdx[tidClassFamily_Disasters, i]) do
            if FindDisaster( Id ) = nil
              then fDisasters.Insert( Instantiate( self ) );
      end;

    var
      i : TPeopleKind;
    begin
      inherited;
      InitServiceInfo;
      fTotalPop := Reader.ReadSingle( 'TotalPop', 0 );
      for i := low(i) to high(i) do
        begin
          LoadFluidData( 'Homeless.' + PeopleKindPrefix[i], fHomeless[i], Reader );
          LoadFluidData( 'Unemployed.' + PeopleKindPrefix[i], fUnemployed[i], Reader );
          LoadFluidData( 'LastPop.' + PeopleKindPrefix[i], fLastPop[i], Reader );
          {
          fResMovedOut[i] := Facility.Town.Parameters[tidTownParameter_ResMovedOut + PeopleKindPrefix[i]];
          fAvgSalary[i]   := Facility.Town.Parameters[tidTownParameter_Salary + PeopleKindPrefix[i]];
          fSalRatio[i]    := Facility.Town.Parameters[tidTownParameter_SalRatio + PeopleKindPrefix[i]];
          }
          if fLastPop[i].Q > 100000
            then fLastPop[i].Q := 10000;
          if fLastPop[i].Q < 0
            then fLastPop[i].Q := 0;
        end;
      Reader.ReadObject( 'PublicFacilities', fPublicFacilities, nil );
      Reader.ReadObject( 'SpontaneousBuildings', fSpontaneousBuildings, nil );
      if fSpontaneousBuildings = nil
        then fSpontaneousBuildings := TCollection.Create( 0, rkUse );
      Reader.ReadObject( 'Disasters', fDisasters, nil );
      if fDisasters = nil
        then fDisasters := TCollection.Create( 0, rkBelonguer );
      UpdateDisasters;
    end;

  procedure TTownHall.StoreToBackup( Writer : IBackupWriter );
    var
      i   : TPeopleKind;
      aux : string;
    begin
      inherited;
      Writer.WriteSingle( 'TotalPop', fTotalPop );
      for i := low(i) to high(i) do
        begin
          aux := 'Homeless.' + PeopleKindPrefix[i];
          StoreFluidData( aux, fHomeless[i], Writer );
          aux := 'Unemployed.' + PeopleKindPrefix[i];
          StoreFluidData( aux, fUnemployed[i], Writer );
          aux := 'LastPop.' + PeopleKindPrefix[i];
          StoreFluidData( aux, fLastPop[i], Writer );
        end;
      Writer.WriteLooseObject( 'PublicFacilities', fPublicFacilities );
      Writer.WriteObject( 'SpontaneousBuildings', fSpontaneousBuildings );
      Writer.WriteObject( 'Disasters', fDisasters );
      aux := '';
    end;

  procedure TTownHall.InitServiceInfo;
    var
      count : integer;
      i     : integer;
      MS    : TMetaService;
      SI    : TServiceInfo;
    begin
      try
        count := TheClassStorage.ClassCount[tidClassFamily_Services];
      except
        count := 0;
      end;
      fServiceInfo := TCollection.Create( count, rkBelonguer );
      for i := 0 to pred(count) do
        begin
          MS := TMetaService(TheClassStorage.ClassByIdx[tidClassFamily_Services, i]);
          SI := TServiceInfo.Create;
          SI.Kind := MS;
          fServiceInfo.Insert( SI );
        end;
    end;

  procedure TTownHall.CreateSpontaneousBuilding;

    const
      MaxAttempts         = 120;
      NeighborMaxAttempts = 4;

    function GetNextPointInMap( xPos, yPos : integer; dir : TGrowthDir; out xLast, yLast : integer ) : boolean;
      var
        attempt : integer;
        dx, dy  : integer;
      begin
        xLast  := xPos;
        yLast  := yPos;
        case dir of
          dirN :
            begin
              dx := 0;
              dy := 1;
            end;
          dirE :
            begin
              dx := 1;
              dy := 0;
            end;
          dirS :
            begin
              dx := 0;
              dy := -1;
            end;
          else
            begin
              dx := -1;
              dy := 0;
            end;
        end;
        attempt := 0;
        repeat
          inc( xLast, dx );
          inc( yLast, dy );
          result := TInhabitedTown(Facility.Town).World.AreaIsClear( xLast, yLast, 2, 2, nil );
          inc( attempt );
        until result or (attempt >= NeighborMaxAttempts);
      end;

    function PickRandomPointInMap( out xLast, yLast : integer ) : boolean;
      var
        xSlope   : integer;
        ySlope   : integer;
        attempts : integer;
        found    : boolean;
      begin
        xLast  := Facility.xPos;
        yLast  := Facility.yPos;
        if random(2) = 0
          then
            begin
              xSlope := 5;
              ySlope := random(3);
            end
          else
            begin
              xSlope := random(3);
              ySlope := 5;
            end;
        attempts := 0;
        repeat
          inc( xLast, xSlope );
          inc( yLast, ySlope );
          with TInhabitedTown(Facility.Town).World do
            found := AreaIsClear( xLast, yLast, 2, 2, nil ) and MatchesZone(xLast, yLast, 2, 2, znResidential);
          inc( attempts );
        until found or (attempts >= MaxAttempts);
        result := found;
      end;

    var
      i        : integer;
      dir      : TGrowthDir;
      xLast    : integer;
      yLast    : integer;
      found    : boolean;
      Building : TFacility;
      Neighbor : TSpontaneousBuilding;
    begin
      try
        Neighbor := nil;
        if fSpontaneousBuildings.Count > 0
          then
            begin
              i := 0;
              found := false;
              while (i < fSpontaneousBuildings.Count) and not found do
                begin
                  with TSpontaneousBuilding(fSpontaneousBuildings[i]) do
                    while GetAvailableDir( dir ) and not found do
                      begin
                        found := GetNextPointInMap( Facility.xPos, Facility.yPos, dir, xLast, yLast );
                        if not found
                          then Blocked[dir] := true
                          else Neighbor := TSpontaneousBuilding(fSpontaneousBuildings[i])
                      end;
                  inc( i );
                end;
              if not found
                then found := PickRandomPointInMap( xLast, yLast );
            end
          else found := PickRandomPointInMap( xLast, yLast );
        if found and (TInhabitedTown(Facility.Town).World.NewFacility( tidFacility_SpontaneousBuilding, 0, xLast, yLast, Building ) = NOERROR)
          then
            begin
              fSpontaneousBuildings.Insert( Building.CurrBlock );
              if Neighbor <> nil
                then
                  begin
                    Neighbor.Neighbors[dir] := TSpontaneousBuilding(Building.CurrBlock);
                    Neighbor.Blocked[dir]   := true;
                  end;
            end;
      except
      end;
    end;

  procedure TTownHall.DestroySpontaneousBuilding( Building : TFacility );
    begin
      fSpontaneousBuildings.Delete( Building.CurrBlock );
    end;

  function TTownHall.GetContextStatusStr( ToTycoon : TTycoon ) : string;

    function PickPublicService : TMetaPublicFacilityInfo;
      var
        i             : integer;
        count         : integer;
        kind          : TMetaPublicFacilityInfo;
        info          : TPublicFacilityInfo;
        CoveredPeople : TFluidValue;
      begin
        try
          count := TheClassStorage.ClassCount[tidClassFamily_PublicFacilities];
        except
          count := 0;
        end;
        i := 0;
        result := nil;
        while (result = nil) and (i < count) do
          begin
            kind := TMetaPublicFacilityInfo(TheClassStorage.ClassByIdx[tidClassFamily_PublicFacilities, i]);
            info := PublicFacilities[kind];
            if info <> nil
              then CoveredPeople := realmin(info.Strength, TotalPopulation)
              else CoveredPeople := 0;
            if (CoveredPeople/TotalPopulation < 0.8) and (random(3) = 0)
              then result := kind;
            inc( i );
          end;
      end;

    function PickService : TServiceInfo;
      var
        i : integer;
      begin
        i := 0;
        result := nil;
        while (result = nil) and (i < fServiceInfo.Count) do
          with TServiceInfo(fServiceInfo[i]) do
            begin
              if (Ratio < 0.8) and (random(5) = 0)
                then result := TServiceInfo(fServiceInfo[i]);
              inc( i );
            end;
      end;

    const
      OptionCount = 5;
    var
      PFKind : TMetaPublicFacilityInfo;
      SvrInf : TServiceInfo;
    begin
      if fTotalPop > 100
        then
          begin
            result := '';
            // Services
            SvrInf := PickService;
            if SvrInf <> nil
              then result := SimHints.GetHintText( mtidServiceNeeded.Values[ToTycoon.Language], [Facility.Town.Name, lowercase(SvrInf.Kind.Name_MLS.Values[ToTycoon.Language])] ); // >> MLS2
            // Public Services
            PFKind := PickPublicService;
            if PFKind <> nil
              then result := SimHints.GetHintText( mtidPublicServiceNeeded.Values[ToTycoon.Language], [Facility.Town.Name, lowercase(PFKind.Name_MLS.Values[ToTycoon.Language])] );
            // Else, visit local newspaper
            if result = ''
              then result := SimHints.GetHintText( mtidVisitNewspaper.Values[ToTycoon.Language], [Facility.Town.Name, Facility.Town.Cluster.NameNewspaper( Facility.Town.Name )] );
          end
        else result := SimHints.GetHintText( mtidDesertedTown.Values[ToTycoon.Language], [Facility.Town.Name, Facility.MetaFacility.ClusterName] );
    end;

  procedure TTownHall.TimeWarp;
    begin
      Logs. Log( tidLog_TimeWarp, '--------------- City of ' + Facility.Town.Name + ' ---------------');
      if Facility.Company.Owner.MasterRole <> nil
        then
          if Facility.Company.Owner.MasterRole.Name = Facility.Company.Owner.Name
          then
            Logs. Log( tidLog_TimeWarp, DateTimeToStr(Now) + ': nobody is ' + Facility.Company.Owner.Name )
          else
            Logs. Log( tidLog_TimeWarp, DateTimeToStr(Now) + ': ' + Facility.Company.Owner.MasterRole.Name + ' is ' + Facility.Company.Owner.Name );
      Logs. Log( tidLog_TimeWarp, DateTimeToStr(Now) + ': ' + Facility.Town.Name + '''s population is ' + FloatToStr(Round(TotalPopulation)) + ' - LC: ' + FloatToStr(GetPopulation(pkLow)) + ' - MC: ' + FloatToStr(GetPopulation(pkMiddle)) + ' - HC: ' + FloatToStr(GetPopulation(pkHigh)));
      Logs. Log( tidLog_TimeWarp, DateTimeToStr(Now) + ': ' + Facility.Town.Name + '''s wealth (spending power) is: LC: ' + FloatToStr(Wealth[pkLow]) + '% - MC: ' + FloatToStr(Wealth[pkMiddle]) + '% - HC: ' + FloatToStr(Wealth[pkHigh]) + '%');
      Logs. Log( tidLog_TimeWarp, DateTimeToStr(Now) + ': ' + Facility.Town.Name + '''s QOL is ' + FloatToStr(Round(GQOL*100)) + '%');
      Logs. Log( tidLog_TimeWarp, DateTimeToStr(Now) + ': ' + Facility.Town.Name + '''s Commerce General Index is ' + FloatToStr(Round(GQOS*100)) + '%');
      Logs. Log( tidLog_TimeWarp, DateTimeToStr(Now) + ': ' + Facility.Town.Name + '''s unemployment is: LC: ' + FloatToStr(Unemployment[pkLow]) + '% - MC: ' + FloatToStr(Unemployment[pkMiddle]) + '% - HC: ' + FloatToStr(Unemployment[pkHigh]) + '%');
      Logs. Log( tidLog_TimeWarp, DateTimeToStr(Now) + ': ' + Facility.Town.Name + '''s Residential Vacancies are ' + ': LC: ' + FloatToStr(Round(ResDemand[pkLow])) + ' - MC: ' + FloatToStr(Round(ResDemand[pkMiddle])) + ' - HC: ' + FloatToStr(Round(ResDemand[pkHigh])));
      Logs. Log( tidLog_TimeWarp, DateTimeToStr(Now) + ': ' + Facility.Town.Name + '''s Residental Rent is: LC: ' + FloatToStr(ResRent[pkLow]) + '% - MC: ' + FloatToStr(ResRent[pkMiddle]) + '% - HC: ' + FloatToStr(ResRent[pkHigh]) + '%');
      Logs. Log( tidLog_TimeWarp, DateTimeToStr(Now) + ': ' + Facility.Town.Name + '''s Residential Quality is: LC: ' + FloatToStr(ResQidx[pkLow]) + '% - MC: ' + FloatToStr(ResQidx[pkMiddle]) + '% - HC: ' + FloatToStr(ResQidx[pkHigh]) + '%');
      Logs. Log( tidLog_TimeWarp, DateTimeToStr(Now) + ': ' + Facility.Town.Name + '''s Jobs Vacancies are ' + ': LC: ' + FloatToStr(Round(WorkDemand[pkLow])) + ' - MC: ' + FloatToStr(Round(WorkDemand[pkMiddle])) + ' - HC: ' + FloatToStr(Round(WorkDemand[pkHigh])));
      Logs. Log( tidLog_TimeWarp, DateTimeToStr(Now) + ': ' + Facility.Town.Name + '''s Average Salaries are: LC: ' + FloatToStr(Salary[pkLow]) + '% - MC: ' + FloatToStr(Salary[pkMiddle]) + '% - HC: ' + FloatToStr(Salary[pkHigh]) + '%');
    end;

  // TInhabitedTown

  constructor TInhabitedTown.Create( aName : string;
                                     aClusterId, aTownHallClass, aTradeCenterClass : string;
                                     aXPos, aYPos : integer;
                                     aWorld : TInhabitedWorld );
    var
      aCluster   : TCluster;
      facX, facY : integer;
      Portal     : TFacility;
    begin
      aCluster := TCluster(TheClassStorage.ClassById[tidClassFamily_Clusters, aClusterId]);
      inherited Create( aName, aXpos, aYPos, aCluster, aWorld, aWorld.MailServer );
      fWorld       := aWorld;
      fNewspaper   := TNewspaper.Create;
      Timer        := aWorld;
      MapRefresh   := aWorld;
      WorldLocator := aWorld;
      RoadHandler  := aWorld;
      ModelFactory := aWorld;
      if aWorld.NewFacility( aTownHallClass, TownCompany.Id, aXPos, aYPos, fTownHall ) = NOERROR
        then fTownHall.Town := self      
        else raise Exception.Create( 'Could not create Town Hall' );
      facX := aXPos;
      facY := aYPos;
      with TMetaFacility(TheClassStorage.ClassById[tidClassFamily_Facilities, aTradeCenterClass]) do
        begin
          // Trade center appears above Town Hall.
          facY := facY - ySize - 1;
        end;
      if aWorld.NewFacility( aTradeCenterClass, Cluster.Company.Id, facX, facY, fTradeCenter ) = NOERROR
        then fTradeCenter.Town := self
        else raise Exception.Create( 'Could not create Trade Center' );
      facX := aXPos;
      facY := aYPos;
      with TMetaFacility(TheClassStorage.ClassById[tidClassFamily_Facilities, 'Portal']) do
        begin
          // Portal appears across Town Hall.
          facX := facX - xSize - 1;
        end;
      if aWorld.NewFacility( 'Portal', Cluster.Company.Id, facX, facY, Portal ) = NOERROR
        then Portal.Town := self
        else raise Exception.Create( 'Could not create Trade Center' );
      fNewspaper.Name := Cluster.NameNewspaper( aName );
      ModelFactory.NewNewspaper( fNewspaper.Name, Cluster.Id, aName );              
      CacheObject( fNewspaper, -1, -1 );
    end;

  function TInhabitedTown.GetContextStatusStr( ToTycoon : TTycoon ) : string;
    begin
      result := TTownHall(fTownHall.CurrBlock).GetContextStatusStr( ToTycoon );
    end;

  procedure TInhabitedTown.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'TownHall', fTownHall, nil );
      Reader.ReadObject( 'TradeCenter', fTradeCenter, nil );
      Reader.ReadObject( 'World', fWorld, nil );
      if fWorld <> nil
        then
          begin
            Timer        := fWorld;
            MapRefresh   := fWorld;
            WorldLocator := fWorld;
            RoadHandler  := fWorld;
            ModelFactory := fWorld;
          end;
    end;

  procedure TInhabitedTown.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObjectRef( 'TownHall', fTownHall );
      Writer.WriteObjectRef( 'TradeCenter', fTradeCenter );
      Writer.WriteObjectRef( 'World', fWorld );
    end;

  procedure TInhabitedTown.Loaded;
    begin
      inherited;
      fNewspaper := TNewspaper.Create;
      fNewspaper.Name := Cluster.NameNewspaper( Name );
      fNewspaper.Town := self;
      ModelFactory.NewNewspaper( fNewspaper.Name, Cluster.Id, Name );
      CacheObject( fNewspaper, -1, -1 );
    end;
     

  // TInhabitedWorld

  constructor TInhabitedWorld.Create( aName : string; axSize : integer; aySize : integer );
    begin
      inherited Create( aName, axSize, aySize );
      fPopulator := TMetaFacility(TheClassStorage.ClassById['Facilities', tidBlock_WorldPopulator]).Instantiate;
      InsertFacility( fPopulator );
    end;

  function TInhabitedWorld.GetTotalPopulation : integer;
    var
      i : integer;
    begin
      result := 0;
      for i := 0 to pred(Towns.Count) do
        with TTownHall(TInhabitedTown(Towns[i]).fTownHall.CurrBlock) do
          result := result + round(fTotalPop);
    end;

  procedure TInhabitedWorld.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Populator', fPopulator, nil );
    end;

  procedure TInhabitedWorld.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObjectRef( 'Populator', fPopulator );
    end;

                                                   
  // TPublicFacilityInfoBackupAgent

  type
    TPublicFacilityInfoBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : IBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : IBackupReader; Obj : TObject); override;
      end;

  class procedure TPublicFacilityInfoBackupAgent.Write(Stream : IBackupWriter; Obj : TObject);
    begin
      TPublicFacilityInfo(Obj).StoreToBackup(Stream);
    end;

  class procedure TPublicFacilityInfoBackupAgent.Read(Stream : IBackupReader; Obj : TObject);
    begin
      TPublicFacilityInfo(Obj).LoadFromBackup(Stream);
    end;


  // RegisterMetaIntances

  procedure RegisterWorldPopulator;
    var
      MB     : TMetaBlock;
      i      : TPeopleKind;
      Sample : TWorldPopulator;
      People : TMetaFluid;
    begin
      Sample := nil;
      MB := TMetaBlock.Create( tidBlock_WorldPopulator, accIdx_None, accIdx_None, TWorldPopulator );
      for i := low(i) to high(i) do
        begin
          MB.CnntRepairable := false;
          People := TMetaFluid(TheClassStorage.ClassById[ 'Fluids', PeopleKindPrefix[i] + tidFluid_People ]);
          MB.MetaInputs.Insert(
            TMetaInput.Create(
              PeopleKindPrefix[i] + tidGate_Emigration,
              inputZero,
              inputIlimited,
              InputData( 0, 0 ),
              qIlimited,
              TPushInput,
              People,
              1000,
              mglBasic,
              [],
              sizeof(Sample.fEmigration[i]),
              Sample.Offset( Sample.fEmigration[i] )));
          MB.MetaOutputs.Insert(
            TMetaOutput.Create(
              PeopleKindPrefix[i] + tidGate_Inmigration,
              fluidIlimited,
              TPushOutput,
              People,
              1000,
              [],
              sizeof(Sample.fInmigration[i]),
              Sample.Offset( Sample.fInmigration[i] )));
          MB.MetaInputs.Insert(
            TMetaInput.Create(
              PeopleKindPrefix[i] + tidGate_TownPerformance,
              inputZero,
              inputIlimited,
              inputZero,
              qIlimited,
              TPushInput,
              nil,
              1000,
              mglBasic,
              [],
              sizeof(Sample.fTownPerformance[i]),
              Sample.Offset( Sample.fTownPerformance[i] )));
        end;
      MB.Register( 'Blocks' );
      with TMetaFacility.Create( tidBlock_WorldPopulator, 'World Population Controller', idWorldPopulator, TFacility ) do
        begin
          Level   := 0;
          Options := [];
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'The WorldPopulator is ready to work', MB ));
          Register( 'Facilities' );
        end;
    end;

  procedure RegisterTownHall( aClusterName, anId : string; aVisualClass : TVisualClassId; axSize, aySize : integer; aBeauty : TSurfaceValue; BlockClass : CBlock );
    var
      MB        : TMetaBlock;
      i         : TPeopleKind;
      Sample    : TTownHall;
      People    : TMetaFluid;
      Offices   : TMetaFluid;
      WorkForce : TMetaFluid;
    begin
      Sample := nil;
      MB := TMetaBlock.Create( anId, accIdx_None, accIdx_None, BlockClass );
      MB.Beauty := aBeauty;
      MB.BeautyStrength := TownHallBeautyStrength;
      for i := low(i) to high(i) do
        begin
          MB.CnntRepairable := false; // >> Corruption prevention
          People    := TMetaFluid(TheClassStorage.ClassById[ 'Fluids', PeopleKindPrefix[i] + tidFluid_People ]);
          WorkForce := TMetaFluid(TheClassStorage.ClassById[ 'Fluids', PeopleKindPrefix[i] + tidFluid_WorkForce ]);
          MB.MetaInputs.Insert(
            TMetaInput.Create(
              PeopleKindPrefix[i] + tidGate_ResDemand,
              inputZero,
              inputIlimited,
              InputData( 0, 0 ),
              qIlimited,
              TPushInput,
              People,
              1000,
              mglBasic,
              [],
              sizeof(Sample.fResDemand[i]),
              Sample.Offset( Sample.fResDemand[i] )));
          MB.MetaInputs.Insert(
            TMetaInput.Create(
              PeopleKindPrefix[i] + tidGate_WorkDemand,
              inputZero,
              inputIlimited,
              InputData( 0, 0 ),
              qIlimited,               
              TPushInput,
              WorkForce,     
              1000,
              mglBasic,
              [],
              sizeof(Sample.fWorkDemand[i]),
              Sample.Offset( Sample.fWorkDemand[i] )));
          MB.MetaInputs.Insert(
            TMetaInput.Create(
              PeopleKindPrefix[i] + tidGate_Workers,
              inputZero,
              inputIlimited,
              InputData( 0, 0 ),
              qIlimited,
              TPushInput,
              WorkForce,
              1000,
              mglBasic,
              [],
              sizeof(Sample.fWorkers[i]),
              Sample.Offset( Sample.fWorkers[i] )));
          MB.MetaInputs.Insert(
            TMetaInput.Create(
              PeopleKindPrefix[i] + tidGate_WorkRecycleIn,
              inputZero,
              inputIlimited,
              InputData( 0, 0 ),
              qIlimited,
              TPushInput,
              WorkForce,
              1000,
              mglBasic,
              [],
              sizeof(Sample.fWorkRecycleIn[i]),
              Sample.Offset( Sample.fWorkRecycleIn[i] )));
          MB.MetaInputs.Insert(
            TMetaInput.Create(
              PeopleKindPrefix[i] + tidGate_Inmigration,
              inputZero,
              inputIlimited,
              InputData( 0, 0 ),
              qIlimited,
              TPushInput,
              People,
              1000,
              mglBasic,
              [],
              sizeof(Sample.fInmigration[i]),
              Sample.Offset( Sample.fInmigration[i] )));
          MB.MetaInputs.Insert(
            TMetaInput.Create(
              PeopleKindPrefix[i] + tidGate_People,
              inputZero,
              inputIlimited,
              inputZero,
              qIlimited,
              TPushInput,
              People,
              1000,
              mglBasic,
              [],
              sizeof(Sample.fPopulation[i]),
              Sample.Offset( Sample.fPopulation[i] )));
          MB.MetaInputs.Insert(
            TMetaInput.Create(
              PeopleKindPrefix[i] + tidGate_RecycleOut,
              inputZero,
              inputIlimited,
              inputZero,
              qIlimited,
              TPushInput,
              People,
              1000,
              mglBasic,
              [],
              sizeof(Sample.fRecycleOut[i]),
              Sample.Offset( Sample.fRecycleOut[i] )));
          MB.MetaOutputs.Insert(
            TMetaOutput.Create(
              PeopleKindPrefix[i] + tidGate_WorkForceIn,
              fluidIlimited,
              TPushOutput,
              WorkForce,
              1000,
              [],
              sizeof(Sample.fWorkForceIn[i]),
              Sample.Offset( Sample.fWorkForceIn[i] )));
          MB.MetaOutputs.Insert(
            TMetaOutput.Create(
              PeopleKindPrefix[i] + tidGate_WorkForceOut,
              fluidIlimited,
              TPushOutput,
              WorkForce,
              1000,
              [],
              sizeof(Sample.fWorkForceOut[i]),
              Sample.Offset( Sample.fWorkForceOut[i] )));
          MB.MetaOutputs.Insert(
            TMetaOutput.Create(
              PeopleKindPrefix[i] + tidGate_ResInmigration,
              fluidIlimited,
              TPushOutput,
              People,
              1000,
              [],
              sizeof(Sample.fResInmigration[i]),
              Sample.Offset( Sample.fResInmigration[i] )));
          MB.MetaOutputs.Insert(
            TMetaOutput.Create(
              PeopleKindPrefix[i] + tidGate_ResEmigration,
              fluidIlimited,
              TPushOutput,
              People,
              1000,
              [],
              sizeof(Sample.fResEmigration[i]),
              Sample.Offset( Sample.fResEmigration[i] )));
          MB.MetaOutputs.Insert(
            TMetaOutput.Create(
              PeopleKindPrefix[i] + tidGate_Emigration,
              fluidIlimited,
              TPushOutput,
              People,
              1000,
              [],
              sizeof(Sample.fEmigration[i]),
              Sample.Offset( Sample.fEmigration[i] )));
          MB.MetaOutputs.Insert(
            TMetaOutput.Create(
              PeopleKindPrefix[i] + tidGate_TownPerformance,
              fluidIlimited,
              TPushOutput,
              nil,
              1000,
              [],
              sizeof(Sample.fPerformance[i]),
              Sample.Offset( Sample.fPerformance[i] )));
          MB.MetaOutputs.Insert(
            TMetaOutput.Create(
              PeopleKindPrefix[i] + tidGate_RecycleIn,
              fluidIlimited,
              TPushOutput,
              nil,
              1000,
              [],
              sizeof(Sample.fRecycleIn[i]),
              Sample.Offset( Sample.fRecycleIn[i] )));
        end;
          {
          fOffices        : TInputData;
          fOfsInmigration : TOutputData;
          fOfsEmigration  : TOutputData;
          fOfsDemand      : TInputData;
          fOfsRecycleIn   : TOutputData;
          fOfsRecycleOut  : TOutputData;
          }

      // Ads
      MB.MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Advertisement,
          inputZero,
          inputZero,
          //inputIlimited,
          InputData( 0, 0 ),
          0, //qIlimited,
          TPullInput, // >> TPushInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Advertisement]),
          1000,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fAdvertisement),                   
          Sample.Offset( Sample.fAdvertisement )));

      // Offices
      Offices := TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Offices]);
      MB.MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Offices,
          inputZero,
          inputIlimited,
          InputData( 0, 0 ),
          qIlimited,
          TPushInput,
          Offices,
          1000,
          mglBasic,
          [],
          sizeof(Sample.fOffices),
          Sample.Offset( Sample.fOffices )));
      MB.MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_OfsInmigration,
          fluidIlimited,
          TPushOutput,
          Offices,
          1000,
          [],
          sizeof(Sample.fOfsInmigration),
          Sample.Offset( Sample.fOfsInmigration )));
      MB.MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_OfsEmigration,
          fluidIlimited,
          TPushOutput,
          Offices,
          1000,
          [],
          sizeof(Sample.fOfsEmigration),
          Sample.Offset( Sample.fOfsEmigration )));
      MB.MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_OfsDemand,
          inputZero,
          inputIlimited,
          InputData( 0, 0 ),
          qIlimited,
          TPushInput,
          Offices,
          1000,
          mglBasic,
          [],
          sizeof(Sample.fOfsDemand),
          Sample.Offset( Sample.fOfsDemand )));
      MB.MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_OfsRecycleIn,
          fluidIlimited,
          TPushOutput,
          Offices,
          1000,
          [],
          sizeof(Sample.fOfsRecycleIn),
          Sample.Offset( Sample.fOfsRecycleIn )));
      MB.MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_OfsRecycleOut,
          inputZero,
          inputIlimited,
          InputData( 0, 0 ),
          qIlimited,
          TPushInput,
          Offices,
          1000,
          mglBasic,
          [],
          sizeof(Sample.fOfsRecycleOut),
          Sample.Offset( Sample.fOfsRecycleOut )));
      MB.Register( tidClassFamily_Blocks );

      with TFacilityKind.Create( aClusterName + tidFacilityKind_TownHall ) do
        begin
          Name        := 'Town Hall';
          SuperType   := tidFacilityKind_TownHall;
          ClusterName := aClusterName;
          Role        := rolImporter;
          Cacheable   := false;
          Register( tidClassFamily_FacilityKinds );
        end;
      with TMetaFacility.Create( anId, 'Town Hall', aVisualClass, TFacility ) do
        begin
          xSize  := axSize;
          ySize  := aySize;
          Level  := 1;
          Cacheable := false;
          Options := [mfcShowCompanyInText, mfcStopDisabled, mfcIgnoreZoning, mfcForbiddenRename];
          EvlStages.Insert( TEvlStage.Create( 'Normal', 'Normal', 'The Town Hall is ready to work', MB ));
          ClusterName := aClusterName;
          FacilityKind := aClusterName + tidFacilityKind_TownHall;
          Register( tidClassFamily_Facilities );
        end;
    end;

  procedure RegisterMetaFluids;
    var
      i : TPeopleKind;
    begin
      for i := low(i) to high(i) do
        begin
          TMetaFluid.Create(
            PeopleKindPrefix[i] + tidFluid_People,
            PeopleKindPrefix[i] + tidFluid_People,
            '',
            'persons',
            'persons/month',
            24*30,
            0,
            70,
            0 ).Register( tidClassFamily_Fluids );
          with TMetaFluid.Create(
            PeopleKindPrefix[i] + tidFluid_WorkForce,
            PeopleKindPrefix[i] + tidFluid_WorkForce,
            '',
            'workers',
            'workers/day',
            24,
            0,
            70,
            0 ) do
            begin
              Options := Options + [mfWorkForce];
              Register( tidClassFamily_Fluids );
            end;
        end;
      with TMetaFluid.Create(
        tidFluid_Offices,
        'Offices',
        'Offices flow',
        '',
        '',
        1,
        0,
        0,
        0 ) do
        begin
          Options := [];
          Register( 'Fluids' );
        end;
    end;

  procedure RegisterMetaInstances;
    begin
      RegisterWorldPopulator;
    end;

  procedure RegisterSurfaces;
    var
      i : TPeopleKind;
    begin
      for i := low(i) to high(i) do
        TSurface.Create( PeopleKindPrefix[i] + tidEnvironment_People, mtidPeopleKindName[i].Values[langDefault] );
    end;

  procedure RegisterBackup;
    begin
      TPublicFacilityInfoBackupAgent.Register( [TPublicFacilityInfo] );
      BackupInterfaces.RegisterClass( TWorldPopulator );
      BackupInterfaces.RegisterClass( TTownHall );
      BackupInterfaces.RegisterClass( TInhabitedTown );
      BackupInterfaces.RegisterClass( TInhabitedWorld );
    end;
    
  procedure RegisterTownParameters;
    var
      i : TPeopleKind;
    begin
      for i := low(i) to high(i) do
        begin
          TMetaTownParameter.Create( tidTownParameter_Salary + PeopleKindPrefix[i], PeopleKindPrefix[i] + 'Salary', true ).Register;
          TMetaTownParameter.Create( tidTownParameter_SalRatio + PeopleKindPrefix[i], PeopleKindPrefix[i] + 'SalRatio', true ).Register;
          TMetaTownParameter.Create( tidTownParameter_PrivateWorkers + PeopleKindPrefix[i], PeopleKindPrefix[i] + 'PrivateWorkers', true ).Register;
        end;
    end;

var
  i : TPeopleKind;

initialization

  for i := low(i) to high(i) do
    OutTotalWeight[i] :=
      OutWorkWeight[i] +
      OutResidentialsWeight[i] +
      OutQOLWeight[i] +
      OutUnemploymentWeight[i] +
      OutServicesWeight[i];

end.








