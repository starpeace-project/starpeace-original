unit ServiceBlock;

interface

  uses
    Classes, MetaInstances, Windows, Collection, Kernel, Protocol, Surfaces, WorkCenterBlock,
    BackupInterfaces, Accounts, CacheAgent, Languages;

  const
    tidClassFamily_Services = 'Services';
    tidNoFluid              = 'noFluid';
    noAdIndex               = -2;

  const                
    FairPrice    = 150;
    ModSupRatio  = 90;
    ModDemRatio  = 30;
    ModCompRatio = 80;

  const
    InertiaInc = 1;
    InertiaMax = 10;

  const
    DefaultAdtnImp = 0.05;

  const
    modServiceBlockQOL       = 3;
    modServiceBlockStrength  = 2;
    modServiceBlockPresence  = 10;

  const
    DefaultServiceQOLValue      = 100;
    DefaultServiceModifierValue = 100;

  const
    SupplyShare = 0.5;
    InvDelPrice = 0.05;

  const
    WFEfficLimit = 0.95;

  const
    MinServiceBlockCompSupport = 0.5;

  const
    htsToStrengh = 1/100;

  type
    TMetaService               = class;
    TMetaServiceBlock          = class;
    TMetaServiceEvaluator      = class;
    TMetaServiceEvaluatorInput = class;
    TServiceBlock              = class;

    TBuyProbability          = single;
    TPeopleBuyProbabilities  = array[TPeopleKind] of TBuyProbability;
    TPeopleServiceImportance = array[TPeopleKind] of single;
    TPeopleServiceMobility   = array[TPeopleKind] of single;

    TMetaService =
      class(TMetaInstance)
        public
          constructor Create(anId, Name     : string;
                             aFluidName     : string;
                             aMarketPrice   : TMoney;
                             aBuyProb       : array of TBuyProbability;
                             theImportances : array of single;
                             theMobility    : array of single);
        private
          fId          : string;
          fName        : string;
          fMarketPrice : TMoney;
          fMetaFluid   : TMetaFluid;
          fBuyProb     : TPeopleBuyProbabilities;
          fImportance  : TPeopleServiceImportance;
          fMobility    : TPeopleServiceMobility;
        private
          function GetTownParameterId(name : string) : string;
          function GetBuyProb(kind : TPeopleKind) : TBuyProbability;
          function GetImportance(kind : TPeopleKind) : single;
          function GetMobility(kind : TPeopleKind) : single;
        protected
          property MetaFluid : TMetaFluid read fMetaFluid write fMetaFluid;
        public
          property Id                              : string          read fId;
          property Name_MLS                        : string          read fName;
          property MarketPrice                     : TMoney          read fMarketPrice;
          property TownParameterIds[name : string] : string          read GetTownParameterId;
          property BuyProb[kind : TPeopleKind]     : TBuyProbability read GetBuyProb;
          property Importance[kind : TPeopleKind]  : single          read GetImportance;
          property Mobility[kind : TPeopleKind]    : single          read GetMobility;
      end;

    TMetaServiceBlock =
      class(TMetaWorkCenter)
        public
          constructor Create(anId           : string;
                             aCapacities    : array of TFluidValue;
                             aSupplyAccount : TAccountId;
                             aSalaryAccount : TAccountId;
                             aSalesAccount  : TAccountId;
                             aMaxAd         : TFluidValue;
                             aBlockClass    : CBlock);
          destructor Destroy; override;
        private
          fMetaServices : TCollection;
          fAdtnImp      : single;
          fPresenceFac  : single;
          fSalesAccount : TAccountId;
          fAdIndex      : integer;
        private
          procedure RegisterService(aService : TMetaServiceEvaluator);
        private
          function GetServiceCount : integer;
          function GetService(index : integer) : TMetaServiceEvaluator;
          function GetServiceByName(index : string) : TMetaServiceEvaluator;
          function GetAdIndex : integer;
        public
          property ServiceCount : integer read GetServiceCount;
          property Services[index : integer] : TMetaServiceEvaluator read GetService;
          property ServiceByName[index : string] : TMetaServiceEvaluator read GetServiceByName;
          property AdtnImp : single read fAdtnImp write fAdtnImp;
          property PresenceFac  : single read fPresenceFac write fPresenceFac;
          property AdIndex : integer read GetAdIndex;
      end;

    TMetaServiceEvaluator =
      class
        public
          constructor Create(aMetaService : TMetaService;
                             aName        : string;
                             aInitPrice   : TPercent;
                             aMaxServices : TFluidValue;
                             aWFPerc      : TPercent;
                             aEvlBuyProb  : array of TBuyProbability);
          destructor  Destroy; override;
        private
          fMetaService : TMetaService;
          fName        : string;
          fMaxServices : TFluidValue;
          fInitPrice   : TPercent;
          fIndex       : integer;
          fInputs      : TCollection;
          fEvlBuyProb  : TPeopleBuyProbabilities;
          fWFPerc      : TPercent;
        public
          procedure RegisterInput(ServiceInput : TMetaServiceEvaluatorInput);
        public
          property MetaService : TMetaService read fMetaService;
          property Name        : string       read fName;
          property Price       : TPercent     read fInitPrice;
          property Index       : integer      read fIndex;
        public
          procedure Register(Owner : TMetaServiceBlock);
          function  FormatOutput(perc : TPercent; ShowName : boolean) : string;
      end;

    TMetaServiceEvaluatorInput =
      class
        public
          constructor Create(aMetaInput : TMetaInput; aServShare, aImportance : single);
        private
          fMetaInput  : TMetaInput;
          fServShare  : single;
          fImportance : single;
      end;

    TServiceDataEntry =
      record
        Price    : TPercent;
        SalePerc : TPercent;
        SupRatio : TPercent;
        DemRatio : TPercent;
      end;

    TTownParamEntry =
      record
        Count    : array[TPeopleKind] of TTownParameter;
        Strength : array[TPeopleKind] of TTownParameter;
        Sales    : TTownParameter;
        Capacity : TTownParameter;
        Price    : TTownParameter;
        Quality  : TTownParameter;
      end;

    PServiceSurfaceIntegrators = ^TServiceSurfaceIntegrators;
    PServiceSurfaceModifiers   = ^TServiceSurfaceModifiers;
    PPeopleSurfaceIntegrators  = ^TPeopleSurfaceIntegrators;
    PServiceData               = ^TServiceData;
    PTownParamArray            = ^TTownParamArray;
    TServiceSurfaceIntegrators = array[0..0] of TSurfaceIntegrator;
    TServiceSurfaceModifiers   = array[0..0] of TSurfaceModifier;
    TPeopleSurfaceIntegrators  = array[TPeopleKind] of TSurfaceIntegrator;
    TServiceData               = array[0..0] of TServiceDataEntry;
    TTownParamArray            = array[0..0] of TTownParamEntry;

    {$M+}
    TServiceBlock =
      class(TFinanciatedWorkCenter)
        protected
          constructor Create(aMetaBlock : TMetaBlock; aFacility : TFacility); override;
        public
          destructor  Destroy; override;
        protected
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
        private
          //function  ServiceDeliverPrice(Service : TMetaServiceEvaluator) : single;
          //function  GetPriceOf(Input : TInput) : TPercent;
          //function  GetAvrgDeliverPrice : TPercent;
          procedure SetTownParameters;
        protected
          function  Evaluate : TEvaluationResult; override;
        public
          procedure AutoConnect( loaded : boolean );        override;
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup(Writer : IBackupWriter);  override;
          procedure Stop;                                   override;
          procedure Resume;                                 override;
          procedure CopySettingsFrom( Block : TBlock );     override;
        private
          fPeopleIntegrators  : TPeopleSurfaceIntegrators;
          fQOLModifier        : TSurfaceModifier;
          fSurfaceModifiers   : PServiceSurfaceModifiers;
          fSurfaceIntegrators : PServiceSurfaceIntegrators;
          fServiceData        : PServiceData;
          fTownParams         : PTownParamArray;
          fPotentialCustomers : array[TPeopleKind] of word;
          fActualCustomers    : array[TPeopleKind] of word;
          fInertia            : byte;
        protected
          procedure CreateBuffers;
          procedure DestroyBuffers;
        private
          function GetServiceCount : integer;
          function GetSurfaceModifier(index : integer) : TSurfaceModifier;
          function GetSurfaceIntegrator(index : integer) : TSurfaceIntegrator;
        published
          procedure RDOSetPrice(index, value : integer);
          function  RDOGetPrice(index : integer) : OleVariant;
          function  RDOGetDemand(index : integer) : OleVariant;
          function  RDOGetSupply(index : integer) : OleVariant;
        public
          property  ServiceData : PServiceData read fServiceData;
          property  ServiceCount : integer read GetServiceCount;
          property  Modifiers  [index : integer] : TSurfaceModifier   read GetSurfaceModifier;
          property  Integrators[index : integer] : TSurfaceIntegrator read GetSurfaceIntegrator;
        private
          function  GetAdvertisement : single;
          procedure SetAdvertisement(value : single);
        protected
          property  Advertisement : single read GetAdvertisement write SetAdvertisement;
        private
          function GetFairness(DelPrice : single) : single;
          function GetPeopleCount(kind : TPeopleKind) : integer;
          //function GetSevicePeopleCount(index : integer; kind : TPeopleKind) : integer;
        protected
          function GetBuyProb(Service : TMetaServiceEvaluator; kind : TPeopleKind) : single;
          function NeedsSupply(var Service : integer) : boolean;
          function GetMinDemandRatio(var Service : integer) : TPercent;
          function GetCompetition(Service : integer) : TPercent;
        public
          procedure StoreToCache( Cache : TObjectCache ); override;
      end;
    {$M-}

  function HitsToStrengh(hits : TFluidValue) : single;

implementation

  uses
    StdFluids, ClassStorage, PyramidalModifier, Population, BasicAccounts,
    SysUtils, MathUtils, SimHints, Logs;

  const
    MaxServiceInput = 10;
    MaxServices     = 6;

  type
    TServiceInput     = array[0..MaxServiceInput] of TFluidValue;
    TDemandTable      = array[0..MaxServices] of TFluidValue;
    TPeolpleBuyTable  = array[TPeopleKind, 0..MaxServices] of TFluidValue;
    TServiceQualities = array[0..MaxServices] of single;
    TShareOutTable    = array[0..MaxServices, 0..MaxServiceInput] of TFluidValue;
    TDelPriceTable    = array[0..MaxServices] of single;
    TServiceOpTable   = array[0..MaxServices] of boolean;
    TServiceSupTable  = array[0..MaxServices] of single;

  // TMetaService

  constructor TMetaService.Create(anId, Name     : string;
                                  aFluidName     : string;
                                  aMarketPrice   : TMoney;
                                  aBuyProb       : array of TBuyProbability;
                                  theImportances : array of single;
                                  theMobility    : array of single);
    var
      kind : TPeopleKind;
    begin
      inherited Create(anId);
      fId := anId;
      fName := Name;

      // fImportance := aImportance;
      if aFluidName <> tidNoFluid
        then
          begin
            fMetaFluid   := TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, aFluidName]);
            fMarketPrice := fMetaFluid.MarketPrice;
          end
        else fMarketPrice := aMarketPrice;

      // Set buy probabilities
      for kind := low(kind) to high(kind) do
        fBuyProb[kind] := aBuyProb[ord(kind)];

      // Set buy probabilities
      for kind := low(kind) to high(kind) do
        fImportance[kind] := theImportances[ord(kind)];

      // Set mobility
      for kind := low(kind) to high(kind) do
        fMobility[kind] := theMobility[ord(kind)];

      // Register Surface
      TSurface.Create(fId, Name);

      // Register Service
      Register(tidClassFamily_Services);

      // Register parameters
      for kind := low(kind) to high(kind) do
        begin
          TMetaTownParameter.Create(TownParameterIds[tidTownParameter_ServiceCount + PeopleKindPrefix[kind]], '', true).Register;
          TMetaTownParameter.Create(TownParameterIds[tidTownParameter_ServiceStrength + PeopleKindPrefix[kind]], '', true).Register;
        end;
      TMetaTownParameter.Create(TownParameterIds[tidTownParameter_ServiceSales], '', true).Register;
      TMetaTownParameter.Create(TownParameterIds[tidTownParameter_ServiceCapacity], '', true).Register;
      TMetaTownParameter.Create(TownParameterIds[tidTownParameter_ServicePrice], '', true).Register;
      TMetaTownParameter.Create(TownParameterIds[tidTownParameter_ServiceQuality], '', true).Register;
    end;

  function TMetaService.GetTownParameterId(name : string) : string;
    begin
      result := name + fId;
    end;

  function TMetaService.GetBuyProb(kind : TPeopleKind) : TBuyProbability;
    begin
      result := fBuyProb[kind];
    end;                                   

  function TMetaService.GetImportance(kind : TPeopleKind) : single;
    begin
      result := fImportance[kind];
    end;

  function TMetaService.GetMobility(kind : TPeopleKind) : single;
    begin
      result := fMobility[kind];
    end;


  // TMetaServiceBlock

  constructor TMetaServiceBlock.Create(anId           : string;
                                       aCapacities    : array of TFluidValue;
                                       aSupplyAccount : TAccountId;
                                       aSalaryAccount : TAccountId;
                                       aSalesAccount  : TAccountId;
                                       aMaxAd         : TFluidValue;
                                       aBlockClass    : CBlock);
    begin
      inherited Create(anId, aCapacities, aSupplyAccount, aSalesAccount, aSalaryAccount, aBlockClass);
      if aMaxAd > 0
        then RegisterCompanyInput(tidFluid_Advertisement, aMaxAd, true)
        else raise Exception.Create('Error no hits declared.');
      fSalesAccount := aSalesAccount;
      // Meta Services
      fMetaServices := Collection.TCollection.Create(0, rkBelonguer);
      // Default Aditional Importance
      fAdtnImp := DefaultAdtnImp;
      // Default Presence
      fPresenceFac := 1;
      // Undefined index
      fAdIndex := noAdIndex;
    end;

  destructor TMetaServiceBlock.Destroy;
    begin
      fMetaServices.Free;
      inherited;
    end;

  procedure TMetaServiceBlock.RegisterService(aService : TMetaServiceEvaluator);
    begin
      fMetaServices.Insert(aService);
      aService.fIndex := pred(fMetaServices.Count);
    end;

  function TMetaServiceBlock.GetServiceCount : integer;
    begin
      result := fMetaServices.Count;
    end;

  function TMetaServiceBlock.GetService(index : integer) : TMetaServiceEvaluator;
    begin
      result := TMetaServiceEvaluator(fMetaServices[index]);
    end;

  function TMetaServiceBlock.GetServiceByName(index : string) : TMetaServiceEvaluator;
    var
      i : integer;
    begin
      i := 0;
      while (i < fMetaServices.Count) and (Services[i].fMetaService.fId = index) do
        inc(i);
      if i < fMetaServices.Count
        then result := Services[i]
        else result := nil;
    end;

  function TMetaServiceBlock.GetAdIndex : integer;
    begin
      if fAdIndex = noAdIndex
        then fAdIndex := InputIndex[tidGate_Advertisement];
      result := fAdIndex;
    end;

  // TMetaServiceEvaluator

  constructor TMetaServiceEvaluator.Create(aMetaService : TMetaService;
                                           aName        : string;
                                           aInitPrice   : TPercent;
                                           aMaxServices : TFluidValue;
                                           aWFPerc      : TPercent;
                                           aEvlBuyProb  : array of TBuyProbability);
    var
      kind : TPeopleKind;
    begin
      inherited Create;
      fMetaService := aMetaService;
      fName        := aName;
      fInitPrice   := aInitPrice;
      fInputs      := TCollection.Create(0, rkBelonguer);
      fMaxServices := aMaxServices;
      fWFPerc      := aWFPerc;
      for kind := low(kind) to high(kind) do
        fEvlBuyProb[kind] := aEvlBuyProb[ord(kind)];
    end;

  destructor TMetaServiceEvaluator.Destroy;
    begin
      fInputs.Free;
      inherited;
    end;

  procedure TMetaServiceEvaluator.RegisterInput(ServiceInput : TMetaServiceEvaluatorInput);
    begin
      if ServiceInput.fMetaInput <> nil
        then fInputs.Insert(ServiceInput)
        else ServiceInput.Free;
    end;

  procedure TMetaServiceEvaluator.Register(Owner : TMetaServiceBlock);
    begin
      Owner.RegisterService(self);
    end;

  function TMetaServiceEvaluator.FormatOutput(perc : TPercent; ShowName : boolean) : string;
    begin
      result := Name + ' sales: ' + IntToStr(perc) + '%';
    end;

  // TMetaServiceEvaluatorInput

  constructor TMetaServiceEvaluatorInput.Create(aMetaInput : TMetaInput;
                                       aServShare, aImportance : single);
    begin
      inherited Create;
      fMetaInput  := aMetaInput;
      fServShare  := aServShare;
      fImportance := aImportance;
    end;

  // TServiceBlock

  constructor TServiceBlock.Create(aMetaBlock : TMetaBlock; aFacility : TFacility);
    var
      i : integer;
    begin
      inherited;
      CreateBuffers;
      with TMetaServiceBlock(MetaBlock) do
        for i := 0 to pred(ServiceCount) do
          fServiceData[i].Price := Services[i].Price;
    end;

  destructor TServiceBlock.Destroy;
    var
      kind : TPeopleKind;
      i    : integer;
    begin
      for kind := low(kind) to high(kind) do
        fPeopleIntegrators[kind].Delete;
      if ServiceCount > 0
        then
          begin
            for i := 0 to pred(ServiceCount) do
              begin
                fSurfaceModifiers[i].Delete;
                fSurfaceIntegrators[i].Delete;
              end;
          end;
      fQOLModifier.Delete;
      DestroyBuffers;
      inherited;
    end;

  function TServiceBlock.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    var
      i, SvrCount   : integer;
      SellsAbstract : string;
      Service       : integer;
    begin
      result := inherited GetStatusText(kind, ToTycoon);
      case kind of
        sttMain :
          if not Facility.CriticalTrouble
            then
              begin
                SellsAbstract := '';
                SvrCount := TMetaServiceBlock(MetaBlock).ServiceCount;
                for i := 0 to pred(SvrCount) do
                  with TMetaServiceBlock(MetaBlock).Services[i] do
                    begin
                      SellsAbstract := SellsAbstract + FormatOutput(fServiceData[i].SalePerc, SvrCount > 0);
                      if i < pred(SvrCount)
                        then SellsAbstract := SellsAbstract + LineBreak;
                    end;
                result := result + SellsAbstract;
              end;
        sttSecondary :
          if not Facility.CriticalTrouble
            then
              begin
                SellsAbstract := SellsAbstract +
                  'Potential customers (per day): ' +
                  IntToStr(fPotentialCustomers[pkHigh]) + ' hi, ' +
                  IntToStr(fPotentialCustomers[pkMiddle]) + ' mid, ' +
                  IntToStr(fPotentialCustomers[pkLow]) + ' lo.  Actual customers: ' +
                  IntToStr(fActualCustomers[pkHigh]) + ' hi, ' +
                  IntToStr(fActualCustomers[pkMiddle]) + ' mid, ' +
                  IntToStr(fActualCustomers[pkLow]) + ' lo.';
                result := result + SellsAbstract;
              end;
        sttHint :
          case Facility.AccessLevelOf( ToTycoon ) of
            acsFull, acsModerate :
              begin
                if not Facility.CriticalTrouble
                  then
                    if fInertia = InertiaMax
                      then
                        begin
                          if Facility.Trouble and facNeedsWorkForce <> 0
                            then result := GetHintText(hidBlockNeedsWorkForce, [PeopleKindName[WFRequired]])
                            else
                              if Facility.Trouble and facNeedCompSupport <> 0
                                then result := GetHintText(hidNeedsCompSupport, [0])
                                else
                                  if NeedsSupply(Service)
                                    then
                                      with TMetaServiceBlock(MetaBlock).Services[Service] do
                                        result := GetHintText(hidServiceLowSupplies, [Name])
                                    else
                                      if GetMinDemandRatio(Service) < ModDemRatio
                                        then
                                          if GetCompetition(Service) > ModCompRatio
                                            then result := GetHintText(hidServiceHighCompetition, [0])
                                            else result := GetHintText(hidServiceWrongPlace, [0])
                                        else result := GetHintText(hidVisitWebSite, [0]);
                        end
                      else result := GetHintText(hidServiceOpening, [0])
                  else GetHintText(hidVisitWebSite, [0]);
              end;
          end;
      end;
    end;

  function TServiceBlock.Evaluate : TEvaluationResult;
    var
      DemandRatios  : TDemandTable;
      BuyTable      : TPeolpleBuyTable;
      Qualities     : TServiceQualities;
      ServiceSup    : TServiceSupTable;
      hiPeople      : TFluidValue;
      miPeople      : TFluidValue;
      loPeople      : TFluidValue;
      hiStarving    : TFluidValue;
      miStarving    : TFluidValue;
      loStarving    : TFluidValue;
      exthiPeople   : TFluidValue;
      extmiPeople   : TFluidValue;
      extloPeople   : TFluidValue;
      tmpStrength   : single;
      ClassDemand   : array[TPeopleKind] of TFluidValue;
      Service       : TMetaServiceEvaluator;
      ServiceInput  : TMetaServiceEvaluatorInput;
      Input         : TInput;
      SvrIdx        : integer;
      InpIdx        : integer;
      ParcheIdx     : integer;
      SrvInpCnt     : integer;
      SrvCount      : integer;
      Fairness      : single;
      Strength      : single;
      sum           : single;
      K             : single;
      Ks            : single;
      LayerDemand   : single;
      LayerPot      : single;
      Presence      : TSurfaceValue;
      IntValue      : TSurfaceValue;
      SupRatio      : single;
      DemRatio      : single;
      svrCapacity   : single;
      frstPurchases : single;
      scndPurchases : single;
      sumPurchases  : single;
      InertiaRatio  : single;
      DelPrices     : TDelPriceTable;
      theDT         : byte;
      WFEffic       : single;
      OpRatio       : single;
      TownHall      : TTownHall;
      TownParam     : TTownParameter;
      CompSup       : single;
      CapRatio      : single;
      AdStrgh       : single;
    begin
      result := inherited Evaluate;
      if not Facility.CriticalTrouble
        then
          begin
            // Company support
            if Facility.CompanyDir <> nil
              then CompSup := Facility.CompanyDir.Support
              else CompSup := 0;
            // No demand at the beggining
            FillChar(ClassDemand, sizeof(ClassDemand), 0);
            // Clear the old values
            FillChar(fPotentialCustomers, sizeof(fPotentialCustomers), 0);
            FillChar(fActualCustomers, sizeof(fPotentialCustomers), 0);
            // Check the efficiency
            WFEffic := WorkForceEfficiency;
            theDT := dt;
            // People
            hiPeople := fPeopleIntegrators[pkHigh].Value;
            miPeople := fPeopleIntegrators[pkMiddle].Value;
            loPeople := fPeopleIntegrators[pkLow].Value;
            // Inertia
            InertiaRatio := fInertia/InertiaMax;
            // Ads
            AdStrgh := HitsToStrengh(Advertisement);
            // For each service
            SrvCount := ServiceCount;
            for SvrIdx := 0 to pred(SrvCount) do
              begin
                Service   := TMetaServiceEvaluator(TMetaServiceBlock(MetaBlock).Services[SvrIdx]);
                K         := 0;
                Ks        := 0;
                SrvInpCnt := Service.fInputs.Count;
                ServiceSup[SvrIdx] := 1;
                for InpIdx := 0 to pred(SrvInpCnt) do
                  begin
                    ServiceInput := TMetaServiceEvaluatorInput(Service.fInputs[InpIdx]);
                    Input := Inputs[ServiceInput.fMetaInput.Index];
                    K     := K  + Input.FluidData.Q*ServiceInput.fImportance*Input.FluidData.K;
                    Ks    := Ks + Input.FluidData.Q*ServiceInput.fImportance;
                    if Input.FluidData.Q > 0
                      then ServiceSup[SvrIdx] := realmin(ServiceSup[SvrIdx], Input.FluidData.Q/(TheDt*Input.MetaInput.MaxFluid.Q))
                      else ServiceSup[SvrIdx] := 0;
                  end;

                // Quality
                if Ks > 0
                  then K := realmax(1, K/Ks)
                  else K := 1;
                Qualities[SvrIdx] := K; // >> Fix it!!!

                // Set Modifier value
                Modifiers[SvrIdx].FixedArea := true;
                Modifiers[SvrIdx].Value := TMetaServiceBlock(MetaBlock).PresenceFac*modServiceBlockStrength + (fServiceData[SvrIdx].SupRatio/100)*AdStrgh;

                // Presence
                IntValue := Integrators[SvrIdx].Value;
                if IntValue > 0
                  then Presence := Modifiers[SvrIdx].Value/IntValue
                  else Presence := 0;

                // Calculate fairness
                DelPrices[SvrIdx] := 1; // >> ServiceDeliverPrice(Service);
                Fairness := GetFairness(2*fServiceData[SvrIdx].Price*DelPrices[SvrIdx]);

                // Starving people
                TownHall   := TTownHall(TInhabitedTown(Facility.Town).TownHall.CurrBlock);
                TownParam  := fTownParams[SvrIdx].Count[pkHigh];
                hiStarving := Service.MetaService.Mobility[pkHigh]*realmax(0, Service.MetaService.BuyProb[pkHigh]*TownHall.LastPop[pkHigh].Q - TownParam.Value);
                TownParam  := fTownParams[SvrIdx].Count[pkMiddle];
                miStarving := Service.MetaService.Importance[pkMiddle]*realmax(0, Service.MetaService.BuyProb[pkMiddle]*TownHall.LastPop[pkMiddle].Q - TownParam.Value);
                TownParam  := fTownParams[SvrIdx].Count[pkLow];
                loStarving := Service.MetaService.Importance[pkLow]*realmax(0, Service.MetaService.BuyProb[pkLow]*TownHall.LastPop[pkLow].Q - TownParam.Value);

                // High people demand
                Strength    := sqr(sqr(K/100))*sqrt(Fairness);
                TownParam   := fTownParams[SvrIdx].Strength[pkHigh];
                tmpStrength := Strength*GetBuyProb(Service, pkHigh)*realcond(hiPeople > 0, 1, 0.2)*(1 + AdStrgh);
                TownParam.CurrValue := TownParam.CurrValue + tmpStrength;
                if TownParam.Value > 0
                  then exthiPeople := hiStarving*tmpStrength/TownParam.Value
                  else exthiPeople := 0;

                LayerPot := InertiaRatio*Presence*hiPeople*GetBuyProb(Service, pkHigh);
                fPotentialCustomers[pkHigh] := fPotentialCustomers[pkHigh] + SmartRound(24*(LayerPot + exthiPeople));
                LayerDemand := exthiPeople + LayerPot*realmin(1, Strength);
                DemandRatios[SvrIdx] := LayerDemand;
                BuyTable[pkHigh, SvrIdx] := LayerPot*Strength;
                ClassDemand[pkHigh]  := ClassDemand[pkHigh] + LayerDemand;

                // Middle people demand
                Strength    := sqr(K/100)*sqr(Fairness);
                TownParam   := fTownParams[SvrIdx].Strength[pkMiddle];
                tmpStrength := Strength*GetBuyProb(Service, pkMiddle)*realcond(miPeople > 0, 1, 0.1)*(1 + AdStrgh);
                TownParam.CurrValue := TownParam.CurrValue + tmpStrength;
                if TownParam.Value > 0
                  then extmiPeople := miStarving*tmpStrength/TownParam.Value
                  else extmiPeople := 0;

                LayerPot := InertiaRatio*Presence*miPeople*GetBuyProb(Service, pkMiddle);
                fPotentialCustomers[pkMiddle] := fPotentialCustomers[pkMiddle] + SmartRound(24*(LayerPot + extmiPeople));
                LayerDemand := extmiPeople + LayerPot*realmin(1, Strength);
                DemandRatios[SvrIdx] := DemandRatios[SvrIdx] + LayerDemand;
                BuyTable[pkMiddle, SvrIdx] := LayerPot*Strength;
                ClassDemand[pkMiddle] := ClassDemand[pkMiddle] + LayerDemand;

                // Low people demand
                Strength    := (K/100)*sqr(sqr(Fairness));
                TownParam   := fTownParams[SvrIdx].Strength[pkLow];
                tmpStrength := Strength*GetBuyProb(Service, pkLow)*realcond(loPeople > 0, 1, 0.05)*(1 + AdStrgh);
                TownParam.CurrValue := TownParam.CurrValue + tmpStrength;
                if TownParam.Value > 0
                  then extloPeople := loStarving*tmpStrength/TownParam.Value
                  else extloPeople := 0;

                LayerPot := InertiaRatio*Presence*loPeople*GetBuyProb(Service, pkLow);
                fPotentialCustomers[pkLow] := fPotentialCustomers[pkLow] + SmartRound(24*(LayerPot + extloPeople));
                LayerDemand := extloPeople + LayerPot*realmin(1, Strength);
                DemandRatios[SvrIdx] := DemandRatios[SvrIdx] + LayerDemand;
                BuyTable[pkLow, SvrIdx] := LayerPot*Strength;
                ClassDemand[pkLow] := ClassDemand[pkLow] + LayerDemand;
              end;

            // Clear Demands
            for SvrIdx := 0 to pred(SrvCount) do
              begin
                Service := TMetaServiceEvaluator(TMetaServiceBlock(MetaBlock).Services[SvrIdx]);
                for InpIdx := 0 to pred(Service.fInputs.Count) do
                  begin
                    ServiceInput := TMetaServiceEvaluatorInput(Service.fInputs[InpIdx]);
                    Input := Inputs[ServiceInput.fMetaInput.Index];
                    Input.ActualMaxFluid.Q := 0;
                  end;
              end;

            // Sum of purchases
            sumPurchases := 0;
            // Calculate profit and plan next period
            OpRatio := 0;
            for SvrIdx := 0 to pred(SrvCount) do
              begin
                // Service
                Service := TMetaServiceEvaluator(TMetaServiceBlock(MetaBlock).Services[SvrIdx]);

                // Calculate supply ratio
                SupRatio := ServiceSup[SvrIdx];

                // Dem Ratio
                DemRatio := DemandRatios[SvrIdx]/Service.fMaxServices;

                // Update hint data
                fServiceData[SvrIdx].SupRatio := round(SupRatio*100);
                fServiceData[SvrIdx].DemRatio := min(high(fServiceData[0].DemRatio), round(100*DemRatio));

                // Calculate capacity
                svrCapacity := WFEffic*CompSup*SupRatio*Service.fMaxServices;

                // Local Clients
                sum := BuyTable[pkHigh, SvrIdx] + BuyTable[pkMiddle, SvrIdx] + BuyTable[pkLow, SvrIdx];

                frstPurchases := realmin(sum, svrCapacity);
                svrCapacity   := svrCapacity - frstPurchases;
                scndPurchases := realmin(svrCapacity, DemandRatios[SvrIdx] - sum);
                sumPurchases  := sumPurchases + frstPurchases + scndPurchases;

                if sum > 0
                  then
                    begin
                      TownParam := fTownParams[SvrIdx].Count[pkHigh];
                      TownParam.CurrValue := TownParam.CurrValue + frstPurchases*BuyTable[pkHigh, SvrIdx]/sum;
                      TownParam := fTownParams[SvrIdx].Count[pkMiddle];
                      TownParam.CurrValue := TownParam.CurrValue + frstPurchases*BuyTable[pkMiddle, SvrIdx]/sum;
                      TownParam := fTownParams[SvrIdx].Count[pkLow];
                      TownParam.CurrValue := TownParam.CurrValue + frstPurchases*BuyTable[pkLow, SvrIdx]/sum;
                    end;

                // Set parameters
                TownParam := fTownParams[SvrIdx].Sales;
                TownParam.CurrValue := TownParam.CurrValue + frstPurchases + scndPurchases;

                TownParam := fTownParams[SvrIdx].Capacity;
                TownParam.CurrValue := TownParam.CurrValue + Service.fMaxServices;

                TownParam := fTownParams[SvrIdx].Price;
                TownParam.CurrValue := TownParam.CurrValue + (frstPurchases + scndPurchases)*(DelPrices[SvrIdx]*2*fServiceData[SvrIdx].Price);
                TownParam.IncCount(frstPurchases + scndPurchases);

                TownParam := fTownParams[SvrIdx].Quality;
                TownParam.CurrValue := TownParam.CurrValue + (frstPurchases + scndPurchases)*Qualities[SvrIdx];
                TownParam.IncCount(frstPurchases + scndPurchases);

                // Generate money
                BlockGenMoney(DelPrices[SvrIdx]*(2*fServiceData[SvrIdx].Price/100)*Service.fMetaService.MarketPrice*(frstPurchases + scndPurchases)*theDT, TMetaServiceBlock(MetaBlock).fSalesAccount);

                // Plan for next period
                DemRatio := realmin(1, WFEffic*CompSup*DemRatio);
                for InpIdx := 0 to pred(Service.fInputs.Count) do
                  begin
                    ServiceInput := TMetaServiceEvaluatorInput(Service.fInputs[InpIdx]);
                    Input := Inputs[ServiceInput.fMetaInput.Index];
                    if (DemRatio <= SupRatio)
                      then
                        Input.ActualMaxFluid.Q :=
                          Input.ActualMaxFluid.Q +
                          ServiceInput.fServShare*
                          Input.MetaInput.MaxFluid.Q*
                          DemRatio
                      else
                        Input.ActualMaxFluid.Q :=
                          Input.ActualMaxFluid.Q +
                          ServiceInput.fServShare*
                          Input.MetaInput.MaxFluid.Q*
                          realmin(DemRatio, SupRatio + 0.1);
                  end;

                // Calculate sell percent
                fServiceData[SvrIdx].SalePerc := round(100*(frstPurchases + scndPurchases)/Service.fMaxServices);
                // Add Operation Ratio
                OpRatio := OpRatio + realmax(1, (frstPurchases + scndPurchases)/Service.fMaxServices);
              end;

            // Actual demand
            sum := ClassDemand[pkHigh] + ClassDemand[pkMiddle] + ClassDemand[pkLow];
            if sum > 0
              then
                begin
                  fActualCustomers[pkHigh]   := SmartRound(24*(sumPurchases*ClassDemand[pkHigh]/sum));
                  fActualCustomers[pkMiddle] := SmartRound(24*(sumPurchases*ClassDemand[pkMiddle]/sum));
                  fActualCustomers[pkLow]    := SmartRound(24*(sumPurchases*ClassDemand[pkLow]/sum));
                end;

            if fInertia < InertiaMax
              then fInertia := min(InertiaMax, fInertia + InertiaInc*theDT);

            OpRatio := realmax(1, OpRatio/SrvCount);

            HireWorkForce(1); //HireWorkForce(OpRatio);
            SetCargoValue( carLight, -10*OpRatio );
          end;
    end;

  procedure TServiceBlock.SetTownParameters;
    var
      i    : integer;
      kind : TPeopleKind;
    begin
      // Set the Parameters References
      for i := 0 to pred(ServiceCount) do
        with TMetaServiceBlock(MetaBlock).Services[i].MetaService do
          begin
            for kind := low(kind) to high(kind) do
              begin
                fTownParams[i].Count[kind]   := Facility.Town.Parameters[TownParameterIds[tidTownParameter_ServiceCount + PeopleKindPrefix[kind]]];
                fTownParams[i].Strength[kind] := Facility.Town.Parameters[TownParameterIds[tidTownParameter_ServiceStrength + PeopleKindPrefix[kind]]];
              end;
            fTownParams[i].Sales      := Facility.Town.Parameters[TownParameterIds[tidTownParameter_ServiceSales]];
            fTownParams[i].Capacity   := Facility.Town.Parameters[TownParameterIds[tidTownParameter_ServiceCapacity]];
            fTownParams[i].Price      := Facility.Town.Parameters[TownParameterIds[tidTownParameter_ServicePrice]];
            fTownParams[i].Quality    := Facility.Town.Parameters[TownParameterIds[tidTownParameter_ServiceQuality]];
          end;
    end;

  procedure TServiceBlock.AutoConnect( loaded : boolean );
    var
      kind   : TPeopleKind;
      i      : integer;
      Area   : TRect;
      MB     : TMetaServiceBlock;
      Input  : TInput;
    begin
      inherited;
      MB   := TMetaServiceBlock(MetaBlock);
      Area := GetArea(round(MB.PresenceFac*modServiceBlockPresence), amdExcludeBlock);
      // Create QOL mofifier
      fQOLModifier :=
        TPyramidalModifier.Create(
          tidEnvironment_QOL,
          Point(xOrigin, yOrigin),
          DefaultServiceQOLValue,
          modServiceBlockQOL);
      // Create people integrators
      for kind := low(kind) to high(kind) do
        fPeopleIntegrators[kind] :=
          TSurfaceIntegrator.Create(
            PeopleKindPrefix[kind] + tidEnvironment_People,
            Area);
      // Create other surfaces
      for i := 0 to pred(ServiceCount) do
        begin
          // Create integrator
          fSurfaceIntegrators[i] :=
            TSurfaceIntegrator.Create(
              TMetaServiceBlock(MetaBlock).Services[i].MetaService.Id,
              Area);
          // Create modifier
          fSurfaceModifiers[i] :=
            TPyramidalModifier.Create(
              TMetaServiceBlock(MetaBlock).Services[i].MetaService.Id,
              Point(xOrigin, yOrigin),
              DefaultServiceModifierValue,
              MB.PresenceFac*modServiceBlockStrength);
          fSurfaceModifiers[i].Area := Area;
        end;
      // Set the town parameters
      SetTownParameters;

      // Ads
      Input := InputsByName[tidGate_Advertisement];
      if Input <> nil
        then
          if not loaded
            then Input.ActualMaxFluid.Q := 0;
    end;

  procedure TServiceBlock.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      CreateBuffers;
      // Read the service data
      Reader.ReadBuffer('ServiceData', fServiceData^, nil, ServiceCount*sizeof(fServiceData[0]));
      // Read inertia
      fInertia := min(InertiaMax, Reader.ReadByte('Inertia', 0));
      // Set the town parameters
      SetTownParameters;
    end;

  procedure TServiceBlock.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      // Write service data
      Writer.WriteBuffer('ServiceData', fServiceData^, ServiceCount*sizeof(fServiceData[0]));
      Writer.WriteByte('Inertia', fInertia);
    end;

  procedure TServiceBlock.Stop;
    var
      i : integer;
    begin
      inherited;
      fQOLModifier.Value := 0;
      fInertia := 0;
      for i := 0 to pred(ServiceCount) do
        begin
          Modifiers[i].Value := 0;
          with fServiceData[i], TMetaServiceBlock(MetaBlock) do
            begin
              SalePerc := 0;
              DemRatio := 0;
            end;
        end;
    end;

  procedure TServiceBlock.Resume;
    var
      i : integer;
    begin
      inherited;
      fQOLModifier.Value := DefaultServiceQOLValue;
      fInertia := 1;
      for i := 0 to pred(ServiceCount) do
        Modifiers[i].Value := DefaultServiceModifierValue;
    end;

  procedure TServiceBlock.CopySettingsFrom( Block : TBlock );
    var
      srvCnt : integer;
      i      : integer;
    begin
      inherited;
      if Block.Facility.MetaFacility.FacId = Facility.MetaFacility.FacId
        then
          begin
            srvCnt := min(TServiceBlock(Block).ServiceCount, ServiceCount);
            for i := 0 to pred(srvCnt) do
              fServiceData[i].Price := TServiceBlock(Block).fServiceData[i].Price;
          end;
    end;

  procedure TServiceBlock.CreateBuffers;
    var
      SvrCnt : integer;
    begin
      SvrCnt := ServiceCount;
      GetMem(fSurfaceModifiers, SvrCnt*sizeof(fSurfaceModifiers[0]));
      GetMem(fSurfaceIntegrators, SvrCnt*sizeof(fSurfaceIntegrators[0]));
      GetMem(fServiceData, SvrCnt*sizeof(fServiceData[0]));
      GetMem(fTownParams, SvrCnt*sizeof(fTownParams[0]));
      FillChar(fSurfaceModifiers^, SvrCnt*sizeof(fSurfaceModifiers[0]), 0);
      FillChar(fSurfaceIntegrators^, SvrCnt*sizeof(fSurfaceIntegrators[0]), 0);
      FillChar(fServiceData^, SvrCnt*sizeof(fServiceData[0]), 0);
    end;

  procedure TServiceBlock.DestroyBuffers;
    begin
      FreeMem(fSurfaceModifiers);
      FreeMem(fSurfaceIntegrators);
      FreeMem(fServiceData);
      FreeMem(fTownParams);
    end;

  function TServiceBlock.GetServiceCount : integer;
    begin
      result := TMetaServiceBlock(MetaBlock).ServiceCount;
    end;

  function TServiceBlock.GetSurfaceModifier(index : integer) : TSurfaceModifier;
    begin
      result := fSurfaceModifiers[index];
    end;

  function TServiceBlock.GetSurfaceIntegrator(index : integer) : TSurfaceIntegrator;
    begin
      result := fSurfaceIntegrators[index];
    end;

  function TServiceBlock.GetAdvertisement : single;
    var
      AdvInput : PCompanyInputData;
    begin
      AdvInput := CompanyInputs[0];
      if AdvInput <> nil
        then result := AdvInput.Q/dt
        else result := 0; 
    end;

  procedure TServiceBlock.SetAdvertisement(value : single);
    var
      index : integer;
      Input : TInput;
    begin
      index := TMetaServiceBlock(MetaBlock).AdIndex;
      if index <> noIndex
        then
          begin
            Input  := Inputs[index];
            Input.ActualMaxFluid.Q := Input.MetaInput.MaxFluid.Q*value;
          end
    end;

  procedure TServiceBlock.RDOSetPrice(index, value : integer);
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Service SetPrice: ' + IntToStr(index) + ', ' + IntToStr(value) );
      try
        fServiceData[index].Price := round(value/2);
        Facility.UpdateCache;
      except
      end;
      Logs.Log( tidLog_Survival, 'OK!');
    end;

  function TServiceBlock.RDOGetPrice(index : integer) : OleVariant;
    begin
      try
        if index < ServiceCount
          then result := 2*fServiceData[index].Price
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TServiceBlock.RDOGetDemand(index : integer) : OleVariant;
    begin
      try
        if index < ServiceCount
          then result := fServiceData[index].DemRatio
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TServiceBlock.RDOGetSupply(index : integer) : OleVariant;
    begin
      try
        if index < ServiceCount
          then result := fServiceData[index].SupRatio
          else result := 0;
      except
        result := 0;
      end;
    end;

  function TServiceBlock.GetFairness(DelPrice : single) : single;
    begin
      if DelPrice >= FairPrice
        then result := sqr(FairPrice/DelPrice)
        else result := 1 + sqr((FairPrice - DelPrice)/FairPrice);
    end;

  function TServiceBlock.GetPeopleCount(kind : TPeopleKind) : integer;
    var
      i      : integer;
      prob   : TBuyProbability;
      PplCnt : TFluidValue;
    begin
      PplCnt := fPeopleIntegrators[kind].Value;
      prob   := 0;
      for i := 0 to pred(ServiceCount) do
        prob := prob + GetBuyProb(TMetaServiceBlock(MetaBlock).Services[i], kind);
      result := SmartRound(prob*PplCnt);
    end;

{
  function TServiceBlock.GetSevicePeopleCount(index : integer; kind : TPeopleKind) : integer;
    begin
      result := SmartRound(TMetaServiceBlock(MetaBlock).Services[index].MetaService.fBuyProb[kind]*fPeopleIntegrators[kind].Value);
    end;
}

  function TServiceBlock.GetBuyProb(Service : TMetaServiceEvaluator; kind : TPeopleKind) : single;
    begin
      result := Service.fEvlBuyProb[kind]*Service.MetaService.fBuyProb[kind];
    end;

  function TServiceBlock.NeedsSupply(var Service : integer) : boolean;
    var
      i     : integer;
      delta : integer;
    begin
      delta   := min(100, fServiceData[0].DemRatio) - fServiceData[0].SupRatio;
      Service := 0;
      for i := 1 to pred(ServiceCount) do
        if delta > min(100, fServiceData[i].DemRatio) - fServiceData[i].SupRatio
          then
            begin
              delta   := min(100, fServiceData[i].DemRatio) - fServiceData[i].SupRatio;
              Service := i;
            end;
      result := delta > 0;
    end;

  function TServiceBlock.GetMinDemandRatio(var Service : integer) : TPercent;
    var
      i : integer;
    begin
      result  := fServiceData[0].DemRatio;
      Service := 0;
      for i := 1 to pred(ServiceCount) do
        if result < fServiceData[i].DemRatio
          then
            begin
              result  := fServiceData[i].DemRatio;
              Service := i;
            end;
    end;

  function TServiceBlock.GetCompetition(Service : integer) : TPercent;
    //var
      //IntValue : TSurfaceValue;
    begin
      {
      IntValue := Integrators[Service].Value;
      if IntValue > 0
        then result := round(100*(1 - Modifiers[Service].Value/IntValue))
        else result := 0;
      }
      result := 0;
    end;

  procedure TServiceBlock.StoreToCache(Cache : TObjectCache);
    var
      i   : integer;
      Svr : TMetaServiceEvaluator;
    begin
      inherited;
      Cache.WriteInteger('ServiceCount', ServiceCount);
      for i := 0 to pred(ServiceCount) do
        begin
          Svr := TMetaServiceBlock(MetaBlock).Services[i];
          Cache.WriteString('srvNames' + IntToStr(i), Svr.Name);
          Cache.WriteInteger('srvIndexes' + IntToStr(i), i);
          // Price
          Cache.WriteInteger('srvPrices' + IntToStr(i), 2*fServiceData[i].Price);
          Cache.WriteCurrency('srvMarketPrices' + IntToStr(i), Svr.MetaService.MarketPrice);
          Cache.WriteCurrency('srvPriceValues' + IntToStr(i), (2*fServiceData[i].Price/100)*Svr.MetaService.MarketPrice);
          // Sales
          Cache.WriteInteger('srvSales' + IntToStr(i), fServiceData[i].SalePerc);
          // Supply
          Cache.WriteInteger('srvSupplies' + IntToStr(i), fServiceData[i].SupRatio);
          // Demand
          Cache.WriteInteger('srvDemands' + IntToStr(i), fServiceData[i].DemRatio);
          // Customers
          //Cache.WriteInteger('srvPotHi' + IntToStr(i), GetSevicePeopleCount(i, pkHigh));
          //Cache.WriteInteger('srvPotMi' + IntToStr(i), GetSevicePeopleCount(i, pkMiddle));
          //Cache.WriteInteger('srvPotLo' + IntToStr(i), GetSevicePeopleCount(i, pkLow));
        end;
      // Potencial customers
      Cache.WriteInteger('hiPotCustomers', GetPeopleCount(pkHigh));
      Cache.WriteInteger('miPotCustomers', GetPeopleCount(pkMiddle));
      Cache.WriteInteger('loPotCustomers', GetPeopleCount(pkLow));
      // Actual customers
      Cache.WriteInteger('hiActCustomers', fPotentialCustomers[pkHigh]);
      Cache.WriteInteger('miActCustomers', fPotentialCustomers[pkMiddle]);
      Cache.WriteInteger('loActCustomers', fPotentialCustomers[pkLow]);
    end;

  function HitsToStrengh(hits : TFluidValue) : single;
    begin
      result := hits*htsToStrengh;
    end;

end.
