unit ServiceBlock;

interface

  uses
    Classes, MetaInstances, Windows, Collection, Kernel, Protocol, Surfaces, WorkCenterBlock,
    BackupInterfaces, Accounts, CacheAgent, Inventions, Languages;

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
    InertiaMax = 60;

  const
    DefaultAdtnImp = 0.05;

  const
    modServiceBlockQOL              = 3;
    modServiceBlockStrength         = 100;
    modServiceBlockPresence         = 10;
    modServiceBlockInertiaMax       = 70;
    modServiceBlockMinBlockStrength = 10;

  const
    DefaultServiceQOLValue      = 100;
    DefaultServiceModifierValue = modServiceBlockStrength;

  const
    SupplyShare = 0.5;
    InvDelPrice = 0.05;

  const
    WFEfficLimit = 0.95;

  const
    MinServiceBlockCompSupport = 0.5;

  const
    htsToStrengh = 1/100;

  const
    SPWeight = 0.9;
    APWeight = 0.1;

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
          fName_MLS    : TMultiString;
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
          property Name_MLS                        : TMultiString    read fName_MLS;
          property MarketPrice                     : TMoney          read fMarketPrice;
          property TownParameterIds[name : string] : string          read GetTownParameterId;
          property BuyProb[kind : TPeopleKind]     : TBuyProbability read GetBuyProb;
          property Importance[kind : TPeopleKind]  : single          read GetImportance;
          property Mobility[kind : TPeopleKind]    : single          read GetMobility;
        public
          procedure RetrieveTexts( Container : TDictionary ); override;
          procedure StoreTexts   ( Container : TDictionary ); override;
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
          fHeight       : single;                       
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
          property Height : single read fHeight write fHeight;
        protected
          procedure ModifyMetaFacility( MetaFacility : TMetaFacility ); override;
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
          property Price       : TPercent     read fInitPrice;
          property Index       : integer      read fIndex;
          property MaxServices : TFluidValue  read fMaxServices;
        public
          procedure Register(Owner : TMetaServiceBlock);
          function  FormatOutput(perc : TPercent; ShowName : boolean; LangId : TLanguageId) : string;
        protected
          procedure CorrectMaxService(aNewMax : TFluidValue);
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
      class(TFinanciatedWorkCenter, IAreaAgent)
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
          function GetFairness( DelPrice, SpendingPower, AvgPrice : single ) : single;
          function GetPeopleCount(kind : TPeopleKind) : integer;
          //function GetSevicePeopleCount(index : integer; kind : TPeopleKind) : integer;
          function IntegrateValues(OldValue, NewValue, OldVWeight, NewVWeight : single) : single;
        protected
          function GetBuyProb(Service : TMetaServiceEvaluator; kind : TPeopleKind) : single;
          function NeedsSupply(var Service : integer) : boolean;
          function GetMinDemandRatio(var Service : integer) : TPercent;
          function GetCompetition(Service : integer) : TPercent;
        public
          procedure StoreToCache( Cache : TObjectCache ); override;
        private
          procedure PlanCompanyInputs(ratio : single);
          function  GetCompanySuppliesEffic : single;
          function  EnoughCompanySupplies   : boolean;
        // IAreaAgent
        private
          function getAgentArea : TRect;
        protected
          function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function _AddRef  : integer; stdcall;
          function _Release : integer; stdcall;
      end;
    {$M-}

  const
    tidInventionClass_SvrBlock = 'Commerce';
    tidInvAttr_SvrEfficiency   = 'Efficiency';
    tidInvAttr_SvrDesirability = 'Desirability';

  type
    TServiceBlockInvention =
      class( TWorkCenterInvention )
        public
          constructor Load( xmlObj : OleVariant ); override;
        private
          fEfficiency   : integer;
          fDesirability : integer;
        public
          property Efficiency   : integer read fEfficiency;
          property Desirability : integer read fDesirability;
        protected
          function GetClientProps( LangId : TLanguageId ) : string; override;
      end;

  procedure RegisterInventionClass;
  procedure LoadConfigData;

  function HitsToStrengh(hits : TFluidValue) : single;

  const
    tidCFGParm_CommerceBost = 'CommerceBoost';

  var
    CommerceBoost : single = -1;


implementation

  uses
    StdFluids, ClassStorage, PyramidalModifier, Population, BasicAccounts,
    SysUtils, MathUtils, SimHints, Logs, Math;

  const
    MaxServiceInput = 20;
    MaxServices     = 20;

  type
    TServiceInput     = array[0..MaxServiceInput] of TFluidValue;
    TDemandTable      = array[0..MaxServices] of TFluidValue;
    TPeolpleBuyTable  = array[TPeopleKind, 0..MaxServices] of TFluidValue;
    TServiceQualities = array[0..MaxServices] of single;
    TShareOutTable    = array[0..MaxServices, 0..MaxServiceInput] of TFluidValue;
    TSupRatioTable    = array[0..MaxServices] of single;
    TDelPriceTable    = array[0..MaxServices] of single;
    TServiceOpTable   = array[0..MaxServices] of boolean;

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

      fName_MLS := TMultiString.Create;
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

  procedure TMetaService.RetrieveTexts( Container : TDictionary );
    begin
      inherited;
      if fName_MLS = nil
        then fName_MLS := TMultiString.Create;
      fName_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Name'];
    end;

  procedure TMetaService.StoreTexts( Container : TDictionary );
    begin
      inherited;
      Container.Values[Family + '.' + Id + '.' + 'Name'] := fName;
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
      Cacheable := true;
      MaxUpgrade := 10;
      Height := 1;
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

  procedure TMetaServiceBlock.ModifyMetaFacility(MetaFacility : TMetaFacility);
    var
      sizefact : single;
      i        : integer;
      Service  : TMetaServiceEvaluator;
      Capacity : TFluidValue;
    begin
      inherited;
      if fMetaServices.Count > 0
        then
          begin
            sizefact := MetaFacility.xSize*MetaFacility.ySize/fMetaServices.Count;
            for i := 0 to pred(fMetaServices.Count) do
              begin
                Service := TMetaServiceEvaluator(fMetaServices[i]);
                Capacity := StrToFloat(TheGlobalConfigHandler.GetConfigParm( Service.MetaService.Id, '-1' ));
                if Capacity > 0
                  then Service.CorrectMaxService(sizefact*Capacity*Height);
              end;
          end;
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

  function TMetaServiceEvaluator.FormatOutput(perc : TPercent; ShowName : boolean; LangId : TLanguageId) : string;
    begin
      result := GetHintText(mtidCommerceMain.Values[LangId], [MetaService.Name_MLS.Values[LangId], perc]);
      //Name + ' sales: ' + IntToStr(perc) + '%';
    end;

  procedure TMetaServiceEvaluator.CorrectMaxService(aNewMax : TFluidValue);
    var
      i         : integer;
      MetaInput : TMetaInput;
      newFluid  : TFluidValue;
    begin
      if fMaxServices <> aNewMax
        then
          begin
            for i := 0 to pred(fInputs.Count) do
              begin
                MetaInput := TMetaServiceEvaluatorInput(fInputs[i]).fMetaInput;
                newFluid  := MetaInput.MaxFluid.Q*aNewMax/fMaxServices;
                MetaInput.MaxFluid.Q := newFluid;
              end;
            fMaxServices := aNewMax;
          end;
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
                      SellsAbstract := SellsAbstract + FormatOutput(fServiceData[i].SalePerc, SvrCount > 0, ToTycoon.Language);
                      if i < pred(SvrCount)
                        then SellsAbstract := SellsAbstract + LineBreak;
                    end;
                result := result + SellsAbstract;
              end;
        sttSecondary :
          if not Facility.CriticalTrouble // >> MLS2
            then
              begin
                SellsAbstract :=
                  GetHintText(
                    mtidServiceSecondary.Values[ToTycoon.Language],
                    [
                    fPotentialCustomers[pkHigh],
                    fPotentialCustomers[pkMiddle],
                    fPotentialCustomers[pkLow],
                    fActualCustomers[pkHigh],
                    fActualCustomers[pkMiddle],
                    fActualCustomers[pkLow]
                    ]);
                result := result + Format(mtidUpgradeLevel.Values[ToTycoon.Language], [UpgradeLevel]) + '  ' + SellsAbstract;
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
                          if not Facility.HasTechnology
                            then result := GetHintText(mtidEvalBlockNeedTechnology.Values[ToTycoon.Language], [Facility.MetaFacility.Technology.Name])
                            else
                              if Facility.Trouble and facNeedsWorkForce <> 0
                                then result := GetHintText(mtidBlockNeedsWorkForce.Values[ToTycoon.Language], [mtidPeopleKindName[WFRequired].Values[ToTycoon.Language]])
                                else
                                  if Facility.Trouble and facNeedCompSupport <> 0
                                    then result := GetHintText(mtidNeedsCompSupport.Values[ToTycoon.Language], [0])
                                    else
                                      if NeedsSupply(Service)
                                        then
                                          with TMetaServiceBlock(MetaBlock).Services[Service] do
                                            result := GetHintText(mtidServiceLowSupplies.Values[ToTycoon.Language], [MetaService.Name_MLS.Values[ToTycoon.Language]])
                                        else
                                          if not EnoughCompanySupplies
                                            then result := GetHintText(mtidEvalBlockNeedsMoreCompSupplies.Values[ToTycoon.Language], [0])
                                            else
                                              if GetMinDemandRatio(Service) < ModDemRatio
                                                then
                                                  if GetCompetition(Service) > ModCompRatio
                                                    then result := GetHintText(mtidServiceHighCompetition.Values[ToTycoon.Language], [0])
                                                    else result := GetHintText(mtidServiceWrongPlace.Values[ToTycoon.Language], [0])
                                                else result := GetHintText(mtidVisitWebSite.Values[ToTycoon.Language], [0]);
                        end
                      else result := GetHintText(mtidServiceOpening.Values[ToTycoon.Language], [0])
                  else GetHintText(mtidVisitWebSite.Values[ToTycoon.Language], [0]);
              end;
          end;
      end;
    end;

  function TServiceBlock.Evaluate : TEvaluationResult;

    procedure IntegrateInventions( out invEfficiency, invDesirability : integer );
      var
        Invention : TServiceBlockInvention;
        i         : integer;
      begin
        invEfficiency   := 0;
        invDesirability := 0;
        for i := 0 to pred(MetaBlock.Inventions.Count) do
          begin
            Invention := TServiceBlockInvention(MetaBlock.Inventions[i]);
            if Facility.Company.HasInvention[Invention.NumId]
              then
                begin
                  invEfficiency   := invEfficiency   + Invention.Efficiency;
                  invDesirability := invDesirability + Invention.Desirability;
                end;
          end;
      end;

    var
      SupRatios     : TSupRatioTable;
      DemandRatios  : TDemandTable;
      BuyTable      : TPeolpleBuyTable;
      Qualities     : TServiceQualities;
      ServiceOp     : TServiceOpTable;
      People        : array[TPeopleKind] of TFluidValue;
      Starving      : array[TPeopleKind] of TFluidValue;
      MaxStarving   : single;
      extPeople     : array[TPeopleKind] of TFluidValue;
      tmpStrength   : single;
      LayerActs     : array[TPeopleKind] of TFluidValue;
      LayerPots     : array[TPeopleKind] of TFluidValue;
      ActualDemand  : TFluidValue;
      Service       : TMetaServiceEvaluator;
      ServiceInput  : TMetaServiceEvaluatorInput;
      Input         : TInput;
      SvrIdx        : integer;
      InpIdx        : integer;
      SrvInpCnt     : integer;
      SrvCount      : integer;
      Fairness      : single;
      Strength      : single;
      Strengths     : array[TPeopleKind] of single;
      sum           : single;
      K             : single;
      Ks            : single;
      LayerDemand   : single;
      LayerPot      : single;
      Presence      : TSurfaceValue;
      IntValue      : TSurfaceValue;
      ModValue      : TSurfaceValue;
      SupRatio      : single;
      DemRatio      : single;
      svrCapacity   : single;
      frstPurchases : single;
      scndPurchases : single;
      sumPurchases  : single;
      InertiaRatio  : single;
      DelPrices     : TDelPriceTable;
      theDT         : TTimeDelta;
      WFEffic       : single;
      OpRatio       : single;
      TownHall      : TTownHall;
      TownParam     : TTownParameter;
      CompSup       : single;
      CapRatio      : single;
      AdStrgh       : single;
      PWeight       : single;
      KWeight       : single;
      invEfficiency   : integer;
      invDesirability : integer;
      SpendPwr      : single;
      kind          : TPeopleKind;
      UpgLevel      : single;
      CompSupEffic  : single;
      SrvAvgPrice   : single;
    begin
      result := inherited Evaluate;
      if CommerceBoost = -1 // <- temp
        then LoadConfigData;
      if not Facility.CriticalTrouble
        then
          begin
            IntegrateInventions( invEfficiency, invDesirability );
            //inc( invDesirability, 3*(UpgradeLevel - 1) + 1 + 3*max(0, 2 - Facility.Company.Owner.Level.Tier) );
            inc( invDesirability, UpgradeLevel + 3*max(0, 2 - Facility.Company.Owner.Level.Tier) );

            // Company support & Company Supplies Efficiency
            if Facility.HasTechnology
              then
                if Facility.CompanyDir <> nil
                  then
                    begin
                      CompSup := realmin(1, Facility.CompanyDir.Support);
                      CompSupEffic := sqrt(GetCompanySuppliesEffic);
                    end
                  else
                    begin
                      CompSup      := 1;
                      CompSupEffic := 1;
                    end
              else
                begin
                  CompSup      := 0;
                  CompSupEffic := 0;
                end;

            // No demand at the beggining
            //FillChar(ClassDemand, sizeof(ClassDemand), 0);
            // Clear the old values
            FillChar(fPotentialCustomers, sizeof(fPotentialCustomers), 0);
            FillChar(fActualCustomers, sizeof(fPotentialCustomers), 0);
            // Clear the SupRatios
            FillChar(SupRatios, sizeof(SupRatios), 0);
            // Check the efficiency
            WFEffic := WorkForceEfficiency;
            theDT := dt;
            UpgLevel := realmax(1, UpgradeLevel);

            // Get the town
            TownHall := TTownHall(TInhabitedTown(Facility.Town).TownHall.CurrBlock);

            // People
            for kind := low(kind) to high(kind) do
              People[kind] := realmin(TownHall.LastPop[kind].Q, fPeopleIntegrators[kind].Value);

            // Inertia
            InertiaRatio := fInertia/InertiaMax;

            // Ads
            AdStrgh := realmax(0, HitsToStrengh(Advertisement) + invDesirability);
            // For each service
            SrvCount := ServiceCount;
            for SvrIdx := 0 to pred(SrvCount) do
              begin
                Service   := TMetaServiceEvaluator(TMetaServiceBlock(MetaBlock).Services[SvrIdx]);
                SupRatio  := 1;
                K         := 0;
                Ks        := 0;
                SrvInpCnt := Service.fInputs.Count;
                ServiceOp[SvrIdx] := true;
                for InpIdx := 0 to pred(SrvInpCnt) do
                  begin
                    ServiceInput := TMetaServiceEvaluatorInput(Service.fInputs[InpIdx]);
                    Input := Inputs[ServiceInput.fMetaInput.Index];
                    // Signal the relation ship between "Service" and "Input"
                    if Input.FluidData.Q > 0
                      then
                        begin
                          K  := K + ServiceInput.fImportance*Input.FluidData.K;
                          Ks := Ks + ServiceInput.fImportance;
                          if Input.ActualMaxFluid.Q > 0
                            then SupRatio := realmin(SupRatio, Input.FluidData.Q/(theDt*Input.ActualMaxFluid.Q))
                            else SupRatio := 0;
                        end
                      else
                        begin
                          ServiceOp[SvrIdx] := ServiceOp[SvrIdx] and (Input.MetaInput.Level <> mglBasic);
                          SupRatio := 0;
                        end;
                  end;

                SupRatios[SvrIdx] := SupRatio;

                // Quality
                if Ks > 0
                  then K := realmax(1, K/Ks)
                  else K := 1; // >> ?
                Qualities[SvrIdx] := K;

                // Set Modifier value
                //Modifiers[SvrIdx].FixedArea := true;
                //Modifiers[SvrIdx].Value := (TMetaServiceBlock(MetaBlock).PresenceFac*modServiceBlockStrength + (fServiceData[SvrIdx].SupRatio/100)*AdStrgh)*UpgLevel;

                // Integrate modifier value to make a soft transitions and avoid jojos
                ModValue :=
                  IntegrateValues(
                    Modifiers[SvrIdx].Value,
                    TMetaServiceBlock(MetaBlock).PresenceFac*UpgLevel*realmax(realmax( modServiceBlockInertiaMax*(1 - InertiaRatio), modServiceBlockMinBlockStrength), 10*(fServiceData[SvrIdx].SupRatio/100)*AdStrgh),
                    0.8,
                    0.2);
                Modifiers[SvrIdx].Value := ModValue;

                // Presence
                IntValue := Integrators[SvrIdx].Value;
                ModValue := Modifiers[SvrIdx].Value;
                if IntValue > 0
                  then Presence := realmin(1, ModValue/IntValue)
                  else Presence := 0;

                // Starving people
                for kind := low(kind) to high(kind) do
                  begin
                    SpendPwr  := realmax(0, realmin(1, TownHall.SalaryRatio[kind]));
                    TownParam := fTownParams[SvrIdx].Count[kind];
                    Starving[kind] := sqrt(sqrt(SpendPwr))*Service.MetaService.Mobility[kind]*realmax(0, Service.MetaService.BuyProb[kind]*TownHall.LastPop[kind].Q - TownParam.Value);
                  end;

                // Price Town Parameter
                TownParam := fTownParams[SvrIdx].Price;

                ActualDemand := 0;
                DelPrices[SvrIdx] := 1;

                // sum of starving
                sum := 0;

                // Local customers
                for kind := low(kind) to high(kind) do
                  begin
                    SpendPwr  := TownHall.SalaryRatio[kind];
                    KWeight   := sqr(sqr(sqr(realmin(1, SpendPwr))));
                    PWeight   := realmax(0, 1 - KWeight);
                    SpendPwr  := realmin(1.5, CommerceBoost + SpendPwr);
                    Fairness  := GetFairness(2*fServiceData[SvrIdx].Price*DelPrices[SvrIdx], SpendPwr, TownParam.Average);
                    Strength  := KWeight*(K/100) + PWeight*Fairness;
                    Strengths[kind] := Strength;
                    Strength  := realmin(1, Strength);
                    LayerPot  := InertiaRatio*Presence*People[kind]*GetBuyProb(Service, kind);
                    LayerDemand := LayerPot*Strength;
                    ActualDemand :=  ActualDemand + LayerDemand;
                    BuyTable[kind, SvrIdx] := LayerDemand;
                    LayerActs[kind] := LayerDemand;
                    LayerPots[kind] := LayerPot;
                    sum := sum + Starving[kind];
                  end;

                // Estimate the max of starving clients to pick
                CapRatio := WFEffic*SupRatio;
                if CapRatio > CompSup
                  then svrCapacity := CompSupEffic*CompSup*Service.fMaxServices*UpgLevel
                  else svrCapacity := CompSupEffic*CapRatio*Service.fMaxServices*UpgLevel;

                //MaxStarving := 10000; //MaxStarving := realmin(sum, realmax(0, svrCapacity - ActualDemand));

                // Actual customer sum clear
                sum := 0;

                // Starving customers
                for kind := low(kind) to high(kind) do
                  begin
                    // calculate starvings
                    TownParam   := fTownParams[SvrIdx].Strength[kind];
                    LayerDemand := ActualDemand;
                    Strength    := Strengths[kind]*{(MaxStarving/10000)*}SupRatio; //SupRatio*realmax(0, 1 - LayerDemand/(Service.fMaxServices*UpgLevel));
                    if Starving[kind] > 0
                      then tmpStrength := Strength*GetBuyProb(Service, kind)*{(People[kind]/Starving[kind])*}(1 + KWeight*AdStrgh)
                      else tmpStrength := Strength*GetBuyProb(Service, kind)*(1 + KWeight*AdStrgh);
                    TownParam.CurrValue := TownParam.CurrValue + tmpStrength;
                    if TownParam.Value > 0
                      then extPeople[kind] := Starving[kind]*realmin(1, tmpStrength/TownParam.Value)
                      else extPeople[kind] := 0;

                    // Strengh
                    //Strength := realmin(1, Strengths[kind]);

                    // Potential Custumers
                    fPotentialCustomers[kind] := max(fPotentialCustomers[kind], SmartRound(24*(LayerPots[kind] + extPeople[kind])));

                    // Actual Customers not bounded by maxCapacity
                    LayerActs[kind] := LayerActs[kind] + extPeople[kind]; //Strength*extPeople[kind];
                    sum := sum + LayerActs[kind];

                    // Total Service demand not bounded
                    ActualDemand := LayerDemand + extPeople[kind]; // Strength*extPeople[kind];
                  end;

                DemandRatios[SvrIdx] := ActualDemand;

                // Adjust Actual customers
                ActualDemand := realmin(ActualDemand, svrCapacity);
                if sum > 0
                  then
                    for kind := low(kind) to high(kind) do
                      begin
                        LayerActs[kind] := ActualDemand*LayerActs[kind]/sum;
                        fActualCustomers[kind] :=  max(fActualCustomers[kind], SmartRound(24*LayerActs[kind]));
                      end;
              end;

            // Clear the ActualMaxFluids
            for InpIdx := 0 to pred(InputCount) do
              begin
                Input := Inputs[InpIdx];
                if (Input.MetaInput.Level = mglBasic) and (mfTradeable in Input.MetaInput.MetaFluid.Options)
                  then Input.ActualMaxFluid.Q := 0;
              end;

            // Sum of purchases
            sumPurchases := 0;

            // Calculate profit and plan next period
            OpRatio := 0;
            for SvrIdx := 0 to pred(SrvCount) do
              begin
                // Calculate supply ratio
                Service  := TMetaServiceEvaluator(TMetaServiceBlock(MetaBlock).Services[SvrIdx]);
                SupRatio := SupRatios[SvrIdx];

                // Dem Ratio
                DemRatio := DemandRatios[SvrIdx]/(Service.fMaxServices*UpgLevel);

                // Update hint data
                //fServiceData[SvrIdx].SupRatio := min(100, ceil(100*realmin(DemRatio, DemRatio*SupRatio)));
                fServiceData[SvrIdx].SupRatio := min(100, ceil(100*SupRatio));
                fServiceData[SvrIdx].DemRatio := min(high(fServiceData[0].DemRatio), ceil(100*DemRatio));//fServiceData[i].DemRatio := round(realmin(1, DemandRatios[i])*100);

                // Calculate capacity
                CapRatio := WFEffic*SupRatio;

                // Check if capacity is compatible with company support
                if CapRatio > CompSup
                  then svrCapacity := CompSupEffic*CompSup*Service.fMaxServices*UpgLevel
                  else svrCapacity := CompSupEffic*CapRatio*Service.fMaxServices*UpgLevel;

                // Local Clients
                sum := BuyTable[pkHigh, SvrIdx] + BuyTable[pkMiddle, SvrIdx] + BuyTable[pkLow, SvrIdx];

                frstPurchases := realmin(sum, svrCapacity);

                svrCapacity := svrCapacity - frstPurchases;

                scndPurchases := realmin(svrCapacity, DemandRatios[SvrIdx] - sum);

                sumPurchases := sumPurchases + frstPurchases + scndPurchases;

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
                TownParam.CurrValue := TownParam.CurrValue + Service.fMaxServices*UpgLevel;

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
                    {
                    if (DemRatio <= SupRatio)
                      then
                    }
                        if DemRatio = 0 // make it 1% of the demand at least
                          then DemRatio := 0.01;

                        Input.ActualMaxFluid.Q :=
                          Input.ActualMaxFluid.Q +
                          ServiceInput.fServShare*
                          Input.MetaInput.MaxFluid.Q*
                          DemRatio*
                          UpgLevel
                    {
                      else
                        Input.ActualMaxFluid.Q :=
                          Input.ActualMaxFluid.Q +
                          ServiceInput.fServShare*
                          Input.MetaInput.MaxFluid.Q*
                          realmin(DemRatio, SupRatio + 0.1)*
                          UpgLevel;
                     }
                  end;
                // Calculate sell percent
                fServiceData[SvrIdx].SalePerc := max(0, min(100, ceil(100*(frstPurchases + scndPurchases)/(Service.fMaxServices*UpgLevel))));
                // Add Operation Ratio
                OpRatio := OpRatio + realmin(1, (frstPurchases + scndPurchases)/(Service.fMaxServices*UpgLevel));
              end;

            // Actual demand
            {
            sum := ClassDemand[pkHigh] + ClassDemand[pkMiddle] + ClassDemand[pkLow];
            if sum > 0
              then
                begin
                  fActualCustomers[pkHigh]   := SmartRound(24*(sumPurchases*ClassDemand[pkHigh]/sum));
                  fActualCustomers[pkMiddle] := SmartRound(24*(sumPurchases*ClassDemand[pkMiddle]/sum));
                  fActualCustomers[pkLow]    := SmartRound(24*(sumPurchases*ClassDemand[pkLow]/sum));
                end;
            }

            if fInertia < InertiaMax
              then fInertia := min(InertiaMax, fInertia + round(InertiaInc*theDT));

            OpRatio := realmax(0, realmin(1, OpRatio/SrvCount));

            // Plan company inputs
            if Facility.CompanyDir <> nil
              then PlanCompanyInputs(realmax(0.5, OpRatio))
              else PlanCompanyInputs(0);

            HireWorkForce(1); //HireWorkForce(OpRatio);
            SetCargoValue( carLight, -10*OpRatio );
          end
        else
          begin
            // clear company inputs
            PlanCompanyInputs(0);
            // Service blocks in troubles will not affect competition in one area.
            for SvrIdx := 0 to pred(ServiceCount) do
              begin
                Modifiers[SvrIdx].Value       := 0;
                fServiceData[SvrIdx].SupRatio := 0;
                fServiceData[SvrIdx].DemRatio := 0;
              end;
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
                fTownParams[i].Count[kind]    := Facility.Town.Parameters[TownParameterIds[tidTownParameter_ServiceCount + PeopleKindPrefix[kind]]];
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
      // Create other modifiers
      for i := 0 to pred(ServiceCount) do
        begin
          // Create modifier
          fSurfaceModifiers[i] :=
            TPyramidalModifier.Create(
              TMetaServiceBlock(MetaBlock).Services[i].MetaService.Id,
              Point(xOrigin, yOrigin),
              DefaultServiceModifierValue,
              MB.PresenceFac*modServiceBlockStrength);
          fSurfaceModifiers[i].Area := Area;
        end;
      // Create people integrators
      for kind := low(kind) to high(kind) do
        fPeopleIntegrators[kind] :=
          TSurfaceIntegrator.Create(
            PeopleKindPrefix[kind] + tidEnvironment_People,
            self );
      // Create other surfaces
      for i := 0 to pred(ServiceCount) do
        begin
          // Create integrator
          fSurfaceIntegrators[i] :=
            TSurfaceIntegrator.Create(
              TMetaServiceBlock(MetaBlock).Services[i].MetaService.Id,
              Modifiers[i] );
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
      fInertia := Reader.ReadByte('Inertia', 0);
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
      if ObjectIs(self.ClassName, Block) and (Block.Facility.MetaFacility.FacId = Facility.MetaFacility.FacId)
        then
          begin
            Advertisement := TServiceBlock(Block).Advertisement;
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
      if CompanyInputCount > 0
        then AdvInput := CompanyInputs[0]
        else AdvInput := nil;
      if AdvInput <> nil
        then result := AdvInput.Q/dt
        else result := 0;
    end;

  procedure TServiceBlock.SetAdvertisement(value : single);
    var
      AdvInput : PCompanyInputData;
    begin
      if CompanyInputCount > 0
        then AdvInput := CompanyInputs[0]
        else AdvInput := nil;
      if AdvInput <> nil
        then AdvInput.Q := value;
    end;

  procedure TServiceBlock.RDOSetPrice(index, value : integer);
    begin
      Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Service SetPrice: ' + IntToStr(index) + ', ' + IntToStr(value) );
      try
        if (index >= 0) and (index < ServiceCount)
          then
            begin
              fServiceData[index].Price := min(high(fServiceData[index].Price), round(value/2));
              Facility.UpdateCache;
            end
          else Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Invalid index in SetPrice Facility: ' + IntToStr(xPos) + ', ' + IntToStr(yPos) );
      except
        Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Error in service SetPrice Facility: ' + IntToStr(xPos) + ', ' + IntToStr(yPos) );
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

  function TServiceBlock.GetFairness(DelPrice, SpendingPower, AvgPrice : single) : single;
    var
      FairPrice : single;
    begin
      FairPrice := SpendingPower*100;
      if FairPrice < AvgPrice
        then FairPrice := SPWeight*FairPrice + APWeight*AvgPrice;
      if DelPrice >= FairPrice
        then
          if DelPrice > 0
            then result := FairPrice/DelPrice
            else result := 1 // >> 2
        else
          if FairPrice > 0
            then result := 1 + (FairPrice - DelPrice)/FairPrice
            else result := 0;
      result := sqr(sqr(result));
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

  function TServiceBlock.IntegrateValues(OldValue, NewValue, OldVWeight, NewVWeight : single) : single;
    begin
      result := (OldVWeight*OldValue + NewVWeight*NewValue)/(OldVWeight + NewVWeight);
    end;

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
          //Cache.WriteString('srvNames' + IntToStr(i), Svr.Name);
          StoreMultiStringToCache( 'srvNames' + IntToStr(i) + '.', Svr.MetaService.Name_MLS, Cache );
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
          if fTownParams[i].Price <> nil
            then Cache.WriteInteger('srvAvgPrices' + IntToStr(i), round(fTownParams[i].Price.Average) )
            else Cache.WriteString('srvParmStatus' + IntToStr(i), 'MISSING!' );
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
                            
  procedure TServiceBlock.PlanCompanyInputs(ratio : single);
    var
      UpgLevel : single;
      i        : integer;
    begin
      UpgLevel := realmax(1, UpgradeLevel);
      // Clear company inputs, Ads not cleared
      for i := 1 to pred(CompanyInputCount) do
        with CompanyInputs[i]^ do
          Max := 0;
      // Plan company inputs, Ads not planned
      for i := 1 to pred(CompanyInputCount) do
        with CompanyInputs[i]^ do
          Max := Max + UpgLevel*ratio*TMetaCompanyInput(MetaBlock.CompanyInputs[i]).Max;
    end;

  function TServiceBlock.GetCompanySuppliesEffic : single;
    var
      CI  : PCompanyInputData;
      i   : integer;
      d1  : double;
      d2  : double;
      d3  : double;
    begin
      result := 1;
      // Ads not counted..
      for i := 1 to pred(CompanyInputCount) do
        begin
          CI := CompanyInputs[i];
          if CI <> nil
            then
              if CI.Max > 0
                then
                  begin
                    // make it a double operation to avoid foating point error
                    d1 := CI.Max;
                    d2 := CI.Q;
                    d3 := d2/d1;
                    result := realmin(1, d3);
                  end
                else result := 1;
        end;
    end;

  function TServiceBlock.EnoughCompanySupplies : boolean;
    begin
      result := GetCompanySuppliesEffic > 0.80;
    end;

  function TServiceBlock.getAgentArea : TRect;
    var
      i : integer;
      A : TRect;
    begin
      result := GetArea( 0, amdExcludeBlock );
      for i := 0 to pred(ServiceCount) do
        begin
          A := Modifiers[i].Area;
          result.Left   := min( result.Left, A.Left );
          result.Top    := min( result.Top, A.Top );
          result.Right  := max( result.Right, A.Right );
          result.Bottom := max( result.Bottom, A.Bottom );
        end;
    end;

  function TServiceBlock.QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TServiceBlock._AddRef  : integer; stdcall;
    begin
      result := 1;
    end;

  function TServiceBlock._Release : integer; stdcall;
    begin
      result := 1;
    end;


  // TServiceBlockInvention

  constructor TServiceBlockInvention.Load(xmlObj : OleVariant);
    var
      Aux   : OleVariant;
    begin
      inherited Load(xmlObj);
      Aux           := xmlObj.children.item(tidInvElement_Props, Unassigned);
      fEfficiency   := GetProperty(Aux, tidInvAttr_SvrEfficiency);
      fDesirability := GetProperty(Aux, tidInvAttr_SvrDesirability);
    end;

  function TServiceBlockInvention.GetClientProps( LangId : TLanguageId ) : string;
    begin
      result := inherited GetClientProps( LangId );
      if fEfficiency <> 0
        then result := result + SimHints.GetHintText(mtidInvEff.Values[LangId], [FormatDelta(fEfficiency)]) + LineBreak;
      if fDesirability <> 0
        then result := result + SimHints.GetHintText(mtidInvDesirability.Values[LangId], [FormatDelta(fDesirability)]) + LineBreak;
    end;


  // RegisterInventionClass

  procedure RegisterInventionClass;
    begin
      TheClassStorage.RegisterClass(
        tidClassFamily_InvClasses,
        tidInventionClass_SvrBlock,
        TInventionClass.Create(TServiceBlockInvention));
    end;

  procedure LoadConfigData;
    begin
      try
        CommerceBoost := StrToFloat( TheGlobalConfigHandler.GetConfigParm( tidCFGParm_CommerceBost, '1.5' ));
      except
        CommerceBoost := 1.5;
      end;
    end;

  function HitsToStrengh(hits : TFluidValue) : single;
    begin
      result := hits*htsToStrengh;
    end;

end.
