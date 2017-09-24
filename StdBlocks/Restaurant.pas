unit Restaurant;

interface

  uses
    Kernel, Surfaces, WorkCenterBlock, StdFluids, ServiceBlock;

  const
    tidService_Restaurant = 'Restaurant';

  const
    RestaurantAveragePrice = 20;

  const
    RestaurantPotencialClients : array[TPeopleKind] of integer = (500, 1000, 3000);

  type
    TMetaRestaurantBlock =
      class(TMetaServiceBlock)
        public
          constructor Create(anId          : string;
                             aCapacities   : array of TFluidValue;
                             aCustomerMax  : TFluidValue;
                             aPricePerc    : TPercent;
                             EvlBuyProb    : array of TBuyProbability;
                             aMaxAd        : TFluidValue;
                             aBlockClass   : CBlock);
      end;

    TRestaurantBlock =
      class(TServiceBlock)
        private
          fFreshFood : TInputData;
          fElabFood  : TInputData;
          fBooze     : TInputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, PyramidalModifier, Classes, BackupInterfaces,
    Population, StdAccounts;

  // TMetaRestaurantBlock

  constructor TMetaRestaurantBlock.Create(anId         : string;
                                          aCapacities  : array of TFluidValue;
                                          aCustomerMax : TFluidValue;
                                          aPricePerc   : TPercent;
                                          EvlBuyProb   : array of TBuyProbability;
                                          aMaxAd       : TFluidValue;
                                          aBlockClass  : CBlock);
    var
      Sample        : TRestaurantBlock;
      Service       : TMetaService;
      aFreshFoodMax : TFluidValue;
      aElabFoodMax  : TFluidValue;
      aBoozeMax     : TFluidValue;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_Restaurant_Supplies,
        accIdx_Restaurant_Salaries,
        accIdx_Restaurant_Sales,
        aMaxAd,
        aBlockClass);
      Sample := nil;
      // Services
      Service := TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, tidService_Restaurant]);
      // Food Input Limits
      aFreshFoodMax := 0.5 * aCustomerMax;
      aElabFoodMax  := 0.3 * aCustomerMax;
      aBoozeMax     := 0.2 * aCustomerMax;
      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_FreshFood,
          inputZero,
          InputData(aFreshFoodMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_FreshFood]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fFreshFood),
          Sample.Offset(Sample.fFreshFood)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_ElabFood,
          inputZero,
          InputData(aElabFoodMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_ElabFood]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fElabFood),
          Sample.Offset(Sample.fElabFood)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Liquors,
          inputZero,
          InputData(aBoozeMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Liquors]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fBooze),
          Sample.Offset(Sample.fBooze)));

      // Service: Restaurant
      with TMetaServiceEvaluator.Create(
        Service,
        'Restaurant Service',
        aPricePerc,
        aCustomerMax,
        100,
        EvlBuyProb) do
        begin
          {
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[PeopleKindPrefix[pkHigh] + tidGate_WorkForce],
              1,
              10));
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[PeopleKindPrefix[pkMiddle] + tidGate_WorkForce],
              1,
              15));
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[PeopleKindPrefix[pkLow] + tidGate_WorkForce],
              1,
              17));
          }
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_FreshFood],
              1,
              150));
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_ElabFood],
              1,
              75));
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_Liquors],
              1,
              50));
          Register(self);
        end;
    end;


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TRestaurantBlock);
    end;


end.

