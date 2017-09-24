unit FoodStore;

interface

  uses
    Kernel, Surfaces, WorkCenterBlock, StdFluids, ServiceBlock;

  const
    tidService_FreshFood = tidFluid_FreshFood;
    tidService_ElabFood  = tidFluid_ElabFood;

  type
    TMetaFoodStoreBlock =
      class(TMetaServiceBlock)
        public
          constructor Create(anId          : string;
                             aCapacities   : array of TFluidValue;
                             aFreshFoodMax : TFluidValue;
                             aElabFoodMax  : TFluidValue;
                             aPricePerc    : TPercent;
                             EvlBuyProb    : array of TBuyProbability;
                             aMaxAd        : TFluidValue;
                             aBlockClass   : CBlock);
      end;

    TFoodStoreBlock =
      class(TServiceBlock)
        private
          fFreshFood : TInputData;
          fElabFood  : TInputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, PyramidalModifier, Classes, BackupInterfaces,
    Population, MathUtils, StdAccounts;

  // TMetaFoodStoreBlock

  constructor TMetaFoodStoreBlock.Create(anId          : string;
                                         aCapacities   : array of TFluidValue;
                                         aFreshFoodMax : TFluidValue;
                                         aElabFoodMax  : TFluidValue;
                                         aPricePerc    : TPercent;
                                         EvlBuyProb    : array of TBuyProbability;
                                         aMaxAd        : TFluidValue;
                                         aBlockClass   : CBlock);
    var
      Sample           : TFoodStoreBlock;
      FreshFoodService : TMetaService;
      ElabFoodService  : TMetaService;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_FoodStore_Supplies,
        accIdx_FoodStore_Salaries,
        accIdx_FoodStore_Sales,
        aMaxAd,
        aBlockClass);
      Sample := nil;
      // Services
      FreshFoodService := TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, tidService_FreshFood]);
      ElabFoodService  := TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, tidService_ElabFood]);
      // Inputs
      if aFreshFoodMax > 0
        then
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

      if aElabFoodMax > 0
        then
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

      // Service: Fresh Food
      if aFreshFoodMax > 0
        then
          with TMetaServiceEvaluator.Create(
            FreshFoodService,
            'Fresh Food',
            aPricePerc,
            aFreshFoodMax,
            50,
            EvlBuyProb) do
            begin
              {
              RegisterInput(
                TMetaServiceEvaluatorInput.Create(
                  InputByName[PeopleKindPrefix[pkHigh] + tidGate_WorkForce],
                  0.5,
                  10));
              RegisterInput(
                TMetaServiceEvaluatorInput.Create(
                  InputByName[PeopleKindPrefix[pkMiddle] + tidGate_WorkForce],
                  0.5,
                  15));
              RegisterInput(
                TMetaServiceEvaluatorInput.Create(
                  InputByName[PeopleKindPrefix[pkLow] + tidGate_WorkForce],
                  0.5,
                  17));
              }
              RegisterInput(
                TMetaServiceEvaluatorInput.Create(
                  InputByName[tidGate_FreshFood],
                  1,
                  100));
              Register(self);
            end;

      // Service: Processed Food
      if aElabFoodMax > 0
        then
          with TMetaServiceEvaluator.Create(
            ElabFoodService,
            'Processed Food',
            aPricePerc,
            aElabFoodMax,
            50,
            EvlBuyProb) do
            begin
              {
              RegisterInput(
                TMetaServiceEvaluatorInput.Create(
                  InputByName[PeopleKindPrefix[pkHigh] + tidGate_WorkForce],
                  0.5,
                  10));
              RegisterInput(
                TMetaServiceEvaluatorInput.Create(
                  InputByName[PeopleKindPrefix[pkMiddle] + tidGate_WorkForce],
                  0.5,
                  15));
              RegisterInput(
                TMetaServiceEvaluatorInput.Create(
                  InputByName[PeopleKindPrefix[pkLow] + tidGate_WorkForce],
                  0.5,
                  17));
              }
              RegisterInput(
                TMetaServiceEvaluatorInput.Create(
                  InputByName[tidGate_ElabFood],
                  1,
                  100));
              Register(self);
            end;
    end;


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TFoodStoreBlock);
    end;


end.

