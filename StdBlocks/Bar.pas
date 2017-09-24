unit Bar;

interface

  uses
    Kernel, Surfaces, WorkCenterBlock, StdFluids, ServiceBlock;

  const
    tidService_Bar = 'Bar';

  const
    BarAveragePrice = 7;

  const
    BarPotencialClients : array[TPeopleKind] of integer = (90, 150, 250);

  type
    TMetaBarBlock =
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

    TBarBlock =
      class(TServiceBlock)
        private
          fElabFood : TInputData;
          fBooze    : TInputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, PyramidalModifier, Classes, BackupInterfaces,
    Population, StdAccounts;

  // TMetaBarBlock

  constructor TMetaBarBlock.Create(anId          : string;
                                   aCapacities   : array of TFluidValue;
                                   aCustomerMax  : TFluidValue;
                                   aPricePerc    : TPercent;
                                   EvlBuyProb    : array of TBuyProbability;
                                   aMaxAd        : TFluidValue;
                                   aBlockClass   : CBlock);
    var
      Sample        : TBarBlock;
      aElabFoodMax  : TFluidValue;
      aBoozeMax     : TFluidValue;
      //BarService    : TMetaService;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_Bar_Supplies,
        accIdx_Bar_Salaries,
        accIdx_Bar_Sales,
        aMaxAd,
        aBlockClass);
      Sample       := nil;
      aElabFoodMax := 0.3*aCustomerMax;
      aBoozeMax    := 0.7*aCustomerMax;
      // Inputs
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

      // Service: Bar
      with TMetaServiceEvaluator.Create(
        TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, tidService_Bar]),
        'Liquor',
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
              InputByName[tidGate_ElabFood],
              1,
              50));
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_Liquors],
              1,
              150));
          Register(self);
        end;
    end;


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TBarBlock);
    end;


end.

