unit FuneralParlors;

interface

  uses
    Kernel, Surfaces, WorkCenterBlock, StdFluids, ServiceBlock;

  const
    tidService_Funeral = 'Funeral';

  const
    FuneralAveragePrice = 20000;

  const
    FuneralPotencialClients : array[TPeopleKind] of integer = (90, 150, 250);

  type
    TMetaFuneralBlock =
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

    TFuneralBlock =
      class(TServiceBlock)
        private
          fElabFood : TInputData;
          fFabrics  : TInputData;
          fWood     : TInputData;
          fChems    : TInputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, PyramidalModifier, Classes, BackupInterfaces,
    Population, StdAccounts;

  // TMetaFuneralBlock

  constructor TMetaFuneralBlock.Create(anId          : string;
                                   aCapacities   : array of TFluidValue;
                                   aCustomerMax  : TFluidValue;
                                   aPricePerc    : TPercent;
                                   EvlBuyProb    : array of TBuyProbability;
                                   aMaxAd        : TFluidValue;
                                   aBlockClass   : CBlock);
    var
      Sample       : TFuneralBlock;
      aElabFoodMax : TFluidValue;
      aFabricsMax  : TFluidValue;
      aWoodMax     : TFluidValue;
      aChemsMax    : TFluidValue;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_FuneralParlor_Supplies,
        accIdx_FuneralParlor_Salaries,
        accIdx_FuneralParlor_Sales,
        aMaxAd,
        aBlockClass);

      Sample       := nil;
      aElabFoodMax := 100*aCustomerMax;
      aFabricsMax  := 20*aCustomerMax;
      aWoodMax     := 40*aCustomerMax;
      aChemsMax    := 1*aCustomerMax;

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
          tidGate_FabThreads,
          inputZero,
          InputData(aFabricsMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_FabThreads]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fFabrics),
          Sample.Offset(Sample.fFabrics)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Timber,
          inputZero,
          InputData(aWoodMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Timber]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fWood),
          Sample.Offset(Sample.fWood)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Chemicals,
          inputZero,
          InputData(aChemsMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Chemicals]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fChems),
          Sample.Offset(Sample.fChems)));

      // Service: Funeral
      with TMetaServiceEvaluator.Create(
        TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, tidService_Funeral]),
        'Funerals',
        aPricePerc,
        aCustomerMax,
        100,
        EvlBuyProb) do
        begin
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_ElabFood],
              1,
              50));
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_FabThreads],
              1,
              150));
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_Timber],
              1,
              150));
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_Chemicals],
              1,
              150));
          Register(self);
        end;
    end;


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TFuneralBlock);
    end;


end.

