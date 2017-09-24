unit GasStation;

interface

  uses
    Kernel, Surfaces, WorkCenterBlock, StdFluids, ServiceBlock;

  const
    tidService_Gasoline = tidFluid_Gasoline;

  const
    GasStationAveragePrice = 2;

  const
    GasPotencialClients  : array[TPeopleKind] of integer = (90, 150, 250);

  type
    TMetaGasStationBlock =
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

    TGasStationBlock =
      class(TServiceBlock)
        private
          fGas : TInputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, PyramidalModifier, Classes, BackupInterfaces,
    Population, MathUtils, StdAccounts;

  // TMetaGasStationBlock

  constructor TMetaGasStationBlock.Create(anId         : string;
                                          aCapacities  : array of TFluidValue;
                                          aCustomerMax : TFluidValue;
                                          aPricePerc   : TPercent;
                                          EvlBuyProb   : array of TBuyProbability;
                                          aMaxAd       : TFluidValue;
                                          aBlockClass  : CBlock);
    var
      Sample     : TGasStationBlock;
      GasService : TMetaService;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_GasStation_Supplies,
        accIdx_GasStation_Salaries,
        accIdx_GasStation_Sales,
        aMaxAd,
        aBlockClass);
      Sample := nil;

      // Services
      GasService := TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, tidService_Gasoline]);

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Gasoline,
          inputZero,
          InputData(aCustomerMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Gasoline]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fGas),
          Sample.Offset(Sample.fGas)));

      // Service: Gas
      with TMetaServiceEvaluator.Create(
        GasService,
        'Gas',
        aPricePerc,
        aCustomerMax,
        100,
        EvlBuyProb) do
        begin
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_Gasoline],
              1,
              100));
          Register(self);
        end;
    end;


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TGasStationBlock);
    end;


end.

