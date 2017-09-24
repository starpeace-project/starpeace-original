unit ToyStore;

interface

  uses
    Kernel, Surfaces, WorkCenterBlock, StdFluids, ServiceBlock;

  const
    tidService_Toys = tidFluid_Toys;

  const
    ToyStoreAveragePrice = 20;

  const
    ToyPotencialClients  : array[TPeopleKind] of integer = (90, 150, 250);

  type
    TMetaToyStoreBlock =
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

    TToyStoreBlock =
      class(TServiceBlock)
        private
          fToy : TInputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, PyramidalModifier, Classes, BackupInterfaces,
    Population, MathUtils, StdAccounts;

  // TMetaToyStoreBlock

  constructor TMetaToyStoreBlock.Create(anId         : string;
                                        aCapacities  : array of TFluidValue;
                                        aCustomerMax : TFluidValue;
                                        aPricePerc   : TPercent;
                                        EvlBuyProb   : array of TBuyProbability;
                                        aMaxAd       : TFluidValue;
                                        aBlockClass  : CBlock);
    var
      Sample     : TToyStoreBlock;
      ToyService : TMetaService;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_ToyStore_Supplies,
        accIdx_ToyStore_Salaries,
        accIdx_ToyStore_Sales,
        aMaxAd,
        aBlockClass);
      Sample := nil;

      // Services
      ToyService := TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, tidService_Toys]);

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Toys,
          inputZero,
          InputData(aCustomerMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Toys]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fToy),
          Sample.Offset(Sample.fToy)));

      // Service: Toys
      with TMetaServiceEvaluator.Create(
        ToyService,
        'Toys',
        aPricePerc,
        aCustomerMax,
        100,
        EvlBuyProb) do
        begin
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_Toys],
              1,
              100));
          Register(self);
        end;
    end;                                                           


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TToyStoreBlock);
    end;


end.

