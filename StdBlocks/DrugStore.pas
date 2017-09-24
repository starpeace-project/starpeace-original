unit DrugStore;

interface

  uses
    Kernel, Surfaces, WorkCenterBlock, StdFluids, ServiceBlock;

  const
    tidService_DrugStore = tidFluid_Drugs;

  const
    DrugStoreAveragePrice = 20;

  const
    DrugPotencialClients  : array[TPeopleKind] of integer = (90, 150, 250);

  type
    TMetaDrugStoreBlock =
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

    TDrugStoreBlock =
      class(TServiceBlock)
        private
          fDrug : TInputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, PyramidalModifier, Classes, BackupInterfaces,
    Population, MathUtils, StdAccounts;

  // TMetaDrugStoreBlock

  constructor TMetaDrugStoreBlock.Create(anId         : string;
                                        aCapacities  : array of TFluidValue;
                                        aCustomerMax : TFluidValue;
                                        aPricePerc   : TPercent;
                                        EvlBuyProb   : array of TBuyProbability;
                                        aMaxAd       : TFluidValue;
                                        aBlockClass  : CBlock);
    var
      Sample      : TDrugStoreBlock;
      DrugService : TMetaService;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_DrugStore_Supplies,
        accIdx_DrugStore_Salaries,
        accIdx_DrugStore_Sales,
        aMaxAd,
        aBlockClass);
      Sample := nil;

      // Services
      DrugService := TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, tidService_DrugStore]);

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Drugs,
          inputZero,
          InputData(aCustomerMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Drugs]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fDrug),
          Sample.Offset(Sample.fDrug)));

      with TMetaServiceEvaluator.Create(
        DrugService,
        'Pharmaceutics',
        aPricePerc,
        aCustomerMax,
        100,
        EvlBuyProb) do
        begin
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_Drugs],
              1,
              100));
          Register(self);
        end;
    end;


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TDrugStoreBlock);
    end;


end.

