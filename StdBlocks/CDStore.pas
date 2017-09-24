unit CDStore;

interface

  uses
    Kernel, Surfaces, WorkCenterBlock, StdFluids, ServiceBlock;

  const
    tidService_CDs = tidFluid_CDs;

  const
    CDStoreAveragePrice = 20;

  const
    CDPotencialClients  : array[TPeopleKind] of integer = (90, 150, 250);

  type
    TMetaCDStoreBlock =
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

    TCDStoreBlock =
      class(TServiceBlock)
        private
          fCDs : TInputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, PyramidalModifier, Classes, BackupInterfaces,
    Population, MathUtils, StdAccounts;

  // TMetaCDStoreBlock

  constructor TMetaCDStoreBlock.Create(anId         : string;
                                        aCapacities  : array of TFluidValue;
                                        aCustomerMax : TFluidValue;
                                        aPricePerc   : TPercent;
                                        EvlBuyProb   : array of TBuyProbability;
                                        aMaxAd       : TFluidValue;
                                        aBlockClass  : CBlock);
    var
      Sample     : TCDStoreBlock;
      CDService : TMetaService;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_CDStore_Supplies,
        accIdx_CDStore_Salaries,
        accIdx_CDStore_Sales,
        aMaxAd,
        aBlockClass);
      Sample := nil;

      // Services
      CDService := TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, tidService_CDs]);

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_CDs,
          inputZero,
          InputData(aCustomerMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_CDs]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fCDs),
          Sample.Offset(Sample.fCDs)));

      // Service: CDs
      with TMetaServiceEvaluator.Create(
        CDService,
        'CDs',
        aPricePerc,
        aCustomerMax,
        100,
        EvlBuyProb) do
        begin
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_CDs],
              1,
              100));
          Register(self);
        end;
    end;                                                           


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TCDStoreBlock);
    end;


end.

