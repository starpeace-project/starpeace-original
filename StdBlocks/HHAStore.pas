unit HHAStore;

interface

  uses
    Kernel, Surfaces, WorkCenterBlock, StdFluids, ServiceBlock;

  const
    tidService_HouseHoldingAppliances = tidFluid_HouseHoldingAppliances;

  const
    HHAPotencialClients  : array[TPeopleKind] of integer = (90, 150, 250);

  type
    TMetaHHAStoreBlock =
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

    THHAStoreBlock =
      class(TServiceBlock)
        private
          fHHA : TInputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, PyramidalModifier, Classes, BackupInterfaces,
    Population, MathUtils, StdAccounts;

  // TMetaHHAStoreBlock

  constructor TMetaHHAStoreBlock.Create(anId         : string;
                                        aCapacities  : array of TFluidValue;
                                        aCustomerMax : TFluidValue;
                                        aPricePerc   : TPercent;
                                        EvlBuyProb   : array of TBuyProbability;
                                        aMaxAd       : TFluidValue;
                                        aBlockClass  : CBlock);
    var
      Sample     : THHAStoreBlock;
      HHAService : TMetaService;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_HHAStore_Supplies,
        accIdx_HHAStore_Salaries,
        accIdx_HHAStore_Sales,
        aMaxAd,
        aBlockClass);
      Sample := nil;

      // Services
      HHAService := TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, tidService_HouseHoldingAppliances]);

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_HouseHoldingAppliances,
          inputZero,
          InputData(aCustomerMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_HouseHoldingAppliances]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fHHA),
          Sample.Offset(Sample.fHHA)));

      // Service: House Holding Appliances
      with TMetaServiceEvaluator.Create(
        HHAService,
        'Household Appliances',
        aPricePerc,
        aCustomerMax,
        100,
        EvlBuyProb) do
        begin
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_HouseHoldingAppliances],
              1,
              100));
          Register(self);
        end;
    end;


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(THHAStoreBlock);
    end;


end.

