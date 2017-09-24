unit FurnitureStore;

interface

  uses
    Kernel, Surfaces, WorkCenterBlock, StdFluids, ServiceBlock;

  const
    tidService_Furniture = tidFluid_Furniture;

  const
    FurnitureStoreAveragePrice = 20;

  const
    FurniturePotencialClients  : array[TPeopleKind] of integer = (90, 150, 250);

  type
    TMetaFurnitureStoreBlock =
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

    TFurnitureStoreBlock =
      class(TServiceBlock)
        private
          fFurniture : TInputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, PyramidalModifier, Classes, BackupInterfaces,
    Population, MathUtils, StdAccounts;

  // TMetaFurnitureStoreBlock

  constructor TMetaFurnitureStoreBlock.Create(anId         : string;
                                              aCapacities  : array of TFluidValue;
                                              aCustomerMax : TFluidValue;
                                              aPricePerc   : TPercent;
                                              EvlBuyProb   : array of TBuyProbability;
                                              aMaxAd       : TFluidValue;
                                              aBlockClass  : CBlock);
    var
      Sample     : TFurnitureStoreBlock;
      FurnitureService : TMetaService;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_FurnitureStore_Supplies,
        accIdx_FurnitureStore_Salaries,
        accIdx_FurnitureStore_Sales,
        aMaxAd,
        aBlockClass);
      Sample := nil;

      // Services
      FurnitureService := TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, tidService_Furniture]);

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Furniture,
          inputZero,
          InputData(aCustomerMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Furniture]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fFurniture),
          Sample.Offset(Sample.fFurniture)));

      // Service: Furniture
      with TMetaServiceEvaluator.Create(
        FurnitureService,
        'Furniture',
        aPricePerc,
        aCustomerMax,
        100,
        EvlBuyProb) do
        begin
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_Furniture],
              1,
              100));
          Register(self);
        end;
    end;


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TFurnitureStoreBlock);
    end;


end.

