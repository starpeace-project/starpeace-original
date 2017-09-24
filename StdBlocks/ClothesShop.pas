unit ClothesShop;

interface

  uses
    Kernel, Surfaces, WorkCenterBlock, StdFluids, ServiceBlock;

  const
    tidService_Clothes = tidFluid_Clothes + '_Market';

  type
    TMetaClothesShopBlock =
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

    TClothesShopBlock =
      class(TServiceBlock)
        private
          fClothes : TInputData;
      end;

  procedure RegisterBackup;

implementation

  uses
    ClassStorage, PyramidalModifier, Classes, BackupInterfaces,
    Population, MathUtils, StdAccounts;

  // TMetaClothesShopBlock

  constructor TMetaClothesShopBlock.Create(anId          : string;
                                           aCapacities   : array of TFluidValue;
                                           aCustomerMax  : TFluidValue;
                                           aPricePerc    : TPercent;
                                           EvlBuyProb    : array of TBuyProbability;
                                           aMaxAd        : TFluidValue;
                                           aBlockClass   : CBlock);
    var
      Sample         : TClothesShopBlock;
      ClothesService : TMetaService;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_ClothesStore_Supplies,
        accIdx_ClothesStore_Salaries,
        accIdx_ClothesStore_Sales,
        aMaxAd,
        aBlockClass);
      Sample  := nil;
      // Clothes Service
      ClothesService := TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, tidService_Clothes]);
      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Clothes,
          inputZero,
          InputData(aCustomerMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Clothes]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fClothes),
          Sample.Offset(Sample.fClothes)));

      // Service: Clothes
      with TMetaServiceEvaluator.Create(
        ClothesService,
        'Clothes',
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
              12));
          }
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_Clothes],
              1,
              100));
          Register(self);
        end;
    end;


  // Backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TClothesShopBlock);
    end;


end.

