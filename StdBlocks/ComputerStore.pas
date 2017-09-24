unit ComputerStore;

interface

  uses
    Kernel, Surfaces, WorkCenterBlock, StdFluids, ServiceBlock;

  const
    tidService_Computers = 'Computers';
    tidFluid_Computers   = tidFluid_BusinessMachines;
    tidGate_Computers    = tidGate_BusinessMachines;

  const
    ComputerStoreAveragePrice = 20;

  const
    ComputerPotencialClients  : array[TPeopleKind] of integer = (90, 150, 250);

  type
    TMetaComputerStoreBlock =
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

    TComputerStoreBlock =
      class(TServiceBlock)
        private
          fComputers : TInputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, PyramidalModifier, Classes, BackupInterfaces,
    Population, MathUtils, StdAccounts;

  // TMetaComputerStoreBlock

  constructor TMetaComputerStoreBlock.Create(anId         : string;
                                        aCapacities  : array of TFluidValue;
                                        aCustomerMax : TFluidValue;
                                        aPricePerc   : TPercent;
                                        EvlBuyProb   : array of TBuyProbability;
                                        aMaxAd       : TFluidValue;
                                        aBlockClass  : CBlock);
    var
      Sample     : TComputerStoreBlock;
      ComputerService : TMetaService;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_ComputerStore_Supplies,
        accIdx_ComputerStore_Salaries,
        accIdx_ComputerStore_Sales,
        aMaxAd,
        aBlockClass);
      Sample := nil;

      // Services
      ComputerService := TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, tidService_Computers]);

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Computers,
          inputZero,
          InputData(aCustomerMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Computers]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fComputers),
          Sample.Offset(Sample.fComputers)));

      // Service: Computers
      with TMetaServiceEvaluator.Create(
        ComputerService,
        'Computers',
        aPricePerc,
        aCustomerMax,
        100,
        EvlBuyProb) do
        begin
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_Computers],
              1,
              100));
          Register(self);
        end;
    end;


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TComputerStoreBlock);
    end;


end.

