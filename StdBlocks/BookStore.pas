unit BookStore;

interface

  uses
    Kernel, Surfaces, WorkCenterBlock, StdFluids, ServiceBlock;

  const
    tidService_Books = tidFluid_Books;

  const
    BookStoreAveragePrice = 20;

  const
    BookPotencialClients  : array[TPeopleKind] of integer = (90, 150, 250);

  type
    TMetaBookStoreBlock =
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

    TBookStoreBlock =
      class(TServiceBlock)
        private
          fBooks : TInputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, PyramidalModifier, Classes, BackupInterfaces,
    Population, MathUtils, StdAccounts;

  // TMetaBookStoreBlock

  constructor TMetaBookStoreBlock.Create(anId         : string;
                                        aCapacities  : array of TFluidValue;
                                        aCustomerMax : TFluidValue;
                                        aPricePerc   : TPercent;
                                        EvlBuyProb   : array of TBuyProbability;
                                        aMaxAd       : TFluidValue;
                                        aBlockClass  : CBlock);
    var
      Sample     : TBookStoreBlock;
      BookService : TMetaService;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_BookStore_Supplies,
        accIdx_BookStore_Salaries,
        accIdx_BookStore_Sales,
        aMaxAd,
        aBlockClass);
      Sample := nil;

      // Services
      BookService := TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, tidService_Books]);

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Books,
          inputZero,
          InputData(aCustomerMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Books]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fBooks),
          Sample.Offset(Sample.fBooks)));

      // Service: Books
      with TMetaServiceEvaluator.Create(
        BookService,
        'Books',
        aPricePerc,
        aCustomerMax,
        100,
        EvlBuyProb) do
        begin
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_Books],
              1,
              100));
          Register(self);
        end;
    end;                                                           


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TBookStoreBlock);
    end;


end.

