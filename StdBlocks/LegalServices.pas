unit LegalServices;

interface                   

  uses
    Kernel, Population, WorkCenterBlock, Surfaces, OutputEvaluators,
    BackupInterfaces, EvaluatedBlock, StdFluids, Accounts;

  type
    TMetaLegalServiceBlock =
      class(TMetaEvaluatedBlock)
        public
          constructor Create(anId          : string;
                             aCapacities   : array of TFluidValue;
                             aCompServMax  : TFluidValue;
                             aLegalServMax : TFluidValue;
                             aMaxBudget    : TMoney;
                             aBlockClass   : CBlock);
      end;

    TLegalServiceBlock =
      class(TEvaluatedBlock)
        private
          fLegalServ : TOutputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, MathUtils, StdAccounts;


  // TMetaLegalServiceBlock

  constructor TMetaLegalServiceBlock.Create(anId          : string;
                                            aCapacities   : array of TFluidValue;
                                            aCompServMax  : TFluidValue;
                                            aLegalServMax : TFluidValue;
                                            aMaxBudget    : TMoney;
                                            aBlockClass   : CBlock);
    var
      Sample : TLegalServiceBlock;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_LegalServices_Supplies,
        accIdx_LegalServices_Products,
        accIdx_LegalServices_Salaries,
        accIdx_LegalServices_Maintenance,
        aBlockClass);

      Sample := nil;

      // Company Inputs
      if aCompServMax > 0
        then RegisterCompanyInput(tidFluid_CompServ, aCompServMax, false);

      // Outputs
      MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_LegalServ,
          FluidData(aLegalServMax, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_LegalServ]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fLegalServ),
          Sample.Offset(Sample.fLegalServ)));

      // MetaEvaluators
      with TMetaOutputEvaluator.Create(
        aMaxBudget,
        0,
        OutputByName[tidGate_LegalServ],
        TOutputEvaluator) do
        begin
          FullOpenTime  := 20;
          FullCloseTime := 22;
          // Company Inputs
          if aCompServMax > 0
            then RegisterCompanyInput(0, 0.15, 0.10, aCompServMax);
          Register(MetaEvaluatorPool);
        end;
    end;


  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TLegalServiceBlock);
    end;

end.

