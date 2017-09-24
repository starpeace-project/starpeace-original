unit Computing;

interface

  uses
    Kernel, Population, WorkCenterBlock, Surfaces, OutputEvaluators,
    BackupInterfaces, EvaluatedBlock, StdFluids, Accounts;

  type
    TMetaSoftwareBlock =
      class(TMetaEvaluatedBlock)
        public
          constructor Create(anId          : string;
                             aCapacities   : array of TFluidValue;
                             aLegalServMax : TFluidValue;
                             aCompMax      : TFluidValue;
                             aMaxBudget    : TMoney;
                             aBlockClass   : CBlock);
      end;

    TSoftwareBlock =
      class(TEvaluatedBlock)
        private
          fCompServ  : TOutputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, MathUtils, StdAccounts;


  // TMetaSoftwareBlock

  constructor TMetaSoftwareBlock.Create(anId            : string;
                                        aCapacities     : array of TFluidValue;
                                        aLegalServMax   : TFluidValue;
                                        aCompMax        : TFluidValue;
                                        aMaxBudget      : TMoney;
                                        aBlockClass     : CBlock);
    var
      Sample : TSoftwareBlock;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_Computing_Supplies,
        accIdx_Computing_Products,
        accIdx_Computing_Salaries,
        accIdx_Computing_Maintenance,
        aBlockClass);
      Sample := nil;

      // Company Inputs
      if aLegalServMax > 0
        then RegisterCompanyInput(tidFluid_LegalServ, aLegalServMax, false);

      // Outputs
      MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_CompServ,
          FluidData(aCompMax, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_CompServ]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fCompServ),
          Sample.Offset(Sample.fCompServ)));

      // MetaEvaluators
      with TMetaOutputEvaluator.Create(
        aMaxBudget,
        0,
        OutputByName[tidGate_CompServ],
        TOutputEvaluator) do
        begin
          FullOpenTime  := 30;
          FullCloseTime := 31;
          // Company Inputs
          if aCompMax > 0
            then RegisterCompanyInput(0, 0.05, 0.05, aLegalServMax);
          Register(MetaEvaluatorPool);
        end;
    end;


  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TSoftwareBlock);
    end;

end.

