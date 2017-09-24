unit StoneMine;

interface

  uses
    Kernel, Population, WorkCenterBlock, Surfaces, OutputEvaluators,
    BackupInterfaces, PolluterWorkCenter, StdFluids, Accounts;

  const
    MineOpenTime  = 1*24;
    MineCloseTime = 1*24;

  type
    TMetaStoneMineBlock =
      class(TMetaPolluterWorkCenterBlock)
        public
          constructor Create(anId            : string;
                             aCapacities     : array of TFluidValue;
                             aLegalServMax   : TFluidValue;
                             aCompMax        : TFluidValue;
                             aChemicalMax    : TFluidValue;
                             aOreStoneMax         : TFluidValue;
                             aMaxBudget      : TMoney;
                             aBlockClass     : CBlock);
      end;

    TStoneMineBlock =
      class(TPolluterWorkCenterBlock)
        private
          fChemicals : TInputData;
          fOreStone       : TOutputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, MathUtils, StdAccounts;

  // TMetaStoneMineBlock

  constructor TMetaStoneMineBlock.Create(anId            : string;
                                    aCapacities     : array of TFluidValue;
                                    aLegalServMax   : TFluidValue;
                                    aCompMax        : TFluidValue;
                                    aChemicalMax    : TFluidValue;
                                    aOreStoneMax         : TFluidValue;
                                    aMaxBudget      : TMoney;
                                    aBlockClass     : CBlock);
    var
      Sample    : TStoneMineBlock;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_Mine_Supplies,
        accIdx_Mine_Products,
        accIdx_Mine_Salaries,
        accIdx_Mine_Maintenance,
        aBlockClass);

      Sample := nil;

      DesignProfPerc := 20; // override the minimun profit of 45%

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Chemicals,
          inputZero,
          InputData(aChemicalMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidGate_Chemicals]),
          5,
          mglAditional,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fChemicals),
          Sample.Offset(Sample.fChemicals)));

      // Company Inputs
      if aCompMax > 0
        then RegisterCompanyInput(tidFluid_CompServ, aCompMax, false);
      if aLegalServMax > 0
        then RegisterCompanyInput(tidFluid_LegalServ, aLegalServMax, false);

      // Outputs
      MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_OreStone,
          FluidData(aOreStoneMax, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_OreStone]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fOreStone),
          Sample.Offset(Sample.fOreStone)));

      // MetaEvaluators
      with TMetaOutputEvaluator.Create(
        aMaxBudget,
        intcond(aMaxBudget > 0, 20, 0),
        OutputByName[tidGate_OreStone],
        TOutputEvaluator) do
        begin
          FullOpenTime  := 30;
          FullCloseTime := 31;
          AdtnQFrac     := 0.2;
          AdtnKFrac     := 0.2;
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Chemicals],
              200,
              1));

          // Company Inputs
          if aCompMax > 0
            then RegisterCompanyInput(0, 0.05, 0.05, aCompMax);
          if aLegalServMax > 0
            then RegisterCompanyInput(intcond(aCompMax > 0, 1, 0), 0.03, 0.02, aLegalServMax);

          Register(MetaEvaluatorPool);
        end;
    end;


  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TStoneMineBlock);
    end;

end.

