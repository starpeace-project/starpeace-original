unit CDPlant;

interface

  uses
    Kernel, Population, WorkCenterBlock, Surfaces, OutputEvaluators,
    StdFluids, BackupInterfaces, PolluterWorkCenter, Accounts;

  const
    ChemOpenTime  = 10;
    ChemCloseTime = 10;

  type
    TMetaCDBlock =
      class(TMetaPolluterWorkCenterBlock)
        public
          constructor Create(anId            : string;
                             aCapacities         : array of TFluidValue;
                             aLegalServMax       : TFluidValue;
                             aCompMax            : TFluidValue;
                             aPlasticMax         : TFluidValue;
                             aPrintedMaterialMax : TFluidValue;
                             aCDMax              : TFluidValue;
                             aMaxBudget          : TMoney;
                             aBlockClass         : CBlock);
      end;

    TCDBlock =
      class(TPolluterWorkCenterBlock)
        private
          fPlastics          : TInputData;
          fPrintedMaterial   : TInputData;
          fCDs               : TOutputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, MathUtils, StdAccounts;


  // TMetaCDBlock

  constructor TMetaCDBlock.Create(anId                : string;
                                  aCapacities         : array of TFluidValue;
                                  aLegalServMax       : TFluidValue;
                                  aCompMax            : TFluidValue;
                                  aPlasticMax         : TFluidValue;
                                  aPrintedMaterialMax : TFluidValue;
                                  aCDMax              : TFluidValue;
                                  aMaxBudget          : TMoney;
                                  aBlockClass         : CBlock);
    var
      Sample    : TCDBlock;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_CDPlant_Supplies,
        accIdx_CDPlant_Products,
        accIdx_CDPlant_Salaries,
        accIdx_CDPlant_Maintenance,
        aBlockClass);

      Sample := nil;

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Plastics,
          inputZero,
          InputData(aPlasticMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidGate_Plastics]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fPlastics),
          Sample.Offset(Sample.fPlastics)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_PrintedMaterial,
          inputZero,
          InputData(aPrintedMaterialMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidGate_PrintedMaterial]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fPrintedMaterial),
          Sample.Offset(Sample.fPrintedMaterial)));

      // Company Inputs
      if aCompMax > 0
        then RegisterCompanyInput(tidFluid_CompServ, aCompMax, false);
      if aLegalServMax > 0
        then RegisterCompanyInput(tidFluid_LegalServ, aLegalServMax, false);

      // Outputs
      MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_CDs,
          FluidData(aCDMax, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_CDs]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fCDs),
          Sample.Offset(Sample.fCDs)));

      // MetaEvaluators
      with TMetaOutputEvaluator.Create(
        aMaxBudget,
        0,
        OutputByName[tidGate_CDs],
        TOutputEvaluator) do
        begin
          FullOpenTime  := 30;
          FullCloseTime := 31;
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Plastics],
              2500,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_PrintedMaterial],
              500,
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
      BackupInterfaces.RegisterClass(TCDBlock);
    end;

end.


