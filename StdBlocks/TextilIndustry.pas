unit TextilIndustry;

interface

  uses
    Kernel, Population, WorkCenterBlock, Surfaces, OutputEvaluators,
    StdFluids, BackupInterfaces, PolluterWorkCenter, Accounts;

  type
    TMetaTextilIndustryBlock =
      class(TMetaPolluterWorkCenterBlock)
        public
          constructor Create(anId            : string;
                             aCapacities     : array of TFluidValue;
                             aOrganicMatMax  : TFluidValue;
                             aChemicalMax    : TFluidValue;
                             aCompMax        : TFluidValue;
                             aLegalServMax   : TFluidValue;
                             aFabThreadsMax  : TFluidValue;
                             aMaxBudget      : TMoney;
                             aBlockClass     : CBlock);
      end;

    TTextilIndustryBlock =
      class(TPolluterWorkCenterBlock)
        private
          fOrganicMat : TInputData;
          fChemicals  : TInputData;
          fFabThreads : TOutputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, MathUtils, StdAccounts;

  // TMetaTextilIndustryBlock

  constructor TMetaTextilIndustryBlock.Create(anId            : string;
                                              aCapacities     : array of TFluidValue;
                                              aOrganicMatMax  : TFluidValue;
                                              aChemicalMax    : TFluidValue;
                                              aCompMax        : TFluidValue;
                                              aLegalServMax   : TFluidValue;
                                              aFabThreadsMax  : TFluidValue;
                                              aMaxBudget      : TMoney;
                                              aBlockClass     : CBlock);
    var
      Sample    : TTextilIndustryBlock;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_TextileIndustry_Supplies,
        accIdx_TextileIndustry_Products,
        accIdx_TextileIndustry_Salaries,
        accIdx_TextileIndustry_Maintenance,
        aBlockClass);
        
      Sample := nil;

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_OrganicMat,
          inputZero,
          InputData(aOrganicMatMax, 100),
          InputData(0, 0),
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_OrganicMat]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fOrganicMat),
          Sample.Offset(Sample.fOrganicMat)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Chemicals,
          inputZero,                                            
          InputData(aChemicalMax, 100),
          InputData(0, 0),
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Chemicals]),
          5,
          mglBasic,
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
          tidGate_FabThreads,
          FluidData(aFabThreadsMax, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_FabThreads]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fFabThreads),
          Sample.Offset(Sample.fFabThreads)));

      // MetaEvaluators
      with TMetaOutputEvaluator.Create(
        aMaxBudget,
        0,
        OutputByName[tidGate_FabThreads],
        TOutputEvaluator) do
        begin
          FullOpenTime  := 30;
          FullCloseTime := 31;
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_OrganicMat],
              1000,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Chemicals],
              400,
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
      BackupInterfaces.RegisterClass(TTextilIndustryBlock);
    end;


end.

