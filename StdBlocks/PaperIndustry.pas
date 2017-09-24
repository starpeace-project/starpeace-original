unit PaperIndustry;

interface

  uses
    Kernel, Population, WorkCenterBlock, Surfaces, OutputEvaluators,
    StdFluids, BackupInterfaces, PolluterWorkCenter, Accounts;

  const
    ChemOpenTime  = 10;
    ChemCloseTime = 10;

  type
    TMetaPaperBlock =
      class(TMetaPolluterWorkCenterBlock)
        public
          constructor Create(anId            : string;
                             aCapacities     : array of TFluidValue;
                             aLegalServMax   : TFluidValue; 
                             aCompMax        : TFluidValue;
                             aWoodMax        : TFluidValue;
                             aOrgMatMax      : TFluidValue;
                             aChemMax        : TFluidValue;
                             aPaperMax       : TFluidValue;
                             aMaxBudget      : TMoney;   // Money wasted by the industry every tick of simulation
                             aBlockClass     : CBlock);
      end;

    TPaperBlock =
      class(TPolluterWorkCenterBlock)
        private
          fWood      : TInputData;
          fOrgMat    : TInputData;
          fChemicals : TInputData;
          fPaper     : TOutputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, MathUtils, StdAccounts;


  // TMetaPaperBlock

  constructor TMetaPaperBlock.Create(
    anId            : string;
    aCapacities     : array of TFluidValue;
    aLegalServMax   : TFluidValue; 
    aCompMax        : TFluidValue;
    aWoodMax        : TFluidValue;
    aOrgMatMax      : TFluidValue;
    aChemMax        : TFluidValue;
    aPaperMax       : TFluidValue;
    aMaxBudget      : TMoney;   // Money wasted by the industry every tick of simulation
    aBlockClass     : CBlock);
    var
      Sample    : TPaperBlock;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_PaperIndustry_Supplies,
        accIdx_PaperIndustry_Products,
        accIdx_PaperIndustry_Salaries,
        accIdx_PaperIndustry_Maintenance,
        aBlockClass);

      Sample := nil;

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Timber,
          inputZero,
          InputData(aWoodMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidGate_Timber]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fWood),
          Sample.Offset(Sample.fWood)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_OrganicMat,
          inputZero,
          InputData(aOrgMatMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidGate_OrganicMat]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fOrgMat),
          Sample.Offset(Sample.fOrgMat)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Chemicals,
          inputZero,
          InputData(aChemMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidGate_Chemicals]),
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
          tidGate_Paper,
          FluidData(aPaperMax, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Paper]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fPaper),
          Sample.Offset(Sample.fPaper)));

      // MetaEvaluators
      with TMetaOutputEvaluator.Create(
        aMaxBudget,
        0,
        OutputByName[tidGate_Paper],
        TOutputEvaluator) do
        begin
          FullOpenTime  := 30;  // Time it takes to fully open from 0% to 100% with ideal conditions
          FullCloseTime := 31;  // To close in case there is no demand.
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Timber],
              2000, // how important this product is..
              1));  // what share goes to this product in case there is more than one output.
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_OrganicMat],
              1000,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Chemicals],
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
      BackupInterfaces.RegisterClass(TPaperBlock);
    end;

end.


