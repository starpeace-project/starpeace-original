unit PrintingPlant;

interface
                 
  uses
    Kernel, Population, WorkCenterBlock, Surfaces, OutputEvaluators,
    StdFluids, BackupInterfaces, PolluterWorkCenter, Accounts;

  const
    ChemOpenTime  = 10;
    ChemCloseTime = 10;

  type
    TMetaPrintingBlock =
      class(TMetaPolluterWorkCenterBlock)
        public
          constructor Create(anId                    : string;
                             aCapacities             : array of TFluidValue;
                             aLegalServMax           : TFluidValue;
                             aCompMax                : TFluidValue;
                             aPaperMax               : TFluidValue;
                             aOrgMatMax              : TFluidValue;
                             aChemMax                : TFluidValue;
                             aBookMax                : TFluidValue;
                             aPrintedMaterialMax     : TFluidValue;
                             aMaxBudget              : TMoney;   // Money wasted by the industry every tick of simulation
                             aBlockClass             : CBlock);
      end;

    TPrintingBlock =
      class(TPolluterWorkCenterBlock)
        private
          fPaper                     : TInputData;
          fOrgMat                    : TInputData;
          fChemicals                 : TInputData;
          fPrintedMaterial           : TOutputData;
          fBooks                     : TOutputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, MathUtils, StdAccounts;


  // TMetaPrintingBlock

  constructor TMetaPrintingBlock.Create(
    anId                  : string;
    aCapacities           : array of TFluidValue;
    aLegalServMax         : TFluidValue;
    aCompMax              : TFluidValue;
    aPaperMax             : TFluidValue;
    aOrgMatMax            : TFluidValue;
    aChemMax              : TFluidValue;
    aBookMax              : TFluidValue;
    aPrintedMaterialMax   : TFluidValue;
    aMaxBudget            : TMoney;   // Money wasted by the industry every tick of simulation
    aBlockClass           : CBlock);
    var
      Sample    : TPrintingBlock;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_PrintingPlant_Supplies,
        accIdx_PrintingPlant_Products,
        accIdx_PrintingPlant_Salaries,
        accIdx_PrintingPlant_Maintenance,
        aBlockClass);

      Sample := nil;

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Paper,
          inputZero,
          InputData(aPaperMax, 100),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidGate_Paper]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fPaper),
          Sample.Offset(Sample.fPaper)));
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
          tidGate_Books,
          FluidData(aBookMax, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Books]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fBooks),
          Sample.Offset(Sample.fBooks)));
      MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_PrintedMaterial,
          FluidData(aPrintedMaterialMax, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_PrintedMaterial]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fPrintedMaterial),
          Sample.Offset(Sample.fPrintedMaterial)));

      // MetaEvaluators
      with TMetaOutputEvaluator.Create(
        aMaxBudget/2,
        0,
        OutputByName[tidGate_Books],
        TOutputEvaluator) do
        begin
          FullOpenTime  := 30;  // Time it takes to fully open from 0% to 100% with ideal conditions
          FullCloseTime := 31;  // To close in case there is no demand.
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Paper],
              2000, // how important this product is..
              0.7));  // what share goes to this product in case there is more than one output.
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_OrganicMat],
              500,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Chemicals],
              1000,
              0.5));

          // Company Inputs
          if aCompMax > 0
            then RegisterCompanyInput(0, 0.05, 0.05, aCompMax);
          if aLegalServMax > 0
            then RegisterCompanyInput(intcond(aCompMax > 0, 1, 0), 0.03, 0.02, aLegalServMax);
          Register(MetaEvaluatorPool);
        end;
      with TMetaOutputEvaluator.Create(
        aMaxBudget/2,
        0,
        OutputByName[tidGate_PrintedMaterial],
        TOutputEvaluator) do
        begin
          FullOpenTime  := 30;  // Time it takes to fully open from 0% to 100% with ideal conditions
          FullCloseTime := 31;  // To close in case there is no demand.
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Paper],
              2000, // how important this product is..
              0.3));  // what share goes to this product in case there is more than one output
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Chemicals],
              1000,
              0.5));

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
      BackupInterfaces.RegisterClass(TPrintingBlock);
    end;

end.
