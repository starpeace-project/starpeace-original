unit ElectronicIndustry;

interface

  uses
    Kernel, Population, WorkCenterBlock, Surfaces, OutputEvaluators,
    StdFluids, BackupInterfaces, PolluterWorkCenter, Accounts;

  const
    ElectCompOpenTime  = 1*24;
    ElectCompCloseTime = 1*24;

  type
    TMetaElectronicIndustryBlock =
      class(TMetaPolluterWorkCenterBlock)
        public
          constructor Create(anId            : string;
                             aCapacities     : array of TFluidValue;
                             aSiliconMax     : TFluidValue;
                             aMetalsMax      : TFluidValue;
                             aChemicalsMax   : TFluidValue;
                             aCompMax        : TFluidValue;
                             aLegalServMax   : TFluidValue;
                             aElectronicsMax : TFluidValue;
                             aMaxBudget      : TMoney;
                             aBlockClass     : CBlock);
      end;

    TElectronicIndustryBlock =
      class(TPolluterWorkCenterBlock)
        private
          fSilicons    : TInputData;
          fMetals      : TInputData;
          fChemicals   : TInputData;
          fElectComp   : TOutputData;
      end;

  procedure RegisterBackup;

implementation

  uses
    ClassStorage, MathUtils, StdAccounts;


  // TMetaElectronicIndustryBlock

  constructor TMetaElectronicIndustryBlock.Create(anId            : string;
                                                  aCapacities     : array of TFluidValue;
                                                  aSiliconMax     : TFluidValue;
                                                  aMetalsMax      : TFluidValue;
                                                  aChemicalsMax   : TFluidValue;
                                                  aCompMax        : TFluidValue;
                                                  aLegalServMax   : TFluidValue;
                                                  aElectronicsMax : TFluidValue;
                                                  aMaxBudget      : TMoney;
                                                  aBlockClass     : CBlock);
    var
      Sample : TElectronicIndustryBlock;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_ElectIndustry_Supplies,
        accIdx_ElectIndustry_Products,
        accIdx_ElectIndustry_Salaries,
        accIdx_ElectIndustry_Maintenance,
        aBlockClass);

      Sample := nil;

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_OreSilicon,
          inputZero,
          InputData(aSiliconMax, 100),
          InputData(0, 0),
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_OreSilicon]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fSilicons),
          Sample.Offset(Sample.fSilicons)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Metals,
          inputZero,
          InputData(aMetalsMax, 100),
          InputData(0, 0),
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Metals]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fMetals),
          Sample.Offset(Sample.fMetals)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Chemicals,
          inputZero,
          InputData(aChemicalsMax, 100),
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
          tidGate_ElectComp,
          FluidData(aElectronicsMax, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_ElectComp]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fElectComp),
          Sample.Offset(Sample.fElectComp)));

      // MetaEvaluators
      with TMetaOutputEvaluator.Create(
        aMaxBudget,
        0,
        OutputByName[tidGate_ElectComp],
        TOutputEvaluator) do
        begin
          FullOpenTime  := 30;
          FullCloseTime := 31;
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_OreSilicon],
              500,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Metals],
              500,
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
        end
    end;

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TElectronicIndustryBlock);
    end;

end.
