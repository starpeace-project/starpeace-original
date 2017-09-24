unit PlasticInd;

interface

  uses
    Kernel, Population, WorkCenterBlock, Surfaces, OutputEvaluators,
    StdFluids, BackupInterfaces, PolluterWorkCenter, Accounts;

  type
    TMetaPlasticIndustryBlock =
      class(TMetaPolluterWorkCenterBlock)
        public
          constructor Create(anId            : string;
                             aCapacities     : array of TFluidValue;
                             aOilMax         : TFluidValue;
                             aChemicalMax    : TFluidValue;
                             aLegalServMax   : TFluidValue;
                             aCompMax        : TFluidValue;
                             aPlasticMax     : TFluidValue;
                             aMaxBudget      : TMoney;
                             aBlockClass     : CBlock);
      end;

    TPlasticIndustryBlock =
      class(TPolluterWorkCenterBlock)
        private
          fOil       : TInputData;
          fChemicals : TInputData;
          fPlastic     : TOutputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, MathUtils, StdAccounts;


  // TMetaPlasticIndustryBlock

  constructor TMetaPlasticIndustryBlock.Create(anId            : string;
                                               aCapacities     : array of TFluidValue;
                                               aOilMax         : TFluidValue;
                                               aChemicalMax    : TFluidValue;
                                               aLegalServMax   : TFluidValue;
                                               aCompMax        : TFluidValue;
                                               aPlasticMax     : TFluidValue;
                                               aMaxBudget      : TMoney;
                                               aBlockClass     : CBlock);
    var
      Sample : TPlasticIndustryBlock;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_PlasticIndustry_Supplies,
        accIdx_PlasticIndustry_Products,
        accIdx_PlasticIndustry_Salaries,
        accIdx_PlasticIndustry_Maintenance,
        aBlockClass);

      Sample := nil;

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Oil,
          inputZero,
          InputData(aOilMax, 100),
          InputData(0, 0),
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidGate_Oil]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fOil),
          Sample.Offset(Sample.fOil)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Chemicals,
          inputZero,
          InputData(aChemicalMax, 100),
          InputData(0, 0),
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
          tidGate_Plastics,
          FluidData(aPlasticMax, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Plastics]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fPlastic),
          Sample.Offset(Sample.fPlastic)));

      // MetaEvaluators
      with TMetaOutputEvaluator.Create(
        aMaxBudget,
        0,
        OutputByName[tidGate_Plastics],
        TOutputEvaluator) do                                                   
        begin
          FullOpenTime  := 30;
          FullCloseTime := 31;
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Oil],
              300,
              1));
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
      BackupInterfaces.RegisterClass(TPlasticIndustryBlock);
    end;

end.

