unit Clothings;

interface

  uses
    Kernel, Population, WorkCenterBlock, Surfaces, OutputEvaluators,
    StdFluids, BackupInterfaces, PolluterWorkCenter, Accounts;

  type
    TMetaClothingsIndustryBlock =
      class(TMetaPolluterWorkCenterBlock)
        public
          constructor Create(anId          : string;
                             aCapacities   : array of TFluidValue;
                             aFabThrdMax   : TFluidValue;
                             aOrgMatMax    : TFluidValue;
                             aChemicalMax  : TFluidValue;
                             aCompMax      : TFluidValue;
                             aLegalServMax : TFluidValue;
                             aClothesMax   : TFluidValue;
                             aMaxBudget    : TMoney;
                             aBlockClass   : CBlock);
      end;

    TClothingsIndustryBlock =
      class(TPolluterWorkCenterBlock)
        private
          fFabThread      : TInputData;
          fOrganicMat     : TInputData;
          fChemicals      : TInputData;
          fClothes        : TOutputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, MathUtils, StdAccounts;


  // TMetaClothingsIndustryBlock

  constructor TMetaClothingsIndustryBlock.Create( anId          : string;
                                                  aCapacities   : array of TFluidValue;
                                                  aFabThrdMax   : TFluidValue;
                                                  aOrgMatMax    : TFluidValue;
                                                  aChemicalMax  : TFluidValue;
                                                  aCompMax      : TFluidValue;
                                                  aLegalServMax : TFluidValue;
                                                  aClothesMax   : TFluidValue;
                                                  aMaxBudget    : TMoney;
                                                  aBlockClass   : CBlock);
    var
      Sample    : TClothingsIndustryBlock;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_ClotheIndustry_Supplies,
        accIdx_ClotheIndustry_Products,
        accIdx_ClotheIndustry_Salaries,
        accIdx_ClotheIndustry_Maintenance,
        aBlockClass);
      Sample := nil;

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_FabThreads,
          inputZero,
          InputData(aFabThrdMax, 100),
          InputData(0, 0),
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_FabThreads]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fFabThread),
          Sample.Offset(Sample.fFabThread)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_OrganicMat,
          inputZero,
          InputData(aOrgMatMax, 100),
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
          tidGate_Clothes,
          FluidData(aClothesMax, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Clothes]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fClothes),
          Sample.Offset(Sample.fClothes)));

      // Clothes
      with TMetaOutputEvaluator.Create(
        aMaxBudget,
        0,
        OutputByName[tidGate_Clothes],
        TOutputEvaluator) do
        begin
          FullOpenTime  := 30;
          FullCloseTime := 31;
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_FabThreads],
              1000,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_OrganicMat],
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
        end;
    end;

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TClothingsIndustryBlock);
    end;


end.

