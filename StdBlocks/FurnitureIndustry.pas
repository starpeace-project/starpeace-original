unit FurnitureIndustry;

interface

  uses
    Kernel, Population, WorkCenterBlock, Surfaces, OutputEvaluators,
    StdFluids, BackupInterfaces, PolluterWorkCenter, Accounts;

  type
    TMetaFurnitureIndustryBlock =
      class(TMetaPolluterWorkCenterBlock)
        public
          constructor Create(anId            : string;
                             aCapacities     : array of TFluidValue;
                             aTimberMax      : TFluidValue;
                             aOrgMatMax      : TFluidValue;
                             aFabThrdMax     : TFluidValue;
                             aChemicalMax    : TFluidValue;
                             aPlasticsMax    : TFluidValue;
                             aMetalMax       : TFluidValue;
                             aCompMax        : TFluidValue;
                             aLegalServMax   : TFluidValue;
                             aFurnitureMax   : TFluidValue;
                             aMaxBudget      : TMoney;
                             aBlockClass     : CBlock);
      end;

    TFurnitureIndustryBlock =
      class(TPolluterWorkCenterBlock)
        private
          fTimber     : TInputData;
          fOrganicMat : TInputData;
          fFabThread  : TInputData;
          fChemicals  : TInputData;
          fPlastics   : TInputData;
          fMetal      : TInputData;
          fFurniture  : TOutputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, MathUtils, StdAccounts;


  // TMetaFurnitureIndustryBlock

  constructor TMetaFurnitureIndustryBlock.Create(anId            : string;
                                                 aCapacities     : array of TFluidValue;
                                                 aTimberMax      : TFluidValue;
                                                 aOrgMatMax      : TFluidValue;
                                                 aFabThrdMax     : TFluidValue;
                                                 aChemicalMax    : TFluidValue;
                                                 aPlasticsMax    : TFluidValue;
                                                 aMetalMax       : TFluidValue;
                                                 aCompMax        : TFluidValue;
                                                 aLegalServMax   : TFluidValue;
                                                 aFurnitureMax   : TFluidValue;
                                                 aMaxBudget      : TMoney;
                                                 aBlockClass     : CBlock);
    var
      Sample : TFurnitureIndustryBlock;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_FurnitureIndustry_Supplies,
        accIdx_FurnitureIndustry_Products,
        accIdx_FurnitureIndustry_Salaries,
        accIdx_FurnitureIndustry_Maintenance,
        aBlockClass);
      Sample := nil;

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Timber,
          inputZero,
          InputData(aTimberMax, 100),
          InputData(0, 0),
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Timber]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fTimber),
          Sample.Offset(Sample.fTimber)));
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
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Plastics,
          inputZero,
          InputData(aPlasticsMax, 100),
          InputData(0, 0),
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Plastics]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fPlastics),
          Sample.Offset(Sample.fPlastics)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Metals,
          inputZero,
          InputData(aMetalMax, 100),
          InputData(0, 0),
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Metals]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fMetal),
          Sample.Offset(Sample.fMetal)));

      // Company Inputs
      if aCompMax > 0
        then RegisterCompanyInput(tidFluid_CompServ, aCompMax, false);
      if aLegalServMax > 0
        then RegisterCompanyInput(tidFluid_LegalServ, aLegalServMax, false);

      // Outputs
      MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_Furniture,
          FluidData(aFurnitureMax, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Furniture]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fFurniture),
          Sample.Offset(Sample.fFurniture)));

      // Furniture Evaluator
      with TMetaOutputEvaluator.Create(
        aMaxBudget,
        0,
        OutputByName[tidGate_Furniture],
        TOutputEvaluator) do
        begin
          FullOpenTime  := 30;
          FullCloseTime := 31;
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Timber],
              200,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_OrganicMat],
              100,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_FabThreads],
              100,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Chemicals],
              250,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Plastics],
              250,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Metals],
              250,
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
      BackupInterfaces.RegisterClass(TFurnitureIndustryBlock);
    end;


end.

