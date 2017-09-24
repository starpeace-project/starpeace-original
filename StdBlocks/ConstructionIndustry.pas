unit ConstructionIndustry;

interface

  uses
    Kernel, Population, WorkCenterBlock, Surfaces, OutputEvaluators,
    StdFluids, BackupInterfaces, PolluterWorkCenter, Accounts;

  const
    ConstOpenTime  = 1*24;
    ConstCloseTime = 1*24;

  type
    TMetaConstructionIndustryBlock =
      class(TMetaPolluterWorkCenterBlock)
        public
          constructor Create(anId            : string;
                             aCapacities     : array of TFluidValue;
                             aOreMax         : TFluidValue;
                             aMetalMax       : TFluidValue;
                             aChemicalMax    : TFluidValue;
                             aTimberMax      : TFluidValue;
                             aLegalServMax   : TFluidValue;
                             aCompMax        : TFluidValue;
                             aConstForceMax  : TFluidValue;
                             aMaxBudget      : TMoney;
                             aBlockClass     : CBlock);
      end;

    TConstructionIndustryBlock =
      class(TPolluterWorkCenterBlock)
        private
          fOre        : TInputData;
          fMetals     : TInputData;
          fChemicals  : TInputData;
          fTimber     : TInputData;
          fConstForce : TOutputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    SysUtils, ClassStorage, MathUtils, StdAccounts;


  // TMetaConstructionIndustryBlock

  constructor TMetaConstructionIndustryBlock.Create(anId           : string;
                                                    aCapacities    : array of TFluidValue;
                                                    aOreMax        : TFluidValue;
                                                    aMetalMax      : TFluidValue;
                                                    aChemicalMax   : TFluidValue;
                                                    aTimberMax     : TFluidValue;
                                                    aLegalServMax  : TFluidValue;
                                                    aCompMax       : TFluidValue;
                                                    aConstForceMax : TFluidValue;
                                                    aMaxBudget     : TMoney;
                                                    aBlockClass    : CBlock);
    var
      Sample     : TConstructionIndustryBlock;
      strParm    : string;
      ConstBoost : single;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_ConstIndustry_Supplies,
        accIdx_ConstIndustry_Products,
        accIdx_ConstIndustry_Salaries,
        accIdx_ConstIndustry_Maintenance,
        aBlockClass);
      Sample := nil;

      strParm := TheGlobalConfigHandler.GetConfigParm('ConstBoost', '');
      if strParm <> ''
        then ConstBoost := StrToFloat(strParm)
        else ConstBoost := 10; // >> Old value

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_OreStone,
          inputZero,
          InputData(aOreMax, 100),
          InputData(0, 0),
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidGate_OreStone]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fOre),
          Sample.Offset(Sample.fOre)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Metals,
          inputZero,
          InputData(aMetalMax, 100),
          InputData(0, 0),
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidGate_Metals]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fMetals),
          Sample.Offset(Sample.fMetals)));
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
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Timber,
          inputZero,
          InputData(aTimberMax, 100),
          InputData(0, 0),
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidGate_Timber]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fTimber),
          Sample.Offset(Sample.fTimber)));

      // Company Inputs
      if aCompMax > 0
        then RegisterCompanyInput(tidFluid_CompServ, aCompMax, false);
      if aLegalServMax > 0
        then RegisterCompanyInput(tidFluid_LegalServ, aLegalServMax, false);

      // Outputs
      MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_ConstructionForce,
          FluidData(ConstBoost*aConstForceMax, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_ConstructionForce]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fConstForce),
          Sample.Offset(Sample.fConstForce)));

      // MetaEvaluators
      with TMetaOutputEvaluator.Create(
        aMaxBudget,
        0,
        OutputByName[tidGate_ConstructionForce],
        TOutputEvaluator) do
        begin
          FullOpenTime  := 15;
          FullCloseTime := 17;
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_OreStone],
              300,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Metals],
              250,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Chemicals],
              200,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Timber],
              300,
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
      BackupInterfaces.RegisterClass(TConstructionIndustryBlock);
    end;

end.

