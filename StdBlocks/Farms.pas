unit Farms;

interface

  uses
    Kernel, Population, WorkCenterBlock, Surfaces, OutputEvaluators, Accounts,
    BackupInterfaces, PolluterWorkCenter, StdFluids;

  const
    modPollutionRatio  = 3;

  const
    tidWeather_FarmProduction = 'Farm-Weather';  

  type
    TMetaFarmBlock =
      class(TMetaPolluterWorkCenterBlock)
        public
          constructor Create(anId            : string;
                             aCapacities     : array of TFluidValue;
                             aChemicalMax    : TFluidValue;
                             aLegalServMax   : TFluidValue;
                             aCompMax        : TFluidValue;
                             aFreshFoodMax   : TFluidValue;
                             anOrganicMatMax : TFluidValue;
                             aMaxBudget      : TMoney;
                             aBlockClass     : CBlock );
      end;

    TFarmBlock =
      class(TPolluterWorkCenterBlock)
        private
          fChemicals  : TInputData;
          fFreshFood  : TOutputData;
          fOrganicMat : TOutputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    SysUtils, ClassStorage, PyramidalModifier, Classes, SimHints, StdAccounts,
    Standards, MathUtils;


  // TMetaFarmBlock

  constructor TMetaFarmBlock.Create(anId            : string;
                                    aCapacities     : array of TFluidValue;
                                    aChemicalMax    : TFluidValue;
                                    aLegalServMax   : TFluidValue;
                                    aCompMax        : TFluidValue;
                                    aFreshFoodMax   : TFluidValue;
                                    anOrganicMatMax : TFluidValue;
                                    aMaxBudget      : TMoney;
                                    aBlockClass     : CBlock);
    var
      Sample : TFarmBlock;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_Farms_Supplies,
        accIdx_Farms_Products,
        accIdx_Farms_Salaries,
        accIdx_Farms_Maintenance,
        aBlockClass);

      Sample := nil;

      WeatherEnvelope := RegisterWeatherEnvelope(tidWeather_FarmProduction);

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Chemicals,
          inputZero,
          InputData( aChemicalMax, 100 ),
          inputZero,
          qIlimited,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Chemicals]),
          5,
          mglAditional,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fChemicals),
          Sample.Offset( Sample.fChemicals )));

      // Company Inputs
      if aCompMax > 0
        then RegisterCompanyInput(tidFluid_CompServ, aCompMax, false);
      if aLegalServMax > 0
        then RegisterCompanyInput(tidFluid_LegalServ, aLegalServMax, false);

      // Outputs
      MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_FreshFood,
          FluidData( aFreshFoodMax, 100 ),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_FreshFood]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fFreshFood),
          Sample.Offset( Sample.fFreshFood )));
      MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_OrganicMat,
          FluidData( anOrganicMatMax, 100 ),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_OrganicMat]),
          5,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fOrganicMat),
          Sample.Offset( Sample.fOrganicMat )));

      // MetaEvaluators
      with TMetaOutputEvaluator.Create(
        aMaxBudget/2,
        50,
        OutputByName[tidGate_FreshFood],
        TOutputEvaluator ) do
        begin
          FullOpenTime  := 30;
          FullCloseTime := 31;
          WFPerc        := 70;
          Importance    := 60;
          AdtnQFrac     := 0.1;
          AdtnKFrac     := 0.1;
          PrdWthrEnv    := WeatherEnvelope;
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

          Register( MetaEvaluatorPool );
        end;
      with TMetaOutputEvaluator.Create(
        aMaxBudget/2,
        50,
        OutputByName[tidGate_OrganicMat],
        TOutputEvaluator ) do
        begin
          FullOpenTime  := 30;
          FullCloseTime := 31;
          WFPerc        := 30;
          Importance    := 40;
          AdtnQFrac     := 0.1;
          AdtnKFrac     := 0.1;
          PrdWthrEnv    := WeatherEnvelope;
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_Chemicals],
              300,
              0.5));

          // Company Inputs
          if aCompMax > 0
            then RegisterCompanyInput(0, 0.05, 0.05, aCompMax);
          if aLegalServMax > 0
            then RegisterCompanyInput(intcond(aCompMax > 0, 1, 0), 0.03, 0.02, aLegalServMax);

          Register( MetaEvaluatorPool );
        end;
    end;

  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( TFarmBlock );
    end;


end.


