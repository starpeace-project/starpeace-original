unit Textil;

interface

  uses
    Kernel, Population, WorkCenterBlock, Surfaces, OutputEvaluators;

  const
    tidGate_OrganicMat = 'OrganicMat';
    tidGate_Chemicals  = 'Chemicals';
    tidGate_LegalServ  = 'LegalServ';
    tidGate_CompServ   = 'CompServ';
    tidGate_FabThreads = 'Fabs&Threads';

  const
    modPollutionRatio = 3;

  type
    TMetaTextilBlock =
      class(TMetaWorkCenter)
        public
          constructor Create(anId            : string;
                             aCapacities     : array of TFluidValue;
                             aPollResistance : TFluidValue;
                             aOrganicMatMax  : TFluidValue;
                             aChemicalMax    : TFluidValue;
                             aLegalServMax   : TFluidValue;
                             aCompMax        : TFluidValue;
                             aFabThreadsMax  : TFluidValue;
                             aMaxBudget      : TMoney;
                             aBlockClass     : CBlock);
          destructor Destroy; override;
        private
          fMetaEvaluatorPool : TMetaEvaluatorPool;
      end;

    TTextilBlock =
      class(TWorkCenter)
        protected
          constructor Create(aMetaBlock : TMetaBlock; aFacility : TFacility); override;
          destructor  Destroy; override;
        private
          fOrganicMat : TInputData;
          fChemicals  : TInputData;
          fLegalServ  : TInputData;
          fCompServ   : TInputData;
          fFabThreads : TOutputData;
        protected
          function GetSurfaceValue(SurfaceId : TSurfaceId) : TSurfaceValue; override;
        protected
          function  Evaluate : TEvaluationResult; override;
          procedure AutoConnect;                  override;
        private
          fEvaluatorPool : TEvaluatorPool;
        private
          fBeautyModifier      : TSurfaceModifier;
          fPollutionIntegrator : TSurfaceIntegrator;
      end;

  procedure RegisterBackup;


implementation

  uses
    StdFluids, ClassStorage, PyramidalModifier, Classes, BackupObjects;


  // TMetaTextilBlock

  constructor TMetaTextilBlock.Create(anId            : string;
                                      aCapacities     : array of TFluidValue;
                                      aPollResistance : TFluidValue;
                                      aOrganicMatMax  : TFluidValue;
                                      aChemicalMax    : TFluidValue;
                                      aLegalServMax   : TFluidValue;
                                      aCompMax        : TFluidValue;
                                      aFabThreadsMax  : TFluidValue;
                                      aMaxBudget      : TMoney;
                                      aBlockClass     : CBlock);
    var
      Sample    : TTextilBlock;
    begin
      inherited Create(anId, aCapacities, aBlockClass);
      Sample := nil;

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_OrganicMat,
          inputZero,
          InputData(aOrganicMatMax, 100, sIlimited),
          InputData(0, 0, sIlimited),
          aOrganicMatMax,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidGate_OrganicMat]),
          5,
          [mgoptCacheable, mgoptBasic, mgoptEditable],
          Sample.Offset(Sample.fOrganicMat)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Chemicals,
          inputZero,
          InputData(aChemicalMax, 100, sIlimited),
          InputData(0, 0, sIlimited),
          aChemicalMax,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidGate_Chemicals]),
          5,
          [mgoptCacheable, mgoptBasic, mgoptEditable],
          Sample.Offset(Sample.fChemicals)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_LegalServ,
          inputZero,
          InputData(aLegalServMax, 100, sIlimited),
          InputData(0, 0, sIlimited),
          aLegalServMax,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_LegalServ]),
          5,
          [mgoptCacheable, mgoptEditable],
          Sample.Offset(Sample.fLegalServ)));
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_CompServ,
          inputZero,
          InputData(aCompMax, 100, sIlimited),
          InputData(0, 0, sIlimited),
          aCompMax,
          TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_CompServ]),
          5,
          [mgoptCacheable, mgoptEditable],
          Sample.Offset(Sample.fCompServ)));

      // Outputs
      MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_FabThreads,
          FluidData(aFabThreadsMax, 100),
          TPullOutput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_FabThreads]),
          5,
          [mgoptCacheable, mgoptEditable],
          Sample.Offset(Sample.fFabThreads)));

      // MetaEvaluators
      fMetaEvaluatorPool := TMetaEvaluatorPool.Create;
      with TMetaOutputEvaluator.Create(
        aMaxBudget,
        60,
        0,
        OutputByName[tidGate_FabThreads],
        TOutputEvaluator) do
        begin
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[PeopleKindPrefix[pkHigh] + tidGate_WorkForce],
              100,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[PeopleKindPrefix[pkMiddle] + tidGate_WorkForce],
              150,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[PeopleKindPrefix[pkLow] + tidGate_WorkForce],
              200,
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
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_LegalServ],
              5,
              1));
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[tidGate_CompServ],
              60,
              1));
          RegisterIntegrator(
            TIntegratorInfo.Create(
              tidEnvironment_Pollution,
              aPollResistance,
              DecreasingEffect));
          Register(fMetaEvaluatorPool);
        end;
    end;

  destructor TMetaTextilBlock.Destroy;
    begin
      fMetaEvaluatorPool.Free;
      inherited;
    end;


  // TTextilBlock

  constructor TTextilBlock.Create(aMetaBlock : TMetaBlock; aFacility : TFacility);
    begin
      inherited;
      fEvaluatorPool := TMetaTextilBlock(aMetaBlock).fMetaEvaluatorPool.Instantiate(self);
    end;

  destructor TTextilBlock.Destroy;
    begin
      fEvaluatorPool.Free;
      fBeautyModifier.Free;
      fPollutionIntegrator.Free;
      inherited;
    end;

  function TTextilBlock.GetSurfaceValue(SurfaceId : TSurfaceId) : TSurfaceValue;
    begin
      if SurfaceId <> tidEnvironment_Pollution
        then result := fPollutionIntegrator.Value
        else result := inherited GetSurfaceValue(SurfaceId);
    end;

  function TTextilBlock.Evaluate : TEvaluationResult;
    begin
      inherited Evaluate;
      if not Facility.CriticalTrouble
        then result := fEvaluatorPool.Evaluate
        else result := evrNormal;
    end;

  procedure TTextilBlock.AutoConnect;
    begin
      inherited;
      fBeautyModifier :=
        TPyramidalModifier.Create(
          tidEnvironment_Beauty,
          Point(xOrigin, yOrigin),
          MetaBlock.Beauty,
          MetaBlock.BeautyStrength);
      fPollutionIntegrator :=
        TSurfaceIntegrator.Create(
          tidEnvironment_Pollution,
          GetArea(modPollutionRatio, amdIncludeBlock));
    end;

  procedure RegisterBackup;
    begin
      BackupObjects.RegisterClass(TTextilBlock);
    end;


end.

