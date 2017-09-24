unit MachineIndustry;

interface

  uses
    Kernel, Population, WorkCenterBlock, Surfaces, OutputEvaluators;

  const
    tidGate_LegalServ   = 'LegalServ';
    tidGate_CompServ    = 'CompServ';
    tidGate_Metals      = 'Metals';
    tidGate_Machinery   = 'Machinery';

  type
    TMetaMachineIndustryBlock =
      class( TMetaWorkCenter )
        public
          constructor Create( anId            : string;
                              aCapacities     : array of TFluidValue;
                              aPollResistance : TFluidValue;
                              aLegalServMax   : TFluidValue;
                              aCompMax        : TFluidValue;
                              aMetalsMax      : TFluidValue;
                              aMachineryMax   : TFluidValue;
                              aMaxBudget      : TMoney;
                              aBlockClass     : CBlock );
          destructor Destroy; override;
        private
          fMetaEvaluatorPool : TMetaEvaluatorPool;
      end;

    TMachineIndustryBlock =
      class( TWorkCenter )
        protected
          constructor Create( aMetaBlock : TMetaBlock; aFacility : TFacility ); override;
          destructor  Destroy; override;
        private
          fLegalServ : TInputData;
          fCompServ  : TInputData;
          fMetals    : TInputData;
          fMachinery : TOutputData;
        protected
          function GetSurfaceValue( SurfaceId : TSurfaceId ) : TSurfaceValue; override;
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

  // TMetaElectronicIndustryBlock

  constructor TMetaMachineIndustryBlock.Create( anId  : string;
                                                   aCapacities     : array of TFluidValue;
                                                   aPollResistance : TFluidValue;
                                                   aLegalServMax   : TFluidValue;
                                                   aCompMax        : TFluidValue;
                                                   aChemicalsMax   : TFluidValue;
                                                   aMetalsMax      : TFluidValue;
                                                   aElectronicsMax : TFluidValue;
                                                   aMaxBudget      : TMoney;
                                                   aBlockClass     : CBlock );
    var
      Sample    : TElectronicIndustryBlock;
      MetaInput : TMetaInput;
    begin
      inherited Create( anId, aCapacities, aBlockClass );
      Sample := nil;

      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_LegalServ,
          inputZero,
          InputData( aLegalServMax, 100, sIlimited ),
          InputData( 0, 0, sIlimited ),
          aLegalServMax,
          TPullInput,
          TMetaFluid( TheClassStorage.ClassById[ tidClassFamily_Fluids, tidFluid_LegalServ ]),
          5,
          [ mgoptCacheable, mgoptEditable ],
          Sample.Offset( Sample.fLegalServ ) ) );
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_CompServ,
          inputZero,
          InputData( aCompMax, 100, sIlimited ),
          InputData( 0, 0, sIlimited ),
          aCompMax,
          TPullInput,
          TMetaFluid( TheClassStorage.ClassById[ tidClassFamily_Fluids, tidFluid_CompServ ] ),
          5,
          [ mgoptCacheable, mgoptEditable ],
          Sample.Offset( Sample.fCompServ ) ) );
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Metals,
          inputZero,
          InputData( aMetalsMax, 100, sIlimited ),
          InputData( 0, 0, sIlimited ),
          aMetalsMax,
          TPullInput,
          TMetaFluid( TheClassStorage.ClassById[ tidClassFamily_Fluids, tidFluid_Metals ] ),
          5,
          [ mgoptCacheable, mgoptBasic, mgoptEditable ],
          Sample.Offset( Sample.fMetals ) ) );

      // Outputs
      MetaOutputs.Insert(
        TMetaOutput.Create(
          tidGate_Machinery,
          FluidData( aMachineryMax, 100 ),
          TPullOutput,
          TMetaFluid( TheClassStorage.ClassById[ tidClassFamily_Fluids, tidFluid_Electronics ] ),
          5,
          [ mgoptCacheable, mgoptEditable ],
          Sample.Offset( Sample.fMachinery ) ) );

      // MetaEvaluators
      fMetaEvaluatorPool := TMetaEvaluatorPool.Create;
      with TMetaOutputEvaluator.Create(
        aMaxBudget,
        99,
        OutputByName[ tidGate_Machinery ],
        TOutputEvaluator ) do
        begin
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[ PeopleKindPrefix[ pkHigh ] + tidGate_WorkForce ],
              100,
              1 ) );
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[ PeopleKindPrefix[ pkMiddle ] + tidGate_WorkForce ],
              3000,
              1 ) );
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[ PeopleKindPrefix[ pkLow ] + tidGate_WorkForce ],
              1000,
              1 ) );
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[ tidGate_LegalServ ],
              50,
              1 ) );
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[ tidGate_CompServ ],
              100,
              1 ) );
          RegisterInput(
            TMetaInputInfo.Create(
              InputByName[ tidGate_Metals ],
              1000,
              1 ) );
          RegisterIntegrator(
            TIntegratorInfo.Create(
              tidEnvironment_Pollution,
              aPollResistance,
              DecreasingEffect ) );
          Register( fMetaEvaluatorPool );
        end
    end;

  destructor TMetaMachineIndustryBlock.Destroy;
    begin
      fMetaEvaluatorPool.Free;
      inherited;
    end;

  // TElectronicIndustryBlock

  constructor TMachineIndustryBlock.Create( aMetaBlock : TMetaBlock; aFacility : TFacility );
    begin
      inherited;
      fEvaluatorPool := TMetaElectronicIndustryBlock( aMetaBlock ).fMetaEvaluatorPool.Instantiate( Self )
    end;

  destructor TMachineIndustryBlock.Destroy;
    begin
      fEvaluatorPool.Free;
      inherited
    end;

  function TMachineIndustryBlock.GetSurfaceValue( SurfaceId : TSurfaceId ) : TSurfaceValue;
    begin
      if SurfaceId <> tidEnvironment_Pollution
        then
          Result := fPollutionIntegrator.Value
        else
          Result := inherited GetSurfaceValue( SurfaceId )
    end;

  function TMachineIndustryBlock.Evaluate : TEvaluationResult;
    begin
      inherited Evaluate;
      if not Facility.CriticalTrouble
        then Result := fEvaluatorPool.Evaluate
        else Result := evrNormal
    end;

  procedure TMachineIndustryBlock.AutoConnect;
    begin
      inherited;
      fBeautyModifier :=
        TPyramidalModifier.Create(
          tidEnvironment_Beauty,
          Point( xOrigin, yOrigin ),
          MetaBlock.Beauty,
          MetaBlock.BeautyStrength );
      fPollutionIntegrator :=
        TSurfaceIntegrator.Create(
          tidEnvironment_Pollution,
          GetArea( modPollutionRatio, amdIncludeBlock ) )
    end;

  procedure RegisterBackup;
    begin
      BackupObjects.RegisterClass( TMachineIndustryBlock )
    end;


end.
