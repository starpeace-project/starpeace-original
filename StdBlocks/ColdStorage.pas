unit ColdStorage;

interface

  uses
    Warehouses, Kernel, Surfaces, WorkCenterBlock, BackupInterfaces, CacheAgent;

  type
    TMetaColdStorage =
      class(TMetaWarehouse)
        public
          constructor Create(anId          : string;
                             aCapacities   : array of TFluidValue;
                             aFreshFoodMax : TFluidValue;
                             aElabFoodMax  : TFluidValue;
                             aLiquorMax    : TFluidValue;
                             theOverPrice  : TPercent;
                             aBlockClass   : CBlock);
      end;

    TColdStorage =
      class(TWarehouse)
        private
          fFreshFoodIn  : TInputData;
          fElabFoodIn   : TInputData;
          fLiquorIn     : TInputData;
          fFreshFoodOut : TOutputData;
          fElabFoodOut  : TOutputData;
          fLiquorOut    : TOutputData;
      end;

  procedure RegisterBackup;

implementation

  uses
    ClassStorage, StdFluids;

  // TMetaColdStorage

  constructor TMetaColdStorage.Create(anId          : string;
                                      aCapacities   : array of TFluidValue;
                                      aFreshFoodMax : TFluidValue;
                                      aElabFoodMax  : TFluidValue;
                                      aLiquorMax    : TFluidValue;
                                      theOverPrice  : TPercent;
                                      aBlockClass   : CBlock);
    var
      Sample : TColdStorage;
    begin
      inherited Create(anId, aCapacities, aBlockClass);
      Sample := nil;
      // Inputs
      NewMetaInput(
        tidGate_FreshFood,
        tidFluid_FreshFood,
        aFreshFoodMax,
        sizeof(Sample.fFreshFoodIn),
        Sample.Offset(Sample.fFreshFoodIn));
      NewMetaInput(
        tidGate_ElabFood,
        tidFluid_ElabFood,
        aElabFoodMax,
        sizeof(Sample.fElabFoodIn),
        Sample.Offset(Sample.fElabFoodIn));
      NewMetaInput(
        tidGate_Liquors,
        tidFluid_Liquors,
        aLiquorMax,
        sizeof(Sample.fLiquorIn),
        Sample.Offset(Sample.fLiquorIn));

      // Outputs
      NewMetaOutput(
        tidGate_FreshFood,
        tidFluid_FreshFood,
        aFreshFoodMax,
        sizeof(Sample.fFreshFoodOut),
        Sample.Offset(Sample.fFreshFoodOut));
      NewMetaOutput(
        tidGate_ElabFood,
        tidFluid_ElabFood,
        aElabFoodMax,
        sizeof(Sample.fElabFoodIn),
        Sample.Offset(Sample.fElabFoodOut));
      NewMetaOutput(
        tidGate_Liquors,
        tidFluid_Liquors,
        aLiquorMax,
        sizeof(Sample.fLiquorIn),
        Sample.Offset(Sample.fLiquorOut));

      // Wares
      RegisterWare(tidGate_FreshFood, 60, 0, theOverPrice, aFreshFoodMax);
      RegisterWare(tidGate_ElabFood,  40, 0, theOverPrice, aElabFoodMax);
      RegisterWare(tidGate_Liquors,  40, 0, theOverPrice, aLiquorMax);
    end;


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TColdStorage);
    end;


end.
