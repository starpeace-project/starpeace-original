unit OreStorage;

interface

  uses
    Warehouses, Kernel, Surfaces, WorkCenterBlock, BackupInterfaces, CacheAgent;

  type
    TMetaOreStorage =
      class(TMetaWarehouse)
        public
          constructor Create(anId         : string;
                             aCapacities  : array of TFluidValue;
                             aOreMax      : TFluidValue;
                             aMetalMax    : TFluidValue;
                             aConstMax    : TFluidValue;
                             aTimberMax   : TFluidValue;
                             theOverPrice : TPercent;
                             aBlockClass  : CBlock);
      end;

    TOreStorage =
      class(TWarehouse)
        private
          fOreIn         : TInputData;
          fOreChemsIn    : TInputData;
          fOreSiliconIn  : TInputData;
          fOreStoneIn    : TInputData;
          fOreCoalIn     : TInputData;
          fMetalIn       : TInputData;
          fConstIn       : TInputData;
          fTimberIn      : TInputData;
          fOreOut        : TOutputData;
          fOreChemsOut   : TOutputData;
          fOreSiliconOut : TOutputData;
          fOreStoneOut   : TOutputData;
          fOreCoalOut    : TOutputData;
          fMetalOut      : TOutputData;
          fConstOut      : TOutputData;
          fTimberOut     : TOutputData;
      end;

  procedure RegisterBackup;

implementation

  uses
    ClassStorage, StdFluids;

  // TMetaOreStorage

  constructor TMetaOreStorage.Create(anId         : string;
                                     aCapacities  : array of TFluidValue;
                                     aOreMax      : TFluidValue;
                                     aMetalMax    : TFluidValue;
                                     aConstMax    : TFluidValue;
                                     aTimberMax   : TFluidValue;
                                     theOverPrice : TPercent;
                                     aBlockClass  : CBlock);
    var
      Sample : TOreStorage;
    begin
      inherited Create(anId, aCapacities, aBlockClass);
      Sample := nil;
      // Inputs
      NewMetaInput(
        tidGate_Ore,
        tidFluid_Ore,
        aOreMax,
        sizeof(Sample.fOreIn),
        Sample.Offset(Sample.fOreIn));
      NewMetaInput(
        tidGate_OreChems,
        tidFluid_OreChems,
        aOreMax,
        sizeof(Sample.fOreChemsIn),
        Sample.Offset(Sample.fOreChemsIn));
      NewMetaInput(
        tidGate_OreSilicon,
        tidFluid_OreSilicon,
        aOreMax,
        sizeof(Sample.fOreSiliconIn),
        Sample.Offset(Sample.fOreSiliconIn));
      NewMetaInput(
        tidGate_OreStone,
        tidFluid_OreStone,
        aOreMax,
        sizeof(Sample.fOreStoneIn),
        Sample.Offset(Sample.fOreStoneIn));
      NewMetaInput(
        tidGate_OreCoal,
        tidFluid_OreCoal,
        aOreMax,
        sizeof(Sample.fOreCoalIn),
        Sample.Offset(Sample.fOreCoalIn));
      NewMetaInput(
        tidGate_Metals,
        tidFluid_Metals,
        aMetalMax,
        sizeof(Sample.fMetalIn),
        Sample.Offset(Sample.fMetalIn));
      NewMetaInput(
        tidGate_ConstructionForce,
        tidFluid_ConstructionForce,
        aConstMax,
        sizeof(Sample.fConstIn),
        Sample.Offset(Sample.fConstIn));
      NewMetaInput(
        tidGate_Timber,
        tidFluid_Timber,
        aTimberMax,
        sizeof(Sample.fTimberIn),
        Sample.Offset(Sample.fTimberIn));

      // Outputs
      NewMetaOutput(
        tidGate_Ore,
        tidFluid_Ore,
        aOreMax,
        sizeof(Sample.fOreOut),
        Sample.Offset(Sample.fOreOut));
      NewMetaOutput(
        tidGate_OreChems,
        tidFluid_OreChems,
        aOreMax,
        sizeof(Sample.fOreChemsOut),
        Sample.Offset(Sample.fOreChemsOut));
      NewMetaOutput(
        tidGate_OreSilicon,
        tidFluid_OreSilicon,
        aOreMax,
        sizeof(Sample.fOreSiliconOut),
        Sample.Offset(Sample.fOreSiliconOut));
      NewMetaOutput(
        tidGate_OreStone,
        tidFluid_OreStone,
        aOreMax,
        sizeof(Sample.fOreStoneOut),
        Sample.Offset(Sample.fOreStoneOut));
      NewMetaOutput(
        tidGate_OreCoal,
        tidFluid_OreCoal,
        aOreMax,
        sizeof(Sample.fOreCoalOut),
        Sample.Offset(Sample.fOreCoalOut));
      NewMetaOutput(
        tidGate_Metals,
        tidFluid_Metals,
        aMetalMax,
        sizeof(Sample.fMetalOut),
        Sample.Offset(Sample.fMetalOut));
      NewMetaOutput(
        tidGate_ConstructionForce,
        tidFluid_ConstructionForce,
        aConstMax,
        sizeof(Sample.fConstOut),
        Sample.Offset(Sample.fConstOut));
      NewMetaOutput(
        tidGate_Timber,
        tidFluid_Timber,
        aTimberMax,
        sizeof(Sample.fTimberOut),
        Sample.Offset(Sample.fTimberOut));

      // Wares
      RegisterWare(tidGate_Ore, 15, 0, theOverPrice, aOreMax);
      RegisterWare(tidGate_OreChems, 15, 0, theOverPrice, aOreMax);
      RegisterWare(tidGate_OreSilicon, 16, 0, theOverPrice, aOreMax);
      RegisterWare(tidGate_OreStone, 17, 0, theOverPrice, aOreMax);
      RegisterWare(tidGate_OreCoal, 17, 0, theOverPrice, aOreMax);
      RegisterWare(tidGate_Metals, 20, 0, theOverPrice, aMetalMax);
      RegisterWare(tidGate_ConstructionForce, 20, 0, theOverPrice, aConstMax);
      RegisterWare(tidGate_Timber, 20, 0, theOverPrice, aTimberMax);
    end;


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TOreStorage);
    end;


end.
