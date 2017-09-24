unit ChemicalStorage;

interface

  uses
    Warehouses, Kernel, Surfaces, WorkCenterBlock, BackupInterfaces, CacheAgent;

  type
    TMetaChemicalStorage =
      class(TMetaWarehouse)
        public
          constructor Create(anId          : string;
                             aCapacities   : array of TFluidValue;
                             aChemicalsMax : TFluidValue;
                             aOilMax       : TFluidValue;
                             aGasolineMax  : TFluidValue;
                             theOverPrice  : TPercent;
                             aBlockClass   : CBlock);
      end;

    TChemicalStorage =
      class(TWarehouse)
        private
          fChemicalsIn  : TInputData;
          fChemicalsOut : TOutputData;
          fOilIn        : TInputData;
          fOilOut       : TOutputData;
          fGasolineIn   : TInputData;
          fGasolineOut  : TOutputData;
      end;

  procedure RegisterBackup;
      
implementation

  uses
    ClassStorage, StdFluids;

  // TMetaChemicalStorage

  constructor TMetaChemicalStorage.Create(anId          : string;
                                          aCapacities   : array of TFluidValue;
                                          aChemicalsMax : TFluidValue;
                                          aOilMax       : TFluidValue;
                                          aGasolineMax  : TFluidValue;
                                          theOverPrice  : TPercent;
                                          aBlockClass   : CBlock);
    var
      Sample : TChemicalStorage;
    begin
      inherited Create(anId, aCapacities, aBlockClass);
      Sample := nil;
      // Inputs
      NewMetaInput(
        tidGate_Chemicals,
        tidFluid_Chemicals,
        aChemicalsMax,
        sizeof(Sample.fChemicalsIn),
        Sample.Offset(Sample.fChemicalsIn));

      // Outputs
      NewMetaOutput(
        tidGate_Chemicals,
        tidFluid_Chemicals,
        aChemicalsMax,
        sizeof(Sample.fChemicalsOut),
        Sample.Offset(Sample.fChemicalsOut));

      // Inputs
      NewMetaInput(
        tidGate_Oil,
        tidFluid_Oil,
        aOilMax,
        sizeof(Sample.fOilIn),
        Sample.Offset(Sample.fOilIn));

      // Outputs
      NewMetaOutput(
        tidGate_Oil,
        tidFluid_Oil,
        aOilMax,
        sizeof(Sample.fOilOut),
        Sample.Offset(Sample.fOilOut));

      // Inputs
      NewMetaInput(
        tidGate_Gasoline,
        tidFluid_Gasoline,
        aGasolineMax,
        sizeof(Sample.fGasolineIn),
        Sample.Offset(Sample.fGasolineIn));

      // Outputs
      NewMetaOutput(
        tidGate_Gasoline,
        tidFluid_Gasoline,
        aGasolineMax,
        sizeof(Sample.fGasolineOut),
        Sample.Offset(Sample.fGasolineOut));

      // Wares
      RegisterWare(tidGate_Chemicals, 100, 0, theOverPrice, aChemicalsMax);
      RegisterWare(tidGate_Oil, 100, 0, theOverPrice, aOilMax);
      RegisterWare(tidGate_Gasoline, 100, 0, theOverPrice, aGasolineMax);
    end;

  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TChemicalStorage);
    end;


end.
