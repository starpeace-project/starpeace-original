unit FabricsStorage;

interface

  uses
    Warehouses, Kernel, Surfaces, WorkCenterBlock, BackupInterfaces, CacheAgent;

  type
    TMetaFabricsStorage =
      class(TMetaWarehouse)
        public
          constructor Create(anId              : string;
                             aCapacities       : array of TFluidValue;
                             aOrgMatMax        : TFluidValue;
                             aFabricsMax       : TFluidValue;
                             aClothesMax       : TFluidValue;
                             aPaperMax         : TFluidValue;
                             aPrintedMatMax    : TFluidValue;
                             theOverPrice      : TPercent;
                             aBlockClass       : CBlock);
      end;

    TFabricsStorage =
      class(TWarehouse)
        private
          fOrganicsIn    : TInputData;
          fFabricsIn     : TInputData;
          fClothesIn     : TInputData;
          fPaperIn       : TInputData;
          fPrintedMatIn  : TInputData;
          fOrganicsOut   : TOutputData;
          fFabricsOut    : TOutputData;
          fClothesOut    : TOutputData;
          fPaperOut      : TOutputData;
          fPrintedMatOut : TOutputData;
      end;

  procedure RegisterBackup;

implementation

  uses
    ClassStorage, StdFluids;

  // TMetaFabricsStorage

  constructor TMetaFabricsStorage.Create(anId              : string;
                                         aCapacities       : array of TFluidValue;
                                         aOrgMatMax        : TFluidValue;
                                         aFabricsMax       : TFluidValue;
                                         aClothesMax       : TFluidValue;
                                         aPaperMax         : TFluidValue;
                                         aPrintedMatMax    : TFluidValue;
                                         theOverPrice      : TPercent;
                                         aBlockClass       : CBlock);
    var
      Sample : TFabricsStorage;
    begin
      inherited Create(anId, aCapacities, aBlockClass);
      Sample := nil;
      // Inputs
      NewMetaInput(
        tidGate_OrganicMat,
        tidFluid_OrganicMat,
        aOrgMatMax,
        sizeof(Sample.fOrganicsIn),
        Sample.Offset(Sample.fOrganicsIn));
      NewMetaInput(
        tidGate_FabThreads,
        tidFluid_FabThreads,
        aFabricsMax,
        sizeof(Sample.fFabricsIn),
        Sample.Offset(Sample.fFabricsIn));
      NewMetaInput(
        tidGate_Clothes,
        tidFluid_Clothes,
        aClothesMax,
        sizeof(Sample.fClothesIn),
        Sample.Offset(Sample.fClothesIn));
      NewMetaInput(
        tidGate_Paper,
        tidFluid_Paper,
        aPaperMax,
        sizeof(Sample.fPaperIn),
        Sample.Offset(Sample.fPaperIn));
      NewMetaInput(
        tidGate_PrintedMaterial,
        tidFluid_PrintedMaterial,
        aPrintedMatMax,
        sizeof(Sample.fPrintedMatIn),
        Sample.Offset(Sample.fPrintedMatIn));

      // Outputs
      NewMetaOutput(
        tidGate_OrganicMat,
        tidFluid_OrganicMat,
        aOrgMatMax,
        sizeof(Sample.fOrganicsOut),
        Sample.Offset(Sample.fOrganicsOut));
      NewMetaOutput(
        tidGate_FabThreads,
        tidFluid_FabThreads,
        aFabricsMax,
        sizeof(Sample.fFabricsOut),
        Sample.Offset(Sample.fFabricsOut));
      NewMetaOutput(
        tidGate_Clothes,
        tidFluid_Clothes,
        aClothesMax,
        sizeof(Sample.fClothesOut),
        Sample.Offset(Sample.fClothesOut));
      NewMetaOutput(
        tidGate_Paper,
        tidFluid_Paper,
        aPaperMax,
        sizeof(Sample.fPaperOut),
        Sample.Offset(Sample.fPaperOut));
      NewMetaOutput(
        tidGate_PrintedMaterial,
        tidFluid_PrintedMaterial,
        aPrintedMatMax,
        sizeof(Sample.fPrintedMatOut),
        Sample.Offset(Sample.fPrintedMatOut));

      // Wares
      RegisterWare(tidGate_OrganicMat, 25, 0, theOverPrice, aOrgMatMax);
      RegisterWare(tidGate_FabThreads, 25, 0, theOverPrice, aFabricsMax);
      RegisterWare(tidGate_Clothes, 25, 0, theOverPrice, aClothesMax);
      RegisterWare(tidGate_Paper, 25, 0, theOverPrice, aPaperMax);
      RegisterWare(tidGate_PrintedMaterial, 25, 0, theOverPrice, aPrintedMatMax);
    end;


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TFabricsStorage);
    end;


end.
