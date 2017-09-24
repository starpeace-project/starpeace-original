unit GeneralStorage;

interface

  uses
    Warehouses, Kernel, Surfaces, WorkCenterBlock, BackupInterfaces, CacheAgent;

  type
    TMetaGeneralStorage =
      class(TMetaWarehouse)
        public
          constructor Create(anId          : string;
                             aCapacities   : array of TFluidValue;
                             aOrgMatMax    : TFluidValue;
                             aFabThrdsMax  : TFluidValue;
                             aClothesMax   : TFluidValue;
                             aElectCompMax : TFluidValue;
                             aHHAMax       : TFluidValue;
                             aBusMachMax   : TFluidValue;
                             aMetalsMax    : TFluidValue;
                             aCarsMax      : TFluidValue;
                             aToysMax      : TFluidValue;
                             aPlasticsMax  : TFluidValue;
                             aDrugsMax     : TFluidValue;
                             aMachMax      : TFluidValue;
                             aFurnitureMax : TFluidValue;
                             aBookMax      : TFluidValue;
                             aCDMax        : TFluidValue;
                             theOverPrice  : TPercent;
                             aBlockClass   : CBlock);
      end;

    TGeneralStorage =
      class(TWarehouse)
        private
          fClothesIn    : TInputData;
          fElectCompIn  : TInputData;
          fBusMachIn    : TInputData;
          fHHAIn        : TInputData;
          fOrgMatIn     : TInputData;
          fFabThrdsIn   : TInputData;
          fMetalsIn     : TInputData;
          fCarsIn       : TInputData;
          fToysIn       : TInputData;
          fPlasticsIn   : TInputData;
          fDrugsIn      : TInputData;
          fMachIn       : TInputData;
          fFurnitureIn  : TInputData;
          fBooksIn      : TInputData;
          fCDsIn        : TInputData;
          fClothesOut   : TOutputData;
          fElectCompOut : TOutputData;
          fBusMachOut   : TOutputData;
          fHHAOut       : TOutputData;
          fOrgMatOut    : TOutputData;
          fFabThrdsOut  : TOutputData;
          fMetalsOut    : TOutputData;
          fCarsOut      : TOutputData;
          fToysOut      : TOutputData;
          fPlasticsOut  : TOutputData;
          fDrugsOut     : TOutputData;
          fMachOut      : TOutputData;
          fFurnitureOut : TOutputData;
          fBooksOut     : TOutputData;
          fCDsOut       : TOutputData;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, StdFluids;

  // TMetaGeneralStorage

  constructor TMetaGeneralStorage.Create(anId          : string;
                                         aCapacities   : array of TFluidValue;
                                         aOrgMatMax    : TFluidValue;
                                         aFabThrdsMax  : TFluidValue;
                                         aClothesMax   : TFluidValue;
                                         aElectCompMax : TFluidValue;
                                         aHHAMax       : TFluidValue;
                                         aBusMachMax   : TFluidValue;
                                         aMetalsMax    : TFluidValue;
                                         aCarsMax      : TFluidValue;
                                         aToysMax      : TFluidValue;
                                         aPlasticsMax  : TFluidValue;
                                         aDrugsMax     : TFluidValue;
                                         aMachMax      : TFluidValue;
                                         aFurnitureMax : TFluidValue;
                                         aBookMax      : TFluidValue;
                                         aCDMax        : TFluidValue;
                                         theOverPrice  : TPercent;
                                         aBlockClass   : CBlock);
    var
      Sample : TGeneralStorage;
    begin
      inherited Create(anId, aCapacities, aBlockClass);
      Sample := nil;
      // Inputs
      NewMetaInput(
        tidGate_OrganicMat,
        tidFluid_OrganicMat,
        aOrgMatMax,
        sizeof(Sample.fOrgMatIn),
        Sample.Offset(Sample.fOrgMatIn));
      NewMetaInput(
        tidGate_FabThreads,
        tidFluid_FabThreads,
        aFabThrdsMax,
        sizeof(Sample.fFabThrdsIn),
        Sample.Offset(Sample.fFabThrdsIn));
      NewMetaInput(
        tidGate_Clothes,
        tidFluid_Clothes,
        aClothesMax,
        sizeof(Sample.fClothesIn),
        Sample.Offset(Sample.fClothesIn));
      NewMetaInput(
        tidGate_ElectComp,
        tidFluid_ElectComp,
        aElectCompMax,
        sizeof(Sample.fElectCompIn),
        Sample.Offset(Sample.fElectCompIn));
      NewMetaInput(
        tidGate_HouseHoldingAppliances,
        tidFluid_HouseHoldingAppliances,
        aHHAMax,
        sizeof(Sample.fHHAIn),
        Sample.Offset(Sample.fHHAIn));
      NewMetaInput(
        tidGate_BusinessMachines,
        tidFluid_BusinessMachines,
        aBusMachMax,
        sizeof(Sample.fBusMachIn),
        Sample.Offset(Sample.fBusMachIn));
      NewMetaInput(
        tidGate_Metals,
        tidFluid_Metals,
        aMetalsMax,
        sizeof(Sample.fMetalsIn),
        Sample.Offset(Sample.fMetalsIn));
      NewMetaInput(
        tidGate_Cars,
        tidFluid_Cars,
        aCarsMax,
        sizeof(Sample.fCarsIn),
        Sample.Offset(Sample.fCarsIn));
      NewMetaInput(
        tidGate_Drugs,
        tidFluid_Drugs,
        aDrugsMax,
        sizeof(Sample.fDrugsIn),
        Sample.Offset(Sample.fDrugsIn));
      NewMetaInput(
        tidGate_Toys,
        tidFluid_Toys,
        aToysMax,
        sizeof(Sample.fToysIn),
        Sample.Offset(Sample.fToysIn));
      NewMetaInput(
        tidGate_Plastics,
        tidFluid_Plastics,
        aPlasticsMax,
        sizeof(Sample.fPlasticsIn),
        Sample.Offset(Sample.fPlasticsIn));
      NewMetaInput(
        tidGate_Machinery,
        tidFluid_Machinery,
        aMachMax,
        sizeof(Sample.fMachIn),
        Sample.Offset(Sample.fMachIn));
      NewMetaInput(
        tidGate_Furniture,
        tidFluid_Furniture,
        aFurnitureMax,
        sizeof(Sample.fFurnitureIn),
        Sample.Offset(Sample.fFurnitureIn));
      NewMetaInput(
        tidGate_Books,
        tidFluid_Books,
        aBookMax,
        sizeof(Sample.fBooksIn),
        Sample.Offset(Sample.fBooksIn));
      NewMetaInput(
        tidGate_CDs,
        tidFluid_CDs,
        aCDMax,
        sizeof(Sample.fCdsIn),
        Sample.Offset(Sample.fCdsIn));

      // Outputs
      NewMetaOutput(
        tidGate_OrganicMat,
        tidFluid_OrganicMat,
        aOrgMatMax,
        sizeof(Sample.fOrgMatOut),
        Sample.Offset(Sample.fOrgMatOut));
      NewMetaOutput(
        tidGate_FabThreads,
        tidFluid_FabThreads,
        aFabThrdsMax,
        sizeof(Sample.fFabThrdsOut),
        Sample.Offset(Sample.fFabThrdsOut));
      NewMetaOutput(
        tidGate_Clothes,
        tidFluid_Clothes,
        aClothesMax,
        sizeof(Sample.fClothesOut),
        Sample.Offset(Sample.fClothesOut));
      NewMetaOutput(
        tidGate_ElectComp,
        tidFluid_ElectComp,
        aElectCompMax,
        sizeof(Sample.fElectCompOut),
        Sample.Offset(Sample.fElectCompOut));
      NewMetaOutput(
        tidGate_HouseHoldingAppliances,
        tidFluid_HouseHoldingAppliances,
        aHHAMax,
        sizeof(Sample.fHHAOut),
        Sample.Offset(Sample.fHHAOut));
      NewMetaOutput(
        tidGate_BusinessMachines,
        tidFluid_BusinessMachines,
        aBusMachMax,
        sizeof(Sample.fBusMachOut),
        Sample.Offset(Sample.fBusMachOut));
      NewMetaOutput(
        tidGate_Metals,
        tidFluid_Metals,
        aMetalsMax,
        sizeof(Sample.fMetalsOut),
        Sample.Offset(Sample.fMetalsOut));
      NewMetaOutput(
        tidGate_Cars,
        tidFluid_Cars,
        aCarsMax,
        sizeof(Sample.fCarsOut),
        Sample.Offset(Sample.fCarsOut));
      NewMetaOutput(
        tidGate_Drugs,
        tidFluid_Drugs,
        aDrugsMax,
        sizeof(Sample.fDrugsOut),
        Sample.Offset(Sample.fDrugsOut));
      NewMetaOutput(
        tidGate_Toys,
        tidFluid_Toys,
        aToysMax,
        sizeof(Sample.fToysOut),
        Sample.Offset(Sample.fToysOut));
      NewMetaOutput(
        tidGate_Plastics,
        tidFluid_Plastics,
        aPlasticsMax,
        sizeof(Sample.fPlasticsOut),
        Sample.Offset(Sample.fPlasticsOut));
      NewMetaOutput(
        tidGate_Machinery,
        tidFluid_Machinery,
        aMachMax,
        sizeof(Sample.fMachOut),
        Sample.Offset(Sample.fMachOut));
      NewMetaOutput(
        tidGate_Furniture,
        tidFluid_Furniture,
        aFurnitureMax,
        sizeof(Sample.fFurnitureOut),
        Sample.Offset(Sample.fFurnitureOut));
      NewMetaOutput(
        tidGate_Books,
        tidFluid_Books,
        aBookMax,
        sizeof(Sample.fBooksOut),
        Sample.Offset(Sample.fBooksOut));
      NewMetaOutput(
        tidGate_CDs,
        tidFluid_CDs,
        aCDMax,
        sizeof(Sample.fCDsOut),
        Sample.Offset(Sample.fCDsOut));

      // Wares
      RegisterWare(tidGate_OrganicMat, 13, 0, theOverPrice, aOrgMatMax);
      RegisterWare(tidGate_FabThreads, 12, 0, theOverPrice, aFabThrdsMax);
      RegisterWare(tidGate_Clothes, 13, 0, theOverPrice, aClothesMax);
      RegisterWare(tidGate_ElectComp, 12, 0, theOverPrice, aElectCompMax);
      RegisterWare(tidGate_HouseHoldingAppliances, 13, 0, theOverPrice, 1000);
      RegisterWare(tidGate_BusinessMachines, 12, 0, theOverPrice, 1000);
      RegisterWare(tidGate_Metals, 12, 0, theOverPrice, 1000);
      RegisterWare(tidGate_Cars, 13, 0, theOverPrice, 1000);
      RegisterWare(tidGate_Toys, 13, 0, theOverPrice, 1000);
      RegisterWare(tidGate_Drugs, 13, 0, theOverPrice, 1000);
      RegisterWare(tidGate_Plastics, 13, 0, theOverPrice, 1000);
      RegisterWare(tidGate_Machinery, 10, 0, theOverPrice, 1000);
      RegisterWare(tidGate_Furniture, 10, 0, theOverPrice, 1000);
      RegisterWare(tidGate_Books, 10, 0, theOverPrice, 1000);
      RegisterWare(tidGate_CDs, 10, 0, theOverPrice, 1000);
    end;


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TGeneralStorage);
    end;


end.

