unit MegaWarehouse;

interface

  uses
    Warehouses, Kernel, Surfaces, WorkCenterBlock, BackupInterfaces, CacheAgent,
    Protocol;

  type
    TMetaMegaStorage =
      class(TMetaWarehouse)
        public
          constructor Create(anId         : string;
                             aCapacities  : array of TFluidValue;
                             theOverPrice : TPercent;
                             aBlockClass  : CBlock);
      end;

    TMegaStorage =
      class(TWarehouse)
        private
          fWareInputs  : array[0..31] of TInputData;
          fWareOutputs : array[0..31] of TOutputData;
        published
          procedure RDOSelectWare(index : integer; value : wordbool);
        protected
          function  GetVisualClassId : TVisualClassId; override;
      end;

  procedure RegisterBackup;

implementation

  uses
    ClassStorage, StdFluids, Collection, ModelServerCache;

  // TMetaMegaStorage

  constructor TMetaMegaStorage.Create(anId         : string;
                                      aCapacities  : array of TFluidValue;
                                      theOverPrice : TPercent;
                                      aBlockClass  : CBlock);
    var
      Sample : TMegaStorage;
      i      : integer;
      count  : integer;
      Fluid  : TMetaFluid;
      Fluids : TCollection;
    begin
      inherited Create(anId, aCapacities, aBlockClass);
      Sample := nil;
      count  := TheClassStorage.ClassCount[tidClassFamily_Fluids];
      Fluids := TCollection.Create(count, rkUse);
      // Collect Fluids
      for i := 0 to pred(count) do
        begin
          Fluid := TMetaFluid(TheClassStorage.ClassByIdx[tidClassFamily_Fluids, i]);
          if Fluid.StorageVol > 0
            then Fluids.Insert(Fluid);
        end;
      // Register Inputs
      for i := 0 to pred(Fluids.Count) do
        begin
          Fluid := TMetaFluid(Fluids[i]);
          NewMetaInput(
            Fluid.Id,
            Fluid.Id,
            Fluid.StorageVol,
            sizeof(Sample.fWareInputs[i]),
            Sample.Offset(Sample.fWareInputs[i]));
        end;
      // Register Outputs
      for i := 0 to pred(Fluids.Count) do
        begin
          Fluid := TMetaFluid(Fluids[i]);
          NewMetaOutput(
            Fluid.Id,
            Fluid.Id,
            Fluid.StorageVol,
            sizeof(Sample.fWareOutputs[i]),
            Sample.Offset(Sample.fWareOutputs[i]));
        end;
      // Register Wares
      for i := 0 to pred(Fluids.Count) do
        begin
          Fluid := TMetaFluid(Fluids[i]);
          RegisterWare(Fluid.Id, round(100/Fluids.Count), 0, theOverPrice, Fluid.StorageVol);
        end;
    end;


  // TMegaStorage

  procedure TMegaStorage.RDOSelectWare(index : integer; value : wordbool);
    var
      Input : TPullInput;
    begin
      if Facility.CheckOpAuthenticity
        then
          begin
            Input := TPullInput(Inputs[TMetaWarehouse(MetaBlock).Wares[index].MetaInput.Index]);
            Input.Selected := value;
            ModelServerCache.InvalidateCache(Facility, false);
            Facility.Town.MapRefresh.RefeshFacility(Facility, fchStructure);
          end;
    end;

  function TMegaStorage.GetVisualClassId : TVisualClassId;
    begin
      result := 0;
    end;


  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TMegaStorage);
    end;

end.
