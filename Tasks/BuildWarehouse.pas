unit BuildWarehouse;

interface

  uses
    Tasks, Kernel, Tutorial, CacheAgent, Population, BuildFacilitiesTask;

  const
    tidTask_BuildWarehouse = 'BuildWh';

  const
    whkCompany = 0;
    whkImport  = 1;
    whkExport  = 2;

  type
    TMetaBuildWarehouseTask =
      class(TMetaBuildFacilitiesTask)
        public
          constructor Create(srcTaskId, newId, newSuperTaskId : string); overload;
        private
          fKind        : byte;
          fMetaFluid   : TMetaFluid;
        private
          function  GetSelectInput : string;
          procedure SetSelectInput(value : string);
        public
          property Kind : byte read fKind write fKind;
          property MetaFluid : TMetaFluid read fMetaFluid;
          property SelectInput : string read GetSelectInput write SetSelectInput;
      end;

    TBuildWarehouseTask =
      class(TBuildFacilitiesTask)
        public
          function  Execute : TTaskResult; override;
          procedure StoreToCache(Prefix : string; Cache : TObjectCache); override;
      end;

  procedure RegisterBackup;

implementation

  uses
    MathUtils, FacIds, BackupInterfaces, Warehouses, MetaInstances, CacheCommon,
    ClassStorage;

  constructor TMetaBuildWarehouseTask.Create(srcTaskId, newId, newSuperTaskId : string);
    begin
      inherited Create(srcTaskId, newId, newSuperTaskId);
      fKind := TMetaBuildWarehouseTask(Source).Kind;
    end;

  function TMetaBuildWarehouseTask.GetSelectInput : string;
    begin
      if fMetaFluid <> nil
        then result := fMetaFluid.Name_MLS.Values['0'] // >> MLS
        else result := '';
    end;

  procedure TMetaBuildWarehouseTask.SetSelectInput(value : string);
    begin
      fMetaFluid := TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, value]);
    end;

  function TBuildWarehouseTask.Execute : TTaskResult;
    var
      Input : TPullInput;
    begin
      result := inherited Execute;
      if result = trFinished
        then
          begin
            result := trContinue;
            try
              if (Facs[0] <> nil) and ObjectIs('TWarehouse', Facs[0].CurrBlock)
                then
                  begin
                    case TMetaBuildWarehouseTask(MetaTask).Kind of
                      whkCompany :
                        if TWarehouse(Facs[0].CurrBlock).Role = rolDistributer
                          then result := trFinished;
                      whkImport :
                        if TWarehouse(Facs[0].CurrBlock).Role = rolCompInport
                          then result := trFinished;
                      whkExport :
                        if TWarehouse(Facs[0].CurrBlock).Role = rolCompExport
                          then result := trFinished;
                    end;
                    if (result = trFinished) and (TMetaBuildWarehouseTask(MetaTask).SelectInput <> '')
                      then
                        begin
                          result := trContinue;
                          Input := TPullInput(Facs[0].CurrBlock.InputsByName[TMetaBuildWarehouseTask(MetaTask).MetaFluid.Id]);
                          if (Input <> nil) and Input.Selected
                            then result := trFinished;
                        end;
                  end
                else fEnded := 0; // Build a Warehouse!!!
            except
              // >>
            end;
          end;
    end;

  procedure TBuildWarehouseTask.StoreToCache(Prefix : string; Cache : TObjectCache);
    begin
      inherited;
      Cache.WriteInteger(Prefix + 'WHType', TMetaBuildWarehouseTask(MetaTask).Kind);
      Cache.WriteString(Prefix + 'SelInput', TMetaBuildWarehouseTask(MetaTask).SelectInput);
    end;

  procedure RegisterBackup;
    begin
      RegisterClass(TBuildWarehouseTask);
    end;

end.
