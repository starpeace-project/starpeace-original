unit ResearchTask;

interface

  uses
    Tasks, Kernel, Accounts, CacheAgent, BackupInterfaces, Inventions, FacIds, Variants;

  type
    TMetaResearchTask =
      class(TMetaTask)
        private
          fTechnology  : string;
          fInventionId : TInventionNumId;
          fInvention   : TInvention;
          fStoreId     : TFacId;
        private
          procedure SetTechnology(aTech : string);
        public
          property Technology  : string read fTechnology write SetTechnology;
          property InventionId : TInventionNumId read fInventionId;
          property Invention   : TInvention read fInvention;
          property StoreId     : integer read fStoreId write fStoreId;
      end;

    TResearchTask =
      class(TAtomicTask)
        public
          class function GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer; override;
          function Execute : TTaskResult; override;
          procedure StoreToCache(Prefix : string; Cache : TObjectCache); override;
      end;

  procedure RegisterBackup;

implementation

  uses
    TaskUtils, ResidentialTasks, ClassStorage;

  // TMetaResearchTask

  procedure TMetaResearchTask.SetTechnology(aTech : string);
    begin
      fTechnology := aTech;
      if aTech <> ''
        then
          begin
            fInvention := TInvention(TheClassStorage.ClassById[tidClassFamily_Inventions, aTech]);
            if fInvention <> nil
              then fInventionId := fInvention.NumId;
          end;
    end;

  // TResearchTask

  class function TResearchTask.GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer;
    var
      Company : TCompany;
    begin
      result := inherited GetPriority(MetaTask, SuperTask, Context);
      if result <> tprIgnoreTask
        then
          begin
            Company := TCompany(Context.getContext(tcIdx_Company));
            if (Company <> nil) and Company.HasInvention[TMetaResearchTask(MetaTask).InventionId]
              then result := tprAccomplished;
          end
    end;

  function TResearchTask.Execute : TTaskResult;
    var
      Company : TCompany;
    begin
      Company  := TCompany(Context.getContext(tcIdx_Company));
      if (Company <> nil) and Company.HasInvention[TMetaResearchTask(MetaTask).InventionId]
        then result := trFinished
        else result := trContinue;
    end;

  procedure TResearchTask.StoreToCache(Prefix : string; Cache : TObjectCache);
    begin
      inherited;
      Cache.WriteString (Prefix + 'ResearchId', TMetaResearchTask(MetaTask).Invention.Id);
      Cache.WriteString (Prefix + 'ResearchName', TMetaResearchTask(MetaTask).Invention.Name); // >> MLS
      Cache.WriteString (Prefix + 'PlaceName', TMetaResearchTask(MetaTask).Invention.Resp); // >> MLS
      Cache.WriteString (Prefix + 'ParentName', TMetaResearchTask(MetaTask).Invention.Parent_MLS.Values['0']); // >> MLS
      Cache.WriteInteger(Prefix + 'CommerceId', TMetaResearchTask(MetaTask).StoreId);
    end;

  procedure RegisterBackup;
    begin
      RegisterClass(TResearchTask);
    end;

end.
