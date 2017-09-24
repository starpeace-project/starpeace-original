unit ConnectFacilityTask;

interface

  uses
    Tasks, Kernel, Accounts, CacheAgent, BackupInterfaces, Inventions;

  type
    TMetaResearchTask =
      class(TMetaTask)
        private
          fTechnology  : string;
          fInventionId : TInventionNumId;
        private
          procedure SetTechnology(aTech : string);
        public
          property Technology  : string read fTechnology write SetTechnology;
          property InventionId : TInventionNumId read fInventionId;
      end;

    TResearchTask =
      class(TAtomicTask)
        public
          function Execute : TTaskResult; override;
      end;

  procedure RegisterBackup;

implementation

  uses
    FacIds, TaskUtils, ResidentialTasks, ClassStorage;

  // TMetaResearchTask

  procedure TMetaResearchTask.SetTechnology(aTech : string);
    var
      Invention : TInvention;
    begin
      fTechnology := aTech;
      if aTech <> ''
        then
          begin
            Invention := TInvention(TheClassStorage.ClassById[tidClassFamily_Inventions, aTech]);
            if Invention <> nil
              then fInventionId := Invention.NumId;
          end;
    end;

  // TResearchTask

  function TResearchTask.Execute : TTaskResult;
    var
      Company : TCompany;
    begin
      Company  := TCompany(Context.getContext(tcIdx_Company));
      if (Company <> nil) and Company.HasInvention[TMetaResearchTask(MetaTask).InventionId]
        then result := trFinished
        else result := trContinue;
    end;


  procedure RegisterBackup;
    begin
      RegisterClass(TResearchTask);
    end;

end.
