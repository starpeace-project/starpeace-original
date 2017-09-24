unit ManuallyConnectTask;

interface

  uses
    Tasks, Kernel, Accounts, CacheAgent, BackupInterfaces, Inventions, FacIds;

  type
    TMetaManuallyConnectTask =
      class(TMetaTask)
        private
          fToFacId : TFacId;
          fFromFacId : TFacId;
        public
          property ToFacId : integer read fToFacId write fToFacId;
          property FromFacId : integer read fFromFacId write fFromFacId;
      end;

    TManuallyConnectTask =
      class(TAtomicTask)
        public
          Connected : boolean;
        public
          function Execute : TTaskResult; override;
        private
          procedure MsgManuallyConnect(var Msg : TMsgFacsConnect); message msgKernel_SellToAll;
        public  
          procedure StoreToCache(Prefix : string; Cache : TObjectCache); override;
      end;

  procedure RegisterBackup;

implementation

  uses
    TaskUtils, ResidentialTasks, ClassStorage;

  // TManuallyConnectTask

  procedure TManuallyConnectTask.MsgManuallyConnect(var Msg : TMsgFacsConnect);
    begin
      if (Msg.Fac1 <> nil) and (Msg.Fac2 <> nil) and (Msg.Fac1.MetaFacility.FacId = TMetaManuallyConnectTask(MetaTask).ToFacId) and (Msg.Fac2.MetaFacility.FacId = TMetaManuallyConnectTask(MetaTask).FromFacId)
      then
        Connected := True;
    end;

  function TManuallyConnectTask.Execute : TTaskResult;
    begin
      if Connected
        then result := trFinished
        else result := trContinue;
    end;

  procedure TManuallyConnectTask.StoreToCache(Prefix : string; Cache : TObjectCache);
    begin
      inherited;
      Cache.WriteInteger(Prefix + 'IndustryId', TMetaManuallyConnectTask(MetaTask).FromFacId);
    end;

  procedure RegisterBackup;
    begin
      RegisterClass(TManuallyConnectTask);
    end;

end.
