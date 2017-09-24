unit CloneAdsTask;

interface

  uses
    Tasks, Kernel, Accounts, CacheAgent, BackupInterfaces, Inventions, FacIds;

  type
    TMetaCloneAdsTask =
      class(TMetaTask)
        private
          fFacId : TFacId;
        public
          property FacId : integer read fFacId write fFacId;
      end;

    TCloneAdsTask =
      class(TAtomicTask)
        public
          Cloned : boolean;
        public
          function Execute : TTaskResult; override;
        private
          procedure MsgFacilityCloned(var Msg : TMsgFacCloned); message msgKernel_FacilityCloned;
      end;

  procedure RegisterBackup;

implementation

  uses
    TaskUtils, ResidentialTasks, ClassStorage;

  // TCloneAdsTask

  procedure TCloneAdsTask.MsgFacilityCloned(var Msg : TMsgFacCloned);
    begin
      if (Msg.Fac <> nil) and (Msg.Fac.MetaFacility.FacId = TMetaCloneAdsTask(MetaTask).FacId)
      then
        Cloned := True;
    end;

  function TCloneAdsTask.Execute : TTaskResult;
    begin
      if Cloned
        then result := trFinished
        else result := trContinue;
    end;

  procedure RegisterBackup;
    begin
      RegisterClass(TCloneAdsTask);
    end;

end.
