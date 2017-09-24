unit UpgradeFacTask;

interface

  uses
    Tasks, Kernel, Accounts, CacheAgent, BackupInterfaces, Inventions, FacIds;

  type
    TMetaUpgradeFacTask =
      class(TMetaTask)
        private
          fFacId : TFacId;
        public
          property FacId : integer read fFacId write fFacId;
      end;

    TUpgradeFacTask =
      class(TAtomicTask)
        public
          Upgraded : boolean;
        public
          function Execute : TTaskResult; override;
        private
          procedure MsgUpgradedFac(var Msg : TMsgFacUpgraded); message msgKernel_FacUpgraded;
      end;

  procedure RegisterBackup;

implementation

  uses
    TaskUtils, ResidentialTasks, ClassStorage;

  // TUpgradeFacTask

  procedure TUpgradeFacTask.MsgUpgradedFac(var Msg : TMsgFacUpgraded);
    begin
      if (Msg.Fac <> nil) and (Msg.Fac.MetaFacility.FacId = TMetaUpgradeFacTask(MetaTask).FacId)
      then
        Upgraded := True;
    end;

  function TUpgradeFacTask.Execute : TTaskResult;
    //var
      //Msg : TBlockOverloadedMsg;
    begin
      {Msg.Msg := msgBlockOverloaded;
      Fac.CurrBlock.Dispatch(Msg);
      if Msg.Result = 1
      then
        result := trFinished
      else
        begin}
          if Upgraded
            then result := trFinished
            else result := trContinue;
        //end;
    end;

  procedure RegisterBackup;
    begin
      RegisterClass(TUpgradeFacTask);
    end;

end.
