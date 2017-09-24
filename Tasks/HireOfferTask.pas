unit HireOfferTask;

interface

  uses
    Tasks, Kernel, Accounts, CacheAgent, BackupInterfaces, Inventions, FacIds;

  type
    TMetaHireOfferTask =
      class(TMetaTask)
        private
          fHire : boolean;
          fGoods : integer;
          fFacId : TFacId;
        public
          property Hire : boolean read fHire write fHire;
          property Goods : integer read fGoods write fGoods;
          property FacId : integer read fFacId write fFacId;
      end;

    THireOfferTask =
      class(TAtomicTask)
        public
          Completed : boolean;
        public
          function Execute : TTaskResult; override;
          procedure StoreToCache(Prefix : string; Cache : TObjectCache); override;
        private
          procedure MsgHireOffer(var Msg : TMsgFacHireOffer); message msgKernel_FacsHireOffer;
      end;

  procedure RegisterBackup;

implementation

  uses
    TaskUtils, ResidentialTasks, ClassStorage;

  // THireOfferTask

  procedure THireOfferTask.MsgHireOffer(var Msg : TMsgFacHireOffer);
    begin
      if (Msg.Fac <> nil) and (Msg.Fac.MetaFacility.FacId = TMetaHireOfferTask(MetaTask).FacId)
      then
        if Msg.Hire = TMetaHireOfferTask(MetaTask).Hire
        then
          Completed := True;
    end;

  function THireOfferTask.Execute : TTaskResult;
    begin
      if Completed
        then result := trFinished
        else result := trContinue;
    end;

  procedure THireOfferTask.StoreToCache(Prefix : string; Cache : TObjectCache);
    begin
      inherited;
      Cache.WriteInteger(Prefix + 'CommerceId', TMetaHireOfferTask(MetaTask).Goods);
    end;

  procedure RegisterBackup;
    begin
      RegisterClass(THireOfferTask);
    end;

end.
