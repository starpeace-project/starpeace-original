unit SellAllTask;

interface

  uses
    Tasks, Kernel, Accounts, CacheAgent, BackupInterfaces, Inventions, FacIds;

  type
    TMetaSellAllTask =
      class(TMetaTask)
        private
          fFacId : TFacId;
          fGoods : integer;
        public
          property FacId : integer read fFacId write fFacId;
          property Goods : integer read fGoods write fGoods;
      end;

    TSellAllTask =
      class(TAtomicTask)
        public
          Sold : boolean;
        public
          function Execute : TTaskResult; override;
        private
          procedure MsgSellToAll(var Msg : TMsgSellToAll); message msgKernel_SellToAll;
        public
          procedure StoreToCache(Prefix : string; Cache : TObjectCache); override;
      end;

  procedure RegisterBackup;

implementation

  uses
    TaskUtils, ResidentialTasks, ClassStorage;

  // TSellAllTask

  procedure TSellAllTask.MsgSellToAll(var Msg : TMsgSellToAll);
    begin
      if (Msg.Fac <> nil) and (Msg.Fac.MetaFacility.FacId = TMetaSellAllTask(MetaTask).FacId) and
      (Msg.FacTypes = 4)
      then
        Sold := True;
    end;

  function TSellAllTask.Execute : TTaskResult;
    begin
      if Sold
        then result := trFinished
        else result := trContinue;
    end;

  procedure TSellAllTask.StoreToCache(Prefix : string; Cache : TObjectCache);
    begin
      inherited;
      Cache.WriteInteger(Prefix + 'CommerceId', TMetaSellAllTask(MetaTask).Goods);
    end;

  procedure RegisterBackup;
    begin
      RegisterClass(TSellAllTask);
    end;

end.
