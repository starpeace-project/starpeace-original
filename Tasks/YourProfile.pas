unit YourProfile;

interface

  uses
    Tasks, InformativeTask, Kernel, Accounts, CacheAgent, BackupInterfaces, MathUtils;

  type
    TMetaYourProfile =
      class(TMetaTask)
        private
          fAnswer : integer;
        public
          property Answer : integer read fAnswer write fAnswer;
      end;

    TYourProfileTask =
      class(TInformativeTask)
        {public
          function Execute : TTaskResult; override;
        private
          fProfit     : TMoney;
          fLastProfit : TMoney;}
        public
          procedure StoreToCache(Prefix : string; Cache : TObjectCache); override;
      end;

    procedure RegisterBackup;

implementation

  // TYourProfileTask

  procedure TYourProfileTask.StoreToCache(Prefix : string; Cache : TObjectCache);
    begin
      inherited;
      
    end;

  procedure RegisterBackup;
    begin
      RegisterClass(TYourProfileTask);
    end;

end.
