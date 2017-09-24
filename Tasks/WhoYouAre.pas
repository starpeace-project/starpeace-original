unit WhoYouAre;

interface

  uses
    Tasks, InformativeTask, Kernel, Accounts, CacheAgent, BackupInterfaces, MathUtils;

  type
    TMetaWhoYouAreTask =
      class(TMetaTask)
        private
          fAnswer : integer;
        public
          property Answer : integer read fAnswer write fAnswer;
      end;

    TWhoYouAreTask =
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

  // TWhoYouAreTask

  procedure TWhoYouAreTask.StoreToCache(Prefix : string; Cache : TObjectCache);
    begin
      inherited;
      
    end;

  procedure RegisterBackup;
    begin
      RegisterClass(TWhoYouAreTask);
    end;

end.
