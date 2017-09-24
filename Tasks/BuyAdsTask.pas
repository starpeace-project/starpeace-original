unit BuyAdsTask;

interface

  uses
    Tasks, Kernel, Accounts, CacheAgent, BackupInterfaces, MathUtils, BasicAccounts;

  type
    TMetaBuyAdsTask =
      class(TMetaTask)
        private
          // >>
        public
          // >>
      end;

    TBuyAdsTask =
      class(TAtomicTask)
        public
          function Execute : TTaskResult; override;
        private
          // >>
        public
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup (Writer : IBackupWriter); override;
      end;

  procedure RegisterBackup;

implementation

  // TBuyAdsTask

  function TBuyAdsTask.Execute : TTaskResult;
    var
      Tycoon  :  TTycoon;
      Account : TAccount;
    begin
      Tycoon  := TTycoon(Context.getContext(tcIdx_Tycoon));
      if Tycoon <> nil
        then Account := Tycoon.Accounts.AccountArray[accIdx_ResearchCenter_Supplies]
        else Account := nil;
      if Account <> nil
        then
          begin
            if Account.Value < 0
              then result := trFinished
              else result := trContinue;
          end
        else result := trFinished
    end;

  procedure TBuyAdsTask.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
    end;

  procedure TBuyAdsTask.StoreToBackup (Writer : IBackupWriter);
    begin
      inherited;
    end;

  procedure RegisterBackup;
    begin
      RegisterClass(TBuyAdsTask);
    end;

end.
