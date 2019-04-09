unit MakeProfitTask;

interface

  uses
    Tasks, Kernel, Accounts, CacheAgent, BackupInterfaces, MathUtils, Variants;

  type
    TMetaMakeProfitTask =
      class(TMetaTask)
        private
          fAccountId : TAccountId;
          fGoal      : TMoney;
        public
          property AccountId : TAccountId read fAccountId write fAccountId;
          property Goal      : TMoney     read fGoal      write fGoal;
      end;

    TMakeProfitTask =
      class(TAtomicTask)
        public
          function Execute : TTaskResult; override;
        private
          fProfit     : TMoney;
          fLastProfit : TMoney;
        public
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup (Writer : IBackupWriter); override;
          procedure StoreToCache(Prefix : string; Cache : TObjectCache); override;
      end;

  procedure RegisterBackup;

implementation

  // TMakeProfitTask

  function TMakeProfitTask.Execute : TTaskResult;
    var
      Tycoon  :  TTycoon;
      Account : TAccount;
    begin
      Tycoon  := TTycoon(Context.getContext(tcIdx_Tycoon));
      if Tycoon <> nil
        then Account := Tycoon.Accounts.AccountArray[TMetaMakeProfitTask(MetaTask).AccountId]
        else Account := nil;
      if Account <> nil
        then
          begin
            if Account.Value - fLastProfit > 0
              then fProfit := fProfit + Account.Value - fLastProfit;
            fLastProfit := Account.Value;
            if fProfit > TMetaMakeProfitTask(MetaTask).Goal
              then result := trFinished
              else result := trContinue;
          end
        else result := trFinished
    end;

  procedure TMakeProfitTask.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      fProfit := Reader.ReadCurrency('Profit', 0);
      fLastProfit := Reader.ReadCurrency('LstProfit', 0);
    end;

  procedure TMakeProfitTask.StoreToBackup (Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteCurrency('Profit', fProfit);
      Writer.WriteCurrency('LstProfit', fLastProfit);
    end;

  procedure TMakeProfitTask.StoreToCache(Prefix : string; Cache : TObjectCache);
    var
      Tycoon : TTycoon;
      Account : TAccount;
    begin
      inherited;
      Tycoon  := TTycoon(Context.getContext(tcIdx_Tycoon));
      Cache.WriteString(Prefix + 'Goal', FormatMoney(TMetaMakeProfitTask(MetaTask).fGoal));
      Cache.WriteInteger(Prefix + 'AccountId', TMetaMakeProfitTask(MetaTask).fAccountId);
      if Tycoon <> nil
        then Account := Tycoon.Accounts.AccountArray[TMetaMakeProfitTask(MetaTask).AccountId]
        else Account := nil;
      if Account <> nil
        then Cache.WriteString(Prefix + 'Profit', FormatMoney(Account.Value));
    end;

  procedure RegisterBackup;
    begin
      RegisterClass(TMakeProfitTask);
    end;

end.
