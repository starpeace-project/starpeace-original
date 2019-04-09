unit LoanMoneyTask;

interface

  uses
    Tasks, Kernel, Accounts, CacheAgent, BackupInterfaces, MathUtils;

  type
    TMetaLoanMoneyTask =
      class(TMetaTask)
        private
          fLoan : TMoney;
        public
          property Loan : TMoney read fLoan write fLoan;
      end;

    TLoanMoneyTask =
      class(TAtomicTask)
        protected
          class function GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer; override;
        public
          function Execute : TTaskResult; override;
        public
          procedure StoreToCache(Prefix : string; Cache : TObjectCache); override;
      end;

  procedure RegisterBackup;

implementation

  // TLoanMoneyTask

  class function TLoanMoneyTask.GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer;
    var
      Tycoon : TTycoon;
    begin
      result := inherited GetPriority(MetaTask, SuperTask, Context);
      if result <> tprIgnoreTask
        then
          begin
            Tycoon:= TTycoon(Context.getContext(tcIdx_Tycoon));
            if (Tycoon <> nil) and (Tycoon.LoanAmount >= TMetaLoanMoneyTask(MetaTask).Loan)
              then result := tprAccomplished;
          end;
    end;

  function TLoanMoneyTask.Execute : TTaskResult;
    var
      Tycoon : TTycoon;
    begin
      Tycoon:= TTycoon(Context.getContext(tcIdx_Tycoon));
      if (Tycoon <> nil) and (Tycoon.LoanAmount >= TMetaLoanMoneyTask(MetaTask).Loan)
        then result := trFinished
        else result := trContinue;
    end;

  procedure TLoanMoneyTask.StoreToCache(Prefix : string; Cache : TObjectCache);
    begin
      inherited;
      Cache.WriteString(Prefix + 'Loan', FormatMoney(TMetaLoanMoneyTask(MetaTask).Loan));
    end;

  procedure RegisterBackup;
    begin
      RegisterClass(TLoanMoneyTask);
    end;

end.
