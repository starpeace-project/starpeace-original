
unit Banks;

interface

  uses
    ClassStorageInt, Kernel, WorkCenterBlock, CacheAgent, BackupInterfaces;

  const
    tidCachePath_Banks = 'Banks\';

  type
    TMetaBankBlock =
      class( TMetaWorkCenter )
        public
          constructor Create( anId        : string;
                              aCapacities : array of TFluidValue;
                              aBlockClass : CBlock );
      end;

    TBankBlock =
      class( TFinanciatedWorkCenter )
        protected
          constructor Create( aMetaBlock : TMetaBlock; aFacility : TFacility ); override;
        public
          destructor Destroy; override;
        private
          fBank : TBank;
        public
          procedure AutoConnect( loaded : boolean ); override;
        private
          function  GetBudgetPerc : integer;
          function  GetInterest   : integer;
          function  GetTerm       : integer;
          procedure SetBudgetPerc( Value : integer );
          procedure SetInterest  ( Value : integer );
          procedure SetTerm      ( Value : integer );
        published
          property BudgetPerc : integer read GetBudgetPerc write SetBudgetPerc;
          property Interest   : integer read GetInterest   write SetInterest;
          property Term       : integer read GetTerm       write SetTerm;
        public
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); override;
        published
          function  RDOEstimateLoan( ClientId : integer ) : olevariant;
          function  RDOAskLoan     ( ClientId : integer; Amount : widestring ) : olevariant;
          procedure RDOSetLoanPerc ( Percent  : integer );
        public
          procedure StoreLinksToCache(Cache : TObjectCache); override;
          procedure StoreToCache(Cache : TObjectCache); override;
          procedure FacNameChanged; override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure BlockLoaded; override;
          procedure Deleted;     override;
      end;

  const
    DefBankInterest = 2;
    DefBankTerm     = 30;

  procedure RegisterBackup;

implementation

  uses
    SysUtils, ModelServerCache, MetaInstances, BasicAccounts, MathUtils, Logs;


  // TMetaBankBlock

  constructor TMetaBankBlock.Create( anId : string; aCapacities : array of TFluidValue; aBlockClass : CBlock );
    begin 
      inherited Create( anId, aCapacities, accIdx_None, accIdx_None, accIdx_Bank_Salaries, aBlockClass );
    end;        
    

  // TBankBlock

  constructor TBankBlock.Create( aMetaBlock : TMetaBlock; aFacility : TFacility );
    begin
      inherited;
      fBank := TBank.Create( Facility.Company.Owner );
      fBank.Interest := DefBankInterest;
      fBank.Term     := DefBankTerm;
    end;

  destructor TBankBlock.Destroy;
    begin
      fBank.Free;
      inherited;
    end;

  procedure TBankBlock.AutoConnect( loaded : boolean );
    begin                                             
      inherited;
      if not loaded
        then Facility.Name := 'Bank of ' + Facility.Company.Owner.Name;
      fBank.Name := 'Bank of ' + Facility.Company.Owner.Name;
      fBank.Timer := Facility.Town.Timer;
      fBank.Locator := Facility.Town.WorldLocator;
    end;
    
  function TBankBlock.GetBudgetPerc : integer;
    begin
      result := Facility.Company.Owner.BankLoanPerc;
    end;

  function TBankBlock.GetInterest : integer;
    begin
      result := fBank.Interest;
    end;

  function TBankBlock.GetTerm : integer;
    begin
      result := fBank.Term;
    end;

  procedure TBankBlock.SetBudgetPerc( Value : integer );
    begin
      Facility.Company.Owner.BankLoanPerc := min( 100, Value );
    end;

  procedure TBankBlock.SetInterest( Value : integer );
    begin
      fBank.Interest := min( high(fBank.Interest), Value );
    end;

  procedure TBankBlock.SetTerm( Value : integer );
    begin
      fBank.Term := Value;
    end;

  procedure TBankBlock.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
    begin
      inherited;
      fBank.EndOfPeriod( PeriodType, PeriodCount );
    end;

  function TBankBlock.RDOEstimateLoan( ClientId : integer ) : olevariant;
    var
      LoanCurr : currency;
      Loan : string;
    begin
      Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'EstimateLoan' );
      try
        LoanCurr := fBank.EstimateLoan( TMoneyDealer(ClientId) );
        if LoanCurr < 0
        then
          Loan := FormatMoney(0)
        else
          Loan := FormatMoney(fBank.EstimateLoan( TMoneyDealer(ClientId) ));
        result := Loan;
      except
      end;
    end;

  function TBankBlock.RDOAskLoan( ClientId : integer; Amount : widestring ) : olevariant;
    begin
      Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' - Fac(' + IntToStr(XPos) + ',' + IntToStr(YPos) + ') ' + 'AskLoan' );
      try
        if not Facility.CriticalTrouble and Facility.HasTechnology
          then result := fBank.AskLoan(TMoneyDealer(ClientId), StrToFloat(Amount))
          else result := brqRejected;
      except
        Logs.Log(tidLog_Survival, DateTimeToStr(Now) + ' Error in AskLoan' );
        result := brqRejected;
      end;
    end;

  procedure TBankBlock.RDOSetLoanPerc( Percent : integer );
    begin
      try
        Facility.Company.Owner.BankLoanPerc := Percent;
        ModelServerCache.BackgroundInvalidateCache(Facility.Company.Owner); //CacheObject( Facility.Company.Owner, noKind, noInfo )
      except
      end;
    end;

  procedure TBankBlock.StoreLinksToCache(Cache : TObjectCache);
    begin
      inherited;
      Cache.AddLink(GetGlobalPath(tidCachePath_Banks));
    end;

  procedure TBankBlock.StoreToCache( Cache : TObjectCache );
    var
      i : integer;
    begin
      inherited;
      // Cache.WriteInteger( 'BankBudget', Facility.Company.Owner.BankLoanPerc );
      Cache.WriteInteger( 'LoanCount', fBank.Loans.Count );
      for i := 0 to pred(fBank.Loans.Count) do
        with TLoan(fBank.Loans[i]) do
          if ObjectIs( TTycoon.ClassName, Debtor )
            then
              begin
                Cache.WriteString( 'Debtor' + IntToStr(i), TTycoon(Debtor).Name );
                Cache.WriteInteger( 'Interest' + IntToStr(i), Interest );
                Cache.WriteCurrency( 'Amount' + IntToStr(i), Amount );
                Cache.WriteCurrency( 'Slice' + IntToStr(i), Slice );
                Cache.WriteInteger( 'Term' + IntToStr(i), Term );
              end;
    end;

  procedure TBankBlock.FacNameChanged;
    begin
      fBank.Name := Facility.Name;
    end;

  procedure TBankBlock.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Bank', fBank, nil );
    end;

  procedure TBankBlock.BlockLoaded;
    begin
      inherited;
      fBank.Loaded;
    end;

  procedure TBankBlock.Deleted;
    begin
      inherited;
      Facility.Town.WorldLocator.RedeemBank(fBank);
    end;

  procedure TBankBlock.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObject( 'Bank', fBank );
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin                         
      RegisterClass( TBankBlock );
    end;


end.



