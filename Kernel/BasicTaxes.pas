unit BasicTaxes;

interface

  uses
    Taxes, Kernel, Accounts, CacheAgent, BackupInterfaces, Languages, Variants;

  const
    taxKind_Percent      = 0;
    taxKind_ValuePerUnit = 1;

  type
    TMetaDealerTax =
      class( TMetaTax )
        public
          constructor Create( anId, aName : string; aKind : TTaxKind; aTaxAccountId : TAccountId; aTaxClass : CTax );
        private
          fTaxAccountId : TAccountId;
        public
          property TaxAccountId : TAccountId read fTaxAccountId;
      end;

    TDealerTax =
      class( TTax )
        public
          procedure Evaluate( Amount : TMoney; TaxPayer, TaxCollector : TObject ); override;
        protected
          procedure CollectTaxesTo( Amount : TMoney; TaxPayer, TaxCollector : TMoneyDealer ); virtual; abstract;
      end;

    TMetaTaxToAccount =
      class( TMetaDealerTax )
        public
          constructor Create( anId : string; aAccountId : TAccountId; aTaxClass : CTax );
        private
          fAccountId : TAccountId;
          fPercent   : single;
        public
          property AccountId : TAccountId read fAccountId;
          property Percent   : single     read fPercent;
        public
          function Instantiate : TTax; override;
      end;

    TTaxToAccount =
      class( TDealerTax )
        private
          fPercent : single;
        public
          property Percent : single read fPercent write fPercent;
        protected
          procedure CollectTaxesTo( Amount : TMoney; TaxPayer, TaxCollector : TMoneyDealer ); override;
        public
          function  HasDefaultValue : boolean; override;
          procedure StoreToCache( Prefix : string; Cache : TObjectCache ); override;
        protected
          function  GetSubsidized : boolean; override;
          procedure SetSubsidized( value : boolean ); override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure ParseValue( value : string ); override;
      end;

    TMetaFixedTax =
      class( TMetaDealerTax )
        private
          fPricePerUnit : TMoney;
        public
          property PricePerUnit : TMoney read fPricePerUnit write fPricePerUnit;
        public
          function Instantiate : TTax; override;
      end;

    TFixedTax =
      class( TDealerTax )
        private
          fPricePerUnit : TMoney;
        public
          property PricePerUnit : TMoney read fPricePerUnit write fPricePerUnit;
        public
          function  HasDefaultValue : boolean; override;
          procedure StoreToCache( Prefix : string; Cache : TObjectCache ); override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure ParseValue( value : string ); override;
      end;
                                           
    TTaxToLand =
      class( TFixedTax )
        protected
          procedure CollectTaxesTo( Amount : TMoney; TaxPayer, TaxCollector : TMoneyDealer ); override;
      end;

  const
    accIdx_Taxes = 10;
                                                               
  procedure RegisterBackup;

implementation

  uses
    MetaInstances, ClassStorage, SysUtils, MathUtils;

  type
    TMetaTaxAccount =
      class( TMetaAccount )
        private
          fTax : TMetaTaxToAccount;
        public
          procedure RetrieveTexts( Container : TDictionary ); override;
          procedure StoreTexts   ( Container : TDictionary ); override;
          procedure EvaluateTexts; override;
      end;

    procedure TMetaTaxAccount.RetrieveTexts( Container : TDictionary );
      begin
      end;

    procedure TMetaTaxAccount.StoreTexts( Container : TDictionary );
      begin
      end;

    procedure TMetaTaxAccount.EvaluateTexts;
      var
        i : integer;
      begin
        inherited;
        for i := 0 to pred(LangList.Count) do
          Name_MLS.Values[LangList[i]] := fTax.Name_MLS.Values[LangList[i]]; 
      end;

  // TMetaDealerTax

  constructor TMetaDealerTax.Create( anId, aName : string; aKind : TTaxKind; aTaxAccountId : TAccountId; aTaxClass : CTax );
    begin
      inherited Create( anId, aName, aKind, aTaxClass );
      fTaxAccountId := aTaxAccountId;
    end;


  // TDealerTax

  procedure TDealerTax.Evaluate( Amount : TMoney; TaxPayer, TaxCollector : TObject );
    begin
      if ObjectIs( TMoneyDealer.ClassName, TaxPayer ) and ObjectIs( TMoneyDealer.ClassName, TaxCollector )
        then CollectTaxesTo( Amount, TMoneyDealer(TaxPayer), TMoneyDealer(TaxCollector) );
    end;


  // TMetaTaxToAccount

  constructor TMetaTaxToAccount.Create( anId : string; aAccountId : TAccountId; aTaxClass : CTax );

    const
      TaxesAccounIdStart = 300;

    function GetNewAccountId : TAccountId;
      begin
        result := TaxesAccounIdStart;
        while TheClassStorage.ClassById[tidClassFamily_Accounts, IntToStr(result)] <> nil do
          inc( result );
      end;

    var
      AccId : TAccountId;
      MA    : TMetaAccount;
      i     : integer;
    begin
      AccId := GetNewAccountId;
      inherited Create( anId, '', taxKind_Percent, AccId, aTaxClass );
      fAccountId := aAccountId;
      fPercent   := 0.1;
      MA := TMetaAccount(TheClassStorage.ClassById[tidClassFamily_Accounts, IntToStr(fAccountId)]);
      for i := 0 to pred(LangList.Count) do
        Name_MLS.Values[LangList[i]] := MA.Name_MLS.Values[LangList[i]];
      with TMetaTaxAccount.Create( AccId, accIdx_Taxes, 'TAX', '', TAccount ) do
        begin
          fTax := self;
          TaxAccount := true;
          Register( tidClassFamily_Accounts );
        end;
    end;

  function TMetaTaxToAccount.Instantiate : TTax;
    begin
      result := inherited Instantiate;
      TTaxToAccount(result).fPercent := fPercent;
    end;


  // TTaxToAccount

  procedure TTaxToAccount.CollectTaxesTo( Amount : TMoney; TaxPayer, TaxCollector : TMoneyDealer );
    var
      ActualAmount : TMoney;       
    begin
      if fPercent > 0
        then ActualAmount := fPercent*realmax( 0, Amount )
        else // it is a subsidy
          if Amount > 0
            then ActualAmount := 0
            else ActualAmount := Amount;
      TaxPayer.GenMoney( -ActualAmount, TMetaDealerTax(MetaTax).TaxAccountId );
      TaxCollector.GenMoney( ActualAmount, TMetaDealerTax(MetaTax).TaxAccountId );
      Revenue := Revenue + ActualAmount;
    end;

  function TTaxToAccount.HasDefaultValue : boolean;
    begin
      result := TMetaTaxToAccount(MetaTax).Percent = Percent;
    end;

  procedure TTaxToAccount.StoreToCache( Prefix : string; Cache : TObjectCache );
    begin
      inherited;
      Cache.WriteInteger( Prefix + 'Percent', round(100*Percent) );
    end;

  procedure TTaxToAccount.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fPercent := Reader.ReadSingle( 'Percent', TMetaTaxToAccount(MetaTax).fPercent );
    end;

  procedure TTaxToAccount.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteSingle( 'Percent', Percent );
    end;

  function TTaxToAccount.GetSubsidized : boolean;
    begin
      result := fPercent < 0;
    end;

  procedure TTaxToAccount.SetSubsidized( value : boolean );
    begin
      if value
        then fPercent := -1
        else fPercent := 0.1;
    end;

  procedure TTaxToAccount.ParseValue( value : string );
    begin
      fPercent := StrToInt(value)/100;
    end;
    

  // TMetaFixedTax

  function TMetaFixedTax.Instantiate : TTax;
    begin
      result := inherited Instantiate;
      TFixedTax(result).fPricePerUnit := fPricePerUnit;
    end;


  // TFixedTax

  function TFixedTax.HasDefaultValue : boolean;
    begin
      result := fPricePerUnit = TMetaFixedTax(MetaTax).fPricePerUnit;
    end;

  procedure TFixedTax.StoreToCache( Prefix : string; Cache : TObjectCache );
    begin
      inherited;
      Cache.WriteCurrency( Prefix + 'PricePerUnit', fPricePerUnit );
    end;

  procedure TFixedTax.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fPricePerUnit := Reader.ReadCurrency( 'PricePerUnit', TMetaFixedTax(MetaTax).fPricePerUnit );
    end;

  procedure TFixedTax.StoreToBackup ( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteCurrency( 'PricePerUnit', fPricePerUnit );
    end;

  procedure TFixedTax.ParseValue( value : string );
    begin
      fPricePerUnit := StrToCurr(value);
    end;


  // TTaxToLand

  procedure TTaxToLand.CollectTaxesTo( Amount : TMoney; TaxPayer, TaxCollector : TMoneyDealer );
    begin
      inherited;
      Revenue := Revenue + Amount;
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TTaxToAccount );
      RegisterClass( TTaxToLand );
    end;
    

end.


