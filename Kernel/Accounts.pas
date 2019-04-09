unit Accounts;

interface

  uses
    MetaInstances, Classes, Collection, Persistent, BackupInterfaces, CacheAgent, Plotter, Languages, Variants;

  const                                            
    tidClassFamily_Accounts = 'Accounts';

  type
    TMoney     = currency;
    TAccountId = integer;

  const
    MaxAccounts = 1000;

  type
    CAccount = class of TAccount;

    TMetaAccount =
      class( TMetaInstance )
        public
          constructor Create( anAccId, aMasterAccId : TAccountId; aName, aDesc : string; anAccountClass : CAccount );
          destructor  Destroy; override;
        private
          fAccId         : TAccountId;
          fAccountClass  : CAccount;
          fMasterAccount : TMetaAccount;
          fSubAccounts   : TStringList;
          fName          : string;
          fDesc          : string;
          fName_MLS      : TMultiString;
          fDesc_MLS      : TMultiString;
          fTaxable       : boolean; 
          fTaxAccount    : boolean;
          fRankeable     : boolean;
        public
          property AccId         : TAccountId   read fAccId;
          property MasterAccount : TMetaAccount read fMasterAccount;
          property SubAccounts   : TStringList  read fSubAccounts;
          //property Name          : string       read fName;
          //property Desc          : string       read fDesc;
          property Name_MLS      : TMultiString read fName_MLS;
          property Desc_MLS      : TMultiString read fDesc_MLS;        
          property Taxable       : boolean      read fTaxable    write fTaxable;
          property TaxAccount    : boolean      read fTaxAccount write fTaxAccount;
          property Rankeable     : boolean      read fRankeable  write fRankeable;
        public
          procedure RetrieveTexts( Container : TDictionary ); override;
          procedure StoreTexts   ( Container : TDictionary ); override;
      end;

    TAccount =
      class( TPersistent )
        protected
          constructor Create( aMetaAccount : TMetaAccount; aSuperAccount : TAccount ); virtual;
        public
          destructor  Destroy; override;
        private
          fMetaAccount  : TMetaAccount;
          fValue        : TMoney;
          fSecValue     : TMoney;
          fSuperAccount : TAccount;
          fSubAccounts  : TCollection;
          fHistory      : TPlotter;
          fModified     : boolean;
        private
          procedure SetValue( aValue : TMoney );
        public
          property MetaAccount  : TMetaAccount read fMetaAccount;
          property Value        : TMoney       read fValue write SetValue;
          property SecValue     : TMoney       read fSecValue write fSecValue;
          property SuperAccount : TAccount     read fSuperAccount;
          property SubAccounts  : TCollection  read fSubAccounts;
          property Modified     : boolean      read fModified;
          property History      : TPlotter     read fHistory;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        protected
          procedure EndOfPeriod; virtual;
          procedure Assign( Account : TAccount ); virtual;
      end;

    TAccountArray     = array[0..MaxAccounts] of TAccount;
    TAccountingSystem =
      class( TPersistent )
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fMasterAccount : TAccount;
          fAccountArray  : TAccountArray;
        public
          property MasterAccount : TAccount      read fMasterAccount;
          property AccountArray  : TAccountArray read fAccountArray;
        public
          procedure StoreToCache( Cache : TObjectCache );
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        private
          procedure InitAccountArray( Account : TAccount );
          function  InstantiateAccount( MetaAccountId : string; SuperAccount : TAccount ) : TAccount;
          function  GetAccount( Id : TAccountId; Accounts : TCollection ) : TAccount;
          procedure CopyAccounts( Account : TAccount; Accounts : TCollection );
          procedure CollectAccounts( Account : TAccount; Collection : TCollection );
        public
          procedure EndOfPeriod( ResetTaxes : boolean ); virtual;
          procedure Reset;
          procedure Check;
      end;

  procedure RegisterBackup;

implementation

  uses
    SysUtils, ClassStorage, BasicAccounts, Logs, Kernel;


  // TMetaAccount

  constructor TMetaAccount.Create( anAccId, aMasterAccId : TAccountId; aName, aDesc : string; anAccountClass : CAccount );
    begin
      inherited Create( IntToStr(anAccId) );
      fAccId        := anAccId;
      fAccountClass := anAccountClass;
      fName         := aName;
      fDesc         := aDesc;
      fSubAccounts  := TStringList.Create;
      if aMasterAccId <> accIdx_None
        then
          begin
            fMasterAccount := TMetaAccount(TheClassStorage.ClassById[tidClassFamily_Accounts, IntToStr(aMasterAccId)]);
            fMasterAccount.fSubAccounts.Add( Id );
          end;
      fName_MLS := TMultiString.Create;
      fDesc_MLS := TMultiString.Create;
    end;

  destructor TMetaAccount.Destroy;
    begin
      fSubAccounts.Free;
      inherited;
    end;

  procedure TMetaAccount.RetrieveTexts( Container : TDictionary );
    begin
      inherited;
      if fName_MLS = nil
        then fName_MLS := TMultiString.Create;
      if fDesc_MLS = nil
        then fDesc_MLS := TMultiString.Create;
      fName_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Name'];
      fDesc_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Desc'];
    end;

  procedure TMetaAccount.StoreTexts( Container : TDictionary );
    begin
      inherited;
      if (fName_MLS = nil) or (fName_MLS.Values[Container.LangId] = '')
        then Container.Values[Family + '.' + Id + '.' + 'Name'] := fName;
      if (fDesc_MLS = nil) or (fDesc_MLS.Values[Container.LangId] = '')
        then Container.Values[Family + '.' + Id + '.' + 'Desc'] := fDesc;
    end;


  // TAccount

  constructor TAccount.Create( aMetaAccount : TMetaAccount; aSuperAccount : TAccount );
    begin
      inherited Create;
      if SuperAccount <> self
        then
          begin
            fMetaAccount  := aMetaAccount;
            fSuperAccount := aSuperAccount;
            fSubAccounts  := TCollection.Create( 0, rkBelonguer );
            fHistory      := TPlotter.Create;
            if fSuperAccount <> nil
              then fSuperAccount.SubAccounts.Insert( self );
          end
        //else raise Exception.Create( 'Invalid SuperAccount for ' + MetaAccount.Name );
    end;

  destructor TAccount.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fSubAccounts.Free;
      inherited;
    end;

  procedure TAccount.SetValue( aValue : TMoney );
    begin
      if SuperAccount <> nil
        then SuperAccount.Value := SuperAccount.Value - fValue + aValue;
      fValue    := aValue;
      fModified := true;
    end;                     
    
  procedure TAccount.LoadFromBackup( Reader : IBackupReader );
    var
      MetaAccountId : string;
    begin
      inherited;
      MetaAccountId := Reader.ReadString( 'MetaAccountId', '' );
      try
        fMetaAccount := TMetaAccount(TheClassStorage.ClassById[tidClassFamily_Accounts, MetaAccountId]);
      except
        fMetaAccount := nil;
      end;
      Reader.ReadObject( 'History', fHistory, nil );
      if fHistory = nil
        then fHistory := TPlotter.Create;
      fModified := Reader.ReadBoolean( 'Modified', false );
      fValue    := Reader.ReadCurrency( 'Value', 0 );
    end;

  procedure TAccount.StoreToBackup( Writer : IBackupWriter );        
    begin
      inherited;
      Writer.WriteString( 'MetaAccountId', fMetaAccount.Id );
      Writer.WriteObject( 'History', fHistory );
      Writer.WriteBoolean( 'Modified', fModified );
      Writer.WriteCurrency( 'Value', fValue );
    end;

  procedure TAccount.EndOfPeriod;
    var
      OldValue : TMoney;
    begin
      OldValue  := fValue;
      fValue    := 0;
      fModified := false;
      fSecValue := 0;
      try
        if fHistory <> nil
          then fHistory.Plot( 0, round(OldValue/1000), round(OldValue/1000) );     
      except
      end;
    end;

  procedure TAccount.Assign( Account : TAccount );
    begin
      fValue := Account.Value;
      fSecValue := Account.SecValue;
      if fHistory <> nil
        then fHistory.Assign( Account.fHistory );
      fModified := Account.Modified;
    end;
    

  // TAccountingSystem

  constructor TAccountingSystem.Create;

    begin
      inherited Create;
      fillchar( fAccountArray, sizeof(fAccountArray), 0 );
      fMasterAccount := InstantiateAccount( IntToStr(accIdx_Master), nil );
      InitAccountArray( fMasterAccount );
    end;

  destructor TAccountingSystem.Destroy;
    begin
      fMasterAccount.Free;
      inherited;
    end;

  procedure TAccountingSystem.StoreToCache( Cache : TObjectCache );
    var
      count : integer;

    procedure StoreAccount( Account : TAccount; level : integer );             
      var
        i : integer;
      begin
        Cache.WriteString( 'AccountId' + IntToStr(count), Account.MetaAccount.Id );
        //Cache.WriteString( 'AccountName' + IntToStr(count), Account.MetaAccount.Name );
        StoreMultiStringToCache( 'AccountName' + IntToStr(count) + '.', Account.MetaAccount.Name_MLS, Cache );
        Cache.WriteCurrency( 'AccountValue' + IntToStr(count), Account.Value );
        Cache.WriteCurrency( 'AccountSecValue' + IntToStr(count), Account.SecValue );
        Cache.WriteInteger( 'AccountLevel' + IntToStr(count), level );
        Cache.WriteBoolean( 'AccountIsTax' + IntToStr(count), Account.MetaAccount.TaxAccount );
        Cache.WriteString( 'AccountHistory' + IntToStr(count), Account.History.Serialize );
        inc( count );
        for i := 0 to pred(Account.SubAccounts.Count) do
          if TAccount(Account.SubAccounts[i]).Modified
            then StoreAccount( TAccount(Account.SubAccounts[i]), succ(level) );
      end;

    begin
      count := 0;
      StoreAccount( fMasterAccount, 0 );
      Cache.WriteInteger( 'AccountCount', count );
    end;

  procedure TAccountingSystem.LoadFromBackup( Reader : IBackupReader );
    var
      Accounts : TCollection;
    begin
      inherited;
      fillchar( fAccountArray, sizeof(fAccountArray), 0 );
      fMasterAccount := InstantiateAccount( IntToStr(accIdx_Master), nil );
      InitAccountArray( fMasterAccount );
      Reader.ReadLooseObject( 'Accounts', Accounts, nil );
      CopyAccounts( fMasterAccount, Accounts );
      Accounts.ExtractAll;
      Accounts.Free;
    end;

  procedure TAccountingSystem.StoreToBackup( Writer : IBackupWriter );
    var
      Accounts : TCollection;
    begin
      inherited;
      //Check; >> ??
      Accounts := TCollection.Create( 0, rkBelonguer );
      try
        CollectAccounts( fMasterAccount, Accounts );
        Writer.WriteLooseObject( 'Accounts', Accounts );
      finally
        Accounts.ExtractAll;
        Accounts.Free;
      end;
    end;

  procedure TAccountingSystem.InitAccountArray( Account : TAccount );
    var
      i : integer;
    begin
      if Account.MetaAccount.AccId < MaxAccounts
        then fAccountArray[Account.MetaAccount.AccId] := Account;
      for i := 0 to pred(Account.SubAccounts.Count) do
        InitAccountArray( TAccount(Account.SubAccounts[i]) );
    end;

  function TAccountingSystem.InstantiateAccount( MetaAccountId : string; SuperAccount : TAccount ) : TAccount;
    var
      MetaAccount : TMetaAccount;
      i           : integer;
    begin
      MetaAccount := TMetaAccount(TheClassStorage.ClassById[tidClassFamily_Accounts, MetaAccountId]);
      result      := MetaAccount.fAccountClass.Create( MetaAccount, SuperAccount );
      for i := 0 to pred(MetaAccount.SubAccounts.Count) do
        InstantiateAccount( MetaAccount.SubAccounts[i], result );
    end;

  function TAccountingSystem.GetAccount( Id : TAccountId; Accounts : TCollection ) : TAccount;
    var
      i : integer;
    begin
      if Accounts <> nil
        then
          begin
            i := 0;
            while (i < Accounts.Count) and (TAccount(Accounts[i]).MetaAccount.AccId <> Id) do
              inc( i );
            if i < Accounts.Count
              then result := TAccount(Accounts[i])
              else result := nil;
          end
        else result := nil;
    end;

  procedure TAccountingSystem.CopyAccounts( Account : TAccount; Accounts : TCollection );
    var
      OldClone : TAccount;
      i        : integer;
    begin
      OldClone := GetAccount( Account.MetaAccount.AccId, Accounts );
      if OldClone <> nil
        then Account.Assign( OldClone );
      for i := 0 to pred(Account.SubAccounts.Count) do
        CopyAccounts( TAccount(Account.SubAccounts[i]), Accounts );   
    end;

  procedure TAccountingSystem.CollectAccounts( Account : TAccount; Collection : TCollection );
    var
      i : integer;
    begin
      try
        if MetaInstances.ObjectIs( TAccount.ClassName, Account )
          then
            begin
              if Account.Value <> 0
                then Collection.Insert( Account );
              for i := 0 to pred(Account.SubAccounts.Count) do
                CollectAccounts( TAccount(Account.SubAccounts[i]), Collection );
            end
          else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Corrupted account skipped!' ); 
      except
        on E : Exception do
          begin
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error collecting account: ' + E.Message );
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' (more) Error collecting account: ' + IntToStr(Account.MetaAccount.AccId) );
            raise;
          end;
      end;
    end;

  procedure TAccountingSystem.EndOfPeriod( ResetTaxes : boolean );

    procedure NotifyEndOfPeriod( Account : TAccount );
      var
        i : integer;
      begin
        if not (Account.MetaAccount.TaxAccount xor ResetTaxes)
          then Account.EndOfPeriod;
        for i := 0 to pred(Account.SubAccounts.Count) do
          NotifyEndOfPeriod( TAccount(Account.SubAccounts[i]) );
      end;

    begin
      NotifyEndOfPeriod( fMasterAccount );
    end;

  procedure TAccountingSystem.Reset;

    procedure ResetAccount( Account : TAccount );
      var
        i : integer;
      begin
        Account.Value := 0;
        for i := 0 to pred(Account.SubAccounts.Count) do
          ResetAccount( TAccount(Account.SubAccounts[i]) );
      end;

    procedure ResetAccountArray;
      var
        i   : integer;
        Acc : TAccount;
      begin
        for i := low(fAccountArray) to high(fAccountArray) do
          begin
            Acc := TAccount(fAccountArray[i]);
            if Acc <> nil
              then Acc.EndOfPeriod;
          end;
      end;

    begin
      ResetAccount( MasterAccount );
      ResetAccountArray;
    end;

  procedure TAccountingSystem.Check;
    var
      Accounts : TCollection;
    begin
      Accounts := TCollection.Create( 0, rkUse );
      try
        CollectAccounts( fMasterAccount, Accounts );
        fillchar( fAccountArray, sizeof(fAccountArray), 0 );
        fMasterAccount := InstantiateAccount( IntToStr(accIdx_Master), nil );
        InitAccountArray( fMasterAccount );
        CopyAccounts( fMasterAccount, Accounts );
      finally
        Accounts.Free;
      end;
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TAccount );
      RegisterClass( TAccountingSystem );
    end;

end.




