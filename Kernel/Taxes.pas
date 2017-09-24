 unit Taxes;

interface

  uses
    MetaInstances, Accounts, Persistent, CacheAgent, BackupInterfaces, Languages;

  const
    tidClassFamily_Taxes = 'Taxes';

  type
    TTaxTarget = (tarTycoon, tarPopulation);
    TTaxKind   = integer;

  type
    TMetaTax = class;                               
    TTax     = class;
    CTax     = class of TTax;

    TMetaTax =
      class( TMetaInstance )
        public
          constructor Create( anId, aName : string; aKind : TTaxKind; aTaxClass : CTax );
        private
          fName     : string;
          fName_MLS : TMultiString;
          fTaxClass : CTax;
          fTarget   : TTaxTarget;
          fKind     : TTaxKind;
        public
          property Name     : string       read fName;
          property Name_MLS : TMultiString read fName_MLS;
          property Target   : TTaxTarget   read fTarget write fTarget;
          property Kind     : TTaxKind     read fKind;
        public
          function Instantiate : TTax; virtual;
      end;

    TTax =
      class( TPersistent )
        protected
          constructor Create( aMetaTax : TMetaTax ); virtual;
        private
          fMetaTax : TMetaTax;
          fRevenue : TMoney;
        protected
          function  GetSubsidized : boolean; virtual;
          procedure SetSubsidized( value : boolean ); virtual;
        public
          property MetaTax    : TMetaTax read fMetaTax;
          property Subsidized : boolean  read GetSubsidized write SetSubsidized;
        protected
          property Revenue : TMoney read fRevenue write fRevenue;
        public
          procedure Evaluate( Amount : TMoney; TaxPayer, TaxCollector : TObject ); virtual; abstract;
          function  HasDefaultValue : boolean;                             virtual;
          procedure StoreToCache( Prefix : string; Cache : TObjectCache ); virtual;
          procedure Reset;                                                 virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure ParseValue( value : string ); virtual;
      end;
                                    
                                                     
implementation

  uses
    ClassStorage;

  // TMetaTax

  constructor TMetaTax.Create( anId, aName : string; aKind : TTaxKind; aTaxClass : CTax );
    begin
      inherited Create( anId );
      fName     := aName;
      fTaxClass := aTaxClass;
      fKind     := aKind;
      fName_MLS := TMultiString.Create;
    end;

  function TMetaTax.Instantiate : TTax;
    begin
      result := fTaxClass.Create( self );
    end;


  // TTax

  constructor TTax.Create( aMetaTax : TMetaTax );
    begin
      inherited Create;
      fMetaTax := aMetaTax;
    end;

  function TTax.GetSubsidized : boolean;
    begin
      result := false;
    end;

  procedure TTax.SetSubsidized( value : boolean );
    begin
    end;

  function TTax.HasDefaultValue : boolean;
    begin
      result := false;
    end;

  procedure TTax.StoreToCache( Prefix : string; Cache : TObjectCache );
    begin
      Cache.WriteString( Prefix + 'Id', MetaTax.Id );
      //Cache.WriteString( Prefix + 'Name', MetaTax.Name );
      StoreMultiStringToCache( Prefix + 'Name', MetaTax.Name_MLS, Cache );
      Cache.WriteInteger( Prefix + 'Kind', MetaTax.Kind );
      Cache.WriteCurrency( Prefix + 'LastYear', Revenue );
    end;

  procedure TTax.Reset;
    begin
      Revenue := 0;
    end;

  procedure TTax.LoadFromBackup( Reader : IBackupReader );
    var
      Id : string;
    begin
      inherited;
      Id := Reader.ReadString( 'MetaTaxId', '' );
      fMetaTax := TMetaTax(TheClassStorage.ClassById[tidClassFamily_Taxes, Id]);
      fRevenue := Reader.ReadCurrency( 'Revenue', 0 );
    end;

  procedure TTax.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'MetaTaxId', fMetaTax.Id );
      Writer.WriteCurrency( 'Revenue', fRevenue );
    end;

  procedure TTax.ParseValue( value : string );
    begin
    end;

end.


