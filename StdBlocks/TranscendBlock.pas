unit TranscendBlock;

interface

  uses
    Collection, Kernel, Environmental, BackupInterfaces, Accounts, Surfaces, Protocol, CacheAgent, Variants;

  type
    TMetaTranscendBlock =
      class( TMetaEnvironmentalBlock )
        public
          constructor Create( anId : string; aLevelReq : string; aTime : integer; aBlockClass : CBlock );
        private
          fLevelReq : string;
          fTime     : integer;
        public
          property LevelReq : string  read fLevelReq;
          property Time     : integer read fTime;
        public
          procedure StoreExtraInfoToCache( Cache : TObjectCache ); override;
        protected
          procedure ModifyMetaFacility( MetaFacility : TMetaFacility ); override;
        public
          function Transcends(Facility : TFacility) : boolean; override;
      end;

    TTranscendBlock =
      class( TEnvironmentalBlock )
        public
          function Evaluate : TEvaluationResult; override;
        public
          function  GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
          procedure AutoConnect( loaded : boolean ); override;
        public
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
          procedure BlockLoaded; override;
        public
          procedure StoreToCache( Cache : TObjectCache ); override;
        private
          fTranscended   : boolean;
          fOwnerName     : string;
          fWordsOfWisdom : string;
          fCanceled      : boolean;
        published
          property Transcended : boolean read fTranscended write fTranscended;
          property Canceled    : boolean read fCanceled write fCanceled;
        published
          procedure RDOSetWordsOfWisdom( words : widestring );
          procedure RDOCacncelTransc;
      end;

  procedure RegisterBackup;

implementation

  uses
    SysUtils, SimHints, Languages, ClassStorage, Logs, MetaInstances;

  // TMetaTranscendBlock

  constructor TMetaTranscendBlock.Create( anId : string; aLevelReq : string; aTime : integer; aBlockClass : CBlock );
    begin
      inherited Create( anId, aBlockClass );
      fLevelReq    := aLevelReq;
      fTime        := aTime;
      IsTransBlock := true;
    end;

  procedure TMetaTranscendBlock.StoreExtraInfoToCache( Cache : TObjectCache );
    var
      Level : TTycoonLevel;
    begin
      inherited;
      Level := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, fLevelReq]);
      if Level <> nil
        then Cache.WriteInteger( 'RequiredLevel', Level.Tier );
    end;

  procedure TMetaTranscendBlock.ModifyMetaFacility( MetaFacility : TMetaFacility );
    {
    var
      i     : integer;
      Level : TTycoonLevel;
    }
    begin
      inherited;
      {
      Level := TTycoonLevel(TheClassStorage.ClassById[tidClassFamily_TycoonLevels, fLevelReq]);
      for i := 0 to pred(LangList.Count) do
        MetaFacility.Requires_MLS.Values[LangList[i]] := SimHints.GetHintText( mtidDescFactoryReq.Values[LangList[i]], [0] ) + ' ' + Level.Name_MLS.Values[LangList[i]];
      }
    end;

  function TMetaTranscendBlock.Transcends(Facility : TFacility) : boolean;
    begin
      if (Facility.CurrBlock <> nil) and ObjectIs('TTranscendBlock', Facility.CurrBlock)
        then result := not TTranscendBlock(Facility.CurrBlock).Canceled
        else result := false;
    end;

  // TTranscendBlock

  function TTranscendBlock.Evaluate : TEvaluationResult;
    var
      Tycoon : TTycoon;
      i      : integer;
      Amount : TMoney;
      facAge : integer;
    begin
      result := inherited Evaluate;
      // >> Check this!!!
      facAge := Facility.Age;
      if not fTranscended and (facAge >= TMetaTranscendBlock(MetaBlock).Time*365*24) and (Facility.Company <> nil) and (Facility.Company.Owner <> nil)
        then
          begin
            Tycoon := Facility.Company.Owner;
            // Calculate debts
            Amount := 0;
            for i := 0 to pred(Tycoon.Loans.Count) do
              Amount := Amount + TLoan(Tycoon.Loans).Amount;
            // Check if he owns money to any bank  
            if Amount = 0
              then
                begin  
                  // >> Time to die
                  fTranscended := true;
                  if (Facility.Town <> nil) and not Facility.Company.Owner.IsRole // Mayors, Presidents and Ministers cannot trascend
                    then
                      begin
                        Tycoon.Transcending := true;
                        Facility.Town.ModelFactory.ResetTycoon(Tycoon);
                      end;
                end;
          end;
    end;

  function TTranscendBlock.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    begin
      result := inherited GetStatusText( kind, ToTycoon );
      case kind of
        sttMain :
          result := fOwnerName;
        sttSecondary :
          if not fTranscended
            then
              result :=
                result +
                SimHints.GetHintText( mtidPlayerWillTranscend.Values[ToTycoon.Language], [round(TMetaTranscendBlock(MetaBlock).Time*365 - Facility.Age/24)] )
            else
              result :=
                result +
                fOwnerName + ': ' +
                fWordsOfWisdom;
        sttHint :
          if not fTranscended
            then
              result :=
                result +
                SimHints.GetHintText( mtidBewareOfTranscend.Values[ToTycoon.Language], [fOwnerName] );
      end;
    end;

  procedure TTranscendBlock.AutoConnect( loaded : boolean );
    begin
      inherited;
      if not loaded
        then fOwnerName := Facility.Company.Owner.Name;
    end;

  procedure TTranscendBlock.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fTranscended   := Reader.ReadBoolean( 'Transcended', false );
      fOwnerName     := Reader.ReadString( 'OwnerName', '' );
      fWordsOfWisdom := Reader.ReadString( 'WordsOfWisdom', '' );
    end;

  procedure TTranscendBlock.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteBoolean( 'Transcended', fTranscended );
      Writer.WriteString( 'OwnerName', fOwnerName );
      Writer.WriteString( 'WordsOfWisdom', fWordsOfWisdom );
    end;

  procedure TTranscendBlock.BlockLoaded;
    begin
      inherited;
      // Patch
      if (Facility <> nil) and Facility.Deleted
        then Facility.Deleted := false;
      if (Facility <> nil) and Facility.Stopped
        then Facility.Stopped := false;
    end;

  procedure TTranscendBlock.StoreToCache( Cache : TObjectCache );
    begin
      inherited;
      Cache.WriteString( 'WordsOfWisdom', fWordsOfWisdom );
      Cache.WriteString( 'OwnerName', fOwnerName );
      Cache.WriteBoolean( 'Transcended', fTranscended );
      Cache.WriteBoolean('TransCanceled', fCanceled);
    end;

  procedure TTranscendBlock.RDOSetWordsOfWisdom( words : widestring );
    var
      Tycoon : TTycoon;
    begin
      if (Facility.Town <> nil) and (Facility.Town.WorldLocator <> nil)
        then Tycoon := Facility.Town.WorldLocator.GetTycoonByName(fOwnerName)
        else Tycoon := nil;
      if (Tycoon <> nil) and Tycoon.CheckOpAuthenticity
        then
          begin
            fWordsOfWisdom := words;
            Facility.Cache(false, true);
          end;
    end;

  procedure TTranscendBlock.RDOCacncelTransc;
    var
      Tycoon : TTycoon;
    begin
      if (Facility.Town <> nil) and (Facility.Town.WorldLocator <> nil)
        then Tycoon := Facility.Town.WorldLocator.GetTycoonByName(fOwnerName)
        else Tycoon := nil;
      if (Tycoon <> nil) and Tycoon.CheckOpAuthenticity
        then
          begin
            fCanceled := true;
            Facility.Town.ModelFactory.RequestDeletion(Facility);
          end;
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( TTranscendBlock );
    end;

end.

