unit WorldPolitics;

{ $DEFINE DebugPolitics}

interface

  uses
    MetaInstances, BackupInterfaces, Persistent, Kernel, Population, Politics, World, Collection,
    Accounts, CacheAgent, BackupObjects, Protocol, Languages, ConnectedBlock;

  const
    PresidentInitialBudget = 1000*1000*1000;
    MinisterInitialBudget  = 100*1000*1000;

  const
    tidClassFamily_WorldRatings  = 'WorldRatings';
    tidClassFamily_WorldProjects = 'WorldProjects';
    tidProjectKind_Minister      = 'Minister';
    tidFacilityKind_Capitol      = 'Capitol';
    tidSuperFacKind_Capitol      = 'Capitol';

  const
    currId_President = 15;
    currId_Minister  = 16;

  const
    MaxPresPeriods = 2;

  type
    TMetaMinisterProject = class;
    TMinisterProject     = class;
    TMetaMinisterRating  = class;
    TMinisterRating      = class;
    TMinistry            = class;
    TMinister            = class;
    TPresident           = class;
    TPoliticalWorld      = class;
    TPresidentialHall    = class;

    CMinister = class of TMinister;

    TMetaMinisterProject =
      class( TMetaProject )
        public
          constructor Create( anId, aKind : string; aName : TMultiString; aWeight : integer; aMinId : TMinistryId; aProjectClass : CProject );
        private
          fMinistryId : TMinistryId;
        public
          property MinistryId : TMinistryId read fMinistryId;
      end;

    TMinProposalState = (minUnassigned, minInvalidName, minTycoonRejected, minApproved);

    TMinisterProject =
      class( TProject )
        private
          fMinisterName  : string;
          fProposalState : TMinProposalState;
        public
          property MinisterName  : string            read fMinisterName;
          property ProposalState : TMinProposalState read fProposalState;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure ParseData( Data : string ); override;
          procedure StoreToCache( Cache : TObjectCache ); override;
        protected
          function ComputeCloseness ( Sample : TPercent ) : TPercent;
          function ComputeAcceptance( Sample : TPercent ) : TPercent;
      end;

    TMetaMinisterRating =
      class( TMetaRating )
        public
          constructor Create( anId : string; aName : TMultiString; aWeight : integer; aMindId : TMinistryId; aRatingClass : CRating );
        private
          fMinId : TMinistryId;
        public
          property MinId : TMinistryId read fMinId;
      end;

    TMinisterRating =
      class( TRating )
        protected
          procedure ComputeIFELRating; override;
      end;

    TMinistry =
      class( TMetaInstance )
        public
          constructor Create( aName : string; aMinId : TMinistryId; aMinisterClass : CMinister );
        private
          fName          : string;
          fName_MLS      : TMultiString;
          fMinId         : TMinistryId;
          fMinisterClass : CMinister;
        public
          property Name          : string       read fName;
          property Name_MLS      : TMultiString read fName_MLS;
          property MinId         : TMinistryId  read fMinId;
          property MinisterClass : CMinister    read fMinisterClass;
        public
          procedure RetrieveTexts( Container : TDictionary ); override;
          procedure StoreTexts   ( Container : TDictionary ); override;
      end;

    TMinister =
      class( TTycoon )
        private
          fRating     : TPercent;
          fWorld      : TPoliticalWorld;
          fYearBudget : TMoney;
          fMinistry   : TMinistry;
        public
          property Rating     : TPercent        read fRating;
          property YearBudget : TMoney          read fYearBudget write fYearBudget;
          property Ministry   : TMinistry       read fMinistry;
          property World      : TPoliticalWorld read fWorld;
        protected
          class function GetIsRole : boolean; override;
          class function DesignedZoner : boolean; override;
        public
          procedure GenMoney( Money : TMoney; Reason : TMoneyReason ); override;
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); override;
        protected
          function  ComputeRating : TPercent; virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure StoreRoleInfoToCache( Cache : TObjectCache ); override;
      end;

    TPresident =
      class( TTycoon )
        private
          fRating : TPercent;
          fWorld  : TPoliticalWorld;
        public
          property Rating : TPercent        read fRating;
          property World  : TPoliticalWorld read fWorld;
        protected
          class function GetIsRole : boolean; override;
          class function DesignedZoner : boolean; override;
        public
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); override;
          function  ComputeRating : TPercent; virtual;
        public
          procedure StoreRoleInfoToCache( Cache : TObjectCache ); override;
      end;

    TTownTaxInfo =
      class( TPersistent )
        public
          constructor Create( aTown : TTown );
        private
          fTown     : TTown;
          fTaxValue : single;
        public
          property Town     : TTown  read fTown;
          property TaxValue : single read fTaxValue write fTaxValue;
        public
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TPoliticalWorld =
      class( TInhabitedWorld, IPoliticalEntity )
        public
          constructor Create( aName : string; axSize : integer; aySize : integer ); override;
          destructor  Destroy; override;
        private
          fPresident        : TPresident;
          fMinisters        : TCollection;
          fMinistriesBudget : TMoney;
          fHall             : TPresidentialHall;
          fRatings          : TRatingSystem;
          fCampaigns        : TCampaignSystem;
          fLastPerPop       : TFluidValue;
          fLastElect        : TVirtDateAbs;
          fLastElectYear    : integer;
          fWinningCampaign  : TCampaign;
          fMandatesInRow    : integer;
          fTownTaxes        : TLockableCollection;
        private
          function GetMinister( Id : TMinistryId ) : TMinister;
        public
          property President                    : TPresident        read fPresident;
          property Ministers[Id : TMinistryId]  : TMinister         read GetMinister;
          property MinistriesBudget             : TMoney            read fMinistriesBudget;
          property Hall                         : TPresidentialHall read fHall;
          property Ratings                      : TRatingSystem     read fRatings;
          property Campaigns                    : TCampaignSystem   read fCampaigns;
        protected
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); override;
          procedure ElectPresident; virtual;
        public
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure Loaded( Notify : TBackupReaderNotify ); override;
          procedure InsertTown( Town : TTown ); override;
        protected
          procedure TycoonDeleted( Tycoon : TTycoon ); override;
        protected
          function  GetMainDealer : TMoneyDealer; override;
        private
          procedure InitMinisters;
        // IPoliticalEntity
        private
          function getWinningCampaign : TCampaign;
          function getParm( id : TPoliticalParmId; const info ) : single;
          function getCacheFolder : string;
          function getRDOId       : integer;
        public
          function TycoonHasCampaign(Tycoon : TTycoon) : boolean;
        published
          procedure RDOSitMinister(minName, tycoonName : widestring);
          procedure RDOSitMayor(TownName, TycoonName : widestring);
        private
          procedure SendMinisterNotification(Minister : TMinister);
      end;

    TMetaPresidentialHall =
      class( TMetaBlock )
        public
          constructor Create( anId : string; aBlockClass : CBlock );
      end;

    TPresidentialHall =
      class( TBlock )
        private
          fAdvertisement : TPullInputData;
          fLastAds       : TFluidData;
        private
          fWorld : TPoliticalWorld;
        public
          property PoliticalWorld : TPoliticalWorld read fWorld;
        protected
          function Evaluate : TEvaluationResult; override;
        public
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
        public
          procedure AutoConnect ( loaded : boolean ); override;
          procedure EndOfPeriod ( PeriodType : TPeriodType; PeriodCount : integer ); override;
          procedure StoreToCache( Cache : TObjectCache ); override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        private
          function TycoonIsAvailable( Tycoon : TTycoon ) : boolean;
          function TycoonHasNoCampaign( Tycoon : TTycoon ) : boolean;
        published
          function  RDOGetRatingFrom( RatingId : widestring; TycoonId : widestring ) : olevariant;
          procedure RDOSetRatingFrom( RatingId : widestring; TycoonId : widestring; Value : integer );
          procedure RDOSetPublicity ( RatingId : widestring; Value : integer );
          function  RDOLaunchCampaign ( TycoonId : widestring ) : olevariant;
          procedure RDOCancelCampaign ( TycoonId : widestring );
          procedure RDOSetProjectData ( TycoonId : widestring; ProjectId, Data : widestring );
          procedure RDOSetMinistryBudget( MinistryId : integer; Budget : widestring );
          procedure RDOSitMinister(MinistryId : integer; name : widestring);
          procedure RDOBanMinister(MinistryId : integer);
          procedure RDOSetTownTaxes( Index, Value : integer );
          procedure RDOSetMinSalaryValue( PopKind, Value : integer );
          procedure RDOSitMayor(TownName, TycoonName : widestring);
          procedure RDOVote(voterTycoon, choiceTycoon : widestring);
          function  RDOVoteOf(tycoonName : widestring) : OleVariant;
      end;

  const
    tidClassFamily_Ministries = 'Ministries';
    tidMinistry               = 'Ministry';
    tidGate_Advertisement     = 'Advertisement';

  const
    tidRole_Minister  = 'Minister';
    tidRole_President = 'President';

  const
    plidMinisterApproval   = 100;
    plidMinisterAcceptance = 101;
    plidMinisterRating     = 102;

  const
    MinMinisterPrestige  = 100;
    MinPresidentPrestige = 1000;

  const
    PresMinBudget      = 200000000;
    MinisterMinBudget  = 100000000;


  // RegisterCapitol

  procedure RegisterCapitol;

  // RegisterPolitics

  procedure RegisterPolitics;
  procedure PostRegisterPolitics;

  // RegisterBackup

  procedure RegisterBackup;

implementation

  uses
    SysUtils, ClassStorage, ModelServerCache, BasicCurriculum, BasicPolitics, ServiceBlock, Logs, CacheCommon,
    SimHints, TownPolitics, Construction, BasicAccounts, StdFluids, MathUtils, Events;


  // TMetaMinisterProject

  constructor TMetaMinisterProject.Create( anId, aKind : string; aName : TMultiString; aWeight : integer; aMinId : TMinistryId; aProjectClass : CProject );
    var
      i : integer;
    begin
      inherited Create( anId, aName.Values[langDefault], aKind, aWeight, aProjectClass );
      for i := 0 to pred(LangList.Count) do
        Name_MLS.Values[LangList[i]] := GetHintText(mtidMinisterName.Values[langDefault], [aName.Values[LangList[i]]]);
      fMinistryId := aMinId;
    end;


  // TMinisterProject

  procedure TMinisterProject.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fMinisterName  := Reader.ReadString( 'MinisterName', '' );
      fProposalState := TMinProposalState(Reader.ReadInteger( 'ProposalState', 0 ));
    end;

  procedure TMinisterProject.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'MinisterName', fMinisterName );
      Writer.WriteInteger( 'ProposalState', integer(fProposalState) );
    end;

  procedure TMinisterProject.ParseData( Data : string );
    begin
      inherited;
      fMinisterName  := Data;
      fProposalState := TMinProposalState(round(Campaign.System.PoliticalEntity.getParm( plidMinisterApproval, fMinisterName )));
    end;

  procedure TMinisterProject.StoreToCache( Cache : TObjectCache );
    begin
      inherited;
      Cache.WriteString( 'MinisterName', fMinisterName );
      Cache.WriteInteger( 'ProposalState', integer(fProposalState) );
    end;

  function TMinisterProject.ComputeCloseness( Sample : TPercent ) : TPercent;
    begin
      result := 100;
    end;

  function TMinisterProject.ComputeAcceptance( Sample : TPercent ) : TPercent;
    begin
      result := round(Campaign.System.PoliticalEntity.getParm( plidMinisterAcceptance, fMinisterName ));
      fProposalState := TMinProposalState(round(Campaign.System.PoliticalEntity.getParm( plidMinisterApproval, fMinisterName )));
      {
      if fProposalState = minApproved
        then
          begin
            Minister := TycoonByName[fMinisterName];
            if Minister <> nil
              then result := min( 100, Minister.Prestige div 10 )
              else
                begin
                  fProposalSate := minInvalidName;
                  result        := 0;
                end;
          end
        else result := 0;
      }
    end;


  // TMetaMinisterRating

  constructor TMetaMinisterRating.Create( anId : string; aName : TMultiString; aWeight : integer; aMindId : TMinistryId; aRatingClass : CRating );
    var
      i : integer;
    begin
      inherited Create( anId, aName.Values[langDefault], aWeight, aRatingClass );
      for i := 0 to pred(LangList.Count) do
        Name_MLS.Values[LangList[i]] := aName.Values[LangList[i]];
      fMinId := aMindId;
    end;


  // TMinisterRating
                                                
  procedure TMinisterRating.ComputeIFELRating;
    begin
      fIFELRating := round(System.PoliticalEntity.getParm( plidMinisterRating, TMetaMinisterRating(MetaRating).MinId ));
    end;


  // TMinistry

  constructor TMinistry.Create( aName : string; aMinId : TMinistryId; aMinisterClass : CMinister );
    var
      Cluster : TCluster;
    begin
      inherited Create( tidMinistry + IntToStr(aMinId) );
      fName          := aName;
      fMinId         := aMinId;
      fMinisterClass := aMinisterClass;
      Cacheable      := false;
      Cluster        := TCluster.Create( aName );
      fName_MLS      := TMultiString.Create;
      Cluster.Register( tidClassFamily_Clusters );
      Register( tidClassFamily_Ministries );
    end;

  procedure TMinistry.RetrieveTexts( Container : TDictionary );
    begin
      if fName_MLS = nil
        then fName_MLS := TMultiString.Create;
      fName_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Name'];
    end;

  procedure TMinistry.StoreTexts( Container : TDictionary );
    begin
      Container.Values[Family + '.' + Id + '.' + 'Name'] := fName;
    end;


  // TMinister

  class function TMinister.GetIsRole : boolean;
    begin
      result := true;
    end;

  class function TMinister.DesignedZoner : boolean;
    begin
      result := true;
    end;

  procedure TMinister.GenMoney( Money : TMoney; Reason : TMoneyReason );
    begin
      inherited;
      fWorld.President.GenMoney( Money, Reason );
    end;
    
  procedure TMinister.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
    begin
      inherited;
      if PeriodType = perDay
        then fRating := ComputeRating;
    end;

  function TMinister.ComputeRating : TPercent;
    begin
      result := 100;
    end;

  procedure TMinister.LoadFromBackup( Reader : IBackupReader );
    var
      MinId : string;
    begin
      inherited;
      fRating := Reader.ReadByte( 'Rating', 0 );
      Reader.ReadObject( 'World', fWorld, nil );
      fYearBudget  := Reader.ReadCurrency( 'YearBudget', 0 );
      MinId        := Reader.ReadString( 'Ministry', '' );
      fMinistry    := TMinistry(TheClassStorage.ClassById[tidClassFamily_Ministries, MinId]);
    end;

  procedure TMinister.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      //Logs.Log( 'Survival', 'Start Storing Minister: ' + Name );
      Writer.WriteByte( 'Rating', fRating );
      Writer.WriteObjectRef( 'World', fWorld );
      Writer.WriteCurrency( 'YearBudget', fYearBudget );
      Writer.WriteString( 'Ministry', fMinistry.Id );
      //Logs.Log( 'Survival', 'End Storing Minister: ' + Name );
    end;


  procedure TMinister.StoreRoleInfoToCache( Cache : TObjectCache );
    begin
      inherited;
      Cache.WriteBoolean( 'IsMinister', true );
      Cache.WriteString( 'Ministry', Ministry.Name );
    end;


  // TPresident

  class function TPresident.GetIsRole : boolean; 
    begin
      result := true;
    end;

  class function TPresident.DesignedZoner : boolean;
    begin
      result := true;
    end;

  procedure TPresident.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
    begin
      inherited;
      case PeriodType of
        perDay :
          fRating := ComputeRating;
      end;
    end;

  function TPresident.ComputeRating : TPercent;
    begin
      result := fWorld.fRatings.OverallRating;
    end;

  procedure TPresident.StoreRoleInfoToCache( Cache : TObjectCache );
    begin
      inherited;
      Cache.WriteBoolean( 'IsPresident', true );
    end;


  // TTownTaxInfo

  constructor TTownTaxInfo.Create( aTown : TTown );
    begin
      inherited Create;
      fTown := aTown;
    end;

  procedure TTownTaxInfo.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fTaxValue := Reader.ReadSingle( 'TaxValue', 0.1 );
      Reader.ReadObject( 'Town', fTown, nil );
    end;

  procedure TTownTaxInfo.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteSingle( 'TaxValue', fTaxValue );
      Writer.WriteObjectRef( 'Town', fTown );
    end;
    

  // TPoliticalWorld

  constructor TPoliticalWorld.Create( aName : string; axSize : integer; aySize : integer );
    begin
      inherited;
      fPresident := TPresident(NewTycoonRole( tidRole_Mayor, 'President of ' + aName, TPresident, PresidentInitialBudget ));
      NewCompany( 'President of ' + Name, TCluster(TheClassStorage.ClassById[tidClassFamily_Clusters, 'PGI']), fPresident ); // >> generalize this later!
      fMinisters := TCollection.Create( 0, rkUse );
      fRatings   := TRatingSystem.Create( self, tidClassFamily_WorldRatings );
      fCampaigns := TCampaignSystem.Create( self );
      fTownTaxes := TLockableCollection.Create( 0, rkBelonguer );
      InitMinisters;
    end;

  destructor TPoliticalWorld.Destroy;
    begin
      fTownTaxes.Free;
      fMinisters.Free;
      fRatings.Free;
      fCampaigns.Free;
      inherited;
    end;

  function TPoliticalWorld.GetMinister( Id : TMinistryId ) : TMinister;
    var
      i : integer;
    begin
      i := 0;
      while (i < fMinisters.Count) and (TMinister(fMinisters[i]).Ministry.MinId <> Id) do
        inc( i );
      if i < fMinisters.Count
        then result := TMinister(fMinisters[i])
        else result := nil;
    end;

  procedure TPoliticalWorld.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );

    procedure FixBudget;
      var
        i         : integer;
        Min       : TMinister;
        ReqBudget : TMoney;
        budperc   : single;
        Budget    : TMoney;
      begin
        {
        if President.Budget < PresMinBudget
          then President.GenMoney( PresMinBudget - President.Budget, accIdx_PublicBudget );
        }
        ReqBudget := 0;
        for i := 0 to pred(fMinisters.Count) do
          begin
            Min := TMinister(fMinisters[i]);
            ReqBudget := ReqBudget + realmax( 0, Min.YearBudget - Min.Budget );
          end;
        if (President.Budget > ReqBudget) or (ReqBudget = 0)
          then budperc := 1
          else budperc := President.Budget/ReqBudget;
        for i := 0 to pred(fMinisters.Count) do
          begin
            Min := TMinister(fMinisters[i]);
            if Min.Budget < Min.YearBudget
              then
                begin
                  Budget := budperc*(Min.YearBudget - Min.Budget);
                  Min.GenMoney( Budget, accIdx_PublicBudget );
                end;
          end;
      end;

    procedure CollectTownTaxes;
      var
        TX    : TTownTaxInfo;
        i     : integer;
        Mayor : TTycoon;
        Tax   : TMoney;
      begin
        for i := 0 to pred(fTownTaxes.Count) do
          try
            TX := TTownTaxInfo(fTownTaxes[i]);
            if TX.TaxValue > 0
              then
                begin
                  Mayor := TX.Town.Mayor;
                  if (Mayor <> nil) and (Mayor.Accounts.MasterAccount.Value > 0)
                    then
                      begin
                        Tax := TX.fTaxValue*Mayor.Accounts.MasterAccount.Value;
                        Mayor.GenMoney( -Tax, accIdx_PublicBudget );
                        President.GenMoney( Tax, accIdx_PublicBudget );
                      end;
                end;
          except
          end;
      end;

    begin
      inherited;
      case PeriodType of
        perHour :
          if (fWinningCampaign <> nil) and (President.SuperRole = nil)
            then
              begin
                fWinningCampaign.Free;
                fWinningCampaign := nil;
              end;
        perDay :
          begin
            fRatings.Evaluate;
            fCampaigns.Evaluate;
            {$IFDEF DebugPolitics}
            FixBudget;
            {$ENDIF}
          end;
        perPoliticalMandate :
          begin
            ElectPresident;
            ClearTycoonVotes;
          end;
        {$IFNDEF DebugPolitics}
        perYear :
          FixBudget;
        {$ENDIF}
      end;
    end;

  procedure TPoliticalWorld.ElectPresident;

    function PresidentStays : boolean;
      begin
        result :=
          (succ(fMandatesInRow) < MaxPresPeriods) and
          (fWinningCampaign <> nil) and
          Campaigns.IsTheBestCampaign(fWinningCampaign);
      end;

    procedure AssignMinisters;
      var
        i           : integer;
        Minister    : TMinister;
        NewMinister : TTycoon;
        MinProject  : TMinisterProject;
      begin
        // Remove old ministers
        for i := 0 to pred(fMinisters.Count) do
          begin
            Minister := TMinister(fMinisters[i]);
            if Minister.MasterRole <> Minister
              then
                begin
                  Minister.MasterRole.AddItemToCurriculum(
                    TOpenItem.Create(
                      '',
                      0,
                      InstantiateMultiString( mtidWasMinister, [Name, fLastElectYear, CurrYear, Minister.Rating] ),
                      //SimHints.GetHintText( hidWasMinister, [Name, fLastElectYear, CurrYear, Minister.Rating] ),
                      20,
                      20 + 20*(Minister.Rating - 50) ));
                  Minister.MasterRole.AbandomRole( Minister );
                end;
          end;
        // Assign new ministers
        with fWinningCampaign do
          for i := 0 to pred(Projects.Count) do
            if ObjectIs( TMinisterProject.ClassName, Projects[i] )
              then
                begin
                  MinProject := TMinisterProject(Projects[i]);
                  if MinProject.MinisterName <> ''
                    then NewMinister := TycoonByName[MinProject.MinisterName]
                    else NewMinister := nil;
                  Minister := Ministers[TMetaMinisterProject(MinProject.MetaProject).MinistryId];

                  {if NewMinister = nil
                    then NewMinister := President.MasterRole
                    else
                      NewMinister.AddItemToCurriculum(
                        TOpenItem.Create(
                          '',
                          0,
                          InstantiateMultiString( mtidAppointedMinister, [Minister.Ministry.Name_MLS.Values[langDefault], CurrYear, President.MasterRole.Name] ),
                          //SimHints.GetHintText( hidAppointedMinister, [Minister.Ministry.Name, CurrYear, President.MasterRole.Name] ),
                          20,
                          100 ));}

                  if NewMinister <> nil
                    then
                      begin
                        NewMinister.AddItemToCurriculum(
                          TOpenItem.Create(
                            '',
                            0,
                            InstantiateMultiString( mtidAppointedMinister, [Minister.Ministry.Name_MLS.Values[langDefault], CurrYear, President.MasterRole.Name] ),
                            //SimHints.GetHintText( hidAppointedMinister, [Minister.Ministry.Name, CurrYear, President.MasterRole.Name] ),
                            20,
                            100 ));
                        NewMinister.AssumeRole( Minister );
                        SendEvent(
                          TEvent.Create(
                            0,
                            VirtualTimeAbs,
                            VirtualTime,
                            200000,
                            15000,
                            InstantiateMultiString( mtidMinisterElected, [NewMinister.Name, Minister.Name] ),
                            //NullString, //NewMinister.Name + ' was appointed ' + Minister.Name + '.',
                            '',
                            'Visual/Voyager/Politics/politics.asp?Capitol=YES&x=' + IntToStr(Hall.xPos) + '&y=' + IntToStr(Hall.yPos) +
                            '&frame_Id=PoliticsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client' ));
                        SendMinisterNotification(Minister);
                      end;
                end;
      end;

    var
      NewPresident : TPresident;
      HallLink     : string;
    begin
      if not PresidentStays and (Campaigns.Campaigns.Count > 0)
        then
          begin
            if fWinningCampaign <> nil
              then fWinningCampaign.Free;
            fWinningCampaign := TCampaign(Campaigns.Campaigns[0]);
            UncacheObject( fWinningCampaign, noKind, noInfo );
            Campaigns.Campaigns.Extract( fWinningCampaign );
            Campaigns.Campaigns.DeleteAll;
            President.MasterRole.AddItemToCurriculum(
              TOpenItem.Create(
                '',
                0,
                InstantiateMultiString( mtidWasPresident, [Name, fLastElectYear, CurrYear, Ratings.OverallRating] ),
                //SimHints.GetHintText( hidWasPresident, [Name, fLastElectYear, CurrYear, Ratings.OverallRating] ),
                20,
                30 + 100*(Ratings.OverallRating - 50) ));
            Ratings.Clear;

            // Assign new president
            NewPresident := TPresident(fWinningCampaign.Tycoon);
            if President.SuperRole <> nil
              then President.SuperRole.AbandomRole( President );
            NewPresident.AssumeRole( President );
            NewPresident.AddItemToCurriculum(
              TOpenItem.Create(
                '',
                0,
                InstantiateMultiString( mtidWonWorldElections, [Name, CurrYear] ),
                //SimHints.GetHintText( hidWonWorldElections, [Name, CurrYear] ),
                20,
                300 ));
            fMandatesInRow := 0;
            SendEvent(
              TEvent.Create(
                0,
                VirtualTimeAbs,
                VirtualTime,
                200000,
                20000,
                InstantiateMultiString( mtidPresidentElected, [NewPresident.Name, Name] ),
                //NullString, //NewPresident.Name + ' was elected President of ' + Name + '.',
                '',
                'Visual/Voyager/Politics/politics.asp?Capitol=YES&x=' + IntToStr(Hall.xPos) + '&y=' + IntToStr(Hall.yPos) +
                '&frame_Id=PoliticsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client' ));
            fLastElectYear := CurrYear;
            fLastPerPop    := TotalPop;

            // Assign ministers
            AssignMinisters;
          end
        else
          begin
            Campaigns.Campaigns.Extract( fWinningCampaign );
            Campaigns.Campaigns.DeleteAll;
            inc( fMandatesInRow );
            if Hall <> nil
              then HallLink := '&x=' + IntToStr(Hall.xPos) + '&y=' + IntToStr(Hall.yPos)
              else HallLink := '';
            if President.MasterRole <> President
              then
                SendEvent(
                  TEvent.Create(
                    0,
                    VirtualTimeAbs,
                    VirtualTime,
                    200000,
                    20000,
                    InstantiateMultiString( mtidPresidentReElected, [President.MasterRole.Name, Name] ),
                    //NullString, //President.MasterRole.Name + ' was reelected President of ' + Name + '.',
                    '',
                    'Visual/Voyager/Politics/politics.asp?Capitol=YES' + HallLink +
                    '&frame_Id=PoliticsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client' ))
              else
                SendEvent(
                  TEvent.Create(
                    0,
                    VirtualTimeAbs,
                    VirtualTime,
                    200000,
                    1000,
                    InstantiateMultiString( mtidNoPresident, [Name] ),
                    //NullString, //Name + ' has no President.',
                    '',
                    'Visual/Voyager/Politics/politics.asp?Capitol=YES' + HallLink +
                    '&frame_Id=PoliticsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client' ))
          end;
      if fWinningCampaign <> nil
        then fWinningCampaign.CurrentVotes := 0;
    end;

  procedure TPoliticalWorld.LoadFromBackup( Reader : IBackupReader );

    procedure InitTownTaxes;
      var
        i  : integer;
        TX : TTownTaxInfo;
      begin
        fTownTaxes := TLockableCollection.Create( 0, rkBelonguer );
        for i := 0 to pred(Towns.Count) do
          begin
            TX := TTownTaxInfo.Create( TTown(Towns[i]) );
            TX.TaxValue := 0.1;
            fTownTaxes.Insert( TX );
          end;
      end;

    begin
      inherited;
      Reader.ReadObject( 'President', fPresident, nil );
      Reader.ReadObject( 'Ministers', fMinisters, nil );
      Reader.ReadObject( 'Hall', fHall, nil );
      Reader.ReadObject( 'Ratings', fRatings, nil );
      Reader.ReadObject( 'Campaigns', fCampaigns, nil );
      Reader.ReadObject( 'WinningCampaign', fWinningCampaign, nil );
      Reader.ReadObject( 'TownTaxes', fTownTaxes, nil );
      if fTownTaxes = nil
        then InitTownTaxes;
      fLastPerPop       := Reader.ReadSingle( 'LastPerPop', 0 );
      fLastElect        := Reader.ReadInteger( 'LastElect', 0 );
      fLastElectYear    := Reader.ReadInteger( 'LastElectYear', 0 );
      fMandatesInRow    := Reader.ReadInteger( 'MandatesInRow', 0 );
      if true //Ratings = nil
        then fRatings := TRatingSystem.Create( self, tidClassFamily_WorldRatings );
      fRatings.PoliticalEntity := self;
      if fCampaigns = nil
        then fCampaigns := TCampaignSystem.Create( self );
      fCampaigns.PoliticalEntity := self;
    end;

  procedure TPoliticalWorld.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObjectRef( 'President', fPresident );
      Writer.WriteObject( 'Ministers', fMinisters );
      Writer.WriteObjectRef( 'Hall', fHall );
      Writer.WriteObject( 'Ratings', fRatings );
      Writer.WriteObject( 'Campaigns', fCampaigns );
      Writer.WriteObject( 'WinningCampaign', fWinningCampaign );
      Writer.WriteObject( 'TownTaxes', fTownTaxes );
      Writer.WriteSingle( 'LastPerPop', fLastPerPop );
      Writer.WriteInteger( 'LastElect', fLastElect );
      Writer.WriteInteger( 'LastElectYear', fLastElectYear );
      Writer.WriteInteger( 'MandatesInRow', fMandatesInRow );
    end;

  procedure TPoliticalWorld.Loaded( Notify : TBackupReaderNotify );
    begin
      inherited;
      if fPresident = nil
        then
          begin
            fPresident := TPresident(NewTycoonRole( tidRole_Mayor, 'President of ' + Name, TPresident, PresidentInitialBudget ));
            NewCompany( 'President of ' + Name, TCluster(TheClassStorage.ClassById[tidClassFamily_Clusters, 'PGI']), fPresident ); // >> Generalize this later...
          end;
      fPresident.fWorld := self;
      if fPresident.Companies.Count = 0
        then NewCompany( 'President of ' + Name, TCluster(TheClassStorage.ClassById[tidClassFamily_Clusters, 'PGI']), fPresident );
      if fMinisters = nil
        then fMinisters := TCollection.Create( 0, rkUse );
      InitMinisters;
      if fPresident.SuperRole = nil
        then
          begin
            fWinningCampaign.Free;
            fWinningCampaign := nil;
          end;
      try
        fRatings.Loaded;
      except
        Logs.Log(tidLog_Survival, TimeToStr(Now) + ' Error TPoliticalWorld::fRatings.Loaded..');
      end;
      try
        fCampaigns.Loaded;
      except
        Logs.Log(tidLog_Survival, TimeToStr(Now) + ' Error TPoliticalWorld::fCampaigns.Loaded..');
      end;
      fLastElectYear := max(2000, fLastElectYear);
    end;

  procedure TPoliticalWorld.InsertTown( Town : TTown );
    begin
      inherited;
      fTownTaxes.Insert( TTownTaxInfo.Create( Town ) );
    end;

  procedure TPoliticalWorld.TycoonDeleted( Tycoon : TTycoon );
    var
      Campaign : TCampaign;
    begin
      inherited;
      Campaign := fCampaigns[Tycoon];
      if Campaign <> nil
        then fCampaigns.Campaigns.Delete( Campaign );
    end;
    
  function TPoliticalWorld.GetMainDealer : TMoneyDealer;
    begin
      result := fPresident;
    end;
    
  procedure TPoliticalWorld.InitMinisters;
    var
      NewMinisters : TCollection;
      count        : integer;
      i            : integer;
      Ministry    : TMinistry;
      Minister     : TMinister;
      Cluster      : TCluster;
    begin
      NewMinisters := TCollection.Create( 0, rkUse );
      count := TheClassStorage.ClassCount[tidClassFamily_Ministries];
      for i := 0 to pred(count) do
        begin
          Ministry := TMinistry(TheClassStorage.ClassByIdx[tidClassFamily_Ministries, i]);
          Minister := Ministers[Ministry.MinId];
          if Minister = nil
            then
              begin
                Minister :=
                  TMinister(NewTycoonRole( tidRole_Minister,
                  GetHintText(mtidMinisterName.Values[langDefault], [Ministry.Name_MLS.Values[langDefault]]),
                  //'Minister of ' + Ministry.Name_MLS.Values[langDefault],
                  Ministry.MinisterClass, 0 ));
                Minister.YearBudget := MinisterInitialBudget;
                Cluster             := ClusterByName[Ministry.Name_MLS.Values[langDefault]];
                Cluster.Company :=
                  NewCompany(
                  GetHintText(mtidMinistryName.Values[langDefault], [Ministry.Name_MLS.Values[langDefault]]),
                  //'Ministry of ' + Ministry.Name,
                  Cluster, Minister );
              end;
          Minister.fWorld    := self;
          Minister.fMinistry := Ministry;
          NewMinisters.Insert( Minister );
        end;
      fMinisters.Free;
      fMinisters := NewMinisters;
    end;
    
  function TPoliticalWorld.getWinningCampaign : TCampaign;
    begin
      result := fWinningCampaign;
    end;

  function TPoliticalWorld.getParm( id : TPoliticalParmId; const info ) : single;
    var
      TycoonName : string                  absolute info;
      TycoonInfo : TTycoon absolute info;
      MPFInfo    : TMetaPublicFacilityInfo absolute info;
      MinId      : TMinistryId             absolute info;
      i          : TPeopleKind;
      unemp      : single;
      Tycoon     : TTycoon;
      Minister   : TMinister;
    begin
      case id of
        plidLastAdsQ :
          if Hall <> nil
            then result := Hall.fLastAds.Q
            else result := 0;
        plidLastAdsK :
          if Hall <> nil
            then result := Hall.fLastAds.K
            else result := 0;
        plidRatingEthics :
          if (TycoonInfo <> nil) and (Campaigns.Campaign[TycoonInfo] = nil)
            then result := 1
            else result := 0;
        plidPopGrowth :
          result := 0;
        plidPublicParm :
          result := realmin(100, PublicFacInfo[MPFInfo.Id].Ratio);
        plidUnemployment :
          if TotalPop > 0
            then
              begin
                unemp := 0;
                for i := low(i) to high(i) do
                  unemp := unemp + Population[i]*Unemployment[i];
                result := 100 - unemp/TotalPop;
              end
            else result := 100;
        plidGQOS :
          result := GQOS;
        plidWealth :
          result := GeneralWealth;
        plidMinisterApproval :
          begin
            if TycoonName <> ''
              then
                begin
                  Tycoon := TycoonByName[TycoonName];
                  if Tycoon <> nil
                    then
                      if (Tycoon.MasterRole.Roles.Count = 0) and (Tycoon.MasterRole = Tycoon) and not Tycoon.IsRole and (Tycoon.Prestige >= MinMinisterPrestige)
                        then result := integer(minApproved)
                        else result := integer(minTycoonRejected)
                    else result := integer(minInvalidName)
                end
              else result := integer(minUnassigned)
          end;
        plidMinisterAcceptance :
          begin
            Tycoon := TycoonByName[TycoonName];
            if Tycoon <> nil
              then
                if Tycoon.Prestige >= MinMinisterPrestige
                  then result := realmin( 100, 100*Tycoon.Prestige/MinMinisterPrestige )
                  else result := 0
              else result := 0;
          end;
        plidMinisterRating :
          begin
            Minister := Ministers[MinId];
            if Minister <> nil
              then result := Minister.Rating
              else result := 0;
          end;
        else
          result := 0;
      end;
    end;

  function TPoliticalWorld.getCacheFolder : string;
    begin
      if fHall <> nil
        then result := GetObjectPath( fHall.Facility, noKind, noInfo )
        else result := '';
    end;

  function TPoliticalWorld.getRDOId : integer;
    begin
      result := integer(fHall);
    end;

  function TPoliticalWorld.TycoonHasCampaign(Tycoon : TTycoon) : boolean;
    begin
      result := fCampaigns.Campaign[Tycoon] <> nil;
    end;

  procedure TPoliticalWorld.RDOSitMinister(minName, tycoonName : widestring);
    var
      Min    : TMinister;
      Tycoon : TTycoon;
    begin
      Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' World Sitting Minister.' );
      try
        Min    := TMinister(TycoonByName[minName]);
        Tycoon := TycoonByName[tycoonName];
        if (Min <> nil) and (Tycoon <> nil) and not Tycoon.IsRole //and ((President = nil) or (President.SuperRole = nil))
          then
            begin
              Min.AbandomRoles;
              Tycoon.AssumeRole(Min);
              Min.Budget := 1000*1000*1000;
              ModelServerCache.BackgroundCache(Min, false);
              ModelServerCache.BackgroundCache(Tycoon, false);
              SendMinisterNotification(Min);
            end;
      except
        Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Error World Sitting Minister.' );
      end;
    end;

  procedure TPoliticalWorld.RDOSitMayor(TownName, TycoonName : widestring);
    var
      Mayor  : TTycoon;
      Tycoon : TTycoon;
      Town   : TPoliticalTown;
    begin
      Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Sitting Mayor.' );
      try
        if (President <> nil) and (President.SuperRole = nil)
          then
            begin
              Town  := TPoliticalTown(TownByName[TownName]);
              Mayor := TycoonByName['Mayor of ' + TownName];
              if (Mayor <> nil) and (Mayor.SuperRole = nil) and (Town <> nil)
                then
                  begin
                    Tycoon := TycoonByName[TycoonName];
                    if (Tycoon <> nil) and (Tycoon.Roles.Count = 0)
                      then
                        begin
                          Town.MayorPeriods := 0;
                          Tycoon.AssumeRole(Mayor);
                          TPoliticalTownHall(Town.TownHall.CurrBlock).SitMayor(Tycoon);
                        end;
                  end;
            end;
      except
        Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Error Sitting Mayor.' );
      end;
    end;

  procedure TPoliticalWorld.SendMinisterNotification(Minister : TMinister);
    var
      URL    : string;
      Tycoon : TTycoon;
      from   : string;
      dest   : string;
    begin
      try
        Tycoon := Minister.MasterRole;
        if (Tycoon <> nil) and (Minister.Ministry <> nil)
          then
            begin
              from := 'mailer@Global' + self.Name + '.net';
              dest := Tycoon.Name + '@' + self.Name + '.net';
              URL  := WorldURL + '/' + Tycoon.Language + '/' + tidURL_SpecialMailMessages + '/NotifyMinister.asp?Minister=' + Tycoon.Name + '&Ministry=' + Minister.Ministry.Name_MLS.Values[Tycoon.Language] + '&President=' + President.MasterRole.Name + '&WorldName=' + self.Name;
              MailServer.SendHTMLMessage(from, dest, 'Welcome Minister', URL);
            end;
      except
        Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Error in SendMinisterNotification.' );
      end;
    end;


  // TMetaPresidentialHall

  constructor TMetaPresidentialHall.Create( anId : string; aBlockClass : CBlock );
    var
      Sample : TPresidentialHall;
    begin
      inherited Create( anId, accIdx_None, accIdx_None, aBlockClass );
      Sample := nil;
      // Ads
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Advertisement,
          inputZero,
          inputZero, //inputIlimited,
          InputData( 0, 0 ),
          0, //qIlimited,
          TPullInput, // >> TPushInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Advertisement]),
          1000,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fAdvertisement),
          Sample.Offset( Sample.fAdvertisement )));
      CnntRepairable := false; // >> Corruption prevention
    end;


  // TPresidentialHall

  function TPresidentialHall.Evaluate : TEvaluationResult;
    begin
      result := inherited Evaluate;
      fLastAds.Q := fAdvertisement.Q;
      fLastAds.K := fAdvertisement.K;
      if fWorld.fHall <> self
        then
          begin
            fWorld.ReportCapitolMoved(self, fWorld.fHall);
            Facility.Town.ModelFactory.RequestDeletion(Facility);
          end;
    end;

  function TPresidentialHall.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    var
      Active : boolean;
    begin
      result := inherited GetStatusText( kind, ToTycoon );
      Active := fWorld.fHall = self;
      case kind of
        sttMain :
          if Active
            then
              begin
                if PoliticalWorld.President.SuperRole <> nil
                  then result := result + ' ' +
                         //President: ' + PoliticalWorld.President.MasterRole.Name;
                         SimHints.GetHintText( mtidCapitolMain.Values[ToTycoon.Language], [PoliticalWorld.President.MasterRole.Name] );
              end;
        sttSecondary :
          if Active
            then
              if PoliticalWorld.President.SuperRole <> nil
                then result := result +
                       //'President rating: ' + IntToStr(PoliticalWorld.President.Rating) + '%'
                       SimHints.GetHintText( mtidCapitolWithPresSec.Values[ToTycoon.Language], [PoliticalWorld.President.Rating, PoliticalWorld.YearsToElections] )
                else result := result +
                       //'No President has been elected. ' + IntToStr(PoliticalWorld.YearsToElections) + ' years to elections.'
                       SimHints.GetHintText( mtidCapitolWithNoPresSec.Values[ToTycoon.Language], [PoliticalWorld.YearsToElections] );
        sttHint :
          if Active
            then result := result +
                   // 'Hint: If you have more than 1000 prestige points you can launch your campaign for the presidency of ' + PoliticalWorld.Name
                   SimHints.GetHintText( mtidCapitolHint.Values[ToTycoon.Language], [PoliticalWorld.Name] );
      end;
    end;
    
  procedure TPresidentialHall.AutoConnect( loaded : boolean );
    var
      AdvInput : TInput;
    begin
      inherited;
      fWorld := TPoliticalWorld(TInhabitedTown(Facility.Town).World);
      fWorld.fHall := self;
      AdvInput := InputsByName[tidGate_Advertisement];
      if AdvInput <> nil
        then                                             
          begin
            AdvInput.ActualMaxFluid.Q := 0;
            Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Publicity gate patched!' );
          end
        else Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Could not patch Publicity gate!' );
    end;

  procedure TPresidentialHall.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
    begin
      inherited;
    end;

  procedure TPresidentialHall.StoreToCache( Cache : TObjectCache );

    procedure StorePublicFacilitiesCoverage( Cache : TObjectCache );
      var
        i          : integer;
        count      : integer;
        kind       : TMetaPublicFacilityInfo;
        PFCoverage : TPFCoverage;
      begin
        count := TheClassStorage.ClassCount[tidClassFamily_PublicFacilities];
        Cache.WriteInteger( 'covCount', count );
        for i := 0 to pred(count) do
          begin
            kind := TMetaPublicFacilityInfo(TheClassStorage.ClassByIdx[tidClassFamily_PublicFacilities, i]);
            PFCoverage := PoliticalWorld.PublicFacInfo[kind.id];
            Cache.WriteString( 'covName' + IntToStr(i), kind.Name );
            Cache.WriteInteger( 'covValue' + IntToStr(i), PFCoverage.Ratio );
          end;
      end;

    procedure StoreServicesCoverage( Cache : TObjectCache );
      var
        MS    : TMetaService;
        SI    : TServiceCoverage;
        count : integer;
        i     : integer;
        iStr  : string;
      begin
        count := TheClassStorage.ClassCount[tidClassFamily_Services];
        Cache.WriteInteger( 'srvCount', count );
        for i := 0 to pred(count) do
          begin
            MS := TMetaService(TheClassStorage.ClassByIdx[tidClassFamily_Services, i]);
            if MS <> nil
              then
                begin
                  SI   := PoliticalWorld.ServiceInfo[MS.Id];
                  iStr := IntToStr(i);
                  //Cache.WriteString  ( 'svrName'        + iStr, MS.Name );
                  StoreMultiStringToCache( 'svrName' + iStr + '.', MS.Name_MLS, Cache );
                  Cache.WriteCurrency( 'svrMarketPrice' + iStr, MS.MarketPrice );
                  Cache.WriteFloat   ( 'svrDemand'      + iStr, SI.Demand );
                  Cache.WriteFloat   ( 'svrOffer'       + iStr, SI.Offer );
                  Cache.WriteFloat   ( 'svrCapacity'    + iStr, SI.Capacity );
                  Cache.WriteFloat   ( 'svrRatio'       + iStr, SI.Ratio/100 );
                  Cache.WriteInteger ( 'svrQuality'     + iStr, SI.Quality );
                  Cache.WriteInteger ( 'svrPrice'       + iStr, SI.Price );
                end;
          end;
      end;

    var
      i        : integer;
      Minister : TMinister;
      kind     : TPeopleKind;
      TX       : TTownTaxInfo;
      iStr     : string;
    begin
      inherited;
      Cache.WriteInteger( 'MinisterCount', PoliticalWorld.fMinisters.Count );
      for i := 0 to pred(PoliticalWorld.fMinisters.Count) do
        begin
          Minister := TMinister(PoliticalWorld.fMinisters[i]);
          //Cache.WriteString( 'Ministry' + IntToStr(i), Minister.Ministry.Name );
          StoreMultiStringToCache( 'Ministry' + IntToStr(i) + '.', Minister.Ministry.Name_MLS, Cache );
          if Minister.MasterRole <> Minister
            then Cache.WriteString( 'Minister' + IntToStr(i), Minister.MasterRole.Name )
            else Cache.WriteString( 'Minister' + IntToStr(i), '' );
          Cache.WriteInteger( 'MinisterRating' + IntToStr(i), Minister.Rating );
          Cache.WriteInteger( 'MinistryId' + IntToStr(i), Minister.Ministry.MinId );
          Cache.WriteCurrency( 'MinisterBudget' + IntToStr(i), Minister.YearBudget  );
        end;
      for kind := low(kind) to high(kind) do
        begin
          Cache.WriteInteger( PeopleKindPrefix[kind] + 'Population', round(PoliticalWorld.Population[kind]) );
          Cache.WriteInteger( PeopleKindPrefix[kind] + 'WorkForce', round(PeopleToWorkForce(kind, PoliticalWorld.Population[kind])) );
          Cache.WriteInteger( PeopleKindPrefix[kind] + 'Unemployment', PoliticalWorld.Unemployment[kind] );
          Cache.WriteInteger( PeopleKindPrefix[kind] + 'ResDemand', round(PoliticalWorld.ResDemand[kind]) );
          Cache.WriteInteger( PeopleKindPrefix[kind] + 'Salary', PoliticalWorld.AvgSalary[kind] );
          Cache.WriteInteger( PeopleKindPrefix[kind] + 'MinSalary', PoliticalWorld.MinSalary[kind] );
          Cache.WriteInteger( PeopleKindPrefix[kind] + 'WorkDemand', round(PoliticalWorld.WorkDemand[kind]) );
          Cache.WriteInteger( PeopleKindPrefix[kind] + 'RentPrice', PoliticalWorld.ResRent[kind] );
          Cache.WriteInteger( PeopleKindPrefix[kind] + 'ResQ', PoliticalWorld.ResQidx[kind] );
          Cache.WriteInteger( PeopleKindPrefix[kind] + 'SalaryValue', PoliticalWorld.Wealth[kind] );
        end;
      Cache.WriteInteger( 'TownCount', fWorld.fTownTaxes.Count );
      for i := 0 to pred(fWorld.fTownTaxes.Count) do
        begin
          TX := TTownTaxInfo(fWorld.fTownTaxes[i]);
          iStr := IntToStr(i);
          Cache.WriteString( 'Town' + IntToStr(i), TX.Town.Name );
          Cache.WriteInteger( 'TownRating' + iStr, round(100*TTownHall(TInhabitedTown(TX.Town).TownHall.CurrBlock).OverallRating) );
          Cache.WriteInteger( 'TownTax' + iStr, round(100*TX.TaxValue) );
          Cache.WriteInteger( 'TownPopulation' + iStr, round(TTownHall(TInhabitedTown(TX.Town).TownHall.CurrBlock).TotalPopulation) );
          Cache.WriteInteger( 'TownQOL' + iStr, round(100*TTownHall(TInhabitedTown(TX.Town).TownHall.CurrBlock).GQOL) );
          Cache.WriteInteger( 'TownQOS' + iStr, round(100*TTownHall(TInhabitedTown(TX.Town).TownHall.CurrBlock).GQOS) );
          Cache.WriteInteger( 'TownWealth' + iStr, round(100*TTownHall(TInhabitedTown(TX.Town).TownHall.CurrBlock).AvgWealth) );
          Cache.WriteBoolean('HasMayor' + iStr, TX.Town.HasMayor);
        end;
      StorePublicFacilitiesCoverage( Cache );
      StoreServicesCoverage( Cache );
      Cache.WriteInteger( 'ExtraSecurityId', integer(PoliticalWorld.President) );
      Cache.WriteBoolean( 'HasPresident', PoliticalWorld.President.SuperRole <> nil );
      for i := 0 to pred(PoliticalWorld.Ratings.Ratings.Count) do
        UpdateObjectCache(PoliticalWorld.Ratings.Ratings[i], noKind, noInfo); //CacheObject( PoliticalWorld.Ratings.Ratings[i], noKind, noInfo );
      for i := 0 to pred(PoliticalWorld.Campaigns.Campaigns.Count) do
        with TCampaign(PoliticalWorld.Campaigns.Campaigns[i]) do
          begin
            Cache.WriteString( 'Tycoon' + IntToStr(i), Tycoon.Name );
            Cache.WriteInteger( 'Rating' + IntToStr(i), Rating );
            Cache.WriteInteger( 'Prestige' + IntToStr(i), round(Tycoon.Prestige) );
            Cache.WriteString( 'CachePath' + IntToStr(i), GetObjectPath( PoliticalWorld.Campaigns.Campaigns[i], noKind, noInfo ) );
            CacheObject( PoliticalWorld.Campaigns.Campaigns[i], noKind, noInfo );
          end;
      Cache.WriteBoolean( 'HasRuler', PoliticalWorld.President.SuperRole <> nil );
      Cache.WriteString( 'ActualRuler', PoliticalWorld.President.MasterRole.Name );
      Cache.WriteInteger( 'RulerPrestige', round(PoliticalWorld.President.Prestige) );
      if PoliticalWorld.President.MasterRole <> nil
        then Cache.WriteInteger( 'RulerActualPrestige', round(PoliticalWorld.President.MasterRole.Prestige) );
      Cache.WriteInteger( 'YearsToElections', round(PoliticalWorld.YearsToElections) );
      Cache.WriteInteger( 'RulerPeriods', PoliticalWorld.fMandatesInRow + 1); // Mandate No: 0 is not OK
      Cache.WriteInteger( 'RulerRating', PoliticalWorld.Ratings.OverallRating );
      Cache.WriteInteger( 'IFELRating', PoliticalWorld.Ratings.IFELRating );
      Cache.WriteInteger( 'TycoonsRating', PoliticalWorld.Ratings.TycoonsRating );
      Cache.WriteInteger( 'CampaignCount', PoliticalWorld.Campaigns.Campaigns.Count );
      Cache.WriteInteger( 'TownHallId', integer(self) );
      Cache.WriteInteger( 'QOL', PoliticalWorld.GQOL );
      Cache.WriteInteger( 'GQOS', PoliticalWorld.GQOS );
      if (fWorld <> nil) and (fWorld.fPresident <> nil)
        then Cache.WriteString( 'SecurityId', fWorld.fPresident.SecurityId )
        else Cache.WriteString( 'SecurityId', '0');
      // New election system
      fWorld.Campaigns.StoreVotesToCache(Cache);
      if (PoliticalWorld.getWinningCampaign <> nil) and (PoliticalWorld.getWinningCampaign.Tycoon <> nil) and (succ(PoliticalWorld.fMandatesInRow) < MaxPresPeriods)
        then
          begin
            Cache.WriteString('RulerName', PoliticalWorld.getWinningCampaign.Tycoon.Name);
            Cache.WriteInteger('RulerVotes', PoliticalWorld.getWinningCampaign.CurrentVotes);
            Cache.WriteInteger('RulerCmpRat', PoliticalWorld.getWinningCampaign.Rating);
            Cache.WriteInteger('RulerCmpPnts', round(PoliticalWorld.getWinningCampaign.GetStrength));
          end;
    end;

  procedure TPresidentialHall.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fWorld := TPoliticalWorld(TInhabitedTown(Facility.Town).World);
    end;

  procedure TPresidentialHall.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
    end;

  function TPresidentialHall.RDOGetRatingFrom( RatingId : widestring; TycoonId : widestring ) : olevariant;
    var
      Tycoon : TTycoon;
      Rating : TRating;
      Survey : TSurvey;
    begin
      try
        Tycoon := PoliticalWorld.TycoonByName[TycoonId];
        Rating := PoliticalWorld.Ratings[RatingId];
        if (Rating <> nil) and (Tycoon <> nil)
          then
            begin
              Survey := Rating.Survey[Tycoon];
              if Survey <> nil
                then result := Survey.Opinion
                else result := -1;
            end
          else result := -1
      except
        result := -1;
      end;
    end;

  procedure TPresidentialHall.RDOSetRatingFrom( RatingId : widestring; TycoonId : widestring; Value : integer );
    var
      Tycoon : TTycoon;
      Rating : TRating;
      Survey : TSurvey;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Setting world politics Tycoon rating: ' + TycoonId + ', ' + RatingId + ', ' + IntToStr(Value) );
      try
        Tycoon := PoliticalWorld.TycoonByName[TycoonId];
        if (Tycoon <> nil) and (Tycoon.MasterRole <> PoliticalWorld.President.MasterRole)
          then
            begin
              Rating := PoliticalWorld.Ratings[RatingId];
              if Rating <> nil
                then
                  begin
                    Survey := Rating.Survey[Tycoon];
                    if Survey = nil
                      then
                        begin
                          Survey := TSurvey.Create( Tycoon );
                          Rating.Surveys.Insert( Survey );
                        end;
                    Survey.Opinion := Value;
                    Rating.Evaluate;
                    ModelServerCache.BackgroundInvalidateCache(Rating);   //CacheObject( Rating, noKind, noInfo )
                    ModelServerCache.BackgroundInvalidateCache(Facility); //CacheObject( Facility, noKind, noInfo )
                  end;
            end;
      except
      end;
      Logs.Log( tidLog_Survival,  'OK!');
    end;

  procedure TPresidentialHall.RDOSetPublicity( RatingId : widestring; Value : integer );
    var
      Rating : TRating;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Setting world politics publicity: ' + RatingId + ', ' + IntToStr(Value) );
      try
        Rating := PoliticalWorld.Ratings[RatingId];
        if Rating <> nil
          then
            begin
              Rating.RulerPublicity := Value;
              Rating.Evaluate;
              ModelServerCache.BackgroundInvalidateCache(Rating);   //CacheObject( Rating, noKind, noInfo )
              ModelServerCache.BackgroundInvalidateCache(Facility); //CacheObject( Facility, noKind, noInfo )
            end;
      except
      end;
      Logs.Log( tidLog_Survival,  'OK!');
    end;

  function TPresidentialHall.TycoonIsAvailable( Tycoon : TTycoon ) : boolean;
    begin
      {$IFNDEF DebugPolitics}
      result := (Tycoon.MasterRole = Tycoon) and (Tycoon.Roles.Count = 0) and TycoonHasNoCampaign(Tycoon) and not Tycoon.IsRole
      {$ELSE}
      result := true;
      {$ENDIF}
    end;

  function TPresidentialHall.TycoonHasNoCampaign( Tycoon : TTycoon ) : boolean;
    {$IFNDEF DebugPolitics}
    var
      i : integer;
    {$ENDIF}
    begin
      {$IFNDEF DebugPolitics}
      if PoliticalWorld.TycoonHasCampaign(Tycoon)
        then result := false
        else
          begin
            PoliticalWorld.Towns.Lock;
            try
              i := 0;
              while (i < PoliticalWorld.Towns.Count) and (TPoliticalTownHall(TPoliticalTown(PoliticalWorld.Towns[i]).TownHall.CurrBlock).Campaigns[Tycoon] = nil) do
                inc( i );
            finally
              PoliticalWorld.Towns.Unlock;
            end;
            result := i = PoliticalWorld.Towns.Count
          end;
      {$ELSE}
      result := true;
      {$ENDIF}
    end;

  function TPresidentialHall.RDOLaunchCampaign( TycoonId : widestring ) : olevariant;
    var
      Tycoon  : TTycoon;
      Campaign : TCampaign;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Launching President campaign: ' + TycoonId );
      try
        if (PoliticalWorld.YearsToElections >= PeriodLength div 2) and PoliticalWorld.ElectionsOn
          then
            begin
              Tycoon := PoliticalWorld.TycoonByName[TycoonId];
              if Tycoon <> nil
                then
                  if TycoonIsAvailable(Tycoon) and (Tycoon.Companies.Count > 0)
                    then
                      {$IFNDEF DebugPolitics}
                      if Tycoon.Prestige >= MinPresidentPrestige
                      {$ELSE}
                      if true
                      {$ENDIF}
                        then
                          begin
                            Campaign := PoliticalWorld.Campaigns[Tycoon];
                            if Campaign = nil
                              then
                                begin
                                  Campaign := TCampaign.Create( Tycoon, PoliticalWorld.Campaigns, tidClassFamily_WorldProjects );
                                  PoliticalWorld.Campaigns.Campaigns.Insert( Campaign );
                                  PoliticalWorld.Campaigns.Campaigns.Sort( PoliticalWorld.Campaigns.CompareCampaigns );
                                  CacheObject( Facility, noKind, noInfo ); // >> Cache_INV ??
                                  Facility.Town.WorldLocator.SendEvent(
                                    TEvent.Create(
                                      0,
                                      Facility.Town.Timer.GetVirtualTimeAbs,
                                      Facility.Town.Timer.GetVirtualTime,
                                      200000,
                                      20000,
                                      InstantiateMultiString( mtidPresidenCampaignLaunched, [Tycoon.Name, Facility.Town.WorldLocator.GetWorldName] ),
                                      '',
                                      'Visual/Voyager/Politics/politics.asp?Capitol=YES&x=' + IntToStr(Facility.xPos) + '&y=' + IntToStr(Facility.yPos) +
                                      '&frame_Id=PoliticsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client' ))
                                end;
                            result := NOERROR;
                          end
                        else result := ERROR_POLITICS_REJECTED
                    else result := ERROR_POLITICS_NOTALLOWED
                else result := NOERROR;
            end
          else result := ERROR_POLITICS_NOTIME;
      except
        result := ERROR_Unknown;
      end;
      Logs.Log( tidLog_Survival,  'OK!');
    end;

  procedure TPresidentialHall.RDOCancelCampaign( TycoonId : widestring );
    var
      Tycoon   : TTycoon;
      Campaign : TCampaign;
      rdoId    : integer;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Cancel President campaign: ' + TycoonId );
      try
        Tycoon := PoliticalWorld.TycoonByName[TycoonId];
        if Tycoon <> nil
          then
            begin
              Campaign := PoliticalWorld.Campaigns[Tycoon];
              if Campaign <> nil
                then
                  begin
                    rdoId := Campaign.System.PoliticalEntity.getRDOId;
                    PoliticalWorld.ReportWithdrawal(TObject(rdoId), Tycoon);
                    ModelServerCache.BackgroundUncache(Campaign); //UncacheObject( Campaign, noKind, noInfo )
                    PoliticalWorld.Campaigns.Campaigns.Delete( Campaign );
                    Facility.Town.WorldLocator.SendEvent(
                      TEvent.Create(
                        0,
                        Facility.Town.Timer.GetVirtualTimeAbs,
                        Facility.Town.Timer.GetVirtualTime,
                        500,
                        20000,
                        InstantiateMultiString( mtidPresidenCampaignCancelled, [Tycoon.Name, Facility.Town.WorldLocator.GetWorldName] ),
                        '',
                        'Visual/Voyager/Politics/politics.asp?Capitol=YES&x=' + IntToStr(Facility.xPos) + '&y=' + IntToStr(Facility.yPos) +
                        '&frame_Id=PoliticsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client' ));
                    CacheObject( Facility, noKind, noInfo ); // >> Cache_INV ??
                  end;
            end;
      except
      end;
      Logs.Log( tidLog_Survival,  'OK!');
    end;

  procedure TPresidentialHall.RDOSetProjectData( TycoonId : widestring; ProjectId, Data : widestring );
    var
      Tycoon   : TTycoon;
      Campaign : TCampaign;
      Project  : TProject;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Setting President campaign data: ' + TycoonId + ', ' + ProjectId + ', ' + Data );
      try
        Tycoon := PoliticalWorld.TycoonByName[TycoonId];
        if Tycoon <> nil
          then
            begin
              Campaign := PoliticalWorld.Campaigns[Tycoon];
              if Campaign <> nil
                then
                  begin
                    Project := Campaign.Project[ProjectId];
                    if Project <> nil
                      then
                        begin
                          Project.ParseData( Data );
                          ModelServerCache.BackgroundInvalidateCache(Project); //CacheObject( Project, noKind, noInfo )
                          Campaign.Evaluate;
                          ModelServerCache.BackgroundCache(Facility, false); //CacheObject( Facility, noKind, noInfo )
                        end;
                  end;
            end;
      except
      end;
      Logs.Log( tidLog_Survival, 'OK!' );
    end;

  procedure TPresidentialHall.RDOSetMinistryBudget( MinistryId : integer; Budget : widestring );
    var
      Min   : TMinister;
      value : currency;
    begin
      Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Setting Ministry Budget.' );
      try
        Min   := fWorld.Ministers[MinistryId];
        value := StrToCurr( Budget );
        if (value > 0) and (Min <> nil) and (fWorld <> nil) and (fWorld.President <> nil) and fWorld.President.CheckOpAuthenticity
          then
            begin
              Min.YearBudget := value;
              ModelServerCache.BackgroundInvalidateCache(Facility); //CacheObject( Facility, noKind, noInfo )
            end;
      except
      end;
    end;

  procedure TPresidentialHall.RDOSitMinister(MinistryId : integer; name : widestring);
    function CanBeMinister(Tycoon : TTycoon) : boolean;
      var
        i : integer;
      begin
        result := Tycoon <> fWorld.President.MasterRole;
        i := pred(Tycoon.Roles.Count);
        while (i >= 0) and result do
          begin
            result := not ObjectIs(TMinister.ClassName, Tycoon.Roles[i]);
            dec(i);
          end;
      end;
    var
      Min    : TMinister;
      Tycoon : TTycoon;
    begin
      Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Sitting Minister.' );
      try
        Min := fWorld.Ministers[MinistryId];
        Tycoon := fWorld.TycoonByName[name];
        if (fWorld <> nil) and (Min <> nil) and
           (Tycoon <> nil) and not Tycoon.IsRole and CanBeMinister(Tycoon) and
           (fWorld.President <> nil) and fWorld.President.CheckOpAuthenticity
          then
            begin
              Min.AbandomRoles;
              Tycoon.AssumeRole(Min);
              ModelServerCache.BackgroundCache(Min, false);
              ModelServerCache.BackgroundCache(Tycoon, false);
              Facility.Town.MapRefresh.RefeshFacility(Facility, fchStructure);
              fWorld.SendMinisterNotification(Min);
            end;
      except
        Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Error Sitting Minister.' );
      end;
    end;

  procedure TPresidentialHall.RDOBanMinister(MinistryId : integer);
    var
      Min : TMinister;
    begin
      Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Banning Minister.' );
      try
        Min := fWorld.Ministers[MinistryId];
        if (Min <> nil) and (fWorld <> nil) and (fWorld.President <> nil) and fWorld.President.CheckOpAuthenticity
          then
            begin
              Min.AbandomRoles;
              Facility.Town.MapRefresh.RefeshFacility(Facility, fchStructure);
            end;
      except
        Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Error Banning Minister.' );
      end;
    end;

  procedure TPresidentialHall.RDOSetTownTaxes( Index, Value : integer );
    var
      TX : TTownTaxInfo;
    begin
      Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Setting Town Taxes.' );
      try
        if (Index < fWorld.fTownTaxes.Count) and (fWorld <> nil) and (fWorld.President <> nil) and fWorld.President.CheckOpAuthenticity
          then
            begin
              TX := TTownTaxInfo(fWorld.fTownTaxes[Index]);
              TX.TaxValue := Value/100;
              ModelServerCache.BackgroundInvalidateCache(Facility); //CacheObject( Facility, noKind, noInfo )
            end;
      except
      end;
    end;

  procedure TPresidentialHall.RDOSetMinSalaryValue( PopKind, Value : integer );
    begin
      Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Setting Ministry Salary.' );
      try
        if (fWorld <> nil) and (fWorld.President <> nil) and fWorld.President.CheckOpAuthenticity
          then
            begin
              fWorld.MinSalary[TPeopleKind(PopKind)] := Value;
              ModelServerCache.BackgroundInvalidateCache(Facility); //CacheObject( Facility, -1, -1 )
            end;
      except
      end;
    end;

  procedure TPresidentialHall.RDOSitMayor(TownName, TycoonName : widestring);
    const
      MinBudget = 10000000;
    var
      Mayor  : TTycoon;
      Tycoon : TTycoon;
      Town   : TPoliticalTown;
    begin
      Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Sitting Mayor.' );
      try
        if (fWorld <> nil) and (fWorld.President <> nil) and fWorld.President.CheckOpAuthenticity
          then
            begin
              Town  := TPoliticalTown(fWorld.TownByName[TownName]);
              Mayor := fWorld.TycoonByName['Mayor of ' + TownName];
              if (Mayor <> nil) and (Mayor.SuperRole = nil) and (Town <> nil) and (Mayor.Budget >= MinBudget)
                then
                  begin
                    Tycoon := fWorld.TycoonByName[TycoonName];
                    if (Tycoon <> nil) and (Tycoon.Roles.Count = 0) and TycoonIsAvailable(Tycoon)
                      then
                        begin
                          Tycoon.AssumeRole(Mayor);
                          TPoliticalTownHall(Town.TownHall.CurrBlock).SitMayor(Tycoon);
                        end;
                  end;
            end;
      except
        Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Error Sitting Mayor.' );
      end;
    end;

  procedure TPresidentialHall.RDOVote(voterTycoon, choiceTycoon : widestring);
    var
      Voter, Votee : TTycoon;
    begin
      Logs.Log( tidLog_Survival, DateTimeToStr(Now) + Format(' Voting: %s by %s', [voterTycoon, choiceTycoon]));
      try
        Voter := fWorld.TycoonByName[voterTycoon];
        Votee := fWorld.TycoonByName[choiceTycoon];
        if (Voter <> nil) and (Voter.AllCompaniesCount > 0) and not Voter.IsRole and (Votee <> nil) and Voter.CheckOpAuthenticity and Voter.PayTaxes
          then
            begin
              fWorld.Campaigns.VoteFor(Voter, Votee);
              fWorld.Campaigns.Evaluate;
              ModelServerCache.InvalidateCache(Facility, false);
              Facility.Town.MapRefresh.RefeshFacility(Facility, fchStructure);
            end;
      except
        Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' error in RDOVote..');
      end;
    end;

  function TPresidentialHall.RDOVoteOf(tycoonName : widestring) : OleVariant;
    var
      WL     : IWorldLocator;
      Tycoon : TTycoon;
      Choice : TTycoon;
    begin
      try
        WL := Facility.Town.WorldLocator;
        Tycoon := WL.GetTycoonByName(tycoonName);
        if Tycoon <> nil
          then Choice := fWorld.Campaigns.VoteOf(Tycoon)
          else Choice := nil;
        if Choice <> nil
          then result := Choice.Name
          else result := '';
      except
        result := '';
      end;
    end;


  // Register Capitol

  const
    tidBlock_CapitolConstr = 'CapitolConstr';
    tidBlock_Capitol       = 'Capitol';
    tidFacility_Capitol    = 'Capitol';
    vidFacility_Capitol    = 151;

  procedure RegisterCapitol;
    begin
      {
      with TFacilityKind.Create( tidFacilityKind_WorldPolitics ) do
        begin
          Name        := 'Headquarters';
          SuperType   := tidSuperFacKind_Headquarter;
          ClusterName := tidClusterName_PGI;
          Register( tidClassFamily_FacilityKinds );
        end;
      }
      with TFacilityKind.Create( tidFacilityKind_Capitol ) do
        begin
          Name        := 'Capitol';
          SuperType   := tidSuperFacKind_Capitol;
          Role        := rolNeutral;
          Cacheable   := false;
          Register( tidClassFamily_FacilityKinds );
        end;
      with TMetaBlockUnderConstruction.Create(
        tidBlock_CapitolConstr,
        10000000,
        [100, 0, 0],
        7,
        TBlockUnderConstruction ) do
        begin
          Register( tidClassFamily_Blocks );
        end;
      with TMetaPresidentialHall.Create(
        tidBlock_Capitol,
        TPresidentialHall ) do
        begin
          Beauty := 1000;
          Register( tidClassFamily_Blocks );
        end;
      with TMetaFacility.Create( tidFacility_Capitol, 'Capitol', vidFacility_Capitol, TFacility ) do
        begin
          XSize   := 7;
          YSize   := 7;
          Level   := 10;
          FacId   := 0;
          Options := Options - [mfcGenerateName, mfcShowCompanyInText, mfcShowProfitInText, mfcIgnoreZoning];
          EvlStages.Insert( TEvlStage.Create( 'Construction', 'Construction', 'Requires construction materials to advance to the next stage.', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_CapitolConstr])));
          EvlStages.Insert( TEvlStage.Create( 'Complete', 'Building Completed', 'Building is ready', TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, tidBlock_Capitol])));
          FacilityKind := tidFacilityKind_Capitol;
          Register( tidClassFamily_Facilities );
        end;
    end;


  // RegisterPolitics

  procedure RegisterPolitics;

    procedure RegisterPublicParmRatings;
      var
        count, i : integer;
        MPFI     : TMetaPublicFacilityInfo;
      begin
        count := TheClassStorage.ClassCount[tidClassFamily_PublicFacilities];
        for i := 0 to pred(count) do
          begin
            MPFI := TMetaPublicFacilityInfo(TheClassStorage.ClassByIdx[tidClassFamily_PublicFacilities, i]);
            TMetaPublicParmRating.Create(
              MPFI.Id,
              MPFI.Name,
              MPFI.Id,
              MPFI.Importance,
              TPublicParmRating ).Register( tidClassFamily_WorldRatings );
          end;
      end;

    procedure RegisterPublicParmProjects;
      var
        count, i : integer;
        MPFI     : TMetaPublicFacilityInfo;
      begin
        count := TheClassStorage.ClassCount[tidClassFamily_PublicFacilities];
        for i := 0 to pred(count) do
          begin
            MPFI := TMetaPublicFacilityInfo(TheClassStorage.ClassByIdx[tidClassFamily_PublicFacilities, i]);
            TMetaPublicParmProject.Create(
              MPFI.Id,
              MPFI.Name,
              MPFI.Id,
              MPFI.Importance,
              cmdGreaterThan,
              TPublicParmProject ).Register( tidClassFamily_WorldProjects );
          end;
      end;

    begin
      // Ratings
      TMetaCampaignAccuracyRating.Create(
        tidRating_CampaignAccuracy,
        'Campaign Acomplishment',
        200,
        TCampaignAccuracyRating ).Register( tidClassFamily_WorldRatings );
      RegisterPublicParmRatings;
      TMetaPopGrowthRating.Create(
        tidRating_PopGrowth,
        'World Growth',
        70,
        TPopGrowthRating ).Register( tidClassFamily_WorldRatings );
      TMetaUnemploymentRating.Create(
        tidRating_Unemployment,
        'Employment',
        100,
        TUnemploymentRating ).Register( tidClassFamily_WorldRatings );
      TMetaServicesRating.Create(
        tidRating_Services,
        'Services and Amusement',
        70,
        TServicesRating ).Register( tidClassFamily_WorldRatings );
      TMetaWealthRating.Create(
        tidRating_Wealth,
        'Economic Wealth',
        80,
        TWealthRating ).Register( tidClassFamily_WorldRatings );
      TMetaTaxesRating.Create(
        tidRating_Taxes,
        'Taxes',
        60,
        TTaxesRating ).Register( tidClassFamily_WorldRatings );

      // Projects
      RegisterPublicParmProjects;
      TMetaServicesProject.Create(
        tidProject_Services,
        'Commerce and Amusement',
        100,
        cmdGreaterThan,
        TServicesProject ).Register( tidClassFamily_WorldProjects );
      TMetaWealthProject.Create(
        tidProject_Wealth,
        'Economic Wealth',
        100,
        cmdGreaterThan,
        TWealthProject ).Register( tidClassFamily_WorldProjects );
      TMetaUnemploymentProject.Create(
        tidProject_Unemployment,
        'Unemployment',
        200,
        cmdLessThan,
        TUnemploymentProject ).Register( tidClassFamily_WorldProjects );
    end;

  procedure PostRegisterPolitics;

    procedure RegisterMinisterRatings;
      var
        count, i : integer;
        MetaMin  : TMinistry;
      begin
        count := TheClassStorage.ClassCount[tidClassFamily_Ministries];
        for i := 0 to pred(count) do
          begin
            MetaMin := TMinistry(TheClassStorage.ClassByIdx[tidClassFamily_Ministries, i]);
            TMetaMinisterRating.Create(
              MetaMin.Id,
              MetaMin.Name_MLS,
              100,
              MetaMin.MinId,
              TMinisterRating ).Register( tidClassFamily_WorldRatings );
          end;
      end;

    procedure RegisterMinisterProjects;
      var
        count, i : integer;
        MetaMin  : TMinistry;
      begin
        count := TheClassStorage.ClassCount[tidClassFamily_Ministries];
        for i := 0 to pred(count) do
          begin
            MetaMin := TMinistry(TheClassStorage.ClassByIdx[tidClassFamily_Ministries, i]);
            TMetaMinisterProject.Create(
              MetaMin.Id,
              tidProjectKind_Minister,
              MetaMin.Name_MLS,
              100,
              MetaMin.MinId,
              TMinisterProject ).Register( tidClassFamily_WorldProjects );
          end;
      end;

    begin
      RegisterMinisterRatings;
      RegisterMinisterProjects;
    end;

  type
    TPoliticalWorldCacheAgent =
      class( TCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;       override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    class function TPoliticalWorldCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;
      begin
        result := 'world.five';
      end;

    class function TPoliticalWorldCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
      begin
        result := inherited GetCache( Obj, kind, info, update );
        with TPoliticalWorld( Obj ) do
          begin
            result.WriteBoolean('TournamentOn', TournamentLength > 0);
            result.WriteBoolean( 'ElectionsOn', ElectionsOn );
            result.WriteBoolean( 'SendingMoneyOn', SendingMoneyOn );
            result.WriteBoolean( 'AlliesPageOn', AlliesPageOn );
            result.WriteBoolean( 'TranscendingOn', TranscendingOn );
            result.WriteBoolean( 'HasCapitol', Hall <> nil );
            result.WriteString( ppTTL, CreateTTL( 0, 0, 15, 0 ));
            if Hall <> nil
              then
                begin
                  result.WriteInteger( 'CapitolX', Hall.xPos );
                  result.WriteInteger( 'CapitolY', Hall.yPos );
                end;
          end;
      end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TMinisterProject );
      RegisterClass( TMinisterRating );
      RegisterClass( TMinister );
      RegisterClass( TPresident );
      RegisterClass( TTownTaxInfo );
      RegisterClass( TPoliticalWorld );
      RegisterClass( TPresidentialHall );

      // Register Cacher (cogiendo botella...)
      RegisterCacher( TPoliticalWorld.ClassName, TPoliticalWorldCacheAgent );
    end;

end.


