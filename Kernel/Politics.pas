unit Politics;

interface

  uses
    MetaInstances, Kernel, Collection, Population, BackupInterfaces, Persistent,
    SyncObjs, CacheAgent, Languages, Variants;

  const
    tidClassFamily_Ratings  = 'Ratings';
    tidClassFamily_Projects = 'Projects';

  const
    PeriodLength = 40;

  const
    PersonalVotesWeight = 0.5;

  type
    TPoliticalParmId = integer;

  const
    plidLastAdsQ     = 1;
    plidLastAdsK     = 2;
    plidRatingEthics = 3;

  type
    TSurvey         = class;
    TMetaRating     = class;
    TRating         = class;
    TRatingSystem   = class;
    TMetaProject    = class;
    TProject        = class;
    TCampaign       = class;
    TCampaignSystem = class;

    CRating  = class of TRating;
    CProject = class of TProject;

    IPoliticalEntity =
      interface
        function getWinningCampaign : TCampaign;
        function getParm( id : TPoliticalParmId; const info ) : single;
        function getCacheFolder : string;
        function getRDOId       : integer;
      end;

    TSurvey =
      class( TPersistent )
        public
          constructor Create( aTycoon : TTycoon );
        private
          fTycoon  : TTycoon;
          fOpinion : TPercent;
        public
          property Tycoon  : TTycoon  read fTycoon;
          property Opinion : TPercent read fOpinion write fOpinion;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TMetaRating =
      class( TMetaInstance )
        public
          constructor Create( anId : string; aName : string; aWeight : integer; aRatingClass : CRating );
        private
          fName        : string;
          fName_MLS    : TMultiString;
          fRatingClass : CRating;
          fWeight      : integer;
        public
          property Name_MLS    : TMultiString read fName_MLS;
          property RatingClass : CRating      read fRatingClass;
          property Weight      : integer      read fWeight;
        public
          procedure RetrieveTexts( Container : TDictionary ); override;
          procedure StoreTexts   ( Container : TDictionary ); override;
      end;

    TRating =
      class( TPersistent )
        public
          constructor Create( aSystem : TRatingSystem; aMetaRating : TMetaRating ); virtual;
          destructor  Destroy; override;
        private
          fSystem  : TRatingSystem;
          fSurveys : TLockableCollection;
        protected
          fMetaRating     : TMetaRating;
          fPeopleRating   : TPercent;
          fIFELRating     : TPercent;
          fTycoonsRating  : TPercent;
          fRulerPublicity : TPercent;
        public
          property System         : TRatingSystem       read fSystem;
          property MetaRating     : TMetaRating         read fMetaRating;
          property PeopleRating   : TPercent            read fPeopleRating;
          property IFELRating     : TPercent            read fIFELRating;
          property TycoonsRating  : TPercent            read fTycoonsRating;
          property RulerPublicity : TPercent            read fRulerPublicity write fRulerPublicity;
          property Surveys        : TLockableCollection read fSurveys;
        private
          function GetSurvey( Tycoon : TTycoon ) : TSurvey;
        public
          property Survey[Tycoon : TTycoon] : TSurvey read GetSurvey;
        protected
          procedure ComputeIFELRating;    virtual;
          procedure ComputeTycoonsRating; virtual;
          procedure ComputePeopleRating;  virtual;
        public
          procedure Evaluate; virtual;
          procedure TycoonDeleted( Tycoon : TTycoon ); virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure Loaded;
      end;

    TRatingSystem =
      class( TPersistent )
        public
          constructor Create( aPoliticalEntity : IPoliticalEntity; aRatingFamily : string );
          destructor  Destroy; override;
        private
          fRatingFamily    : string;
          fPoliticalEntity : IPoliticalEntity;
          fRatings         : TLockableCollection;
        public
          property RatingFamily    : string              read fRatingFamily    write fRatingFamily;
          property PoliticalEntity : IPoliticalEntity    read fPoliticalEntity write fPoliticalEntity;
          property Ratings         : TLockableCollection read fRatings;
        private
          function GetRating( Id : string ) : TRating;
        public
          property Rating[Id : string] : TRating read GetRating; default;
        public
          procedure Evaluate; virtual;
          procedure Clear;    virtual;
        private
          fOverallRating : TPercent;
          fIFELRating    : TPercent;
          fTycoonsRating : TPercent;
        public
          property OverallRating : TPercent read fOverallRating;
          property IFELRating    : TPercent read fIFELRating;
          property TycoonsRating : TPercent read fTycoonsRating;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure ReCache;
          procedure TycoonDeleted( Tycoon : TTycoon ); virtual;
          procedure Loaded;
      end;

    TMetaProject =
      class( TMetaInstance )
        public
          constructor Create( anId, aName, aKind : string; aWeight : integer; aProjectClass : CProject );
        private
          fKind         : string;
          fName         : string;
          fName_MLS     : TMultiString;
          fProjectClass : CProject;
          fWeight       : integer;
        public
          property Kind         : string       read fKind;
          property Name_MLS     : TMultiString read fName_MLS;
          property ProjectClass : CProject     read fProjectClass;
          property Weight       : integer      read fWeight;
        public
          procedure RetrieveTexts( Container : TDictionary ); override;
          procedure StoreTexts   ( Container : TDictionary ); override;
      end;

    TProject =
      class( TPersistent )
        public
          constructor Create( aCampaign : TCampaign; aMetaProject : TMetaProject );
        private
          fMetaProject : TMetaProject;
          fCampaign    : TCampaign;
        protected
          function GetRating   : TPercent; virtual;
          function GetAccuracy : TPercent; virtual;
        public
          property MetaProject : TMetaProject read fMetaProject;
          property Campaign    : TCampaign    read fCampaign;
          property Rating      : TPercent     read GetRating;
          property Accuracy    : TPercent     read GetAccuracy;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure ParseData( Data : string ); virtual;
          procedure SetDefaultValue; virtual;
        public
          procedure StoreToCache( Cache : TObjectCache ); virtual;
      end;

    TCampaign =
      class( TPersistent )
        public
          constructor Create( aTycoon : TTycoon; aSystem : TCampaignSystem; aProjectsFamily : string; cache : boolean = true );
          destructor  Destroy; override;
        private
          fTycoon         : TTycoon;
          fSystem         : TCampaignSystem;
          fProjectsFamily : string;
          fProjects       : TLockableCollection;
          fRating         : TPercent;
          fAccuracy       : TPercent;
          fCurrentVotes   : integer;
        public
          procedure Evaluate; virtual;
        public
          property Tycoon         : TTycoon             read fTycoon write fTycoon;
          property System         : TCampaignSystem     read fSystem;
          property ProjectsFamily : string              read fProjectsFamily write fProjectsFamily;
          property Projects       : TLockableCollection read fProjects;
          property Rating         : TPercent            read fRating;
          property Accuracy       : TPercent            read fAccuracy;
          property CurrentVotes   : integer             read fCurrentVotes write fCurrentVotes;
        private
          function GetProject( Id : string ) : TProject;
        public
          property Project[Id : string] : TProject read GetProject; default;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure AddVotes(count : integer);
          function  GetStrength : single;
      end;

    TCampaignSystem =
      class( TPersistent )
        public
          constructor Create( aPoliticalEntity : IPoliticalEntity );
          destructor  Destroy; override;
        private
          fPoliticalEntity : IPoliticalEntity;
          fCampaigns       : TSortedCollection;
        public
          property PoliticalEntity : IPoliticalEntity  read fPoliticalEntity write fPoliticalEntity;
          property Campaigns       : TSortedCollection read fCampaigns;
        private
          function GetCampaign( Tycoon : TTycoon ) : TCampaign;
        public
          property Campaign[Tycoon : TTycoon] : TCampaign read GetCampaign; default;
        public
          procedure Evaluate; virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        private
          fLock : TCriticalSection;
        public
          function  CompareCampaigns( Item1, Item2 : TObject ) : integer;
          procedure TycoonDeleted( Tycoon : TTycoon ); virtual;
        public
          procedure ReCache;
          procedure StoreVotesToCache(Cache : TObjectCache);
        public
          procedure VoteFor(Voter, Votee : TTycoon);
          function  VoteOf(Tycoon : TTycoon) : TTycoon;
          function  IsTheBestCampaign(Camp : TCampaign) : boolean;
          procedure Loaded;
      end;


  // RegisterBackup

  procedure RegisterBackup;

implementation

  uses
    SysUtils, ClassStorage, MathUtils, Protocol, ModelServerCache, BasicCurriculum, SimHints, World, Logs;


  // TSurvey

  constructor TSurvey.Create( aTycoon : TTycoon );
    begin
      inherited Create;
      fTycoon  := aTycoon;
      fOpinion := 100;
    end;

  procedure TSurvey.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Tycoon', fTycoon, nil );
      fOpinion := Reader.ReadByte( 'Opinion', 100 ); 
    end;

  procedure TSurvey.StoreToBackup ( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObjectRef( 'Tycoon', fTycoon );
      Writer.WriteByte( 'Opinion', fOpinion );
    end;


  // TMetaRating

  constructor TMetaRating.Create( anId : string; aName : string; aWeight : integer; aRatingClass : CRating );
    begin
      inherited Create( anId );
      fName        := aName;
      fName_MLS    := TMultiString.Create;
      fRatingClass := aRatingClass;
      fWeight      := aWeight;
    end;

  procedure TMetaRating.RetrieveTexts( Container : TDictionary );
    begin
      if fName_MLS = nil
        then fName_MLS := TMultiString.Create;
      fName_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Name'];
    end;

  procedure TMetaRating.StoreTexts( Container : TDictionary );
    begin
      Container.Values[Family + '.' + Id + '.' + 'Name'] := fName;
    end;


  // TRating

  constructor TRating.Create( aSystem : TRatingSystem; aMetaRating : TMetaRating );
    begin
      inherited Create;
      fSystem     := aSystem;
      fMetaRating := aMetaRating;
      fSurveys    := TLockableCollection.Create( 0, rkBelonguer );
    end;

  destructor TRating.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fSurveys.Free;
      inherited;
    end;

  function TRating.GetSurvey( Tycoon : TTycoon ) : TSurvey;
    var
      i : integer;
    begin
      Surveys.Lock;
      try
        i := 0;
        while (i < Surveys.Count) and (TSurvey(Surveys[i]).Tycoon <> Tycoon) do
          inc( i );
        if i < Surveys.Count
          then result := TSurvey(Surveys[i])
          else result := nil;
      finally
        Surveys.Unlock;
      end;
    end;

  procedure TRating.ComputeIFELRating;
    begin
      fIFELRating := 100;
    end;

  procedure TRating.ComputeTycoonsRating;
    var
      i        : integer;
      Opinions : single;
      Count    : single;
    begin
      Surveys.Lock;
      try
        Opinions := 0;
        Count    := 0;
        for i := 0 to pred(Surveys.Count) do
          with TSurvey(Surveys[i]) do
            begin
              Opinions := Opinions + (Tycoon.Prestige + 1)*System.PoliticalEntity.getParm( plidRatingEthics, Tycoon )*Opinion;
              Count    := Count + (Tycoon.Prestige + 1);
            end;
        if Count > 0
          then fTycoonsRating := round(Opinions/Count)
          else fTycoonsRating := fIFELRating;
      finally
        Surveys.Unlock;
      end;
    end;

  procedure TRating.ComputePeopleRating;
    const
      IFELWeight    = 30;
      TycoonsWeight = 50;
      RulerWeight   = 20;
    const
      PubToRating = 100;
    var
      LastAdsQ    : single;
      LastAdsK    : single;
      AdsRating   : TPercent;
      NoPubRating : TPercent;
      PubRating   : TPercent;
    begin
      LastAdsQ := System.PoliticalEntity.getParm( plidLastAdsQ, self );
      LastAdsK := System.PoliticalEntity.getParm( plidLastAdsK, self );

      AdsRating := min( 100, round(LastAdsQ*LastAdsK/(100*100*PubToRating)) );
      NoPubRating := (IFELWeight*fIFELRating + TycoonsWeight*fTycoonsRating) div (IFELWeight + TycoonsWeight);
      PubRating := (IFELWeight*fIFELRating + TycoonsWeight*fTycoonsRating + RulerWeight*AdsRating) div 100;
      fPeopleRating := max( PubRating, NoPubRating );
    end;

  procedure TRating.Evaluate;
    begin
      ComputeIFELRating;
      ComputeTycoonsRating;
      ComputePeopleRating;
    end;

  procedure TRating.TycoonDeleted( Tycoon : TTycoon );
    var
      S : TSurvey;
    begin
      S := Survey[Tycoon];
      while S <> nil do
        begin
          Surveys.Delete(S);
          S := Survey[Tycoon];
        end;
    end;
    
  procedure TRating.LoadFromBackup( Reader : IBackupReader );
    var
      MetaRatingId     : string;
      MetaRatingFamily : string;
    begin
      inherited;
      Reader.ReadObject( 'System', fSystem, nil );
      MetaRatingId     := Reader.ReadString( 'MetaRatingId', '' );
      MetaRatingFamily := Reader.ReadString( 'MetaRatingFamily', tidClassFamily_Ratings );
      fMetaRating      := TMetaRating(TheClassStorage.ClassById[MetaRatingFamily, MetaRatingId]);
      fPeopleRating    := Reader.ReadByte( 'PeopleRating', 0 );
      fIFELRating      := Reader.ReadByte( 'IFELRating', 0 );
      fTycoonsRating   := Reader.ReadByte( 'TycoonsRating', 0 );
      fRulerPublicity  := Reader.ReadByte( 'RulerPublicity', 0 );
      Reader.ReadObject( 'Surveys', fSurveys, nil );
      if fSurveys = nil
        then fSurveys := TLockableCollection.Create( 0, rkBelonguer );
    end;

  procedure TRating.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObjectRef( 'System', fSystem );
      Writer.WriteString( 'MetaRatingId', fMetaRating.Id );
      Writer.WriteString( 'MetaRatingFamily', System.RatingFamily );
      Writer.WriteByte( 'PeopleRating', fPeopleRating );
      Writer.WriteByte( 'IFELRating', fIFELRating );
      Writer.WriteByte( 'TycoonsRating', fTycoonsRating );
      Writer.WriteByte( 'RulerPublicity', fRulerPublicity );
      Writer.WriteObject( 'Surveys', fSurveys );
    end;

  procedure TRating.Loaded;
    var
      i : integer;
    begin
      for i := pred(fSurveys.Count) downto 0 do
        if TSurvey(fSurveys[i]).fTycoon = nil
          then fSurveys.Extract(fSurveys[i]);
    end;


  // TRatingSystem

  constructor TRatingSystem.Create( aPoliticalEntity : IPoliticalEntity; aRatingFamily : string );

    procedure InitRatings;
      var
        i     : integer;
        count : integer;
        MR    : TMetaRating;
      begin
        count := TheClassStorage.ClassCount[fRatingFamily];
        for i := 0 to pred(count) do
          begin
            MR := TMetaRating(TheClassStorage.ClassByIdx[fRatingFamily, i]);
            fRatings.Insert( MR.RatingClass.Create( self, MR ) );
          end;
      end;

    begin
      inherited Create;
      fPoliticalEntity := aPoliticalEntity;
      fRatings         := TLockableCollection.Create( 0, rkBelonguer );
      fRatingFamily    := aRatingFamily;
      InitRatings;
    end;

  destructor TRatingSystem.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fRatings.Free;
      inherited;
    end;

  function TRatingSystem.GetRating( Id : string ) : TRating;
    var
      i : integer;
    begin
      Ratings.Lock;
      try
        i := 0;
        while (i < Ratings.Count) and (TRating(Ratings[i]).MetaRating.Id <> Id) do
          inc( i );
        if i < Ratings.Count
          then result := TRating(Ratings[i])
          else result := nil;
      finally
        Ratings.Unlock;
      end;
    end;

  procedure TRatingSystem.Evaluate;
    var
      i          : integer;
      sum        : integer;
      sumIFEL    : integer;
      sumTycoons : integer;
      count      : integer;
    begin
      Ratings.Lock;
      try
        sum        := 0;
        sumIFEL    := 0;
        sumTycoons := 0;
        count      := 0;
        for i := 0 to pred(Ratings.Count) do
          with TRating(Ratings[i]) do
            begin
              Evaluate;
              sum        := sum + MetaRating.Weight*PeopleRating;
              sumIFEL    := sumIFEL + MetaRating.Weight*IFELRating;
              sumTycoons := sumTycoons + MetaRating.Weight*TycoonsRating;
              count := count + MetaRating.Weight;
            end;
        if count > 0
          then
            begin
              fOverallRating := sum div count;
              fIFELRating    := sumIFEL div count;
              fTycoonsRating := sumTycoons div count;
            end
          else
            begin
              fOverallRating := 0;
              fIFELRating    := 0;
              fTycoonsRating := 0;
            end;
      finally
        Ratings.Unlock;
      end;
    end;

  procedure TRatingSystem.Clear;
    var
      i : integer;
    begin
      fRatings.Lock;
      try
        for i := 0 to pred(Ratings.Count) do
          begin
            TRating(Ratings[i]).Surveys.DeleteAll;
            TRating(Ratings[i]).Evaluate;
            ModelServerCache.BackgroundCache(Ratings[i], false); //CacheObject( Ratings[i], -1, -1 )
          end;
      finally
        fRatings.Unlock;
      end;
    end;

  procedure TRatingSystem.LoadFromBackup( Reader : IBackupReader );

    procedure UpdateRatings;
      var
        i     : integer;
        count : integer;
        MR    : TMetaRating;
      begin
        // Delete outdated items
        for i := pred(Ratings.Count) downto 0 do
          if TRating(Ratings[i]).fMetaRating = nil
            then Ratings.AtDelete( i );
        // Insert new items
        count := TheClassStorage.ClassCount[fRatingFamily];
        for i := 0 to pred(count) do
          begin
            MR := TMetaRating(TheClassStorage.ClassByIdx[fRatingFamily, i]);
            if Rating[MR.Id] = nil
              then fRatings.Insert( MR.RatingClass.Create( self, MR ) );
          end;
      end;

    begin
      inherited;
      Reader.ReadObject( 'Ratings', fRatings, nil );
      fRatingFamily := Reader.ReadString( 'RatingFamily', tidClassFamily_Ratings );
      UpdateRatings;
    end;

  procedure TRatingSystem.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObject( 'Ratings', fRatings );
      Writer.WriteString( 'RatingFamily', fRatingFamily );
    end;

  procedure TRatingSystem.ReCache;
    var
      i : integer;
    begin
      Ratings.Lock;
      try
        for i := pred(Ratings.Count) downto 0 do
          ModelServerCache.BackgroundCache(Ratings[i], false); //CacheObject( Ratings[i], -1, -1 )
      finally
        Ratings.Unlock;
      end;
    end;

  procedure TRatingSystem.TycoonDeleted( Tycoon : TTycoon );
    var
      i : integer;
    begin
      Ratings.Lock;
      try
        for i := pred(Ratings.Count) downto 0 do
          TRating(Ratings[i]).TycoonDeleted( Tycoon );
      finally
        Ratings.Unlock;
      end;
    end;

  procedure TRatingSystem.Loaded;
    var
      i : integer;
    begin
      for i := 0 to pred(fRatings.Count) do
        TRating(fRatings[i]).Loaded;
    end;


  // TMetaProject

  constructor TMetaProject.Create( anId, aName, aKind : string; aWeight : integer; aProjectClass : CProject );
    begin
      inherited Create( anId );
      fProjectClass := aProjectClass;
      fName         := aName;
      fName_MLS     := TMultiString.Create;
      fWeight       := aWeight;
      fKind         := aKind;
    end;

  procedure TMetaProject.RetrieveTexts( Container : TDictionary );
    begin
      if fName_MLS = nil
        then fName_MLS := TMultiString.Create;
      fName_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Name'];
    end;

  procedure TMetaProject.StoreTexts( Container : TDictionary );
    begin
      Container.Values[Family + '.' + Id + '.' + 'Name'] := fName;
    end;


  // TProject

  constructor TProject.Create( aCampaign : TCampaign; aMetaProject : TMetaProject );
    begin
      inherited Create;
      fCampaign     := aCampaign;
      fMetaProject := aMetaProject;
    end;

  function TProject.GetRating : TPercent;
    begin
      result := 0;
    end;

  function TProject.GetAccuracy : TPercent;
    begin
      result := 100;
    end;

  procedure TProject.LoadFromBackup( Reader : IBackupReader );
    var
      MetaProjectId     : string;
      MetaProjectFamily : string;
    begin
      inherited;
      Reader.ReadObject( 'Campaign', fCampaign, nil );
      MetaProjectId     := Reader.ReadString( 'MetaProjectId', '' );
      MetaProjectFamily := Reader.ReadString( 'MetaProjectFamily', tidClassFamily_Projects );
      fMetaProject      := TMetaProject(TheClassStorage.ClassById[MetaProjectFamily, MetaProjectId]);
    end;

  procedure TProject.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObjectRef( 'Campaign', fCampaign );
      Writer.WriteString( 'MetaProjectId', fMetaProject.Id );
      Writer.WriteString( 'MetaProjectFamily', fCampaign.ProjectsFamily );
    end;

  procedure TProject.ParseData( Data : string );
    begin
    end;

  procedure TProject.SetDefaultValue;
    begin
    end;

  procedure TProject.StoreToCache( Cache : TObjectCache );
    begin
      StoreMultiStringToCache( 'Name', MetaProject.Name_MLS, Cache );
      Cache.WriteString( 'TypeId', MetaProject.Kind );
    end;


  // TCampaign

  constructor TCampaign.Create( aTycoon : TTycoon; aSystem : TCampaignSystem; aProjectsFamily : string; cache : boolean );

    procedure InitProjects;
      var
        i       : integer;
        count   : integer;
        MP      : TMetaProject;
        Project : TProject;
      begin
        count := TheClassStorage.ClassCount[fProjectsFamily];
        for i := 0 to pred(count) do
          begin
            MP      := TMetaProject(TheClassStorage.ClassByIdx[fProjectsFamily, i]);
            Project := MP.ProjectClass.Create( self, MP );
            Project.SetDefaultValue;
            fProjects.Insert( Project );
            if cache
              then ModelServerCache.BackgroundCache(Project, false); //CacheObject( Project, -1, -1 )
          end;
      end;

    begin
      inherited Create;
      fTycoon         := aTycoon;
      fSystem         := aSystem;
      fProjects       := TLockableCollection.Create( 0, rkBelonguer );
      fProjectsFamily := aProjectsFamily;
      InitProjects;
      Evaluate;
    end;

  destructor TCampaign.Destroy;
    begin
      ModelServerCache.BackgroundUncache(self); //UncacheObject( self, -1, -1 )
      fProjects.Free;
      inherited;
    end;

  procedure TCampaign.Evaluate;
    const
      PrestigeToRating = 10;

    var
      i          : integer;
      Ratings    : integer;
      Accuracies : integer;
      Count      : integer;
    begin
      Projects.Lock;
      try
        Ratings    := 0;
        Count      := 0;
        Accuracies := 0;
        for i := 0 to pred(Projects.Count) do
          with TProject(Projects[i]) do
            begin
              Ratings    := Ratings + MetaProject.Weight*Rating;
              Accuracies := Accuracies + MetaProject.Weight*Accuracy;
              Count      := Count + MetaProject.Weight;
            end;
        if Count > 0
          then
            begin
              fRating   := Ratings div Count;
              fAccuracy := Accuracies div Count;
            end
          else
            begin
              fRating   := 100;
              fAccuracy := 100;
            end;
        //fRating := min(100, fRating + round(Tycoon.Prestige/PrestigeToRating));
        fRating := min(100, (fRating div 2) + (min(100, round(realmax(0, Tycoon.Prestige)/PrestigeToRating)) div 2));
      finally
        Projects.Unlock;
      end;
    end;

  function TCampaign.GetProject( Id : string ) : TProject;
    var
      i : integer;
    begin
      Projects.Lock;
      try
        i := 0;
        while (i < Projects.Count) and (TProject(Projects[i]).MetaProject.Id <> Id) do
          inc( i );
        if i < Projects.Count
          then result := TProject(Projects[i])
          else result := nil;
      finally
        Projects.Unlock;
      end;
    end;

  procedure TCampaign.LoadFromBackup( Reader : IBackupReader );

    procedure UpdateProjects;
      var
        i     : integer;
        count : integer;
        MP    : TMetaProject;
      begin
        // Delete outdated items
        for i := pred(Projects.Count) downto 0 do
          if TProject(Projects[i]).fMetaProject = nil
            then Projects.AtDelete( i );
        // Insert new items
        count := TheClassStorage.ClassCount[ProjectsFamily];
        for i := 0 to pred(count) do
          begin
            MP := TMetaProject(TheClassStorage.ClassByIdx[ProjectsFamily, i]);
            if Project[MP.Id] = nil
              then fProjects.Insert( MP.ProjectClass.Create( self, MP ) );
          end;
      end;

    begin
      inherited;
      Reader.ReadObject( 'Tycoon', fTycoon, nil );
      Reader.ReadObject( 'System', fSystem, nil );
      Reader.ReadObject( 'Projects', fProjects, nil );
      fRating := Reader.ReadByte( 'Rating', 0 );
      fProjectsFamily := Reader.ReadString( 'ProjectsFamily', tidClassFamily_Projects );
      fCurrentVotes := Reader.ReadInteger('Votes', 0);
      UpdateProjects;
    end;

  procedure TCampaign.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObjectRef( 'Tycoon', fTycoon );
      Writer.WriteObjectRef( 'System', fSystem );
      Writer.WriteObject( 'Projects', fProjects );
      Writer.WriteByte( 'Rating', fRating );
      Writer.WriteString( 'ProjectsFamily', fProjectsFamily );
      Writer.WriteInteger('Votes', fCurrentVotes);
    end;

  procedure TCampaign.AddVotes(count : integer);
    begin
      inc(fCurrentVotes, count);
    end;

  function TCampaign.GetStrength : single;
    begin
      result := 0.5*fRating + 5*fCurrentVotes;
    end;


  // TCampaignSystem

  constructor TCampaignSystem.Create( aPoliticalEntity : IPoliticalEntity );
    begin
      inherited Create;
      fPoliticalEntity := aPoliticalEntity;
      fCampaigns       := TSortedCollection.Create( 0, rkBelonguer, CompareCampaigns );
      fLock            := TCriticalSection.Create;
    end;

  destructor TCampaignSystem.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fCampaigns.Free;
      fLock.Free;
      inherited;
    end;

  function TCampaignSystem.GetCampaign( Tycoon : TTycoon ) : TCampaign;
    var
      i : integer;
    begin
      fLock.Enter;
      try
        i := 0;
        while (i < Campaigns.Count) and (TCampaign(Campaigns[i]).Tycoon <> Tycoon) do
          inc( i );
        if i < Campaigns.Count
          then result := TCampaign(Campaigns[i])
          else
            if (fPoliticalEntity <> nil) and (fPoliticalEntity.getWinningCampaign <> nil) and (fPoliticalEntity.getWinningCampaign.Tycoon = Tycoon)
              then result := fPoliticalEntity.getWinningCampaign
              else result := nil;
      finally
        fLock.Leave;
      end;
    end;

  procedure TCampaignSystem.Evaluate;
    var
      i : integer;
    begin
      fLock.Enter;
      try
        for i := 0 to pred(Campaigns.Count) do
          TCampaign(Campaigns[i]).Evaluate;
        Campaigns.Sort( CompareCampaigns );
      finally
        fLock.Leave;
      end;
    end;

  procedure TCampaignSystem.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Campaigns', fCampaigns, nil );
      fLock := TCriticalSection.Create;
    end;

  procedure TCampaignSystem.StoreToBackup ( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObject( 'Campaigns', fCampaigns );
    end;

  function TCampaignSystem.CompareCampaigns( Item1, Item2 : TObject ) : integer;
    var
      Campaign1, Campaign2 : TCampaign;
      strength1, strength2 : single;
    begin
      Campaign1  := TCampaign(Item1);
      Campaign2  := TCampaign(Item2);
      strength1  := Campaign1.GetStrength; //PersonalVotesWeight*Campaign1.fCurrentVotes + (1 - PersonalVotesWeight)*(Campaign1.Rating + Campaign1.Tycoon.Prestige/100);
      strength2  := Campaign2.GetStrength; //PersonalVotesWeight*Campaign2.fCurrentVotes + (1 - PersonalVotesWeight)*(Campaign2.Rating + Campaign2.Tycoon.Prestige/100);
      if strength1 < strength2
        then result := 1
        else
          if strength1 > strength2
            then result := -1
            else
              if Campaign1.Tycoon.Prestige < Campaign1.Tycoon.Prestige
                then result := 1
                else
                  if Campaign1.Tycoon.Prestige > Campaign1.Tycoon.Prestige
                    then result := -1
                    else result := 0;
    end;

  procedure TCampaignSystem.TycoonDeleted( Tycoon : TTycoon );
    var
      C : TCampaign;
    begin
      C := Campaign[Tycoon];
      if C <> nil
        then fCampaigns.Delete( C );
    end;

  procedure TCampaignSystem.ReCache;
    var
      i : integer;
    begin
      fLock.Enter;
      try
        for i := 0 to pred(Campaigns.Count) do
          ModelServerCache.BackgroundCache(Campaigns[i], false); //CacheObject( Campaigns[i], -1, -1 )
      finally
        fLock.Leave;
      end;
    end;

  procedure TCampaignSystem.StoreVotesToCache(Cache : TObjectCache);
    var
      i        : integer;
      aux      : string;
      Campaign : TCampaign;
    begin
      fLock.Enter;
      Cache.WriteInteger('CampaignCount', Campaigns.Count);
      try
        for i := 0 to pred(Campaigns.Count) do
          begin
            Campaign := TCampaign(Campaigns[i]);
            if Campaign.Tycoon <> nil
              then
                begin
                  aux := IntToStr(i);
                  Cache.WriteString('Candidate' + aux, Campaign.Tycoon.Name);
                  Cache.WriteInteger('Votes' + aux, Campaign.CurrentVotes);
                  Cache.WriteInteger('CmpRat' + aux, Campaign.Rating);
                  Cache.WriteInteger('CmpPnts' + aux, round(Campaign.GetStrength));
                end;
          end;
      finally
        fLock.Leave;
      end;
    end;

  procedure TCampaignSystem.VoteFor(Voter, Votee : TTycoon);
    var
      prevTyc : TTycoon;
      prevCmp : TCampaign;
      newCmp  : TCampaign;
      Loc     : TObject;
    begin
      fLock.Enter;
      try
        // get location
        Loc := TObject(fPoliticalEntity.getRDOId);
        // get old vote in this place
        prevTyc := TTycoon(Voter.Votes[Loc]);
        // get his campaign
        if prevTyc <> nil
          then prevCmp := Campaign[prevTyc]
          else prevCmp := nil;
        // find new campaign
        newCmp := Campaign[Votee];
        // check if the campain is the current ruler's
        if newCmp = nil
          then
            begin
              newCmp := fPoliticalEntity.getWinningCampaign;
              if (newCmp <> nil) and (newCmp.fTycoon <> Votee)
                then newCmp := nil;
            end;
        // if exists add the vote
        if newCmp <> nil
          then
            begin
              newCmp.AddVotes(1);
              // if exists then dec votes
              if prevCmp <> nil
                then prevCmp.AddVotes(-1);
              Voter.Votes[Loc] := Votee;
            end;
      finally
        fLock.Leave;
      end;
    end;

  function TCampaignSystem.VoteOf(Tycoon : TTycoon) : TTycoon;
    var
      Loc : TObject;
    begin
      Loc := TObject(fPoliticalEntity.getRDOId);
      result := TTycoon(Tycoon.Votes[Loc]);
    end;

  function TCampaignSystem.IsTheBestCampaign(Camp : TCampaign) : boolean;
    var
      First : TCampaign;
    begin
      if fCampaigns.Count = 0
        then result := true
        else
          begin
            First  := TCampaign(fCampaigns[0]);
            result := CompareCampaigns(First, Camp) >= 0;
          end;
    end;

  procedure TCampaignSystem.Loaded;
    var
      i : integer;
    begin
      for i := pred(fCampaigns.Count) downto 0 do
        if TCampaign(fCampaigns[i]).Tycoon = nil
          then fCampaigns.Extract(fCampaigns[i]);
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TSurvey );
      RegisterClass( TRating );
      RegisterClass( TRatingSystem );
      RegisterClass( TProject );
      RegisterClass( TCampaign );
      RegisterClass( TCampaignSystem );
    end;

end.




