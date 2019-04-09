unit TownPolitics;

{ $DEFINE DebugPolitics}

interface

  uses
    Kernel, Politics, Population, CacheAgent, Windows, BackupInterfaces, Languages, Variants;

  const
    currId_Mayor = 10;

  type
    TPoliticalTownHall = class;
    TPoliticalTown     = class;

    TPoliticalTownHall =
      class( TTownHall, IPoliticalEntity )
        protected
          constructor Create( aMetaBlock : TMetaBlock; aFacility : TFacility ); override;
        public
          destructor Destroy; override;
        private
          fRatings    : TRatingSystem;
          fCampaigns  : TCampaignSystem;
          fLastPerPop : TFluidValue;
          fLastElect  : TVirtDateAbs;
          fCapital    : boolean;
        private
          function GetCityGrowth : integer;
        public
          property Ratings           : TRatingSystem   read fRatings;
          property Campaigns         : TCampaignSystem read fCampaigns;
          property LastPerPopulation : TFluidValue     read fLastPerPop;
          property LastElections     : TVirtDateAbs    read fLastElect;
          property CityGrowth        : integer         read GetCityGrowth;
          property Capital           : boolean         read fCapital;
        published
          function  RDOGetRatingFrom( RatingId : widestring; TycoonId : widestring ) : olevariant;
          procedure RDOSetRatingFrom( RatingId : widestring; TycoonId : widestring; Value : integer );
          procedure RDOSetPublicity ( RatingId : widestring; Value : integer );
        published
          function  RDOLaunchCampaign ( TycoonId : widestring ) : olevariant;
          procedure RDOCancelCampaign ( TycoonId : widestring );
          procedure RDOSetProjectData ( TycoonId : widestring; ProjectId, Data : widestring );
          procedure RDOVote(voterTycoon, choiceTycoon : widestring);
          function  RDOVoteOf(tycoonName : widestring) : OleVariant;
        public
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); override;
        public
          procedure StoreToCache( Cache : TObjectCache ); override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        protected
          procedure TycoonDeleted( Tycoon : TTycoon ); virtual;
        private
          function GetPoliticalTown : TPoliticalTown;
        public
          property PoliticalTown : TPoliticalTown read GetPoliticalTown;
        // IPoliticalEntity
        private
          function getWinningCampaign : TCampaign;
          function getParm( id : TPoliticalParmId; const info ) : single;
          function getCacheFolder : string;
          function getRDOId       : integer;
        // IUnknown
        private
          function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function _AddRef  : integer; stdcall;
          function _Release : integer; stdcall;
        public
          procedure SitMayor(Tycoon : TTycoon);
          procedure BlockLoaded; override;
      end;

    TMayor =
      class( TTycoon )
        private
          fTownHall : TPoliticalTownHall;
        protected
          class function GetIsRole : boolean; override;
        public
          procedure StoreRoleInfoToCache( Cache : TObjectCache ); override;
      end;

    TPoliticalTown =
      class( TInhabitedTown )
        private
          fMayorPeriods     : integer;
          fWinningCampaign  : TCampaign;
          fLastElections    : integer;
        public
          property MayorPeriods    : integer   read fMayorPeriods write fMayorPeriods;
          property WinningCampaign : TCampaign read fWinningCampaign write fWinningCampaign;
        public
          procedure ElectMayor; virtual;
          procedure FireMayor;  virtual;
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); override;
          procedure TycoonDeleted( Tycoon : TTycoon ); override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        public
          procedure Loaded; override;
      end;

  const
    MinPopulation = 2000;


  // RegisterBackup

  procedure RegisterBackup;

implementation

  uses
    ModelServerCache, World, Protocol, BasicCurriculum, SimHints, BasicPolitics, MathUtils, Logs,
    SysUtils, Taxes, Collection, Events;


  // TPoliticalTownHall

  constructor TPoliticalTownHall.Create( aMetaBlock : TMetaBlock; aFacility : TFacility );
    begin
      inherited;
      fRatings  := TRatingSystem.Create( self, tidClassFamily_Ratings );
      fCampaigns := TCampaignSystem.Create( self );
    end;

  destructor TPoliticalTownHall.Destroy;
    begin
      fRatings.Free;
      fCampaigns.Free;
      inherited;
    end;

  function TPoliticalTownHall.GetCityGrowth : integer;
    const
      HoursInPeriod = 24*365*PeriodLength;
    var
      CurrPop   : TFluidValue;
      CurrDate  : TVirtDateAbs;
      EstPop    : extended; //TFluidValue;
      PopGrowth : extended; //TFluidValue;
    begin
      CurrPop   := TotalPopulation;
      CurrDate  := PoliticalTown.Timer.GetVirtualTimeAbs;
      if CurrDate > LastElections
        then EstPop := (CurrPop - LastPerPopulation)*HoursInPeriod/(CurrDate - LastElections) + LastPerPopulation
        else EstPop := LastPerPopulation;
      if LastPerPopulation > 0.1
        then PopGrowth := EstPop/LastPerPopulation - 1
        else
          begin
            PopGrowth := 1;
            fLastPerPop := CurrPop;
          end;
      result := round(100*PopGrowth);
    end;

  function TPoliticalTownHall.RDOGetRatingFrom( RatingId : widestring; TycoonId : widestring ) : olevariant;
    var
      Tycoon : TTycoon;
      Rating : TRating;
      Survey : TSurvey;
    begin
      try
        Tycoon := TTycoon(TycoonId);
        Rating := fRatings[RatingId];
        if Rating <> nil
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

  procedure TPoliticalTownHall.RDOSetRatingFrom( RatingId : widestring; TycoonId : widestring; Value : integer );
    var
      Tycoon : TTycoon;
      Rating : TRating;
      Survey : TSurvey;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Setting town politics Tycoon rating: ' + TycoonId + ', ' + RatingId + ', ' + IntToStr(Value) );
      try
        Tycoon := PoliticalTown.WorldLocator.GetTycoonByName( TycoonId );
        if (Tycoon <> nil) and (Tycoon.MasterRole <> PoliticalTown.Mayor.MasterRole)
          then
            begin
              Rating := fRatings[RatingId]; 
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
                    ModelServerCache.BackgroundInvalidateCache(Rating); //CacheObject( Rating, noKind, noInfo )
                    ModelServerCache.BackgroundInvalidateCache(PoliticalTown); //CacheObject( PoliticalTown, noKind, noInfo )
                  end;
            end;
      except
      end;
      Logs.Log( tidLog_Survival,  'OK!');
    end;

  procedure TPoliticalTownHall.RDOSetPublicity( RatingId : widestring; Value : integer );
    var
      Rating : TRating;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Setting town politics publicity: ' + RatingId + ', ' + IntToStr(Value) );
      try
        Rating := fRatings[RatingId];
        if Rating <> nil
          then
            begin
              Rating.RulerPublicity := Value;
              Rating.Evaluate;
              ModelServerCache.BackgroundInvalidateCache(Rating); //CacheObject( Rating, noKind, noInfo )
              ModelServerCache.BackgroundInvalidateCache(PoliticalTown); //CacheObject( PoliticalTown, noKind, noInfo )
            end;
      except
        Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Error setting town politics publicity' );
      end;
      Logs.Log( tidLog_Survival,  'OK!');
    end;

  function TPoliticalTownHall.RDOLaunchCampaign( TycoonId : widestring ) : olevariant;

    function TycoonHasNoCampaign( Tycoon : TTycoon ) : boolean;
      var
        World : TWorld;
        i     : integer;
      begin
        World := TInhabitedTown(Facility.Town).World;
        World.Towns.Lock;
        try
          i := 0;
          while (i < World.Towns.Count) and (TPoliticalTownHall(TPoliticalTown(World.Towns[i]).TownHall.CurrBlock).Campaigns[Tycoon] = nil) do
            inc( i );
        finally
          World.Towns.Unlock;
        end;
        result := i = World.Towns.Count
      end;

    function TycoonIsAvailable( Tycoon : TTycoon ) : boolean;
      begin
        {$IFNDEF DebugPolitics}
        result := (Tycoon.MasterRole = Tycoon) and (Tycoon.Roles.Count = 0) and TycoonHasNoCampaign( Tycoon )
        {$ELSE}
        result := true;
        {$ENDIF}
      end;

    const
      MinMayorPrestige = 200;
    var
      Tycoon   : TTycoon;
      Campaign : TCampaign;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Launching Mayor campaign: ' + TycoonId );
      try
        if (TInhabitedTown(Facility.Town).World.YearsToElections >= PeriodLength div 2) and TInhabitedTown(Facility.Town).World.ElectionsOn
          then
            begin
              Tycoon := PoliticalTown.WorldLocator.GetTycoonByName( TycoonId );
              if Tycoon <> nil
                then
                  if TycoonIsAvailable( Tycoon )
                    then
                      {$IFNDEF DebugPolitics}
                      if Tycoon.Prestige >= MinMayorPrestige
                      {$ELSE}
                      if true
                      {$ENDIF}
                        then
                          begin
                            Campaign := fCampaigns[Tycoon];
                            if Campaign = nil
                              then
                                begin
                                  Campaign := TCampaign.Create( Tycoon, fCampaigns, tidClassFamily_Projects );
                                  fCampaigns.Campaigns.Insert( Campaign );
                                  fCampaigns.Campaigns.Sort( fCampaigns.CompareCampaigns );
                                  CacheObject( PoliticalTown, noKind, noInfo ); // >> Cache_INV ???
                                  Facility.Town.WorldLocator.SendEvent(
                                    TEvent.Create(
                                      0,
                                      Facility.Town.Timer.GetVirtualTimeAbs,
                                      Facility.Town.Timer.GetVirtualTime,
                                      100000,
                                      10000,
                                      InstantiateMultiString( mtidLaunchedCampaign, [Tycoon.Name, Facility.Town.Name] ),
                                      //NullString, //Tycoon.Name + ' launched a campaign for ' + Facility.Town.Name + '.',
                                      Tycoon.Name,
                                      'Visual/Voyager/Politics/politics.asp?TownName=' + Facility.Town.Name +
                                      '&frame_Id=PoliticsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client' ));
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

  procedure TPoliticalTownHall.RDOCancelCampaign( TycoonId : widestring );
    var
      Tycoon   : TTycoon;
      Campaign : TCampaign;
      rdoId    : integer;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Cancel Mayor campaign: ' + TycoonId );
      try
        Tycoon := PoliticalTown.WorldLocator.GetTycoonByName( TycoonId );
        if Tycoon <> nil
          then
            begin
              Campaign := fCampaigns[Tycoon];
              if Campaign <> nil
                then
                  begin
                    rdoId := Campaign.System.PoliticalEntity.getRDOId;
                    PoliticalTown.WorldLocator.ReportWithdrawal(TObject(rdoId), Tycoon);
                    ModelServerCache.BackgroundUncache(Campaign); //UncacheObject( Campaign, noKind, noInfo )
                    fCampaigns.Campaigns.Delete( Campaign );
                    CacheObject( PoliticalTown, noKind, noInfo ); // >> Cache_INV ??
                    Facility.Town.WorldLocator.SendEvent(
                      TEvent.Create(
                        0,
                        Facility.Town.Timer.GetVirtualTimeAbs,
                        Facility.Town.Timer.GetVirtualTime,
                        100000,
                        10000,
                        InstantiateMultiString( mtidCancelledCampaign, [Tycoon.Name, Facility.Town.Name] ),
                        //NullString, // Tycoon.Name + ' withdrawed campaign for ' + Facility.Town.Name + '.',
                        Tycoon.Name,
                        'Visual/Voyager/Politics/politics.asp?TownName=' + Facility.Town.Name +
                        '&frame_Id=PoliticsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client' ));
                  end;
            end;
      except
      end;
      Logs.Log( tidLog_Survival,  'OK!');
    end;

  procedure TPoliticalTownHall.RDOSetProjectData( TycoonId : widestring; ProjectId, Data : widestring );
    var
      Tycoon   : TTycoon;
      Campaign : TCampaign;
      Project  : TProject;
    begin
      Logs.Log( tidLog_Survival, TimeToStr(Now) + ' Setting Mayor campaign data: ' + TycoonId + ', ' + ProjectId + ', ' + Data );
      try
        Tycoon := PoliticalTown.WorldLocator.GetTycoonByName( TycoonId );
        if Tycoon <> nil
          then
            begin
              Campaign := fCampaigns[Tycoon];
              if Campaign <> nil
                then
                  begin
                    Project := Campaign.Project[ProjectId];
                    if Project <> nil
                      then Project.ParseData( Data );
                    ModelServerCache.BackgroundInvalidateCache(Project); //CacheObject( Project, noKind, noInfo )
                    Campaign.Evaluate;
                    ModelServerCache.BackgroundInvalidateCache(PoliticalTown); //CacheObject( PoliticalTown, noKind, noInfo )
                  end;
            end;
      except
      end;
      Logs.Log( tidLog_Survival,  'OK!');
    end;

  procedure TPoliticalTownHall.RDOVote(voterTycoon, choiceTycoon : widestring);
    var
      WL : IWorldLocator;
      Voter, Votee : TTycoon;
    begin
      Logs.Log( tidLog_Survival, DateTimeToStr(Now) + Format(' Voting: %s by %s', [voterTycoon, choiceTycoon]));
      try
        WL := Facility.Town.WorldLocator;
        Voter := WL.GetTycoonByName(voterTycoon);
        Votee := WL.GetTycoonByName(choiceTycoon);
        if (Voter <> nil) and Voter.PaysTaxesInTown(Facility.Town) and (Votee <> nil) and Voter.CheckOpAuthenticity
          then
            begin
              fCampaigns.VoteFor(Voter, Votee);
              fCampaigns.Evaluate;
              ModelServerCache.InvalidateCache(Facility, false);
              Facility.Town.MapRefresh.RefeshFacility(Facility, fchStructure);
            end;
      except
        Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' error in RDOVote..');
      end;
    end;

  function TPoliticalTownHall.RDOVoteOf(tycoonName : widestring) : OleVariant;
    var
      WL     : IWorldLocator;
      Tycoon : TTycoon;
      Choice : TTycoon;
    begin
      try
        WL := Facility.Town.WorldLocator;
        Tycoon := WL.GetTycoonByName(tycoonName);
        if Tycoon <> nil
          then Choice := fCampaigns.VoteOf(Tycoon)
          else Choice := nil;
        if Choice <> nil
          then result := Choice.Name
          else result := '';
      except
        result := '';
      end;
    end;

  procedure TPoliticalTownHall.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );

    procedure CheckForCapitalStatus;
      const
        CapitalPop = 100;
      var
        i         : integer;
        Towns     : TCollection;
        TownHall  : TPoliticalTownHall;
      begin                                             
        if TotalPopulation >= CapitalPop
          then
            begin
              Towns := TInhabitedTown(Facility.Town).World.Towns;
              i := 0;
              repeat
                TownHall := TPoliticalTownHall(TInhabitedTown(Towns[i]).TownHall.CurrBlock);
                inc( i );
              until (i = Towns.Count) or (((TownHall.TotalPopulation > TotalPopulation)) and not (TownHall = self));
              fCapital := i = Towns.Count;
            end;
      end;

    begin
      inherited;
      TMayor(Facility.Town.Mayor).fTownHall := self; // >> Temporary
      case PeriodType of
        perDay :
          begin
            fRatings.Evaluate;
            fCampaigns.Evaluate;
          end;
        perPoliticalMandate :
          begin
            CheckForCapitalStatus;
          end;
      end;
    end;

  procedure TPoliticalTownHall.StoreToCache( Cache : TObjectCache );
    begin
      inherited;
      Cache.WriteBoolean( 'Capital', fCapital );
      // New election system
      fCampaigns.StoreVotesToCache(Cache);
      if (PoliticalTown.WinningCampaign <> nil) and (PoliticalTown.WinningCampaign.Tycoon <> nil)
        then
          begin
            Cache.WriteString('RulerName', PoliticalTown.WinningCampaign.Tycoon.Name);
            Cache.WriteInteger('RulerVotes', PoliticalTown.WinningCampaign.CurrentVotes);
            Cache.WriteInteger('RulerCmpRat', PoliticalTown.WinningCampaign.Rating);
            Cache.WriteInteger('RulerCmpPnts', round(PoliticalTown.WinningCampaign.GetStrength));
          end;

    end;
    
  procedure TPoliticalTownHall.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fLastPerPop := Reader.ReadSingle( 'LastPerPopulation', 0 );
      fLastElect  := Reader.ReadInteger( 'LastElections', 0 );
      Reader.ReadObject( 'Ratings', fRatings, nil );                   
      Reader.ReadObject( 'Campaigns', fCampaigns, nil );
      if fRatings = nil
        then fRatings := TRatingSystem.Create( self, tidClassFamily_Ratings );
      fRatings.PoliticalEntity := self;
      if fCampaigns = nil
        then fCampaigns := TCampaignSystem.Create( self );     
      fCampaigns.PoliticalEntity := self;
      // >> Binary inconsistency
      fCapital := Reader.ReadBoolean( 'Capital', false );
    end;

  procedure TPoliticalTownHall.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteSingle( 'LastPerPopulation', fLastPerPop );
      Writer.WriteInteger( 'LastElections', fLastElect );
      Writer.WriteObject( 'Ratings', fRatings );
      Writer.WriteObject( 'Campaigns', fCampaigns );
      Writer.WriteBoolean( 'Capital', fCapital );
    end;

  procedure TPoliticalTownHall.TycoonDeleted( Tycoon : TTycoon );
    begin
      Campaigns.TycoonDeleted( Tycoon );
      Ratings.TycoonDeleted( Tycoon );
    end;

  function TPoliticalTownHall.GetPoliticalTown : TPoliticalTown;
    begin
      result := TPoliticalTown(Facility.Town);
    end;
    
  function TPoliticalTownHall.getWinningCampaign : TCampaign;
    begin
      result := PoliticalTown.WinningCampaign;
    end;

  function TPoliticalTownHall.getParm( id : TPoliticalParmId; const info ) : single;
    var
      MPFInfo : TMetaPublicFacilityInfo absolute info;
      Tycoon  : TTycoon absolute info;
      PFInfo  : TPublicFacilityInfo;
      i       : TPeopleKind;
      unemp   : single;
    begin
      case id of
        plidLastAdsQ :
          result := LastAds.Q;      
        plidLastAdsK :
          result := LastAds.K;
        plidRatingEthics :
          if (Tycoon <> nil) and (Campaigns.Campaign[Tycoon] = nil)
            then result := 1
            else result := 0;
        plidPopGrowth :
          result := CityGrowth;
        plidPublicParm :
          begin
            if TotalPopulation > 0
              then
                begin
                  PFInfo := PublicFacilities[MPFInfo];
                  if PFInfo <> nil
                    then result := realmin(100, 100*PFInfo.Strength/TotalPopulation)
                    else result := 0;
                end
              else result := 0;
          end;
        plidUnemployment :
          if TotalPopulation > 0
            then
              begin
                unemp := 0;
                for i := low(i) to high(i) do
                  unemp := unemp + LastPop[i].Q*Unemployment[i];
                result := 100 - unemp/TotalPopulation;
              end
            else result := 100;
        plidGQOS :
          result := realmin(100, 100*GQOS);
        plidWealth :
          result := realmin(100, 100*AvgWealth);
        else
          result := 0;
      end;
    end;

  function TPoliticalTownHall.getCacheFolder : string;
    begin
      result := GetObjectPath( PoliticalTown, noKind, noInfo );
    end;

  function TPoliticalTownHall.getRDOId : integer;
    begin
      result := integer(self);
    end;

  function TPoliticalTownHall.QueryInterface(const IID: TGUID; out Obj): hresult;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TPoliticalTownHall._AddRef  : integer;
    begin
      result := 1;
    end;

  function TPoliticalTownHall._Release : integer;
    begin
      result := 1;
    end;

  procedure TPoliticalTownHall.SitMayor(Tycoon : TTycoon);
    begin
      PoliticalTown.WinningCampaign := TCampaign.Create(Tycoon, fCampaigns, tidClassFamily_Projects, false);
      PoliticalTown.fLastElections  := PoliticalTown.World.CurrYear;
      PoliticalTown.fMayorPeriods   := 0;
    end;

  procedure TPoliticalTownHall.BlockLoaded;
    begin
      inherited;
      try
        fRatings.Loaded;
      except
        Logs.Log(tidLog_Survival, TimeToStr(Now) + ' Error TPoliticalTownHall::fRatings.Loaded..');
      end;
      try
        fCampaigns.Loaded;
      except
        Logs.Log(tidLog_Survival, TimeToStr(Now) + ' Error TPoliticalTownHall::fCampaigns.Loaded..');
      end;
    end;

  // TMayor

  class function TMayor.GetIsRole : boolean;
    begin
      result := true;
    end;

  procedure TMayor.StoreRoleInfoToCache( Cache : TObjectCache );
    begin
      inherited;
      Cache.WriteBoolean( 'IsMayor', true );
      if fTownHall <> nil
        then
          begin
            Cache.WriteString( 'Town', fTownHall.Facility.Town.Name );
            Cache.WriteBoolean( 'IsCapitalMayor', fTownHall.fCapital );
          end;
    end;


  // TPoliticalTown

  procedure TPoliticalTown.ElectMayor;
    var
      PTH : TPoliticalTownHall;

    function MayorStays : boolean;
      const
        MaxPeriods          = 2;  // >> Generalize later...
        MinReelectionRating = 80; // >> Generalize later...
      begin
        result :=
          (Mayor.SuperRole <> nil) and
          //(fMayorPeriods < MaxPeriods) and
          //(PTH.fRatings.OverallRating > MinReelectionRating) and
          (fWinningCampaign <> nil) and
          PTH.Campaigns.IsTheBestCampaign(fWinningCampaign);
      end;

    var
      NewMayor : TTycoon;
    begin
      inherited;
      PTH := TPoliticalTownHall(TownHall.CurrBlock);
      PTH.Ratings.Evaluate;
      PTH.Campaigns.Evaluate;
      if (PTH.Campaigns.Campaigns.Count > 0) and not MayorStays
        then
          begin
            // Locate and set winning Campaign
            if fWinningCampaign <> nil
              then fWinningCampaign.Free;
            fWinningCampaign := TCampaign(PTH.Campaigns.Campaigns[0]);
            UncacheObject( fWinningCampaign, noKind, noInfo );
            PTH.Campaigns.Campaigns.Extract( fWinningCampaign );
            PTH.Campaigns.Campaigns.DeleteAll;
            if Mayor.MasterRole <> Mayor
              then
                begin
                  if fLastElections <= 0
                    then fLastElections := max(2000, World.CurrYear - PeriodLength);
                  Mayor.MasterRole.AddItemToCurriculum(
                    TOpenItem.Create(
                      '',
                      0,
                      InstantiateMultiString( mtidWasMayor, [Name, fLastElections, World.CurrYear, PTH.Ratings.OverallRating] ),
                      //SimHints.GetHintText( hidWasMayor, [Name, fLastElections, World.CurrYear, PTH.Ratings.OverallRating] ),
                      20,
                      10*(PTH.Ratings.OverallRating - 50) ));
                end;
            PTH.Ratings.Clear;

            // Assign new mayor
            NewMayor := fWinningCampaign.Tycoon;
            if Mayor.SuperRole <> nil
              then Mayor.SuperRole.AbandomRole( Mayor );
            NewMayor.AssumeRole( Mayor );
            NewMayor.AddItemToCurriculum(
              TOpenItem.Create(
                '',
                currId_Mayor,
                InstantiateMultiString( mtidWonElections, [Name, World.CurrYear] ),
                //SimHints.GetHintText( hidWonElections, [Name, World.CurrYear] ),
                20,
                50 ));
            fMayorPeriods  := 0;
            fLastElections := World.CurrYear;
            PTH.fLastPerPop := PTH.TotalPopulation;
            WorldLocator.SendEvent(
              TEvent.Create(
                0,
                Timer.GetVirtualTimeAbs,
                Timer.GetVirtualTime,
                200000,
                10000,
                InstantiateMultiString( mtidMayorElected, [NewMayor.Name, Name] ),
                //NullString, // NewMayor.Name + ' was elected Mayor of ' + Name + '.',
                '',
                'Visual/Voyager/Politics/politics.asp?TownName=' + Name +
                '&frame_Id=PoliticsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client' ));
          end
        else
          begin
            inc(fMayorPeriods);

            if PTH.Campaigns.Campaigns.Count > 0
              then
                begin
                  PTH.Campaigns.Campaigns.Extract(fWinningCampaign);
                  PTH.Campaigns.Campaigns.DeleteAll;
                end;

            if fWinningCampaign <> nil
              then UncacheObject(fWinningCampaign, noKind, noInfo);

            if Mayor.MasterRole <> Mayor
              then
                begin
                  // Give prestige for the former term
                  Mayor.MasterRole.AddItemToCurriculum(
                    TOpenItem.Create(
                      '',
                      0,
                      InstantiateMultiString( mtidWasMayor, [Name, fLastElections, World.CurrYear, PTH.Ratings.OverallRating] ),
                      //SimHints.GetHintText( hidWasMayor, [Name, fLastElections, World.CurrYear, PTH.Ratings.OverallRating] ),
                      20,
                      10*(PTH.Ratings.OverallRating - 50) ));
                  // Send event
                  WorldLocator.SendEvent(
                    TEvent.Create(
                      0,
                      Timer.GetVirtualTimeAbs,
                      Timer.GetVirtualTime,
                      200000,
                      10000,
                      InstantiateMultiString( mtidMayorReElected, [Mayor.MasterRole.Name, Name] ),
                      //NullString, //Mayor.MasterRole.Name + ' was reelected Mayor of ' + Name + '.',
                      '',
                      'Visual/Voyager/Politics/politics.asp?TownName=' + Name +
                      '&frame_Id=PoliticsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client' ))
                end
              else
                WorldLocator.SendEvent(
                  TEvent.Create(
                    0,
                    Timer.GetVirtualTimeAbs,
                    Timer.GetVirtualTime,
                    200000,
                    10000,
                    InstantiateMultiString( mtidCityHasNoMayor, [Name] ),
                    //NullString, //Name + ' has no Mayor.',
                    '',
                    'Visual/Voyager/Politics/politics.asp?TownName=' + Name +
                    '&frame_Id=PoliticsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client' ));
          end;
      if fWinningCampaign <> nil
        then fWinningCampaign.CurrentVotes := 0;
      ModelServerCache.InvalidateCache(self, false);
    end;

  procedure TPoliticalTown.FireMayor;
    var
      PTH : TPoliticalTownHall;
    begin
      inherited;
      PTH := TPoliticalTownHall(TownHall.CurrBlock);
      if Mayor.MasterRole <> Mayor
        then
          begin
            WorldLocator.SendEvent(
              TEvent.Create(
                0,
                Timer.GetVirtualTimeAbs,
                Timer.GetVirtualTime,
                200000,
                10000,
                InstantiateMultiString( mtidMayorFired, [Mayor.MasterRole.Name, Name] ),
                //NullString, //Mayor.MasterRole.Name + ' was fired from Mayor of ' + Name + '.',
                '',
                'Visual/Voyager/Politics/politics.asp?TownName=' + Name +
                '&frame_Id=PoliticsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client' ));
            Mayor.MasterRole.AddItemToCurriculum(
              TOpenItem.Create(
                '',
                12,
                InstantiateMultiString( mtidWasFiredFromMayor, [Name, World.CurrYear] ),
                //SimHints.GetHintText( hidWasFiredFromMayor, [Name, World.CurrYear] ),
                20,
                -400 ));
            PTH.Ratings.Clear;
            if Mayor.SuperRole <> nil
              then Mayor.SuperRole.AbandomRole( Mayor );
            fMayorPeriods   := 0;
            fLastElections  := World.CurrYear;
            PTH.fLastPerPop := PTH.TotalPopulation;
            // No winning Campaign
            fWinningCampaign.Free;
            fWinningCampaign := nil;
          end;
      Mayor.Budget := 300*1000*1000;
      ModelServerCache.InvalidateCache(self, false);
    end;

  procedure TPoliticalTown.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
    var
      i         : integer;
      nomayor   : boolean;
      SendEvent : boolean;
    begin
      inherited;
      case PeriodType of
        perHour :
          begin
            // When the mayor goes
            if (fWinningCampaign <> nil) and (Mayor.SuperRole = nil)
              then
                begin
                  fWinningCampaign.Free;
                  fWinningCampaign := nil;
                end;
            // When the mayor has no campaign
            if (fWinningCampaign = nil) and (Mayor.SuperRole <> nil)
              then TPoliticalTownHall(TownHall.CurrBlock).SitMayor(Mayor.SuperRole);
            // When the Mayor inherits a campaign, this should never happen
            if (fWinningCampaign <> nil) and (fWinningCampaign.Tycoon <> Mayor.SuperRole)
              then fWinningCampaign.Tycoon := Mayor.SuperRole;
          end;
        perMonth :
          if Mayor.Budget < 0
            then FireMayor;
        perYear :
          if (Mayor.SuperRole = nil) and (TTownHall(TownHall.CurrBlock).TotalPopulation > 9000)
            then
              begin
                SendEvent := true;
                for i := 0 to pred(AllTaxes.Count) do
                  begin
                    if not TTax(AllTaxes[i]).Subsidized
                      then SendEvent := false;
                    TTax(AllTaxes[i]).Subsidized := false;
                  end;
                if SendEvent
                  then
                    WorldLocator.SendEvent(
                      TEvent.Create(
                        0,
                        Timer.GetVirtualTimeAbs,
                        Timer.GetVirtualTime,
                        100000,
                        100,
                        InstantiateMultiString( mtidSubsidiesRemoved, [Name] ),
                        //NullString, //Name + ' removed its subsidies. Population is already greater than 9,000',
                        '', '' ));
              end;
        perPoliticalMandate :
          begin
            nomayor := Mayor.SuperRole = nil;
            ElectMayor;
            if (Mayor.SuperRole <> nil) and nomayor
              then
                for i := 0 to pred(AllTaxes.Count) do
                  TTax(AllTaxes[i]).Subsidized := false;
          end;
      end;
    end;

  procedure TPoliticalTown.TycoonDeleted( Tycoon : TTycoon );
    begin
      TPoliticalTownHall(TownHall.CurrBlock).TycoonDeleted( Tycoon );
      if (fWinningCampaign <> nil) and (fWinningCampaign.Tycoon = Tycoon)
        then
          begin
            fWinningCampaign.Free;
            fWinningCampaign := nil;
          end;
    end;

  procedure TPoliticalTown.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fMayorPeriods := Reader.ReadInteger( 'MayorPeriods', 0 );
      Reader.ReadObject( 'WinningCampaign', fWinningCampaign, nil );
    end;

  procedure TPoliticalTown.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteInteger( 'MayorPeriods', fMayorPeriods );
      Writer.WriteObject( 'WinningCampaign', fWinningCampaign );
    end;

  procedure TPoliticalTown.Loaded;
    begin
      inherited;
      if (Mayor <> nil) and ((Mayor.SuperRole = Mayor) or (Mayor.SuperRole = nil))
        then
          begin
            fWinningCampaign.Free;
            fWinningCampaign := nil;
          end;
    end;

  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TPoliticalTownHall );
      RegisterClass( TMayor );
      RegisterClass( TPoliticalTown );
    end;


end.
