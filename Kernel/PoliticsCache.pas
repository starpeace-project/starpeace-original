unit PoliticsCache;

interface

  uses
    Politics, CacheAgent, KernelCache, Variants;

  const
    tidCachePath_Surveys        = 'Surveys\';
    tidCachePath_Ratings        = 'Ratings\';
    tidCachePath_RatingSystem   = 'CampaignSystem\';
    tidCachePath_Projects       = 'Projects\';
    tidCachePath_Campaigns      = 'Campaigns\';
    tidCachePath_CampaignSystem = 'CampaignSystem\';

  type
    TRatingCacheAgent =
      class( TCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;       override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TProjectCacheAgent =
      class( TCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;       override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TCampaignCacheAgent =
      class( TCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;       override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TPoliticalTownCacheAgent =
      class( TTownCacheAgent )
        public
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

  procedure RegisterCachers;

implementation

  uses
    ModelServerCache, SysUtils, TownPolitics, Languages, Logs;


  // TRatingCacheAgent

  class function TRatingCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;
    begin
      with TRating(Obj) do
        result := System.PoliticalEntity.getCacheFolder + tidCachePath_Ratings + MetaRating.Id + '.five';
    end;

  class function TRatingCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      with TRating(Obj) do
        begin
          result.WriteString( 'Id', MetaRating.Id );
          StoreMultiStringToCache( 'Name', MetaRating.Name_MLS, result );
          result.WriteInteger( 'PeopleRating',   PeopleRating );
          result.WriteInteger( 'IFELRating',     IFELRating );
          result.WriteInteger( 'TycoonsRating',  TycoonsRating );
          result.WriteInteger( 'RulerPublicity', RulerPublicity );
          result.WriteInteger( 'TownHallId', System.PoliticalEntity.getRDOId );
        end;
    end;


  // TProjectCacheAgent

  class function TProjectCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;
    begin
      with TProject(Obj) do
        result := GetObjectPath( Campaign, noKind, noInfo ) + tidCachePath_Projects + MetaProject.Id + '.five';
    end;

  class function TProjectCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      with TProject(Obj) do
        begin
          result.WriteString( 'Id', MetaProject.Id );
          result.WriteString( 'Kind', MetaProject.Kind );
          StoreMultiStringToCache( 'Name', MetaProject.Name_MLS, result );
          result.WriteInteger( 'TownHallId', Campaign.System.PoliticalEntity.getRDOId );
          try
            StoreToCache( result );
          except
          end;
        end;
    end;


  // TCampaignCacheAgent

  class function TCampaignCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;
    begin
      with TCampaign(Obj) do
        result := System.PoliticalEntity.getCacheFolder + tidCachePath_Campaigns + Tycoon.Name + '.five\';
    end;

  class function TCampaignCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    var
      i : integer;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      with TCampaign(Obj) do
        begin
          result.WriteString( 'Tycoon', Tycoon.Name );
          result.WriteInteger( 'Rating', Rating );
          result.WriteInteger( 'Prestige', round(Tycoon.Prestige) );
          result.WriteInteger( 'Ranking', System.Campaigns.IndexOf( Obj ) );
          result.WriteInteger( 'TownHallId', System.PoliticalEntity.getRDOId );
          Projects.Lock;
          try
            for i := 0 to pred(Projects.Count) do
              CacheObject( Projects[i], noKind, noInfo ); // >> CACHE_OPTIMIZE!!
          finally
            Projects.Unlock;
          end;
        end;
    end;


  // TPoliticalTownCacheAgent

  class function TPoliticalTownCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    var
      PTH : TPoliticalTownHall;
      i   : integer;
    begin
      Logs.Log( 'Survival', DateTimeToStr(Now) + ' Caching Town..'); 
      result := inherited GetCache( Obj, kind, info, update );
      with TPoliticalTown(Obj) do
        begin
          PTH := TPoliticalTownHall(TownHall.CurrBlock);
          result.WriteBoolean( 'CanHaveElections', PTH.TotalPopulation > MinPopulation );
          result.WriteBoolean( 'HasRuler', Mayor.SuperRole <> nil );
          result.WriteString( 'ActualRuler', Mayor.MasterRole.Name );
          result.WriteInteger( 'RulerPrestige', round(Mayor.Prestige) );
          if Mayor.MasterRole <> nil
            then result.WriteInteger( 'RulerActualPrestige', round(Mayor.MasterRole.Prestige) );
          result.WriteInteger( 'YearsToElections', round(World.YearsToElections) );
          result.WriteInteger( 'RulerPeriods', round(MayorPeriods) + 1); // Mandate No: 0 is not OK
          result.WriteInteger( 'RulerRating', PTH.Ratings.OverallRating );
          result.WriteInteger( 'IFELRating', PTH.Ratings.IFELRating );
          result.WriteInteger( 'TycoonsRating', PTH.Ratings.TycoonsRating );
          result.WriteInteger( 'CampaignCount', PTH.Campaigns.Campaigns.Count );
          result.WriteInteger( 'TownHallId', integer(TownHall.CurrBlock) );
          for i := 0 to pred(PTH.Campaigns.Campaigns.Count) do
            with TCampaign(PTH.Campaigns.Campaigns[i]) do
              begin
                result.WriteString( 'Tycoon' + IntToStr(i), Tycoon.Name );
                result.WriteInteger( 'Rating' + IntToStr(i), Rating );
                result.WriteInteger( 'Prestige' + IntToStr(i), round(Tycoon.Prestige) );
                result.WriteString( 'CachePath' + IntToStr(i), GetObjectPath( PTH.Campaigns.Campaigns[i], noKind, noInfo ) );
                if not update
                  then CacheObject( PTH.Campaigns.Campaigns[i], noKind, noInfo ); // >> Cache_FIX!!
              end;
          if not update
            then
              for i := 0 to pred(PTH.Ratings.Ratings.Count) do
                CacheObject( PTH.Ratings.Ratings[i], noKind, noInfo ); // >> Cache_FIX!!
        end;
    end;


  // RegisterCachers

  procedure RegisterCachers;
    begin
      RegisterCacher( TRating.ClassName, TRatingCacheAgent );
      RegisterCacher( TProject.ClassName, TProjectCacheAgent );
      RegisterCacher( TCampaign.ClassName, TCampaignCacheAgent );
      RegisterCacher( TPoliticalTown.ClassName, TPoliticalTownCacheAgent );
    end;

end.


