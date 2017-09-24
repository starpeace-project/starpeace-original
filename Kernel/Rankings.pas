unit Rankings;

interface

  uses
    MetaInstances, Classes, Collection, SyncObjs, CacheAgent, Languages;

  type
    TRanking =
      class( TMetaInstance )
        public
          constructor Create( anId, aSuperRanking : string; aName : TMultiString; Max : integer );
          destructor  Destroy; override;
        private
          fName    : TMultiString;
          fSuper   : string;
          fObjects : TSortedCollection;
          fLock    : TCriticalSection;
          fMax     : integer;
        public
          property Name         : TMultiString read fName;
          property SuperRanking : string read fSuper;
          property Objects : TSortedCollection read fObjects;
        public
          procedure Update( const Context );
          procedure AddObject( Obj : TObject );
          procedure DelObject( Obj : TObject );
          procedure Lock;
          procedure Unlock;
          procedure StoreToCache( Cache : TObjectCache ); virtual;
          function  RankingOf( Obj : TObject ) : integer;
        public
          function  CompareObjects( const Obj1, Obj2 : TObject; const Context ) : integer; virtual;
          procedure CacheItem( index : integer; const Obj : TObject; Cache : TObjectCache ); virtual;
          function  IsRankeable( const Obj : TObject ) : boolean; virtual;
          function  AppliesTo( const Obj : TObject ) : boolean; virtual;
          procedure Serialize( Prefix : string; List : TStringList ); virtual;
          procedure SerializeItem( Prefix : string; index : integer; const Obj : TObject; List : TStringList ); virtual;
          class function IsOverall : boolean; virtual;
          class function Serializable : boolean; virtual;
        private
          fContext : pointer;
        private
          function CompareItemsFunc( Obj1, Obj2 : TObject ) : integer;
      end;

  const
    tidClassFamily_Rankings = 'Rankings';
    tidCachePath_Rankings   = 'Rankings\';


  procedure RegisterCachers;


implementation

  uses
    ModelServerCache, ClassStorage, MathUtils, RankProtocol, SysUtils, Logs;


  // TRanking

  constructor TRanking.Create( anId, aSuperRanking : string; aName : TMultiString; Max : integer );
    begin
      inherited Create( anId );
      fName    := aName;
      fMax     := Max;
      fSuper   := aSuperRanking;
      fObjects := TSortedCollection.Create( 0, rkUse, CompareItemsFunc );
      fLock    := TCriticalSection.Create;
    end;

  destructor TRanking.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fObjects.Free;
      fLock.Free;
      fName.Free;
      inherited;
    end;                                             

  procedure TRanking.Update( const Context );
    begin                                               
      Lock;
      try
        fContext := @Context;
        fObjects.Sort( CompareItemsFunc );
      finally
        Unlock;
      end;
    end;
    
  procedure TRanking.AddObject( Obj : TObject );
    begin
      Lock;
      try
        if fObjects.Indexof(Obj) = noIndex
          then fObjects.Insert(Obj);
      finally
        Unlock;
      end;
    end;

  procedure TRanking.DelObject( Obj : TObject );
    begin
      Lock;
      try
        fObjects.Delete( Obj );
      finally
        Unlock;
      end;
    end;

  procedure TRanking.Lock;
    begin
      fLock.Enter;
    end;

  procedure TRanking.Unlock;
    begin
      fLock.Leave;
    end;

  procedure TRanking.StoreToCache( Cache : TObjectCache );
    var
      i     : integer;
      count : integer;
    begin
      Lock;
      try
        //Cache.WriteString( 'Name', fName );
        StoreMultiStringToCache( 'RankingName', fName, Cache );
        Cache.WriteString( 'Id', Id );                                          
        Cache.WriteString( 'SuperRanking', fSuper );
        count := 0;
        for i := 0 to pred(min(fMax, fObjects.Count)) do
          if IsRankeable( fObjects[i] )
            then
              begin
                CacheItem( count, fObjects[i], Cache );
                inc( count );
              end;
        Cache.WriteInteger( 'Count', count );
      finally                                                
        Unlock;
      end;
    end;

  function TRanking.RankingOf( Obj : TObject ) : integer;
    begin
      Lock;
      try
        result := fObjects.IndexOf( Obj );
      finally
        Unlock;
      end;
    end;
    
  function TRanking.CompareObjects( const Obj1, Obj2 : TObject; const Context ) : integer;
    begin
      result := 0;
    end;

  procedure TRanking.CacheItem( index : integer; const Obj : TObject; Cache : TObjectCache );
    begin
    end;

  function TRanking.IsRankeable( const Obj : TObject ) : boolean;
    begin
      result := true;
    end;

  function TRanking.AppliesTo( const Obj : TObject ) : boolean;
    begin
      result := true;
    end;

  function TRanking.CompareItemsFunc( Obj1, Obj2 : TObject ) : integer;
    begin
      result := CompareObjects( Obj1, Obj2, fContext );
    end;

  procedure TRanking.Serialize( Prefix : string; List : TStringList );
    var
      i     : integer;
      count : integer;
    begin
      try
        List.Values[Prefix + tidRankings_RankName]  := Name.Values[langDefault];
        List.Values[Prefix + tidRankings_RankId]    := Id;
        List.Values[Prefix + tidRankings_RankSuper] := fSuper;
        count := 0;
        Lock;
        try
          for i := 0 to pred(fObjects.Count) do
            try
              if IsRankeable( fObjects[i] )
                then
                  begin
                    SerializeItem( Prefix + tidRankings_Member + IntToStr(count), i, fObjects[i], List );
                    inc( count );
                  end;
            except
            end;
        finally
          Unlock;
        end;
        List.Values[Prefix + tidRankings_RankMemberCount] := IntToStr(count);
      except
      end;
    end;

  procedure TRanking.SerializeItem( Prefix : string; index : integer; const Obj : TObject; List : TStringList );
    begin
    end;

  class function TRanking.IsOverall : boolean;
    begin
      result := false;
    end;

  class function TRanking.Serializable : boolean;
    begin
      result := true;
    end;

  // TRankingCache

  type
    TRankingCacheAgent =
      class( TCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;       override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

  class function TRankingCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;

    function GetFullPath( Ranking : TRanking ) : string;
      begin
        if Ranking.SuperRanking <> ''
          then result := GetFullPath( TRanking(TheClassStorage.ClassById[tidClassFamily_Rankings, Ranking.SuperRanking]) )
          else result := '';
        result := result + Ranking.Id + '.five\';
      end;

    begin
      result := tidCachePath_Rankings + GetFullPath( TRanking(Obj) );
    end;

  class function TRankingCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      result.WriteString( 'Path', GetPath( Obj, kind, info ) );
      TRanking(Obj).StoreToCache( result );
    end;


  // RegisterCachers

  procedure RegisterCachers;
    begin
      RegisterCacher( TRanking.ClassName, TRankingCacheAgent );
    end;


end.


