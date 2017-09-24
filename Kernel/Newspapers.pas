unit Newspapers;

interface

  uses
    ModelServerCache, CacheAgent, Kernel;

  const
    tidCachePath_Newspapers = 'Newspapers\';

  type
    TNewspaper =
      class
        private
          fName : string;
          fTown : TTown;
        public
          property Name : string read fName write fName;
          property Town : TTown  read fTown write fTown;
      end;

  type
    TNewspaperCacheAgent =
      class( TCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;       override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

  procedure RegisterCachers;
  
implementation

  // TNewspaperCacheAgent

  class function TNewspaperCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;
    begin
      result := tidCachePath_Newspapers + TNewspaper(Obj).Name + '.five';
    end;

  class function TNewspaperCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      result.WriteString( 'Name', TNewspaper(Obj).Name );
      if TNewspaper(Obj).Town <> nil
        then result.WriteString( 'TownName', TNewspaper(Obj).Town.Name );
    end;


  // RegisterCachers

  procedure RegisterCachers;
    begin
      RegisterCacher( TNewspaper.ClassName, TNewspaperCacheAgent );
    end;

end.
