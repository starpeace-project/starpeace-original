unit CacheCommon;

interface

  uses
    SpecialChars;

  type
    TCacheAction = integer;

  const
    SmallFloatShift = 10000;   

  const
    caCreate = 0;                 
    caUpdate = 1;
    caDelete = 2;

  const
    SubObjForFile = 10;

  // These names are reserved for cache data control, none property can be named
  // using one of them

  const
    PathName   = 'Pth';
    LinkName   = '_Lnk';
    DelPName   = '_DPth';
    ActionName = '_Act';
    ppObjId    = 'ObjectId';
    ppTTL      = '_TTL';
    ppLastMod  = '_LMd';
    TTLNever   = '';
    NULLTTL    = '0×0×0×0';
    ppKind     = 'sobjKind';
    ppInfo     = 'sobjInfo';
    ppVersion  = '_MSVer';
    ppRnAgent  = '_RnAgnt';
    ppObjLoc   = '_ObjLoc';
    ppRefLinks = '_RfLnks';

  const
    resOK    = 'OK';
    resError = 'ERROR';

  const
    fkUnknown     = 0;
    fkTradeCenter = 1 shl 0; // 1
    fkWarehouse   = 1 shl 1; // 2
    fkFacility    = 1 shl 2; // 4

  type                //0,          1,           2,              4,        8,           16,            32,
    TFacilityRole    = (rolNeutral, rolProducer, rolDistributer, rolBuyer, rolImporter, rolCompExport, rolCompInport);
    TFacilityRoleSet = set of TFacilityRole;

  // Registered Cache Object Names

  const
    WSObjectCacherName = 'WSObjectCacher';
    WSCacheUpdaterName = 'WSCacheUpdater';
    MSObjectCacherName = 'MSObjectCacher';

  const
    NotAllowedChars = ['\', '/', ':', '*', '?', '"', '<', '>', '|', #0, BackslashChar, NameSeparator, LinkSep];

  function CreateTTL(Days, Hours, Min, Sec : integer) : string;
  function TTLToDateTime(const TTL : string) : TDateTime;
  function ValidName(name : string) : boolean;

implementation

  uses
    SysUtils, CompStringsParser;

  function CreateTTL( Days, Hours, Min, Sec : integer ) : string;
    begin
      inc( Min, Sec div 60);
      Sec := Sec mod 60;
      inc( Hours, Min div 60 );
      Min := Min mod 60;
      inc( Days, Hours div 24 );
      Hours := Hours mod 24;
      result := IntToStr( Days ) + '×' + IntToStr( Hours ) + '×' + IntToStr( Min ) + '×' + IntToStr( Sec );
    end;

  function TTLToDateTime( const TTL : string ) : TDateTime;
    var
      Arr  : array[1..4] of integer;
      Str  : string;
      p, i : integer;
    begin
      FillChar( Arr, sizeof(Arr), 0 );
      p     := 1;
      i     := 1;
      Str   := GetNextStringUpTo( TTl, p, '×' );
      while (i <= 4) and (Str <> '') do
        begin
          try
            Arr[i] := StrToInt( Str );
          except
            Arr[i] := 0;
          end;
          inc( p );
          inc( i );
          Str := GetNextStringUpTo( TTl, p, '×' );
        end;
      result := Arr[1] + EncodeTime( Arr[2], Arr[3], Arr[4], 0 );
    end;

  function ValidName(name : string) : boolean;
    var
      i : integer;
      l : integer;
    begin
      i := 1;
      l := length(name);
      while (i <= l) and not (name[i] in NotAllowedChars) do
        inc(i);
      result := (l > 0) and (i > l);
      if result
        then
          begin
            i := pos('..', name);
            result := i = 0;
          end;
    end;

end.
